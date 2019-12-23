{-# LANGUAGE Safe #-}

module Control.Monad.Interrupt (
  InterruptT(..),
) where

import qualified Control.Applicative as A
import qualified Control.Concurrent as IO
import qualified Control.Concurrent.Event as Event
import qualified Control.Exception as Exn
import qualified Control.Monad as M
import qualified Control.Monad.Catch as M
import qualified Control.Monad.Except as M
import qualified Control.Monad.Fork as M
import qualified Data.IORef as IO
import qualified System.Posix.Signals as IO

newtype InterruptT m a = InterruptT
  { runInterruptT :: m a
  }

instance Functor m => Functor (InterruptT m) where
  fmap f = InterruptT . fmap f . runInterruptT

instance Applicative m => Applicative (InterruptT m) where
  pure = InterruptT . pure
  InterruptT f <*> InterruptT x = InterruptT (f <*> x)

instance A.Alternative m => A.Alternative (InterruptT m) where
  empty = InterruptT A.empty
  a <|> b = InterruptT $ runInterruptT a A.<|> runInterruptT b

instance Monad m => Monad (InterruptT m) where
  return = InterruptT . return
  InterruptT x >>= f = InterruptT (x >>= runInterruptT . f)

instance M.MonadTrans InterruptT where
  lift = InterruptT

instance M.MonadIO m => M.MonadIO (InterruptT m) where
  liftIO = InterruptT . M.liftIO

instance M.MonadThrow m => M.MonadThrow (InterruptT m) where
  throwM = M.lift . M.throwM

instance M.MonadCatch m => M.MonadCatch (InterruptT m) where
  catch m f = InterruptT $ M.catch (runInterruptT m) $ runInterruptT . f

instance M.MonadMask m => M.MonadMask (InterruptT m) where

  mask go = InterruptT $ M.mask $ \restore ->
    runInterruptT $ go $ InterruptT . restore . runInterruptT

  uninterruptibleMask go = InterruptT $ M.uninterruptibleMask $ \restore ->
    runInterruptT $ go $ InterruptT . restore . runInterruptT

  generalBracket acquire release go = InterruptT $ do
    let acquire' = runInterruptT acquire
        release' x = runInterruptT . release x
        go' = runInterruptT . go
    M.generalBracket acquire' release' go'

instance
  ( M.MonadCatch m
  , M.MonadForkJoin m
  , M.MonadIO m
  ) => M.MonadForkJoin (InterruptT m) where

  forkJoin action = InterruptT $ do

    (eValue, oldHandler, done) <- M.liftIO $ do
      eValue <- IO.newIORef $ Left $ Exn.SomeException Exn.NonTermination
      oldHandler <- IO.newIORef Nothing
      done <- Event.new
      pure (eValue, oldHandler, done)

    eResult <- M.forkJoin $ \thread -> do
      let errorHandler e = do
            IO.killThread thread
            IO.writeIORef eValue $ Left e
            Event.set done

      M.liftIO $ do
        let handler = IO.Catch $ errorHandler $ Exn.SomeException Exn.UserInterrupt
        IO.writeIORef oldHandler . Just =<< IO.installHandler IO.sigINT handler Nothing

      M.handle (M.liftIO . errorHandler) $ do
        x <- runInterruptT $ action thread
        M.liftIO $ do
          IO.writeIORef eValue $ Right x
          Event.set done

    let restore = IO.readIORef oldHandler >>= \mH -> case mH of
          Just h -> M.void $ IO.installHandler IO.sigINT h Nothing
          Nothing -> pure ()

    case eResult of

      Left e -> do
        M.liftIO restore
        pure $ Left e

      Right () -> M.liftIO $ do
        Event.wait done
        restore
        IO.readIORef eValue

