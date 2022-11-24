{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}

-- |
-- Module      : Amazonka.Auth.Background
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Helpers for authentication schemes which refresh themselves in the
-- background.
module Amazonka.Auth.Background where

import Amazonka.Auth.Exception
import Amazonka.Data
import Amazonka.Prelude
import Amazonka.Types
import Control.Concurrent (ThreadId)
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import qualified Data.Time as Time
import System.Mem.Weak (Weak)
import qualified System.Mem.Weak as Weak

-- | Implements the background fetching behavior used by (among others)
-- 'fromProfileName' and 'fromContainer'. Given an 'IO' action that produces an
-- 'AuthEnv', this spawns a thread that mutates the 'IORef' returned in the
-- resulting 'Auth' to keep the temporary credentials up to date.
fetchAuthInBackground :: IO AuthEnv -> IO Auth
fetchAuthInBackground menv =
  menv >>= \env -> liftIO $
    case expiration env of
      Nothing -> pure (Auth env)
      Just x -> do
        r <- IORef.newIORef env
        p <- Concurrent.myThreadId
        s <- timer menv r p x

        pure (Ref s r)
  where
    timer :: IO AuthEnv -> IORef AuthEnv -> ThreadId -> ISO8601 -> IO ThreadId
    timer ma r p x =
      Concurrent.forkIO $ do
        s <- Concurrent.myThreadId
        w <- IORef.mkWeakIORef r (Concurrent.killThread s)

        loop ma w p x

    loop :: IO AuthEnv -> Weak (IORef AuthEnv) -> ThreadId -> ISO8601 -> IO ()
    loop ma w p x = do
      untilExpiry <- diff x <$> Time.getCurrentTime
      -- Refresh the token within 5 minutes of expiry, or half its lifetime if
      -- sooner than that. This is to account for execution time of the refresh action.
      let fiveMinutes = 5 * 60 * 1000000
      Concurrent.threadDelay $
        if untilExpiry > fiveMinutes
          then untilExpiry - fiveMinutes
          else untilExpiry `div` 2

      env <- Exception.try ma
      case env of
        Left e -> Exception.throwTo p (RetrievalError e)
        Right a -> do
          mr <- Weak.deRefWeak w
          case mr of
            Nothing -> pure ()
            Just r -> do
              IORef.atomicWriteIORef r a
              maybe (pure ()) (loop ma w p) (expiration a)

    diff (Time x) y = picoToMicro $ if n > 0 then n else 1
      where
        n = truncate (Time.diffUTCTime x y) - 60
        picoToMicro = (* 1000000)
