{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module      : Test.Amazonka.Auth.Background
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Auth.Background (tests) where

import Amazonka.Auth.Background (fetchAuthInBackground)
import Amazonka.Auth.Exception (AuthError (..))
import Amazonka.Core (Sensitive (..), Time (..))
import Amazonka.Types
  ( AccessKey (..),
    AuthEnv (..),
    SecretKey (..),
    ServiceError (..),
  )
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, SomeException (..), throwIO, try)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Network.HTTP.Client (HttpException (..))
import Network.HTTP.Types.Status (status500)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

-- | How long we wait for the background refresh to run (microseconds).
refreshWaitMicros :: Int
refreshWaitMicros = 700_000

tests :: TestTree
tests =
  testGroup
    "Auth background error categorization"
    [ testCase "OtherAuthError wraps non-auth exceptions" $ do
        menv <- mkFailOnRefreshAuthAction (userError "random failure")
        -- fetchAuthInBackground seeds an initial AuthEnv (first call succeeds)
        -- and then a background thread refreshes (subsequent call throws).
        res <- try @AuthError $ do
          _ <- fetchAuthInBackground menv
          -- Why we delay here (and not before):
          --   * fetchAuthInBackground calls the supplied action once, synchronously,
          --     to seed an initial AuthEnv. Only after that, does it spawn a background
          --     thread that waits until the credentials are near expiry before
          --     refreshing.
          --   * The mkFailOnRefreshAuthAction helper (defined below) returns an
          --     AuthEnv expiring ~1s from "now" and throws on subsequent calls.
          --     The background logic will therefore schedule a
          --     refresh roughly halfway to expiry (~0.5s). We sleep a little longer
          --     than that (refreshWaitMicros = 0.7s) so the background thread has
          --     time to reach the refresh point and trigger the failure.
          --   * Sleeping before the initial call would not help, because the
          --     background thread is only created after that seeding call returns.
          threadDelay refreshWaitMicros
        case res of
          Left (OtherAuthError (SomeException _)) -> pure ()
          Left e -> assertFailure ("unexpected error: " ++ show e)
          Right _ -> assertFailure "expected OtherAuthError",
      testCase "AuthServiceError is propagated" $ do
        menv <-
          mkFailOnRefreshAuthAction
            ( AuthServiceError
                ServiceError'
                  { abbrev = "sts",
                    status = status500,
                    headers = [],
                    code = "InternalFailure",
                    message = Nothing,
                    requestId = Nothing
                  }
            )
        res <- try @AuthError $ do
          _ <- fetchAuthInBackground menv
          -- Delay after seeding to give the background thread time to schedule
          -- and perform the refresh (see detailed note in the first test).
          threadDelay refreshWaitMicros
        case res of
          Left (AuthServiceError ServiceError' {code}) -> code @?= "InternalFailure"
          Left e -> assertFailure ("unexpected error: " ++ show e)
          Right _ -> assertFailure "expected AuthServiceError",
      testCase "RetrievalError is propagated" $ do
        menv <- mkFailOnRefreshAuthAction (RetrievalError (InvalidUrlException "url" "bad"))
        res <- try @AuthError $ do
          _ <- fetchAuthInBackground menv
          -- Delay after seeding to give the background thread time to schedule
          -- and perform the refresh (see detailed note in the first test).
          threadDelay refreshWaitMicros
        case res of
          Left (RetrievalError _) -> pure ()
          Left e -> assertFailure ("unexpected error: " ++ show e)
          Right _ -> assertFailure "expected RetrievalError"
    ]

-- | Make an IO AuthEnv-producing action used by fetchAuthInBackground.
-- First call returns a near-expiring AuthEnv; subsequent calls throw `ex`.
mkFailOnRefreshAuthAction :: (Exception e) => e -> IO (IO AuthEnv)
mkFailOnRefreshAuthAction ex = do
  firstRef <- newIORef True
  now <- getCurrentTime
  let env0 =
        AuthEnv
          { accessKeyId = AccessKey "test-ak",
            secretAccessKey = Sensitive (SecretKey "test-sk"),
            sessionToken = Nothing,
            expiration = Just . Time $ addUTCTime 1 now
          }
  pure $ do
    isFirst <- atomicModifyIORef' firstRef (False,)
    if isFirst then pure env0 else throwIO ex
