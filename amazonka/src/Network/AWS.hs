{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The core module for making requests to the various AWS services.
module Network.AWS
    (
    -- * Environment
      Env
    , envAuth
    , envRegion
    , envManager
    , envLogger
    , envRetry
    -- ** Creating the environment
    , Credentials (..)
    , newEnv
    , getEnv

    -- * Logging
    , LogLevel    (..)
    , Logger
    , newLogger

    -- * Requests
    -- ** Synchronous
    , send
    -- ** Paginated
    , paginate
    -- ** Eventual consistency
    , await
    -- ** Pre-signing URLs
    , presign

    -- * Types
    , module Network.AWS.Types
    , module Network.AWS.Error
    ) where

import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Trans.Resource
import           Control.Retry
import           Data.Conduit                 hiding (await)
import           Data.Time
import           Network.AWS.Auth
import           Network.AWS.Data
import           Network.AWS.Error
import           Network.AWS.Internal.Env
import           Network.AWS.Internal.Log
import qualified Network.AWS.Signing          as Sign
import           Network.AWS.Types            hiding (exponentialBackon)
import           Network.HTTP.Conduit         hiding (Response)

-- | This creates a new environment without debug logging and uses 'getAuth'
-- to expand/discover the supplied 'Credentials'.
--
-- Lenses such as 'envLogger' can be used to modify the 'Env' with a debug logger.
newEnv :: (Functor m, MonadIO m)
       => Region
       -> Credentials
       -> Manager
       -> ExceptT String m Env
newEnv r c m = Env r (\_ _ -> return ()) m True `liftM` getAuth m c

-- | Create a new environment without debug logging, creating a new 'Manager'.
--
-- Any errors are thrown using 'error'.
--
-- /See:/ 'newEnv'
getEnv :: Region -> Credentials -> IO Env
getEnv r c = do
    m <- newManager conduitManagerSettings
    e <- runExceptT (newEnv r c m)
    either error
           return
           e

-- | Send a data type which is an instance of 'AWSRequest', returning either the
-- associated 'Rs' response type in the success case, or the related service's
-- 'Er' type in the error case.
--
-- This includes 'HTTPExceptions', serialisation errors, and any service
-- errors returned as part of the 'Response'.
send :: (MonadCatch m, MonadResource m, AWSRequest a)
     => Env
     -> a
     -> m (Response a)
send Env{..} x@(request -> rq)
    | _envRetry = retrying _rPolicy check go
    | otherwise = go
  where
    go = attempt `catch` err >>= response x

    attempt = do
        trace _envLogger (build rq)
        t  <- liftIO getCurrentTime
        Signed m s <- Sign.sign _envAuth _envRegion rq t
        debug _envLogger (build s)
        trace _envLogger (build m)
        rs <- liftResourceT (http s _envManager)
        return (Right rs)

    err e = return (Left (e :: HttpException))

    check n rs
        | Left (ServiceError _ s e) <- rs
        , _rCheck s e = do
             debug _envLogger ("[Retrying] after " <> build (n + 1) <> " attempts.")
             return True
        | otherwise   = return False

    Retry   {..} = _svcRetry
    Service {..} = serviceOf rq

-- | Send a data type which is an instance of 'AWSPager' and paginate over
-- the associated 'Rs' response type in the success case, or the related service's
-- 'Er' type in the error case.
--
-- /Note:/ The 'ResumableSource' will close when there are no more results or the
-- 'ResourceT' computation is unwrapped. See: 'runResourceT' for more information.
paginate :: (MonadCatch m, MonadResource m, AWSPager a)
         => Env
         -> a
         -> Source m (Response a)
paginate e = go
  where
    go x = do
        y <- lift (send e x)
        yield y
        either (const (return ()))
               (maybe (return ()) go . page x)
               y

-- | !!!
await :: (MonadCatch m, MonadResource m, AWSRequest a)
      => Env
      -> Wait a
      -> a
      -> m (Response a)
await e Wait{..} = retrying policy check . send e
  where
    policy = limitRetries _waitAttempts <> constantDelay _waitDelay

    check n rs = debug (_envLogger e) msg >> return p
      where
        msg = "[Await "
            <> build _waitName
            <> "] "
            <> s
            <> " "
            <> build (n + 1)
            <> " attempt."

        (s, p) = case rs of
            Left  _             -> ("Error",   False)
            Right x
                | _waitAccept x -> ("Success", True)
                | otherwise     -> ("Failure", False)

-- | Presign a URL with expiry to be used at a later time.
--
-- /Note:/ Requires the service's signer to be an instance of 'AWSPresigner'.
-- Not all signing process support this.
presign :: (MonadIO m, AWSRequest a, AWSPresigner (Sg (Sv a)))
        => Env
        -> a       -- ^ Request to presign.
        -> UTCTime -- ^ Signing time.
        -> UTCTime -- ^ Expiry time.
        -> m (Signed a (Sg (Sv a)))
presign Env{..} (request -> rq) = Sign.presign _envAuth _envRegion rq
