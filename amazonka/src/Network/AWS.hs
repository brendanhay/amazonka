{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ScopedTypeVariables      #-}

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
    -- ** Pre-signing URLs
    , presign

    -- * Types
    , module Network.AWS.Types
    ) where

import           Control.Concurrent           (threadDelay)
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Monoid
import           Data.Time
import           Network.AWS.Auth
import           Network.AWS.Data
import           Network.AWS.Internal.Env
import           Network.AWS.Internal.Log
import qualified Network.AWS.Signing          as Sign
import           Network.AWS.Types
import           Network.HTTP.Conduit         hiding (Response)
import           Network.HTTP.Types.Status    (statusCode)

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
send e x
    | _envRetry e = retry e x
    | otherwise   = raw e x >>= response x

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

retry :: forall m a. (MonadCatch m, MonadResource m, AWSRequest a)
      => Env
      -> a
      -> m (Response a)
retry e@Env{..} x = go _svcDelay
  where
    go cur = do
        y <- raw e x
        z <- response x y
        case z of
            Right rs -> return (Right rs)
            Left  er
                | Just next <- decrement cur
                , attempt y er -> wait (seconds cur) >> go next
                | otherwise    -> return (Left er)

    Service{..} = service :: Service (Sv a)

    attempt (Right rs) = _svcRetry (statusCode (responseStatus rs))
    attempt (Left  _)  = const False

    wait n = do
        debug _envLogger ("Retrying in " <> build n <> "s ...")
        liftIO (threadDelay (n * 1000000))

    decrement (Exp base grow n)
        | n == 0    = Nothing
        | otherwise = Just (Exp base grow (n - 1))

    seconds (Exp base grow n) =
        truncate $ base * (fromIntegral grow ^^ (n - 1))

raw :: (MonadCatch m, MonadResource m, AWSRequest a)
    => Env
    -> a
    -> m (Either HttpException ClientResponse)
raw Env{..} (request -> rq) = go `catch` er
  where
    go = do
        trace _envLogger (build rq)

        t  <- liftIO getCurrentTime

        Signed m s <- Sign.sign _envAuth _envRegion rq t
        debug  _envLogger (build s)
        trace _envLogger (build m)

        rs <- liftResourceT (http s _envManager)

        return $! Right rs

    er ex = return (Left (ex :: HttpException))
