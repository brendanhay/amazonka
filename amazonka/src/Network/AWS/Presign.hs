{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Network.AWS.Presign
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The core module for making requests to the various AWS services.
module Network.AWS.Presign where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch             (MonadCatch (..), catch)
import           Control.Monad.Error.Lens        (catching)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import           Data.Conduit                    hiding (await)
import           Data.Time                       (getCurrentTime)
import           Network.AWS.Auth
import           Network.AWS.Data.Time
import           Network.AWS.Env
import           Network.AWS.Error
import           Network.AWS.Internal.Body
import           Network.AWS.Internal.Retry
import           Network.AWS.Logger
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request             (requestURL)
import           Network.AWS.Sign.V4
import           Network.AWS.Types
import           Network.AWS.Waiter
import           Network.HTTP.Conduit            hiding (Proxy, Request,
                                                  Response)

import           Control.Concurrent.Async.Lifted
import           Control.Monad.Base
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Free        (FreeF (..), FreeT (..))
import           Control.Monad.Trans.Free.Church

-- /See:/ 'presign', 'presignWith'
presignURL :: (MonadIO m, AWSPresigner (Sg (Sv a)), AWSRequest a)
           => Env
           -> UTCTime     -- ^ Signing time.
           -> Integer     -- ^ Expiry time in seconds.
           -> a           -- ^ Request to presign.
           -> m ByteString
presignURL e t ex = liftM requestURL . presign e t ex

-- | Presign an HTTP request that expires after the specified amount of time
-- in the future.
--
-- /Note:/ Requires the 'Service' signer to be an instance of 'AWSPresigner'.
-- Not all signing algorithms support this.
--
-- /See:/ 'presignWith'
presign :: (MonadIO m, AWSPresigner (Sg (Sv a)), AWSRequest a)
        => Env
        -> UTCTime     -- ^ Signing time.
        -> Integer     -- ^ Expiry time in seconds.
        -> a           -- ^ Request to presign.
        -> m ClientRequest
presign e t ex x = presignWith e (serviceOf x) t ex x

-- /Note:/ You can used "Network.AWS.Request.requestURL" to extract a fully
-- signed URL from the request.
presignWith :: (MonadIO m, AWSPresigner (Sg s), AWSRequest a)
            => Env
            -> Service s -- ^ Service configuration.
            -> UTCTime   -- ^ Signing time.
            -> Integer   -- ^ Expiry time in seconds.
            -> a         -- ^ Request to presign.
            -> m ClientRequest
presignWith e s t ex x =
    withAuth (e ^. envAuth) $ \a ->
        return . view sgRequest $
            presigned a (e ^. envRegion) t ex s (request x)
