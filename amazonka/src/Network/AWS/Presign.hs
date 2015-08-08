{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Network.AWS.Presign
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- This module contains functions for presigning requests using 'MonadIO' and
-- not one of the AWS specific transformers.
--
-- It is intended for use directly with "Network.AWS.Auth" when only presigning
-- is required and you wish to avoid using the 'Network.AWS.AWS' monad, since it
-- does not use the underlying 'FreeT' 'Network.AWS.Free.Command' DSL.
-- If you wish to presign requests and are using either 'Network.AWS.AWS' or
-- 'Control.Monad.Trans.AWS.AWST', then prefer one of the relevant
-- 'Control.Monad.Trans.AWS.presign'ing functions available there.
module Network.AWS.Presign where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Network.AWS.Data.Time
import           Network.AWS.Prelude
import           Network.AWS.Request    (requestURL)
import           Network.AWS.Types

import           Prelude

-- | Presign an URL that is valid from the specified time until the
-- number of seconds expiry has elapsed.
--
-- /See:/ 'presign', 'presignWith'
presignURL :: (MonadIO m, AWSPresigner (Sg (Sv a)), AWSRequest a)
           => Auth
           -> Region
           -> UTCTime     -- ^ Signing time.
           -> Seconds     -- ^ Expiry time.
           -> a           -- ^ Request to presign.
           -> m ByteString
presignURL a r e ts = liftM requestURL . presign a r e ts

-- | Presign an HTTP request that is valid from the specified time until the
-- number of seconds expiry has elapsed.
--
-- This requires the 'Service' signer to be an instance of 'AWSPresigner'.
-- Not all signing algorithms support this.
--
-- /See:/ 'presignWith'
presign :: (MonadIO m, AWSPresigner (Sg (Sv a)), AWSRequest a)
        => Auth
        -> Region
        -> UTCTime     -- ^ Signing time.
        -> Seconds     -- ^ Expiry time.
        -> a           -- ^ Request to presign.
        -> m ClientRequest
presign a r ts ex = presignWith id a r ts ex

-- | A variant of 'presign' that allows specifying the 'Service' definition
-- used to configure the request.
presignWith :: (MonadIO m, AWSPresigner (Sg s), AWSRequest a)
            => (Service (Sv a) -> Service s) -- ^ Modify the default service configuration.
            -> Auth
            -> Region
            -> UTCTime                       -- ^ Signing time.
            -> Seconds                       -- ^ Expiry time.
            -> a                             -- ^ Request to presign.
            -> m ClientRequest
presignWith s a r ts ex x =
    withAuth a $ \ae ->
        return . view sgRequest $
            presigned ae r ts ex (s (serviceOf x)) (request x)
