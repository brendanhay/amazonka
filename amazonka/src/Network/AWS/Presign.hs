{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Network.AWS.Presign
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- This module contains functions for presigning requests using 'MonadIO' and
-- not one of the AWS specific transformers.
--
-- It is intended for use directly with "Network.AWS.Auth" when only presigning
-- and no other AWS actions are required.
module Network.AWS.Presign where

import           Control.Monad
import           Control.Monad.IO.Class
import           Network.AWS.Data.Time
import           Network.AWS.Lens       ((%~), (&))
import           Network.AWS.Prelude
import           Network.AWS.Request    (requestURL)
import           Network.AWS.Types

-- | Presign an URL that is valid from the specified time until the
-- number of seconds expiry has elapsed.
--
-- /See:/ 'presign', 'presignWith'
presignURL :: (MonadIO m, AWSRequest a)
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
-- /See:/ 'presignWith'
presign :: (MonadIO m, AWSRequest a)
        => Auth
        -> Region
        -> UTCTime     -- ^ Signing time.
        -> Seconds     -- ^ Expiry time.
        -> a           -- ^ Request to presign.
        -> m ClientRequest
presign = presignWith id

-- | A variant of 'presign' that allows modifying the default 'Service'
-- definition used to configure the request.
presignWith :: (MonadIO m, AWSRequest a)
            => (Service -> Service) -- ^ Modify the default service configuration.
            -> Auth
            -> Region
            -> UTCTime              -- ^ Signing time.
            -> Seconds              -- ^ Expiry time.
            -> a                    -- ^ Request to presign.
            -> m ClientRequest
presignWith f a r ts ex x =
    withAuth a $ \ae ->
        return $! sgRequest $
            rqPresign ex (request x & rqService %~ f) ae r ts
