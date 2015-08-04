{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Network.AWS.Internal.Presign
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Internal.Presign where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Network.AWS.Data.Time
import           Network.AWS.Env
import           Network.AWS.Prelude
import           Network.AWS.Request    (requestURL)
import           Network.AWS.Types

-- | Presign an URL that is valid from the specified time until the
-- number of seconds expiry has elapsed.
--
-- /See:/ 'presign', 'presignWith'
presignURL :: (MonadIO m, AWSPresigner (Sg (Sv a)), AWSRequest a)
           => Env
           -> UTCTime     -- ^ Signing time.
           -> Seconds     -- ^ Expiry time.
           -> a           -- ^ Request to presign.
           -> m ByteString
presignURL e t ex = liftM requestURL . presign e t ex

-- | Presign an HTTP request that is valid from the specified time until the
-- number of seconds expiry has elapsed.
--
-- This requires the 'Service' signer to be an instance of 'AWSPresigner'.
-- Not all signing algorithms support this.
--
-- /See:/ 'presignWith'
presign :: (MonadIO m, AWSPresigner (Sg (Sv a)), AWSRequest a)
        => Env
        -> UTCTime     -- ^ Signing time.
        -> Seconds     -- ^ Expiry time.
        -> a           -- ^ Request to presign.
        -> m ClientRequest
presign e t ex = presignWith e id t ex

-- | A variant of 'presign' that allows specifying the 'Service' definition
-- used to configure the request.
presignWith :: (MonadIO m, AWSPresigner (Sg s), AWSRequest a)
            => Env
            -> (Service (Sv a) -> Service s) -- ^ Function to modify the service configuration.
            -> UTCTime                       -- ^ Signing time.
            -> Seconds                       -- ^ Expiry time.
            -> a                             -- ^ Request to presign.
            -> m ClientRequest
presignWith e f t ex x =
    withAuth (e ^. envAuth) $ \a ->
        return . view sgRequest $
            presigned a (e ^. envRegion) t ex (f (serviceOf x)) (request x)
