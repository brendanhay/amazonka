{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Signing.Common
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Signing.Common where

import           Control.Monad.Base
import qualified Crypto.Hash.SHA256     as SHA256
import qualified Crypto.MAC.HMAC        as HMAC
import           Data.ByteString        (ByteString)
import           Data.Time
import           Network.AWS.Types
import           System.Locale

sign :: (MonadBase IO m, AWSRequest a, AWSSigner (Sg (Sv a)))
     => Auth      -- ^ AWS authentication credentials.
     -> Region    -- ^ AWS Region.
     -> Request a -- ^ Request to sign.
     -> UTCTime   -- ^ Signing time.
     -> m (Signed a (Sg (Sv a)))
sign a r rq t = withAuth a $ \e -> return $
    signed service e r rq defaultTimeLocale t

presign :: (MonadBase IO m, AWSRequest a, AWSPresigner (Sg (Sv a)))
        => Auth      -- ^ AWS authentication credentials.
        -> Region    -- ^ AWS Region.
        -> Request a -- ^ Request to presign.
        -> UTCTime   -- ^ Signing time.
        -> Int       -- ^ Expiry time in seconds.
        -> m (Signed a (Sg (Sv a)))
presign a r rq t x = withAuth a $ \e -> return $
    presigned service e r rq defaultTimeLocale t x

hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 = HMAC.hmac SHA256.hash 64
