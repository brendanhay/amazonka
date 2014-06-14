{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Signing.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Signing.Types where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class
import qualified Crypto.Hash.SHA256     as SHA256
import qualified Crypto.MAC.HMAC        as HMAC
import           Data.ByteString        (ByteString)
import           Data.Time
import           Network.AWS.Types
import           System.Locale

data family Meta v :: *

data Signed a v = Signed
    { _sgMeta    :: Meta v
    , _sgRequest :: ClientRequest
    }

sgMeta :: Functor f => LensLike' f (Signed a v) (Meta v)
sgMeta f x = (\y -> x { _sgMeta = y }) <$> f (_sgMeta x)

sgRequest :: Functor f => LensLike' f (Signed a v) ClientRequest
sgRequest f x = (\y -> x { _sgRequest = y }) <$> f (_sgRequest x)

class AWSSigner v where
    signed :: v ~ Signer' (Service' a)
           => Service (Service' a)
           -> AuthEnv
           -> Region
           -> Request a
           -> TimeLocale
           -> UTCTime
           -> Signed a v

class AWSPresigner v where
    presigned :: v ~ Signer' (Service' a)
              => Service (Service' a)
              -> AuthEnv
              -> Region
              -> Request a
              -> TimeLocale
              -> Int
              -> UTCTime
              -> Signed a v

sign :: (MonadIO m, AWSRequest a, AWSSigner (Signer' (Service' a)))
     => Auth    -- ^ AWS authentication credentials.
     -> Region  -- ^ AWS Region.
     -> a       -- ^ Request to sign.
     -> UTCTime -- ^ Signing time.
     -> m (Signed a (Signer' (Service' a)))
sign a r rq t = withAuth a $ \e -> return $
    signed service e r (request rq) defaultTimeLocale t

presign :: (MonadIO m, AWSRequest a, AWSPresigner (Signer' (Service' a)))
        => Auth    -- ^ AWS authentication credentials.
        -> Region  -- ^ AWS Region.
        -> a       -- ^ Request to presign.
        -> Int     -- ^ Expiry time in seconds.
        -> UTCTime -- ^ Signing time.
        -> m (Signed a (Signer' (Service' a)))
presign a r rq x t = withAuth a $ \e -> return $
    presigned service e r (request rq) defaultTimeLocale x t

hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 = HMAC.hmac SHA256.hash 64
