{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Network.AWS.S3.Encryption.Encrypt
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Encryption.Encrypt where

import           Control.Lens (Setter', lens, view, (&), (%~), (<>~), (^.), to)
import           Control.Monad
import           Control.Monad.Trans.AWS
import           Data.Coerce
import           Data.Proxy
import           Network.AWS.Prelude
import           Network.AWS.S3
import           Network.AWS.S3.Encryption.Envelope
import           Network.AWS.S3.Encryption.Instructions
import           Network.AWS.S3.Encryption.Types

-- FIXME: Material

-- | Note about how it doesn't attach metadata by default.
-- You can re-set the location and then discard the PutInstructions request.
encrypted :: (AWSConstraint r m, HasKeyEnv r, ToEncrypted a)
          => a
          -> m (Encrypted a, PutInstructions)
encrypted x = do
    e <- join $ newEnvelope <$> view envKey <*> view environment
    return ( encryptWith x Discard e
           , putInstructions x e
           )

encryptPart :: Encrypted CreateMultipartUpload
            -> UploadPart
            -> Encrypted UploadPart
encryptPart e x = encryptWith x Discard (envelope e)

data Encrypted a = Encrypted
    { _encPayload  :: a
    , _encHeaders  :: [Header]
    , _encLocation :: Location
    , _encEnvelope :: Envelope
    }

location :: Setter' (Encrypted a) Location
location = lens _encLocation (\s a -> s { _encLocation = a })

envelope :: Encrypted a -> Envelope
envelope = _encEnvelope

instance AWSRequest a => AWSRequest (Encrypted a) where
    type Rs (Encrypted a) = Rs a

    request (Encrypted x xs l e) = coerce (request x)
        & rqBody     %~ f
        & rqHeaders <>~ hs
      where
        f b | contentLength b > 0 = bodyEncrypt e b
            | otherwise           = b

        hs  | l == Metadata = xs <> toHeaders e
            | otherwise     = xs

    response l s p = response l s (proxy p)

proxy :: forall a. Proxy (Encrypted a) -> Proxy a
proxy = const Proxy

class AddInstructions a => ToEncrypted a where
    -- | Create an encryption context.
    encryptWith :: a -> Location -> Envelope -> Encrypted a

instance ToEncrypted CreateMultipartUpload where
    encryptWith x = Encrypted x []

instance ToEncrypted PutObject where
    encryptWith x = Encrypted x (len : maybeToList md5)
     where
        len = ("X-Amz-Unencrypted-Content-Length",
            toBS (contentLength (x ^. poBody)))

        md5 = ("X-Amz-Unencrypted-Content-MD5",)
            <$> x ^. poBody . to md5Base64

-- FIXME: verify these additional headers.
instance ToEncrypted UploadPart where
    encryptWith x = Encrypted x (len : maybeToList md5)
     where
        len = ("X-Amz-Unencrypted-Content-Length",
            toBS (contentLength (x ^. upBody)))

        md5 = ("X-Amz-Unencrypted-Content-MD5",)
            <$> x ^. upBody . to md5Base64
