{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Amazonka.S3.Encryption.Encrypt
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Encryption.Encrypt where

import qualified Amazonka as AWS
import Amazonka.Core
import Amazonka.Prelude
import qualified Amazonka.S3 as S3
import Amazonka.S3.Encryption.Envelope
import Amazonka.S3.Encryption.Instructions
import Amazonka.S3.Encryption.Types
import qualified Amazonka.S3.Lens as S3
import Control.Lens ((^.))
import qualified Control.Lens as Lens

-- FIXME: Material

-- | Note about how it doesn't attach metadata by default.
-- You can re-set the location and then discard the PutInstructions request.
encrypted ::
  (MonadResource m, ToEncrypted a) =>
  Key ->
  AWS.Env ->
  a ->
  m (Encrypted a, PutInstructions)
encrypted key env x = do
  e <- newEnvelope key env

  pure
    ( encryptWith x Discard e,
      putInstructions x e
    )

encryptPart ::
  Encrypted S3.CreateMultipartUpload ->
  S3.UploadPart ->
  Encrypted S3.UploadPart
encryptPart e x = encryptWith x Discard (envelope e)

data Encrypted a = Encrypted
  { _encPayload :: a,
    _encHeaders :: [Header],
    _encLocation :: Location,
    _encEnvelope :: Envelope
  }

location :: Setter' (Encrypted a) Location
location = Lens.lens _encLocation (\s a -> s {_encLocation = a})

envelope :: Encrypted a -> Envelope
envelope = _encEnvelope

instance AWSRequest a => AWSRequest (Encrypted a) where
  type AWSResponse (Encrypted a) = AWSResponse a

  request overrides (Encrypted x xs l e) =
    coerce (request overrides x) & updateBodyAndHeaders
    where
      updateBodyAndHeaders :: Request x -> Request x
      updateBodyAndHeaders rq@Request {body, headers} =
        rq
          { body = f body,
            headers = headers <> hs
          }

      f b
        | contentLength b > 0 = bodyEncrypt e b
        | otherwise = b

      hs
        | l == Metadata = xs <> toHeaders e
        | otherwise = xs

  response l s p =
    response l s (proxy p)

proxy :: forall a. Proxy (Encrypted a) -> Proxy a
proxy = const Proxy

class AddInstructions a => ToEncrypted a where
  -- | Create an encryption context.
  encryptWith :: a -> Location -> Envelope -> Encrypted a

instance ToEncrypted S3.CreateMultipartUpload where
  encryptWith x = Encrypted x []

instance ToEncrypted S3.PutObject where
  encryptWith x = Encrypted x (len : maybeToList md5)
    where
      len = ("X-Amz-Unencrypted-Content-Length", toBS (contentLength body))
      md5 = ("X-Amz-Unencrypted-Content-MD5",) <$> md5Base64 body

      body = x ^. S3.putObject_body

-- FIXME: verify these additional headers.
instance ToEncrypted S3.UploadPart where
  encryptWith x = Encrypted x (len : maybeToList md5)
    where
      len = ("X-Amz-Unencrypted-Content-Length", toBS (contentLength body))
      md5 = ("X-Amz-Unencrypted-Content-MD5",) <$> md5Base64 body

      body = x ^. S3.uploadPart_body
