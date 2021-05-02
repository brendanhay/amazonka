{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Network.AWS.S3.Encryption.Envelope
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Encryption.Envelope where

import qualified Conduit
import Control.Lens ((&), (+~), (?~), (^.))
import Control.Monad.Except (ExceptT (ExceptT))
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (MonadResource)
import qualified Crypto.Cipher.AES as AES
import Crypto.Cipher.Types (BlockCipher, Cipher)
import qualified Crypto.Cipher.Types as Cipher
import qualified Crypto.Data.Padding as Padding
import qualified Crypto.Error
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import Crypto.PubKey.RSA.Types (KeyPair, toPrivateKey, toPublicKey)
import Crypto.Random (MonadRandom, getRandomBytes)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap, first)
import qualified Data.Bifunctor as Bifunctor
import Data.ByteArray (ByteArray)
import qualified Data.ByteArray as ByteArray
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as Map
import qualified Network.AWS as AWS
import qualified Network.AWS.KMS as KMS
import qualified Network.AWS.KMS.Lens as KMS
import Network.AWS.Prelude hiding (response)
import Network.AWS.S3.Encryption.Body
import Network.AWS.S3.Encryption.Types

data V1Envelope = V1Envelope
  { -- | @x-amz-key@: Content encrypting key (cek) in encrypted form, base64
    -- encoded. The cek is randomly generated per S3 object, and is always
    -- an AES 256-bit key. The corresponding cipher is always @AES/CBC/PKCS5Padding@.
    _v1Key :: !ByteString,
    -- | @x-amz-iv@: Randomly generated IV (per S3 object), base64 encoded.
    _v1IV :: !(Cipher.IV AES.AES256),
    -- | @x-amz-matdesc@: Customer provided material description in JSON (UTF8)
    -- format.
    _v1Description :: !Description
  }

newV1 ::
  MonadIO m =>
  (ByteString -> IO (Either EncryptionError ByteString)) ->
  Description ->
  m (Either EncryptionError Envelope)
newV1 decryptKey desc =
  liftIO . Except.runExceptT $ do
    ivBytes <- Except.lift (getRandomBytes aesBlockSize)
    keyBytes <- Except.lift (getRandomBytes aesKeySize)

    key <- ExceptT (decryptKey keyBytes)
    iv <- Except.liftEither (createIV ivBytes)
    cipher <- Except.liftEither (createCipher keyBytes)

    pure . V1 cipher $
      V1Envelope
        { _v1Key = key,
          _v1IV = iv,
          _v1Description = desc
        }

decodeV1 ::
  MonadResource m =>
  (ByteString -> IO (Either EncryptionError ByteString)) ->
  [(CI Text, Text)] ->
  m (Either EncryptionError Envelope)
decodeV1 decryptKey meta =
  liftIO . Except.runExceptT $ do
    Base64 k <- Except.liftEither (meta .& "X-Amz-Key")
    Base64 i <- Except.liftEither (meta .& "X-Amz-IV")
    d <- Except.liftEither (meta .& "X-Amz-Matdesc")

    key <- ExceptT (decryptKey k)
    iv <- Except.liftEither (createIV i)
    cipher <- Except.liftEither (createCipher key)

    pure . V1 cipher $
      V1Envelope
        { _v1Key = key,
          _v1IV = iv,
          _v1Description = d
        }

data V2Envelope = V2Envelope
  { -- | @x-amz-key-v2@: CEK in key wrapped form. This is necessary so that
    -- the S3 encryption client that doesn't recognize the v2 format will not
    -- mistakenly decrypt S3 object encrypted in v2 format.
    _v2Key :: !ByteString,
    -- | @x-amz-iv@: Randomly generated IV (per S3 object), base64 encoded.
    _v2IV :: !(Cipher.IV AES.AES256),
    -- | @x-amz-cek-alg@: Content encryption algorithm used.  Supported values:
    -- @AES/GCM/NoPadding@, @AES/CBC/PKCS5Padding@ Default to @AES/CBC/PKCS5Padding@
    -- if this key is absent.
    --
    -- Supported values: @AESWrap@, @RSA/ECB/OAEPWithSHA-256AndMGF1Padding@, @kms@ No
    -- standard key wrapping is used if this meta information is absent Always set to
    -- @kms@ if KMS is used for client-side encryption
    _v2CEKAlgorithm :: !ContentAlgorithm,
    -- | @x-amz-wrap-alg@: Key wrapping algorithm used.
    _v2WrapAlgorithm :: !WrappingAlgorithm,
    -- | @x-amz-matdesc@: Customer provided material description in JSON format.
    -- Used to identify the client-side master key. For KMS client side
    -- encryption, the KMS Customer Master Key ID is stored as part of the material
    -- description, @x-amz-matdesc, under the key-name @kms_cmk_id@.
    _v2Description :: !Description
  }

newV2 ::
  MonadResource m =>
  AWS.Env ->
  Text ->
  Description ->
  m (Either EncryptionError Envelope)
newV2 env keyId desc = do
  let context = Map.insert "kms_cmk_id" keyId (fromDescription desc)

  response <-
    AWS.runAWS env . AWS.send $
      KMS.newGenerateDataKey keyId
        & KMS.generateDataKey_encryptionContext ?~ context
        & KMS.generateDataKey_keySpec ?~ KMS.DataKeySpec_AES_256

  ivBytes <- liftIO (getRandomBytes aesBlockSize)

  pure $ do
    iv <- createIV ivBytes
    cipher <- createCipher (response ^. KMS.generateDataKeyResponse_plaintext)

    pure . V2 cipher $
      V2Envelope
        { _v2Key = response ^. KMS.generateDataKeyResponse_ciphertextBlob,
          _v2IV = iv,
          _v2CEKAlgorithm = AES_CBC_PKCS5Padding,
          _v2WrapAlgorithm = KMSWrap,
          _v2Description = Description context
        }

decodeV2 ::
  MonadResource m =>
  AWS.Env ->
  [(CI Text, Text)] ->
  Description ->
  m (Either EncryptionError Envelope)
decodeV2 env meta desc =
  Except.runExceptT $ do
    a <- Except.liftEither (meta .& "X-Amz-CEK-Alg")
    w <- Except.liftEither (meta .& "X-Amz-Wrap-Alg")
    Base64 raw <- Except.liftEither (meta .& "X-Amz-Key-V2")
    Base64 i <- Except.liftEither (meta .& "X-Amz-IV")
    d <- Except.liftEither (meta .& "X-Amz-Matdesc")

    -- Left-associative merge for material description, keys in the supplied
    -- description override those parsed from the envelope.
    response <-
      AWS.runAWS env . AWS.send $
        KMS.newDecrypt raw
          & KMS.decrypt_encryptionContext ?~ fromDescription (desc <> d)

    key <-
      case response ^. KMS.decryptResponse_plaintext of
        Nothing -> Except.throwError PlaintextUnavailable
        Just x -> pure x

    iv <- Except.liftEither (createIV i)
    cipher <- Except.liftEither (createCipher key)

    pure . V2 cipher $
      V2Envelope
        { _v2Key = key,
          _v2IV = iv,
          _v2CEKAlgorithm = a,
          _v2WrapAlgorithm = w,
          _v2Description = d
        }

data Envelope
  = V1 AES.AES256 V1Envelope
  | V2 AES.AES256 V2Envelope

instance ToHeaders Envelope where
  toHeaders = fmap (first (CI.map ("X-Amz-Meta-" <>))) . toMetadata

instance ToJSON Envelope where
  toJSON = object . map (bimap k v) . toMetadata
    where
      k = toText . CI.foldedCase
      v = Aeson.String . toText

instance ToBody Envelope where
  toBody = toBody . toJSON

toMetadata :: Envelope -> [(CI ByteString, ByteString)]
toMetadata = \case
  V1 _ x -> v1 x
  V2 _ x -> v2 x
  where
    v1 V1Envelope {..} =
      [ ("X-Amz-Key", b64 _v1Key),
        ("X-Amz-IV", b64 (ByteArray.convert _v1IV)),
        ("X-Amz-Matdesc", toBS _v1Description)
      ]

    v2 V2Envelope {..} =
      [ ("X-Amz-Key-V2", b64 _v2Key),
        ("X-Amz-IV", b64 (ByteArray.convert _v2IV)),
        ("X-Amz-CEK-Alg", toBS _v2CEKAlgorithm),
        ("X-Amz-Wrap-Alg", toBS _v2WrapAlgorithm),
        ("X-Amz-Matdesc", toBS _v2Description)
      ]

    b64 :: ByteString -> ByteString
    b64 = toBS . Base64

newEnvelope ::
  MonadResource m =>
  AWS.Env ->
  Key ->
  m (Either EncryptionError Envelope)
newEnvelope env key =
  case key of
    Symmetric c d -> newV1 (pure . Right . Cipher.ecbEncrypt c) d
    Asymmetric p d -> newV1 (rsaEncrypt p) d
    KMS kid d -> newV2 env kid d

decodeEnvelope ::
  MonadResource m =>
  AWS.Env ->
  Key ->
  [(CI Text, Text)] ->
  m (Either EncryptionError Envelope)
decodeEnvelope env key meta =
  case key of
    Symmetric c _ -> decodeV1 (pure . Right . Cipher.ecbDecrypt c) meta
    Asymmetric p _ -> decodeV1 (rsaDecrypt p) meta
    KMS _ d -> decodeV2 env meta d

fromMetadata ::
  MonadResource m =>
  AWS.Env ->
  Key ->
  HashMap Text Text ->
  m (Either EncryptionError Envelope)
fromMetadata env key =
  decodeEnvelope env key . map (first CI.mk)
    . Map.toList

aesKeySize, aesBlockSize :: Int
aesKeySize = 32
aesBlockSize = 16

bodyEncrypt :: Envelope -> RqBody -> RqBody
bodyEncrypt (getCipher -> (aes, iv)) x =
  Chunked $ y `fuseChunks` Conduit.awaitForever (Conduit.yield . go)
  where
    go = Cipher.cbcEncrypt aes iv . Padding.pad (Padding.PKCS7 aesBlockSize)
    y = toChunked x & chunkedLength +~ padding

    padding = n - (contentLength x `mod` n)
    n = fromIntegral aesBlockSize

bodyDecrypt :: Envelope -> RsBody -> RsBody
bodyDecrypt (getCipher -> (aes, iv)) x =
  x `fuseStream` Conduit.awaitForever (Conduit.yield . go)
  where
    go b =
      let r = Cipher.cbcDecrypt aes iv b
       in fromMaybe r (Padding.unpad (Padding.PKCS7 aesBlockSize) r)

rsaEncrypt :: MonadRandom m => KeyPair -> ByteString -> m (Either EncryptionError ByteString)
rsaEncrypt k =
  fmap (Bifunctor.first PubKeyFailure)
    . RSA.encrypt (toPublicKey k)

rsaDecrypt :: MonadRandom m => KeyPair -> ByteString -> m (Either EncryptionError ByteString)
rsaDecrypt k =
  fmap (Bifunctor.first PubKeyFailure)
    . RSA.decryptSafer (toPrivateKey k)

getCipher :: Envelope -> (AES.AES256, Cipher.IV AES.AES256)
getCipher = \case
  V1 c v1 -> (c, _v1IV v1)
  V2 c v2 -> (c, _v2IV v2)

createCipher :: (ByteArray a, Cipher b) => a -> Either EncryptionError b
createCipher =
  Crypto.Error.onCryptoFailure (Left . CipherFailure) Right
    . Cipher.cipherInit

createIV :: BlockCipher a => ByteString -> Either EncryptionError (Cipher.IV a)
createIV b = maybe (Left (IVInvalid (ByteArray.convert b))) Right (Cipher.makeIV b)

(.&) :: FromText a => [(CI Text, Text)] -> CI Text -> Either EncryptionError a
xs .& k =
  case k `lookup` xs of
    Nothing -> Left (EnvelopeMissing k)
    Just x -> Bifunctor.first (EnvelopeInvalid k) (fromText x)
