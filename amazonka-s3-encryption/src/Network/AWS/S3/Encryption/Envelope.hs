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

import qualified Control.Exception as Exception
import qualified Conduit
import Control.Lens ( (+~), (?~), (^.))
import qualified Crypto.Cipher.AES as AES
import Crypto.Cipher.Types (BlockCipher, Cipher)
import qualified Crypto.Cipher.Types as Cipher
import qualified Crypto.Data.Padding as Padding
import qualified Crypto.Error
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import Crypto.PubKey.RSA.Types (KeyPair, toPrivateKey, toPublicKey)
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as Aeson
import Data.ByteArray (ByteArray)
import qualified Data.ByteArray as ByteArray
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as Map
import qualified Network.AWS as AWS
import qualified Network.AWS.KMS as KMS
import qualified Network.AWS.KMS.Lens as KMS
import Network.AWS.Core
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

newV1 :: MonadIO m => (ByteString -> IO ByteString) -> Description -> m Envelope
newV1 f d =
 liftIO $ do
  k <- getRandomBytes aesKeySize
  c <- createCipher k
  ek <- f k
  iv <- createIV =<< getRandomBytes aesBlockSize
  
  pure . V1 c $
    V1Envelope
      { _v1Key = ek,
        _v1IV = iv,
        _v1Description = d
      }

decodeV1 ::
  MonadResource m =>
  [(CI Text, Text)] ->
  (ByteString -> IO ByteString) ->
  m Envelope
decodeV1 xs f = do
  k <- xs .& "X-Amz-Key" >>= liftIO . f . unBase64
  iv <- xs .& "X-Amz-IV" >>= createIV . unBase64
  d <- xs .& "X-Amz-Matdesc"
  c <- createCipher k
  pure . V1 c $ V1Envelope k iv d

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

newV2 :: MonadResource m => Text -> AWS.Env -> Description -> m Envelope
newV2 kid env d = do
  let ctx = Map.insert "kms_cmk_id" kid (fromDescription d)
  
  rs <-
    AWS.send env $
      KMS.newGenerateDataKey kid
        & KMS.generateDataKey_encryptionContext ?~ ctx
        & KMS.generateDataKey_keySpec ?~ KMS.DataKeySpec_AES_256

  c <- createCipher (rs ^. KMS.generateDataKeyResponse_plaintext)
  iv <- createIV =<< liftIO (getRandomBytes aesBlockSize)

  pure . V2 c $
    V2Envelope
      { _v2Key = rs ^. KMS.generateDataKeyResponse_ciphertextBlob,
        _v2IV = iv,
        _v2CEKAlgorithm = AES_CBC_PKCS5Padding,
        _v2WrapAlgorithm = KMSWrap,
        _v2Description = Description ctx
      }

decodeV2 ::
  MonadResource m =>
  AWS.Env ->
  [(CI Text, Text)] ->
  Description ->
  m Envelope
decodeV2 env xs m = do
  a <- xs .& "X-Amz-CEK-Alg"
  w <- xs .& "X-Amz-Wrap-Alg"
  raw <- xs .& "X-Amz-Key-V2" >>= pure . unBase64
  iv <- xs .& "X-Amz-IV" >>= createIV . unBase64
  d <- xs .& "X-Amz-Matdesc"

  rs <-
    AWS.send env $
      KMS.newDecrypt raw
        & KMS.decrypt_encryptionContext ?~ fromDescription (m <> d)
  -- Left-associative merge for material description,
  -- keys in the supplied description override those
  -- on the envelope.

  k <- plaintext rs
  c <- createCipher k

  pure . V2 c $ V2Envelope k iv a w d

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

newEnvelope :: MonadResource m => Key -> AWS.Env -> m Envelope
newEnvelope key env =
  case key of
    Symmetric c d -> newV1 (pure . Cipher.ecbEncrypt c) d
    Asymmetric p d -> newV1 (rsaEncrypt p) d
    KMS kid d -> newV2 kid env d

decodeEnvelope ::
  MonadResource m =>
  Key ->
  AWS.Env ->
  [(CI Text, Text)] ->
  m Envelope
decodeEnvelope key env xs =
  case key of
    Symmetric c _ -> decodeV1 xs (pure . Cipher.ecbDecrypt c)
    Asymmetric p _ -> decodeV1 xs (rsaDecrypt p)
    KMS _ d -> decodeV2 env xs d

fromMetadata ::
   MonadResource m =>
  Key ->
  AWS.Env ->
  HashMap Text Text ->
  m Envelope
fromMetadata key env =
  decodeEnvelope key env
  . map (first CI.mk)
  . Map.toList

aesKeySize, aesBlockSize :: Int
aesKeySize = 32
aesBlockSize = 16

bodyEncrypt :: Envelope -> RequestBody -> RequestBody
bodyEncrypt (getCipher -> (aes, iv)) x =
  Chunked $ y `fuseChunks` Conduit.awaitForever (Conduit.yield . go)
  where
    go = Cipher.cbcEncrypt aes iv . Padding.pad (Padding.PKCS7 aesBlockSize)
    y = toChunked x & chunkedLength +~ padding

    padding = n - (contentLength x `mod` n)
    n = fromIntegral aesBlockSize

bodyDecrypt :: Envelope -> ResponseBody -> ResponseBody
bodyDecrypt (getCipher -> (aes, iv)) x =
  x `fuseStream` Conduit.awaitForever (Conduit.yield . go)
  where
    go b =
      let r = Cipher.cbcDecrypt aes iv b
       in fromMaybe r (Padding.unpad (Padding.PKCS7 aesBlockSize) r)

rsaEncrypt :: KeyPair -> ByteString -> IO ByteString
rsaEncrypt k = RSA.encrypt (toPublicKey k) >=> hoistEither PubKeyFailure

rsaDecrypt :: KeyPair -> ByteString -> IO ByteString
rsaDecrypt k = RSA.decryptSafer (toPrivateKey k) >=> hoistEither PubKeyFailure

getCipher :: Envelope -> (AES.AES256, Cipher.IV AES.AES256)
getCipher = \case
  V1 c v1 -> (c, _v1IV v1)
  V2 c v2 -> (c, _v2IV v2)

createCipher :: (MonadIO m, ByteArray a, Cipher b) => a -> m b
createCipher =
  Crypto.Error.onCryptoFailure (throwIO . CipherFailure) pure
    . Cipher.cipherInit

createIV :: (MonadIO m, BlockCipher a) => ByteString -> m (Cipher.IV a)
createIV b = maybe (throwIO $ IVInvalid (ByteArray.convert b)) pure (Cipher.makeIV b)

plaintext :: MonadIO m => KMS.DecryptResponse -> m ByteString
plaintext rs =
  case rs ^. KMS.decryptResponse_plaintext of
    Nothing -> throwIO PlaintextUnavailable
    Just x -> pure x

(.&) :: (MonadIO m, FromText a) => [(CI Text, Text)] -> CI Text -> m a
xs .& k =
  case k `lookup` xs of
    Nothing -> throwIO (EnvelopeMissing k)
    Just x -> hoistEither (EnvelopeInvalid k) (fromText x)

hoistEither :: MonadIO m => (e -> EncryptionError) -> Either e a -> m a
hoistEither f (Left e) = throwIO (f e)
hoistEither _ (Right x) = pure x

throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . Exception.throwIO
