{-# LANGUAGE CPP #-}

-- |
-- Module      : Amazonka.S3.Encryption.Envelope
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Encryption.Envelope where

import qualified Amazonka as AWS
import Amazonka.Core
import qualified Amazonka.KMS as KMS
import qualified Amazonka.KMS.Lens as KMS
import Amazonka.Prelude hiding (length)
import Amazonka.S3.Encryption.Body
import Amazonka.S3.Encryption.Types
import Conduit ((.|))
import qualified Conduit
import qualified Control.Exception as Exception
import Control.Lens ((?~), (^.))
import Crypto.Cipher.AES (AES256)
import qualified Crypto.Cipher.AES as AES
import Crypto.Cipher.Types (BlockCipher, Cipher, IV)
import qualified Crypto.Cipher.Types as Cipher
import qualified Crypto.Data.Padding as Padding
import qualified Crypto.Error
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import Crypto.PubKey.RSA.Types (KeyPair, toPrivateKey, toPublicKey)
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as Aeson
import Data.ByteArray (ByteArray)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as Map

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Key
#endif

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
  (ByteString -> IO ByteString) ->
  [(CI Text, Text)] ->
  m Envelope
decodeV1 decryptKey meta = do
  Base64 k <- meta .& "X-Amz-Key"
  Base64 i <- meta .& "X-Amz-IV"
  d <- meta .& "X-Amz-Matdesc"

  key <- liftIO (decryptKey k)
  iv <- createIV i
  cipher <- createCipher key

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
  Text ->
  AWS.Env ->
  Description ->
  m Envelope
newV2 kid env d = do
  let context = Map.insert "kms_cmk_id" kid (fromDescription d)

  rs <-
    AWS.send env $
      KMS.newGenerateDataKey kid
        & KMS.generateDataKey_encryptionContext ?~ context
        & KMS.generateDataKey_keySpec ?~ KMS.DataKeySpec_AES_256

  ivBytes <- liftIO (getRandomBytes aesBlockSize)
  iv <- createIV ivBytes
  cipher <- createCipher (rs ^. KMS.generateDataKeyResponse_plaintext)

  pure . V2 cipher $
    V2Envelope
      { _v2Key = rs ^. KMS.generateDataKeyResponse_ciphertextBlob,
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

#if MIN_VERSION_aeson(2,0,0)
instance ToJSON Envelope where
  toJSON = object . map (bimap k v) . toMetadata
    where
      k = Key.fromText . toText . CI.foldedCase
      v = Aeson.String . toText
#else
instance ToJSON Envelope where
  toJSON = object . map (bimap k v) . toMetadata
    where
      k = toText . CI.foldedCase
      v = Aeson.String . toText
#endif

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
  Key ->
  AWS.Env ->
  m Envelope
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
    Symmetric c _ -> decodeV1 (pure . Cipher.ecbDecrypt c) xs
    Asymmetric p _ -> decodeV1 (rsaDecrypt p) xs
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
bodyEncrypt (getCipher -> (aes, iv0)) rqBody =
  Chunked $
    toChunked rqBody
      -- Realign body chunks for upload (AWS enforces chunk limits on all but last)
      & (`fuseChunks` (encryptChunks .| Conduit.chunksOfCE (fromIntegral defaultChunkSize)))
      & addPadding -- extend length for any required AES padding
  where
    encryptChunks = aesCbc iv0 nextChunk lastChunk

    nextChunk iv b =
      let iv' = fromMaybe iv . Cipher.makeIV $ BS.drop (BS.length b - aesBlockSize) r
          r = Cipher.cbcEncrypt aes iv b
       in (iv', r)

    lastChunk iv = Cipher.cbcEncrypt aes iv . Padding.pad (Padding.PKCS7 aesBlockSize)

    addPadding c@ChunkedBody {length} = c {length = length + padding}
    padding = n - (contentLength rqBody `mod` n)
    n = fromIntegral aesBlockSize

bodyDecrypt :: Envelope -> ResponseBody -> ResponseBody
bodyDecrypt (getCipher -> (aes, iv0)) rsBody =
  rsBody `fuseStream` decryptChunks
  where
    decryptChunks = aesCbc iv0 nextChunk lastChunk

    nextChunk iv b =
      let iv' = fromMaybe iv . Cipher.makeIV $ BS.drop (BS.length b - aesBlockSize) b
          r = Cipher.cbcDecrypt aes iv b
       in (iv', r)

    lastChunk iv b =
      let r = Cipher.cbcDecrypt aes iv b
       in fromMaybe r (Padding.unpad (Padding.PKCS7 aesBlockSize) r)

aesCbc ::
  Monad m =>
  IV AES256 ->
  (IV AES256 -> ByteString -> (IV AES256, ByteString)) ->
  (IV AES256 -> ByteString -> ByteString) ->
  Conduit.ConduitT ByteString ByteString m ()
aesCbc iv0 onNextChunk onLastChunk =
  Conduit.chunksOfCE aesBlockSize .| goChunk iv0 Nothing
  where
    goChunk iv carry =
      do
        carry' <- Conduit.await
        case carry' of
          Nothing -> maybe (pure ()) (Conduit.yield . onLastChunk iv) carry
          Just _ -> case carry of
            Nothing -> goChunk iv carry'
            Just chunk -> do
              let (iv', encrypted) = onNextChunk iv chunk
              Conduit.yield encrypted
              goChunk iv' carry'

rsaEncrypt :: KeyPair -> ByteString -> IO ByteString
rsaEncrypt k =
  RSA.encrypt (toPublicKey k)
    >=> hoistEither . first PubKeyFailure

rsaDecrypt :: KeyPair -> ByteString -> IO ByteString
rsaDecrypt k =
  RSA.decryptSafer (toPrivateKey k)
    >=> hoistEither . first PubKeyFailure

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
    Just x -> hoistEither (EnvelopeInvalid k `first` fromText x)

hoistEither :: MonadIO m => Either EncryptionError a -> m a
hoistEither = either throwIO pure

throwIO :: MonadIO m => EncryptionError -> m a
throwIO = liftIO . Exception.throwIO
