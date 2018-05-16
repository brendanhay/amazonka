{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module      : Network.AWS.S3.Encryption.Envelope
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Encryption.Envelope where

import           Conduit
import           Control.Lens
import           Control.Monad
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Data.Padding
import           Crypto.Error
import qualified Crypto.PubKey.RSA.PKCS15        as RSA
import           Crypto.PubKey.RSA.Types
import           Crypto.Random
import           Data.Aeson
import           Data.Bifunctor
import           Data.ByteArray
import           Data.CaseInsensitive            (CI)
import qualified Data.CaseInsensitive            as CI
import qualified Data.HashMap.Strict             as Map
import           Network.AWS                     hiding (PKCS7)
import           Network.AWS.KMS                 as KMS
import           Network.AWS.Prelude
import           Network.AWS.S3.Encryption.Body
import           Network.AWS.S3.Encryption.Types

data V1Envelope = V1Envelope
    { _v1Key         :: !ByteString
      -- ^ @x-amz-key@: Content encrypting key (cek) in encrypted form, base64
      -- encoded. The cek is randomly generated per S3 object, and is always
      -- an AES 256-bit key. The corresponding cipher is always @AES/CBC/PKCS5Padding@.
    , _v1IV          :: !(IV AES256)
      -- ^ @x-amz-iv@: Randomly generated IV (per S3 object), base64 encoded.
    , _v1Description :: !Description
      -- ^ @x-amz-matdesc@: Customer provided material description in JSON (UTF8)
      -- format.
    }

newV1 :: MonadIO m => (ByteString -> IO ByteString) -> Description -> m Envelope
newV1 f d = liftIO $ do
    k  <- getRandomBytes aesKeySize
    c  <- createCipher k
    ek <- f k
    iv <- createIV =<< getRandomBytes aesBlockSize
    return . V1 c $ V1Envelope
        { _v1Key         = ek
        , _v1IV          = iv
        , _v1Description = d
        }

decodeV1 :: (MonadResource m, MonadThrow m)
         => [(CI Text, Text)]
         -> (ByteString -> IO ByteString)
         -> m Envelope
decodeV1 xs f = do
    k  <- xs .& "X-Amz-Key" >>= liftIO . f . unBase64
    iv <- xs .& "X-Amz-IV"  >>= createIV . unBase64
    d  <- xs .& "X-Amz-Matdesc"
    c  <- createCipher k
    return . V1 c $ V1Envelope k iv d

data V2Envelope = V2Envelope
    { _v2Key           :: !ByteString
      -- ^ @x-amz-key-v2@: CEK in key wrapped form. This is necessary so that
      -- the S3 encryption client that doesn't recognize the v2 format will not
      -- mistakenly decrypt S3 object encrypted in v2 format.
    , _v2IV            :: !(IV AES256)
      -- ^ @x-amz-iv@: Randomly generated IV (per S3 object), base64 encoded.
    , _v2CEKAlgorithm  :: !ContentAlgorithm
      -- ^ @x-amz-cek-alg@: Content encryption algorithm used.  Supported values:
      -- @AES/GCM/NoPadding@, @AES/CBC/PKCS5Padding@ Default to @AES/CBC/PKCS5Padding@
      -- if this key is absent.
      --
      -- Supported values: @AESWrap@, @RSA/ECB/OAEPWithSHA-256AndMGF1Padding@, @kms@ No
      -- standard key wrapping is used if this meta information is absent Always set to
      -- @kms@ if KMS is used for client-side encryption
    , _v2WrapAlgorithm :: !WrappingAlgorithm
      -- ^ @x-amz-wrap-alg@: Key wrapping algorithm used.
    , _v2Description   :: !Description
      -- ^ @x-amz-matdesc@: Customer provided material description in JSON format.
      -- Used to identify the client-side master key. For KMS client side
      -- encryption, the KMS Customer Master Key ID is stored as part of the material
      -- description, @x-amz-matdesc, under the key-name @kms_cmk_id@.
    }

newV2 :: (MonadResource m, MonadThrow m) => Text -> Description -> Env -> m Envelope
newV2 kid d e = do
    let ctx = Map.insert "kms_cmk_id" kid (fromDescription d)
    rs <- runAWS e . send $
        KMS.generateDataKey kid
            & gdkEncryptionContext .~ ctx
            & gdkKeySpec           ?~ KMS.AES256

    c  <- createCipher (rs ^. gdkrsPlaintext)
    iv <- createIV =<< liftIO (getRandomBytes aesBlockSize)

    return . V2 c $ V2Envelope
        { _v2Key           = rs ^. gdkrsCiphertextBlob
        , _v2IV            = iv
        , _v2CEKAlgorithm  = AES_CBC_PKCS5Padding
        , _v2WrapAlgorithm = KMSWrap
        , _v2Description   = Description ctx
        }

decodeV2 :: (MonadResource m, MonadThrow m)
         => [(CI Text, Text)]
         -> Description
         -> Env
         -> m Envelope
decodeV2 xs m e = do
    a   <- xs .& "X-Amz-CEK-Alg"
    w   <- xs .& "X-Amz-Wrap-Alg"
    raw <- xs .& "X-Amz-Key-V2" >>= return   . unBase64
    iv  <- xs .& "X-Amz-IV"     >>= createIV . unBase64
    d   <- xs .& "X-Amz-Matdesc"

    rs  <- runAWS e . send $
        KMS.decrypt raw
            & decEncryptionContext .~ fromDescription (m <> d)
            -- Left-associative merge for material description,
            -- keys in the supplied description override those
            -- on the envelope.

    k   <- plaintext rs
    c   <- createCipher k

    return . V2 c $ V2Envelope k iv a w d

data Envelope
    = V1 AES256 V1Envelope
    | V2 AES256 V2Envelope

instance ToHeaders Envelope where
    toHeaders = fmap (first (CI.map ("X-Amz-Meta-" <>))) . toMetadata

instance ToJSON Envelope where
    toJSON = object . map (bimap k v) . toMetadata
      where
        k = toText . CI.foldedCase
        v = String . toText

instance ToBody Envelope where
    toBody = toBody . toJSON

toMetadata :: Envelope -> [(CI ByteString, ByteString)]
toMetadata = \case
    V1 _ x -> v1 x
    V2 _ x -> v2 x
  where
    v1 V1Envelope {..} =
        [ ("X-Amz-Key",      b64  _v1Key)
        , ("X-Amz-IV",       b64  (convert _v1IV))
        , ("X-Amz-Matdesc",  toBS _v1Description)
        ]

    v2 V2Envelope {..} =
        [ ("X-Amz-Key-V2",   b64  _v2Key)
        , ("X-Amz-IV",       b64  (convert _v2IV))
        , ("X-Amz-CEK-Alg",  toBS _v2CEKAlgorithm)
        , ("X-Amz-Wrap-Alg", toBS _v2WrapAlgorithm)
        , ("X-Amz-Matdesc",  toBS _v2Description)
        ]

    b64 :: ByteString -> ByteString
    b64 = toBS . Base64

newEnvelope :: (MonadResource m, MonadThrow m) => Key -> Env -> m Envelope
newEnvelope k e =
    case k of
        Symmetric  c   d -> newV1 (return . ecbEncrypt c) d
        Asymmetric p   d -> newV1 (rsaEncrypt p) d
        KMS        kid d -> newV2 kid d e

decodeEnvelope :: (MonadResource m, MonadThrow m)
               => Key
               -> Env
               -> [(CI Text, Text)]
               -> m Envelope
decodeEnvelope k e xs =
    case k of
        Symmetric  c _ -> decodeV1 xs (return . ecbDecrypt c)
        Asymmetric p _ -> decodeV1 xs (rsaDecrypt p)
        KMS        _ d -> decodeV2 xs d e

fromMetadata :: (MonadResource m, MonadThrow m) => Key -> Env -> HashMap Text Text -> m Envelope
fromMetadata key e = decodeEnvelope key e . map (first CI.mk) . Map.toList

aesKeySize, aesBlockSize :: Int
aesKeySize   = 32
aesBlockSize = 16

bodyEncrypt :: Envelope -> RqBody -> RqBody
bodyEncrypt (getCipher -> (aes, iv)) x = Chunked $
    y `fuseChunks` awaitForever (yield . go)
  where
    go = cbcEncrypt aes iv . pad (PKCS7 aesBlockSize)
    y  = toChunked x & chunkedLength +~ padding

    padding = n - (contentLength x `mod` n)
    n       = fromIntegral aesBlockSize

bodyDecrypt :: Envelope -> RsBody -> RsBody
bodyDecrypt (getCipher -> (aes, iv)) x =
    x `fuseStream` awaitForever (yield . go)
  where
    go b = let r = cbcDecrypt aes iv b
            in fromMaybe r (unpad (PKCS7 aesBlockSize) r)

rsaEncrypt :: (MonadThrow m, MonadRandom m) => KeyPair -> ByteString -> m ByteString
rsaEncrypt k = RSA.encrypt (toPublicKey k) >=> hoistError PubKeyFailure

rsaDecrypt :: (MonadThrow m, MonadRandom m) => KeyPair -> ByteString -> m ByteString
rsaDecrypt k = RSA.decryptSafer (toPrivateKey k) >=> hoistError PubKeyFailure

getCipher :: Envelope -> (AES256, IV AES256)
getCipher = \case
    V1 c v1 -> (c, _v1IV v1)
    V2 c v2 -> (c, _v2IV v2)

createCipher :: (MonadThrow m, ByteArray a, Cipher b) => a -> m b
createCipher = onCryptoFailure (throwM . CipherFailure) return . cipherInit

createIV :: (MonadThrow m, BlockCipher a) => ByteString -> m (IV a)
createIV b = maybe (throwM $ IVInvalid (convert b)) return (makeIV b)

plaintext :: MonadThrow m => DecryptResponse -> m ByteString
plaintext rs =
   case rs ^. drsPlaintext of
       Nothing -> throwM PlaintextUnavailable
       Just x  -> return x

(.&) :: (MonadThrow m, FromText a) => [(CI Text, Text)] -> CI Text -> m a
xs .& k =
    case k `lookup` xs of
        Nothing -> throwM (EnvelopeMissing k)
        Just x  -> hoistError (EnvelopeInvalid k) (fromText x)

hoistError :: MonadThrow m => (e -> EncryptionError) -> Either e a -> m a
hoistError f (Left  e) = throwM (f e)
hoistError _ (Right x) = return x
