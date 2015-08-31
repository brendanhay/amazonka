{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- |
-- Module      : Network.AWS.S3.Encryption
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Encryption where

import           Conduit
import           Control.Monad.IO.Class
import           Crypto.Cipher.Types
import           Crypto.Error
import           Crypto.Random
import           Data.Aeson
import           Data.Bifunctor
import           Data.ByteArray
import qualified Data.ByteString.Lazy          as LBS
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.ByteVector
import           Data.Coerce
import           Data.HashMap.Strict           (HashMap)
import           Data.Proxy
import           Network.AWS.Prelude           hiding (coerce)
import           Network.AWS.S3                hiding (getObject, putObject)
import qualified Network.AWS.S3                as S3
import           System.IO

data Location
    = Metadata
--    | InstructionFile Text

-- ^ Not sure how to support the instruction file.
--   Transparent: have AWSRequest.request return 1 or more requests to send.
--   Un-transparent: return two requests from the 'putObject' constructor
--                   if instructionFile is specified.

newtype Materials = Materials (HashMap Text Text)
-- 'x-amz-matdesc' => Json.dump(encryption_context)
-- "{\"kms_cmk_id\":\"kms-key-id\"}",

instance FromText Materials where
    parser = fmap Materials $
        takeText >>= either fail pure . eitherDecode . LBS.fromStrict . toBS

instance ToByteString Materials where
    toBS (Materials m) = toBS (encode m)

data ContentAlgorithm
    = AES_GCM_NoPadding    -- ^ AES/GCM/NoPadding
    | AES_CBC_PKCS5Padding -- ^ AES/CBC/PKCS5Padding

instance FromText ContentAlgorithm where
    parser = takeText >>= \case
--        "AES/GCM/NoPadding"    -> pure AES_GCM_NoPadding
        "AES/CBC/PKCS5Padding" -> pure AES_CBC_PKCS5Padding
        e -> fromTextError $ "Failure parsing CEK algorithm from " <> e

instance ToByteString ContentAlgorithm where
    toBS = \case
        AES_GCM_NoPadding    -> "AES/GCM/NoPadding"
        AES_CBC_PKCS5Padding -> "AES/CBC/PKCS5Padding"

data WrappingAlgorithm
    = AESWrap       -- ^ AESWrap
    | RSA_ECB_OAEP  -- ^ RSA/ECB/OAEPWithSHA-256AndMGF1Padding
    | KMS           -- ^ Key Management Service.

instance FromText WrappingAlgorithm where
    parser = takeText >>= \case
        "AESWrap"                               -> pure AESWrap
        "RSA/ECB/OAEPWithSHA-256AndMGF1Padding" -> pure RSA_ECB_OAEP
        "kms"                                   -> pure KMS
        e -> fromTextError $ "Failure parsing key wrapping algorithim from " <> e

instance ToByteString WrappingAlgorithm where
    toBS = \case
        AESWrap       -> "AESWrap"
        RSA_ECB_OAEP  -> "RSA/ECB/OAEPWithSHA-256AndMGF1Padding"
        KMS           -> "kms"

-- FIXME: base64 encoding

data V1Envelope = V1Envelope
    { _v1Key           :: !ByteString
      -- ^ @x-amz-key@: Content encrypting key (cek) in encrypted form, base64
      -- encoded. The cek is randomly generated per S3 object, and is always
      -- an AES 256-bit key. The corresponding cipher is always @AES/CBC/PKCS5Padding@.
    , _v1IV            :: !Int
      -- ^ @x-amz-iv@: Randomly generated IV (per S3 object), base64 encoded.
    , _v1Materials     :: !Materials
      -- ^ @x-amz-matdesc@: Customer provided material description in JSON (UTF8)
      -- format. Used to identify the client-side master key (ie used to encrypt/wrap
      -- the generated content encrypting key).
    , _v1ContentLength :: !(Maybe Integer)
      -- ^ @x-amz-unencrypted-content-length@: Unencrypted content length (optional but
      -- should be specified whenever possible).
    }

data V2Envelope = V2Envelope
    { _v2Key               :: !ByteString
      -- ^ @x-amz-key-v2@: CEK in key wrapped form. This is necessary so that
      -- the S3 encryption client that doesn't recognize the v2 format will not
      -- mistakenly decrypt S3 object encrypted in v2 format.
    , _v2IV                :: !Int
      -- ^ @x-amz-iv@: Randomly generated IV (per S3 object), base64 encoded.
    , _v2CEKAlgorithm      :: !ContentAlgorithm
      -- ^ @x-amz-cek-alg@: Content encryption algorithm used.  Supported values:
      -- @AES/GCM/NoPadding@, @AES/CBC/PKCS5Padding@ Default to @AES/CBC/PKCS5Padding@
      -- if this key is absent.
      --
      -- Supported values: @AESWrap@, @RSA/ECB/OAEPWithSHA-256AndMGF1Padding@, @kms@ No
      -- standard key wrapping is used if this meta information is absent Always set to
      -- @kms@ if KMS is used for client-side encryption
    , _v2WrapAlgorithm     :: !WrappingAlgorithm
      -- ^ @x-amz-wrap-alg@: Key wrapping algorithm used.
    , _v2Materials         :: !Materials
      -- ^ @x-amz-matdesc@: Customer provided material description in JSON format.
      -- Used to identify the client-side master key.  For KMS client side
      -- encryption, the KMS Customer Master Key ID is stored as part of the material
      -- description, @x-amz-matdesc, under the key-name @kms_cmk_id@.
    , _v2TagLength         :: !(Maybe Integer)
      -- ^ @x-amz-tag-len@: Tag length (in bits) when AEAD is in use.  Only applicable if
      -- AEAD is in use. This meta information is absent otherwise, or if KMS is in use.
      -- Supported value: @128@
    , _v2UnencryptedLength :: !(Maybe Integer)
      -- ^ @x-amz-unencrypted-content-length@: Unencrypted content length. (optional but
      -- should be specified whenever possible.)
    }

data Envelope
    = V1 !V1Envelope
    | V2 !V2Envelope

instance ToHeaders Envelope where
    toHeaders (V1 V1Envelope {..}) =
        [ ("X-Amz-Meta-X-Amz-Key",          _v1Key)
        , ("X-Amz-Meta-X-Amz-IV",      toBS _v1IV)
        , ("X-Amz-Meta-X-Amz-Matdesc", toBS _v1Materials)
        ]
        --    , ("x-amz-unencrypted-content-length", _v1ContentLength)
    toHeaders (V2 V2Envelope {..}) =
        [ ("X-Amz-Meta-X-Amz-Key-V2",        _v2Key)
        , ("X-Amz-Meta-X-Amz-IV",       toBS _v2IV)
        , ("X-Amz-Meta-X-Amz-CEK-Alg",  toBS _v2CEKAlgorithm)
        , ("X-Amz-Meta-X-Amz-Wrap-Alg", toBS _v2WrapAlgorithm)
        , ("X-Amz-Meta-X-Amz-Matdesc",  toBS _v2Materials)
        ]
--    , ("X-Amz-Meta-X-Amz-Unencrypted-content-length",       toBS _v2ContentLength)
--        , ("X-Amz-Meta-X-Amz-Tag-len",                    encode _v2Materials) FIXME: Maybe

fromMetadata :: [Header] -> Either String Envelope
fromMetadata x = V1 <$> v1 <|> V2 <$> v2
  where
    v1 = V1Envelope
        <$> x .#  "X-Amz-Meta-X-Amz-Key"
        <*> x .#  "X-Amz-Meta-X-Amz-IV"
        <*> x .#  "X-Amz-Meta-X-Amz-Matdesc"
        <*> x .#? "X-Amz-Meta-X-Amz-Unencrypted-Content-Length"

    v2 = V2Envelope
        <$> x .#  "X-Amz-Meta-X-Amz-Key-V2"
        <*> x .#  "X-Amz-Meta-X-Amz-IV"
        <*> x .#  "X-Amz-Meta-X-Amz-CEK-Alg"
        <*> x .#  "X-Amz-Meta-X-Amz-Wrap-Alg"
        <*> x .#  "X-Amz-Meta-X-Amz-Matdesc"
        <*> x .#? "X-Amz-Meta-X-Amz-Tag-Len"
        <*> x .#? "X-Amz-Meta-X-Amz-Unencrypted-Content-Length"

-- | FIXME: If toEncryptedBody is used above, this should be opaque/not-exported
-- to prevent people creating unencrypted bodies using the constructor.
-- Shit. The iso would break this.
--
-- Unexported constructor.
data Encrypted a = Encrypted { _envelope :: Envelope, _encrypted :: a }

-- | Only encrypted requests can be modified. 'Encrypted' 'RqBody'
-- is for all intents and purposes opaque, hence the constraint.
encrypted :: AWSRequest a => Lens' (Encrypted a) a
encrypted = lens _encrypted (\s a -> s { _encrypted = a })

putObject :: BucketName
          -> ObjectKey
          -> Encrypted RqBody
          -> Encrypted PutObject
putObject b k (Encrypted e x) = Encrypted e (S3.putObject b k x)

instance AWSRequest (Encrypted PutObject) where
    type Rs (Encrypted PutObject) = PutObjectResponse

    request (Encrypted e x) = coerce (request x) & rqHeaders <>~ toHeaders e
    -- FIXME: add envelope to headers.

--   if md5 = context.params.delete(:content_md5)
--     context.params[:metadata]['x-amz-unencrypted-content-md5'] = md5
--   end

    response l s = const (response l s (Proxy :: Proxy PutObject))

encryptFileIO :: (MonadResource m,  MonadRandom m)
              => FilePath
              -> m (Encrypted RqBody)
encryptFileIO f = liftIO $ do
    -- FIXME: Just going to assume AES256/CBC initially.
    iv  <- randomIV n
    k   <- getRandomBytes n
    aes <- createCipher k


Do the S3 incremental signer. Then figure out how to integrate it nicely:
  pre-signing plugins?
  specialised RqBody just for S3 putObject?

  How to support the mutliple requests it generates?
   - Signers yield one or more requests?

  - What about .instruction files, and their multiple requests?
    Explicit better than implicit. smart ctor return tupled requests?


    -- get original file size.

    -- create secure temporary file.

    -- stream and encrypt the contents to this temp file.

    -- contentSHA256

    (sha, ) <- runResourceT (sourceFile f =$= setChunkSize sz $$ go)

    RqBody (sourceFile f =$= setChunkSize sz =$= go)
  where
    n = 32 -- AES256 key size.

    sz | defaultChunkSize `mod` n == 0 = defaultChunkSize
       | otherwise                     = 32 * 1024

    randomIV c = do
        r <- getRandomBytes 16
        case makeIV r of
            Nothing -> error "getRandomBytes" -- FIXME: explosion!
            Just x  -> return x

    go hash ciph = undefined

setChunkSize :: MonadResource m => Int -> Conduit ByteString m ByteString
setChunkSize n = vectorBuilderC n mapM_CE =$= mapC fromByteVector

-- -- | A 'Conduit' that decrypts a stream of 'B.ByteString'@s@
-- -- using CBC mode.  Expects the input length to be a multiple of
-- -- the block size of the cipher and fails otherwise.
-- conduitDecryptCbc :: (Monad m, C.BlockCipher k) =>
--                      k      -- ^ Cipher key.
--                   -> C.IV k -- ^ Initialization vector.
--                   -> Conduit B.ByteString m B.ByteString
-- conduitDecryptCbc k iv =
--     blockCipherConduit k
--       StrictChunkSize
--       (S.encode iv)
--       (\iv' input -> let output = C.decryptBlock k input `zwp` iv'
--                      in (input, output))
--       (\_ _ -> fail "conduitDecryptCbc: input has an incomplete final block.")


-- conduit-combinators.encodeBase64 :: Monad m => Conduit ByteString m ByteString

-- -- | A cryptonite compatible incremental hash sink.
-- sinkHash :: (Monad m, HashAlgorithm a) => Consumer ByteString m (Digest a)
-- sinkHash = sink hashInit
--   where
--     sink ctx = do
--         b <- await
--         case b of
--             Nothing -> return $! hashFinalize ctx
--             Just bs -> sink $! hashUpdate ctx bs

createCipher :: (Monad m, ByteArray k, Cipher a) => k -> m a
createCipher = onCryptoFailure (error "Crypto.Error") return . cipherInit

-- -- getRandom :: (MonadThrow m, MonadRandom m, BlockCipher a) => a -> m b
-- getRandom c = getRandomBytes n
--   where
--     n = case cipherKeySize c of
--         KeySizeRange _ u -> u
--         KeySizeEnum  xs  -> fromMaybe (blockSize c) (listToMaybe xs)
--         KeySizeFixed x   -> x

   -- random Key

   -- calculate Digest SHA256
   -- encrypt the body





-- re-export entire Network.AWS.S3, with operations that support
-- encryption hidden/replaced.

-- Does 'import Network.AWS.S3.Encryption' make sense, or is
-- 'import Network.AWS.S3.Encrypted' better?

-- New Rq/RsBody wrapper, and relevant conduit functions.

-- Envelopes:
--   Support for adjacent .instruction files (custom file suffix, defaults to .instruction)
--   Support for metadata envelope (Is this just extra headers?) (this is the default)
--     Attempts to retrieve envelope first?

-- 'X-Amz-Meta-X-Amz-Key'=>'gX+a4JQYj7FP0y5TAAvxTz4e2l0DvOItbXByml/NPtKQcUlsoGHoYR/T0TuYHcNj',
-- 'X-Amz-Meta-X-Amz-Iv' => 'TO5mQgtOzWkTfoX4RE5tsA==',
-- 'X-Amz-Meta-X-Amz-Matdesc' => '{}',
-- 'X-Amz-Meta-X-Amz-Unencrypted-Content-Length' => '6'

-- Local:

-- KMS:

-- What about createMultipartUpload?

-- -- | FIXME: If toEncryptedBody is used above, this should be opaque/not-exported
-- -- to prevent people creating unencrypted bodies using the constructor.
-- -- Shit. The iso would break this.
-- --
-- -- Unexported constructor.
-- data Encrypted a = Encrypted { _envelope :: Envelope, _encrypted :: a }

-- -- | Only encrypted requests can be modified. 'Encrypted' 'RqBody'
-- -- is for all intents and purposes opaque, hence the constraint.
-- encrypted :: AWSRequest a => Lens' (Encrypted a) a
-- encrypted = lens _encrypted (\s a -> s { _encrypted = a })

-- putObject :: BucketName
--           -> ObjectKey
--           -> Encrypted RqBody
--           -> Encrypted PutObject
-- putObject b k (Encrypted e x) = Encrypted e (S3.putObject b k x)

-- instance AWSRequest (Encrypted PutObject) where
--     type Rs (Encrypted PutObject) = PutObjectResponse

--     request (Encrypted e x) = coerce (request x)
--     -- FIXME: add envelope to headers.

--     response l s = const (response l s (Proxy :: Proxy PutObject))

-- envelope = {
--     'x-amz-key-v2' => encode64(key_data.ciphertext_blob),
--     'x-amz-iv' => encode64(cipher.iv = cipher.random_iv),
--     'x-amz-cek-alg' => 'AES/CBC/PKCS5Padding',
--     'x-amz-wrap-alg' => 'kms',
--     'x-amz-matdesc' => Json.dump(encryption_context)
--   }

-- V1_ENVELOPE_KEYS = %w(
--   x-amz-key
--   x-amz-iv
--   x-amz-matdesc
-- )

-- V2_ENVELOPE_KEYS = %w(
--   x-amz-key-v2
--   x-amz-iv
--   x-amz-cek-alg
--   x-amz-wrap-alg
--   x-amz-matdesc
-- )

-- -- | Anything that can be safely encrypted to a 'EncryptedBody'.
-- class ToBody a => ToEncryptedBody a where
--     -- | Convert a value to a request body.
--     toEncryptedBody :: Envelope -> a -> Encrypted RqBody

-- encryptBytes :: ByteString -> Encrypted RqBody
-- encryptBytes = undefined

-- encryptHandle :: Handle -> Encrypted RqBody
-- encryptHandle = undefined

-- encryptFile :: FilePath -> Encrypted RqBody
-- encryptFile = undefined





-- newtype Decrypted a = Decrypted { decrypted :: a }

-- getObject :: BucketName
--           -> ObjectKey
--           -> Decrypted GetObject
-- getObject b k = Decrypted (S3.getObject b k)

-- instance AWSRequest (Decrypted GetObject) where
--     type Rs (Decrypted GetObject) = GetObjectResponse

--     request (Decrypted x) = coerce (request x)

--     response l s _ r = do
--         (n, x)        <- response l s (Proxy :: Proxy GetObject) r
--         e :: Envelope <- undefined (x ^. gorsMetadata)
--         return (n, undefined) -- x & gorsBody .~ decryptStream)

--     -- FIXME: deserialize envelope from the headers.

--     -- Is there a way to modify the RsBody to
--     -- do the decryption during streaming/reading?

-- -- decryptBody :: Envelope -> RsBody -> RsBody
-- -- decryptBody e (RsBody src) = RsBody (src =$= decrypt)
-- --   where
-- --     decrypt = do


-- -- instance AWSRequest (Decrypted GetObject) where
-- --     type Rs (Decrypted GetObject) = Encrypted GetObjectResponse
-- --     request (Decrypted x) = request x
-- --     response l s _ r = do
-- --         (n, x) <- response l s (Proxy :: Proxy GetObject) r
-- --         e      <- fromHeaders (x ^. gorsMetadata)
-- --         return (n, Encrypted e x)
-- -- decryptBody :: MonadResource m
-- --             => Encrypted RsBody
-- --             -> Sink ByteString m a
-- --             -> m a
-- -- decryptBody = undefined
