{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

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
import           Control.Lens                  (view)
import           Control.Monad
import           Control.Monad.IO.Class
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import qualified Crypto.PubKey.RSA.PKCS15      as RSA
import           Crypto.PubKey.RSA.Types
import           Crypto.Random
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.Bifunctor
import           Data.ByteArray                hiding (view)
import qualified Data.ByteString.Lazy          as LBS
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.ByteVector
import           Data.CaseInsensitive          (CI, FoldCase)
import qualified Data.CaseInsensitive          as CI
import           Data.Coerce
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as Map
import           Data.Proxy
import           Data.String
import           GHC.TypeLits
import           Network.AWS
import           Network.AWS.KMS               hiding (AES256)
import qualified Network.AWS.KMS               as KMS
import           Network.AWS.Prelude           hiding (coerce)
import           Network.AWS.Response
import           Network.AWS.Response
import           Network.AWS.S3
import           Network.HTTP.Client           (responseHeaders, responseStatus)
import qualified Network.HTTP.Client           as Client
import           System.IO

data ContentAlgorithm = AES_CBC_PKCS5Padding -- ^ AES/CBC/PKCS5Padding

instance FromText ContentAlgorithm where
    parser = takeText >>= \case
        "AES/CBC/PKCS5Padding" -> pure AES_CBC_PKCS5Padding
        e -> fromTextError $ "Unrecognised content encryption algorithm: " <> e

instance ToByteString ContentAlgorithm where
    toBS AES_CBC_PKCS5Padding = "AES/CBC/PKCS5Padding"

data WrappingAlgorithm = KMSWrap -- ^ Key Management Service.

instance FromText WrappingAlgorithm where
    parser = takeText >>= \case
        "kms" -> pure KMSWrap
        e     -> fromTextError $ "Unrecognised key wrapping algorithm: " <> e

instance ToByteString WrappingAlgorithm where
    toBS KMSWrap = "kms"

-- FIXME: base64 encoding

data V1Envelope = V1Envelope
    { _v1Key      :: !ByteString
      -- ^ @x-amz-key@: Content encrypting key (cek) in encrypted form, base64
      -- encoded. The cek is randomly generated per S3 object, and is always
      -- an AES 256-bit key. The corresponding cipher is always @AES/CBC/PKCS5Padding@.
    , _v1IV       :: !(IV AES256)
      -- ^ @x-amz-iv@: Randomly generated IV (per S3 object), base64 encoded.
    , _v1Material :: !Material
      -- ^ @x-amz-matdesc@: Customer provided material description in JSON (UTF8)
      -- format. Used to identify the client-side master key (ie used to encrypt/wrap
      -- the generated content encrypting key).
    }

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
    , _v2Material      :: !Material
      -- ^ @x-amz-matdesc@: Customer provided material description in JSON format.
      -- Used to identify the client-side master key.  For KMS client side
      -- encryption, the KMS Customer Master Key ID is stored as part of the material
      -- description, @x-amz-matdesc, under the key-name @kms_cmk_id@.
    }

newtype Material = Material (HashMap Text Text)

-- Provide opaque, smart constructors to create this?
data Key
    = Symmetric  AES256  Material
    | Asymmetric KeyPair Material
    | KMS        Text -- ^ master key id

data Envelope
    = V1 AES256 V1Envelope
    | V2 AES256 V2Envelope

envelopeCipher :: Envelope -> (AES256, IV AES256)
envelopeCipher = \case
    V1 c v1 -> (c, _v1IV v1)
    V2 c v2 -> (c, _v2IV v2)

newEnvelope :: (MonadResource m, MonadRandom m)
            => Env
            -> Key
            -> m Envelope
newEnvelope e = \case
    Symmetric  secret m -> v1 (ecbEncrypt secret) m
    Asymmetric pair   m -> undefined -- v1 (RSA.encrypt (toPublicKey pair)) m FIXME: handle errors
    KMS        mkid     -> v2 mkid
   where
    v1 encrypt m = do
        k  <- getRandomBytes aesKeySize
        c  <- createCipher k
        iv <- randomIV
        return . V1 c $ V1Envelope
            { _v1Key      = encrypt k
            , _v1IV       = iv
            , _v1Material = m
            }

    v2 mkid = do
        let ctx = Map.singleton "kms_cmk_id" mkid

        rs <- runAWS e . send $
            generateDataKey mkid
                & gdkEncryptionContext .~ ctx
                & gdkKeySpec           ?~ KMS.AES256

        c  <- createCipher (rs ^. gdkrsPlaintext)
        iv <- randomIV
        return . V2 c $ V2Envelope
            { _v2Key           =  rs ^. gdkrsCiphertextBlob
            , _v2IV            = iv
            , _v2CEKAlgorithm  = AES_CBC_PKCS5Padding
            , _v2WrapAlgorithm = KMSWrap
            , _v2Material      = Material ctx
            }

    aesKeySize   = 32 -- AES256 key size.
    aesBlockSize = 16

    -- sz | defaultChunkSize `mod` n == 0 = defaultChunkSize
    --    | otherwise                     = 32 * 1024

    randomIV = do
        r <- getRandomBytes aesBlockSize
        case makeIV (r :: ByteString) :: Maybe (IV AES256) of
            Nothing -> error "getRandomBytes" -- FIXME: explosion!
            Just x  -> return x

parseEnvelope :: MonadResource m
              => Env
              -> Key
              -> [(CI ByteString, ByteString)]
              -> m Envelope
parseEnvelope e k = undefined

createCipher :: (Monad m, ByteArray k, Cipher a) => k -> m a
createCipher = onCryptoFailure (error "Crypto.Error") return . cipherInit

encryptBody :: Envelope -> RqBody -> RqBody
encryptBody (envelopeCipher -> (aes, iv)) x =
    Chunked (toChunked x `fuseChunks` encrypt)
  where
    encrypt = awaitForever (yield . cbcEncrypt aes iv)

decryptBody :: Envelope -> RsBody -> RsBody
decryptBody (envelopeCipher -> (aes, iv)) x =
    x `fuseStream` decrypt
  where
    decrypt = awaitForever (yield . cbcDecrypt aes iv)

instructionSuffix :: Text
instructionSuffix = ".instruction"

data Location = Metadata | Instruction

data Encrypted a = Encrypted a !Location Envelope
data Decrypted a = Decrypted a (Maybe Envelope)

-- putEncryptedObject :: (MonadRandom m, MonadReader r m, HasEnv r)
--                    => Key
--                    -> BucketName
--                    -> ObjectKey
--                    -> RqBody
--                    -> m (Encrypted PutObject)
putEncryptedObject key b k x = do
    env <- view environment
    Encrypted (putObject b k x) Metadata <$>
        undefined -- newEnvelope key
-- Both of these go into normal headers for putObject, not metadata:
-- context.params[:metadata]['x-amz-unencrypted-content-length'] = io.size
-- if md5 = context.params.delete(:content_md5)
-- context.params[:metadata]['x-amz-unencrypted-content-md5'] = md5

instance AWSRequest (Encrypted PutObject) where
    type Rs (Encrypted PutObject) = PutObjectResponse

    request (Encrypted x l e) =
        let rq = coerce (request x) & rqBody %~ encryptBody e
         in case l of
            Metadata    -> rq & rqHeaders <>~ toHeaders e
            Instruction -> rq

    response l s (Encrypted x _ _) = response l s x

-- getEncryptedObject :: BucketName
--                    -> ObjectKey
--                    -> Decrypted GetObject
-- getEncryptedObject b k = Decrypted (getObject b k) Nothing

-- instance AWSRequest (Decrypted GetObject) where
--     type Rs (Decrypted GetObject) = GetObjectResponse

--     request (Decrypted x _) = coerce (request x)

--     response l s (Decrypted x m) r = do
--         e       <- maybe (parseEnvelope s r) return m
--         (n, rs) <- response l s x r
--         return (n, rs & gorsBody %~ decryptBody e)

-- data PutInstruction = PutInstruction BucketName ObjectKey Text RqBody Envelope

-- putEncryptedObjectInstruction :: MonadRandom m
--                               => Key
--                               -> BucketName
--                               -> ObjectKey
--                               -> RqBody
--                               -> m PutInstruction
-- putEncryptedObjectInstruction key b k x =
--     PutInstruction b k instructionSuffix x <$>
--         createEnvelope key (contentLength x)

-- instance AWSRequest PutInstruction where
--     type Rs PutInstruction = Encrypted PutObject

--     request (PutInstruction b k ext _ e) =
--         coerce . request $ putObject b (k {- <.> ext -}) (toBody (toJSON e))

--     response l s rq@(PutInstruction b k _ x e) =
--         receiveNull (Encrypted (putObject b k x) Instruction e) l s rq

-- data GetInstruction = GetInstruction BucketName ObjectKey Text

-- -- giInstructionExtension :: Lens' GetInstruction Text

-- getEncryptedObjectInstruction :: BucketName
--                                -> ObjectKey
--                                -> GetInstruction
-- getEncryptedObjectInstruction b k = GetInstruction b k instructionSuffix

-- instance AWSRequest GetInstruction where
--     type Rs GetInstruction = Decrypted GetObject

--     request (GetInstruction b k ext) =
--         coerce . request $ getObject b (k) -- <.> ext)

--     response l s rq@(GetInstruction b k _) = receiveJSON (\_ _ -> f) l s rq
--        where
--          f o = do
--              e <- parseEither parseJSON (Object o)
--              return $! Decrypted (getObject b k) (Just e)

-- -- Headers storage.
-- instance ToHeaders Envelope where
--     toHeaders = map (first CI.mk) . toKVs -- prepend x-amz-meta ?

-- -- Instruction file storage.
-- instance ToJSON Envelope where
--     toJSON = toJSON . Map.fromList . map (bimap toText toText) . toKVs

-- instance FromJSON Envelope where
--     parseJSON = parseJSON >=> either fail pure . fromKVs . f
--       where
--         f :: HashMap Text Text -> [Header]
--         f = map (bimap (CI.mk . toBS) toBS) . Map.toList

-- toKVs :: Envelope -> [(ByteString, ByteString)]
-- toKVs = \case
--     V1 V1Envelope {..} ->
--         [ ("x-amz-key",          _v1Key)
--     --        , ("X-Amz-Meta-X-Amz-IV",      toBS _v1IV)
--         , ("x-amz-matdesc", toBS _v1Material)
--         ]

--     V2 V2Envelope {..} -> []
--         -- [ ("X-Amz-Meta-X-Amz-Key-V2",        _v2Key)
--         -- , ("X-Amz-Meta-X-Amz-IV",       toBS _v2IV)
--         -- , ("X-Amz-Meta-X-Amz-CEK-Alg",  toBS _v2CEKAlgorithm)
--         -- , ("X-Amz-Meta-X-Amz-Wrap-Alg", toBS _v2WrapAlgorithm)
--         -- , ("X-Amz-Meta-X-Amz-Matdesc",  toBS _v2Material)
--         -- ]


-- --       Base64 encoding.


      -- FIXME: md5
      -- if md5 = context.params.delete(:content_md5)
      --   context.params[:metadata]['x-amz-unencrypted-content-md5'] = md5
      -- end

-- fromKVs :: [(CI ByteString, ByteString)] -> Either String Envelope
-- fromKVs x = V1 <$> v1 <|> V2 <$> v2
--   where
--     v1 = V1Envelope
--         <$> x .#  "X-Amz-Key"
--         <*> undefined -- iv
--         <*> x .#  "X-Amz-Matdesc"
--         <*> x .#? "X-Amz-Unencrypted-Content-Length"

--     v2 = V2Envelope
--         <$> x .#  "X-Amz-Meta-X-Amz-Key-V2"
--         <*> x .#  "X-Amz-Meta-X-Amz-IV"
--         <*> x .#  "X-Amz-Meta-X-Amz-CEK-Alg"
--         <*> x .#  "X-Amz-Meta-X-Amz-Wrap-Alg"
--         <*> x .#  "X-Amz-Meta-X-Amz-Matdesc"
--         <*> x .#? "X-Amz-Meta-X-Amz-Tag-Len"
--         <*> x .#? "X-Amz-Meta-X-Amz-Unencrypted-Content-Length"

-- parseEnvelope :: MonadThrow m => Service -> ClientResponse -> m Envelope
-- parseEnvelope s r =
--     case fromKVs (responseHeaders r) of
--         Right m -> return m
--         Left  e -> throwM . SerializeError $
--             SerializeError' (_svcAbbrev s) (responseStatus r) e

-- - Note about or enforce the chunkSize to be a multiple of 128?
-- - How to get this module to appear in the generated cabal file?
-- - How to customise materials, kms vs aes etc.
-- - FUTURE: getObject should be transparent, or, if instruction file is supported then not?

-- target :: AWSRequest a => Lens' (Encryd a) a
-- target = lens _target (\s a -> s { _tar = a })

-- setChunkSize :: MonadResource m => Int -> Conduit ByteString m ByteString
-- setChunkSize n = vectorBuilderC n mapM_CE =$= mapC fromByteVector

-- -- -- | A 'Conduit' that decrypts a stream of 'B.ByteString'@s@
-- -- -- using CBC mode.  Expects the input length to be a multiple of
-- -- -- the block size of the cipher and fails otherwise.
-- -- conduitDecryptCbc :: (Monad m, C.BlockCipher k) =>
-- --                      k      -- ^ Cipher key.
-- --                   -> C.IV k -- ^ Initialization vector.
-- --                   -> Conduit B.ByteString m B.ByteString
-- -- conduitDecryptCbc k iv =
-- --     blockCipherConduit k
-- --       StrictChunkSize
-- --       (S.encode iv)
-- --       (\iv' input -> let output = C.decryptBlock k input `zwp` iv'
-- --                      in (input, output))
-- --       (\_ _ -> fail "conduitDecryptCbc: input has an incomplete final block.")

-- -- conduit-combinators.encodeBase64 :: Monad m => Conduit ByteString m ByteString

-- -- -- | A cryptonite compatible incremental hash sink.
-- -- sinkHash :: (Monad m, HashAlgorithm a) => Consumer ByteString m (Digest a)
-- -- sinkHash = sink hashInit
-- --   where
-- --     sink ctx = do
-- --         b <- await
-- --         case b of
-- --             Nothing -> return $! hashFinalize ctx
-- --             Just bs -> sink $! hashUpdate ctx bs

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
