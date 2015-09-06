{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

-- |
-- Module      : Network.AWS.S3.Encryption.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Encryption.Types where

import           Control.Exception
import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import qualified Crypto.PubKey.RSA.PKCS15     as RSA
import           Crypto.PubKey.RSA.Types      as RSA
import           Crypto.Random
import           Data.Aeson
import           Data.Bifunctor
import           Data.ByteArray
import qualified Data.ByteString.Lazy         as LBS
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import qualified Data.CaseInsensitive         as CI
import           Data.Conduit
import qualified Data.HashMap.Strict          as Map
import           Data.String
import qualified Data.Text                    as Text
import           Network.AWS
import           Network.AWS.KMS              as KMS
import           Network.AWS.Prelude
import           Network.AWS.S3               (ObjectKey (..))
import qualified Network.AWS.S3               as S3

-- | An error thrown when performing encryption or decryption.
data EncryptionError
    = CipherFailure CryptoError
      -- ^ Error creating an AES cipher.

    | PubKeyFailure RSA.Error
      -- ^ Failure performing asymmetric encryption/decryption.

    | IVInvalid ByteString
      -- ^ Failure creating an IV from some bytes.

    | EnvelopeMissing (CI Text)
      -- ^ Required envelope field missing.

    | EnvelopeInvalid (CI Text) String
      -- ^ Error parsing envelope.

    | PlaintextUnavailable
      -- ^ KMS error when retrieving decrypted plaintext.

      deriving (Eq, Show, Typeable)

instance Exception EncryptionError

makeClassyPrisms ''EncryptionError

instance AsEncryptionError SomeException where
    _EncryptionError = exception

data ContentAlgorithm = AES_CBC_PKCS5Padding -- ^ AES/CBC/PKCS5Padding

instance FromText ContentAlgorithm where
    parser = takeText >>= \case
        "AES/CBC/PKCS5Padding" -> pure AES_CBC_PKCS5Padding
        e                      -> fromTextError $
            "Unrecognised content encryption algorithm: " <> e

instance ToByteString ContentAlgorithm where
    toBS AES_CBC_PKCS5Padding = "AES/CBC/PKCS5Padding"

data WrappingAlgorithm = KMSWrap -- ^ Key Management Service.

instance FromText WrappingAlgorithm where
    parser = takeText >>= \case
        "kms" -> pure KMSWrap
        e     -> fromTextError $
            "Unrecognised key wrapping algorithm: " <> e

instance ToByteString WrappingAlgorithm where
    toBS KMSWrap = "kms"

data Location = Metadata | Discard
    deriving (Eq)

newtype Ext = Ext Text
    deriving (Eq, Show, IsString)

defaultSuffix :: Ext
defaultSuffix = ".instruction"

appendSuffix :: Ext -> ObjectKey -> ObjectKey
appendSuffix (Ext s) o@(ObjectKey k)
    | s `Text.isSuffixOf` k = o
    | otherwise             = ObjectKey (k <> s)

newtype Material = Material { material :: HashMap Text Text }
    deriving (Eq, Show, FromJSON, ToJSON)

instance ToByteString Material where
    toBS = toBS . encode

instance FromText Material where
    parser = parser >>=
        either fail pure . eitherDecode . LBS.fromStrict

-- Provide opaque, smart constructors to create this?
data Key
    = Symmetric  AES256  Material -- Materials is not really used currently?
    | Asymmetric KeyPair Material -- ^
    | KMS        Text -- ^ master key id
