{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- |
-- Module      : Network.AWS.S3.Encryption.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Encryption.Types where

import           Control.Exception
import           Control.Exception.Lens
import           Control.Lens
import           Crypto.Cipher.AES
import           Crypto.Error
import           Crypto.PubKey.RSA.Types as RSA
import           Data.Aeson
import qualified Data.ByteString.Lazy    as LBS
import           Data.CaseInsensitive    (CI)
import           Data.String
import qualified Data.Text               as Text
import           Network.AWS
import           Network.AWS.Prelude
import           Network.AWS.S3          (ObjectKey (..))

-- | An error thrown when performing encryption or decryption.
data EncryptionError
    = CipherFailure CryptoError
      -- ^ Error initialising an AES cipher from a secret key.

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

-- | An instructions file extension.
newtype Ext = Ext Text
    deriving (Eq, Show, IsString)

-- | Defaults to @.instruction@
defaultExtension :: Ext
defaultExtension = ".instruction"

appendExtension :: Ext -> ObjectKey -> ObjectKey
appendExtension (Ext s) o@(ObjectKey k)
    | s `Text.isSuffixOf` k = o
    | otherwise             = ObjectKey (k <> s)

-- | A key material description. This is attached in plaintext to the metadata,
-- and will be logged using CloudTrail. For KMS decryption any supplemental
-- material description is merged with the description stored on the object during
-- decryption.
newtype Description = Description { fromDescription :: HashMap Text Text }
    deriving (Eq, Show, Monoid, FromJSON, ToJSON)

instance ToByteString Description where
    toBS = toBS . encode

instance FromText Description where
    parser = parser >>= either fail pure . eitherDecode . LBS.fromStrict

-- | Master key used for encryption and decryption.
data Key
    = Symmetric  AES256  Description
    | Asymmetric KeyPair Description
    | KMS        Text    Description

-- | Modify the material description of a key.
--
-- /See:/ 'Description'.
description :: Lens' Key Description
description = lens f (flip g)
  where
    f = \case
        Symmetric  _ a -> a
        Asymmetric _ a -> a
        KMS        _ a -> a

    g a = \case
        Symmetric  c _ -> Symmetric  c a
        Asymmetric k _ -> Asymmetric k a
        KMS        k _ -> KMS        k a

-- | An 'AWS' environment composed with the master key used to encrypt/decrypt
-- requests. This environment is used in place of 'AWST.Env' when
-- running AWS actions.
data KeyEnv = KeyEnv
    { _envExtended :: !Env -- ^ The underlying 'AWS' environment.
    , _envKey      :: !Key -- ^ The master 'Key' used for encryption.
    }

instance HasEnv KeyEnv where
    environment = lens _envExtended (\s a -> s { _envExtended = a })

class HasKeyEnv a where
    keyed :: Lens' a KeyEnv

    -- | Key material used to encrypt/decrypt request envelopes.
    envKey :: Lens' a Key
    envKey = keyed . lens _envKey (\s a -> s { _envKey = a })

instance HasKeyEnv KeyEnv where
    keyed = id
