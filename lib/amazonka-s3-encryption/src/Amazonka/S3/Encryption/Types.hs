{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Amazonka.S3.Encryption.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Encryption.Types where

import Amazonka.Data
import Amazonka.Prelude
import qualified Amazonka.S3 as S3
import Control.Exception (fromException, toException)
import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Error
import qualified Crypto.PubKey.RSA.Types as RSA
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Lens.Micro (lens)
import Lens.Micro.Pro (prism')

-- | An error thrown when performing encryption or decryption.
data EncryptionError
  = -- | Error initialising an AES cipher from a secret key.
    CipherFailure Crypto.Error.CryptoError
  | -- | Failure performing asymmetric encryption/decryption.
    PubKeyFailure RSA.Error
  | -- | Failure creating an IV from some bytes.
    IVInvalid ByteString
  | -- | Required envelope field missing.
    EnvelopeMissing (CI Text)
  | -- | Error parsing envelope.
    EnvelopeInvalid (CI Text) String
  | -- | KMS error when retrieving decrypted plaintext.
    PlaintextUnavailable
  deriving stock (Eq, Show)

instance Exception EncryptionError

class AsEncryptionError s where
  _EncryptionError :: Prism' s EncryptionError

  _CipherFailure :: Prism' s Crypto.Error.CryptoError
  _PubKeyFailure :: Prism' s RSA.Error
  _IVInvalid :: Prism' s ByteString
  _EnvelopeMissing :: Prism' s (CI Text)
  _EnvelopeInvalid :: Prism' s (CI Text, String)
  _PlaintextUnavailable :: Prism' s ()

  _CipherFailure = _EncryptionError . _CipherFailure
  _PubKeyFailure = _EncryptionError . _PubKeyFailure
  _IVInvalid = _EncryptionError . _IVInvalid
  _EnvelopeMissing = _EncryptionError . _EnvelopeMissing
  _EnvelopeInvalid = _EncryptionError . _EnvelopeInvalid
  _PlaintextUnavailable = _EncryptionError . _PlaintextUnavailable

instance AsEncryptionError EncryptionError where
  _EncryptionError = id

instance AsEncryptionError SomeException where
  _EncryptionError = prism' toException fromException

data ContentAlgorithm
  = -- | AES/CBC/PKCS5Padding
    AES_CBC_PKCS5Padding

instance FromText ContentAlgorithm where
  fromText = \case
    "AES/CBC/PKCS5Padding" -> pure AES_CBC_PKCS5Padding
    other -> Left ("Unrecognised content encryption algorithm: " ++ show other)

instance ToByteString ContentAlgorithm where
  toBS AES_CBC_PKCS5Padding = "AES/CBC/PKCS5Padding"

data WrappingAlgorithm
  = -- | Key Management Service.
    KMSWrap

instance FromText WrappingAlgorithm where
  fromText = \case
    "kms" -> pure KMSWrap
    other -> Left ("Unrecognised key wrapping algorithm: " ++ show other)

instance ToByteString WrappingAlgorithm where
  toBS KMSWrap = "kms"

data Location = Metadata | Discard
  deriving stock (Eq)

-- | An instructions file extension.
newtype Ext = Ext Text
  deriving stock (Eq, Show)
  deriving newtype (IsString)

-- | Defaults to @.instruction@
defaultExtension :: Ext
defaultExtension = ".instruction"

appendExtension :: Ext -> S3.ObjectKey -> S3.ObjectKey
appendExtension (Ext s) o@(S3.ObjectKey k)
  | s `Text.isSuffixOf` k = o
  | otherwise = S3.ObjectKey (k <> s)

-- | A key material description. This is attached in plaintext to the metadata,
-- and will be logged using CloudTrail. For KMS decryption any supplemental
-- material description is merged with the description stored on the object during
-- decryption.
newtype Description = Description {fromDescription :: HashMap Text Text}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, FromJSON, ToJSON)

instance ToByteString Description where
  toBS = toBS . Aeson.encode

instance FromText Description where
  fromText = Aeson.eitherDecodeStrict' . Text.encodeUtf8

-- | The key used for encryption and decryption.
data Key
  = Symmetric AES.AES256 Description
  | Asymmetric RSA.KeyPair Description
  | KMS Text Description

-- | Modify the material description of a key.
--
-- /See:/ 'Description'.
description :: Lens' Key Description
description = lens f (flip g)
  where
    f = \case
      Symmetric _ a -> a
      Asymmetric _ a -> a
      KMS _ a -> a

    g a = \case
      Symmetric c _ -> Symmetric c a
      Asymmetric k _ -> Asymmetric k a
      KMS k _ -> KMS k a
