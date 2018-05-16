{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.Sum where

import Network.AWS.Prelude

data AlgorithmSpec
  = RsaesOaepSha1
  | RsaesOaepSha256
  | RsaesPKCS1V15
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AlgorithmSpec where
    parser = takeLowerText >>= \case
        "rsaes_oaep_sha_1" -> pure RsaesOaepSha1
        "rsaes_oaep_sha_256" -> pure RsaesOaepSha256
        "rsaes_pkcs1_v1_5" -> pure RsaesPKCS1V15
        e -> fromTextError $ "Failure parsing AlgorithmSpec from value: '" <> e
           <> "'. Accepted values: rsaes_oaep_sha_1, rsaes_oaep_sha_256, rsaes_pkcs1_v1_5"

instance ToText AlgorithmSpec where
    toText = \case
        RsaesOaepSha1 -> "RSAES_OAEP_SHA_1"
        RsaesOaepSha256 -> "RSAES_OAEP_SHA_256"
        RsaesPKCS1V15 -> "RSAES_PKCS1_V1_5"

instance Hashable     AlgorithmSpec
instance NFData       AlgorithmSpec
instance ToByteString AlgorithmSpec
instance ToQuery      AlgorithmSpec
instance ToHeader     AlgorithmSpec

instance ToJSON AlgorithmSpec where
    toJSON = toJSONText

data DataKeySpec
  = AES128
  | AES256
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DataKeySpec where
    parser = takeLowerText >>= \case
        "aes_128" -> pure AES128
        "aes_256" -> pure AES256
        e -> fromTextError $ "Failure parsing DataKeySpec from value: '" <> e
           <> "'. Accepted values: aes_128, aes_256"

instance ToText DataKeySpec where
    toText = \case
        AES128 -> "AES_128"
        AES256 -> "AES_256"

instance Hashable     DataKeySpec
instance NFData       DataKeySpec
instance ToByteString DataKeySpec
instance ToQuery      DataKeySpec
instance ToHeader     DataKeySpec

instance ToJSON DataKeySpec where
    toJSON = toJSONText

data ExpirationModelType
  = KeyMaterialDoesNotExpire
  | KeyMaterialExpires
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExpirationModelType where
    parser = takeLowerText >>= \case
        "key_material_does_not_expire" -> pure KeyMaterialDoesNotExpire
        "key_material_expires" -> pure KeyMaterialExpires
        e -> fromTextError $ "Failure parsing ExpirationModelType from value: '" <> e
           <> "'. Accepted values: key_material_does_not_expire, key_material_expires"

instance ToText ExpirationModelType where
    toText = \case
        KeyMaterialDoesNotExpire -> "KEY_MATERIAL_DOES_NOT_EXPIRE"
        KeyMaterialExpires -> "KEY_MATERIAL_EXPIRES"

instance Hashable     ExpirationModelType
instance NFData       ExpirationModelType
instance ToByteString ExpirationModelType
instance ToQuery      ExpirationModelType
instance ToHeader     ExpirationModelType

instance ToJSON ExpirationModelType where
    toJSON = toJSONText

instance FromJSON ExpirationModelType where
    parseJSON = parseJSONText "ExpirationModelType"

data GrantOperation
  = CreateGrant
  | Decrypt
  | DescribeKey
  | Encrypt
  | GenerateDataKey
  | GenerateDataKeyWithoutPlaintext
  | ReEncryptFrom
  | ReEncryptTo
  | RetireGrant
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GrantOperation where
    parser = takeLowerText >>= \case
        "creategrant" -> pure CreateGrant
        "decrypt" -> pure Decrypt
        "describekey" -> pure DescribeKey
        "encrypt" -> pure Encrypt
        "generatedatakey" -> pure GenerateDataKey
        "generatedatakeywithoutplaintext" -> pure GenerateDataKeyWithoutPlaintext
        "reencryptfrom" -> pure ReEncryptFrom
        "reencryptto" -> pure ReEncryptTo
        "retiregrant" -> pure RetireGrant
        e -> fromTextError $ "Failure parsing GrantOperation from value: '" <> e
           <> "'. Accepted values: creategrant, decrypt, describekey, encrypt, generatedatakey, generatedatakeywithoutplaintext, reencryptfrom, reencryptto, retiregrant"

instance ToText GrantOperation where
    toText = \case
        CreateGrant -> "CreateGrant"
        Decrypt -> "Decrypt"
        DescribeKey -> "DescribeKey"
        Encrypt -> "Encrypt"
        GenerateDataKey -> "GenerateDataKey"
        GenerateDataKeyWithoutPlaintext -> "GenerateDataKeyWithoutPlaintext"
        ReEncryptFrom -> "ReEncryptFrom"
        ReEncryptTo -> "ReEncryptTo"
        RetireGrant -> "RetireGrant"

instance Hashable     GrantOperation
instance NFData       GrantOperation
instance ToByteString GrantOperation
instance ToQuery      GrantOperation
instance ToHeader     GrantOperation

instance ToJSON GrantOperation where
    toJSON = toJSONText

instance FromJSON GrantOperation where
    parseJSON = parseJSONText "GrantOperation"

data KeyManagerType
  = AWS
  | Customer
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText KeyManagerType where
    parser = takeLowerText >>= \case
        "aws" -> pure AWS
        "customer" -> pure Customer
        e -> fromTextError $ "Failure parsing KeyManagerType from value: '" <> e
           <> "'. Accepted values: aws, customer"

instance ToText KeyManagerType where
    toText = \case
        AWS -> "AWS"
        Customer -> "CUSTOMER"

instance Hashable     KeyManagerType
instance NFData       KeyManagerType
instance ToByteString KeyManagerType
instance ToQuery      KeyManagerType
instance ToHeader     KeyManagerType

instance FromJSON KeyManagerType where
    parseJSON = parseJSONText "KeyManagerType"

data KeyState
  = Disabled
  | Enabled
  | PendingDeletion
  | PendingImport
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText KeyState where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        "pendingdeletion" -> pure PendingDeletion
        "pendingimport" -> pure PendingImport
        e -> fromTextError $ "Failure parsing KeyState from value: '" <> e
           <> "'. Accepted values: disabled, enabled, pendingdeletion, pendingimport"

instance ToText KeyState where
    toText = \case
        Disabled -> "Disabled"
        Enabled -> "Enabled"
        PendingDeletion -> "PendingDeletion"
        PendingImport -> "PendingImport"

instance Hashable     KeyState
instance NFData       KeyState
instance ToByteString KeyState
instance ToQuery      KeyState
instance ToHeader     KeyState

instance FromJSON KeyState where
    parseJSON = parseJSONText "KeyState"

data KeyUsageType =
  EncryptDecrypt
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText KeyUsageType where
    parser = takeLowerText >>= \case
        "encrypt_decrypt" -> pure EncryptDecrypt
        e -> fromTextError $ "Failure parsing KeyUsageType from value: '" <> e
           <> "'. Accepted values: encrypt_decrypt"

instance ToText KeyUsageType where
    toText = \case
        EncryptDecrypt -> "ENCRYPT_DECRYPT"

instance Hashable     KeyUsageType
instance NFData       KeyUsageType
instance ToByteString KeyUsageType
instance ToQuery      KeyUsageType
instance ToHeader     KeyUsageType

instance ToJSON KeyUsageType where
    toJSON = toJSONText

instance FromJSON KeyUsageType where
    parseJSON = parseJSONText "KeyUsageType"

data OriginType
  = AWSKMS
  | External
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OriginType where
    parser = takeLowerText >>= \case
        "aws_kms" -> pure AWSKMS
        "external" -> pure External
        e -> fromTextError $ "Failure parsing OriginType from value: '" <> e
           <> "'. Accepted values: aws_kms, external"

instance ToText OriginType where
    toText = \case
        AWSKMS -> "AWS_KMS"
        External -> "EXTERNAL"

instance Hashable     OriginType
instance NFData       OriginType
instance ToByteString OriginType
instance ToQuery      OriginType
instance ToHeader     OriginType

instance ToJSON OriginType where
    toJSON = toJSONText

instance FromJSON OriginType where
    parseJSON = parseJSONText "OriginType"

data WrappingKeySpec =
  Rsa2048
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WrappingKeySpec where
    parser = takeLowerText >>= \case
        "rsa_2048" -> pure Rsa2048
        e -> fromTextError $ "Failure parsing WrappingKeySpec from value: '" <> e
           <> "'. Accepted values: rsa_2048"

instance ToText WrappingKeySpec where
    toText = \case
        Rsa2048 -> "RSA_2048"

instance Hashable     WrappingKeySpec
instance NFData       WrappingKeySpec
instance ToByteString WrappingKeySpec
instance ToQuery      WrappingKeySpec
instance ToHeader     WrappingKeySpec

instance ToJSON WrappingKeySpec where
    toJSON = toJSONText
