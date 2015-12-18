{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.Sum where

import           Network.AWS.Prelude

data DataKeySpec
    = AES128
    | AES256
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DataKeySpec where
    parser = takeLowerText >>= \case
        "aes_128" -> pure AES128
        "aes_256" -> pure AES256
        e -> fromTextError $ "Failure parsing DataKeySpec from value: '" <> e
           <> "'. Accepted values: AES_128, AES_256"

instance ToText DataKeySpec where
    toText = \case
        AES128 -> "AES_128"
        AES256 -> "AES_256"

instance Hashable     DataKeySpec
instance ToByteString DataKeySpec
instance ToQuery      DataKeySpec
instance ToHeader     DataKeySpec

instance ToJSON DataKeySpec where
    toJSON = toJSONText

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
           <> "'. Accepted values: CreateGrant, Decrypt, DescribeKey, Encrypt, GenerateDataKey, GenerateDataKeyWithoutPlaintext, ReEncryptFrom, ReEncryptTo, RetireGrant"

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
instance ToByteString GrantOperation
instance ToQuery      GrantOperation
instance ToHeader     GrantOperation

instance ToJSON GrantOperation where
    toJSON = toJSONText

instance FromJSON GrantOperation where
    parseJSON = parseJSONText "GrantOperation"

data KeyState
    = Disabled
    | Enabled
    | PendingDeletion
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText KeyState where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        "pendingdeletion" -> pure PendingDeletion
        e -> fromTextError $ "Failure parsing KeyState from value: '" <> e
           <> "'. Accepted values: Disabled, Enabled, PendingDeletion"

instance ToText KeyState where
    toText = \case
        Disabled -> "Disabled"
        Enabled -> "Enabled"
        PendingDeletion -> "PendingDeletion"

instance Hashable     KeyState
instance ToByteString KeyState
instance ToQuery      KeyState
instance ToHeader     KeyState

instance FromJSON KeyState where
    parseJSON = parseJSONText "KeyState"

data KeyUsageType =
    EncryptDecrypt
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText KeyUsageType where
    parser = takeLowerText >>= \case
        "encrypt_decrypt" -> pure EncryptDecrypt
        e -> fromTextError $ "Failure parsing KeyUsageType from value: '" <> e
           <> "'. Accepted values: ENCRYPT_DECRYPT"

instance ToText KeyUsageType where
    toText = \case
        EncryptDecrypt -> "ENCRYPT_DECRYPT"

instance Hashable     KeyUsageType
instance ToByteString KeyUsageType
instance ToQuery      KeyUsageType
instance ToHeader     KeyUsageType

instance ToJSON KeyUsageType where
    toJSON = toJSONText

instance FromJSON KeyUsageType where
    parseJSON = parseJSONText "KeyUsageType"
