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
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.Sum where

import           Network.AWS.Prelude

data DataKeySpec
    = AES128
    | AES256
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DataKeySpec where
    parser = takeLowerText >>= \case
        "aes_128" -> pure AES128
        "aes_256" -> pure AES256
        e -> fromTextError $ "Failure parsing DataKeySpec from value: '" <> e
           <> "'. Accepted values: aes_128, aes_256"

instance ToText DataKeySpec where
    toText = \case
        AES128 -> "aes_128"
        AES256 -> "aes_256"

instance Hashable DataKeySpec
instance ToQuery DataKeySpec
instance ToHeader DataKeySpec

instance ToJSON DataKeySpec where
    toJSON = toJSONText

data GrantOperation
    = Encrypt
    | GenerateDataKeyWithoutPlaintext
    | CreateGrant
    | RetireGrant
    | GenerateDataKey
    | Decrypt
    | ReEncryptTo
    | ReEncryptFrom
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText GrantOperation where
    parser = takeLowerText >>= \case
        "creategrant" -> pure CreateGrant
        "decrypt" -> pure Decrypt
        "encrypt" -> pure Encrypt
        "generatedatakey" -> pure GenerateDataKey
        "generatedatakeywithoutplaintext" -> pure GenerateDataKeyWithoutPlaintext
        "reencryptfrom" -> pure ReEncryptFrom
        "reencryptto" -> pure ReEncryptTo
        "retiregrant" -> pure RetireGrant
        e -> fromTextError $ "Failure parsing GrantOperation from value: '" <> e
           <> "'. Accepted values: creategrant, decrypt, encrypt, generatedatakey, generatedatakeywithoutplaintext, reencryptfrom, reencryptto, retiregrant"

instance ToText GrantOperation where
    toText = \case
        CreateGrant -> "creategrant"
        Decrypt -> "decrypt"
        Encrypt -> "encrypt"
        GenerateDataKey -> "generatedatakey"
        GenerateDataKeyWithoutPlaintext -> "generatedatakeywithoutplaintext"
        ReEncryptFrom -> "reencryptfrom"
        ReEncryptTo -> "reencryptto"
        RetireGrant -> "retiregrant"

instance Hashable GrantOperation
instance ToQuery GrantOperation
instance ToHeader GrantOperation

instance ToJSON GrantOperation where
    toJSON = toJSONText

instance FromJSON GrantOperation where
    parseJSON = parseJSONText "GrantOperation"

data KeyUsageType =
    EncryptDecrypt
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText KeyUsageType where
    parser = takeLowerText >>= \case
        "encrypt_decrypt" -> pure EncryptDecrypt
        e -> fromTextError $ "Failure parsing KeyUsageType from value: '" <> e
           <> "'. Accepted values: encrypt_decrypt"

instance ToText KeyUsageType where
    toText = \case
        EncryptDecrypt -> "encrypt_decrypt"

instance Hashable KeyUsageType
instance ToQuery KeyUsageType
instance ToHeader KeyUsageType

instance ToJSON KeyUsageType where
    toJSON = toJSONText

instance FromJSON KeyUsageType where
    parseJSON = parseJSONText "KeyUsageType"
