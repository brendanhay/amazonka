{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.GrantOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.GrantOperation where

import Network.AWS.Prelude

data GrantOperation
  = CreateGrant
  | Decrypt
  | DescribeKey
  | Encrypt
  | GenerateDataKey
  | GenerateDataKeyPair
  | GenerateDataKeyPairWithoutPlaintext
  | GenerateDataKeyWithoutPlaintext
  | GetPublicKey
  | ReEncryptFrom
  | ReEncryptTo
  | RetireGrant
  | Sign
  | Verify
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText GrantOperation where
  parser =
    takeLowerText >>= \case
      "creategrant" -> pure CreateGrant
      "decrypt" -> pure Decrypt
      "describekey" -> pure DescribeKey
      "encrypt" -> pure Encrypt
      "generatedatakey" -> pure GenerateDataKey
      "generatedatakeypair" -> pure GenerateDataKeyPair
      "generatedatakeypairwithoutplaintext" -> pure GenerateDataKeyPairWithoutPlaintext
      "generatedatakeywithoutplaintext" -> pure GenerateDataKeyWithoutPlaintext
      "getpublickey" -> pure GetPublicKey
      "reencryptfrom" -> pure ReEncryptFrom
      "reencryptto" -> pure ReEncryptTo
      "retiregrant" -> pure RetireGrant
      "sign" -> pure Sign
      "verify" -> pure Verify
      e ->
        fromTextError $
          "Failure parsing GrantOperation from value: '" <> e
            <> "'. Accepted values: creategrant, decrypt, describekey, encrypt, generatedatakey, generatedatakeypair, generatedatakeypairwithoutplaintext, generatedatakeywithoutplaintext, getpublickey, reencryptfrom, reencryptto, retiregrant, sign, verify"

instance ToText GrantOperation where
  toText = \case
    CreateGrant -> "CreateGrant"
    Decrypt -> "Decrypt"
    DescribeKey -> "DescribeKey"
    Encrypt -> "Encrypt"
    GenerateDataKey -> "GenerateDataKey"
    GenerateDataKeyPair -> "GenerateDataKeyPair"
    GenerateDataKeyPairWithoutPlaintext -> "GenerateDataKeyPairWithoutPlaintext"
    GenerateDataKeyWithoutPlaintext -> "GenerateDataKeyWithoutPlaintext"
    GetPublicKey -> "GetPublicKey"
    ReEncryptFrom -> "ReEncryptFrom"
    ReEncryptTo -> "ReEncryptTo"
    RetireGrant -> "RetireGrant"
    Sign -> "Sign"
    Verify -> "Verify"

instance Hashable GrantOperation

instance NFData GrantOperation

instance ToByteString GrantOperation

instance ToQuery GrantOperation

instance ToHeader GrantOperation

instance ToJSON GrantOperation where
  toJSON = toJSONText

instance FromJSON GrantOperation where
  parseJSON = parseJSONText "GrantOperation"
