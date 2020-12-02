{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.EncryptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.EncryptionType where

import Network.AWS.Prelude

data EncryptionType
  = AES256
  | KMS
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

instance FromText EncryptionType where
  parser =
    takeLowerText >>= \case
      "aes256" -> pure AES256
      "kms" -> pure KMS
      e ->
        fromTextError $
          "Failure parsing EncryptionType from value: '" <> e
            <> "'. Accepted values: aes256, kms"

instance ToText EncryptionType where
  toText = \case
    AES256 -> "AES256"
    KMS -> "KMS"

instance Hashable EncryptionType

instance NFData EncryptionType

instance ToByteString EncryptionType

instance ToQuery EncryptionType

instance ToHeader EncryptionType

instance ToJSON EncryptionType where
  toJSON = toJSONText

instance FromJSON EncryptionType where
  parseJSON = parseJSONText "EncryptionType"
