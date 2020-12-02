{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.EncryptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.EncryptionType where

import Network.AWS.Prelude

data EncryptionType
  = KMS
  | None
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
      "kms" -> pure KMS
      "none" -> pure None
      e ->
        fromTextError $
          "Failure parsing EncryptionType from value: '" <> e
            <> "'. Accepted values: kms, none"

instance ToText EncryptionType where
  toText = \case
    KMS -> "KMS"
    None -> "NONE"

instance Hashable EncryptionType

instance NFData EncryptionType

instance ToByteString EncryptionType

instance ToQuery EncryptionType

instance ToHeader EncryptionType

instance ToJSON EncryptionType where
  toJSON = toJSONText

instance FromJSON EncryptionType where
  parseJSON = parseJSONText "EncryptionType"
