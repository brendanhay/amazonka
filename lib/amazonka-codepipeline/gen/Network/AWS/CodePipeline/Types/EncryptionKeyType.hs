{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.EncryptionKeyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.EncryptionKeyType where

import Network.AWS.Prelude

data EncryptionKeyType = KMS
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

instance FromText EncryptionKeyType where
  parser =
    takeLowerText >>= \case
      "kms" -> pure KMS
      e ->
        fromTextError $
          "Failure parsing EncryptionKeyType from value: '" <> e
            <> "'. Accepted values: kms"

instance ToText EncryptionKeyType where
  toText = \case
    KMS -> "KMS"

instance Hashable EncryptionKeyType

instance NFData EncryptionKeyType

instance ToByteString EncryptionKeyType

instance ToQuery EncryptionKeyType

instance ToHeader EncryptionKeyType

instance ToJSON EncryptionKeyType where
  toJSON = toJSONText

instance FromJSON EncryptionKeyType where
  parseJSON = parseJSONText "EncryptionKeyType"
