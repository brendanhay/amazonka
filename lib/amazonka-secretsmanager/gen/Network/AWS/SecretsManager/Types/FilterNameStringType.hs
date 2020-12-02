{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.FilterNameStringType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.FilterNameStringType where

import Network.AWS.Prelude

data FilterNameStringType
  = All
  | Description
  | Name
  | TagKey
  | TagValue
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

instance FromText FilterNameStringType where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "description" -> pure Description
      "name" -> pure Name
      "tag-key" -> pure TagKey
      "tag-value" -> pure TagValue
      e ->
        fromTextError $
          "Failure parsing FilterNameStringType from value: '" <> e
            <> "'. Accepted values: all, description, name, tag-key, tag-value"

instance ToText FilterNameStringType where
  toText = \case
    All -> "all"
    Description -> "description"
    Name -> "name"
    TagKey -> "tag-key"
    TagValue -> "tag-value"

instance Hashable FilterNameStringType

instance NFData FilterNameStringType

instance ToByteString FilterNameStringType

instance ToQuery FilterNameStringType

instance ToHeader FilterNameStringType

instance ToJSON FilterNameStringType where
  toJSON = toJSONText
