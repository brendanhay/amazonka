{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemDataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemDataType where

import Network.AWS.Prelude

data OpsItemDataType
  = SearchableString
  | String
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

instance FromText OpsItemDataType where
  parser =
    takeLowerText >>= \case
      "searchablestring" -> pure SearchableString
      "string" -> pure String
      e ->
        fromTextError $
          "Failure parsing OpsItemDataType from value: '" <> e
            <> "'. Accepted values: searchablestring, string"

instance ToText OpsItemDataType where
  toText = \case
    SearchableString -> "SearchableString"
    String -> "String"

instance Hashable OpsItemDataType

instance NFData OpsItemDataType

instance ToByteString OpsItemDataType

instance ToQuery OpsItemDataType

instance ToHeader OpsItemDataType

instance ToJSON OpsItemDataType where
  toJSON = toJSONText

instance FromJSON OpsItemDataType where
  parseJSON = parseJSONText "OpsItemDataType"
