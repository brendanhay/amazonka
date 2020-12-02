{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Select
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Select where

import Network.AWS.Prelude

data Select
  = AllAttributes
  | AllProjectedAttributes
  | Count
  | SpecificAttributes
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

instance FromText Select where
  parser =
    takeLowerText >>= \case
      "all_attributes" -> pure AllAttributes
      "all_projected_attributes" -> pure AllProjectedAttributes
      "count" -> pure Count
      "specific_attributes" -> pure SpecificAttributes
      e ->
        fromTextError $
          "Failure parsing Select from value: '" <> e
            <> "'. Accepted values: all_attributes, all_projected_attributes, count, specific_attributes"

instance ToText Select where
  toText = \case
    AllAttributes -> "ALL_ATTRIBUTES"
    AllProjectedAttributes -> "ALL_PROJECTED_ATTRIBUTES"
    Count -> "COUNT"
    SpecificAttributes -> "SPECIFIC_ATTRIBUTES"

instance Hashable Select

instance NFData Select

instance ToByteString Select

instance ToQuery Select

instance ToHeader Select

instance ToJSON Select where
  toJSON = toJSONText
