{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityType where

import Network.AWS.Prelude

data EntityType
  = CommercialItem
  | Date
  | Event
  | Location
  | Organization
  | Other
  | Person
  | Quantity
  | Title
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

instance FromText EntityType where
  parser =
    takeLowerText >>= \case
      "commercial_item" -> pure CommercialItem
      "date" -> pure Date
      "event" -> pure Event
      "location" -> pure Location
      "organization" -> pure Organization
      "other" -> pure Other
      "person" -> pure Person
      "quantity" -> pure Quantity
      "title" -> pure Title
      e ->
        fromTextError $
          "Failure parsing EntityType from value: '" <> e
            <> "'. Accepted values: commercial_item, date, event, location, organization, other, person, quantity, title"

instance ToText EntityType where
  toText = \case
    CommercialItem -> "COMMERCIAL_ITEM"
    Date -> "DATE"
    Event -> "EVENT"
    Location -> "LOCATION"
    Organization -> "ORGANIZATION"
    Other -> "OTHER"
    Person -> "PERSON"
    Quantity -> "QUANTITY"
    Title -> "TITLE"

instance Hashable EntityType

instance NFData EntityType

instance ToByteString EntityType

instance ToQuery EntityType

instance ToHeader EntityType

instance FromJSON EntityType where
  parseJSON = parseJSONText "EntityType"
