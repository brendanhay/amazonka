{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventAggregateField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventAggregateField where

import Network.AWS.Prelude

data EventAggregateField = EventTypeCategory
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

instance FromText EventAggregateField where
  parser =
    takeLowerText >>= \case
      "eventtypecategory" -> pure EventTypeCategory
      e ->
        fromTextError $
          "Failure parsing EventAggregateField from value: '" <> e
            <> "'. Accepted values: eventtypecategory"

instance ToText EventAggregateField where
  toText = \case
    EventTypeCategory -> "eventTypeCategory"

instance Hashable EventAggregateField

instance NFData EventAggregateField

instance ToByteString EventAggregateField

instance ToQuery EventAggregateField

instance ToHeader EventAggregateField

instance ToJSON EventAggregateField where
  toJSON = toJSONText
