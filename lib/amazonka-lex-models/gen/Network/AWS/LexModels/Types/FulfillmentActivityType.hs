{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.FulfillmentActivityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.FulfillmentActivityType where

import Network.AWS.Prelude

data FulfillmentActivityType
  = CodeHook
  | ReturnIntent
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

instance FromText FulfillmentActivityType where
  parser =
    takeLowerText >>= \case
      "codehook" -> pure CodeHook
      "returnintent" -> pure ReturnIntent
      e ->
        fromTextError $
          "Failure parsing FulfillmentActivityType from value: '" <> e
            <> "'. Accepted values: codehook, returnintent"

instance ToText FulfillmentActivityType where
  toText = \case
    CodeHook -> "CodeHook"
    ReturnIntent -> "ReturnIntent"

instance Hashable FulfillmentActivityType

instance NFData FulfillmentActivityType

instance ToByteString FulfillmentActivityType

instance ToQuery FulfillmentActivityType

instance ToHeader FulfillmentActivityType

instance ToJSON FulfillmentActivityType where
  toJSON = toJSONText

instance FromJSON FulfillmentActivityType where
  parseJSON = parseJSONText "FulfillmentActivityType"
