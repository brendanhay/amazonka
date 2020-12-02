{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.FulfillmentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.FulfillmentState where

import Network.AWS.Prelude

data FulfillmentState
  = FSFailed
  | FSFulfilled
  | FSReadyForFulfillment
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

instance FromText FulfillmentState where
  parser =
    takeLowerText >>= \case
      "failed" -> pure FSFailed
      "fulfilled" -> pure FSFulfilled
      "readyforfulfillment" -> pure FSReadyForFulfillment
      e ->
        fromTextError $
          "Failure parsing FulfillmentState from value: '" <> e
            <> "'. Accepted values: failed, fulfilled, readyforfulfillment"

instance ToText FulfillmentState where
  toText = \case
    FSFailed -> "Failed"
    FSFulfilled -> "Fulfilled"
    FSReadyForFulfillment -> "ReadyForFulfillment"

instance Hashable FulfillmentState

instance NFData FulfillmentState

instance ToByteString FulfillmentState

instance ToQuery FulfillmentState

instance ToHeader FulfillmentState

instance ToJSON FulfillmentState where
  toJSON = toJSONText

instance FromJSON FulfillmentState where
  parseJSON = parseJSONText "FulfillmentState"
