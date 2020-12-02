{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.SubscriptionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.SubscriptionState where

import Network.AWS.Prelude

data SubscriptionState
  = Active
  | Inactive
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

instance FromText SubscriptionState where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "inactive" -> pure Inactive
      e ->
        fromTextError $
          "Failure parsing SubscriptionState from value: '" <> e
            <> "'. Accepted values: active, inactive"

instance ToText SubscriptionState where
  toText = \case
    Active -> "ACTIVE"
    Inactive -> "INACTIVE"

instance Hashable SubscriptionState

instance NFData SubscriptionState

instance ToByteString SubscriptionState

instance ToQuery SubscriptionState

instance ToHeader SubscriptionState

instance FromJSON SubscriptionState where
  parseJSON = parseJSONText "SubscriptionState"
