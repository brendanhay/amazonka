{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SubscriberStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SubscriberStatus where

import Network.AWS.Prelude

data SubscriberStatus
  = Confirmed
  | Declined
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

instance FromText SubscriberStatus where
  parser =
    takeLowerText >>= \case
      "confirmed" -> pure Confirmed
      "declined" -> pure Declined
      e ->
        fromTextError $
          "Failure parsing SubscriberStatus from value: '" <> e
            <> "'. Accepted values: confirmed, declined"

instance ToText SubscriberStatus where
  toText = \case
    Confirmed -> "CONFIRMED"
    Declined -> "DECLINED"

instance Hashable SubscriberStatus

instance NFData SubscriberStatus

instance ToByteString SubscriberStatus

instance ToQuery SubscriberStatus

instance ToHeader SubscriberStatus

instance ToJSON SubscriberStatus where
  toJSON = toJSONText

instance FromJSON SubscriberStatus where
  parseJSON = parseJSONText "SubscriberStatus"
