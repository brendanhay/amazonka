{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AnomalySubscriptionFrequency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalySubscriptionFrequency where

import Network.AWS.Prelude

data AnomalySubscriptionFrequency
  = ASFDaily
  | ASFImmediate
  | ASFWeekly
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

instance FromText AnomalySubscriptionFrequency where
  parser =
    takeLowerText >>= \case
      "daily" -> pure ASFDaily
      "immediate" -> pure ASFImmediate
      "weekly" -> pure ASFWeekly
      e ->
        fromTextError $
          "Failure parsing AnomalySubscriptionFrequency from value: '" <> e
            <> "'. Accepted values: daily, immediate, weekly"

instance ToText AnomalySubscriptionFrequency where
  toText = \case
    ASFDaily -> "DAILY"
    ASFImmediate -> "IMMEDIATE"
    ASFWeekly -> "WEEKLY"

instance Hashable AnomalySubscriptionFrequency

instance NFData AnomalySubscriptionFrequency

instance ToByteString AnomalySubscriptionFrequency

instance ToQuery AnomalySubscriptionFrequency

instance ToHeader AnomalySubscriptionFrequency

instance ToJSON AnomalySubscriptionFrequency where
  toJSON = toJSONText

instance FromJSON AnomalySubscriptionFrequency where
  parseJSON = parseJSONText "AnomalySubscriptionFrequency"
