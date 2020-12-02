{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.FindingPublishingFrequency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.FindingPublishingFrequency where

import Network.AWS.Prelude

data FindingPublishingFrequency
  = FifteenMinutes
  | OneHour
  | SixHours
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

instance FromText FindingPublishingFrequency where
  parser =
    takeLowerText >>= \case
      "fifteen_minutes" -> pure FifteenMinutes
      "one_hour" -> pure OneHour
      "six_hours" -> pure SixHours
      e ->
        fromTextError $
          "Failure parsing FindingPublishingFrequency from value: '" <> e
            <> "'. Accepted values: fifteen_minutes, one_hour, six_hours"

instance ToText FindingPublishingFrequency where
  toText = \case
    FifteenMinutes -> "FIFTEEN_MINUTES"
    OneHour -> "ONE_HOUR"
    SixHours -> "SIX_HOURS"

instance Hashable FindingPublishingFrequency

instance NFData FindingPublishingFrequency

instance ToByteString FindingPublishingFrequency

instance ToQuery FindingPublishingFrequency

instance ToHeader FindingPublishingFrequency

instance ToJSON FindingPublishingFrequency where
  toJSON = toJSONText

instance FromJSON FindingPublishingFrequency where
  parseJSON = parseJSONText "FindingPublishingFrequency"
