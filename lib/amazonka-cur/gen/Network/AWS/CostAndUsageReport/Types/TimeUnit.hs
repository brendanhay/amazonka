{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.TimeUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.TimeUnit where

import Network.AWS.Prelude

-- | The length of time covered by the report.
data TimeUnit
  = Daily
  | Hourly
  | Monthly
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

instance FromText TimeUnit where
  parser =
    takeLowerText >>= \case
      "daily" -> pure Daily
      "hourly" -> pure Hourly
      "monthly" -> pure Monthly
      e ->
        fromTextError $
          "Failure parsing TimeUnit from value: '" <> e
            <> "'. Accepted values: daily, hourly, monthly"

instance ToText TimeUnit where
  toText = \case
    Daily -> "DAILY"
    Hourly -> "HOURLY"
    Monthly -> "MONTHLY"

instance Hashable TimeUnit

instance NFData TimeUnit

instance ToByteString TimeUnit

instance ToQuery TimeUnit

instance ToHeader TimeUnit

instance ToJSON TimeUnit where
  toJSON = toJSONText

instance FromJSON TimeUnit where
  parseJSON = parseJSONText "TimeUnit"
