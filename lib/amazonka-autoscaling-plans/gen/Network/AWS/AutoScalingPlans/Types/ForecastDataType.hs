{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ForecastDataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ForecastDataType where

import Network.AWS.Prelude

data ForecastDataType
  = CapacityForecast
  | LoadForecast
  | ScheduledActionMaxCapacity
  | ScheduledActionMinCapacity
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

instance FromText ForecastDataType where
  parser =
    takeLowerText >>= \case
      "capacityforecast" -> pure CapacityForecast
      "loadforecast" -> pure LoadForecast
      "scheduledactionmaxcapacity" -> pure ScheduledActionMaxCapacity
      "scheduledactionmincapacity" -> pure ScheduledActionMinCapacity
      e ->
        fromTextError $
          "Failure parsing ForecastDataType from value: '" <> e
            <> "'. Accepted values: capacityforecast, loadforecast, scheduledactionmaxcapacity, scheduledactionmincapacity"

instance ToText ForecastDataType where
  toText = \case
    CapacityForecast -> "CapacityForecast"
    LoadForecast -> "LoadForecast"
    ScheduledActionMaxCapacity -> "ScheduledActionMaxCapacity"
    ScheduledActionMinCapacity -> "ScheduledActionMinCapacity"

instance Hashable ForecastDataType

instance NFData ForecastDataType

instance ToByteString ForecastDataType

instance ToQuery ForecastDataType

instance ToHeader ForecastDataType

instance ToJSON ForecastDataType where
  toJSON = toJSONText
