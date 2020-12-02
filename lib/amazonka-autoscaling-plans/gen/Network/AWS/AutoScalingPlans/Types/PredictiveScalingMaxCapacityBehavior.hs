{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.PredictiveScalingMaxCapacityBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.PredictiveScalingMaxCapacityBehavior where

import Network.AWS.Prelude

data PredictiveScalingMaxCapacityBehavior
  = SetForecastCapacityToMaxCapacity
  | SetMaxCapacityAboveForecastCapacity
  | SetMaxCapacityToForecastCapacity
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

instance FromText PredictiveScalingMaxCapacityBehavior where
  parser =
    takeLowerText >>= \case
      "setforecastcapacitytomaxcapacity" -> pure SetForecastCapacityToMaxCapacity
      "setmaxcapacityaboveforecastcapacity" -> pure SetMaxCapacityAboveForecastCapacity
      "setmaxcapacitytoforecastcapacity" -> pure SetMaxCapacityToForecastCapacity
      e ->
        fromTextError $
          "Failure parsing PredictiveScalingMaxCapacityBehavior from value: '" <> e
            <> "'. Accepted values: setforecastcapacitytomaxcapacity, setmaxcapacityaboveforecastcapacity, setmaxcapacitytoforecastcapacity"

instance ToText PredictiveScalingMaxCapacityBehavior where
  toText = \case
    SetForecastCapacityToMaxCapacity -> "SetForecastCapacityToMaxCapacity"
    SetMaxCapacityAboveForecastCapacity -> "SetMaxCapacityAboveForecastCapacity"
    SetMaxCapacityToForecastCapacity -> "SetMaxCapacityToForecastCapacity"

instance Hashable PredictiveScalingMaxCapacityBehavior

instance NFData PredictiveScalingMaxCapacityBehavior

instance ToByteString PredictiveScalingMaxCapacityBehavior

instance ToQuery PredictiveScalingMaxCapacityBehavior

instance ToHeader PredictiveScalingMaxCapacityBehavior

instance ToJSON PredictiveScalingMaxCapacityBehavior where
  toJSON = toJSONText

instance FromJSON PredictiveScalingMaxCapacityBehavior where
  parseJSON = parseJSONText "PredictiveScalingMaxCapacityBehavior"
