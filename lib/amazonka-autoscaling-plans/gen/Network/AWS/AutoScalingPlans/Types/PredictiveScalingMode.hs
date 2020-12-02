{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.PredictiveScalingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.PredictiveScalingMode where

import Network.AWS.Prelude

data PredictiveScalingMode
  = ForecastAndScale
  | ForecastOnly
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

instance FromText PredictiveScalingMode where
  parser =
    takeLowerText >>= \case
      "forecastandscale" -> pure ForecastAndScale
      "forecastonly" -> pure ForecastOnly
      e ->
        fromTextError $
          "Failure parsing PredictiveScalingMode from value: '" <> e
            <> "'. Accepted values: forecastandscale, forecastonly"

instance ToText PredictiveScalingMode where
  toText = \case
    ForecastAndScale -> "ForecastAndScale"
    ForecastOnly -> "ForecastOnly"

instance Hashable PredictiveScalingMode

instance NFData PredictiveScalingMode

instance ToByteString PredictiveScalingMode

instance ToQuery PredictiveScalingMode

instance ToHeader PredictiveScalingMode

instance ToJSON PredictiveScalingMode where
  toJSON = toJSONText

instance FromJSON PredictiveScalingMode where
  parseJSON = parseJSONText "PredictiveScalingMode"
