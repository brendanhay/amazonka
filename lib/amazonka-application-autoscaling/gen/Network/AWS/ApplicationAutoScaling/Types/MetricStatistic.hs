{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.MetricStatistic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.MetricStatistic where

import Network.AWS.Prelude

data MetricStatistic
  = Average
  | Maximum
  | Minimum
  | SampleCount
  | Sum
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

instance FromText MetricStatistic where
  parser =
    takeLowerText >>= \case
      "average" -> pure Average
      "maximum" -> pure Maximum
      "minimum" -> pure Minimum
      "samplecount" -> pure SampleCount
      "sum" -> pure Sum
      e ->
        fromTextError $
          "Failure parsing MetricStatistic from value: '" <> e
            <> "'. Accepted values: average, maximum, minimum, samplecount, sum"

instance ToText MetricStatistic where
  toText = \case
    Average -> "Average"
    Maximum -> "Maximum"
    Minimum -> "Minimum"
    SampleCount -> "SampleCount"
    Sum -> "Sum"

instance Hashable MetricStatistic

instance NFData MetricStatistic

instance ToByteString MetricStatistic

instance ToQuery MetricStatistic

instance ToHeader MetricStatistic

instance ToJSON MetricStatistic where
  toJSON = toJSONText

instance FromJSON MetricStatistic where
  parseJSON = parseJSONText "MetricStatistic"
