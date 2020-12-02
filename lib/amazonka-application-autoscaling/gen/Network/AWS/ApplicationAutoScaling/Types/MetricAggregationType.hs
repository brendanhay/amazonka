{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.MetricAggregationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.MetricAggregationType where

import Network.AWS.Prelude

data MetricAggregationType
  = MATAverage
  | MATMaximum
  | MATMinimum
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

instance FromText MetricAggregationType where
  parser =
    takeLowerText >>= \case
      "average" -> pure MATAverage
      "maximum" -> pure MATMaximum
      "minimum" -> pure MATMinimum
      e ->
        fromTextError $
          "Failure parsing MetricAggregationType from value: '" <> e
            <> "'. Accepted values: average, maximum, minimum"

instance ToText MetricAggregationType where
  toText = \case
    MATAverage -> "Average"
    MATMaximum -> "Maximum"
    MATMinimum -> "Minimum"

instance Hashable MetricAggregationType

instance NFData MetricAggregationType

instance ToByteString MetricAggregationType

instance ToQuery MetricAggregationType

instance ToHeader MetricAggregationType

instance ToJSON MetricAggregationType where
  toJSON = toJSONText

instance FromJSON MetricAggregationType where
  parseJSON = parseJSONText "MetricAggregationType"
