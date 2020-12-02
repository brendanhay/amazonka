{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.MetricType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.MetricType where

import Network.AWS.Prelude

data MetricType
  = ALBRequestCountPerTarget
  | ASGAverageCPUUtilization
  | ASGAverageNetworkIn
  | ASGAverageNetworkOut
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

instance FromText MetricType where
  parser =
    takeLowerText >>= \case
      "albrequestcountpertarget" -> pure ALBRequestCountPerTarget
      "asgaveragecpuutilization" -> pure ASGAverageCPUUtilization
      "asgaveragenetworkin" -> pure ASGAverageNetworkIn
      "asgaveragenetworkout" -> pure ASGAverageNetworkOut
      e ->
        fromTextError $
          "Failure parsing MetricType from value: '" <> e
            <> "'. Accepted values: albrequestcountpertarget, asgaveragecpuutilization, asgaveragenetworkin, asgaveragenetworkout"

instance ToText MetricType where
  toText = \case
    ALBRequestCountPerTarget -> "ALBRequestCountPerTarget"
    ASGAverageCPUUtilization -> "ASGAverageCPUUtilization"
    ASGAverageNetworkIn -> "ASGAverageNetworkIn"
    ASGAverageNetworkOut -> "ASGAverageNetworkOut"

instance Hashable MetricType

instance NFData MetricType

instance ToByteString MetricType

instance ToQuery MetricType

instance ToHeader MetricType

instance FromXML MetricType where
  parseXML = parseXMLText "MetricType"
