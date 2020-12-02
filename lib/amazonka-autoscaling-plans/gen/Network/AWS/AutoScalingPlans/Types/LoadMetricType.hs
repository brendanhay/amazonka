{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.LoadMetricType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.LoadMetricType where

import Network.AWS.Prelude

data LoadMetricType
  = ALBTargetGroupRequestCount
  | ASGTotalCPUUtilization
  | ASGTotalNetworkIn
  | ASGTotalNetworkOut
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

instance FromText LoadMetricType where
  parser =
    takeLowerText >>= \case
      "albtargetgrouprequestcount" -> pure ALBTargetGroupRequestCount
      "asgtotalcpuutilization" -> pure ASGTotalCPUUtilization
      "asgtotalnetworkin" -> pure ASGTotalNetworkIn
      "asgtotalnetworkout" -> pure ASGTotalNetworkOut
      e ->
        fromTextError $
          "Failure parsing LoadMetricType from value: '" <> e
            <> "'. Accepted values: albtargetgrouprequestcount, asgtotalcpuutilization, asgtotalnetworkin, asgtotalnetworkout"

instance ToText LoadMetricType where
  toText = \case
    ALBTargetGroupRequestCount -> "ALBTargetGroupRequestCount"
    ASGTotalCPUUtilization -> "ASGTotalCPUUtilization"
    ASGTotalNetworkIn -> "ASGTotalNetworkIn"
    ASGTotalNetworkOut -> "ASGTotalNetworkOut"

instance Hashable LoadMetricType

instance NFData LoadMetricType

instance ToByteString LoadMetricType

instance ToQuery LoadMetricType

instance ToHeader LoadMetricType

instance ToJSON LoadMetricType where
  toJSON = toJSONText

instance FromJSON LoadMetricType where
  parseJSON = parseJSONText "LoadMetricType"
