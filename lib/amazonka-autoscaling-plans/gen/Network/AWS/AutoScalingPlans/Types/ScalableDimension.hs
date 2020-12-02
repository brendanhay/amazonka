{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalableDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalableDimension where

import Network.AWS.Prelude

data ScalableDimension
  = AutoscalingAutoScalingGroupDesiredCapacity
  | DynamodbIndexReadCapacityUnits
  | DynamodbIndexWriteCapacityUnits
  | DynamodbTableReadCapacityUnits
  | DynamodbTableWriteCapacityUnits
  | EC2SpotFleetRequestTargetCapacity
  | EcsServiceDesiredCount
  | RDSClusterReadReplicaCount
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

instance FromText ScalableDimension where
  parser =
    takeLowerText >>= \case
      "autoscaling:autoscalinggroup:desiredcapacity" -> pure AutoscalingAutoScalingGroupDesiredCapacity
      "dynamodb:index:readcapacityunits" -> pure DynamodbIndexReadCapacityUnits
      "dynamodb:index:writecapacityunits" -> pure DynamodbIndexWriteCapacityUnits
      "dynamodb:table:readcapacityunits" -> pure DynamodbTableReadCapacityUnits
      "dynamodb:table:writecapacityunits" -> pure DynamodbTableWriteCapacityUnits
      "ec2:spot-fleet-request:targetcapacity" -> pure EC2SpotFleetRequestTargetCapacity
      "ecs:service:desiredcount" -> pure EcsServiceDesiredCount
      "rds:cluster:readreplicacount" -> pure RDSClusterReadReplicaCount
      e ->
        fromTextError $
          "Failure parsing ScalableDimension from value: '" <> e
            <> "'. Accepted values: autoscaling:autoscalinggroup:desiredcapacity, dynamodb:index:readcapacityunits, dynamodb:index:writecapacityunits, dynamodb:table:readcapacityunits, dynamodb:table:writecapacityunits, ec2:spot-fleet-request:targetcapacity, ecs:service:desiredcount, rds:cluster:readreplicacount"

instance ToText ScalableDimension where
  toText = \case
    AutoscalingAutoScalingGroupDesiredCapacity -> "autoscaling:autoScalingGroup:DesiredCapacity"
    DynamodbIndexReadCapacityUnits -> "dynamodb:index:ReadCapacityUnits"
    DynamodbIndexWriteCapacityUnits -> "dynamodb:index:WriteCapacityUnits"
    DynamodbTableReadCapacityUnits -> "dynamodb:table:ReadCapacityUnits"
    DynamodbTableWriteCapacityUnits -> "dynamodb:table:WriteCapacityUnits"
    EC2SpotFleetRequestTargetCapacity -> "ec2:spot-fleet-request:TargetCapacity"
    EcsServiceDesiredCount -> "ecs:service:DesiredCount"
    RDSClusterReadReplicaCount -> "rds:cluster:ReadReplicaCount"

instance Hashable ScalableDimension

instance NFData ScalableDimension

instance ToByteString ScalableDimension

instance ToQuery ScalableDimension

instance ToHeader ScalableDimension

instance ToJSON ScalableDimension where
  toJSON = toJSONText

instance FromJSON ScalableDimension where
  parseJSON = parseJSONText "ScalableDimension"
