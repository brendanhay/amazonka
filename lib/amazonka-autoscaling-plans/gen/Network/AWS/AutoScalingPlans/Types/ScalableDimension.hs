{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalableDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalableDimension
  ( ScalableDimension
      ( ScalableDimension',
        ScalableDimensionAutoscalingAutoScalingGroupDesiredCapacity,
        ScalableDimensionEcsServiceDesiredCount,
        ScalableDimensionEC2SpotFleetRequestTargetCapacity,
        ScalableDimensionRdsClusterReadReplicaCount,
        ScalableDimensionDynamodbTableReadCapacityUnits,
        ScalableDimensionDynamodbTableWriteCapacityUnits,
        ScalableDimensionDynamodbIndexReadCapacityUnits,
        ScalableDimensionDynamodbIndexWriteCapacityUnits,
        fromScalableDimension
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ScalableDimension = ScalableDimension'
  { fromScalableDimension ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ScalableDimensionAutoscalingAutoScalingGroupDesiredCapacity :: ScalableDimension
pattern ScalableDimensionAutoscalingAutoScalingGroupDesiredCapacity = ScalableDimension' "autoscaling:autoScalingGroup:DesiredCapacity"

pattern ScalableDimensionEcsServiceDesiredCount :: ScalableDimension
pattern ScalableDimensionEcsServiceDesiredCount = ScalableDimension' "ecs:service:DesiredCount"

pattern ScalableDimensionEC2SpotFleetRequestTargetCapacity :: ScalableDimension
pattern ScalableDimensionEC2SpotFleetRequestTargetCapacity = ScalableDimension' "ec2:spot-fleet-request:TargetCapacity"

pattern ScalableDimensionRdsClusterReadReplicaCount :: ScalableDimension
pattern ScalableDimensionRdsClusterReadReplicaCount = ScalableDimension' "rds:cluster:ReadReplicaCount"

pattern ScalableDimensionDynamodbTableReadCapacityUnits :: ScalableDimension
pattern ScalableDimensionDynamodbTableReadCapacityUnits = ScalableDimension' "dynamodb:table:ReadCapacityUnits"

pattern ScalableDimensionDynamodbTableWriteCapacityUnits :: ScalableDimension
pattern ScalableDimensionDynamodbTableWriteCapacityUnits = ScalableDimension' "dynamodb:table:WriteCapacityUnits"

pattern ScalableDimensionDynamodbIndexReadCapacityUnits :: ScalableDimension
pattern ScalableDimensionDynamodbIndexReadCapacityUnits = ScalableDimension' "dynamodb:index:ReadCapacityUnits"

pattern ScalableDimensionDynamodbIndexWriteCapacityUnits :: ScalableDimension
pattern ScalableDimensionDynamodbIndexWriteCapacityUnits = ScalableDimension' "dynamodb:index:WriteCapacityUnits"

{-# COMPLETE
  ScalableDimensionAutoscalingAutoScalingGroupDesiredCapacity,
  ScalableDimensionEcsServiceDesiredCount,
  ScalableDimensionEC2SpotFleetRequestTargetCapacity,
  ScalableDimensionRdsClusterReadReplicaCount,
  ScalableDimensionDynamodbTableReadCapacityUnits,
  ScalableDimensionDynamodbTableWriteCapacityUnits,
  ScalableDimensionDynamodbIndexReadCapacityUnits,
  ScalableDimensionDynamodbIndexWriteCapacityUnits,
  ScalableDimension'
  #-}
