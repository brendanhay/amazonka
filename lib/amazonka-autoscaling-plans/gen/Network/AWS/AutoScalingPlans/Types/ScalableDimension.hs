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
        AutoscalingAutoScalingGroupDesiredCapacity,
        EcsServiceDesiredCount,
        EC2SpotFleetRequestTargetCapacity,
        RDSClusterReadReplicaCount,
        DynamodbTableReadCapacityUnits,
        DynamodbTableWriteCapacityUnits,
        DynamodbIndexReadCapacityUnits,
        DynamodbIndexWriteCapacityUnits
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ScalableDimension = ScalableDimension' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AutoscalingAutoScalingGroupDesiredCapacity :: ScalableDimension
pattern AutoscalingAutoScalingGroupDesiredCapacity = ScalableDimension' "autoscaling:autoScalingGroup:DesiredCapacity"

pattern EcsServiceDesiredCount :: ScalableDimension
pattern EcsServiceDesiredCount = ScalableDimension' "ecs:service:DesiredCount"

pattern EC2SpotFleetRequestTargetCapacity :: ScalableDimension
pattern EC2SpotFleetRequestTargetCapacity = ScalableDimension' "ec2:spot-fleet-request:TargetCapacity"

pattern RDSClusterReadReplicaCount :: ScalableDimension
pattern RDSClusterReadReplicaCount = ScalableDimension' "rds:cluster:ReadReplicaCount"

pattern DynamodbTableReadCapacityUnits :: ScalableDimension
pattern DynamodbTableReadCapacityUnits = ScalableDimension' "dynamodb:table:ReadCapacityUnits"

pattern DynamodbTableWriteCapacityUnits :: ScalableDimension
pattern DynamodbTableWriteCapacityUnits = ScalableDimension' "dynamodb:table:WriteCapacityUnits"

pattern DynamodbIndexReadCapacityUnits :: ScalableDimension
pattern DynamodbIndexReadCapacityUnits = ScalableDimension' "dynamodb:index:ReadCapacityUnits"

pattern DynamodbIndexWriteCapacityUnits :: ScalableDimension
pattern DynamodbIndexWriteCapacityUnits = ScalableDimension' "dynamodb:index:WriteCapacityUnits"

{-# COMPLETE
  AutoscalingAutoScalingGroupDesiredCapacity,
  EcsServiceDesiredCount,
  EC2SpotFleetRequestTargetCapacity,
  RDSClusterReadReplicaCount,
  DynamodbTableReadCapacityUnits,
  DynamodbTableWriteCapacityUnits,
  DynamodbIndexReadCapacityUnits,
  DynamodbIndexWriteCapacityUnits,
  ScalableDimension'
  #-}
