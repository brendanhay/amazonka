{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalableDimension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalableDimension
  ( ScalableDimension
      ( ..,
        ScalableDimension_Autoscaling_autoScalingGroup_DesiredCapacity,
        ScalableDimension_Dynamodb_index_ReadCapacityUnits,
        ScalableDimension_Dynamodb_index_WriteCapacityUnits,
        ScalableDimension_Dynamodb_table_ReadCapacityUnits,
        ScalableDimension_Dynamodb_table_WriteCapacityUnits,
        ScalableDimension_Ec2_spot_fleet_request_TargetCapacity,
        ScalableDimension_Ecs_service_DesiredCount,
        ScalableDimension_Rds_cluster_ReadReplicaCount
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ScalableDimension = ScalableDimension'
  { fromScalableDimension ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ScalableDimension_Autoscaling_autoScalingGroup_DesiredCapacity :: ScalableDimension
pattern ScalableDimension_Autoscaling_autoScalingGroup_DesiredCapacity = ScalableDimension' "autoscaling:autoScalingGroup:DesiredCapacity"

pattern ScalableDimension_Dynamodb_index_ReadCapacityUnits :: ScalableDimension
pattern ScalableDimension_Dynamodb_index_ReadCapacityUnits = ScalableDimension' "dynamodb:index:ReadCapacityUnits"

pattern ScalableDimension_Dynamodb_index_WriteCapacityUnits :: ScalableDimension
pattern ScalableDimension_Dynamodb_index_WriteCapacityUnits = ScalableDimension' "dynamodb:index:WriteCapacityUnits"

pattern ScalableDimension_Dynamodb_table_ReadCapacityUnits :: ScalableDimension
pattern ScalableDimension_Dynamodb_table_ReadCapacityUnits = ScalableDimension' "dynamodb:table:ReadCapacityUnits"

pattern ScalableDimension_Dynamodb_table_WriteCapacityUnits :: ScalableDimension
pattern ScalableDimension_Dynamodb_table_WriteCapacityUnits = ScalableDimension' "dynamodb:table:WriteCapacityUnits"

pattern ScalableDimension_Ec2_spot_fleet_request_TargetCapacity :: ScalableDimension
pattern ScalableDimension_Ec2_spot_fleet_request_TargetCapacity = ScalableDimension' "ec2:spot-fleet-request:TargetCapacity"

pattern ScalableDimension_Ecs_service_DesiredCount :: ScalableDimension
pattern ScalableDimension_Ecs_service_DesiredCount = ScalableDimension' "ecs:service:DesiredCount"

pattern ScalableDimension_Rds_cluster_ReadReplicaCount :: ScalableDimension
pattern ScalableDimension_Rds_cluster_ReadReplicaCount = ScalableDimension' "rds:cluster:ReadReplicaCount"

{-# COMPLETE
  ScalableDimension_Autoscaling_autoScalingGroup_DesiredCapacity,
  ScalableDimension_Dynamodb_index_ReadCapacityUnits,
  ScalableDimension_Dynamodb_index_WriteCapacityUnits,
  ScalableDimension_Dynamodb_table_ReadCapacityUnits,
  ScalableDimension_Dynamodb_table_WriteCapacityUnits,
  ScalableDimension_Ec2_spot_fleet_request_TargetCapacity,
  ScalableDimension_Ecs_service_DesiredCount,
  ScalableDimension_Rds_cluster_ReadReplicaCount,
  ScalableDimension'
  #-}
