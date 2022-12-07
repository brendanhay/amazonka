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
-- Module      : Amazonka.AutoScalingPlans.Types.ScalableDimension
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Types.ScalableDimension
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScalableDimension = ScalableDimension'
  { fromScalableDimension ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
