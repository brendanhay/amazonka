{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingMetricType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingMetricType
  ( ScalingMetricType
      ( ScalingMetricType',
        ScalingMetricTypeASGAverageCPUUtilization,
        ScalingMetricTypeASGAverageNetworkIn,
        ScalingMetricTypeASGAverageNetworkOut,
        ScalingMetricTypeDynamoDBReadCapacityUtilization,
        ScalingMetricTypeDynamoDBWriteCapacityUtilization,
        ScalingMetricTypeECSServiceAverageCPUUtilization,
        ScalingMetricTypeECSServiceAverageMemoryUtilization,
        ScalingMetricTypeALBRequestCountPerTarget,
        ScalingMetricTypeRDSReaderAverageCPUUtilization,
        ScalingMetricTypeRDSReaderAverageDatabaseConnections,
        ScalingMetricTypeEC2SpotFleetRequestAverageCPUUtilization,
        ScalingMetricTypeEC2SpotFleetRequestAverageNetworkIn,
        ScalingMetricTypeEC2SpotFleetRequestAverageNetworkOut,
        fromScalingMetricType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ScalingMetricType = ScalingMetricType'
  { fromScalingMetricType ::
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

pattern ScalingMetricTypeASGAverageCPUUtilization :: ScalingMetricType
pattern ScalingMetricTypeASGAverageCPUUtilization = ScalingMetricType' "ASGAverageCPUUtilization"

pattern ScalingMetricTypeASGAverageNetworkIn :: ScalingMetricType
pattern ScalingMetricTypeASGAverageNetworkIn = ScalingMetricType' "ASGAverageNetworkIn"

pattern ScalingMetricTypeASGAverageNetworkOut :: ScalingMetricType
pattern ScalingMetricTypeASGAverageNetworkOut = ScalingMetricType' "ASGAverageNetworkOut"

pattern ScalingMetricTypeDynamoDBReadCapacityUtilization :: ScalingMetricType
pattern ScalingMetricTypeDynamoDBReadCapacityUtilization = ScalingMetricType' "DynamoDBReadCapacityUtilization"

pattern ScalingMetricTypeDynamoDBWriteCapacityUtilization :: ScalingMetricType
pattern ScalingMetricTypeDynamoDBWriteCapacityUtilization = ScalingMetricType' "DynamoDBWriteCapacityUtilization"

pattern ScalingMetricTypeECSServiceAverageCPUUtilization :: ScalingMetricType
pattern ScalingMetricTypeECSServiceAverageCPUUtilization = ScalingMetricType' "ECSServiceAverageCPUUtilization"

pattern ScalingMetricTypeECSServiceAverageMemoryUtilization :: ScalingMetricType
pattern ScalingMetricTypeECSServiceAverageMemoryUtilization = ScalingMetricType' "ECSServiceAverageMemoryUtilization"

pattern ScalingMetricTypeALBRequestCountPerTarget :: ScalingMetricType
pattern ScalingMetricTypeALBRequestCountPerTarget = ScalingMetricType' "ALBRequestCountPerTarget"

pattern ScalingMetricTypeRDSReaderAverageCPUUtilization :: ScalingMetricType
pattern ScalingMetricTypeRDSReaderAverageCPUUtilization = ScalingMetricType' "RDSReaderAverageCPUUtilization"

pattern ScalingMetricTypeRDSReaderAverageDatabaseConnections :: ScalingMetricType
pattern ScalingMetricTypeRDSReaderAverageDatabaseConnections = ScalingMetricType' "RDSReaderAverageDatabaseConnections"

pattern ScalingMetricTypeEC2SpotFleetRequestAverageCPUUtilization :: ScalingMetricType
pattern ScalingMetricTypeEC2SpotFleetRequestAverageCPUUtilization = ScalingMetricType' "EC2SpotFleetRequestAverageCPUUtilization"

pattern ScalingMetricTypeEC2SpotFleetRequestAverageNetworkIn :: ScalingMetricType
pattern ScalingMetricTypeEC2SpotFleetRequestAverageNetworkIn = ScalingMetricType' "EC2SpotFleetRequestAverageNetworkIn"

pattern ScalingMetricTypeEC2SpotFleetRequestAverageNetworkOut :: ScalingMetricType
pattern ScalingMetricTypeEC2SpotFleetRequestAverageNetworkOut = ScalingMetricType' "EC2SpotFleetRequestAverageNetworkOut"

{-# COMPLETE
  ScalingMetricTypeASGAverageCPUUtilization,
  ScalingMetricTypeASGAverageNetworkIn,
  ScalingMetricTypeASGAverageNetworkOut,
  ScalingMetricTypeDynamoDBReadCapacityUtilization,
  ScalingMetricTypeDynamoDBWriteCapacityUtilization,
  ScalingMetricTypeECSServiceAverageCPUUtilization,
  ScalingMetricTypeECSServiceAverageMemoryUtilization,
  ScalingMetricTypeALBRequestCountPerTarget,
  ScalingMetricTypeRDSReaderAverageCPUUtilization,
  ScalingMetricTypeRDSReaderAverageDatabaseConnections,
  ScalingMetricTypeEC2SpotFleetRequestAverageCPUUtilization,
  ScalingMetricTypeEC2SpotFleetRequestAverageNetworkIn,
  ScalingMetricTypeEC2SpotFleetRequestAverageNetworkOut,
  ScalingMetricType'
  #-}
