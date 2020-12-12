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
        ALBRequestCountPerTarget,
        ASGAverageCPUUtilization,
        ASGAverageNetworkIn,
        ASGAverageNetworkOut,
        DynamoDBReadCapacityUtilization,
        DynamoDBWriteCapacityUtilization,
        EC2SpotFleetRequestAverageCPUUtilization,
        EC2SpotFleetRequestAverageNetworkIn,
        EC2SpotFleetRequestAverageNetworkOut,
        ECSServiceAverageCPUUtilization,
        ECSServiceAverageMemoryUtilization,
        RDSReaderAverageCPUUtilization,
        RDSReaderAverageDatabaseConnections
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ScalingMetricType = ScalingMetricType' Lude.Text
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

pattern ALBRequestCountPerTarget :: ScalingMetricType
pattern ALBRequestCountPerTarget = ScalingMetricType' "ALBRequestCountPerTarget"

pattern ASGAverageCPUUtilization :: ScalingMetricType
pattern ASGAverageCPUUtilization = ScalingMetricType' "ASGAverageCPUUtilization"

pattern ASGAverageNetworkIn :: ScalingMetricType
pattern ASGAverageNetworkIn = ScalingMetricType' "ASGAverageNetworkIn"

pattern ASGAverageNetworkOut :: ScalingMetricType
pattern ASGAverageNetworkOut = ScalingMetricType' "ASGAverageNetworkOut"

pattern DynamoDBReadCapacityUtilization :: ScalingMetricType
pattern DynamoDBReadCapacityUtilization = ScalingMetricType' "DynamoDBReadCapacityUtilization"

pattern DynamoDBWriteCapacityUtilization :: ScalingMetricType
pattern DynamoDBWriteCapacityUtilization = ScalingMetricType' "DynamoDBWriteCapacityUtilization"

pattern EC2SpotFleetRequestAverageCPUUtilization :: ScalingMetricType
pattern EC2SpotFleetRequestAverageCPUUtilization = ScalingMetricType' "EC2SpotFleetRequestAverageCPUUtilization"

pattern EC2SpotFleetRequestAverageNetworkIn :: ScalingMetricType
pattern EC2SpotFleetRequestAverageNetworkIn = ScalingMetricType' "EC2SpotFleetRequestAverageNetworkIn"

pattern EC2SpotFleetRequestAverageNetworkOut :: ScalingMetricType
pattern EC2SpotFleetRequestAverageNetworkOut = ScalingMetricType' "EC2SpotFleetRequestAverageNetworkOut"

pattern ECSServiceAverageCPUUtilization :: ScalingMetricType
pattern ECSServiceAverageCPUUtilization = ScalingMetricType' "ECSServiceAverageCPUUtilization"

pattern ECSServiceAverageMemoryUtilization :: ScalingMetricType
pattern ECSServiceAverageMemoryUtilization = ScalingMetricType' "ECSServiceAverageMemoryUtilization"

pattern RDSReaderAverageCPUUtilization :: ScalingMetricType
pattern RDSReaderAverageCPUUtilization = ScalingMetricType' "RDSReaderAverageCPUUtilization"

pattern RDSReaderAverageDatabaseConnections :: ScalingMetricType
pattern RDSReaderAverageDatabaseConnections = ScalingMetricType' "RDSReaderAverageDatabaseConnections"

{-# COMPLETE
  ALBRequestCountPerTarget,
  ASGAverageCPUUtilization,
  ASGAverageNetworkIn,
  ASGAverageNetworkOut,
  DynamoDBReadCapacityUtilization,
  DynamoDBWriteCapacityUtilization,
  EC2SpotFleetRequestAverageCPUUtilization,
  EC2SpotFleetRequestAverageNetworkIn,
  EC2SpotFleetRequestAverageNetworkOut,
  ECSServiceAverageCPUUtilization,
  ECSServiceAverageMemoryUtilization,
  RDSReaderAverageCPUUtilization,
  RDSReaderAverageDatabaseConnections,
  ScalingMetricType'
  #-}
