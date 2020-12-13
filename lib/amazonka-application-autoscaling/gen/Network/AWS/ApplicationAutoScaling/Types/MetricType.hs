{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.MetricType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.MetricType
  ( MetricType
      ( MetricType',
        DynamoDBReadCapacityUtilization,
        DynamoDBWriteCapacityUtilization,
        ALBRequestCountPerTarget,
        RDSReaderAverageCPUUtilization,
        RDSReaderAverageDatabaseConnections,
        EC2SpotFleetRequestAverageCPUUtilization,
        EC2SpotFleetRequestAverageNetworkIn,
        EC2SpotFleetRequestAverageNetworkOut,
        SageMakerVariantInvocationsPerInstance,
        ECSServiceAverageCPUUtilization,
        ECSServiceAverageMemoryUtilization,
        AppStreamAverageCapacityUtilization,
        ComprehendInferenceUtilization,
        LambdaProvisionedConcurrencyUtilization,
        CassandraReadCapacityUtilization,
        CassandraWriteCapacityUtilization,
        KafkaBrokerStorageUtilization
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MetricType = MetricType' Lude.Text
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

pattern DynamoDBReadCapacityUtilization :: MetricType
pattern DynamoDBReadCapacityUtilization = MetricType' "DynamoDBReadCapacityUtilization"

pattern DynamoDBWriteCapacityUtilization :: MetricType
pattern DynamoDBWriteCapacityUtilization = MetricType' "DynamoDBWriteCapacityUtilization"

pattern ALBRequestCountPerTarget :: MetricType
pattern ALBRequestCountPerTarget = MetricType' "ALBRequestCountPerTarget"

pattern RDSReaderAverageCPUUtilization :: MetricType
pattern RDSReaderAverageCPUUtilization = MetricType' "RDSReaderAverageCPUUtilization"

pattern RDSReaderAverageDatabaseConnections :: MetricType
pattern RDSReaderAverageDatabaseConnections = MetricType' "RDSReaderAverageDatabaseConnections"

pattern EC2SpotFleetRequestAverageCPUUtilization :: MetricType
pattern EC2SpotFleetRequestAverageCPUUtilization = MetricType' "EC2SpotFleetRequestAverageCPUUtilization"

pattern EC2SpotFleetRequestAverageNetworkIn :: MetricType
pattern EC2SpotFleetRequestAverageNetworkIn = MetricType' "EC2SpotFleetRequestAverageNetworkIn"

pattern EC2SpotFleetRequestAverageNetworkOut :: MetricType
pattern EC2SpotFleetRequestAverageNetworkOut = MetricType' "EC2SpotFleetRequestAverageNetworkOut"

pattern SageMakerVariantInvocationsPerInstance :: MetricType
pattern SageMakerVariantInvocationsPerInstance = MetricType' "SageMakerVariantInvocationsPerInstance"

pattern ECSServiceAverageCPUUtilization :: MetricType
pattern ECSServiceAverageCPUUtilization = MetricType' "ECSServiceAverageCPUUtilization"

pattern ECSServiceAverageMemoryUtilization :: MetricType
pattern ECSServiceAverageMemoryUtilization = MetricType' "ECSServiceAverageMemoryUtilization"

pattern AppStreamAverageCapacityUtilization :: MetricType
pattern AppStreamAverageCapacityUtilization = MetricType' "AppStreamAverageCapacityUtilization"

pattern ComprehendInferenceUtilization :: MetricType
pattern ComprehendInferenceUtilization = MetricType' "ComprehendInferenceUtilization"

pattern LambdaProvisionedConcurrencyUtilization :: MetricType
pattern LambdaProvisionedConcurrencyUtilization = MetricType' "LambdaProvisionedConcurrencyUtilization"

pattern CassandraReadCapacityUtilization :: MetricType
pattern CassandraReadCapacityUtilization = MetricType' "CassandraReadCapacityUtilization"

pattern CassandraWriteCapacityUtilization :: MetricType
pattern CassandraWriteCapacityUtilization = MetricType' "CassandraWriteCapacityUtilization"

pattern KafkaBrokerStorageUtilization :: MetricType
pattern KafkaBrokerStorageUtilization = MetricType' "KafkaBrokerStorageUtilization"

{-# COMPLETE
  DynamoDBReadCapacityUtilization,
  DynamoDBWriteCapacityUtilization,
  ALBRequestCountPerTarget,
  RDSReaderAverageCPUUtilization,
  RDSReaderAverageDatabaseConnections,
  EC2SpotFleetRequestAverageCPUUtilization,
  EC2SpotFleetRequestAverageNetworkIn,
  EC2SpotFleetRequestAverageNetworkOut,
  SageMakerVariantInvocationsPerInstance,
  ECSServiceAverageCPUUtilization,
  ECSServiceAverageMemoryUtilization,
  AppStreamAverageCapacityUtilization,
  ComprehendInferenceUtilization,
  LambdaProvisionedConcurrencyUtilization,
  CassandraReadCapacityUtilization,
  CassandraWriteCapacityUtilization,
  KafkaBrokerStorageUtilization,
  MetricType'
  #-}
