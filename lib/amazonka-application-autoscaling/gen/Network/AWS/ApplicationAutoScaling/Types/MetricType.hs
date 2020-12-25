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
        MetricTypeDynamoDBReadCapacityUtilization,
        MetricTypeDynamoDBWriteCapacityUtilization,
        MetricTypeALBRequestCountPerTarget,
        MetricTypeRDSReaderAverageCPUUtilization,
        MetricTypeRDSReaderAverageDatabaseConnections,
        MetricTypeEC2SpotFleetRequestAverageCPUUtilization,
        MetricTypeEC2SpotFleetRequestAverageNetworkIn,
        MetricTypeEC2SpotFleetRequestAverageNetworkOut,
        MetricTypeSageMakerVariantInvocationsPerInstance,
        MetricTypeECSServiceAverageCPUUtilization,
        MetricTypeECSServiceAverageMemoryUtilization,
        MetricTypeAppStreamAverageCapacityUtilization,
        MetricTypeComprehendInferenceUtilization,
        MetricTypeLambdaProvisionedConcurrencyUtilization,
        MetricTypeCassandraReadCapacityUtilization,
        MetricTypeCassandraWriteCapacityUtilization,
        MetricTypeKafkaBrokerStorageUtilization,
        fromMetricType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype MetricType = MetricType' {fromMetricType :: Core.Text}
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

pattern MetricTypeDynamoDBReadCapacityUtilization :: MetricType
pattern MetricTypeDynamoDBReadCapacityUtilization = MetricType' "DynamoDBReadCapacityUtilization"

pattern MetricTypeDynamoDBWriteCapacityUtilization :: MetricType
pattern MetricTypeDynamoDBWriteCapacityUtilization = MetricType' "DynamoDBWriteCapacityUtilization"

pattern MetricTypeALBRequestCountPerTarget :: MetricType
pattern MetricTypeALBRequestCountPerTarget = MetricType' "ALBRequestCountPerTarget"

pattern MetricTypeRDSReaderAverageCPUUtilization :: MetricType
pattern MetricTypeRDSReaderAverageCPUUtilization = MetricType' "RDSReaderAverageCPUUtilization"

pattern MetricTypeRDSReaderAverageDatabaseConnections :: MetricType
pattern MetricTypeRDSReaderAverageDatabaseConnections = MetricType' "RDSReaderAverageDatabaseConnections"

pattern MetricTypeEC2SpotFleetRequestAverageCPUUtilization :: MetricType
pattern MetricTypeEC2SpotFleetRequestAverageCPUUtilization = MetricType' "EC2SpotFleetRequestAverageCPUUtilization"

pattern MetricTypeEC2SpotFleetRequestAverageNetworkIn :: MetricType
pattern MetricTypeEC2SpotFleetRequestAverageNetworkIn = MetricType' "EC2SpotFleetRequestAverageNetworkIn"

pattern MetricTypeEC2SpotFleetRequestAverageNetworkOut :: MetricType
pattern MetricTypeEC2SpotFleetRequestAverageNetworkOut = MetricType' "EC2SpotFleetRequestAverageNetworkOut"

pattern MetricTypeSageMakerVariantInvocationsPerInstance :: MetricType
pattern MetricTypeSageMakerVariantInvocationsPerInstance = MetricType' "SageMakerVariantInvocationsPerInstance"

pattern MetricTypeECSServiceAverageCPUUtilization :: MetricType
pattern MetricTypeECSServiceAverageCPUUtilization = MetricType' "ECSServiceAverageCPUUtilization"

pattern MetricTypeECSServiceAverageMemoryUtilization :: MetricType
pattern MetricTypeECSServiceAverageMemoryUtilization = MetricType' "ECSServiceAverageMemoryUtilization"

pattern MetricTypeAppStreamAverageCapacityUtilization :: MetricType
pattern MetricTypeAppStreamAverageCapacityUtilization = MetricType' "AppStreamAverageCapacityUtilization"

pattern MetricTypeComprehendInferenceUtilization :: MetricType
pattern MetricTypeComprehendInferenceUtilization = MetricType' "ComprehendInferenceUtilization"

pattern MetricTypeLambdaProvisionedConcurrencyUtilization :: MetricType
pattern MetricTypeLambdaProvisionedConcurrencyUtilization = MetricType' "LambdaProvisionedConcurrencyUtilization"

pattern MetricTypeCassandraReadCapacityUtilization :: MetricType
pattern MetricTypeCassandraReadCapacityUtilization = MetricType' "CassandraReadCapacityUtilization"

pattern MetricTypeCassandraWriteCapacityUtilization :: MetricType
pattern MetricTypeCassandraWriteCapacityUtilization = MetricType' "CassandraWriteCapacityUtilization"

pattern MetricTypeKafkaBrokerStorageUtilization :: MetricType
pattern MetricTypeKafkaBrokerStorageUtilization = MetricType' "KafkaBrokerStorageUtilization"

{-# COMPLETE
  MetricTypeDynamoDBReadCapacityUtilization,
  MetricTypeDynamoDBWriteCapacityUtilization,
  MetricTypeALBRequestCountPerTarget,
  MetricTypeRDSReaderAverageCPUUtilization,
  MetricTypeRDSReaderAverageDatabaseConnections,
  MetricTypeEC2SpotFleetRequestAverageCPUUtilization,
  MetricTypeEC2SpotFleetRequestAverageNetworkIn,
  MetricTypeEC2SpotFleetRequestAverageNetworkOut,
  MetricTypeSageMakerVariantInvocationsPerInstance,
  MetricTypeECSServiceAverageCPUUtilization,
  MetricTypeECSServiceAverageMemoryUtilization,
  MetricTypeAppStreamAverageCapacityUtilization,
  MetricTypeComprehendInferenceUtilization,
  MetricTypeLambdaProvisionedConcurrencyUtilization,
  MetricTypeCassandraReadCapacityUtilization,
  MetricTypeCassandraWriteCapacityUtilization,
  MetricTypeKafkaBrokerStorageUtilization,
  MetricType'
  #-}
