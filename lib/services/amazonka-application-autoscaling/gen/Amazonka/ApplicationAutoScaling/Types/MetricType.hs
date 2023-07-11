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
-- Module      : Amazonka.ApplicationAutoScaling.Types.MetricType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.MetricType
  ( MetricType
      ( ..,
        MetricType_ALBRequestCountPerTarget,
        MetricType_AppStreamAverageCapacityUtilization,
        MetricType_CassandraReadCapacityUtilization,
        MetricType_CassandraWriteCapacityUtilization,
        MetricType_ComprehendInferenceUtilization,
        MetricType_DynamoDBReadCapacityUtilization,
        MetricType_DynamoDBWriteCapacityUtilization,
        MetricType_EC2SpotFleetRequestAverageCPUUtilization,
        MetricType_EC2SpotFleetRequestAverageNetworkIn,
        MetricType_EC2SpotFleetRequestAverageNetworkOut,
        MetricType_ECSServiceAverageCPUUtilization,
        MetricType_ECSServiceAverageMemoryUtilization,
        MetricType_ElastiCacheDatabaseMemoryUsageCountedForEvictPercentage,
        MetricType_ElastiCachePrimaryEngineCPUUtilization,
        MetricType_ElastiCacheReplicaEngineCPUUtilization,
        MetricType_KafkaBrokerStorageUtilization,
        MetricType_LambdaProvisionedConcurrencyUtilization,
        MetricType_NeptuneReaderAverageCPUUtilization,
        MetricType_RDSReaderAverageCPUUtilization,
        MetricType_RDSReaderAverageDatabaseConnections,
        MetricType_SageMakerVariantInvocationsPerInstance
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MetricType = MetricType'
  { fromMetricType ::
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

pattern MetricType_ALBRequestCountPerTarget :: MetricType
pattern MetricType_ALBRequestCountPerTarget = MetricType' "ALBRequestCountPerTarget"

pattern MetricType_AppStreamAverageCapacityUtilization :: MetricType
pattern MetricType_AppStreamAverageCapacityUtilization = MetricType' "AppStreamAverageCapacityUtilization"

pattern MetricType_CassandraReadCapacityUtilization :: MetricType
pattern MetricType_CassandraReadCapacityUtilization = MetricType' "CassandraReadCapacityUtilization"

pattern MetricType_CassandraWriteCapacityUtilization :: MetricType
pattern MetricType_CassandraWriteCapacityUtilization = MetricType' "CassandraWriteCapacityUtilization"

pattern MetricType_ComprehendInferenceUtilization :: MetricType
pattern MetricType_ComprehendInferenceUtilization = MetricType' "ComprehendInferenceUtilization"

pattern MetricType_DynamoDBReadCapacityUtilization :: MetricType
pattern MetricType_DynamoDBReadCapacityUtilization = MetricType' "DynamoDBReadCapacityUtilization"

pattern MetricType_DynamoDBWriteCapacityUtilization :: MetricType
pattern MetricType_DynamoDBWriteCapacityUtilization = MetricType' "DynamoDBWriteCapacityUtilization"

pattern MetricType_EC2SpotFleetRequestAverageCPUUtilization :: MetricType
pattern MetricType_EC2SpotFleetRequestAverageCPUUtilization = MetricType' "EC2SpotFleetRequestAverageCPUUtilization"

pattern MetricType_EC2SpotFleetRequestAverageNetworkIn :: MetricType
pattern MetricType_EC2SpotFleetRequestAverageNetworkIn = MetricType' "EC2SpotFleetRequestAverageNetworkIn"

pattern MetricType_EC2SpotFleetRequestAverageNetworkOut :: MetricType
pattern MetricType_EC2SpotFleetRequestAverageNetworkOut = MetricType' "EC2SpotFleetRequestAverageNetworkOut"

pattern MetricType_ECSServiceAverageCPUUtilization :: MetricType
pattern MetricType_ECSServiceAverageCPUUtilization = MetricType' "ECSServiceAverageCPUUtilization"

pattern MetricType_ECSServiceAverageMemoryUtilization :: MetricType
pattern MetricType_ECSServiceAverageMemoryUtilization = MetricType' "ECSServiceAverageMemoryUtilization"

pattern MetricType_ElastiCacheDatabaseMemoryUsageCountedForEvictPercentage :: MetricType
pattern MetricType_ElastiCacheDatabaseMemoryUsageCountedForEvictPercentage = MetricType' "ElastiCacheDatabaseMemoryUsageCountedForEvictPercentage"

pattern MetricType_ElastiCachePrimaryEngineCPUUtilization :: MetricType
pattern MetricType_ElastiCachePrimaryEngineCPUUtilization = MetricType' "ElastiCachePrimaryEngineCPUUtilization"

pattern MetricType_ElastiCacheReplicaEngineCPUUtilization :: MetricType
pattern MetricType_ElastiCacheReplicaEngineCPUUtilization = MetricType' "ElastiCacheReplicaEngineCPUUtilization"

pattern MetricType_KafkaBrokerStorageUtilization :: MetricType
pattern MetricType_KafkaBrokerStorageUtilization = MetricType' "KafkaBrokerStorageUtilization"

pattern MetricType_LambdaProvisionedConcurrencyUtilization :: MetricType
pattern MetricType_LambdaProvisionedConcurrencyUtilization = MetricType' "LambdaProvisionedConcurrencyUtilization"

pattern MetricType_NeptuneReaderAverageCPUUtilization :: MetricType
pattern MetricType_NeptuneReaderAverageCPUUtilization = MetricType' "NeptuneReaderAverageCPUUtilization"

pattern MetricType_RDSReaderAverageCPUUtilization :: MetricType
pattern MetricType_RDSReaderAverageCPUUtilization = MetricType' "RDSReaderAverageCPUUtilization"

pattern MetricType_RDSReaderAverageDatabaseConnections :: MetricType
pattern MetricType_RDSReaderAverageDatabaseConnections = MetricType' "RDSReaderAverageDatabaseConnections"

pattern MetricType_SageMakerVariantInvocationsPerInstance :: MetricType
pattern MetricType_SageMakerVariantInvocationsPerInstance = MetricType' "SageMakerVariantInvocationsPerInstance"

{-# COMPLETE
  MetricType_ALBRequestCountPerTarget,
  MetricType_AppStreamAverageCapacityUtilization,
  MetricType_CassandraReadCapacityUtilization,
  MetricType_CassandraWriteCapacityUtilization,
  MetricType_ComprehendInferenceUtilization,
  MetricType_DynamoDBReadCapacityUtilization,
  MetricType_DynamoDBWriteCapacityUtilization,
  MetricType_EC2SpotFleetRequestAverageCPUUtilization,
  MetricType_EC2SpotFleetRequestAverageNetworkIn,
  MetricType_EC2SpotFleetRequestAverageNetworkOut,
  MetricType_ECSServiceAverageCPUUtilization,
  MetricType_ECSServiceAverageMemoryUtilization,
  MetricType_ElastiCacheDatabaseMemoryUsageCountedForEvictPercentage,
  MetricType_ElastiCachePrimaryEngineCPUUtilization,
  MetricType_ElastiCacheReplicaEngineCPUUtilization,
  MetricType_KafkaBrokerStorageUtilization,
  MetricType_LambdaProvisionedConcurrencyUtilization,
  MetricType_NeptuneReaderAverageCPUUtilization,
  MetricType_RDSReaderAverageCPUUtilization,
  MetricType_RDSReaderAverageDatabaseConnections,
  MetricType_SageMakerVariantInvocationsPerInstance,
  MetricType'
  #-}
