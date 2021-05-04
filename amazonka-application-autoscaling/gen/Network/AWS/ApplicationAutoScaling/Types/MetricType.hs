{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.MetricType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.MetricType
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
        MetricType_KafkaBrokerStorageUtilization,
        MetricType_LambdaProvisionedConcurrencyUtilization,
        MetricType_RDSReaderAverageCPUUtilization,
        MetricType_RDSReaderAverageDatabaseConnections,
        MetricType_SageMakerVariantInvocationsPerInstance
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype MetricType = MetricType'
  { fromMetricType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

pattern MetricType_KafkaBrokerStorageUtilization :: MetricType
pattern MetricType_KafkaBrokerStorageUtilization = MetricType' "KafkaBrokerStorageUtilization"

pattern MetricType_LambdaProvisionedConcurrencyUtilization :: MetricType
pattern MetricType_LambdaProvisionedConcurrencyUtilization = MetricType' "LambdaProvisionedConcurrencyUtilization"

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
  MetricType_KafkaBrokerStorageUtilization,
  MetricType_LambdaProvisionedConcurrencyUtilization,
  MetricType_RDSReaderAverageCPUUtilization,
  MetricType_RDSReaderAverageDatabaseConnections,
  MetricType_SageMakerVariantInvocationsPerInstance,
  MetricType'
  #-}
