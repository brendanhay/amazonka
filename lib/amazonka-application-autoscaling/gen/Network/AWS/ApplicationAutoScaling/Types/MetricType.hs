{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.MetricType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.MetricType where

import Network.AWS.Prelude

data MetricType
  = ALBRequestCountPerTarget
  | AppStreamAverageCapacityUtilization
  | CassandraReadCapacityUtilization
  | CassandraWriteCapacityUtilization
  | ComprehendInferenceUtilization
  | DynamoDBReadCapacityUtilization
  | DynamoDBWriteCapacityUtilization
  | EC2SpotFleetRequestAverageCPUUtilization
  | EC2SpotFleetRequestAverageNetworkIn
  | EC2SpotFleetRequestAverageNetworkOut
  | ECSServiceAverageCPUUtilization
  | ECSServiceAverageMemoryUtilization
  | KafkaBrokerStorageUtilization
  | LambdaProvisionedConcurrencyUtilization
  | RDSReaderAverageCPUUtilization
  | RDSReaderAverageDatabaseConnections
  | SageMakerVariantInvocationsPerInstance
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText MetricType where
  parser =
    takeLowerText >>= \case
      "albrequestcountpertarget" -> pure ALBRequestCountPerTarget
      "appstreamaveragecapacityutilization" -> pure AppStreamAverageCapacityUtilization
      "cassandrareadcapacityutilization" -> pure CassandraReadCapacityUtilization
      "cassandrawritecapacityutilization" -> pure CassandraWriteCapacityUtilization
      "comprehendinferenceutilization" -> pure ComprehendInferenceUtilization
      "dynamodbreadcapacityutilization" -> pure DynamoDBReadCapacityUtilization
      "dynamodbwritecapacityutilization" -> pure DynamoDBWriteCapacityUtilization
      "ec2spotfleetrequestaveragecpuutilization" -> pure EC2SpotFleetRequestAverageCPUUtilization
      "ec2spotfleetrequestaveragenetworkin" -> pure EC2SpotFleetRequestAverageNetworkIn
      "ec2spotfleetrequestaveragenetworkout" -> pure EC2SpotFleetRequestAverageNetworkOut
      "ecsserviceaveragecpuutilization" -> pure ECSServiceAverageCPUUtilization
      "ecsserviceaveragememoryutilization" -> pure ECSServiceAverageMemoryUtilization
      "kafkabrokerstorageutilization" -> pure KafkaBrokerStorageUtilization
      "lambdaprovisionedconcurrencyutilization" -> pure LambdaProvisionedConcurrencyUtilization
      "rdsreaderaveragecpuutilization" -> pure RDSReaderAverageCPUUtilization
      "rdsreaderaveragedatabaseconnections" -> pure RDSReaderAverageDatabaseConnections
      "sagemakervariantinvocationsperinstance" -> pure SageMakerVariantInvocationsPerInstance
      e ->
        fromTextError $
          "Failure parsing MetricType from value: '" <> e
            <> "'. Accepted values: albrequestcountpertarget, appstreamaveragecapacityutilization, cassandrareadcapacityutilization, cassandrawritecapacityutilization, comprehendinferenceutilization, dynamodbreadcapacityutilization, dynamodbwritecapacityutilization, ec2spotfleetrequestaveragecpuutilization, ec2spotfleetrequestaveragenetworkin, ec2spotfleetrequestaveragenetworkout, ecsserviceaveragecpuutilization, ecsserviceaveragememoryutilization, kafkabrokerstorageutilization, lambdaprovisionedconcurrencyutilization, rdsreaderaveragecpuutilization, rdsreaderaveragedatabaseconnections, sagemakervariantinvocationsperinstance"

instance ToText MetricType where
  toText = \case
    ALBRequestCountPerTarget -> "ALBRequestCountPerTarget"
    AppStreamAverageCapacityUtilization -> "AppStreamAverageCapacityUtilization"
    CassandraReadCapacityUtilization -> "CassandraReadCapacityUtilization"
    CassandraWriteCapacityUtilization -> "CassandraWriteCapacityUtilization"
    ComprehendInferenceUtilization -> "ComprehendInferenceUtilization"
    DynamoDBReadCapacityUtilization -> "DynamoDBReadCapacityUtilization"
    DynamoDBWriteCapacityUtilization -> "DynamoDBWriteCapacityUtilization"
    EC2SpotFleetRequestAverageCPUUtilization -> "EC2SpotFleetRequestAverageCPUUtilization"
    EC2SpotFleetRequestAverageNetworkIn -> "EC2SpotFleetRequestAverageNetworkIn"
    EC2SpotFleetRequestAverageNetworkOut -> "EC2SpotFleetRequestAverageNetworkOut"
    ECSServiceAverageCPUUtilization -> "ECSServiceAverageCPUUtilization"
    ECSServiceAverageMemoryUtilization -> "ECSServiceAverageMemoryUtilization"
    KafkaBrokerStorageUtilization -> "KafkaBrokerStorageUtilization"
    LambdaProvisionedConcurrencyUtilization -> "LambdaProvisionedConcurrencyUtilization"
    RDSReaderAverageCPUUtilization -> "RDSReaderAverageCPUUtilization"
    RDSReaderAverageDatabaseConnections -> "RDSReaderAverageDatabaseConnections"
    SageMakerVariantInvocationsPerInstance -> "SageMakerVariantInvocationsPerInstance"

instance Hashable MetricType

instance NFData MetricType

instance ToByteString MetricType

instance ToQuery MetricType

instance ToHeader MetricType

instance ToJSON MetricType where
  toJSON = toJSONText

instance FromJSON MetricType where
  parseJSON = parseJSONText "MetricType"
