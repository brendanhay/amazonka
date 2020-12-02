{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScalableDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ScalableDimension where

import Network.AWS.Prelude

data ScalableDimension
  = AppstreamFleetDesiredCapacity
  | CassandraTableReadCapacityUnits
  | CassandraTableWriteCapacityUnits
  | ComprehendDocumentClassifierEndpointDesiredInferenceUnits
  | ComprehendEntityRecognizerEndpointDesiredInferenceUnits
  | CustomResourceResourceTypeProperty
  | DynamodbIndexReadCapacityUnits
  | DynamodbIndexWriteCapacityUnits
  | DynamodbTableReadCapacityUnits
  | DynamodbTableWriteCapacityUnits
  | EC2SpotFleetRequestTargetCapacity
  | EcsServiceDesiredCount
  | ElasticmapreduceInstancegroupInstanceCount
  | KafkaBrokerStorageVolumeSize
  | LambdaFunctionProvisionedConcurrency
  | RDSClusterReadReplicaCount
  | SagemakerVariantDesiredInstanceCount
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

instance FromText ScalableDimension where
  parser =
    takeLowerText >>= \case
      "appstream:fleet:desiredcapacity" -> pure AppstreamFleetDesiredCapacity
      "cassandra:table:readcapacityunits" -> pure CassandraTableReadCapacityUnits
      "cassandra:table:writecapacityunits" -> pure CassandraTableWriteCapacityUnits
      "comprehend:document-classifier-endpoint:desiredinferenceunits" -> pure ComprehendDocumentClassifierEndpointDesiredInferenceUnits
      "comprehend:entity-recognizer-endpoint:desiredinferenceunits" -> pure ComprehendEntityRecognizerEndpointDesiredInferenceUnits
      "custom-resource:resourcetype:property" -> pure CustomResourceResourceTypeProperty
      "dynamodb:index:readcapacityunits" -> pure DynamodbIndexReadCapacityUnits
      "dynamodb:index:writecapacityunits" -> pure DynamodbIndexWriteCapacityUnits
      "dynamodb:table:readcapacityunits" -> pure DynamodbTableReadCapacityUnits
      "dynamodb:table:writecapacityunits" -> pure DynamodbTableWriteCapacityUnits
      "ec2:spot-fleet-request:targetcapacity" -> pure EC2SpotFleetRequestTargetCapacity
      "ecs:service:desiredcount" -> pure EcsServiceDesiredCount
      "elasticmapreduce:instancegroup:instancecount" -> pure ElasticmapreduceInstancegroupInstanceCount
      "kafka:broker-storage:volumesize" -> pure KafkaBrokerStorageVolumeSize
      "lambda:function:provisionedconcurrency" -> pure LambdaFunctionProvisionedConcurrency
      "rds:cluster:readreplicacount" -> pure RDSClusterReadReplicaCount
      "sagemaker:variant:desiredinstancecount" -> pure SagemakerVariantDesiredInstanceCount
      e ->
        fromTextError $
          "Failure parsing ScalableDimension from value: '" <> e
            <> "'. Accepted values: appstream:fleet:desiredcapacity, cassandra:table:readcapacityunits, cassandra:table:writecapacityunits, comprehend:document-classifier-endpoint:desiredinferenceunits, comprehend:entity-recognizer-endpoint:desiredinferenceunits, custom-resource:resourcetype:property, dynamodb:index:readcapacityunits, dynamodb:index:writecapacityunits, dynamodb:table:readcapacityunits, dynamodb:table:writecapacityunits, ec2:spot-fleet-request:targetcapacity, ecs:service:desiredcount, elasticmapreduce:instancegroup:instancecount, kafka:broker-storage:volumesize, lambda:function:provisionedconcurrency, rds:cluster:readreplicacount, sagemaker:variant:desiredinstancecount"

instance ToText ScalableDimension where
  toText = \case
    AppstreamFleetDesiredCapacity -> "appstream:fleet:DesiredCapacity"
    CassandraTableReadCapacityUnits -> "cassandra:table:ReadCapacityUnits"
    CassandraTableWriteCapacityUnits -> "cassandra:table:WriteCapacityUnits"
    ComprehendDocumentClassifierEndpointDesiredInferenceUnits -> "comprehend:document-classifier-endpoint:DesiredInferenceUnits"
    ComprehendEntityRecognizerEndpointDesiredInferenceUnits -> "comprehend:entity-recognizer-endpoint:DesiredInferenceUnits"
    CustomResourceResourceTypeProperty -> "custom-resource:ResourceType:Property"
    DynamodbIndexReadCapacityUnits -> "dynamodb:index:ReadCapacityUnits"
    DynamodbIndexWriteCapacityUnits -> "dynamodb:index:WriteCapacityUnits"
    DynamodbTableReadCapacityUnits -> "dynamodb:table:ReadCapacityUnits"
    DynamodbTableWriteCapacityUnits -> "dynamodb:table:WriteCapacityUnits"
    EC2SpotFleetRequestTargetCapacity -> "ec2:spot-fleet-request:TargetCapacity"
    EcsServiceDesiredCount -> "ecs:service:DesiredCount"
    ElasticmapreduceInstancegroupInstanceCount -> "elasticmapreduce:instancegroup:InstanceCount"
    KafkaBrokerStorageVolumeSize -> "kafka:broker-storage:VolumeSize"
    LambdaFunctionProvisionedConcurrency -> "lambda:function:ProvisionedConcurrency"
    RDSClusterReadReplicaCount -> "rds:cluster:ReadReplicaCount"
    SagemakerVariantDesiredInstanceCount -> "sagemaker:variant:DesiredInstanceCount"

instance Hashable ScalableDimension

instance NFData ScalableDimension

instance ToByteString ScalableDimension

instance ToQuery ScalableDimension

instance ToHeader ScalableDimension

instance ToJSON ScalableDimension where
  toJSON = toJSONText

instance FromJSON ScalableDimension where
  parseJSON = parseJSONText "ScalableDimension"
