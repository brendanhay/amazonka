-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScalableDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ScalableDimension
  ( ScalableDimension
      ( ScalableDimension',
        AppstreamFleetDesiredCapacity,
        CassandraTableReadCapacityUnits,
        CassandraTableWriteCapacityUnits,
        ComprehendDocumentClassifierEndpointDesiredInferenceUnits,
        ComprehendEntityRecognizerEndpointDesiredInferenceUnits,
        CustomResourceResourceTypeProperty,
        DynamodbIndexReadCapacityUnits,
        DynamodbIndexWriteCapacityUnits,
        DynamodbTableReadCapacityUnits,
        DynamodbTableWriteCapacityUnits,
        EC2SpotFleetRequestTargetCapacity,
        EcsServiceDesiredCount,
        ElasticmapreduceInstancegroupInstanceCount,
        KafkaBrokerStorageVolumeSize,
        LambdaFunctionProvisionedConcurrency,
        RDSClusterReadReplicaCount,
        SagemakerVariantDesiredInstanceCount
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ScalableDimension = ScalableDimension' Lude.Text
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

pattern AppstreamFleetDesiredCapacity :: ScalableDimension
pattern AppstreamFleetDesiredCapacity = ScalableDimension' "appstream:fleet:DesiredCapacity"

pattern CassandraTableReadCapacityUnits :: ScalableDimension
pattern CassandraTableReadCapacityUnits = ScalableDimension' "cassandra:table:ReadCapacityUnits"

pattern CassandraTableWriteCapacityUnits :: ScalableDimension
pattern CassandraTableWriteCapacityUnits = ScalableDimension' "cassandra:table:WriteCapacityUnits"

pattern ComprehendDocumentClassifierEndpointDesiredInferenceUnits :: ScalableDimension
pattern ComprehendDocumentClassifierEndpointDesiredInferenceUnits = ScalableDimension' "comprehend:document-classifier-endpoint:DesiredInferenceUnits"

pattern ComprehendEntityRecognizerEndpointDesiredInferenceUnits :: ScalableDimension
pattern ComprehendEntityRecognizerEndpointDesiredInferenceUnits = ScalableDimension' "comprehend:entity-recognizer-endpoint:DesiredInferenceUnits"

pattern CustomResourceResourceTypeProperty :: ScalableDimension
pattern CustomResourceResourceTypeProperty = ScalableDimension' "custom-resource:ResourceType:Property"

pattern DynamodbIndexReadCapacityUnits :: ScalableDimension
pattern DynamodbIndexReadCapacityUnits = ScalableDimension' "dynamodb:index:ReadCapacityUnits"

pattern DynamodbIndexWriteCapacityUnits :: ScalableDimension
pattern DynamodbIndexWriteCapacityUnits = ScalableDimension' "dynamodb:index:WriteCapacityUnits"

pattern DynamodbTableReadCapacityUnits :: ScalableDimension
pattern DynamodbTableReadCapacityUnits = ScalableDimension' "dynamodb:table:ReadCapacityUnits"

pattern DynamodbTableWriteCapacityUnits :: ScalableDimension
pattern DynamodbTableWriteCapacityUnits = ScalableDimension' "dynamodb:table:WriteCapacityUnits"

pattern EC2SpotFleetRequestTargetCapacity :: ScalableDimension
pattern EC2SpotFleetRequestTargetCapacity = ScalableDimension' "ec2:spot-fleet-request:TargetCapacity"

pattern EcsServiceDesiredCount :: ScalableDimension
pattern EcsServiceDesiredCount = ScalableDimension' "ecs:service:DesiredCount"

pattern ElasticmapreduceInstancegroupInstanceCount :: ScalableDimension
pattern ElasticmapreduceInstancegroupInstanceCount = ScalableDimension' "elasticmapreduce:instancegroup:InstanceCount"

pattern KafkaBrokerStorageVolumeSize :: ScalableDimension
pattern KafkaBrokerStorageVolumeSize = ScalableDimension' "kafka:broker-storage:VolumeSize"

pattern LambdaFunctionProvisionedConcurrency :: ScalableDimension
pattern LambdaFunctionProvisionedConcurrency = ScalableDimension' "lambda:function:ProvisionedConcurrency"

pattern RDSClusterReadReplicaCount :: ScalableDimension
pattern RDSClusterReadReplicaCount = ScalableDimension' "rds:cluster:ReadReplicaCount"

pattern SagemakerVariantDesiredInstanceCount :: ScalableDimension
pattern SagemakerVariantDesiredInstanceCount = ScalableDimension' "sagemaker:variant:DesiredInstanceCount"

{-# COMPLETE
  AppstreamFleetDesiredCapacity,
  CassandraTableReadCapacityUnits,
  CassandraTableWriteCapacityUnits,
  ComprehendDocumentClassifierEndpointDesiredInferenceUnits,
  ComprehendEntityRecognizerEndpointDesiredInferenceUnits,
  CustomResourceResourceTypeProperty,
  DynamodbIndexReadCapacityUnits,
  DynamodbIndexWriteCapacityUnits,
  DynamodbTableReadCapacityUnits,
  DynamodbTableWriteCapacityUnits,
  EC2SpotFleetRequestTargetCapacity,
  EcsServiceDesiredCount,
  ElasticmapreduceInstancegroupInstanceCount,
  KafkaBrokerStorageVolumeSize,
  LambdaFunctionProvisionedConcurrency,
  RDSClusterReadReplicaCount,
  SagemakerVariantDesiredInstanceCount,
  ScalableDimension'
  #-}
