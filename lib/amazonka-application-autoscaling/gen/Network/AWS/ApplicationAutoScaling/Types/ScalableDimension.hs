{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        ScalableDimensionEcsServiceDesiredCount,
        ScalableDimensionEC2SpotFleetRequestTargetCapacity,
        ScalableDimensionElasticmapreduceInstancegroupInstanceCount,
        ScalableDimensionAppstreamFleetDesiredCapacity,
        ScalableDimensionDynamodbTableReadCapacityUnits,
        ScalableDimensionDynamodbTableWriteCapacityUnits,
        ScalableDimensionDynamodbIndexReadCapacityUnits,
        ScalableDimensionDynamodbIndexWriteCapacityUnits,
        ScalableDimensionRdsClusterReadReplicaCount,
        ScalableDimensionSagemakerVariantDesiredInstanceCount,
        ScalableDimensionCustomResourceResourceTypeProperty,
        ScalableDimensionComprehendDocumentClassifierEndpointDesiredInferenceUnits,
        ScalableDimensionComprehendEntityRecognizerEndpointDesiredInferenceUnits,
        ScalableDimensionLambdaFunctionProvisionedConcurrency,
        ScalableDimensionCassandraTableReadCapacityUnits,
        ScalableDimensionCassandraTableWriteCapacityUnits,
        ScalableDimensionKafkaBrokerStorageVolumeSize,
        fromScalableDimension
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ScalableDimension = ScalableDimension'
  { fromScalableDimension ::
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

pattern ScalableDimensionEcsServiceDesiredCount :: ScalableDimension
pattern ScalableDimensionEcsServiceDesiredCount = ScalableDimension' "ecs:service:DesiredCount"

pattern ScalableDimensionEC2SpotFleetRequestTargetCapacity :: ScalableDimension
pattern ScalableDimensionEC2SpotFleetRequestTargetCapacity = ScalableDimension' "ec2:spot-fleet-request:TargetCapacity"

pattern ScalableDimensionElasticmapreduceInstancegroupInstanceCount :: ScalableDimension
pattern ScalableDimensionElasticmapreduceInstancegroupInstanceCount = ScalableDimension' "elasticmapreduce:instancegroup:InstanceCount"

pattern ScalableDimensionAppstreamFleetDesiredCapacity :: ScalableDimension
pattern ScalableDimensionAppstreamFleetDesiredCapacity = ScalableDimension' "appstream:fleet:DesiredCapacity"

pattern ScalableDimensionDynamodbTableReadCapacityUnits :: ScalableDimension
pattern ScalableDimensionDynamodbTableReadCapacityUnits = ScalableDimension' "dynamodb:table:ReadCapacityUnits"

pattern ScalableDimensionDynamodbTableWriteCapacityUnits :: ScalableDimension
pattern ScalableDimensionDynamodbTableWriteCapacityUnits = ScalableDimension' "dynamodb:table:WriteCapacityUnits"

pattern ScalableDimensionDynamodbIndexReadCapacityUnits :: ScalableDimension
pattern ScalableDimensionDynamodbIndexReadCapacityUnits = ScalableDimension' "dynamodb:index:ReadCapacityUnits"

pattern ScalableDimensionDynamodbIndexWriteCapacityUnits :: ScalableDimension
pattern ScalableDimensionDynamodbIndexWriteCapacityUnits = ScalableDimension' "dynamodb:index:WriteCapacityUnits"

pattern ScalableDimensionRdsClusterReadReplicaCount :: ScalableDimension
pattern ScalableDimensionRdsClusterReadReplicaCount = ScalableDimension' "rds:cluster:ReadReplicaCount"

pattern ScalableDimensionSagemakerVariantDesiredInstanceCount :: ScalableDimension
pattern ScalableDimensionSagemakerVariantDesiredInstanceCount = ScalableDimension' "sagemaker:variant:DesiredInstanceCount"

pattern ScalableDimensionCustomResourceResourceTypeProperty :: ScalableDimension
pattern ScalableDimensionCustomResourceResourceTypeProperty = ScalableDimension' "custom-resource:ResourceType:Property"

pattern ScalableDimensionComprehendDocumentClassifierEndpointDesiredInferenceUnits :: ScalableDimension
pattern ScalableDimensionComprehendDocumentClassifierEndpointDesiredInferenceUnits = ScalableDimension' "comprehend:document-classifier-endpoint:DesiredInferenceUnits"

pattern ScalableDimensionComprehendEntityRecognizerEndpointDesiredInferenceUnits :: ScalableDimension
pattern ScalableDimensionComprehendEntityRecognizerEndpointDesiredInferenceUnits = ScalableDimension' "comprehend:entity-recognizer-endpoint:DesiredInferenceUnits"

pattern ScalableDimensionLambdaFunctionProvisionedConcurrency :: ScalableDimension
pattern ScalableDimensionLambdaFunctionProvisionedConcurrency = ScalableDimension' "lambda:function:ProvisionedConcurrency"

pattern ScalableDimensionCassandraTableReadCapacityUnits :: ScalableDimension
pattern ScalableDimensionCassandraTableReadCapacityUnits = ScalableDimension' "cassandra:table:ReadCapacityUnits"

pattern ScalableDimensionCassandraTableWriteCapacityUnits :: ScalableDimension
pattern ScalableDimensionCassandraTableWriteCapacityUnits = ScalableDimension' "cassandra:table:WriteCapacityUnits"

pattern ScalableDimensionKafkaBrokerStorageVolumeSize :: ScalableDimension
pattern ScalableDimensionKafkaBrokerStorageVolumeSize = ScalableDimension' "kafka:broker-storage:VolumeSize"

{-# COMPLETE
  ScalableDimensionEcsServiceDesiredCount,
  ScalableDimensionEC2SpotFleetRequestTargetCapacity,
  ScalableDimensionElasticmapreduceInstancegroupInstanceCount,
  ScalableDimensionAppstreamFleetDesiredCapacity,
  ScalableDimensionDynamodbTableReadCapacityUnits,
  ScalableDimensionDynamodbTableWriteCapacityUnits,
  ScalableDimensionDynamodbIndexReadCapacityUnits,
  ScalableDimensionDynamodbIndexWriteCapacityUnits,
  ScalableDimensionRdsClusterReadReplicaCount,
  ScalableDimensionSagemakerVariantDesiredInstanceCount,
  ScalableDimensionCustomResourceResourceTypeProperty,
  ScalableDimensionComprehendDocumentClassifierEndpointDesiredInferenceUnits,
  ScalableDimensionComprehendEntityRecognizerEndpointDesiredInferenceUnits,
  ScalableDimensionLambdaFunctionProvisionedConcurrency,
  ScalableDimensionCassandraTableReadCapacityUnits,
  ScalableDimensionCassandraTableWriteCapacityUnits,
  ScalableDimensionKafkaBrokerStorageVolumeSize,
  ScalableDimension'
  #-}
