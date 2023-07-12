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
-- Module      : Amazonka.ApplicationAutoScaling.Types.ScalableDimension
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.ScalableDimension
  ( ScalableDimension
      ( ..,
        ScalableDimension_Appstream_fleet_DesiredCapacity,
        ScalableDimension_Cassandra_table_ReadCapacityUnits,
        ScalableDimension_Cassandra_table_WriteCapacityUnits,
        ScalableDimension_Comprehend_document_classifier_endpoint_DesiredInferenceUnits,
        ScalableDimension_Comprehend_entity_recognizer_endpoint_DesiredInferenceUnits,
        ScalableDimension_Custom_resource_ResourceType_Property,
        ScalableDimension_Dynamodb_index_ReadCapacityUnits,
        ScalableDimension_Dynamodb_index_WriteCapacityUnits,
        ScalableDimension_Dynamodb_table_ReadCapacityUnits,
        ScalableDimension_Dynamodb_table_WriteCapacityUnits,
        ScalableDimension_Ec2_spot_fleet_request_TargetCapacity,
        ScalableDimension_Ecs_service_DesiredCount,
        ScalableDimension_Elasticache_replication_group_NodeGroups,
        ScalableDimension_Elasticache_replication_group_Replicas,
        ScalableDimension_Elasticmapreduce_instancegroup_InstanceCount,
        ScalableDimension_Kafka_broker_storage_VolumeSize,
        ScalableDimension_Lambda_function_ProvisionedConcurrency,
        ScalableDimension_Neptune_cluster_ReadReplicaCount,
        ScalableDimension_Rds_cluster_ReadReplicaCount,
        ScalableDimension_Sagemaker_variant_DesiredInstanceCount
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScalableDimension = ScalableDimension'
  { fromScalableDimension ::
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

pattern ScalableDimension_Appstream_fleet_DesiredCapacity :: ScalableDimension
pattern ScalableDimension_Appstream_fleet_DesiredCapacity = ScalableDimension' "appstream:fleet:DesiredCapacity"

pattern ScalableDimension_Cassandra_table_ReadCapacityUnits :: ScalableDimension
pattern ScalableDimension_Cassandra_table_ReadCapacityUnits = ScalableDimension' "cassandra:table:ReadCapacityUnits"

pattern ScalableDimension_Cassandra_table_WriteCapacityUnits :: ScalableDimension
pattern ScalableDimension_Cassandra_table_WriteCapacityUnits = ScalableDimension' "cassandra:table:WriteCapacityUnits"

pattern ScalableDimension_Comprehend_document_classifier_endpoint_DesiredInferenceUnits :: ScalableDimension
pattern ScalableDimension_Comprehend_document_classifier_endpoint_DesiredInferenceUnits = ScalableDimension' "comprehend:document-classifier-endpoint:DesiredInferenceUnits"

pattern ScalableDimension_Comprehend_entity_recognizer_endpoint_DesiredInferenceUnits :: ScalableDimension
pattern ScalableDimension_Comprehend_entity_recognizer_endpoint_DesiredInferenceUnits = ScalableDimension' "comprehend:entity-recognizer-endpoint:DesiredInferenceUnits"

pattern ScalableDimension_Custom_resource_ResourceType_Property :: ScalableDimension
pattern ScalableDimension_Custom_resource_ResourceType_Property = ScalableDimension' "custom-resource:ResourceType:Property"

pattern ScalableDimension_Dynamodb_index_ReadCapacityUnits :: ScalableDimension
pattern ScalableDimension_Dynamodb_index_ReadCapacityUnits = ScalableDimension' "dynamodb:index:ReadCapacityUnits"

pattern ScalableDimension_Dynamodb_index_WriteCapacityUnits :: ScalableDimension
pattern ScalableDimension_Dynamodb_index_WriteCapacityUnits = ScalableDimension' "dynamodb:index:WriteCapacityUnits"

pattern ScalableDimension_Dynamodb_table_ReadCapacityUnits :: ScalableDimension
pattern ScalableDimension_Dynamodb_table_ReadCapacityUnits = ScalableDimension' "dynamodb:table:ReadCapacityUnits"

pattern ScalableDimension_Dynamodb_table_WriteCapacityUnits :: ScalableDimension
pattern ScalableDimension_Dynamodb_table_WriteCapacityUnits = ScalableDimension' "dynamodb:table:WriteCapacityUnits"

pattern ScalableDimension_Ec2_spot_fleet_request_TargetCapacity :: ScalableDimension
pattern ScalableDimension_Ec2_spot_fleet_request_TargetCapacity = ScalableDimension' "ec2:spot-fleet-request:TargetCapacity"

pattern ScalableDimension_Ecs_service_DesiredCount :: ScalableDimension
pattern ScalableDimension_Ecs_service_DesiredCount = ScalableDimension' "ecs:service:DesiredCount"

pattern ScalableDimension_Elasticache_replication_group_NodeGroups :: ScalableDimension
pattern ScalableDimension_Elasticache_replication_group_NodeGroups = ScalableDimension' "elasticache:replication-group:NodeGroups"

pattern ScalableDimension_Elasticache_replication_group_Replicas :: ScalableDimension
pattern ScalableDimension_Elasticache_replication_group_Replicas = ScalableDimension' "elasticache:replication-group:Replicas"

pattern ScalableDimension_Elasticmapreduce_instancegroup_InstanceCount :: ScalableDimension
pattern ScalableDimension_Elasticmapreduce_instancegroup_InstanceCount = ScalableDimension' "elasticmapreduce:instancegroup:InstanceCount"

pattern ScalableDimension_Kafka_broker_storage_VolumeSize :: ScalableDimension
pattern ScalableDimension_Kafka_broker_storage_VolumeSize = ScalableDimension' "kafka:broker-storage:VolumeSize"

pattern ScalableDimension_Lambda_function_ProvisionedConcurrency :: ScalableDimension
pattern ScalableDimension_Lambda_function_ProvisionedConcurrency = ScalableDimension' "lambda:function:ProvisionedConcurrency"

pattern ScalableDimension_Neptune_cluster_ReadReplicaCount :: ScalableDimension
pattern ScalableDimension_Neptune_cluster_ReadReplicaCount = ScalableDimension' "neptune:cluster:ReadReplicaCount"

pattern ScalableDimension_Rds_cluster_ReadReplicaCount :: ScalableDimension
pattern ScalableDimension_Rds_cluster_ReadReplicaCount = ScalableDimension' "rds:cluster:ReadReplicaCount"

pattern ScalableDimension_Sagemaker_variant_DesiredInstanceCount :: ScalableDimension
pattern ScalableDimension_Sagemaker_variant_DesiredInstanceCount = ScalableDimension' "sagemaker:variant:DesiredInstanceCount"

{-# COMPLETE
  ScalableDimension_Appstream_fleet_DesiredCapacity,
  ScalableDimension_Cassandra_table_ReadCapacityUnits,
  ScalableDimension_Cassandra_table_WriteCapacityUnits,
  ScalableDimension_Comprehend_document_classifier_endpoint_DesiredInferenceUnits,
  ScalableDimension_Comprehend_entity_recognizer_endpoint_DesiredInferenceUnits,
  ScalableDimension_Custom_resource_ResourceType_Property,
  ScalableDimension_Dynamodb_index_ReadCapacityUnits,
  ScalableDimension_Dynamodb_index_WriteCapacityUnits,
  ScalableDimension_Dynamodb_table_ReadCapacityUnits,
  ScalableDimension_Dynamodb_table_WriteCapacityUnits,
  ScalableDimension_Ec2_spot_fleet_request_TargetCapacity,
  ScalableDimension_Ecs_service_DesiredCount,
  ScalableDimension_Elasticache_replication_group_NodeGroups,
  ScalableDimension_Elasticache_replication_group_Replicas,
  ScalableDimension_Elasticmapreduce_instancegroup_InstanceCount,
  ScalableDimension_Kafka_broker_storage_VolumeSize,
  ScalableDimension_Lambda_function_ProvisionedConcurrency,
  ScalableDimension_Neptune_cluster_ReadReplicaCount,
  ScalableDimension_Rds_cluster_ReadReplicaCount,
  ScalableDimension_Sagemaker_variant_DesiredInstanceCount,
  ScalableDimension'
  #-}
