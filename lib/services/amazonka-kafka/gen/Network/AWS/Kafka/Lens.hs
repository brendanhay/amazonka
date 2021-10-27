{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kafka.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kafka.Lens
  ( -- * Operations

    -- ** CreateConfiguration
    createConfiguration_kafkaVersions,
    createConfiguration_description,
    createConfiguration_serverProperties,
    createConfiguration_name,
    createConfigurationResponse_creationTime,
    createConfigurationResponse_state,
    createConfigurationResponse_arn,
    createConfigurationResponse_latestRevision,
    createConfigurationResponse_name,
    createConfigurationResponse_httpStatus,

    -- ** DescribeCluster
    describeCluster_clusterArn,
    describeClusterResponse_clusterInfo,
    describeClusterResponse_httpStatus,

    -- ** RebootBroker
    rebootBroker_clusterArn,
    rebootBroker_brokerIds,
    rebootBrokerResponse_clusterArn,
    rebootBrokerResponse_clusterOperationArn,
    rebootBrokerResponse_httpStatus,

    -- ** ListConfigurationRevisions
    listConfigurationRevisions_nextToken,
    listConfigurationRevisions_maxResults,
    listConfigurationRevisions_arn,
    listConfigurationRevisionsResponse_nextToken,
    listConfigurationRevisionsResponse_revisions,
    listConfigurationRevisionsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListKafkaVersions
    listKafkaVersions_nextToken,
    listKafkaVersions_maxResults,
    listKafkaVersionsResponse_kafkaVersions,
    listKafkaVersionsResponse_nextToken,
    listKafkaVersionsResponse_httpStatus,

    -- ** UpdateMonitoring
    updateMonitoring_enhancedMonitoring,
    updateMonitoring_openMonitoring,
    updateMonitoring_loggingInfo,
    updateMonitoring_clusterArn,
    updateMonitoring_currentVersion,
    updateMonitoringResponse_clusterArn,
    updateMonitoringResponse_clusterOperationArn,
    updateMonitoringResponse_httpStatus,

    -- ** BatchAssociateScramSecret
    batchAssociateScramSecret_clusterArn,
    batchAssociateScramSecret_secretArnList,
    batchAssociateScramSecretResponse_clusterArn,
    batchAssociateScramSecretResponse_unprocessedScramSecrets,
    batchAssociateScramSecretResponse_httpStatus,

    -- ** UpdateBrokerStorage
    updateBrokerStorage_clusterArn,
    updateBrokerStorage_targetBrokerEBSVolumeInfo,
    updateBrokerStorage_currentVersion,
    updateBrokerStorageResponse_clusterArn,
    updateBrokerStorageResponse_clusterOperationArn,
    updateBrokerStorageResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_currentVersion,
    deleteCluster_clusterArn,
    deleteClusterResponse_state,
    deleteClusterResponse_clusterArn,
    deleteClusterResponse_httpStatus,

    -- ** UpdateClusterConfiguration
    updateClusterConfiguration_clusterArn,
    updateClusterConfiguration_currentVersion,
    updateClusterConfiguration_configurationInfo,
    updateClusterConfigurationResponse_clusterArn,
    updateClusterConfigurationResponse_clusterOperationArn,
    updateClusterConfigurationResponse_httpStatus,

    -- ** CreateCluster
    createCluster_enhancedMonitoring,
    createCluster_openMonitoring,
    createCluster_configurationInfo,
    createCluster_loggingInfo,
    createCluster_clientAuthentication,
    createCluster_tags,
    createCluster_encryptionInfo,
    createCluster_brokerNodeGroupInfo,
    createCluster_kafkaVersion,
    createCluster_numberOfBrokerNodes,
    createCluster_clusterName,
    createClusterResponse_state,
    createClusterResponse_clusterArn,
    createClusterResponse_clusterName,
    createClusterResponse_httpStatus,

    -- ** UpdateBrokerCount
    updateBrokerCount_clusterArn,
    updateBrokerCount_currentVersion,
    updateBrokerCount_targetNumberOfBrokerNodes,
    updateBrokerCountResponse_clusterArn,
    updateBrokerCountResponse_clusterOperationArn,
    updateBrokerCountResponse_httpStatus,

    -- ** ListConfigurations
    listConfigurations_nextToken,
    listConfigurations_maxResults,
    listConfigurationsResponse_configurations,
    listConfigurationsResponse_nextToken,
    listConfigurationsResponse_httpStatus,

    -- ** GetBootstrapBrokers
    getBootstrapBrokers_clusterArn,
    getBootstrapBrokersResponse_bootstrapBrokerString,
    getBootstrapBrokersResponse_bootstrapBrokerStringSaslScram,
    getBootstrapBrokersResponse_bootstrapBrokerStringTls,
    getBootstrapBrokersResponse_bootstrapBrokerStringSaslIam,
    getBootstrapBrokersResponse_httpStatus,

    -- ** UpdateClusterKafkaVersion
    updateClusterKafkaVersion_configurationInfo,
    updateClusterKafkaVersion_clusterArn,
    updateClusterKafkaVersion_targetKafkaVersion,
    updateClusterKafkaVersion_currentVersion,
    updateClusterKafkaVersionResponse_clusterArn,
    updateClusterKafkaVersionResponse_clusterOperationArn,
    updateClusterKafkaVersionResponse_httpStatus,

    -- ** UpdateSecurity
    updateSecurity_clientAuthentication,
    updateSecurity_encryptionInfo,
    updateSecurity_clusterArn,
    updateSecurity_currentVersion,
    updateSecurityResponse_clusterArn,
    updateSecurityResponse_clusterOperationArn,
    updateSecurityResponse_httpStatus,

    -- ** GetCompatibleKafkaVersions
    getCompatibleKafkaVersions_clusterArn,
    getCompatibleKafkaVersionsResponse_compatibleKafkaVersions,
    getCompatibleKafkaVersionsResponse_httpStatus,

    -- ** DescribeClusterOperation
    describeClusterOperation_clusterOperationArn,
    describeClusterOperationResponse_clusterOperationInfo,
    describeClusterOperationResponse_httpStatus,

    -- ** UpdateBrokerType
    updateBrokerType_clusterArn,
    updateBrokerType_currentVersion,
    updateBrokerType_targetInstanceType,
    updateBrokerTypeResponse_clusterArn,
    updateBrokerTypeResponse_clusterOperationArn,
    updateBrokerTypeResponse_httpStatus,

    -- ** DescribeConfiguration
    describeConfiguration_arn,
    describeConfigurationResponse_creationTime,
    describeConfigurationResponse_state,
    describeConfigurationResponse_kafkaVersions,
    describeConfigurationResponse_arn,
    describeConfigurationResponse_latestRevision,
    describeConfigurationResponse_name,
    describeConfigurationResponse_description,
    describeConfigurationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** ListClusters
    listClusters_clusterNameFilter,
    listClusters_nextToken,
    listClusters_maxResults,
    listClustersResponse_nextToken,
    listClustersResponse_clusterInfoList,
    listClustersResponse_httpStatus,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** ListClusterOperations
    listClusterOperations_nextToken,
    listClusterOperations_maxResults,
    listClusterOperations_clusterArn,
    listClusterOperationsResponse_clusterOperationInfoList,
    listClusterOperationsResponse_nextToken,
    listClusterOperationsResponse_httpStatus,

    -- ** BatchDisassociateScramSecret
    batchDisassociateScramSecret_clusterArn,
    batchDisassociateScramSecret_secretArnList,
    batchDisassociateScramSecretResponse_clusterArn,
    batchDisassociateScramSecretResponse_unprocessedScramSecrets,
    batchDisassociateScramSecretResponse_httpStatus,

    -- ** DescribeConfigurationRevision
    describeConfigurationRevision_revision,
    describeConfigurationRevision_arn,
    describeConfigurationRevisionResponse_creationTime,
    describeConfigurationRevisionResponse_serverProperties,
    describeConfigurationRevisionResponse_arn,
    describeConfigurationRevisionResponse_revision,
    describeConfigurationRevisionResponse_description,
    describeConfigurationRevisionResponse_httpStatus,

    -- ** DeleteConfiguration
    deleteConfiguration_arn,
    deleteConfigurationResponse_state,
    deleteConfigurationResponse_arn,
    deleteConfigurationResponse_httpStatus,

    -- ** UpdateConfiguration
    updateConfiguration_description,
    updateConfiguration_arn,
    updateConfiguration_serverProperties,
    updateConfigurationResponse_arn,
    updateConfigurationResponse_latestRevision,
    updateConfigurationResponse_httpStatus,

    -- ** ListNodes
    listNodes_nextToken,
    listNodes_maxResults,
    listNodes_clusterArn,
    listNodesResponse_nodeInfoList,
    listNodesResponse_nextToken,
    listNodesResponse_httpStatus,

    -- ** ListScramSecrets
    listScramSecrets_nextToken,
    listScramSecrets_maxResults,
    listScramSecrets_clusterArn,
    listScramSecretsResponse_nextToken,
    listScramSecretsResponse_secretArnList,
    listScramSecretsResponse_httpStatus,

    -- * Types

    -- ** BrokerEBSVolumeInfo
    brokerEBSVolumeInfo_volumeSizeGB,
    brokerEBSVolumeInfo_kafkaBrokerNodeId,

    -- ** BrokerLogs
    brokerLogs_cloudWatchLogs,
    brokerLogs_firehose,
    brokerLogs_s3,

    -- ** BrokerNodeGroupInfo
    brokerNodeGroupInfo_storageInfo,
    brokerNodeGroupInfo_brokerAZDistribution,
    brokerNodeGroupInfo_securityGroups,
    brokerNodeGroupInfo_clientSubnets,
    brokerNodeGroupInfo_instanceType,

    -- ** BrokerNodeInfo
    brokerNodeInfo_currentBrokerSoftwareInfo,
    brokerNodeInfo_clientSubnet,
    brokerNodeInfo_attachedENIId,
    brokerNodeInfo_endpoints,
    brokerNodeInfo_clientVpcIpAddress,
    brokerNodeInfo_brokerId,

    -- ** BrokerSoftwareInfo
    brokerSoftwareInfo_configurationRevision,
    brokerSoftwareInfo_kafkaVersion,
    brokerSoftwareInfo_configurationArn,

    -- ** ClientAuthentication
    clientAuthentication_sasl,
    clientAuthentication_tls,
    clientAuthentication_unauthenticated,

    -- ** CloudWatchLogs
    cloudWatchLogs_logGroup,
    cloudWatchLogs_enabled,

    -- ** ClusterInfo
    clusterInfo_creationTime,
    clusterInfo_activeOperationArn,
    clusterInfo_state,
    clusterInfo_clusterArn,
    clusterInfo_numberOfBrokerNodes,
    clusterInfo_enhancedMonitoring,
    clusterInfo_brokerNodeGroupInfo,
    clusterInfo_openMonitoring,
    clusterInfo_currentBrokerSoftwareInfo,
    clusterInfo_currentVersion,
    clusterInfo_stateInfo,
    clusterInfo_loggingInfo,
    clusterInfo_zookeeperConnectString,
    clusterInfo_clusterName,
    clusterInfo_zookeeperConnectStringTls,
    clusterInfo_clientAuthentication,
    clusterInfo_tags,
    clusterInfo_encryptionInfo,

    -- ** ClusterOperationInfo
    clusterOperationInfo_creationTime,
    clusterOperationInfo_clusterArn,
    clusterOperationInfo_clientRequestId,
    clusterOperationInfo_targetClusterInfo,
    clusterOperationInfo_sourceClusterInfo,
    clusterOperationInfo_operationSteps,
    clusterOperationInfo_operationState,
    clusterOperationInfo_endTime,
    clusterOperationInfo_operationType,
    clusterOperationInfo_operationArn,
    clusterOperationInfo_errorInfo,

    -- ** ClusterOperationStep
    clusterOperationStep_stepName,
    clusterOperationStep_stepInfo,

    -- ** ClusterOperationStepInfo
    clusterOperationStepInfo_stepStatus,

    -- ** CompatibleKafkaVersion
    compatibleKafkaVersion_sourceVersion,
    compatibleKafkaVersion_targetVersions,

    -- ** Configuration
    configuration_description,
    configuration_latestRevision,
    configuration_creationTime,
    configuration_kafkaVersions,
    configuration_arn,
    configuration_name,
    configuration_state,

    -- ** ConfigurationInfo
    configurationInfo_revision,
    configurationInfo_arn,

    -- ** ConfigurationRevision
    configurationRevision_description,
    configurationRevision_revision,
    configurationRevision_creationTime,

    -- ** EBSStorageInfo
    eBSStorageInfo_volumeSize,

    -- ** EncryptionAtRest
    encryptionAtRest_dataVolumeKMSKeyId,

    -- ** EncryptionInTransit
    encryptionInTransit_clientBroker,
    encryptionInTransit_inCluster,

    -- ** EncryptionInfo
    encryptionInfo_encryptionAtRest,
    encryptionInfo_encryptionInTransit,

    -- ** ErrorInfo
    errorInfo_errorString,
    errorInfo_errorCode,

    -- ** Firehose
    firehose_deliveryStream,
    firehose_enabled,

    -- ** Iam
    iam_enabled,

    -- ** JmxExporter
    jmxExporter_enabledInBroker,

    -- ** JmxExporterInfo
    jmxExporterInfo_enabledInBroker,

    -- ** KafkaVersion
    kafkaVersion_status,
    kafkaVersion_version,

    -- ** LoggingInfo
    loggingInfo_brokerLogs,

    -- ** MutableClusterInfo
    mutableClusterInfo_numberOfBrokerNodes,
    mutableClusterInfo_enhancedMonitoring,
    mutableClusterInfo_openMonitoring,
    mutableClusterInfo_configurationInfo,
    mutableClusterInfo_instanceType,
    mutableClusterInfo_kafkaVersion,
    mutableClusterInfo_loggingInfo,
    mutableClusterInfo_clientAuthentication,
    mutableClusterInfo_brokerEBSVolumeInfo,
    mutableClusterInfo_encryptionInfo,

    -- ** NodeExporter
    nodeExporter_enabledInBroker,

    -- ** NodeExporterInfo
    nodeExporterInfo_enabledInBroker,

    -- ** NodeInfo
    nodeInfo_addedToClusterTime,
    nodeInfo_nodeARN,
    nodeInfo_zookeeperNodeInfo,
    nodeInfo_instanceType,
    nodeInfo_brokerNodeInfo,
    nodeInfo_nodeType,

    -- ** OpenMonitoring
    openMonitoring_prometheus,

    -- ** OpenMonitoringInfo
    openMonitoringInfo_prometheus,

    -- ** Prometheus
    prometheus_jmxExporter,
    prometheus_nodeExporter,

    -- ** PrometheusInfo
    prometheusInfo_jmxExporter,
    prometheusInfo_nodeExporter,

    -- ** S3
    s3_prefix,
    s3_bucket,
    s3_enabled,

    -- ** Sasl
    sasl_iam,
    sasl_scram,

    -- ** Scram
    scram_enabled,

    -- ** StateInfo
    stateInfo_code,
    stateInfo_message,

    -- ** StorageInfo
    storageInfo_ebsStorageInfo,

    -- ** Tls
    tls_enabled,
    tls_certificateAuthorityArnList,

    -- ** Unauthenticated
    unauthenticated_enabled,

    -- ** UnprocessedScramSecret
    unprocessedScramSecret_errorCode,
    unprocessedScramSecret_errorMessage,
    unprocessedScramSecret_secretArn,

    -- ** ZookeeperNodeInfo
    zookeeperNodeInfo_zookeeperVersion,
    zookeeperNodeInfo_attachedENIId,
    zookeeperNodeInfo_endpoints,
    zookeeperNodeInfo_clientVpcIpAddress,
    zookeeperNodeInfo_zookeeperId,
  )
where

import Network.AWS.Kafka.BatchAssociateScramSecret
import Network.AWS.Kafka.BatchDisassociateScramSecret
import Network.AWS.Kafka.CreateCluster
import Network.AWS.Kafka.CreateConfiguration
import Network.AWS.Kafka.DeleteCluster
import Network.AWS.Kafka.DeleteConfiguration
import Network.AWS.Kafka.DescribeCluster
import Network.AWS.Kafka.DescribeClusterOperation
import Network.AWS.Kafka.DescribeConfiguration
import Network.AWS.Kafka.DescribeConfigurationRevision
import Network.AWS.Kafka.GetBootstrapBrokers
import Network.AWS.Kafka.GetCompatibleKafkaVersions
import Network.AWS.Kafka.ListClusterOperations
import Network.AWS.Kafka.ListClusters
import Network.AWS.Kafka.ListConfigurationRevisions
import Network.AWS.Kafka.ListConfigurations
import Network.AWS.Kafka.ListKafkaVersions
import Network.AWS.Kafka.ListNodes
import Network.AWS.Kafka.ListScramSecrets
import Network.AWS.Kafka.ListTagsForResource
import Network.AWS.Kafka.RebootBroker
import Network.AWS.Kafka.TagResource
import Network.AWS.Kafka.Types.BrokerEBSVolumeInfo
import Network.AWS.Kafka.Types.BrokerLogs
import Network.AWS.Kafka.Types.BrokerNodeGroupInfo
import Network.AWS.Kafka.Types.BrokerNodeInfo
import Network.AWS.Kafka.Types.BrokerSoftwareInfo
import Network.AWS.Kafka.Types.ClientAuthentication
import Network.AWS.Kafka.Types.CloudWatchLogs
import Network.AWS.Kafka.Types.ClusterInfo
import Network.AWS.Kafka.Types.ClusterOperationInfo
import Network.AWS.Kafka.Types.ClusterOperationStep
import Network.AWS.Kafka.Types.ClusterOperationStepInfo
import Network.AWS.Kafka.Types.CompatibleKafkaVersion
import Network.AWS.Kafka.Types.Configuration
import Network.AWS.Kafka.Types.ConfigurationInfo
import Network.AWS.Kafka.Types.ConfigurationRevision
import Network.AWS.Kafka.Types.EBSStorageInfo
import Network.AWS.Kafka.Types.EncryptionAtRest
import Network.AWS.Kafka.Types.EncryptionInTransit
import Network.AWS.Kafka.Types.EncryptionInfo
import Network.AWS.Kafka.Types.ErrorInfo
import Network.AWS.Kafka.Types.Firehose
import Network.AWS.Kafka.Types.Iam
import Network.AWS.Kafka.Types.JmxExporter
import Network.AWS.Kafka.Types.JmxExporterInfo
import Network.AWS.Kafka.Types.KafkaVersion
import Network.AWS.Kafka.Types.LoggingInfo
import Network.AWS.Kafka.Types.MutableClusterInfo
import Network.AWS.Kafka.Types.NodeExporter
import Network.AWS.Kafka.Types.NodeExporterInfo
import Network.AWS.Kafka.Types.NodeInfo
import Network.AWS.Kafka.Types.OpenMonitoring
import Network.AWS.Kafka.Types.OpenMonitoringInfo
import Network.AWS.Kafka.Types.Prometheus
import Network.AWS.Kafka.Types.PrometheusInfo
import Network.AWS.Kafka.Types.S3
import Network.AWS.Kafka.Types.Sasl
import Network.AWS.Kafka.Types.Scram
import Network.AWS.Kafka.Types.StateInfo
import Network.AWS.Kafka.Types.StorageInfo
import Network.AWS.Kafka.Types.Tls
import Network.AWS.Kafka.Types.Unauthenticated
import Network.AWS.Kafka.Types.UnprocessedScramSecret
import Network.AWS.Kafka.Types.ZookeeperNodeInfo
import Network.AWS.Kafka.UntagResource
import Network.AWS.Kafka.UpdateBrokerCount
import Network.AWS.Kafka.UpdateBrokerStorage
import Network.AWS.Kafka.UpdateBrokerType
import Network.AWS.Kafka.UpdateClusterConfiguration
import Network.AWS.Kafka.UpdateClusterKafkaVersion
import Network.AWS.Kafka.UpdateConfiguration
import Network.AWS.Kafka.UpdateMonitoring
import Network.AWS.Kafka.UpdateSecurity
