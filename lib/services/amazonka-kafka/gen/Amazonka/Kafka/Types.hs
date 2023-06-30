{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Kafka.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BadRequestException,
    _ConflictException,
    _ForbiddenException,
    _InternalServerErrorException,
    _NotFoundException,
    _ServiceUnavailableException,
    _TooManyRequestsException,
    _UnauthorizedException,

    -- * BrokerAZDistribution
    BrokerAZDistribution (..),

    -- * ClientBroker
    ClientBroker (..),

    -- * ClusterState
    ClusterState (..),

    -- * ClusterType
    ClusterType (..),

    -- * ConfigurationState
    ConfigurationState (..),

    -- * EnhancedMonitoring
    EnhancedMonitoring (..),

    -- * KafkaVersionStatus
    KafkaVersionStatus (..),

    -- * NodeType
    NodeType (..),

    -- * StorageMode
    StorageMode (..),

    -- * BrokerEBSVolumeInfo
    BrokerEBSVolumeInfo (..),
    newBrokerEBSVolumeInfo,
    brokerEBSVolumeInfo_provisionedThroughput,
    brokerEBSVolumeInfo_volumeSizeGB,
    brokerEBSVolumeInfo_kafkaBrokerNodeId,

    -- * BrokerLogs
    BrokerLogs (..),
    newBrokerLogs,
    brokerLogs_cloudWatchLogs,
    brokerLogs_firehose,
    brokerLogs_s3,

    -- * BrokerNodeGroupInfo
    BrokerNodeGroupInfo (..),
    newBrokerNodeGroupInfo,
    brokerNodeGroupInfo_brokerAZDistribution,
    brokerNodeGroupInfo_connectivityInfo,
    brokerNodeGroupInfo_securityGroups,
    brokerNodeGroupInfo_storageInfo,
    brokerNodeGroupInfo_clientSubnets,
    brokerNodeGroupInfo_instanceType,

    -- * BrokerNodeInfo
    BrokerNodeInfo (..),
    newBrokerNodeInfo,
    brokerNodeInfo_attachedENIId,
    brokerNodeInfo_brokerId,
    brokerNodeInfo_clientSubnet,
    brokerNodeInfo_clientVpcIpAddress,
    brokerNodeInfo_currentBrokerSoftwareInfo,
    brokerNodeInfo_endpoints,

    -- * BrokerSoftwareInfo
    BrokerSoftwareInfo (..),
    newBrokerSoftwareInfo,
    brokerSoftwareInfo_configurationArn,
    brokerSoftwareInfo_configurationRevision,
    brokerSoftwareInfo_kafkaVersion,

    -- * ClientAuthentication
    ClientAuthentication (..),
    newClientAuthentication,
    clientAuthentication_sasl,
    clientAuthentication_tls,
    clientAuthentication_unauthenticated,

    -- * CloudWatchLogs
    CloudWatchLogs (..),
    newCloudWatchLogs,
    cloudWatchLogs_logGroup,
    cloudWatchLogs_enabled,

    -- * Cluster
    Cluster (..),
    newCluster,
    cluster_activeOperationArn,
    cluster_clusterArn,
    cluster_clusterName,
    cluster_clusterType,
    cluster_creationTime,
    cluster_currentVersion,
    cluster_provisioned,
    cluster_serverless,
    cluster_state,
    cluster_stateInfo,
    cluster_tags,

    -- * ClusterInfo
    ClusterInfo (..),
    newClusterInfo,
    clusterInfo_activeOperationArn,
    clusterInfo_brokerNodeGroupInfo,
    clusterInfo_clientAuthentication,
    clusterInfo_clusterArn,
    clusterInfo_clusterName,
    clusterInfo_creationTime,
    clusterInfo_currentBrokerSoftwareInfo,
    clusterInfo_currentVersion,
    clusterInfo_encryptionInfo,
    clusterInfo_enhancedMonitoring,
    clusterInfo_loggingInfo,
    clusterInfo_numberOfBrokerNodes,
    clusterInfo_openMonitoring,
    clusterInfo_state,
    clusterInfo_stateInfo,
    clusterInfo_storageMode,
    clusterInfo_tags,
    clusterInfo_zookeeperConnectString,
    clusterInfo_zookeeperConnectStringTls,

    -- * ClusterOperationInfo
    ClusterOperationInfo (..),
    newClusterOperationInfo,
    clusterOperationInfo_clientRequestId,
    clusterOperationInfo_clusterArn,
    clusterOperationInfo_creationTime,
    clusterOperationInfo_endTime,
    clusterOperationInfo_errorInfo,
    clusterOperationInfo_operationArn,
    clusterOperationInfo_operationState,
    clusterOperationInfo_operationSteps,
    clusterOperationInfo_operationType,
    clusterOperationInfo_sourceClusterInfo,
    clusterOperationInfo_targetClusterInfo,

    -- * ClusterOperationStep
    ClusterOperationStep (..),
    newClusterOperationStep,
    clusterOperationStep_stepInfo,
    clusterOperationStep_stepName,

    -- * ClusterOperationStepInfo
    ClusterOperationStepInfo (..),
    newClusterOperationStepInfo,
    clusterOperationStepInfo_stepStatus,

    -- * CompatibleKafkaVersion
    CompatibleKafkaVersion (..),
    newCompatibleKafkaVersion,
    compatibleKafkaVersion_sourceVersion,
    compatibleKafkaVersion_targetVersions,

    -- * Configuration
    Configuration (..),
    newConfiguration,
    configuration_description,
    configuration_latestRevision,
    configuration_creationTime,
    configuration_kafkaVersions,
    configuration_arn,
    configuration_name,
    configuration_state,

    -- * ConfigurationInfo
    ConfigurationInfo (..),
    newConfigurationInfo,
    configurationInfo_revision,
    configurationInfo_arn,

    -- * ConfigurationRevision
    ConfigurationRevision (..),
    newConfigurationRevision,
    configurationRevision_description,
    configurationRevision_revision,
    configurationRevision_creationTime,

    -- * ConnectivityInfo
    ConnectivityInfo (..),
    newConnectivityInfo,
    connectivityInfo_publicAccess,

    -- * EBSStorageInfo
    EBSStorageInfo (..),
    newEBSStorageInfo,
    eBSStorageInfo_provisionedThroughput,
    eBSStorageInfo_volumeSize,

    -- * EncryptionAtRest
    EncryptionAtRest (..),
    newEncryptionAtRest,
    encryptionAtRest_dataVolumeKMSKeyId,

    -- * EncryptionInTransit
    EncryptionInTransit (..),
    newEncryptionInTransit,
    encryptionInTransit_clientBroker,
    encryptionInTransit_inCluster,

    -- * EncryptionInfo
    EncryptionInfo (..),
    newEncryptionInfo,
    encryptionInfo_encryptionAtRest,
    encryptionInfo_encryptionInTransit,

    -- * ErrorInfo
    ErrorInfo (..),
    newErrorInfo,
    errorInfo_errorCode,
    errorInfo_errorString,

    -- * Firehose
    Firehose (..),
    newFirehose,
    firehose_deliveryStream,
    firehose_enabled,

    -- * Iam
    Iam (..),
    newIam,
    iam_enabled,

    -- * JmxExporter
    JmxExporter (..),
    newJmxExporter,
    jmxExporter_enabledInBroker,

    -- * JmxExporterInfo
    JmxExporterInfo (..),
    newJmxExporterInfo,
    jmxExporterInfo_enabledInBroker,

    -- * KafkaVersion
    KafkaVersion (..),
    newKafkaVersion,
    kafkaVersion_status,
    kafkaVersion_version,

    -- * LoggingInfo
    LoggingInfo (..),
    newLoggingInfo,
    loggingInfo_brokerLogs,

    -- * MutableClusterInfo
    MutableClusterInfo (..),
    newMutableClusterInfo,
    mutableClusterInfo_brokerEBSVolumeInfo,
    mutableClusterInfo_clientAuthentication,
    mutableClusterInfo_configurationInfo,
    mutableClusterInfo_connectivityInfo,
    mutableClusterInfo_encryptionInfo,
    mutableClusterInfo_enhancedMonitoring,
    mutableClusterInfo_instanceType,
    mutableClusterInfo_kafkaVersion,
    mutableClusterInfo_loggingInfo,
    mutableClusterInfo_numberOfBrokerNodes,
    mutableClusterInfo_openMonitoring,
    mutableClusterInfo_storageMode,

    -- * NodeExporter
    NodeExporter (..),
    newNodeExporter,
    nodeExporter_enabledInBroker,

    -- * NodeExporterInfo
    NodeExporterInfo (..),
    newNodeExporterInfo,
    nodeExporterInfo_enabledInBroker,

    -- * NodeInfo
    NodeInfo (..),
    newNodeInfo,
    nodeInfo_addedToClusterTime,
    nodeInfo_brokerNodeInfo,
    nodeInfo_instanceType,
    nodeInfo_nodeARN,
    nodeInfo_nodeType,
    nodeInfo_zookeeperNodeInfo,

    -- * OpenMonitoring
    OpenMonitoring (..),
    newOpenMonitoring,
    openMonitoring_prometheus,

    -- * OpenMonitoringInfo
    OpenMonitoringInfo (..),
    newOpenMonitoringInfo,
    openMonitoringInfo_prometheus,

    -- * Prometheus
    Prometheus (..),
    newPrometheus,
    prometheus_jmxExporter,
    prometheus_nodeExporter,

    -- * PrometheusInfo
    PrometheusInfo (..),
    newPrometheusInfo,
    prometheusInfo_jmxExporter,
    prometheusInfo_nodeExporter,

    -- * Provisioned
    Provisioned (..),
    newProvisioned,
    provisioned_clientAuthentication,
    provisioned_currentBrokerSoftwareInfo,
    provisioned_encryptionInfo,
    provisioned_enhancedMonitoring,
    provisioned_loggingInfo,
    provisioned_openMonitoring,
    provisioned_storageMode,
    provisioned_zookeeperConnectString,
    provisioned_zookeeperConnectStringTls,
    provisioned_brokerNodeGroupInfo,
    provisioned_numberOfBrokerNodes,

    -- * ProvisionedRequest
    ProvisionedRequest (..),
    newProvisionedRequest,
    provisionedRequest_clientAuthentication,
    provisionedRequest_configurationInfo,
    provisionedRequest_encryptionInfo,
    provisionedRequest_enhancedMonitoring,
    provisionedRequest_loggingInfo,
    provisionedRequest_openMonitoring,
    provisionedRequest_storageMode,
    provisionedRequest_brokerNodeGroupInfo,
    provisionedRequest_kafkaVersion,
    provisionedRequest_numberOfBrokerNodes,

    -- * ProvisionedThroughput
    ProvisionedThroughput (..),
    newProvisionedThroughput,
    provisionedThroughput_enabled,
    provisionedThroughput_volumeThroughput,

    -- * PublicAccess
    PublicAccess (..),
    newPublicAccess,
    publicAccess_type,

    -- * S3
    S3 (..),
    newS3,
    s3_bucket,
    s3_prefix,
    s3_enabled,

    -- * Sasl
    Sasl (..),
    newSasl,
    sasl_iam,
    sasl_scram,

    -- * Scram
    Scram (..),
    newScram,
    scram_enabled,

    -- * Serverless
    Serverless (..),
    newServerless,
    serverless_clientAuthentication,
    serverless_vpcConfigs,

    -- * ServerlessClientAuthentication
    ServerlessClientAuthentication (..),
    newServerlessClientAuthentication,
    serverlessClientAuthentication_sasl,

    -- * ServerlessRequest
    ServerlessRequest (..),
    newServerlessRequest,
    serverlessRequest_clientAuthentication,
    serverlessRequest_vpcConfigs,

    -- * ServerlessSasl
    ServerlessSasl (..),
    newServerlessSasl,
    serverlessSasl_iam,

    -- * StateInfo
    StateInfo (..),
    newStateInfo,
    stateInfo_code,
    stateInfo_message,

    -- * StorageInfo
    StorageInfo (..),
    newStorageInfo,
    storageInfo_ebsStorageInfo,

    -- * Tls
    Tls (..),
    newTls,
    tls_certificateAuthorityArnList,
    tls_enabled,

    -- * Unauthenticated
    Unauthenticated (..),
    newUnauthenticated,
    unauthenticated_enabled,

    -- * UnprocessedScramSecret
    UnprocessedScramSecret (..),
    newUnprocessedScramSecret,
    unprocessedScramSecret_errorCode,
    unprocessedScramSecret_errorMessage,
    unprocessedScramSecret_secretArn,

    -- * VpcConfig
    VpcConfig (..),
    newVpcConfig,
    vpcConfig_securityGroupIds,
    vpcConfig_subnetIds,

    -- * ZookeeperNodeInfo
    ZookeeperNodeInfo (..),
    newZookeeperNodeInfo,
    zookeeperNodeInfo_attachedENIId,
    zookeeperNodeInfo_clientVpcIpAddress,
    zookeeperNodeInfo_endpoints,
    zookeeperNodeInfo_zookeeperId,
    zookeeperNodeInfo_zookeeperVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kafka.Types.BrokerAZDistribution
import Amazonka.Kafka.Types.BrokerEBSVolumeInfo
import Amazonka.Kafka.Types.BrokerLogs
import Amazonka.Kafka.Types.BrokerNodeGroupInfo
import Amazonka.Kafka.Types.BrokerNodeInfo
import Amazonka.Kafka.Types.BrokerSoftwareInfo
import Amazonka.Kafka.Types.ClientAuthentication
import Amazonka.Kafka.Types.ClientBroker
import Amazonka.Kafka.Types.CloudWatchLogs
import Amazonka.Kafka.Types.Cluster
import Amazonka.Kafka.Types.ClusterInfo
import Amazonka.Kafka.Types.ClusterOperationInfo
import Amazonka.Kafka.Types.ClusterOperationStep
import Amazonka.Kafka.Types.ClusterOperationStepInfo
import Amazonka.Kafka.Types.ClusterState
import Amazonka.Kafka.Types.ClusterType
import Amazonka.Kafka.Types.CompatibleKafkaVersion
import Amazonka.Kafka.Types.Configuration
import Amazonka.Kafka.Types.ConfigurationInfo
import Amazonka.Kafka.Types.ConfigurationRevision
import Amazonka.Kafka.Types.ConfigurationState
import Amazonka.Kafka.Types.ConnectivityInfo
import Amazonka.Kafka.Types.EBSStorageInfo
import Amazonka.Kafka.Types.EncryptionAtRest
import Amazonka.Kafka.Types.EncryptionInTransit
import Amazonka.Kafka.Types.EncryptionInfo
import Amazonka.Kafka.Types.EnhancedMonitoring
import Amazonka.Kafka.Types.ErrorInfo
import Amazonka.Kafka.Types.Firehose
import Amazonka.Kafka.Types.Iam
import Amazonka.Kafka.Types.JmxExporter
import Amazonka.Kafka.Types.JmxExporterInfo
import Amazonka.Kafka.Types.KafkaVersion
import Amazonka.Kafka.Types.KafkaVersionStatus
import Amazonka.Kafka.Types.LoggingInfo
import Amazonka.Kafka.Types.MutableClusterInfo
import Amazonka.Kafka.Types.NodeExporter
import Amazonka.Kafka.Types.NodeExporterInfo
import Amazonka.Kafka.Types.NodeInfo
import Amazonka.Kafka.Types.NodeType
import Amazonka.Kafka.Types.OpenMonitoring
import Amazonka.Kafka.Types.OpenMonitoringInfo
import Amazonka.Kafka.Types.Prometheus
import Amazonka.Kafka.Types.PrometheusInfo
import Amazonka.Kafka.Types.Provisioned
import Amazonka.Kafka.Types.ProvisionedRequest
import Amazonka.Kafka.Types.ProvisionedThroughput
import Amazonka.Kafka.Types.PublicAccess
import Amazonka.Kafka.Types.S3
import Amazonka.Kafka.Types.Sasl
import Amazonka.Kafka.Types.Scram
import Amazonka.Kafka.Types.Serverless
import Amazonka.Kafka.Types.ServerlessClientAuthentication
import Amazonka.Kafka.Types.ServerlessRequest
import Amazonka.Kafka.Types.ServerlessSasl
import Amazonka.Kafka.Types.StateInfo
import Amazonka.Kafka.Types.StorageInfo
import Amazonka.Kafka.Types.StorageMode
import Amazonka.Kafka.Types.Tls
import Amazonka.Kafka.Types.Unauthenticated
import Amazonka.Kafka.Types.UnprocessedScramSecret
import Amazonka.Kafka.Types.VpcConfig
import Amazonka.Kafka.Types.ZookeeperNodeInfo
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-11-14@ of the Amazon Managed Streaming for Kafka SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Kafka",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "kafka",
      Core.signingName = "kafka",
      Core.version = "2018-11-14",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Kafka",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Returns information about an error.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | Returns information about an error.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Returns information about an error.
_ForbiddenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | Returns information about an error.
_InternalServerErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | Returns information about an error.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | Returns information about an error.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | Returns information about an error.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | Returns information about an error.
_UnauthorizedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401
