{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kafka.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kafka.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConflictException,
    _ForbiddenException,
    _NotFoundException,
    _TooManyRequestsException,
    _InternalServerErrorException,
    _ServiceUnavailableException,
    _UnauthorizedException,
    _BadRequestException,

    -- * BrokerAZDistribution
    BrokerAZDistribution (..),

    -- * ClientBroker
    ClientBroker (..),

    -- * ClusterState
    ClusterState (..),

    -- * ConfigurationState
    ConfigurationState (..),

    -- * EnhancedMonitoring
    EnhancedMonitoring (..),

    -- * KafkaVersionStatus
    KafkaVersionStatus (..),

    -- * NodeType
    NodeType (..),

    -- * BrokerEBSVolumeInfo
    BrokerEBSVolumeInfo (..),
    newBrokerEBSVolumeInfo,
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
    brokerNodeGroupInfo_storageInfo,
    brokerNodeGroupInfo_brokerAZDistribution,
    brokerNodeGroupInfo_securityGroups,
    brokerNodeGroupInfo_clientSubnets,
    brokerNodeGroupInfo_instanceType,

    -- * BrokerNodeInfo
    BrokerNodeInfo (..),
    newBrokerNodeInfo,
    brokerNodeInfo_currentBrokerSoftwareInfo,
    brokerNodeInfo_clientSubnet,
    brokerNodeInfo_attachedENIId,
    brokerNodeInfo_endpoints,
    brokerNodeInfo_clientVpcIpAddress,
    brokerNodeInfo_brokerId,

    -- * BrokerSoftwareInfo
    BrokerSoftwareInfo (..),
    newBrokerSoftwareInfo,
    brokerSoftwareInfo_configurationRevision,
    brokerSoftwareInfo_kafkaVersion,
    brokerSoftwareInfo_configurationArn,

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

    -- * ClusterInfo
    ClusterInfo (..),
    newClusterInfo,
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

    -- * ClusterOperationInfo
    ClusterOperationInfo (..),
    newClusterOperationInfo,
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

    -- * ClusterOperationStep
    ClusterOperationStep (..),
    newClusterOperationStep,
    clusterOperationStep_stepName,
    clusterOperationStep_stepInfo,

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

    -- * EBSStorageInfo
    EBSStorageInfo (..),
    newEBSStorageInfo,
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
    errorInfo_errorString,
    errorInfo_errorCode,

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
    nodeInfo_nodeARN,
    nodeInfo_zookeeperNodeInfo,
    nodeInfo_instanceType,
    nodeInfo_brokerNodeInfo,
    nodeInfo_nodeType,

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

    -- * S3
    S3 (..),
    newS3,
    s3_prefix,
    s3_bucket,
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
    tls_enabled,
    tls_certificateAuthorityArnList,

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

    -- * ZookeeperNodeInfo
    ZookeeperNodeInfo (..),
    newZookeeperNodeInfo,
    zookeeperNodeInfo_zookeeperVersion,
    zookeeperNodeInfo_attachedENIId,
    zookeeperNodeInfo_endpoints,
    zookeeperNodeInfo_clientVpcIpAddress,
    zookeeperNodeInfo_zookeeperId,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kafka.Types.BrokerAZDistribution
import Network.AWS.Kafka.Types.BrokerEBSVolumeInfo
import Network.AWS.Kafka.Types.BrokerLogs
import Network.AWS.Kafka.Types.BrokerNodeGroupInfo
import Network.AWS.Kafka.Types.BrokerNodeInfo
import Network.AWS.Kafka.Types.BrokerSoftwareInfo
import Network.AWS.Kafka.Types.ClientAuthentication
import Network.AWS.Kafka.Types.ClientBroker
import Network.AWS.Kafka.Types.CloudWatchLogs
import Network.AWS.Kafka.Types.ClusterInfo
import Network.AWS.Kafka.Types.ClusterOperationInfo
import Network.AWS.Kafka.Types.ClusterOperationStep
import Network.AWS.Kafka.Types.ClusterOperationStepInfo
import Network.AWS.Kafka.Types.ClusterState
import Network.AWS.Kafka.Types.CompatibleKafkaVersion
import Network.AWS.Kafka.Types.Configuration
import Network.AWS.Kafka.Types.ConfigurationInfo
import Network.AWS.Kafka.Types.ConfigurationRevision
import Network.AWS.Kafka.Types.ConfigurationState
import Network.AWS.Kafka.Types.EBSStorageInfo
import Network.AWS.Kafka.Types.EncryptionAtRest
import Network.AWS.Kafka.Types.EncryptionInTransit
import Network.AWS.Kafka.Types.EncryptionInfo
import Network.AWS.Kafka.Types.EnhancedMonitoring
import Network.AWS.Kafka.Types.ErrorInfo
import Network.AWS.Kafka.Types.Firehose
import Network.AWS.Kafka.Types.Iam
import Network.AWS.Kafka.Types.JmxExporter
import Network.AWS.Kafka.Types.JmxExporterInfo
import Network.AWS.Kafka.Types.KafkaVersion
import Network.AWS.Kafka.Types.KafkaVersionStatus
import Network.AWS.Kafka.Types.LoggingInfo
import Network.AWS.Kafka.Types.MutableClusterInfo
import Network.AWS.Kafka.Types.NodeExporter
import Network.AWS.Kafka.Types.NodeExporterInfo
import Network.AWS.Kafka.Types.NodeInfo
import Network.AWS.Kafka.Types.NodeType
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-11-14@ of the Amazon Managed Streaming for Kafka SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Kafka",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "kafka",
      Core._serviceSigningName = "kafka",
      Core._serviceVersion = "2018-11-14",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Kafka",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Returns information about an error.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Returns information about an error.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | Returns information about an error.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | Returns information about an error.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | Returns information about an error.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | Returns information about an error.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | Returns information about an error.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401

-- | Returns information about an error.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
