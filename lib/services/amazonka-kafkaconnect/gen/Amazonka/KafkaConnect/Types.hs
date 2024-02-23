{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KafkaConnect.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types
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

    -- * ConnectorState
    ConnectorState (..),

    -- * CustomPluginContentType
    CustomPluginContentType (..),

    -- * CustomPluginState
    CustomPluginState (..),

    -- * KafkaClusterClientAuthenticationType
    KafkaClusterClientAuthenticationType (..),

    -- * KafkaClusterEncryptionInTransitType
    KafkaClusterEncryptionInTransitType (..),

    -- * ApacheKafkaCluster
    ApacheKafkaCluster (..),
    newApacheKafkaCluster,
    apacheKafkaCluster_bootstrapServers,
    apacheKafkaCluster_vpc,

    -- * ApacheKafkaClusterDescription
    ApacheKafkaClusterDescription (..),
    newApacheKafkaClusterDescription,
    apacheKafkaClusterDescription_bootstrapServers,
    apacheKafkaClusterDescription_vpc,

    -- * AutoScaling
    AutoScaling (..),
    newAutoScaling,
    autoScaling_scaleInPolicy,
    autoScaling_scaleOutPolicy,
    autoScaling_maxWorkerCount,
    autoScaling_mcuCount,
    autoScaling_minWorkerCount,

    -- * AutoScalingDescription
    AutoScalingDescription (..),
    newAutoScalingDescription,
    autoScalingDescription_maxWorkerCount,
    autoScalingDescription_mcuCount,
    autoScalingDescription_minWorkerCount,
    autoScalingDescription_scaleInPolicy,
    autoScalingDescription_scaleOutPolicy,

    -- * AutoScalingUpdate
    AutoScalingUpdate (..),
    newAutoScalingUpdate,
    autoScalingUpdate_maxWorkerCount,
    autoScalingUpdate_mcuCount,
    autoScalingUpdate_minWorkerCount,
    autoScalingUpdate_scaleInPolicy,
    autoScalingUpdate_scaleOutPolicy,

    -- * Capacity
    Capacity (..),
    newCapacity,
    capacity_autoScaling,
    capacity_provisionedCapacity,

    -- * CapacityDescription
    CapacityDescription (..),
    newCapacityDescription,
    capacityDescription_autoScaling,
    capacityDescription_provisionedCapacity,

    -- * CapacityUpdate
    CapacityUpdate (..),
    newCapacityUpdate,
    capacityUpdate_autoScaling,
    capacityUpdate_provisionedCapacity,

    -- * CloudWatchLogsLogDelivery
    CloudWatchLogsLogDelivery (..),
    newCloudWatchLogsLogDelivery,
    cloudWatchLogsLogDelivery_logGroup,
    cloudWatchLogsLogDelivery_enabled,

    -- * CloudWatchLogsLogDeliveryDescription
    CloudWatchLogsLogDeliveryDescription (..),
    newCloudWatchLogsLogDeliveryDescription,
    cloudWatchLogsLogDeliveryDescription_enabled,
    cloudWatchLogsLogDeliveryDescription_logGroup,

    -- * ConnectorSummary
    ConnectorSummary (..),
    newConnectorSummary,
    connectorSummary_capacity,
    connectorSummary_connectorArn,
    connectorSummary_connectorDescription,
    connectorSummary_connectorName,
    connectorSummary_connectorState,
    connectorSummary_creationTime,
    connectorSummary_currentVersion,
    connectorSummary_kafkaCluster,
    connectorSummary_kafkaClusterClientAuthentication,
    connectorSummary_kafkaClusterEncryptionInTransit,
    connectorSummary_kafkaConnectVersion,
    connectorSummary_logDelivery,
    connectorSummary_plugins,
    connectorSummary_serviceExecutionRoleArn,
    connectorSummary_workerConfiguration,

    -- * CustomPlugin
    CustomPlugin (..),
    newCustomPlugin,
    customPlugin_customPluginArn,
    customPlugin_revision,

    -- * CustomPluginDescription
    CustomPluginDescription (..),
    newCustomPluginDescription,
    customPluginDescription_customPluginArn,
    customPluginDescription_revision,

    -- * CustomPluginFileDescription
    CustomPluginFileDescription (..),
    newCustomPluginFileDescription,
    customPluginFileDescription_fileMd5,
    customPluginFileDescription_fileSize,

    -- * CustomPluginLocation
    CustomPluginLocation (..),
    newCustomPluginLocation,
    customPluginLocation_s3Location,

    -- * CustomPluginLocationDescription
    CustomPluginLocationDescription (..),
    newCustomPluginLocationDescription,
    customPluginLocationDescription_s3Location,

    -- * CustomPluginRevisionSummary
    CustomPluginRevisionSummary (..),
    newCustomPluginRevisionSummary,
    customPluginRevisionSummary_contentType,
    customPluginRevisionSummary_creationTime,
    customPluginRevisionSummary_description,
    customPluginRevisionSummary_fileDescription,
    customPluginRevisionSummary_location,
    customPluginRevisionSummary_revision,

    -- * CustomPluginSummary
    CustomPluginSummary (..),
    newCustomPluginSummary,
    customPluginSummary_creationTime,
    customPluginSummary_customPluginArn,
    customPluginSummary_customPluginState,
    customPluginSummary_description,
    customPluginSummary_latestRevision,
    customPluginSummary_name,

    -- * FirehoseLogDelivery
    FirehoseLogDelivery (..),
    newFirehoseLogDelivery,
    firehoseLogDelivery_deliveryStream,
    firehoseLogDelivery_enabled,

    -- * FirehoseLogDeliveryDescription
    FirehoseLogDeliveryDescription (..),
    newFirehoseLogDeliveryDescription,
    firehoseLogDeliveryDescription_deliveryStream,
    firehoseLogDeliveryDescription_enabled,

    -- * KafkaCluster
    KafkaCluster (..),
    newKafkaCluster,
    kafkaCluster_apacheKafkaCluster,

    -- * KafkaClusterClientAuthentication
    KafkaClusterClientAuthentication (..),
    newKafkaClusterClientAuthentication,
    kafkaClusterClientAuthentication_authenticationType,

    -- * KafkaClusterClientAuthenticationDescription
    KafkaClusterClientAuthenticationDescription (..),
    newKafkaClusterClientAuthenticationDescription,
    kafkaClusterClientAuthenticationDescription_authenticationType,

    -- * KafkaClusterDescription
    KafkaClusterDescription (..),
    newKafkaClusterDescription,
    kafkaClusterDescription_apacheKafkaCluster,

    -- * KafkaClusterEncryptionInTransit
    KafkaClusterEncryptionInTransit (..),
    newKafkaClusterEncryptionInTransit,
    kafkaClusterEncryptionInTransit_encryptionType,

    -- * KafkaClusterEncryptionInTransitDescription
    KafkaClusterEncryptionInTransitDescription (..),
    newKafkaClusterEncryptionInTransitDescription,
    kafkaClusterEncryptionInTransitDescription_encryptionType,

    -- * LogDelivery
    LogDelivery (..),
    newLogDelivery,
    logDelivery_workerLogDelivery,

    -- * LogDeliveryDescription
    LogDeliveryDescription (..),
    newLogDeliveryDescription,
    logDeliveryDescription_workerLogDelivery,

    -- * Plugin
    Plugin (..),
    newPlugin,
    plugin_customPlugin,

    -- * PluginDescription
    PluginDescription (..),
    newPluginDescription,
    pluginDescription_customPlugin,

    -- * ProvisionedCapacity
    ProvisionedCapacity (..),
    newProvisionedCapacity,
    provisionedCapacity_mcuCount,
    provisionedCapacity_workerCount,

    -- * ProvisionedCapacityDescription
    ProvisionedCapacityDescription (..),
    newProvisionedCapacityDescription,
    provisionedCapacityDescription_mcuCount,
    provisionedCapacityDescription_workerCount,

    -- * ProvisionedCapacityUpdate
    ProvisionedCapacityUpdate (..),
    newProvisionedCapacityUpdate,
    provisionedCapacityUpdate_mcuCount,
    provisionedCapacityUpdate_workerCount,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_objectVersion,
    s3Location_bucketArn,
    s3Location_fileKey,

    -- * S3LocationDescription
    S3LocationDescription (..),
    newS3LocationDescription,
    s3LocationDescription_bucketArn,
    s3LocationDescription_fileKey,
    s3LocationDescription_objectVersion,

    -- * S3LogDelivery
    S3LogDelivery (..),
    newS3LogDelivery,
    s3LogDelivery_bucket,
    s3LogDelivery_prefix,
    s3LogDelivery_enabled,

    -- * S3LogDeliveryDescription
    S3LogDeliveryDescription (..),
    newS3LogDeliveryDescription,
    s3LogDeliveryDescription_bucket,
    s3LogDeliveryDescription_enabled,
    s3LogDeliveryDescription_prefix,

    -- * ScaleInPolicy
    ScaleInPolicy (..),
    newScaleInPolicy,
    scaleInPolicy_cpuUtilizationPercentage,

    -- * ScaleInPolicyDescription
    ScaleInPolicyDescription (..),
    newScaleInPolicyDescription,
    scaleInPolicyDescription_cpuUtilizationPercentage,

    -- * ScaleInPolicyUpdate
    ScaleInPolicyUpdate (..),
    newScaleInPolicyUpdate,
    scaleInPolicyUpdate_cpuUtilizationPercentage,

    -- * ScaleOutPolicy
    ScaleOutPolicy (..),
    newScaleOutPolicy,
    scaleOutPolicy_cpuUtilizationPercentage,

    -- * ScaleOutPolicyDescription
    ScaleOutPolicyDescription (..),
    newScaleOutPolicyDescription,
    scaleOutPolicyDescription_cpuUtilizationPercentage,

    -- * ScaleOutPolicyUpdate
    ScaleOutPolicyUpdate (..),
    newScaleOutPolicyUpdate,
    scaleOutPolicyUpdate_cpuUtilizationPercentage,

    -- * StateDescription
    StateDescription (..),
    newStateDescription,
    stateDescription_code,
    stateDescription_message,

    -- * Vpc
    Vpc (..),
    newVpc,
    vpc_securityGroups,
    vpc_subnets,

    -- * VpcDescription
    VpcDescription (..),
    newVpcDescription,
    vpcDescription_securityGroups,
    vpcDescription_subnets,

    -- * WorkerConfiguration
    WorkerConfiguration (..),
    newWorkerConfiguration,
    workerConfiguration_revision,
    workerConfiguration_workerConfigurationArn,

    -- * WorkerConfigurationDescription
    WorkerConfigurationDescription (..),
    newWorkerConfigurationDescription,
    workerConfigurationDescription_revision,
    workerConfigurationDescription_workerConfigurationArn,

    -- * WorkerConfigurationRevisionDescription
    WorkerConfigurationRevisionDescription (..),
    newWorkerConfigurationRevisionDescription,
    workerConfigurationRevisionDescription_creationTime,
    workerConfigurationRevisionDescription_description,
    workerConfigurationRevisionDescription_propertiesFileContent,
    workerConfigurationRevisionDescription_revision,

    -- * WorkerConfigurationRevisionSummary
    WorkerConfigurationRevisionSummary (..),
    newWorkerConfigurationRevisionSummary,
    workerConfigurationRevisionSummary_creationTime,
    workerConfigurationRevisionSummary_description,
    workerConfigurationRevisionSummary_revision,

    -- * WorkerConfigurationSummary
    WorkerConfigurationSummary (..),
    newWorkerConfigurationSummary,
    workerConfigurationSummary_creationTime,
    workerConfigurationSummary_description,
    workerConfigurationSummary_latestRevision,
    workerConfigurationSummary_name,
    workerConfigurationSummary_workerConfigurationArn,

    -- * WorkerLogDelivery
    WorkerLogDelivery (..),
    newWorkerLogDelivery,
    workerLogDelivery_cloudWatchLogs,
    workerLogDelivery_firehose,
    workerLogDelivery_s3,

    -- * WorkerLogDeliveryDescription
    WorkerLogDeliveryDescription (..),
    newWorkerLogDeliveryDescription,
    workerLogDeliveryDescription_cloudWatchLogs,
    workerLogDeliveryDescription_firehose,
    workerLogDeliveryDescription_s3,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KafkaConnect.Types.ApacheKafkaCluster
import Amazonka.KafkaConnect.Types.ApacheKafkaClusterDescription
import Amazonka.KafkaConnect.Types.AutoScaling
import Amazonka.KafkaConnect.Types.AutoScalingDescription
import Amazonka.KafkaConnect.Types.AutoScalingUpdate
import Amazonka.KafkaConnect.Types.Capacity
import Amazonka.KafkaConnect.Types.CapacityDescription
import Amazonka.KafkaConnect.Types.CapacityUpdate
import Amazonka.KafkaConnect.Types.CloudWatchLogsLogDelivery
import Amazonka.KafkaConnect.Types.CloudWatchLogsLogDeliveryDescription
import Amazonka.KafkaConnect.Types.ConnectorState
import Amazonka.KafkaConnect.Types.ConnectorSummary
import Amazonka.KafkaConnect.Types.CustomPlugin
import Amazonka.KafkaConnect.Types.CustomPluginContentType
import Amazonka.KafkaConnect.Types.CustomPluginDescription
import Amazonka.KafkaConnect.Types.CustomPluginFileDescription
import Amazonka.KafkaConnect.Types.CustomPluginLocation
import Amazonka.KafkaConnect.Types.CustomPluginLocationDescription
import Amazonka.KafkaConnect.Types.CustomPluginRevisionSummary
import Amazonka.KafkaConnect.Types.CustomPluginState
import Amazonka.KafkaConnect.Types.CustomPluginSummary
import Amazonka.KafkaConnect.Types.FirehoseLogDelivery
import Amazonka.KafkaConnect.Types.FirehoseLogDeliveryDescription
import Amazonka.KafkaConnect.Types.KafkaCluster
import Amazonka.KafkaConnect.Types.KafkaClusterClientAuthentication
import Amazonka.KafkaConnect.Types.KafkaClusterClientAuthenticationDescription
import Amazonka.KafkaConnect.Types.KafkaClusterClientAuthenticationType
import Amazonka.KafkaConnect.Types.KafkaClusterDescription
import Amazonka.KafkaConnect.Types.KafkaClusterEncryptionInTransit
import Amazonka.KafkaConnect.Types.KafkaClusterEncryptionInTransitDescription
import Amazonka.KafkaConnect.Types.KafkaClusterEncryptionInTransitType
import Amazonka.KafkaConnect.Types.LogDelivery
import Amazonka.KafkaConnect.Types.LogDeliveryDescription
import Amazonka.KafkaConnect.Types.Plugin
import Amazonka.KafkaConnect.Types.PluginDescription
import Amazonka.KafkaConnect.Types.ProvisionedCapacity
import Amazonka.KafkaConnect.Types.ProvisionedCapacityDescription
import Amazonka.KafkaConnect.Types.ProvisionedCapacityUpdate
import Amazonka.KafkaConnect.Types.S3Location
import Amazonka.KafkaConnect.Types.S3LocationDescription
import Amazonka.KafkaConnect.Types.S3LogDelivery
import Amazonka.KafkaConnect.Types.S3LogDeliveryDescription
import Amazonka.KafkaConnect.Types.ScaleInPolicy
import Amazonka.KafkaConnect.Types.ScaleInPolicyDescription
import Amazonka.KafkaConnect.Types.ScaleInPolicyUpdate
import Amazonka.KafkaConnect.Types.ScaleOutPolicy
import Amazonka.KafkaConnect.Types.ScaleOutPolicyDescription
import Amazonka.KafkaConnect.Types.ScaleOutPolicyUpdate
import Amazonka.KafkaConnect.Types.StateDescription
import Amazonka.KafkaConnect.Types.Vpc
import Amazonka.KafkaConnect.Types.VpcDescription
import Amazonka.KafkaConnect.Types.WorkerConfiguration
import Amazonka.KafkaConnect.Types.WorkerConfigurationDescription
import Amazonka.KafkaConnect.Types.WorkerConfigurationRevisionDescription
import Amazonka.KafkaConnect.Types.WorkerConfigurationRevisionSummary
import Amazonka.KafkaConnect.Types.WorkerConfigurationSummary
import Amazonka.KafkaConnect.Types.WorkerLogDelivery
import Amazonka.KafkaConnect.Types.WorkerLogDeliveryDescription
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-09-14@ of the Amazon Managed Streaming for Kafka Connect SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "KafkaConnect",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "kafkaconnect",
      Core.signingName = "kafkaconnect",
      Core.version = "2021-09-14",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "KafkaConnect",
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

-- | HTTP Status Code 400: Bad request due to incorrect input. Correct your
-- request and then retry it.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | HTTP Status Code 409: Conflict. A resource with this name already
-- exists. Retry your request with another name.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | HTTP Status Code 403: Access forbidden. Correct your credentials and
-- then retry your request.
_ForbiddenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | HTTP Status Code 500: Unexpected internal server error. Retrying your
-- request might resolve the issue.
_InternalServerErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | HTTP Status Code 404: Resource not found due to incorrect input. Correct
-- your request and then retry it.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | HTTP Status Code 503: Service Unavailable. Retrying your request in some
-- time might resolve the issue.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | HTTP Status Code 429: Limit exceeded. Resource limit reached.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | HTTP Status Code 401: Unauthorized request. The provided credentials
-- couldn\'t be validated.
_UnauthorizedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401
