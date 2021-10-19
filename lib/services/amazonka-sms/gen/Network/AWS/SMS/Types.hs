{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ReplicationRunLimitExceededException,
    _InvalidParameterException,
    _NoConnectorsAvailableException,
    _ReplicationJobNotFoundException,
    _ServerCannotBeReplicatedException,
    _DryRunOperationException,
    _InternalError,
    _ReplicationJobAlreadyExistsException,
    _OperationNotPermittedException,
    _TemporarilyUnavailableException,
    _MissingRequiredParameterException,
    _UnauthorizedOperationException,

    -- * AppLaunchConfigurationStatus
    AppLaunchConfigurationStatus (..),

    -- * AppLaunchStatus
    AppLaunchStatus (..),

    -- * AppReplicationConfigurationStatus
    AppReplicationConfigurationStatus (..),

    -- * AppReplicationStatus
    AppReplicationStatus (..),

    -- * AppStatus
    AppStatus (..),

    -- * AppValidationStrategy
    AppValidationStrategy (..),

    -- * ConnectorCapability
    ConnectorCapability (..),

    -- * ConnectorStatus
    ConnectorStatus (..),

    -- * LicenseType
    LicenseType (..),

    -- * OutputFormat
    OutputFormat (..),

    -- * ReplicationJobState
    ReplicationJobState (..),

    -- * ReplicationRunState
    ReplicationRunState (..),

    -- * ReplicationRunType
    ReplicationRunType (..),

    -- * ScriptType
    ScriptType (..),

    -- * ServerCatalogStatus
    ServerCatalogStatus (..),

    -- * ServerType
    ServerType (..),

    -- * ServerValidationStrategy
    ServerValidationStrategy (..),

    -- * ValidationStatus
    ValidationStatus (..),

    -- * VmManagerType
    VmManagerType (..),

    -- * AppSummary
    AppSummary (..),
    newAppSummary,
    appSummary_creationTime,
    appSummary_totalServers,
    appSummary_status,
    appSummary_launchDetails,
    appSummary_launchStatusMessage,
    appSummary_replicationConfigurationStatus,
    appSummary_replicationStatusMessage,
    appSummary_totalServerGroups,
    appSummary_roleName,
    appSummary_launchConfigurationStatus,
    appSummary_launchStatus,
    appSummary_appId,
    appSummary_name,
    appSummary_statusMessage,
    appSummary_latestReplicationTime,
    appSummary_importedAppId,
    appSummary_replicationStatus,
    appSummary_lastModified,
    appSummary_description,

    -- * AppValidationConfiguration
    AppValidationConfiguration (..),
    newAppValidationConfiguration,
    appValidationConfiguration_ssmValidationParameters,
    appValidationConfiguration_name,
    appValidationConfiguration_validationId,
    appValidationConfiguration_appValidationStrategy,

    -- * AppValidationOutput
    AppValidationOutput (..),
    newAppValidationOutput,
    appValidationOutput_ssmOutput,

    -- * Connector
    Connector (..),
    newConnector,
    connector_status,
    connector_vmManagerName,
    connector_ipAddress,
    connector_vmManagerId,
    connector_vmManagerType,
    connector_connectorId,
    connector_associatedOn,
    connector_macAddress,
    connector_version,
    connector_capabilityList,

    -- * LaunchDetails
    LaunchDetails (..),
    newLaunchDetails,
    launchDetails_stackId,
    launchDetails_latestLaunchTime,
    launchDetails_stackName,

    -- * NotificationContext
    NotificationContext (..),
    newNotificationContext,
    notificationContext_status,
    notificationContext_statusMessage,
    notificationContext_validationId,

    -- * ReplicationJob
    ReplicationJob (..),
    newReplicationJob,
    replicationJob_frequency,
    replicationJob_numberOfRecentAmisToKeep,
    replicationJob_state,
    replicationJob_serverType,
    replicationJob_serverId,
    replicationJob_licenseType,
    replicationJob_roleName,
    replicationJob_vmServer,
    replicationJob_encrypted,
    replicationJob_replicationJobId,
    replicationJob_replicationRunList,
    replicationJob_nextReplicationRunStartTime,
    replicationJob_statusMessage,
    replicationJob_kmsKeyId,
    replicationJob_latestAmiId,
    replicationJob_seedReplicationTime,
    replicationJob_runOnce,
    replicationJob_description,

    -- * ReplicationRun
    ReplicationRun (..),
    newReplicationRun,
    replicationRun_state,
    replicationRun_replicationRunId,
    replicationRun_encrypted,
    replicationRun_stageDetails,
    replicationRun_scheduledStartTime,
    replicationRun_statusMessage,
    replicationRun_kmsKeyId,
    replicationRun_completedTime,
    replicationRun_amiId,
    replicationRun_type,
    replicationRun_description,

    -- * ReplicationRunStageDetails
    ReplicationRunStageDetails (..),
    newReplicationRunStageDetails,
    replicationRunStageDetails_stage,
    replicationRunStageDetails_stageProgress,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_bucket,
    s3Location_key,

    -- * SSMOutput
    SSMOutput (..),
    newSSMOutput,
    sSMOutput_s3Location,

    -- * SSMValidationParameters
    SSMValidationParameters (..),
    newSSMValidationParameters,
    sSMValidationParameters_instanceId,
    sSMValidationParameters_command,
    sSMValidationParameters_executionTimeoutSeconds,
    sSMValidationParameters_scriptType,
    sSMValidationParameters_source,
    sSMValidationParameters_outputS3BucketName,

    -- * Server
    Server (..),
    newServer,
    server_serverType,
    server_serverId,
    server_replicationJobTerminated,
    server_vmServer,
    server_replicationJobId,

    -- * ServerGroup
    ServerGroup (..),
    newServerGroup,
    serverGroup_serverList,
    serverGroup_name,
    serverGroup_serverGroupId,

    -- * ServerGroupLaunchConfiguration
    ServerGroupLaunchConfiguration (..),
    newServerGroupLaunchConfiguration,
    serverGroupLaunchConfiguration_serverGroupId,
    serverGroupLaunchConfiguration_launchOrder,
    serverGroupLaunchConfiguration_serverLaunchConfigurations,

    -- * ServerGroupReplicationConfiguration
    ServerGroupReplicationConfiguration (..),
    newServerGroupReplicationConfiguration,
    serverGroupReplicationConfiguration_serverGroupId,
    serverGroupReplicationConfiguration_serverReplicationConfigurations,

    -- * ServerGroupValidationConfiguration
    ServerGroupValidationConfiguration (..),
    newServerGroupValidationConfiguration,
    serverGroupValidationConfiguration_serverValidationConfigurations,
    serverGroupValidationConfiguration_serverGroupId,

    -- * ServerLaunchConfiguration
    ServerLaunchConfiguration (..),
    newServerLaunchConfiguration,
    serverLaunchConfiguration_ec2KeyName,
    serverLaunchConfiguration_configureScriptType,
    serverLaunchConfiguration_associatePublicIpAddress,
    serverLaunchConfiguration_iamInstanceProfileName,
    serverLaunchConfiguration_subnet,
    serverLaunchConfiguration_logicalId,
    serverLaunchConfiguration_securityGroup,
    serverLaunchConfiguration_userData,
    serverLaunchConfiguration_instanceType,
    serverLaunchConfiguration_configureScript,
    serverLaunchConfiguration_server,
    serverLaunchConfiguration_vpc,

    -- * ServerReplicationConfiguration
    ServerReplicationConfiguration (..),
    newServerReplicationConfiguration,
    serverReplicationConfiguration_serverReplicationParameters,
    serverReplicationConfiguration_server,

    -- * ServerReplicationParameters
    ServerReplicationParameters (..),
    newServerReplicationParameters,
    serverReplicationParameters_frequency,
    serverReplicationParameters_numberOfRecentAmisToKeep,
    serverReplicationParameters_seedTime,
    serverReplicationParameters_licenseType,
    serverReplicationParameters_encrypted,
    serverReplicationParameters_kmsKeyId,
    serverReplicationParameters_runOnce,

    -- * ServerValidationConfiguration
    ServerValidationConfiguration (..),
    newServerValidationConfiguration,
    serverValidationConfiguration_serverValidationStrategy,
    serverValidationConfiguration_userDataValidationParameters,
    serverValidationConfiguration_name,
    serverValidationConfiguration_server,
    serverValidationConfiguration_validationId,

    -- * ServerValidationOutput
    ServerValidationOutput (..),
    newServerValidationOutput,
    serverValidationOutput_server,

    -- * Source
    Source (..),
    newSource,
    source_s3Location,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * UserData
    UserData (..),
    newUserData,
    userData_s3Location,

    -- * UserDataValidationParameters
    UserDataValidationParameters (..),
    newUserDataValidationParameters,
    userDataValidationParameters_scriptType,
    userDataValidationParameters_source,

    -- * ValidationOutput
    ValidationOutput (..),
    newValidationOutput,
    validationOutput_status,
    validationOutput_appValidationOutput,
    validationOutput_latestValidationTime,
    validationOutput_name,
    validationOutput_statusMessage,
    validationOutput_validationId,
    validationOutput_serverValidationOutput,

    -- * VmServer
    VmServer (..),
    newVmServer,
    vmServer_vmManagerName,
    vmServer_vmManagerType,
    vmServer_vmServerAddress,
    vmServer_vmName,
    vmServer_vmPath,

    -- * VmServerAddress
    VmServerAddress (..),
    newVmServerAddress,
    vmServerAddress_vmManagerId,
    vmServerAddress_vmId,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.AppLaunchConfigurationStatus
import Network.AWS.SMS.Types.AppLaunchStatus
import Network.AWS.SMS.Types.AppReplicationConfigurationStatus
import Network.AWS.SMS.Types.AppReplicationStatus
import Network.AWS.SMS.Types.AppStatus
import Network.AWS.SMS.Types.AppSummary
import Network.AWS.SMS.Types.AppValidationConfiguration
import Network.AWS.SMS.Types.AppValidationOutput
import Network.AWS.SMS.Types.AppValidationStrategy
import Network.AWS.SMS.Types.Connector
import Network.AWS.SMS.Types.ConnectorCapability
import Network.AWS.SMS.Types.ConnectorStatus
import Network.AWS.SMS.Types.LaunchDetails
import Network.AWS.SMS.Types.LicenseType
import Network.AWS.SMS.Types.NotificationContext
import Network.AWS.SMS.Types.OutputFormat
import Network.AWS.SMS.Types.ReplicationJob
import Network.AWS.SMS.Types.ReplicationJobState
import Network.AWS.SMS.Types.ReplicationRun
import Network.AWS.SMS.Types.ReplicationRunStageDetails
import Network.AWS.SMS.Types.ReplicationRunState
import Network.AWS.SMS.Types.ReplicationRunType
import Network.AWS.SMS.Types.S3Location
import Network.AWS.SMS.Types.SSMOutput
import Network.AWS.SMS.Types.SSMValidationParameters
import Network.AWS.SMS.Types.ScriptType
import Network.AWS.SMS.Types.Server
import Network.AWS.SMS.Types.ServerCatalogStatus
import Network.AWS.SMS.Types.ServerGroup
import Network.AWS.SMS.Types.ServerGroupLaunchConfiguration
import Network.AWS.SMS.Types.ServerGroupReplicationConfiguration
import Network.AWS.SMS.Types.ServerGroupValidationConfiguration
import Network.AWS.SMS.Types.ServerLaunchConfiguration
import Network.AWS.SMS.Types.ServerReplicationConfiguration
import Network.AWS.SMS.Types.ServerReplicationParameters
import Network.AWS.SMS.Types.ServerType
import Network.AWS.SMS.Types.ServerValidationConfiguration
import Network.AWS.SMS.Types.ServerValidationOutput
import Network.AWS.SMS.Types.ServerValidationStrategy
import Network.AWS.SMS.Types.Source
import Network.AWS.SMS.Types.Tag
import Network.AWS.SMS.Types.UserData
import Network.AWS.SMS.Types.UserDataValidationParameters
import Network.AWS.SMS.Types.ValidationOutput
import Network.AWS.SMS.Types.ValidationStatus
import Network.AWS.SMS.Types.VmManagerType
import Network.AWS.SMS.Types.VmServer
import Network.AWS.SMS.Types.VmServerAddress
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-10-24@ of the Amazon Server Migration Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "SMS",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "sms",
      Core._serviceSigningName = "sms",
      Core._serviceVersion = "2016-10-24",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "SMS",
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

-- | You have exceeded the number of on-demand replication runs you can
-- request in a 24-hour period.
_ReplicationRunLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationRunLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ReplicationRunLimitExceededException"

-- | A specified parameter is not valid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | There are no connectors available.
_NoConnectorsAvailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoConnectorsAvailableException =
  Core._MatchServiceError
    defaultService
    "NoConnectorsAvailableException"

-- | The specified replication job does not exist.
_ReplicationJobNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationJobNotFoundException =
  Core._MatchServiceError
    defaultService
    "ReplicationJobNotFoundException"

-- | The specified server cannot be replicated.
_ServerCannotBeReplicatedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServerCannotBeReplicatedException =
  Core._MatchServiceError
    defaultService
    "ServerCannotBeReplicatedException"

-- | The user has the required permissions, so the request would have
-- succeeded, but a dry run was performed.
_DryRunOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DryRunOperationException =
  Core._MatchServiceError
    defaultService
    "DryRunOperationException"

-- | An internal error occurred.
_InternalError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalError =
  Core._MatchServiceError
    defaultService
    "InternalError"

-- | The specified replication job already exists.
_ReplicationJobAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationJobAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ReplicationJobAlreadyExistsException"

-- | This operation is not allowed.
_OperationNotPermittedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OperationNotPermittedException"

-- | The service is temporarily unavailable.
_TemporarilyUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TemporarilyUnavailableException =
  Core._MatchServiceError
    defaultService
    "TemporarilyUnavailableException"

-- | A required parameter is missing.
_MissingRequiredParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingRequiredParameterException =
  Core._MatchServiceError
    defaultService
    "MissingRequiredParameterException"

-- | You lack permissions needed to perform this operation. Check your IAM
-- policies, and ensure that you are using the correct access keys.
_UnauthorizedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedOperationException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedOperationException"
