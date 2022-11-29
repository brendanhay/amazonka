{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SMS.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ServerCannotBeReplicatedException,
    _NoConnectorsAvailableException,
    _UnauthorizedOperationException,
    _InternalError,
    _MissingRequiredParameterException,
    _ReplicationJobAlreadyExistsException,
    _ReplicationJobNotFoundException,
    _TemporarilyUnavailableException,
    _OperationNotPermittedException,
    _DryRunOperationException,
    _ReplicationRunLimitExceededException,
    _InvalidParameterException,

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
    appSummary_launchStatus,
    appSummary_replicationConfigurationStatus,
    appSummary_name,
    appSummary_roleName,
    appSummary_replicationStatusMessage,
    appSummary_totalServerGroups,
    appSummary_launchDetails,
    appSummary_replicationStatus,
    appSummary_totalServers,
    appSummary_status,
    appSummary_description,
    appSummary_latestReplicationTime,
    appSummary_lastModified,
    appSummary_creationTime,
    appSummary_launchConfigurationStatus,
    appSummary_launchStatusMessage,
    appSummary_importedAppId,
    appSummary_statusMessage,
    appSummary_appId,

    -- * AppValidationConfiguration
    AppValidationConfiguration (..),
    newAppValidationConfiguration,
    appValidationConfiguration_name,
    appValidationConfiguration_ssmValidationParameters,
    appValidationConfiguration_appValidationStrategy,
    appValidationConfiguration_validationId,

    -- * AppValidationOutput
    AppValidationOutput (..),
    newAppValidationOutput,
    appValidationOutput_ssmOutput,

    -- * Connector
    Connector (..),
    newConnector,
    connector_associatedOn,
    connector_vmManagerName,
    connector_vmManagerId,
    connector_connectorId,
    connector_capabilityList,
    connector_status,
    connector_macAddress,
    connector_vmManagerType,
    connector_version,
    connector_ipAddress,

    -- * LaunchDetails
    LaunchDetails (..),
    newLaunchDetails,
    launchDetails_stackId,
    launchDetails_latestLaunchTime,
    launchDetails_stackName,

    -- * NotificationContext
    NotificationContext (..),
    newNotificationContext,
    notificationContext_validationId,
    notificationContext_status,
    notificationContext_statusMessage,

    -- * ReplicationJob
    ReplicationJob (..),
    newReplicationJob,
    replicationJob_replicationRunList,
    replicationJob_roleName,
    replicationJob_licenseType,
    replicationJob_frequency,
    replicationJob_runOnce,
    replicationJob_state,
    replicationJob_latestAmiId,
    replicationJob_description,
    replicationJob_serverType,
    replicationJob_replicationJobId,
    replicationJob_encrypted,
    replicationJob_seedReplicationTime,
    replicationJob_kmsKeyId,
    replicationJob_serverId,
    replicationJob_vmServer,
    replicationJob_nextReplicationRunStartTime,
    replicationJob_statusMessage,
    replicationJob_numberOfRecentAmisToKeep,

    -- * ReplicationRun
    ReplicationRun (..),
    newReplicationRun,
    replicationRun_scheduledStartTime,
    replicationRun_amiId,
    replicationRun_type,
    replicationRun_completedTime,
    replicationRun_state,
    replicationRun_stageDetails,
    replicationRun_description,
    replicationRun_replicationRunId,
    replicationRun_encrypted,
    replicationRun_kmsKeyId,
    replicationRun_statusMessage,

    -- * ReplicationRunStageDetails
    ReplicationRunStageDetails (..),
    newReplicationRunStageDetails,
    replicationRunStageDetails_stageProgress,
    replicationRunStageDetails_stage,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_key,
    s3Location_bucket,

    -- * SSMOutput
    SSMOutput (..),
    newSSMOutput,
    sSMOutput_s3Location,

    -- * SSMValidationParameters
    SSMValidationParameters (..),
    newSSMValidationParameters,
    sSMValidationParameters_command,
    sSMValidationParameters_instanceId,
    sSMValidationParameters_source,
    sSMValidationParameters_executionTimeoutSeconds,
    sSMValidationParameters_scriptType,
    sSMValidationParameters_outputS3BucketName,

    -- * Server
    Server (..),
    newServer,
    server_serverType,
    server_replicationJobId,
    server_serverId,
    server_vmServer,
    server_replicationJobTerminated,

    -- * ServerGroup
    ServerGroup (..),
    newServerGroup,
    serverGroup_name,
    serverGroup_serverGroupId,
    serverGroup_serverList,

    -- * ServerGroupLaunchConfiguration
    ServerGroupLaunchConfiguration (..),
    newServerGroupLaunchConfiguration,
    serverGroupLaunchConfiguration_serverGroupId,
    serverGroupLaunchConfiguration_serverLaunchConfigurations,
    serverGroupLaunchConfiguration_launchOrder,

    -- * ServerGroupReplicationConfiguration
    ServerGroupReplicationConfiguration (..),
    newServerGroupReplicationConfiguration,
    serverGroupReplicationConfiguration_serverReplicationConfigurations,
    serverGroupReplicationConfiguration_serverGroupId,

    -- * ServerGroupValidationConfiguration
    ServerGroupValidationConfiguration (..),
    newServerGroupValidationConfiguration,
    serverGroupValidationConfiguration_serverGroupId,
    serverGroupValidationConfiguration_serverValidationConfigurations,

    -- * ServerLaunchConfiguration
    ServerLaunchConfiguration (..),
    newServerLaunchConfiguration,
    serverLaunchConfiguration_ec2KeyName,
    serverLaunchConfiguration_vpc,
    serverLaunchConfiguration_userData,
    serverLaunchConfiguration_associatePublicIpAddress,
    serverLaunchConfiguration_securityGroup,
    serverLaunchConfiguration_configureScriptType,
    serverLaunchConfiguration_instanceType,
    serverLaunchConfiguration_server,
    serverLaunchConfiguration_logicalId,
    serverLaunchConfiguration_subnet,
    serverLaunchConfiguration_iamInstanceProfileName,
    serverLaunchConfiguration_configureScript,

    -- * ServerReplicationConfiguration
    ServerReplicationConfiguration (..),
    newServerReplicationConfiguration,
    serverReplicationConfiguration_serverReplicationParameters,
    serverReplicationConfiguration_server,

    -- * ServerReplicationParameters
    ServerReplicationParameters (..),
    newServerReplicationParameters,
    serverReplicationParameters_seedTime,
    serverReplicationParameters_licenseType,
    serverReplicationParameters_frequency,
    serverReplicationParameters_runOnce,
    serverReplicationParameters_encrypted,
    serverReplicationParameters_kmsKeyId,
    serverReplicationParameters_numberOfRecentAmisToKeep,

    -- * ServerValidationConfiguration
    ServerValidationConfiguration (..),
    newServerValidationConfiguration,
    serverValidationConfiguration_name,
    serverValidationConfiguration_userDataValidationParameters,
    serverValidationConfiguration_validationId,
    serverValidationConfiguration_serverValidationStrategy,
    serverValidationConfiguration_server,

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
    tag_key,
    tag_value,

    -- * UserData
    UserData (..),
    newUserData,
    userData_s3Location,

    -- * UserDataValidationParameters
    UserDataValidationParameters (..),
    newUserDataValidationParameters,
    userDataValidationParameters_source,
    userDataValidationParameters_scriptType,

    -- * ValidationOutput
    ValidationOutput (..),
    newValidationOutput,
    validationOutput_name,
    validationOutput_validationId,
    validationOutput_status,
    validationOutput_serverValidationOutput,
    validationOutput_appValidationOutput,
    validationOutput_statusMessage,
    validationOutput_latestValidationTime,

    -- * VmServer
    VmServer (..),
    newVmServer,
    vmServer_vmServerAddress,
    vmServer_vmManagerName,
    vmServer_vmName,
    vmServer_vmManagerType,
    vmServer_vmPath,

    -- * VmServerAddress
    VmServerAddress (..),
    newVmServerAddress,
    vmServerAddress_vmManagerId,
    vmServerAddress_vmId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.AppLaunchConfigurationStatus
import Amazonka.SMS.Types.AppLaunchStatus
import Amazonka.SMS.Types.AppReplicationConfigurationStatus
import Amazonka.SMS.Types.AppReplicationStatus
import Amazonka.SMS.Types.AppStatus
import Amazonka.SMS.Types.AppSummary
import Amazonka.SMS.Types.AppValidationConfiguration
import Amazonka.SMS.Types.AppValidationOutput
import Amazonka.SMS.Types.AppValidationStrategy
import Amazonka.SMS.Types.Connector
import Amazonka.SMS.Types.ConnectorCapability
import Amazonka.SMS.Types.ConnectorStatus
import Amazonka.SMS.Types.LaunchDetails
import Amazonka.SMS.Types.LicenseType
import Amazonka.SMS.Types.NotificationContext
import Amazonka.SMS.Types.OutputFormat
import Amazonka.SMS.Types.ReplicationJob
import Amazonka.SMS.Types.ReplicationJobState
import Amazonka.SMS.Types.ReplicationRun
import Amazonka.SMS.Types.ReplicationRunStageDetails
import Amazonka.SMS.Types.ReplicationRunState
import Amazonka.SMS.Types.ReplicationRunType
import Amazonka.SMS.Types.S3Location
import Amazonka.SMS.Types.SSMOutput
import Amazonka.SMS.Types.SSMValidationParameters
import Amazonka.SMS.Types.ScriptType
import Amazonka.SMS.Types.Server
import Amazonka.SMS.Types.ServerCatalogStatus
import Amazonka.SMS.Types.ServerGroup
import Amazonka.SMS.Types.ServerGroupLaunchConfiguration
import Amazonka.SMS.Types.ServerGroupReplicationConfiguration
import Amazonka.SMS.Types.ServerGroupValidationConfiguration
import Amazonka.SMS.Types.ServerLaunchConfiguration
import Amazonka.SMS.Types.ServerReplicationConfiguration
import Amazonka.SMS.Types.ServerReplicationParameters
import Amazonka.SMS.Types.ServerType
import Amazonka.SMS.Types.ServerValidationConfiguration
import Amazonka.SMS.Types.ServerValidationOutput
import Amazonka.SMS.Types.ServerValidationStrategy
import Amazonka.SMS.Types.Source
import Amazonka.SMS.Types.Tag
import Amazonka.SMS.Types.UserData
import Amazonka.SMS.Types.UserDataValidationParameters
import Amazonka.SMS.Types.ValidationOutput
import Amazonka.SMS.Types.ValidationStatus
import Amazonka.SMS.Types.VmManagerType
import Amazonka.SMS.Types.VmServer
import Amazonka.SMS.Types.VmServerAddress
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-10-24@ of the Amazon Server Migration Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SMS",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "sms",
      Core.signingName = "sms",
      Core.version = "2016-10-24",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "SMS",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified server cannot be replicated.
_ServerCannotBeReplicatedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServerCannotBeReplicatedException =
  Core._MatchServiceError
    defaultService
    "ServerCannotBeReplicatedException"

-- | There are no connectors available.
_NoConnectorsAvailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoConnectorsAvailableException =
  Core._MatchServiceError
    defaultService
    "NoConnectorsAvailableException"

-- | You lack permissions needed to perform this operation. Check your IAM
-- policies, and ensure that you are using the correct access keys.
_UnauthorizedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedOperationException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedOperationException"

-- | An internal error occurred.
_InternalError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalError =
  Core._MatchServiceError
    defaultService
    "InternalError"

-- | A required parameter is missing.
_MissingRequiredParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingRequiredParameterException =
  Core._MatchServiceError
    defaultService
    "MissingRequiredParameterException"

-- | The specified replication job already exists.
_ReplicationJobAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationJobAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ReplicationJobAlreadyExistsException"

-- | The specified replication job does not exist.
_ReplicationJobNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationJobNotFoundException =
  Core._MatchServiceError
    defaultService
    "ReplicationJobNotFoundException"

-- | The service is temporarily unavailable.
_TemporarilyUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TemporarilyUnavailableException =
  Core._MatchServiceError
    defaultService
    "TemporarilyUnavailableException"

-- | This operation is not allowed.
_OperationNotPermittedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OperationNotPermittedException"

-- | The user has the required permissions, so the request would have
-- succeeded, but a dry run was performed.
_DryRunOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DryRunOperationException =
  Core._MatchServiceError
    defaultService
    "DryRunOperationException"

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
