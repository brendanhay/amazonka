{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SMS.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DryRunOperationException,
    _InternalError,
    _InvalidParameterException,
    _MissingRequiredParameterException,
    _NoConnectorsAvailableException,
    _OperationNotPermittedException,
    _ReplicationJobAlreadyExistsException,
    _ReplicationJobNotFoundException,
    _ReplicationRunLimitExceededException,
    _ServerCannotBeReplicatedException,
    _TemporarilyUnavailableException,
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
    appSummary_appId,
    appSummary_creationTime,
    appSummary_description,
    appSummary_importedAppId,
    appSummary_lastModified,
    appSummary_latestReplicationTime,
    appSummary_launchConfigurationStatus,
    appSummary_launchDetails,
    appSummary_launchStatus,
    appSummary_launchStatusMessage,
    appSummary_name,
    appSummary_replicationConfigurationStatus,
    appSummary_replicationStatus,
    appSummary_replicationStatusMessage,
    appSummary_roleName,
    appSummary_status,
    appSummary_statusMessage,
    appSummary_totalServerGroups,
    appSummary_totalServers,

    -- * AppValidationConfiguration
    AppValidationConfiguration (..),
    newAppValidationConfiguration,
    appValidationConfiguration_appValidationStrategy,
    appValidationConfiguration_name,
    appValidationConfiguration_ssmValidationParameters,
    appValidationConfiguration_validationId,

    -- * AppValidationOutput
    AppValidationOutput (..),
    newAppValidationOutput,
    appValidationOutput_ssmOutput,

    -- * Connector
    Connector (..),
    newConnector,
    connector_associatedOn,
    connector_capabilityList,
    connector_connectorId,
    connector_ipAddress,
    connector_macAddress,
    connector_status,
    connector_version,
    connector_vmManagerId,
    connector_vmManagerName,
    connector_vmManagerType,

    -- * LaunchDetails
    LaunchDetails (..),
    newLaunchDetails,
    launchDetails_latestLaunchTime,
    launchDetails_stackId,
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
    replicationJob_description,
    replicationJob_encrypted,
    replicationJob_frequency,
    replicationJob_kmsKeyId,
    replicationJob_latestAmiId,
    replicationJob_licenseType,
    replicationJob_nextReplicationRunStartTime,
    replicationJob_numberOfRecentAmisToKeep,
    replicationJob_replicationJobId,
    replicationJob_replicationRunList,
    replicationJob_roleName,
    replicationJob_runOnce,
    replicationJob_seedReplicationTime,
    replicationJob_serverId,
    replicationJob_serverType,
    replicationJob_state,
    replicationJob_statusMessage,
    replicationJob_vmServer,

    -- * ReplicationRun
    ReplicationRun (..),
    newReplicationRun,
    replicationRun_amiId,
    replicationRun_completedTime,
    replicationRun_description,
    replicationRun_encrypted,
    replicationRun_kmsKeyId,
    replicationRun_replicationRunId,
    replicationRun_scheduledStartTime,
    replicationRun_stageDetails,
    replicationRun_state,
    replicationRun_statusMessage,
    replicationRun_type,

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
    sSMValidationParameters_command,
    sSMValidationParameters_executionTimeoutSeconds,
    sSMValidationParameters_instanceId,
    sSMValidationParameters_outputS3BucketName,
    sSMValidationParameters_scriptType,
    sSMValidationParameters_source,

    -- * Server
    Server (..),
    newServer,
    server_replicationJobId,
    server_replicationJobTerminated,
    server_serverId,
    server_serverType,
    server_vmServer,

    -- * ServerGroup
    ServerGroup (..),
    newServerGroup,
    serverGroup_name,
    serverGroup_serverGroupId,
    serverGroup_serverList,

    -- * ServerGroupLaunchConfiguration
    ServerGroupLaunchConfiguration (..),
    newServerGroupLaunchConfiguration,
    serverGroupLaunchConfiguration_launchOrder,
    serverGroupLaunchConfiguration_serverGroupId,
    serverGroupLaunchConfiguration_serverLaunchConfigurations,

    -- * ServerGroupReplicationConfiguration
    ServerGroupReplicationConfiguration (..),
    newServerGroupReplicationConfiguration,
    serverGroupReplicationConfiguration_serverGroupId,
    serverGroupReplicationConfiguration_serverReplicationConfigurations,

    -- * ServerGroupValidationConfiguration
    ServerGroupValidationConfiguration (..),
    newServerGroupValidationConfiguration,
    serverGroupValidationConfiguration_serverGroupId,
    serverGroupValidationConfiguration_serverValidationConfigurations,

    -- * ServerLaunchConfiguration
    ServerLaunchConfiguration (..),
    newServerLaunchConfiguration,
    serverLaunchConfiguration_associatePublicIpAddress,
    serverLaunchConfiguration_configureScript,
    serverLaunchConfiguration_configureScriptType,
    serverLaunchConfiguration_ec2KeyName,
    serverLaunchConfiguration_iamInstanceProfileName,
    serverLaunchConfiguration_instanceType,
    serverLaunchConfiguration_logicalId,
    serverLaunchConfiguration_securityGroup,
    serverLaunchConfiguration_server,
    serverLaunchConfiguration_subnet,
    serverLaunchConfiguration_userData,
    serverLaunchConfiguration_vpc,

    -- * ServerReplicationConfiguration
    ServerReplicationConfiguration (..),
    newServerReplicationConfiguration,
    serverReplicationConfiguration_server,
    serverReplicationConfiguration_serverReplicationParameters,

    -- * ServerReplicationParameters
    ServerReplicationParameters (..),
    newServerReplicationParameters,
    serverReplicationParameters_encrypted,
    serverReplicationParameters_frequency,
    serverReplicationParameters_kmsKeyId,
    serverReplicationParameters_licenseType,
    serverReplicationParameters_numberOfRecentAmisToKeep,
    serverReplicationParameters_runOnce,
    serverReplicationParameters_seedTime,

    -- * ServerValidationConfiguration
    ServerValidationConfiguration (..),
    newServerValidationConfiguration,
    serverValidationConfiguration_name,
    serverValidationConfiguration_server,
    serverValidationConfiguration_serverValidationStrategy,
    serverValidationConfiguration_userDataValidationParameters,
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
    tag_key,
    tag_value,

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
    validationOutput_appValidationOutput,
    validationOutput_latestValidationTime,
    validationOutput_name,
    validationOutput_serverValidationOutput,
    validationOutput_status,
    validationOutput_statusMessage,
    validationOutput_validationId,

    -- * VmServer
    VmServer (..),
    newVmServer,
    vmServer_vmManagerName,
    vmServer_vmManagerType,
    vmServer_vmName,
    vmServer_vmPath,
    vmServer_vmServerAddress,

    -- * VmServerAddress
    VmServerAddress (..),
    newVmServerAddress,
    vmServerAddress_vmId,
    vmServerAddress_vmManagerId,
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

-- | The user has the required permissions, so the request would have
-- succeeded, but a dry run was performed.
_DryRunOperationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DryRunOperationException =
  Core._MatchServiceError
    defaultService
    "DryRunOperationException"

-- | An internal error occurred.
_InternalError :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalError =
  Core._MatchServiceError
    defaultService
    "InternalError"

-- | A specified parameter is not valid.
_InvalidParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | A required parameter is missing.
_MissingRequiredParameterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MissingRequiredParameterException =
  Core._MatchServiceError
    defaultService
    "MissingRequiredParameterException"

-- | There are no connectors available.
_NoConnectorsAvailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoConnectorsAvailableException =
  Core._MatchServiceError
    defaultService
    "NoConnectorsAvailableException"

-- | This operation is not allowed.
_OperationNotPermittedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OperationNotPermittedException"

-- | The specified replication job already exists.
_ReplicationJobAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReplicationJobAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ReplicationJobAlreadyExistsException"

-- | The specified replication job does not exist.
_ReplicationJobNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReplicationJobNotFoundException =
  Core._MatchServiceError
    defaultService
    "ReplicationJobNotFoundException"

-- | You have exceeded the number of on-demand replication runs you can
-- request in a 24-hour period.
_ReplicationRunLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReplicationRunLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ReplicationRunLimitExceededException"

-- | The specified server cannot be replicated.
_ServerCannotBeReplicatedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServerCannotBeReplicatedException =
  Core._MatchServiceError
    defaultService
    "ServerCannotBeReplicatedException"

-- | The service is temporarily unavailable.
_TemporarilyUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TemporarilyUnavailableException =
  Core._MatchServiceError
    defaultService
    "TemporarilyUnavailableException"

-- | You lack permissions needed to perform this operation. Check your IAM
-- policies, and ensure that you are using the correct access keys.
_UnauthorizedOperationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnauthorizedOperationException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedOperationException"
