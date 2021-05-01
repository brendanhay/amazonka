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
    _InternalError,
    _NoConnectorsAvailableException,
    _ReplicationRunLimitExceededException,
    _UnauthorizedOperationException,
    _MissingRequiredParameterException,
    _TemporarilyUnavailableException,
    _OperationNotPermittedException,
    _ReplicationJobNotFoundException,
    _InvalidParameterException,
    _DryRunOperationException,
    _ServerCannotBeReplicatedException,
    _ReplicationJobAlreadyExistsException,

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
    appSummary_statusMessage,
    appSummary_appId,
    appSummary_status,
    appSummary_creationTime,
    appSummary_totalServers,
    appSummary_launchStatus,
    appSummary_replicationStatusMessage,
    appSummary_roleName,
    appSummary_replicationStatus,
    appSummary_importedAppId,
    appSummary_replicationConfigurationStatus,
    appSummary_latestReplicationTime,
    appSummary_launchDetails,
    appSummary_name,
    appSummary_launchConfigurationStatus,
    appSummary_description,
    appSummary_lastModified,
    appSummary_totalServerGroups,
    appSummary_launchStatusMessage,

    -- * AppValidationConfiguration
    AppValidationConfiguration (..),
    newAppValidationConfiguration,
    appValidationConfiguration_ssmValidationParameters,
    appValidationConfiguration_appValidationStrategy,
    appValidationConfiguration_validationId,
    appValidationConfiguration_name,

    -- * AppValidationOutput
    AppValidationOutput (..),
    newAppValidationOutput,
    appValidationOutput_ssmOutput,

    -- * Connector
    Connector (..),
    newConnector,
    connector_status,
    connector_macAddress,
    connector_associatedOn,
    connector_connectorId,
    connector_vmManagerId,
    connector_version,
    connector_vmManagerName,
    connector_ipAddress,
    connector_vmManagerType,
    connector_capabilityList,

    -- * LaunchDetails
    LaunchDetails (..),
    newLaunchDetails,
    launchDetails_stackName,
    launchDetails_stackId,
    launchDetails_latestLaunchTime,

    -- * NotificationContext
    NotificationContext (..),
    newNotificationContext,
    notificationContext_statusMessage,
    notificationContext_status,
    notificationContext_validationId,

    -- * ReplicationJob
    ReplicationJob (..),
    newReplicationJob,
    replicationJob_nextReplicationRunStartTime,
    replicationJob_statusMessage,
    replicationJob_numberOfRecentAmisToKeep,
    replicationJob_encrypted,
    replicationJob_latestAmiId,
    replicationJob_roleName,
    replicationJob_serverId,
    replicationJob_state,
    replicationJob_replicationRunList,
    replicationJob_kmsKeyId,
    replicationJob_frequency,
    replicationJob_runOnce,
    replicationJob_description,
    replicationJob_replicationJobId,
    replicationJob_seedReplicationTime,
    replicationJob_vmServer,
    replicationJob_licenseType,
    replicationJob_serverType,

    -- * ReplicationRun
    ReplicationRun (..),
    newReplicationRun,
    replicationRun_statusMessage,
    replicationRun_encrypted,
    replicationRun_replicationRunId,
    replicationRun_amiId,
    replicationRun_completedTime,
    replicationRun_state,
    replicationRun_kmsKeyId,
    replicationRun_scheduledStartTime,
    replicationRun_stageDetails,
    replicationRun_description,
    replicationRun_type,

    -- * ReplicationRunStageDetails
    ReplicationRunStageDetails (..),
    newReplicationRunStageDetails,
    replicationRunStageDetails_stage,
    replicationRunStageDetails_stageProgress,

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
    sSMValidationParameters_instanceId,
    sSMValidationParameters_outputS3BucketName,
    sSMValidationParameters_source,
    sSMValidationParameters_scriptType,
    sSMValidationParameters_command,
    sSMValidationParameters_executionTimeoutSeconds,

    -- * Server
    Server (..),
    newServer,
    server_serverId,
    server_replicationJobId,
    server_replicationJobTerminated,
    server_vmServer,
    server_serverType,

    -- * ServerGroup
    ServerGroup (..),
    newServerGroup,
    serverGroup_serverGroupId,
    serverGroup_name,
    serverGroup_serverList,

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
    serverGroupValidationConfiguration_serverGroupId,
    serverGroupValidationConfiguration_serverValidationConfigurations,

    -- * ServerLaunchConfiguration
    ServerLaunchConfiguration (..),
    newServerLaunchConfiguration,
    serverLaunchConfiguration_configureScript,
    serverLaunchConfiguration_ec2KeyName,
    serverLaunchConfiguration_instanceType,
    serverLaunchConfiguration_userData,
    serverLaunchConfiguration_logicalId,
    serverLaunchConfiguration_subnet,
    serverLaunchConfiguration_iamInstanceProfileName,
    serverLaunchConfiguration_server,
    serverLaunchConfiguration_associatePublicIpAddress,
    serverLaunchConfiguration_configureScriptType,
    serverLaunchConfiguration_securityGroup,
    serverLaunchConfiguration_vpc,

    -- * ServerReplicationConfiguration
    ServerReplicationConfiguration (..),
    newServerReplicationConfiguration,
    serverReplicationConfiguration_server,
    serverReplicationConfiguration_serverReplicationParameters,

    -- * ServerReplicationParameters
    ServerReplicationParameters (..),
    newServerReplicationParameters,
    serverReplicationParameters_numberOfRecentAmisToKeep,
    serverReplicationParameters_encrypted,
    serverReplicationParameters_seedTime,
    serverReplicationParameters_kmsKeyId,
    serverReplicationParameters_frequency,
    serverReplicationParameters_runOnce,
    serverReplicationParameters_licenseType,

    -- * ServerValidationConfiguration
    ServerValidationConfiguration (..),
    newServerValidationConfiguration,
    serverValidationConfiguration_validationId,
    serverValidationConfiguration_userDataValidationParameters,
    serverValidationConfiguration_server,
    serverValidationConfiguration_name,
    serverValidationConfiguration_serverValidationStrategy,

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
    validationOutput_statusMessage,
    validationOutput_status,
    validationOutput_validationId,
    validationOutput_appValidationOutput,
    validationOutput_name,
    validationOutput_serverValidationOutput,
    validationOutput_latestValidationTime,

    -- * VmServer
    VmServer (..),
    newVmServer,
    vmServer_vmPath,
    vmServer_vmManagerName,
    vmServer_vmName,
    vmServer_vmServerAddress,
    vmServer_vmManagerType,

    -- * VmServerAddress
    VmServerAddress (..),
    newVmServerAddress,
    vmServerAddress_vmManagerId,
    vmServerAddress_vmId,
  )
where

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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "SMS",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "sms",
      Prelude._svcSigningName = "sms",
      Prelude._svcVersion = "2016-10-24",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "SMS",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | An internal error occurred.
_InternalError :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalError =
  Prelude._MatchServiceError
    defaultService
    "InternalError"

-- | There are no connectors available.
_NoConnectorsAvailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoConnectorsAvailableException =
  Prelude._MatchServiceError
    defaultService
    "NoConnectorsAvailableException"

-- | You have exceeded the number of on-demand replication runs you can
-- request in a 24-hour period.
_ReplicationRunLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReplicationRunLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "ReplicationRunLimitExceededException"

-- | You lack permissions needed to perform this operation. Check your IAM
-- policies, and ensure that you are using the correct access keys.
_UnauthorizedOperationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnauthorizedOperationException =
  Prelude._MatchServiceError
    defaultService
    "UnauthorizedOperationException"

-- | A required parameter is missing.
_MissingRequiredParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MissingRequiredParameterException =
  Prelude._MatchServiceError
    defaultService
    "MissingRequiredParameterException"

-- | The service is temporarily unavailable.
_TemporarilyUnavailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TemporarilyUnavailableException =
  Prelude._MatchServiceError
    defaultService
    "TemporarilyUnavailableException"

-- | This operation is not allowed.
_OperationNotPermittedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OperationNotPermittedException =
  Prelude._MatchServiceError
    defaultService
    "OperationNotPermittedException"

-- | The specified replication job does not exist.
_ReplicationJobNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReplicationJobNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ReplicationJobNotFoundException"

-- | A specified parameter is not valid.
_InvalidParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The user has the required permissions, so the request would have
-- succeeded, but a dry run was performed.
_DryRunOperationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DryRunOperationException =
  Prelude._MatchServiceError
    defaultService
    "DryRunOperationException"

-- | The specified server cannot be replicated.
_ServerCannotBeReplicatedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServerCannotBeReplicatedException =
  Prelude._MatchServiceError
    defaultService
    "ServerCannotBeReplicatedException"

-- | The specified replication job already exists.
_ReplicationJobAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReplicationJobAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "ReplicationJobAlreadyExistsException"
