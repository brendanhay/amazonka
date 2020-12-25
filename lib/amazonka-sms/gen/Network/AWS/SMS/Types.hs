-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types
  ( -- * Service configuration
    mkServiceConfig,

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

    -- * ReplicationJobStatusMessage
    ReplicationJobStatusMessage (..),

    -- * InstanceId
    InstanceId (..),

    -- * NonEmptyStringWithMaxLen255
    NonEmptyStringWithMaxLen255 (..),

    -- * ValidationOutput
    ValidationOutput (..),
    mkValidationOutput,
    voAppValidationOutput,
    voLatestValidationTime,
    voName,
    voServerValidationOutput,
    voStatus,
    voStatusMessage,
    voValidationId,

    -- * ReplicationRunStageProgress
    ReplicationRunStageProgress (..),

    -- * EC2KeyName
    EC2KeyName (..),

    -- * Command
    Command (..),

    -- * LaunchDetails
    LaunchDetails (..),
    mkLaunchDetails,
    ldLatestLaunchTime,
    ldStackId,
    ldStackName,

    -- * ConnectorStatus
    ConnectorStatus (..),

    -- * VmManagerName
    VmManagerName (..),

    -- * ServerGroupLaunchConfiguration
    ServerGroupLaunchConfiguration (..),
    mkServerGroupLaunchConfiguration,
    sglcLaunchOrder,
    sglcServerGroupId,
    sglcServerLaunchConfigurations,

    -- * IpAddress
    IpAddress (..),

    -- * SSMOutput
    SSMOutput (..),
    mkSSMOutput,
    ssmoS3Location,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * ServerCatalogStatus
    ServerCatalogStatus (..),

    -- * AppReplicationStatusMessage
    AppReplicationStatusMessage (..),

    -- * ServerGroup
    ServerGroup (..),
    mkServerGroup,
    sgName,
    sgServerGroupId,
    sgServerList,

    -- * ReplicationRunState
    ReplicationRunState (..),

    -- * ClientToken
    ClientToken (..),

    -- * AppValidationConfiguration
    AppValidationConfiguration (..),
    mkAppValidationConfiguration,
    avcAppValidationStrategy,
    avcName,
    avcSsmValidationParameters,
    avcValidationId,

    -- * VmManagerId
    VmManagerId (..),

    -- * ReplicationRunType
    ReplicationRunType (..),

    -- * ServerValidationStrategy
    ServerValidationStrategy (..),

    -- * AppStatus
    AppStatus (..),

    -- * ServerType
    ServerType (..),

    -- * AppIdWithValidation
    AppIdWithValidation (..),

    -- * AppValidationOutput
    AppValidationOutput (..),
    mkAppValidationOutput,
    avoSsmOutput,

    -- * ServerLaunchConfiguration
    ServerLaunchConfiguration (..),
    mkServerLaunchConfiguration,
    slcAssociatePublicIpAddress,
    slcConfigureScript,
    slcConfigureScriptType,
    slcEc2KeyName,
    slcIamInstanceProfileName,
    slcInstanceType,
    slcLogicalId,
    slcSecurityGroup,
    slcServer,
    slcSubnet,
    slcUserData,
    slcVpc,

    -- * ServerId
    ServerId (..),

    -- * VmManagerType
    VmManagerType (..),

    -- * VmServerAddress
    VmServerAddress (..),
    mkVmServerAddress,
    vsaVmId,
    vsaVmManagerId,

    -- * AppLaunchStatus
    AppLaunchStatus (..),

    -- * UserDataValidationParameters
    UserDataValidationParameters (..),
    mkUserDataValidationParameters,
    udvpScriptType,
    udvpSource,

    -- * ReplicationRunId
    ReplicationRunId (..),

    -- * ReplicationJob
    ReplicationJob (..),
    mkReplicationJob,
    rjDescription,
    rjEncrypted,
    rjFrequency,
    rjKmsKeyId,
    rjLatestAmiId,
    rjLicenseType,
    rjNextReplicationRunStartTime,
    rjNumberOfRecentAmisToKeep,
    rjReplicationJobId,
    rjReplicationRunList,
    rjRoleName,
    rjRunOnce,
    rjSeedReplicationTime,
    rjServerId,
    rjServerType,
    rjState,
    rjStatusMessage,
    rjVmServer,

    -- * LicenseType
    LicenseType (..),

    -- * Subnet
    Subnet (..),

    -- * ReplicationRunStageDetails
    ReplicationRunStageDetails (..),
    mkReplicationRunStageDetails,
    rrsdStage,
    rrsdStageProgress,

    -- * ServerReplicationParameters
    ServerReplicationParameters (..),
    mkServerReplicationParameters,
    srpEncrypted,
    srpFrequency,
    srpKmsKeyId,
    srpLicenseType,
    srpNumberOfRecentAmisToKeep,
    srpRunOnce,
    srpSeedTime,

    -- * RoleName
    RoleName (..),

    -- * Connector
    Connector (..),
    mkConnector,
    cAssociatedOn,
    cCapabilityList,
    cConnectorId,
    cIpAddress,
    cMacAddress,
    cStatus,
    cVersion,
    cVmManagerId,
    cVmManagerName,
    cVmManagerType,

    -- * AppName
    AppName (..),

    -- * VmId
    VmId (..),

    -- * VmServer
    VmServer (..),
    mkVmServer,
    vsVmManagerName,
    vsVmManagerType,
    vsVmName,
    vsVmPath,
    vsVmServerAddress,

    -- * ServerValidationConfiguration
    ServerValidationConfiguration (..),
    mkServerValidationConfiguration,
    svcName,
    svcServer,
    svcServerValidationStrategy,
    svcUserDataValidationParameters,
    svcValidationId,

    -- * LogicalId
    LogicalId (..),

    -- * ConnectorId
    ConnectorId (..),

    -- * ReplicationJobId
    ReplicationJobId (..),

    -- * MacAddress
    MacAddress (..),

    -- * InstanceType
    InstanceType (..),

    -- * AppReplicationStatus
    AppReplicationStatus (..),

    -- * SecurityGroup
    SecurityGroup (..),

    -- * AppReplicationConfigurationStatus
    AppReplicationConfigurationStatus (..),

    -- * BucketName
    BucketName (..),

    -- * UserData
    UserData (..),
    mkUserData,
    udS3Location,

    -- * NextToken
    NextToken (..),

    -- * SSMValidationParameters
    SSMValidationParameters (..),
    mkSSMValidationParameters,
    ssmvpCommand,
    ssmvpExecutionTimeoutSeconds,
    ssmvpInstanceId,
    ssmvpOutputS3BucketName,
    ssmvpScriptType,
    ssmvpSource,

    -- * OutputFormat
    OutputFormat (..),

    -- * ValidationStatusMessage
    ValidationStatusMessage (..),

    -- * AppId
    AppId (..),

    -- * ReplicationJobState
    ReplicationJobState (..),

    -- * VmName
    VmName (..),

    -- * KmsKeyId
    KmsKeyId (..),

    -- * ValidationStatus
    ValidationStatus (..),

    -- * ScriptType
    ScriptType (..),

    -- * AppDescription
    AppDescription (..),

    -- * ServerGroupId
    ServerGroupId (..),

    -- * ReplicationRunStatusMessage
    ReplicationRunStatusMessage (..),

    -- * NotificationContext
    NotificationContext (..),
    mkNotificationContext,
    ncStatus,
    ncStatusMessage,
    ncValidationId,

    -- * Source
    Source (..),
    mkSource,
    sS3Location,

    -- * ReplicationRun
    ReplicationRun (..),
    mkReplicationRun,
    rrAmiId,
    rrCompletedTime,
    rrDescription,
    rrEncrypted,
    rrKmsKeyId,
    rrReplicationRunId,
    rrScheduledStartTime,
    rrStageDetails,
    rrState,
    rrStatusMessage,
    rrType,

    -- * AppSummary
    AppSummary (..),
    mkAppSummary,
    asAppId,
    asCreationTime,
    asDescription,
    asImportedAppId,
    asLastModified,
    asLatestReplicationTime,
    asLaunchConfigurationStatus,
    asLaunchDetails,
    asLaunchStatus,
    asLaunchStatusMessage,
    asName,
    asReplicationConfigurationStatus,
    asReplicationStatus,
    asReplicationStatusMessage,
    asRoleName,
    asStatus,
    asStatusMessage,
    asTotalServerGroups,
    asTotalServers,

    -- * S3Location
    S3Location (..),
    mkS3Location,
    slBucket,
    slKey,

    -- * Server
    Server (..),
    mkServer,
    sReplicationJobId,
    sReplicationJobTerminated,
    sServerId,
    sServerType,
    sVmServer,

    -- * AppLaunchConfigurationStatus
    AppLaunchConfigurationStatus (..),

    -- * AmiId
    AmiId (..),

    -- * ImportedAppId
    ImportedAppId (..),

    -- * ConnectorCapability
    ConnectorCapability (..),

    -- * StackId
    StackId (..),

    -- * ValidationId
    ValidationId (..),

    -- * Description
    Description (..),

    -- * VmPath
    VmPath (..),

    -- * ServerGroupReplicationConfiguration
    ServerGroupReplicationConfiguration (..),
    mkServerGroupReplicationConfiguration,
    sgrcServerGroupId,
    sgrcServerReplicationConfigurations,

    -- * ServerReplicationConfiguration
    ServerReplicationConfiguration (..),
    mkServerReplicationConfiguration,
    srcServer,
    srcServerReplicationParameters,

    -- * StackName
    StackName (..),

    -- * ServerGroupValidationConfiguration
    ServerGroupValidationConfiguration (..),
    mkServerGroupValidationConfiguration,
    sgvcServerGroupId,
    sgvcServerValidationConfigurations,

    -- * AppValidationStrategy
    AppValidationStrategy (..),

    -- * ServerValidationOutput
    ServerValidationOutput (..),
    mkServerValidationOutput,
    svoServer,

    -- * StatusMessage
    StatusMessage (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * Name
    Name (..),

    -- * IamInstanceProfileName
    IamInstanceProfileName (..),

    -- * Vpc
    Vpc (..),

    -- * LatestAmiId
    LatestAmiId (..),

    -- * Stage
    Stage (..),

    -- * Version
    Version (..),

    -- * LaunchStatusMessage
    LaunchStatusMessage (..),

    -- * Bucket
    Bucket (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import Network.AWS.SMS.Types.AmiId
import Network.AWS.SMS.Types.AppDescription
import Network.AWS.SMS.Types.AppId
import Network.AWS.SMS.Types.AppIdWithValidation
import Network.AWS.SMS.Types.AppLaunchConfigurationStatus
import Network.AWS.SMS.Types.AppLaunchStatus
import Network.AWS.SMS.Types.AppName
import Network.AWS.SMS.Types.AppReplicationConfigurationStatus
import Network.AWS.SMS.Types.AppReplicationStatus
import Network.AWS.SMS.Types.AppReplicationStatusMessage
import Network.AWS.SMS.Types.AppStatus
import Network.AWS.SMS.Types.AppSummary
import Network.AWS.SMS.Types.AppValidationConfiguration
import Network.AWS.SMS.Types.AppValidationOutput
import Network.AWS.SMS.Types.AppValidationStrategy
import Network.AWS.SMS.Types.Bucket
import Network.AWS.SMS.Types.BucketName
import Network.AWS.SMS.Types.ClientToken
import Network.AWS.SMS.Types.Command
import Network.AWS.SMS.Types.Connector
import Network.AWS.SMS.Types.ConnectorCapability
import Network.AWS.SMS.Types.ConnectorId
import Network.AWS.SMS.Types.ConnectorStatus
import Network.AWS.SMS.Types.Description
import Network.AWS.SMS.Types.EC2KeyName
import Network.AWS.SMS.Types.IamInstanceProfileName
import Network.AWS.SMS.Types.ImportedAppId
import Network.AWS.SMS.Types.InstanceId
import Network.AWS.SMS.Types.InstanceType
import Network.AWS.SMS.Types.IpAddress
import Network.AWS.SMS.Types.Key
import Network.AWS.SMS.Types.KmsKeyId
import Network.AWS.SMS.Types.LatestAmiId
import Network.AWS.SMS.Types.LaunchDetails
import Network.AWS.SMS.Types.LaunchStatusMessage
import Network.AWS.SMS.Types.LicenseType
import Network.AWS.SMS.Types.LogicalId
import Network.AWS.SMS.Types.MacAddress
import Network.AWS.SMS.Types.Name
import Network.AWS.SMS.Types.NextToken
import Network.AWS.SMS.Types.NonEmptyStringWithMaxLen255
import Network.AWS.SMS.Types.NotificationContext
import Network.AWS.SMS.Types.OutputFormat
import Network.AWS.SMS.Types.ReplicationJob
import Network.AWS.SMS.Types.ReplicationJobId
import Network.AWS.SMS.Types.ReplicationJobState
import Network.AWS.SMS.Types.ReplicationJobStatusMessage
import Network.AWS.SMS.Types.ReplicationRun
import Network.AWS.SMS.Types.ReplicationRunId
import Network.AWS.SMS.Types.ReplicationRunStageDetails
import Network.AWS.SMS.Types.ReplicationRunStageProgress
import Network.AWS.SMS.Types.ReplicationRunState
import Network.AWS.SMS.Types.ReplicationRunStatusMessage
import Network.AWS.SMS.Types.ReplicationRunType
import Network.AWS.SMS.Types.RoleName
import Network.AWS.SMS.Types.S3Location
import Network.AWS.SMS.Types.SSMOutput
import Network.AWS.SMS.Types.SSMValidationParameters
import Network.AWS.SMS.Types.ScriptType
import Network.AWS.SMS.Types.SecurityGroup
import Network.AWS.SMS.Types.Server
import Network.AWS.SMS.Types.ServerCatalogStatus
import Network.AWS.SMS.Types.ServerGroup
import Network.AWS.SMS.Types.ServerGroupId
import Network.AWS.SMS.Types.ServerGroupLaunchConfiguration
import Network.AWS.SMS.Types.ServerGroupReplicationConfiguration
import Network.AWS.SMS.Types.ServerGroupValidationConfiguration
import Network.AWS.SMS.Types.ServerId
import Network.AWS.SMS.Types.ServerLaunchConfiguration
import Network.AWS.SMS.Types.ServerReplicationConfiguration
import Network.AWS.SMS.Types.ServerReplicationParameters
import Network.AWS.SMS.Types.ServerType
import Network.AWS.SMS.Types.ServerValidationConfiguration
import Network.AWS.SMS.Types.ServerValidationOutput
import Network.AWS.SMS.Types.ServerValidationStrategy
import Network.AWS.SMS.Types.Source
import Network.AWS.SMS.Types.StackId
import Network.AWS.SMS.Types.StackName
import Network.AWS.SMS.Types.Stage
import Network.AWS.SMS.Types.StatusMessage
import Network.AWS.SMS.Types.Subnet
import Network.AWS.SMS.Types.Tag
import Network.AWS.SMS.Types.UserData
import Network.AWS.SMS.Types.UserDataValidationParameters
import Network.AWS.SMS.Types.ValidationId
import Network.AWS.SMS.Types.ValidationOutput
import Network.AWS.SMS.Types.ValidationStatus
import Network.AWS.SMS.Types.ValidationStatusMessage
import Network.AWS.SMS.Types.Value
import Network.AWS.SMS.Types.Version
import Network.AWS.SMS.Types.VmId
import Network.AWS.SMS.Types.VmManagerId
import Network.AWS.SMS.Types.VmManagerName
import Network.AWS.SMS.Types.VmManagerType
import Network.AWS.SMS.Types.VmName
import Network.AWS.SMS.Types.VmPath
import Network.AWS.SMS.Types.VmServer
import Network.AWS.SMS.Types.VmServerAddress
import Network.AWS.SMS.Types.Vpc
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-10-24@ of the Amazon Server Migration Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "SMS",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "sms",
      Core._svcVersion = "2016-10-24",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "SMS",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | You have exceeded the number of on-demand replication runs you can request in a 24-hour period.
_ReplicationRunLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReplicationRunLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "ReplicationRunLimitExceededException"
{-# DEPRECATED _ReplicationRunLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | A specified parameter is not valid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidParameterException"
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead." #-}

-- | There are no connectors available.
_NoConnectorsAvailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoConnectorsAvailableException =
  Core._MatchServiceError
    mkServiceConfig
    "NoConnectorsAvailableException"
{-# DEPRECATED _NoConnectorsAvailableException "Use generic-lens or generic-optics instead." #-}

-- | The specified replication job does not exist.
_ReplicationJobNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReplicationJobNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ReplicationJobNotFoundException"
{-# DEPRECATED _ReplicationJobNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The specified server cannot be replicated.
_ServerCannotBeReplicatedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServerCannotBeReplicatedException =
  Core._MatchServiceError
    mkServiceConfig
    "ServerCannotBeReplicatedException"
{-# DEPRECATED _ServerCannotBeReplicatedException "Use generic-lens or generic-optics instead." #-}

-- | The user has the required permissions, so the request would have succeeded, but a dry run was performed.
_DryRunOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DryRunOperationException =
  Core._MatchServiceError
    mkServiceConfig
    "DryRunOperationException"
{-# DEPRECATED _DryRunOperationException "Use generic-lens or generic-optics instead." #-}

-- | An internal error occurred.
_InternalError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalError =
  Core._MatchServiceError mkServiceConfig "InternalError"
{-# DEPRECATED _InternalError "Use generic-lens or generic-optics instead." #-}

-- | The specified replication job already exists.
_ReplicationJobAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReplicationJobAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "ReplicationJobAlreadyExistsException"
{-# DEPRECATED _ReplicationJobAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | This operation is not allowed.
_OperationNotPermittedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    mkServiceConfig
    "OperationNotPermittedException"
{-# DEPRECATED _OperationNotPermittedException "Use generic-lens or generic-optics instead." #-}

-- | The service is temporarily unavailable.
_TemporarilyUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TemporarilyUnavailableException =
  Core._MatchServiceError
    mkServiceConfig
    "TemporarilyUnavailableException"
{-# DEPRECATED _TemporarilyUnavailableException "Use generic-lens or generic-optics instead." #-}

-- | A required parameter is missing.
_MissingRequiredParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MissingRequiredParameterException =
  Core._MatchServiceError
    mkServiceConfig
    "MissingRequiredParameterException"
{-# DEPRECATED _MissingRequiredParameterException "Use generic-lens or generic-optics instead." #-}

-- | You lack permissions needed to perform this operation. Check your IAM policies, and ensure that you are using the correct access keys.
_UnauthorizedOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedOperationException =
  Core._MatchServiceError
    mkServiceConfig
    "UnauthorizedOperationException"
{-# DEPRECATED _UnauthorizedOperationException "Use generic-lens or generic-optics instead." #-}
