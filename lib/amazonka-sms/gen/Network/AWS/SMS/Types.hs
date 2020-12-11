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
    smsService,

    -- * Errors

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

    -- * VMManagerType
    VMManagerType (..),

    -- * ValidationStatus
    ValidationStatus (..),

    -- * AppSummary
    AppSummary (..),
    mkAppSummary,
    asCreationTime,
    asTotalServers,
    asStatus,
    asLaunchDetails,
    asLaunchStatusMessage,
    asReplicationConfigurationStatus,
    asReplicationStatusMessage,
    asTotalServerGroups,
    asRoleName,
    asLaunchConfigurationStatus,
    asLaunchStatus,
    asAppId,
    asName,
    asStatusMessage,
    asLatestReplicationTime,
    asImportedAppId,
    asReplicationStatus,
    asLastModified,
    asDescription,

    -- * AppValidationConfiguration
    AppValidationConfiguration (..),
    mkAppValidationConfiguration,
    avcSsmValidationParameters,
    avcName,
    avcValidationId,
    avcAppValidationStrategy,

    -- * AppValidationOutput
    AppValidationOutput (..),
    mkAppValidationOutput,
    avoSsmOutput,

    -- * Connector
    Connector (..),
    mkConnector,
    cStatus,
    cVmManagerName,
    cIpAddress,
    cVmManagerId,
    cVmManagerType,
    cConnectorId,
    cAssociatedOn,
    cMacAddress,
    cVersion,
    cCapabilityList,

    -- * LaunchDetails
    LaunchDetails (..),
    mkLaunchDetails,
    ldStackId,
    ldLatestLaunchTime,
    ldStackName,

    -- * NotificationContext
    NotificationContext (..),
    mkNotificationContext,
    ncStatus,
    ncStatusMessage,
    ncValidationId,

    -- * ReplicationJob
    ReplicationJob (..),
    mkReplicationJob,
    rjFrequency,
    rjNumberOfRecentAMIsToKeep,
    rjState,
    rjServerType,
    rjServerId,
    rjLicenseType,
    rjRoleName,
    rjVmServer,
    rjEncrypted,
    rjReplicationJobId,
    rjReplicationRunList,
    rjNextReplicationRunStartTime,
    rjStatusMessage,
    rjKmsKeyId,
    rjLatestAMIId,
    rjSeedReplicationTime,
    rjRunOnce,
    rjDescription,

    -- * ReplicationRun
    ReplicationRun (..),
    mkReplicationRun,
    rrState,
    rrReplicationRunId,
    rrEncrypted,
    rrStageDetails,
    rrScheduledStartTime,
    rrStatusMessage,
    rrKmsKeyId,
    rrCompletedTime,
    rrAmiId,
    rrType,
    rrDescription,

    -- * ReplicationRunStageDetails
    ReplicationRunStageDetails (..),
    mkReplicationRunStageDetails,
    rrsdStage,
    rrsdStageProgress,

    -- * S3Location
    S3Location (..),
    mkS3Location,
    slBucket,
    slKey,

    -- * SSMOutput
    SSMOutput (..),
    mkSSMOutput,
    ssmoS3Location,

    -- * SSMValidationParameters
    SSMValidationParameters (..),
    mkSSMValidationParameters,
    ssmvpInstanceId,
    ssmvpCommand,
    ssmvpExecutionTimeoutSeconds,
    ssmvpScriptType,
    ssmvpSource,
    ssmvpOutputS3BucketName,

    -- * Server
    Server (..),
    mkServer,
    sServerType,
    sServerId,
    sReplicationJobTerminated,
    sVmServer,
    sReplicationJobId,

    -- * ServerGroup
    ServerGroup (..),
    mkServerGroup,
    sgServerList,
    sgName,
    sgServerGroupId,

    -- * ServerGroupLaunchConfiguration
    ServerGroupLaunchConfiguration (..),
    mkServerGroupLaunchConfiguration,
    sglcServerGroupId,
    sglcLaunchOrder,
    sglcServerLaunchConfigurations,

    -- * ServerGroupReplicationConfiguration
    ServerGroupReplicationConfiguration (..),
    mkServerGroupReplicationConfiguration,
    sgrcServerGroupId,
    sgrcServerReplicationConfigurations,

    -- * ServerGroupValidationConfiguration
    ServerGroupValidationConfiguration (..),
    mkServerGroupValidationConfiguration,
    sgvcServerValidationConfigurations,
    sgvcServerGroupId,

    -- * ServerLaunchConfiguration
    ServerLaunchConfiguration (..),
    mkServerLaunchConfiguration,
    slcEc2KeyName,
    slcConfigureScriptType,
    slcAssociatePublicIPAddress,
    slcIamInstanceProfileName,
    slcSubnet,
    slcLogicalId,
    slcSecurityGroup,
    slcUserData,
    slcInstanceType,
    slcConfigureScript,
    slcServer,
    slcVpc,

    -- * ServerReplicationConfiguration
    ServerReplicationConfiguration (..),
    mkServerReplicationConfiguration,
    srcServerReplicationParameters,
    srcServer,

    -- * ServerReplicationParameters
    ServerReplicationParameters (..),
    mkServerReplicationParameters,
    srpFrequency,
    srpNumberOfRecentAMIsToKeep,
    srpSeedTime,
    srpLicenseType,
    srpEncrypted,
    srpKmsKeyId,
    srpRunOnce,

    -- * ServerValidationConfiguration
    ServerValidationConfiguration (..),
    mkServerValidationConfiguration,
    svcServerValidationStrategy,
    svcUserDataValidationParameters,
    svcName,
    svcServer,
    svcValidationId,

    -- * ServerValidationOutput
    ServerValidationOutput (..),
    mkServerValidationOutput,
    svoServer,

    -- * Source
    Source (..),
    mkSource,
    sS3Location,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * UserData
    UserData (..),
    mkUserData,
    udS3Location,

    -- * UserDataValidationParameters
    UserDataValidationParameters (..),
    mkUserDataValidationParameters,
    udvpScriptType,
    udvpSource,

    -- * VMServer
    VMServer (..),
    mkVMServer,
    vmsVmManagerName,
    vmsVmManagerType,
    vmsVmServerAddress,
    vmsVmName,
    vmsVmPath,

    -- * VMServerAddress
    VMServerAddress (..),
    mkVMServerAddress,
    vmsaVmManagerId,
    vmsaVmId,

    -- * ValidationOutput
    ValidationOutput (..),
    mkValidationOutput,
    voStatus,
    voAppValidationOutput,
    voLatestValidationTime,
    voName,
    voStatusMessage,
    voValidationId,
    voServerValidationOutput,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
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
import Network.AWS.SMS.Types.VMManagerType
import Network.AWS.SMS.Types.VMServer
import Network.AWS.SMS.Types.VMServerAddress
import Network.AWS.SMS.Types.ValidationOutput
import Network.AWS.SMS.Types.ValidationStatus
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-10-24@ of the Amazon Server Migration Service SDK configuration.
smsService :: Lude.Service
smsService =
  Lude.Service
    { Lude._svcAbbrev = "SMS",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "sms",
      Lude._svcVersion = "2016-10-24",
      Lude._svcEndpoint = Lude.defaultEndpoint smsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "SMS",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
