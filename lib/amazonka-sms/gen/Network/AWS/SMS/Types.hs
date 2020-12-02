{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types
  ( -- * Service Configuration
    sms,

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
    AppSummary,
    appSummary,
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
    AppValidationConfiguration,
    appValidationConfiguration,
    avcSsmValidationParameters,
    avcName,
    avcValidationId,
    avcAppValidationStrategy,

    -- * AppValidationOutput
    AppValidationOutput,
    appValidationOutput,
    avoSsmOutput,

    -- * Connector
    Connector,
    connector,
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
    LaunchDetails,
    launchDetails,
    ldStackId,
    ldLatestLaunchTime,
    ldStackName,

    -- * NotificationContext
    NotificationContext,
    notificationContext,
    ncStatus,
    ncStatusMessage,
    ncValidationId,

    -- * ReplicationJob
    ReplicationJob,
    replicationJob,
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
    ReplicationRun,
    replicationRun,
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
    ReplicationRunStageDetails,
    replicationRunStageDetails,
    rrsdStage,
    rrsdStageProgress,

    -- * S3Location
    S3Location,
    s3Location,
    slBucket,
    slKey,

    -- * SSMOutput
    SSMOutput,
    sSMOutput,
    ssmoS3Location,

    -- * SSMValidationParameters
    SSMValidationParameters,
    sSMValidationParameters,
    ssmvpInstanceId,
    ssmvpCommand,
    ssmvpExecutionTimeoutSeconds,
    ssmvpScriptType,
    ssmvpSource,
    ssmvpOutputS3BucketName,

    -- * Server
    Server,
    server,
    sServerType,
    sServerId,
    sReplicationJobTerminated,
    sVmServer,
    sReplicationJobId,

    -- * ServerGroup
    ServerGroup,
    serverGroup,
    sgServerList,
    sgName,
    sgServerGroupId,

    -- * ServerGroupLaunchConfiguration
    ServerGroupLaunchConfiguration,
    serverGroupLaunchConfiguration,
    sglcServerGroupId,
    sglcLaunchOrder,
    sglcServerLaunchConfigurations,

    -- * ServerGroupReplicationConfiguration
    ServerGroupReplicationConfiguration,
    serverGroupReplicationConfiguration,
    sgrcServerGroupId,
    sgrcServerReplicationConfigurations,

    -- * ServerGroupValidationConfiguration
    ServerGroupValidationConfiguration,
    serverGroupValidationConfiguration,
    sgvcServerValidationConfigurations,
    sgvcServerGroupId,

    -- * ServerLaunchConfiguration
    ServerLaunchConfiguration,
    serverLaunchConfiguration,
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
    ServerReplicationConfiguration,
    serverReplicationConfiguration,
    srcServerReplicationParameters,
    srcServer,

    -- * ServerReplicationParameters
    ServerReplicationParameters,
    serverReplicationParameters,
    srpFrequency,
    srpNumberOfRecentAMIsToKeep,
    srpSeedTime,
    srpLicenseType,
    srpEncrypted,
    srpKmsKeyId,
    srpRunOnce,

    -- * ServerValidationConfiguration
    ServerValidationConfiguration,
    serverValidationConfiguration,
    svcServerValidationStrategy,
    svcUserDataValidationParameters,
    svcName,
    svcServer,
    svcValidationId,

    -- * ServerValidationOutput
    ServerValidationOutput,
    serverValidationOutput,
    svoServer,

    -- * Source
    Source,
    source,
    sS3Location,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * UserData
    UserData,
    userData,
    udS3Location,

    -- * UserDataValidationParameters
    UserDataValidationParameters,
    userDataValidationParameters,
    udvpScriptType,
    udvpSource,

    -- * VMServer
    VMServer,
    vMServer,
    vmsVmManagerName,
    vmsVmManagerType,
    vmsVmServerAddress,
    vmsVmName,
    vmsVmPath,

    -- * VMServerAddress
    VMServerAddress,
    vMServerAddress,
    vmsaVmManagerId,
    vmsaVmId,

    -- * ValidationOutput
    ValidationOutput,
    validationOutput,
    voStatus,
    voAppValidationOutput,
    voLatestValidationTime,
    voName,
    voStatusMessage,
    voValidationId,
    voServerValidationOutput,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
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
import Network.AWS.Sign.V4

-- | API version @2016-10-24@ of the Amazon Server Migration Service SDK configuration.
sms :: Service
sms =
  Service
    { _svcAbbrev = "SMS",
      _svcSigner = v4,
      _svcPrefix = "sms",
      _svcVersion = "2016-10-24",
      _svcEndpoint = defaultEndpoint sms,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "SMS",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
