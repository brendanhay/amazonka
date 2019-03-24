{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types
    (
    -- * Service Configuration
      sms

    -- * Errors
    , _ReplicationRunLimitExceededException
    , _InvalidParameterException
    , _NoConnectorsAvailableException
    , _ReplicationJobNotFoundException
    , _ServerCannotBeReplicatedException
    , _InternalError
    , _ReplicationJobAlreadyExistsException
    , _OperationNotPermittedException
    , _TemporarilyUnavailableException
    , _MissingRequiredParameterException
    , _UnauthorizedOperationException

    -- * AppLaunchStatus
    , AppLaunchStatus (..)

    -- * AppReplicationStatus
    , AppReplicationStatus (..)

    -- * AppStatus
    , AppStatus (..)

    -- * ConnectorCapability
    , ConnectorCapability (..)

    -- * ConnectorStatus
    , ConnectorStatus (..)

    -- * LicenseType
    , LicenseType (..)

    -- * OutputFormat
    , OutputFormat (..)

    -- * ReplicationJobState
    , ReplicationJobState (..)

    -- * ReplicationRunState
    , ReplicationRunState (..)

    -- * ReplicationRunType
    , ReplicationRunType (..)

    -- * ServerCatalogStatus
    , ServerCatalogStatus (..)

    -- * ServerType
    , ServerType (..)

    -- * VMManagerType
    , VMManagerType (..)

    -- * AppSummary
    , AppSummary
    , appSummary
    , asCreationTime
    , asTotalServers
    , asStatus
    , asLaunchDetails
    , asLaunchStatusMessage
    , asReplicationStatusMessage
    , asTotalServerGroups
    , asRoleName
    , asLaunchStatus
    , asAppId
    , asName
    , asStatusMessage
    , asLatestReplicationTime
    , asReplicationStatus
    , asLastModified
    , asDescription

    -- * Connector
    , Connector
    , connector
    , cStatus
    , cVmManagerName
    , cIpAddress
    , cVmManagerId
    , cVmManagerType
    , cConnectorId
    , cAssociatedOn
    , cMacAddress
    , cVersion
    , cCapabilityList

    -- * LaunchDetails
    , LaunchDetails
    , launchDetails
    , ldStackId
    , ldLatestLaunchTime
    , ldStackName

    -- * ReplicationJob
    , ReplicationJob
    , replicationJob
    , rjFrequency
    , rjNumberOfRecentAMIsToKeep
    , rjState
    , rjServerType
    , rjServerId
    , rjLicenseType
    , rjRoleName
    , rjVmServer
    , rjEncrypted
    , rjReplicationJobId
    , rjReplicationRunList
    , rjNextReplicationRunStartTime
    , rjStatusMessage
    , rjKmsKeyId
    , rjLatestAMIId
    , rjSeedReplicationTime
    , rjRunOnce
    , rjDescription

    -- * ReplicationRun
    , ReplicationRun
    , replicationRun
    , rrState
    , rrReplicationRunId
    , rrEncrypted
    , rrStageDetails
    , rrScheduledStartTime
    , rrStatusMessage
    , rrKmsKeyId
    , rrCompletedTime
    , rrAmiId
    , rrType
    , rrDescription

    -- * ReplicationRunStageDetails
    , ReplicationRunStageDetails
    , replicationRunStageDetails
    , rrsdStage
    , rrsdStageProgress

    -- * S3Location
    , S3Location
    , s3Location
    , slBucket
    , slKey

    -- * Server
    , Server
    , server
    , sServerType
    , sServerId
    , sReplicationJobTerminated
    , sVmServer
    , sReplicationJobId

    -- * ServerGroup
    , ServerGroup
    , serverGroup
    , sgServerList
    , sgName
    , sgServerGroupId

    -- * ServerGroupLaunchConfiguration
    , ServerGroupLaunchConfiguration
    , serverGroupLaunchConfiguration
    , sglcServerGroupId
    , sglcLaunchOrder
    , sglcServerLaunchConfigurations

    -- * ServerGroupReplicationConfiguration
    , ServerGroupReplicationConfiguration
    , serverGroupReplicationConfiguration
    , sgrcServerGroupId
    , sgrcServerReplicationConfigurations

    -- * ServerLaunchConfiguration
    , ServerLaunchConfiguration
    , serverLaunchConfiguration
    , slcEc2KeyName
    , slcAssociatePublicIPAddress
    , slcSubnet
    , slcLogicalId
    , slcSecurityGroup
    , slcUserData
    , slcInstanceType
    , slcServer
    , slcVpc

    -- * ServerReplicationConfiguration
    , ServerReplicationConfiguration
    , serverReplicationConfiguration
    , srcServerReplicationParameters
    , srcServer

    -- * ServerReplicationParameters
    , ServerReplicationParameters
    , serverReplicationParameters
    , srpFrequency
    , srpNumberOfRecentAMIsToKeep
    , srpSeedTime
    , srpLicenseType
    , srpEncrypted
    , srpKmsKeyId
    , srpRunOnce

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * UserData
    , UserData
    , userData
    , udS3Location

    -- * VMServer
    , VMServer
    , vMServer
    , vmsVmManagerName
    , vmsVmManagerType
    , vmsVmServerAddress
    , vmsVmName
    , vmsVmPath

    -- * VMServerAddress
    , VMServerAddress
    , vMServerAddress
    , vmsaVmManagerId
    , vmsaVmId
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.SMS.Types.Product
import Network.AWS.SMS.Types.Sum

-- | API version @2016-10-24@ of the Amazon Server Migration Service SDK configuration.
sms :: Service
sms =
  Service
    { _svcAbbrev = "SMS"
    , _svcSigner = v4
    , _svcPrefix = "sms"
    , _svcVersion = "2016-10-24"
    , _svcEndpoint = defaultEndpoint sms
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "SMS"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | You have exceeded the number of on-demand replication runs you can request in a 24-hour period.
--
--
_ReplicationRunLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicationRunLimitExceededException =
  _MatchServiceError sms "ReplicationRunLimitExceededException"


-- | A specified parameter is not valid.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException = _MatchServiceError sms "InvalidParameterException"


-- | There are no connectors available.
--
--
_NoConnectorsAvailableException :: AsError a => Getting (First ServiceError) a ServiceError
_NoConnectorsAvailableException =
  _MatchServiceError sms "NoConnectorsAvailableException"


-- | The specified replication job does not exist.
--
--
_ReplicationJobNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicationJobNotFoundException =
  _MatchServiceError sms "ReplicationJobNotFoundException"


-- | The specified server cannot be replicated.
--
--
_ServerCannotBeReplicatedException :: AsError a => Getting (First ServiceError) a ServiceError
_ServerCannotBeReplicatedException =
  _MatchServiceError sms "ServerCannotBeReplicatedException"


-- | An internal error occurred.
--
--
_InternalError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalError = _MatchServiceError sms "InternalError"


-- | The specified replication job already exists.
--
--
_ReplicationJobAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicationJobAlreadyExistsException =
  _MatchServiceError sms "ReplicationJobAlreadyExistsException"


-- | This operation is not allowed.
--
--
_OperationNotPermittedException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationNotPermittedException =
  _MatchServiceError sms "OperationNotPermittedException"


-- | The service is temporarily unavailable.
--
--
_TemporarilyUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_TemporarilyUnavailableException =
  _MatchServiceError sms "TemporarilyUnavailableException"


-- | A required parameter is missing.
--
--
_MissingRequiredParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_MissingRequiredParameterException =
  _MatchServiceError sms "MissingRequiredParameterException"


-- | You lack permissions needed to perform this operation. Check your IAM policies, and ensure that you are using the correct access keys.
--
--
_UnauthorizedOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedOperationException =
  _MatchServiceError sms "UnauthorizedOperationException"

