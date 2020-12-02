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
    , _MissingRequiredParameterException
    , _UnauthorizedOperationException

    -- * ConnectorCapability
    , ConnectorCapability (..)

    -- * ConnectorStatus
    , ConnectorStatus (..)

    -- * LicenseType
    , LicenseType (..)

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

    -- * ReplicationJob
    , ReplicationJob
    , replicationJob
    , rjFrequency
    , rjState
    , rjServerType
    , rjServerId
    , rjLicenseType
    , rjRoleName
    , rjVmServer
    , rjReplicationJobId
    , rjReplicationRunList
    , rjNextReplicationRunStartTime
    , rjStatusMessage
    , rjLatestAMIId
    , rjSeedReplicationTime
    , rjDescription

    -- * ReplicationRun
    , ReplicationRun
    , replicationRun
    , rrState
    , rrReplicationRunId
    , rrScheduledStartTime
    , rrStatusMessage
    , rrCompletedTime
    , rrAmiId
    , rrType
    , rrDescription

    -- * Server
    , Server
    , server
    , sServerType
    , sServerId
    , sReplicationJobTerminated
    , sVmServer
    , sReplicationJobId

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


-- | This user has exceeded the maximum allowed Replication Run limit.
_ReplicationRunLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicationRunLimitExceededException =
  _MatchServiceError sms "ReplicationRunLimitExceededException"


-- | A parameter specified in the request is not valid, is unsupported, or cannot be used.
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException = _MatchServiceError sms "InvalidParameterException"


-- | No connectors are available to handle this request. Please associate connector(s) and verify any existing connectors are healthy and can respond to requests.
_NoConnectorsAvailableException :: AsError a => Getting (First ServiceError) a ServiceError
_NoConnectorsAvailableException =
  _MatchServiceError sms "NoConnectorsAvailableException"


-- | The specified Replication Job cannot be found.
_ReplicationJobNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicationJobNotFoundException =
  _MatchServiceError sms "ReplicationJobNotFoundException"


-- | The provided server cannot be replicated.
_ServerCannotBeReplicatedException :: AsError a => Getting (First ServiceError) a ServiceError
_ServerCannotBeReplicatedException =
  _MatchServiceError sms "ServerCannotBeReplicatedException"


-- | An internal error has occured.
_InternalError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalError = _MatchServiceError sms "InternalError"


-- | An active Replication Job already exists for the specified server.
_ReplicationJobAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicationJobAlreadyExistsException =
  _MatchServiceError sms "ReplicationJobAlreadyExistsException"


-- | The specified operation is not allowed. This error can occur for a number of reasons; for example, you might be trying to start a Replication Run before seed Replication Run.
_OperationNotPermittedException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationNotPermittedException =
  _MatchServiceError sms "OperationNotPermittedException"


-- | The request is missing a required parameter. Ensure that you have supplied all the required parameters for the request.
_MissingRequiredParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_MissingRequiredParameterException =
  _MatchServiceError sms "MissingRequiredParameterException"


-- | This user does not have permissions to perform this operation.
_UnauthorizedOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedOperationException =
  _MatchServiceError sms "UnauthorizedOperationException"

