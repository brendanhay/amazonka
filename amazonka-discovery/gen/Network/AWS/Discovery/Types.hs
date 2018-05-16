{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types
    (
    -- * Service Configuration
      discovery

    -- * Errors
    , _AuthorizationErrorException
    , _InvalidParameterException
    , _InvalidParameterValueException
    , _ServerInternalErrorException
    , _OperationNotPermittedException
    , _ResourceNotFoundException

    -- * AgentStatus
    , AgentStatus (..)

    -- * ConfigurationItemType
    , ConfigurationItemType (..)

    -- * ExportDataFormat
    , ExportDataFormat (..)

    -- * ExportStatus
    , ExportStatus (..)

    -- * OrderString
    , OrderString (..)

    -- * AgentConfigurationStatus
    , AgentConfigurationStatus
    , agentConfigurationStatus
    , acsAgentId
    , acsOperationSucceeded
    , acsDescription

    -- * AgentInfo
    , AgentInfo
    , agentInfo
    , aiHostName
    , aiLastHealthPingTime
    , aiAgentNetworkInfoList
    , aiConnectorId
    , aiHealth
    , aiAgentId
    , aiVersion
    , aiCollectionStatus
    , aiRegisteredTime
    , aiAgentType

    -- * AgentNetworkInfo
    , AgentNetworkInfo
    , agentNetworkInfo
    , aniIpAddress
    , aniMacAddress

    -- * ConfigurationTag
    , ConfigurationTag
    , configurationTag
    , ctTimeOfCreation
    , ctConfigurationId
    , ctConfigurationType
    , ctValue
    , ctKey

    -- * CustomerAgentInfo
    , CustomerAgentInfo
    , customerAgentInfo
    , caiActiveAgents
    , caiHealthyAgents
    , caiBlackListedAgents
    , caiShutdownAgents
    , caiUnhealthyAgents
    , caiTotalAgents
    , caiUnknownAgents

    -- * CustomerConnectorInfo
    , CustomerConnectorInfo
    , customerConnectorInfo
    , cciActiveConnectors
    , cciHealthyConnectors
    , cciBlackListedConnectors
    , cciShutdownConnectors
    , cciUnhealthyConnectors
    , cciTotalConnectors
    , cciUnknownConnectors

    -- * ExportFilter
    , ExportFilter
    , exportFilter
    , efName
    , efValues
    , efCondition

    -- * ExportInfo
    , ExportInfo
    , exportInfo
    , eiConfigurationsDownloadURL
    , eiRequestedStartTime
    , eiRequestedEndTime
    , eiIsTruncated
    , eiExportId
    , eiExportStatus
    , eiStatusMessage
    , eiExportRequestTime

    -- * Filter
    , Filter
    , filter'
    , fName
    , fValues
    , fCondition

    -- * NeighborConnectionDetail
    , NeighborConnectionDetail
    , neighborConnectionDetail
    , ncdTransportProtocol
    , ncdDestinationPort
    , ncdSourceServerId
    , ncdDestinationServerId
    , ncdConnectionsCount

    -- * OrderByElement
    , OrderByElement
    , orderByElement
    , obeSortOrder
    , obeFieldName

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * TagFilter
    , TagFilter
    , tagFilter
    , tfName
    , tfValues
    ) where

import Network.AWS.Discovery.Types.Product
import Network.AWS.Discovery.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-11-01@ of the Amazon Application Discovery Service SDK configuration.
discovery :: Service
discovery =
  Service
    { _svcAbbrev = "Discovery"
    , _svcSigner = v4
    , _svcPrefix = "discovery"
    , _svcVersion = "2015-11-01"
    , _svcEndpoint = defaultEndpoint discovery
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Discovery"
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


-- | The AWS user account does not have permission to perform the action. Check the IAM policy associated with this account.
--
--
_AuthorizationErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationErrorException =
  _MatchServiceError discovery "AuthorizationErrorException"


-- | One or more parameters are not valid. Verify the parameters and try again.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError discovery "InvalidParameterException"


-- | The value of one or more parameters are either invalid or out of range. Verify the parameter values and try again.
--
--
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
  _MatchServiceError discovery "InvalidParameterValueException"


-- | The server experienced an internal error. Try again.
--
--
_ServerInternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_ServerInternalErrorException =
  _MatchServiceError discovery "ServerInternalErrorException"


-- | This operation is not permitted.
--
--
_OperationNotPermittedException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationNotPermittedException =
  _MatchServiceError discovery "OperationNotPermittedException"


-- | The specified configuration ID was not located. Verify the configuration ID and try again.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError discovery "ResourceNotFoundException"

