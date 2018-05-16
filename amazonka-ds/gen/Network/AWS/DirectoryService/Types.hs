{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types
    (
    -- * Service Configuration
      directoryService

    -- * Errors
    , _DirectoryUnavailableException
    , _AuthenticationFailedException
    , _InvalidParameterException
    , _UnsupportedOperationException
    , _EntityAlreadyExistsException
    , _DirectoryLimitExceededException
    , _IPRouteLimitExceededException
    , _EntityDoesNotExistException
    , _InsufficientPermissionsException
    , _InvalidNextTokenException
    , _ServiceException
    , _SnapshotLimitExceededException
    , _DomainControllerLimitExceededException
    , _TagLimitExceededException
    , _ClientException

    -- * DirectoryEdition
    , DirectoryEdition (..)

    -- * DirectorySize
    , DirectorySize (..)

    -- * DirectoryStage
    , DirectoryStage (..)

    -- * DirectoryType
    , DirectoryType (..)

    -- * DomainControllerStatus
    , DomainControllerStatus (..)

    -- * IPRouteStatusMsg
    , IPRouteStatusMsg (..)

    -- * RadiusAuthenticationProtocol
    , RadiusAuthenticationProtocol (..)

    -- * RadiusStatus
    , RadiusStatus (..)

    -- * ReplicationScope
    , ReplicationScope (..)

    -- * SchemaExtensionStatus
    , SchemaExtensionStatus (..)

    -- * SnapshotStatus
    , SnapshotStatus (..)

    -- * SnapshotType
    , SnapshotType (..)

    -- * TopicStatus
    , TopicStatus (..)

    -- * TrustDirection
    , TrustDirection (..)

    -- * TrustState
    , TrustState (..)

    -- * TrustType
    , TrustType (..)

    -- * Attribute
    , Attribute
    , attribute
    , aValue
    , aName

    -- * Computer
    , Computer
    , computer
    , cComputerId
    , cComputerAttributes
    , cComputerName

    -- * ConditionalForwarder
    , ConditionalForwarder
    , conditionalForwarder
    , cfDNSIPAddrs
    , cfRemoteDomainName
    , cfReplicationScope

    -- * DirectoryConnectSettings
    , DirectoryConnectSettings
    , directoryConnectSettings
    , dcsVPCId
    , dcsSubnetIds
    , dcsCustomerDNSIPs
    , dcsCustomerUserName

    -- * DirectoryConnectSettingsDescription
    , DirectoryConnectSettingsDescription
    , directoryConnectSettingsDescription
    , dcsdCustomerUserName
    , dcsdSubnetIds
    , dcsdVPCId
    , dcsdSecurityGroupId
    , dcsdConnectIPs
    , dcsdAvailabilityZones

    -- * DirectoryDescription
    , DirectoryDescription
    , directoryDescription
    , ddEdition
    , ddRadiusStatus
    , ddStage
    , ddDirectoryId
    , ddAccessURL
    , ddShortName
    , ddSize
    , ddDesiredNumberOfDomainControllers
    , ddRadiusSettings
    , ddLaunchTime
    , ddAlias
    , ddName
    , ddStageLastUpdatedDateTime
    , ddSSOEnabled
    , ddDNSIPAddrs
    , ddVPCSettings
    , ddType
    , ddStageReason
    , ddConnectSettings
    , ddDescription

    -- * DirectoryLimits
    , DirectoryLimits
    , directoryLimits
    , dlConnectedDirectoriesCurrentCount
    , dlCloudOnlyMicrosoftADLimitReached
    , dlConnectedDirectoriesLimit
    , dlConnectedDirectoriesLimitReached
    , dlCloudOnlyMicrosoftADLimit
    , dlCloudOnlyDirectoriesLimit
    , dlCloudOnlyDirectoriesCurrentCount
    , dlCloudOnlyDirectoriesLimitReached
    , dlCloudOnlyMicrosoftADCurrentCount

    -- * DirectoryVPCSettings
    , DirectoryVPCSettings
    , directoryVPCSettings
    , dvsVPCId
    , dvsSubnetIds

    -- * DirectoryVPCSettingsDescription
    , DirectoryVPCSettingsDescription
    , directoryVPCSettingsDescription
    , dvsdSubnetIds
    , dvsdVPCId
    , dvsdSecurityGroupId
    , dvsdAvailabilityZones

    -- * DomainController
    , DomainController
    , domainController
    , dcStatus
    , dcDirectoryId
    , dcVPCId
    , dcLaunchTime
    , dcSubnetId
    , dcAvailabilityZone
    , dcStatusLastUpdatedDateTime
    , dcStatusReason
    , dcDNSIPAddr
    , dcDomainControllerId

    -- * EventTopic
    , EventTopic
    , eventTopic
    , etStatus
    , etDirectoryId
    , etTopicName
    , etTopicARN
    , etCreatedDateTime

    -- * IPRoute
    , IPRoute
    , ipRoute
    , irCidrIP
    , irDescription

    -- * IPRouteInfo
    , IPRouteInfo
    , ipRouteInfo
    , iriDirectoryId
    , iriIPRouteStatusReason
    , iriAddedDateTime
    , iriCidrIP
    , iriIPRouteStatusMsg
    , iriDescription

    -- * RadiusSettings
    , RadiusSettings
    , radiusSettings
    , rsDisplayLabel
    , rsRadiusRetries
    , rsAuthenticationProtocol
    , rsRadiusServers
    , rsUseSameUsername
    , rsSharedSecret
    , rsRadiusTimeout
    , rsRadiusPort

    -- * SchemaExtensionInfo
    , SchemaExtensionInfo
    , schemaExtensionInfo
    , seiDirectoryId
    , seiSchemaExtensionId
    , seiSchemaExtensionStatusReason
    , seiSchemaExtensionStatus
    , seiDescription
    , seiEndDateTime
    , seiStartDateTime

    -- * Snapshot
    , Snapshot
    , snapshot
    , sStatus
    , sDirectoryId
    , sStartTime
    , sName
    , sType
    , sSnapshotId

    -- * SnapshotLimits
    , SnapshotLimits
    , snapshotLimits
    , slManualSnapshotsLimitReached
    , slManualSnapshotsCurrentCount
    , slManualSnapshotsLimit

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * Trust
    , Trust
    , trust
    , tDirectoryId
    , tTrustState
    , tLastUpdatedDateTime
    , tTrustDirection
    , tStateLastUpdatedDateTime
    , tTrustType
    , tTrustStateReason
    , tRemoteDomainName
    , tTrustId
    , tCreatedDateTime
    ) where

import Network.AWS.DirectoryService.Types.Product
import Network.AWS.DirectoryService.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-04-16@ of the Amazon Directory Service SDK configuration.
directoryService :: Service
directoryService =
  Service
    { _svcAbbrev = "DirectoryService"
    , _svcSigner = v4
    , _svcPrefix = "ds"
    , _svcVersion = "2015-04-16"
    , _svcEndpoint = defaultEndpoint directoryService
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "DirectoryService"
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


-- | The specified directory is unavailable or could not be found.
--
--
_DirectoryUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectoryUnavailableException =
  _MatchServiceError directoryService "DirectoryUnavailableException"


-- | An authentication error occurred.
--
--
_AuthenticationFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_AuthenticationFailedException =
  _MatchServiceError directoryService "AuthenticationFailedException"


-- | One or more parameters are not valid.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError directoryService "InvalidParameterException"


-- | The operation is not supported.
--
--
_UnsupportedOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedOperationException =
  _MatchServiceError directoryService "UnsupportedOperationException"


-- | The specified entity already exists.
--
--
_EntityAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_EntityAlreadyExistsException =
  _MatchServiceError directoryService "EntityAlreadyExistsException"


-- | The maximum number of directories in the region has been reached. You can use the 'GetDirectoryLimits' operation to determine your directory limits in the region.
--
--
_DirectoryLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectoryLimitExceededException =
  _MatchServiceError directoryService "DirectoryLimitExceededException"


-- | The maximum allowed number of IP addresses was exceeded. The default limit is 100 IP address blocks.
--
--
_IPRouteLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_IPRouteLimitExceededException =
  _MatchServiceError directoryService "IpRouteLimitExceededException"


-- | The specified entity could not be found.
--
--
_EntityDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_EntityDoesNotExistException =
  _MatchServiceError directoryService "EntityDoesNotExistException"


-- | The account does not have sufficient permission to perform the operation.
--
--
_InsufficientPermissionsException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientPermissionsException =
  _MatchServiceError directoryService "InsufficientPermissionsException"


-- | The /NextToken/ value is not valid.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
  _MatchServiceError directoryService "InvalidNextTokenException"


-- | An exception has occurred in AWS Directory Service.
--
--
_ServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceException = _MatchServiceError directoryService "ServiceException"


-- | The maximum number of manual snapshots for the directory has been reached. You can use the 'GetSnapshotLimits' operation to determine the snapshot limits for a directory.
--
--
_SnapshotLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotLimitExceededException =
  _MatchServiceError directoryService "SnapshotLimitExceededException"


-- | The maximum allowed number of domain controllers per directory was exceeded. The default limit per directory is 20 domain controllers.
--
--
_DomainControllerLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_DomainControllerLimitExceededException =
  _MatchServiceError directoryService "DomainControllerLimitExceededException"


-- | The maximum allowed number of tags was exceeded.
--
--
_TagLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TagLimitExceededException =
  _MatchServiceError directoryService "TagLimitExceededException"


-- | A client exception has occurred.
--
--
_ClientException :: AsError a => Getting (First ServiceError) a ServiceError
_ClientException = _MatchServiceError directoryService "ClientException"

