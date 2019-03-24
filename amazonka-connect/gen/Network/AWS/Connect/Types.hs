{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types
    (
    -- * Service Configuration
      connect

    -- * Errors
    , _OutboundContactNotPermittedException
    , _InvalidParameterException
    , _InvalidRequestException
    , _DuplicateResourceException
    , _UserNotFoundException
    , _DestinationNotAllowedException
    , _ContactNotFoundException
    , _ThrottlingException
    , _InternalServiceException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * Channel
    , Channel (..)

    -- * Comparison
    , Comparison (..)

    -- * CurrentMetricName
    , CurrentMetricName (..)

    -- * Grouping
    , Grouping (..)

    -- * HistoricalMetricName
    , HistoricalMetricName (..)

    -- * PhoneType
    , PhoneType (..)

    -- * Statistic
    , Statistic (..)

    -- * Unit
    , Unit (..)

    -- * Credentials
    , Credentials
    , credentials
    , cAccessTokenExpiration
    , cAccessToken
    , cRefreshToken
    , cRefreshTokenExpiration

    -- * CurrentMetric
    , CurrentMetric
    , currentMetric
    , cmName
    , cmUnit

    -- * CurrentMetricData
    , CurrentMetricData
    , currentMetricData
    , cmdValue
    , cmdMetric

    -- * CurrentMetricResult
    , CurrentMetricResult
    , currentMetricResult
    , cmrCollections
    , cmrDimensions

    -- * Dimensions
    , Dimensions
    , dimensions
    , dChannel
    , dQueue

    -- * Filters
    , Filters
    , filters
    , fQueues
    , fChannels

    -- * HierarchyGroup
    , HierarchyGroup
    , hierarchyGroup
    , hgARN
    , hgName
    , hgHierarchyPath
    , hgId
    , hgLevelId

    -- * HierarchyGroupSummary
    , HierarchyGroupSummary
    , hierarchyGroupSummary
    , hgsARN
    , hgsName
    , hgsId

    -- * HierarchyLevel
    , HierarchyLevel
    , hierarchyLevel
    , hlARN
    , hlName
    , hlId

    -- * HierarchyPath
    , HierarchyPath
    , hierarchyPath
    , hpLevelFive
    , hpLevelThree
    , hpLevelFour
    , hpLevelTwo
    , hpLevelOne

    -- * HierarchyStructure
    , HierarchyStructure
    , hierarchyStructure
    , hsLevelFive
    , hsLevelThree
    , hsLevelFour
    , hsLevelTwo
    , hsLevelOne

    -- * HistoricalMetric
    , HistoricalMetric
    , historicalMetric
    , hmName
    , hmThreshold
    , hmUnit
    , hmStatistic

    -- * HistoricalMetricData
    , HistoricalMetricData
    , historicalMetricData
    , hmdValue
    , hmdMetric

    -- * HistoricalMetricResult
    , HistoricalMetricResult
    , historicalMetricResult
    , hmrCollections
    , hmrDimensions

    -- * QueueReference
    , QueueReference
    , queueReference
    , qrARN
    , qrId

    -- * RoutingProfileSummary
    , RoutingProfileSummary
    , routingProfileSummary
    , rpsARN
    , rpsName
    , rpsId

    -- * SecurityProfileSummary
    , SecurityProfileSummary
    , securityProfileSummary
    , spsARN
    , spsName
    , spsId

    -- * Threshold
    , Threshold
    , threshold
    , tThresholdValue
    , tComparison

    -- * User
    , User
    , user
    , uRoutingProfileId
    , uDirectoryUserId
    , uARN
    , uIdentityInfo
    , uSecurityProfileIds
    , uUsername
    , uId
    , uHierarchyGroupId
    , uPhoneConfig

    -- * UserIdentityInfo
    , UserIdentityInfo
    , userIdentityInfo
    , uiiEmail
    , uiiLastName
    , uiiFirstName

    -- * UserPhoneConfig
    , UserPhoneConfig
    , userPhoneConfig
    , upcAutoAccept
    , upcAfterContactWorkTimeLimit
    , upcDeskPhoneNumber
    , upcPhoneType

    -- * UserSummary
    , UserSummary
    , userSummary
    , usARN
    , usUsername
    , usId
    ) where

import Network.AWS.Connect.Types.Product
import Network.AWS.Connect.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-08-08@ of the Amazon Connect Service SDK configuration.
connect :: Service
connect =
  Service
    { _svcAbbrev = "Connect"
    , _svcSigner = v4
    , _svcPrefix = "connect"
    , _svcVersion = "2017-08-08"
    , _svcEndpoint = defaultEndpoint connect
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Connect"
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


-- | The contact is not permitted.
--
--
_OutboundContactNotPermittedException :: AsError a => Getting (First ServiceError) a ServiceError
_OutboundContactNotPermittedException =
  _MatchServiceError connect "OutboundContactNotPermittedException" .
  hasStatus 403


-- | One or more of the parameters provided to the operation are not valid.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError connect "InvalidParameterException" . hasStatus 400


-- | The request is not valid.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException =
  _MatchServiceError connect "InvalidRequestException" . hasStatus 400


-- | A resource with that name already exists.
--
--
_DuplicateResourceException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateResourceException =
  _MatchServiceError connect "DuplicateResourceException" . hasStatus 409


-- | No user with the specified credentials was found in the Amazon Connect instance.
--
--
_UserNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_UserNotFoundException =
  _MatchServiceError connect "UserNotFoundException" . hasStatus 404


-- | Outbound calls to the destination number are not allowed.
--
--
_DestinationNotAllowedException :: AsError a => Getting (First ServiceError) a ServiceError
_DestinationNotAllowedException =
  _MatchServiceError connect "DestinationNotAllowedException" . hasStatus 403


-- | The contact with the specified ID is not active or does not exist.
--
--
_ContactNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ContactNotFoundException =
  _MatchServiceError connect "ContactNotFoundException" . hasStatus 410


-- | The throttling limit has been exceeded.
--
--
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException =
  _MatchServiceError connect "ThrottlingException" . hasStatus 429


-- | Request processing failed due to an error or failure with the service.
--
--
_InternalServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceException =
  _MatchServiceError connect "InternalServiceException" . hasStatus 500


-- | The specified resource was not found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError connect "ResourceNotFoundException" . hasStatus 404


-- | The allowed limit for the resource has been reached.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError connect "LimitExceededException" . hasStatus 429

