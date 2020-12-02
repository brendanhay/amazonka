{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types
    (
    -- * Service Configuration
      shield

    -- * Errors
    , _InvalidResourceException
    , _InvalidParameterException
    , _LimitsExceededException
    , _InternalErrorException
    , _ResourceAlreadyExistsException
    , _OptimisticLockException
    , _InvalidOperationException
    , _LockedSubscriptionException
    , _ResourceNotFoundException

    -- * AttackLayer
    , AttackLayer (..)

    -- * AttackPropertyIdentifier
    , AttackPropertyIdentifier (..)

    -- * SubResourceType
    , SubResourceType (..)

    -- * SubscriptionState
    , SubscriptionState (..)

    -- * Unit
    , Unit (..)

    -- * AttackDetail
    , AttackDetail
    , attackDetail
    , adAttackId
    , adStartTime
    , adSubResources
    , adMitigations
    , adAttackProperties
    , adAttackCounters
    , adResourceARN
    , adEndTime

    -- * AttackProperty
    , AttackProperty
    , attackProperty
    , apAttackLayer
    , apTopContributors
    , apAttackPropertyIdentifier
    , apTotal
    , apUnit

    -- * AttackSummary
    , AttackSummary
    , attackSummary
    , asAttackVectors
    , asAttackId
    , asStartTime
    , asResourceARN
    , asEndTime

    -- * AttackVectorDescription
    , AttackVectorDescription
    , attackVectorDescription
    , avdVectorType

    -- * Contributor
    , Contributor
    , contributor
    , cValue
    , cName

    -- * Mitigation
    , Mitigation
    , mitigation
    , mMitigationName

    -- * Protection
    , Protection
    , protection
    , pResourceARN
    , pName
    , pId

    -- * SubResourceSummary
    , SubResourceSummary
    , subResourceSummary
    , srsCounters
    , srsAttackVectors
    , srsId
    , srsType

    -- * Subscription
    , Subscription
    , subscription
    , sTimeCommitmentInSeconds
    , sStartTime

    -- * SummarizedAttackVector
    , SummarizedAttackVector
    , summarizedAttackVector
    , savVectorCounters
    , savVectorType

    -- * SummarizedCounter
    , SummarizedCounter
    , summarizedCounter
    , scMax
    , scAverage
    , scN
    , scName
    , scSum
    , scUnit

    -- * TimeRange
    , TimeRange
    , timeRange
    , trFromInclusive
    , trToExclusive
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Shield.Types.Product
import Network.AWS.Shield.Types.Sum
import Network.AWS.Sign.V4

-- | API version @2016-06-02@ of the Amazon Shield SDK configuration.
shield :: Service
shield =
  Service
    { _svcAbbrev = "Shield"
    , _svcSigner = v4
    , _svcPrefix = "shield"
    , _svcVersion = "2016-06-02"
    , _svcEndpoint = defaultEndpoint shield
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Shield"
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


-- | Exception that indicates that the resource is invalid. You might not have access to the resource, or the resource might not exist.
--
--
_InvalidResourceException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResourceException = _MatchServiceError shield "InvalidResourceException"


-- | Exception that indicates that the parameters passed to the API are invalid.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError shield "InvalidParameterException"


-- | Exception that indicates that the operation would exceed a limit.
--
--
-- @Type@ is the type of limit that would be exceeded.
--
-- @Limit@ is the threshold that would be exceeded.
--
_LimitsExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitsExceededException = _MatchServiceError shield "LimitsExceededException"


-- | Exception that indicates that a problem occurred with the service infrastructure. You can retry the request.
--
--
_InternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalErrorException = _MatchServiceError shield "InternalErrorException"


-- | Exception indicating the specified resource already exists.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException =
  _MatchServiceError shield "ResourceAlreadyExistsException"


-- | Exception that indicates that the protection state has been modified by another client. You can retry the request.
--
--
_OptimisticLockException :: AsError a => Getting (First ServiceError) a ServiceError
_OptimisticLockException = _MatchServiceError shield "OptimisticLockException"


-- | Exception that indicates that the operation would not cause any change to occur.
--
--
_InvalidOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOperationException =
  _MatchServiceError shield "InvalidOperationException"


-- | Exception that indicates that the subscription you are trying to delete has not yet completed the 1-year commitment. You cannot delete this subscription.
--
--
_LockedSubscriptionException :: AsError a => Getting (First ServiceError) a ServiceError
_LockedSubscriptionException =
  _MatchServiceError shield "LockedSubscriptionException"


-- | Exception indicating the specified resource does not exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError shield "ResourceNotFoundException"

