{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types
    (
    -- * Service Configuration
      cloudWatchEvents

    -- * Errors
    , _ConcurrentModificationException
    , _InvalidEventPatternException
    , _InternalException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * RuleState
    , RuleState (..)

    -- * PutEventsRequestEntry
    , PutEventsRequestEntry
    , putEventsRequestEntry
    , pereTime
    , pereDetailType
    , pereResources
    , pereSource
    , pereDetail

    -- * PutEventsResultEntry
    , PutEventsResultEntry
    , putEventsResultEntry
    , pereErrorCode
    , pereErrorMessage
    , pereEventId

    -- * PutTargetsResultEntry
    , PutTargetsResultEntry
    , putTargetsResultEntry
    , ptreTargetId
    , ptreErrorCode
    , ptreErrorMessage

    -- * RemoveTargetsResultEntry
    , RemoveTargetsResultEntry
    , removeTargetsResultEntry
    , rtreTargetId
    , rtreErrorCode
    , rtreErrorMessage

    -- * Rule
    , Rule
    , rule
    , rEventPattern
    , rState
    , rARN
    , rScheduleExpression
    , rName
    , rDescription
    , rRoleARN

    -- * Target
    , Target
    , target
    , tInput
    , tInputPath
    , tId
    , tARN
    ) where

import           Network.AWS.CloudWatchEvents.Types.Product
import           Network.AWS.CloudWatchEvents.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2015-10-07' of the Amazon CloudWatch Events SDK configuration.
cloudWatchEvents :: Service
cloudWatchEvents =
    Service
    { _svcAbbrev = "CloudWatchEvents"
    , _svcSigner = v4
    , _svcPrefix = "events"
    , _svcVersion = "2015-10-07"
    , _svcEndpoint = defaultEndpoint cloudWatchEvents
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError
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
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | This exception occurs if there is concurrent modification on rule or
-- target.
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
    _ServiceError . hasCode "ConcurrentModificationException"

-- | The event pattern is invalid.
_InvalidEventPatternException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEventPatternException =
    _ServiceError . hasCode "InvalidEventPatternException"

-- | This exception occurs due to unexpected causes.
_InternalException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalException = _ServiceError . hasCode "InternalException"

-- | The rule does not exist.
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasCode "ResourceNotFoundException"

-- | This exception occurs if you try to create more rules or add more
-- targets to a rule than allowed by default.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"
