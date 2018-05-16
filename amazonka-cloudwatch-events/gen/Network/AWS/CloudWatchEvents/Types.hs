{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types
    (
    -- * Service Configuration
      cloudWatchEvents

    -- * Errors
    , _PolicyLengthExceededException
    , _ConcurrentModificationException
    , _InvalidEventPatternException
    , _InternalException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * RuleState
    , RuleState (..)

    -- * BatchArrayProperties
    , BatchArrayProperties
    , batchArrayProperties
    , bapSize

    -- * BatchParameters
    , BatchParameters
    , batchParameters
    , bpRetryStrategy
    , bpArrayProperties
    , bpJobDefinition
    , bpJobName

    -- * BatchRetryStrategy
    , BatchRetryStrategy
    , batchRetryStrategy
    , brsAttempts

    -- * EcsParameters
    , EcsParameters
    , ecsParameters
    , epTaskCount
    , epTaskDefinitionARN

    -- * InputTransformer
    , InputTransformer
    , inputTransformer
    , itInputPathsMap
    , itInputTemplate

    -- * KinesisParameters
    , KinesisParameters
    , kinesisParameters
    , kpPartitionKeyPath

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

    -- * RunCommandParameters
    , RunCommandParameters
    , runCommandParameters
    , rcpRunCommandTargets

    -- * RunCommandTarget
    , RunCommandTarget
    , runCommandTarget
    , rctKey
    , rctValues

    -- * SqsParameters
    , SqsParameters
    , sqsParameters
    , spMessageGroupId

    -- * Target
    , Target
    , target
    , tRunCommandParameters
    , tKinesisParameters
    , tInputTransformer
    , tSqsParameters
    , tInput
    , tBatchParameters
    , tEcsParameters
    , tInputPath
    , tRoleARN
    , tId
    , tARN
    ) where

import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.CloudWatchEvents.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-10-07@ of the Amazon CloudWatch Events SDK configuration.
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
    , _svcError = parseJSONError "CloudWatchEvents"
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


-- | The event bus policy is too long. For more information, see the limits.
--
--
_PolicyLengthExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyLengthExceededException =
  _MatchServiceError cloudWatchEvents "PolicyLengthExceededException"


-- | There is concurrent modification on a rule or target.
--
--
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
  _MatchServiceError cloudWatchEvents "ConcurrentModificationException"


-- | The event pattern is not valid.
--
--
_InvalidEventPatternException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEventPatternException =
  _MatchServiceError cloudWatchEvents "InvalidEventPatternException"


-- | This exception occurs due to unexpected causes.
--
--
_InternalException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalException = _MatchServiceError cloudWatchEvents "InternalException"


-- | An entity that you specified does not exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError cloudWatchEvents "ResourceNotFoundException"


-- | You tried to create more rules or add more targets to a rule than is allowed.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError cloudWatchEvents "LimitExceededException"

