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
    , _ManagedRuleException
    , _PolicyLengthExceededException
    , _ResourceAlreadyExistsException
    , _ConcurrentModificationException
    , _InvalidEventPatternException
    , _InternalException
    , _ResourceNotFoundException
    , _InvalidStateException
    , _LimitExceededException

    -- * AssignPublicIP
    , AssignPublicIP (..)

    -- * EventSourceState
    , EventSourceState (..)

    -- * LaunchType
    , LaunchType (..)

    -- * RuleState
    , RuleState (..)

    -- * AWSVPCConfiguration
    , AWSVPCConfiguration
    , awsVPCConfiguration
    , avcSecurityGroups
    , avcAssignPublicIP
    , avcSubnets

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

    -- * Condition
    , Condition
    , condition
    , cType
    , cKey
    , cValue

    -- * EcsParameters
    , EcsParameters
    , ecsParameters
    , epGroup
    , epPlatformVersion
    , epLaunchType
    , epTaskCount
    , epNetworkConfiguration
    , epTaskDefinitionARN

    -- * EventBus
    , EventBus
    , eventBus
    , ebARN
    , ebName
    , ebPolicy

    -- * EventSource
    , EventSource
    , eventSource
    , esCreationTime
    , esState
    , esARN
    , esCreatedBy
    , esName
    , esExpirationTime

    -- * InputTransformer
    , InputTransformer
    , inputTransformer
    , itInputPathsMap
    , itInputTemplate

    -- * KinesisParameters
    , KinesisParameters
    , kinesisParameters
    , kpPartitionKeyPath

    -- * NetworkConfiguration
    , NetworkConfiguration
    , networkConfiguration
    , ncAwsvpcConfiguration

    -- * PartnerEventSource
    , PartnerEventSource
    , partnerEventSource
    , pesARN
    , pesName

    -- * PartnerEventSourceAccount
    , PartnerEventSourceAccount
    , partnerEventSourceAccount
    , pesaCreationTime
    , pesaState
    , pesaAccount
    , pesaExpirationTime

    -- * PutEventsRequestEntry
    , PutEventsRequestEntry
    , putEventsRequestEntry
    , pereTime
    , pereDetailType
    , pereResources
    , pereEventBusName
    , pereSource
    , pereDetail

    -- * PutEventsResultEntry
    , PutEventsResultEntry
    , putEventsResultEntry
    , pereErrorCode
    , pereErrorMessage
    , pereEventId

    -- * PutPartnerEventsRequestEntry
    , PutPartnerEventsRequestEntry
    , putPartnerEventsRequestEntry
    , ppereTime
    , ppereDetailType
    , ppereResources
    , ppereSource
    , ppereDetail

    -- * PutPartnerEventsResultEntry
    , PutPartnerEventsResultEntry
    , putPartnerEventsResultEntry
    , ppereErrorCode
    , ppereErrorMessage
    , ppereEventId

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
    , rEventBusName
    , rScheduleExpression
    , rName
    , rDescription
    , rManagedBy
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

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

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

-- | API version @2015-10-07@ of the Amazon EventBridge SDK configuration.
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


-- | An AWS service created this rule on behalf of your account. That service manages it. If you see this error in response to @DeleteRule@ or @RemoveTargets@ , you can use the @Force@ parameter in those calls to delete the rule or remove targets from the rule. You can't modify these managed rules by using @DisableRule@ , @EnableRule@ , @PutTargets@ , @PutRule@ , @TagResource@ , or @UntagResource@ .
--
--
_ManagedRuleException :: AsError a => Getting (First ServiceError) a ServiceError
_ManagedRuleException =
  _MatchServiceError cloudWatchEvents "ManagedRuleException"


-- | The event bus policy is too long. For more information, see the limits.
--
--
_PolicyLengthExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyLengthExceededException =
  _MatchServiceError cloudWatchEvents "PolicyLengthExceededException"


-- | The resource that you're trying to create already exists.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException =
  _MatchServiceError cloudWatchEvents "ResourceAlreadyExistsException"


-- | There is concurrent modification on a resource.
--
--
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
  _MatchServiceError cloudWatchEvents "ConcurrentModificationException"


-- | The event pattern isn't valid.
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


-- | An entity that you specified doesn't exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError cloudWatchEvents "ResourceNotFoundException"


-- | The specified state isn't a valid state for an event source.
--
--
_InvalidStateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidStateException =
  _MatchServiceError cloudWatchEvents "InvalidStateException"


-- | You tried to create more resources than is allowed.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError cloudWatchEvents "LimitExceededException"

