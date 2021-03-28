-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _ExecutionLimitExceeded
    , _InvalidDefinition
    , _StateMachineLimitExceeded
    , _ExecutionAlreadyExists
    , _StateMachineAlreadyExists
    , _TaskTimedOut
    , _InvalidExecutionInput
    , _InvalidOutput
    , _InvalidName
    , _TaskDoesNotExist
    , _ActivityDoesNotExist
    , _StateMachineDeleting
    , _StateMachineTypeNotSupported
    , _MissingRequiredParameter
    , _InvalidArn
    , _InvalidToken
    , _InvalidLoggingConfiguration
    , _ActivityWorkerLimitExceeded
    , _InvalidTracingConfiguration
    , _ActivityLimitExceeded
    , _TooManyTags
    , _ExecutionDoesNotExist
    , _StateMachineDoesNotExist
    , _ResourceNotFound

    -- * TaskSubmitFailedEventDetails
    , TaskSubmitFailedEventDetails (..)
    , mkTaskSubmitFailedEventDetails
    , tsfedResourceType
    , tsfedResource
    , tsfedCause
    , tsfedError

    -- * SensitiveError
    , SensitiveError (..)

    -- * CloudWatchLogsLogGroup
    , CloudWatchLogsLogGroup (..)
    , mkCloudWatchLogsLogGroup
    , cwllgLogGroupArn

    -- * SensitiveData
    , SensitiveData (..)

    -- * MapStateStartedEventDetails
    , MapStateStartedEventDetails (..)
    , mkMapStateStartedEventDetails
    , mssedLength

    -- * CloudWatchEventsExecutionDataDetails
    , CloudWatchEventsExecutionDataDetails (..)
    , mkCloudWatchEventsExecutionDataDetails
    , cweeddIncluded

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * TaskStartedEventDetails
    , TaskStartedEventDetails (..)
    , mkTaskStartedEventDetails
    , tsedsResourceType
    , tsedsResource

    -- * ActivityListItem
    , ActivityListItem (..)
    , mkActivityListItem
    , aliActivityArn
    , aliName
    , aliCreationDate

    -- * LogLevel
    , LogLevel (..)

    -- * ActivityStartedEventDetails
    , ActivityStartedEventDetails (..)
    , mkActivityStartedEventDetails
    , asedWorkerName

    -- * StateMachineType
    , StateMachineType (..)

    -- * TaskSubmittedEventDetails
    , TaskSubmittedEventDetails (..)
    , mkTaskSubmittedEventDetails
    , tsedfResourceType
    , tsedfResource
    , tsedfOutput
    , tsedfOutputDetails

    -- * Definition
    , Definition (..)

    -- * Arn
    , Arn (..)

    -- * ExecutionListItem
    , ExecutionListItem (..)
    , mkExecutionListItem
    , eliExecutionArn
    , eliStateMachineArn
    , eliName
    , eliStatus
    , eliStartDate
    , eliStopDate

    -- * SensitiveCause
    , SensitiveCause (..)

    -- * StateMachineListItem
    , StateMachineListItem (..)
    , mkStateMachineListItem
    , smliStateMachineArn
    , smliName
    , smliType
    , smliCreationDate

    -- * LambdaFunctionSucceededEventDetails
    , LambdaFunctionSucceededEventDetails (..)
    , mkLambdaFunctionSucceededEventDetails
    , lfsedOutput
    , lfsedOutputDetails

    -- * TaskSucceededEventDetails
    , TaskSucceededEventDetails (..)
    , mkTaskSucceededEventDetails
    , tsedgResourceType
    , tsedgResource
    , tsedgOutput
    , tsedgOutputDetails

    -- * LambdaFunctionStartFailedEventDetails
    , LambdaFunctionStartFailedEventDetails (..)
    , mkLambdaFunctionStartFailedEventDetails
    , lCause
    , lError

    -- * StateExitedEventDetails
    , StateExitedEventDetails (..)
    , mkStateExitedEventDetails
    , sName
    , sOutput
    , sOutputDetails

    -- * TaskStartFailedEventDetails
    , TaskStartFailedEventDetails (..)
    , mkTaskStartFailedEventDetails
    , tResourceType
    , tResource
    , tCause
    , tError

    -- * LogDestination
    , LogDestination (..)
    , mkLogDestination
    , ldCloudWatchLogsLogGroup

    -- * ActivitySucceededEventDetails
    , ActivitySucceededEventDetails (..)
    , mkActivitySucceededEventDetails
    , asedOutput
    , asedOutputDetails

    -- * ListExecutionsPageToken
    , ListExecutionsPageToken (..)

    -- * ExecutionStatus
    , ExecutionStatus (..)

    -- * StateMachineStatus
    , StateMachineStatus (..)

    -- * HistoryEventType
    , HistoryEventType (..)

    -- * ActivityTimedOutEventDetails
    , ActivityTimedOutEventDetails (..)
    , mkActivityTimedOutEventDetails
    , atoedCause
    , atoedError

    -- * TaskTimedOutEventDetails
    , TaskTimedOutEventDetails (..)
    , mkTaskTimedOutEventDetails
    , ttoedResourceType
    , ttoedResource
    , ttoedCause
    , ttoedError

    -- * TracingConfiguration
    , TracingConfiguration (..)
    , mkTracingConfiguration
    , tcEnabled

    -- * LambdaFunctionTimedOutEventDetails
    , LambdaFunctionTimedOutEventDetails (..)
    , mkLambdaFunctionTimedOutEventDetails
    , lftoedCause
    , lftoedError

    -- * ExecutionFailedEventDetails
    , ExecutionFailedEventDetails (..)
    , mkExecutionFailedEventDetails
    , efedCause
    , efedError

    -- * ExecutionAbortedEventDetails
    , ExecutionAbortedEventDetails (..)
    , mkExecutionAbortedEventDetails
    , eaedCause
    , eaedError

    -- * ExecutionSucceededEventDetails
    , ExecutionSucceededEventDetails (..)
    , mkExecutionSucceededEventDetails
    , esedOutput
    , esedOutputDetails

    -- * Name
    , Name (..)

    -- * LambdaFunctionScheduledEventDetails
    , LambdaFunctionScheduledEventDetails (..)
    , mkLambdaFunctionScheduledEventDetails
    , lfsedResource
    , lfsedInput
    , lfsedInputDetails
    , lfsedTimeoutInSeconds

    -- * ActivityScheduledEventDetails
    , ActivityScheduledEventDetails (..)
    , mkActivityScheduledEventDetails
    , asedResource
    , asedHeartbeatInSeconds
    , asedInput
    , asedInputDetails
    , asedTimeoutInSeconds

    -- * TaskScheduledEventDetails
    , TaskScheduledEventDetails (..)
    , mkTaskScheduledEventDetails
    , tsedResourceType
    , tsedResource
    , tsedRegion
    , tsedParameters
    , tsedHeartbeatInSeconds
    , tsedTimeoutInSeconds

    -- * LambdaFunctionScheduleFailedEventDetails
    , LambdaFunctionScheduleFailedEventDetails (..)
    , mkLambdaFunctionScheduleFailedEventDetails
    , lfsfedCause
    , lfsfedError

    -- * ActivityScheduleFailedEventDetails
    , ActivityScheduleFailedEventDetails (..)
    , mkActivityScheduleFailedEventDetails
    , asfedCause
    , asfedError

    -- * ExecutionStartedEventDetails
    , ExecutionStartedEventDetails (..)
    , mkExecutionStartedEventDetails
    , esedInput
    , esedInputDetails
    , esedRoleArn

    -- * TagKey
    , TagKey (..)

    -- * MapIterationEventDetails
    , MapIterationEventDetails (..)
    , mkMapIterationEventDetails
    , miedIndex
    , miedName

    -- * StateEnteredEventDetails
    , StateEnteredEventDetails (..)
    , mkStateEnteredEventDetails
    , seedName
    , seedInput
    , seedInputDetails

    -- * TaskFailedEventDetails
    , TaskFailedEventDetails (..)
    , mkTaskFailedEventDetails
    , tfedResourceType
    , tfedResource
    , tfedCause
    , tfedError

    -- * LambdaFunctionFailedEventDetails
    , LambdaFunctionFailedEventDetails (..)
    , mkLambdaFunctionFailedEventDetails
    , lffedCause
    , lffedError

    -- * PageToken
    , PageToken (..)

    -- * ActivityFailedEventDetails
    , ActivityFailedEventDetails (..)
    , mkActivityFailedEventDetails
    , afedCause
    , afedError

    -- * LoggingConfiguration
    , LoggingConfiguration (..)
    , mkLoggingConfiguration
    , lcDestinations
    , lcIncludeExecutionData
    , lcLevel

    -- * ExecutionTimedOutEventDetails
    , ExecutionTimedOutEventDetails (..)
    , mkExecutionTimedOutEventDetails
    , etoedCause
    , etoedError

    -- * HistoryEventExecutionDataDetails
    , HistoryEventExecutionDataDetails (..)
    , mkHistoryEventExecutionDataDetails
    , heeddTruncated

    -- * HistoryEvent
    , HistoryEvent (..)
    , mkHistoryEvent
    , heTimestamp
    , heType
    , heId
    , heActivityFailedEventDetails
    , heActivityScheduleFailedEventDetails
    , heActivityScheduledEventDetails
    , heActivityStartedEventDetails
    , heActivitySucceededEventDetails
    , heActivityTimedOutEventDetails
    , heExecutionAbortedEventDetails
    , heExecutionFailedEventDetails
    , heExecutionStartedEventDetails
    , heExecutionSucceededEventDetails
    , heExecutionTimedOutEventDetails
    , heLambdaFunctionFailedEventDetails
    , heLambdaFunctionScheduleFailedEventDetails
    , heLambdaFunctionScheduledEventDetails
    , heLambdaFunctionStartFailedEventDetails
    , heLambdaFunctionSucceededEventDetails
    , heLambdaFunctionTimedOutEventDetails
    , heMapIterationAbortedEventDetails
    , heMapIterationFailedEventDetails
    , heMapIterationStartedEventDetails
    , heMapIterationSucceededEventDetails
    , heMapStateStartedEventDetails
    , hePreviousEventId
    , heStateEnteredEventDetails
    , heStateExitedEventDetails
    , heTaskFailedEventDetails
    , heTaskScheduledEventDetails
    , heTaskStartFailedEventDetails
    , heTaskStartedEventDetails
    , heTaskSubmitFailedEventDetails
    , heTaskSubmittedEventDetails
    , heTaskSucceededEventDetails
    , heTaskTimedOutEventDetails

    -- * TraceHeader
    , TraceHeader (..)

    -- * TaskToken
    , TaskToken (..)

    -- * SyncExecutionStatus
    , SyncExecutionStatus (..)

    -- * BillingDetails
    , BillingDetails (..)
    , mkBillingDetails
    , bdBilledDurationInMilliseconds
    , bdBilledMemoryUsedInMB

    -- * Output
    , Output (..)

    -- * StateMachineArn
    , StateMachineArn (..)

    -- * Input
    , Input (..)

    -- * ResourceType
    , ResourceType (..)

    -- * Resource
    , Resource (..)

    -- * Cause
    , Cause (..)

    -- * Error
    , Error (..)

    -- * LogGroupArn
    , LogGroupArn (..)

    -- * NextToken
    , NextToken (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * ExecutionArn
    , ExecutionArn (..)

    -- * ActivityArn
    , ActivityArn (..)

    -- * WorkerName
    , WorkerName (..)

    -- * Parameters
    , Parameters (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
  
  
  
import Network.AWS.StepFunctions.Types.TaskSubmitFailedEventDetails
  
  
import Network.AWS.StepFunctions.Types.SensitiveError
  
import Network.AWS.StepFunctions.Types.CloudWatchLogsLogGroup
  
import Network.AWS.StepFunctions.Types.SensitiveData
  
  
import Network.AWS.StepFunctions.Types.MapStateStartedEventDetails
  
import Network.AWS.StepFunctions.Types.CloudWatchEventsExecutionDataDetails
  
import Network.AWS.StepFunctions.Types.Tag
  
import Network.AWS.StepFunctions.Types.TaskStartedEventDetails
  
import Network.AWS.StepFunctions.Types.ActivityListItem
  
import Network.AWS.StepFunctions.Types.LogLevel
  
import Network.AWS.StepFunctions.Types.ActivityStartedEventDetails
  
import Network.AWS.StepFunctions.Types.StateMachineType
  
import Network.AWS.StepFunctions.Types.TaskSubmittedEventDetails
  
  
import Network.AWS.StepFunctions.Types.Definition
  
import Network.AWS.StepFunctions.Types.Arn
  
import Network.AWS.StepFunctions.Types.ExecutionListItem
  
  
import Network.AWS.StepFunctions.Types.SensitiveCause
  
import Network.AWS.StepFunctions.Types.StateMachineListItem
  
import Network.AWS.StepFunctions.Types.LambdaFunctionSucceededEventDetails
  
import Network.AWS.StepFunctions.Types.TaskSucceededEventDetails
  
import Network.AWS.StepFunctions.Types.LambdaFunctionStartFailedEventDetails
  
import Network.AWS.StepFunctions.Types.StateExitedEventDetails
  
import Network.AWS.StepFunctions.Types.TaskStartFailedEventDetails
  
import Network.AWS.StepFunctions.Types.LogDestination
  
import Network.AWS.StepFunctions.Types.ActivitySucceededEventDetails
  
import Network.AWS.StepFunctions.Types.ListExecutionsPageToken
  
  
  
import Network.AWS.StepFunctions.Types.ExecutionStatus
  
  
import Network.AWS.StepFunctions.Types.StateMachineStatus
  
  
import Network.AWS.StepFunctions.Types.HistoryEventType
  
  
  
  
import Network.AWS.StepFunctions.Types.ActivityTimedOutEventDetails
  
  
import Network.AWS.StepFunctions.Types.TaskTimedOutEventDetails
  
import Network.AWS.StepFunctions.Types.TracingConfiguration
  
import Network.AWS.StepFunctions.Types.LambdaFunctionTimedOutEventDetails
  
import Network.AWS.StepFunctions.Types.ExecutionFailedEventDetails
  
import Network.AWS.StepFunctions.Types.ExecutionAbortedEventDetails
  
import Network.AWS.StepFunctions.Types.ExecutionSucceededEventDetails
  
import Network.AWS.StepFunctions.Types.Name
  
  
import Network.AWS.StepFunctions.Types.LambdaFunctionScheduledEventDetails
  
import Network.AWS.StepFunctions.Types.ActivityScheduledEventDetails
  
import Network.AWS.StepFunctions.Types.TaskScheduledEventDetails
  
  
import Network.AWS.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails
  
  
  
import Network.AWS.StepFunctions.Types.ActivityScheduleFailedEventDetails
  
import Network.AWS.StepFunctions.Types.ExecutionStartedEventDetails
  
import Network.AWS.StepFunctions.Types.TagKey
  
import Network.AWS.StepFunctions.Types.MapIterationEventDetails
  
import Network.AWS.StepFunctions.Types.StateEnteredEventDetails
  
import Network.AWS.StepFunctions.Types.TaskFailedEventDetails
  
import Network.AWS.StepFunctions.Types.LambdaFunctionFailedEventDetails
  
import Network.AWS.StepFunctions.Types.PageToken
  
import Network.AWS.StepFunctions.Types.ActivityFailedEventDetails
  
import Network.AWS.StepFunctions.Types.LoggingConfiguration
  
import Network.AWS.StepFunctions.Types.ExecutionTimedOutEventDetails
  
  
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails
  
import Network.AWS.StepFunctions.Types.HistoryEvent
  
  
import Network.AWS.StepFunctions.Types.TraceHeader
  
import Network.AWS.StepFunctions.Types.TaskToken
  
  
import Network.AWS.StepFunctions.Types.SyncExecutionStatus
  
import Network.AWS.StepFunctions.Types.BillingDetails
  
  
  
import Network.AWS.StepFunctions.Types.Output
  
import Network.AWS.StepFunctions.Types.StateMachineArn
  
import Network.AWS.StepFunctions.Types.Input
  
import Network.AWS.StepFunctions.Types.ResourceType
  
import Network.AWS.StepFunctions.Types.Resource
  
import Network.AWS.StepFunctions.Types.Cause
  
import Network.AWS.StepFunctions.Types.Error
  
import Network.AWS.StepFunctions.Types.LogGroupArn
  
import Network.AWS.StepFunctions.Types.NextToken
  
import Network.AWS.StepFunctions.Types.Key
  
import Network.AWS.StepFunctions.Types.Value
  
import Network.AWS.StepFunctions.Types.ExecutionArn
  
import Network.AWS.StepFunctions.Types.ActivityArn
  
import Network.AWS.StepFunctions.Types.WorkerName
  
import Network.AWS.StepFunctions.Types.Parameters
  

-- | API version @2016-11-23@ of the Amazon Step Functions SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "StepFunctions",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "states",
                 Core._svcVersion = "2016-11-23", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "StepFunctions",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The maximum number of running executions has been reached. Running executions must end or be stopped before a new execution can be started.
_ExecutionLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExecutionLimitExceeded
  = Core._MatchServiceError mkServiceConfig "ExecutionLimitExceeded"
{-# INLINEABLE _ExecutionLimitExceeded #-}
{-# DEPRECATED _ExecutionLimitExceeded "Use generic-lens or generic-optics instead"  #-}

-- | The provided Amazon States Language definition is invalid.
_InvalidDefinition :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDefinition
  = Core._MatchServiceError mkServiceConfig "InvalidDefinition"
{-# INLINEABLE _InvalidDefinition #-}
{-# DEPRECATED _InvalidDefinition "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of state machines has been reached. Existing state machines must be deleted before a new state machine can be created.
_StateMachineLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StateMachineLimitExceeded
  = Core._MatchServiceError mkServiceConfig
      "StateMachineLimitExceeded"
{-# INLINEABLE _StateMachineLimitExceeded #-}
{-# DEPRECATED _StateMachineLimitExceeded "Use generic-lens or generic-optics instead"  #-}

-- | The execution has the same @name@ as another execution (but a different @input@ ).
_ExecutionAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExecutionAlreadyExists
  = Core._MatchServiceError mkServiceConfig "ExecutionAlreadyExists"
{-# INLINEABLE _ExecutionAlreadyExists #-}
{-# DEPRECATED _ExecutionAlreadyExists "Use generic-lens or generic-optics instead"  #-}

-- | A state machine with the same name but a different definition or role ARN already exists.
_StateMachineAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StateMachineAlreadyExists
  = Core._MatchServiceError mkServiceConfig
      "StateMachineAlreadyExists"
{-# INLINEABLE _StateMachineAlreadyExists #-}
{-# DEPRECATED _StateMachineAlreadyExists "Use generic-lens or generic-optics instead"  #-}

-- | Prism for 'TaskTimedOut' errors.
_TaskTimedOut :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TaskTimedOut
  = Core._MatchServiceError mkServiceConfig "TaskTimedOut"
{-# INLINEABLE _TaskTimedOut #-}
{-# DEPRECATED _TaskTimedOut "Use generic-lens or generic-optics instead"  #-}

-- | The provided JSON input data is invalid.
_InvalidExecutionInput :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidExecutionInput
  = Core._MatchServiceError mkServiceConfig "InvalidExecutionInput"
{-# INLINEABLE _InvalidExecutionInput #-}
{-# DEPRECATED _InvalidExecutionInput "Use generic-lens or generic-optics instead"  #-}

-- | The provided JSON output data is invalid.
_InvalidOutput :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOutput
  = Core._MatchServiceError mkServiceConfig "InvalidOutput"
{-# INLINEABLE _InvalidOutput #-}
{-# DEPRECATED _InvalidOutput "Use generic-lens or generic-optics instead"  #-}

-- | The provided name is invalid.
_InvalidName :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidName
  = Core._MatchServiceError mkServiceConfig "InvalidName"
{-# INLINEABLE _InvalidName #-}
{-# DEPRECATED _InvalidName "Use generic-lens or generic-optics instead"  #-}

-- | Prism for 'TaskDoesNotExist' errors.
_TaskDoesNotExist :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TaskDoesNotExist
  = Core._MatchServiceError mkServiceConfig "TaskDoesNotExist"
{-# INLINEABLE _TaskDoesNotExist #-}
{-# DEPRECATED _TaskDoesNotExist "Use generic-lens or generic-optics instead"  #-}

-- | The specified activity does not exist.
_ActivityDoesNotExist :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ActivityDoesNotExist
  = Core._MatchServiceError mkServiceConfig "ActivityDoesNotExist"
{-# INLINEABLE _ActivityDoesNotExist #-}
{-# DEPRECATED _ActivityDoesNotExist "Use generic-lens or generic-optics instead"  #-}

-- | The specified state machine is being deleted.
_StateMachineDeleting :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StateMachineDeleting
  = Core._MatchServiceError mkServiceConfig "StateMachineDeleting"
{-# INLINEABLE _StateMachineDeleting #-}
{-# DEPRECATED _StateMachineDeleting "Use generic-lens or generic-optics instead"  #-}

-- | 
_StateMachineTypeNotSupported :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StateMachineTypeNotSupported
  = Core._MatchServiceError mkServiceConfig
      "StateMachineTypeNotSupported"
{-# INLINEABLE _StateMachineTypeNotSupported #-}
{-# DEPRECATED _StateMachineTypeNotSupported "Use generic-lens or generic-optics instead"  #-}

-- | Request is missing a required parameter. This error occurs if both @definition@ and @roleArn@ are not specified.
_MissingRequiredParameter :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MissingRequiredParameter
  = Core._MatchServiceError mkServiceConfig
      "MissingRequiredParameter"
{-# INLINEABLE _MissingRequiredParameter #-}
{-# DEPRECATED _MissingRequiredParameter "Use generic-lens or generic-optics instead"  #-}

-- | The provided Amazon Resource Name (ARN) is invalid.
_InvalidArn :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArn = Core._MatchServiceError mkServiceConfig "InvalidArn"
{-# INLINEABLE _InvalidArn #-}
{-# DEPRECATED _InvalidArn "Use generic-lens or generic-optics instead"  #-}

-- | The provided token is invalid.
_InvalidToken :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidToken
  = Core._MatchServiceError mkServiceConfig "InvalidToken"
{-# INLINEABLE _InvalidToken #-}
{-# DEPRECATED _InvalidToken "Use generic-lens or generic-optics instead"  #-}

-- | 
_InvalidLoggingConfiguration :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLoggingConfiguration
  = Core._MatchServiceError mkServiceConfig
      "InvalidLoggingConfiguration"
{-# INLINEABLE _InvalidLoggingConfiguration #-}
{-# DEPRECATED _InvalidLoggingConfiguration "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of workers concurrently polling for activity tasks has been reached.
_ActivityWorkerLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ActivityWorkerLimitExceeded
  = Core._MatchServiceError mkServiceConfig
      "ActivityWorkerLimitExceeded"
{-# INLINEABLE _ActivityWorkerLimitExceeded #-}
{-# DEPRECATED _ActivityWorkerLimitExceeded "Use generic-lens or generic-optics instead"  #-}

-- | Your @tracingConfiguration@ key does not match, or @enabled@ has not been set to @true@ or @false@ .
_InvalidTracingConfiguration :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTracingConfiguration
  = Core._MatchServiceError mkServiceConfig
      "InvalidTracingConfiguration"
{-# INLINEABLE _InvalidTracingConfiguration #-}
{-# DEPRECATED _InvalidTracingConfiguration "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of activities has been reached. Existing activities must be deleted before a new activity can be created.
_ActivityLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ActivityLimitExceeded
  = Core._MatchServiceError mkServiceConfig "ActivityLimitExceeded"
{-# INLINEABLE _ActivityLimitExceeded #-}
{-# DEPRECATED _ActivityLimitExceeded "Use generic-lens or generic-optics instead"  #-}

-- | You've exceeded the number of tags allowed for a resource. See the <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html Limits Topic> in the AWS Step Functions Developer Guide.
_TooManyTags :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTags
  = Core._MatchServiceError mkServiceConfig "TooManyTags"
{-# INLINEABLE _TooManyTags #-}
{-# DEPRECATED _TooManyTags "Use generic-lens or generic-optics instead"  #-}

-- | The specified execution does not exist.
_ExecutionDoesNotExist :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExecutionDoesNotExist
  = Core._MatchServiceError mkServiceConfig "ExecutionDoesNotExist"
{-# INLINEABLE _ExecutionDoesNotExist #-}
{-# DEPRECATED _ExecutionDoesNotExist "Use generic-lens or generic-optics instead"  #-}

-- | The specified state machine does not exist.
_StateMachineDoesNotExist :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StateMachineDoesNotExist
  = Core._MatchServiceError mkServiceConfig
      "StateMachineDoesNotExist"
{-# INLINEABLE _StateMachineDoesNotExist #-}
{-# DEPRECATED _StateMachineDoesNotExist "Use generic-lens or generic-optics instead"  #-}

-- | Could not find the referenced resource. Only state machine and activity ARNs are supported.
_ResourceNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFound
  = Core._MatchServiceError mkServiceConfig "ResourceNotFound"
{-# INLINEABLE _ResourceNotFound #-}
{-# DEPRECATED _ResourceNotFound "Use generic-lens or generic-optics instead"  #-}
