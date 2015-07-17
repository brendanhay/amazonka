{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types
    (
    -- * Service
      SWF

    -- * Errors
    , _LimitExceededFault
    , _WorkflowExecutionAlreadyStartedFault
    , _DomainAlreadyExistsFault
    , _UnknownResourceFault
    , _OperationNotPermittedFault
    , _DefaultUndefinedFault
    , _TypeDeprecatedFault
    , _TypeAlreadyExistsFault
    , _DomainDeprecatedFault

    -- * ActivityTaskTimeoutType
    , ActivityTaskTimeoutType (..)

    -- * CancelTimerFailedCause
    , CancelTimerFailedCause (..)

    -- * CancelWorkflowExecutionFailedCause
    , CancelWorkflowExecutionFailedCause (..)

    -- * ChildPolicy
    , ChildPolicy (..)

    -- * CloseStatus
    , CloseStatus (..)

    -- * CompleteWorkflowExecutionFailedCause
    , CompleteWorkflowExecutionFailedCause (..)

    -- * ContinueAsNewWorkflowExecutionFailedCause
    , ContinueAsNewWorkflowExecutionFailedCause (..)

    -- * DecisionTaskTimeoutType
    , DecisionTaskTimeoutType (..)

    -- * DecisionType
    , DecisionType (..)

    -- * EventType
    , EventType (..)

    -- * ExecutionStatus
    , ExecutionStatus (..)

    -- * FailWorkflowExecutionFailedCause
    , FailWorkflowExecutionFailedCause (..)

    -- * RecordMarkerFailedCause
    , RecordMarkerFailedCause (..)

    -- * RegistrationStatus
    , RegistrationStatus (..)

    -- * RequestCancelActivityTaskFailedCause
    , RequestCancelActivityTaskFailedCause (..)

    -- * RequestCancelExternalWorkflowExecutionFailedCause
    , RequestCancelExternalWorkflowExecutionFailedCause (..)

    -- * ScheduleActivityTaskFailedCause
    , ScheduleActivityTaskFailedCause (..)

    -- * SignalExternalWorkflowExecutionFailedCause
    , SignalExternalWorkflowExecutionFailedCause (..)

    -- * StartChildWorkflowExecutionFailedCause
    , StartChildWorkflowExecutionFailedCause (..)

    -- * StartTimerFailedCause
    , StartTimerFailedCause (..)

    -- * WorkflowExecutionCancelRequestedCause
    , WorkflowExecutionCancelRequestedCause (..)

    -- * WorkflowExecutionTerminatedCause
    , WorkflowExecutionTerminatedCause (..)

    -- * WorkflowExecutionTimeoutType
    , WorkflowExecutionTimeoutType (..)

    -- * ActivityTaskCancelRequestedEventAttributes
    , ActivityTaskCancelRequestedEventAttributes
    , activityTaskCancelRequestedEventAttributes
    , atcreaDecisionTaskCompletedEventId
    , atcreaActivityId

    -- * ActivityTaskCanceledEventAttributes
    , ActivityTaskCanceledEventAttributes
    , activityTaskCanceledEventAttributes
    , actLatestCancelRequestedEventId
    , actDetails
    , actScheduledEventId
    , actStartedEventId

    -- * ActivityTaskCompletedEventAttributes
    , ActivityTaskCompletedEventAttributes
    , activityTaskCompletedEventAttributes
    , atceaResult
    , atceaScheduledEventId
    , atceaStartedEventId

    -- * ActivityTaskFailedEventAttributes
    , ActivityTaskFailedEventAttributes
    , activityTaskFailedEventAttributes
    , atfeaReason
    , atfeaDetails
    , atfeaScheduledEventId
    , atfeaStartedEventId

    -- * ActivityTaskScheduledEventAttributes
    , ActivityTaskScheduledEventAttributes
    , activityTaskScheduledEventAttributes
    , atseaControl
    , atseaScheduleToCloseTimeout
    , atseaHeartbeatTimeout
    , atseaInput
    , atseaTaskPriority
    , atseaScheduleToStartTimeout
    , atseaStartToCloseTimeout
    , atseaActivityType
    , atseaActivityId
    , atseaTaskList
    , atseaDecisionTaskCompletedEventId

    -- * ActivityTaskStartedEventAttributes
    , ActivityTaskStartedEventAttributes
    , activityTaskStartedEventAttributes
    , atseaIdentity
    , atseaScheduledEventId

    -- * ActivityTaskTimedOutEventAttributes
    , ActivityTaskTimedOutEventAttributes
    , activityTaskTimedOutEventAttributes
    , attoeaDetails
    , attoeaTimeoutType
    , attoeaScheduledEventId
    , attoeaStartedEventId

    -- * ActivityType
    , ActivityType
    , activityType
    , atName
    , atVersion

    -- * ActivityTypeConfiguration
    , ActivityTypeConfiguration
    , activityTypeConfiguration
    , atcDefaultTaskScheduleToStartTimeout
    , atcDefaultTaskList
    , atcDefaultTaskPriority
    , atcDefaultTaskHeartbeatTimeout
    , atcDefaultTaskScheduleToCloseTimeout
    , atcDefaultTaskStartToCloseTimeout

    -- * ActivityTypeInfo
    , ActivityTypeInfo
    , activityTypeInfo
    , atiDeprecationDate
    , atiDescription
    , atiActivityType
    , atiStatus
    , atiCreationDate

    -- * CancelTimerDecisionAttributes
    , CancelTimerDecisionAttributes
    , cancelTimerDecisionAttributes
    , ctdaTimerId

    -- * CancelTimerFailedEventAttributes
    , CancelTimerFailedEventAttributes
    , cancelTimerFailedEventAttributes
    , ctfeaTimerId
    , ctfeaCause
    , ctfeaDecisionTaskCompletedEventId

    -- * CancelWorkflowExecutionDecisionAttributes
    , CancelWorkflowExecutionDecisionAttributes
    , cancelWorkflowExecutionDecisionAttributes
    , cwedaDetails

    -- * CancelWorkflowExecutionFailedEventAttributes
    , CancelWorkflowExecutionFailedEventAttributes
    , cancelWorkflowExecutionFailedEventAttributes
    , canCause
    , canDecisionTaskCompletedEventId

    -- * ChildWorkflowExecutionCanceledEventAttributes
    , ChildWorkflowExecutionCanceledEventAttributes
    , childWorkflowExecutionCanceledEventAttributes
    , chiDetails
    , chiWorkflowExecution
    , chiWorkflowType
    , chiInitiatedEventId
    , chiStartedEventId

    -- * ChildWorkflowExecutionCompletedEventAttributes
    , ChildWorkflowExecutionCompletedEventAttributes
    , childWorkflowExecutionCompletedEventAttributes
    , cweceaResult
    , cweceaWorkflowExecution
    , cweceaWorkflowType
    , cweceaInitiatedEventId
    , cweceaStartedEventId

    -- * ChildWorkflowExecutionFailedEventAttributes
    , ChildWorkflowExecutionFailedEventAttributes
    , childWorkflowExecutionFailedEventAttributes
    , cwefeaReason
    , cwefeaDetails
    , cwefeaWorkflowExecution
    , cwefeaWorkflowType
    , cwefeaInitiatedEventId
    , cwefeaStartedEventId

    -- * ChildWorkflowExecutionStartedEventAttributes
    , ChildWorkflowExecutionStartedEventAttributes
    , childWorkflowExecutionStartedEventAttributes
    , cweseaWorkflowExecution
    , cweseaWorkflowType
    , cweseaInitiatedEventId

    -- * ChildWorkflowExecutionTerminatedEventAttributes
    , ChildWorkflowExecutionTerminatedEventAttributes
    , childWorkflowExecutionTerminatedEventAttributes
    , cweteaWorkflowExecution
    , cweteaWorkflowType
    , cweteaInitiatedEventId
    , cweteaStartedEventId

    -- * ChildWorkflowExecutionTimedOutEventAttributes
    , ChildWorkflowExecutionTimedOutEventAttributes
    , childWorkflowExecutionTimedOutEventAttributes
    , cwetoeaWorkflowExecution
    , cwetoeaWorkflowType
    , cwetoeaTimeoutType
    , cwetoeaInitiatedEventId
    , cwetoeaStartedEventId

    -- * CloseStatusFilter
    , CloseStatusFilter
    , closeStatusFilter
    , csfStatus

    -- * CompleteWorkflowExecutionDecisionAttributes
    , CompleteWorkflowExecutionDecisionAttributes
    , completeWorkflowExecutionDecisionAttributes
    , cwedaResult

    -- * CompleteWorkflowExecutionFailedEventAttributes
    , CompleteWorkflowExecutionFailedEventAttributes
    , completeWorkflowExecutionFailedEventAttributes
    , cwefeaCause
    , cwefeaDecisionTaskCompletedEventId

    -- * ContinueAsNewWorkflowExecutionDecisionAttributes
    , ContinueAsNewWorkflowExecutionDecisionAttributes
    , continueAsNewWorkflowExecutionDecisionAttributes
    , canwedaTagList
    , canwedaTaskStartToCloseTimeout
    , canwedaInput
    , canwedaWorkflowTypeVersion
    , canwedaExecutionStartToCloseTimeout
    , canwedaTaskList
    , canwedaTaskPriority
    , canwedaChildPolicy

    -- * ContinueAsNewWorkflowExecutionFailedEventAttributes
    , ContinueAsNewWorkflowExecutionFailedEventAttributes
    , continueAsNewWorkflowExecutionFailedEventAttributes
    , canwefeaCause
    , canwefeaDecisionTaskCompletedEventId

    -- * Decision
    , Decision
    , decision
    , decRequestCancelExternalWorkflowExecutionDecisionAttributes
    , decScheduleActivityTaskDecisionAttributes
    , decSignalExternalWorkflowExecutionDecisionAttributes
    , decStartTimerDecisionAttributes
    , decRecordMarkerDecisionAttributes
    , decFailWorkflowExecutionDecisionAttributes
    , decStartChildWorkflowExecutionDecisionAttributes
    , decCompleteWorkflowExecutionDecisionAttributes
    , decRequestCancelActivityTaskDecisionAttributes
    , decCancelWorkflowExecutionDecisionAttributes
    , decCancelTimerDecisionAttributes
    , decContinueAsNewWorkflowExecutionDecisionAttributes
    , decDecisionType

    -- * DecisionTaskCompletedEventAttributes
    , DecisionTaskCompletedEventAttributes
    , decisionTaskCompletedEventAttributes
    , dtceaExecutionContext
    , dtceaScheduledEventId
    , dtceaStartedEventId

    -- * DecisionTaskScheduledEventAttributes
    , DecisionTaskScheduledEventAttributes
    , decisionTaskScheduledEventAttributes
    , dtseaTaskPriority
    , dtseaStartToCloseTimeout
    , dtseaTaskList

    -- * DecisionTaskStartedEventAttributes
    , DecisionTaskStartedEventAttributes
    , decisionTaskStartedEventAttributes
    , dtseaIdentity
    , dtseaScheduledEventId

    -- * DecisionTaskTimedOutEventAttributes
    , DecisionTaskTimedOutEventAttributes
    , decisionTaskTimedOutEventAttributes
    , dttoeaTimeoutType
    , dttoeaScheduledEventId
    , dttoeaStartedEventId

    -- * DomainConfiguration
    , DomainConfiguration
    , domainConfiguration
    , dcWorkflowExecutionRetentionPeriodInDays

    -- * DomainInfo
    , DomainInfo
    , domainInfo
    , diDescription
    , diName
    , diStatus

    -- * ExecutionTimeFilter
    , ExecutionTimeFilter
    , executionTimeFilter
    , etfLatestDate
    , etfOldestDate

    -- * ExternalWorkflowExecutionCancelRequestedEventAttributes
    , ExternalWorkflowExecutionCancelRequestedEventAttributes
    , externalWorkflowExecutionCancelRequestedEventAttributes
    , ewecreaWorkflowExecution
    , ewecreaInitiatedEventId

    -- * ExternalWorkflowExecutionSignaledEventAttributes
    , ExternalWorkflowExecutionSignaledEventAttributes
    , externalWorkflowExecutionSignaledEventAttributes
    , eweseaWorkflowExecution
    , eweseaInitiatedEventId

    -- * FailWorkflowExecutionDecisionAttributes
    , FailWorkflowExecutionDecisionAttributes
    , failWorkflowExecutionDecisionAttributes
    , fwedaReason
    , fwedaDetails

    -- * FailWorkflowExecutionFailedEventAttributes
    , FailWorkflowExecutionFailedEventAttributes
    , failWorkflowExecutionFailedEventAttributes
    , fwefeaCause
    , fwefeaDecisionTaskCompletedEventId

    -- * HistoryEvent
    , HistoryEvent
    , historyEvent
    , heWorkflowExecutionCancelRequestedEventAttributes
    , heDecisionTaskScheduledEventAttributes
    , heStartTimerFailedEventAttributes
    , heRecordMarkerFailedEventAttributes
    , heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , heWorkflowExecutionCompletedEventAttributes
    , heActivityTaskScheduledEventAttributes
    , heChildWorkflowExecutionCompletedEventAttributes
    , heScheduleActivityTaskFailedEventAttributes
    , heMarkerRecordedEventAttributes
    , heCompleteWorkflowExecutionFailedEventAttributes
    , heRequestCancelExternalWorkflowExecutionFailedEventAttributes
    , heTimerCanceledEventAttributes
    , heWorkflowExecutionStartedEventAttributes
    , heActivityTaskCompletedEventAttributes
    , heChildWorkflowExecutionStartedEventAttributes
    , heDecisionTaskTimedOutEventAttributes
    , heCancelTimerFailedEventAttributes
    , heActivityTaskTimedOutEventAttributes
    , heActivityTaskCanceledEventAttributes
    , heChildWorkflowExecutionCanceledEventAttributes
    , heDecisionTaskStartedEventAttributes
    , heCancelWorkflowExecutionFailedEventAttributes
    , heChildWorkflowExecutionTimedOutEventAttributes
    , heRequestCancelActivityTaskFailedEventAttributes
    , heWorkflowExecutionTerminatedEventAttributes
    , heStartChildWorkflowExecutionInitiatedEventAttributes
    , heActivityTaskStartedEventAttributes
    , heSignalExternalWorkflowExecutionFailedEventAttributes
    , heTimerStartedEventAttributes
    , heWorkflowExecutionTimedOutEventAttributes
    , heActivityTaskCancelRequestedEventAttributes
    , heChildWorkflowExecutionTerminatedEventAttributes
    , heWorkflowExecutionCanceledEventAttributes
    , heWorkflowExecutionSignaledEventAttributes
    , heActivityTaskFailedEventAttributes
    , heExternalWorkflowExecutionSignaledEventAttributes
    , heTimerFiredEventAttributes
    , heFailWorkflowExecutionFailedEventAttributes
    , heChildWorkflowExecutionFailedEventAttributes
    , heDecisionTaskCompletedEventAttributes
    , heStartChildWorkflowExecutionFailedEventAttributes
    , heSignalExternalWorkflowExecutionInitiatedEventAttributes
    , heContinueAsNewWorkflowExecutionFailedEventAttributes
    , heWorkflowExecutionFailedEventAttributes
    , heWorkflowExecutionContinuedAsNewEventAttributes
    , heExternalWorkflowExecutionCancelRequestedEventAttributes
    , heEventTimestamp
    , heEventType
    , heEventId

    -- * MarkerRecordedEventAttributes
    , MarkerRecordedEventAttributes
    , markerRecordedEventAttributes
    , mreaDetails
    , mreaMarkerName
    , mreaDecisionTaskCompletedEventId

    -- * PendingTaskCount
    , PendingTaskCount
    , pendingTaskCount
    , ptcTruncated
    , ptcCount

    -- * RecordMarkerDecisionAttributes
    , RecordMarkerDecisionAttributes
    , recordMarkerDecisionAttributes
    , rmdaDetails
    , rmdaMarkerName

    -- * RecordMarkerFailedEventAttributes
    , RecordMarkerFailedEventAttributes
    , recordMarkerFailedEventAttributes
    , rmfeaMarkerName
    , rmfeaCause
    , rmfeaDecisionTaskCompletedEventId

    -- * RequestCancelActivityTaskDecisionAttributes
    , RequestCancelActivityTaskDecisionAttributes
    , requestCancelActivityTaskDecisionAttributes
    , rcatdaActivityId

    -- * RequestCancelActivityTaskFailedEventAttributes
    , RequestCancelActivityTaskFailedEventAttributes
    , requestCancelActivityTaskFailedEventAttributes
    , rcatfeaActivityId
    , rcatfeaCause
    , rcatfeaDecisionTaskCompletedEventId

    -- * RequestCancelExternalWorkflowExecutionDecisionAttributes
    , RequestCancelExternalWorkflowExecutionDecisionAttributes
    , requestCancelExternalWorkflowExecutionDecisionAttributes
    , rcewedaControl
    , rcewedaRunId
    , rcewedaWorkflowId

    -- * RequestCancelExternalWorkflowExecutionFailedEventAttributes
    , RequestCancelExternalWorkflowExecutionFailedEventAttributes
    , requestCancelExternalWorkflowExecutionFailedEventAttributes
    , rcewefeaControl
    , rcewefeaRunId
    , rcewefeaWorkflowId
    , rcewefeaCause
    , rcewefeaInitiatedEventId
    , rcewefeaDecisionTaskCompletedEventId

    -- * RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , requestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , rceweieaControl
    , rceweieaRunId
    , rceweieaWorkflowId
    , rceweieaDecisionTaskCompletedEventId

    -- * ScheduleActivityTaskDecisionAttributes
    , ScheduleActivityTaskDecisionAttributes
    , scheduleActivityTaskDecisionAttributes
    , satdaControl
    , satdaScheduleToCloseTimeout
    , satdaHeartbeatTimeout
    , satdaInput
    , satdaTaskList
    , satdaTaskPriority
    , satdaScheduleToStartTimeout
    , satdaStartToCloseTimeout
    , satdaActivityType
    , satdaActivityId

    -- * ScheduleActivityTaskFailedEventAttributes
    , ScheduleActivityTaskFailedEventAttributes
    , scheduleActivityTaskFailedEventAttributes
    , satfeaActivityType
    , satfeaActivityId
    , satfeaCause
    , satfeaDecisionTaskCompletedEventId

    -- * SignalExternalWorkflowExecutionDecisionAttributes
    , SignalExternalWorkflowExecutionDecisionAttributes
    , signalExternalWorkflowExecutionDecisionAttributes
    , sewedaControl
    , sewedaInput
    , sewedaRunId
    , sewedaWorkflowId
    , sewedaSignalName

    -- * SignalExternalWorkflowExecutionFailedEventAttributes
    , SignalExternalWorkflowExecutionFailedEventAttributes
    , signalExternalWorkflowExecutionFailedEventAttributes
    , sewefeaControl
    , sewefeaRunId
    , sewefeaWorkflowId
    , sewefeaCause
    , sewefeaInitiatedEventId
    , sewefeaDecisionTaskCompletedEventId

    -- * SignalExternalWorkflowExecutionInitiatedEventAttributes
    , SignalExternalWorkflowExecutionInitiatedEventAttributes
    , signalExternalWorkflowExecutionInitiatedEventAttributes
    , seweieaControl
    , seweieaInput
    , seweieaRunId
    , seweieaWorkflowId
    , seweieaSignalName
    , seweieaDecisionTaskCompletedEventId

    -- * StartChildWorkflowExecutionDecisionAttributes
    , StartChildWorkflowExecutionDecisionAttributes
    , startChildWorkflowExecutionDecisionAttributes
    , scwedaControl
    , scwedaTagList
    , scwedaTaskStartToCloseTimeout
    , scwedaInput
    , scwedaExecutionStartToCloseTimeout
    , scwedaTaskList
    , scwedaTaskPriority
    , scwedaChildPolicy
    , scwedaWorkflowType
    , scwedaWorkflowId

    -- * StartChildWorkflowExecutionFailedEventAttributes
    , StartChildWorkflowExecutionFailedEventAttributes
    , startChildWorkflowExecutionFailedEventAttributes
    , scwefeaControl
    , scwefeaWorkflowType
    , scwefeaCause
    , scwefeaWorkflowId
    , scwefeaInitiatedEventId
    , scwefeaDecisionTaskCompletedEventId

    -- * StartChildWorkflowExecutionInitiatedEventAttributes
    , StartChildWorkflowExecutionInitiatedEventAttributes
    , startChildWorkflowExecutionInitiatedEventAttributes
    , scweieaControl
    , scweieaTagList
    , scweieaTaskStartToCloseTimeout
    , scweieaInput
    , scweieaExecutionStartToCloseTimeout
    , scweieaTaskPriority
    , scweieaWorkflowId
    , scweieaWorkflowType
    , scweieaTaskList
    , scweieaDecisionTaskCompletedEventId
    , scweieaChildPolicy

    -- * StartTimerDecisionAttributes
    , StartTimerDecisionAttributes
    , startTimerDecisionAttributes
    , stdaControl
    , stdaTimerId
    , stdaStartToFireTimeout

    -- * StartTimerFailedEventAttributes
    , StartTimerFailedEventAttributes
    , startTimerFailedEventAttributes
    , stfeaTimerId
    , stfeaCause
    , stfeaDecisionTaskCompletedEventId

    -- * TagFilter
    , TagFilter
    , tagFilter
    , tfTag

    -- * TaskList
    , TaskList
    , taskList
    , tlName

    -- * TimerCanceledEventAttributes
    , TimerCanceledEventAttributes
    , timerCanceledEventAttributes
    , tceaTimerId
    , tceaStartedEventId
    , tceaDecisionTaskCompletedEventId

    -- * TimerFiredEventAttributes
    , TimerFiredEventAttributes
    , timerFiredEventAttributes
    , tfeaTimerId
    , tfeaStartedEventId

    -- * TimerStartedEventAttributes
    , TimerStartedEventAttributes
    , timerStartedEventAttributes
    , tseaControl
    , tseaTimerId
    , tseaStartToFireTimeout
    , tseaDecisionTaskCompletedEventId

    -- * WorkflowExecution
    , WorkflowExecution
    , workflowExecution
    , weWorkflowId
    , weRunId

    -- * WorkflowExecutionCancelRequestedEventAttributes
    , WorkflowExecutionCancelRequestedEventAttributes
    , workflowExecutionCancelRequestedEventAttributes
    , wecreaExternalWorkflowExecution
    , wecreaExternalInitiatedEventId
    , wecreaCause

    -- * WorkflowExecutionCanceledEventAttributes
    , WorkflowExecutionCanceledEventAttributes
    , workflowExecutionCanceledEventAttributes
    , worDetails
    , worDecisionTaskCompletedEventId

    -- * WorkflowExecutionCompletedEventAttributes
    , WorkflowExecutionCompletedEventAttributes
    , workflowExecutionCompletedEventAttributes
    , weceaResult
    , weceaDecisionTaskCompletedEventId

    -- * WorkflowExecutionConfiguration
    , WorkflowExecutionConfiguration
    , workflowExecutionConfiguration
    , wecTaskPriority
    , wecTaskStartToCloseTimeout
    , wecExecutionStartToCloseTimeout
    , wecTaskList
    , wecChildPolicy

    -- * WorkflowExecutionContinuedAsNewEventAttributes
    , WorkflowExecutionContinuedAsNewEventAttributes
    , workflowExecutionContinuedAsNewEventAttributes
    , wecaneaTagList
    , wecaneaTaskStartToCloseTimeout
    , wecaneaInput
    , wecaneaExecutionStartToCloseTimeout
    , wecaneaTaskPriority
    , wecaneaDecisionTaskCompletedEventId
    , wecaneaNewExecutionRunId
    , wecaneaTaskList
    , wecaneaChildPolicy
    , wecaneaWorkflowType

    -- * WorkflowExecutionCount
    , WorkflowExecutionCount
    , workflowExecutionCount
    , wecTruncated
    , wecCount

    -- * WorkflowExecutionFailedEventAttributes
    , WorkflowExecutionFailedEventAttributes
    , workflowExecutionFailedEventAttributes
    , wefeaReason
    , wefeaDetails
    , wefeaDecisionTaskCompletedEventId

    -- * WorkflowExecutionFilter
    , WorkflowExecutionFilter
    , workflowExecutionFilter
    , wefWorkflowId

    -- * WorkflowExecutionInfo
    , WorkflowExecutionInfo
    , workflowExecutionInfo
    , weiParent
    , weiTagList
    , weiCloseStatus
    , weiCloseTimestamp
    , weiCancelRequested
    , weiExecution
    , weiWorkflowType
    , weiStartTimestamp
    , weiExecutionStatus

    -- * WorkflowExecutionInfos
    , WorkflowExecutionInfos
    , workflowExecutionInfos
    , weiNextPageToken
    , weiExecutionInfos

    -- * WorkflowExecutionOpenCounts
    , WorkflowExecutionOpenCounts
    , workflowExecutionOpenCounts
    , weocOpenActivityTasks
    , weocOpenDecisionTasks
    , weocOpenTimers
    , weocOpenChildWorkflowExecutions

    -- * WorkflowExecutionSignaledEventAttributes
    , WorkflowExecutionSignaledEventAttributes
    , workflowExecutionSignaledEventAttributes
    , worExternalWorkflowExecution
    , worExternalInitiatedEventId
    , worInput
    , worSignalName

    -- * WorkflowExecutionStartedEventAttributes
    , WorkflowExecutionStartedEventAttributes
    , workflowExecutionStartedEventAttributes
    , weseaParentInitiatedEventId
    , weseaTagList
    , weseaTaskStartToCloseTimeout
    , weseaInput
    , weseaExecutionStartToCloseTimeout
    , weseaTaskPriority
    , weseaParentWorkflowExecution
    , weseaContinuedExecutionRunId
    , weseaChildPolicy
    , weseaTaskList
    , weseaWorkflowType

    -- * WorkflowExecutionTerminatedEventAttributes
    , WorkflowExecutionTerminatedEventAttributes
    , workflowExecutionTerminatedEventAttributes
    , weteaCause
    , weteaReason
    , weteaDetails
    , weteaChildPolicy

    -- * WorkflowExecutionTimedOutEventAttributes
    , WorkflowExecutionTimedOutEventAttributes
    , workflowExecutionTimedOutEventAttributes
    , wetoeaTimeoutType
    , wetoeaChildPolicy

    -- * WorkflowType
    , WorkflowType
    , workflowType
    , wtName
    , wtVersion

    -- * WorkflowTypeConfiguration
    , WorkflowTypeConfiguration
    , workflowTypeConfiguration
    , wtcDefaultChildPolicy
    , wtcDefaultTaskList
    , wtcDefaultTaskPriority
    , wtcDefaultExecutionStartToCloseTimeout
    , wtcDefaultTaskStartToCloseTimeout

    -- * WorkflowTypeFilter
    , WorkflowTypeFilter
    , workflowTypeFilter
    , wtfVersion
    , wtfName

    -- * WorkflowTypeInfo
    , WorkflowTypeInfo
    , workflowTypeInfo
    , wtiDeprecationDate
    , wtiDescription
    , wtiWorkflowType
    , wtiStatus
    , wtiCreationDate
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Network.AWS.SWF.Types.Product
import           Network.AWS.SWF.Types.Sum

-- | Version @2012-01-25@ of the Amazon Simple Workflow Service SDK.
data SWF

instance AWSService SWF where
    type Sg SWF = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "SWF"
            , _svcPrefix = "swf"
            , _svcVersion = "2012-01-25"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Returned by any operation if a system imposed limitation has been
-- reached. To address this fault you should either clean up unused
-- resources or increase the limit by contacting AWS.
_LimitExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededFault = _ServiceError . hasCode "LimitExceededFault"

-- | Returned by StartWorkflowExecution when an open execution with the same
-- workflowId is already running in the specified domain.
_WorkflowExecutionAlreadyStartedFault :: AWSError a => Getting (First ServiceError) a ServiceError
_WorkflowExecutionAlreadyStartedFault =
    _ServiceError . hasCode "WorkflowExecutionAlreadyStartedFault"

-- | Returned if the specified domain already exists. You will get this fault
-- even if the existing domain is in deprecated status.
_DomainAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DomainAlreadyExistsFault = _ServiceError . hasCode "DomainAlreadyExistsFault"

-- | Returned when the named resource cannot be found with in the scope of
-- this operation (region or domain). This could happen if the named
-- resource was never created or is no longer available for this operation.
_UnknownResourceFault :: AWSError a => Getting (First ServiceError) a ServiceError
_UnknownResourceFault = _ServiceError . hasCode "UnknownResourceFault"

-- | Returned when the caller does not have sufficient permissions to invoke
-- the action.
_OperationNotPermittedFault :: AWSError a => Getting (First ServiceError) a ServiceError
_OperationNotPermittedFault =
    _ServiceError . hasCode "OperationNotPermittedFault"

-- | Prism for DefaultUndefinedFault' errors.
_DefaultUndefinedFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DefaultUndefinedFault = _ServiceError . hasCode "DefaultUndefinedFault"

-- | Returned when the specified activity or workflow type was already
-- deprecated.
_TypeDeprecatedFault :: AWSError a => Getting (First ServiceError) a ServiceError
_TypeDeprecatedFault = _ServiceError . hasCode "TypeDeprecatedFault"

-- | Returned if the type already exists in the specified domain. You will
-- get this fault even if the existing type is in deprecated status. You
-- can specify another version if the intent is to create a new distinct
-- version of the type.
_TypeAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_TypeAlreadyExistsFault = _ServiceError . hasCode "TypeAlreadyExistsFault"

-- | Returned when the specified domain has been deprecated.
_DomainDeprecatedFault :: AWSError a => Getting (First ServiceError) a ServiceError
_DomainDeprecatedFault = _ServiceError . hasCode "DomainDeprecatedFault"
