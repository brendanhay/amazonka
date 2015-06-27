{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SWF.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

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
            , _svcTimeout = 80000000
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

data ActivityTaskTimeoutType
    = ATTTScheduleTOClose
    | ATTTHeartbeat
    | ATTTStartTOClose
    | ATTTScheduleTOStart
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ActivityTaskTimeoutType where
    parser = takeLowerText >>= \case
        "HEARTBEAT" -> pure ATTTHeartbeat
        "SCHEDULE_TO_CLOSE" -> pure ATTTScheduleTOClose
        "SCHEDULE_TO_START" -> pure ATTTScheduleTOStart
        "START_TO_CLOSE" -> pure ATTTStartTOClose
        e -> fail ("Failure parsing ActivityTaskTimeoutType from " ++ show e)

instance ToText ActivityTaskTimeoutType where
    toText = \case
        ATTTHeartbeat -> "HEARTBEAT"
        ATTTScheduleTOClose -> "SCHEDULE_TO_CLOSE"
        ATTTScheduleTOStart -> "SCHEDULE_TO_START"
        ATTTStartTOClose -> "START_TO_CLOSE"

instance Hashable ActivityTaskTimeoutType
instance ToQuery ActivityTaskTimeoutType
instance ToHeader ActivityTaskTimeoutType

instance FromJSON ActivityTaskTimeoutType where
    parseJSON = parseJSONText "ActivityTaskTimeoutType"

data CancelTimerFailedCause
    = CTFCTimerIDUnknown
    | CTFCOperationNotPermitted
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText CancelTimerFailedCause where
    parser = takeLowerText >>= \case
        "OPERATION_NOT_PERMITTED" -> pure CTFCOperationNotPermitted
        "TIMER_ID_UNKNOWN" -> pure CTFCTimerIDUnknown
        e -> fail ("Failure parsing CancelTimerFailedCause from " ++ show e)

instance ToText CancelTimerFailedCause where
    toText = \case
        CTFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        CTFCTimerIDUnknown -> "TIMER_ID_UNKNOWN"

instance Hashable CancelTimerFailedCause
instance ToQuery CancelTimerFailedCause
instance ToHeader CancelTimerFailedCause

instance FromJSON CancelTimerFailedCause where
    parseJSON = parseJSONText "CancelTimerFailedCause"

data CancelWorkflowExecutionFailedCause
    = CanOperationNotPermitted
    | CanUnhandledDecision
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText CancelWorkflowExecutionFailedCause where
    parser = takeLowerText >>= \case
        "OPERATION_NOT_PERMITTED" -> pure CanOperationNotPermitted
        "UNHANDLED_DECISION" -> pure CanUnhandledDecision
        e -> fail ("Failure parsing CancelWorkflowExecutionFailedCause from " ++ show e)

instance ToText CancelWorkflowExecutionFailedCause where
    toText = \case
        CanOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        CanUnhandledDecision -> "UNHANDLED_DECISION"

instance Hashable CancelWorkflowExecutionFailedCause
instance ToQuery CancelWorkflowExecutionFailedCause
instance ToHeader CancelWorkflowExecutionFailedCause

instance FromJSON CancelWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "CancelWorkflowExecutionFailedCause"

data ChildPolicy
    = Abandon
    | RequestCancel
    | Terminate
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ChildPolicy where
    parser = takeLowerText >>= \case
        "ABANDON" -> pure Abandon
        "REQUEST_CANCEL" -> pure RequestCancel
        "TERMINATE" -> pure Terminate
        e -> fail ("Failure parsing ChildPolicy from " ++ show e)

instance ToText ChildPolicy where
    toText = \case
        Abandon -> "ABANDON"
        RequestCancel -> "REQUEST_CANCEL"
        Terminate -> "TERMINATE"

instance Hashable ChildPolicy
instance ToQuery ChildPolicy
instance ToHeader ChildPolicy

instance ToJSON ChildPolicy where
    toJSON = toJSONText

instance FromJSON ChildPolicy where
    parseJSON = parseJSONText "ChildPolicy"

data CloseStatus
    = Canceled
    | TimedOut
    | Terminated
    | Completed
    | Failed
    | ContinuedASNew
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText CloseStatus where
    parser = takeLowerText >>= \case
        "CANCELED" -> pure Canceled
        "COMPLETED" -> pure Completed
        "CONTINUED_AS_NEW" -> pure ContinuedASNew
        "FAILED" -> pure Failed
        "TERMINATED" -> pure Terminated
        "TIMED_OUT" -> pure TimedOut
        e -> fail ("Failure parsing CloseStatus from " ++ show e)

instance ToText CloseStatus where
    toText = \case
        Canceled -> "CANCELED"
        Completed -> "COMPLETED"
        ContinuedASNew -> "CONTINUED_AS_NEW"
        Failed -> "FAILED"
        Terminated -> "TERMINATED"
        TimedOut -> "TIMED_OUT"

instance Hashable CloseStatus
instance ToQuery CloseStatus
instance ToHeader CloseStatus

instance ToJSON CloseStatus where
    toJSON = toJSONText

instance FromJSON CloseStatus where
    parseJSON = parseJSONText "CloseStatus"

data CompleteWorkflowExecutionFailedCause
    = CWEFCOperationNotPermitted
    | CWEFCUnhandledDecision
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText CompleteWorkflowExecutionFailedCause where
    parser = takeLowerText >>= \case
        "OPERATION_NOT_PERMITTED" -> pure CWEFCOperationNotPermitted
        "UNHANDLED_DECISION" -> pure CWEFCUnhandledDecision
        e -> fail ("Failure parsing CompleteWorkflowExecutionFailedCause from " ++ show e)

instance ToText CompleteWorkflowExecutionFailedCause where
    toText = \case
        CWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        CWEFCUnhandledDecision -> "UNHANDLED_DECISION"

instance Hashable CompleteWorkflowExecutionFailedCause
instance ToQuery CompleteWorkflowExecutionFailedCause
instance ToHeader CompleteWorkflowExecutionFailedCause

instance FromJSON CompleteWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "CompleteWorkflowExecutionFailedCause"

data ContinueAsNewWorkflowExecutionFailedCause
    = CANWEFCContinueASNewWorkflowExecutionRateExceeded
    | CANWEFCDefaultTaskListUndefined
    | CANWEFCWorkflowTypeDoesNotExist
    | CANWEFCDefaultExecutionStartTOCloseTimeoutUndefined
    | CANWEFCUnhandledDecision
    | CANWEFCOperationNotPermitted
    | CANWEFCDefaultChildPolicyUndefined
    | CANWEFCDefaultTaskStartTOCloseTimeoutUndefined
    | CANWEFCWorkflowTypeDeprecated
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ContinueAsNewWorkflowExecutionFailedCause where
    parser = takeLowerText >>= \case
        "CONTINUE_AS_NEW_WORKFLOW_EXECUTION_RATE_EXCEEDED" -> pure CANWEFCContinueASNewWorkflowExecutionRateExceeded
        "DEFAULT_CHILD_POLICY_UNDEFINED" -> pure CANWEFCDefaultChildPolicyUndefined
        "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED" -> pure CANWEFCDefaultExecutionStartTOCloseTimeoutUndefined
        "DEFAULT_TASK_LIST_UNDEFINED" -> pure CANWEFCDefaultTaskListUndefined
        "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED" -> pure CANWEFCDefaultTaskStartTOCloseTimeoutUndefined
        "OPERATION_NOT_PERMITTED" -> pure CANWEFCOperationNotPermitted
        "UNHANDLED_DECISION" -> pure CANWEFCUnhandledDecision
        "WORKFLOW_TYPE_DEPRECATED" -> pure CANWEFCWorkflowTypeDeprecated
        "WORKFLOW_TYPE_DOES_NOT_EXIST" -> pure CANWEFCWorkflowTypeDoesNotExist
        e -> fail ("Failure parsing ContinueAsNewWorkflowExecutionFailedCause from " ++ show e)

instance ToText ContinueAsNewWorkflowExecutionFailedCause where
    toText = \case
        CANWEFCContinueASNewWorkflowExecutionRateExceeded -> "CONTINUE_AS_NEW_WORKFLOW_EXECUTION_RATE_EXCEEDED"
        CANWEFCDefaultChildPolicyUndefined -> "DEFAULT_CHILD_POLICY_UNDEFINED"
        CANWEFCDefaultExecutionStartTOCloseTimeoutUndefined -> "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
        CANWEFCDefaultTaskListUndefined -> "DEFAULT_TASK_LIST_UNDEFINED"
        CANWEFCDefaultTaskStartTOCloseTimeoutUndefined -> "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
        CANWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        CANWEFCUnhandledDecision -> "UNHANDLED_DECISION"
        CANWEFCWorkflowTypeDeprecated -> "WORKFLOW_TYPE_DEPRECATED"
        CANWEFCWorkflowTypeDoesNotExist -> "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance Hashable ContinueAsNewWorkflowExecutionFailedCause
instance ToQuery ContinueAsNewWorkflowExecutionFailedCause
instance ToHeader ContinueAsNewWorkflowExecutionFailedCause

instance FromJSON ContinueAsNewWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "ContinueAsNewWorkflowExecutionFailedCause"

data DecisionTaskTimeoutType =
    StartTOClose
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText DecisionTaskTimeoutType where
    parser = takeLowerText >>= \case
        "START_TO_CLOSE" -> pure StartTOClose
        e -> fail ("Failure parsing DecisionTaskTimeoutType from " ++ show e)

instance ToText DecisionTaskTimeoutType where
    toText = \case
        StartTOClose -> "START_TO_CLOSE"

instance Hashable DecisionTaskTimeoutType
instance ToQuery DecisionTaskTimeoutType
instance ToHeader DecisionTaskTimeoutType

instance FromJSON DecisionTaskTimeoutType where
    parseJSON = parseJSONText "DecisionTaskTimeoutType"

data DecisionType
    = StartTimer
    | RecordMarker
    | SignalExternalWorkflowExecution
    | ScheduleActivityTask
    | RequestCancelExternalWorkflowExecution
    | ContinueAsNewWorkflowExecution
    | CancelTimer
    | RequestCancelActivityTask
    | CancelWorkflowExecution
    | CompleteWorkflowExecution
    | StartChildWorkflowExecution
    | FailWorkflowExecution
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText DecisionType where
    parser = takeLowerText >>= \case
        "CancelTimer" -> pure CancelTimer
        "CancelWorkflowExecution" -> pure CancelWorkflowExecution
        "CompleteWorkflowExecution" -> pure CompleteWorkflowExecution
        "ContinueAsNewWorkflowExecution" -> pure ContinueAsNewWorkflowExecution
        "FailWorkflowExecution" -> pure FailWorkflowExecution
        "RecordMarker" -> pure RecordMarker
        "RequestCancelActivityTask" -> pure RequestCancelActivityTask
        "RequestCancelExternalWorkflowExecution" -> pure RequestCancelExternalWorkflowExecution
        "ScheduleActivityTask" -> pure ScheduleActivityTask
        "SignalExternalWorkflowExecution" -> pure SignalExternalWorkflowExecution
        "StartChildWorkflowExecution" -> pure StartChildWorkflowExecution
        "StartTimer" -> pure StartTimer
        e -> fail ("Failure parsing DecisionType from " ++ show e)

instance ToText DecisionType where
    toText = \case
        CancelTimer -> "CancelTimer"
        CancelWorkflowExecution -> "CancelWorkflowExecution"
        CompleteWorkflowExecution -> "CompleteWorkflowExecution"
        ContinueAsNewWorkflowExecution -> "ContinueAsNewWorkflowExecution"
        FailWorkflowExecution -> "FailWorkflowExecution"
        RecordMarker -> "RecordMarker"
        RequestCancelActivityTask -> "RequestCancelActivityTask"
        RequestCancelExternalWorkflowExecution -> "RequestCancelExternalWorkflowExecution"
        ScheduleActivityTask -> "ScheduleActivityTask"
        SignalExternalWorkflowExecution -> "SignalExternalWorkflowExecution"
        StartChildWorkflowExecution -> "StartChildWorkflowExecution"
        StartTimer -> "StartTimer"

instance Hashable DecisionType
instance ToQuery DecisionType
instance ToHeader DecisionType

instance ToJSON DecisionType where
    toJSON = toJSONText

data EventType
    = ChildWorkflowExecutionCanceled
    | ChildWorkflowExecutionTimedOut
    | StartChildWorkflowExecutionInitiated
    | WorkflowExecutionTerminated
    | CancelWorkflowExecutionFailed
    | DecisionTaskStarted
    | RequestCancelActivityTaskFailed
    | WorkflowExecutionCancelRequested
    | WorkflowExecutionContinuedAsNew
    | ExternalWorkflowExecutionCancelRequested
    | WorkflowExecutionFailed
    | ContinueAsNewWorkflowExecutionFailed
    | SignalExternalWorkflowExecutionInitiated
    | StartChildWorkflowExecutionFailed
    | FailWorkflowExecutionFailed
    | ChildWorkflowExecutionFailed
    | DecisionTaskCompleted
    | CompleteWorkflowExecutionFailed
    | ChildWorkflowExecutionCompleted
    | ScheduleActivityTaskFailed
    | MarkerRecorded
    | ActivityTaskScheduled
    | RecordMarkerFailed
    | StartTimerFailed
    | RequestCancelExternalWorkflowExecutionInitiated
    | DecisionTaskScheduled
    | WorkflowExecutionCompleted
    | ActivityTaskTimedOut
    | ActivityTaskCanceled
    | ChildWorkflowExecutionStarted
    | CancelTimerFailed
    | DecisionTaskTimedOut
    | ActivityTaskCompleted
    | TimerCanceled
    | WorkflowExecutionStarted
    | RequestCancelExternalWorkflowExecutionFailed
    | TimerFired
    | ExternalWorkflowExecutionSignaled
    | ActivityTaskFailed
    | WorkflowExecutionSignaled
    | WorkflowExecutionCanceled
    | WorkflowExecutionTimedOut
    | ChildWorkflowExecutionTerminated
    | ActivityTaskCancelRequested
    | TimerStarted
    | ActivityTaskStarted
    | SignalExternalWorkflowExecutionFailed
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText EventType where
    parser = takeLowerText >>= \case
        "ActivityTaskCancelRequested" -> pure ActivityTaskCancelRequested
        "ActivityTaskCanceled" -> pure ActivityTaskCanceled
        "ActivityTaskCompleted" -> pure ActivityTaskCompleted
        "ActivityTaskFailed" -> pure ActivityTaskFailed
        "ActivityTaskScheduled" -> pure ActivityTaskScheduled
        "ActivityTaskStarted" -> pure ActivityTaskStarted
        "ActivityTaskTimedOut" -> pure ActivityTaskTimedOut
        "CancelTimerFailed" -> pure CancelTimerFailed
        "CancelWorkflowExecutionFailed" -> pure CancelWorkflowExecutionFailed
        "ChildWorkflowExecutionCanceled" -> pure ChildWorkflowExecutionCanceled
        "ChildWorkflowExecutionCompleted" -> pure ChildWorkflowExecutionCompleted
        "ChildWorkflowExecutionFailed" -> pure ChildWorkflowExecutionFailed
        "ChildWorkflowExecutionStarted" -> pure ChildWorkflowExecutionStarted
        "ChildWorkflowExecutionTerminated" -> pure ChildWorkflowExecutionTerminated
        "ChildWorkflowExecutionTimedOut" -> pure ChildWorkflowExecutionTimedOut
        "CompleteWorkflowExecutionFailed" -> pure CompleteWorkflowExecutionFailed
        "ContinueAsNewWorkflowExecutionFailed" -> pure ContinueAsNewWorkflowExecutionFailed
        "DecisionTaskCompleted" -> pure DecisionTaskCompleted
        "DecisionTaskScheduled" -> pure DecisionTaskScheduled
        "DecisionTaskStarted" -> pure DecisionTaskStarted
        "DecisionTaskTimedOut" -> pure DecisionTaskTimedOut
        "ExternalWorkflowExecutionCancelRequested" -> pure ExternalWorkflowExecutionCancelRequested
        "ExternalWorkflowExecutionSignaled" -> pure ExternalWorkflowExecutionSignaled
        "FailWorkflowExecutionFailed" -> pure FailWorkflowExecutionFailed
        "MarkerRecorded" -> pure MarkerRecorded
        "RecordMarkerFailed" -> pure RecordMarkerFailed
        "RequestCancelActivityTaskFailed" -> pure RequestCancelActivityTaskFailed
        "RequestCancelExternalWorkflowExecutionFailed" -> pure RequestCancelExternalWorkflowExecutionFailed
        "RequestCancelExternalWorkflowExecutionInitiated" -> pure RequestCancelExternalWorkflowExecutionInitiated
        "ScheduleActivityTaskFailed" -> pure ScheduleActivityTaskFailed
        "SignalExternalWorkflowExecutionFailed" -> pure SignalExternalWorkflowExecutionFailed
        "SignalExternalWorkflowExecutionInitiated" -> pure SignalExternalWorkflowExecutionInitiated
        "StartChildWorkflowExecutionFailed" -> pure StartChildWorkflowExecutionFailed
        "StartChildWorkflowExecutionInitiated" -> pure StartChildWorkflowExecutionInitiated
        "StartTimerFailed" -> pure StartTimerFailed
        "TimerCanceled" -> pure TimerCanceled
        "TimerFired" -> pure TimerFired
        "TimerStarted" -> pure TimerStarted
        "WorkflowExecutionCancelRequested" -> pure WorkflowExecutionCancelRequested
        "WorkflowExecutionCanceled" -> pure WorkflowExecutionCanceled
        "WorkflowExecutionCompleted" -> pure WorkflowExecutionCompleted
        "WorkflowExecutionContinuedAsNew" -> pure WorkflowExecutionContinuedAsNew
        "WorkflowExecutionFailed" -> pure WorkflowExecutionFailed
        "WorkflowExecutionSignaled" -> pure WorkflowExecutionSignaled
        "WorkflowExecutionStarted" -> pure WorkflowExecutionStarted
        "WorkflowExecutionTerminated" -> pure WorkflowExecutionTerminated
        "WorkflowExecutionTimedOut" -> pure WorkflowExecutionTimedOut
        e -> fail ("Failure parsing EventType from " ++ show e)

instance ToText EventType where
    toText = \case
        ActivityTaskCancelRequested -> "ActivityTaskCancelRequested"
        ActivityTaskCanceled -> "ActivityTaskCanceled"
        ActivityTaskCompleted -> "ActivityTaskCompleted"
        ActivityTaskFailed -> "ActivityTaskFailed"
        ActivityTaskScheduled -> "ActivityTaskScheduled"
        ActivityTaskStarted -> "ActivityTaskStarted"
        ActivityTaskTimedOut -> "ActivityTaskTimedOut"
        CancelTimerFailed -> "CancelTimerFailed"
        CancelWorkflowExecutionFailed -> "CancelWorkflowExecutionFailed"
        ChildWorkflowExecutionCanceled -> "ChildWorkflowExecutionCanceled"
        ChildWorkflowExecutionCompleted -> "ChildWorkflowExecutionCompleted"
        ChildWorkflowExecutionFailed -> "ChildWorkflowExecutionFailed"
        ChildWorkflowExecutionStarted -> "ChildWorkflowExecutionStarted"
        ChildWorkflowExecutionTerminated -> "ChildWorkflowExecutionTerminated"
        ChildWorkflowExecutionTimedOut -> "ChildWorkflowExecutionTimedOut"
        CompleteWorkflowExecutionFailed -> "CompleteWorkflowExecutionFailed"
        ContinueAsNewWorkflowExecutionFailed -> "ContinueAsNewWorkflowExecutionFailed"
        DecisionTaskCompleted -> "DecisionTaskCompleted"
        DecisionTaskScheduled -> "DecisionTaskScheduled"
        DecisionTaskStarted -> "DecisionTaskStarted"
        DecisionTaskTimedOut -> "DecisionTaskTimedOut"
        ExternalWorkflowExecutionCancelRequested -> "ExternalWorkflowExecutionCancelRequested"
        ExternalWorkflowExecutionSignaled -> "ExternalWorkflowExecutionSignaled"
        FailWorkflowExecutionFailed -> "FailWorkflowExecutionFailed"
        MarkerRecorded -> "MarkerRecorded"
        RecordMarkerFailed -> "RecordMarkerFailed"
        RequestCancelActivityTaskFailed -> "RequestCancelActivityTaskFailed"
        RequestCancelExternalWorkflowExecutionFailed -> "RequestCancelExternalWorkflowExecutionFailed"
        RequestCancelExternalWorkflowExecutionInitiated -> "RequestCancelExternalWorkflowExecutionInitiated"
        ScheduleActivityTaskFailed -> "ScheduleActivityTaskFailed"
        SignalExternalWorkflowExecutionFailed -> "SignalExternalWorkflowExecutionFailed"
        SignalExternalWorkflowExecutionInitiated -> "SignalExternalWorkflowExecutionInitiated"
        StartChildWorkflowExecutionFailed -> "StartChildWorkflowExecutionFailed"
        StartChildWorkflowExecutionInitiated -> "StartChildWorkflowExecutionInitiated"
        StartTimerFailed -> "StartTimerFailed"
        TimerCanceled -> "TimerCanceled"
        TimerFired -> "TimerFired"
        TimerStarted -> "TimerStarted"
        WorkflowExecutionCancelRequested -> "WorkflowExecutionCancelRequested"
        WorkflowExecutionCanceled -> "WorkflowExecutionCanceled"
        WorkflowExecutionCompleted -> "WorkflowExecutionCompleted"
        WorkflowExecutionContinuedAsNew -> "WorkflowExecutionContinuedAsNew"
        WorkflowExecutionFailed -> "WorkflowExecutionFailed"
        WorkflowExecutionSignaled -> "WorkflowExecutionSignaled"
        WorkflowExecutionStarted -> "WorkflowExecutionStarted"
        WorkflowExecutionTerminated -> "WorkflowExecutionTerminated"
        WorkflowExecutionTimedOut -> "WorkflowExecutionTimedOut"

instance Hashable EventType
instance ToQuery EventType
instance ToHeader EventType

instance FromJSON EventType where
    parseJSON = parseJSONText "EventType"

data ExecutionStatus
    = Closed
    | Open
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ExecutionStatus where
    parser = takeLowerText >>= \case
        "CLOSED" -> pure Closed
        "OPEN" -> pure Open
        e -> fail ("Failure parsing ExecutionStatus from " ++ show e)

instance ToText ExecutionStatus where
    toText = \case
        Closed -> "CLOSED"
        Open -> "OPEN"

instance Hashable ExecutionStatus
instance ToQuery ExecutionStatus
instance ToHeader ExecutionStatus

instance FromJSON ExecutionStatus where
    parseJSON = parseJSONText "ExecutionStatus"

data FailWorkflowExecutionFailedCause
    = FWEFCUnhandledDecision
    | FWEFCOperationNotPermitted
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText FailWorkflowExecutionFailedCause where
    parser = takeLowerText >>= \case
        "OPERATION_NOT_PERMITTED" -> pure FWEFCOperationNotPermitted
        "UNHANDLED_DECISION" -> pure FWEFCUnhandledDecision
        e -> fail ("Failure parsing FailWorkflowExecutionFailedCause from " ++ show e)

instance ToText FailWorkflowExecutionFailedCause where
    toText = \case
        FWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        FWEFCUnhandledDecision -> "UNHANDLED_DECISION"

instance Hashable FailWorkflowExecutionFailedCause
instance ToQuery FailWorkflowExecutionFailedCause
instance ToHeader FailWorkflowExecutionFailedCause

instance FromJSON FailWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "FailWorkflowExecutionFailedCause"

data RecordMarkerFailedCause =
    RMFCOperationNotPermitted
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText RecordMarkerFailedCause where
    parser = takeLowerText >>= \case
        "OPERATION_NOT_PERMITTED" -> pure RMFCOperationNotPermitted
        e -> fail ("Failure parsing RecordMarkerFailedCause from " ++ show e)

instance ToText RecordMarkerFailedCause where
    toText = \case
        RMFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"

instance Hashable RecordMarkerFailedCause
instance ToQuery RecordMarkerFailedCause
instance ToHeader RecordMarkerFailedCause

instance FromJSON RecordMarkerFailedCause where
    parseJSON = parseJSONText "RecordMarkerFailedCause"

data RegistrationStatus
    = Registered
    | Deprecated
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText RegistrationStatus where
    parser = takeLowerText >>= \case
        "DEPRECATED" -> pure Deprecated
        "REGISTERED" -> pure Registered
        e -> fail ("Failure parsing RegistrationStatus from " ++ show e)

instance ToText RegistrationStatus where
    toText = \case
        Deprecated -> "DEPRECATED"
        Registered -> "REGISTERED"

instance Hashable RegistrationStatus
instance ToQuery RegistrationStatus
instance ToHeader RegistrationStatus

instance ToJSON RegistrationStatus where
    toJSON = toJSONText

instance FromJSON RegistrationStatus where
    parseJSON = parseJSONText "RegistrationStatus"

data RequestCancelActivityTaskFailedCause
    = RCATFCActivityIDUnknown
    | RCATFCOperationNotPermitted
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText RequestCancelActivityTaskFailedCause where
    parser = takeLowerText >>= \case
        "ACTIVITY_ID_UNKNOWN" -> pure RCATFCActivityIDUnknown
        "OPERATION_NOT_PERMITTED" -> pure RCATFCOperationNotPermitted
        e -> fail ("Failure parsing RequestCancelActivityTaskFailedCause from " ++ show e)

instance ToText RequestCancelActivityTaskFailedCause where
    toText = \case
        RCATFCActivityIDUnknown -> "ACTIVITY_ID_UNKNOWN"
        RCATFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"

instance Hashable RequestCancelActivityTaskFailedCause
instance ToQuery RequestCancelActivityTaskFailedCause
instance ToHeader RequestCancelActivityTaskFailedCause

instance FromJSON RequestCancelActivityTaskFailedCause where
    parseJSON = parseJSONText "RequestCancelActivityTaskFailedCause"

data RequestCancelExternalWorkflowExecutionFailedCause
    = RCEWEFCRequestCancelExternalWorkflowExecutionRateExceeded
    | RCEWEFCUnknownExternalWorkflowExecution
    | RCEWEFCOperationNotPermitted
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText RequestCancelExternalWorkflowExecutionFailedCause where
    parser = takeLowerText >>= \case
        "OPERATION_NOT_PERMITTED" -> pure RCEWEFCOperationNotPermitted
        "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED" -> pure RCEWEFCRequestCancelExternalWorkflowExecutionRateExceeded
        "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION" -> pure RCEWEFCUnknownExternalWorkflowExecution
        e -> fail ("Failure parsing RequestCancelExternalWorkflowExecutionFailedCause from " ++ show e)

instance ToText RequestCancelExternalWorkflowExecutionFailedCause where
    toText = \case
        RCEWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        RCEWEFCRequestCancelExternalWorkflowExecutionRateExceeded -> "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
        RCEWEFCUnknownExternalWorkflowExecution -> "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance Hashable RequestCancelExternalWorkflowExecutionFailedCause
instance ToQuery RequestCancelExternalWorkflowExecutionFailedCause
instance ToHeader RequestCancelExternalWorkflowExecutionFailedCause

instance FromJSON RequestCancelExternalWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "RequestCancelExternalWorkflowExecutionFailedCause"

data ScheduleActivityTaskFailedCause
    = SATFCDefaultStartTOCloseTimeoutUndefined
    | SATFCDefaultScheduleTOStartTimeoutUndefined
    | SATFCOpenActivitiesLimitExceeded
    | SATFCActivityTypeDoesNotExist
    | SATFCDefaultTaskListUndefined
    | SATFCOperationNotPermitted
    | SATFCActivityTypeDeprecated
    | SATFCActivityCreationRateExceeded
    | SATFCDefaultScheduleTOCloseTimeoutUndefined
    | SATFCActivityIDAlreadyINUse
    | SATFCDefaultHeartbeatTimeoutUndefined
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ScheduleActivityTaskFailedCause where
    parser = takeLowerText >>= \case
        "ACTIVITY_CREATION_RATE_EXCEEDED" -> pure SATFCActivityCreationRateExceeded
        "ACTIVITY_ID_ALREADY_IN_USE" -> pure SATFCActivityIDAlreadyINUse
        "ACTIVITY_TYPE_DEPRECATED" -> pure SATFCActivityTypeDeprecated
        "ACTIVITY_TYPE_DOES_NOT_EXIST" -> pure SATFCActivityTypeDoesNotExist
        "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED" -> pure SATFCDefaultHeartbeatTimeoutUndefined
        "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED" -> pure SATFCDefaultScheduleTOCloseTimeoutUndefined
        "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED" -> pure SATFCDefaultScheduleTOStartTimeoutUndefined
        "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED" -> pure SATFCDefaultStartTOCloseTimeoutUndefined
        "DEFAULT_TASK_LIST_UNDEFINED" -> pure SATFCDefaultTaskListUndefined
        "OPEN_ACTIVITIES_LIMIT_EXCEEDED" -> pure SATFCOpenActivitiesLimitExceeded
        "OPERATION_NOT_PERMITTED" -> pure SATFCOperationNotPermitted
        e -> fail ("Failure parsing ScheduleActivityTaskFailedCause from " ++ show e)

instance ToText ScheduleActivityTaskFailedCause where
    toText = \case
        SATFCActivityCreationRateExceeded -> "ACTIVITY_CREATION_RATE_EXCEEDED"
        SATFCActivityIDAlreadyINUse -> "ACTIVITY_ID_ALREADY_IN_USE"
        SATFCActivityTypeDeprecated -> "ACTIVITY_TYPE_DEPRECATED"
        SATFCActivityTypeDoesNotExist -> "ACTIVITY_TYPE_DOES_NOT_EXIST"
        SATFCDefaultHeartbeatTimeoutUndefined -> "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED"
        SATFCDefaultScheduleTOCloseTimeoutUndefined -> "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED"
        SATFCDefaultScheduleTOStartTimeoutUndefined -> "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED"
        SATFCDefaultStartTOCloseTimeoutUndefined -> "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED"
        SATFCDefaultTaskListUndefined -> "DEFAULT_TASK_LIST_UNDEFINED"
        SATFCOpenActivitiesLimitExceeded -> "OPEN_ACTIVITIES_LIMIT_EXCEEDED"
        SATFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"

instance Hashable ScheduleActivityTaskFailedCause
instance ToQuery ScheduleActivityTaskFailedCause
instance ToHeader ScheduleActivityTaskFailedCause

instance FromJSON ScheduleActivityTaskFailedCause where
    parseJSON = parseJSONText "ScheduleActivityTaskFailedCause"

data SignalExternalWorkflowExecutionFailedCause
    = SEWEFCSignalExternalWorkflowExecutionRateExceeded
    | SEWEFCUnknownExternalWorkflowExecution
    | SEWEFCOperationNotPermitted
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText SignalExternalWorkflowExecutionFailedCause where
    parser = takeLowerText >>= \case
        "OPERATION_NOT_PERMITTED" -> pure SEWEFCOperationNotPermitted
        "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED" -> pure SEWEFCSignalExternalWorkflowExecutionRateExceeded
        "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION" -> pure SEWEFCUnknownExternalWorkflowExecution
        e -> fail ("Failure parsing SignalExternalWorkflowExecutionFailedCause from " ++ show e)

instance ToText SignalExternalWorkflowExecutionFailedCause where
    toText = \case
        SEWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        SEWEFCSignalExternalWorkflowExecutionRateExceeded -> "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
        SEWEFCUnknownExternalWorkflowExecution -> "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance Hashable SignalExternalWorkflowExecutionFailedCause
instance ToQuery SignalExternalWorkflowExecutionFailedCause
instance ToHeader SignalExternalWorkflowExecutionFailedCause

instance FromJSON SignalExternalWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "SignalExternalWorkflowExecutionFailedCause"

data StartChildWorkflowExecutionFailedCause
    = SCWEFCWorkflowTypeDoesNotExist
    | SCWEFCChildCreationRateExceeded
    | SCWEFCOperationNotPermitted
    | SCWEFCDefaultTaskListUndefined
    | SCWEFCDefaultChildPolicyUndefined
    | SCWEFCDefaultExecutionStartTOCloseTimeoutUndefined
    | SCWEFCWorkflowTypeDeprecated
    | SCWEFCWorkflowAlreadyRunning
    | SCWEFCDefaultTaskStartTOCloseTimeoutUndefined
    | SCWEFCOpenWorkflowsLimitExceeded
    | SCWEFCOpenChildrenLimitExceeded
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText StartChildWorkflowExecutionFailedCause where
    parser = takeLowerText >>= \case
        "CHILD_CREATION_RATE_EXCEEDED" -> pure SCWEFCChildCreationRateExceeded
        "DEFAULT_CHILD_POLICY_UNDEFINED" -> pure SCWEFCDefaultChildPolicyUndefined
        "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED" -> pure SCWEFCDefaultExecutionStartTOCloseTimeoutUndefined
        "DEFAULT_TASK_LIST_UNDEFINED" -> pure SCWEFCDefaultTaskListUndefined
        "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED" -> pure SCWEFCDefaultTaskStartTOCloseTimeoutUndefined
        "OPEN_CHILDREN_LIMIT_EXCEEDED" -> pure SCWEFCOpenChildrenLimitExceeded
        "OPEN_WORKFLOWS_LIMIT_EXCEEDED" -> pure SCWEFCOpenWorkflowsLimitExceeded
        "OPERATION_NOT_PERMITTED" -> pure SCWEFCOperationNotPermitted
        "WORKFLOW_ALREADY_RUNNING" -> pure SCWEFCWorkflowAlreadyRunning
        "WORKFLOW_TYPE_DEPRECATED" -> pure SCWEFCWorkflowTypeDeprecated
        "WORKFLOW_TYPE_DOES_NOT_EXIST" -> pure SCWEFCWorkflowTypeDoesNotExist
        e -> fail ("Failure parsing StartChildWorkflowExecutionFailedCause from " ++ show e)

instance ToText StartChildWorkflowExecutionFailedCause where
    toText = \case
        SCWEFCChildCreationRateExceeded -> "CHILD_CREATION_RATE_EXCEEDED"
        SCWEFCDefaultChildPolicyUndefined -> "DEFAULT_CHILD_POLICY_UNDEFINED"
        SCWEFCDefaultExecutionStartTOCloseTimeoutUndefined -> "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
        SCWEFCDefaultTaskListUndefined -> "DEFAULT_TASK_LIST_UNDEFINED"
        SCWEFCDefaultTaskStartTOCloseTimeoutUndefined -> "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
        SCWEFCOpenChildrenLimitExceeded -> "OPEN_CHILDREN_LIMIT_EXCEEDED"
        SCWEFCOpenWorkflowsLimitExceeded -> "OPEN_WORKFLOWS_LIMIT_EXCEEDED"
        SCWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        SCWEFCWorkflowAlreadyRunning -> "WORKFLOW_ALREADY_RUNNING"
        SCWEFCWorkflowTypeDeprecated -> "WORKFLOW_TYPE_DEPRECATED"
        SCWEFCWorkflowTypeDoesNotExist -> "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance Hashable StartChildWorkflowExecutionFailedCause
instance ToQuery StartChildWorkflowExecutionFailedCause
instance ToHeader StartChildWorkflowExecutionFailedCause

instance FromJSON StartChildWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "StartChildWorkflowExecutionFailedCause"

data StartTimerFailedCause
    = TimerIDAlreadyINUse
    | TimerCreationRateExceeded
    | OperationNotPermitted
    | OpenTimersLimitExceeded
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText StartTimerFailedCause where
    parser = takeLowerText >>= \case
        "OPEN_TIMERS_LIMIT_EXCEEDED" -> pure OpenTimersLimitExceeded
        "OPERATION_NOT_PERMITTED" -> pure OperationNotPermitted
        "TIMER_CREATION_RATE_EXCEEDED" -> pure TimerCreationRateExceeded
        "TIMER_ID_ALREADY_IN_USE" -> pure TimerIDAlreadyINUse
        e -> fail ("Failure parsing StartTimerFailedCause from " ++ show e)

instance ToText StartTimerFailedCause where
    toText = \case
        OpenTimersLimitExceeded -> "OPEN_TIMERS_LIMIT_EXCEEDED"
        OperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        TimerCreationRateExceeded -> "TIMER_CREATION_RATE_EXCEEDED"
        TimerIDAlreadyINUse -> "TIMER_ID_ALREADY_IN_USE"

instance Hashable StartTimerFailedCause
instance ToQuery StartTimerFailedCause
instance ToHeader StartTimerFailedCause

instance FromJSON StartTimerFailedCause where
    parseJSON = parseJSONText "StartTimerFailedCause"

data WorkflowExecutionCancelRequestedCause =
    ChildPolicyApplied
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText WorkflowExecutionCancelRequestedCause where
    parser = takeLowerText >>= \case
        "CHILD_POLICY_APPLIED" -> pure ChildPolicyApplied
        e -> fail ("Failure parsing WorkflowExecutionCancelRequestedCause from " ++ show e)

instance ToText WorkflowExecutionCancelRequestedCause where
    toText = \case
        ChildPolicyApplied -> "CHILD_POLICY_APPLIED"

instance Hashable WorkflowExecutionCancelRequestedCause
instance ToQuery WorkflowExecutionCancelRequestedCause
instance ToHeader WorkflowExecutionCancelRequestedCause

instance FromJSON WorkflowExecutionCancelRequestedCause where
    parseJSON = parseJSONText "WorkflowExecutionCancelRequestedCause"

data WorkflowExecutionTerminatedCause
    = WETCEventLimitExceeded
    | WETCOperatorInitiated
    | WETCChildPolicyApplied
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText WorkflowExecutionTerminatedCause where
    parser = takeLowerText >>= \case
        "CHILD_POLICY_APPLIED" -> pure WETCChildPolicyApplied
        "EVENT_LIMIT_EXCEEDED" -> pure WETCEventLimitExceeded
        "OPERATOR_INITIATED" -> pure WETCOperatorInitiated
        e -> fail ("Failure parsing WorkflowExecutionTerminatedCause from " ++ show e)

instance ToText WorkflowExecutionTerminatedCause where
    toText = \case
        WETCChildPolicyApplied -> "CHILD_POLICY_APPLIED"
        WETCEventLimitExceeded -> "EVENT_LIMIT_EXCEEDED"
        WETCOperatorInitiated -> "OPERATOR_INITIATED"

instance Hashable WorkflowExecutionTerminatedCause
instance ToQuery WorkflowExecutionTerminatedCause
instance ToHeader WorkflowExecutionTerminatedCause

instance FromJSON WorkflowExecutionTerminatedCause where
    parseJSON = parseJSONText "WorkflowExecutionTerminatedCause"

data WorkflowExecutionTimeoutType =
    WETTStartTOClose
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText WorkflowExecutionTimeoutType where
    parser = takeLowerText >>= \case
        "START_TO_CLOSE" -> pure WETTStartTOClose
        e -> fail ("Failure parsing WorkflowExecutionTimeoutType from " ++ show e)

instance ToText WorkflowExecutionTimeoutType where
    toText = \case
        WETTStartTOClose -> "START_TO_CLOSE"

instance Hashable WorkflowExecutionTimeoutType
instance ToQuery WorkflowExecutionTimeoutType
instance ToHeader WorkflowExecutionTimeoutType

instance FromJSON WorkflowExecutionTimeoutType where
    parseJSON = parseJSONText "WorkflowExecutionTimeoutType"

-- | Provides details of the @ActivityTaskCancelRequested@ event.
--
-- /See:/ 'activityTaskCancelRequestedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atcreaDecisionTaskCompletedEventId'
--
-- * 'atcreaActivityId'
data ActivityTaskCancelRequestedEventAttributes = ActivityTaskCancelRequestedEventAttributes'
    { _atcreaDecisionTaskCompletedEventId :: !Integer
    , _atcreaActivityId                   :: Text
    } deriving (Eq,Read,Show)

-- | 'ActivityTaskCancelRequestedEventAttributes' smart constructor.
activityTaskCancelRequestedEventAttributes :: Integer -> Text -> ActivityTaskCancelRequestedEventAttributes
activityTaskCancelRequestedEventAttributes pDecisionTaskCompletedEventId pActivityId =
    ActivityTaskCancelRequestedEventAttributes'
    { _atcreaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    , _atcreaActivityId = pActivityId
    }

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @RequestCancelActivityTask@ decision
-- for this cancellation request. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
atcreaDecisionTaskCompletedEventId :: Lens' ActivityTaskCancelRequestedEventAttributes Integer
atcreaDecisionTaskCompletedEventId = lens _atcreaDecisionTaskCompletedEventId (\ s a -> s{_atcreaDecisionTaskCompletedEventId = a});

-- | The unique ID of the task.
atcreaActivityId :: Lens' ActivityTaskCancelRequestedEventAttributes Text
atcreaActivityId = lens _atcreaActivityId (\ s a -> s{_atcreaActivityId = a});

instance FromJSON
         ActivityTaskCancelRequestedEventAttributes where
        parseJSON
          = withObject
              "ActivityTaskCancelRequestedEventAttributes"
              (\ x ->
                 ActivityTaskCancelRequestedEventAttributes' <$>
                   (x .: "decisionTaskCompletedEventId") <*>
                     (x .: "activityId"))

-- | Provides details of the @ActivityTaskCanceled@ event.
--
-- /See:/ 'activityTaskCanceledEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'actLatestCancelRequestedEventId'
--
-- * 'actDetails'
--
-- * 'actScheduledEventId'
--
-- * 'actStartedEventId'
data ActivityTaskCanceledEventAttributes = ActivityTaskCanceledEventAttributes'
    { _actLatestCancelRequestedEventId :: Maybe Integer
    , _actDetails                      :: Maybe Text
    , _actScheduledEventId             :: !Integer
    , _actStartedEventId               :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ActivityTaskCanceledEventAttributes' smart constructor.
activityTaskCanceledEventAttributes :: Integer -> Integer -> ActivityTaskCanceledEventAttributes
activityTaskCanceledEventAttributes pScheduledEventId pStartedEventId =
    ActivityTaskCanceledEventAttributes'
    { _actLatestCancelRequestedEventId = Nothing
    , _actDetails = Nothing
    , _actScheduledEventId = pScheduledEventId
    , _actStartedEventId = pStartedEventId
    }

-- | If set, contains the Id of the last @ActivityTaskCancelRequested@ event
-- recorded for this activity task. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
actLatestCancelRequestedEventId :: Lens' ActivityTaskCanceledEventAttributes (Maybe Integer)
actLatestCancelRequestedEventId = lens _actLatestCancelRequestedEventId (\ s a -> s{_actLatestCancelRequestedEventId = a});

-- | Details of the cancellation (if any).
actDetails :: Lens' ActivityTaskCanceledEventAttributes (Maybe Text)
actDetails = lens _actDetails (\ s a -> s{_actDetails = a});

-- | The id of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
actScheduledEventId :: Lens' ActivityTaskCanceledEventAttributes Integer
actScheduledEventId = lens _actScheduledEventId (\ s a -> s{_actScheduledEventId = a});

-- | The Id of the @ActivityTaskStarted@ event recorded when this activity
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
actStartedEventId :: Lens' ActivityTaskCanceledEventAttributes Integer
actStartedEventId = lens _actStartedEventId (\ s a -> s{_actStartedEventId = a});

instance FromJSON ActivityTaskCanceledEventAttributes
         where
        parseJSON
          = withObject "ActivityTaskCanceledEventAttributes"
              (\ x ->
                 ActivityTaskCanceledEventAttributes' <$>
                   (x .:? "latestCancelRequestedEventId") <*>
                     (x .:? "details")
                     <*> (x .: "scheduledEventId")
                     <*> (x .: "startedEventId"))

-- | Provides details of the @ActivityTaskCompleted@ event.
--
-- /See:/ 'activityTaskCompletedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atceaResult'
--
-- * 'atceaScheduledEventId'
--
-- * 'atceaStartedEventId'
data ActivityTaskCompletedEventAttributes = ActivityTaskCompletedEventAttributes'
    { _atceaResult           :: Maybe Text
    , _atceaScheduledEventId :: !Integer
    , _atceaStartedEventId   :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ActivityTaskCompletedEventAttributes' smart constructor.
activityTaskCompletedEventAttributes :: Integer -> Integer -> ActivityTaskCompletedEventAttributes
activityTaskCompletedEventAttributes pScheduledEventId pStartedEventId =
    ActivityTaskCompletedEventAttributes'
    { _atceaResult = Nothing
    , _atceaScheduledEventId = pScheduledEventId
    , _atceaStartedEventId = pStartedEventId
    }

-- | The results of the activity task (if any).
atceaResult :: Lens' ActivityTaskCompletedEventAttributes (Maybe Text)
atceaResult = lens _atceaResult (\ s a -> s{_atceaResult = a});

-- | The id of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
atceaScheduledEventId :: Lens' ActivityTaskCompletedEventAttributes Integer
atceaScheduledEventId = lens _atceaScheduledEventId (\ s a -> s{_atceaScheduledEventId = a});

-- | The Id of the @ActivityTaskStarted@ event recorded when this activity
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
atceaStartedEventId :: Lens' ActivityTaskCompletedEventAttributes Integer
atceaStartedEventId = lens _atceaStartedEventId (\ s a -> s{_atceaStartedEventId = a});

instance FromJSON
         ActivityTaskCompletedEventAttributes where
        parseJSON
          = withObject "ActivityTaskCompletedEventAttributes"
              (\ x ->
                 ActivityTaskCompletedEventAttributes' <$>
                   (x .:? "result") <*> (x .: "scheduledEventId") <*>
                     (x .: "startedEventId"))

-- | Provides details of the @ActivityTaskFailed@ event.
--
-- /See:/ 'activityTaskFailedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atfeaReason'
--
-- * 'atfeaDetails'
--
-- * 'atfeaScheduledEventId'
--
-- * 'atfeaStartedEventId'
data ActivityTaskFailedEventAttributes = ActivityTaskFailedEventAttributes'
    { _atfeaReason           :: Maybe Text
    , _atfeaDetails          :: Maybe Text
    , _atfeaScheduledEventId :: !Integer
    , _atfeaStartedEventId   :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ActivityTaskFailedEventAttributes' smart constructor.
activityTaskFailedEventAttributes :: Integer -> Integer -> ActivityTaskFailedEventAttributes
activityTaskFailedEventAttributes pScheduledEventId pStartedEventId =
    ActivityTaskFailedEventAttributes'
    { _atfeaReason = Nothing
    , _atfeaDetails = Nothing
    , _atfeaScheduledEventId = pScheduledEventId
    , _atfeaStartedEventId = pStartedEventId
    }

-- | The reason provided for the failure (if any).
atfeaReason :: Lens' ActivityTaskFailedEventAttributes (Maybe Text)
atfeaReason = lens _atfeaReason (\ s a -> s{_atfeaReason = a});

-- | The details of the failure (if any).
atfeaDetails :: Lens' ActivityTaskFailedEventAttributes (Maybe Text)
atfeaDetails = lens _atfeaDetails (\ s a -> s{_atfeaDetails = a});

-- | The id of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
atfeaScheduledEventId :: Lens' ActivityTaskFailedEventAttributes Integer
atfeaScheduledEventId = lens _atfeaScheduledEventId (\ s a -> s{_atfeaScheduledEventId = a});

-- | The Id of the @ActivityTaskStarted@ event recorded when this activity
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
atfeaStartedEventId :: Lens' ActivityTaskFailedEventAttributes Integer
atfeaStartedEventId = lens _atfeaStartedEventId (\ s a -> s{_atfeaStartedEventId = a});

instance FromJSON ActivityTaskFailedEventAttributes
         where
        parseJSON
          = withObject "ActivityTaskFailedEventAttributes"
              (\ x ->
                 ActivityTaskFailedEventAttributes' <$>
                   (x .:? "reason") <*> (x .:? "details") <*>
                     (x .: "scheduledEventId")
                     <*> (x .: "startedEventId"))

-- | Provides details of the @ActivityTaskScheduled@ event.
--
-- /See:/ 'activityTaskScheduledEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atseaControl'
--
-- * 'atseaScheduleToCloseTimeout'
--
-- * 'atseaHeartbeatTimeout'
--
-- * 'atseaInput'
--
-- * 'atseaTaskPriority'
--
-- * 'atseaScheduleToStartTimeout'
--
-- * 'atseaStartToCloseTimeout'
--
-- * 'atseaActivityType'
--
-- * 'atseaActivityId'
--
-- * 'atseaTaskList'
--
-- * 'atseaDecisionTaskCompletedEventId'
data ActivityTaskScheduledEventAttributes = ActivityTaskScheduledEventAttributes'
    { _atseaControl                      :: Maybe Text
    , _atseaScheduleToCloseTimeout       :: Maybe Text
    , _atseaHeartbeatTimeout             :: Maybe Text
    , _atseaInput                        :: Maybe Text
    , _atseaTaskPriority                 :: Maybe Text
    , _atseaScheduleToStartTimeout       :: Maybe Text
    , _atseaStartToCloseTimeout          :: Maybe Text
    , _atseaActivityType                 :: ActivityType
    , _atseaActivityId                   :: Text
    , _atseaTaskList                     :: TaskList
    , _atseaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ActivityTaskScheduledEventAttributes' smart constructor.
activityTaskScheduledEventAttributes :: ActivityType -> Text -> TaskList -> Integer -> ActivityTaskScheduledEventAttributes
activityTaskScheduledEventAttributes pActivityType pActivityId pTaskList pDecisionTaskCompletedEventId =
    ActivityTaskScheduledEventAttributes'
    { _atseaControl = Nothing
    , _atseaScheduleToCloseTimeout = Nothing
    , _atseaHeartbeatTimeout = Nothing
    , _atseaInput = Nothing
    , _atseaTaskPriority = Nothing
    , _atseaScheduleToStartTimeout = Nothing
    , _atseaStartToCloseTimeout = Nothing
    , _atseaActivityType = pActivityType
    , _atseaActivityId = pActivityId
    , _atseaTaskList = pTaskList
    , _atseaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | /Optional./ Data attached to the event that can be used by the decider
-- in subsequent workflow tasks. This data is not sent to the activity.
atseaControl :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaControl = lens _atseaControl (\ s a -> s{_atseaControl = a});

-- | The maximum amount of time for this activity task.
atseaScheduleToCloseTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaScheduleToCloseTimeout = lens _atseaScheduleToCloseTimeout (\ s a -> s{_atseaScheduleToCloseTimeout = a});

-- | The maximum time before which the worker processing this task must
-- report progress by calling RecordActivityTaskHeartbeat. If the timeout
-- is exceeded, the activity task is automatically timed out. If the worker
-- subsequently attempts to record a heartbeat or return a result, it will
-- be ignored.
atseaHeartbeatTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaHeartbeatTimeout = lens _atseaHeartbeatTimeout (\ s a -> s{_atseaHeartbeatTimeout = a});

-- | The input provided to the activity task.
atseaInput :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaInput = lens _atseaInput (\ s a -> s{_atseaInput = a});

-- | /Optional./ The priority to assign to the scheduled activity task. This
-- will override any default priority that was assigned when the activity
-- type was registered. If not set, then the priority set on the activity
-- type is used as the task priority.
--
-- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon Simple Workflow Developer Guide/.
atseaTaskPriority :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaTaskPriority = lens _atseaTaskPriority (\ s a -> s{_atseaTaskPriority = a});

-- | The maximum amount of time the activity task can wait to be assigned to
-- a worker.
atseaScheduleToStartTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaScheduleToStartTimeout = lens _atseaScheduleToStartTimeout (\ s a -> s{_atseaScheduleToStartTimeout = a});

-- | The maximum amount of time a worker may take to process the activity
-- task.
atseaStartToCloseTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaStartToCloseTimeout = lens _atseaStartToCloseTimeout (\ s a -> s{_atseaStartToCloseTimeout = a});

-- | The type of the activity task.
atseaActivityType :: Lens' ActivityTaskScheduledEventAttributes ActivityType
atseaActivityType = lens _atseaActivityType (\ s a -> s{_atseaActivityType = a});

-- | The unique id of the activity task.
atseaActivityId :: Lens' ActivityTaskScheduledEventAttributes Text
atseaActivityId = lens _atseaActivityId (\ s a -> s{_atseaActivityId = a});

-- | The task list in which the activity task has been scheduled.
atseaTaskList :: Lens' ActivityTaskScheduledEventAttributes TaskList
atseaTaskList = lens _atseaTaskList (\ s a -> s{_atseaTaskList = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision that resulted in the scheduling of this activity task. This
-- information can be useful for diagnosing problems by tracing back the
-- chain of events leading up to this event.
atseaDecisionTaskCompletedEventId :: Lens' ActivityTaskScheduledEventAttributes Integer
atseaDecisionTaskCompletedEventId = lens _atseaDecisionTaskCompletedEventId (\ s a -> s{_atseaDecisionTaskCompletedEventId = a});

instance FromJSON
         ActivityTaskScheduledEventAttributes where
        parseJSON
          = withObject "ActivityTaskScheduledEventAttributes"
              (\ x ->
                 ActivityTaskScheduledEventAttributes' <$>
                   (x .:? "control") <*>
                     (x .:? "scheduleToCloseTimeout")
                     <*> (x .:? "heartbeatTimeout")
                     <*> (x .:? "input")
                     <*> (x .:? "taskPriority")
                     <*> (x .:? "scheduleToStartTimeout")
                     <*> (x .:? "startToCloseTimeout")
                     <*> (x .: "activityType")
                     <*> (x .: "activityId")
                     <*> (x .: "taskList")
                     <*> (x .: "decisionTaskCompletedEventId"))

-- | Provides details of the @ActivityTaskStarted@ event.
--
-- /See:/ 'activityTaskStartedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atseaIdentity'
--
-- * 'atseaScheduledEventId'
data ActivityTaskStartedEventAttributes = ActivityTaskStartedEventAttributes'
    { _atseaIdentity         :: Maybe Text
    , _atseaScheduledEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ActivityTaskStartedEventAttributes' smart constructor.
activityTaskStartedEventAttributes :: Integer -> ActivityTaskStartedEventAttributes
activityTaskStartedEventAttributes pScheduledEventId =
    ActivityTaskStartedEventAttributes'
    { _atseaIdentity = Nothing
    , _atseaScheduledEventId = pScheduledEventId
    }

-- | Identity of the worker that was assigned this task. This aids
-- diagnostics when problems arise. The form of this identity is user
-- defined.
atseaIdentity :: Lens' ActivityTaskStartedEventAttributes (Maybe Text)
atseaIdentity = lens _atseaIdentity (\ s a -> s{_atseaIdentity = a});

-- | The id of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
atseaScheduledEventId :: Lens' ActivityTaskStartedEventAttributes Integer
atseaScheduledEventId = lens _atseaScheduledEventId (\ s a -> s{_atseaScheduledEventId = a});

instance FromJSON ActivityTaskStartedEventAttributes
         where
        parseJSON
          = withObject "ActivityTaskStartedEventAttributes"
              (\ x ->
                 ActivityTaskStartedEventAttributes' <$>
                   (x .:? "identity") <*> (x .: "scheduledEventId"))

-- | Provides details of the @ActivityTaskTimedOut@ event.
--
-- /See:/ 'activityTaskTimedOutEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attoeaDetails'
--
-- * 'attoeaTimeoutType'
--
-- * 'attoeaScheduledEventId'
--
-- * 'attoeaStartedEventId'
data ActivityTaskTimedOutEventAttributes = ActivityTaskTimedOutEventAttributes'
    { _attoeaDetails          :: Maybe Text
    , _attoeaTimeoutType      :: ActivityTaskTimeoutType
    , _attoeaScheduledEventId :: !Integer
    , _attoeaStartedEventId   :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ActivityTaskTimedOutEventAttributes' smart constructor.
activityTaskTimedOutEventAttributes :: ActivityTaskTimeoutType -> Integer -> Integer -> ActivityTaskTimedOutEventAttributes
activityTaskTimedOutEventAttributes pTimeoutType pScheduledEventId pStartedEventId =
    ActivityTaskTimedOutEventAttributes'
    { _attoeaDetails = Nothing
    , _attoeaTimeoutType = pTimeoutType
    , _attoeaScheduledEventId = pScheduledEventId
    , _attoeaStartedEventId = pStartedEventId
    }

-- | Contains the content of the @details@ parameter for the last call made
-- by the activity to @RecordActivityTaskHeartbeat@.
attoeaDetails :: Lens' ActivityTaskTimedOutEventAttributes (Maybe Text)
attoeaDetails = lens _attoeaDetails (\ s a -> s{_attoeaDetails = a});

-- | The type of the timeout that caused this event.
attoeaTimeoutType :: Lens' ActivityTaskTimedOutEventAttributes ActivityTaskTimeoutType
attoeaTimeoutType = lens _attoeaTimeoutType (\ s a -> s{_attoeaTimeoutType = a});

-- | The id of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
attoeaScheduledEventId :: Lens' ActivityTaskTimedOutEventAttributes Integer
attoeaScheduledEventId = lens _attoeaScheduledEventId (\ s a -> s{_attoeaScheduledEventId = a});

-- | The Id of the @ActivityTaskStarted@ event recorded when this activity
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
attoeaStartedEventId :: Lens' ActivityTaskTimedOutEventAttributes Integer
attoeaStartedEventId = lens _attoeaStartedEventId (\ s a -> s{_attoeaStartedEventId = a});

instance FromJSON ActivityTaskTimedOutEventAttributes
         where
        parseJSON
          = withObject "ActivityTaskTimedOutEventAttributes"
              (\ x ->
                 ActivityTaskTimedOutEventAttributes' <$>
                   (x .:? "details") <*> (x .: "timeoutType") <*>
                     (x .: "scheduledEventId")
                     <*> (x .: "startedEventId"))

-- | Represents an activity type.
--
-- /See:/ 'activityType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atName'
--
-- * 'atVersion'
data ActivityType = ActivityType'
    { _atName    :: Text
    , _atVersion :: Text
    } deriving (Eq,Read,Show)

-- | 'ActivityType' smart constructor.
activityType :: Text -> Text -> ActivityType
activityType pName pVersion =
    ActivityType'
    { _atName = pName
    , _atVersion = pVersion
    }

-- | The name of this activity.
--
-- The combination of activity type name and version must be unique within
-- a domain.
atName :: Lens' ActivityType Text
atName = lens _atName (\ s a -> s{_atName = a});

-- | The version of this activity.
--
-- The combination of activity type name and version must be unique with in
-- a domain.
atVersion :: Lens' ActivityType Text
atVersion = lens _atVersion (\ s a -> s{_atVersion = a});

instance FromJSON ActivityType where
        parseJSON
          = withObject "ActivityType"
              (\ x ->
                 ActivityType' <$> (x .: "name") <*> (x .: "version"))

instance ToJSON ActivityType where
        toJSON ActivityType'{..}
          = object ["name" .= _atName, "version" .= _atVersion]

-- | Configuration settings registered with the activity type.
--
-- /See:/ 'activityTypeConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atcDefaultTaskScheduleToStartTimeout'
--
-- * 'atcDefaultTaskList'
--
-- * 'atcDefaultTaskPriority'
--
-- * 'atcDefaultTaskHeartbeatTimeout'
--
-- * 'atcDefaultTaskScheduleToCloseTimeout'
--
-- * 'atcDefaultTaskStartToCloseTimeout'
data ActivityTypeConfiguration = ActivityTypeConfiguration'
    { _atcDefaultTaskScheduleToStartTimeout :: Maybe Text
    , _atcDefaultTaskList                   :: Maybe TaskList
    , _atcDefaultTaskPriority               :: Maybe Text
    , _atcDefaultTaskHeartbeatTimeout       :: Maybe Text
    , _atcDefaultTaskScheduleToCloseTimeout :: Maybe Text
    , _atcDefaultTaskStartToCloseTimeout    :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'ActivityTypeConfiguration' smart constructor.
activityTypeConfiguration :: ActivityTypeConfiguration
activityTypeConfiguration =
    ActivityTypeConfiguration'
    { _atcDefaultTaskScheduleToStartTimeout = Nothing
    , _atcDefaultTaskList = Nothing
    , _atcDefaultTaskPriority = Nothing
    , _atcDefaultTaskHeartbeatTimeout = Nothing
    , _atcDefaultTaskScheduleToCloseTimeout = Nothing
    , _atcDefaultTaskStartToCloseTimeout = Nothing
    }

-- | /Optional./ The default maximum duration, specified when registering the
-- activity type, that a task of an activity type can wait before being
-- assigned to a worker. You can override this default when scheduling a
-- task through the @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
atcDefaultTaskScheduleToStartTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskScheduleToStartTimeout = lens _atcDefaultTaskScheduleToStartTimeout (\ s a -> s{_atcDefaultTaskScheduleToStartTimeout = a});

-- | /Optional./ The default task list specified for this activity type at
-- registration. This default is used if a task list is not provided when a
-- task is scheduled through the @ScheduleActivityTask@ Decision. You can
-- override the default registered task list when scheduling a task through
-- the @ScheduleActivityTask@ Decision.
atcDefaultTaskList :: Lens' ActivityTypeConfiguration (Maybe TaskList)
atcDefaultTaskList = lens _atcDefaultTaskList (\ s a -> s{_atcDefaultTaskList = a});

-- | /Optional./ The default task priority for tasks of this activity type,
-- specified at registration. If not set, then \"0\" will be used as the
-- default priority. This default can be overridden when scheduling an
-- activity task.
--
-- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon Simple Workflow Developer Guide/.
atcDefaultTaskPriority :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskPriority = lens _atcDefaultTaskPriority (\ s a -> s{_atcDefaultTaskPriority = a});

-- | /Optional./ The default maximum time, in seconds, before which a worker
-- processing a task must report progress by calling
-- RecordActivityTaskHeartbeat.
--
-- You can specify this value only when /registering/ an activity type. The
-- registered default value can be overridden when you schedule a task
-- through the @ScheduleActivityTask@ Decision. If the activity worker
-- subsequently attempts to record a heartbeat or returns a result, the
-- activity worker receives an @UnknownResource@ fault. In this case,
-- Amazon SWF no longer considers the activity task to be valid; the
-- activity worker should clean up the activity task.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
atcDefaultTaskHeartbeatTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskHeartbeatTimeout = lens _atcDefaultTaskHeartbeatTimeout (\ s a -> s{_atcDefaultTaskHeartbeatTimeout = a});

-- | /Optional./ The default maximum duration, specified when registering the
-- activity type, for tasks of this activity type. You can override this
-- default when scheduling a task through the @ScheduleActivityTask@
-- Decision.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
atcDefaultTaskScheduleToCloseTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskScheduleToCloseTimeout = lens _atcDefaultTaskScheduleToCloseTimeout (\ s a -> s{_atcDefaultTaskScheduleToCloseTimeout = a});

-- | /Optional./ The default maximum duration for tasks of an activity type
-- specified when registering the activity type. You can override this
-- default when scheduling a task through the @ScheduleActivityTask@
-- Decision.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
atcDefaultTaskStartToCloseTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskStartToCloseTimeout = lens _atcDefaultTaskStartToCloseTimeout (\ s a -> s{_atcDefaultTaskStartToCloseTimeout = a});

instance FromJSON ActivityTypeConfiguration where
        parseJSON
          = withObject "ActivityTypeConfiguration"
              (\ x ->
                 ActivityTypeConfiguration' <$>
                   (x .:? "defaultTaskScheduleToStartTimeout") <*>
                     (x .:? "defaultTaskList")
                     <*> (x .:? "defaultTaskPriority")
                     <*> (x .:? "defaultTaskHeartbeatTimeout")
                     <*> (x .:? "defaultTaskScheduleToCloseTimeout")
                     <*> (x .:? "defaultTaskStartToCloseTimeout"))

-- | Detailed information about an activity type.
--
-- /See:/ 'activityTypeInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atiDeprecationDate'
--
-- * 'atiDescription'
--
-- * 'atiActivityType'
--
-- * 'atiStatus'
--
-- * 'atiCreationDate'
data ActivityTypeInfo = ActivityTypeInfo'
    { _atiDeprecationDate :: Maybe POSIX
    , _atiDescription     :: Maybe Text
    , _atiActivityType    :: ActivityType
    , _atiStatus          :: RegistrationStatus
    , _atiCreationDate    :: POSIX
    } deriving (Eq,Read,Show)

-- | 'ActivityTypeInfo' smart constructor.
activityTypeInfo :: ActivityType -> RegistrationStatus -> UTCTime -> ActivityTypeInfo
activityTypeInfo pActivityType pStatus pCreationDate =
    ActivityTypeInfo'
    { _atiDeprecationDate = Nothing
    , _atiDescription = Nothing
    , _atiActivityType = pActivityType
    , _atiStatus = pStatus
    , _atiCreationDate = _Time # pCreationDate
    }

-- | If DEPRECATED, the date and time DeprecateActivityType was called.
atiDeprecationDate :: Lens' ActivityTypeInfo (Maybe UTCTime)
atiDeprecationDate = lens _atiDeprecationDate (\ s a -> s{_atiDeprecationDate = a}) . mapping _Time;

-- | The description of the activity type provided in RegisterActivityType.
atiDescription :: Lens' ActivityTypeInfo (Maybe Text)
atiDescription = lens _atiDescription (\ s a -> s{_atiDescription = a});

-- | The ActivityType type structure representing the activity type.
atiActivityType :: Lens' ActivityTypeInfo ActivityType
atiActivityType = lens _atiActivityType (\ s a -> s{_atiActivityType = a});

-- | The current status of the activity type.
atiStatus :: Lens' ActivityTypeInfo RegistrationStatus
atiStatus = lens _atiStatus (\ s a -> s{_atiStatus = a});

-- | The date and time this activity type was created through
-- RegisterActivityType.
atiCreationDate :: Lens' ActivityTypeInfo UTCTime
atiCreationDate = lens _atiCreationDate (\ s a -> s{_atiCreationDate = a}) . _Time;

instance FromJSON ActivityTypeInfo where
        parseJSON
          = withObject "ActivityTypeInfo"
              (\ x ->
                 ActivityTypeInfo' <$>
                   (x .:? "deprecationDate") <*> (x .:? "description")
                     <*> (x .: "activityType")
                     <*> (x .: "status")
                     <*> (x .: "creationDate"))

-- | Provides details of the @CancelTimer@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- /See:/ 'cancelTimerDecisionAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctdaTimerId'
newtype CancelTimerDecisionAttributes = CancelTimerDecisionAttributes'
    { _ctdaTimerId :: Text
    } deriving (Eq,Read,Show)

-- | 'CancelTimerDecisionAttributes' smart constructor.
cancelTimerDecisionAttributes :: Text -> CancelTimerDecisionAttributes
cancelTimerDecisionAttributes pTimerId =
    CancelTimerDecisionAttributes'
    { _ctdaTimerId = pTimerId
    }

-- | __Required.__ The unique Id of the timer to cancel.
ctdaTimerId :: Lens' CancelTimerDecisionAttributes Text
ctdaTimerId = lens _ctdaTimerId (\ s a -> s{_ctdaTimerId = a});

instance ToJSON CancelTimerDecisionAttributes where
        toJSON CancelTimerDecisionAttributes'{..}
          = object ["timerId" .= _ctdaTimerId]

-- | Provides details of the @CancelTimerFailed@ event.
--
-- /See:/ 'cancelTimerFailedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctfeaTimerId'
--
-- * 'ctfeaCause'
--
-- * 'ctfeaDecisionTaskCompletedEventId'
data CancelTimerFailedEventAttributes = CancelTimerFailedEventAttributes'
    { _ctfeaTimerId                      :: Text
    , _ctfeaCause                        :: CancelTimerFailedCause
    , _ctfeaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'CancelTimerFailedEventAttributes' smart constructor.
cancelTimerFailedEventAttributes :: Text -> CancelTimerFailedCause -> Integer -> CancelTimerFailedEventAttributes
cancelTimerFailedEventAttributes pTimerId pCause pDecisionTaskCompletedEventId =
    CancelTimerFailedEventAttributes'
    { _ctfeaTimerId = pTimerId
    , _ctfeaCause = pCause
    , _ctfeaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | The timerId provided in the @CancelTimer@ decision that failed.
ctfeaTimerId :: Lens' CancelTimerFailedEventAttributes Text
ctfeaTimerId = lens _ctfeaTimerId (\ s a -> s{_ctfeaTimerId = a});

-- | The cause of the failure. This information is generated by the system
-- and can be useful for diagnostic purposes.
--
-- If __cause__ is set to OPERATION_NOT_PERMITTED, the decision failed
-- because it lacked sufficient permissions. For details and example IAM
-- policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
ctfeaCause :: Lens' CancelTimerFailedEventAttributes CancelTimerFailedCause
ctfeaCause = lens _ctfeaCause (\ s a -> s{_ctfeaCause = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @CancelTimer@ decision to cancel this
-- timer. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
ctfeaDecisionTaskCompletedEventId :: Lens' CancelTimerFailedEventAttributes Integer
ctfeaDecisionTaskCompletedEventId = lens _ctfeaDecisionTaskCompletedEventId (\ s a -> s{_ctfeaDecisionTaskCompletedEventId = a});

instance FromJSON CancelTimerFailedEventAttributes
         where
        parseJSON
          = withObject "CancelTimerFailedEventAttributes"
              (\ x ->
                 CancelTimerFailedEventAttributes' <$>
                   (x .: "timerId") <*> (x .: "cause") <*>
                     (x .: "decisionTaskCompletedEventId"))

-- | Provides details of the @CancelWorkflowExecution@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- /See:/ 'cancelWorkflowExecutionDecisionAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwedaDetails'
newtype CancelWorkflowExecutionDecisionAttributes = CancelWorkflowExecutionDecisionAttributes'
    { _cwedaDetails :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'CancelWorkflowExecutionDecisionAttributes' smart constructor.
cancelWorkflowExecutionDecisionAttributes :: CancelWorkflowExecutionDecisionAttributes
cancelWorkflowExecutionDecisionAttributes =
    CancelWorkflowExecutionDecisionAttributes'
    { _cwedaDetails = Nothing
    }

-- | /Optional./ details of the cancellation.
cwedaDetails :: Lens' CancelWorkflowExecutionDecisionAttributes (Maybe Text)
cwedaDetails = lens _cwedaDetails (\ s a -> s{_cwedaDetails = a});

instance ToJSON
         CancelWorkflowExecutionDecisionAttributes where
        toJSON CancelWorkflowExecutionDecisionAttributes'{..}
          = object ["details" .= _cwedaDetails]

-- | Provides details of the @CancelWorkflowExecutionFailed@ event.
--
-- /See:/ 'cancelWorkflowExecutionFailedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'canCause'
--
-- * 'canDecisionTaskCompletedEventId'
data CancelWorkflowExecutionFailedEventAttributes = CancelWorkflowExecutionFailedEventAttributes'
    { _canCause                        :: CancelWorkflowExecutionFailedCause
    , _canDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'CancelWorkflowExecutionFailedEventAttributes' smart constructor.
cancelWorkflowExecutionFailedEventAttributes :: CancelWorkflowExecutionFailedCause -> Integer -> CancelWorkflowExecutionFailedEventAttributes
cancelWorkflowExecutionFailedEventAttributes pCause pDecisionTaskCompletedEventId =
    CancelWorkflowExecutionFailedEventAttributes'
    { _canCause = pCause
    , _canDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | The cause of the failure. This information is generated by the system
-- and can be useful for diagnostic purposes.
--
-- If __cause__ is set to OPERATION_NOT_PERMITTED, the decision failed
-- because it lacked sufficient permissions. For details and example IAM
-- policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
canCause :: Lens' CancelWorkflowExecutionFailedEventAttributes CancelWorkflowExecutionFailedCause
canCause = lens _canCause (\ s a -> s{_canCause = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @CancelWorkflowExecution@ decision
-- for this cancellation request. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
canDecisionTaskCompletedEventId :: Lens' CancelWorkflowExecutionFailedEventAttributes Integer
canDecisionTaskCompletedEventId = lens _canDecisionTaskCompletedEventId (\ s a -> s{_canDecisionTaskCompletedEventId = a});

instance FromJSON
         CancelWorkflowExecutionFailedEventAttributes where
        parseJSON
          = withObject
              "CancelWorkflowExecutionFailedEventAttributes"
              (\ x ->
                 CancelWorkflowExecutionFailedEventAttributes' <$>
                   (x .: "cause") <*>
                     (x .: "decisionTaskCompletedEventId"))

-- | Provide details of the @ChildWorkflowExecutionCanceled@ event.
--
-- /See:/ 'childWorkflowExecutionCanceledEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chiDetails'
--
-- * 'chiWorkflowExecution'
--
-- * 'chiWorkflowType'
--
-- * 'chiInitiatedEventId'
--
-- * 'chiStartedEventId'
data ChildWorkflowExecutionCanceledEventAttributes = ChildWorkflowExecutionCanceledEventAttributes'
    { _chiDetails           :: Maybe Text
    , _chiWorkflowExecution :: WorkflowExecution
    , _chiWorkflowType      :: WorkflowType
    , _chiInitiatedEventId  :: !Integer
    , _chiStartedEventId    :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ChildWorkflowExecutionCanceledEventAttributes' smart constructor.
childWorkflowExecutionCanceledEventAttributes :: WorkflowExecution -> WorkflowType -> Integer -> Integer -> ChildWorkflowExecutionCanceledEventAttributes
childWorkflowExecutionCanceledEventAttributes pWorkflowExecution pWorkflowType pInitiatedEventId pStartedEventId =
    ChildWorkflowExecutionCanceledEventAttributes'
    { _chiDetails = Nothing
    , _chiWorkflowExecution = pWorkflowExecution
    , _chiWorkflowType = pWorkflowType
    , _chiInitiatedEventId = pInitiatedEventId
    , _chiStartedEventId = pStartedEventId
    }

-- | Details of the cancellation (if provided).
chiDetails :: Lens' ChildWorkflowExecutionCanceledEventAttributes (Maybe Text)
chiDetails = lens _chiDetails (\ s a -> s{_chiDetails = a});

-- | The child workflow execution that was canceled.
chiWorkflowExecution :: Lens' ChildWorkflowExecutionCanceledEventAttributes WorkflowExecution
chiWorkflowExecution = lens _chiWorkflowExecution (\ s a -> s{_chiWorkflowExecution = a});

-- | The type of the child workflow execution.
chiWorkflowType :: Lens' ChildWorkflowExecutionCanceledEventAttributes WorkflowType
chiWorkflowType = lens _chiWorkflowType (\ s a -> s{_chiWorkflowType = a});

-- | The id of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
chiInitiatedEventId :: Lens' ChildWorkflowExecutionCanceledEventAttributes Integer
chiInitiatedEventId = lens _chiInitiatedEventId (\ s a -> s{_chiInitiatedEventId = a});

-- | The Id of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
chiStartedEventId :: Lens' ChildWorkflowExecutionCanceledEventAttributes Integer
chiStartedEventId = lens _chiStartedEventId (\ s a -> s{_chiStartedEventId = a});

instance FromJSON
         ChildWorkflowExecutionCanceledEventAttributes where
        parseJSON
          = withObject
              "ChildWorkflowExecutionCanceledEventAttributes"
              (\ x ->
                 ChildWorkflowExecutionCanceledEventAttributes' <$>
                   (x .:? "details") <*> (x .: "workflowExecution") <*>
                     (x .: "workflowType")
                     <*> (x .: "initiatedEventId")
                     <*> (x .: "startedEventId"))

-- | Provides details of the @ChildWorkflowExecutionCompleted@ event.
--
-- /See:/ 'childWorkflowExecutionCompletedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cweceaResult'
--
-- * 'cweceaWorkflowExecution'
--
-- * 'cweceaWorkflowType'
--
-- * 'cweceaInitiatedEventId'
--
-- * 'cweceaStartedEventId'
data ChildWorkflowExecutionCompletedEventAttributes = ChildWorkflowExecutionCompletedEventAttributes'
    { _cweceaResult            :: Maybe Text
    , _cweceaWorkflowExecution :: WorkflowExecution
    , _cweceaWorkflowType      :: WorkflowType
    , _cweceaInitiatedEventId  :: !Integer
    , _cweceaStartedEventId    :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ChildWorkflowExecutionCompletedEventAttributes' smart constructor.
childWorkflowExecutionCompletedEventAttributes :: WorkflowExecution -> WorkflowType -> Integer -> Integer -> ChildWorkflowExecutionCompletedEventAttributes
childWorkflowExecutionCompletedEventAttributes pWorkflowExecution pWorkflowType pInitiatedEventId pStartedEventId =
    ChildWorkflowExecutionCompletedEventAttributes'
    { _cweceaResult = Nothing
    , _cweceaWorkflowExecution = pWorkflowExecution
    , _cweceaWorkflowType = pWorkflowType
    , _cweceaInitiatedEventId = pInitiatedEventId
    , _cweceaStartedEventId = pStartedEventId
    }

-- | The result of the child workflow execution (if any).
cweceaResult :: Lens' ChildWorkflowExecutionCompletedEventAttributes (Maybe Text)
cweceaResult = lens _cweceaResult (\ s a -> s{_cweceaResult = a});

-- | The child workflow execution that was completed.
cweceaWorkflowExecution :: Lens' ChildWorkflowExecutionCompletedEventAttributes WorkflowExecution
cweceaWorkflowExecution = lens _cweceaWorkflowExecution (\ s a -> s{_cweceaWorkflowExecution = a});

-- | The type of the child workflow execution.
cweceaWorkflowType :: Lens' ChildWorkflowExecutionCompletedEventAttributes WorkflowType
cweceaWorkflowType = lens _cweceaWorkflowType (\ s a -> s{_cweceaWorkflowType = a});

-- | The id of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
cweceaInitiatedEventId :: Lens' ChildWorkflowExecutionCompletedEventAttributes Integer
cweceaInitiatedEventId = lens _cweceaInitiatedEventId (\ s a -> s{_cweceaInitiatedEventId = a});

-- | The Id of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
cweceaStartedEventId :: Lens' ChildWorkflowExecutionCompletedEventAttributes Integer
cweceaStartedEventId = lens _cweceaStartedEventId (\ s a -> s{_cweceaStartedEventId = a});

instance FromJSON
         ChildWorkflowExecutionCompletedEventAttributes where
        parseJSON
          = withObject
              "ChildWorkflowExecutionCompletedEventAttributes"
              (\ x ->
                 ChildWorkflowExecutionCompletedEventAttributes' <$>
                   (x .:? "result") <*> (x .: "workflowExecution") <*>
                     (x .: "workflowType")
                     <*> (x .: "initiatedEventId")
                     <*> (x .: "startedEventId"))

-- | Provides details of the @ChildWorkflowExecutionFailed@ event.
--
-- /See:/ 'childWorkflowExecutionFailedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwefeaReason'
--
-- * 'cwefeaDetails'
--
-- * 'cwefeaWorkflowExecution'
--
-- * 'cwefeaWorkflowType'
--
-- * 'cwefeaInitiatedEventId'
--
-- * 'cwefeaStartedEventId'
data ChildWorkflowExecutionFailedEventAttributes = ChildWorkflowExecutionFailedEventAttributes'
    { _cwefeaReason            :: Maybe Text
    , _cwefeaDetails           :: Maybe Text
    , _cwefeaWorkflowExecution :: WorkflowExecution
    , _cwefeaWorkflowType      :: WorkflowType
    , _cwefeaInitiatedEventId  :: !Integer
    , _cwefeaStartedEventId    :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ChildWorkflowExecutionFailedEventAttributes' smart constructor.
childWorkflowExecutionFailedEventAttributes :: WorkflowExecution -> WorkflowType -> Integer -> Integer -> ChildWorkflowExecutionFailedEventAttributes
childWorkflowExecutionFailedEventAttributes pWorkflowExecution pWorkflowType pInitiatedEventId pStartedEventId =
    ChildWorkflowExecutionFailedEventAttributes'
    { _cwefeaReason = Nothing
    , _cwefeaDetails = Nothing
    , _cwefeaWorkflowExecution = pWorkflowExecution
    , _cwefeaWorkflowType = pWorkflowType
    , _cwefeaInitiatedEventId = pInitiatedEventId
    , _cwefeaStartedEventId = pStartedEventId
    }

-- | The reason for the failure (if provided).
cwefeaReason :: Lens' ChildWorkflowExecutionFailedEventAttributes (Maybe Text)
cwefeaReason = lens _cwefeaReason (\ s a -> s{_cwefeaReason = a});

-- | The details of the failure (if provided).
cwefeaDetails :: Lens' ChildWorkflowExecutionFailedEventAttributes (Maybe Text)
cwefeaDetails = lens _cwefeaDetails (\ s a -> s{_cwefeaDetails = a});

-- | The child workflow execution that failed.
cwefeaWorkflowExecution :: Lens' ChildWorkflowExecutionFailedEventAttributes WorkflowExecution
cwefeaWorkflowExecution = lens _cwefeaWorkflowExecution (\ s a -> s{_cwefeaWorkflowExecution = a});

-- | The type of the child workflow execution.
cwefeaWorkflowType :: Lens' ChildWorkflowExecutionFailedEventAttributes WorkflowType
cwefeaWorkflowType = lens _cwefeaWorkflowType (\ s a -> s{_cwefeaWorkflowType = a});

-- | The id of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
cwefeaInitiatedEventId :: Lens' ChildWorkflowExecutionFailedEventAttributes Integer
cwefeaInitiatedEventId = lens _cwefeaInitiatedEventId (\ s a -> s{_cwefeaInitiatedEventId = a});

-- | The Id of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
cwefeaStartedEventId :: Lens' ChildWorkflowExecutionFailedEventAttributes Integer
cwefeaStartedEventId = lens _cwefeaStartedEventId (\ s a -> s{_cwefeaStartedEventId = a});

instance FromJSON
         ChildWorkflowExecutionFailedEventAttributes where
        parseJSON
          = withObject
              "ChildWorkflowExecutionFailedEventAttributes"
              (\ x ->
                 ChildWorkflowExecutionFailedEventAttributes' <$>
                   (x .:? "reason") <*> (x .:? "details") <*>
                     (x .: "workflowExecution")
                     <*> (x .: "workflowType")
                     <*> (x .: "initiatedEventId")
                     <*> (x .: "startedEventId"))

-- | Provides details of the @ChildWorkflowExecutionStarted@ event.
--
-- /See:/ 'childWorkflowExecutionStartedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cweseaWorkflowExecution'
--
-- * 'cweseaWorkflowType'
--
-- * 'cweseaInitiatedEventId'
data ChildWorkflowExecutionStartedEventAttributes = ChildWorkflowExecutionStartedEventAttributes'
    { _cweseaWorkflowExecution :: WorkflowExecution
    , _cweseaWorkflowType      :: WorkflowType
    , _cweseaInitiatedEventId  :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ChildWorkflowExecutionStartedEventAttributes' smart constructor.
childWorkflowExecutionStartedEventAttributes :: WorkflowExecution -> WorkflowType -> Integer -> ChildWorkflowExecutionStartedEventAttributes
childWorkflowExecutionStartedEventAttributes pWorkflowExecution pWorkflowType pInitiatedEventId =
    ChildWorkflowExecutionStartedEventAttributes'
    { _cweseaWorkflowExecution = pWorkflowExecution
    , _cweseaWorkflowType = pWorkflowType
    , _cweseaInitiatedEventId = pInitiatedEventId
    }

-- | The child workflow execution that was started.
cweseaWorkflowExecution :: Lens' ChildWorkflowExecutionStartedEventAttributes WorkflowExecution
cweseaWorkflowExecution = lens _cweseaWorkflowExecution (\ s a -> s{_cweseaWorkflowExecution = a});

-- | The type of the child workflow execution.
cweseaWorkflowType :: Lens' ChildWorkflowExecutionStartedEventAttributes WorkflowType
cweseaWorkflowType = lens _cweseaWorkflowType (\ s a -> s{_cweseaWorkflowType = a});

-- | The id of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
cweseaInitiatedEventId :: Lens' ChildWorkflowExecutionStartedEventAttributes Integer
cweseaInitiatedEventId = lens _cweseaInitiatedEventId (\ s a -> s{_cweseaInitiatedEventId = a});

instance FromJSON
         ChildWorkflowExecutionStartedEventAttributes where
        parseJSON
          = withObject
              "ChildWorkflowExecutionStartedEventAttributes"
              (\ x ->
                 ChildWorkflowExecutionStartedEventAttributes' <$>
                   (x .: "workflowExecution") <*> (x .: "workflowType")
                     <*> (x .: "initiatedEventId"))

-- | Provides details of the @ChildWorkflowExecutionTerminated@ event.
--
-- /See:/ 'childWorkflowExecutionTerminatedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cweteaWorkflowExecution'
--
-- * 'cweteaWorkflowType'
--
-- * 'cweteaInitiatedEventId'
--
-- * 'cweteaStartedEventId'
data ChildWorkflowExecutionTerminatedEventAttributes = ChildWorkflowExecutionTerminatedEventAttributes'
    { _cweteaWorkflowExecution :: WorkflowExecution
    , _cweteaWorkflowType      :: WorkflowType
    , _cweteaInitiatedEventId  :: !Integer
    , _cweteaStartedEventId    :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ChildWorkflowExecutionTerminatedEventAttributes' smart constructor.
childWorkflowExecutionTerminatedEventAttributes :: WorkflowExecution -> WorkflowType -> Integer -> Integer -> ChildWorkflowExecutionTerminatedEventAttributes
childWorkflowExecutionTerminatedEventAttributes pWorkflowExecution pWorkflowType pInitiatedEventId pStartedEventId =
    ChildWorkflowExecutionTerminatedEventAttributes'
    { _cweteaWorkflowExecution = pWorkflowExecution
    , _cweteaWorkflowType = pWorkflowType
    , _cweteaInitiatedEventId = pInitiatedEventId
    , _cweteaStartedEventId = pStartedEventId
    }

-- | The child workflow execution that was terminated.
cweteaWorkflowExecution :: Lens' ChildWorkflowExecutionTerminatedEventAttributes WorkflowExecution
cweteaWorkflowExecution = lens _cweteaWorkflowExecution (\ s a -> s{_cweteaWorkflowExecution = a});

-- | The type of the child workflow execution.
cweteaWorkflowType :: Lens' ChildWorkflowExecutionTerminatedEventAttributes WorkflowType
cweteaWorkflowType = lens _cweteaWorkflowType (\ s a -> s{_cweteaWorkflowType = a});

-- | The id of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
cweteaInitiatedEventId :: Lens' ChildWorkflowExecutionTerminatedEventAttributes Integer
cweteaInitiatedEventId = lens _cweteaInitiatedEventId (\ s a -> s{_cweteaInitiatedEventId = a});

-- | The Id of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
cweteaStartedEventId :: Lens' ChildWorkflowExecutionTerminatedEventAttributes Integer
cweteaStartedEventId = lens _cweteaStartedEventId (\ s a -> s{_cweteaStartedEventId = a});

instance FromJSON
         ChildWorkflowExecutionTerminatedEventAttributes where
        parseJSON
          = withObject
              "ChildWorkflowExecutionTerminatedEventAttributes"
              (\ x ->
                 ChildWorkflowExecutionTerminatedEventAttributes' <$>
                   (x .: "workflowExecution") <*> (x .: "workflowType")
                     <*> (x .: "initiatedEventId")
                     <*> (x .: "startedEventId"))

-- | Provides details of the @ChildWorkflowExecutionTimedOut@ event.
--
-- /See:/ 'childWorkflowExecutionTimedOutEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwetoeaWorkflowExecution'
--
-- * 'cwetoeaWorkflowType'
--
-- * 'cwetoeaTimeoutType'
--
-- * 'cwetoeaInitiatedEventId'
--
-- * 'cwetoeaStartedEventId'
data ChildWorkflowExecutionTimedOutEventAttributes = ChildWorkflowExecutionTimedOutEventAttributes'
    { _cwetoeaWorkflowExecution :: WorkflowExecution
    , _cwetoeaWorkflowType      :: WorkflowType
    , _cwetoeaTimeoutType       :: WorkflowExecutionTimeoutType
    , _cwetoeaInitiatedEventId  :: !Integer
    , _cwetoeaStartedEventId    :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ChildWorkflowExecutionTimedOutEventAttributes' smart constructor.
childWorkflowExecutionTimedOutEventAttributes :: WorkflowExecution -> WorkflowType -> WorkflowExecutionTimeoutType -> Integer -> Integer -> ChildWorkflowExecutionTimedOutEventAttributes
childWorkflowExecutionTimedOutEventAttributes pWorkflowExecution pWorkflowType pTimeoutType pInitiatedEventId pStartedEventId =
    ChildWorkflowExecutionTimedOutEventAttributes'
    { _cwetoeaWorkflowExecution = pWorkflowExecution
    , _cwetoeaWorkflowType = pWorkflowType
    , _cwetoeaTimeoutType = pTimeoutType
    , _cwetoeaInitiatedEventId = pInitiatedEventId
    , _cwetoeaStartedEventId = pStartedEventId
    }

-- | The child workflow execution that timed out.
cwetoeaWorkflowExecution :: Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowExecution
cwetoeaWorkflowExecution = lens _cwetoeaWorkflowExecution (\ s a -> s{_cwetoeaWorkflowExecution = a});

-- | The type of the child workflow execution.
cwetoeaWorkflowType :: Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowType
cwetoeaWorkflowType = lens _cwetoeaWorkflowType (\ s a -> s{_cwetoeaWorkflowType = a});

-- | The type of the timeout that caused the child workflow execution to time
-- out.
cwetoeaTimeoutType :: Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowExecutionTimeoutType
cwetoeaTimeoutType = lens _cwetoeaTimeoutType (\ s a -> s{_cwetoeaTimeoutType = a});

-- | The id of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
cwetoeaInitiatedEventId :: Lens' ChildWorkflowExecutionTimedOutEventAttributes Integer
cwetoeaInitiatedEventId = lens _cwetoeaInitiatedEventId (\ s a -> s{_cwetoeaInitiatedEventId = a});

-- | The Id of the @ChildWorkflowExecutionStarted@ event recorded when this
-- child workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
cwetoeaStartedEventId :: Lens' ChildWorkflowExecutionTimedOutEventAttributes Integer
cwetoeaStartedEventId = lens _cwetoeaStartedEventId (\ s a -> s{_cwetoeaStartedEventId = a});

instance FromJSON
         ChildWorkflowExecutionTimedOutEventAttributes where
        parseJSON
          = withObject
              "ChildWorkflowExecutionTimedOutEventAttributes"
              (\ x ->
                 ChildWorkflowExecutionTimedOutEventAttributes' <$>
                   (x .: "workflowExecution") <*> (x .: "workflowType")
                     <*> (x .: "timeoutType")
                     <*> (x .: "initiatedEventId")
                     <*> (x .: "startedEventId"))

-- | Used to filter the closed workflow executions in visibility APIs by
-- their close status.
--
-- /See:/ 'closeStatusFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfStatus'
newtype CloseStatusFilter = CloseStatusFilter'
    { _csfStatus :: CloseStatus
    } deriving (Eq,Read,Show)

-- | 'CloseStatusFilter' smart constructor.
closeStatusFilter :: CloseStatus -> CloseStatusFilter
closeStatusFilter pStatus =
    CloseStatusFilter'
    { _csfStatus = pStatus
    }

-- | __Required.__ The close status that must match the close status of an
-- execution for it to meet the criteria of this filter.
csfStatus :: Lens' CloseStatusFilter CloseStatus
csfStatus = lens _csfStatus (\ s a -> s{_csfStatus = a});

instance ToJSON CloseStatusFilter where
        toJSON CloseStatusFilter'{..}
          = object ["status" .= _csfStatus]

-- | Provides details of the @CompleteWorkflowExecution@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- /See:/ 'completeWorkflowExecutionDecisionAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwedaResult'
newtype CompleteWorkflowExecutionDecisionAttributes = CompleteWorkflowExecutionDecisionAttributes'
    { _cwedaResult :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'CompleteWorkflowExecutionDecisionAttributes' smart constructor.
completeWorkflowExecutionDecisionAttributes :: CompleteWorkflowExecutionDecisionAttributes
completeWorkflowExecutionDecisionAttributes =
    CompleteWorkflowExecutionDecisionAttributes'
    { _cwedaResult = Nothing
    }

-- | The result of the workflow execution. The form of the result is
-- implementation defined.
cwedaResult :: Lens' CompleteWorkflowExecutionDecisionAttributes (Maybe Text)
cwedaResult = lens _cwedaResult (\ s a -> s{_cwedaResult = a});

instance ToJSON
         CompleteWorkflowExecutionDecisionAttributes where
        toJSON
          CompleteWorkflowExecutionDecisionAttributes'{..}
          = object ["result" .= _cwedaResult]

-- | Provides details of the @CompleteWorkflowExecutionFailed@ event.
--
-- /See:/ 'completeWorkflowExecutionFailedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwefeaCause'
--
-- * 'cwefeaDecisionTaskCompletedEventId'
data CompleteWorkflowExecutionFailedEventAttributes = CompleteWorkflowExecutionFailedEventAttributes'
    { _cwefeaCause                        :: CompleteWorkflowExecutionFailedCause
    , _cwefeaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'CompleteWorkflowExecutionFailedEventAttributes' smart constructor.
completeWorkflowExecutionFailedEventAttributes :: CompleteWorkflowExecutionFailedCause -> Integer -> CompleteWorkflowExecutionFailedEventAttributes
completeWorkflowExecutionFailedEventAttributes pCause pDecisionTaskCompletedEventId =
    CompleteWorkflowExecutionFailedEventAttributes'
    { _cwefeaCause = pCause
    , _cwefeaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | The cause of the failure. This information is generated by the system
-- and can be useful for diagnostic purposes.
--
-- If __cause__ is set to OPERATION_NOT_PERMITTED, the decision failed
-- because it lacked sufficient permissions. For details and example IAM
-- policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
cwefeaCause :: Lens' CompleteWorkflowExecutionFailedEventAttributes CompleteWorkflowExecutionFailedCause
cwefeaCause = lens _cwefeaCause (\ s a -> s{_cwefeaCause = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @CompleteWorkflowExecution@ decision
-- to complete this execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
cwefeaDecisionTaskCompletedEventId :: Lens' CompleteWorkflowExecutionFailedEventAttributes Integer
cwefeaDecisionTaskCompletedEventId = lens _cwefeaDecisionTaskCompletedEventId (\ s a -> s{_cwefeaDecisionTaskCompletedEventId = a});

instance FromJSON
         CompleteWorkflowExecutionFailedEventAttributes where
        parseJSON
          = withObject
              "CompleteWorkflowExecutionFailedEventAttributes"
              (\ x ->
                 CompleteWorkflowExecutionFailedEventAttributes' <$>
                   (x .: "cause") <*>
                     (x .: "decisionTaskCompletedEventId"))

-- | Provides details of the @ContinueAsNewWorkflowExecution@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--     -   @tag@: /Optional./. A tag used to identify the workflow
--         execution
--     -   @taskList@: String constraint. The key is @swf:taskList.name@.
--     -   @workflowType.version@: String constraint. The key is
--         @swf:workflowType.version@.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- /See:/ 'continueAsNewWorkflowExecutionDecisionAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'canwedaTagList'
--
-- * 'canwedaTaskStartToCloseTimeout'
--
-- * 'canwedaInput'
--
-- * 'canwedaWorkflowTypeVersion'
--
-- * 'canwedaExecutionStartToCloseTimeout'
--
-- * 'canwedaTaskList'
--
-- * 'canwedaTaskPriority'
--
-- * 'canwedaChildPolicy'
data ContinueAsNewWorkflowExecutionDecisionAttributes = ContinueAsNewWorkflowExecutionDecisionAttributes'
    { _canwedaTagList                      :: Maybe [Text]
    , _canwedaTaskStartToCloseTimeout      :: Maybe Text
    , _canwedaInput                        :: Maybe Text
    , _canwedaWorkflowTypeVersion          :: Maybe Text
    , _canwedaExecutionStartToCloseTimeout :: Maybe Text
    , _canwedaTaskList                     :: Maybe TaskList
    , _canwedaTaskPriority                 :: Maybe Text
    , _canwedaChildPolicy                  :: Maybe ChildPolicy
    } deriving (Eq,Read,Show)

-- | 'ContinueAsNewWorkflowExecutionDecisionAttributes' smart constructor.
continueAsNewWorkflowExecutionDecisionAttributes :: ContinueAsNewWorkflowExecutionDecisionAttributes
continueAsNewWorkflowExecutionDecisionAttributes =
    ContinueAsNewWorkflowExecutionDecisionAttributes'
    { _canwedaTagList = Nothing
    , _canwedaTaskStartToCloseTimeout = Nothing
    , _canwedaInput = Nothing
    , _canwedaWorkflowTypeVersion = Nothing
    , _canwedaExecutionStartToCloseTimeout = Nothing
    , _canwedaTaskList = Nothing
    , _canwedaTaskPriority = Nothing
    , _canwedaChildPolicy = Nothing
    }

-- | The list of tags to associate with the new workflow execution. A maximum
-- of 5 tags can be specified. You can list workflow executions with a
-- specific tag by calling ListOpenWorkflowExecutions or
-- ListClosedWorkflowExecutions and specifying a TagFilter.
canwedaTagList :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes [Text]
canwedaTagList = lens _canwedaTagList (\ s a -> s{_canwedaTagList = a}) . _Default;

-- | Specifies the maximum duration of decision tasks for the new workflow
-- execution. This parameter overrides the @defaultTaskStartToCloseTimout@
-- specified when registering the workflow type using RegisterWorkflowType.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
--
-- A task start-to-close timeout for the new workflow execution must be
-- specified either as a default for the workflow type or through this
-- parameter. If neither this parameter is set nor a default task
-- start-to-close timeout was specified at registration time then a fault
-- will be returned.
canwedaTaskStartToCloseTimeout :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaTaskStartToCloseTimeout = lens _canwedaTaskStartToCloseTimeout (\ s a -> s{_canwedaTaskStartToCloseTimeout = a});

-- | The input provided to the new workflow execution.
canwedaInput :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaInput = lens _canwedaInput (\ s a -> s{_canwedaInput = a});

-- | FIXME: Undocumented member.
canwedaWorkflowTypeVersion :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaWorkflowTypeVersion = lens _canwedaWorkflowTypeVersion (\ s a -> s{_canwedaWorkflowTypeVersion = a});

-- | If set, specifies the total duration for this workflow execution. This
-- overrides the @defaultExecutionStartToCloseTimeout@ specified when
-- registering the workflow type.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
--
-- An execution start-to-close timeout for this workflow execution must be
-- specified either as a default for the workflow type or through this
-- field. If neither this field is set nor a default execution
-- start-to-close timeout was specified at registration time then a fault
-- will be returned.
canwedaExecutionStartToCloseTimeout :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaExecutionStartToCloseTimeout = lens _canwedaExecutionStartToCloseTimeout (\ s a -> s{_canwedaExecutionStartToCloseTimeout = a});

-- | FIXME: Undocumented member.
canwedaTaskList :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe TaskList)
canwedaTaskList = lens _canwedaTaskList (\ s a -> s{_canwedaTaskList = a});

-- | /Optional./ The task priority that, if set, specifies the priority for
-- the decision tasks for this workflow execution. This overrides the
-- defaultTaskPriority specified when registering the workflow type. Valid
-- values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon Simple Workflow Developer Guide/.
canwedaTaskPriority :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaTaskPriority = lens _canwedaTaskPriority (\ s a -> s{_canwedaTaskPriority = a});

-- | If set, specifies the policy to use for the child workflow executions of
-- the new execution if it is terminated by calling the
-- TerminateWorkflowExecution action explicitly or due to an expired
-- timeout. This policy overrides the default child policy specified when
-- registering the workflow type using RegisterWorkflowType.
--
-- The supported child policies are:
--
-- -   __TERMINATE:__ the child executions will be terminated.
-- -   __REQUEST_CANCEL:__ a request to cancel will be attempted for each
--     child execution by recording a @WorkflowExecutionCancelRequested@
--     event in its history. It is up to the decider to take appropriate
--     actions when it receives an execution history with this event.
-- -   __ABANDON:__ no action will be taken. The child executions will
--     continue to run.
--
-- A child policy for this workflow execution must be specified either as a
-- default for the workflow type or through this parameter. If neither this
-- parameter is set nor a default child policy was specified at
-- registration time then a fault will be returned.
canwedaChildPolicy :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe ChildPolicy)
canwedaChildPolicy = lens _canwedaChildPolicy (\ s a -> s{_canwedaChildPolicy = a});

instance ToJSON
         ContinueAsNewWorkflowExecutionDecisionAttributes
         where
        toJSON
          ContinueAsNewWorkflowExecutionDecisionAttributes'{..}
          = object
              ["tagList" .= _canwedaTagList,
               "taskStartToCloseTimeout" .=
                 _canwedaTaskStartToCloseTimeout,
               "input" .= _canwedaInput,
               "workflowTypeVersion" .= _canwedaWorkflowTypeVersion,
               "executionStartToCloseTimeout" .=
                 _canwedaExecutionStartToCloseTimeout,
               "taskList" .= _canwedaTaskList,
               "taskPriority" .= _canwedaTaskPriority,
               "childPolicy" .= _canwedaChildPolicy]

-- | Provides details of the @ContinueAsNewWorkflowExecutionFailed@ event.
--
-- /See:/ 'continueAsNewWorkflowExecutionFailedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'canwefeaCause'
--
-- * 'canwefeaDecisionTaskCompletedEventId'
data ContinueAsNewWorkflowExecutionFailedEventAttributes = ContinueAsNewWorkflowExecutionFailedEventAttributes'
    { _canwefeaCause                        :: ContinueAsNewWorkflowExecutionFailedCause
    , _canwefeaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ContinueAsNewWorkflowExecutionFailedEventAttributes' smart constructor.
continueAsNewWorkflowExecutionFailedEventAttributes :: ContinueAsNewWorkflowExecutionFailedCause -> Integer -> ContinueAsNewWorkflowExecutionFailedEventAttributes
continueAsNewWorkflowExecutionFailedEventAttributes pCause pDecisionTaskCompletedEventId =
    ContinueAsNewWorkflowExecutionFailedEventAttributes'
    { _canwefeaCause = pCause
    , _canwefeaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | The cause of the failure. This information is generated by the system
-- and can be useful for diagnostic purposes.
--
-- If __cause__ is set to OPERATION_NOT_PERMITTED, the decision failed
-- because it lacked sufficient permissions. For details and example IAM
-- policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
canwefeaCause :: Lens' ContinueAsNewWorkflowExecutionFailedEventAttributes ContinueAsNewWorkflowExecutionFailedCause
canwefeaCause = lens _canwefeaCause (\ s a -> s{_canwefeaCause = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @ContinueAsNewWorkflowExecution@
-- decision that started this execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
canwefeaDecisionTaskCompletedEventId :: Lens' ContinueAsNewWorkflowExecutionFailedEventAttributes Integer
canwefeaDecisionTaskCompletedEventId = lens _canwefeaDecisionTaskCompletedEventId (\ s a -> s{_canwefeaDecisionTaskCompletedEventId = a});

instance FromJSON
         ContinueAsNewWorkflowExecutionFailedEventAttributes
         where
        parseJSON
          = withObject
              "ContinueAsNewWorkflowExecutionFailedEventAttributes"
              (\ x ->
                 ContinueAsNewWorkflowExecutionFailedEventAttributes'
                   <$>
                   (x .: "cause") <*>
                     (x .: "decisionTaskCompletedEventId"))

-- | Specifies a decision made by the decider. A decision can be one of these
-- types:
--
-- -   __CancelTimer__: cancels a previously started timer and records a
--     @TimerCanceled@ event in the history.
-- -   __CancelWorkflowExecution__: closes the workflow execution and
--     records a @WorkflowExecutionCanceled@ event in the history.
-- -   __CompleteWorkflowExecution__: closes the workflow execution and
--     records a @WorkflowExecutionCompleted@ event in the history .
-- -   __ContinueAsNewWorkflowExecution__: closes the workflow execution
--     and starts a new workflow execution of the same type using the same
--     workflow id and a unique run Id. A @WorkflowExecutionContinuedAsNew@
--     event is recorded in the history.
-- -   __FailWorkflowExecution__: closes the workflow execution and records
--     a @WorkflowExecutionFailed@ event in the history.
-- -   __RecordMarker__: records a @MarkerRecorded@ event in the history.
--     Markers can be used for adding custom information in the history for
--     instance to let deciders know that they do not need to look at the
--     history beyond the marker event.
-- -   __RequestCancelActivityTask__: attempts to cancel a previously
--     scheduled activity task. If the activity task was scheduled but has
--     not been assigned to a worker, then it will be canceled. If the
--     activity task was already assigned to a worker, then the worker will
--     be informed that cancellation has been requested in the response to
--     RecordActivityTaskHeartbeat.
-- -   __RequestCancelExternalWorkflowExecution__: requests that a request
--     be made to cancel the specified external workflow execution and
--     records a @RequestCancelExternalWorkflowExecutionInitiated@ event in
--     the history.
-- -   __ScheduleActivityTask__: schedules an activity task.
-- -   __SignalExternalWorkflowExecution__: requests a signal to be
--     delivered to the specified external workflow execution and records a
--     @SignalExternalWorkflowExecutionInitiated@ event in the history.
-- -   __StartChildWorkflowExecution__: requests that a child workflow
--     execution be started and records a
--     @StartChildWorkflowExecutionInitiated@ event in the history. The
--     child workflow execution is a separate workflow execution with its
--     own history.
-- -   __StartTimer__: starts a timer for this workflow execution and
--     records a @TimerStarted@ event in the history. This timer will fire
--     after the specified delay and record a @TimerFired@ event.
--
-- __Access Control__
--
-- If you grant permission to use @RespondDecisionTaskCompleted@, you can
-- use IAM policies to express permissions for the list of decisions
-- returned by this action as if they were members of the API. Treating
-- decisions as a pseudo API maintains a uniform conceptual model and helps
-- keep policies readable. For details and example IAM policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- __Decision Failure__
--
-- Decisions can fail for several reasons
--
-- -   The ordering of decisions should follow a logical flow. Some
--     decisions might not make sense in the current context of the
--     workflow execution and will therefore fail.
-- -   A limit on your account was reached.
-- -   The decision lacks sufficient permissions.
--
-- One of the following events might be added to the history to indicate an
-- error. The event attribute\'s __cause__ parameter indicates the cause.
-- If __cause__ is set to OPERATION_NOT_PERMITTED, the decision failed
-- because it lacked sufficient permissions. For details and example IAM
-- policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- -   __ScheduleActivityTaskFailed__: a ScheduleActivityTask decision
--     failed. This could happen if the activity type specified in the
--     decision is not registered, is in a deprecated state, or the
--     decision is not properly configured.
-- -   __RequestCancelActivityTaskFailed__: a RequestCancelActivityTask
--     decision failed. This could happen if there is no open activity task
--     with the specified activityId.
-- -   __StartTimerFailed__: a StartTimer decision failed. This could
--     happen if there is another open timer with the same timerId.
-- -   __CancelTimerFailed__: a CancelTimer decision failed. This could
--     happen if there is no open timer with the specified timerId.
-- -   __StartChildWorkflowExecutionFailed__: a StartChildWorkflowExecution
--     decision failed. This could happen if the workflow type specified is
--     not registered, is deprecated, or the decision is not properly
--     configured.
-- -   __SignalExternalWorkflowExecutionFailed__: a
--     SignalExternalWorkflowExecution decision failed. This could happen
--     if the @workflowID@ specified in the decision was incorrect.
-- -   __RequestCancelExternalWorkflowExecutionFailed__: a
--     RequestCancelExternalWorkflowExecution decision failed. This could
--     happen if the @workflowID@ specified in the decision was incorrect.
-- -   __CancelWorkflowExecutionFailed__: a CancelWorkflowExecution
--     decision failed. This could happen if there is an unhandled decision
--     task pending in the workflow execution.
-- -   __CompleteWorkflowExecutionFailed__: a CompleteWorkflowExecution
--     decision failed. This could happen if there is an unhandled decision
--     task pending in the workflow execution.
-- -   __ContinueAsNewWorkflowExecutionFailed__: a
--     ContinueAsNewWorkflowExecution decision failed. This could happen if
--     there is an unhandled decision task pending in the workflow
--     execution or the ContinueAsNewWorkflowExecution decision was not
--     configured correctly.
-- -   __FailWorkflowExecutionFailed__: a FailWorkflowExecution decision
--     failed. This could happen if there is an unhandled decision task
--     pending in the workflow execution.
--
-- The preceding error events might occur due to an error in the decider
-- logic, which might put the workflow execution in an unstable state The
-- cause field in the event structure for the error event indicates the
-- cause of the error.
--
-- A workflow execution may be closed by the decider by returning one of
-- the following decisions when completing a decision task:
-- @CompleteWorkflowExecution@, @FailWorkflowExecution@,
-- @CancelWorkflowExecution@ and @ContinueAsNewWorkflowExecution@. An
-- UnhandledDecision fault will be returned if a workflow closing decision
-- is specified and a signal or activity event had been added to the
-- history while the decision task was being performed by the decider.
-- Unlike the above situations which are logic issues, this fault is always
-- possible because of race conditions in a distributed system. The right
-- action here is to call RespondDecisionTaskCompleted without any
-- decisions. This would result in another decision task with these new
-- events included in the history. The decider should handle the new events
-- and may decide to close the workflow execution.
--
-- __How to Code a Decision__
--
-- You code a decision by first setting the decision type field to one of
-- the above decision values, and then set the corresponding attributes
-- field shown below:
--
-- -   ScheduleActivityTaskDecisionAttributes
-- -   RequestCancelActivityTaskDecisionAttributes
-- -   CompleteWorkflowExecutionDecisionAttributes
-- -   FailWorkflowExecutionDecisionAttributes
-- -   CancelWorkflowExecutionDecisionAttributes
-- -   ContinueAsNewWorkflowExecutionDecisionAttributes
-- -   RecordMarkerDecisionAttributes
-- -   StartTimerDecisionAttributes
-- -   CancelTimerDecisionAttributes
-- -   SignalExternalWorkflowExecutionDecisionAttributes
-- -   RequestCancelExternalWorkflowExecutionDecisionAttributes
-- -   StartChildWorkflowExecutionDecisionAttributes
--
-- /See:/ 'decision' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'decRequestCancelExternalWorkflowExecutionDecisionAttributes'
--
-- * 'decScheduleActivityTaskDecisionAttributes'
--
-- * 'decSignalExternalWorkflowExecutionDecisionAttributes'
--
-- * 'decStartTimerDecisionAttributes'
--
-- * 'decRecordMarkerDecisionAttributes'
--
-- * 'decFailWorkflowExecutionDecisionAttributes'
--
-- * 'decStartChildWorkflowExecutionDecisionAttributes'
--
-- * 'decCompleteWorkflowExecutionDecisionAttributes'
--
-- * 'decRequestCancelActivityTaskDecisionAttributes'
--
-- * 'decCancelWorkflowExecutionDecisionAttributes'
--
-- * 'decCancelTimerDecisionAttributes'
--
-- * 'decContinueAsNewWorkflowExecutionDecisionAttributes'
--
-- * 'decDecisionType'
data Decision = Decision'
    { _decRequestCancelExternalWorkflowExecutionDecisionAttributes :: Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes
    , _decScheduleActivityTaskDecisionAttributes                   :: Maybe ScheduleActivityTaskDecisionAttributes
    , _decSignalExternalWorkflowExecutionDecisionAttributes        :: Maybe SignalExternalWorkflowExecutionDecisionAttributes
    , _decStartTimerDecisionAttributes                             :: Maybe StartTimerDecisionAttributes
    , _decRecordMarkerDecisionAttributes                           :: Maybe RecordMarkerDecisionAttributes
    , _decFailWorkflowExecutionDecisionAttributes                  :: Maybe FailWorkflowExecutionDecisionAttributes
    , _decStartChildWorkflowExecutionDecisionAttributes            :: Maybe StartChildWorkflowExecutionDecisionAttributes
    , _decCompleteWorkflowExecutionDecisionAttributes              :: Maybe CompleteWorkflowExecutionDecisionAttributes
    , _decRequestCancelActivityTaskDecisionAttributes              :: Maybe RequestCancelActivityTaskDecisionAttributes
    , _decCancelWorkflowExecutionDecisionAttributes                :: Maybe CancelWorkflowExecutionDecisionAttributes
    , _decCancelTimerDecisionAttributes                            :: Maybe CancelTimerDecisionAttributes
    , _decContinueAsNewWorkflowExecutionDecisionAttributes         :: Maybe ContinueAsNewWorkflowExecutionDecisionAttributes
    , _decDecisionType                                             :: DecisionType
    } deriving (Eq,Read,Show)

-- | 'Decision' smart constructor.
decision :: DecisionType -> Decision
decision pDecisionType =
    Decision'
    { _decRequestCancelExternalWorkflowExecutionDecisionAttributes = Nothing
    , _decScheduleActivityTaskDecisionAttributes = Nothing
    , _decSignalExternalWorkflowExecutionDecisionAttributes = Nothing
    , _decStartTimerDecisionAttributes = Nothing
    , _decRecordMarkerDecisionAttributes = Nothing
    , _decFailWorkflowExecutionDecisionAttributes = Nothing
    , _decStartChildWorkflowExecutionDecisionAttributes = Nothing
    , _decCompleteWorkflowExecutionDecisionAttributes = Nothing
    , _decRequestCancelActivityTaskDecisionAttributes = Nothing
    , _decCancelWorkflowExecutionDecisionAttributes = Nothing
    , _decCancelTimerDecisionAttributes = Nothing
    , _decContinueAsNewWorkflowExecutionDecisionAttributes = Nothing
    , _decDecisionType = pDecisionType
    }

-- | Provides details of the @RequestCancelExternalWorkflowExecution@
-- decision. It is not set for other decision types.
decRequestCancelExternalWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes)
decRequestCancelExternalWorkflowExecutionDecisionAttributes = lens _decRequestCancelExternalWorkflowExecutionDecisionAttributes (\ s a -> s{_decRequestCancelExternalWorkflowExecutionDecisionAttributes = a});

-- | Provides details of the @ScheduleActivityTask@ decision. It is not set
-- for other decision types.
decScheduleActivityTaskDecisionAttributes :: Lens' Decision (Maybe ScheduleActivityTaskDecisionAttributes)
decScheduleActivityTaskDecisionAttributes = lens _decScheduleActivityTaskDecisionAttributes (\ s a -> s{_decScheduleActivityTaskDecisionAttributes = a});

-- | Provides details of the @SignalExternalWorkflowExecution@ decision. It
-- is not set for other decision types.
decSignalExternalWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe SignalExternalWorkflowExecutionDecisionAttributes)
decSignalExternalWorkflowExecutionDecisionAttributes = lens _decSignalExternalWorkflowExecutionDecisionAttributes (\ s a -> s{_decSignalExternalWorkflowExecutionDecisionAttributes = a});

-- | Provides details of the @StartTimer@ decision. It is not set for other
-- decision types.
decStartTimerDecisionAttributes :: Lens' Decision (Maybe StartTimerDecisionAttributes)
decStartTimerDecisionAttributes = lens _decStartTimerDecisionAttributes (\ s a -> s{_decStartTimerDecisionAttributes = a});

-- | Provides details of the @RecordMarker@ decision. It is not set for other
-- decision types.
decRecordMarkerDecisionAttributes :: Lens' Decision (Maybe RecordMarkerDecisionAttributes)
decRecordMarkerDecisionAttributes = lens _decRecordMarkerDecisionAttributes (\ s a -> s{_decRecordMarkerDecisionAttributes = a});

-- | Provides details of the @FailWorkflowExecution@ decision. It is not set
-- for other decision types.
decFailWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe FailWorkflowExecutionDecisionAttributes)
decFailWorkflowExecutionDecisionAttributes = lens _decFailWorkflowExecutionDecisionAttributes (\ s a -> s{_decFailWorkflowExecutionDecisionAttributes = a});

-- | Provides details of the @StartChildWorkflowExecution@ decision. It is
-- not set for other decision types.
decStartChildWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe StartChildWorkflowExecutionDecisionAttributes)
decStartChildWorkflowExecutionDecisionAttributes = lens _decStartChildWorkflowExecutionDecisionAttributes (\ s a -> s{_decStartChildWorkflowExecutionDecisionAttributes = a});

-- | Provides details of the @CompleteWorkflowExecution@ decision. It is not
-- set for other decision types.
decCompleteWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe CompleteWorkflowExecutionDecisionAttributes)
decCompleteWorkflowExecutionDecisionAttributes = lens _decCompleteWorkflowExecutionDecisionAttributes (\ s a -> s{_decCompleteWorkflowExecutionDecisionAttributes = a});

-- | Provides details of the @RequestCancelActivityTask@ decision. It is not
-- set for other decision types.
decRequestCancelActivityTaskDecisionAttributes :: Lens' Decision (Maybe RequestCancelActivityTaskDecisionAttributes)
decRequestCancelActivityTaskDecisionAttributes = lens _decRequestCancelActivityTaskDecisionAttributes (\ s a -> s{_decRequestCancelActivityTaskDecisionAttributes = a});

-- | Provides details of the @CancelWorkflowExecution@ decision. It is not
-- set for other decision types.
decCancelWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe CancelWorkflowExecutionDecisionAttributes)
decCancelWorkflowExecutionDecisionAttributes = lens _decCancelWorkflowExecutionDecisionAttributes (\ s a -> s{_decCancelWorkflowExecutionDecisionAttributes = a});

-- | Provides details of the @CancelTimer@ decision. It is not set for other
-- decision types.
decCancelTimerDecisionAttributes :: Lens' Decision (Maybe CancelTimerDecisionAttributes)
decCancelTimerDecisionAttributes = lens _decCancelTimerDecisionAttributes (\ s a -> s{_decCancelTimerDecisionAttributes = a});

-- | Provides details of the @ContinueAsNewWorkflowExecution@ decision. It is
-- not set for other decision types.
decContinueAsNewWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe ContinueAsNewWorkflowExecutionDecisionAttributes)
decContinueAsNewWorkflowExecutionDecisionAttributes = lens _decContinueAsNewWorkflowExecutionDecisionAttributes (\ s a -> s{_decContinueAsNewWorkflowExecutionDecisionAttributes = a});

-- | Specifies the type of the decision.
decDecisionType :: Lens' Decision DecisionType
decDecisionType = lens _decDecisionType (\ s a -> s{_decDecisionType = a});

instance ToJSON Decision where
        toJSON Decision'{..}
          = object
              ["requestCancelExternalWorkflowExecutionDecisionAttributes"
                 .=
                 _decRequestCancelExternalWorkflowExecutionDecisionAttributes,
               "scheduleActivityTaskDecisionAttributes" .=
                 _decScheduleActivityTaskDecisionAttributes,
               "signalExternalWorkflowExecutionDecisionAttributes"
                 .=
                 _decSignalExternalWorkflowExecutionDecisionAttributes,
               "startTimerDecisionAttributes" .=
                 _decStartTimerDecisionAttributes,
               "recordMarkerDecisionAttributes" .=
                 _decRecordMarkerDecisionAttributes,
               "failWorkflowExecutionDecisionAttributes" .=
                 _decFailWorkflowExecutionDecisionAttributes,
               "startChildWorkflowExecutionDecisionAttributes" .=
                 _decStartChildWorkflowExecutionDecisionAttributes,
               "completeWorkflowExecutionDecisionAttributes" .=
                 _decCompleteWorkflowExecutionDecisionAttributes,
               "requestCancelActivityTaskDecisionAttributes" .=
                 _decRequestCancelActivityTaskDecisionAttributes,
               "cancelWorkflowExecutionDecisionAttributes" .=
                 _decCancelWorkflowExecutionDecisionAttributes,
               "cancelTimerDecisionAttributes" .=
                 _decCancelTimerDecisionAttributes,
               "continueAsNewWorkflowExecutionDecisionAttributes" .=
                 _decContinueAsNewWorkflowExecutionDecisionAttributes,
               "decisionType" .= _decDecisionType]

-- | Provides details of the @DecisionTaskCompleted@ event.
--
-- /See:/ 'decisionTaskCompletedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtceaExecutionContext'
--
-- * 'dtceaScheduledEventId'
--
-- * 'dtceaStartedEventId'
data DecisionTaskCompletedEventAttributes = DecisionTaskCompletedEventAttributes'
    { _dtceaExecutionContext :: Maybe Text
    , _dtceaScheduledEventId :: !Integer
    , _dtceaStartedEventId   :: !Integer
    } deriving (Eq,Read,Show)

-- | 'DecisionTaskCompletedEventAttributes' smart constructor.
decisionTaskCompletedEventAttributes :: Integer -> Integer -> DecisionTaskCompletedEventAttributes
decisionTaskCompletedEventAttributes pScheduledEventId pStartedEventId =
    DecisionTaskCompletedEventAttributes'
    { _dtceaExecutionContext = Nothing
    , _dtceaScheduledEventId = pScheduledEventId
    , _dtceaStartedEventId = pStartedEventId
    }

-- | User defined context for the workflow execution.
dtceaExecutionContext :: Lens' DecisionTaskCompletedEventAttributes (Maybe Text)
dtceaExecutionContext = lens _dtceaExecutionContext (\ s a -> s{_dtceaExecutionContext = a});

-- | The id of the @DecisionTaskScheduled@ event that was recorded when this
-- decision task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
dtceaScheduledEventId :: Lens' DecisionTaskCompletedEventAttributes Integer
dtceaScheduledEventId = lens _dtceaScheduledEventId (\ s a -> s{_dtceaScheduledEventId = a});

-- | The Id of the @DecisionTaskStarted@ event recorded when this decision
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
dtceaStartedEventId :: Lens' DecisionTaskCompletedEventAttributes Integer
dtceaStartedEventId = lens _dtceaStartedEventId (\ s a -> s{_dtceaStartedEventId = a});

instance FromJSON
         DecisionTaskCompletedEventAttributes where
        parseJSON
          = withObject "DecisionTaskCompletedEventAttributes"
              (\ x ->
                 DecisionTaskCompletedEventAttributes' <$>
                   (x .:? "executionContext") <*>
                     (x .: "scheduledEventId")
                     <*> (x .: "startedEventId"))

-- | Provides details about the @DecisionTaskScheduled@ event.
--
-- /See:/ 'decisionTaskScheduledEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtseaTaskPriority'
--
-- * 'dtseaStartToCloseTimeout'
--
-- * 'dtseaTaskList'
data DecisionTaskScheduledEventAttributes = DecisionTaskScheduledEventAttributes'
    { _dtseaTaskPriority        :: Maybe Text
    , _dtseaStartToCloseTimeout :: Maybe Text
    , _dtseaTaskList            :: TaskList
    } deriving (Eq,Read,Show)

-- | 'DecisionTaskScheduledEventAttributes' smart constructor.
decisionTaskScheduledEventAttributes :: TaskList -> DecisionTaskScheduledEventAttributes
decisionTaskScheduledEventAttributes pTaskList =
    DecisionTaskScheduledEventAttributes'
    { _dtseaTaskPriority = Nothing
    , _dtseaStartToCloseTimeout = Nothing
    , _dtseaTaskList = pTaskList
    }

-- | /Optional./ A task priority that, if set, specifies the priority for
-- this decision task. Valid values are integers that range from Java\'s
-- @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647).
-- Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon Simple Workflow Developer Guide/.
dtseaTaskPriority :: Lens' DecisionTaskScheduledEventAttributes (Maybe Text)
dtseaTaskPriority = lens _dtseaTaskPriority (\ s a -> s{_dtseaTaskPriority = a});

-- | The maximum duration for this decision task. The task is considered
-- timed out if it does not completed within this duration.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
dtseaStartToCloseTimeout :: Lens' DecisionTaskScheduledEventAttributes (Maybe Text)
dtseaStartToCloseTimeout = lens _dtseaStartToCloseTimeout (\ s a -> s{_dtseaStartToCloseTimeout = a});

-- | The name of the task list in which the decision task was scheduled.
dtseaTaskList :: Lens' DecisionTaskScheduledEventAttributes TaskList
dtseaTaskList = lens _dtseaTaskList (\ s a -> s{_dtseaTaskList = a});

instance FromJSON
         DecisionTaskScheduledEventAttributes where
        parseJSON
          = withObject "DecisionTaskScheduledEventAttributes"
              (\ x ->
                 DecisionTaskScheduledEventAttributes' <$>
                   (x .:? "taskPriority") <*>
                     (x .:? "startToCloseTimeout")
                     <*> (x .: "taskList"))

-- | Provides details of the @DecisionTaskStarted@ event.
--
-- /See:/ 'decisionTaskStartedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtseaIdentity'
--
-- * 'dtseaScheduledEventId'
data DecisionTaskStartedEventAttributes = DecisionTaskStartedEventAttributes'
    { _dtseaIdentity         :: Maybe Text
    , _dtseaScheduledEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'DecisionTaskStartedEventAttributes' smart constructor.
decisionTaskStartedEventAttributes :: Integer -> DecisionTaskStartedEventAttributes
decisionTaskStartedEventAttributes pScheduledEventId =
    DecisionTaskStartedEventAttributes'
    { _dtseaIdentity = Nothing
    , _dtseaScheduledEventId = pScheduledEventId
    }

-- | Identity of the decider making the request. This enables diagnostic
-- tracing when problems arise. The form of this identity is user defined.
dtseaIdentity :: Lens' DecisionTaskStartedEventAttributes (Maybe Text)
dtseaIdentity = lens _dtseaIdentity (\ s a -> s{_dtseaIdentity = a});

-- | The id of the @DecisionTaskScheduled@ event that was recorded when this
-- decision task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
dtseaScheduledEventId :: Lens' DecisionTaskStartedEventAttributes Integer
dtseaScheduledEventId = lens _dtseaScheduledEventId (\ s a -> s{_dtseaScheduledEventId = a});

instance FromJSON DecisionTaskStartedEventAttributes
         where
        parseJSON
          = withObject "DecisionTaskStartedEventAttributes"
              (\ x ->
                 DecisionTaskStartedEventAttributes' <$>
                   (x .:? "identity") <*> (x .: "scheduledEventId"))

-- | Provides details of the @DecisionTaskTimedOut@ event.
--
-- /See:/ 'decisionTaskTimedOutEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dttoeaTimeoutType'
--
-- * 'dttoeaScheduledEventId'
--
-- * 'dttoeaStartedEventId'
data DecisionTaskTimedOutEventAttributes = DecisionTaskTimedOutEventAttributes'
    { _dttoeaTimeoutType      :: DecisionTaskTimeoutType
    , _dttoeaScheduledEventId :: !Integer
    , _dttoeaStartedEventId   :: !Integer
    } deriving (Eq,Read,Show)

-- | 'DecisionTaskTimedOutEventAttributes' smart constructor.
decisionTaskTimedOutEventAttributes :: DecisionTaskTimeoutType -> Integer -> Integer -> DecisionTaskTimedOutEventAttributes
decisionTaskTimedOutEventAttributes pTimeoutType pScheduledEventId pStartedEventId =
    DecisionTaskTimedOutEventAttributes'
    { _dttoeaTimeoutType = pTimeoutType
    , _dttoeaScheduledEventId = pScheduledEventId
    , _dttoeaStartedEventId = pStartedEventId
    }

-- | The type of timeout that expired before the decision task could be
-- completed.
dttoeaTimeoutType :: Lens' DecisionTaskTimedOutEventAttributes DecisionTaskTimeoutType
dttoeaTimeoutType = lens _dttoeaTimeoutType (\ s a -> s{_dttoeaTimeoutType = a});

-- | The id of the @DecisionTaskScheduled@ event that was recorded when this
-- decision task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
dttoeaScheduledEventId :: Lens' DecisionTaskTimedOutEventAttributes Integer
dttoeaScheduledEventId = lens _dttoeaScheduledEventId (\ s a -> s{_dttoeaScheduledEventId = a});

-- | The Id of the @DecisionTaskStarted@ event recorded when this decision
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
dttoeaStartedEventId :: Lens' DecisionTaskTimedOutEventAttributes Integer
dttoeaStartedEventId = lens _dttoeaStartedEventId (\ s a -> s{_dttoeaStartedEventId = a});

instance FromJSON DecisionTaskTimedOutEventAttributes
         where
        parseJSON
          = withObject "DecisionTaskTimedOutEventAttributes"
              (\ x ->
                 DecisionTaskTimedOutEventAttributes' <$>
                   (x .: "timeoutType") <*> (x .: "scheduledEventId")
                     <*> (x .: "startedEventId"))

-- | Contains the configuration settings of a domain.
--
-- /See:/ 'domainConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcWorkflowExecutionRetentionPeriodInDays'
newtype DomainConfiguration = DomainConfiguration'
    { _dcWorkflowExecutionRetentionPeriodInDays :: Text
    } deriving (Eq,Read,Show)

-- | 'DomainConfiguration' smart constructor.
domainConfiguration :: Text -> DomainConfiguration
domainConfiguration pWorkflowExecutionRetentionPeriodInDays =
    DomainConfiguration'
    { _dcWorkflowExecutionRetentionPeriodInDays = pWorkflowExecutionRetentionPeriodInDays
    }

-- | The retention period for workflow executions in this domain.
dcWorkflowExecutionRetentionPeriodInDays :: Lens' DomainConfiguration Text
dcWorkflowExecutionRetentionPeriodInDays = lens _dcWorkflowExecutionRetentionPeriodInDays (\ s a -> s{_dcWorkflowExecutionRetentionPeriodInDays = a});

instance FromJSON DomainConfiguration where
        parseJSON
          = withObject "DomainConfiguration"
              (\ x ->
                 DomainConfiguration' <$>
                   (x .: "workflowExecutionRetentionPeriodInDays"))

-- | Contains general information about a domain.
--
-- /See:/ 'domainInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diDescription'
--
-- * 'diName'
--
-- * 'diStatus'
data DomainInfo = DomainInfo'
    { _diDescription :: Maybe Text
    , _diName        :: Text
    , _diStatus      :: RegistrationStatus
    } deriving (Eq,Read,Show)

-- | 'DomainInfo' smart constructor.
domainInfo :: Text -> RegistrationStatus -> DomainInfo
domainInfo pName pStatus =
    DomainInfo'
    { _diDescription = Nothing
    , _diName = pName
    , _diStatus = pStatus
    }

-- | The description of the domain provided through RegisterDomain.
diDescription :: Lens' DomainInfo (Maybe Text)
diDescription = lens _diDescription (\ s a -> s{_diDescription = a});

-- | The name of the domain. This name is unique within the account.
diName :: Lens' DomainInfo Text
diName = lens _diName (\ s a -> s{_diName = a});

-- | The status of the domain:
--
-- -   __REGISTERED__: The domain is properly registered and available. You
--     can use this domain for registering types and creating new workflow
--     executions.
-- -   __DEPRECATED__: The domain was deprecated using DeprecateDomain, but
--     is still in use. You should not create new workflow executions in
--     this domain.
diStatus :: Lens' DomainInfo RegistrationStatus
diStatus = lens _diStatus (\ s a -> s{_diStatus = a});

instance FromJSON DomainInfo where
        parseJSON
          = withObject "DomainInfo"
              (\ x ->
                 DomainInfo' <$>
                   (x .:? "description") <*> (x .: "name") <*>
                     (x .: "status"))

-- | Used to filter the workflow executions in visibility APIs by various
-- time-based rules. Each parameter, if specified, defines a rule that must
-- be satisfied by each returned query result. The parameter values are in
-- the <https://en.wikipedia.org/wiki/Unix_time Unix Time format>. For
-- example: @\"oldestDate\": 1325376070.@
--
-- /See:/ 'executionTimeFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etfLatestDate'
--
-- * 'etfOldestDate'
data ExecutionTimeFilter = ExecutionTimeFilter'
    { _etfLatestDate :: Maybe POSIX
    , _etfOldestDate :: POSIX
    } deriving (Eq,Read,Show)

-- | 'ExecutionTimeFilter' smart constructor.
executionTimeFilter :: UTCTime -> ExecutionTimeFilter
executionTimeFilter pOldestDate =
    ExecutionTimeFilter'
    { _etfLatestDate = Nothing
    , _etfOldestDate = _Time # pOldestDate
    }

-- | Specifies the latest start or close date and time to return.
etfLatestDate :: Lens' ExecutionTimeFilter (Maybe UTCTime)
etfLatestDate = lens _etfLatestDate (\ s a -> s{_etfLatestDate = a}) . mapping _Time;

-- | Specifies the oldest start or close date and time to return.
etfOldestDate :: Lens' ExecutionTimeFilter UTCTime
etfOldestDate = lens _etfOldestDate (\ s a -> s{_etfOldestDate = a}) . _Time;

instance ToJSON ExecutionTimeFilter where
        toJSON ExecutionTimeFilter'{..}
          = object
              ["latestDate" .= _etfLatestDate,
               "oldestDate" .= _etfOldestDate]

-- | Provides details of the @ExternalWorkflowExecutionCancelRequested@
-- event.
--
-- /See:/ 'externalWorkflowExecutionCancelRequestedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ewecreaWorkflowExecution'
--
-- * 'ewecreaInitiatedEventId'
data ExternalWorkflowExecutionCancelRequestedEventAttributes = ExternalWorkflowExecutionCancelRequestedEventAttributes'
    { _ewecreaWorkflowExecution :: WorkflowExecution
    , _ewecreaInitiatedEventId  :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ExternalWorkflowExecutionCancelRequestedEventAttributes' smart constructor.
externalWorkflowExecutionCancelRequestedEventAttributes :: WorkflowExecution -> Integer -> ExternalWorkflowExecutionCancelRequestedEventAttributes
externalWorkflowExecutionCancelRequestedEventAttributes pWorkflowExecution pInitiatedEventId =
    ExternalWorkflowExecutionCancelRequestedEventAttributes'
    { _ewecreaWorkflowExecution = pWorkflowExecution
    , _ewecreaInitiatedEventId = pInitiatedEventId
    }

-- | The external workflow execution to which the cancellation request was
-- delivered.
ewecreaWorkflowExecution :: Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes WorkflowExecution
ewecreaWorkflowExecution = lens _ewecreaWorkflowExecution (\ s a -> s{_ewecreaWorkflowExecution = a});

-- | The id of the @RequestCancelExternalWorkflowExecutionInitiated@ event
-- corresponding to the @RequestCancelExternalWorkflowExecution@ decision
-- to cancel this external workflow execution. This information can be
-- useful for diagnosing problems by tracing back the chain of events
-- leading up to this event.
ewecreaInitiatedEventId :: Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes Integer
ewecreaInitiatedEventId = lens _ewecreaInitiatedEventId (\ s a -> s{_ewecreaInitiatedEventId = a});

instance FromJSON
         ExternalWorkflowExecutionCancelRequestedEventAttributes
         where
        parseJSON
          = withObject
              "ExternalWorkflowExecutionCancelRequestedEventAttributes"
              (\ x ->
                 ExternalWorkflowExecutionCancelRequestedEventAttributes'
                   <$>
                   (x .: "workflowExecution") <*>
                     (x .: "initiatedEventId"))

-- | Provides details of the @ExternalWorkflowExecutionSignaled@ event.
--
-- /See:/ 'externalWorkflowExecutionSignaledEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eweseaWorkflowExecution'
--
-- * 'eweseaInitiatedEventId'
data ExternalWorkflowExecutionSignaledEventAttributes = ExternalWorkflowExecutionSignaledEventAttributes'
    { _eweseaWorkflowExecution :: WorkflowExecution
    , _eweseaInitiatedEventId  :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ExternalWorkflowExecutionSignaledEventAttributes' smart constructor.
externalWorkflowExecutionSignaledEventAttributes :: WorkflowExecution -> Integer -> ExternalWorkflowExecutionSignaledEventAttributes
externalWorkflowExecutionSignaledEventAttributes pWorkflowExecution pInitiatedEventId =
    ExternalWorkflowExecutionSignaledEventAttributes'
    { _eweseaWorkflowExecution = pWorkflowExecution
    , _eweseaInitiatedEventId = pInitiatedEventId
    }

-- | The external workflow execution that the signal was delivered to.
eweseaWorkflowExecution :: Lens' ExternalWorkflowExecutionSignaledEventAttributes WorkflowExecution
eweseaWorkflowExecution = lens _eweseaWorkflowExecution (\ s a -> s{_eweseaWorkflowExecution = a});

-- | The id of the @SignalExternalWorkflowExecutionInitiated@ event
-- corresponding to the @SignalExternalWorkflowExecution@ decision to
-- request this signal. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
eweseaInitiatedEventId :: Lens' ExternalWorkflowExecutionSignaledEventAttributes Integer
eweseaInitiatedEventId = lens _eweseaInitiatedEventId (\ s a -> s{_eweseaInitiatedEventId = a});

instance FromJSON
         ExternalWorkflowExecutionSignaledEventAttributes
         where
        parseJSON
          = withObject
              "ExternalWorkflowExecutionSignaledEventAttributes"
              (\ x ->
                 ExternalWorkflowExecutionSignaledEventAttributes' <$>
                   (x .: "workflowExecution") <*>
                     (x .: "initiatedEventId"))

-- | Provides details of the @FailWorkflowExecution@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- /See:/ 'failWorkflowExecutionDecisionAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fwedaReason'
--
-- * 'fwedaDetails'
data FailWorkflowExecutionDecisionAttributes = FailWorkflowExecutionDecisionAttributes'
    { _fwedaReason  :: Maybe Text
    , _fwedaDetails :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'FailWorkflowExecutionDecisionAttributes' smart constructor.
failWorkflowExecutionDecisionAttributes :: FailWorkflowExecutionDecisionAttributes
failWorkflowExecutionDecisionAttributes =
    FailWorkflowExecutionDecisionAttributes'
    { _fwedaReason = Nothing
    , _fwedaDetails = Nothing
    }

-- | A descriptive reason for the failure that may help in diagnostics.
fwedaReason :: Lens' FailWorkflowExecutionDecisionAttributes (Maybe Text)
fwedaReason = lens _fwedaReason (\ s a -> s{_fwedaReason = a});

-- | /Optional./ Details of the failure.
fwedaDetails :: Lens' FailWorkflowExecutionDecisionAttributes (Maybe Text)
fwedaDetails = lens _fwedaDetails (\ s a -> s{_fwedaDetails = a});

instance ToJSON
         FailWorkflowExecutionDecisionAttributes where
        toJSON FailWorkflowExecutionDecisionAttributes'{..}
          = object
              ["reason" .= _fwedaReason,
               "details" .= _fwedaDetails]

-- | Provides details of the @FailWorkflowExecutionFailed@ event.
--
-- /See:/ 'failWorkflowExecutionFailedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fwefeaCause'
--
-- * 'fwefeaDecisionTaskCompletedEventId'
data FailWorkflowExecutionFailedEventAttributes = FailWorkflowExecutionFailedEventAttributes'
    { _fwefeaCause                        :: FailWorkflowExecutionFailedCause
    , _fwefeaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'FailWorkflowExecutionFailedEventAttributes' smart constructor.
failWorkflowExecutionFailedEventAttributes :: FailWorkflowExecutionFailedCause -> Integer -> FailWorkflowExecutionFailedEventAttributes
failWorkflowExecutionFailedEventAttributes pCause pDecisionTaskCompletedEventId =
    FailWorkflowExecutionFailedEventAttributes'
    { _fwefeaCause = pCause
    , _fwefeaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | The cause of the failure. This information is generated by the system
-- and can be useful for diagnostic purposes.
--
-- If __cause__ is set to OPERATION_NOT_PERMITTED, the decision failed
-- because it lacked sufficient permissions. For details and example IAM
-- policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
fwefeaCause :: Lens' FailWorkflowExecutionFailedEventAttributes FailWorkflowExecutionFailedCause
fwefeaCause = lens _fwefeaCause (\ s a -> s{_fwefeaCause = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @FailWorkflowExecution@ decision to
-- fail this execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
fwefeaDecisionTaskCompletedEventId :: Lens' FailWorkflowExecutionFailedEventAttributes Integer
fwefeaDecisionTaskCompletedEventId = lens _fwefeaDecisionTaskCompletedEventId (\ s a -> s{_fwefeaDecisionTaskCompletedEventId = a});

instance FromJSON
         FailWorkflowExecutionFailedEventAttributes where
        parseJSON
          = withObject
              "FailWorkflowExecutionFailedEventAttributes"
              (\ x ->
                 FailWorkflowExecutionFailedEventAttributes' <$>
                   (x .: "cause") <*>
                     (x .: "decisionTaskCompletedEventId"))

-- | Event within a workflow execution. A history event can be one of these
-- types:
--
-- -   __WorkflowExecutionStarted__: The workflow execution was started.
-- -   __WorkflowExecutionCompleted__: The workflow execution was closed
--     due to successful completion.
-- -   __WorkflowExecutionFailed__: The workflow execution closed due to a
--     failure.
-- -   __WorkflowExecutionTimedOut__: The workflow execution was closed
--     because a time out was exceeded.
-- -   __WorkflowExecutionCanceled__: The workflow execution was
--     successfully canceled and closed.
-- -   __WorkflowExecutionTerminated__: The workflow execution was
--     terminated.
-- -   __WorkflowExecutionContinuedAsNew__: The workflow execution was
--     closed and a new execution of the same type was created with the
--     same workflowId.
-- -   __WorkflowExecutionCancelRequested__: A request to cancel this
--     workflow execution was made.
-- -   __DecisionTaskScheduled__: A decision task was scheduled for the
--     workflow execution.
-- -   __DecisionTaskStarted__: The decision task was dispatched to a
--     decider.
-- -   __DecisionTaskCompleted__: The decider successfully completed a
--     decision task by calling RespondDecisionTaskCompleted.
-- -   __DecisionTaskTimedOut__: The decision task timed out.
-- -   __ActivityTaskScheduled__: An activity task was scheduled for
--     execution.
-- -   __ScheduleActivityTaskFailed__: Failed to process
--     ScheduleActivityTask decision. This happens when the decision is not
--     configured properly, for example the activity type specified is not
--     registered.
-- -   __ActivityTaskStarted__: The scheduled activity task was dispatched
--     to a worker.
-- -   __ActivityTaskCompleted__: An activity worker successfully completed
--     an activity task by calling RespondActivityTaskCompleted.
-- -   __ActivityTaskFailed__: An activity worker failed an activity task
--     by calling RespondActivityTaskFailed.
-- -   __ActivityTaskTimedOut__: The activity task timed out.
-- -   __ActivityTaskCanceled__: The activity task was successfully
--     canceled.
-- -   __ActivityTaskCancelRequested__: A @RequestCancelActivityTask@
--     decision was received by the system.
-- -   __RequestCancelActivityTaskFailed__: Failed to process
--     RequestCancelActivityTask decision. This happens when the decision
--     is not configured properly.
-- -   __WorkflowExecutionSignaled__: An external signal was received for
--     the workflow execution.
-- -   __MarkerRecorded__: A marker was recorded in the workflow history as
--     the result of a @RecordMarker@ decision.
-- -   __TimerStarted__: A timer was started for the workflow execution due
--     to a @StartTimer@ decision.
-- -   __StartTimerFailed__: Failed to process StartTimer decision. This
--     happens when the decision is not configured properly, for example a
--     timer already exists with the specified timer Id.
-- -   __TimerFired__: A timer, previously started for this workflow
--     execution, fired.
-- -   __TimerCanceled__: A timer, previously started for this workflow
--     execution, was successfully canceled.
-- -   __CancelTimerFailed__: Failed to process CancelTimer decision. This
--     happens when the decision is not configured properly, for example no
--     timer exists with the specified timer Id.
-- -   __StartChildWorkflowExecutionInitiated__: A request was made to
--     start a child workflow execution.
-- -   __StartChildWorkflowExecutionFailed__: Failed to process
--     StartChildWorkflowExecution decision. This happens when the decision
--     is not configured properly, for example the workflow type specified
--     is not registered.
-- -   __ChildWorkflowExecutionStarted__: A child workflow execution was
--     successfully started.
-- -   __ChildWorkflowExecutionCompleted__: A child workflow execution,
--     started by this workflow execution, completed successfully and was
--     closed.
-- -   __ChildWorkflowExecutionFailed__: A child workflow execution,
--     started by this workflow execution, failed to complete successfully
--     and was closed.
-- -   __ChildWorkflowExecutionTimedOut__: A child workflow execution,
--     started by this workflow execution, timed out and was closed.
-- -   __ChildWorkflowExecutionCanceled__: A child workflow execution,
--     started by this workflow execution, was canceled and closed.
-- -   __ChildWorkflowExecutionTerminated__: A child workflow execution,
--     started by this workflow execution, was terminated.
-- -   __SignalExternalWorkflowExecutionInitiated__: A request to signal an
--     external workflow was made.
-- -   __ExternalWorkflowExecutionSignaled__: A signal, requested by this
--     workflow execution, was successfully delivered to the target
--     external workflow execution.
-- -   __SignalExternalWorkflowExecutionFailed__: The request to signal an
--     external workflow execution failed.
-- -   __RequestCancelExternalWorkflowExecutionInitiated__: A request was
--     made to request the cancellation of an external workflow execution.
-- -   __ExternalWorkflowExecutionCancelRequested__: Request to cancel an
--     external workflow execution was successfully delivered to the target
--     execution.
-- -   __RequestCancelExternalWorkflowExecutionFailed__: Request to cancel
--     an external workflow execution failed.
--
-- /See:/ 'historyEvent' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'heWorkflowExecutionCancelRequestedEventAttributes'
--
-- * 'heDecisionTaskScheduledEventAttributes'
--
-- * 'heStartTimerFailedEventAttributes'
--
-- * 'heRecordMarkerFailedEventAttributes'
--
-- * 'heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
--
-- * 'heWorkflowExecutionCompletedEventAttributes'
--
-- * 'heActivityTaskScheduledEventAttributes'
--
-- * 'heChildWorkflowExecutionCompletedEventAttributes'
--
-- * 'heScheduleActivityTaskFailedEventAttributes'
--
-- * 'heMarkerRecordedEventAttributes'
--
-- * 'heCompleteWorkflowExecutionFailedEventAttributes'
--
-- * 'heRequestCancelExternalWorkflowExecutionFailedEventAttributes'
--
-- * 'heTimerCanceledEventAttributes'
--
-- * 'heWorkflowExecutionStartedEventAttributes'
--
-- * 'heActivityTaskCompletedEventAttributes'
--
-- * 'heChildWorkflowExecutionStartedEventAttributes'
--
-- * 'heDecisionTaskTimedOutEventAttributes'
--
-- * 'heCancelTimerFailedEventAttributes'
--
-- * 'heActivityTaskTimedOutEventAttributes'
--
-- * 'heActivityTaskCanceledEventAttributes'
--
-- * 'heChildWorkflowExecutionCanceledEventAttributes'
--
-- * 'heDecisionTaskStartedEventAttributes'
--
-- * 'heCancelWorkflowExecutionFailedEventAttributes'
--
-- * 'heChildWorkflowExecutionTimedOutEventAttributes'
--
-- * 'heRequestCancelActivityTaskFailedEventAttributes'
--
-- * 'heWorkflowExecutionTerminatedEventAttributes'
--
-- * 'heStartChildWorkflowExecutionInitiatedEventAttributes'
--
-- * 'heActivityTaskStartedEventAttributes'
--
-- * 'heSignalExternalWorkflowExecutionFailedEventAttributes'
--
-- * 'heTimerStartedEventAttributes'
--
-- * 'heWorkflowExecutionTimedOutEventAttributes'
--
-- * 'heActivityTaskCancelRequestedEventAttributes'
--
-- * 'heChildWorkflowExecutionTerminatedEventAttributes'
--
-- * 'heWorkflowExecutionCanceledEventAttributes'
--
-- * 'heWorkflowExecutionSignaledEventAttributes'
--
-- * 'heActivityTaskFailedEventAttributes'
--
-- * 'heExternalWorkflowExecutionSignaledEventAttributes'
--
-- * 'heTimerFiredEventAttributes'
--
-- * 'heFailWorkflowExecutionFailedEventAttributes'
--
-- * 'heChildWorkflowExecutionFailedEventAttributes'
--
-- * 'heDecisionTaskCompletedEventAttributes'
--
-- * 'heStartChildWorkflowExecutionFailedEventAttributes'
--
-- * 'heSignalExternalWorkflowExecutionInitiatedEventAttributes'
--
-- * 'heContinueAsNewWorkflowExecutionFailedEventAttributes'
--
-- * 'heWorkflowExecutionFailedEventAttributes'
--
-- * 'heWorkflowExecutionContinuedAsNewEventAttributes'
--
-- * 'heExternalWorkflowExecutionCancelRequestedEventAttributes'
--
-- * 'heEventTimestamp'
--
-- * 'heEventType'
--
-- * 'heEventId'
data HistoryEvent = HistoryEvent'
    { _heWorkflowExecutionCancelRequestedEventAttributes                :: Maybe WorkflowExecutionCancelRequestedEventAttributes
    , _heDecisionTaskScheduledEventAttributes                           :: Maybe DecisionTaskScheduledEventAttributes
    , _heStartTimerFailedEventAttributes                                :: Maybe StartTimerFailedEventAttributes
    , _heRecordMarkerFailedEventAttributes                              :: Maybe RecordMarkerFailedEventAttributes
    , _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , _heWorkflowExecutionCompletedEventAttributes                      :: Maybe WorkflowExecutionCompletedEventAttributes
    , _heActivityTaskScheduledEventAttributes                           :: Maybe ActivityTaskScheduledEventAttributes
    , _heChildWorkflowExecutionCompletedEventAttributes                 :: Maybe ChildWorkflowExecutionCompletedEventAttributes
    , _heScheduleActivityTaskFailedEventAttributes                      :: Maybe ScheduleActivityTaskFailedEventAttributes
    , _heMarkerRecordedEventAttributes                                  :: Maybe MarkerRecordedEventAttributes
    , _heCompleteWorkflowExecutionFailedEventAttributes                 :: Maybe CompleteWorkflowExecutionFailedEventAttributes
    , _heRequestCancelExternalWorkflowExecutionFailedEventAttributes    :: Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes
    , _heTimerCanceledEventAttributes                                   :: Maybe TimerCanceledEventAttributes
    , _heWorkflowExecutionStartedEventAttributes                        :: Maybe WorkflowExecutionStartedEventAttributes
    , _heActivityTaskCompletedEventAttributes                           :: Maybe ActivityTaskCompletedEventAttributes
    , _heChildWorkflowExecutionStartedEventAttributes                   :: Maybe ChildWorkflowExecutionStartedEventAttributes
    , _heDecisionTaskTimedOutEventAttributes                            :: Maybe DecisionTaskTimedOutEventAttributes
    , _heCancelTimerFailedEventAttributes                               :: Maybe CancelTimerFailedEventAttributes
    , _heActivityTaskTimedOutEventAttributes                            :: Maybe ActivityTaskTimedOutEventAttributes
    , _heActivityTaskCanceledEventAttributes                            :: Maybe ActivityTaskCanceledEventAttributes
    , _heChildWorkflowExecutionCanceledEventAttributes                  :: Maybe ChildWorkflowExecutionCanceledEventAttributes
    , _heDecisionTaskStartedEventAttributes                             :: Maybe DecisionTaskStartedEventAttributes
    , _heCancelWorkflowExecutionFailedEventAttributes                   :: Maybe CancelWorkflowExecutionFailedEventAttributes
    , _heChildWorkflowExecutionTimedOutEventAttributes                  :: Maybe ChildWorkflowExecutionTimedOutEventAttributes
    , _heRequestCancelActivityTaskFailedEventAttributes                 :: Maybe RequestCancelActivityTaskFailedEventAttributes
    , _heWorkflowExecutionTerminatedEventAttributes                     :: Maybe WorkflowExecutionTerminatedEventAttributes
    , _heStartChildWorkflowExecutionInitiatedEventAttributes            :: Maybe StartChildWorkflowExecutionInitiatedEventAttributes
    , _heActivityTaskStartedEventAttributes                             :: Maybe ActivityTaskStartedEventAttributes
    , _heSignalExternalWorkflowExecutionFailedEventAttributes           :: Maybe SignalExternalWorkflowExecutionFailedEventAttributes
    , _heTimerStartedEventAttributes                                    :: Maybe TimerStartedEventAttributes
    , _heWorkflowExecutionTimedOutEventAttributes                       :: Maybe WorkflowExecutionTimedOutEventAttributes
    , _heActivityTaskCancelRequestedEventAttributes                     :: Maybe ActivityTaskCancelRequestedEventAttributes
    , _heChildWorkflowExecutionTerminatedEventAttributes                :: Maybe ChildWorkflowExecutionTerminatedEventAttributes
    , _heWorkflowExecutionCanceledEventAttributes                       :: Maybe WorkflowExecutionCanceledEventAttributes
    , _heWorkflowExecutionSignaledEventAttributes                       :: Maybe WorkflowExecutionSignaledEventAttributes
    , _heActivityTaskFailedEventAttributes                              :: Maybe ActivityTaskFailedEventAttributes
    , _heExternalWorkflowExecutionSignaledEventAttributes               :: Maybe ExternalWorkflowExecutionSignaledEventAttributes
    , _heTimerFiredEventAttributes                                      :: Maybe TimerFiredEventAttributes
    , _heFailWorkflowExecutionFailedEventAttributes                     :: Maybe FailWorkflowExecutionFailedEventAttributes
    , _heChildWorkflowExecutionFailedEventAttributes                    :: Maybe ChildWorkflowExecutionFailedEventAttributes
    , _heDecisionTaskCompletedEventAttributes                           :: Maybe DecisionTaskCompletedEventAttributes
    , _heStartChildWorkflowExecutionFailedEventAttributes               :: Maybe StartChildWorkflowExecutionFailedEventAttributes
    , _heSignalExternalWorkflowExecutionInitiatedEventAttributes        :: Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes
    , _heContinueAsNewWorkflowExecutionFailedEventAttributes            :: Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes
    , _heWorkflowExecutionFailedEventAttributes                         :: Maybe WorkflowExecutionFailedEventAttributes
    , _heWorkflowExecutionContinuedAsNewEventAttributes                 :: Maybe WorkflowExecutionContinuedAsNewEventAttributes
    , _heExternalWorkflowExecutionCancelRequestedEventAttributes        :: Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes
    , _heEventTimestamp                                                 :: POSIX
    , _heEventType                                                      :: EventType
    , _heEventId                                                        :: !Integer
    } deriving (Eq,Read,Show)

-- | 'HistoryEvent' smart constructor.
historyEvent :: UTCTime -> EventType -> Integer -> HistoryEvent
historyEvent pEventTimestamp pEventType pEventId =
    HistoryEvent'
    { _heWorkflowExecutionCancelRequestedEventAttributes = Nothing
    , _heDecisionTaskScheduledEventAttributes = Nothing
    , _heStartTimerFailedEventAttributes = Nothing
    , _heRecordMarkerFailedEventAttributes = Nothing
    , _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes = Nothing
    , _heWorkflowExecutionCompletedEventAttributes = Nothing
    , _heActivityTaskScheduledEventAttributes = Nothing
    , _heChildWorkflowExecutionCompletedEventAttributes = Nothing
    , _heScheduleActivityTaskFailedEventAttributes = Nothing
    , _heMarkerRecordedEventAttributes = Nothing
    , _heCompleteWorkflowExecutionFailedEventAttributes = Nothing
    , _heRequestCancelExternalWorkflowExecutionFailedEventAttributes = Nothing
    , _heTimerCanceledEventAttributes = Nothing
    , _heWorkflowExecutionStartedEventAttributes = Nothing
    , _heActivityTaskCompletedEventAttributes = Nothing
    , _heChildWorkflowExecutionStartedEventAttributes = Nothing
    , _heDecisionTaskTimedOutEventAttributes = Nothing
    , _heCancelTimerFailedEventAttributes = Nothing
    , _heActivityTaskTimedOutEventAttributes = Nothing
    , _heActivityTaskCanceledEventAttributes = Nothing
    , _heChildWorkflowExecutionCanceledEventAttributes = Nothing
    , _heDecisionTaskStartedEventAttributes = Nothing
    , _heCancelWorkflowExecutionFailedEventAttributes = Nothing
    , _heChildWorkflowExecutionTimedOutEventAttributes = Nothing
    , _heRequestCancelActivityTaskFailedEventAttributes = Nothing
    , _heWorkflowExecutionTerminatedEventAttributes = Nothing
    , _heStartChildWorkflowExecutionInitiatedEventAttributes = Nothing
    , _heActivityTaskStartedEventAttributes = Nothing
    , _heSignalExternalWorkflowExecutionFailedEventAttributes = Nothing
    , _heTimerStartedEventAttributes = Nothing
    , _heWorkflowExecutionTimedOutEventAttributes = Nothing
    , _heActivityTaskCancelRequestedEventAttributes = Nothing
    , _heChildWorkflowExecutionTerminatedEventAttributes = Nothing
    , _heWorkflowExecutionCanceledEventAttributes = Nothing
    , _heWorkflowExecutionSignaledEventAttributes = Nothing
    , _heActivityTaskFailedEventAttributes = Nothing
    , _heExternalWorkflowExecutionSignaledEventAttributes = Nothing
    , _heTimerFiredEventAttributes = Nothing
    , _heFailWorkflowExecutionFailedEventAttributes = Nothing
    , _heChildWorkflowExecutionFailedEventAttributes = Nothing
    , _heDecisionTaskCompletedEventAttributes = Nothing
    , _heStartChildWorkflowExecutionFailedEventAttributes = Nothing
    , _heSignalExternalWorkflowExecutionInitiatedEventAttributes = Nothing
    , _heContinueAsNewWorkflowExecutionFailedEventAttributes = Nothing
    , _heWorkflowExecutionFailedEventAttributes = Nothing
    , _heWorkflowExecutionContinuedAsNewEventAttributes = Nothing
    , _heExternalWorkflowExecutionCancelRequestedEventAttributes = Nothing
    , _heEventTimestamp = _Time # pEventTimestamp
    , _heEventType = pEventType
    , _heEventId = pEventId
    }

-- | If the event is of type @WorkflowExecutionCancelRequested@ then this
-- member is set and provides detailed information about the event. It is
-- not set for other event types.
heWorkflowExecutionCancelRequestedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionCancelRequestedEventAttributes)
heWorkflowExecutionCancelRequestedEventAttributes = lens _heWorkflowExecutionCancelRequestedEventAttributes (\ s a -> s{_heWorkflowExecutionCancelRequestedEventAttributes = a});

-- | If the event is of type @DecisionTaskScheduled@ then this member is set
-- and provides detailed information about the event. It is not set for
-- other event types.
heDecisionTaskScheduledEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskScheduledEventAttributes)
heDecisionTaskScheduledEventAttributes = lens _heDecisionTaskScheduledEventAttributes (\ s a -> s{_heDecisionTaskScheduledEventAttributes = a});

-- | If the event is of type @StartTimerFailed@ then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heStartTimerFailedEventAttributes :: Lens' HistoryEvent (Maybe StartTimerFailedEventAttributes)
heStartTimerFailedEventAttributes = lens _heStartTimerFailedEventAttributes (\ s a -> s{_heStartTimerFailedEventAttributes = a});

-- | If the event is of type @DecisionTaskFailed@ then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heRecordMarkerFailedEventAttributes :: Lens' HistoryEvent (Maybe RecordMarkerFailedEventAttributes)
heRecordMarkerFailedEventAttributes = lens _heRecordMarkerFailedEventAttributes (\ s a -> s{_heRecordMarkerFailedEventAttributes = a});

-- | If the event is of type
-- @RequestCancelExternalWorkflowExecutionInitiated@ then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Lens' HistoryEvent (Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes = lens _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes (\ s a -> s{_heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes = a});

-- | If the event is of type @WorkflowExecutionCompleted@ then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heWorkflowExecutionCompletedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionCompletedEventAttributes)
heWorkflowExecutionCompletedEventAttributes = lens _heWorkflowExecutionCompletedEventAttributes (\ s a -> s{_heWorkflowExecutionCompletedEventAttributes = a});

-- | If the event is of type @ActivityTaskScheduled@ then this member is set
-- and provides detailed information about the event. It is not set for
-- other event types.
heActivityTaskScheduledEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskScheduledEventAttributes)
heActivityTaskScheduledEventAttributes = lens _heActivityTaskScheduledEventAttributes (\ s a -> s{_heActivityTaskScheduledEventAttributes = a});

-- | If the event is of type @ChildWorkflowExecutionCompleted@ then this
-- member is set and provides detailed information about the event. It is
-- not set for other event types.
heChildWorkflowExecutionCompletedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionCompletedEventAttributes)
heChildWorkflowExecutionCompletedEventAttributes = lens _heChildWorkflowExecutionCompletedEventAttributes (\ s a -> s{_heChildWorkflowExecutionCompletedEventAttributes = a});

-- | If the event is of type @ScheduleActivityTaskFailed@ then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heScheduleActivityTaskFailedEventAttributes :: Lens' HistoryEvent (Maybe ScheduleActivityTaskFailedEventAttributes)
heScheduleActivityTaskFailedEventAttributes = lens _heScheduleActivityTaskFailedEventAttributes (\ s a -> s{_heScheduleActivityTaskFailedEventAttributes = a});

-- | If the event is of type @MarkerRecorded@ then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heMarkerRecordedEventAttributes :: Lens' HistoryEvent (Maybe MarkerRecordedEventAttributes)
heMarkerRecordedEventAttributes = lens _heMarkerRecordedEventAttributes (\ s a -> s{_heMarkerRecordedEventAttributes = a});

-- | If the event is of type @CompleteWorkflowExecutionFailed@ then this
-- member is set and provides detailed information about the event. It is
-- not set for other event types.
heCompleteWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe CompleteWorkflowExecutionFailedEventAttributes)
heCompleteWorkflowExecutionFailedEventAttributes = lens _heCompleteWorkflowExecutionFailedEventAttributes (\ s a -> s{_heCompleteWorkflowExecutionFailedEventAttributes = a});

-- | If the event is of type @RequestCancelExternalWorkflowExecutionFailed@
-- then this member is set and provides detailed information about the
-- event. It is not set for other event types.
heRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes)
heRequestCancelExternalWorkflowExecutionFailedEventAttributes = lens _heRequestCancelExternalWorkflowExecutionFailedEventAttributes (\ s a -> s{_heRequestCancelExternalWorkflowExecutionFailedEventAttributes = a});

-- | If the event is of type @TimerCanceled@ then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heTimerCanceledEventAttributes :: Lens' HistoryEvent (Maybe TimerCanceledEventAttributes)
heTimerCanceledEventAttributes = lens _heTimerCanceledEventAttributes (\ s a -> s{_heTimerCanceledEventAttributes = a});

-- | If the event is of type @WorkflowExecutionStarted@ then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heWorkflowExecutionStartedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionStartedEventAttributes)
heWorkflowExecutionStartedEventAttributes = lens _heWorkflowExecutionStartedEventAttributes (\ s a -> s{_heWorkflowExecutionStartedEventAttributes = a});

-- | If the event is of type @ActivityTaskCompleted@ then this member is set
-- and provides detailed information about the event. It is not set for
-- other event types.
heActivityTaskCompletedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskCompletedEventAttributes)
heActivityTaskCompletedEventAttributes = lens _heActivityTaskCompletedEventAttributes (\ s a -> s{_heActivityTaskCompletedEventAttributes = a});

-- | If the event is of type @ChildWorkflowExecutionStarted@ then this member
-- is set and provides detailed information about the event. It is not set
-- for other event types.
heChildWorkflowExecutionStartedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionStartedEventAttributes)
heChildWorkflowExecutionStartedEventAttributes = lens _heChildWorkflowExecutionStartedEventAttributes (\ s a -> s{_heChildWorkflowExecutionStartedEventAttributes = a});

-- | If the event is of type @DecisionTaskTimedOut@ then this member is set
-- and provides detailed information about the event. It is not set for
-- other event types.
heDecisionTaskTimedOutEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskTimedOutEventAttributes)
heDecisionTaskTimedOutEventAttributes = lens _heDecisionTaskTimedOutEventAttributes (\ s a -> s{_heDecisionTaskTimedOutEventAttributes = a});

-- | If the event is of type @CancelTimerFailed@ then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heCancelTimerFailedEventAttributes :: Lens' HistoryEvent (Maybe CancelTimerFailedEventAttributes)
heCancelTimerFailedEventAttributes = lens _heCancelTimerFailedEventAttributes (\ s a -> s{_heCancelTimerFailedEventAttributes = a});

-- | If the event is of type @ActivityTaskTimedOut@ then this member is set
-- and provides detailed information about the event. It is not set for
-- other event types.
heActivityTaskTimedOutEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskTimedOutEventAttributes)
heActivityTaskTimedOutEventAttributes = lens _heActivityTaskTimedOutEventAttributes (\ s a -> s{_heActivityTaskTimedOutEventAttributes = a});

-- | If the event is of type @ActivityTaskCanceled@ then this member is set
-- and provides detailed information about the event. It is not set for
-- other event types.
heActivityTaskCanceledEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskCanceledEventAttributes)
heActivityTaskCanceledEventAttributes = lens _heActivityTaskCanceledEventAttributes (\ s a -> s{_heActivityTaskCanceledEventAttributes = a});

-- | If the event is of type @ChildWorkflowExecutionCanceled@ then this
-- member is set and provides detailed information about the event. It is
-- not set for other event types.
heChildWorkflowExecutionCanceledEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionCanceledEventAttributes)
heChildWorkflowExecutionCanceledEventAttributes = lens _heChildWorkflowExecutionCanceledEventAttributes (\ s a -> s{_heChildWorkflowExecutionCanceledEventAttributes = a});

-- | If the event is of type @DecisionTaskStarted@ then this member is set
-- and provides detailed information about the event. It is not set for
-- other event types.
heDecisionTaskStartedEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskStartedEventAttributes)
heDecisionTaskStartedEventAttributes = lens _heDecisionTaskStartedEventAttributes (\ s a -> s{_heDecisionTaskStartedEventAttributes = a});

-- | If the event is of type @CancelWorkflowExecutionFailed@ then this member
-- is set and provides detailed information about the event. It is not set
-- for other event types.
heCancelWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe CancelWorkflowExecutionFailedEventAttributes)
heCancelWorkflowExecutionFailedEventAttributes = lens _heCancelWorkflowExecutionFailedEventAttributes (\ s a -> s{_heCancelWorkflowExecutionFailedEventAttributes = a});

-- | If the event is of type @ChildWorkflowExecutionTimedOut@ then this
-- member is set and provides detailed information about the event. It is
-- not set for other event types.
heChildWorkflowExecutionTimedOutEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionTimedOutEventAttributes)
heChildWorkflowExecutionTimedOutEventAttributes = lens _heChildWorkflowExecutionTimedOutEventAttributes (\ s a -> s{_heChildWorkflowExecutionTimedOutEventAttributes = a});

-- | If the event is of type @RequestCancelActivityTaskFailed@ then this
-- member is set and provides detailed information about the event. It is
-- not set for other event types.
heRequestCancelActivityTaskFailedEventAttributes :: Lens' HistoryEvent (Maybe RequestCancelActivityTaskFailedEventAttributes)
heRequestCancelActivityTaskFailedEventAttributes = lens _heRequestCancelActivityTaskFailedEventAttributes (\ s a -> s{_heRequestCancelActivityTaskFailedEventAttributes = a});

-- | If the event is of type @WorkflowExecutionTerminated@ then this member
-- is set and provides detailed information about the event. It is not set
-- for other event types.
heWorkflowExecutionTerminatedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionTerminatedEventAttributes)
heWorkflowExecutionTerminatedEventAttributes = lens _heWorkflowExecutionTerminatedEventAttributes (\ s a -> s{_heWorkflowExecutionTerminatedEventAttributes = a});

-- | If the event is of type @StartChildWorkflowExecutionInitiated@ then this
-- member is set and provides detailed information about the event. It is
-- not set for other event types.
heStartChildWorkflowExecutionInitiatedEventAttributes :: Lens' HistoryEvent (Maybe StartChildWorkflowExecutionInitiatedEventAttributes)
heStartChildWorkflowExecutionInitiatedEventAttributes = lens _heStartChildWorkflowExecutionInitiatedEventAttributes (\ s a -> s{_heStartChildWorkflowExecutionInitiatedEventAttributes = a});

-- | If the event is of type @ActivityTaskStarted@ then this member is set
-- and provides detailed information about the event. It is not set for
-- other event types.
heActivityTaskStartedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskStartedEventAttributes)
heActivityTaskStartedEventAttributes = lens _heActivityTaskStartedEventAttributes (\ s a -> s{_heActivityTaskStartedEventAttributes = a});

-- | If the event is of type @SignalExternalWorkflowExecutionFailed@ then
-- this member is set and provides detailed information about the event. It
-- is not set for other event types.
heSignalExternalWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe SignalExternalWorkflowExecutionFailedEventAttributes)
heSignalExternalWorkflowExecutionFailedEventAttributes = lens _heSignalExternalWorkflowExecutionFailedEventAttributes (\ s a -> s{_heSignalExternalWorkflowExecutionFailedEventAttributes = a});

-- | If the event is of type @TimerStarted@ then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heTimerStartedEventAttributes :: Lens' HistoryEvent (Maybe TimerStartedEventAttributes)
heTimerStartedEventAttributes = lens _heTimerStartedEventAttributes (\ s a -> s{_heTimerStartedEventAttributes = a});

-- | If the event is of type @WorkflowExecutionTimedOut@ then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heWorkflowExecutionTimedOutEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionTimedOutEventAttributes)
heWorkflowExecutionTimedOutEventAttributes = lens _heWorkflowExecutionTimedOutEventAttributes (\ s a -> s{_heWorkflowExecutionTimedOutEventAttributes = a});

-- | If the event is of type @ActivityTaskcancelRequested@ then this member
-- is set and provides detailed information about the event. It is not set
-- for other event types.
heActivityTaskCancelRequestedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskCancelRequestedEventAttributes)
heActivityTaskCancelRequestedEventAttributes = lens _heActivityTaskCancelRequestedEventAttributes (\ s a -> s{_heActivityTaskCancelRequestedEventAttributes = a});

-- | If the event is of type @ChildWorkflowExecutionTerminated@ then this
-- member is set and provides detailed information about the event. It is
-- not set for other event types.
heChildWorkflowExecutionTerminatedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionTerminatedEventAttributes)
heChildWorkflowExecutionTerminatedEventAttributes = lens _heChildWorkflowExecutionTerminatedEventAttributes (\ s a -> s{_heChildWorkflowExecutionTerminatedEventAttributes = a});

-- | If the event is of type @WorkflowExecutionCanceled@ then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heWorkflowExecutionCanceledEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionCanceledEventAttributes)
heWorkflowExecutionCanceledEventAttributes = lens _heWorkflowExecutionCanceledEventAttributes (\ s a -> s{_heWorkflowExecutionCanceledEventAttributes = a});

-- | If the event is of type @WorkflowExecutionSignaled@ then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heWorkflowExecutionSignaledEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionSignaledEventAttributes)
heWorkflowExecutionSignaledEventAttributes = lens _heWorkflowExecutionSignaledEventAttributes (\ s a -> s{_heWorkflowExecutionSignaledEventAttributes = a});

-- | If the event is of type @ActivityTaskFailed@ then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskFailedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskFailedEventAttributes)
heActivityTaskFailedEventAttributes = lens _heActivityTaskFailedEventAttributes (\ s a -> s{_heActivityTaskFailedEventAttributes = a});

-- | If the event is of type @ExternalWorkflowExecutionSignaled@ then this
-- member is set and provides detailed information about the event. It is
-- not set for other event types.
heExternalWorkflowExecutionSignaledEventAttributes :: Lens' HistoryEvent (Maybe ExternalWorkflowExecutionSignaledEventAttributes)
heExternalWorkflowExecutionSignaledEventAttributes = lens _heExternalWorkflowExecutionSignaledEventAttributes (\ s a -> s{_heExternalWorkflowExecutionSignaledEventAttributes = a});

-- | If the event is of type @TimerFired@ then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heTimerFiredEventAttributes :: Lens' HistoryEvent (Maybe TimerFiredEventAttributes)
heTimerFiredEventAttributes = lens _heTimerFiredEventAttributes (\ s a -> s{_heTimerFiredEventAttributes = a});

-- | If the event is of type @FailWorkflowExecutionFailed@ then this member
-- is set and provides detailed information about the event. It is not set
-- for other event types.
heFailWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe FailWorkflowExecutionFailedEventAttributes)
heFailWorkflowExecutionFailedEventAttributes = lens _heFailWorkflowExecutionFailedEventAttributes (\ s a -> s{_heFailWorkflowExecutionFailedEventAttributes = a});

-- | If the event is of type @ChildWorkflowExecutionFailed@ then this member
-- is set and provides detailed information about the event. It is not set
-- for other event types.
heChildWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionFailedEventAttributes)
heChildWorkflowExecutionFailedEventAttributes = lens _heChildWorkflowExecutionFailedEventAttributes (\ s a -> s{_heChildWorkflowExecutionFailedEventAttributes = a});

-- | If the event is of type @DecisionTaskCompleted@ then this member is set
-- and provides detailed information about the event. It is not set for
-- other event types.
heDecisionTaskCompletedEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskCompletedEventAttributes)
heDecisionTaskCompletedEventAttributes = lens _heDecisionTaskCompletedEventAttributes (\ s a -> s{_heDecisionTaskCompletedEventAttributes = a});

-- | If the event is of type @StartChildWorkflowExecutionFailed@ then this
-- member is set and provides detailed information about the event. It is
-- not set for other event types.
heStartChildWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe StartChildWorkflowExecutionFailedEventAttributes)
heStartChildWorkflowExecutionFailedEventAttributes = lens _heStartChildWorkflowExecutionFailedEventAttributes (\ s a -> s{_heStartChildWorkflowExecutionFailedEventAttributes = a});

-- | If the event is of type @SignalExternalWorkflowExecutionInitiated@ then
-- this member is set and provides detailed information about the event. It
-- is not set for other event types.
heSignalExternalWorkflowExecutionInitiatedEventAttributes :: Lens' HistoryEvent (Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes)
heSignalExternalWorkflowExecutionInitiatedEventAttributes = lens _heSignalExternalWorkflowExecutionInitiatedEventAttributes (\ s a -> s{_heSignalExternalWorkflowExecutionInitiatedEventAttributes = a});

-- | If the event is of type @ContinueAsNewWorkflowExecutionFailed@ then this
-- member is set and provides detailed information about the event. It is
-- not set for other event types.
heContinueAsNewWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes)
heContinueAsNewWorkflowExecutionFailedEventAttributes = lens _heContinueAsNewWorkflowExecutionFailedEventAttributes (\ s a -> s{_heContinueAsNewWorkflowExecutionFailedEventAttributes = a});

-- | If the event is of type @WorkflowExecutionFailed@ then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionFailedEventAttributes)
heWorkflowExecutionFailedEventAttributes = lens _heWorkflowExecutionFailedEventAttributes (\ s a -> s{_heWorkflowExecutionFailedEventAttributes = a});

-- | If the event is of type @WorkflowExecutionContinuedAsNew@ then this
-- member is set and provides detailed information about the event. It is
-- not set for other event types.
heWorkflowExecutionContinuedAsNewEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionContinuedAsNewEventAttributes)
heWorkflowExecutionContinuedAsNewEventAttributes = lens _heWorkflowExecutionContinuedAsNewEventAttributes (\ s a -> s{_heWorkflowExecutionContinuedAsNewEventAttributes = a});

-- | If the event is of type @ExternalWorkflowExecutionCancelRequested@ then
-- this member is set and provides detailed information about the event. It
-- is not set for other event types.
heExternalWorkflowExecutionCancelRequestedEventAttributes :: Lens' HistoryEvent (Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes)
heExternalWorkflowExecutionCancelRequestedEventAttributes = lens _heExternalWorkflowExecutionCancelRequestedEventAttributes (\ s a -> s{_heExternalWorkflowExecutionCancelRequestedEventAttributes = a});

-- | The date and time when the event occurred.
heEventTimestamp :: Lens' HistoryEvent UTCTime
heEventTimestamp = lens _heEventTimestamp (\ s a -> s{_heEventTimestamp = a}) . _Time;

-- | The type of the history event.
heEventType :: Lens' HistoryEvent EventType
heEventType = lens _heEventType (\ s a -> s{_heEventType = a});

-- | The system generated id of the event. This id uniquely identifies the
-- event with in the workflow execution history.
heEventId :: Lens' HistoryEvent Integer
heEventId = lens _heEventId (\ s a -> s{_heEventId = a});

instance FromJSON HistoryEvent where
        parseJSON
          = withObject "HistoryEvent"
              (\ x ->
                 HistoryEvent' <$>
                   (x .:?
                      "workflowExecutionCancelRequestedEventAttributes")
                     <*> (x .:? "decisionTaskScheduledEventAttributes")
                     <*> (x .:? "startTimerFailedEventAttributes")
                     <*> (x .:? "recordMarkerFailedEventAttributes")
                     <*>
                     (x .:?
                        "requestCancelExternalWorkflowExecutionInitiatedEventAttributes")
                     <*>
                     (x .:? "workflowExecutionCompletedEventAttributes")
                     <*> (x .:? "activityTaskScheduledEventAttributes")
                     <*>
                     (x .:?
                        "childWorkflowExecutionCompletedEventAttributes")
                     <*>
                     (x .:? "scheduleActivityTaskFailedEventAttributes")
                     <*> (x .:? "markerRecordedEventAttributes")
                     <*>
                     (x .:?
                        "completeWorkflowExecutionFailedEventAttributes")
                     <*>
                     (x .:?
                        "requestCancelExternalWorkflowExecutionFailedEventAttributes")
                     <*> (x .:? "timerCanceledEventAttributes")
                     <*> (x .:? "workflowExecutionStartedEventAttributes")
                     <*> (x .:? "activityTaskCompletedEventAttributes")
                     <*>
                     (x .:?
                        "childWorkflowExecutionStartedEventAttributes")
                     <*> (x .:? "decisionTaskTimedOutEventAttributes")
                     <*> (x .:? "cancelTimerFailedEventAttributes")
                     <*> (x .:? "activityTaskTimedOutEventAttributes")
                     <*> (x .:? "activityTaskCanceledEventAttributes")
                     <*>
                     (x .:?
                        "childWorkflowExecutionCanceledEventAttributes")
                     <*> (x .:? "decisionTaskStartedEventAttributes")
                     <*>
                     (x .:?
                        "cancelWorkflowExecutionFailedEventAttributes")
                     <*>
                     (x .:?
                        "childWorkflowExecutionTimedOutEventAttributes")
                     <*>
                     (x .:?
                        "requestCancelActivityTaskFailedEventAttributes")
                     <*>
                     (x .:? "workflowExecutionTerminatedEventAttributes")
                     <*>
                     (x .:?
                        "startChildWorkflowExecutionInitiatedEventAttributes")
                     <*> (x .:? "activityTaskStartedEventAttributes")
                     <*>
                     (x .:?
                        "signalExternalWorkflowExecutionFailedEventAttributes")
                     <*> (x .:? "timerStartedEventAttributes")
                     <*>
                     (x .:? "workflowExecutionTimedOutEventAttributes")
                     <*>
                     (x .:? "activityTaskCancelRequestedEventAttributes")
                     <*>
                     (x .:?
                        "childWorkflowExecutionTerminatedEventAttributes")
                     <*>
                     (x .:? "workflowExecutionCanceledEventAttributes")
                     <*>
                     (x .:? "workflowExecutionSignaledEventAttributes")
                     <*> (x .:? "activityTaskFailedEventAttributes")
                     <*>
                     (x .:?
                        "externalWorkflowExecutionSignaledEventAttributes")
                     <*> (x .:? "timerFiredEventAttributes")
                     <*>
                     (x .:? "failWorkflowExecutionFailedEventAttributes")
                     <*>
                     (x .:? "childWorkflowExecutionFailedEventAttributes")
                     <*> (x .:? "decisionTaskCompletedEventAttributes")
                     <*>
                     (x .:?
                        "startChildWorkflowExecutionFailedEventAttributes")
                     <*>
                     (x .:?
                        "signalExternalWorkflowExecutionInitiatedEventAttributes")
                     <*>
                     (x .:?
                        "continueAsNewWorkflowExecutionFailedEventAttributes")
                     <*> (x .:? "workflowExecutionFailedEventAttributes")
                     <*>
                     (x .:?
                        "workflowExecutionContinuedAsNewEventAttributes")
                     <*>
                     (x .:?
                        "externalWorkflowExecutionCancelRequestedEventAttributes")
                     <*> (x .: "eventTimestamp")
                     <*> (x .: "eventType")
                     <*> (x .: "eventId"))

-- | Provides details of the @MarkerRecorded@ event.
--
-- /See:/ 'markerRecordedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mreaDetails'
--
-- * 'mreaMarkerName'
--
-- * 'mreaDecisionTaskCompletedEventId'
data MarkerRecordedEventAttributes = MarkerRecordedEventAttributes'
    { _mreaDetails                      :: Maybe Text
    , _mreaMarkerName                   :: Text
    , _mreaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'MarkerRecordedEventAttributes' smart constructor.
markerRecordedEventAttributes :: Text -> Integer -> MarkerRecordedEventAttributes
markerRecordedEventAttributes pMarkerName pDecisionTaskCompletedEventId =
    MarkerRecordedEventAttributes'
    { _mreaDetails = Nothing
    , _mreaMarkerName = pMarkerName
    , _mreaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | Details of the marker (if any).
mreaDetails :: Lens' MarkerRecordedEventAttributes (Maybe Text)
mreaDetails = lens _mreaDetails (\ s a -> s{_mreaDetails = a});

-- | The name of the marker.
mreaMarkerName :: Lens' MarkerRecordedEventAttributes Text
mreaMarkerName = lens _mreaMarkerName (\ s a -> s{_mreaMarkerName = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @RecordMarker@ decision that
-- requested this marker. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
mreaDecisionTaskCompletedEventId :: Lens' MarkerRecordedEventAttributes Integer
mreaDecisionTaskCompletedEventId = lens _mreaDecisionTaskCompletedEventId (\ s a -> s{_mreaDecisionTaskCompletedEventId = a});

instance FromJSON MarkerRecordedEventAttributes where
        parseJSON
          = withObject "MarkerRecordedEventAttributes"
              (\ x ->
                 MarkerRecordedEventAttributes' <$>
                   (x .:? "details") <*> (x .: "markerName") <*>
                     (x .: "decisionTaskCompletedEventId"))

-- | Contains the count of tasks in a task list.
--
-- /See:/ 'pendingTaskCount' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptcTruncated'
--
-- * 'ptcCount'
data PendingTaskCount = PendingTaskCount'
    { _ptcTruncated :: Maybe Bool
    , _ptcCount     :: !Nat
    } deriving (Eq,Read,Show)

-- | 'PendingTaskCount' smart constructor.
pendingTaskCount :: Natural -> PendingTaskCount
pendingTaskCount pCount =
    PendingTaskCount'
    { _ptcTruncated = Nothing
    , _ptcCount = _Nat # pCount
    }

-- | If set to true, indicates that the actual count was more than the
-- maximum supported by this API and the count returned is the truncated
-- value.
ptcTruncated :: Lens' PendingTaskCount (Maybe Bool)
ptcTruncated = lens _ptcTruncated (\ s a -> s{_ptcTruncated = a});

-- | The number of tasks in the task list.
ptcCount :: Lens' PendingTaskCount Natural
ptcCount = lens _ptcCount (\ s a -> s{_ptcCount = a}) . _Nat;

instance FromJSON PendingTaskCount where
        parseJSON
          = withObject "PendingTaskCount"
              (\ x ->
                 PendingTaskCount' <$>
                   (x .:? "truncated") <*> (x .: "count"))

-- | Provides details of the @RecordMarker@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- /See:/ 'recordMarkerDecisionAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rmdaDetails'
--
-- * 'rmdaMarkerName'
data RecordMarkerDecisionAttributes = RecordMarkerDecisionAttributes'
    { _rmdaDetails    :: Maybe Text
    , _rmdaMarkerName :: Text
    } deriving (Eq,Read,Show)

-- | 'RecordMarkerDecisionAttributes' smart constructor.
recordMarkerDecisionAttributes :: Text -> RecordMarkerDecisionAttributes
recordMarkerDecisionAttributes pMarkerName =
    RecordMarkerDecisionAttributes'
    { _rmdaDetails = Nothing
    , _rmdaMarkerName = pMarkerName
    }

-- | /Optional./ details of the marker.
rmdaDetails :: Lens' RecordMarkerDecisionAttributes (Maybe Text)
rmdaDetails = lens _rmdaDetails (\ s a -> s{_rmdaDetails = a});

-- | __Required.__ The name of the marker.
rmdaMarkerName :: Lens' RecordMarkerDecisionAttributes Text
rmdaMarkerName = lens _rmdaMarkerName (\ s a -> s{_rmdaMarkerName = a});

instance ToJSON RecordMarkerDecisionAttributes where
        toJSON RecordMarkerDecisionAttributes'{..}
          = object
              ["details" .= _rmdaDetails,
               "markerName" .= _rmdaMarkerName]

-- | Provides details of the @RecordMarkerFailed@ event.
--
-- /See:/ 'recordMarkerFailedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rmfeaMarkerName'
--
-- * 'rmfeaCause'
--
-- * 'rmfeaDecisionTaskCompletedEventId'
data RecordMarkerFailedEventAttributes = RecordMarkerFailedEventAttributes'
    { _rmfeaMarkerName                   :: Text
    , _rmfeaCause                        :: RecordMarkerFailedCause
    , _rmfeaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'RecordMarkerFailedEventAttributes' smart constructor.
recordMarkerFailedEventAttributes :: Text -> RecordMarkerFailedCause -> Integer -> RecordMarkerFailedEventAttributes
recordMarkerFailedEventAttributes pMarkerName pCause pDecisionTaskCompletedEventId =
    RecordMarkerFailedEventAttributes'
    { _rmfeaMarkerName = pMarkerName
    , _rmfeaCause = pCause
    , _rmfeaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | The marker\'s name.
rmfeaMarkerName :: Lens' RecordMarkerFailedEventAttributes Text
rmfeaMarkerName = lens _rmfeaMarkerName (\ s a -> s{_rmfeaMarkerName = a});

-- | The cause of the failure. This information is generated by the system
-- and can be useful for diagnostic purposes.
--
-- If __cause__ is set to OPERATION_NOT_PERMITTED, the decision failed
-- because it lacked sufficient permissions. For details and example IAM
-- policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
rmfeaCause :: Lens' RecordMarkerFailedEventAttributes RecordMarkerFailedCause
rmfeaCause = lens _rmfeaCause (\ s a -> s{_rmfeaCause = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @RecordMarkerFailed@ decision for
-- this cancellation request. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
rmfeaDecisionTaskCompletedEventId :: Lens' RecordMarkerFailedEventAttributes Integer
rmfeaDecisionTaskCompletedEventId = lens _rmfeaDecisionTaskCompletedEventId (\ s a -> s{_rmfeaDecisionTaskCompletedEventId = a});

instance FromJSON RecordMarkerFailedEventAttributes
         where
        parseJSON
          = withObject "RecordMarkerFailedEventAttributes"
              (\ x ->
                 RecordMarkerFailedEventAttributes' <$>
                   (x .: "markerName") <*> (x .: "cause") <*>
                     (x .: "decisionTaskCompletedEventId"))

-- | Provides details of the @RequestCancelActivityTask@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- /See:/ 'requestCancelActivityTaskDecisionAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcatdaActivityId'
newtype RequestCancelActivityTaskDecisionAttributes = RequestCancelActivityTaskDecisionAttributes'
    { _rcatdaActivityId :: Text
    } deriving (Eq,Read,Show)

-- | 'RequestCancelActivityTaskDecisionAttributes' smart constructor.
requestCancelActivityTaskDecisionAttributes :: Text -> RequestCancelActivityTaskDecisionAttributes
requestCancelActivityTaskDecisionAttributes pActivityId =
    RequestCancelActivityTaskDecisionAttributes'
    { _rcatdaActivityId = pActivityId
    }

-- | The @activityId@ of the activity task to be canceled.
rcatdaActivityId :: Lens' RequestCancelActivityTaskDecisionAttributes Text
rcatdaActivityId = lens _rcatdaActivityId (\ s a -> s{_rcatdaActivityId = a});

instance ToJSON
         RequestCancelActivityTaskDecisionAttributes where
        toJSON
          RequestCancelActivityTaskDecisionAttributes'{..}
          = object ["activityId" .= _rcatdaActivityId]

-- | Provides details of the @RequestCancelActivityTaskFailed@ event.
--
-- /See:/ 'requestCancelActivityTaskFailedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcatfeaActivityId'
--
-- * 'rcatfeaCause'
--
-- * 'rcatfeaDecisionTaskCompletedEventId'
data RequestCancelActivityTaskFailedEventAttributes = RequestCancelActivityTaskFailedEventAttributes'
    { _rcatfeaActivityId                   :: Text
    , _rcatfeaCause                        :: RequestCancelActivityTaskFailedCause
    , _rcatfeaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'RequestCancelActivityTaskFailedEventAttributes' smart constructor.
requestCancelActivityTaskFailedEventAttributes :: Text -> RequestCancelActivityTaskFailedCause -> Integer -> RequestCancelActivityTaskFailedEventAttributes
requestCancelActivityTaskFailedEventAttributes pActivityId pCause pDecisionTaskCompletedEventId =
    RequestCancelActivityTaskFailedEventAttributes'
    { _rcatfeaActivityId = pActivityId
    , _rcatfeaCause = pCause
    , _rcatfeaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | The activityId provided in the @RequestCancelActivityTask@ decision that
-- failed.
rcatfeaActivityId :: Lens' RequestCancelActivityTaskFailedEventAttributes Text
rcatfeaActivityId = lens _rcatfeaActivityId (\ s a -> s{_rcatfeaActivityId = a});

-- | The cause of the failure. This information is generated by the system
-- and can be useful for diagnostic purposes.
--
-- If __cause__ is set to OPERATION_NOT_PERMITTED, the decision failed
-- because it lacked sufficient permissions. For details and example IAM
-- policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
rcatfeaCause :: Lens' RequestCancelActivityTaskFailedEventAttributes RequestCancelActivityTaskFailedCause
rcatfeaCause = lens _rcatfeaCause (\ s a -> s{_rcatfeaCause = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @RequestCancelActivityTask@ decision
-- for this cancellation request. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
rcatfeaDecisionTaskCompletedEventId :: Lens' RequestCancelActivityTaskFailedEventAttributes Integer
rcatfeaDecisionTaskCompletedEventId = lens _rcatfeaDecisionTaskCompletedEventId (\ s a -> s{_rcatfeaDecisionTaskCompletedEventId = a});

instance FromJSON
         RequestCancelActivityTaskFailedEventAttributes where
        parseJSON
          = withObject
              "RequestCancelActivityTaskFailedEventAttributes"
              (\ x ->
                 RequestCancelActivityTaskFailedEventAttributes' <$>
                   (x .: "activityId") <*> (x .: "cause") <*>
                     (x .: "decisionTaskCompletedEventId"))

-- | Provides details of the @RequestCancelExternalWorkflowExecution@
-- decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- /See:/ 'requestCancelExternalWorkflowExecutionDecisionAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcewedaControl'
--
-- * 'rcewedaRunId'
--
-- * 'rcewedaWorkflowId'
data RequestCancelExternalWorkflowExecutionDecisionAttributes = RequestCancelExternalWorkflowExecutionDecisionAttributes'
    { _rcewedaControl    :: Maybe Text
    , _rcewedaRunId      :: Maybe Text
    , _rcewedaWorkflowId :: Text
    } deriving (Eq,Read,Show)

-- | 'RequestCancelExternalWorkflowExecutionDecisionAttributes' smart constructor.
requestCancelExternalWorkflowExecutionDecisionAttributes :: Text -> RequestCancelExternalWorkflowExecutionDecisionAttributes
requestCancelExternalWorkflowExecutionDecisionAttributes pWorkflowId =
    RequestCancelExternalWorkflowExecutionDecisionAttributes'
    { _rcewedaControl = Nothing
    , _rcewedaRunId = Nothing
    , _rcewedaWorkflowId = pWorkflowId
    }

-- | /Optional./ Data attached to the event that can be used by the decider
-- in subsequent workflow tasks.
rcewedaControl :: Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Maybe Text)
rcewedaControl = lens _rcewedaControl (\ s a -> s{_rcewedaControl = a});

-- | The @runId@ of the external workflow execution to cancel.
rcewedaRunId :: Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Maybe Text)
rcewedaRunId = lens _rcewedaRunId (\ s a -> s{_rcewedaRunId = a});

-- | __Required.__ The @workflowId@ of the external workflow execution to
-- cancel.
rcewedaWorkflowId :: Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes Text
rcewedaWorkflowId = lens _rcewedaWorkflowId (\ s a -> s{_rcewedaWorkflowId = a});

instance ToJSON
         RequestCancelExternalWorkflowExecutionDecisionAttributes
         where
        toJSON
          RequestCancelExternalWorkflowExecutionDecisionAttributes'{..}
          = object
              ["control" .= _rcewedaControl,
               "runId" .= _rcewedaRunId,
               "workflowId" .= _rcewedaWorkflowId]

-- | Provides details of the @RequestCancelExternalWorkflowExecutionFailed@
-- event.
--
-- /See:/ 'requestCancelExternalWorkflowExecutionFailedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcewefeaControl'
--
-- * 'rcewefeaRunId'
--
-- * 'rcewefeaWorkflowId'
--
-- * 'rcewefeaCause'
--
-- * 'rcewefeaInitiatedEventId'
--
-- * 'rcewefeaDecisionTaskCompletedEventId'
data RequestCancelExternalWorkflowExecutionFailedEventAttributes = RequestCancelExternalWorkflowExecutionFailedEventAttributes'
    { _rcewefeaControl                      :: Maybe Text
    , _rcewefeaRunId                        :: Maybe Text
    , _rcewefeaWorkflowId                   :: Text
    , _rcewefeaCause                        :: RequestCancelExternalWorkflowExecutionFailedCause
    , _rcewefeaInitiatedEventId             :: !Integer
    , _rcewefeaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'RequestCancelExternalWorkflowExecutionFailedEventAttributes' smart constructor.
requestCancelExternalWorkflowExecutionFailedEventAttributes :: Text -> RequestCancelExternalWorkflowExecutionFailedCause -> Integer -> Integer -> RequestCancelExternalWorkflowExecutionFailedEventAttributes
requestCancelExternalWorkflowExecutionFailedEventAttributes pWorkflowId pCause pInitiatedEventId pDecisionTaskCompletedEventId =
    RequestCancelExternalWorkflowExecutionFailedEventAttributes'
    { _rcewefeaControl = Nothing
    , _rcewefeaRunId = Nothing
    , _rcewefeaWorkflowId = pWorkflowId
    , _rcewefeaCause = pCause
    , _rcewefeaInitiatedEventId = pInitiatedEventId
    , _rcewefeaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | FIXME: Undocumented member.
rcewefeaControl :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
rcewefeaControl = lens _rcewefeaControl (\ s a -> s{_rcewefeaControl = a});

-- | The @runId@ of the external workflow execution.
rcewefeaRunId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
rcewefeaRunId = lens _rcewefeaRunId (\ s a -> s{_rcewefeaRunId = a});

-- | The @workflowId@ of the external workflow to which the cancel request
-- was to be delivered.
rcewefeaWorkflowId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes Text
rcewefeaWorkflowId = lens _rcewefeaWorkflowId (\ s a -> s{_rcewefeaWorkflowId = a});

-- | The cause of the failure. This information is generated by the system
-- and can be useful for diagnostic purposes.
--
-- If __cause__ is set to OPERATION_NOT_PERMITTED, the decision failed
-- because it lacked sufficient permissions. For details and example IAM
-- policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
rcewefeaCause :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes RequestCancelExternalWorkflowExecutionFailedCause
rcewefeaCause = lens _rcewefeaCause (\ s a -> s{_rcewefeaCause = a});

-- | The id of the @RequestCancelExternalWorkflowExecutionInitiated@ event
-- corresponding to the @RequestCancelExternalWorkflowExecution@ decision
-- to cancel this external workflow execution. This information can be
-- useful for diagnosing problems by tracing back the chain of events
-- leading up to this event.
rcewefeaInitiatedEventId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes Integer
rcewefeaInitiatedEventId = lens _rcewefeaInitiatedEventId (\ s a -> s{_rcewefeaInitiatedEventId = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the
-- @RequestCancelExternalWorkflowExecution@ decision for this cancellation
-- request. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
rcewefeaDecisionTaskCompletedEventId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes Integer
rcewefeaDecisionTaskCompletedEventId = lens _rcewefeaDecisionTaskCompletedEventId (\ s a -> s{_rcewefeaDecisionTaskCompletedEventId = a});

instance FromJSON
         RequestCancelExternalWorkflowExecutionFailedEventAttributes
         where
        parseJSON
          = withObject
              "RequestCancelExternalWorkflowExecutionFailedEventAttributes"
              (\ x ->
                 RequestCancelExternalWorkflowExecutionFailedEventAttributes'
                   <$>
                   (x .:? "control") <*> (x .:? "runId") <*>
                     (x .: "workflowId")
                     <*> (x .: "cause")
                     <*> (x .: "initiatedEventId")
                     <*> (x .: "decisionTaskCompletedEventId"))

-- | Provides details of the
-- @RequestCancelExternalWorkflowExecutionInitiated@ event.
--
-- /See:/ 'requestCancelExternalWorkflowExecutionInitiatedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rceweieaControl'
--
-- * 'rceweieaRunId'
--
-- * 'rceweieaWorkflowId'
--
-- * 'rceweieaDecisionTaskCompletedEventId'
data RequestCancelExternalWorkflowExecutionInitiatedEventAttributes = RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
    { _rceweieaControl                      :: Maybe Text
    , _rceweieaRunId                        :: Maybe Text
    , _rceweieaWorkflowId                   :: Text
    , _rceweieaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'RequestCancelExternalWorkflowExecutionInitiatedEventAttributes' smart constructor.
requestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Text -> Integer -> RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
requestCancelExternalWorkflowExecutionInitiatedEventAttributes pWorkflowId pDecisionTaskCompletedEventId =
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
    { _rceweieaControl = Nothing
    , _rceweieaRunId = Nothing
    , _rceweieaWorkflowId = pWorkflowId
    , _rceweieaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | /Optional./ Data attached to the event that can be used by the decider
-- in subsequent workflow tasks.
rceweieaControl :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
rceweieaControl = lens _rceweieaControl (\ s a -> s{_rceweieaControl = a});

-- | The @runId@ of the external workflow execution to be canceled.
rceweieaRunId :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
rceweieaRunId = lens _rceweieaRunId (\ s a -> s{_rceweieaRunId = a});

-- | The @workflowId@ of the external workflow execution to be canceled.
rceweieaWorkflowId :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes Text
rceweieaWorkflowId = lens _rceweieaWorkflowId (\ s a -> s{_rceweieaWorkflowId = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the
-- @RequestCancelExternalWorkflowExecution@ decision for this cancellation
-- request. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
rceweieaDecisionTaskCompletedEventId :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes Integer
rceweieaDecisionTaskCompletedEventId = lens _rceweieaDecisionTaskCompletedEventId (\ s a -> s{_rceweieaDecisionTaskCompletedEventId = a});

instance FromJSON
         RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
         where
        parseJSON
          = withObject
              "RequestCancelExternalWorkflowExecutionInitiatedEventAttributes"
              (\ x ->
                 RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
                   <$>
                   (x .:? "control") <*> (x .:? "runId") <*>
                     (x .: "workflowId")
                     <*> (x .: "decisionTaskCompletedEventId"))

-- | Provides details of the @ScheduleActivityTask@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--     -   @activityType.name@: String constraint. The key is
--         @swf:activityType.name@.
--     -   @activityType.version@: String constraint. The key is
--         @swf:activityType.version@.
--     -   @taskList@: String constraint. The key is @swf:taskList.name@.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- /See:/ 'scheduleActivityTaskDecisionAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'satdaControl'
--
-- * 'satdaScheduleToCloseTimeout'
--
-- * 'satdaHeartbeatTimeout'
--
-- * 'satdaInput'
--
-- * 'satdaTaskList'
--
-- * 'satdaTaskPriority'
--
-- * 'satdaScheduleToStartTimeout'
--
-- * 'satdaStartToCloseTimeout'
--
-- * 'satdaActivityType'
--
-- * 'satdaActivityId'
data ScheduleActivityTaskDecisionAttributes = ScheduleActivityTaskDecisionAttributes'
    { _satdaControl                :: Maybe Text
    , _satdaScheduleToCloseTimeout :: Maybe Text
    , _satdaHeartbeatTimeout       :: Maybe Text
    , _satdaInput                  :: Maybe Text
    , _satdaTaskList               :: Maybe TaskList
    , _satdaTaskPriority           :: Maybe Text
    , _satdaScheduleToStartTimeout :: Maybe Text
    , _satdaStartToCloseTimeout    :: Maybe Text
    , _satdaActivityType           :: ActivityType
    , _satdaActivityId             :: Text
    } deriving (Eq,Read,Show)

-- | 'ScheduleActivityTaskDecisionAttributes' smart constructor.
scheduleActivityTaskDecisionAttributes :: ActivityType -> Text -> ScheduleActivityTaskDecisionAttributes
scheduleActivityTaskDecisionAttributes pActivityType pActivityId =
    ScheduleActivityTaskDecisionAttributes'
    { _satdaControl = Nothing
    , _satdaScheduleToCloseTimeout = Nothing
    , _satdaHeartbeatTimeout = Nothing
    , _satdaInput = Nothing
    , _satdaTaskList = Nothing
    , _satdaTaskPriority = Nothing
    , _satdaScheduleToStartTimeout = Nothing
    , _satdaStartToCloseTimeout = Nothing
    , _satdaActivityType = pActivityType
    , _satdaActivityId = pActivityId
    }

-- | /Optional./ Data attached to the event that can be used by the decider
-- in subsequent workflow tasks. This data is not sent to the activity.
satdaControl :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaControl = lens _satdaControl (\ s a -> s{_satdaControl = a});

-- | The maximum duration for this activity task.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
--
-- A schedule-to-close timeout for this activity task must be specified
-- either as a default for the activity type or through this field. If
-- neither this field is set nor a default schedule-to-close timeout was
-- specified at registration time then a fault will be returned.
satdaScheduleToCloseTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaScheduleToCloseTimeout = lens _satdaScheduleToCloseTimeout (\ s a -> s{_satdaScheduleToCloseTimeout = a});

-- | If set, specifies the maximum time before which a worker processing a
-- task of this type must report progress by calling
-- RecordActivityTaskHeartbeat. If the timeout is exceeded, the activity
-- task is automatically timed out. If the worker subsequently attempts to
-- record a heartbeat or returns a result, it will be ignored. This
-- overrides the default heartbeat timeout specified when registering the
-- activity type using RegisterActivityType.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
satdaHeartbeatTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaHeartbeatTimeout = lens _satdaHeartbeatTimeout (\ s a -> s{_satdaHeartbeatTimeout = a});

-- | The input provided to the activity task.
satdaInput :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaInput = lens _satdaInput (\ s a -> s{_satdaInput = a});

-- | If set, specifies the name of the task list in which to schedule the
-- activity task. If not specified, the @defaultTaskList@ registered with
-- the activity type will be used.
--
-- A task list for this activity task must be specified either as a default
-- for the activity type or through this field. If neither this field is
-- set nor a default task list was specified at registration time then a
-- fault will be returned.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
satdaTaskList :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe TaskList)
satdaTaskList = lens _satdaTaskList (\ s a -> s{_satdaTaskList = a});

-- | /Optional./ If set, specifies the priority with which the activity task
-- is to be assigned to a worker. This overrides the defaultTaskPriority
-- specified when registering the activity type using RegisterActivityType.
-- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon Simple Workflow Developer Guide/.
satdaTaskPriority :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaTaskPriority = lens _satdaTaskPriority (\ s a -> s{_satdaTaskPriority = a});

-- | /Optional./ If set, specifies the maximum duration the activity task can
-- wait to be assigned to a worker. This overrides the default
-- schedule-to-start timeout specified when registering the activity type
-- using RegisterActivityType.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
--
-- A schedule-to-start timeout for this activity task must be specified
-- either as a default for the activity type or through this field. If
-- neither this field is set nor a default schedule-to-start timeout was
-- specified at registration time then a fault will be returned.
satdaScheduleToStartTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaScheduleToStartTimeout = lens _satdaScheduleToStartTimeout (\ s a -> s{_satdaScheduleToStartTimeout = a});

-- | If set, specifies the maximum duration a worker may take to process this
-- activity task. This overrides the default start-to-close timeout
-- specified when registering the activity type using RegisterActivityType.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
--
-- A start-to-close timeout for this activity task must be specified either
-- as a default for the activity type or through this field. If neither
-- this field is set nor a default start-to-close timeout was specified at
-- registration time then a fault will be returned.
satdaStartToCloseTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaStartToCloseTimeout = lens _satdaStartToCloseTimeout (\ s a -> s{_satdaStartToCloseTimeout = a});

-- | __Required.__ The type of the activity task to schedule.
satdaActivityType :: Lens' ScheduleActivityTaskDecisionAttributes ActivityType
satdaActivityType = lens _satdaActivityType (\ s a -> s{_satdaActivityType = a});

-- | __Required.__ The @activityId@ of the activity task.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
satdaActivityId :: Lens' ScheduleActivityTaskDecisionAttributes Text
satdaActivityId = lens _satdaActivityId (\ s a -> s{_satdaActivityId = a});

instance ToJSON
         ScheduleActivityTaskDecisionAttributes where
        toJSON ScheduleActivityTaskDecisionAttributes'{..}
          = object
              ["control" .= _satdaControl,
               "scheduleToCloseTimeout" .=
                 _satdaScheduleToCloseTimeout,
               "heartbeatTimeout" .= _satdaHeartbeatTimeout,
               "input" .= _satdaInput, "taskList" .= _satdaTaskList,
               "taskPriority" .= _satdaTaskPriority,
               "scheduleToStartTimeout" .=
                 _satdaScheduleToStartTimeout,
               "startToCloseTimeout" .= _satdaStartToCloseTimeout,
               "activityType" .= _satdaActivityType,
               "activityId" .= _satdaActivityId]

-- | Provides details of the @ScheduleActivityTaskFailed@ event.
--
-- /See:/ 'scheduleActivityTaskFailedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'satfeaActivityType'
--
-- * 'satfeaActivityId'
--
-- * 'satfeaCause'
--
-- * 'satfeaDecisionTaskCompletedEventId'
data ScheduleActivityTaskFailedEventAttributes = ScheduleActivityTaskFailedEventAttributes'
    { _satfeaActivityType                 :: ActivityType
    , _satfeaActivityId                   :: Text
    , _satfeaCause                        :: ScheduleActivityTaskFailedCause
    , _satfeaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'ScheduleActivityTaskFailedEventAttributes' smart constructor.
scheduleActivityTaskFailedEventAttributes :: ActivityType -> Text -> ScheduleActivityTaskFailedCause -> Integer -> ScheduleActivityTaskFailedEventAttributes
scheduleActivityTaskFailedEventAttributes pActivityType pActivityId pCause pDecisionTaskCompletedEventId =
    ScheduleActivityTaskFailedEventAttributes'
    { _satfeaActivityType = pActivityType
    , _satfeaActivityId = pActivityId
    , _satfeaCause = pCause
    , _satfeaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | The activity type provided in the @ScheduleActivityTask@ decision that
-- failed.
satfeaActivityType :: Lens' ScheduleActivityTaskFailedEventAttributes ActivityType
satfeaActivityType = lens _satfeaActivityType (\ s a -> s{_satfeaActivityType = a});

-- | The activityId provided in the @ScheduleActivityTask@ decision that
-- failed.
satfeaActivityId :: Lens' ScheduleActivityTaskFailedEventAttributes Text
satfeaActivityId = lens _satfeaActivityId (\ s a -> s{_satfeaActivityId = a});

-- | The cause of the failure. This information is generated by the system
-- and can be useful for diagnostic purposes.
--
-- If __cause__ is set to OPERATION_NOT_PERMITTED, the decision failed
-- because it lacked sufficient permissions. For details and example IAM
-- policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
satfeaCause :: Lens' ScheduleActivityTaskFailedEventAttributes ScheduleActivityTaskFailedCause
satfeaCause = lens _satfeaCause (\ s a -> s{_satfeaCause = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision that resulted in the scheduling of this activity task. This
-- information can be useful for diagnosing problems by tracing back the
-- chain of events leading up to this event.
satfeaDecisionTaskCompletedEventId :: Lens' ScheduleActivityTaskFailedEventAttributes Integer
satfeaDecisionTaskCompletedEventId = lens _satfeaDecisionTaskCompletedEventId (\ s a -> s{_satfeaDecisionTaskCompletedEventId = a});

instance FromJSON
         ScheduleActivityTaskFailedEventAttributes where
        parseJSON
          = withObject
              "ScheduleActivityTaskFailedEventAttributes"
              (\ x ->
                 ScheduleActivityTaskFailedEventAttributes' <$>
                   (x .: "activityType") <*> (x .: "activityId") <*>
                     (x .: "cause")
                     <*> (x .: "decisionTaskCompletedEventId"))

-- | Provides details of the @SignalExternalWorkflowExecution@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- /See:/ 'signalExternalWorkflowExecutionDecisionAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sewedaControl'
--
-- * 'sewedaInput'
--
-- * 'sewedaRunId'
--
-- * 'sewedaWorkflowId'
--
-- * 'sewedaSignalName'
data SignalExternalWorkflowExecutionDecisionAttributes = SignalExternalWorkflowExecutionDecisionAttributes'
    { _sewedaControl    :: Maybe Text
    , _sewedaInput      :: Maybe Text
    , _sewedaRunId      :: Maybe Text
    , _sewedaWorkflowId :: Text
    , _sewedaSignalName :: Text
    } deriving (Eq,Read,Show)

-- | 'SignalExternalWorkflowExecutionDecisionAttributes' smart constructor.
signalExternalWorkflowExecutionDecisionAttributes :: Text -> Text -> SignalExternalWorkflowExecutionDecisionAttributes
signalExternalWorkflowExecutionDecisionAttributes pWorkflowId pSignalName =
    SignalExternalWorkflowExecutionDecisionAttributes'
    { _sewedaControl = Nothing
    , _sewedaInput = Nothing
    , _sewedaRunId = Nothing
    , _sewedaWorkflowId = pWorkflowId
    , _sewedaSignalName = pSignalName
    }

-- | /Optional./ Data attached to the event that can be used by the decider
-- in subsequent decision tasks.
sewedaControl :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Maybe Text)
sewedaControl = lens _sewedaControl (\ s a -> s{_sewedaControl = a});

-- | /Optional./ Input data to be provided with the signal. The target
-- workflow execution will use the signal name and input data to process
-- the signal.
sewedaInput :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Maybe Text)
sewedaInput = lens _sewedaInput (\ s a -> s{_sewedaInput = a});

-- | The @runId@ of the workflow execution to be signaled.
sewedaRunId :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Maybe Text)
sewedaRunId = lens _sewedaRunId (\ s a -> s{_sewedaRunId = a});

-- | __Required.__ The @workflowId@ of the workflow execution to be signaled.
sewedaWorkflowId :: Lens' SignalExternalWorkflowExecutionDecisionAttributes Text
sewedaWorkflowId = lens _sewedaWorkflowId (\ s a -> s{_sewedaWorkflowId = a});

-- | __Required.__ The name of the signal.The target workflow execution will
-- use the signal name and input to process the signal.
sewedaSignalName :: Lens' SignalExternalWorkflowExecutionDecisionAttributes Text
sewedaSignalName = lens _sewedaSignalName (\ s a -> s{_sewedaSignalName = a});

instance ToJSON
         SignalExternalWorkflowExecutionDecisionAttributes
         where
        toJSON
          SignalExternalWorkflowExecutionDecisionAttributes'{..}
          = object
              ["control" .= _sewedaControl,
               "input" .= _sewedaInput, "runId" .= _sewedaRunId,
               "workflowId" .= _sewedaWorkflowId,
               "signalName" .= _sewedaSignalName]

-- | Provides details of the @SignalExternalWorkflowExecutionFailed@ event.
--
-- /See:/ 'signalExternalWorkflowExecutionFailedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sewefeaControl'
--
-- * 'sewefeaRunId'
--
-- * 'sewefeaWorkflowId'
--
-- * 'sewefeaCause'
--
-- * 'sewefeaInitiatedEventId'
--
-- * 'sewefeaDecisionTaskCompletedEventId'
data SignalExternalWorkflowExecutionFailedEventAttributes = SignalExternalWorkflowExecutionFailedEventAttributes'
    { _sewefeaControl                      :: Maybe Text
    , _sewefeaRunId                        :: Maybe Text
    , _sewefeaWorkflowId                   :: Text
    , _sewefeaCause                        :: SignalExternalWorkflowExecutionFailedCause
    , _sewefeaInitiatedEventId             :: !Integer
    , _sewefeaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'SignalExternalWorkflowExecutionFailedEventAttributes' smart constructor.
signalExternalWorkflowExecutionFailedEventAttributes :: Text -> SignalExternalWorkflowExecutionFailedCause -> Integer -> Integer -> SignalExternalWorkflowExecutionFailedEventAttributes
signalExternalWorkflowExecutionFailedEventAttributes pWorkflowId pCause pInitiatedEventId pDecisionTaskCompletedEventId =
    SignalExternalWorkflowExecutionFailedEventAttributes'
    { _sewefeaControl = Nothing
    , _sewefeaRunId = Nothing
    , _sewefeaWorkflowId = pWorkflowId
    , _sewefeaCause = pCause
    , _sewefeaInitiatedEventId = pInitiatedEventId
    , _sewefeaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | FIXME: Undocumented member.
sewefeaControl :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
sewefeaControl = lens _sewefeaControl (\ s a -> s{_sewefeaControl = a});

-- | The @runId@ of the external workflow execution that the signal was being
-- delivered to.
sewefeaRunId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
sewefeaRunId = lens _sewefeaRunId (\ s a -> s{_sewefeaRunId = a});

-- | The @workflowId@ of the external workflow execution that the signal was
-- being delivered to.
sewefeaWorkflowId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes Text
sewefeaWorkflowId = lens _sewefeaWorkflowId (\ s a -> s{_sewefeaWorkflowId = a});

-- | The cause of the failure. This information is generated by the system
-- and can be useful for diagnostic purposes.
--
-- If __cause__ is set to OPERATION_NOT_PERMITTED, the decision failed
-- because it lacked sufficient permissions. For details and example IAM
-- policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
sewefeaCause :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes SignalExternalWorkflowExecutionFailedCause
sewefeaCause = lens _sewefeaCause (\ s a -> s{_sewefeaCause = a});

-- | The id of the @SignalExternalWorkflowExecutionInitiated@ event
-- corresponding to the @SignalExternalWorkflowExecution@ decision to
-- request this signal. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
sewefeaInitiatedEventId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes Integer
sewefeaInitiatedEventId = lens _sewefeaInitiatedEventId (\ s a -> s{_sewefeaInitiatedEventId = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @SignalExternalWorkflowExecution@
-- decision for this signal. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
sewefeaDecisionTaskCompletedEventId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes Integer
sewefeaDecisionTaskCompletedEventId = lens _sewefeaDecisionTaskCompletedEventId (\ s a -> s{_sewefeaDecisionTaskCompletedEventId = a});

instance FromJSON
         SignalExternalWorkflowExecutionFailedEventAttributes
         where
        parseJSON
          = withObject
              "SignalExternalWorkflowExecutionFailedEventAttributes"
              (\ x ->
                 SignalExternalWorkflowExecutionFailedEventAttributes'
                   <$>
                   (x .:? "control") <*> (x .:? "runId") <*>
                     (x .: "workflowId")
                     <*> (x .: "cause")
                     <*> (x .: "initiatedEventId")
                     <*> (x .: "decisionTaskCompletedEventId"))

-- | Provides details of the @SignalExternalWorkflowExecutionInitiated@
-- event.
--
-- /See:/ 'signalExternalWorkflowExecutionInitiatedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'seweieaControl'
--
-- * 'seweieaInput'
--
-- * 'seweieaRunId'
--
-- * 'seweieaWorkflowId'
--
-- * 'seweieaSignalName'
--
-- * 'seweieaDecisionTaskCompletedEventId'
data SignalExternalWorkflowExecutionInitiatedEventAttributes = SignalExternalWorkflowExecutionInitiatedEventAttributes'
    { _seweieaControl                      :: Maybe Text
    , _seweieaInput                        :: Maybe Text
    , _seweieaRunId                        :: Maybe Text
    , _seweieaWorkflowId                   :: Text
    , _seweieaSignalName                   :: Text
    , _seweieaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'SignalExternalWorkflowExecutionInitiatedEventAttributes' smart constructor.
signalExternalWorkflowExecutionInitiatedEventAttributes :: Text -> Text -> Integer -> SignalExternalWorkflowExecutionInitiatedEventAttributes
signalExternalWorkflowExecutionInitiatedEventAttributes pWorkflowId pSignalName pDecisionTaskCompletedEventId =
    SignalExternalWorkflowExecutionInitiatedEventAttributes'
    { _seweieaControl = Nothing
    , _seweieaInput = Nothing
    , _seweieaRunId = Nothing
    , _seweieaWorkflowId = pWorkflowId
    , _seweieaSignalName = pSignalName
    , _seweieaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | /Optional./ data attached to the event that can be used by the decider
-- in subsequent decision tasks.
seweieaControl :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
seweieaControl = lens _seweieaControl (\ s a -> s{_seweieaControl = a});

-- | Input provided to the signal (if any).
seweieaInput :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
seweieaInput = lens _seweieaInput (\ s a -> s{_seweieaInput = a});

-- | The @runId@ of the external workflow execution to send the signal to.
seweieaRunId :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
seweieaRunId = lens _seweieaRunId (\ s a -> s{_seweieaRunId = a});

-- | The @workflowId@ of the external workflow execution.
seweieaWorkflowId :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Text
seweieaWorkflowId = lens _seweieaWorkflowId (\ s a -> s{_seweieaWorkflowId = a});

-- | The name of the signal.
seweieaSignalName :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Text
seweieaSignalName = lens _seweieaSignalName (\ s a -> s{_seweieaSignalName = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @SignalExternalWorkflowExecution@
-- decision for this signal. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
seweieaDecisionTaskCompletedEventId :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Integer
seweieaDecisionTaskCompletedEventId = lens _seweieaDecisionTaskCompletedEventId (\ s a -> s{_seweieaDecisionTaskCompletedEventId = a});

instance FromJSON
         SignalExternalWorkflowExecutionInitiatedEventAttributes
         where
        parseJSON
          = withObject
              "SignalExternalWorkflowExecutionInitiatedEventAttributes"
              (\ x ->
                 SignalExternalWorkflowExecutionInitiatedEventAttributes'
                   <$>
                   (x .:? "control") <*> (x .:? "input") <*>
                     (x .:? "runId")
                     <*> (x .: "workflowId")
                     <*> (x .: "signalName")
                     <*> (x .: "decisionTaskCompletedEventId"))

-- | Provides details of the @StartChildWorkflowExecution@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--     -   @tagList.member.N@: The key is \"swf:tagList.N\" where N is the
--         tag number from 0 to 4, inclusive.
--     -   @taskList@: String constraint. The key is @swf:taskList.name@.
--     -   @workflowType.name@: String constraint. The key is
--         @swf:workflowType.name@.
--     -   @workflowType.version@: String constraint. The key is
--         @swf:workflowType.version@.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- /See:/ 'startChildWorkflowExecutionDecisionAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scwedaControl'
--
-- * 'scwedaTagList'
--
-- * 'scwedaTaskStartToCloseTimeout'
--
-- * 'scwedaInput'
--
-- * 'scwedaExecutionStartToCloseTimeout'
--
-- * 'scwedaTaskList'
--
-- * 'scwedaTaskPriority'
--
-- * 'scwedaChildPolicy'
--
-- * 'scwedaWorkflowType'
--
-- * 'scwedaWorkflowId'
data StartChildWorkflowExecutionDecisionAttributes = StartChildWorkflowExecutionDecisionAttributes'
    { _scwedaControl                      :: Maybe Text
    , _scwedaTagList                      :: Maybe [Text]
    , _scwedaTaskStartToCloseTimeout      :: Maybe Text
    , _scwedaInput                        :: Maybe Text
    , _scwedaExecutionStartToCloseTimeout :: Maybe Text
    , _scwedaTaskList                     :: Maybe TaskList
    , _scwedaTaskPriority                 :: Maybe Text
    , _scwedaChildPolicy                  :: Maybe ChildPolicy
    , _scwedaWorkflowType                 :: WorkflowType
    , _scwedaWorkflowId                   :: Text
    } deriving (Eq,Read,Show)

-- | 'StartChildWorkflowExecutionDecisionAttributes' smart constructor.
startChildWorkflowExecutionDecisionAttributes :: WorkflowType -> Text -> StartChildWorkflowExecutionDecisionAttributes
startChildWorkflowExecutionDecisionAttributes pWorkflowType pWorkflowId =
    StartChildWorkflowExecutionDecisionAttributes'
    { _scwedaControl = Nothing
    , _scwedaTagList = Nothing
    , _scwedaTaskStartToCloseTimeout = Nothing
    , _scwedaInput = Nothing
    , _scwedaExecutionStartToCloseTimeout = Nothing
    , _scwedaTaskList = Nothing
    , _scwedaTaskPriority = Nothing
    , _scwedaChildPolicy = Nothing
    , _scwedaWorkflowType = pWorkflowType
    , _scwedaWorkflowId = pWorkflowId
    }

-- | /Optional./ Data attached to the event that can be used by the decider
-- in subsequent workflow tasks. This data is not sent to the child
-- workflow execution.
scwedaControl :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaControl = lens _scwedaControl (\ s a -> s{_scwedaControl = a});

-- | The list of tags to associate with the child workflow execution. A
-- maximum of 5 tags can be specified. You can list workflow executions
-- with a specific tag by calling ListOpenWorkflowExecutions or
-- ListClosedWorkflowExecutions and specifying a TagFilter.
scwedaTagList :: Lens' StartChildWorkflowExecutionDecisionAttributes [Text]
scwedaTagList = lens _scwedaTagList (\ s a -> s{_scwedaTagList = a}) . _Default;

-- | Specifies the maximum duration of decision tasks for this workflow
-- execution. This parameter overrides the @defaultTaskStartToCloseTimout@
-- specified when registering the workflow type using RegisterWorkflowType.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
--
-- A task start-to-close timeout for this workflow execution must be
-- specified either as a default for the workflow type or through this
-- parameter. If neither this parameter is set nor a default task
-- start-to-close timeout was specified at registration time then a fault
-- will be returned.
scwedaTaskStartToCloseTimeout :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaTaskStartToCloseTimeout = lens _scwedaTaskStartToCloseTimeout (\ s a -> s{_scwedaTaskStartToCloseTimeout = a});

-- | The input to be provided to the workflow execution.
scwedaInput :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaInput = lens _scwedaInput (\ s a -> s{_scwedaInput = a});

-- | The total duration for this workflow execution. This overrides the
-- defaultExecutionStartToCloseTimeout specified when registering the
-- workflow type.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
--
-- An execution start-to-close timeout for this workflow execution must be
-- specified either as a default for the workflow type or through this
-- parameter. If neither this parameter is set nor a default execution
-- start-to-close timeout was specified at registration time then a fault
-- will be returned.
scwedaExecutionStartToCloseTimeout :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaExecutionStartToCloseTimeout = lens _scwedaExecutionStartToCloseTimeout (\ s a -> s{_scwedaExecutionStartToCloseTimeout = a});

-- | The name of the task list to be used for decision tasks of the child
-- workflow execution.
--
-- A task list for this workflow execution must be specified either as a
-- default for the workflow type or through this parameter. If neither this
-- parameter is set nor a default task list was specified at registration
-- time then a fault will be returned.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
scwedaTaskList :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe TaskList)
scwedaTaskList = lens _scwedaTaskList (\ s a -> s{_scwedaTaskList = a});

-- | /Optional./ A task priority that, if set, specifies the priority for a
-- decision task of this workflow execution. This overrides the
-- defaultTaskPriority specified when registering the workflow type. Valid
-- values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon Simple Workflow Developer Guide/.
scwedaTaskPriority :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaTaskPriority = lens _scwedaTaskPriority (\ s a -> s{_scwedaTaskPriority = a});

-- | /Optional./ If set, specifies the policy to use for the child workflow
-- executions if the workflow execution being started is terminated by
-- calling the TerminateWorkflowExecution action explicitly or due to an
-- expired timeout. This policy overrides the default child policy
-- specified when registering the workflow type using RegisterWorkflowType.
--
-- The supported child policies are:
--
-- -   __TERMINATE:__ the child executions will be terminated.
-- -   __REQUEST_CANCEL:__ a request to cancel will be attempted for each
--     child execution by recording a @WorkflowExecutionCancelRequested@
--     event in its history. It is up to the decider to take appropriate
--     actions when it receives an execution history with this event.
-- -   __ABANDON:__ no action will be taken. The child executions will
--     continue to run.
--
-- A child policy for this workflow execution must be specified either as a
-- default for the workflow type or through this parameter. If neither this
-- parameter is set nor a default child policy was specified at
-- registration time then a fault will be returned.
scwedaChildPolicy :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe ChildPolicy)
scwedaChildPolicy = lens _scwedaChildPolicy (\ s a -> s{_scwedaChildPolicy = a});

-- | __Required.__ The type of the workflow execution to be started.
scwedaWorkflowType :: Lens' StartChildWorkflowExecutionDecisionAttributes WorkflowType
scwedaWorkflowType = lens _scwedaWorkflowType (\ s a -> s{_scwedaWorkflowType = a});

-- | __Required.__ The @workflowId@ of the workflow execution.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
scwedaWorkflowId :: Lens' StartChildWorkflowExecutionDecisionAttributes Text
scwedaWorkflowId = lens _scwedaWorkflowId (\ s a -> s{_scwedaWorkflowId = a});

instance ToJSON
         StartChildWorkflowExecutionDecisionAttributes where
        toJSON
          StartChildWorkflowExecutionDecisionAttributes'{..}
          = object
              ["control" .= _scwedaControl,
               "tagList" .= _scwedaTagList,
               "taskStartToCloseTimeout" .=
                 _scwedaTaskStartToCloseTimeout,
               "input" .= _scwedaInput,
               "executionStartToCloseTimeout" .=
                 _scwedaExecutionStartToCloseTimeout,
               "taskList" .= _scwedaTaskList,
               "taskPriority" .= _scwedaTaskPriority,
               "childPolicy" .= _scwedaChildPolicy,
               "workflowType" .= _scwedaWorkflowType,
               "workflowId" .= _scwedaWorkflowId]

-- | Provides details of the @StartChildWorkflowExecutionFailed@ event.
--
-- /See:/ 'startChildWorkflowExecutionFailedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scwefeaControl'
--
-- * 'scwefeaWorkflowType'
--
-- * 'scwefeaCause'
--
-- * 'scwefeaWorkflowId'
--
-- * 'scwefeaInitiatedEventId'
--
-- * 'scwefeaDecisionTaskCompletedEventId'
data StartChildWorkflowExecutionFailedEventAttributes = StartChildWorkflowExecutionFailedEventAttributes'
    { _scwefeaControl                      :: Maybe Text
    , _scwefeaWorkflowType                 :: WorkflowType
    , _scwefeaCause                        :: StartChildWorkflowExecutionFailedCause
    , _scwefeaWorkflowId                   :: Text
    , _scwefeaInitiatedEventId             :: !Integer
    , _scwefeaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'StartChildWorkflowExecutionFailedEventAttributes' smart constructor.
startChildWorkflowExecutionFailedEventAttributes :: WorkflowType -> StartChildWorkflowExecutionFailedCause -> Text -> Integer -> Integer -> StartChildWorkflowExecutionFailedEventAttributes
startChildWorkflowExecutionFailedEventAttributes pWorkflowType pCause pWorkflowId pInitiatedEventId pDecisionTaskCompletedEventId =
    StartChildWorkflowExecutionFailedEventAttributes'
    { _scwefeaControl = Nothing
    , _scwefeaWorkflowType = pWorkflowType
    , _scwefeaCause = pCause
    , _scwefeaWorkflowId = pWorkflowId
    , _scwefeaInitiatedEventId = pInitiatedEventId
    , _scwefeaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | FIXME: Undocumented member.
scwefeaControl :: Lens' StartChildWorkflowExecutionFailedEventAttributes (Maybe Text)
scwefeaControl = lens _scwefeaControl (\ s a -> s{_scwefeaControl = a});

-- | The workflow type provided in the @StartChildWorkflowExecution@ Decision
-- that failed.
scwefeaWorkflowType :: Lens' StartChildWorkflowExecutionFailedEventAttributes WorkflowType
scwefeaWorkflowType = lens _scwefeaWorkflowType (\ s a -> s{_scwefeaWorkflowType = a});

-- | The cause of the failure. This information is generated by the system
-- and can be useful for diagnostic purposes.
--
-- If __cause__ is set to OPERATION_NOT_PERMITTED, the decision failed
-- because it lacked sufficient permissions. For details and example IAM
-- policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
scwefeaCause :: Lens' StartChildWorkflowExecutionFailedEventAttributes StartChildWorkflowExecutionFailedCause
scwefeaCause = lens _scwefeaCause (\ s a -> s{_scwefeaCause = a});

-- | The @workflowId@ of the child workflow execution.
scwefeaWorkflowId :: Lens' StartChildWorkflowExecutionFailedEventAttributes Text
scwefeaWorkflowId = lens _scwefeaWorkflowId (\ s a -> s{_scwefeaWorkflowId = a});

-- | The id of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this child
-- workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
scwefeaInitiatedEventId :: Lens' StartChildWorkflowExecutionFailedEventAttributes Integer
scwefeaInitiatedEventId = lens _scwefeaInitiatedEventId (\ s a -> s{_scwefeaInitiatedEventId = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @StartChildWorkflowExecution@
-- Decision to request this child workflow execution. This information can
-- be useful for diagnosing problems by tracing back the cause of events.
scwefeaDecisionTaskCompletedEventId :: Lens' StartChildWorkflowExecutionFailedEventAttributes Integer
scwefeaDecisionTaskCompletedEventId = lens _scwefeaDecisionTaskCompletedEventId (\ s a -> s{_scwefeaDecisionTaskCompletedEventId = a});

instance FromJSON
         StartChildWorkflowExecutionFailedEventAttributes
         where
        parseJSON
          = withObject
              "StartChildWorkflowExecutionFailedEventAttributes"
              (\ x ->
                 StartChildWorkflowExecutionFailedEventAttributes' <$>
                   (x .:? "control") <*> (x .: "workflowType") <*>
                     (x .: "cause")
                     <*> (x .: "workflowId")
                     <*> (x .: "initiatedEventId")
                     <*> (x .: "decisionTaskCompletedEventId"))

-- | Provides details of the @StartChildWorkflowExecutionInitiated@ event.
--
-- /See:/ 'startChildWorkflowExecutionInitiatedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scweieaControl'
--
-- * 'scweieaTagList'
--
-- * 'scweieaTaskStartToCloseTimeout'
--
-- * 'scweieaInput'
--
-- * 'scweieaExecutionStartToCloseTimeout'
--
-- * 'scweieaTaskPriority'
--
-- * 'scweieaWorkflowId'
--
-- * 'scweieaWorkflowType'
--
-- * 'scweieaTaskList'
--
-- * 'scweieaDecisionTaskCompletedEventId'
--
-- * 'scweieaChildPolicy'
data StartChildWorkflowExecutionInitiatedEventAttributes = StartChildWorkflowExecutionInitiatedEventAttributes'
    { _scweieaControl                      :: Maybe Text
    , _scweieaTagList                      :: Maybe [Text]
    , _scweieaTaskStartToCloseTimeout      :: Maybe Text
    , _scweieaInput                        :: Maybe Text
    , _scweieaExecutionStartToCloseTimeout :: Maybe Text
    , _scweieaTaskPriority                 :: Maybe Text
    , _scweieaWorkflowId                   :: Text
    , _scweieaWorkflowType                 :: WorkflowType
    , _scweieaTaskList                     :: TaskList
    , _scweieaDecisionTaskCompletedEventId :: !Integer
    , _scweieaChildPolicy                  :: ChildPolicy
    } deriving (Eq,Read,Show)

-- | 'StartChildWorkflowExecutionInitiatedEventAttributes' smart constructor.
startChildWorkflowExecutionInitiatedEventAttributes :: Text -> WorkflowType -> TaskList -> Integer -> ChildPolicy -> StartChildWorkflowExecutionInitiatedEventAttributes
startChildWorkflowExecutionInitiatedEventAttributes pWorkflowId pWorkflowType pTaskList pDecisionTaskCompletedEventId pChildPolicy =
    StartChildWorkflowExecutionInitiatedEventAttributes'
    { _scweieaControl = Nothing
    , _scweieaTagList = Nothing
    , _scweieaTaskStartToCloseTimeout = Nothing
    , _scweieaInput = Nothing
    , _scweieaExecutionStartToCloseTimeout = Nothing
    , _scweieaTaskPriority = Nothing
    , _scweieaWorkflowId = pWorkflowId
    , _scweieaWorkflowType = pWorkflowType
    , _scweieaTaskList = pTaskList
    , _scweieaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    , _scweieaChildPolicy = pChildPolicy
    }

-- | /Optional./ Data attached to the event that can be used by the decider
-- in subsequent decision tasks. This data is not sent to the activity.
scweieaControl :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaControl = lens _scweieaControl (\ s a -> s{_scweieaControl = a});

-- | The list of tags to associated with the child workflow execution.
scweieaTagList :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes [Text]
scweieaTagList = lens _scweieaTagList (\ s a -> s{_scweieaTagList = a}) . _Default;

-- | The maximum duration allowed for the decision tasks for this workflow
-- execution.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
scweieaTaskStartToCloseTimeout :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaTaskStartToCloseTimeout = lens _scweieaTaskStartToCloseTimeout (\ s a -> s{_scweieaTaskStartToCloseTimeout = a});

-- | The inputs provided to the child workflow execution (if any).
scweieaInput :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaInput = lens _scweieaInput (\ s a -> s{_scweieaInput = a});

-- | The maximum duration for the child workflow execution. If the workflow
-- execution is not closed within this duration, it will be timed out and
-- force terminated.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
scweieaExecutionStartToCloseTimeout :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaExecutionStartToCloseTimeout = lens _scweieaExecutionStartToCloseTimeout (\ s a -> s{_scweieaExecutionStartToCloseTimeout = a});

-- | /Optional./ The priority assigned for the decision tasks for this
-- workflow execution. Valid values are integers that range from Java\'s
-- @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647).
-- Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon Simple Workflow Developer Guide/.
scweieaTaskPriority :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaTaskPriority = lens _scweieaTaskPriority (\ s a -> s{_scweieaTaskPriority = a});

-- | The @workflowId@ of the child workflow execution.
scweieaWorkflowId :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes Text
scweieaWorkflowId = lens _scweieaWorkflowId (\ s a -> s{_scweieaWorkflowId = a});

-- | The type of the child workflow execution.
scweieaWorkflowType :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes WorkflowType
scweieaWorkflowType = lens _scweieaWorkflowType (\ s a -> s{_scweieaWorkflowType = a});

-- | The name of the task list used for the decision tasks of the child
-- workflow execution.
scweieaTaskList :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes TaskList
scweieaTaskList = lens _scweieaTaskList (\ s a -> s{_scweieaTaskList = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @StartChildWorkflowExecution@
-- Decision to request this child workflow execution. This information can
-- be useful for diagnosing problems by tracing back the cause of events.
scweieaDecisionTaskCompletedEventId :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes Integer
scweieaDecisionTaskCompletedEventId = lens _scweieaDecisionTaskCompletedEventId (\ s a -> s{_scweieaDecisionTaskCompletedEventId = a});

-- | The policy to use for the child workflow executions if this execution
-- gets terminated by explicitly calling the TerminateWorkflowExecution
-- action or due to an expired timeout.
--
-- The supported child policies are:
--
-- -   __TERMINATE:__ the child executions will be terminated.
-- -   __REQUEST_CANCEL:__ a request to cancel will be attempted for each
--     child execution by recording a @WorkflowExecutionCancelRequested@
--     event in its history. It is up to the decider to take appropriate
--     actions when it receives an execution history with this event.
-- -   __ABANDON:__ no action will be taken. The child executions will
--     continue to run.
scweieaChildPolicy :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes ChildPolicy
scweieaChildPolicy = lens _scweieaChildPolicy (\ s a -> s{_scweieaChildPolicy = a});

instance FromJSON
         StartChildWorkflowExecutionInitiatedEventAttributes
         where
        parseJSON
          = withObject
              "StartChildWorkflowExecutionInitiatedEventAttributes"
              (\ x ->
                 StartChildWorkflowExecutionInitiatedEventAttributes'
                   <$>
                   (x .:? "control") <*> (x .:? "tagList" .!= mempty)
                     <*> (x .:? "taskStartToCloseTimeout")
                     <*> (x .:? "input")
                     <*> (x .:? "executionStartToCloseTimeout")
                     <*> (x .:? "taskPriority")
                     <*> (x .: "workflowId")
                     <*> (x .: "workflowType")
                     <*> (x .: "taskList")
                     <*> (x .: "decisionTaskCompletedEventId")
                     <*> (x .: "childPolicy"))

-- | Provides details of the @StartTimer@ decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- /See:/ 'startTimerDecisionAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stdaControl'
--
-- * 'stdaTimerId'
--
-- * 'stdaStartToFireTimeout'
data StartTimerDecisionAttributes = StartTimerDecisionAttributes'
    { _stdaControl            :: Maybe Text
    , _stdaTimerId            :: Text
    , _stdaStartToFireTimeout :: Text
    } deriving (Eq,Read,Show)

-- | 'StartTimerDecisionAttributes' smart constructor.
startTimerDecisionAttributes :: Text -> Text -> StartTimerDecisionAttributes
startTimerDecisionAttributes pTimerId pStartToFireTimeout =
    StartTimerDecisionAttributes'
    { _stdaControl = Nothing
    , _stdaTimerId = pTimerId
    , _stdaStartToFireTimeout = pStartToFireTimeout
    }

-- | /Optional./ Data attached to the event that can be used by the decider
-- in subsequent workflow tasks.
stdaControl :: Lens' StartTimerDecisionAttributes (Maybe Text)
stdaControl = lens _stdaControl (\ s a -> s{_stdaControl = a});

-- | __Required.__ The unique Id of the timer.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (\\u0000-\\u001f | \\u007f - \\u009f). Also, it must not
-- contain the literal string quotarnquot.
stdaTimerId :: Lens' StartTimerDecisionAttributes Text
stdaTimerId = lens _stdaTimerId (\ s a -> s{_stdaTimerId = a});

-- | __Required.__ The duration to wait before firing the timer.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0.
stdaStartToFireTimeout :: Lens' StartTimerDecisionAttributes Text
stdaStartToFireTimeout = lens _stdaStartToFireTimeout (\ s a -> s{_stdaStartToFireTimeout = a});

instance ToJSON StartTimerDecisionAttributes where
        toJSON StartTimerDecisionAttributes'{..}
          = object
              ["control" .= _stdaControl,
               "timerId" .= _stdaTimerId,
               "startToFireTimeout" .= _stdaStartToFireTimeout]

-- | Provides details of the @StartTimerFailed@ event.
--
-- /See:/ 'startTimerFailedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stfeaTimerId'
--
-- * 'stfeaCause'
--
-- * 'stfeaDecisionTaskCompletedEventId'
data StartTimerFailedEventAttributes = StartTimerFailedEventAttributes'
    { _stfeaTimerId                      :: Text
    , _stfeaCause                        :: StartTimerFailedCause
    , _stfeaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'StartTimerFailedEventAttributes' smart constructor.
startTimerFailedEventAttributes :: Text -> StartTimerFailedCause -> Integer -> StartTimerFailedEventAttributes
startTimerFailedEventAttributes pTimerId pCause pDecisionTaskCompletedEventId =
    StartTimerFailedEventAttributes'
    { _stfeaTimerId = pTimerId
    , _stfeaCause = pCause
    , _stfeaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | The timerId provided in the @StartTimer@ decision that failed.
stfeaTimerId :: Lens' StartTimerFailedEventAttributes Text
stfeaTimerId = lens _stfeaTimerId (\ s a -> s{_stfeaTimerId = a});

-- | The cause of the failure. This information is generated by the system
-- and can be useful for diagnostic purposes.
--
-- If __cause__ is set to OPERATION_NOT_PERMITTED, the decision failed
-- because it lacked sufficient permissions. For details and example IAM
-- policies, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
stfeaCause :: Lens' StartTimerFailedEventAttributes StartTimerFailedCause
stfeaCause = lens _stfeaCause (\ s a -> s{_stfeaCause = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @StartTimer@ decision for this
-- activity task. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
stfeaDecisionTaskCompletedEventId :: Lens' StartTimerFailedEventAttributes Integer
stfeaDecisionTaskCompletedEventId = lens _stfeaDecisionTaskCompletedEventId (\ s a -> s{_stfeaDecisionTaskCompletedEventId = a});

instance FromJSON StartTimerFailedEventAttributes
         where
        parseJSON
          = withObject "StartTimerFailedEventAttributes"
              (\ x ->
                 StartTimerFailedEventAttributes' <$>
                   (x .: "timerId") <*> (x .: "cause") <*>
                     (x .: "decisionTaskCompletedEventId"))

-- | Used to filter the workflow executions in visibility APIs based on a
-- tag.
--
-- /See:/ 'tagFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tfTag'
newtype TagFilter = TagFilter'
    { _tfTag :: Text
    } deriving (Eq,Read,Show)

-- | 'TagFilter' smart constructor.
tagFilter :: Text -> TagFilter
tagFilter pTag =
    TagFilter'
    { _tfTag = pTag
    }

-- | __Required.__ Specifies the tag that must be associated with the
-- execution for it to meet the filter criteria.
tfTag :: Lens' TagFilter Text
tfTag = lens _tfTag (\ s a -> s{_tfTag = a});

instance ToJSON TagFilter where
        toJSON TagFilter'{..} = object ["tag" .= _tfTag]

-- | Represents a task list.
--
-- /See:/ 'taskList' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tlName'
newtype TaskList = TaskList'
    { _tlName :: Text
    } deriving (Eq,Read,Show)

-- | 'TaskList' smart constructor.
taskList :: Text -> TaskList
taskList pName =
    TaskList'
    { _tlName = pName
    }

-- | The name of the task list.
tlName :: Lens' TaskList Text
tlName = lens _tlName (\ s a -> s{_tlName = a});

instance FromJSON TaskList where
        parseJSON
          = withObject "TaskList"
              (\ x -> TaskList' <$> (x .: "name"))

instance ToJSON TaskList where
        toJSON TaskList'{..} = object ["name" .= _tlName]

-- | Provides details of the @TimerCanceled@ event.
--
-- /See:/ 'timerCanceledEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tceaTimerId'
--
-- * 'tceaStartedEventId'
--
-- * 'tceaDecisionTaskCompletedEventId'
data TimerCanceledEventAttributes = TimerCanceledEventAttributes'
    { _tceaTimerId                      :: Text
    , _tceaStartedEventId               :: !Integer
    , _tceaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'TimerCanceledEventAttributes' smart constructor.
timerCanceledEventAttributes :: Text -> Integer -> Integer -> TimerCanceledEventAttributes
timerCanceledEventAttributes pTimerId pStartedEventId pDecisionTaskCompletedEventId =
    TimerCanceledEventAttributes'
    { _tceaTimerId = pTimerId
    , _tceaStartedEventId = pStartedEventId
    , _tceaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | The unique Id of the timer that was canceled.
tceaTimerId :: Lens' TimerCanceledEventAttributes Text
tceaTimerId = lens _tceaTimerId (\ s a -> s{_tceaTimerId = a});

-- | The id of the @TimerStarted@ event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
tceaStartedEventId :: Lens' TimerCanceledEventAttributes Integer
tceaStartedEventId = lens _tceaStartedEventId (\ s a -> s{_tceaStartedEventId = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @CancelTimer@ decision to cancel this
-- timer. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
tceaDecisionTaskCompletedEventId :: Lens' TimerCanceledEventAttributes Integer
tceaDecisionTaskCompletedEventId = lens _tceaDecisionTaskCompletedEventId (\ s a -> s{_tceaDecisionTaskCompletedEventId = a});

instance FromJSON TimerCanceledEventAttributes where
        parseJSON
          = withObject "TimerCanceledEventAttributes"
              (\ x ->
                 TimerCanceledEventAttributes' <$>
                   (x .: "timerId") <*> (x .: "startedEventId") <*>
                     (x .: "decisionTaskCompletedEventId"))

-- | Provides details of the @TimerFired@ event.
--
-- /See:/ 'timerFiredEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tfeaTimerId'
--
-- * 'tfeaStartedEventId'
data TimerFiredEventAttributes = TimerFiredEventAttributes'
    { _tfeaTimerId        :: Text
    , _tfeaStartedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'TimerFiredEventAttributes' smart constructor.
timerFiredEventAttributes :: Text -> Integer -> TimerFiredEventAttributes
timerFiredEventAttributes pTimerId pStartedEventId =
    TimerFiredEventAttributes'
    { _tfeaTimerId = pTimerId
    , _tfeaStartedEventId = pStartedEventId
    }

-- | The unique Id of the timer that fired.
tfeaTimerId :: Lens' TimerFiredEventAttributes Text
tfeaTimerId = lens _tfeaTimerId (\ s a -> s{_tfeaTimerId = a});

-- | The id of the @TimerStarted@ event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
tfeaStartedEventId :: Lens' TimerFiredEventAttributes Integer
tfeaStartedEventId = lens _tfeaStartedEventId (\ s a -> s{_tfeaStartedEventId = a});

instance FromJSON TimerFiredEventAttributes where
        parseJSON
          = withObject "TimerFiredEventAttributes"
              (\ x ->
                 TimerFiredEventAttributes' <$>
                   (x .: "timerId") <*> (x .: "startedEventId"))

-- | Provides details of the @TimerStarted@ event.
--
-- /See:/ 'timerStartedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tseaControl'
--
-- * 'tseaTimerId'
--
-- * 'tseaStartToFireTimeout'
--
-- * 'tseaDecisionTaskCompletedEventId'
data TimerStartedEventAttributes = TimerStartedEventAttributes'
    { _tseaControl                      :: Maybe Text
    , _tseaTimerId                      :: Text
    , _tseaStartToFireTimeout           :: Text
    , _tseaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'TimerStartedEventAttributes' smart constructor.
timerStartedEventAttributes :: Text -> Text -> Integer -> TimerStartedEventAttributes
timerStartedEventAttributes pTimerId pStartToFireTimeout pDecisionTaskCompletedEventId =
    TimerStartedEventAttributes'
    { _tseaControl = Nothing
    , _tseaTimerId = pTimerId
    , _tseaStartToFireTimeout = pStartToFireTimeout
    , _tseaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | /Optional./ Data attached to the event that can be used by the decider
-- in subsequent workflow tasks.
tseaControl :: Lens' TimerStartedEventAttributes (Maybe Text)
tseaControl = lens _tseaControl (\ s a -> s{_tseaControl = a});

-- | The unique Id of the timer that was started.
tseaTimerId :: Lens' TimerStartedEventAttributes Text
tseaTimerId = lens _tseaTimerId (\ s a -> s{_tseaTimerId = a});

-- | The duration of time after which the timer will fire.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0.
tseaStartToFireTimeout :: Lens' TimerStartedEventAttributes Text
tseaStartToFireTimeout = lens _tseaStartToFireTimeout (\ s a -> s{_tseaStartToFireTimeout = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @StartTimer@ decision for this
-- activity task. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
tseaDecisionTaskCompletedEventId :: Lens' TimerStartedEventAttributes Integer
tseaDecisionTaskCompletedEventId = lens _tseaDecisionTaskCompletedEventId (\ s a -> s{_tseaDecisionTaskCompletedEventId = a});

instance FromJSON TimerStartedEventAttributes where
        parseJSON
          = withObject "TimerStartedEventAttributes"
              (\ x ->
                 TimerStartedEventAttributes' <$>
                   (x .:? "control") <*> (x .: "timerId") <*>
                     (x .: "startToFireTimeout")
                     <*> (x .: "decisionTaskCompletedEventId"))

-- | Represents a workflow execution.
--
-- /See:/ 'workflowExecution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'weWorkflowId'
--
-- * 'weRunId'
data WorkflowExecution = WorkflowExecution'
    { _weWorkflowId :: Text
    , _weRunId      :: Text
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecution' smart constructor.
workflowExecution :: Text -> Text -> WorkflowExecution
workflowExecution pWorkflowId pRunId =
    WorkflowExecution'
    { _weWorkflowId = pWorkflowId
    , _weRunId = pRunId
    }

-- | The user defined identifier associated with the workflow execution.
weWorkflowId :: Lens' WorkflowExecution Text
weWorkflowId = lens _weWorkflowId (\ s a -> s{_weWorkflowId = a});

-- | A system-generated unique identifier for the workflow execution.
weRunId :: Lens' WorkflowExecution Text
weRunId = lens _weRunId (\ s a -> s{_weRunId = a});

instance FromJSON WorkflowExecution where
        parseJSON
          = withObject "WorkflowExecution"
              (\ x ->
                 WorkflowExecution' <$>
                   (x .: "workflowId") <*> (x .: "runId"))

instance ToJSON WorkflowExecution where
        toJSON WorkflowExecution'{..}
          = object
              ["workflowId" .= _weWorkflowId, "runId" .= _weRunId]

-- | Provides details of the @WorkflowExecutionCancelRequested@ event.
--
-- /See:/ 'workflowExecutionCancelRequestedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wecreaExternalWorkflowExecution'
--
-- * 'wecreaExternalInitiatedEventId'
--
-- * 'wecreaCause'
data WorkflowExecutionCancelRequestedEventAttributes = WorkflowExecutionCancelRequestedEventAttributes'
    { _wecreaExternalWorkflowExecution :: Maybe WorkflowExecution
    , _wecreaExternalInitiatedEventId  :: Maybe Integer
    , _wecreaCause                     :: Maybe WorkflowExecutionCancelRequestedCause
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecutionCancelRequestedEventAttributes' smart constructor.
workflowExecutionCancelRequestedEventAttributes :: WorkflowExecutionCancelRequestedEventAttributes
workflowExecutionCancelRequestedEventAttributes =
    WorkflowExecutionCancelRequestedEventAttributes'
    { _wecreaExternalWorkflowExecution = Nothing
    , _wecreaExternalInitiatedEventId = Nothing
    , _wecreaCause = Nothing
    }

-- | The external workflow execution for which the cancellation was
-- requested.
wecreaExternalWorkflowExecution :: Lens' WorkflowExecutionCancelRequestedEventAttributes (Maybe WorkflowExecution)
wecreaExternalWorkflowExecution = lens _wecreaExternalWorkflowExecution (\ s a -> s{_wecreaExternalWorkflowExecution = a});

-- | The id of the @RequestCancelExternalWorkflowExecutionInitiated@ event
-- corresponding to the @RequestCancelExternalWorkflowExecution@ decision
-- to cancel this workflow execution.The source event with this Id can be
-- found in the history of the source workflow execution. This information
-- can be useful for diagnosing problems by tracing back the chain of
-- events leading up to this event.
wecreaExternalInitiatedEventId :: Lens' WorkflowExecutionCancelRequestedEventAttributes (Maybe Integer)
wecreaExternalInitiatedEventId = lens _wecreaExternalInitiatedEventId (\ s a -> s{_wecreaExternalInitiatedEventId = a});

-- | If set, indicates that the request to cancel the workflow execution was
-- automatically generated, and specifies the cause. This happens if the
-- parent workflow execution times out or is terminated, and the child
-- policy is set to cancel child executions.
wecreaCause :: Lens' WorkflowExecutionCancelRequestedEventAttributes (Maybe WorkflowExecutionCancelRequestedCause)
wecreaCause = lens _wecreaCause (\ s a -> s{_wecreaCause = a});

instance FromJSON
         WorkflowExecutionCancelRequestedEventAttributes where
        parseJSON
          = withObject
              "WorkflowExecutionCancelRequestedEventAttributes"
              (\ x ->
                 WorkflowExecutionCancelRequestedEventAttributes' <$>
                   (x .:? "externalWorkflowExecution") <*>
                     (x .:? "externalInitiatedEventId")
                     <*> (x .:? "cause"))

-- | Provides details of the @WorkflowExecutionCanceled@ event.
--
-- /See:/ 'workflowExecutionCanceledEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'worDetails'
--
-- * 'worDecisionTaskCompletedEventId'
data WorkflowExecutionCanceledEventAttributes = WorkflowExecutionCanceledEventAttributes'
    { _worDetails                      :: Maybe Text
    , _worDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecutionCanceledEventAttributes' smart constructor.
workflowExecutionCanceledEventAttributes :: Integer -> WorkflowExecutionCanceledEventAttributes
workflowExecutionCanceledEventAttributes pDecisionTaskCompletedEventId =
    WorkflowExecutionCanceledEventAttributes'
    { _worDetails = Nothing
    , _worDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | Details for the cancellation (if any).
worDetails :: Lens' WorkflowExecutionCanceledEventAttributes (Maybe Text)
worDetails = lens _worDetails (\ s a -> s{_worDetails = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @CancelWorkflowExecution@ decision
-- for this cancellation request. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
worDecisionTaskCompletedEventId :: Lens' WorkflowExecutionCanceledEventAttributes Integer
worDecisionTaskCompletedEventId = lens _worDecisionTaskCompletedEventId (\ s a -> s{_worDecisionTaskCompletedEventId = a});

instance FromJSON
         WorkflowExecutionCanceledEventAttributes where
        parseJSON
          = withObject
              "WorkflowExecutionCanceledEventAttributes"
              (\ x ->
                 WorkflowExecutionCanceledEventAttributes' <$>
                   (x .:? "details") <*>
                     (x .: "decisionTaskCompletedEventId"))

-- | Provides details of the @WorkflowExecutionCompleted@ event.
--
-- /See:/ 'workflowExecutionCompletedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'weceaResult'
--
-- * 'weceaDecisionTaskCompletedEventId'
data WorkflowExecutionCompletedEventAttributes = WorkflowExecutionCompletedEventAttributes'
    { _weceaResult                       :: Maybe Text
    , _weceaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecutionCompletedEventAttributes' smart constructor.
workflowExecutionCompletedEventAttributes :: Integer -> WorkflowExecutionCompletedEventAttributes
workflowExecutionCompletedEventAttributes pDecisionTaskCompletedEventId =
    WorkflowExecutionCompletedEventAttributes'
    { _weceaResult = Nothing
    , _weceaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | The result produced by the workflow execution upon successful
-- completion.
weceaResult :: Lens' WorkflowExecutionCompletedEventAttributes (Maybe Text)
weceaResult = lens _weceaResult (\ s a -> s{_weceaResult = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @CompleteWorkflowExecution@ decision
-- to complete this execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
weceaDecisionTaskCompletedEventId :: Lens' WorkflowExecutionCompletedEventAttributes Integer
weceaDecisionTaskCompletedEventId = lens _weceaDecisionTaskCompletedEventId (\ s a -> s{_weceaDecisionTaskCompletedEventId = a});

instance FromJSON
         WorkflowExecutionCompletedEventAttributes where
        parseJSON
          = withObject
              "WorkflowExecutionCompletedEventAttributes"
              (\ x ->
                 WorkflowExecutionCompletedEventAttributes' <$>
                   (x .:? "result") <*>
                     (x .: "decisionTaskCompletedEventId"))

-- | The configuration settings for a workflow execution including timeout
-- values, tasklist etc. These configuration settings are determined from
-- the defaults specified when registering the workflow type and those
-- specified when starting the workflow execution.
--
-- /See:/ 'workflowExecutionConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wecTaskPriority'
--
-- * 'wecTaskStartToCloseTimeout'
--
-- * 'wecExecutionStartToCloseTimeout'
--
-- * 'wecTaskList'
--
-- * 'wecChildPolicy'
data WorkflowExecutionConfiguration = WorkflowExecutionConfiguration'
    { _wecTaskPriority                 :: Maybe Text
    , _wecTaskStartToCloseTimeout      :: Text
    , _wecExecutionStartToCloseTimeout :: Text
    , _wecTaskList                     :: TaskList
    , _wecChildPolicy                  :: ChildPolicy
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecutionConfiguration' smart constructor.
workflowExecutionConfiguration :: Text -> Text -> TaskList -> ChildPolicy -> WorkflowExecutionConfiguration
workflowExecutionConfiguration pTaskStartToCloseTimeout pExecutionStartToCloseTimeout pTaskList pChildPolicy =
    WorkflowExecutionConfiguration'
    { _wecTaskPriority = Nothing
    , _wecTaskStartToCloseTimeout = pTaskStartToCloseTimeout
    , _wecExecutionStartToCloseTimeout = pExecutionStartToCloseTimeout
    , _wecTaskList = pTaskList
    , _wecChildPolicy = pChildPolicy
    }

-- | The priority assigned to decision tasks for this workflow execution.
-- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon Simple Workflow Developer Guide/.
wecTaskPriority :: Lens' WorkflowExecutionConfiguration (Maybe Text)
wecTaskPriority = lens _wecTaskPriority (\ s a -> s{_wecTaskPriority = a});

-- | The maximum duration allowed for decision tasks for this workflow
-- execution.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
wecTaskStartToCloseTimeout :: Lens' WorkflowExecutionConfiguration Text
wecTaskStartToCloseTimeout = lens _wecTaskStartToCloseTimeout (\ s a -> s{_wecTaskStartToCloseTimeout = a});

-- | The total duration for this workflow execution.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
wecExecutionStartToCloseTimeout :: Lens' WorkflowExecutionConfiguration Text
wecExecutionStartToCloseTimeout = lens _wecExecutionStartToCloseTimeout (\ s a -> s{_wecExecutionStartToCloseTimeout = a});

-- | The task list used for the decision tasks generated for this workflow
-- execution.
wecTaskList :: Lens' WorkflowExecutionConfiguration TaskList
wecTaskList = lens _wecTaskList (\ s a -> s{_wecTaskList = a});

-- | The policy to use for the child workflow executions if this workflow
-- execution is terminated, by calling the TerminateWorkflowExecution
-- action explicitly or due to an expired timeout.
--
-- The supported child policies are:
--
-- -   __TERMINATE:__ the child executions will be terminated.
-- -   __REQUEST_CANCEL:__ a request to cancel will be attempted for each
--     child execution by recording a @WorkflowExecutionCancelRequested@
--     event in its history. It is up to the decider to take appropriate
--     actions when it receives an execution history with this event.
-- -   __ABANDON:__ no action will be taken. The child executions will
--     continue to run.
wecChildPolicy :: Lens' WorkflowExecutionConfiguration ChildPolicy
wecChildPolicy = lens _wecChildPolicy (\ s a -> s{_wecChildPolicy = a});

instance FromJSON WorkflowExecutionConfiguration
         where
        parseJSON
          = withObject "WorkflowExecutionConfiguration"
              (\ x ->
                 WorkflowExecutionConfiguration' <$>
                   (x .:? "taskPriority") <*>
                     (x .: "taskStartToCloseTimeout")
                     <*> (x .: "executionStartToCloseTimeout")
                     <*> (x .: "taskList")
                     <*> (x .: "childPolicy"))

-- | Provides details of the @WorkflowExecutionContinuedAsNew@ event.
--
-- /See:/ 'workflowExecutionContinuedAsNewEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wecaneaTagList'
--
-- * 'wecaneaTaskStartToCloseTimeout'
--
-- * 'wecaneaInput'
--
-- * 'wecaneaExecutionStartToCloseTimeout'
--
-- * 'wecaneaTaskPriority'
--
-- * 'wecaneaDecisionTaskCompletedEventId'
--
-- * 'wecaneaNewExecutionRunId'
--
-- * 'wecaneaTaskList'
--
-- * 'wecaneaChildPolicy'
--
-- * 'wecaneaWorkflowType'
data WorkflowExecutionContinuedAsNewEventAttributes = WorkflowExecutionContinuedAsNewEventAttributes'
    { _wecaneaTagList                      :: Maybe [Text]
    , _wecaneaTaskStartToCloseTimeout      :: Maybe Text
    , _wecaneaInput                        :: Maybe Text
    , _wecaneaExecutionStartToCloseTimeout :: Maybe Text
    , _wecaneaTaskPriority                 :: Maybe Text
    , _wecaneaDecisionTaskCompletedEventId :: !Integer
    , _wecaneaNewExecutionRunId            :: Text
    , _wecaneaTaskList                     :: TaskList
    , _wecaneaChildPolicy                  :: ChildPolicy
    , _wecaneaWorkflowType                 :: WorkflowType
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecutionContinuedAsNewEventAttributes' smart constructor.
workflowExecutionContinuedAsNewEventAttributes :: Integer -> Text -> TaskList -> ChildPolicy -> WorkflowType -> WorkflowExecutionContinuedAsNewEventAttributes
workflowExecutionContinuedAsNewEventAttributes pDecisionTaskCompletedEventId pNewExecutionRunId pTaskList pChildPolicy pWorkflowType =
    WorkflowExecutionContinuedAsNewEventAttributes'
    { _wecaneaTagList = Nothing
    , _wecaneaTaskStartToCloseTimeout = Nothing
    , _wecaneaInput = Nothing
    , _wecaneaExecutionStartToCloseTimeout = Nothing
    , _wecaneaTaskPriority = Nothing
    , _wecaneaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    , _wecaneaNewExecutionRunId = pNewExecutionRunId
    , _wecaneaTaskList = pTaskList
    , _wecaneaChildPolicy = pChildPolicy
    , _wecaneaWorkflowType = pWorkflowType
    }

-- | The list of tags associated with the new workflow execution.
wecaneaTagList :: Lens' WorkflowExecutionContinuedAsNewEventAttributes [Text]
wecaneaTagList = lens _wecaneaTagList (\ s a -> s{_wecaneaTagList = a}) . _Default;

-- | The maximum duration of decision tasks for the new workflow execution.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
wecaneaTaskStartToCloseTimeout :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaTaskStartToCloseTimeout = lens _wecaneaTaskStartToCloseTimeout (\ s a -> s{_wecaneaTaskStartToCloseTimeout = a});

-- | The input provided to the new workflow execution.
wecaneaInput :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaInput = lens _wecaneaInput (\ s a -> s{_wecaneaInput = a});

-- | The total duration allowed for the new workflow execution.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
wecaneaExecutionStartToCloseTimeout :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaExecutionStartToCloseTimeout = lens _wecaneaExecutionStartToCloseTimeout (\ s a -> s{_wecaneaExecutionStartToCloseTimeout = a});

-- | FIXME: Undocumented member.
wecaneaTaskPriority :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaTaskPriority = lens _wecaneaTaskPriority (\ s a -> s{_wecaneaTaskPriority = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @ContinueAsNewWorkflowExecution@
-- decision that started this execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
wecaneaDecisionTaskCompletedEventId :: Lens' WorkflowExecutionContinuedAsNewEventAttributes Integer
wecaneaDecisionTaskCompletedEventId = lens _wecaneaDecisionTaskCompletedEventId (\ s a -> s{_wecaneaDecisionTaskCompletedEventId = a});

-- | The @runId@ of the new workflow execution.
wecaneaNewExecutionRunId :: Lens' WorkflowExecutionContinuedAsNewEventAttributes Text
wecaneaNewExecutionRunId = lens _wecaneaNewExecutionRunId (\ s a -> s{_wecaneaNewExecutionRunId = a});

-- | FIXME: Undocumented member.
wecaneaTaskList :: Lens' WorkflowExecutionContinuedAsNewEventAttributes TaskList
wecaneaTaskList = lens _wecaneaTaskList (\ s a -> s{_wecaneaTaskList = a});

-- | The policy to use for the child workflow executions of the new execution
-- if it is terminated by calling the TerminateWorkflowExecution action
-- explicitly or due to an expired timeout.
--
-- The supported child policies are:
--
-- -   __TERMINATE:__ the child executions will be terminated.
-- -   __REQUEST_CANCEL:__ a request to cancel will be attempted for each
--     child execution by recording a @WorkflowExecutionCancelRequested@
--     event in its history. It is up to the decider to take appropriate
--     actions when it receives an execution history with this event.
-- -   __ABANDON:__ no action will be taken. The child executions will
--     continue to run.
wecaneaChildPolicy :: Lens' WorkflowExecutionContinuedAsNewEventAttributes ChildPolicy
wecaneaChildPolicy = lens _wecaneaChildPolicy (\ s a -> s{_wecaneaChildPolicy = a});

-- | FIXME: Undocumented member.
wecaneaWorkflowType :: Lens' WorkflowExecutionContinuedAsNewEventAttributes WorkflowType
wecaneaWorkflowType = lens _wecaneaWorkflowType (\ s a -> s{_wecaneaWorkflowType = a});

instance FromJSON
         WorkflowExecutionContinuedAsNewEventAttributes where
        parseJSON
          = withObject
              "WorkflowExecutionContinuedAsNewEventAttributes"
              (\ x ->
                 WorkflowExecutionContinuedAsNewEventAttributes' <$>
                   (x .:? "tagList" .!= mempty) <*>
                     (x .:? "taskStartToCloseTimeout")
                     <*> (x .:? "input")
                     <*> (x .:? "executionStartToCloseTimeout")
                     <*> (x .:? "taskPriority")
                     <*> (x .: "decisionTaskCompletedEventId")
                     <*> (x .: "newExecutionRunId")
                     <*> (x .: "taskList")
                     <*> (x .: "childPolicy")
                     <*> (x .: "workflowType"))

-- | Contains the count of workflow executions returned from
-- CountOpenWorkflowExecutions or CountClosedWorkflowExecutions
--
-- /See:/ 'workflowExecutionCount' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wecTruncated'
--
-- * 'wecCount'
data WorkflowExecutionCount = WorkflowExecutionCount'
    { _wecTruncated :: Maybe Bool
    , _wecCount     :: !Nat
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecutionCount' smart constructor.
workflowExecutionCount :: Natural -> WorkflowExecutionCount
workflowExecutionCount pCount =
    WorkflowExecutionCount'
    { _wecTruncated = Nothing
    , _wecCount = _Nat # pCount
    }

-- | If set to true, indicates that the actual count was more than the
-- maximum supported by this API and the count returned is the truncated
-- value.
wecTruncated :: Lens' WorkflowExecutionCount (Maybe Bool)
wecTruncated = lens _wecTruncated (\ s a -> s{_wecTruncated = a});

-- | The number of workflow executions.
wecCount :: Lens' WorkflowExecutionCount Natural
wecCount = lens _wecCount (\ s a -> s{_wecCount = a}) . _Nat;

instance FromJSON WorkflowExecutionCount where
        parseJSON
          = withObject "WorkflowExecutionCount"
              (\ x ->
                 WorkflowExecutionCount' <$>
                   (x .:? "truncated") <*> (x .: "count"))

-- | Provides details of the @WorkflowExecutionFailed@ event.
--
-- /See:/ 'workflowExecutionFailedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wefeaReason'
--
-- * 'wefeaDetails'
--
-- * 'wefeaDecisionTaskCompletedEventId'
data WorkflowExecutionFailedEventAttributes = WorkflowExecutionFailedEventAttributes'
    { _wefeaReason                       :: Maybe Text
    , _wefeaDetails                      :: Maybe Text
    , _wefeaDecisionTaskCompletedEventId :: !Integer
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecutionFailedEventAttributes' smart constructor.
workflowExecutionFailedEventAttributes :: Integer -> WorkflowExecutionFailedEventAttributes
workflowExecutionFailedEventAttributes pDecisionTaskCompletedEventId =
    WorkflowExecutionFailedEventAttributes'
    { _wefeaReason = Nothing
    , _wefeaDetails = Nothing
    , _wefeaDecisionTaskCompletedEventId = pDecisionTaskCompletedEventId
    }

-- | The descriptive reason provided for the failure (if any).
wefeaReason :: Lens' WorkflowExecutionFailedEventAttributes (Maybe Text)
wefeaReason = lens _wefeaReason (\ s a -> s{_wefeaReason = a});

-- | The details of the failure (if any).
wefeaDetails :: Lens' WorkflowExecutionFailedEventAttributes (Maybe Text)
wefeaDetails = lens _wefeaDetails (\ s a -> s{_wefeaDetails = a});

-- | The id of the @DecisionTaskCompleted@ event corresponding to the
-- decision task that resulted in the @FailWorkflowExecution@ decision to
-- fail this execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
wefeaDecisionTaskCompletedEventId :: Lens' WorkflowExecutionFailedEventAttributes Integer
wefeaDecisionTaskCompletedEventId = lens _wefeaDecisionTaskCompletedEventId (\ s a -> s{_wefeaDecisionTaskCompletedEventId = a});

instance FromJSON
         WorkflowExecutionFailedEventAttributes where
        parseJSON
          = withObject "WorkflowExecutionFailedEventAttributes"
              (\ x ->
                 WorkflowExecutionFailedEventAttributes' <$>
                   (x .:? "reason") <*> (x .:? "details") <*>
                     (x .: "decisionTaskCompletedEventId"))

-- | Used to filter the workflow executions in visibility APIs by their
-- @workflowId@.
--
-- /See:/ 'workflowExecutionFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wefWorkflowId'
newtype WorkflowExecutionFilter = WorkflowExecutionFilter'
    { _wefWorkflowId :: Text
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecutionFilter' smart constructor.
workflowExecutionFilter :: Text -> WorkflowExecutionFilter
workflowExecutionFilter pWorkflowId =
    WorkflowExecutionFilter'
    { _wefWorkflowId = pWorkflowId
    }

-- | The workflowId to pass of match the criteria of this filter.
wefWorkflowId :: Lens' WorkflowExecutionFilter Text
wefWorkflowId = lens _wefWorkflowId (\ s a -> s{_wefWorkflowId = a});

instance ToJSON WorkflowExecutionFilter where
        toJSON WorkflowExecutionFilter'{..}
          = object ["workflowId" .= _wefWorkflowId]

-- | Contains information about a workflow execution.
--
-- /See:/ 'workflowExecutionInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'weiParent'
--
-- * 'weiTagList'
--
-- * 'weiCloseStatus'
--
-- * 'weiCloseTimestamp'
--
-- * 'weiCancelRequested'
--
-- * 'weiExecution'
--
-- * 'weiWorkflowType'
--
-- * 'weiStartTimestamp'
--
-- * 'weiExecutionStatus'
data WorkflowExecutionInfo = WorkflowExecutionInfo'
    { _weiParent          :: Maybe WorkflowExecution
    , _weiTagList         :: Maybe [Text]
    , _weiCloseStatus     :: Maybe CloseStatus
    , _weiCloseTimestamp  :: Maybe POSIX
    , _weiCancelRequested :: Maybe Bool
    , _weiExecution       :: WorkflowExecution
    , _weiWorkflowType    :: WorkflowType
    , _weiStartTimestamp  :: POSIX
    , _weiExecutionStatus :: ExecutionStatus
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecutionInfo' smart constructor.
workflowExecutionInfo :: WorkflowExecution -> WorkflowType -> UTCTime -> ExecutionStatus -> WorkflowExecutionInfo
workflowExecutionInfo pExecution pWorkflowType pStartTimestamp pExecutionStatus =
    WorkflowExecutionInfo'
    { _weiParent = Nothing
    , _weiTagList = Nothing
    , _weiCloseStatus = Nothing
    , _weiCloseTimestamp = Nothing
    , _weiCancelRequested = Nothing
    , _weiExecution = pExecution
    , _weiWorkflowType = pWorkflowType
    , _weiStartTimestamp = _Time # pStartTimestamp
    , _weiExecutionStatus = pExecutionStatus
    }

-- | If this workflow execution is a child of another execution then contains
-- the workflow execution that started this execution.
weiParent :: Lens' WorkflowExecutionInfo (Maybe WorkflowExecution)
weiParent = lens _weiParent (\ s a -> s{_weiParent = a});

-- | The list of tags associated with the workflow execution. Tags can be
-- used to identify and list workflow executions of interest through the
-- visibility APIs. A workflow execution can have a maximum of 5 tags.
weiTagList :: Lens' WorkflowExecutionInfo [Text]
weiTagList = lens _weiTagList (\ s a -> s{_weiTagList = a}) . _Default;

-- | If the execution status is closed then this specifies how the execution
-- was closed:
--
-- -   @COMPLETED@: the execution was successfully completed.
-- -   @CANCELED@: the execution was canceled.Cancellation allows the
--     implementation to gracefully clean up before the execution is
--     closed.
-- -   @TERMINATED@: the execution was force terminated.
-- -   @FAILED@: the execution failed to complete.
-- -   @TIMED_OUT@: the execution did not complete in the alloted time and
--     was automatically timed out.
-- -   @CONTINUED_AS_NEW@: the execution is logically continued. This means
--     the current execution was completed and a new execution was started
--     to carry on the workflow.
weiCloseStatus :: Lens' WorkflowExecutionInfo (Maybe CloseStatus)
weiCloseStatus = lens _weiCloseStatus (\ s a -> s{_weiCloseStatus = a});

-- | The time when the workflow execution was closed. Set only if the
-- execution status is CLOSED.
weiCloseTimestamp :: Lens' WorkflowExecutionInfo (Maybe UTCTime)
weiCloseTimestamp = lens _weiCloseTimestamp (\ s a -> s{_weiCloseTimestamp = a}) . mapping _Time;

-- | Set to true if a cancellation is requested for this workflow execution.
weiCancelRequested :: Lens' WorkflowExecutionInfo (Maybe Bool)
weiCancelRequested = lens _weiCancelRequested (\ s a -> s{_weiCancelRequested = a});

-- | The workflow execution this information is about.
weiExecution :: Lens' WorkflowExecutionInfo WorkflowExecution
weiExecution = lens _weiExecution (\ s a -> s{_weiExecution = a});

-- | The type of the workflow execution.
weiWorkflowType :: Lens' WorkflowExecutionInfo WorkflowType
weiWorkflowType = lens _weiWorkflowType (\ s a -> s{_weiWorkflowType = a});

-- | The time when the execution was started.
weiStartTimestamp :: Lens' WorkflowExecutionInfo UTCTime
weiStartTimestamp = lens _weiStartTimestamp (\ s a -> s{_weiStartTimestamp = a}) . _Time;

-- | The current status of the execution.
weiExecutionStatus :: Lens' WorkflowExecutionInfo ExecutionStatus
weiExecutionStatus = lens _weiExecutionStatus (\ s a -> s{_weiExecutionStatus = a});

instance FromJSON WorkflowExecutionInfo where
        parseJSON
          = withObject "WorkflowExecutionInfo"
              (\ x ->
                 WorkflowExecutionInfo' <$>
                   (x .:? "parent") <*> (x .:? "tagList" .!= mempty) <*>
                     (x .:? "closeStatus")
                     <*> (x .:? "closeTimestamp")
                     <*> (x .:? "cancelRequested")
                     <*> (x .: "execution")
                     <*> (x .: "workflowType")
                     <*> (x .: "startTimestamp")
                     <*> (x .: "executionStatus"))

-- | Contains a paginated list of information about workflow executions.
--
-- /See:/ 'workflowExecutionInfos' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'weiNextPageToken'
--
-- * 'weiExecutionInfos'
data WorkflowExecutionInfos = WorkflowExecutionInfos'
    { _weiNextPageToken  :: Maybe Text
    , _weiExecutionInfos :: [WorkflowExecutionInfo]
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecutionInfos' smart constructor.
workflowExecutionInfos :: WorkflowExecutionInfos
workflowExecutionInfos =
    WorkflowExecutionInfos'
    { _weiNextPageToken = Nothing
    , _weiExecutionInfos = mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
weiNextPageToken :: Lens' WorkflowExecutionInfos (Maybe Text)
weiNextPageToken = lens _weiNextPageToken (\ s a -> s{_weiNextPageToken = a});

-- | The list of workflow information structures.
weiExecutionInfos :: Lens' WorkflowExecutionInfos [WorkflowExecutionInfo]
weiExecutionInfos = lens _weiExecutionInfos (\ s a -> s{_weiExecutionInfos = a});

instance FromJSON WorkflowExecutionInfos where
        parseJSON
          = withObject "WorkflowExecutionInfos"
              (\ x ->
                 WorkflowExecutionInfos' <$>
                   (x .:? "nextPageToken") <*>
                     (x .:? "executionInfos" .!= mempty))

-- | Contains the counts of open tasks, child workflow executions and timers
-- for a workflow execution.
--
-- /See:/ 'workflowExecutionOpenCounts' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'weocOpenActivityTasks'
--
-- * 'weocOpenDecisionTasks'
--
-- * 'weocOpenTimers'
--
-- * 'weocOpenChildWorkflowExecutions'
data WorkflowExecutionOpenCounts = WorkflowExecutionOpenCounts'
    { _weocOpenActivityTasks           :: !Nat
    , _weocOpenDecisionTasks           :: !Nat
    , _weocOpenTimers                  :: !Nat
    , _weocOpenChildWorkflowExecutions :: !Nat
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecutionOpenCounts' smart constructor.
workflowExecutionOpenCounts :: Natural -> Natural -> Natural -> Natural -> WorkflowExecutionOpenCounts
workflowExecutionOpenCounts pOpenActivityTasks pOpenDecisionTasks pOpenTimers pOpenChildWorkflowExecutions =
    WorkflowExecutionOpenCounts'
    { _weocOpenActivityTasks = _Nat # pOpenActivityTasks
    , _weocOpenDecisionTasks = _Nat # pOpenDecisionTasks
    , _weocOpenTimers = _Nat # pOpenTimers
    , _weocOpenChildWorkflowExecutions = _Nat # pOpenChildWorkflowExecutions
    }

-- | The count of activity tasks whose status is OPEN.
weocOpenActivityTasks :: Lens' WorkflowExecutionOpenCounts Natural
weocOpenActivityTasks = lens _weocOpenActivityTasks (\ s a -> s{_weocOpenActivityTasks = a}) . _Nat;

-- | The count of decision tasks whose status is OPEN. A workflow execution
-- can have at most one open decision task.
weocOpenDecisionTasks :: Lens' WorkflowExecutionOpenCounts Natural
weocOpenDecisionTasks = lens _weocOpenDecisionTasks (\ s a -> s{_weocOpenDecisionTasks = a}) . _Nat;

-- | The count of timers started by this workflow execution that have not
-- fired yet.
weocOpenTimers :: Lens' WorkflowExecutionOpenCounts Natural
weocOpenTimers = lens _weocOpenTimers (\ s a -> s{_weocOpenTimers = a}) . _Nat;

-- | The count of child workflow executions whose status is OPEN.
weocOpenChildWorkflowExecutions :: Lens' WorkflowExecutionOpenCounts Natural
weocOpenChildWorkflowExecutions = lens _weocOpenChildWorkflowExecutions (\ s a -> s{_weocOpenChildWorkflowExecutions = a}) . _Nat;

instance FromJSON WorkflowExecutionOpenCounts where
        parseJSON
          = withObject "WorkflowExecutionOpenCounts"
              (\ x ->
                 WorkflowExecutionOpenCounts' <$>
                   (x .: "openActivityTasks") <*>
                     (x .: "openDecisionTasks")
                     <*> (x .: "openTimers")
                     <*> (x .: "openChildWorkflowExecutions"))

-- | Provides details of the @WorkflowExecutionSignaled@ event.
--
-- /See:/ 'workflowExecutionSignaledEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'worExternalWorkflowExecution'
--
-- * 'worExternalInitiatedEventId'
--
-- * 'worInput'
--
-- * 'worSignalName'
data WorkflowExecutionSignaledEventAttributes = WorkflowExecutionSignaledEventAttributes'
    { _worExternalWorkflowExecution :: Maybe WorkflowExecution
    , _worExternalInitiatedEventId  :: Maybe Integer
    , _worInput                     :: Maybe Text
    , _worSignalName                :: Text
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecutionSignaledEventAttributes' smart constructor.
workflowExecutionSignaledEventAttributes :: Text -> WorkflowExecutionSignaledEventAttributes
workflowExecutionSignaledEventAttributes pSignalName =
    WorkflowExecutionSignaledEventAttributes'
    { _worExternalWorkflowExecution = Nothing
    , _worExternalInitiatedEventId = Nothing
    , _worInput = Nothing
    , _worSignalName = pSignalName
    }

-- | The workflow execution that sent the signal. This is set only of the
-- signal was sent by another workflow execution.
worExternalWorkflowExecution :: Lens' WorkflowExecutionSignaledEventAttributes (Maybe WorkflowExecution)
worExternalWorkflowExecution = lens _worExternalWorkflowExecution (\ s a -> s{_worExternalWorkflowExecution = a});

-- | The id of the @SignalExternalWorkflowExecutionInitiated@ event
-- corresponding to the @SignalExternalWorkflow@ decision to signal this
-- workflow execution.The source event with this Id can be found in the
-- history of the source workflow execution. This information can be useful
-- for diagnosing problems by tracing back the chain of events leading up
-- to this event. This field is set only if the signal was initiated by
-- another workflow execution.
worExternalInitiatedEventId :: Lens' WorkflowExecutionSignaledEventAttributes (Maybe Integer)
worExternalInitiatedEventId = lens _worExternalInitiatedEventId (\ s a -> s{_worExternalInitiatedEventId = a});

-- | Inputs provided with the signal (if any). The decider can use the signal
-- name and inputs to determine how to process the signal.
worInput :: Lens' WorkflowExecutionSignaledEventAttributes (Maybe Text)
worInput = lens _worInput (\ s a -> s{_worInput = a});

-- | The name of the signal received. The decider can use the signal name and
-- inputs to determine how to the process the signal.
worSignalName :: Lens' WorkflowExecutionSignaledEventAttributes Text
worSignalName = lens _worSignalName (\ s a -> s{_worSignalName = a});

instance FromJSON
         WorkflowExecutionSignaledEventAttributes where
        parseJSON
          = withObject
              "WorkflowExecutionSignaledEventAttributes"
              (\ x ->
                 WorkflowExecutionSignaledEventAttributes' <$>
                   (x .:? "externalWorkflowExecution") <*>
                     (x .:? "externalInitiatedEventId")
                     <*> (x .:? "input")
                     <*> (x .: "signalName"))

-- | Provides details of @WorkflowExecutionStarted@ event.
--
-- /See:/ 'workflowExecutionStartedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'weseaParentInitiatedEventId'
--
-- * 'weseaTagList'
--
-- * 'weseaTaskStartToCloseTimeout'
--
-- * 'weseaInput'
--
-- * 'weseaExecutionStartToCloseTimeout'
--
-- * 'weseaTaskPriority'
--
-- * 'weseaParentWorkflowExecution'
--
-- * 'weseaContinuedExecutionRunId'
--
-- * 'weseaChildPolicy'
--
-- * 'weseaTaskList'
--
-- * 'weseaWorkflowType'
data WorkflowExecutionStartedEventAttributes = WorkflowExecutionStartedEventAttributes'
    { _weseaParentInitiatedEventId       :: Maybe Integer
    , _weseaTagList                      :: Maybe [Text]
    , _weseaTaskStartToCloseTimeout      :: Maybe Text
    , _weseaInput                        :: Maybe Text
    , _weseaExecutionStartToCloseTimeout :: Maybe Text
    , _weseaTaskPriority                 :: Maybe Text
    , _weseaParentWorkflowExecution      :: Maybe WorkflowExecution
    , _weseaContinuedExecutionRunId      :: Maybe Text
    , _weseaChildPolicy                  :: ChildPolicy
    , _weseaTaskList                     :: TaskList
    , _weseaWorkflowType                 :: WorkflowType
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecutionStartedEventAttributes' smart constructor.
workflowExecutionStartedEventAttributes :: ChildPolicy -> TaskList -> WorkflowType -> WorkflowExecutionStartedEventAttributes
workflowExecutionStartedEventAttributes pChildPolicy pTaskList pWorkflowType =
    WorkflowExecutionStartedEventAttributes'
    { _weseaParentInitiatedEventId = Nothing
    , _weseaTagList = Nothing
    , _weseaTaskStartToCloseTimeout = Nothing
    , _weseaInput = Nothing
    , _weseaExecutionStartToCloseTimeout = Nothing
    , _weseaTaskPriority = Nothing
    , _weseaParentWorkflowExecution = Nothing
    , _weseaContinuedExecutionRunId = Nothing
    , _weseaChildPolicy = pChildPolicy
    , _weseaTaskList = pTaskList
    , _weseaWorkflowType = pWorkflowType
    }

-- | The id of the @StartChildWorkflowExecutionInitiated@ event corresponding
-- to the @StartChildWorkflowExecution@ Decision to start this workflow
-- execution. The source event with this Id can be found in the history of
-- the source workflow execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
weseaParentInitiatedEventId :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Integer)
weseaParentInitiatedEventId = lens _weseaParentInitiatedEventId (\ s a -> s{_weseaParentInitiatedEventId = a});

-- | The list of tags associated with this workflow execution. An execution
-- can have up to 5 tags.
weseaTagList :: Lens' WorkflowExecutionStartedEventAttributes [Text]
weseaTagList = lens _weseaTagList (\ s a -> s{_weseaTagList = a}) . _Default;

-- | The maximum duration of decision tasks for this workflow type.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
weseaTaskStartToCloseTimeout :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaTaskStartToCloseTimeout = lens _weseaTaskStartToCloseTimeout (\ s a -> s{_weseaTaskStartToCloseTimeout = a});

-- | The input provided to the workflow execution (if any).
weseaInput :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaInput = lens _weseaInput (\ s a -> s{_weseaInput = a});

-- | The maximum duration for this workflow execution.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
weseaExecutionStartToCloseTimeout :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaExecutionStartToCloseTimeout = lens _weseaExecutionStartToCloseTimeout (\ s a -> s{_weseaExecutionStartToCloseTimeout = a});

-- | FIXME: Undocumented member.
weseaTaskPriority :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaTaskPriority = lens _weseaTaskPriority (\ s a -> s{_weseaTaskPriority = a});

-- | The source workflow execution that started this workflow execution. The
-- member is not set if the workflow execution was not started by a
-- workflow.
weseaParentWorkflowExecution :: Lens' WorkflowExecutionStartedEventAttributes (Maybe WorkflowExecution)
weseaParentWorkflowExecution = lens _weseaParentWorkflowExecution (\ s a -> s{_weseaParentWorkflowExecution = a});

-- | If this workflow execution was started due to a
-- @ContinueAsNewWorkflowExecution@ decision, then it contains the @runId@
-- of the previous workflow execution that was closed and continued as this
-- execution.
weseaContinuedExecutionRunId :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaContinuedExecutionRunId = lens _weseaContinuedExecutionRunId (\ s a -> s{_weseaContinuedExecutionRunId = a});

-- | The policy to use for the child workflow executions if this workflow
-- execution is terminated, by calling the TerminateWorkflowExecution
-- action explicitly or due to an expired timeout.
--
-- The supported child policies are:
--
-- -   __TERMINATE:__ the child executions will be terminated.
-- -   __REQUEST_CANCEL:__ a request to cancel will be attempted for each
--     child execution by recording a @WorkflowExecutionCancelRequested@
--     event in its history. It is up to the decider to take appropriate
--     actions when it receives an execution history with this event.
-- -   __ABANDON:__ no action will be taken. The child executions will
--     continue to run.
weseaChildPolicy :: Lens' WorkflowExecutionStartedEventAttributes ChildPolicy
weseaChildPolicy = lens _weseaChildPolicy (\ s a -> s{_weseaChildPolicy = a});

-- | The name of the task list for scheduling the decision tasks for this
-- workflow execution.
weseaTaskList :: Lens' WorkflowExecutionStartedEventAttributes TaskList
weseaTaskList = lens _weseaTaskList (\ s a -> s{_weseaTaskList = a});

-- | The workflow type of this execution.
weseaWorkflowType :: Lens' WorkflowExecutionStartedEventAttributes WorkflowType
weseaWorkflowType = lens _weseaWorkflowType (\ s a -> s{_weseaWorkflowType = a});

instance FromJSON
         WorkflowExecutionStartedEventAttributes where
        parseJSON
          = withObject
              "WorkflowExecutionStartedEventAttributes"
              (\ x ->
                 WorkflowExecutionStartedEventAttributes' <$>
                   (x .:? "parentInitiatedEventId") <*>
                     (x .:? "tagList" .!= mempty)
                     <*> (x .:? "taskStartToCloseTimeout")
                     <*> (x .:? "input")
                     <*> (x .:? "executionStartToCloseTimeout")
                     <*> (x .:? "taskPriority")
                     <*> (x .:? "parentWorkflowExecution")
                     <*> (x .:? "continuedExecutionRunId")
                     <*> (x .: "childPolicy")
                     <*> (x .: "taskList")
                     <*> (x .: "workflowType"))

-- | Provides details of the @WorkflowExecutionTerminated@ event.
--
-- /See:/ 'workflowExecutionTerminatedEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'weteaCause'
--
-- * 'weteaReason'
--
-- * 'weteaDetails'
--
-- * 'weteaChildPolicy'
data WorkflowExecutionTerminatedEventAttributes = WorkflowExecutionTerminatedEventAttributes'
    { _weteaCause       :: Maybe WorkflowExecutionTerminatedCause
    , _weteaReason      :: Maybe Text
    , _weteaDetails     :: Maybe Text
    , _weteaChildPolicy :: ChildPolicy
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecutionTerminatedEventAttributes' smart constructor.
workflowExecutionTerminatedEventAttributes :: ChildPolicy -> WorkflowExecutionTerminatedEventAttributes
workflowExecutionTerminatedEventAttributes pChildPolicy =
    WorkflowExecutionTerminatedEventAttributes'
    { _weteaCause = Nothing
    , _weteaReason = Nothing
    , _weteaDetails = Nothing
    , _weteaChildPolicy = pChildPolicy
    }

-- | If set, indicates that the workflow execution was automatically
-- terminated, and specifies the cause. This happens if the parent workflow
-- execution times out or is terminated and the child policy is set to
-- terminate child executions.
weteaCause :: Lens' WorkflowExecutionTerminatedEventAttributes (Maybe WorkflowExecutionTerminatedCause)
weteaCause = lens _weteaCause (\ s a -> s{_weteaCause = a});

-- | The reason provided for the termination (if any).
weteaReason :: Lens' WorkflowExecutionTerminatedEventAttributes (Maybe Text)
weteaReason = lens _weteaReason (\ s a -> s{_weteaReason = a});

-- | The details provided for the termination (if any).
weteaDetails :: Lens' WorkflowExecutionTerminatedEventAttributes (Maybe Text)
weteaDetails = lens _weteaDetails (\ s a -> s{_weteaDetails = a});

-- | The policy used for the child workflow executions of this workflow
-- execution.
--
-- The supported child policies are:
--
-- -   __TERMINATE:__ the child executions will be terminated.
-- -   __REQUEST_CANCEL:__ a request to cancel will be attempted for each
--     child execution by recording a @WorkflowExecutionCancelRequested@
--     event in its history. It is up to the decider to take appropriate
--     actions when it receives an execution history with this event.
-- -   __ABANDON:__ no action will be taken. The child executions will
--     continue to run.
weteaChildPolicy :: Lens' WorkflowExecutionTerminatedEventAttributes ChildPolicy
weteaChildPolicy = lens _weteaChildPolicy (\ s a -> s{_weteaChildPolicy = a});

instance FromJSON
         WorkflowExecutionTerminatedEventAttributes where
        parseJSON
          = withObject
              "WorkflowExecutionTerminatedEventAttributes"
              (\ x ->
                 WorkflowExecutionTerminatedEventAttributes' <$>
                   (x .:? "cause") <*> (x .:? "reason") <*>
                     (x .:? "details")
                     <*> (x .: "childPolicy"))

-- | Provides details of the @WorkflowExecutionTimedOut@ event.
--
-- /See:/ 'workflowExecutionTimedOutEventAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wetoeaTimeoutType'
--
-- * 'wetoeaChildPolicy'
data WorkflowExecutionTimedOutEventAttributes = WorkflowExecutionTimedOutEventAttributes'
    { _wetoeaTimeoutType :: WorkflowExecutionTimeoutType
    , _wetoeaChildPolicy :: ChildPolicy
    } deriving (Eq,Read,Show)

-- | 'WorkflowExecutionTimedOutEventAttributes' smart constructor.
workflowExecutionTimedOutEventAttributes :: WorkflowExecutionTimeoutType -> ChildPolicy -> WorkflowExecutionTimedOutEventAttributes
workflowExecutionTimedOutEventAttributes pTimeoutType pChildPolicy =
    WorkflowExecutionTimedOutEventAttributes'
    { _wetoeaTimeoutType = pTimeoutType
    , _wetoeaChildPolicy = pChildPolicy
    }

-- | The type of timeout that caused this event.
wetoeaTimeoutType :: Lens' WorkflowExecutionTimedOutEventAttributes WorkflowExecutionTimeoutType
wetoeaTimeoutType = lens _wetoeaTimeoutType (\ s a -> s{_wetoeaTimeoutType = a});

-- | The policy used for the child workflow executions of this workflow
-- execution.
--
-- The supported child policies are:
--
-- -   __TERMINATE:__ the child executions will be terminated.
-- -   __REQUEST_CANCEL:__ a request to cancel will be attempted for each
--     child execution by recording a @WorkflowExecutionCancelRequested@
--     event in its history. It is up to the decider to take appropriate
--     actions when it receives an execution history with this event.
-- -   __ABANDON:__ no action will be taken. The child executions will
--     continue to run.
wetoeaChildPolicy :: Lens' WorkflowExecutionTimedOutEventAttributes ChildPolicy
wetoeaChildPolicy = lens _wetoeaChildPolicy (\ s a -> s{_wetoeaChildPolicy = a});

instance FromJSON
         WorkflowExecutionTimedOutEventAttributes where
        parseJSON
          = withObject
              "WorkflowExecutionTimedOutEventAttributes"
              (\ x ->
                 WorkflowExecutionTimedOutEventAttributes' <$>
                   (x .: "timeoutType") <*> (x .: "childPolicy"))

-- | Represents a workflow type.
--
-- /See:/ 'workflowType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wtName'
--
-- * 'wtVersion'
data WorkflowType = WorkflowType'
    { _wtName    :: Text
    , _wtVersion :: Text
    } deriving (Eq,Read,Show)

-- | 'WorkflowType' smart constructor.
workflowType :: Text -> Text -> WorkflowType
workflowType pName pVersion =
    WorkflowType'
    { _wtName = pName
    , _wtVersion = pVersion
    }

-- | __Required.__ The name of the workflow type.
--
-- The combination of workflow type name and version must be unique with in
-- a domain.
wtName :: Lens' WorkflowType Text
wtName = lens _wtName (\ s a -> s{_wtName = a});

-- | __Required.__ The version of the workflow type.
--
-- The combination of workflow type name and version must be unique with in
-- a domain.
wtVersion :: Lens' WorkflowType Text
wtVersion = lens _wtVersion (\ s a -> s{_wtVersion = a});

instance FromJSON WorkflowType where
        parseJSON
          = withObject "WorkflowType"
              (\ x ->
                 WorkflowType' <$> (x .: "name") <*> (x .: "version"))

instance ToJSON WorkflowType where
        toJSON WorkflowType'{..}
          = object ["name" .= _wtName, "version" .= _wtVersion]

-- | The configuration settings of a workflow type.
--
-- /See:/ 'workflowTypeConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wtcDefaultChildPolicy'
--
-- * 'wtcDefaultTaskList'
--
-- * 'wtcDefaultTaskPriority'
--
-- * 'wtcDefaultExecutionStartToCloseTimeout'
--
-- * 'wtcDefaultTaskStartToCloseTimeout'
data WorkflowTypeConfiguration = WorkflowTypeConfiguration'
    { _wtcDefaultChildPolicy                  :: Maybe ChildPolicy
    , _wtcDefaultTaskList                     :: Maybe TaskList
    , _wtcDefaultTaskPriority                 :: Maybe Text
    , _wtcDefaultExecutionStartToCloseTimeout :: Maybe Text
    , _wtcDefaultTaskStartToCloseTimeout      :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'WorkflowTypeConfiguration' smart constructor.
workflowTypeConfiguration :: WorkflowTypeConfiguration
workflowTypeConfiguration =
    WorkflowTypeConfiguration'
    { _wtcDefaultChildPolicy = Nothing
    , _wtcDefaultTaskList = Nothing
    , _wtcDefaultTaskPriority = Nothing
    , _wtcDefaultExecutionStartToCloseTimeout = Nothing
    , _wtcDefaultTaskStartToCloseTimeout = Nothing
    }

-- | /Optional./ The default policy to use for the child workflow executions
-- when a workflow execution of this type is terminated, by calling the
-- TerminateWorkflowExecution action explicitly or due to an expired
-- timeout. This default can be overridden when starting a workflow
-- execution using the StartWorkflowExecution action or the
-- @StartChildWorkflowExecution@ Decision.
--
-- The supported child policies are:
--
-- -   __TERMINATE:__ the child executions will be terminated.
-- -   __REQUEST_CANCEL:__ a request to cancel will be attempted for each
--     child execution by recording a @WorkflowExecutionCancelRequested@
--     event in its history. It is up to the decider to take appropriate
--     actions when it receives an execution history with this event.
-- -   __ABANDON:__ no action will be taken. The child executions will
--     continue to run.
wtcDefaultChildPolicy :: Lens' WorkflowTypeConfiguration (Maybe ChildPolicy)
wtcDefaultChildPolicy = lens _wtcDefaultChildPolicy (\ s a -> s{_wtcDefaultChildPolicy = a});

-- | /Optional./ The default task list, specified when registering the
-- workflow type, for decisions tasks scheduled for workflow executions of
-- this type. This default can be overridden when starting a workflow
-- execution using the StartWorkflowExecution action or the
-- @StartChildWorkflowExecution@ Decision.
wtcDefaultTaskList :: Lens' WorkflowTypeConfiguration (Maybe TaskList)
wtcDefaultTaskList = lens _wtcDefaultTaskList (\ s a -> s{_wtcDefaultTaskList = a});

-- | /Optional./ The default task priority, specified when registering the
-- workflow type, for all decision tasks of this workflow type. This
-- default can be overridden when starting a workflow execution using the
-- StartWorkflowExecution action or the @StartChildWorkflowExecution@
-- decision.
--
-- Valid values are integers that range from Java\'s @Integer.MIN_VALUE@
-- (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers
-- indicate higher priority.
--
-- For more information about setting task priority, see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /Amazon Simple Workflow Developer Guide/.
wtcDefaultTaskPriority :: Lens' WorkflowTypeConfiguration (Maybe Text)
wtcDefaultTaskPriority = lens _wtcDefaultTaskPriority (\ s a -> s{_wtcDefaultTaskPriority = a});

-- | /Optional./ The default maximum duration, specified when registering the
-- workflow type, for executions of this workflow type. This default can be
-- overridden when starting a workflow execution using the
-- StartWorkflowExecution action or the @StartChildWorkflowExecution@
-- Decision.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
wtcDefaultExecutionStartToCloseTimeout :: Lens' WorkflowTypeConfiguration (Maybe Text)
wtcDefaultExecutionStartToCloseTimeout = lens _wtcDefaultExecutionStartToCloseTimeout (\ s a -> s{_wtcDefaultExecutionStartToCloseTimeout = a});

-- | /Optional./ The default maximum duration, specified when registering the
-- workflow type, that a decision task for executions of this workflow type
-- might take before returning completion or failure. If the task does not
-- close in the specified time then the task is automatically timed out and
-- rescheduled. If the decider eventually reports a completion or failure,
-- it is ignored. This default can be overridden when starting a workflow
-- execution using the StartWorkflowExecution action or the
-- @StartChildWorkflowExecution@ Decision.
--
-- The duration is specified in seconds; an integer greater than or equal
-- to 0. The value \"NONE\" can be used to specify unlimited duration.
wtcDefaultTaskStartToCloseTimeout :: Lens' WorkflowTypeConfiguration (Maybe Text)
wtcDefaultTaskStartToCloseTimeout = lens _wtcDefaultTaskStartToCloseTimeout (\ s a -> s{_wtcDefaultTaskStartToCloseTimeout = a});

instance FromJSON WorkflowTypeConfiguration where
        parseJSON
          = withObject "WorkflowTypeConfiguration"
              (\ x ->
                 WorkflowTypeConfiguration' <$>
                   (x .:? "defaultChildPolicy") <*>
                     (x .:? "defaultTaskList")
                     <*> (x .:? "defaultTaskPriority")
                     <*> (x .:? "defaultExecutionStartToCloseTimeout")
                     <*> (x .:? "defaultTaskStartToCloseTimeout"))

-- | Used to filter workflow execution query results by type. Each parameter,
-- if specified, defines a rule that must be satisfied by each returned
-- result.
--
-- /See:/ 'workflowTypeFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wtfVersion'
--
-- * 'wtfName'
data WorkflowTypeFilter = WorkflowTypeFilter'
    { _wtfVersion :: Maybe Text
    , _wtfName    :: Text
    } deriving (Eq,Read,Show)

-- | 'WorkflowTypeFilter' smart constructor.
workflowTypeFilter :: Text -> WorkflowTypeFilter
workflowTypeFilter pName =
    WorkflowTypeFilter'
    { _wtfVersion = Nothing
    , _wtfName = pName
    }

-- | Version of the workflow type.
wtfVersion :: Lens' WorkflowTypeFilter (Maybe Text)
wtfVersion = lens _wtfVersion (\ s a -> s{_wtfVersion = a});

-- | __Required.__ Name of the workflow type.
wtfName :: Lens' WorkflowTypeFilter Text
wtfName = lens _wtfName (\ s a -> s{_wtfName = a});

instance ToJSON WorkflowTypeFilter where
        toJSON WorkflowTypeFilter'{..}
          = object
              ["version" .= _wtfVersion, "name" .= _wtfName]

-- | Contains information about a workflow type.
--
-- /See:/ 'workflowTypeInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wtiDeprecationDate'
--
-- * 'wtiDescription'
--
-- * 'wtiWorkflowType'
--
-- * 'wtiStatus'
--
-- * 'wtiCreationDate'
data WorkflowTypeInfo = WorkflowTypeInfo'
    { _wtiDeprecationDate :: Maybe POSIX
    , _wtiDescription     :: Maybe Text
    , _wtiWorkflowType    :: WorkflowType
    , _wtiStatus          :: RegistrationStatus
    , _wtiCreationDate    :: POSIX
    } deriving (Eq,Read,Show)

-- | 'WorkflowTypeInfo' smart constructor.
workflowTypeInfo :: WorkflowType -> RegistrationStatus -> UTCTime -> WorkflowTypeInfo
workflowTypeInfo pWorkflowType pStatus pCreationDate =
    WorkflowTypeInfo'
    { _wtiDeprecationDate = Nothing
    , _wtiDescription = Nothing
    , _wtiWorkflowType = pWorkflowType
    , _wtiStatus = pStatus
    , _wtiCreationDate = _Time # pCreationDate
    }

-- | If the type is in deprecated state, then it is set to the date when the
-- type was deprecated.
wtiDeprecationDate :: Lens' WorkflowTypeInfo (Maybe UTCTime)
wtiDeprecationDate = lens _wtiDeprecationDate (\ s a -> s{_wtiDeprecationDate = a}) . mapping _Time;

-- | The description of the type registered through RegisterWorkflowType.
wtiDescription :: Lens' WorkflowTypeInfo (Maybe Text)
wtiDescription = lens _wtiDescription (\ s a -> s{_wtiDescription = a});

-- | The workflow type this information is about.
wtiWorkflowType :: Lens' WorkflowTypeInfo WorkflowType
wtiWorkflowType = lens _wtiWorkflowType (\ s a -> s{_wtiWorkflowType = a});

-- | The current status of the workflow type.
wtiStatus :: Lens' WorkflowTypeInfo RegistrationStatus
wtiStatus = lens _wtiStatus (\ s a -> s{_wtiStatus = a});

-- | The date when this type was registered.
wtiCreationDate :: Lens' WorkflowTypeInfo UTCTime
wtiCreationDate = lens _wtiCreationDate (\ s a -> s{_wtiCreationDate = a}) . _Time;

instance FromJSON WorkflowTypeInfo where
        parseJSON
          = withObject "WorkflowTypeInfo"
              (\ x ->
                 WorkflowTypeInfo' <$>
                   (x .:? "deprecationDate") <*> (x .:? "description")
                     <*> (x .: "workflowType")
                     <*> (x .: "status")
                     <*> (x .: "creationDate"))
