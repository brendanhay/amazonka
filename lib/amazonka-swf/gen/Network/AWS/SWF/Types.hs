{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types
    (
    -- * Service Configuration
      swf

    -- * Errors
    , _DomainAlreadyExistsFault
    , _LimitExceededFault
    , _WorkflowExecutionAlreadyStartedFault
    , _OperationNotPermittedFault
    , _UnknownResourceFault
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

    -- * LambdaFunctionTimeoutType
    , LambdaFunctionTimeoutType (..)

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

    -- * ScheduleLambdaFunctionFailedCause
    , ScheduleLambdaFunctionFailedCause (..)

    -- * SignalExternalWorkflowExecutionFailedCause
    , SignalExternalWorkflowExecutionFailedCause (..)

    -- * StartChildWorkflowExecutionFailedCause
    , StartChildWorkflowExecutionFailedCause (..)

    -- * StartLambdaFunctionFailedCause
    , StartLambdaFunctionFailedCause (..)

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
    , aLatestCancelRequestedEventId
    , aDetails
    , aScheduledEventId
    , aStartedEventId

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
    , atseaHeartbeatTimeout
    , atseaScheduleToCloseTimeout
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
    , cCause
    , cDecisionTaskCompletedEventId

    -- * ChildWorkflowExecutionCanceledEventAttributes
    , ChildWorkflowExecutionCanceledEventAttributes
    , childWorkflowExecutionCanceledEventAttributes
    , cDetails
    , cWorkflowExecution
    , cWorkflowType
    , cInitiatedEventId
    , cStartedEventId

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
    , canwedaLambdaRole
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
    , dRequestCancelExternalWorkflowExecutionDecisionAttributes
    , dScheduleActivityTaskDecisionAttributes
    , dSignalExternalWorkflowExecutionDecisionAttributes
    , dStartTimerDecisionAttributes
    , dRecordMarkerDecisionAttributes
    , dFailWorkflowExecutionDecisionAttributes
    , dStartChildWorkflowExecutionDecisionAttributes
    , dCompleteWorkflowExecutionDecisionAttributes
    , dScheduleLambdaFunctionDecisionAttributes
    , dRequestCancelActivityTaskDecisionAttributes
    , dCancelWorkflowExecutionDecisionAttributes
    , dCancelTimerDecisionAttributes
    , dContinueAsNewWorkflowExecutionDecisionAttributes
    , dDecisionType

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
    , heRecordMarkerFailedEventAttributes
    , heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , heLambdaFunctionStartedEventAttributes
    , heDecisionTaskScheduledEventAttributes
    , heWorkflowExecutionCompletedEventAttributes
    , heStartTimerFailedEventAttributes
    , heActivityTaskScheduledEventAttributes
    , heScheduleActivityTaskFailedEventAttributes
    , heChildWorkflowExecutionCompletedEventAttributes
    , heMarkerRecordedEventAttributes
    , heScheduleLambdaFunctionFailedEventAttributes
    , heCompleteWorkflowExecutionFailedEventAttributes
    , heLambdaFunctionCompletedEventAttributes
    , heRequestCancelExternalWorkflowExecutionFailedEventAttributes
    , heTimerCanceledEventAttributes
    , heWorkflowExecutionStartedEventAttributes
    , heActivityTaskCompletedEventAttributes
    , heDecisionTaskTimedOutEventAttributes
    , heCancelTimerFailedEventAttributes
    , heChildWorkflowExecutionStartedEventAttributes
    , heActivityTaskCanceledEventAttributes
    , heActivityTaskTimedOutEventAttributes
    , heDecisionTaskStartedEventAttributes
    , heWorkflowExecutionTerminatedEventAttributes
    , heChildWorkflowExecutionCanceledEventAttributes
    , heRequestCancelActivityTaskFailedEventAttributes
    , heLambdaFunctionScheduledEventAttributes
    , heChildWorkflowExecutionTimedOutEventAttributes
    , heCancelWorkflowExecutionFailedEventAttributes
    , heStartChildWorkflowExecutionInitiatedEventAttributes
    , heSignalExternalWorkflowExecutionFailedEventAttributes
    , heActivityTaskStartedEventAttributes
    , heStartLambdaFunctionFailedEventAttributes
    , heChildWorkflowExecutionTerminatedEventAttributes
    , heLambdaFunctionFailedEventAttributes
    , heWorkflowExecutionCanceledEventAttributes
    , heTimerStartedEventAttributes
    , heActivityTaskCancelRequestedEventAttributes
    , heWorkflowExecutionTimedOutEventAttributes
    , heWorkflowExecutionSignaledEventAttributes
    , heTimerFiredEventAttributes
    , heActivityTaskFailedEventAttributes
    , heExternalWorkflowExecutionSignaledEventAttributes
    , heDecisionTaskCompletedEventAttributes
    , heStartChildWorkflowExecutionFailedEventAttributes
    , heChildWorkflowExecutionFailedEventAttributes
    , heFailWorkflowExecutionFailedEventAttributes
    , heContinueAsNewWorkflowExecutionFailedEventAttributes
    , heSignalExternalWorkflowExecutionInitiatedEventAttributes
    , heLambdaFunctionTimedOutEventAttributes
    , heWorkflowExecutionFailedEventAttributes
    , heWorkflowExecutionContinuedAsNewEventAttributes
    , heExternalWorkflowExecutionCancelRequestedEventAttributes
    , heEventTimestamp
    , heEventType
    , heEventId

    -- * LambdaFunctionCompletedEventAttributes
    , LambdaFunctionCompletedEventAttributes
    , lambdaFunctionCompletedEventAttributes
    , lfceaResult
    , lfceaScheduledEventId
    , lfceaStartedEventId

    -- * LambdaFunctionFailedEventAttributes
    , LambdaFunctionFailedEventAttributes
    , lambdaFunctionFailedEventAttributes
    , lffeaReason
    , lffeaDetails
    , lffeaScheduledEventId
    , lffeaStartedEventId

    -- * LambdaFunctionScheduledEventAttributes
    , LambdaFunctionScheduledEventAttributes
    , lambdaFunctionScheduledEventAttributes
    , lfseaControl
    , lfseaInput
    , lfseaStartToCloseTimeout
    , lfseaId
    , lfseaName
    , lfseaDecisionTaskCompletedEventId

    -- * LambdaFunctionStartedEventAttributes
    , LambdaFunctionStartedEventAttributes
    , lambdaFunctionStartedEventAttributes
    , lfseaScheduledEventId

    -- * LambdaFunctionTimedOutEventAttributes
    , LambdaFunctionTimedOutEventAttributes
    , lambdaFunctionTimedOutEventAttributes
    , lftoeaTimeoutType
    , lftoeaScheduledEventId
    , lftoeaStartedEventId

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
    , satdaHeartbeatTimeout
    , satdaScheduleToCloseTimeout
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

    -- * ScheduleLambdaFunctionDecisionAttributes
    , ScheduleLambdaFunctionDecisionAttributes
    , scheduleLambdaFunctionDecisionAttributes
    , slfdaControl
    , slfdaInput
    , slfdaStartToCloseTimeout
    , slfdaId
    , slfdaName

    -- * ScheduleLambdaFunctionFailedEventAttributes
    , ScheduleLambdaFunctionFailedEventAttributes
    , scheduleLambdaFunctionFailedEventAttributes
    , slffeaId
    , slffeaName
    , slffeaCause
    , slffeaDecisionTaskCompletedEventId

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
    , scwedaLambdaRole
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
    , scweieaLambdaRole
    , scweieaInput
    , scweieaExecutionStartToCloseTimeout
    , scweieaTaskPriority
    , scweieaWorkflowId
    , scweieaWorkflowType
    , scweieaTaskList
    , scweieaDecisionTaskCompletedEventId
    , scweieaChildPolicy

    -- * StartLambdaFunctionFailedEventAttributes
    , StartLambdaFunctionFailedEventAttributes
    , startLambdaFunctionFailedEventAttributes
    , sScheduledEventId
    , sCause
    , sMessage

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
    , wDetails
    , wDecisionTaskCompletedEventId

    -- * WorkflowExecutionCompletedEventAttributes
    , WorkflowExecutionCompletedEventAttributes
    , workflowExecutionCompletedEventAttributes
    , weceaResult
    , weceaDecisionTaskCompletedEventId

    -- * WorkflowExecutionConfiguration
    , WorkflowExecutionConfiguration
    , workflowExecutionConfiguration
    , wecLambdaRole
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
    , wecaneaLambdaRole
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
    , weocOpenLambdaFunctions
    , weocOpenActivityTasks
    , weocOpenDecisionTasks
    , weocOpenTimers
    , weocOpenChildWorkflowExecutions

    -- * WorkflowExecutionSignaledEventAttributes
    , WorkflowExecutionSignaledEventAttributes
    , workflowExecutionSignaledEventAttributes
    , wExternalWorkflowExecution
    , wExternalInitiatedEventId
    , wInput
    , wSignalName

    -- * WorkflowExecutionStartedEventAttributes
    , WorkflowExecutionStartedEventAttributes
    , workflowExecutionStartedEventAttributes
    , weseaParentInitiatedEventId
    , weseaTagList
    , weseaTaskStartToCloseTimeout
    , weseaLambdaRole
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
    , wtcDefaultLambdaRole
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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.SWF.Types.Product
import Network.AWS.SWF.Types.Sum

-- | API version @2012-01-25@ of the Amazon Simple Workflow Service SDK configuration.
swf :: Service
swf =
  Service
    { _svcAbbrev = "SWF"
    , _svcSigner = v4
    , _svcPrefix = "swf"
    , _svcVersion = "2012-01-25"
    , _svcEndpoint = defaultEndpoint swf
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "SWF"
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


-- | Returned if the specified domain already exists. You get this fault even if the existing domain is in deprecated status.
--
--
_DomainAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DomainAlreadyExistsFault = _MatchServiceError swf "DomainAlreadyExistsFault"


-- | Returned by any operation if a system imposed limitation has been reached. To address this fault you should either clean up unused resources or increase the limit by contacting AWS.
--
--
_LimitExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededFault = _MatchServiceError swf "LimitExceededFault"


-- | Returned by 'StartWorkflowExecution' when an open execution with the same workflowId is already running in the specified domain.
--
--
_WorkflowExecutionAlreadyStartedFault :: AsError a => Getting (First ServiceError) a ServiceError
_WorkflowExecutionAlreadyStartedFault =
  _MatchServiceError swf "WorkflowExecutionAlreadyStartedFault"


-- | Returned when the caller doesn't have sufficient permissions to invoke the action.
--
--
_OperationNotPermittedFault :: AsError a => Getting (First ServiceError) a ServiceError
_OperationNotPermittedFault =
  _MatchServiceError swf "OperationNotPermittedFault"


-- | Returned when the named resource cannot be found with in the scope of this operation (region or domain). This could happen if the named resource was never created or is no longer available for this operation.
--
--
_UnknownResourceFault :: AsError a => Getting (First ServiceError) a ServiceError
_UnknownResourceFault = _MatchServiceError swf "UnknownResourceFault"


-- | The @StartWorkflowExecution@ API action was called without the required parameters set.
--
--
-- Some workflow execution parameters, such as the decision @taskList@ , must be set to start the execution. However, these parameters might have been set as defaults when the workflow type was registered. In this case, you can omit these parameters from the @StartWorkflowExecution@ call and Amazon SWF uses the values defined in the workflow type.
--
_DefaultUndefinedFault :: AsError a => Getting (First ServiceError) a ServiceError
_DefaultUndefinedFault = _MatchServiceError swf "DefaultUndefinedFault"


-- | Returned when the specified activity or workflow type was already deprecated.
--
--
_TypeDeprecatedFault :: AsError a => Getting (First ServiceError) a ServiceError
_TypeDeprecatedFault = _MatchServiceError swf "TypeDeprecatedFault"


-- | Returned if the type already exists in the specified domain. You get this fault even if the existing type is in deprecated status. You can specify another version if the intent is to create a new distinct version of the type.
--
--
_TypeAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_TypeAlreadyExistsFault = _MatchServiceError swf "TypeAlreadyExistsFault"


-- | Returned when the specified domain has been deprecated.
--
--
_DomainDeprecatedFault :: AsError a => Getting (First ServiceError) a ServiceError
_DomainDeprecatedFault = _MatchServiceError swf "DomainDeprecatedFault"

