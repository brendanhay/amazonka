{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Simple Workflow Service__
--
-- The Amazon Simple Workflow Service (Amazon SWF) makes it easy to build applications that use Amazon's cloud to coordinate work across distributed components. In Amazon SWF, a /task/ represents a logical unit of work that is performed by a component of your workflow. Coordinating tasks in a workflow involves managing intertask dependencies, scheduling, and concurrency in accordance with the logical flow of the application.
--
-- Amazon SWF gives you full control over implementing tasks and coordinating them without worrying about underlying complexities such as tracking their progress and maintaining their state.
--
-- This documentation serves as reference only. For a broader overview of the Amazon SWF programming model, see the /<http:\/\/docs.aws.amazon.com\/amazonswf\/latest\/developerguide\/ Amazon SWF Developer Guide> / .
--
module Network.AWS.SWF
    (
    -- * Service Configuration
      swf

    -- * Errors
    -- $errors

    -- ** DomainAlreadyExistsFault
    , _DomainAlreadyExistsFault

    -- ** LimitExceededFault
    , _LimitExceededFault

    -- ** WorkflowExecutionAlreadyStartedFault
    , _WorkflowExecutionAlreadyStartedFault

    -- ** OperationNotPermittedFault
    , _OperationNotPermittedFault

    -- ** UnknownResourceFault
    , _UnknownResourceFault

    -- ** DefaultUndefinedFault
    , _DefaultUndefinedFault

    -- ** TypeDeprecatedFault
    , _TypeDeprecatedFault

    -- ** TypeAlreadyExistsFault
    , _TypeAlreadyExistsFault

    -- ** DomainDeprecatedFault
    , _DomainDeprecatedFault

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListOpenWorkflowExecutions (Paginated)
    , module Network.AWS.SWF.ListOpenWorkflowExecutions

    -- ** RegisterActivityType
    , module Network.AWS.SWF.RegisterActivityType

    -- ** ListActivityTypes (Paginated)
    , module Network.AWS.SWF.ListActivityTypes

    -- ** CountPendingActivityTasks
    , module Network.AWS.SWF.CountPendingActivityTasks

    -- ** RegisterWorkflowType
    , module Network.AWS.SWF.RegisterWorkflowType

    -- ** ListWorkflowTypes (Paginated)
    , module Network.AWS.SWF.ListWorkflowTypes

    -- ** RespondActivityTaskFailed
    , module Network.AWS.SWF.RespondActivityTaskFailed

    -- ** CountOpenWorkflowExecutions
    , module Network.AWS.SWF.CountOpenWorkflowExecutions

    -- ** DescribeWorkflowType
    , module Network.AWS.SWF.DescribeWorkflowType

    -- ** DeprecateWorkflowType
    , module Network.AWS.SWF.DeprecateWorkflowType

    -- ** RequestCancelWorkflowExecution
    , module Network.AWS.SWF.RequestCancelWorkflowExecution

    -- ** RegisterDomain
    , module Network.AWS.SWF.RegisterDomain

    -- ** RespondDecisionTaskCompleted
    , module Network.AWS.SWF.RespondDecisionTaskCompleted

    -- ** PollForActivityTask
    , module Network.AWS.SWF.PollForActivityTask

    -- ** RespondActivityTaskCompleted
    , module Network.AWS.SWF.RespondActivityTaskCompleted

    -- ** DescribeWorkflowExecution
    , module Network.AWS.SWF.DescribeWorkflowExecution

    -- ** SignalWorkflowExecution
    , module Network.AWS.SWF.SignalWorkflowExecution

    -- ** CountPendingDecisionTasks
    , module Network.AWS.SWF.CountPendingDecisionTasks

    -- ** ListClosedWorkflowExecutions (Paginated)
    , module Network.AWS.SWF.ListClosedWorkflowExecutions

    -- ** RecordActivityTaskHeartbeat
    , module Network.AWS.SWF.RecordActivityTaskHeartbeat

    -- ** DescribeDomain
    , module Network.AWS.SWF.DescribeDomain

    -- ** GetWorkflowExecutionHistory (Paginated)
    , module Network.AWS.SWF.GetWorkflowExecutionHistory

    -- ** DeprecateDomain
    , module Network.AWS.SWF.DeprecateDomain

    -- ** TerminateWorkflowExecution
    , module Network.AWS.SWF.TerminateWorkflowExecution

    -- ** DescribeActivityType
    , module Network.AWS.SWF.DescribeActivityType

    -- ** DeprecateActivityType
    , module Network.AWS.SWF.DeprecateActivityType

    -- ** CountClosedWorkflowExecutions
    , module Network.AWS.SWF.CountClosedWorkflowExecutions

    -- ** RespondActivityTaskCanceled
    , module Network.AWS.SWF.RespondActivityTaskCanceled

    -- ** StartWorkflowExecution
    , module Network.AWS.SWF.StartWorkflowExecution

    -- ** PollForDecisionTask (Paginated)
    , module Network.AWS.SWF.PollForDecisionTask

    -- ** ListDomains (Paginated)
    , module Network.AWS.SWF.ListDomains

    -- * Types

    -- ** ActivityTaskTimeoutType
    , ActivityTaskTimeoutType (..)

    -- ** CancelTimerFailedCause
    , CancelTimerFailedCause (..)

    -- ** CancelWorkflowExecutionFailedCause
    , CancelWorkflowExecutionFailedCause (..)

    -- ** ChildPolicy
    , ChildPolicy (..)

    -- ** CloseStatus
    , CloseStatus (..)

    -- ** CompleteWorkflowExecutionFailedCause
    , CompleteWorkflowExecutionFailedCause (..)

    -- ** ContinueAsNewWorkflowExecutionFailedCause
    , ContinueAsNewWorkflowExecutionFailedCause (..)

    -- ** DecisionTaskTimeoutType
    , DecisionTaskTimeoutType (..)

    -- ** DecisionType
    , DecisionType (..)

    -- ** EventType
    , EventType (..)

    -- ** ExecutionStatus
    , ExecutionStatus (..)

    -- ** FailWorkflowExecutionFailedCause
    , FailWorkflowExecutionFailedCause (..)

    -- ** LambdaFunctionTimeoutType
    , LambdaFunctionTimeoutType (..)

    -- ** RecordMarkerFailedCause
    , RecordMarkerFailedCause (..)

    -- ** RegistrationStatus
    , RegistrationStatus (..)

    -- ** RequestCancelActivityTaskFailedCause
    , RequestCancelActivityTaskFailedCause (..)

    -- ** RequestCancelExternalWorkflowExecutionFailedCause
    , RequestCancelExternalWorkflowExecutionFailedCause (..)

    -- ** ScheduleActivityTaskFailedCause
    , ScheduleActivityTaskFailedCause (..)

    -- ** ScheduleLambdaFunctionFailedCause
    , ScheduleLambdaFunctionFailedCause (..)

    -- ** SignalExternalWorkflowExecutionFailedCause
    , SignalExternalWorkflowExecutionFailedCause (..)

    -- ** StartChildWorkflowExecutionFailedCause
    , StartChildWorkflowExecutionFailedCause (..)

    -- ** StartLambdaFunctionFailedCause
    , StartLambdaFunctionFailedCause (..)

    -- ** StartTimerFailedCause
    , StartTimerFailedCause (..)

    -- ** WorkflowExecutionCancelRequestedCause
    , WorkflowExecutionCancelRequestedCause (..)

    -- ** WorkflowExecutionTerminatedCause
    , WorkflowExecutionTerminatedCause (..)

    -- ** WorkflowExecutionTimeoutType
    , WorkflowExecutionTimeoutType (..)

    -- ** ActivityTaskCancelRequestedEventAttributes
    , ActivityTaskCancelRequestedEventAttributes
    , activityTaskCancelRequestedEventAttributes
    , atcreaDecisionTaskCompletedEventId
    , atcreaActivityId

    -- ** ActivityTaskCanceledEventAttributes
    , ActivityTaskCanceledEventAttributes
    , activityTaskCanceledEventAttributes
    , aLatestCancelRequestedEventId
    , aDetails
    , aScheduledEventId
    , aStartedEventId

    -- ** ActivityTaskCompletedEventAttributes
    , ActivityTaskCompletedEventAttributes
    , activityTaskCompletedEventAttributes
    , atceaResult
    , atceaScheduledEventId
    , atceaStartedEventId

    -- ** ActivityTaskFailedEventAttributes
    , ActivityTaskFailedEventAttributes
    , activityTaskFailedEventAttributes
    , atfeaReason
    , atfeaDetails
    , atfeaScheduledEventId
    , atfeaStartedEventId

    -- ** ActivityTaskScheduledEventAttributes
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

    -- ** ActivityTaskStartedEventAttributes
    , ActivityTaskStartedEventAttributes
    , activityTaskStartedEventAttributes
    , atseaIdentity
    , atseaScheduledEventId

    -- ** ActivityTaskTimedOutEventAttributes
    , ActivityTaskTimedOutEventAttributes
    , activityTaskTimedOutEventAttributes
    , attoeaDetails
    , attoeaTimeoutType
    , attoeaScheduledEventId
    , attoeaStartedEventId

    -- ** ActivityType
    , ActivityType
    , activityType
    , atName
    , atVersion

    -- ** ActivityTypeConfiguration
    , ActivityTypeConfiguration
    , activityTypeConfiguration
    , atcDefaultTaskScheduleToStartTimeout
    , atcDefaultTaskList
    , atcDefaultTaskPriority
    , atcDefaultTaskHeartbeatTimeout
    , atcDefaultTaskScheduleToCloseTimeout
    , atcDefaultTaskStartToCloseTimeout

    -- ** ActivityTypeInfo
    , ActivityTypeInfo
    , activityTypeInfo
    , atiDeprecationDate
    , atiDescription
    , atiActivityType
    , atiStatus
    , atiCreationDate

    -- ** CancelTimerDecisionAttributes
    , CancelTimerDecisionAttributes
    , cancelTimerDecisionAttributes
    , ctdaTimerId

    -- ** CancelTimerFailedEventAttributes
    , CancelTimerFailedEventAttributes
    , cancelTimerFailedEventAttributes
    , ctfeaTimerId
    , ctfeaCause
    , ctfeaDecisionTaskCompletedEventId

    -- ** CancelWorkflowExecutionDecisionAttributes
    , CancelWorkflowExecutionDecisionAttributes
    , cancelWorkflowExecutionDecisionAttributes
    , cwedaDetails

    -- ** CancelWorkflowExecutionFailedEventAttributes
    , CancelWorkflowExecutionFailedEventAttributes
    , cancelWorkflowExecutionFailedEventAttributes
    , cCause
    , cDecisionTaskCompletedEventId

    -- ** ChildWorkflowExecutionCanceledEventAttributes
    , ChildWorkflowExecutionCanceledEventAttributes
    , childWorkflowExecutionCanceledEventAttributes
    , cDetails
    , cWorkflowExecution
    , cWorkflowType
    , cInitiatedEventId
    , cStartedEventId

    -- ** ChildWorkflowExecutionCompletedEventAttributes
    , ChildWorkflowExecutionCompletedEventAttributes
    , childWorkflowExecutionCompletedEventAttributes
    , cweceaResult
    , cweceaWorkflowExecution
    , cweceaWorkflowType
    , cweceaInitiatedEventId
    , cweceaStartedEventId

    -- ** ChildWorkflowExecutionFailedEventAttributes
    , ChildWorkflowExecutionFailedEventAttributes
    , childWorkflowExecutionFailedEventAttributes
    , cwefeaReason
    , cwefeaDetails
    , cwefeaWorkflowExecution
    , cwefeaWorkflowType
    , cwefeaInitiatedEventId
    , cwefeaStartedEventId

    -- ** ChildWorkflowExecutionStartedEventAttributes
    , ChildWorkflowExecutionStartedEventAttributes
    , childWorkflowExecutionStartedEventAttributes
    , cweseaWorkflowExecution
    , cweseaWorkflowType
    , cweseaInitiatedEventId

    -- ** ChildWorkflowExecutionTerminatedEventAttributes
    , ChildWorkflowExecutionTerminatedEventAttributes
    , childWorkflowExecutionTerminatedEventAttributes
    , cweteaWorkflowExecution
    , cweteaWorkflowType
    , cweteaInitiatedEventId
    , cweteaStartedEventId

    -- ** ChildWorkflowExecutionTimedOutEventAttributes
    , ChildWorkflowExecutionTimedOutEventAttributes
    , childWorkflowExecutionTimedOutEventAttributes
    , cwetoeaWorkflowExecution
    , cwetoeaWorkflowType
    , cwetoeaTimeoutType
    , cwetoeaInitiatedEventId
    , cwetoeaStartedEventId

    -- ** CloseStatusFilter
    , CloseStatusFilter
    , closeStatusFilter
    , csfStatus

    -- ** CompleteWorkflowExecutionDecisionAttributes
    , CompleteWorkflowExecutionDecisionAttributes
    , completeWorkflowExecutionDecisionAttributes
    , cwedaResult

    -- ** CompleteWorkflowExecutionFailedEventAttributes
    , CompleteWorkflowExecutionFailedEventAttributes
    , completeWorkflowExecutionFailedEventAttributes
    , cwefeaCause
    , cwefeaDecisionTaskCompletedEventId

    -- ** ContinueAsNewWorkflowExecutionDecisionAttributes
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

    -- ** ContinueAsNewWorkflowExecutionFailedEventAttributes
    , ContinueAsNewWorkflowExecutionFailedEventAttributes
    , continueAsNewWorkflowExecutionFailedEventAttributes
    , canwefeaCause
    , canwefeaDecisionTaskCompletedEventId

    -- ** Decision
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

    -- ** DecisionTaskCompletedEventAttributes
    , DecisionTaskCompletedEventAttributes
    , decisionTaskCompletedEventAttributes
    , dtceaExecutionContext
    , dtceaScheduledEventId
    , dtceaStartedEventId

    -- ** DecisionTaskScheduledEventAttributes
    , DecisionTaskScheduledEventAttributes
    , decisionTaskScheduledEventAttributes
    , dtseaTaskPriority
    , dtseaStartToCloseTimeout
    , dtseaTaskList

    -- ** DecisionTaskStartedEventAttributes
    , DecisionTaskStartedEventAttributes
    , decisionTaskStartedEventAttributes
    , dtseaIdentity
    , dtseaScheduledEventId

    -- ** DecisionTaskTimedOutEventAttributes
    , DecisionTaskTimedOutEventAttributes
    , decisionTaskTimedOutEventAttributes
    , dttoeaTimeoutType
    , dttoeaScheduledEventId
    , dttoeaStartedEventId

    -- ** DomainConfiguration
    , DomainConfiguration
    , domainConfiguration
    , dcWorkflowExecutionRetentionPeriodInDays

    -- ** DomainInfo
    , DomainInfo
    , domainInfo
    , diDescription
    , diName
    , diStatus

    -- ** ExecutionTimeFilter
    , ExecutionTimeFilter
    , executionTimeFilter
    , etfLatestDate
    , etfOldestDate

    -- ** ExternalWorkflowExecutionCancelRequestedEventAttributes
    , ExternalWorkflowExecutionCancelRequestedEventAttributes
    , externalWorkflowExecutionCancelRequestedEventAttributes
    , ewecreaWorkflowExecution
    , ewecreaInitiatedEventId

    -- ** ExternalWorkflowExecutionSignaledEventAttributes
    , ExternalWorkflowExecutionSignaledEventAttributes
    , externalWorkflowExecutionSignaledEventAttributes
    , eweseaWorkflowExecution
    , eweseaInitiatedEventId

    -- ** FailWorkflowExecutionDecisionAttributes
    , FailWorkflowExecutionDecisionAttributes
    , failWorkflowExecutionDecisionAttributes
    , fwedaReason
    , fwedaDetails

    -- ** FailWorkflowExecutionFailedEventAttributes
    , FailWorkflowExecutionFailedEventAttributes
    , failWorkflowExecutionFailedEventAttributes
    , fwefeaCause
    , fwefeaDecisionTaskCompletedEventId

    -- ** HistoryEvent
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

    -- ** LambdaFunctionCompletedEventAttributes
    , LambdaFunctionCompletedEventAttributes
    , lambdaFunctionCompletedEventAttributes
    , lfceaResult
    , lfceaScheduledEventId
    , lfceaStartedEventId

    -- ** LambdaFunctionFailedEventAttributes
    , LambdaFunctionFailedEventAttributes
    , lambdaFunctionFailedEventAttributes
    , lffeaReason
    , lffeaDetails
    , lffeaScheduledEventId
    , lffeaStartedEventId

    -- ** LambdaFunctionScheduledEventAttributes
    , LambdaFunctionScheduledEventAttributes
    , lambdaFunctionScheduledEventAttributes
    , lfseaControl
    , lfseaInput
    , lfseaStartToCloseTimeout
    , lfseaId
    , lfseaName
    , lfseaDecisionTaskCompletedEventId

    -- ** LambdaFunctionStartedEventAttributes
    , LambdaFunctionStartedEventAttributes
    , lambdaFunctionStartedEventAttributes
    , lfseaScheduledEventId

    -- ** LambdaFunctionTimedOutEventAttributes
    , LambdaFunctionTimedOutEventAttributes
    , lambdaFunctionTimedOutEventAttributes
    , lftoeaTimeoutType
    , lftoeaScheduledEventId
    , lftoeaStartedEventId

    -- ** MarkerRecordedEventAttributes
    , MarkerRecordedEventAttributes
    , markerRecordedEventAttributes
    , mreaDetails
    , mreaMarkerName
    , mreaDecisionTaskCompletedEventId

    -- ** PendingTaskCount
    , PendingTaskCount
    , pendingTaskCount
    , ptcTruncated
    , ptcCount

    -- ** RecordMarkerDecisionAttributes
    , RecordMarkerDecisionAttributes
    , recordMarkerDecisionAttributes
    , rmdaDetails
    , rmdaMarkerName

    -- ** RecordMarkerFailedEventAttributes
    , RecordMarkerFailedEventAttributes
    , recordMarkerFailedEventAttributes
    , rmfeaMarkerName
    , rmfeaCause
    , rmfeaDecisionTaskCompletedEventId

    -- ** RequestCancelActivityTaskDecisionAttributes
    , RequestCancelActivityTaskDecisionAttributes
    , requestCancelActivityTaskDecisionAttributes
    , rcatdaActivityId

    -- ** RequestCancelActivityTaskFailedEventAttributes
    , RequestCancelActivityTaskFailedEventAttributes
    , requestCancelActivityTaskFailedEventAttributes
    , rcatfeaActivityId
    , rcatfeaCause
    , rcatfeaDecisionTaskCompletedEventId

    -- ** RequestCancelExternalWorkflowExecutionDecisionAttributes
    , RequestCancelExternalWorkflowExecutionDecisionAttributes
    , requestCancelExternalWorkflowExecutionDecisionAttributes
    , rcewedaControl
    , rcewedaRunId
    , rcewedaWorkflowId

    -- ** RequestCancelExternalWorkflowExecutionFailedEventAttributes
    , RequestCancelExternalWorkflowExecutionFailedEventAttributes
    , requestCancelExternalWorkflowExecutionFailedEventAttributes
    , rcewefeaControl
    , rcewefeaRunId
    , rcewefeaWorkflowId
    , rcewefeaCause
    , rcewefeaInitiatedEventId
    , rcewefeaDecisionTaskCompletedEventId

    -- ** RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , requestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , rceweieaControl
    , rceweieaRunId
    , rceweieaWorkflowId
    , rceweieaDecisionTaskCompletedEventId

    -- ** ScheduleActivityTaskDecisionAttributes
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

    -- ** ScheduleActivityTaskFailedEventAttributes
    , ScheduleActivityTaskFailedEventAttributes
    , scheduleActivityTaskFailedEventAttributes
    , satfeaActivityType
    , satfeaActivityId
    , satfeaCause
    , satfeaDecisionTaskCompletedEventId

    -- ** ScheduleLambdaFunctionDecisionAttributes
    , ScheduleLambdaFunctionDecisionAttributes
    , scheduleLambdaFunctionDecisionAttributes
    , slfdaControl
    , slfdaInput
    , slfdaStartToCloseTimeout
    , slfdaId
    , slfdaName

    -- ** ScheduleLambdaFunctionFailedEventAttributes
    , ScheduleLambdaFunctionFailedEventAttributes
    , scheduleLambdaFunctionFailedEventAttributes
    , slffeaId
    , slffeaName
    , slffeaCause
    , slffeaDecisionTaskCompletedEventId

    -- ** SignalExternalWorkflowExecutionDecisionAttributes
    , SignalExternalWorkflowExecutionDecisionAttributes
    , signalExternalWorkflowExecutionDecisionAttributes
    , sewedaControl
    , sewedaInput
    , sewedaRunId
    , sewedaWorkflowId
    , sewedaSignalName

    -- ** SignalExternalWorkflowExecutionFailedEventAttributes
    , SignalExternalWorkflowExecutionFailedEventAttributes
    , signalExternalWorkflowExecutionFailedEventAttributes
    , sewefeaControl
    , sewefeaRunId
    , sewefeaWorkflowId
    , sewefeaCause
    , sewefeaInitiatedEventId
    , sewefeaDecisionTaskCompletedEventId

    -- ** SignalExternalWorkflowExecutionInitiatedEventAttributes
    , SignalExternalWorkflowExecutionInitiatedEventAttributes
    , signalExternalWorkflowExecutionInitiatedEventAttributes
    , seweieaControl
    , seweieaInput
    , seweieaRunId
    , seweieaWorkflowId
    , seweieaSignalName
    , seweieaDecisionTaskCompletedEventId

    -- ** StartChildWorkflowExecutionDecisionAttributes
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

    -- ** StartChildWorkflowExecutionFailedEventAttributes
    , StartChildWorkflowExecutionFailedEventAttributes
    , startChildWorkflowExecutionFailedEventAttributes
    , scwefeaControl
    , scwefeaWorkflowType
    , scwefeaCause
    , scwefeaWorkflowId
    , scwefeaInitiatedEventId
    , scwefeaDecisionTaskCompletedEventId

    -- ** StartChildWorkflowExecutionInitiatedEventAttributes
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

    -- ** StartLambdaFunctionFailedEventAttributes
    , StartLambdaFunctionFailedEventAttributes
    , startLambdaFunctionFailedEventAttributes
    , sScheduledEventId
    , sCause
    , sMessage

    -- ** StartTimerDecisionAttributes
    , StartTimerDecisionAttributes
    , startTimerDecisionAttributes
    , stdaControl
    , stdaTimerId
    , stdaStartToFireTimeout

    -- ** StartTimerFailedEventAttributes
    , StartTimerFailedEventAttributes
    , startTimerFailedEventAttributes
    , stfeaTimerId
    , stfeaCause
    , stfeaDecisionTaskCompletedEventId

    -- ** TagFilter
    , TagFilter
    , tagFilter
    , tfTag

    -- ** TaskList
    , TaskList
    , taskList
    , tlName

    -- ** TimerCanceledEventAttributes
    , TimerCanceledEventAttributes
    , timerCanceledEventAttributes
    , tceaTimerId
    , tceaStartedEventId
    , tceaDecisionTaskCompletedEventId

    -- ** TimerFiredEventAttributes
    , TimerFiredEventAttributes
    , timerFiredEventAttributes
    , tfeaTimerId
    , tfeaStartedEventId

    -- ** TimerStartedEventAttributes
    , TimerStartedEventAttributes
    , timerStartedEventAttributes
    , tseaControl
    , tseaTimerId
    , tseaStartToFireTimeout
    , tseaDecisionTaskCompletedEventId

    -- ** WorkflowExecution
    , WorkflowExecution
    , workflowExecution
    , weWorkflowId
    , weRunId

    -- ** WorkflowExecutionCancelRequestedEventAttributes
    , WorkflowExecutionCancelRequestedEventAttributes
    , workflowExecutionCancelRequestedEventAttributes
    , wecreaExternalWorkflowExecution
    , wecreaExternalInitiatedEventId
    , wecreaCause

    -- ** WorkflowExecutionCanceledEventAttributes
    , WorkflowExecutionCanceledEventAttributes
    , workflowExecutionCanceledEventAttributes
    , wDetails
    , wDecisionTaskCompletedEventId

    -- ** WorkflowExecutionCompletedEventAttributes
    , WorkflowExecutionCompletedEventAttributes
    , workflowExecutionCompletedEventAttributes
    , weceaResult
    , weceaDecisionTaskCompletedEventId

    -- ** WorkflowExecutionConfiguration
    , WorkflowExecutionConfiguration
    , workflowExecutionConfiguration
    , wecLambdaRole
    , wecTaskPriority
    , wecTaskStartToCloseTimeout
    , wecExecutionStartToCloseTimeout
    , wecTaskList
    , wecChildPolicy

    -- ** WorkflowExecutionContinuedAsNewEventAttributes
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

    -- ** WorkflowExecutionCount
    , WorkflowExecutionCount
    , workflowExecutionCount
    , wecTruncated
    , wecCount

    -- ** WorkflowExecutionFailedEventAttributes
    , WorkflowExecutionFailedEventAttributes
    , workflowExecutionFailedEventAttributes
    , wefeaReason
    , wefeaDetails
    , wefeaDecisionTaskCompletedEventId

    -- ** WorkflowExecutionFilter
    , WorkflowExecutionFilter
    , workflowExecutionFilter
    , wefWorkflowId

    -- ** WorkflowExecutionInfo
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

    -- ** WorkflowExecutionInfos
    , WorkflowExecutionInfos
    , workflowExecutionInfos
    , weiNextPageToken
    , weiExecutionInfos

    -- ** WorkflowExecutionOpenCounts
    , WorkflowExecutionOpenCounts
    , workflowExecutionOpenCounts
    , weocOpenLambdaFunctions
    , weocOpenActivityTasks
    , weocOpenDecisionTasks
    , weocOpenTimers
    , weocOpenChildWorkflowExecutions

    -- ** WorkflowExecutionSignaledEventAttributes
    , WorkflowExecutionSignaledEventAttributes
    , workflowExecutionSignaledEventAttributes
    , wExternalWorkflowExecution
    , wExternalInitiatedEventId
    , wInput
    , wSignalName

    -- ** WorkflowExecutionStartedEventAttributes
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

    -- ** WorkflowExecutionTerminatedEventAttributes
    , WorkflowExecutionTerminatedEventAttributes
    , workflowExecutionTerminatedEventAttributes
    , weteaCause
    , weteaReason
    , weteaDetails
    , weteaChildPolicy

    -- ** WorkflowExecutionTimedOutEventAttributes
    , WorkflowExecutionTimedOutEventAttributes
    , workflowExecutionTimedOutEventAttributes
    , wetoeaTimeoutType
    , wetoeaChildPolicy

    -- ** WorkflowType
    , WorkflowType
    , workflowType
    , wtName
    , wtVersion

    -- ** WorkflowTypeConfiguration
    , WorkflowTypeConfiguration
    , workflowTypeConfiguration
    , wtcDefaultLambdaRole
    , wtcDefaultChildPolicy
    , wtcDefaultTaskList
    , wtcDefaultTaskPriority
    , wtcDefaultExecutionStartToCloseTimeout
    , wtcDefaultTaskStartToCloseTimeout

    -- ** WorkflowTypeFilter
    , WorkflowTypeFilter
    , workflowTypeFilter
    , wtfVersion
    , wtfName

    -- ** WorkflowTypeInfo
    , WorkflowTypeInfo
    , workflowTypeInfo
    , wtiDeprecationDate
    , wtiDescription
    , wtiWorkflowType
    , wtiStatus
    , wtiCreationDate
    ) where

import Network.AWS.SWF.CountClosedWorkflowExecutions
import Network.AWS.SWF.CountOpenWorkflowExecutions
import Network.AWS.SWF.CountPendingActivityTasks
import Network.AWS.SWF.CountPendingDecisionTasks
import Network.AWS.SWF.DeprecateActivityType
import Network.AWS.SWF.DeprecateDomain
import Network.AWS.SWF.DeprecateWorkflowType
import Network.AWS.SWF.DescribeActivityType
import Network.AWS.SWF.DescribeDomain
import Network.AWS.SWF.DescribeWorkflowExecution
import Network.AWS.SWF.DescribeWorkflowType
import Network.AWS.SWF.GetWorkflowExecutionHistory
import Network.AWS.SWF.ListActivityTypes
import Network.AWS.SWF.ListClosedWorkflowExecutions
import Network.AWS.SWF.ListDomains
import Network.AWS.SWF.ListOpenWorkflowExecutions
import Network.AWS.SWF.ListWorkflowTypes
import Network.AWS.SWF.PollForActivityTask
import Network.AWS.SWF.PollForDecisionTask
import Network.AWS.SWF.RecordActivityTaskHeartbeat
import Network.AWS.SWF.RegisterActivityType
import Network.AWS.SWF.RegisterDomain
import Network.AWS.SWF.RegisterWorkflowType
import Network.AWS.SWF.RequestCancelWorkflowExecution
import Network.AWS.SWF.RespondActivityTaskCanceled
import Network.AWS.SWF.RespondActivityTaskCompleted
import Network.AWS.SWF.RespondActivityTaskFailed
import Network.AWS.SWF.RespondDecisionTaskCompleted
import Network.AWS.SWF.SignalWorkflowExecution
import Network.AWS.SWF.StartWorkflowExecution
import Network.AWS.SWF.TerminateWorkflowExecution
import Network.AWS.SWF.Types
import Network.AWS.SWF.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'SWF'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
