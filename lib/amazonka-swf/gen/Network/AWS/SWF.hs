{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Simple Workflow Service__ 
--
-- The Amazon Simple Workflow Service (Amazon SWF) makes it easy to build applications that use Amazon's cloud to coordinate work across distributed components. In Amazon SWF, a /task/ represents a logical unit of work that is performed by a component of your workflow. Coordinating tasks in a workflow involves managing intertask dependencies, scheduling, and concurrency in accordance with the logical flow of the application.
-- Amazon SWF gives you full control over implementing tasks and coordinating them without worrying about underlying complexities such as tracking their progress and maintaining their state.
-- This documentation serves as reference only. For a broader overview of the Amazon SWF programming model, see the /<https:\/\/docs.aws.amazon.com\/amazonswf\/latest\/developerguide\/ Amazon SWF Developer Guide> / .
module Network.AWS.SWF
    (
    -- * Service configuration
      mkServiceConfig

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

    -- ** TooManyTagsFault
    , _TooManyTagsFault

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

    -- ** ListTagsForResource 
    , module Network.AWS.SWF.ListTagsForResource

    -- ** RespondActivityTaskFailed 
    , module Network.AWS.SWF.RespondActivityTaskFailed

    -- ** CountOpenWorkflowExecutions 
    , module Network.AWS.SWF.CountOpenWorkflowExecutions

    -- ** UndeprecateDomain 
    , module Network.AWS.SWF.UndeprecateDomain

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

    -- ** UndeprecateWorkflowType 
    , module Network.AWS.SWF.UndeprecateWorkflowType

    -- ** TerminateWorkflowExecution 
    , module Network.AWS.SWF.TerminateWorkflowExecution

    -- ** DescribeActivityType 
    , module Network.AWS.SWF.DescribeActivityType

    -- ** TagResource 
    , module Network.AWS.SWF.TagResource

    -- ** DeprecateActivityType 
    , module Network.AWS.SWF.DeprecateActivityType

    -- ** UndeprecateActivityType 
    , module Network.AWS.SWF.UndeprecateActivityType

    -- ** CountClosedWorkflowExecutions 
    , module Network.AWS.SWF.CountClosedWorkflowExecutions

    -- ** UntagResource 
    , module Network.AWS.SWF.UntagResource

    -- ** RespondActivityTaskCanceled 
    , module Network.AWS.SWF.RespondActivityTaskCanceled

    -- ** StartWorkflowExecution 
    , module Network.AWS.SWF.StartWorkflowExecution

    -- ** PollForDecisionTask (Paginated)
    , module Network.AWS.SWF.PollForDecisionTask

    -- ** ListDomains (Paginated)
    , module Network.AWS.SWF.ListDomains

    -- * Types

    -- ** FailureReason
    , FailureReason (..)

    -- ** WorkflowExecutionCancelRequestedEventAttributes
    , WorkflowExecutionCancelRequestedEventAttributes (..)
    , mkWorkflowExecutionCancelRequestedEventAttributes
    , wecreaCause
    , wecreaExternalInitiatedEventId
    , wecreaExternalWorkflowExecution

    -- ** RequestCancelExternalWorkflowExecutionDecisionAttributes
    , RequestCancelExternalWorkflowExecutionDecisionAttributes (..)
    , mkRequestCancelExternalWorkflowExecutionDecisionAttributes
    , rcewedaWorkflowId
    , rcewedaControl
    , rcewedaRunId

    -- ** LimitedData
    , LimitedData (..)

    -- ** DecisionTaskScheduledEventAttributes
    , DecisionTaskScheduledEventAttributes (..)
    , mkDecisionTaskScheduledEventAttributes
    , dtseaTaskList
    , dtseaStartToCloseTimeout
    , dtseaTaskPriority

    -- ** ResourceTagKey
    , ResourceTagKey (..)

    -- ** WorkflowExecutionCompletedEventAttributes
    , WorkflowExecutionCompletedEventAttributes (..)
    , mkWorkflowExecutionCompletedEventAttributes
    , wDecisionTaskCompletedEventId
    , wResult

    -- ** LambdaFunctionStartedEventAttributes
    , LambdaFunctionStartedEventAttributes (..)
    , mkLambdaFunctionStartedEventAttributes
    , lfseaScheduledEventId

    -- ** ExecutionTimeFilter
    , ExecutionTimeFilter (..)
    , mkExecutionTimeFilter
    , etfOldestDate
    , etfLatestDate

    -- ** StartLambdaFunctionFailedCause
    , StartLambdaFunctionFailedCause (..)

    -- ** StartTimerFailedEventAttributes
    , StartTimerFailedEventAttributes (..)
    , mkStartTimerFailedEventAttributes
    , stfeaTimerId
    , stfeaCause
    , stfeaDecisionTaskCompletedEventId

    -- ** RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (..)
    , mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , rceweieaWorkflowId
    , rceweieaDecisionTaskCompletedEventId
    , rceweieaControl
    , rceweieaRunId

    -- ** RecordMarkerFailedEventAttributes
    , RecordMarkerFailedEventAttributes (..)
    , mkRecordMarkerFailedEventAttributes
    , rmfeaMarkerName
    , rmfeaCause
    , rmfeaDecisionTaskCompletedEventId

    -- ** WorkflowExecutionCount
    , WorkflowExecutionCount (..)
    , mkWorkflowExecutionCount
    , wecCount
    , wecTruncated

    -- ** ActivityTaskScheduledEventAttributes
    , ActivityTaskScheduledEventAttributes (..)
    , mkActivityTaskScheduledEventAttributes
    , atseaActivityType
    , atseaActivityId
    , atseaTaskList
    , atseaDecisionTaskCompletedEventId
    , atseaControl
    , atseaHeartbeatTimeout
    , atseaInput
    , atseaScheduleToCloseTimeout
    , atseaScheduleToStartTimeout
    , atseaStartToCloseTimeout
    , atseaTaskPriority

    -- ** CloseStatusFilter
    , CloseStatusFilter (..)
    , mkCloseStatusFilter
    , csfStatus

    -- ** Tag
    , Tag (..)

    -- ** WorkflowExecutionTimeoutType
    , WorkflowExecutionTimeoutType (..)

    -- ** ScheduleActivityTaskDecisionAttributes
    , ScheduleActivityTaskDecisionAttributes (..)
    , mkScheduleActivityTaskDecisionAttributes
    , satdaActivityType
    , satdaActivityId
    , satdaControl
    , satdaHeartbeatTimeout
    , satdaInput
    , satdaScheduleToCloseTimeout
    , satdaScheduleToStartTimeout
    , satdaStartToCloseTimeout
    , satdaTaskList
    , satdaTaskPriority

    -- ** CauseMessage
    , CauseMessage (..)

    -- ** MarkerName
    , MarkerName (..)

    -- ** ActivityTypeConfiguration
    , ActivityTypeConfiguration (..)
    , mkActivityTypeConfiguration
    , atcDefaultTaskHeartbeatTimeout
    , atcDefaultTaskList
    , atcDefaultTaskPriority
    , atcDefaultTaskScheduleToCloseTimeout
    , atcDefaultTaskScheduleToStartTimeout
    , atcDefaultTaskStartToCloseTimeout

    -- ** Arn
    , Arn (..)

    -- ** ActivityType
    , ActivityType (..)
    , mkActivityType
    , atName
    , atVersion

    -- ** WorkflowTypeInfo
    , WorkflowTypeInfo (..)
    , mkWorkflowTypeInfo
    , wtiWorkflowType
    , wtiStatus
    , wtiCreationDate
    , wtiDeprecationDate
    , wtiDescription

    -- ** ChildWorkflowExecutionCompletedEventAttributes
    , ChildWorkflowExecutionCompletedEventAttributes (..)
    , mkChildWorkflowExecutionCompletedEventAttributes
    , cWorkflowExecution
    , cWorkflowType
    , cInitiatedEventId
    , cStartedEventId
    , cResult

    -- ** WorkflowExecutionOpenCounts
    , WorkflowExecutionOpenCounts (..)
    , mkWorkflowExecutionOpenCounts
    , weocOpenActivityTasks
    , weocOpenDecisionTasks
    , weocOpenTimers
    , weocOpenChildWorkflowExecutions
    , weocOpenLambdaFunctions

    -- ** RequestCancelActivityTaskFailedCause
    , RequestCancelActivityTaskFailedCause (..)

    -- ** ScheduleActivityTaskFailedEventAttributes
    , ScheduleActivityTaskFailedEventAttributes (..)
    , mkScheduleActivityTaskFailedEventAttributes
    , satfeaActivityType
    , satfeaActivityId
    , satfeaCause
    , satfeaDecisionTaskCompletedEventId

    -- ** MarkerRecordedEventAttributes
    , MarkerRecordedEventAttributes (..)
    , mkMarkerRecordedEventAttributes
    , mreaMarkerName
    , mreaDecisionTaskCompletedEventId
    , mreaDetails

    -- ** TerminateReason
    , TerminateReason (..)

    -- ** SignalExternalWorkflowExecutionDecisionAttributes
    , SignalExternalWorkflowExecutionDecisionAttributes (..)
    , mkSignalExternalWorkflowExecutionDecisionAttributes
    , sewedaWorkflowId
    , sewedaSignalName
    , sewedaControl
    , sewedaInput
    , sewedaRunId

    -- ** WorkflowExecutionTerminatedCause
    , WorkflowExecutionTerminatedCause (..)

    -- ** CancelWorkflowExecutionFailedCause
    , CancelWorkflowExecutionFailedCause (..)

    -- ** Data
    , Data (..)

    -- ** ScheduleLambdaFunctionFailedEventAttributes
    , ScheduleLambdaFunctionFailedEventAttributes (..)
    , mkScheduleLambdaFunctionFailedEventAttributes
    , slffeaId
    , slffeaName
    , slffeaCause
    , slffeaDecisionTaskCompletedEventId

    -- ** SignalExternalWorkflowExecutionFailedCause
    , SignalExternalWorkflowExecutionFailedCause (..)

    -- ** RecordMarkerDecisionAttributes
    , RecordMarkerDecisionAttributes (..)
    , mkRecordMarkerDecisionAttributes
    , rmdaMarkerName
    , rmdaDetails

    -- ** CompleteWorkflowExecutionFailedEventAttributes
    , CompleteWorkflowExecutionFailedEventAttributes (..)
    , mkCompleteWorkflowExecutionFailedEventAttributes
    , cCause
    , cDecisionTaskCompletedEventId

    -- ** StartTimerDecisionAttributes
    , StartTimerDecisionAttributes (..)
    , mkStartTimerDecisionAttributes
    , stdaTimerId
    , stdaStartToFireTimeout
    , stdaControl

    -- ** DecisionType
    , DecisionType (..)

    -- ** ActivityId
    , ActivityId (..)

    -- ** RequestCancelExternalWorkflowExecutionFailedEventAttributes
    , RequestCancelExternalWorkflowExecutionFailedEventAttributes (..)
    , mkRequestCancelExternalWorkflowExecutionFailedEventAttributes
    , rcewefeaWorkflowId
    , rcewefeaCause
    , rcewefeaInitiatedEventId
    , rcewefeaDecisionTaskCompletedEventId
    , rcewefeaControl
    , rcewefeaRunId

    -- ** ActivityTypeInfo
    , ActivityTypeInfo (..)
    , mkActivityTypeInfo
    , atiActivityType
    , atiStatus
    , atiCreationDate
    , atiDeprecationDate
    , atiDescription

    -- ** TimerCanceledEventAttributes
    , TimerCanceledEventAttributes (..)
    , mkTimerCanceledEventAttributes
    , tceaTimerId
    , tceaStartedEventId
    , tceaDecisionTaskCompletedEventId

    -- ** WorkflowExecutionStartedEventAttributes
    , WorkflowExecutionStartedEventAttributes (..)
    , mkWorkflowExecutionStartedEventAttributes
    , wChildPolicy
    , wTaskList
    , wWorkflowType
    , wContinuedExecutionRunId
    , wExecutionStartToCloseTimeout
    , wInput
    , wLambdaRole
    , wParentInitiatedEventId
    , wParentWorkflowExecution
    , wTagList
    , wTaskPriority
    , wTaskStartToCloseTimeout

    -- ** WorkflowTypeConfiguration
    , WorkflowTypeConfiguration (..)
    , mkWorkflowTypeConfiguration
    , wtcDefaultChildPolicy
    , wtcDefaultExecutionStartToCloseTimeout
    , wtcDefaultLambdaRole
    , wtcDefaultTaskList
    , wtcDefaultTaskPriority
    , wtcDefaultTaskStartToCloseTimeout

    -- ** ActivityTaskTimeoutType
    , ActivityTaskTimeoutType (..)

    -- ** LambdaFunctionCompletedEventAttributes
    , LambdaFunctionCompletedEventAttributes (..)
    , mkLambdaFunctionCompletedEventAttributes
    , lfceaScheduledEventId
    , lfceaStartedEventId
    , lfceaResult

    -- ** WorkflowType
    , WorkflowType (..)
    , mkWorkflowType
    , wtName
    , wtVersion

    -- ** ActivityTaskCompletedEventAttributes
    , ActivityTaskCompletedEventAttributes (..)
    , mkActivityTaskCompletedEventAttributes
    , aScheduledEventId
    , aStartedEventId
    , aResult

    -- ** ExecutionStatus
    , ExecutionStatus (..)

    -- ** DecisionTaskTimeoutType
    , DecisionTaskTimeoutType (..)

    -- ** DurationInDays
    , DurationInDays (..)

    -- ** WorkflowExecutionCancelRequestedCause
    , WorkflowExecutionCancelRequestedCause (..)

    -- ** StartChildWorkflowExecutionFailedCause
    , StartChildWorkflowExecutionFailedCause (..)

    -- ** DecisionTaskTimedOutEventAttributes
    , DecisionTaskTimedOutEventAttributes (..)
    , mkDecisionTaskTimedOutEventAttributes
    , dttoeaTimeoutType
    , dttoeaScheduledEventId
    , dttoeaStartedEventId

    -- ** ChildWorkflowExecutionStartedEventAttributes
    , ChildWorkflowExecutionStartedEventAttributes (..)
    , mkChildWorkflowExecutionStartedEventAttributes
    , cweseaWorkflowExecution
    , cweseaWorkflowType
    , cweseaInitiatedEventId

    -- ** CancelTimerFailedEventAttributes
    , CancelTimerFailedEventAttributes (..)
    , mkCancelTimerFailedEventAttributes
    , ctfeaTimerId
    , ctfeaCause
    , ctfeaDecisionTaskCompletedEventId

    -- ** FailWorkflowExecutionFailedCause
    , FailWorkflowExecutionFailedCause (..)

    -- ** WorkflowExecutionFilter
    , WorkflowExecutionFilter (..)
    , mkWorkflowExecutionFilter
    , wefWorkflowId

    -- ** FunctionInput
    , FunctionInput (..)

    -- ** ActivityTaskCanceledEventAttributes
    , ActivityTaskCanceledEventAttributes (..)
    , mkActivityTaskCanceledEventAttributes
    , atceaScheduledEventId
    , atceaStartedEventId
    , atceaDetails
    , atceaLatestCancelRequestedEventId

    -- ** WorkflowExecutionInfos
    , WorkflowExecutionInfos (..)
    , mkWorkflowExecutionInfos
    , weiExecutionInfos
    , weiNextPageToken

    -- ** StartChildWorkflowExecutionDecisionAttributes
    , StartChildWorkflowExecutionDecisionAttributes (..)
    , mkStartChildWorkflowExecutionDecisionAttributes
    , scwedaWorkflowType
    , scwedaWorkflowId
    , scwedaChildPolicy
    , scwedaControl
    , scwedaExecutionStartToCloseTimeout
    , scwedaInput
    , scwedaLambdaRole
    , scwedaTagList
    , scwedaTaskList
    , scwedaTaskPriority
    , scwedaTaskStartToCloseTimeout

    -- ** ContinueAsNewWorkflowExecutionFailedCause
    , ContinueAsNewWorkflowExecutionFailedCause (..)

    -- ** FailWorkflowExecutionDecisionAttributes
    , FailWorkflowExecutionDecisionAttributes (..)
    , mkFailWorkflowExecutionDecisionAttributes
    , fwedaDetails
    , fwedaReason

    -- ** VersionOptional
    , VersionOptional (..)

    -- ** DurationInSecondsOptional
    , DurationInSecondsOptional (..)

    -- ** EventType
    , EventType (..)

    -- ** ActivityTaskTimedOutEventAttributes
    , ActivityTaskTimedOutEventAttributes (..)
    , mkActivityTaskTimedOutEventAttributes
    , attoeaTimeoutType
    , attoeaScheduledEventId
    , attoeaStartedEventId
    , attoeaDetails

    -- ** FunctionId
    , FunctionId (..)

    -- ** RequestCancelActivityTaskFailedEventAttributes
    , RequestCancelActivityTaskFailedEventAttributes (..)
    , mkRequestCancelActivityTaskFailedEventAttributes
    , rcatfeaActivityId
    , rcatfeaCause
    , rcatfeaDecisionTaskCompletedEventId

    -- ** CompleteWorkflowExecutionDecisionAttributes
    , CompleteWorkflowExecutionDecisionAttributes (..)
    , mkCompleteWorkflowExecutionDecisionAttributes
    , cwedaResult

    -- ** DecisionTaskStartedEventAttributes
    , DecisionTaskStartedEventAttributes (..)
    , mkDecisionTaskStartedEventAttributes
    , dtseaScheduledEventId
    , dtseaIdentity

    -- ** ChildWorkflowExecutionTimedOutEventAttributes
    , ChildWorkflowExecutionTimedOutEventAttributes (..)
    , mkChildWorkflowExecutionTimedOutEventAttributes
    , cwetoeaWorkflowExecution
    , cwetoeaWorkflowType
    , cwetoeaTimeoutType
    , cwetoeaInitiatedEventId
    , cwetoeaStartedEventId

    -- ** StartChildWorkflowExecutionInitiatedEventAttributes
    , StartChildWorkflowExecutionInitiatedEventAttributes (..)
    , mkStartChildWorkflowExecutionInitiatedEventAttributes
    , scweieaWorkflowId
    , scweieaWorkflowType
    , scweieaTaskList
    , scweieaDecisionTaskCompletedEventId
    , scweieaChildPolicy
    , scweieaControl
    , scweieaExecutionStartToCloseTimeout
    , scweieaInput
    , scweieaLambdaRole
    , scweieaTagList
    , scweieaTaskPriority
    , scweieaTaskStartToCloseTimeout

    -- ** CancelWorkflowExecutionFailedEventAttributes
    , CancelWorkflowExecutionFailedEventAttributes (..)
    , mkCancelWorkflowExecutionFailedEventAttributes
    , cwefeaCause
    , cwefeaDecisionTaskCompletedEventId

    -- ** WorkflowExecutionTerminatedEventAttributes
    , WorkflowExecutionTerminatedEventAttributes (..)
    , mkWorkflowExecutionTerminatedEventAttributes
    , weteaChildPolicy
    , weteaCause
    , weteaDetails
    , weteaReason

    -- ** DomainName
    , DomainName (..)

    -- ** TaskList
    , TaskList (..)
    , mkTaskList
    , tlName

    -- ** TaskPriority
    , TaskPriority (..)

    -- ** WorkflowRunId
    , WorkflowRunId (..)

    -- ** ScheduleLambdaFunctionDecisionAttributes
    , ScheduleLambdaFunctionDecisionAttributes (..)
    , mkScheduleLambdaFunctionDecisionAttributes
    , slfdaId
    , slfdaName
    , slfdaControl
    , slfdaInput
    , slfdaStartToCloseTimeout

    -- ** LambdaFunctionScheduledEventAttributes
    , LambdaFunctionScheduledEventAttributes (..)
    , mkLambdaFunctionScheduledEventAttributes
    , lfseaId
    , lfseaName
    , lfseaDecisionTaskCompletedEventId
    , lfseaControl
    , lfseaInput
    , lfseaStartToCloseTimeout

    -- ** Name
    , Name (..)

    -- ** ScheduleActivityTaskFailedCause
    , ScheduleActivityTaskFailedCause (..)

    -- ** ChildWorkflowExecutionCanceledEventAttributes
    , ChildWorkflowExecutionCanceledEventAttributes (..)
    , mkChildWorkflowExecutionCanceledEventAttributes
    , cweceaWorkflowExecution
    , cweceaWorkflowType
    , cweceaInitiatedEventId
    , cweceaStartedEventId
    , cweceaDetails

    -- ** WorkflowExecutionInfo
    , WorkflowExecutionInfo (..)
    , mkWorkflowExecutionInfo
    , weiExecution
    , weiWorkflowType
    , weiStartTimestamp
    , weiExecutionStatus
    , weiCancelRequested
    , weiCloseStatus
    , weiCloseTimestamp
    , weiParent
    , weiTagList

    -- ** SignalExternalWorkflowExecutionFailedEventAttributes
    , SignalExternalWorkflowExecutionFailedEventAttributes (..)
    , mkSignalExternalWorkflowExecutionFailedEventAttributes
    , sewefeaWorkflowId
    , sewefeaCause
    , sewefeaInitiatedEventId
    , sewefeaDecisionTaskCompletedEventId
    , sewefeaControl
    , sewefeaRunId

    -- ** TagFilter
    , TagFilter (..)
    , mkTagFilter
    , tfTag

    -- ** Version
    , Version (..)

    -- ** ScheduleLambdaFunctionFailedCause
    , ScheduleLambdaFunctionFailedCause (..)

    -- ** ChildPolicy
    , ChildPolicy (..)

    -- ** ActivityTaskStartedEventAttributes
    , ActivityTaskStartedEventAttributes (..)
    , mkActivityTaskStartedEventAttributes
    , atseaScheduledEventId
    , atseaIdentity

    -- ** TimerId
    , TimerId (..)

    -- ** DurationInSeconds
    , DurationInSeconds (..)

    -- ** CloseStatus
    , CloseStatus (..)

    -- ** CompleteWorkflowExecutionFailedCause
    , CompleteWorkflowExecutionFailedCause (..)

    -- ** FunctionName
    , FunctionName (..)

    -- ** StartTimerFailedCause
    , StartTimerFailedCause (..)

    -- ** ActivityTaskCancelRequestedEventAttributes
    , ActivityTaskCancelRequestedEventAttributes (..)
    , mkActivityTaskCancelRequestedEventAttributes
    , atcreaDecisionTaskCompletedEventId
    , atcreaActivityId

    -- ** WorkflowExecutionTimedOutEventAttributes
    , WorkflowExecutionTimedOutEventAttributes (..)
    , mkWorkflowExecutionTimedOutEventAttributes
    , wetoeaTimeoutType
    , wetoeaChildPolicy

    -- ** ChildWorkflowExecutionTerminatedEventAttributes
    , ChildWorkflowExecutionTerminatedEventAttributes (..)
    , mkChildWorkflowExecutionTerminatedEventAttributes
    , cweteaWorkflowExecution
    , cweteaWorkflowType
    , cweteaInitiatedEventId
    , cweteaStartedEventId

    -- ** WorkflowExecutionCanceledEventAttributes
    , WorkflowExecutionCanceledEventAttributes (..)
    , mkWorkflowExecutionCanceledEventAttributes
    , weceaDecisionTaskCompletedEventId
    , weceaDetails

    -- ** StartLambdaFunctionFailedEventAttributes
    , StartLambdaFunctionFailedEventAttributes (..)
    , mkStartLambdaFunctionFailedEventAttributes
    , sCause
    , sMessage
    , sScheduledEventId

    -- ** WorkflowExecutionSignaledEventAttributes
    , WorkflowExecutionSignaledEventAttributes (..)
    , mkWorkflowExecutionSignaledEventAttributes
    , weseaSignalName
    , weseaExternalInitiatedEventId
    , weseaExternalWorkflowExecution
    , weseaInput

    -- ** RecordMarkerFailedCause
    , RecordMarkerFailedCause (..)

    -- ** RegistrationStatus
    , RegistrationStatus (..)

    -- ** TimerStartedEventAttributes
    , TimerStartedEventAttributes (..)
    , mkTimerStartedEventAttributes
    , tseaTimerId
    , tseaStartToFireTimeout
    , tseaDecisionTaskCompletedEventId
    , tseaControl

    -- ** LambdaFunctionFailedEventAttributes
    , LambdaFunctionFailedEventAttributes (..)
    , mkLambdaFunctionFailedEventAttributes
    , lffeaScheduledEventId
    , lffeaStartedEventId
    , lffeaDetails
    , lffeaReason

    -- ** RequestCancelActivityTaskDecisionAttributes
    , RequestCancelActivityTaskDecisionAttributes (..)
    , mkRequestCancelActivityTaskDecisionAttributes
    , rcatdaActivityId

    -- ** Decision
    , Decision (..)
    , mkDecision
    , dDecisionType
    , dCancelTimerDecisionAttributes
    , dCancelWorkflowExecutionDecisionAttributes
    , dCompleteWorkflowExecutionDecisionAttributes
    , dContinueAsNewWorkflowExecutionDecisionAttributes
    , dFailWorkflowExecutionDecisionAttributes
    , dRecordMarkerDecisionAttributes
    , dRequestCancelActivityTaskDecisionAttributes
    , dRequestCancelExternalWorkflowExecutionDecisionAttributes
    , dScheduleActivityTaskDecisionAttributes
    , dScheduleLambdaFunctionDecisionAttributes
    , dSignalExternalWorkflowExecutionDecisionAttributes
    , dStartChildWorkflowExecutionDecisionAttributes
    , dStartTimerDecisionAttributes

    -- ** ResourceTag
    , ResourceTag (..)
    , mkResourceTag
    , rtKey
    , rtValue

    -- ** TimerFiredEventAttributes
    , TimerFiredEventAttributes (..)
    , mkTimerFiredEventAttributes
    , tfeaTimerId
    , tfeaStartedEventId

    -- ** PageToken
    , PageToken (..)

    -- ** DomainConfiguration
    , DomainConfiguration (..)
    , mkDomainConfiguration
    , dcWorkflowExecutionRetentionPeriodInDays

    -- ** ExternalWorkflowExecutionSignaledEventAttributes
    , ExternalWorkflowExecutionSignaledEventAttributes (..)
    , mkExternalWorkflowExecutionSignaledEventAttributes
    , eweseaWorkflowExecution
    , eweseaInitiatedEventId

    -- ** CancelWorkflowExecutionDecisionAttributes
    , CancelWorkflowExecutionDecisionAttributes (..)
    , mkCancelWorkflowExecutionDecisionAttributes
    , cwedaDetails

    -- ** ActivityTaskFailedEventAttributes
    , ActivityTaskFailedEventAttributes (..)
    , mkActivityTaskFailedEventAttributes
    , atfeaScheduledEventId
    , atfeaStartedEventId
    , atfeaDetails
    , atfeaReason

    -- ** FailWorkflowExecutionFailedEventAttributes
    , FailWorkflowExecutionFailedEventAttributes (..)
    , mkFailWorkflowExecutionFailedEventAttributes
    , fwefeaCause
    , fwefeaDecisionTaskCompletedEventId

    -- ** StartChildWorkflowExecutionFailedEventAttributes
    , StartChildWorkflowExecutionFailedEventAttributes (..)
    , mkStartChildWorkflowExecutionFailedEventAttributes
    , scwefeaWorkflowType
    , scwefeaCause
    , scwefeaWorkflowId
    , scwefeaInitiatedEventId
    , scwefeaDecisionTaskCompletedEventId
    , scwefeaControl

    -- ** WorkflowTypeFilter
    , WorkflowTypeFilter (..)
    , mkWorkflowTypeFilter
    , wtfName
    , wtfVersion

    -- ** CancelTimerFailedCause
    , CancelTimerFailedCause (..)

    -- ** DecisionTaskCompletedEventAttributes
    , DecisionTaskCompletedEventAttributes (..)
    , mkDecisionTaskCompletedEventAttributes
    , dtceaScheduledEventId
    , dtceaStartedEventId
    , dtceaExecutionContext

    -- ** ChildWorkflowExecutionFailedEventAttributes
    , ChildWorkflowExecutionFailedEventAttributes (..)
    , mkChildWorkflowExecutionFailedEventAttributes
    , cwefeaWorkflowExecution
    , cwefeaWorkflowType
    , cwefeaInitiatedEventId
    , cwefeaStartedEventId
    , cwefeaDetails
    , cwefeaReason

    -- ** DomainInfo
    , DomainInfo (..)
    , mkDomainInfo
    , diName
    , diStatus
    , diArn
    , diDescription

    -- ** HistoryEvent
    , HistoryEvent (..)
    , mkHistoryEvent
    , heEventTimestamp
    , heEventType
    , heEventId
    , heActivityTaskCancelRequestedEventAttributes
    , heActivityTaskCanceledEventAttributes
    , heActivityTaskCompletedEventAttributes
    , heActivityTaskFailedEventAttributes
    , heActivityTaskScheduledEventAttributes
    , heActivityTaskStartedEventAttributes
    , heActivityTaskTimedOutEventAttributes
    , heCancelTimerFailedEventAttributes
    , heCancelWorkflowExecutionFailedEventAttributes
    , heChildWorkflowExecutionCanceledEventAttributes
    , heChildWorkflowExecutionCompletedEventAttributes
    , heChildWorkflowExecutionFailedEventAttributes
    , heChildWorkflowExecutionStartedEventAttributes
    , heChildWorkflowExecutionTerminatedEventAttributes
    , heChildWorkflowExecutionTimedOutEventAttributes
    , heCompleteWorkflowExecutionFailedEventAttributes
    , heContinueAsNewWorkflowExecutionFailedEventAttributes
    , heDecisionTaskCompletedEventAttributes
    , heDecisionTaskScheduledEventAttributes
    , heDecisionTaskStartedEventAttributes
    , heDecisionTaskTimedOutEventAttributes
    , heExternalWorkflowExecutionCancelRequestedEventAttributes
    , heExternalWorkflowExecutionSignaledEventAttributes
    , heFailWorkflowExecutionFailedEventAttributes
    , heLambdaFunctionCompletedEventAttributes
    , heLambdaFunctionFailedEventAttributes
    , heLambdaFunctionScheduledEventAttributes
    , heLambdaFunctionStartedEventAttributes
    , heLambdaFunctionTimedOutEventAttributes
    , heMarkerRecordedEventAttributes
    , heRecordMarkerFailedEventAttributes
    , heRequestCancelActivityTaskFailedEventAttributes
    , heRequestCancelExternalWorkflowExecutionFailedEventAttributes
    , heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , heScheduleActivityTaskFailedEventAttributes
    , heScheduleLambdaFunctionFailedEventAttributes
    , heSignalExternalWorkflowExecutionFailedEventAttributes
    , heSignalExternalWorkflowExecutionInitiatedEventAttributes
    , heStartChildWorkflowExecutionFailedEventAttributes
    , heStartChildWorkflowExecutionInitiatedEventAttributes
    , heStartLambdaFunctionFailedEventAttributes
    , heStartTimerFailedEventAttributes
    , heTimerCanceledEventAttributes
    , heTimerFiredEventAttributes
    , heTimerStartedEventAttributes
    , heWorkflowExecutionCancelRequestedEventAttributes
    , heWorkflowExecutionCanceledEventAttributes
    , heWorkflowExecutionCompletedEventAttributes
    , heWorkflowExecutionContinuedAsNewEventAttributes
    , heWorkflowExecutionFailedEventAttributes
    , heWorkflowExecutionSignaledEventAttributes
    , heWorkflowExecutionStartedEventAttributes
    , heWorkflowExecutionTerminatedEventAttributes
    , heWorkflowExecutionTimedOutEventAttributes

    -- ** WorkflowId
    , WorkflowId (..)

    -- ** LambdaFunctionTimeoutType
    , LambdaFunctionTimeoutType (..)

    -- ** Description
    , Description (..)

    -- ** Identity
    , Identity (..)

    -- ** ContinueAsNewWorkflowExecutionFailedEventAttributes
    , ContinueAsNewWorkflowExecutionFailedEventAttributes (..)
    , mkContinueAsNewWorkflowExecutionFailedEventAttributes
    , canwefeaCause
    , canwefeaDecisionTaskCompletedEventId

    -- ** SignalExternalWorkflowExecutionInitiatedEventAttributes
    , SignalExternalWorkflowExecutionInitiatedEventAttributes (..)
    , mkSignalExternalWorkflowExecutionInitiatedEventAttributes
    , seweieaWorkflowId
    , seweieaSignalName
    , seweieaDecisionTaskCompletedEventId
    , seweieaControl
    , seweieaInput
    , seweieaRunId

    -- ** TaskToken
    , TaskToken (..)

    -- ** CancelTimerDecisionAttributes
    , CancelTimerDecisionAttributes (..)
    , mkCancelTimerDecisionAttributes
    , ctdaTimerId

    -- ** SignalName
    , SignalName (..)

    -- ** WorkflowExecutionFailedEventAttributes
    , WorkflowExecutionFailedEventAttributes (..)
    , mkWorkflowExecutionFailedEventAttributes
    , wefeaDecisionTaskCompletedEventId
    , wefeaDetails
    , wefeaReason

    -- ** WorkflowExecutionConfiguration
    , WorkflowExecutionConfiguration (..)
    , mkWorkflowExecutionConfiguration
    , wecTaskStartToCloseTimeout
    , wecExecutionStartToCloseTimeout
    , wecTaskList
    , wecChildPolicy
    , wecLambdaRole
    , wecTaskPriority

    -- ** WorkflowExecution
    , WorkflowExecution (..)
    , mkWorkflowExecution
    , weWorkflowId
    , weRunId

    -- ** RequestCancelExternalWorkflowExecutionFailedCause
    , RequestCancelExternalWorkflowExecutionFailedCause (..)

    -- ** ContinueAsNewWorkflowExecutionDecisionAttributes
    , ContinueAsNewWorkflowExecutionDecisionAttributes (..)
    , mkContinueAsNewWorkflowExecutionDecisionAttributes
    , canwedaChildPolicy
    , canwedaExecutionStartToCloseTimeout
    , canwedaInput
    , canwedaLambdaRole
    , canwedaTagList
    , canwedaTaskList
    , canwedaTaskPriority
    , canwedaTaskStartToCloseTimeout
    , canwedaWorkflowTypeVersion

    -- ** ExternalWorkflowExecutionCancelRequestedEventAttributes
    , ExternalWorkflowExecutionCancelRequestedEventAttributes (..)
    , mkExternalWorkflowExecutionCancelRequestedEventAttributes
    , ewecreaWorkflowExecution
    , ewecreaInitiatedEventId

    -- ** PendingTaskCount
    , PendingTaskCount (..)
    , mkPendingTaskCount
    , ptcCount
    , ptcTruncated

    -- ** LambdaFunctionTimedOutEventAttributes
    , LambdaFunctionTimedOutEventAttributes (..)
    , mkLambdaFunctionTimedOutEventAttributes
    , lftoeaScheduledEventId
    , lftoeaStartedEventId
    , lftoeaTimeoutType

    -- ** WorkflowExecutionContinuedAsNewEventAttributes
    , WorkflowExecutionContinuedAsNewEventAttributes (..)
    , mkWorkflowExecutionContinuedAsNewEventAttributes
    , wecaneaDecisionTaskCompletedEventId
    , wecaneaNewExecutionRunId
    , wecaneaTaskList
    , wecaneaChildPolicy
    , wecaneaWorkflowType
    , wecaneaExecutionStartToCloseTimeout
    , wecaneaInput
    , wecaneaLambdaRole
    , wecaneaTagList
    , wecaneaTaskPriority
    , wecaneaTaskStartToCloseTimeout

    -- ** Domain
    , Domain (..)

    -- ** NextPageToken
    , NextPageToken (..)

    -- ** Control
    , Control (..)

    -- ** RunId
    , RunId (..)

    -- ** StartToCloseTimeout
    , StartToCloseTimeout (..)

    -- ** WorkflowExecutionRetentionPeriodInDays
    , WorkflowExecutionRetentionPeriodInDays (..)

    -- ** Result
    , Result (..)

    -- ** ExecutionContext
    , ExecutionContext (..)

    -- ** HeartbeatTimeout
    , HeartbeatTimeout (..)

    -- ** Input
    , Input (..)

    -- ** ScheduleToCloseTimeout
    , ScheduleToCloseTimeout (..)

    -- ** ScheduleToStartTimeout
    , ScheduleToStartTimeout (..)

    -- ** DefaultTaskHeartbeatTimeout
    , DefaultTaskHeartbeatTimeout (..)

    -- ** DefaultTaskPriority
    , DefaultTaskPriority (..)

    -- ** DefaultTaskScheduleToCloseTimeout
    , DefaultTaskScheduleToCloseTimeout (..)

    -- ** DefaultTaskScheduleToStartTimeout
    , DefaultTaskScheduleToStartTimeout (..)

    -- ** DefaultTaskStartToCloseTimeout
    , DefaultTaskStartToCloseTimeout (..)

    -- ** Details
    , Details (..)

    -- ** Id
    , Id (..)

    -- ** StartToFireTimeout
    , StartToFireTimeout (..)

    -- ** ContinuedExecutionRunId
    , ContinuedExecutionRunId (..)

    -- ** ExecutionStartToCloseTimeout
    , ExecutionStartToCloseTimeout (..)

    -- ** TaskStartToCloseTimeout
    , TaskStartToCloseTimeout (..)

    -- ** DefaultExecutionStartToCloseTimeout
    , DefaultExecutionStartToCloseTimeout (..)

    -- ** Value
    , Value (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.SWF.Types
import Network.AWS.SWF.Waiters
import Network.AWS.SWF.ListOpenWorkflowExecutions
import Network.AWS.SWF.RegisterActivityType
import Network.AWS.SWF.ListActivityTypes
import Network.AWS.SWF.CountPendingActivityTasks
import Network.AWS.SWF.RegisterWorkflowType
import Network.AWS.SWF.ListWorkflowTypes
import Network.AWS.SWF.ListTagsForResource
import Network.AWS.SWF.RespondActivityTaskFailed
import Network.AWS.SWF.CountOpenWorkflowExecutions
import Network.AWS.SWF.UndeprecateDomain
import Network.AWS.SWF.DescribeWorkflowType
import Network.AWS.SWF.DeprecateWorkflowType
import Network.AWS.SWF.RequestCancelWorkflowExecution
import Network.AWS.SWF.RegisterDomain
import Network.AWS.SWF.RespondDecisionTaskCompleted
import Network.AWS.SWF.PollForActivityTask
import Network.AWS.SWF.RespondActivityTaskCompleted
import Network.AWS.SWF.DescribeWorkflowExecution
import Network.AWS.SWF.SignalWorkflowExecution
import Network.AWS.SWF.CountPendingDecisionTasks
import Network.AWS.SWF.ListClosedWorkflowExecutions
import Network.AWS.SWF.RecordActivityTaskHeartbeat
import Network.AWS.SWF.DescribeDomain
import Network.AWS.SWF.GetWorkflowExecutionHistory
import Network.AWS.SWF.DeprecateDomain
import Network.AWS.SWF.UndeprecateWorkflowType
import Network.AWS.SWF.TerminateWorkflowExecution
import Network.AWS.SWF.DescribeActivityType
import Network.AWS.SWF.TagResource
import Network.AWS.SWF.DeprecateActivityType
import Network.AWS.SWF.UndeprecateActivityType
import Network.AWS.SWF.CountClosedWorkflowExecutions
import Network.AWS.SWF.UntagResource
import Network.AWS.SWF.RespondActivityTaskCanceled
import Network.AWS.SWF.StartWorkflowExecution
import Network.AWS.SWF.PollForDecisionTask
import Network.AWS.SWF.ListDomains
import qualified Network.AWS.Prelude as Lude

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
