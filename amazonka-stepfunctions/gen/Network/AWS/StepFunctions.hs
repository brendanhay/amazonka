{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Step Functions
--
-- AWS Step Functions is a service that lets you coordinate the components
-- of distributed applications and microservices using visual workflows.
--
-- You can use Step Functions to build applications from individual
-- components, each of which performs a discrete function, or /task/,
-- allowing you to scale and change applications quickly. Step Functions
-- provides a console that helps visualize the components of your
-- application as a series of steps. Step Functions automatically triggers
-- and tracks each step, and retries steps when there are errors, so your
-- application executes predictably and in the right order every time. Step
-- Functions logs the state of each step, so you can quickly diagnose and
-- debug any issues.
--
-- Step Functions manages operations and underlying infrastructure to
-- ensure your application is available at any scale. You can run tasks on
-- AWS, your own servers, or any system that has access to AWS. You can
-- access and use Step Functions using the console, the AWS SDKs, or an
-- HTTP API. For more information about Step Functions, see the
-- /<https://docs.aws.amazon.com/step-functions/latest/dg/welcome.html AWS Step Functions Developer Guide>/
-- .
module Network.AWS.StepFunctions
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ExecutionLimitExceeded
    _ExecutionLimitExceeded,

    -- ** ExecutionAlreadyExists
    _ExecutionAlreadyExists,

    -- ** StateMachineTypeNotSupported
    _StateMachineTypeNotSupported,

    -- ** ExecutionDoesNotExist
    _ExecutionDoesNotExist,

    -- ** InvalidName
    _InvalidName,

    -- ** InvalidOutput
    _InvalidOutput,

    -- ** ActivityLimitExceeded
    _ActivityLimitExceeded,

    -- ** InvalidExecutionInput
    _InvalidExecutionInput,

    -- ** InvalidLoggingConfiguration
    _InvalidLoggingConfiguration,

    -- ** TaskTimedOut
    _TaskTimedOut,

    -- ** StateMachineLimitExceeded
    _StateMachineLimitExceeded,

    -- ** InvalidArn
    _InvalidArn,

    -- ** InvalidDefinition
    _InvalidDefinition,

    -- ** MissingRequiredParameter
    _MissingRequiredParameter,

    -- ** StateMachineAlreadyExists
    _StateMachineAlreadyExists,

    -- ** ResourceNotFound
    _ResourceNotFound,

    -- ** StateMachineDoesNotExist
    _StateMachineDoesNotExist,

    -- ** TaskDoesNotExist
    _TaskDoesNotExist,

    -- ** StateMachineDeleting
    _StateMachineDeleting,

    -- ** ActivityDoesNotExist
    _ActivityDoesNotExist,

    -- ** TooManyTags
    _TooManyTags,

    -- ** ActivityWorkerLimitExceeded
    _ActivityWorkerLimitExceeded,

    -- ** InvalidTracingConfiguration
    _InvalidTracingConfiguration,

    -- ** InvalidToken
    _InvalidToken,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeExecution
    DescribeExecution (DescribeExecution'),
    newDescribeExecution,
    DescribeExecutionResponse (DescribeExecutionResponse'),
    newDescribeExecutionResponse,

    -- ** DescribeStateMachine
    DescribeStateMachine (DescribeStateMachine'),
    newDescribeStateMachine,
    DescribeStateMachineResponse (DescribeStateMachineResponse'),
    newDescribeStateMachineResponse,

    -- ** DeleteActivity
    DeleteActivity (DeleteActivity'),
    newDeleteActivity,
    DeleteActivityResponse (DeleteActivityResponse'),
    newDeleteActivityResponse,

    -- ** CreateActivity
    CreateActivity (CreateActivity'),
    newCreateActivity,
    CreateActivityResponse (CreateActivityResponse'),
    newCreateActivityResponse,

    -- ** ListActivities (Paginated)
    ListActivities (ListActivities'),
    newListActivities,
    ListActivitiesResponse (ListActivitiesResponse'),
    newListActivitiesResponse,

    -- ** CreateStateMachine
    CreateStateMachine (CreateStateMachine'),
    newCreateStateMachine,
    CreateStateMachineResponse (CreateStateMachineResponse'),
    newCreateStateMachineResponse,

    -- ** GetActivityTask
    GetActivityTask (GetActivityTask'),
    newGetActivityTask,
    GetActivityTaskResponse (GetActivityTaskResponse'),
    newGetActivityTaskResponse,

    -- ** UpdateStateMachine
    UpdateStateMachine (UpdateStateMachine'),
    newUpdateStateMachine,
    UpdateStateMachineResponse (UpdateStateMachineResponse'),
    newUpdateStateMachineResponse,

    -- ** ListExecutions (Paginated)
    ListExecutions (ListExecutions'),
    newListExecutions,
    ListExecutionsResponse (ListExecutionsResponse'),
    newListExecutionsResponse,

    -- ** DeleteStateMachine
    DeleteStateMachine (DeleteStateMachine'),
    newDeleteStateMachine,
    DeleteStateMachineResponse (DeleteStateMachineResponse'),
    newDeleteStateMachineResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DescribeActivity
    DescribeActivity (DescribeActivity'),
    newDescribeActivity,
    DescribeActivityResponse (DescribeActivityResponse'),
    newDescribeActivityResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** StartSyncExecution
    StartSyncExecution (StartSyncExecution'),
    newStartSyncExecution,
    StartSyncExecutionResponse (StartSyncExecutionResponse'),
    newStartSyncExecutionResponse,

    -- ** SendTaskSuccess
    SendTaskSuccess (SendTaskSuccess'),
    newSendTaskSuccess,
    SendTaskSuccessResponse (SendTaskSuccessResponse'),
    newSendTaskSuccessResponse,

    -- ** SendTaskHeartbeat
    SendTaskHeartbeat (SendTaskHeartbeat'),
    newSendTaskHeartbeat,
    SendTaskHeartbeatResponse (SendTaskHeartbeatResponse'),
    newSendTaskHeartbeatResponse,

    -- ** SendTaskFailure
    SendTaskFailure (SendTaskFailure'),
    newSendTaskFailure,
    SendTaskFailureResponse (SendTaskFailureResponse'),
    newSendTaskFailureResponse,

    -- ** DescribeStateMachineForExecution
    DescribeStateMachineForExecution (DescribeStateMachineForExecution'),
    newDescribeStateMachineForExecution,
    DescribeStateMachineForExecutionResponse (DescribeStateMachineForExecutionResponse'),
    newDescribeStateMachineForExecutionResponse,

    -- ** GetExecutionHistory (Paginated)
    GetExecutionHistory (GetExecutionHistory'),
    newGetExecutionHistory,
    GetExecutionHistoryResponse (GetExecutionHistoryResponse'),
    newGetExecutionHistoryResponse,

    -- ** ListStateMachines (Paginated)
    ListStateMachines (ListStateMachines'),
    newListStateMachines,
    ListStateMachinesResponse (ListStateMachinesResponse'),
    newListStateMachinesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StopExecution
    StopExecution (StopExecution'),
    newStopExecution,
    StopExecutionResponse (StopExecutionResponse'),
    newStopExecutionResponse,

    -- ** StartExecution
    StartExecution (StartExecution'),
    newStartExecution,
    StartExecutionResponse (StartExecutionResponse'),
    newStartExecutionResponse,

    -- * Types

    -- ** ExecutionStatus
    ExecutionStatus (..),

    -- ** HistoryEventType
    HistoryEventType (..),

    -- ** LogLevel
    LogLevel (..),

    -- ** StateMachineStatus
    StateMachineStatus (..),

    -- ** StateMachineType
    StateMachineType (..),

    -- ** SyncExecutionStatus
    SyncExecutionStatus (..),

    -- ** ActivityFailedEventDetails
    ActivityFailedEventDetails (ActivityFailedEventDetails'),
    newActivityFailedEventDetails,

    -- ** ActivityListItem
    ActivityListItem (ActivityListItem'),
    newActivityListItem,

    -- ** ActivityScheduleFailedEventDetails
    ActivityScheduleFailedEventDetails (ActivityScheduleFailedEventDetails'),
    newActivityScheduleFailedEventDetails,

    -- ** ActivityScheduledEventDetails
    ActivityScheduledEventDetails (ActivityScheduledEventDetails'),
    newActivityScheduledEventDetails,

    -- ** ActivityStartedEventDetails
    ActivityStartedEventDetails (ActivityStartedEventDetails'),
    newActivityStartedEventDetails,

    -- ** ActivitySucceededEventDetails
    ActivitySucceededEventDetails (ActivitySucceededEventDetails'),
    newActivitySucceededEventDetails,

    -- ** ActivityTimedOutEventDetails
    ActivityTimedOutEventDetails (ActivityTimedOutEventDetails'),
    newActivityTimedOutEventDetails,

    -- ** BillingDetails
    BillingDetails (BillingDetails'),
    newBillingDetails,

    -- ** CloudWatchEventsExecutionDataDetails
    CloudWatchEventsExecutionDataDetails (CloudWatchEventsExecutionDataDetails'),
    newCloudWatchEventsExecutionDataDetails,

    -- ** CloudWatchLogsLogGroup
    CloudWatchLogsLogGroup (CloudWatchLogsLogGroup'),
    newCloudWatchLogsLogGroup,

    -- ** ExecutionAbortedEventDetails
    ExecutionAbortedEventDetails (ExecutionAbortedEventDetails'),
    newExecutionAbortedEventDetails,

    -- ** ExecutionFailedEventDetails
    ExecutionFailedEventDetails (ExecutionFailedEventDetails'),
    newExecutionFailedEventDetails,

    -- ** ExecutionListItem
    ExecutionListItem (ExecutionListItem'),
    newExecutionListItem,

    -- ** ExecutionStartedEventDetails
    ExecutionStartedEventDetails (ExecutionStartedEventDetails'),
    newExecutionStartedEventDetails,

    -- ** ExecutionSucceededEventDetails
    ExecutionSucceededEventDetails (ExecutionSucceededEventDetails'),
    newExecutionSucceededEventDetails,

    -- ** ExecutionTimedOutEventDetails
    ExecutionTimedOutEventDetails (ExecutionTimedOutEventDetails'),
    newExecutionTimedOutEventDetails,

    -- ** HistoryEvent
    HistoryEvent (HistoryEvent'),
    newHistoryEvent,

    -- ** HistoryEventExecutionDataDetails
    HistoryEventExecutionDataDetails (HistoryEventExecutionDataDetails'),
    newHistoryEventExecutionDataDetails,

    -- ** LambdaFunctionFailedEventDetails
    LambdaFunctionFailedEventDetails (LambdaFunctionFailedEventDetails'),
    newLambdaFunctionFailedEventDetails,

    -- ** LambdaFunctionScheduleFailedEventDetails
    LambdaFunctionScheduleFailedEventDetails (LambdaFunctionScheduleFailedEventDetails'),
    newLambdaFunctionScheduleFailedEventDetails,

    -- ** LambdaFunctionScheduledEventDetails
    LambdaFunctionScheduledEventDetails (LambdaFunctionScheduledEventDetails'),
    newLambdaFunctionScheduledEventDetails,

    -- ** LambdaFunctionStartFailedEventDetails
    LambdaFunctionStartFailedEventDetails (LambdaFunctionStartFailedEventDetails'),
    newLambdaFunctionStartFailedEventDetails,

    -- ** LambdaFunctionSucceededEventDetails
    LambdaFunctionSucceededEventDetails (LambdaFunctionSucceededEventDetails'),
    newLambdaFunctionSucceededEventDetails,

    -- ** LambdaFunctionTimedOutEventDetails
    LambdaFunctionTimedOutEventDetails (LambdaFunctionTimedOutEventDetails'),
    newLambdaFunctionTimedOutEventDetails,

    -- ** LogDestination
    LogDestination (LogDestination'),
    newLogDestination,

    -- ** LoggingConfiguration
    LoggingConfiguration (LoggingConfiguration'),
    newLoggingConfiguration,

    -- ** MapIterationEventDetails
    MapIterationEventDetails (MapIterationEventDetails'),
    newMapIterationEventDetails,

    -- ** MapStateStartedEventDetails
    MapStateStartedEventDetails (MapStateStartedEventDetails'),
    newMapStateStartedEventDetails,

    -- ** StateEnteredEventDetails
    StateEnteredEventDetails (StateEnteredEventDetails'),
    newStateEnteredEventDetails,

    -- ** StateExitedEventDetails
    StateExitedEventDetails (StateExitedEventDetails'),
    newStateExitedEventDetails,

    -- ** StateMachineListItem
    StateMachineListItem (StateMachineListItem'),
    newStateMachineListItem,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TaskFailedEventDetails
    TaskFailedEventDetails (TaskFailedEventDetails'),
    newTaskFailedEventDetails,

    -- ** TaskScheduledEventDetails
    TaskScheduledEventDetails (TaskScheduledEventDetails'),
    newTaskScheduledEventDetails,

    -- ** TaskStartFailedEventDetails
    TaskStartFailedEventDetails (TaskStartFailedEventDetails'),
    newTaskStartFailedEventDetails,

    -- ** TaskStartedEventDetails
    TaskStartedEventDetails (TaskStartedEventDetails'),
    newTaskStartedEventDetails,

    -- ** TaskSubmitFailedEventDetails
    TaskSubmitFailedEventDetails (TaskSubmitFailedEventDetails'),
    newTaskSubmitFailedEventDetails,

    -- ** TaskSubmittedEventDetails
    TaskSubmittedEventDetails (TaskSubmittedEventDetails'),
    newTaskSubmittedEventDetails,

    -- ** TaskSucceededEventDetails
    TaskSucceededEventDetails (TaskSucceededEventDetails'),
    newTaskSucceededEventDetails,

    -- ** TaskTimedOutEventDetails
    TaskTimedOutEventDetails (TaskTimedOutEventDetails'),
    newTaskTimedOutEventDetails,

    -- ** TracingConfiguration
    TracingConfiguration (TracingConfiguration'),
    newTracingConfiguration,
  )
where

import Network.AWS.StepFunctions.CreateActivity
import Network.AWS.StepFunctions.CreateStateMachine
import Network.AWS.StepFunctions.DeleteActivity
import Network.AWS.StepFunctions.DeleteStateMachine
import Network.AWS.StepFunctions.DescribeActivity
import Network.AWS.StepFunctions.DescribeExecution
import Network.AWS.StepFunctions.DescribeStateMachine
import Network.AWS.StepFunctions.DescribeStateMachineForExecution
import Network.AWS.StepFunctions.GetActivityTask
import Network.AWS.StepFunctions.GetExecutionHistory
import Network.AWS.StepFunctions.Lens
import Network.AWS.StepFunctions.ListActivities
import Network.AWS.StepFunctions.ListExecutions
import Network.AWS.StepFunctions.ListStateMachines
import Network.AWS.StepFunctions.ListTagsForResource
import Network.AWS.StepFunctions.SendTaskFailure
import Network.AWS.StepFunctions.SendTaskHeartbeat
import Network.AWS.StepFunctions.SendTaskSuccess
import Network.AWS.StepFunctions.StartExecution
import Network.AWS.StepFunctions.StartSyncExecution
import Network.AWS.StepFunctions.StopExecution
import Network.AWS.StepFunctions.TagResource
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.UntagResource
import Network.AWS.StepFunctions.UpdateStateMachine
import Network.AWS.StepFunctions.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'StepFunctions'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
