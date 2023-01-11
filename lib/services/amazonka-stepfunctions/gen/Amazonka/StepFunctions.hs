{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.StepFunctions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-11-23@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Step Functions
--
-- Step Functions is a service that lets you coordinate the components of
-- distributed applications and microservices using visual workflows.
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
-- Amazon Web Services, your own servers, or any system that has access to
-- Amazon Web Services. You can access and use Step Functions using the
-- console, the Amazon Web Services SDKs, or an HTTP API. For more
-- information about Step Functions, see the
-- /<https://docs.aws.amazon.com/step-functions/latest/dg/welcome.html Step Functions Developer Guide>/
-- .
module Amazonka.StepFunctions
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ActivityDoesNotExist
    _ActivityDoesNotExist,

    -- ** ActivityLimitExceeded
    _ActivityLimitExceeded,

    -- ** ActivityWorkerLimitExceeded
    _ActivityWorkerLimitExceeded,

    -- ** ExecutionAlreadyExists
    _ExecutionAlreadyExists,

    -- ** ExecutionDoesNotExist
    _ExecutionDoesNotExist,

    -- ** ExecutionLimitExceeded
    _ExecutionLimitExceeded,

    -- ** InvalidArn
    _InvalidArn,

    -- ** InvalidDefinition
    _InvalidDefinition,

    -- ** InvalidExecutionInput
    _InvalidExecutionInput,

    -- ** InvalidLoggingConfiguration
    _InvalidLoggingConfiguration,

    -- ** InvalidName
    _InvalidName,

    -- ** InvalidOutput
    _InvalidOutput,

    -- ** InvalidToken
    _InvalidToken,

    -- ** InvalidTracingConfiguration
    _InvalidTracingConfiguration,

    -- ** MissingRequiredParameter
    _MissingRequiredParameter,

    -- ** ResourceNotFound
    _ResourceNotFound,

    -- ** StateMachineAlreadyExists
    _StateMachineAlreadyExists,

    -- ** StateMachineDeleting
    _StateMachineDeleting,

    -- ** StateMachineDoesNotExist
    _StateMachineDoesNotExist,

    -- ** StateMachineLimitExceeded
    _StateMachineLimitExceeded,

    -- ** StateMachineTypeNotSupported
    _StateMachineTypeNotSupported,

    -- ** TaskDoesNotExist
    _TaskDoesNotExist,

    -- ** TaskTimedOut
    _TaskTimedOut,

    -- ** TooManyTags
    _TooManyTags,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateActivity
    CreateActivity (CreateActivity'),
    newCreateActivity,
    CreateActivityResponse (CreateActivityResponse'),
    newCreateActivityResponse,

    -- ** CreateStateMachine
    CreateStateMachine (CreateStateMachine'),
    newCreateStateMachine,
    CreateStateMachineResponse (CreateStateMachineResponse'),
    newCreateStateMachineResponse,

    -- ** DeleteActivity
    DeleteActivity (DeleteActivity'),
    newDeleteActivity,
    DeleteActivityResponse (DeleteActivityResponse'),
    newDeleteActivityResponse,

    -- ** DeleteStateMachine
    DeleteStateMachine (DeleteStateMachine'),
    newDeleteStateMachine,
    DeleteStateMachineResponse (DeleteStateMachineResponse'),
    newDeleteStateMachineResponse,

    -- ** DescribeActivity
    DescribeActivity (DescribeActivity'),
    newDescribeActivity,
    DescribeActivityResponse (DescribeActivityResponse'),
    newDescribeActivityResponse,

    -- ** DescribeExecution
    DescribeExecution (DescribeExecution'),
    newDescribeExecution,
    DescribeExecutionResponse (DescribeExecutionResponse'),
    newDescribeExecutionResponse,

    -- ** DescribeMapRun
    DescribeMapRun (DescribeMapRun'),
    newDescribeMapRun,
    DescribeMapRunResponse (DescribeMapRunResponse'),
    newDescribeMapRunResponse,

    -- ** DescribeStateMachine
    DescribeStateMachine (DescribeStateMachine'),
    newDescribeStateMachine,
    DescribeStateMachineResponse (DescribeStateMachineResponse'),
    newDescribeStateMachineResponse,

    -- ** DescribeStateMachineForExecution
    DescribeStateMachineForExecution (DescribeStateMachineForExecution'),
    newDescribeStateMachineForExecution,
    DescribeStateMachineForExecutionResponse (DescribeStateMachineForExecutionResponse'),
    newDescribeStateMachineForExecutionResponse,

    -- ** GetActivityTask
    GetActivityTask (GetActivityTask'),
    newGetActivityTask,
    GetActivityTaskResponse (GetActivityTaskResponse'),
    newGetActivityTaskResponse,

    -- ** GetExecutionHistory (Paginated)
    GetExecutionHistory (GetExecutionHistory'),
    newGetExecutionHistory,
    GetExecutionHistoryResponse (GetExecutionHistoryResponse'),
    newGetExecutionHistoryResponse,

    -- ** ListActivities (Paginated)
    ListActivities (ListActivities'),
    newListActivities,
    ListActivitiesResponse (ListActivitiesResponse'),
    newListActivitiesResponse,

    -- ** ListExecutions (Paginated)
    ListExecutions (ListExecutions'),
    newListExecutions,
    ListExecutionsResponse (ListExecutionsResponse'),
    newListExecutionsResponse,

    -- ** ListMapRuns (Paginated)
    ListMapRuns (ListMapRuns'),
    newListMapRuns,
    ListMapRunsResponse (ListMapRunsResponse'),
    newListMapRunsResponse,

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

    -- ** SendTaskFailure
    SendTaskFailure (SendTaskFailure'),
    newSendTaskFailure,
    SendTaskFailureResponse (SendTaskFailureResponse'),
    newSendTaskFailureResponse,

    -- ** SendTaskHeartbeat
    SendTaskHeartbeat (SendTaskHeartbeat'),
    newSendTaskHeartbeat,
    SendTaskHeartbeatResponse (SendTaskHeartbeatResponse'),
    newSendTaskHeartbeatResponse,

    -- ** SendTaskSuccess
    SendTaskSuccess (SendTaskSuccess'),
    newSendTaskSuccess,
    SendTaskSuccessResponse (SendTaskSuccessResponse'),
    newSendTaskSuccessResponse,

    -- ** StartExecution
    StartExecution (StartExecution'),
    newStartExecution,
    StartExecutionResponse (StartExecutionResponse'),
    newStartExecutionResponse,

    -- ** StartSyncExecution
    StartSyncExecution (StartSyncExecution'),
    newStartSyncExecution,
    StartSyncExecutionResponse (StartSyncExecutionResponse'),
    newStartSyncExecutionResponse,

    -- ** StopExecution
    StopExecution (StopExecution'),
    newStopExecution,
    StopExecutionResponse (StopExecutionResponse'),
    newStopExecutionResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateMapRun
    UpdateMapRun (UpdateMapRun'),
    newUpdateMapRun,
    UpdateMapRunResponse (UpdateMapRunResponse'),
    newUpdateMapRunResponse,

    -- ** UpdateStateMachine
    UpdateStateMachine (UpdateStateMachine'),
    newUpdateStateMachine,
    UpdateStateMachineResponse (UpdateStateMachineResponse'),
    newUpdateStateMachineResponse,

    -- * Types

    -- ** ExecutionStatus
    ExecutionStatus (..),

    -- ** HistoryEventType
    HistoryEventType (..),

    -- ** LogLevel
    LogLevel (..),

    -- ** MapRunStatus
    MapRunStatus (..),

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

    -- ** MapRunExecutionCounts
    MapRunExecutionCounts (MapRunExecutionCounts'),
    newMapRunExecutionCounts,

    -- ** MapRunFailedEventDetails
    MapRunFailedEventDetails (MapRunFailedEventDetails'),
    newMapRunFailedEventDetails,

    -- ** MapRunItemCounts
    MapRunItemCounts (MapRunItemCounts'),
    newMapRunItemCounts,

    -- ** MapRunListItem
    MapRunListItem (MapRunListItem'),
    newMapRunListItem,

    -- ** MapRunStartedEventDetails
    MapRunStartedEventDetails (MapRunStartedEventDetails'),
    newMapRunStartedEventDetails,

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

    -- ** TaskCredentials
    TaskCredentials (TaskCredentials'),
    newTaskCredentials,

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

import Amazonka.StepFunctions.CreateActivity
import Amazonka.StepFunctions.CreateStateMachine
import Amazonka.StepFunctions.DeleteActivity
import Amazonka.StepFunctions.DeleteStateMachine
import Amazonka.StepFunctions.DescribeActivity
import Amazonka.StepFunctions.DescribeExecution
import Amazonka.StepFunctions.DescribeMapRun
import Amazonka.StepFunctions.DescribeStateMachine
import Amazonka.StepFunctions.DescribeStateMachineForExecution
import Amazonka.StepFunctions.GetActivityTask
import Amazonka.StepFunctions.GetExecutionHistory
import Amazonka.StepFunctions.Lens
import Amazonka.StepFunctions.ListActivities
import Amazonka.StepFunctions.ListExecutions
import Amazonka.StepFunctions.ListMapRuns
import Amazonka.StepFunctions.ListStateMachines
import Amazonka.StepFunctions.ListTagsForResource
import Amazonka.StepFunctions.SendTaskFailure
import Amazonka.StepFunctions.SendTaskHeartbeat
import Amazonka.StepFunctions.SendTaskSuccess
import Amazonka.StepFunctions.StartExecution
import Amazonka.StepFunctions.StartSyncExecution
import Amazonka.StepFunctions.StopExecution
import Amazonka.StepFunctions.TagResource
import Amazonka.StepFunctions.Types
import Amazonka.StepFunctions.UntagResource
import Amazonka.StepFunctions.UpdateMapRun
import Amazonka.StepFunctions.UpdateStateMachine
import Amazonka.StepFunctions.Waiters

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
