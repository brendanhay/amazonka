{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Step Functions__
--
-- AWS Step Functions is a service that lets you coordinate the components of distributed applications and microservices using visual workflows.
-- You can use Step Functions to build applications from individual components, each of which performs a discrete function, or /task/ , allowing you to scale and change applications quickly. Step Functions provides a console that helps visualize the components of your application as a series of steps. Step Functions automatically triggers and tracks each step, and retries steps when there are errors, so your application executes predictably and in the right order every time. Step Functions logs the state of each step, so you can quickly diagnose and debug any issues.
-- Step Functions manages operations and underlying infrastructure to ensure your application is available at any scale. You can run tasks on AWS, your own servers, or any system that has access to AWS. You can access and use Step Functions using the console, the AWS SDKs, or an HTTP API. For more information about Step Functions, see the /<https:\/\/docs.aws.amazon.com\/step-functions\/latest\/dg\/welcome.html AWS Step Functions Developer Guide> / .
module Network.AWS.StepFunctions
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** ExecutionLimitExceeded
    _ExecutionLimitExceeded,

    -- ** InvalidDefinition
    _InvalidDefinition,

    -- ** StateMachineLimitExceeded
    _StateMachineLimitExceeded,

    -- ** ExecutionAlreadyExists
    _ExecutionAlreadyExists,

    -- ** StateMachineAlreadyExists
    _StateMachineAlreadyExists,

    -- ** TaskTimedOut
    _TaskTimedOut,

    -- ** InvalidExecutionInput
    _InvalidExecutionInput,

    -- ** InvalidOutput
    _InvalidOutput,

    -- ** InvalidName
    _InvalidName,

    -- ** TaskDoesNotExist
    _TaskDoesNotExist,

    -- ** ActivityDoesNotExist
    _ActivityDoesNotExist,

    -- ** StateMachineDeleting
    _StateMachineDeleting,

    -- ** StateMachineTypeNotSupported
    _StateMachineTypeNotSupported,

    -- ** MissingRequiredParameter
    _MissingRequiredParameter,

    -- ** InvalidArn
    _InvalidArn,

    -- ** InvalidToken
    _InvalidToken,

    -- ** InvalidLoggingConfiguration
    _InvalidLoggingConfiguration,

    -- ** ActivityWorkerLimitExceeded
    _ActivityWorkerLimitExceeded,

    -- ** InvalidTracingConfiguration
    _InvalidTracingConfiguration,

    -- ** ActivityLimitExceeded
    _ActivityLimitExceeded,

    -- ** TooManyTags
    _TooManyTags,

    -- ** ExecutionDoesNotExist
    _ExecutionDoesNotExist,

    -- ** StateMachineDoesNotExist
    _StateMachineDoesNotExist,

    -- ** ResourceNotFound
    _ResourceNotFound,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteActivity
    module Network.AWS.StepFunctions.DeleteActivity,

    -- ** DescribeStateMachine
    module Network.AWS.StepFunctions.DescribeStateMachine,

    -- ** ListTagsForResource
    module Network.AWS.StepFunctions.ListTagsForResource,

    -- ** StopExecution
    module Network.AWS.StepFunctions.StopExecution,

    -- ** DescribeActivity
    module Network.AWS.StepFunctions.DescribeActivity,

    -- ** ListStateMachines (Paginated)
    module Network.AWS.StepFunctions.ListStateMachines,

    -- ** ListExecutions (Paginated)
    module Network.AWS.StepFunctions.ListExecutions,

    -- ** DeleteStateMachine
    module Network.AWS.StepFunctions.DeleteStateMachine,

    -- ** UpdateStateMachine
    module Network.AWS.StepFunctions.UpdateStateMachine,

    -- ** DescribeStateMachineForExecution
    module Network.AWS.StepFunctions.DescribeStateMachineForExecution,

    -- ** GetActivityTask
    module Network.AWS.StepFunctions.GetActivityTask,

    -- ** CreateActivity
    module Network.AWS.StepFunctions.CreateActivity,

    -- ** ListActivities (Paginated)
    module Network.AWS.StepFunctions.ListActivities,

    -- ** SendTaskHeartbeat
    module Network.AWS.StepFunctions.SendTaskHeartbeat,

    -- ** SendTaskFailure
    module Network.AWS.StepFunctions.SendTaskFailure,

    -- ** DescribeExecution
    module Network.AWS.StepFunctions.DescribeExecution,

    -- ** SendTaskSuccess
    module Network.AWS.StepFunctions.SendTaskSuccess,

    -- ** StartExecution
    module Network.AWS.StepFunctions.StartExecution,

    -- ** StartSyncExecution
    module Network.AWS.StepFunctions.StartSyncExecution,

    -- ** TagResource
    module Network.AWS.StepFunctions.TagResource,

    -- ** UntagResource
    module Network.AWS.StepFunctions.UntagResource,

    -- ** GetExecutionHistory (Paginated)
    module Network.AWS.StepFunctions.GetExecutionHistory,

    -- ** CreateStateMachine
    module Network.AWS.StepFunctions.CreateStateMachine,

    -- * Types

    -- ** TaskSubmitFailedEventDetails
    TaskSubmitFailedEventDetails (..),
    mkTaskSubmitFailedEventDetails,
    tsfedResourceType,
    tsfedResource,
    tsfedCause,
    tsfedError,

    -- ** SensitiveError
    SensitiveError (..),

    -- ** CloudWatchLogsLogGroup
    CloudWatchLogsLogGroup (..),
    mkCloudWatchLogsLogGroup,
    cwllgLogGroupArn,

    -- ** SensitiveData
    SensitiveData (..),

    -- ** MapStateStartedEventDetails
    MapStateStartedEventDetails (..),
    mkMapStateStartedEventDetails,
    mssedLength,

    -- ** CloudWatchEventsExecutionDataDetails
    CloudWatchEventsExecutionDataDetails (..),
    mkCloudWatchEventsExecutionDataDetails,
    cweeddIncluded,

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** TaskStartedEventDetails
    TaskStartedEventDetails (..),
    mkTaskStartedEventDetails,
    tsedsResourceType,
    tsedsResource,

    -- ** ActivityListItem
    ActivityListItem (..),
    mkActivityListItem,
    aliActivityArn,
    aliName,
    aliCreationDate,

    -- ** LogLevel
    LogLevel (..),

    -- ** ActivityStartedEventDetails
    ActivityStartedEventDetails (..),
    mkActivityStartedEventDetails,
    asedWorkerName,

    -- ** StateMachineType
    StateMachineType (..),

    -- ** TaskSubmittedEventDetails
    TaskSubmittedEventDetails (..),
    mkTaskSubmittedEventDetails,
    tsedfResourceType,
    tsedfResource,
    tsedfOutput,
    tsedfOutputDetails,

    -- ** Definition
    Definition (..),

    -- ** Arn
    Arn (..),

    -- ** ExecutionListItem
    ExecutionListItem (..),
    mkExecutionListItem,
    eliExecutionArn,
    eliStateMachineArn,
    eliName,
    eliStatus,
    eliStartDate,
    eliStopDate,

    -- ** SensitiveCause
    SensitiveCause (..),

    -- ** StateMachineListItem
    StateMachineListItem (..),
    mkStateMachineListItem,
    smliStateMachineArn,
    smliName,
    smliType,
    smliCreationDate,

    -- ** LambdaFunctionSucceededEventDetails
    LambdaFunctionSucceededEventDetails (..),
    mkLambdaFunctionSucceededEventDetails,
    lfsedOutput,
    lfsedOutputDetails,

    -- ** TaskSucceededEventDetails
    TaskSucceededEventDetails (..),
    mkTaskSucceededEventDetails,
    tsedgResourceType,
    tsedgResource,
    tsedgOutput,
    tsedgOutputDetails,

    -- ** LambdaFunctionStartFailedEventDetails
    LambdaFunctionStartFailedEventDetails (..),
    mkLambdaFunctionStartFailedEventDetails,
    lCause,
    lError,

    -- ** StateExitedEventDetails
    StateExitedEventDetails (..),
    mkStateExitedEventDetails,
    sName,
    sOutput,
    sOutputDetails,

    -- ** TaskStartFailedEventDetails
    TaskStartFailedEventDetails (..),
    mkTaskStartFailedEventDetails,
    tResourceType,
    tResource,
    tCause,
    tError,

    -- ** LogDestination
    LogDestination (..),
    mkLogDestination,
    ldCloudWatchLogsLogGroup,

    -- ** ActivitySucceededEventDetails
    ActivitySucceededEventDetails (..),
    mkActivitySucceededEventDetails,
    asedOutput,
    asedOutputDetails,

    -- ** ListExecutionsPageToken
    ListExecutionsPageToken (..),

    -- ** ExecutionStatus
    ExecutionStatus (..),

    -- ** StateMachineStatus
    StateMachineStatus (..),

    -- ** HistoryEventType
    HistoryEventType (..),

    -- ** ActivityTimedOutEventDetails
    ActivityTimedOutEventDetails (..),
    mkActivityTimedOutEventDetails,
    atoedCause,
    atoedError,

    -- ** TaskTimedOutEventDetails
    TaskTimedOutEventDetails (..),
    mkTaskTimedOutEventDetails,
    ttoedResourceType,
    ttoedResource,
    ttoedCause,
    ttoedError,

    -- ** TracingConfiguration
    TracingConfiguration (..),
    mkTracingConfiguration,
    tcEnabled,

    -- ** LambdaFunctionTimedOutEventDetails
    LambdaFunctionTimedOutEventDetails (..),
    mkLambdaFunctionTimedOutEventDetails,
    lftoedCause,
    lftoedError,

    -- ** ExecutionFailedEventDetails
    ExecutionFailedEventDetails (..),
    mkExecutionFailedEventDetails,
    efedCause,
    efedError,

    -- ** ExecutionAbortedEventDetails
    ExecutionAbortedEventDetails (..),
    mkExecutionAbortedEventDetails,
    eaedCause,
    eaedError,

    -- ** ExecutionSucceededEventDetails
    ExecutionSucceededEventDetails (..),
    mkExecutionSucceededEventDetails,
    esedOutput,
    esedOutputDetails,

    -- ** Name
    Name (..),

    -- ** LambdaFunctionScheduledEventDetails
    LambdaFunctionScheduledEventDetails (..),
    mkLambdaFunctionScheduledEventDetails,
    lfsedResource,
    lfsedInput,
    lfsedInputDetails,
    lfsedTimeoutInSeconds,

    -- ** ActivityScheduledEventDetails
    ActivityScheduledEventDetails (..),
    mkActivityScheduledEventDetails,
    asedResource,
    asedHeartbeatInSeconds,
    asedInput,
    asedInputDetails,
    asedTimeoutInSeconds,

    -- ** TaskScheduledEventDetails
    TaskScheduledEventDetails (..),
    mkTaskScheduledEventDetails,
    tsedResourceType,
    tsedResource,
    tsedRegion,
    tsedParameters,
    tsedHeartbeatInSeconds,
    tsedTimeoutInSeconds,

    -- ** LambdaFunctionScheduleFailedEventDetails
    LambdaFunctionScheduleFailedEventDetails (..),
    mkLambdaFunctionScheduleFailedEventDetails,
    lfsfedCause,
    lfsfedError,

    -- ** ActivityScheduleFailedEventDetails
    ActivityScheduleFailedEventDetails (..),
    mkActivityScheduleFailedEventDetails,
    asfedCause,
    asfedError,

    -- ** ExecutionStartedEventDetails
    ExecutionStartedEventDetails (..),
    mkExecutionStartedEventDetails,
    esedInput,
    esedInputDetails,
    esedRoleArn,

    -- ** TagKey
    TagKey (..),

    -- ** MapIterationEventDetails
    MapIterationEventDetails (..),
    mkMapIterationEventDetails,
    miedIndex,
    miedName,

    -- ** StateEnteredEventDetails
    StateEnteredEventDetails (..),
    mkStateEnteredEventDetails,
    seedName,
    seedInput,
    seedInputDetails,

    -- ** TaskFailedEventDetails
    TaskFailedEventDetails (..),
    mkTaskFailedEventDetails,
    tfedResourceType,
    tfedResource,
    tfedCause,
    tfedError,

    -- ** LambdaFunctionFailedEventDetails
    LambdaFunctionFailedEventDetails (..),
    mkLambdaFunctionFailedEventDetails,
    lffedCause,
    lffedError,

    -- ** PageToken
    PageToken (..),

    -- ** ActivityFailedEventDetails
    ActivityFailedEventDetails (..),
    mkActivityFailedEventDetails,
    afedCause,
    afedError,

    -- ** LoggingConfiguration
    LoggingConfiguration (..),
    mkLoggingConfiguration,
    lcDestinations,
    lcIncludeExecutionData,
    lcLevel,

    -- ** ExecutionTimedOutEventDetails
    ExecutionTimedOutEventDetails (..),
    mkExecutionTimedOutEventDetails,
    etoedCause,
    etoedError,

    -- ** HistoryEventExecutionDataDetails
    HistoryEventExecutionDataDetails (..),
    mkHistoryEventExecutionDataDetails,
    heeddTruncated,

    -- ** HistoryEvent
    HistoryEvent (..),
    mkHistoryEvent,
    heTimestamp,
    heType,
    heId,
    heActivityFailedEventDetails,
    heActivityScheduleFailedEventDetails,
    heActivityScheduledEventDetails,
    heActivityStartedEventDetails,
    heActivitySucceededEventDetails,
    heActivityTimedOutEventDetails,
    heExecutionAbortedEventDetails,
    heExecutionFailedEventDetails,
    heExecutionStartedEventDetails,
    heExecutionSucceededEventDetails,
    heExecutionTimedOutEventDetails,
    heLambdaFunctionFailedEventDetails,
    heLambdaFunctionScheduleFailedEventDetails,
    heLambdaFunctionScheduledEventDetails,
    heLambdaFunctionStartFailedEventDetails,
    heLambdaFunctionSucceededEventDetails,
    heLambdaFunctionTimedOutEventDetails,
    heMapIterationAbortedEventDetails,
    heMapIterationFailedEventDetails,
    heMapIterationStartedEventDetails,
    heMapIterationSucceededEventDetails,
    heMapStateStartedEventDetails,
    hePreviousEventId,
    heStateEnteredEventDetails,
    heStateExitedEventDetails,
    heTaskFailedEventDetails,
    heTaskScheduledEventDetails,
    heTaskStartFailedEventDetails,
    heTaskStartedEventDetails,
    heTaskSubmitFailedEventDetails,
    heTaskSubmittedEventDetails,
    heTaskSucceededEventDetails,
    heTaskTimedOutEventDetails,

    -- ** TraceHeader
    TraceHeader (..),

    -- ** TaskToken
    TaskToken (..),

    -- ** SyncExecutionStatus
    SyncExecutionStatus (..),

    -- ** BillingDetails
    BillingDetails (..),
    mkBillingDetails,
    bdBilledDurationInMilliseconds,
    bdBilledMemoryUsedInMB,

    -- ** Output
    Output (..),

    -- ** StateMachineArn
    StateMachineArn (..),

    -- ** Input
    Input (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** Resource
    Resource (..),

    -- ** Cause
    Cause (..),

    -- ** Error
    Error (..),

    -- ** LogGroupArn
    LogGroupArn (..),

    -- ** NextToken
    NextToken (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** ExecutionArn
    ExecutionArn (..),

    -- ** ActivityArn
    ActivityArn (..),

    -- ** WorkerName
    WorkerName (..),

    -- ** Parameters
    Parameters (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import qualified Network.AWS.Prelude as Lude
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
