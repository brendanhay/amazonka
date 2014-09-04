{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Simple Workflow Service (Amazon SWF) makes it easy to build
-- applications that coordinate work across distributed components. In Amazon
-- SWF, a task represents a logical unit of work that is performed by a
-- component of your application. Coordinating tasks across the application
-- involves managing intertask dependencies, scheduling, and concurrency in
-- accordance with the logical flow of the application. Amazon SWF gives you
-- full control over implementing tasks and coordinating them without worrying
-- about underlying complexities such as tracking their progress and
-- maintaining their state.
module Network.AWS.SWF.V2012_01_25.Types
    (
    -- * Service
      SWF
    -- ** Errors
    , Er (..)
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

    -- * CancelTimerDecisionAttributes
    , CancelTimerDecisionAttributes
    , mkCancelTimerDecisionAttributes
    , ctdaTimerId

    -- * CancelWorkflowExecutionDecisionAttributes
    , CancelWorkflowExecutionDecisionAttributes
    , mkCancelWorkflowExecutionDecisionAttributes
    , cwedbDetails

    -- * CloseStatusFilter
    , CloseStatusFilter
    , mkCloseStatusFilter
    , csfStatus

    -- * CompleteWorkflowExecutionDecisionAttributes
    , CompleteWorkflowExecutionDecisionAttributes
    , mkCompleteWorkflowExecutionDecisionAttributes
    , cwedaResult

    -- * DomainConfiguration
    , DomainConfiguration
    , dcWorkflowExecutionRetentionPeriodInDays

    -- * RequestCancelActivityTaskDecisionAttributes
    , RequestCancelActivityTaskDecisionAttributes
    , mkRequestCancelActivityTaskDecisionAttributes
    , rcatdaActivityId

    -- * TagFilter
    , TagFilter
    , mkTagFilter
    , tfTag

    -- * TaskList
    , TaskList
    , mkTaskList
    , tlName

    -- * WorkflowExecutionFilter
    , WorkflowExecutionFilter
    , mkWorkflowExecutionFilter
    , wefWorkflowId

    -- * ActivityTaskCancelRequestedEventAttributes
    , ActivityTaskCancelRequestedEventAttributes
    , mkActivityTaskCancelRequestedEventAttributes
    , atcreaDecisionTaskCompletedEventId
    , atcreaActivityId

    -- * ActivityTaskCanceledEventAttributes
    , ActivityTaskCanceledEventAttributes
    , mkActivityTaskCanceledEventAttributes
    , atcebDetails
    , atcebScheduledEventId
    , atcebStartedEventId
    , atcebLatestCancelRequestedEventId

    -- * ActivityTaskCompletedEventAttributes
    , ActivityTaskCompletedEventAttributes
    , mkActivityTaskCompletedEventAttributes
    , atceaResult
    , atceaScheduledEventId
    , atceaStartedEventId

    -- * ActivityTaskFailedEventAttributes
    , ActivityTaskFailedEventAttributes
    , mkActivityTaskFailedEventAttributes
    , atfeaReason
    , atfeaDetails
    , atfeaScheduledEventId
    , atfeaStartedEventId

    -- * ActivityTaskScheduledEventAttributes
    , ActivityTaskScheduledEventAttributes
    , mkActivityTaskScheduledEventAttributes
    , atseaActivityType
    , atseaActivityId
    , atseaInput
    , atseaControl
    , atseaScheduleToStartTimeout
    , atseaScheduleToCloseTimeout
    , atseaStartToCloseTimeout
    , atseaTaskList
    , atseaDecisionTaskCompletedEventId
    , atseaHeartbeatTimeout

    -- * ActivityTaskStartedEventAttributes
    , ActivityTaskStartedEventAttributes
    , mkActivityTaskStartedEventAttributes
    , atsebIdentity
    , atsebScheduledEventId

    -- * ActivityTaskTimedOutEventAttributes
    , ActivityTaskTimedOutEventAttributes
    , mkActivityTaskTimedOutEventAttributes
    , attoeaTimeoutType
    , attoeaScheduledEventId
    , attoeaStartedEventId
    , attoeaDetails

    -- * ActivityType
    , ActivityType
    , mkActivityType
    , atName
    , atVersion

    -- * ActivityTypeConfiguration
    , ActivityTypeConfiguration
    , atcDefaultTaskStartToCloseTimeout
    , atcDefaultTaskHeartbeatTimeout
    , atcDefaultTaskList
    , atcDefaultTaskScheduleToStartTimeout
    , atcDefaultTaskScheduleToCloseTimeout

    -- * ActivityTypeInfo
    , ActivityTypeInfo
    , atiActivityType
    , atiStatus
    , atiDescription
    , atiCreationDate
    , atiDeprecationDate

    -- * CancelTimerFailedEventAttributes
    , CancelTimerFailedEventAttributes
    , mkCancelTimerFailedEventAttributes
    , ctfeaTimerId
    , ctfeaCause
    , ctfeaDecisionTaskCompletedEventId

    -- * CancelWorkflowExecutionFailedEventAttributes
    , CancelWorkflowExecutionFailedEventAttributes
    , mkCancelWorkflowExecutionFailedEventAttributes
    , cwefebCause
    , cwefebDecisionTaskCompletedEventId

    -- * ChildWorkflowExecutionCanceledEventAttributes
    , ChildWorkflowExecutionCanceledEventAttributes
    , mkChildWorkflowExecutionCanceledEventAttributes
    , cwecebWorkflowExecution
    , cwecebWorkflowType
    , cwecebDetails
    , cwecebInitiatedEventId
    , cwecebStartedEventId

    -- * ChildWorkflowExecutionCompletedEventAttributes
    , ChildWorkflowExecutionCompletedEventAttributes
    , mkChildWorkflowExecutionCompletedEventAttributes
    , cweceaWorkflowExecution
    , cweceaWorkflowType
    , cweceaResult
    , cweceaInitiatedEventId
    , cweceaStartedEventId

    -- * ChildWorkflowExecutionFailedEventAttributes
    , ChildWorkflowExecutionFailedEventAttributes
    , mkChildWorkflowExecutionFailedEventAttributes
    , cwefecWorkflowExecution
    , cwefecWorkflowType
    , cwefecReason
    , cwefecDetails
    , cwefecInitiatedEventId
    , cwefecStartedEventId

    -- * ChildWorkflowExecutionStartedEventAttributes
    , ChildWorkflowExecutionStartedEventAttributes
    , mkChildWorkflowExecutionStartedEventAttributes
    , cweseaWorkflowExecution
    , cweseaWorkflowType
    , cweseaInitiatedEventId

    -- * ChildWorkflowExecutionTerminatedEventAttributes
    , ChildWorkflowExecutionTerminatedEventAttributes
    , mkChildWorkflowExecutionTerminatedEventAttributes
    , cweteaWorkflowExecution
    , cweteaWorkflowType
    , cweteaInitiatedEventId
    , cweteaStartedEventId

    -- * ChildWorkflowExecutionTimedOutEventAttributes
    , ChildWorkflowExecutionTimedOutEventAttributes
    , mkChildWorkflowExecutionTimedOutEventAttributes
    , cwetoeaWorkflowExecution
    , cwetoeaWorkflowType
    , cwetoeaTimeoutType
    , cwetoeaInitiatedEventId
    , cwetoeaStartedEventId

    -- * CompleteWorkflowExecutionFailedEventAttributes
    , CompleteWorkflowExecutionFailedEventAttributes
    , mkCompleteWorkflowExecutionFailedEventAttributes
    , cwefeaCause
    , cwefeaDecisionTaskCompletedEventId

    -- * ContinueAsNewWorkflowExecutionDecisionAttributes
    , ContinueAsNewWorkflowExecutionDecisionAttributes
    , mkContinueAsNewWorkflowExecutionDecisionAttributes
    , canwedaInput
    , canwedaExecutionStartToCloseTimeout
    , canwedaTaskList
    , canwedaTaskStartToCloseTimeout
    , canwedaChildPolicy
    , canwedaTagList
    , canwedaWorkflowTypeVersion

    -- * ContinueAsNewWorkflowExecutionFailedEventAttributes
    , ContinueAsNewWorkflowExecutionFailedEventAttributes
    , mkContinueAsNewWorkflowExecutionFailedEventAttributes
    , canwefeaCause
    , canwefeaDecisionTaskCompletedEventId

    -- * Decision
    , Decision
    , mkDecision
    , ddddrDecisionType
    , ddddrScheduleActivityTaskDecisionAttributes
    , ddddrRequestCancelActivityTaskDecisionAttributes
    , ddddrCompleteWorkflowExecutionDecisionAttributes
    , ddddrFailWorkflowExecutionDecisionAttributes
    , ddddrCancelWorkflowExecutionDecisionAttributes
    , ddddrContinueAsNewWorkflowExecutionDecisionAttributes
    , ddddrRecordMarkerDecisionAttributes
    , ddddrStartTimerDecisionAttributes
    , ddddrCancelTimerDecisionAttributes
    , ddddrSignalExternalWorkflowExecutionDecisionAttributes
    , ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes
    , ddddrStartChildWorkflowExecutionDecisionAttributes

    -- * DecisionTaskCompletedEventAttributes
    , DecisionTaskCompletedEventAttributes
    , mkDecisionTaskCompletedEventAttributes
    , dtceaExecutionContext
    , dtceaScheduledEventId
    , dtceaStartedEventId

    -- * DecisionTaskScheduledEventAttributes
    , DecisionTaskScheduledEventAttributes
    , mkDecisionTaskScheduledEventAttributes
    , dtseaTaskList
    , dtseaStartToCloseTimeout

    -- * DecisionTaskStartedEventAttributes
    , DecisionTaskStartedEventAttributes
    , mkDecisionTaskStartedEventAttributes
    , dtsebIdentity
    , dtsebScheduledEventId

    -- * DecisionTaskTimedOutEventAttributes
    , DecisionTaskTimedOutEventAttributes
    , mkDecisionTaskTimedOutEventAttributes
    , dttoeaTimeoutType
    , dttoeaScheduledEventId
    , dttoeaStartedEventId

    -- * DomainInfo
    , DomainInfo
    , diName
    , diStatus
    , diDescription

    -- * ExecutionTimeFilter
    , ExecutionTimeFilter
    , mkExecutionTimeFilter
    , etfOldestDate
    , etfLatestDate

    -- * ExternalWorkflowExecutionCancelRequestedEventAttributes
    , ExternalWorkflowExecutionCancelRequestedEventAttributes
    , mkExternalWorkflowExecutionCancelRequestedEventAttributes
    , ewecreaWorkflowExecution
    , ewecreaInitiatedEventId

    -- * ExternalWorkflowExecutionSignaledEventAttributes
    , ExternalWorkflowExecutionSignaledEventAttributes
    , mkExternalWorkflowExecutionSignaledEventAttributes
    , eweseaWorkflowExecution
    , eweseaInitiatedEventId

    -- * FailWorkflowExecutionDecisionAttributes
    , FailWorkflowExecutionDecisionAttributes
    , mkFailWorkflowExecutionDecisionAttributes
    , fwedaReason
    , fwedaDetails

    -- * FailWorkflowExecutionFailedEventAttributes
    , FailWorkflowExecutionFailedEventAttributes
    , mkFailWorkflowExecutionFailedEventAttributes
    , fwefeaCause
    , fwefeaDecisionTaskCompletedEventId

    -- * HistoryEvent
    , HistoryEvent
    , heEventTimestamp
    , heEventType
    , heEventId
    , heWorkflowExecutionStartedEventAttributes
    , heWorkflowExecutionCompletedEventAttributes
    , heCompleteWorkflowExecutionFailedEventAttributes
    , heWorkflowExecutionFailedEventAttributes
    , heFailWorkflowExecutionFailedEventAttributes
    , heWorkflowExecutionTimedOutEventAttributes
    , heWorkflowExecutionCanceledEventAttributes
    , heCancelWorkflowExecutionFailedEventAttributes
    , heWorkflowExecutionContinuedAsNewEventAttributes
    , heContinueAsNewWorkflowExecutionFailedEventAttributes
    , heWorkflowExecutionTerminatedEventAttributes
    , heWorkflowExecutionCancelRequestedEventAttributes
    , heDecisionTaskScheduledEventAttributes
    , heDecisionTaskStartedEventAttributes
    , heDecisionTaskCompletedEventAttributes
    , heDecisionTaskTimedOutEventAttributes
    , heActivityTaskScheduledEventAttributes
    , heActivityTaskStartedEventAttributes
    , heActivityTaskCompletedEventAttributes
    , heActivityTaskFailedEventAttributes
    , heActivityTaskTimedOutEventAttributes
    , heActivityTaskCanceledEventAttributes
    , heActivityTaskCancelRequestedEventAttributes
    , heWorkflowExecutionSignaledEventAttributes
    , heMarkerRecordedEventAttributes
    , heRecordMarkerFailedEventAttributes
    , heTimerStartedEventAttributes
    , heTimerFiredEventAttributes
    , heTimerCanceledEventAttributes
    , heStartChildWorkflowExecutionInitiatedEventAttributes
    , heChildWorkflowExecutionStartedEventAttributes
    , heChildWorkflowExecutionCompletedEventAttributes
    , heChildWorkflowExecutionFailedEventAttributes
    , heChildWorkflowExecutionTimedOutEventAttributes
    , heChildWorkflowExecutionCanceledEventAttributes
    , heChildWorkflowExecutionTerminatedEventAttributes
    , heSignalExternalWorkflowExecutionInitiatedEventAttributes
    , heExternalWorkflowExecutionSignaledEventAttributes
    , heSignalExternalWorkflowExecutionFailedEventAttributes
    , heExternalWorkflowExecutionCancelRequestedEventAttributes
    , heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , heRequestCancelExternalWorkflowExecutionFailedEventAttributes
    , heScheduleActivityTaskFailedEventAttributes
    , heRequestCancelActivityTaskFailedEventAttributes
    , heStartTimerFailedEventAttributes
    , heCancelTimerFailedEventAttributes
    , heStartChildWorkflowExecutionFailedEventAttributes

    -- * MarkerRecordedEventAttributes
    , MarkerRecordedEventAttributes
    , mkMarkerRecordedEventAttributes
    , mreaMarkerName
    , mreaDetails
    , mreaDecisionTaskCompletedEventId

    -- * RecordMarkerDecisionAttributes
    , RecordMarkerDecisionAttributes
    , mkRecordMarkerDecisionAttributes
    , rmdaMarkerName
    , rmdaDetails

    -- * RecordMarkerFailedEventAttributes
    , RecordMarkerFailedEventAttributes
    , mkRecordMarkerFailedEventAttributes
    , rmfeaMarkerName
    , rmfeaCause
    , rmfeaDecisionTaskCompletedEventId

    -- * RequestCancelActivityTaskFailedEventAttributes
    , RequestCancelActivityTaskFailedEventAttributes
    , mkRequestCancelActivityTaskFailedEventAttributes
    , rcatfeaActivityId
    , rcatfeaCause
    , rcatfeaDecisionTaskCompletedEventId

    -- * RequestCancelExternalWorkflowExecutionDecisionAttributes
    , RequestCancelExternalWorkflowExecutionDecisionAttributes
    , mkRequestCancelExternalWorkflowExecutionDecisionAttributes
    , rcewedaWorkflowId
    , rcewedaRunId
    , rcewedaControl

    -- * RequestCancelExternalWorkflowExecutionFailedEventAttributes
    , RequestCancelExternalWorkflowExecutionFailedEventAttributes
    , mkRequestCancelExternalWorkflowExecutionFailedEventAttributes
    , rcewefeaWorkflowId
    , rcewefeaRunId
    , rcewefeaCause
    , rcewefeaInitiatedEventId
    , rcewefeaDecisionTaskCompletedEventId
    , rcewefeaControl

    -- * RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , rceweieaWorkflowId
    , rceweieaRunId
    , rceweieaDecisionTaskCompletedEventId
    , rceweieaControl

    -- * ScheduleActivityTaskDecisionAttributes
    , ScheduleActivityTaskDecisionAttributes
    , mkScheduleActivityTaskDecisionAttributes
    , satdaActivityType
    , satdaActivityId
    , satdaControl
    , satdaInput
    , satdaScheduleToCloseTimeout
    , satdaTaskList
    , satdaScheduleToStartTimeout
    , satdaStartToCloseTimeout
    , satdaHeartbeatTimeout

    -- * ScheduleActivityTaskFailedEventAttributes
    , ScheduleActivityTaskFailedEventAttributes
    , mkScheduleActivityTaskFailedEventAttributes
    , satfeaActivityType
    , satfeaActivityId
    , satfeaCause
    , satfeaDecisionTaskCompletedEventId

    -- * SignalExternalWorkflowExecutionDecisionAttributes
    , SignalExternalWorkflowExecutionDecisionAttributes
    , mkSignalExternalWorkflowExecutionDecisionAttributes
    , sewedaWorkflowId
    , sewedaRunId
    , sewedaSignalName
    , sewedaInput
    , sewedaControl

    -- * SignalExternalWorkflowExecutionFailedEventAttributes
    , SignalExternalWorkflowExecutionFailedEventAttributes
    , mkSignalExternalWorkflowExecutionFailedEventAttributes
    , sewefeaWorkflowId
    , sewefeaRunId
    , sewefeaCause
    , sewefeaInitiatedEventId
    , sewefeaDecisionTaskCompletedEventId
    , sewefeaControl

    -- * SignalExternalWorkflowExecutionInitiatedEventAttributes
    , SignalExternalWorkflowExecutionInitiatedEventAttributes
    , mkSignalExternalWorkflowExecutionInitiatedEventAttributes
    , seweieaWorkflowId
    , seweieaRunId
    , seweieaSignalName
    , seweieaInput
    , seweieaDecisionTaskCompletedEventId
    , seweieaControl

    -- * StartChildWorkflowExecutionDecisionAttributes
    , StartChildWorkflowExecutionDecisionAttributes
    , mkStartChildWorkflowExecutionDecisionAttributes
    , scwedaWorkflowType
    , scwedaWorkflowId
    , scwedaControl
    , scwedaInput
    , scwedaExecutionStartToCloseTimeout
    , scwedaTaskList
    , scwedaTaskStartToCloseTimeout
    , scwedaChildPolicy
    , scwedaTagList

    -- * StartChildWorkflowExecutionFailedEventAttributes
    , StartChildWorkflowExecutionFailedEventAttributes
    , mkStartChildWorkflowExecutionFailedEventAttributes
    , scwefeaWorkflowType
    , scwefeaCause
    , scwefeaWorkflowId
    , scwefeaInitiatedEventId
    , scwefeaDecisionTaskCompletedEventId
    , scwefeaControl

    -- * StartChildWorkflowExecutionInitiatedEventAttributes
    , StartChildWorkflowExecutionInitiatedEventAttributes
    , mkStartChildWorkflowExecutionInitiatedEventAttributes
    , scweieaWorkflowId
    , scweieaWorkflowType
    , scweieaControl
    , scweieaInput
    , scweieaExecutionStartToCloseTimeout
    , scweieaTaskList
    , scweieaDecisionTaskCompletedEventId
    , scweieaChildPolicy
    , scweieaTaskStartToCloseTimeout
    , scweieaTagList

    -- * StartTimerDecisionAttributes
    , StartTimerDecisionAttributes
    , mkStartTimerDecisionAttributes
    , stdaTimerId
    , stdaControl
    , stdaStartToFireTimeout

    -- * StartTimerFailedEventAttributes
    , StartTimerFailedEventAttributes
    , mkStartTimerFailedEventAttributes
    , stfeaTimerId
    , stfeaCause
    , stfeaDecisionTaskCompletedEventId

    -- * TimerCanceledEventAttributes
    , TimerCanceledEventAttributes
    , mkTimerCanceledEventAttributes
    , tceaTimerId
    , tceaStartedEventId
    , tceaDecisionTaskCompletedEventId

    -- * TimerFiredEventAttributes
    , TimerFiredEventAttributes
    , mkTimerFiredEventAttributes
    , tfeaTimerId
    , tfeaStartedEventId

    -- * TimerStartedEventAttributes
    , TimerStartedEventAttributes
    , mkTimerStartedEventAttributes
    , tseaTimerId
    , tseaControl
    , tseaStartToFireTimeout
    , tseaDecisionTaskCompletedEventId

    -- * WorkflowExecution
    , WorkflowExecution
    , mkWorkflowExecution
    , weWorkflowId
    , weRunId

    -- * WorkflowExecutionCancelRequestedEventAttributes
    , WorkflowExecutionCancelRequestedEventAttributes
    , mkWorkflowExecutionCancelRequestedEventAttributes
    , wecreaExternalWorkflowExecution
    , wecreaExternalInitiatedEventId
    , wecreaCause

    -- * WorkflowExecutionCanceledEventAttributes
    , WorkflowExecutionCanceledEventAttributes
    , mkWorkflowExecutionCanceledEventAttributes
    , wecebDetails
    , wecebDecisionTaskCompletedEventId

    -- * WorkflowExecutionCompletedEventAttributes
    , WorkflowExecutionCompletedEventAttributes
    , mkWorkflowExecutionCompletedEventAttributes
    , weceaResult
    , weceaDecisionTaskCompletedEventId

    -- * WorkflowExecutionConfiguration
    , WorkflowExecutionConfiguration
    , wehTaskStartToCloseTimeout
    , wehExecutionStartToCloseTimeout
    , wehTaskList
    , wehChildPolicy

    -- * WorkflowExecutionContinuedAsNewEventAttributes
    , WorkflowExecutionContinuedAsNewEventAttributes
    , mkWorkflowExecutionContinuedAsNewEventAttributes
    , wecaneaInput
    , wecaneaDecisionTaskCompletedEventId
    , wecaneaNewExecutionRunId
    , wecaneaExecutionStartToCloseTimeout
    , wecaneaTaskList
    , wecaneaTaskStartToCloseTimeout
    , wecaneaChildPolicy
    , wecaneaTagList
    , wecaneaWorkflowType

    -- * WorkflowExecutionFailedEventAttributes
    , WorkflowExecutionFailedEventAttributes
    , mkWorkflowExecutionFailedEventAttributes
    , wefeaReason
    , wefeaDetails
    , wefeaDecisionTaskCompletedEventId

    -- * WorkflowExecutionInfo
    , WorkflowExecutionInfo
    , weiExecution
    , weiWorkflowType
    , weiStartTimestamp
    , weiCloseTimestamp
    , weiExecutionStatus
    , weiCloseStatus
    , weiParent
    , weiTagList
    , weiCancelRequested

    -- * WorkflowExecutionOpenCounts
    , WorkflowExecutionOpenCounts
    , weocOpenActivityTasks
    , weocOpenDecisionTasks
    , weocOpenTimers
    , weocOpenChildWorkflowExecutions

    -- * WorkflowExecutionSignaledEventAttributes
    , WorkflowExecutionSignaledEventAttributes
    , mkWorkflowExecutionSignaledEventAttributes
    , wesebSignalName
    , wesebInput
    , wesebExternalWorkflowExecution
    , wesebExternalInitiatedEventId

    -- * WorkflowExecutionStartedEventAttributes
    , WorkflowExecutionStartedEventAttributes
    , mkWorkflowExecutionStartedEventAttributes
    , weseaInput
    , weseaExecutionStartToCloseTimeout
    , weseaTaskStartToCloseTimeout
    , weseaChildPolicy
    , weseaTaskList
    , weseaWorkflowType
    , weseaTagList
    , weseaContinuedExecutionRunId
    , weseaParentWorkflowExecution
    , weseaParentInitiatedEventId

    -- * WorkflowExecutionTerminatedEventAttributes
    , WorkflowExecutionTerminatedEventAttributes
    , mkWorkflowExecutionTerminatedEventAttributes
    , weteaReason
    , weteaDetails
    , weteaChildPolicy
    , weteaCause

    -- * WorkflowExecutionTimedOutEventAttributes
    , WorkflowExecutionTimedOutEventAttributes
    , mkWorkflowExecutionTimedOutEventAttributes
    , wetoeaTimeoutType
    , wetoeaChildPolicy

    -- * WorkflowType
    , WorkflowType
    , mkWorkflowType
    , wtName
    , wtVersion

    -- * WorkflowTypeConfiguration
    , WorkflowTypeConfiguration
    , wtcDefaultTaskStartToCloseTimeout
    , wtcDefaultExecutionStartToCloseTimeout
    , wtcDefaultTaskList
    , wtcDefaultChildPolicy

    -- * WorkflowTypeFilter
    , WorkflowTypeFilter
    , mkWorkflowTypeFilter
    , wtfName
    , wtfVersion

    -- * WorkflowTypeInfo
    , WorkflowTypeInfo
    , wtiWorkflowType
    , wtiStatus
    , wtiDescription
    , wtiCreationDate
    , wtiDeprecationDate
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2012-01-25@) of the
-- @Amazon Simple Workflow Service@ service.
data SWF deriving (Typeable)

instance AWSService SWF where
    type Sg SWF = V4
    data Er SWF
        = DefaultUndefinedFault
            { _dufMessage :: Maybe Text
            }
        | DomainAlreadyExistsFault
            { _daefMessage :: Maybe Text
            }
        | DomainDeprecatedFault
            { _ddfMessage :: Maybe Text
            }
        | LimitExceededFault
            { _lefMessage :: Maybe Text
            }
        | OperationNotPermittedFault
            { _onpfMessage :: Maybe Text
            }
        | SWFClient HttpException
        | SWFSerializer String
        | SWFService String
        | TypeAlreadyExistsFault
            { _taefMessage :: Maybe Text
            }
        | TypeDeprecatedFault
            { _tdfMessage :: Maybe Text
            }
        | UnknownResourceFault
            { _urfMessage :: Maybe Text
            }
        | WorkflowExecutionAlreadyStartedFault
            { _weasfMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "swf"
        , _svcVersion  = "2012-01-25"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er SWF)
deriving instance Generic (Er SWF)

instance AWSError (Er SWF) where
    awsError = const "SWFError"

instance AWSServiceError (Er SWF) where
    serviceError    = SWFService
    clientError     = SWFClient
    serializerError = SWFSerializer

instance Exception (Er SWF)

-- | The type of the timeout that caused this event.
data ActivityTaskTimeoutType
    = ActivityTaskTimeoutTypeHeartbeat -- ^ HEARTBEAT
    | ActivityTaskTimeoutTypeScheduleToClose -- ^ SCHEDULE_TO_CLOSE
    | ActivityTaskTimeoutTypeScheduleToStart -- ^ SCHEDULE_TO_START
    | ActivityTaskTimeoutTypeStartToClose -- ^ START_TO_CLOSE
      deriving (Eq, Show, Generic)

instance Hashable ActivityTaskTimeoutType

instance FromText ActivityTaskTimeoutType where
    parser = match "HEARTBEAT" ActivityTaskTimeoutTypeHeartbeat
         <|> match "SCHEDULE_TO_CLOSE" ActivityTaskTimeoutTypeScheduleToClose
         <|> match "SCHEDULE_TO_START" ActivityTaskTimeoutTypeScheduleToStart
         <|> match "START_TO_CLOSE" ActivityTaskTimeoutTypeStartToClose

instance ToText ActivityTaskTimeoutType where
    toText ActivityTaskTimeoutTypeHeartbeat = "HEARTBEAT"
    toText ActivityTaskTimeoutTypeScheduleToClose = "SCHEDULE_TO_CLOSE"
    toText ActivityTaskTimeoutTypeScheduleToStart = "SCHEDULE_TO_START"
    toText ActivityTaskTimeoutTypeStartToClose = "START_TO_CLOSE"

instance ToByteString ActivityTaskTimeoutType where
    toBS ActivityTaskTimeoutTypeHeartbeat = "HEARTBEAT"
    toBS ActivityTaskTimeoutTypeScheduleToClose = "SCHEDULE_TO_CLOSE"
    toBS ActivityTaskTimeoutTypeScheduleToStart = "SCHEDULE_TO_START"
    toBS ActivityTaskTimeoutTypeStartToClose = "START_TO_CLOSE"

instance ToHeader ActivityTaskTimeoutType where
    toHeader k = toHeader k . toBS

instance ToQuery ActivityTaskTimeoutType where
    toQuery = toQuery . toBS

instance FromJSON ActivityTaskTimeoutType

instance ToJSON ActivityTaskTimeoutType

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data CancelTimerFailedCause
    = CancelTimerFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | CancelTimerFailedCauseTimerIdUnknown -- ^ TIMER_ID_UNKNOWN
      deriving (Eq, Show, Generic)

instance Hashable CancelTimerFailedCause

instance FromText CancelTimerFailedCause where
    parser = match "OPERATION_NOT_PERMITTED" CancelTimerFailedCauseOperationNotPermitted
         <|> match "TIMER_ID_UNKNOWN" CancelTimerFailedCauseTimerIdUnknown

instance ToText CancelTimerFailedCause where
    toText CancelTimerFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText CancelTimerFailedCauseTimerIdUnknown = "TIMER_ID_UNKNOWN"

instance ToByteString CancelTimerFailedCause where
    toBS CancelTimerFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS CancelTimerFailedCauseTimerIdUnknown = "TIMER_ID_UNKNOWN"

instance ToHeader CancelTimerFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery CancelTimerFailedCause where
    toQuery = toQuery . toBS

instance FromJSON CancelTimerFailedCause

instance ToJSON CancelTimerFailedCause

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.
data CancelWorkflowExecutionFailedCause
    = CancelWorkflowExecutionFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | CancelWorkflowExecutionFailedCauseUnhandledDecision -- ^ UNHANDLED_DECISION
      deriving (Eq, Show, Generic)

instance Hashable CancelWorkflowExecutionFailedCause

instance FromText CancelWorkflowExecutionFailedCause where
    parser = match "OPERATION_NOT_PERMITTED" CancelWorkflowExecutionFailedCauseOperationNotPermitted
         <|> match "UNHANDLED_DECISION" CancelWorkflowExecutionFailedCauseUnhandledDecision

instance ToText CancelWorkflowExecutionFailedCause where
    toText CancelWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText CancelWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"

instance ToByteString CancelWorkflowExecutionFailedCause where
    toBS CancelWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS CancelWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"

instance ToHeader CancelWorkflowExecutionFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery CancelWorkflowExecutionFailedCause where
    toQuery = toQuery . toBS

instance FromJSON CancelWorkflowExecutionFailedCause

instance ToJSON CancelWorkflowExecutionFailedCause

-- | The policy to use for the child workflow executions if this workflow
-- execution is terminated, by calling the TerminateWorkflowExecution action
-- explicitly or due to an expired timeout. The supported child policies are:
-- TERMINATE: the child executions will be terminated. REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a
-- WorkflowExecutionCancelRequested event in its history. It is up to the
-- decider to take appropriate actions when it receives an execution history
-- with this event. ABANDON: no action will be taken. The child executions
-- will continue to run.
data ChildPolicy
    = ChildPolicyAbandon -- ^ ABANDON
    | ChildPolicyRequestCancel -- ^ REQUEST_CANCEL
    | ChildPolicyTerminate -- ^ TERMINATE
      deriving (Eq, Show, Generic)

instance Hashable ChildPolicy

instance FromText ChildPolicy where
    parser = match "ABANDON" ChildPolicyAbandon
         <|> match "REQUEST_CANCEL" ChildPolicyRequestCancel
         <|> match "TERMINATE" ChildPolicyTerminate

instance ToText ChildPolicy where
    toText ChildPolicyAbandon = "ABANDON"
    toText ChildPolicyRequestCancel = "REQUEST_CANCEL"
    toText ChildPolicyTerminate = "TERMINATE"

instance ToByteString ChildPolicy where
    toBS ChildPolicyAbandon = "ABANDON"
    toBS ChildPolicyRequestCancel = "REQUEST_CANCEL"
    toBS ChildPolicyTerminate = "TERMINATE"

instance ToHeader ChildPolicy where
    toHeader k = toHeader k . toBS

instance ToQuery ChildPolicy where
    toQuery = toQuery . toBS

instance FromJSON ChildPolicy

instance ToJSON ChildPolicy

-- | The close status that must match the close status of an execution for it to
-- meet the criteria of this filter. This field is required.
data CloseStatus
    = CloseStatusCanceled -- ^ CANCELED
    | CloseStatusCompleted -- ^ COMPLETED
    | CloseStatusContinuedAsNew -- ^ CONTINUED_AS_NEW
    | CloseStatusFailed -- ^ FAILED
    | CloseStatusTerminated -- ^ TERMINATED
    | CloseStatusTimedOut -- ^ TIMED_OUT
      deriving (Eq, Show, Generic)

instance Hashable CloseStatus

instance FromText CloseStatus where
    parser = match "CANCELED" CloseStatusCanceled
         <|> match "COMPLETED" CloseStatusCompleted
         <|> match "CONTINUED_AS_NEW" CloseStatusContinuedAsNew
         <|> match "FAILED" CloseStatusFailed
         <|> match "TERMINATED" CloseStatusTerminated
         <|> match "TIMED_OUT" CloseStatusTimedOut

instance ToText CloseStatus where
    toText CloseStatusCanceled = "CANCELED"
    toText CloseStatusCompleted = "COMPLETED"
    toText CloseStatusContinuedAsNew = "CONTINUED_AS_NEW"
    toText CloseStatusFailed = "FAILED"
    toText CloseStatusTerminated = "TERMINATED"
    toText CloseStatusTimedOut = "TIMED_OUT"

instance ToByteString CloseStatus where
    toBS CloseStatusCanceled = "CANCELED"
    toBS CloseStatusCompleted = "COMPLETED"
    toBS CloseStatusContinuedAsNew = "CONTINUED_AS_NEW"
    toBS CloseStatusFailed = "FAILED"
    toBS CloseStatusTerminated = "TERMINATED"
    toBS CloseStatusTimedOut = "TIMED_OUT"

instance ToHeader CloseStatus where
    toHeader k = toHeader k . toBS

instance ToQuery CloseStatus where
    toQuery = toQuery . toBS

instance FromJSON CloseStatus

instance ToJSON CloseStatus

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.
data CompleteWorkflowExecutionFailedCause
    = CompleteWorkflowExecutionFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | CompleteWorkflowExecutionFailedCauseUnhandledDecision -- ^ UNHANDLED_DECISION
      deriving (Eq, Show, Generic)

instance Hashable CompleteWorkflowExecutionFailedCause

instance FromText CompleteWorkflowExecutionFailedCause where
    parser = match "OPERATION_NOT_PERMITTED" CompleteWorkflowExecutionFailedCauseOperationNotPermitted
         <|> match "UNHANDLED_DECISION" CompleteWorkflowExecutionFailedCauseUnhandledDecision

instance ToText CompleteWorkflowExecutionFailedCause where
    toText CompleteWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText CompleteWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"

instance ToByteString CompleteWorkflowExecutionFailedCause where
    toBS CompleteWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS CompleteWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"

instance ToHeader CompleteWorkflowExecutionFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery CompleteWorkflowExecutionFailedCause where
    toQuery = toQuery . toBS

instance FromJSON CompleteWorkflowExecutionFailedCause

instance ToJSON CompleteWorkflowExecutionFailedCause

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.
data ContinueAsNewWorkflowExecutionFailedCause
    = ContinueAsNewWorkflowExecutionFailedCauseDefaultChildPolicyUndefined -- ^ DEFAULT_CHILD_POLICY_UNDEFINED
    | ContinueAsNewWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined -- ^ DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskListUndefined -- ^ DEFAULT_TASK_LIST_UNDEFINED
    | ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined -- ^ DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | ContinueAsNewWorkflowExecutionFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | ContinueAsNewWorkflowExecutionFailedCauseUnhandledDecision -- ^ UNHANDLED_DECISION
    | ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDeprecated -- ^ WORKFLOW_TYPE_DEPRECATED
    | ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist -- ^ WORKFLOW_TYPE_DOES_NOT_EXIST
      deriving (Eq, Show, Generic)

instance Hashable ContinueAsNewWorkflowExecutionFailedCause

instance FromText ContinueAsNewWorkflowExecutionFailedCause where
    parser = match "DEFAULT_CHILD_POLICY_UNDEFINED" ContinueAsNewWorkflowExecutionFailedCauseDefaultChildPolicyUndefined
         <|> match "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED" ContinueAsNewWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined
         <|> match "DEFAULT_TASK_LIST_UNDEFINED" ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskListUndefined
         <|> match "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED" ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined
         <|> match "OPERATION_NOT_PERMITTED" ContinueAsNewWorkflowExecutionFailedCauseOperationNotPermitted
         <|> match "UNHANDLED_DECISION" ContinueAsNewWorkflowExecutionFailedCauseUnhandledDecision
         <|> match "WORKFLOW_TYPE_DEPRECATED" ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDeprecated
         <|> match "WORKFLOW_TYPE_DOES_NOT_EXIST" ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist

instance ToText ContinueAsNewWorkflowExecutionFailedCause where
    toText ContinueAsNewWorkflowExecutionFailedCauseDefaultChildPolicyUndefined = "DEFAULT_CHILD_POLICY_UNDEFINED"
    toText ContinueAsNewWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined = "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskListUndefined = "DEFAULT_TASK_LIST_UNDEFINED"
    toText ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined = "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText ContinueAsNewWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText ContinueAsNewWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"
    toText ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDeprecated = "WORKFLOW_TYPE_DEPRECATED"
    toText ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist = "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance ToByteString ContinueAsNewWorkflowExecutionFailedCause where
    toBS ContinueAsNewWorkflowExecutionFailedCauseDefaultChildPolicyUndefined = "DEFAULT_CHILD_POLICY_UNDEFINED"
    toBS ContinueAsNewWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined = "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toBS ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskListUndefined = "DEFAULT_TASK_LIST_UNDEFINED"
    toBS ContinueAsNewWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined = "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toBS ContinueAsNewWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS ContinueAsNewWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"
    toBS ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDeprecated = "WORKFLOW_TYPE_DEPRECATED"
    toBS ContinueAsNewWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist = "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance ToHeader ContinueAsNewWorkflowExecutionFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery ContinueAsNewWorkflowExecutionFailedCause where
    toQuery = toQuery . toBS

instance FromJSON ContinueAsNewWorkflowExecutionFailedCause

instance ToJSON ContinueAsNewWorkflowExecutionFailedCause

-- | The type of timeout that expired before the decision task could be
-- completed.
data DecisionTaskTimeoutType
    = DecisionTaskTimeoutTypeStartToClose -- ^ START_TO_CLOSE
      deriving (Eq, Show, Generic)

instance Hashable DecisionTaskTimeoutType

instance FromText DecisionTaskTimeoutType where
    parser = match "START_TO_CLOSE" DecisionTaskTimeoutTypeStartToClose

instance ToText DecisionTaskTimeoutType where
    toText DecisionTaskTimeoutTypeStartToClose = "START_TO_CLOSE"

instance ToByteString DecisionTaskTimeoutType where
    toBS DecisionTaskTimeoutTypeStartToClose = "START_TO_CLOSE"

instance ToHeader DecisionTaskTimeoutType where
    toHeader k = toHeader k . toBS

instance ToQuery DecisionTaskTimeoutType where
    toQuery = toQuery . toBS

instance FromJSON DecisionTaskTimeoutType

instance ToJSON DecisionTaskTimeoutType

-- | Specifies the type of the decision.
data DecisionType
    = DecisionTypeCancelTimer -- ^ CancelTimer
    | DecisionTypeCancelWorkflowExecution -- ^ CancelWorkflowExecution
    | DecisionTypeCompleteWorkflowExecution -- ^ CompleteWorkflowExecution
    | DecisionTypeContinueAsNewWorkflowExecution -- ^ ContinueAsNewWorkflowExecution
    | DecisionTypeFailWorkflowExecution -- ^ FailWorkflowExecution
    | DecisionTypeRecordMarker -- ^ RecordMarker
    | DecisionTypeRequestCancelActivityTask -- ^ RequestCancelActivityTask
    | DecisionTypeRequestCancelExternalWorkflowExecution -- ^ RequestCancelExternalWorkflowExecution
    | DecisionTypeScheduleActivityTask -- ^ ScheduleActivityTask
    | DecisionTypeSignalExternalWorkflowExecution -- ^ SignalExternalWorkflowExecution
    | DecisionTypeStartChildWorkflowExecution -- ^ StartChildWorkflowExecution
    | DecisionTypeStartTimer -- ^ StartTimer
      deriving (Eq, Show, Generic)

instance Hashable DecisionType

instance FromText DecisionType where
    parser = match "CancelTimer" DecisionTypeCancelTimer
         <|> match "CancelWorkflowExecution" DecisionTypeCancelWorkflowExecution
         <|> match "CompleteWorkflowExecution" DecisionTypeCompleteWorkflowExecution
         <|> match "ContinueAsNewWorkflowExecution" DecisionTypeContinueAsNewWorkflowExecution
         <|> match "FailWorkflowExecution" DecisionTypeFailWorkflowExecution
         <|> match "RecordMarker" DecisionTypeRecordMarker
         <|> match "RequestCancelActivityTask" DecisionTypeRequestCancelActivityTask
         <|> match "RequestCancelExternalWorkflowExecution" DecisionTypeRequestCancelExternalWorkflowExecution
         <|> match "ScheduleActivityTask" DecisionTypeScheduleActivityTask
         <|> match "SignalExternalWorkflowExecution" DecisionTypeSignalExternalWorkflowExecution
         <|> match "StartChildWorkflowExecution" DecisionTypeStartChildWorkflowExecution
         <|> match "StartTimer" DecisionTypeStartTimer

instance ToText DecisionType where
    toText DecisionTypeCancelTimer = "CancelTimer"
    toText DecisionTypeCancelWorkflowExecution = "CancelWorkflowExecution"
    toText DecisionTypeCompleteWorkflowExecution = "CompleteWorkflowExecution"
    toText DecisionTypeContinueAsNewWorkflowExecution = "ContinueAsNewWorkflowExecution"
    toText DecisionTypeFailWorkflowExecution = "FailWorkflowExecution"
    toText DecisionTypeRecordMarker = "RecordMarker"
    toText DecisionTypeRequestCancelActivityTask = "RequestCancelActivityTask"
    toText DecisionTypeRequestCancelExternalWorkflowExecution = "RequestCancelExternalWorkflowExecution"
    toText DecisionTypeScheduleActivityTask = "ScheduleActivityTask"
    toText DecisionTypeSignalExternalWorkflowExecution = "SignalExternalWorkflowExecution"
    toText DecisionTypeStartChildWorkflowExecution = "StartChildWorkflowExecution"
    toText DecisionTypeStartTimer = "StartTimer"

instance ToByteString DecisionType where
    toBS DecisionTypeCancelTimer = "CancelTimer"
    toBS DecisionTypeCancelWorkflowExecution = "CancelWorkflowExecution"
    toBS DecisionTypeCompleteWorkflowExecution = "CompleteWorkflowExecution"
    toBS DecisionTypeContinueAsNewWorkflowExecution = "ContinueAsNewWorkflowExecution"
    toBS DecisionTypeFailWorkflowExecution = "FailWorkflowExecution"
    toBS DecisionTypeRecordMarker = "RecordMarker"
    toBS DecisionTypeRequestCancelActivityTask = "RequestCancelActivityTask"
    toBS DecisionTypeRequestCancelExternalWorkflowExecution = "RequestCancelExternalWorkflowExecution"
    toBS DecisionTypeScheduleActivityTask = "ScheduleActivityTask"
    toBS DecisionTypeSignalExternalWorkflowExecution = "SignalExternalWorkflowExecution"
    toBS DecisionTypeStartChildWorkflowExecution = "StartChildWorkflowExecution"
    toBS DecisionTypeStartTimer = "StartTimer"

instance ToHeader DecisionType where
    toHeader k = toHeader k . toBS

instance ToQuery DecisionType where
    toQuery = toQuery . toBS

instance FromJSON DecisionType

instance ToJSON DecisionType

-- | The type of the history event.
data EventType
    = EventTypeActivityTaskCancelRequested -- ^ ActivityTaskCancelRequested
    | EventTypeActivityTaskCanceled -- ^ ActivityTaskCanceled
    | EventTypeActivityTaskCompleted -- ^ ActivityTaskCompleted
    | EventTypeActivityTaskFailed -- ^ ActivityTaskFailed
    | EventTypeActivityTaskScheduled -- ^ ActivityTaskScheduled
    | EventTypeActivityTaskStarted -- ^ ActivityTaskStarted
    | EventTypeActivityTaskTimedOut -- ^ ActivityTaskTimedOut
    | EventTypeCancelTimerFailed -- ^ CancelTimerFailed
    | EventTypeCancelWorkflowExecutionFailed -- ^ CancelWorkflowExecutionFailed
    | EventTypeChildWorkflowExecutionCanceled -- ^ ChildWorkflowExecutionCanceled
    | EventTypeChildWorkflowExecutionCompleted -- ^ ChildWorkflowExecutionCompleted
    | EventTypeChildWorkflowExecutionFailed -- ^ ChildWorkflowExecutionFailed
    | EventTypeChildWorkflowExecutionStarted -- ^ ChildWorkflowExecutionStarted
    | EventTypeChildWorkflowExecutionTerminated -- ^ ChildWorkflowExecutionTerminated
    | EventTypeChildWorkflowExecutionTimedOut -- ^ ChildWorkflowExecutionTimedOut
    | EventTypeCompleteWorkflowExecutionFailed -- ^ CompleteWorkflowExecutionFailed
    | EventTypeContinueAsNewWorkflowExecutionFailed -- ^ ContinueAsNewWorkflowExecutionFailed
    | EventTypeDecisionTaskCompleted -- ^ DecisionTaskCompleted
    | EventTypeDecisionTaskScheduled -- ^ DecisionTaskScheduled
    | EventTypeDecisionTaskStarted -- ^ DecisionTaskStarted
    | EventTypeDecisionTaskTimedOut -- ^ DecisionTaskTimedOut
    | EventTypeExternalWorkflowExecutionCancelRequested -- ^ ExternalWorkflowExecutionCancelRequested
    | EventTypeExternalWorkflowExecutionSignaled -- ^ ExternalWorkflowExecutionSignaled
    | EventTypeFailWorkflowExecutionFailed -- ^ FailWorkflowExecutionFailed
    | EventTypeMarkerRecorded -- ^ MarkerRecorded
    | EventTypeRecordMarkerFailed -- ^ RecordMarkerFailed
    | EventTypeRequestCancelActivityTaskFailed -- ^ RequestCancelActivityTaskFailed
    | EventTypeRequestCancelExternalWorkflowExecutionFailed -- ^ RequestCancelExternalWorkflowExecutionFailed
    | EventTypeRequestCancelExternalWorkflowExecutionInitiated -- ^ RequestCancelExternalWorkflowExecutionInitiated
    | EventTypeScheduleActivityTaskFailed -- ^ ScheduleActivityTaskFailed
    | EventTypeSignalExternalWorkflowExecutionFailed -- ^ SignalExternalWorkflowExecutionFailed
    | EventTypeSignalExternalWorkflowExecutionInitiated -- ^ SignalExternalWorkflowExecutionInitiated
    | EventTypeStartChildWorkflowExecutionFailed -- ^ StartChildWorkflowExecutionFailed
    | EventTypeStartChildWorkflowExecutionInitiated -- ^ StartChildWorkflowExecutionInitiated
    | EventTypeStartTimerFailed -- ^ StartTimerFailed
    | EventTypeTimerCanceled -- ^ TimerCanceled
    | EventTypeTimerFired -- ^ TimerFired
    | EventTypeTimerStarted -- ^ TimerStarted
    | EventTypeWorkflowExecutionCancelRequested -- ^ WorkflowExecutionCancelRequested
    | EventTypeWorkflowExecutionCanceled -- ^ WorkflowExecutionCanceled
    | EventTypeWorkflowExecutionCompleted -- ^ WorkflowExecutionCompleted
    | EventTypeWorkflowExecutionContinuedAsNew -- ^ WorkflowExecutionContinuedAsNew
    | EventTypeWorkflowExecutionFailed -- ^ WorkflowExecutionFailed
    | EventTypeWorkflowExecutionSignaled -- ^ WorkflowExecutionSignaled
    | EventTypeWorkflowExecutionStarted -- ^ WorkflowExecutionStarted
    | EventTypeWorkflowExecutionTerminated -- ^ WorkflowExecutionTerminated
    | EventTypeWorkflowExecutionTimedOut -- ^ WorkflowExecutionTimedOut
      deriving (Eq, Show, Generic)

instance Hashable EventType

instance FromText EventType where
    parser = match "ActivityTaskCancelRequested" EventTypeActivityTaskCancelRequested
         <|> match "ActivityTaskCanceled" EventTypeActivityTaskCanceled
         <|> match "ActivityTaskCompleted" EventTypeActivityTaskCompleted
         <|> match "ActivityTaskFailed" EventTypeActivityTaskFailed
         <|> match "ActivityTaskScheduled" EventTypeActivityTaskScheduled
         <|> match "ActivityTaskStarted" EventTypeActivityTaskStarted
         <|> match "ActivityTaskTimedOut" EventTypeActivityTaskTimedOut
         <|> match "CancelTimerFailed" EventTypeCancelTimerFailed
         <|> match "CancelWorkflowExecutionFailed" EventTypeCancelWorkflowExecutionFailed
         <|> match "ChildWorkflowExecutionCanceled" EventTypeChildWorkflowExecutionCanceled
         <|> match "ChildWorkflowExecutionCompleted" EventTypeChildWorkflowExecutionCompleted
         <|> match "ChildWorkflowExecutionFailed" EventTypeChildWorkflowExecutionFailed
         <|> match "ChildWorkflowExecutionStarted" EventTypeChildWorkflowExecutionStarted
         <|> match "ChildWorkflowExecutionTerminated" EventTypeChildWorkflowExecutionTerminated
         <|> match "ChildWorkflowExecutionTimedOut" EventTypeChildWorkflowExecutionTimedOut
         <|> match "CompleteWorkflowExecutionFailed" EventTypeCompleteWorkflowExecutionFailed
         <|> match "ContinueAsNewWorkflowExecutionFailed" EventTypeContinueAsNewWorkflowExecutionFailed
         <|> match "DecisionTaskCompleted" EventTypeDecisionTaskCompleted
         <|> match "DecisionTaskScheduled" EventTypeDecisionTaskScheduled
         <|> match "DecisionTaskStarted" EventTypeDecisionTaskStarted
         <|> match "DecisionTaskTimedOut" EventTypeDecisionTaskTimedOut
         <|> match "ExternalWorkflowExecutionCancelRequested" EventTypeExternalWorkflowExecutionCancelRequested
         <|> match "ExternalWorkflowExecutionSignaled" EventTypeExternalWorkflowExecutionSignaled
         <|> match "FailWorkflowExecutionFailed" EventTypeFailWorkflowExecutionFailed
         <|> match "MarkerRecorded" EventTypeMarkerRecorded
         <|> match "RecordMarkerFailed" EventTypeRecordMarkerFailed
         <|> match "RequestCancelActivityTaskFailed" EventTypeRequestCancelActivityTaskFailed
         <|> match "RequestCancelExternalWorkflowExecutionFailed" EventTypeRequestCancelExternalWorkflowExecutionFailed
         <|> match "RequestCancelExternalWorkflowExecutionInitiated" EventTypeRequestCancelExternalWorkflowExecutionInitiated
         <|> match "ScheduleActivityTaskFailed" EventTypeScheduleActivityTaskFailed
         <|> match "SignalExternalWorkflowExecutionFailed" EventTypeSignalExternalWorkflowExecutionFailed
         <|> match "SignalExternalWorkflowExecutionInitiated" EventTypeSignalExternalWorkflowExecutionInitiated
         <|> match "StartChildWorkflowExecutionFailed" EventTypeStartChildWorkflowExecutionFailed
         <|> match "StartChildWorkflowExecutionInitiated" EventTypeStartChildWorkflowExecutionInitiated
         <|> match "StartTimerFailed" EventTypeStartTimerFailed
         <|> match "TimerCanceled" EventTypeTimerCanceled
         <|> match "TimerFired" EventTypeTimerFired
         <|> match "TimerStarted" EventTypeTimerStarted
         <|> match "WorkflowExecutionCancelRequested" EventTypeWorkflowExecutionCancelRequested
         <|> match "WorkflowExecutionCanceled" EventTypeWorkflowExecutionCanceled
         <|> match "WorkflowExecutionCompleted" EventTypeWorkflowExecutionCompleted
         <|> match "WorkflowExecutionContinuedAsNew" EventTypeWorkflowExecutionContinuedAsNew
         <|> match "WorkflowExecutionFailed" EventTypeWorkflowExecutionFailed
         <|> match "WorkflowExecutionSignaled" EventTypeWorkflowExecutionSignaled
         <|> match "WorkflowExecutionStarted" EventTypeWorkflowExecutionStarted
         <|> match "WorkflowExecutionTerminated" EventTypeWorkflowExecutionTerminated
         <|> match "WorkflowExecutionTimedOut" EventTypeWorkflowExecutionTimedOut

instance ToText EventType where
    toText EventTypeActivityTaskCancelRequested = "ActivityTaskCancelRequested"
    toText EventTypeActivityTaskCanceled = "ActivityTaskCanceled"
    toText EventTypeActivityTaskCompleted = "ActivityTaskCompleted"
    toText EventTypeActivityTaskFailed = "ActivityTaskFailed"
    toText EventTypeActivityTaskScheduled = "ActivityTaskScheduled"
    toText EventTypeActivityTaskStarted = "ActivityTaskStarted"
    toText EventTypeActivityTaskTimedOut = "ActivityTaskTimedOut"
    toText EventTypeCancelTimerFailed = "CancelTimerFailed"
    toText EventTypeCancelWorkflowExecutionFailed = "CancelWorkflowExecutionFailed"
    toText EventTypeChildWorkflowExecutionCanceled = "ChildWorkflowExecutionCanceled"
    toText EventTypeChildWorkflowExecutionCompleted = "ChildWorkflowExecutionCompleted"
    toText EventTypeChildWorkflowExecutionFailed = "ChildWorkflowExecutionFailed"
    toText EventTypeChildWorkflowExecutionStarted = "ChildWorkflowExecutionStarted"
    toText EventTypeChildWorkflowExecutionTerminated = "ChildWorkflowExecutionTerminated"
    toText EventTypeChildWorkflowExecutionTimedOut = "ChildWorkflowExecutionTimedOut"
    toText EventTypeCompleteWorkflowExecutionFailed = "CompleteWorkflowExecutionFailed"
    toText EventTypeContinueAsNewWorkflowExecutionFailed = "ContinueAsNewWorkflowExecutionFailed"
    toText EventTypeDecisionTaskCompleted = "DecisionTaskCompleted"
    toText EventTypeDecisionTaskScheduled = "DecisionTaskScheduled"
    toText EventTypeDecisionTaskStarted = "DecisionTaskStarted"
    toText EventTypeDecisionTaskTimedOut = "DecisionTaskTimedOut"
    toText EventTypeExternalWorkflowExecutionCancelRequested = "ExternalWorkflowExecutionCancelRequested"
    toText EventTypeExternalWorkflowExecutionSignaled = "ExternalWorkflowExecutionSignaled"
    toText EventTypeFailWorkflowExecutionFailed = "FailWorkflowExecutionFailed"
    toText EventTypeMarkerRecorded = "MarkerRecorded"
    toText EventTypeRecordMarkerFailed = "RecordMarkerFailed"
    toText EventTypeRequestCancelActivityTaskFailed = "RequestCancelActivityTaskFailed"
    toText EventTypeRequestCancelExternalWorkflowExecutionFailed = "RequestCancelExternalWorkflowExecutionFailed"
    toText EventTypeRequestCancelExternalWorkflowExecutionInitiated = "RequestCancelExternalWorkflowExecutionInitiated"
    toText EventTypeScheduleActivityTaskFailed = "ScheduleActivityTaskFailed"
    toText EventTypeSignalExternalWorkflowExecutionFailed = "SignalExternalWorkflowExecutionFailed"
    toText EventTypeSignalExternalWorkflowExecutionInitiated = "SignalExternalWorkflowExecutionInitiated"
    toText EventTypeStartChildWorkflowExecutionFailed = "StartChildWorkflowExecutionFailed"
    toText EventTypeStartChildWorkflowExecutionInitiated = "StartChildWorkflowExecutionInitiated"
    toText EventTypeStartTimerFailed = "StartTimerFailed"
    toText EventTypeTimerCanceled = "TimerCanceled"
    toText EventTypeTimerFired = "TimerFired"
    toText EventTypeTimerStarted = "TimerStarted"
    toText EventTypeWorkflowExecutionCancelRequested = "WorkflowExecutionCancelRequested"
    toText EventTypeWorkflowExecutionCanceled = "WorkflowExecutionCanceled"
    toText EventTypeWorkflowExecutionCompleted = "WorkflowExecutionCompleted"
    toText EventTypeWorkflowExecutionContinuedAsNew = "WorkflowExecutionContinuedAsNew"
    toText EventTypeWorkflowExecutionFailed = "WorkflowExecutionFailed"
    toText EventTypeWorkflowExecutionSignaled = "WorkflowExecutionSignaled"
    toText EventTypeWorkflowExecutionStarted = "WorkflowExecutionStarted"
    toText EventTypeWorkflowExecutionTerminated = "WorkflowExecutionTerminated"
    toText EventTypeWorkflowExecutionTimedOut = "WorkflowExecutionTimedOut"

instance ToByteString EventType where
    toBS EventTypeActivityTaskCancelRequested = "ActivityTaskCancelRequested"
    toBS EventTypeActivityTaskCanceled = "ActivityTaskCanceled"
    toBS EventTypeActivityTaskCompleted = "ActivityTaskCompleted"
    toBS EventTypeActivityTaskFailed = "ActivityTaskFailed"
    toBS EventTypeActivityTaskScheduled = "ActivityTaskScheduled"
    toBS EventTypeActivityTaskStarted = "ActivityTaskStarted"
    toBS EventTypeActivityTaskTimedOut = "ActivityTaskTimedOut"
    toBS EventTypeCancelTimerFailed = "CancelTimerFailed"
    toBS EventTypeCancelWorkflowExecutionFailed = "CancelWorkflowExecutionFailed"
    toBS EventTypeChildWorkflowExecutionCanceled = "ChildWorkflowExecutionCanceled"
    toBS EventTypeChildWorkflowExecutionCompleted = "ChildWorkflowExecutionCompleted"
    toBS EventTypeChildWorkflowExecutionFailed = "ChildWorkflowExecutionFailed"
    toBS EventTypeChildWorkflowExecutionStarted = "ChildWorkflowExecutionStarted"
    toBS EventTypeChildWorkflowExecutionTerminated = "ChildWorkflowExecutionTerminated"
    toBS EventTypeChildWorkflowExecutionTimedOut = "ChildWorkflowExecutionTimedOut"
    toBS EventTypeCompleteWorkflowExecutionFailed = "CompleteWorkflowExecutionFailed"
    toBS EventTypeContinueAsNewWorkflowExecutionFailed = "ContinueAsNewWorkflowExecutionFailed"
    toBS EventTypeDecisionTaskCompleted = "DecisionTaskCompleted"
    toBS EventTypeDecisionTaskScheduled = "DecisionTaskScheduled"
    toBS EventTypeDecisionTaskStarted = "DecisionTaskStarted"
    toBS EventTypeDecisionTaskTimedOut = "DecisionTaskTimedOut"
    toBS EventTypeExternalWorkflowExecutionCancelRequested = "ExternalWorkflowExecutionCancelRequested"
    toBS EventTypeExternalWorkflowExecutionSignaled = "ExternalWorkflowExecutionSignaled"
    toBS EventTypeFailWorkflowExecutionFailed = "FailWorkflowExecutionFailed"
    toBS EventTypeMarkerRecorded = "MarkerRecorded"
    toBS EventTypeRecordMarkerFailed = "RecordMarkerFailed"
    toBS EventTypeRequestCancelActivityTaskFailed = "RequestCancelActivityTaskFailed"
    toBS EventTypeRequestCancelExternalWorkflowExecutionFailed = "RequestCancelExternalWorkflowExecutionFailed"
    toBS EventTypeRequestCancelExternalWorkflowExecutionInitiated = "RequestCancelExternalWorkflowExecutionInitiated"
    toBS EventTypeScheduleActivityTaskFailed = "ScheduleActivityTaskFailed"
    toBS EventTypeSignalExternalWorkflowExecutionFailed = "SignalExternalWorkflowExecutionFailed"
    toBS EventTypeSignalExternalWorkflowExecutionInitiated = "SignalExternalWorkflowExecutionInitiated"
    toBS EventTypeStartChildWorkflowExecutionFailed = "StartChildWorkflowExecutionFailed"
    toBS EventTypeStartChildWorkflowExecutionInitiated = "StartChildWorkflowExecutionInitiated"
    toBS EventTypeStartTimerFailed = "StartTimerFailed"
    toBS EventTypeTimerCanceled = "TimerCanceled"
    toBS EventTypeTimerFired = "TimerFired"
    toBS EventTypeTimerStarted = "TimerStarted"
    toBS EventTypeWorkflowExecutionCancelRequested = "WorkflowExecutionCancelRequested"
    toBS EventTypeWorkflowExecutionCanceled = "WorkflowExecutionCanceled"
    toBS EventTypeWorkflowExecutionCompleted = "WorkflowExecutionCompleted"
    toBS EventTypeWorkflowExecutionContinuedAsNew = "WorkflowExecutionContinuedAsNew"
    toBS EventTypeWorkflowExecutionFailed = "WorkflowExecutionFailed"
    toBS EventTypeWorkflowExecutionSignaled = "WorkflowExecutionSignaled"
    toBS EventTypeWorkflowExecutionStarted = "WorkflowExecutionStarted"
    toBS EventTypeWorkflowExecutionTerminated = "WorkflowExecutionTerminated"
    toBS EventTypeWorkflowExecutionTimedOut = "WorkflowExecutionTimedOut"

instance ToHeader EventType where
    toHeader k = toHeader k . toBS

instance ToQuery EventType where
    toQuery = toQuery . toBS

instance FromJSON EventType

instance ToJSON EventType

-- | The current status of the execution.
data ExecutionStatus
    = ExecutionStatusClosed -- ^ CLOSED
    | ExecutionStatusOpen -- ^ OPEN
      deriving (Eq, Show, Generic)

instance Hashable ExecutionStatus

instance FromText ExecutionStatus where
    parser = match "CLOSED" ExecutionStatusClosed
         <|> match "OPEN" ExecutionStatusOpen

instance ToText ExecutionStatus where
    toText ExecutionStatusClosed = "CLOSED"
    toText ExecutionStatusOpen = "OPEN"

instance ToByteString ExecutionStatus where
    toBS ExecutionStatusClosed = "CLOSED"
    toBS ExecutionStatusOpen = "OPEN"

instance ToHeader ExecutionStatus where
    toHeader k = toHeader k . toBS

instance ToQuery ExecutionStatus where
    toQuery = toQuery . toBS

instance FromJSON ExecutionStatus

instance ToJSON ExecutionStatus

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.
data FailWorkflowExecutionFailedCause
    = FailWorkflowExecutionFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | FailWorkflowExecutionFailedCauseUnhandledDecision -- ^ UNHANDLED_DECISION
      deriving (Eq, Show, Generic)

instance Hashable FailWorkflowExecutionFailedCause

instance FromText FailWorkflowExecutionFailedCause where
    parser = match "OPERATION_NOT_PERMITTED" FailWorkflowExecutionFailedCauseOperationNotPermitted
         <|> match "UNHANDLED_DECISION" FailWorkflowExecutionFailedCauseUnhandledDecision

instance ToText FailWorkflowExecutionFailedCause where
    toText FailWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText FailWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"

instance ToByteString FailWorkflowExecutionFailedCause where
    toBS FailWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS FailWorkflowExecutionFailedCauseUnhandledDecision = "UNHANDLED_DECISION"

instance ToHeader FailWorkflowExecutionFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery FailWorkflowExecutionFailedCause where
    toQuery = toQuery . toBS

instance FromJSON FailWorkflowExecutionFailedCause

instance ToJSON FailWorkflowExecutionFailedCause

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data RecordMarkerFailedCause
    = RecordMarkerFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
      deriving (Eq, Show, Generic)

instance Hashable RecordMarkerFailedCause

instance FromText RecordMarkerFailedCause where
    parser = match "OPERATION_NOT_PERMITTED" RecordMarkerFailedCauseOperationNotPermitted

instance ToText RecordMarkerFailedCause where
    toText RecordMarkerFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"

instance ToByteString RecordMarkerFailedCause where
    toBS RecordMarkerFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"

instance ToHeader RecordMarkerFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery RecordMarkerFailedCause where
    toQuery = toQuery . toBS

instance FromJSON RecordMarkerFailedCause

instance ToJSON RecordMarkerFailedCause

-- | The current status of the activity type.
data RegistrationStatus
    = RegistrationStatusDeprecated -- ^ DEPRECATED
    | RegistrationStatusRegistered -- ^ REGISTERED
      deriving (Eq, Show, Generic)

instance Hashable RegistrationStatus

instance FromText RegistrationStatus where
    parser = match "DEPRECATED" RegistrationStatusDeprecated
         <|> match "REGISTERED" RegistrationStatusRegistered

instance ToText RegistrationStatus where
    toText RegistrationStatusDeprecated = "DEPRECATED"
    toText RegistrationStatusRegistered = "REGISTERED"

instance ToByteString RegistrationStatus where
    toBS RegistrationStatusDeprecated = "DEPRECATED"
    toBS RegistrationStatusRegistered = "REGISTERED"

instance ToHeader RegistrationStatus where
    toHeader k = toHeader k . toBS

instance ToQuery RegistrationStatus where
    toQuery = toQuery . toBS

instance FromJSON RegistrationStatus

instance ToJSON RegistrationStatus

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data RequestCancelActivityTaskFailedCause
    = RequestCancelActivityTaskFailedCauseActivityIdUnknown -- ^ ACTIVITY_ID_UNKNOWN
    | RequestCancelActivityTaskFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
      deriving (Eq, Show, Generic)

instance Hashable RequestCancelActivityTaskFailedCause

instance FromText RequestCancelActivityTaskFailedCause where
    parser = match "ACTIVITY_ID_UNKNOWN" RequestCancelActivityTaskFailedCauseActivityIdUnknown
         <|> match "OPERATION_NOT_PERMITTED" RequestCancelActivityTaskFailedCauseOperationNotPermitted

instance ToText RequestCancelActivityTaskFailedCause where
    toText RequestCancelActivityTaskFailedCauseActivityIdUnknown = "ACTIVITY_ID_UNKNOWN"
    toText RequestCancelActivityTaskFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"

instance ToByteString RequestCancelActivityTaskFailedCause where
    toBS RequestCancelActivityTaskFailedCauseActivityIdUnknown = "ACTIVITY_ID_UNKNOWN"
    toBS RequestCancelActivityTaskFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"

instance ToHeader RequestCancelActivityTaskFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery RequestCancelActivityTaskFailedCause where
    toQuery = toQuery . toBS

instance FromJSON RequestCancelActivityTaskFailedCause

instance ToJSON RequestCancelActivityTaskFailedCause

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data RequestCancelExternalWorkflowExecutionFailedCause
    = RequestCancelExternalWorkflowExecutionFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | RequestCancelExternalWorkflowExecutionFailedCauseRequestCancelExternalWorkflowExecutionRateExceeded -- ^ REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED
    | RequestCancelExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution -- ^ UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION
      deriving (Eq, Show, Generic)

instance Hashable RequestCancelExternalWorkflowExecutionFailedCause

instance FromText RequestCancelExternalWorkflowExecutionFailedCause where
    parser = match "OPERATION_NOT_PERMITTED" RequestCancelExternalWorkflowExecutionFailedCauseOperationNotPermitted
         <|> match "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED" RequestCancelExternalWorkflowExecutionFailedCauseRequestCancelExternalWorkflowExecutionRateExceeded
         <|> match "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION" RequestCancelExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution

instance ToText RequestCancelExternalWorkflowExecutionFailedCause where
    toText RequestCancelExternalWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText RequestCancelExternalWorkflowExecutionFailedCauseRequestCancelExternalWorkflowExecutionRateExceeded = "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
    toText RequestCancelExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution = "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance ToByteString RequestCancelExternalWorkflowExecutionFailedCause where
    toBS RequestCancelExternalWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS RequestCancelExternalWorkflowExecutionFailedCauseRequestCancelExternalWorkflowExecutionRateExceeded = "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
    toBS RequestCancelExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution = "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance ToHeader RequestCancelExternalWorkflowExecutionFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery RequestCancelExternalWorkflowExecutionFailedCause where
    toQuery = toQuery . toBS

instance FromJSON RequestCancelExternalWorkflowExecutionFailedCause

instance ToJSON RequestCancelExternalWorkflowExecutionFailedCause

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data ScheduleActivityTaskFailedCause
    = ScheduleActivityTaskFailedCauseActivityCreationRateExceeded -- ^ ACTIVITY_CREATION_RATE_EXCEEDED
    | ScheduleActivityTaskFailedCauseActivityIdAlreadyInUse -- ^ ACTIVITY_ID_ALREADY_IN_USE
    | ScheduleActivityTaskFailedCauseActivityTypeDeprecated -- ^ ACTIVITY_TYPE_DEPRECATED
    | ScheduleActivityTaskFailedCauseActivityTypeDoesNotExist -- ^ ACTIVITY_TYPE_DOES_NOT_EXIST
    | ScheduleActivityTaskFailedCauseDefaultHeartbeatTimeoutUndefined -- ^ DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED
    | ScheduleActivityTaskFailedCauseDefaultScheduleToCloseTimeoutUndefined -- ^ DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED
    | ScheduleActivityTaskFailedCauseDefaultScheduleToStartTimeoutUndefined -- ^ DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED
    | ScheduleActivityTaskFailedCauseDefaultStartToCloseTimeoutUndefined -- ^ DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | ScheduleActivityTaskFailedCauseDefaultTaskListUndefined -- ^ DEFAULT_TASK_LIST_UNDEFINED
    | ScheduleActivityTaskFailedCauseOpenActivitiesLimitExceeded -- ^ OPEN_ACTIVITIES_LIMIT_EXCEEDED
    | ScheduleActivityTaskFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
      deriving (Eq, Show, Generic)

instance Hashable ScheduleActivityTaskFailedCause

instance FromText ScheduleActivityTaskFailedCause where
    parser = match "ACTIVITY_CREATION_RATE_EXCEEDED" ScheduleActivityTaskFailedCauseActivityCreationRateExceeded
         <|> match "ACTIVITY_ID_ALREADY_IN_USE" ScheduleActivityTaskFailedCauseActivityIdAlreadyInUse
         <|> match "ACTIVITY_TYPE_DEPRECATED" ScheduleActivityTaskFailedCauseActivityTypeDeprecated
         <|> match "ACTIVITY_TYPE_DOES_NOT_EXIST" ScheduleActivityTaskFailedCauseActivityTypeDoesNotExist
         <|> match "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED" ScheduleActivityTaskFailedCauseDefaultHeartbeatTimeoutUndefined
         <|> match "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED" ScheduleActivityTaskFailedCauseDefaultScheduleToCloseTimeoutUndefined
         <|> match "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED" ScheduleActivityTaskFailedCauseDefaultScheduleToStartTimeoutUndefined
         <|> match "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED" ScheduleActivityTaskFailedCauseDefaultStartToCloseTimeoutUndefined
         <|> match "DEFAULT_TASK_LIST_UNDEFINED" ScheduleActivityTaskFailedCauseDefaultTaskListUndefined
         <|> match "OPEN_ACTIVITIES_LIMIT_EXCEEDED" ScheduleActivityTaskFailedCauseOpenActivitiesLimitExceeded
         <|> match "OPERATION_NOT_PERMITTED" ScheduleActivityTaskFailedCauseOperationNotPermitted

instance ToText ScheduleActivityTaskFailedCause where
    toText ScheduleActivityTaskFailedCauseActivityCreationRateExceeded = "ACTIVITY_CREATION_RATE_EXCEEDED"
    toText ScheduleActivityTaskFailedCauseActivityIdAlreadyInUse = "ACTIVITY_ID_ALREADY_IN_USE"
    toText ScheduleActivityTaskFailedCauseActivityTypeDeprecated = "ACTIVITY_TYPE_DEPRECATED"
    toText ScheduleActivityTaskFailedCauseActivityTypeDoesNotExist = "ACTIVITY_TYPE_DOES_NOT_EXIST"
    toText ScheduleActivityTaskFailedCauseDefaultHeartbeatTimeoutUndefined = "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED"
    toText ScheduleActivityTaskFailedCauseDefaultScheduleToCloseTimeoutUndefined = "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText ScheduleActivityTaskFailedCauseDefaultScheduleToStartTimeoutUndefined = "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED"
    toText ScheduleActivityTaskFailedCauseDefaultStartToCloseTimeoutUndefined = "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText ScheduleActivityTaskFailedCauseDefaultTaskListUndefined = "DEFAULT_TASK_LIST_UNDEFINED"
    toText ScheduleActivityTaskFailedCauseOpenActivitiesLimitExceeded = "OPEN_ACTIVITIES_LIMIT_EXCEEDED"
    toText ScheduleActivityTaskFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"

instance ToByteString ScheduleActivityTaskFailedCause where
    toBS ScheduleActivityTaskFailedCauseActivityCreationRateExceeded = "ACTIVITY_CREATION_RATE_EXCEEDED"
    toBS ScheduleActivityTaskFailedCauseActivityIdAlreadyInUse = "ACTIVITY_ID_ALREADY_IN_USE"
    toBS ScheduleActivityTaskFailedCauseActivityTypeDeprecated = "ACTIVITY_TYPE_DEPRECATED"
    toBS ScheduleActivityTaskFailedCauseActivityTypeDoesNotExist = "ACTIVITY_TYPE_DOES_NOT_EXIST"
    toBS ScheduleActivityTaskFailedCauseDefaultHeartbeatTimeoutUndefined = "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED"
    toBS ScheduleActivityTaskFailedCauseDefaultScheduleToCloseTimeoutUndefined = "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED"
    toBS ScheduleActivityTaskFailedCauseDefaultScheduleToStartTimeoutUndefined = "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED"
    toBS ScheduleActivityTaskFailedCauseDefaultStartToCloseTimeoutUndefined = "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toBS ScheduleActivityTaskFailedCauseDefaultTaskListUndefined = "DEFAULT_TASK_LIST_UNDEFINED"
    toBS ScheduleActivityTaskFailedCauseOpenActivitiesLimitExceeded = "OPEN_ACTIVITIES_LIMIT_EXCEEDED"
    toBS ScheduleActivityTaskFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"

instance ToHeader ScheduleActivityTaskFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery ScheduleActivityTaskFailedCause where
    toQuery = toQuery . toBS

instance FromJSON ScheduleActivityTaskFailedCause

instance ToJSON ScheduleActivityTaskFailedCause

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data SignalExternalWorkflowExecutionFailedCause
    = SignalExternalWorkflowExecutionFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | SignalExternalWorkflowExecutionFailedCauseSignalExternalWorkflowExecutionRateExceeded -- ^ SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED
    | SignalExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution -- ^ UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION
      deriving (Eq, Show, Generic)

instance Hashable SignalExternalWorkflowExecutionFailedCause

instance FromText SignalExternalWorkflowExecutionFailedCause where
    parser = match "OPERATION_NOT_PERMITTED" SignalExternalWorkflowExecutionFailedCauseOperationNotPermitted
         <|> match "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED" SignalExternalWorkflowExecutionFailedCauseSignalExternalWorkflowExecutionRateExceeded
         <|> match "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION" SignalExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution

instance ToText SignalExternalWorkflowExecutionFailedCause where
    toText SignalExternalWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText SignalExternalWorkflowExecutionFailedCauseSignalExternalWorkflowExecutionRateExceeded = "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
    toText SignalExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution = "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance ToByteString SignalExternalWorkflowExecutionFailedCause where
    toBS SignalExternalWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS SignalExternalWorkflowExecutionFailedCauseSignalExternalWorkflowExecutionRateExceeded = "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
    toBS SignalExternalWorkflowExecutionFailedCauseUnknownExternalWorkflowExecution = "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance ToHeader SignalExternalWorkflowExecutionFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery SignalExternalWorkflowExecutionFailedCause where
    toQuery = toQuery . toBS

instance FromJSON SignalExternalWorkflowExecutionFailedCause

instance ToJSON SignalExternalWorkflowExecutionFailedCause

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data StartChildWorkflowExecutionFailedCause
    = StartChildWorkflowExecutionFailedCauseChildCreationRateExceeded -- ^ CHILD_CREATION_RATE_EXCEEDED
    | StartChildWorkflowExecutionFailedCauseDefaultChildPolicyUndefined -- ^ DEFAULT_CHILD_POLICY_UNDEFINED
    | StartChildWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined -- ^ DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | StartChildWorkflowExecutionFailedCauseDefaultTaskListUndefined -- ^ DEFAULT_TASK_LIST_UNDEFINED
    | StartChildWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined -- ^ DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | StartChildWorkflowExecutionFailedCauseOpenChildrenLimitExceeded -- ^ OPEN_CHILDREN_LIMIT_EXCEEDED
    | StartChildWorkflowExecutionFailedCauseOpenWorkflowsLimitExceeded -- ^ OPEN_WORKFLOWS_LIMIT_EXCEEDED
    | StartChildWorkflowExecutionFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | StartChildWorkflowExecutionFailedCauseWorkflowAlreadyRunning -- ^ WORKFLOW_ALREADY_RUNNING
    | StartChildWorkflowExecutionFailedCauseWorkflowTypeDeprecated -- ^ WORKFLOW_TYPE_DEPRECATED
    | StartChildWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist -- ^ WORKFLOW_TYPE_DOES_NOT_EXIST
      deriving (Eq, Show, Generic)

instance Hashable StartChildWorkflowExecutionFailedCause

instance FromText StartChildWorkflowExecutionFailedCause where
    parser = match "CHILD_CREATION_RATE_EXCEEDED" StartChildWorkflowExecutionFailedCauseChildCreationRateExceeded
         <|> match "DEFAULT_CHILD_POLICY_UNDEFINED" StartChildWorkflowExecutionFailedCauseDefaultChildPolicyUndefined
         <|> match "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED" StartChildWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined
         <|> match "DEFAULT_TASK_LIST_UNDEFINED" StartChildWorkflowExecutionFailedCauseDefaultTaskListUndefined
         <|> match "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED" StartChildWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined
         <|> match "OPEN_CHILDREN_LIMIT_EXCEEDED" StartChildWorkflowExecutionFailedCauseOpenChildrenLimitExceeded
         <|> match "OPEN_WORKFLOWS_LIMIT_EXCEEDED" StartChildWorkflowExecutionFailedCauseOpenWorkflowsLimitExceeded
         <|> match "OPERATION_NOT_PERMITTED" StartChildWorkflowExecutionFailedCauseOperationNotPermitted
         <|> match "WORKFLOW_ALREADY_RUNNING" StartChildWorkflowExecutionFailedCauseWorkflowAlreadyRunning
         <|> match "WORKFLOW_TYPE_DEPRECATED" StartChildWorkflowExecutionFailedCauseWorkflowTypeDeprecated
         <|> match "WORKFLOW_TYPE_DOES_NOT_EXIST" StartChildWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist

instance ToText StartChildWorkflowExecutionFailedCause where
    toText StartChildWorkflowExecutionFailedCauseChildCreationRateExceeded = "CHILD_CREATION_RATE_EXCEEDED"
    toText StartChildWorkflowExecutionFailedCauseDefaultChildPolicyUndefined = "DEFAULT_CHILD_POLICY_UNDEFINED"
    toText StartChildWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined = "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText StartChildWorkflowExecutionFailedCauseDefaultTaskListUndefined = "DEFAULT_TASK_LIST_UNDEFINED"
    toText StartChildWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined = "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toText StartChildWorkflowExecutionFailedCauseOpenChildrenLimitExceeded = "OPEN_CHILDREN_LIMIT_EXCEEDED"
    toText StartChildWorkflowExecutionFailedCauseOpenWorkflowsLimitExceeded = "OPEN_WORKFLOWS_LIMIT_EXCEEDED"
    toText StartChildWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText StartChildWorkflowExecutionFailedCauseWorkflowAlreadyRunning = "WORKFLOW_ALREADY_RUNNING"
    toText StartChildWorkflowExecutionFailedCauseWorkflowTypeDeprecated = "WORKFLOW_TYPE_DEPRECATED"
    toText StartChildWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist = "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance ToByteString StartChildWorkflowExecutionFailedCause where
    toBS StartChildWorkflowExecutionFailedCauseChildCreationRateExceeded = "CHILD_CREATION_RATE_EXCEEDED"
    toBS StartChildWorkflowExecutionFailedCauseDefaultChildPolicyUndefined = "DEFAULT_CHILD_POLICY_UNDEFINED"
    toBS StartChildWorkflowExecutionFailedCauseDefaultExecutionStartToCloseTimeoutUndefined = "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toBS StartChildWorkflowExecutionFailedCauseDefaultTaskListUndefined = "DEFAULT_TASK_LIST_UNDEFINED"
    toBS StartChildWorkflowExecutionFailedCauseDefaultTaskStartToCloseTimeoutUndefined = "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    toBS StartChildWorkflowExecutionFailedCauseOpenChildrenLimitExceeded = "OPEN_CHILDREN_LIMIT_EXCEEDED"
    toBS StartChildWorkflowExecutionFailedCauseOpenWorkflowsLimitExceeded = "OPEN_WORKFLOWS_LIMIT_EXCEEDED"
    toBS StartChildWorkflowExecutionFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS StartChildWorkflowExecutionFailedCauseWorkflowAlreadyRunning = "WORKFLOW_ALREADY_RUNNING"
    toBS StartChildWorkflowExecutionFailedCauseWorkflowTypeDeprecated = "WORKFLOW_TYPE_DEPRECATED"
    toBS StartChildWorkflowExecutionFailedCauseWorkflowTypeDoesNotExist = "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance ToHeader StartChildWorkflowExecutionFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery StartChildWorkflowExecutionFailedCause where
    toQuery = toQuery . toBS

instance FromJSON StartChildWorkflowExecutionFailedCause

instance ToJSON StartChildWorkflowExecutionFailedCause

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
data StartTimerFailedCause
    = StartTimerFailedCauseOpenTimersLimitExceeded -- ^ OPEN_TIMERS_LIMIT_EXCEEDED
    | StartTimerFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | StartTimerFailedCauseTimerCreationRateExceeded -- ^ TIMER_CREATION_RATE_EXCEEDED
    | StartTimerFailedCauseTimerIdAlreadyInUse -- ^ TIMER_ID_ALREADY_IN_USE
      deriving (Eq, Show, Generic)

instance Hashable StartTimerFailedCause

instance FromText StartTimerFailedCause where
    parser = match "OPEN_TIMERS_LIMIT_EXCEEDED" StartTimerFailedCauseOpenTimersLimitExceeded
         <|> match "OPERATION_NOT_PERMITTED" StartTimerFailedCauseOperationNotPermitted
         <|> match "TIMER_CREATION_RATE_EXCEEDED" StartTimerFailedCauseTimerCreationRateExceeded
         <|> match "TIMER_ID_ALREADY_IN_USE" StartTimerFailedCauseTimerIdAlreadyInUse

instance ToText StartTimerFailedCause where
    toText StartTimerFailedCauseOpenTimersLimitExceeded = "OPEN_TIMERS_LIMIT_EXCEEDED"
    toText StartTimerFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toText StartTimerFailedCauseTimerCreationRateExceeded = "TIMER_CREATION_RATE_EXCEEDED"
    toText StartTimerFailedCauseTimerIdAlreadyInUse = "TIMER_ID_ALREADY_IN_USE"

instance ToByteString StartTimerFailedCause where
    toBS StartTimerFailedCauseOpenTimersLimitExceeded = "OPEN_TIMERS_LIMIT_EXCEEDED"
    toBS StartTimerFailedCauseOperationNotPermitted = "OPERATION_NOT_PERMITTED"
    toBS StartTimerFailedCauseTimerCreationRateExceeded = "TIMER_CREATION_RATE_EXCEEDED"
    toBS StartTimerFailedCauseTimerIdAlreadyInUse = "TIMER_ID_ALREADY_IN_USE"

instance ToHeader StartTimerFailedCause where
    toHeader k = toHeader k . toBS

instance ToQuery StartTimerFailedCause where
    toQuery = toQuery . toBS

instance FromJSON StartTimerFailedCause

instance ToJSON StartTimerFailedCause

-- | If set, indicates that the request to cancel the workflow execution was
-- automatically generated, and specifies the cause. This happens if the
-- parent workflow execution times out or is terminated, and the child policy
-- is set to cancel child executions.
data WorkflowExecutionCancelRequestedCause
    = WorkflowExecutionCancelRequestedCauseChildPolicyApplied -- ^ CHILD_POLICY_APPLIED
      deriving (Eq, Show, Generic)

instance Hashable WorkflowExecutionCancelRequestedCause

instance FromText WorkflowExecutionCancelRequestedCause where
    parser = match "CHILD_POLICY_APPLIED" WorkflowExecutionCancelRequestedCauseChildPolicyApplied

instance ToText WorkflowExecutionCancelRequestedCause where
    toText WorkflowExecutionCancelRequestedCauseChildPolicyApplied = "CHILD_POLICY_APPLIED"

instance ToByteString WorkflowExecutionCancelRequestedCause where
    toBS WorkflowExecutionCancelRequestedCauseChildPolicyApplied = "CHILD_POLICY_APPLIED"

instance ToHeader WorkflowExecutionCancelRequestedCause where
    toHeader k = toHeader k . toBS

instance ToQuery WorkflowExecutionCancelRequestedCause where
    toQuery = toQuery . toBS

instance FromJSON WorkflowExecutionCancelRequestedCause

instance ToJSON WorkflowExecutionCancelRequestedCause

-- | If set, indicates that the workflow execution was automatically terminated,
-- and specifies the cause. This happens if the parent workflow execution
-- times out or is terminated and the child policy is set to terminate child
-- executions.
data WorkflowExecutionTerminatedCause
    = WorkflowExecutionTerminatedCauseChildPolicyApplied -- ^ CHILD_POLICY_APPLIED
    | WorkflowExecutionTerminatedCauseEventLimitExceeded -- ^ EVENT_LIMIT_EXCEEDED
    | WorkflowExecutionTerminatedCauseOperatorInitiated -- ^ OPERATOR_INITIATED
      deriving (Eq, Show, Generic)

instance Hashable WorkflowExecutionTerminatedCause

instance FromText WorkflowExecutionTerminatedCause where
    parser = match "CHILD_POLICY_APPLIED" WorkflowExecutionTerminatedCauseChildPolicyApplied
         <|> match "EVENT_LIMIT_EXCEEDED" WorkflowExecutionTerminatedCauseEventLimitExceeded
         <|> match "OPERATOR_INITIATED" WorkflowExecutionTerminatedCauseOperatorInitiated

instance ToText WorkflowExecutionTerminatedCause where
    toText WorkflowExecutionTerminatedCauseChildPolicyApplied = "CHILD_POLICY_APPLIED"
    toText WorkflowExecutionTerminatedCauseEventLimitExceeded = "EVENT_LIMIT_EXCEEDED"
    toText WorkflowExecutionTerminatedCauseOperatorInitiated = "OPERATOR_INITIATED"

instance ToByteString WorkflowExecutionTerminatedCause where
    toBS WorkflowExecutionTerminatedCauseChildPolicyApplied = "CHILD_POLICY_APPLIED"
    toBS WorkflowExecutionTerminatedCauseEventLimitExceeded = "EVENT_LIMIT_EXCEEDED"
    toBS WorkflowExecutionTerminatedCauseOperatorInitiated = "OPERATOR_INITIATED"

instance ToHeader WorkflowExecutionTerminatedCause where
    toHeader k = toHeader k . toBS

instance ToQuery WorkflowExecutionTerminatedCause where
    toQuery = toQuery . toBS

instance FromJSON WorkflowExecutionTerminatedCause

instance ToJSON WorkflowExecutionTerminatedCause

-- | The type of timeout that caused this event.
data WorkflowExecutionTimeoutType
    = WorkflowExecutionTimeoutTypeStartToClose -- ^ START_TO_CLOSE
      deriving (Eq, Show, Generic)

instance Hashable WorkflowExecutionTimeoutType

instance FromText WorkflowExecutionTimeoutType where
    parser = match "START_TO_CLOSE" WorkflowExecutionTimeoutTypeStartToClose

instance ToText WorkflowExecutionTimeoutType where
    toText WorkflowExecutionTimeoutTypeStartToClose = "START_TO_CLOSE"

instance ToByteString WorkflowExecutionTimeoutType where
    toBS WorkflowExecutionTimeoutTypeStartToClose = "START_TO_CLOSE"

instance ToHeader WorkflowExecutionTimeoutType where
    toHeader k = toHeader k . toBS

instance ToQuery WorkflowExecutionTimeoutType where
    toQuery = toQuery . toBS

instance FromJSON WorkflowExecutionTimeoutType

instance ToJSON WorkflowExecutionTimeoutType

-- | Provides details of the CancelTimer decision. It is not set for other
-- decision types.
newtype CancelTimerDecisionAttributes = CancelTimerDecisionAttributes
    { _ctdaTimerId :: Text
      -- ^ The unique Id of the timer to cancel. This field is required.
    } deriving (Show, Generic)

-- | The unique Id of the timer to cancel. This field is required.
ctdaTimerId :: Lens' CancelTimerDecisionAttributes (Text)
ctdaTimerId = lens _ctdaTimerId (\s a -> s { _ctdaTimerId = a })
{-# INLINE ctdaTimerId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CancelTimerDecisionAttributes' data type to populate a request.
mkCancelTimerDecisionAttributes :: Text -- ^ 'ctdaTimerId'
                                -> CancelTimerDecisionAttributes
mkCancelTimerDecisionAttributes p1 = CancelTimerDecisionAttributes
    { _ctdaTimerId = p1
    }
{-# INLINE mkCancelTimerDecisionAttributes #-}

instance FromJSON CancelTimerDecisionAttributes

instance ToJSON CancelTimerDecisionAttributes

-- | Provides details of the CancelWorkflowExecution decision. It is not set for
-- other decision types.
newtype CancelWorkflowExecutionDecisionAttributes = CancelWorkflowExecutionDecisionAttributes
    { _cwedbDetails :: Maybe Text
      -- ^ Optional details of the cancellation.
    } deriving (Show, Generic)

-- | Optional details of the cancellation.
cwedbDetails :: Lens' CancelWorkflowExecutionDecisionAttributes (Maybe Text)
cwedbDetails = lens _cwedbDetails (\s a -> s { _cwedbDetails = a })
{-# INLINE cwedbDetails #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CancelWorkflowExecutionDecisionAttributes' data type to populate a request.
mkCancelWorkflowExecutionDecisionAttributes :: CancelWorkflowExecutionDecisionAttributes
mkCancelWorkflowExecutionDecisionAttributes = CancelWorkflowExecutionDecisionAttributes
    { _cwedbDetails = Nothing
    }
{-# INLINE mkCancelWorkflowExecutionDecisionAttributes #-}

instance FromJSON CancelWorkflowExecutionDecisionAttributes

instance ToJSON CancelWorkflowExecutionDecisionAttributes

-- | If specified, only workflow executions that match this close status are
-- counted. This filter has an affect only if executionStatus is specified as
-- CLOSED. closeStatusFilter, executionFilter, typeFilter and tagFilter are
-- mutually exclusive. You can specify at most one of these in a request.
newtype CloseStatusFilter = CloseStatusFilter
    { _csfStatus :: CloseStatus
      -- ^ The close status that must match the close status of an execution
      -- for it to meet the criteria of this filter. This field is
      -- required.
    } deriving (Show, Generic)

-- | The close status that must match the close status of an execution for it to
-- meet the criteria of this filter. This field is required.
csfStatus :: Lens' CloseStatusFilter (CloseStatus)
csfStatus = lens _csfStatus (\s a -> s { _csfStatus = a })
{-# INLINE csfStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CloseStatusFilter' data type to populate a request.
mkCloseStatusFilter :: CloseStatus -- ^ 'csfStatus'
                    -> CloseStatusFilter
mkCloseStatusFilter p1 = CloseStatusFilter
    { _csfStatus = p1
    }
{-# INLINE mkCloseStatusFilter #-}

instance ToJSON CloseStatusFilter

-- | Provides details of the CompleteWorkflowExecution decision. It is not set
-- for other decision types.
newtype CompleteWorkflowExecutionDecisionAttributes = CompleteWorkflowExecutionDecisionAttributes
    { _cwedaResult :: Maybe Text
      -- ^ The result of the workflow execution. The form of the result is
      -- implementation defined.
    } deriving (Show, Generic)

-- | The result of the workflow execution. The form of the result is
-- implementation defined.
cwedaResult :: Lens' CompleteWorkflowExecutionDecisionAttributes (Maybe Text)
cwedaResult = lens _cwedaResult (\s a -> s { _cwedaResult = a })
{-# INLINE cwedaResult #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CompleteWorkflowExecutionDecisionAttributes' data type to populate a request.
mkCompleteWorkflowExecutionDecisionAttributes :: CompleteWorkflowExecutionDecisionAttributes
mkCompleteWorkflowExecutionDecisionAttributes = CompleteWorkflowExecutionDecisionAttributes
    { _cwedaResult = Nothing
    }
{-# INLINE mkCompleteWorkflowExecutionDecisionAttributes #-}

instance FromJSON CompleteWorkflowExecutionDecisionAttributes

instance ToJSON CompleteWorkflowExecutionDecisionAttributes

-- | Contains the configuration settings of a domain.
newtype DomainConfiguration = DomainConfiguration
    { _dcWorkflowExecutionRetentionPeriodInDays :: Text
      -- ^ The retention period for workflow executions in this domain.
    } deriving (Show, Generic)

-- | The retention period for workflow executions in this domain.
dcWorkflowExecutionRetentionPeriodInDays :: Lens' DomainConfiguration (Text)
dcWorkflowExecutionRetentionPeriodInDays = lens _dcWorkflowExecutionRetentionPeriodInDays (\s a -> s { _dcWorkflowExecutionRetentionPeriodInDays = a })
{-# INLINE dcWorkflowExecutionRetentionPeriodInDays #-}

instance FromJSON DomainConfiguration

-- | Provides details of the RequestCancelActivityTask decision. It is not set
-- for other decision types.
newtype RequestCancelActivityTaskDecisionAttributes = RequestCancelActivityTaskDecisionAttributes
    { _rcatdaActivityId :: Text
      -- ^ The activityId of the activity task to be canceled.
    } deriving (Show, Generic)

-- | The activityId of the activity task to be canceled.
rcatdaActivityId :: Lens' RequestCancelActivityTaskDecisionAttributes (Text)
rcatdaActivityId = lens _rcatdaActivityId (\s a -> s { _rcatdaActivityId = a })
{-# INLINE rcatdaActivityId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RequestCancelActivityTaskDecisionAttributes' data type to populate a request.
mkRequestCancelActivityTaskDecisionAttributes :: Text -- ^ 'rcatdaActivityId'
                                              -> RequestCancelActivityTaskDecisionAttributes
mkRequestCancelActivityTaskDecisionAttributes p1 = RequestCancelActivityTaskDecisionAttributes
    { _rcatdaActivityId = p1
    }
{-# INLINE mkRequestCancelActivityTaskDecisionAttributes #-}

instance FromJSON RequestCancelActivityTaskDecisionAttributes

instance ToJSON RequestCancelActivityTaskDecisionAttributes

-- | If specified, only executions that have a tag that matches the filter are
-- counted. closeStatusFilter, executionFilter, typeFilter and tagFilter are
-- mutually exclusive. You can specify at most one of these in a request.
newtype TagFilter = TagFilter
    { _tfTag :: Text
      -- ^ Specifies the tag that must be associated with the execution for
      -- it to meet the filter criteria. This field is required.
    } deriving (Show, Generic)

-- | Specifies the tag that must be associated with the execution for it to meet
-- the filter criteria. This field is required.
tfTag :: Lens' TagFilter (Text)
tfTag = lens _tfTag (\s a -> s { _tfTag = a })
{-# INLINE tfTag #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TagFilter' data type to populate a request.
mkTagFilter :: Text -- ^ 'tfTag'
            -> TagFilter
mkTagFilter p1 = TagFilter
    { _tfTag = p1
    }
{-# INLINE mkTagFilter #-}

instance ToJSON TagFilter

-- | The name of the task list.
newtype TaskList = TaskList
    { _tlName :: Text
      -- ^ The name of the task list.
    } deriving (Show, Generic)

-- | The name of the task list.
tlName :: Lens' TaskList (Text)
tlName = lens _tlName (\s a -> s { _tlName = a })
{-# INLINE tlName #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TaskList' data type to populate a request.
mkTaskList :: Text -- ^ 'tlName'
           -> TaskList
mkTaskList p1 = TaskList
    { _tlName = p1
    }
{-# INLINE mkTaskList #-}

instance FromJSON TaskList

instance ToJSON TaskList

-- | If specified, only workflow executions matching the WorkflowId in the
-- filter are counted. closeStatusFilter, executionFilter, typeFilter and
-- tagFilter are mutually exclusive. You can specify at most one of these in a
-- request.
newtype WorkflowExecutionFilter = WorkflowExecutionFilter
    { _wefWorkflowId :: Text
      -- ^ The workflowId to pass of match the criteria of this filter.
    } deriving (Show, Generic)

-- | The workflowId to pass of match the criteria of this filter.
wefWorkflowId :: Lens' WorkflowExecutionFilter (Text)
wefWorkflowId = lens _wefWorkflowId (\s a -> s { _wefWorkflowId = a })
{-# INLINE wefWorkflowId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WorkflowExecutionFilter' data type to populate a request.
mkWorkflowExecutionFilter :: Text -- ^ 'wefWorkflowId'
                          -> WorkflowExecutionFilter
mkWorkflowExecutionFilter p1 = WorkflowExecutionFilter
    { _wefWorkflowId = p1
    }
{-# INLINE mkWorkflowExecutionFilter #-}

instance ToJSON WorkflowExecutionFilter

-- | If the event is of type ActivityTaskcancelRequested then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskCancelRequestedEventAttributes = ActivityTaskCancelRequestedEventAttributes
    { _atcreaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the RequestCancelActivityTask
      -- decision for this cancellation request. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    , _atcreaActivityId :: Text
      -- ^ The unique ID of the task.
    } deriving (Show, Generic)

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RequestCancelActivityTask decision for this
-- cancellation request. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
atcreaDecisionTaskCompletedEventId :: Lens' ActivityTaskCancelRequestedEventAttributes (Integer)
atcreaDecisionTaskCompletedEventId = lens _atcreaDecisionTaskCompletedEventId (\s a -> s { _atcreaDecisionTaskCompletedEventId = a })
{-# INLINE atcreaDecisionTaskCompletedEventId #-}

-- | The unique ID of the task.
atcreaActivityId :: Lens' ActivityTaskCancelRequestedEventAttributes (Text)
atcreaActivityId = lens _atcreaActivityId (\s a -> s { _atcreaActivityId = a })
{-# INLINE atcreaActivityId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ActivityTaskCancelRequestedEventAttributes' data type to populate a request.
mkActivityTaskCancelRequestedEventAttributes :: Integer -- ^ 'atcreaDecisionTaskCompletedEventId'
                                             -> Text -- ^ 'atcreaActivityId'
                                             -> ActivityTaskCancelRequestedEventAttributes
mkActivityTaskCancelRequestedEventAttributes p1 p2 = ActivityTaskCancelRequestedEventAttributes
    { _atcreaDecisionTaskCompletedEventId = p1
    , _atcreaActivityId = p2
    }
{-# INLINE mkActivityTaskCancelRequestedEventAttributes #-}

instance FromJSON ActivityTaskCancelRequestedEventAttributes

instance ToJSON ActivityTaskCancelRequestedEventAttributes

-- | If the event is of type ActivityTaskCanceled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskCanceledEventAttributes = ActivityTaskCanceledEventAttributes
    { _atcebDetails :: Maybe Text
      -- ^ Details of the cancellation (if any).
    , _atcebScheduledEventId :: Integer
      -- ^ The id of the ActivityTaskScheduled event that was recorded when
      -- this activity task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    , _atcebStartedEventId :: Integer
      -- ^ The Id of the ActivityTaskStarted event recorded when this
      -- activity task was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    , _atcebLatestCancelRequestedEventId :: Maybe Integer
      -- ^ If set, contains the Id of the last ActivityTaskCancelRequested
      -- event recorded for this activity task. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    } deriving (Show, Generic)

-- | Details of the cancellation (if any).
atcebDetails :: Lens' ActivityTaskCanceledEventAttributes (Maybe Text)
atcebDetails = lens _atcebDetails (\s a -> s { _atcebDetails = a })
{-# INLINE atcebDetails #-}

-- | The id of the ActivityTaskScheduled event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atcebScheduledEventId :: Lens' ActivityTaskCanceledEventAttributes (Integer)
atcebScheduledEventId = lens _atcebScheduledEventId (\s a -> s { _atcebScheduledEventId = a })
{-# INLINE atcebScheduledEventId #-}

-- | The Id of the ActivityTaskStarted event recorded when this activity task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
atcebStartedEventId :: Lens' ActivityTaskCanceledEventAttributes (Integer)
atcebStartedEventId = lens _atcebStartedEventId (\s a -> s { _atcebStartedEventId = a })
{-# INLINE atcebStartedEventId #-}

-- | If set, contains the Id of the last ActivityTaskCancelRequested event
-- recorded for this activity task. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
atcebLatestCancelRequestedEventId :: Lens' ActivityTaskCanceledEventAttributes (Maybe Integer)
atcebLatestCancelRequestedEventId = lens _atcebLatestCancelRequestedEventId (\s a -> s { _atcebLatestCancelRequestedEventId = a })
{-# INLINE atcebLatestCancelRequestedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ActivityTaskCanceledEventAttributes' data type to populate a request.
mkActivityTaskCanceledEventAttributes :: Integer -- ^ 'atcebScheduledEventId'
                                      -> Integer -- ^ 'atcebStartedEventId'
                                      -> ActivityTaskCanceledEventAttributes
mkActivityTaskCanceledEventAttributes p1 p2 = ActivityTaskCanceledEventAttributes
    { _atcebDetails = Nothing
    , _atcebScheduledEventId = p2
    , _atcebStartedEventId = p3
    , _atcebLatestCancelRequestedEventId = Nothing
    }
{-# INLINE mkActivityTaskCanceledEventAttributes #-}

instance FromJSON ActivityTaskCanceledEventAttributes

instance ToJSON ActivityTaskCanceledEventAttributes

-- | If the event is of type ActivityTaskCompleted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskCompletedEventAttributes = ActivityTaskCompletedEventAttributes
    { _atceaResult :: Maybe Text
      -- ^ The results of the activity task (if any).
    , _atceaScheduledEventId :: Integer
      -- ^ The id of the ActivityTaskScheduled event that was recorded when
      -- this activity task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    , _atceaStartedEventId :: Integer
      -- ^ The Id of the ActivityTaskStarted event recorded when this
      -- activity task was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    } deriving (Show, Generic)

-- | The results of the activity task (if any).
atceaResult :: Lens' ActivityTaskCompletedEventAttributes (Maybe Text)
atceaResult = lens _atceaResult (\s a -> s { _atceaResult = a })
{-# INLINE atceaResult #-}

-- | The id of the ActivityTaskScheduled event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atceaScheduledEventId :: Lens' ActivityTaskCompletedEventAttributes (Integer)
atceaScheduledEventId = lens _atceaScheduledEventId (\s a -> s { _atceaScheduledEventId = a })
{-# INLINE atceaScheduledEventId #-}

-- | The Id of the ActivityTaskStarted event recorded when this activity task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
atceaStartedEventId :: Lens' ActivityTaskCompletedEventAttributes (Integer)
atceaStartedEventId = lens _atceaStartedEventId (\s a -> s { _atceaStartedEventId = a })
{-# INLINE atceaStartedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ActivityTaskCompletedEventAttributes' data type to populate a request.
mkActivityTaskCompletedEventAttributes :: Integer -- ^ 'atceaScheduledEventId'
                                       -> Integer -- ^ 'atceaStartedEventId'
                                       -> ActivityTaskCompletedEventAttributes
mkActivityTaskCompletedEventAttributes p1 p2 = ActivityTaskCompletedEventAttributes
    { _atceaResult = Nothing
    , _atceaScheduledEventId = p2
    , _atceaStartedEventId = p3
    }
{-# INLINE mkActivityTaskCompletedEventAttributes #-}

instance FromJSON ActivityTaskCompletedEventAttributes

instance ToJSON ActivityTaskCompletedEventAttributes

-- | If the event is of type ActivityTaskFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskFailedEventAttributes = ActivityTaskFailedEventAttributes
    { _atfeaReason :: Maybe Text
      -- ^ The reason provided for the failure (if any).
    , _atfeaDetails :: Maybe Text
      -- ^ The details of the failure (if any).
    , _atfeaScheduledEventId :: Integer
      -- ^ The id of the ActivityTaskScheduled event that was recorded when
      -- this activity task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    , _atfeaStartedEventId :: Integer
      -- ^ The Id of the ActivityTaskStarted event recorded when this
      -- activity task was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    } deriving (Show, Generic)

-- | The reason provided for the failure (if any).
atfeaReason :: Lens' ActivityTaskFailedEventAttributes (Maybe Text)
atfeaReason = lens _atfeaReason (\s a -> s { _atfeaReason = a })
{-# INLINE atfeaReason #-}

-- | The details of the failure (if any).
atfeaDetails :: Lens' ActivityTaskFailedEventAttributes (Maybe Text)
atfeaDetails = lens _atfeaDetails (\s a -> s { _atfeaDetails = a })
{-# INLINE atfeaDetails #-}

-- | The id of the ActivityTaskScheduled event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atfeaScheduledEventId :: Lens' ActivityTaskFailedEventAttributes (Integer)
atfeaScheduledEventId = lens _atfeaScheduledEventId (\s a -> s { _atfeaScheduledEventId = a })
{-# INLINE atfeaScheduledEventId #-}

-- | The Id of the ActivityTaskStarted event recorded when this activity task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
atfeaStartedEventId :: Lens' ActivityTaskFailedEventAttributes (Integer)
atfeaStartedEventId = lens _atfeaStartedEventId (\s a -> s { _atfeaStartedEventId = a })
{-# INLINE atfeaStartedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ActivityTaskFailedEventAttributes' data type to populate a request.
mkActivityTaskFailedEventAttributes :: Integer -- ^ 'atfeaScheduledEventId'
                                    -> Integer -- ^ 'atfeaStartedEventId'
                                    -> ActivityTaskFailedEventAttributes
mkActivityTaskFailedEventAttributes p1 p2 = ActivityTaskFailedEventAttributes
    { _atfeaReason = Nothing
    , _atfeaDetails = Nothing
    , _atfeaScheduledEventId = p3
    , _atfeaStartedEventId = p4
    }
{-# INLINE mkActivityTaskFailedEventAttributes #-}

instance FromJSON ActivityTaskFailedEventAttributes

instance ToJSON ActivityTaskFailedEventAttributes

-- | If the event is of type ActivityTaskScheduled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskScheduledEventAttributes = ActivityTaskScheduledEventAttributes
    { _atseaActivityType :: ActivityType
      -- ^ The type of the activity task.
    , _atseaActivityId :: Text
      -- ^ The unique id of the activity task.
    , _atseaInput :: Maybe Text
      -- ^ The input provided to the activity task.
    , _atseaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent workflow tasks. This data is not sent to
      -- the activity.
    , _atseaScheduleToStartTimeout :: Maybe Text
      -- ^ The maximum amount of time the activity task can wait to be
      -- assigned to a worker.
    , _atseaScheduleToCloseTimeout :: Maybe Text
      -- ^ The maximum amount of time for this activity task.
    , _atseaStartToCloseTimeout :: Maybe Text
      -- ^ The maximum amount of time a worker may take to process the
      -- activity task.
    , _atseaTaskList :: TaskList
      -- ^ The task list in which the activity task has been scheduled.
    , _atseaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision that resulted in the scheduling of this activity task.
      -- This information can be useful for diagnosing problems by tracing
      -- back the chain of events leading up to this event.
    , _atseaHeartbeatTimeout :: Maybe Text
      -- ^ The maximum time before which the worker processing this task
      -- must report progress by calling RecordActivityTaskHeartbeat. If
      -- the timeout is exceeded, the activity task is automatically timed
      -- out. If the worker subsequently attempts to record a heartbeat or
      -- return a result, it will be ignored.
    } deriving (Show, Generic)

-- | The type of the activity task.
atseaActivityType :: Lens' ActivityTaskScheduledEventAttributes (ActivityType)
atseaActivityType = lens _atseaActivityType (\s a -> s { _atseaActivityType = a })
{-# INLINE atseaActivityType #-}

-- | The unique id of the activity task.
atseaActivityId :: Lens' ActivityTaskScheduledEventAttributes (Text)
atseaActivityId = lens _atseaActivityId (\s a -> s { _atseaActivityId = a })
{-# INLINE atseaActivityId #-}

-- | The input provided to the activity task.
atseaInput :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaInput = lens _atseaInput (\s a -> s { _atseaInput = a })
{-# INLINE atseaInput #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks. This data is not sent to the activity.
atseaControl :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaControl = lens _atseaControl (\s a -> s { _atseaControl = a })
{-# INLINE atseaControl #-}

-- | The maximum amount of time the activity task can wait to be assigned to a
-- worker.
atseaScheduleToStartTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaScheduleToStartTimeout = lens _atseaScheduleToStartTimeout (\s a -> s { _atseaScheduleToStartTimeout = a })
{-# INLINE atseaScheduleToStartTimeout #-}

-- | The maximum amount of time for this activity task.
atseaScheduleToCloseTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaScheduleToCloseTimeout = lens _atseaScheduleToCloseTimeout (\s a -> s { _atseaScheduleToCloseTimeout = a })
{-# INLINE atseaScheduleToCloseTimeout #-}

-- | The maximum amount of time a worker may take to process the activity task.
atseaStartToCloseTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaStartToCloseTimeout = lens _atseaStartToCloseTimeout (\s a -> s { _atseaStartToCloseTimeout = a })
{-# INLINE atseaStartToCloseTimeout #-}

-- | The task list in which the activity task has been scheduled.
atseaTaskList :: Lens' ActivityTaskScheduledEventAttributes (TaskList)
atseaTaskList = lens _atseaTaskList (\s a -> s { _atseaTaskList = a })
{-# INLINE atseaTaskList #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- that resulted in the scheduling of this activity task. This information can
-- be useful for diagnosing problems by tracing back the chain of events
-- leading up to this event.
atseaDecisionTaskCompletedEventId :: Lens' ActivityTaskScheduledEventAttributes (Integer)
atseaDecisionTaskCompletedEventId = lens _atseaDecisionTaskCompletedEventId (\s a -> s { _atseaDecisionTaskCompletedEventId = a })
{-# INLINE atseaDecisionTaskCompletedEventId #-}

-- | The maximum time before which the worker processing this task must report
-- progress by calling RecordActivityTaskHeartbeat. If the timeout is
-- exceeded, the activity task is automatically timed out. If the worker
-- subsequently attempts to record a heartbeat or return a result, it will be
-- ignored.
atseaHeartbeatTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaHeartbeatTimeout = lens _atseaHeartbeatTimeout (\s a -> s { _atseaHeartbeatTimeout = a })
{-# INLINE atseaHeartbeatTimeout #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ActivityTaskScheduledEventAttributes' data type to populate a request.
mkActivityTaskScheduledEventAttributes :: ActivityType -- ^ 'atseaActivityType'
                                       -> Text -- ^ 'atseaActivityId'
                                       -> TaskList -- ^ 'atseaTaskList'
                                       -> Integer -- ^ 'atseaDecisionTaskCompletedEventId'
                                       -> ActivityTaskScheduledEventAttributes
mkActivityTaskScheduledEventAttributes p1 p2 p3 p4 = ActivityTaskScheduledEventAttributes
    { _atseaActivityType = p1
    , _atseaActivityId = p2
    , _atseaInput = Nothing
    , _atseaControl = Nothing
    , _atseaScheduleToStartTimeout = Nothing
    , _atseaScheduleToCloseTimeout = Nothing
    , _atseaStartToCloseTimeout = Nothing
    , _atseaTaskList = p8
    , _atseaDecisionTaskCompletedEventId = p9
    , _atseaHeartbeatTimeout = Nothing
    }
{-# INLINE mkActivityTaskScheduledEventAttributes #-}

instance FromJSON ActivityTaskScheduledEventAttributes

instance ToJSON ActivityTaskScheduledEventAttributes

-- | If the event is of type ActivityTaskStarted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskStartedEventAttributes = ActivityTaskStartedEventAttributes
    { _atsebIdentity :: Maybe Text
      -- ^ Identity of the worker that was assigned this task. This aids
      -- diagnostics when problems arise. The form of this identity is
      -- user defined.
    , _atsebScheduledEventId :: Integer
      -- ^ The id of the ActivityTaskScheduled event that was recorded when
      -- this activity task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    } deriving (Show, Generic)

-- | Identity of the worker that was assigned this task. This aids diagnostics
-- when problems arise. The form of this identity is user defined.
atsebIdentity :: Lens' ActivityTaskStartedEventAttributes (Maybe Text)
atsebIdentity = lens _atsebIdentity (\s a -> s { _atsebIdentity = a })
{-# INLINE atsebIdentity #-}

-- | The id of the ActivityTaskScheduled event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atsebScheduledEventId :: Lens' ActivityTaskStartedEventAttributes (Integer)
atsebScheduledEventId = lens _atsebScheduledEventId (\s a -> s { _atsebScheduledEventId = a })
{-# INLINE atsebScheduledEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ActivityTaskStartedEventAttributes' data type to populate a request.
mkActivityTaskStartedEventAttributes :: Integer -- ^ 'atsebScheduledEventId'
                                     -> ActivityTaskStartedEventAttributes
mkActivityTaskStartedEventAttributes p1 = ActivityTaskStartedEventAttributes
    { _atsebIdentity = Nothing
    , _atsebScheduledEventId = p2
    }
{-# INLINE mkActivityTaskStartedEventAttributes #-}

instance FromJSON ActivityTaskStartedEventAttributes

instance ToJSON ActivityTaskStartedEventAttributes

-- | If the event is of type ActivityTaskTimedOut then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data ActivityTaskTimedOutEventAttributes = ActivityTaskTimedOutEventAttributes
    { _attoeaTimeoutType :: ActivityTaskTimeoutType
      -- ^ The type of the timeout that caused this event.
    , _attoeaScheduledEventId :: Integer
      -- ^ The id of the ActivityTaskScheduled event that was recorded when
      -- this activity task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    , _attoeaStartedEventId :: Integer
      -- ^ The Id of the ActivityTaskStarted event recorded when this
      -- activity task was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    , _attoeaDetails :: Maybe Text
      -- ^ Contains the content of the details parameter for the last call
      -- made by the activity to RecordActivityTaskHeartbeat.
    } deriving (Show, Generic)

-- | The type of the timeout that caused this event.
attoeaTimeoutType :: Lens' ActivityTaskTimedOutEventAttributes (ActivityTaskTimeoutType)
attoeaTimeoutType = lens _attoeaTimeoutType (\s a -> s { _attoeaTimeoutType = a })
{-# INLINE attoeaTimeoutType #-}

-- | The id of the ActivityTaskScheduled event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
attoeaScheduledEventId :: Lens' ActivityTaskTimedOutEventAttributes (Integer)
attoeaScheduledEventId = lens _attoeaScheduledEventId (\s a -> s { _attoeaScheduledEventId = a })
{-# INLINE attoeaScheduledEventId #-}

-- | The Id of the ActivityTaskStarted event recorded when this activity task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
attoeaStartedEventId :: Lens' ActivityTaskTimedOutEventAttributes (Integer)
attoeaStartedEventId = lens _attoeaStartedEventId (\s a -> s { _attoeaStartedEventId = a })
{-# INLINE attoeaStartedEventId #-}

-- | Contains the content of the details parameter for the last call made by the
-- activity to RecordActivityTaskHeartbeat.
attoeaDetails :: Lens' ActivityTaskTimedOutEventAttributes (Maybe Text)
attoeaDetails = lens _attoeaDetails (\s a -> s { _attoeaDetails = a })
{-# INLINE attoeaDetails #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ActivityTaskTimedOutEventAttributes' data type to populate a request.
mkActivityTaskTimedOutEventAttributes :: ActivityTaskTimeoutType -- ^ 'attoeaTimeoutType'
                                      -> Integer -- ^ 'attoeaScheduledEventId'
                                      -> Integer -- ^ 'attoeaStartedEventId'
                                      -> ActivityTaskTimedOutEventAttributes
mkActivityTaskTimedOutEventAttributes p1 p2 p3 = ActivityTaskTimedOutEventAttributes
    { _attoeaTimeoutType = p1
    , _attoeaScheduledEventId = p2
    , _attoeaStartedEventId = p3
    , _attoeaDetails = Nothing
    }
{-# INLINE mkActivityTaskTimedOutEventAttributes #-}

instance FromJSON ActivityTaskTimedOutEventAttributes

instance ToJSON ActivityTaskTimedOutEventAttributes

-- | The activity type to deprecate.
data ActivityType = ActivityType
    { _atName :: Text
      -- ^ The name of this activity. The combination of activity type name
      -- and version must be unique within a domain.
    , _atVersion :: Text
      -- ^ The version of this activity. The combination of activity type
      -- name and version must be unique with in a domain.
    } deriving (Show, Generic)

-- | The name of this activity. The combination of activity type name and
-- version must be unique within a domain.
atName :: Lens' ActivityType (Text)
atName = lens _atName (\s a -> s { _atName = a })
{-# INLINE atName #-}

-- | The version of this activity. The combination of activity type name and
-- version must be unique with in a domain.
atVersion :: Lens' ActivityType (Text)
atVersion = lens _atVersion (\s a -> s { _atVersion = a })
{-# INLINE atVersion #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ActivityType' data type to populate a request.
mkActivityType :: Text -- ^ 'atName'
               -> Text -- ^ 'atVersion'
               -> ActivityType
mkActivityType p1 p2 = ActivityType
    { _atName = p1
    , _atVersion = p2
    }
{-# INLINE mkActivityType #-}

instance FromJSON ActivityType

instance ToJSON ActivityType

-- | The configuration settings registered with the activity type.
data ActivityTypeConfiguration = ActivityTypeConfiguration
    { _atcDefaultTaskStartToCloseTimeout :: Maybe Text
      -- ^ The optional default maximum duration for tasks of an activity
      -- type specified when registering the activity type. You can
      -- override this default when scheduling a task through the
      -- ScheduleActivityTask Decision. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify
      -- unlimited duration.
    , _atcDefaultTaskHeartbeatTimeout :: Maybe Text
      -- ^ The optional default maximum time, specified when registering the
      -- activity type, before which a worker processing a task must
      -- report progress by calling RecordActivityTaskHeartbeat. You can
      -- override this default when scheduling a task through the
      -- ScheduleActivityTask Decision. If the activity worker
      -- subsequently attempts to record a heartbeat or returns a result,
      -- the activity worker receives an UnknownResource fault. In this
      -- case, Amazon SWF no longer considers the activity task to be
      -- valid; the activity worker should clean up the activity task. The
      -- valid values are integers greater than or equal to 0. An integer
      -- value can be used to specify the duration in seconds while NONE
      -- can be used to specify unlimited duration.
    , _atcDefaultTaskList :: Maybe TaskList
      -- ^ The optional default task list specified for this activity type
      -- at registration. This default task list is used if a task list is
      -- not provided when a task is scheduled through the
      -- ScheduleActivityTask Decision. You can override this default when
      -- scheduling a task through the ScheduleActivityTask Decision.
    , _atcDefaultTaskScheduleToStartTimeout :: Maybe Text
      -- ^ The optional default maximum duration, specified when registering
      -- the activity type, that a task of an activity type can wait
      -- before being assigned to a worker. You can override this default
      -- when scheduling a task through the ScheduleActivityTask Decision.
      -- The valid values are integers greater than or equal to 0. An
      -- integer value can be used to specify the duration in seconds
      -- while NONE can be used to specify unlimited duration.
    , _atcDefaultTaskScheduleToCloseTimeout :: Maybe Text
      -- ^ The optional default maximum duration, specified when registering
      -- the activity type, for tasks of this activity type. You can
      -- override this default when scheduling a task through the
      -- ScheduleActivityTask Decision. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify
      -- unlimited duration.
    } deriving (Show, Generic)

-- | The optional default maximum duration for tasks of an activity type
-- specified when registering the activity type. You can override this default
-- when scheduling a task through the ScheduleActivityTask Decision. The valid
-- values are integers greater than or equal to 0. An integer value can be
-- used to specify the duration in seconds while NONE can be used to specify
-- unlimited duration.
atcDefaultTaskStartToCloseTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskStartToCloseTimeout = lens _atcDefaultTaskStartToCloseTimeout (\s a -> s { _atcDefaultTaskStartToCloseTimeout = a })
{-# INLINE atcDefaultTaskStartToCloseTimeout #-}

-- | The optional default maximum time, specified when registering the activity
-- type, before which a worker processing a task must report progress by
-- calling RecordActivityTaskHeartbeat. You can override this default when
-- scheduling a task through the ScheduleActivityTask Decision. If the
-- activity worker subsequently attempts to record a heartbeat or returns a
-- result, the activity worker receives an UnknownResource fault. In this
-- case, Amazon SWF no longer considers the activity task to be valid; the
-- activity worker should clean up the activity task. The valid values are
-- integers greater than or equal to 0. An integer value can be used to
-- specify the duration in seconds while NONE can be used to specify unlimited
-- duration.
atcDefaultTaskHeartbeatTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskHeartbeatTimeout = lens _atcDefaultTaskHeartbeatTimeout (\s a -> s { _atcDefaultTaskHeartbeatTimeout = a })
{-# INLINE atcDefaultTaskHeartbeatTimeout #-}

-- | The optional default task list specified for this activity type at
-- registration. This default task list is used if a task list is not provided
-- when a task is scheduled through the ScheduleActivityTask Decision. You can
-- override this default when scheduling a task through the
-- ScheduleActivityTask Decision.
atcDefaultTaskList :: Lens' ActivityTypeConfiguration (Maybe TaskList)
atcDefaultTaskList = lens _atcDefaultTaskList (\s a -> s { _atcDefaultTaskList = a })
{-# INLINE atcDefaultTaskList #-}

-- | The optional default maximum duration, specified when registering the
-- activity type, that a task of an activity type can wait before being
-- assigned to a worker. You can override this default when scheduling a task
-- through the ScheduleActivityTask Decision. The valid values are integers
-- greater than or equal to 0. An integer value can be used to specify the
-- duration in seconds while NONE can be used to specify unlimited duration.
atcDefaultTaskScheduleToStartTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskScheduleToStartTimeout = lens _atcDefaultTaskScheduleToStartTimeout (\s a -> s { _atcDefaultTaskScheduleToStartTimeout = a })
{-# INLINE atcDefaultTaskScheduleToStartTimeout #-}

-- | The optional default maximum duration, specified when registering the
-- activity type, for tasks of this activity type. You can override this
-- default when scheduling a task through the ScheduleActivityTask Decision.
-- The valid values are integers greater than or equal to 0. An integer value
-- can be used to specify the duration in seconds while NONE can be used to
-- specify unlimited duration.
atcDefaultTaskScheduleToCloseTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskScheduleToCloseTimeout = lens _atcDefaultTaskScheduleToCloseTimeout (\s a -> s { _atcDefaultTaskScheduleToCloseTimeout = a })
{-# INLINE atcDefaultTaskScheduleToCloseTimeout #-}

instance FromJSON ActivityTypeConfiguration

-- | General information about the activity type. The status of activity type
-- (returned in the ActivityTypeInfo structure) can be one of the following.
-- REGISTERED: The type is registered and available. Workers supporting this
-- type should be running. DEPRECATED: The type was deprecated using
-- DeprecateActivityType, but is still in use. You should keep workers
-- supporting this type running. You cannot create new tasks of this type.
data ActivityTypeInfo = ActivityTypeInfo
    { _atiActivityType :: ActivityType
      -- ^ The ActivityType type structure representing the activity type.
    , _atiStatus :: RegistrationStatus
      -- ^ The current status of the activity type.
    , _atiDescription :: Maybe Text
      -- ^ The description of the activity type provided in
      -- RegisterActivityType.
    , _atiCreationDate :: POSIX
      -- ^ The date and time this activity type was created through
      -- RegisterActivityType.
    , _atiDeprecationDate :: Maybe POSIX
      -- ^ If DEPRECATED, the date and time DeprecateActivityType was
      -- called.
    } deriving (Show, Generic)

-- | The ActivityType type structure representing the activity type.
atiActivityType :: Lens' ActivityTypeInfo (ActivityType)
atiActivityType = lens _atiActivityType (\s a -> s { _atiActivityType = a })
{-# INLINE atiActivityType #-}

-- | The current status of the activity type.
atiStatus :: Lens' ActivityTypeInfo (RegistrationStatus)
atiStatus = lens _atiStatus (\s a -> s { _atiStatus = a })
{-# INLINE atiStatus #-}

-- | The description of the activity type provided in RegisterActivityType.
atiDescription :: Lens' ActivityTypeInfo (Maybe Text)
atiDescription = lens _atiDescription (\s a -> s { _atiDescription = a })
{-# INLINE atiDescription #-}

-- | The date and time this activity type was created through
-- RegisterActivityType.
atiCreationDate :: Lens' ActivityTypeInfo (POSIX)
atiCreationDate = lens _atiCreationDate (\s a -> s { _atiCreationDate = a })
{-# INLINE atiCreationDate #-}

-- | If DEPRECATED, the date and time DeprecateActivityType was called.
atiDeprecationDate :: Lens' ActivityTypeInfo (Maybe POSIX)
atiDeprecationDate = lens _atiDeprecationDate (\s a -> s { _atiDeprecationDate = a })
{-# INLINE atiDeprecationDate #-}

instance FromJSON ActivityTypeInfo

-- | If the event is of type CancelTimerFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data CancelTimerFailedEventAttributes = CancelTimerFailedEventAttributes
    { _ctfeaTimerId :: Text
      -- ^ The timerId provided in the CancelTimer decision that failed.
    , _ctfeaCause :: CancelTimerFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _ctfeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the CancelTimer decision to cancel
      -- this timer. This information can be useful for diagnosing
      -- problems by tracing back the cause of events.
    } deriving (Show, Generic)

-- | The timerId provided in the CancelTimer decision that failed.
ctfeaTimerId :: Lens' CancelTimerFailedEventAttributes (Text)
ctfeaTimerId = lens _ctfeaTimerId (\s a -> s { _ctfeaTimerId = a })
{-# INLINE ctfeaTimerId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
ctfeaCause :: Lens' CancelTimerFailedEventAttributes (CancelTimerFailedCause)
ctfeaCause = lens _ctfeaCause (\s a -> s { _ctfeaCause = a })
{-# INLINE ctfeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CancelTimer decision to cancel this timer. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
ctfeaDecisionTaskCompletedEventId :: Lens' CancelTimerFailedEventAttributes (Integer)
ctfeaDecisionTaskCompletedEventId = lens _ctfeaDecisionTaskCompletedEventId (\s a -> s { _ctfeaDecisionTaskCompletedEventId = a })
{-# INLINE ctfeaDecisionTaskCompletedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CancelTimerFailedEventAttributes' data type to populate a request.
mkCancelTimerFailedEventAttributes :: Text -- ^ 'ctfeaTimerId'
                                   -> CancelTimerFailedCause -- ^ 'ctfeaCause'
                                   -> Integer -- ^ 'ctfeaDecisionTaskCompletedEventId'
                                   -> CancelTimerFailedEventAttributes
mkCancelTimerFailedEventAttributes p1 p2 p3 = CancelTimerFailedEventAttributes
    { _ctfeaTimerId = p1
    , _ctfeaCause = p2
    , _ctfeaDecisionTaskCompletedEventId = p3
    }
{-# INLINE mkCancelTimerFailedEventAttributes #-}

instance FromJSON CancelTimerFailedEventAttributes

instance ToJSON CancelTimerFailedEventAttributes

-- | If the event is of type CancelWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data CancelWorkflowExecutionFailedEventAttributes = CancelWorkflowExecutionFailedEventAttributes
    { _cwefebCause :: CancelWorkflowExecutionFailedCause
      -- ^ The cause of the failure. This information is generated by the
      -- system and can be useful for diagnostic purposes. If cause is set
      -- to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see
      -- Using IAM to Manage Access to Amazon SWF Workflows.
    , _cwefebDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the CancelWorkflowExecution
      -- decision for this cancellation request. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    } deriving (Show, Generic)

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.
cwefebCause :: Lens' CancelWorkflowExecutionFailedEventAttributes (CancelWorkflowExecutionFailedCause)
cwefebCause = lens _cwefebCause (\s a -> s { _cwefebCause = a })
{-# INLINE cwefebCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CancelWorkflowExecution decision for this
-- cancellation request. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
cwefebDecisionTaskCompletedEventId :: Lens' CancelWorkflowExecutionFailedEventAttributes (Integer)
cwefebDecisionTaskCompletedEventId = lens _cwefebDecisionTaskCompletedEventId (\s a -> s { _cwefebDecisionTaskCompletedEventId = a })
{-# INLINE cwefebDecisionTaskCompletedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CancelWorkflowExecutionFailedEventAttributes' data type to populate a request.
mkCancelWorkflowExecutionFailedEventAttributes :: CancelWorkflowExecutionFailedCause -- ^ 'cwefebCause'
                                               -> Integer -- ^ 'cwefebDecisionTaskCompletedEventId'
                                               -> CancelWorkflowExecutionFailedEventAttributes
mkCancelWorkflowExecutionFailedEventAttributes p1 p2 = CancelWorkflowExecutionFailedEventAttributes
    { _cwefebCause = p1
    , _cwefebDecisionTaskCompletedEventId = p2
    }
{-# INLINE mkCancelWorkflowExecutionFailedEventAttributes #-}

instance FromJSON CancelWorkflowExecutionFailedEventAttributes

instance ToJSON CancelWorkflowExecutionFailedEventAttributes

-- | If the event is of type ChildWorkflowExecutionCanceled then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionCanceledEventAttributes = ChildWorkflowExecutionCanceledEventAttributes
    { _cwecebWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that was canceled.
    , _cwecebWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    , _cwecebDetails :: Maybe Text
      -- ^ Details of the cancellation (if provided).
    , _cwecebInitiatedEventId :: Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this child workflow execution. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cwecebStartedEventId :: Integer
      -- ^ The Id of the ChildWorkflowExecutionStarted event recorded when
      -- this child workflow execution was started. This information can
      -- be useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    } deriving (Show, Generic)

-- | The child workflow execution that was canceled.
cwecebWorkflowExecution :: Lens' ChildWorkflowExecutionCanceledEventAttributes (WorkflowExecution)
cwecebWorkflowExecution = lens _cwecebWorkflowExecution (\s a -> s { _cwecebWorkflowExecution = a })
{-# INLINE cwecebWorkflowExecution #-}

-- | The type of the child workflow execution.
cwecebWorkflowType :: Lens' ChildWorkflowExecutionCanceledEventAttributes (WorkflowType)
cwecebWorkflowType = lens _cwecebWorkflowType (\s a -> s { _cwecebWorkflowType = a })
{-# INLINE cwecebWorkflowType #-}

-- | Details of the cancellation (if provided).
cwecebDetails :: Lens' ChildWorkflowExecutionCanceledEventAttributes (Maybe Text)
cwecebDetails = lens _cwecebDetails (\s a -> s { _cwecebDetails = a })
{-# INLINE cwecebDetails #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cwecebInitiatedEventId :: Lens' ChildWorkflowExecutionCanceledEventAttributes (Integer)
cwecebInitiatedEventId = lens _cwecebInitiatedEventId (\s a -> s { _cwecebInitiatedEventId = a })
{-# INLINE cwecebInitiatedEventId #-}

-- | The Id of the ChildWorkflowExecutionStarted event recorded when this child
-- workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
cwecebStartedEventId :: Lens' ChildWorkflowExecutionCanceledEventAttributes (Integer)
cwecebStartedEventId = lens _cwecebStartedEventId (\s a -> s { _cwecebStartedEventId = a })
{-# INLINE cwecebStartedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ChildWorkflowExecutionCanceledEventAttributes' data type to populate a request.
mkChildWorkflowExecutionCanceledEventAttributes :: WorkflowExecution -- ^ 'cwecebWorkflowExecution'
                                                -> WorkflowType -- ^ 'cwecebWorkflowType'
                                                -> Integer -- ^ 'cwecebInitiatedEventId'
                                                -> Integer -- ^ 'cwecebStartedEventId'
                                                -> ChildWorkflowExecutionCanceledEventAttributes
mkChildWorkflowExecutionCanceledEventAttributes p1 p2 p3 p4 = ChildWorkflowExecutionCanceledEventAttributes
    { _cwecebWorkflowExecution = p1
    , _cwecebWorkflowType = p2
    , _cwecebDetails = Nothing
    , _cwecebInitiatedEventId = p4
    , _cwecebStartedEventId = p5
    }
{-# INLINE mkChildWorkflowExecutionCanceledEventAttributes #-}

instance FromJSON ChildWorkflowExecutionCanceledEventAttributes

instance ToJSON ChildWorkflowExecutionCanceledEventAttributes

-- | If the event is of type ChildWorkflowExecutionCompleted then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionCompletedEventAttributes = ChildWorkflowExecutionCompletedEventAttributes
    { _cweceaWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that was completed.
    , _cweceaWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    , _cweceaResult :: Maybe Text
      -- ^ The result of the child workflow execution (if any).
    , _cweceaInitiatedEventId :: Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this child workflow execution. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cweceaStartedEventId :: Integer
      -- ^ The Id of the ChildWorkflowExecutionStarted event recorded when
      -- this child workflow execution was started. This information can
      -- be useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    } deriving (Show, Generic)

-- | The child workflow execution that was completed.
cweceaWorkflowExecution :: Lens' ChildWorkflowExecutionCompletedEventAttributes (WorkflowExecution)
cweceaWorkflowExecution = lens _cweceaWorkflowExecution (\s a -> s { _cweceaWorkflowExecution = a })
{-# INLINE cweceaWorkflowExecution #-}

-- | The type of the child workflow execution.
cweceaWorkflowType :: Lens' ChildWorkflowExecutionCompletedEventAttributes (WorkflowType)
cweceaWorkflowType = lens _cweceaWorkflowType (\s a -> s { _cweceaWorkflowType = a })
{-# INLINE cweceaWorkflowType #-}

-- | The result of the child workflow execution (if any).
cweceaResult :: Lens' ChildWorkflowExecutionCompletedEventAttributes (Maybe Text)
cweceaResult = lens _cweceaResult (\s a -> s { _cweceaResult = a })
{-# INLINE cweceaResult #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cweceaInitiatedEventId :: Lens' ChildWorkflowExecutionCompletedEventAttributes (Integer)
cweceaInitiatedEventId = lens _cweceaInitiatedEventId (\s a -> s { _cweceaInitiatedEventId = a })
{-# INLINE cweceaInitiatedEventId #-}

-- | The Id of the ChildWorkflowExecutionStarted event recorded when this child
-- workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
cweceaStartedEventId :: Lens' ChildWorkflowExecutionCompletedEventAttributes (Integer)
cweceaStartedEventId = lens _cweceaStartedEventId (\s a -> s { _cweceaStartedEventId = a })
{-# INLINE cweceaStartedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ChildWorkflowExecutionCompletedEventAttributes' data type to populate a request.
mkChildWorkflowExecutionCompletedEventAttributes :: WorkflowExecution -- ^ 'cweceaWorkflowExecution'
                                                 -> WorkflowType -- ^ 'cweceaWorkflowType'
                                                 -> Integer -- ^ 'cweceaInitiatedEventId'
                                                 -> Integer -- ^ 'cweceaStartedEventId'
                                                 -> ChildWorkflowExecutionCompletedEventAttributes
mkChildWorkflowExecutionCompletedEventAttributes p1 p2 p3 p4 = ChildWorkflowExecutionCompletedEventAttributes
    { _cweceaWorkflowExecution = p1
    , _cweceaWorkflowType = p2
    , _cweceaResult = Nothing
    , _cweceaInitiatedEventId = p4
    , _cweceaStartedEventId = p5
    }
{-# INLINE mkChildWorkflowExecutionCompletedEventAttributes #-}

instance FromJSON ChildWorkflowExecutionCompletedEventAttributes

instance ToJSON ChildWorkflowExecutionCompletedEventAttributes

-- | If the event is of type ChildWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionFailedEventAttributes = ChildWorkflowExecutionFailedEventAttributes
    { _cwefecWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that failed.
    , _cwefecWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    , _cwefecReason :: Maybe Text
      -- ^ The reason for the failure (if provided).
    , _cwefecDetails :: Maybe Text
      -- ^ The details of the failure (if provided).
    , _cwefecInitiatedEventId :: Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this child workflow execution. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cwefecStartedEventId :: Integer
      -- ^ The Id of the ChildWorkflowExecutionStarted event recorded when
      -- this child workflow execution was started. This information can
      -- be useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    } deriving (Show, Generic)

-- | The child workflow execution that failed.
cwefecWorkflowExecution :: Lens' ChildWorkflowExecutionFailedEventAttributes (WorkflowExecution)
cwefecWorkflowExecution = lens _cwefecWorkflowExecution (\s a -> s { _cwefecWorkflowExecution = a })
{-# INLINE cwefecWorkflowExecution #-}

-- | The type of the child workflow execution.
cwefecWorkflowType :: Lens' ChildWorkflowExecutionFailedEventAttributes (WorkflowType)
cwefecWorkflowType = lens _cwefecWorkflowType (\s a -> s { _cwefecWorkflowType = a })
{-# INLINE cwefecWorkflowType #-}

-- | The reason for the failure (if provided).
cwefecReason :: Lens' ChildWorkflowExecutionFailedEventAttributes (Maybe Text)
cwefecReason = lens _cwefecReason (\s a -> s { _cwefecReason = a })
{-# INLINE cwefecReason #-}

-- | The details of the failure (if provided).
cwefecDetails :: Lens' ChildWorkflowExecutionFailedEventAttributes (Maybe Text)
cwefecDetails = lens _cwefecDetails (\s a -> s { _cwefecDetails = a })
{-# INLINE cwefecDetails #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cwefecInitiatedEventId :: Lens' ChildWorkflowExecutionFailedEventAttributes (Integer)
cwefecInitiatedEventId = lens _cwefecInitiatedEventId (\s a -> s { _cwefecInitiatedEventId = a })
{-# INLINE cwefecInitiatedEventId #-}

-- | The Id of the ChildWorkflowExecutionStarted event recorded when this child
-- workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
cwefecStartedEventId :: Lens' ChildWorkflowExecutionFailedEventAttributes (Integer)
cwefecStartedEventId = lens _cwefecStartedEventId (\s a -> s { _cwefecStartedEventId = a })
{-# INLINE cwefecStartedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ChildWorkflowExecutionFailedEventAttributes' data type to populate a request.
mkChildWorkflowExecutionFailedEventAttributes :: WorkflowExecution -- ^ 'cwefecWorkflowExecution'
                                              -> WorkflowType -- ^ 'cwefecWorkflowType'
                                              -> Integer -- ^ 'cwefecInitiatedEventId'
                                              -> Integer -- ^ 'cwefecStartedEventId'
                                              -> ChildWorkflowExecutionFailedEventAttributes
mkChildWorkflowExecutionFailedEventAttributes p1 p2 p3 p4 = ChildWorkflowExecutionFailedEventAttributes
    { _cwefecWorkflowExecution = p1
    , _cwefecWorkflowType = p2
    , _cwefecReason = Nothing
    , _cwefecDetails = Nothing
    , _cwefecInitiatedEventId = p5
    , _cwefecStartedEventId = p6
    }
{-# INLINE mkChildWorkflowExecutionFailedEventAttributes #-}

instance FromJSON ChildWorkflowExecutionFailedEventAttributes

instance ToJSON ChildWorkflowExecutionFailedEventAttributes

-- | If the event is of type ChildWorkflowExecutionStarted then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionStartedEventAttributes = ChildWorkflowExecutionStartedEventAttributes
    { _cweseaWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that was started.
    , _cweseaWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    , _cweseaInitiatedEventId :: Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this child workflow execution. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    } deriving (Show, Generic)

-- | The child workflow execution that was started.
cweseaWorkflowExecution :: Lens' ChildWorkflowExecutionStartedEventAttributes (WorkflowExecution)
cweseaWorkflowExecution = lens _cweseaWorkflowExecution (\s a -> s { _cweseaWorkflowExecution = a })
{-# INLINE cweseaWorkflowExecution #-}

-- | The type of the child workflow execution.
cweseaWorkflowType :: Lens' ChildWorkflowExecutionStartedEventAttributes (WorkflowType)
cweseaWorkflowType = lens _cweseaWorkflowType (\s a -> s { _cweseaWorkflowType = a })
{-# INLINE cweseaWorkflowType #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cweseaInitiatedEventId :: Lens' ChildWorkflowExecutionStartedEventAttributes (Integer)
cweseaInitiatedEventId = lens _cweseaInitiatedEventId (\s a -> s { _cweseaInitiatedEventId = a })
{-# INLINE cweseaInitiatedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ChildWorkflowExecutionStartedEventAttributes' data type to populate a request.
mkChildWorkflowExecutionStartedEventAttributes :: WorkflowExecution -- ^ 'cweseaWorkflowExecution'
                                               -> WorkflowType -- ^ 'cweseaWorkflowType'
                                               -> Integer -- ^ 'cweseaInitiatedEventId'
                                               -> ChildWorkflowExecutionStartedEventAttributes
mkChildWorkflowExecutionStartedEventAttributes p1 p2 p3 = ChildWorkflowExecutionStartedEventAttributes
    { _cweseaWorkflowExecution = p1
    , _cweseaWorkflowType = p2
    , _cweseaInitiatedEventId = p3
    }
{-# INLINE mkChildWorkflowExecutionStartedEventAttributes #-}

instance FromJSON ChildWorkflowExecutionStartedEventAttributes

instance ToJSON ChildWorkflowExecutionStartedEventAttributes

-- | If the event is of type ChildWorkflowExecutionTerminated then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionTerminatedEventAttributes = ChildWorkflowExecutionTerminatedEventAttributes
    { _cweteaWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that was terminated.
    , _cweteaWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    , _cweteaInitiatedEventId :: Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this child workflow execution. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cweteaStartedEventId :: Integer
      -- ^ The Id of the ChildWorkflowExecutionStarted event recorded when
      -- this child workflow execution was started. This information can
      -- be useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    } deriving (Show, Generic)

-- | The child workflow execution that was terminated.
cweteaWorkflowExecution :: Lens' ChildWorkflowExecutionTerminatedEventAttributes (WorkflowExecution)
cweteaWorkflowExecution = lens _cweteaWorkflowExecution (\s a -> s { _cweteaWorkflowExecution = a })
{-# INLINE cweteaWorkflowExecution #-}

-- | The type of the child workflow execution.
cweteaWorkflowType :: Lens' ChildWorkflowExecutionTerminatedEventAttributes (WorkflowType)
cweteaWorkflowType = lens _cweteaWorkflowType (\s a -> s { _cweteaWorkflowType = a })
{-# INLINE cweteaWorkflowType #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cweteaInitiatedEventId :: Lens' ChildWorkflowExecutionTerminatedEventAttributes (Integer)
cweteaInitiatedEventId = lens _cweteaInitiatedEventId (\s a -> s { _cweteaInitiatedEventId = a })
{-# INLINE cweteaInitiatedEventId #-}

-- | The Id of the ChildWorkflowExecutionStarted event recorded when this child
-- workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
cweteaStartedEventId :: Lens' ChildWorkflowExecutionTerminatedEventAttributes (Integer)
cweteaStartedEventId = lens _cweteaStartedEventId (\s a -> s { _cweteaStartedEventId = a })
{-# INLINE cweteaStartedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ChildWorkflowExecutionTerminatedEventAttributes' data type to populate a request.
mkChildWorkflowExecutionTerminatedEventAttributes :: WorkflowExecution -- ^ 'cweteaWorkflowExecution'
                                                  -> WorkflowType -- ^ 'cweteaWorkflowType'
                                                  -> Integer -- ^ 'cweteaInitiatedEventId'
                                                  -> Integer -- ^ 'cweteaStartedEventId'
                                                  -> ChildWorkflowExecutionTerminatedEventAttributes
mkChildWorkflowExecutionTerminatedEventAttributes p1 p2 p3 p4 = ChildWorkflowExecutionTerminatedEventAttributes
    { _cweteaWorkflowExecution = p1
    , _cweteaWorkflowType = p2
    , _cweteaInitiatedEventId = p3
    , _cweteaStartedEventId = p4
    }
{-# INLINE mkChildWorkflowExecutionTerminatedEventAttributes #-}

instance FromJSON ChildWorkflowExecutionTerminatedEventAttributes

instance ToJSON ChildWorkflowExecutionTerminatedEventAttributes

-- | If the event is of type ChildWorkflowExecutionTimedOut then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data ChildWorkflowExecutionTimedOutEventAttributes = ChildWorkflowExecutionTimedOutEventAttributes
    { _cwetoeaWorkflowExecution :: WorkflowExecution
      -- ^ The child workflow execution that timed out.
    , _cwetoeaWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    , _cwetoeaTimeoutType :: WorkflowExecutionTimeoutType
      -- ^ The type of the timeout that caused the child workflow execution
      -- to time out.
    , _cwetoeaInitiatedEventId :: Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this child workflow execution. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _cwetoeaStartedEventId :: Integer
      -- ^ The Id of the ChildWorkflowExecutionStarted event recorded when
      -- this child workflow execution was started. This information can
      -- be useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    } deriving (Show, Generic)

-- | The child workflow execution that timed out.
cwetoeaWorkflowExecution :: Lens' ChildWorkflowExecutionTimedOutEventAttributes (WorkflowExecution)
cwetoeaWorkflowExecution = lens _cwetoeaWorkflowExecution (\s a -> s { _cwetoeaWorkflowExecution = a })
{-# INLINE cwetoeaWorkflowExecution #-}

-- | The type of the child workflow execution.
cwetoeaWorkflowType :: Lens' ChildWorkflowExecutionTimedOutEventAttributes (WorkflowType)
cwetoeaWorkflowType = lens _cwetoeaWorkflowType (\s a -> s { _cwetoeaWorkflowType = a })
{-# INLINE cwetoeaWorkflowType #-}

-- | The type of the timeout that caused the child workflow execution to time
-- out.
cwetoeaTimeoutType :: Lens' ChildWorkflowExecutionTimedOutEventAttributes (WorkflowExecutionTimeoutType)
cwetoeaTimeoutType = lens _cwetoeaTimeoutType (\s a -> s { _cwetoeaTimeoutType = a })
{-# INLINE cwetoeaTimeoutType #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cwetoeaInitiatedEventId :: Lens' ChildWorkflowExecutionTimedOutEventAttributes (Integer)
cwetoeaInitiatedEventId = lens _cwetoeaInitiatedEventId (\s a -> s { _cwetoeaInitiatedEventId = a })
{-# INLINE cwetoeaInitiatedEventId #-}

-- | The Id of the ChildWorkflowExecutionStarted event recorded when this child
-- workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
cwetoeaStartedEventId :: Lens' ChildWorkflowExecutionTimedOutEventAttributes (Integer)
cwetoeaStartedEventId = lens _cwetoeaStartedEventId (\s a -> s { _cwetoeaStartedEventId = a })
{-# INLINE cwetoeaStartedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ChildWorkflowExecutionTimedOutEventAttributes' data type to populate a request.
mkChildWorkflowExecutionTimedOutEventAttributes :: WorkflowExecution -- ^ 'cwetoeaWorkflowExecution'
                                                -> WorkflowType -- ^ 'cwetoeaWorkflowType'
                                                -> WorkflowExecutionTimeoutType -- ^ 'cwetoeaTimeoutType'
                                                -> Integer -- ^ 'cwetoeaInitiatedEventId'
                                                -> Integer -- ^ 'cwetoeaStartedEventId'
                                                -> ChildWorkflowExecutionTimedOutEventAttributes
mkChildWorkflowExecutionTimedOutEventAttributes p1 p2 p3 p4 p5 = ChildWorkflowExecutionTimedOutEventAttributes
    { _cwetoeaWorkflowExecution = p1
    , _cwetoeaWorkflowType = p2
    , _cwetoeaTimeoutType = p3
    , _cwetoeaInitiatedEventId = p4
    , _cwetoeaStartedEventId = p5
    }
{-# INLINE mkChildWorkflowExecutionTimedOutEventAttributes #-}

instance FromJSON ChildWorkflowExecutionTimedOutEventAttributes

instance ToJSON ChildWorkflowExecutionTimedOutEventAttributes

-- | If the event is of type CompleteWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data CompleteWorkflowExecutionFailedEventAttributes = CompleteWorkflowExecutionFailedEventAttributes
    { _cwefeaCause :: CompleteWorkflowExecutionFailedCause
      -- ^ The cause of the failure. This information is generated by the
      -- system and can be useful for diagnostic purposes. If cause is set
      -- to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see
      -- Using IAM to Manage Access to Amazon SWF Workflows.
    , _cwefeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the CompleteWorkflowExecution
      -- decision to complete this execution. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    } deriving (Show, Generic)

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.
cwefeaCause :: Lens' CompleteWorkflowExecutionFailedEventAttributes (CompleteWorkflowExecutionFailedCause)
cwefeaCause = lens _cwefeaCause (\s a -> s { _cwefeaCause = a })
{-# INLINE cwefeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CompleteWorkflowExecution decision to complete
-- this execution. This information can be useful for diagnosing problems by
-- tracing back the cause of events.
cwefeaDecisionTaskCompletedEventId :: Lens' CompleteWorkflowExecutionFailedEventAttributes (Integer)
cwefeaDecisionTaskCompletedEventId = lens _cwefeaDecisionTaskCompletedEventId (\s a -> s { _cwefeaDecisionTaskCompletedEventId = a })
{-# INLINE cwefeaDecisionTaskCompletedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CompleteWorkflowExecutionFailedEventAttributes' data type to populate a request.
mkCompleteWorkflowExecutionFailedEventAttributes :: CompleteWorkflowExecutionFailedCause -- ^ 'cwefeaCause'
                                                 -> Integer -- ^ 'cwefeaDecisionTaskCompletedEventId'
                                                 -> CompleteWorkflowExecutionFailedEventAttributes
mkCompleteWorkflowExecutionFailedEventAttributes p1 p2 = CompleteWorkflowExecutionFailedEventAttributes
    { _cwefeaCause = p1
    , _cwefeaDecisionTaskCompletedEventId = p2
    }
{-# INLINE mkCompleteWorkflowExecutionFailedEventAttributes #-}

instance FromJSON CompleteWorkflowExecutionFailedEventAttributes

instance ToJSON CompleteWorkflowExecutionFailedEventAttributes

-- | Provides details of the ContinueAsNewWorkflowExecution decision. It is not
-- set for other decision types.
data ContinueAsNewWorkflowExecutionDecisionAttributes = ContinueAsNewWorkflowExecutionDecisionAttributes
    { _canwedaInput :: Maybe Text
      -- ^ The input provided to the new workflow execution.
    , _canwedaExecutionStartToCloseTimeout :: Maybe Text
      -- ^ If set, specifies the total duration for this workflow execution.
      -- This overrides the defaultExecutionStartToCloseTimeout specified
      -- when registering the workflow type. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify
      -- unlimited duration. An execution start-to-close timeout for this
      -- workflow execution must be specified either as a default for the
      -- workflow type or through this field. If neither this field is set
      -- nor a default execution start-to-close timeout was specified at
      -- registration time then a fault will be returned.
    , _canwedaTaskList :: Maybe TaskList
      -- ^ Represents a task list.
    , _canwedaTaskStartToCloseTimeout :: Maybe Text
      -- ^ Specifies the maximum duration of decision tasks for the new
      -- workflow execution. This parameter overrides the
      -- defaultTaskStartToCloseTimout specified when registering the
      -- workflow type using RegisterWorkflowType. The valid values are
      -- integers greater than or equal to 0. An integer value can be used
      -- to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration. A task start-to-close timeout for the
      -- new workflow execution must be specified either as a default for
      -- the workflow type or through this parameter. If neither this
      -- parameter is set nor a default task start-to-close timeout was
      -- specified at registration time then a fault will be returned.
    , _canwedaChildPolicy :: Maybe ChildPolicy
      -- ^ If set, specifies the policy to use for the child workflow
      -- executions of the new execution if it is terminated by calling
      -- the TerminateWorkflowExecution action explicitly or due to an
      -- expired timeout. This policy overrides the default child policy
      -- specified when registering the workflow type using
      -- RegisterWorkflowType. The supported child policies are:
      -- TERMINATE: the child executions will be terminated.
      -- REQUEST_CANCEL: a request to cancel will be attempted for each
      -- child execution by recording a WorkflowExecutionCancelRequested
      -- event in its history. It is up to the decider to take appropriate
      -- actions when it receives an execution history with this event.
      -- ABANDON: no action will be taken. The child executions will
      -- continue to run. A child policy for the new workflow execution
      -- must be specified either as a default registered for its workflow
      -- type or through this field. If neither this field is set nor a
      -- default child policy was specified at registration time then a
      -- fault will be returned.
    , _canwedaTagList :: [Text]
      -- ^ The list of tags to associate with the new workflow execution. A
      -- maximum of 5 tags can be specified. You can list workflow
      -- executions with a specific tag by calling
      -- ListOpenWorkflowExecutions or ListClosedWorkflowExecutions and
      -- specifying a TagFilter.
    , _canwedaWorkflowTypeVersion :: Maybe Text
    } deriving (Show, Generic)

-- | The input provided to the new workflow execution.
canwedaInput :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaInput = lens _canwedaInput (\s a -> s { _canwedaInput = a })
{-# INLINE canwedaInput #-}

-- | If set, specifies the total duration for this workflow execution. This
-- overrides the defaultExecutionStartToCloseTimeout specified when
-- registering the workflow type. The valid values are integers greater than
-- or equal to 0. An integer value can be used to specify the duration in
-- seconds while NONE can be used to specify unlimited duration. An execution
-- start-to-close timeout for this workflow execution must be specified either
-- as a default for the workflow type or through this field. If neither this
-- field is set nor a default execution start-to-close timeout was specified
-- at registration time then a fault will be returned.
canwedaExecutionStartToCloseTimeout :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaExecutionStartToCloseTimeout = lens _canwedaExecutionStartToCloseTimeout (\s a -> s { _canwedaExecutionStartToCloseTimeout = a })
{-# INLINE canwedaExecutionStartToCloseTimeout #-}

-- | Represents a task list.
canwedaTaskList :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe TaskList)
canwedaTaskList = lens _canwedaTaskList (\s a -> s { _canwedaTaskList = a })
{-# INLINE canwedaTaskList #-}

-- | Specifies the maximum duration of decision tasks for the new workflow
-- execution. This parameter overrides the defaultTaskStartToCloseTimout
-- specified when registering the workflow type using RegisterWorkflowType.
-- The valid values are integers greater than or equal to 0. An integer value
-- can be used to specify the duration in seconds while NONE can be used to
-- specify unlimited duration. A task start-to-close timeout for the new
-- workflow execution must be specified either as a default for the workflow
-- type or through this parameter. If neither this parameter is set nor a
-- default task start-to-close timeout was specified at registration time then
-- a fault will be returned.
canwedaTaskStartToCloseTimeout :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaTaskStartToCloseTimeout = lens _canwedaTaskStartToCloseTimeout (\s a -> s { _canwedaTaskStartToCloseTimeout = a })
{-# INLINE canwedaTaskStartToCloseTimeout #-}

-- | If set, specifies the policy to use for the child workflow executions of
-- the new execution if it is terminated by calling the
-- TerminateWorkflowExecution action explicitly or due to an expired timeout.
-- This policy overrides the default child policy specified when registering
-- the workflow type using RegisterWorkflowType. The supported child policies
-- are: TERMINATE: the child executions will be terminated. REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a
-- WorkflowExecutionCancelRequested event in its history. It is up to the
-- decider to take appropriate actions when it receives an execution history
-- with this event. ABANDON: no action will be taken. The child executions
-- will continue to run. A child policy for the new workflow execution must be
-- specified either as a default registered for its workflow type or through
-- this field. If neither this field is set nor a default child policy was
-- specified at registration time then a fault will be returned.
canwedaChildPolicy :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe ChildPolicy)
canwedaChildPolicy = lens _canwedaChildPolicy (\s a -> s { _canwedaChildPolicy = a })
{-# INLINE canwedaChildPolicy #-}

-- | The list of tags to associate with the new workflow execution. A maximum of
-- 5 tags can be specified. You can list workflow executions with a specific
-- tag by calling ListOpenWorkflowExecutions or ListClosedWorkflowExecutions
-- and specifying a TagFilter.
canwedaTagList :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes ([Text])
canwedaTagList = lens _canwedaTagList (\s a -> s { _canwedaTagList = a })
{-# INLINE canwedaTagList #-}

canwedaWorkflowTypeVersion :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaWorkflowTypeVersion = lens _canwedaWorkflowTypeVersion (\s a -> s { _canwedaWorkflowTypeVersion = a })
{-# INLINE canwedaWorkflowTypeVersion #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ContinueAsNewWorkflowExecutionDecisionAttributes' data type to populate a request.
mkContinueAsNewWorkflowExecutionDecisionAttributes :: ContinueAsNewWorkflowExecutionDecisionAttributes
mkContinueAsNewWorkflowExecutionDecisionAttributes = ContinueAsNewWorkflowExecutionDecisionAttributes
    { _canwedaInput = Nothing
    , _canwedaExecutionStartToCloseTimeout = Nothing
    , _canwedaTaskList = Nothing
    , _canwedaTaskStartToCloseTimeout = Nothing
    , _canwedaChildPolicy = Nothing
    , _canwedaTagList = mempty
    , _canwedaWorkflowTypeVersion = Nothing
    }
{-# INLINE mkContinueAsNewWorkflowExecutionDecisionAttributes #-}

instance FromJSON ContinueAsNewWorkflowExecutionDecisionAttributes

instance ToJSON ContinueAsNewWorkflowExecutionDecisionAttributes

-- | If the event is of type ContinueAsNewWorkflowExecutionFailed then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
data ContinueAsNewWorkflowExecutionFailedEventAttributes = ContinueAsNewWorkflowExecutionFailedEventAttributes
    { _canwefeaCause :: ContinueAsNewWorkflowExecutionFailedCause
      -- ^ The cause of the failure. This information is generated by the
      -- system and can be useful for diagnostic purposes. If cause is set
      -- to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see
      -- Using IAM to Manage Access to Amazon SWF Workflows.
    , _canwefeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the ContinueAsNewWorkflowExecution
      -- decision that started this execution. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    } deriving (Show, Generic)

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.
canwefeaCause :: Lens' ContinueAsNewWorkflowExecutionFailedEventAttributes (ContinueAsNewWorkflowExecutionFailedCause)
canwefeaCause = lens _canwefeaCause (\s a -> s { _canwefeaCause = a })
{-# INLINE canwefeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the ContinueAsNewWorkflowExecution decision that
-- started this execution. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
canwefeaDecisionTaskCompletedEventId :: Lens' ContinueAsNewWorkflowExecutionFailedEventAttributes (Integer)
canwefeaDecisionTaskCompletedEventId = lens _canwefeaDecisionTaskCompletedEventId (\s a -> s { _canwefeaDecisionTaskCompletedEventId = a })
{-# INLINE canwefeaDecisionTaskCompletedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ContinueAsNewWorkflowExecutionFailedEventAttributes' data type to populate a request.
mkContinueAsNewWorkflowExecutionFailedEventAttributes :: ContinueAsNewWorkflowExecutionFailedCause -- ^ 'canwefeaCause'
                                                      -> Integer -- ^ 'canwefeaDecisionTaskCompletedEventId'
                                                      -> ContinueAsNewWorkflowExecutionFailedEventAttributes
mkContinueAsNewWorkflowExecutionFailedEventAttributes p1 p2 = ContinueAsNewWorkflowExecutionFailedEventAttributes
    { _canwefeaCause = p1
    , _canwefeaDecisionTaskCompletedEventId = p2
    }
{-# INLINE mkContinueAsNewWorkflowExecutionFailedEventAttributes #-}

instance FromJSON ContinueAsNewWorkflowExecutionFailedEventAttributes

instance ToJSON ContinueAsNewWorkflowExecutionFailedEventAttributes

-- | Specifies a decision made by the decider. A decision can be one of these
-- types: CancelTimer cancels a previously started timer and records a
-- TimerCanceled event in the history. CancelWorkflowExecution closes the
-- workflow execution and records a WorkflowExecutionCanceled event in the
-- history. CompleteWorkflowExecution closes the workflow execution and
-- records a WorkflowExecutionCompleted event in the history .
-- ContinueAsNewWorkflowExecution closes the workflow execution and starts a
-- new workflow execution of the same type using the same workflow id and a
-- unique run Id. A WorkflowExecutionContinuedAsNew event is recorded in the
-- history. FailWorkflowExecution closes the workflow execution and records a
-- WorkflowExecutionFailed event in the history. RecordMarker records a
-- MarkerRecorded event in the history. Markers can be used for adding custom
-- information in the history for instance to let deciders know that they do
-- not need to look at the history beyond the marker event.
-- RequestCancelActivityTask attempts to cancel a previously scheduled
-- activity task. If the activity task was scheduled but has not been assigned
-- to a worker, then it will be canceled. If the activity task was already
-- assigned to a worker, then the worker will be informed that cancellation
-- has been requested in the response to RecordActivityTaskHeartbeat.
-- RequestCancelExternalWorkflowExecution requests that a request be made to
-- cancel the specified external workflow execution and records a
-- RequestCancelExternalWorkflowExecutionInitiated event in the history.
-- ScheduleActivityTask schedules an activity task.
-- SignalExternalWorkflowExecution requests a signal to be delivered to the
-- specified external workflow execution and records a
-- SignalExternalWorkflowExecutionInitiated event in the history.
-- StartChildWorkflowExecution requests that a child workflow execution be
-- started and records a StartChildWorkflowExecutionInitiated event in the
-- history. The child workflow execution is a separate workflow execution with
-- its own history. StartTimer starts a timer for this workflow execution and
-- records a TimerStarted event in the history. This timer will fire after the
-- specified delay and record a TimerFired event. Access Control If you grant
-- permission to use RespondDecisionTaskCompleted, you can use IAM policies to
-- express permissions for the list of decisions returned by this action as if
-- they were members of the API. Treating decisions as a pseudo API maintains
-- a uniform conceptual model and helps keep policies readable. For details
-- and example IAM policies, see Using IAM to Manage Access to Amazon SWF
-- Workflows. Decision Failure Decisions can fail for several reasons The
-- ordering of decisions should follow a logical flow. Some decisions might
-- not make sense in the current context of the workflow execution and will
-- therefore fail. A limit on your account was reached. The decision lacks
-- sufficient permissions. One of the following events might be added to the
-- history to indicate an error. The event attribute's cause parameter
-- indicates the cause. If cause is set to OPERATION_NOT_PERMITTED, the
-- decision failed because it lacked sufficient permissions.
-- ScheduleActivityTaskFailed a ScheduleActivityTask decision failed. This
-- could happen if the activity type specified in the decision is not
-- registered, is in a deprecated state, or the decision is not properly
-- configured. RequestCancelActivityTaskFailed a RequestCancelActivityTask
-- decision failed. This could happen if there is no open activity task with
-- the specified activityId. StartTimerFailed a StartTimer decision failed.
-- This could happen if there is another open timer with the same timerId.
-- CancelTimerFailed a CancelTimer decision failed. This could happen if there
-- is no open timer with the specified timerId.
-- StartChildWorkflowExecutionFailed a StartChildWorkflowExecution decision
-- failed. This could happen if the workflow type specified is not registered,
-- is deprecated, or the decision is not properly configured.
-- SignalExternalWorkflowExecutionFailed a SignalExternalWorkflowExecution
-- decision failed. This could happen if the workflowID specified in the
-- decision was incorrect. RequestCancelExternalWorkflowExecutionFailed a
-- RequestCancelExternalWorkflowExecution decision failed. This could happen
-- if the workflowID specified in the decision was incorrect.
-- CancelWorkflowExecutionFailed a CancelWorkflowExecution decision failed.
-- This could happen if there is an unhandled decision task pending in the
-- workflow execution. CompleteWorkflowExecutionFailed a
-- CompleteWorkflowExecution decision failed. This could happen if there is an
-- unhandled decision task pending in the workflow execution.
-- ContinueAsNewWorkflowExecutionFailed a ContinueAsNewWorkflowExecution
-- decision failed. This could happen if there is an unhandled decision task
-- pending in the workflow execution or the ContinueAsNewWorkflowExecution
-- decision was not configured correctly. FailWorkflowExecutionFailed a
-- FailWorkflowExecution decision failed. This could happen if there is an
-- unhandled decision task pending in the workflow execution. The preceding
-- error events might occur due to an error in the decider logic, which might
-- put the workflow execution in an unstable state The cause field in the
-- event structure for the error event indicates the cause of the error. A
-- workflow execution may be closed by the decider by returning one of the
-- following decisions when completing a decision task:
-- CompleteWorkflowExecution, FailWorkflowExecution, CancelWorkflowExecution
-- and ContinueAsNewWorkflowExecution. An UnhandledDecision fault will be
-- returned if a workflow closing decision is specified and a signal or
-- activity event had been added to the history while the decision task was
-- being performed by the decider. Unlike the above situations which are logic
-- issues, this fault is always possible because of race conditions in a
-- distributed system. The right action here is to call
-- RespondDecisionTaskCompleted without any decisions. This would result in
-- another decision task with these new events included in the history. The
-- decider should handle the new events and may decide to close the workflow
-- execution. How to Code a Decision You code a decision by first setting the
-- decision type field to one of the above decision values, and then set the
-- corresponding attributes field shown below:
-- ScheduleActivityTaskDecisionAttributes
-- RequestCancelActivityTaskDecisionAttributes
-- CompleteWorkflowExecutionDecisionAttributes
-- FailWorkflowExecutionDecisionAttributes
-- CancelWorkflowExecutionDecisionAttributes
-- ContinueAsNewWorkflowExecutionDecisionAttributes
-- RecordMarkerDecisionAttributes StartTimerDecisionAttributes
-- CancelTimerDecisionAttributes
-- SignalExternalWorkflowExecutionDecisionAttributes
-- RequestCancelExternalWorkflowExecutionDecisionAttributes
-- StartChildWorkflowExecutionDecisionAttributes.
data Decision = Decision
    { _ddddrDecisionType :: DecisionType
      -- ^ Specifies the type of the decision.
    , _ddddrScheduleActivityTaskDecisionAttributes :: Maybe ScheduleActivityTaskDecisionAttributes
      -- ^ Provides details of the ScheduleActivityTask decision. It is not
      -- set for other decision types.
    , _ddddrRequestCancelActivityTaskDecisionAttributes :: Maybe RequestCancelActivityTaskDecisionAttributes
      -- ^ Provides details of the RequestCancelActivityTask decision. It is
      -- not set for other decision types.
    , _ddddrCompleteWorkflowExecutionDecisionAttributes :: Maybe CompleteWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the CompleteWorkflowExecution decision. It is
      -- not set for other decision types.
    , _ddddrFailWorkflowExecutionDecisionAttributes :: Maybe FailWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the FailWorkflowExecution decision. It is not
      -- set for other decision types.
    , _ddddrCancelWorkflowExecutionDecisionAttributes :: Maybe CancelWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the CancelWorkflowExecution decision. It is
      -- not set for other decision types.
    , _ddddrContinueAsNewWorkflowExecutionDecisionAttributes :: Maybe ContinueAsNewWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the ContinueAsNewWorkflowExecution decision.
      -- It is not set for other decision types.
    , _ddddrRecordMarkerDecisionAttributes :: Maybe RecordMarkerDecisionAttributes
      -- ^ Provides details of the RecordMarker decision. It is not set for
      -- other decision types.
    , _ddddrStartTimerDecisionAttributes :: Maybe StartTimerDecisionAttributes
      -- ^ Provides details of the StartTimer decision. It is not set for
      -- other decision types.
    , _ddddrCancelTimerDecisionAttributes :: Maybe CancelTimerDecisionAttributes
      -- ^ Provides details of the CancelTimer decision. It is not set for
      -- other decision types.
    , _ddddrSignalExternalWorkflowExecutionDecisionAttributes :: Maybe SignalExternalWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the SignalExternalWorkflowExecution decision.
      -- It is not set for other decision types.
    , _ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes :: Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the RequestCancelExternalWorkflowExecution
      -- decision. It is not set for other decision types.
    , _ddddrStartChildWorkflowExecutionDecisionAttributes :: Maybe StartChildWorkflowExecutionDecisionAttributes
      -- ^ Provides details of the StartChildWorkflowExecution decision. It
      -- is not set for other decision types.
    } deriving (Show, Generic)

-- | Specifies the type of the decision.
ddddrDecisionType :: Lens' Decision (DecisionType)
ddddrDecisionType = lens _ddddrDecisionType (\s a -> s { _ddddrDecisionType = a })
{-# INLINE ddddrDecisionType #-}

-- | Provides details of the ScheduleActivityTask decision. It is not set for
-- other decision types.
ddddrScheduleActivityTaskDecisionAttributes :: Lens' Decision (Maybe ScheduleActivityTaskDecisionAttributes)
ddddrScheduleActivityTaskDecisionAttributes = lens _ddddrScheduleActivityTaskDecisionAttributes (\s a -> s { _ddddrScheduleActivityTaskDecisionAttributes = a })
{-# INLINE ddddrScheduleActivityTaskDecisionAttributes #-}

-- | Provides details of the RequestCancelActivityTask decision. It is not set
-- for other decision types.
ddddrRequestCancelActivityTaskDecisionAttributes :: Lens' Decision (Maybe RequestCancelActivityTaskDecisionAttributes)
ddddrRequestCancelActivityTaskDecisionAttributes = lens _ddddrRequestCancelActivityTaskDecisionAttributes (\s a -> s { _ddddrRequestCancelActivityTaskDecisionAttributes = a })
{-# INLINE ddddrRequestCancelActivityTaskDecisionAttributes #-}

-- | Provides details of the CompleteWorkflowExecution decision. It is not set
-- for other decision types.
ddddrCompleteWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe CompleteWorkflowExecutionDecisionAttributes)
ddddrCompleteWorkflowExecutionDecisionAttributes = lens _ddddrCompleteWorkflowExecutionDecisionAttributes (\s a -> s { _ddddrCompleteWorkflowExecutionDecisionAttributes = a })
{-# INLINE ddddrCompleteWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the FailWorkflowExecution decision. It is not set for
-- other decision types.
ddddrFailWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe FailWorkflowExecutionDecisionAttributes)
ddddrFailWorkflowExecutionDecisionAttributes = lens _ddddrFailWorkflowExecutionDecisionAttributes (\s a -> s { _ddddrFailWorkflowExecutionDecisionAttributes = a })
{-# INLINE ddddrFailWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the CancelWorkflowExecution decision. It is not set for
-- other decision types.
ddddrCancelWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe CancelWorkflowExecutionDecisionAttributes)
ddddrCancelWorkflowExecutionDecisionAttributes = lens _ddddrCancelWorkflowExecutionDecisionAttributes (\s a -> s { _ddddrCancelWorkflowExecutionDecisionAttributes = a })
{-# INLINE ddddrCancelWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the ContinueAsNewWorkflowExecution decision. It is not
-- set for other decision types.
ddddrContinueAsNewWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe ContinueAsNewWorkflowExecutionDecisionAttributes)
ddddrContinueAsNewWorkflowExecutionDecisionAttributes = lens _ddddrContinueAsNewWorkflowExecutionDecisionAttributes (\s a -> s { _ddddrContinueAsNewWorkflowExecutionDecisionAttributes = a })
{-# INLINE ddddrContinueAsNewWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the RecordMarker decision. It is not set for other
-- decision types.
ddddrRecordMarkerDecisionAttributes :: Lens' Decision (Maybe RecordMarkerDecisionAttributes)
ddddrRecordMarkerDecisionAttributes = lens _ddddrRecordMarkerDecisionAttributes (\s a -> s { _ddddrRecordMarkerDecisionAttributes = a })
{-# INLINE ddddrRecordMarkerDecisionAttributes #-}

-- | Provides details of the StartTimer decision. It is not set for other
-- decision types.
ddddrStartTimerDecisionAttributes :: Lens' Decision (Maybe StartTimerDecisionAttributes)
ddddrStartTimerDecisionAttributes = lens _ddddrStartTimerDecisionAttributes (\s a -> s { _ddddrStartTimerDecisionAttributes = a })
{-# INLINE ddddrStartTimerDecisionAttributes #-}

-- | Provides details of the CancelTimer decision. It is not set for other
-- decision types.
ddddrCancelTimerDecisionAttributes :: Lens' Decision (Maybe CancelTimerDecisionAttributes)
ddddrCancelTimerDecisionAttributes = lens _ddddrCancelTimerDecisionAttributes (\s a -> s { _ddddrCancelTimerDecisionAttributes = a })
{-# INLINE ddddrCancelTimerDecisionAttributes #-}

-- | Provides details of the SignalExternalWorkflowExecution decision. It is not
-- set for other decision types.
ddddrSignalExternalWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe SignalExternalWorkflowExecutionDecisionAttributes)
ddddrSignalExternalWorkflowExecutionDecisionAttributes = lens _ddddrSignalExternalWorkflowExecutionDecisionAttributes (\s a -> s { _ddddrSignalExternalWorkflowExecutionDecisionAttributes = a })
{-# INLINE ddddrSignalExternalWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the RequestCancelExternalWorkflowExecution decision. It
-- is not set for other decision types.
ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes)
ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes = lens _ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes (\s a -> s { _ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes = a })
{-# INLINE ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the StartChildWorkflowExecution decision. It is not set
-- for other decision types.
ddddrStartChildWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe StartChildWorkflowExecutionDecisionAttributes)
ddddrStartChildWorkflowExecutionDecisionAttributes = lens _ddddrStartChildWorkflowExecutionDecisionAttributes (\s a -> s { _ddddrStartChildWorkflowExecutionDecisionAttributes = a })
{-# INLINE ddddrStartChildWorkflowExecutionDecisionAttributes #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Decision' data type to populate a request.
mkDecision :: DecisionType -- ^ 'ddddrDecisionType'
           -> Decision
mkDecision p1 = Decision
    { _ddddrDecisionType = p1
    , _ddddrScheduleActivityTaskDecisionAttributes = Nothing
    , _ddddrRequestCancelActivityTaskDecisionAttributes = Nothing
    , _ddddrCompleteWorkflowExecutionDecisionAttributes = Nothing
    , _ddddrFailWorkflowExecutionDecisionAttributes = Nothing
    , _ddddrCancelWorkflowExecutionDecisionAttributes = Nothing
    , _ddddrContinueAsNewWorkflowExecutionDecisionAttributes = Nothing
    , _ddddrRecordMarkerDecisionAttributes = Nothing
    , _ddddrStartTimerDecisionAttributes = Nothing
    , _ddddrCancelTimerDecisionAttributes = Nothing
    , _ddddrSignalExternalWorkflowExecutionDecisionAttributes = Nothing
    , _ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes = Nothing
    , _ddddrStartChildWorkflowExecutionDecisionAttributes = Nothing
    }
{-# INLINE mkDecision #-}

instance ToJSON Decision

-- | If the event is of type DecisionTaskCompleted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data DecisionTaskCompletedEventAttributes = DecisionTaskCompletedEventAttributes
    { _dtceaExecutionContext :: Maybe Text
      -- ^ User defined context for the workflow execution.
    , _dtceaScheduledEventId :: Integer
      -- ^ The id of the DecisionTaskScheduled event that was recorded when
      -- this decision task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    , _dtceaStartedEventId :: Integer
      -- ^ The Id of the DecisionTaskStarted event recorded when this
      -- decision task was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    } deriving (Show, Generic)

-- | User defined context for the workflow execution.
dtceaExecutionContext :: Lens' DecisionTaskCompletedEventAttributes (Maybe Text)
dtceaExecutionContext = lens _dtceaExecutionContext (\s a -> s { _dtceaExecutionContext = a })
{-# INLINE dtceaExecutionContext #-}

-- | The id of the DecisionTaskScheduled event that was recorded when this
-- decision task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
dtceaScheduledEventId :: Lens' DecisionTaskCompletedEventAttributes (Integer)
dtceaScheduledEventId = lens _dtceaScheduledEventId (\s a -> s { _dtceaScheduledEventId = a })
{-# INLINE dtceaScheduledEventId #-}

-- | The Id of the DecisionTaskStarted event recorded when this decision task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
dtceaStartedEventId :: Lens' DecisionTaskCompletedEventAttributes (Integer)
dtceaStartedEventId = lens _dtceaStartedEventId (\s a -> s { _dtceaStartedEventId = a })
{-# INLINE dtceaStartedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DecisionTaskCompletedEventAttributes' data type to populate a request.
mkDecisionTaskCompletedEventAttributes :: Integer -- ^ 'dtceaScheduledEventId'
                                       -> Integer -- ^ 'dtceaStartedEventId'
                                       -> DecisionTaskCompletedEventAttributes
mkDecisionTaskCompletedEventAttributes p1 p2 = DecisionTaskCompletedEventAttributes
    { _dtceaExecutionContext = Nothing
    , _dtceaScheduledEventId = p2
    , _dtceaStartedEventId = p3
    }
{-# INLINE mkDecisionTaskCompletedEventAttributes #-}

instance FromJSON DecisionTaskCompletedEventAttributes

instance ToJSON DecisionTaskCompletedEventAttributes

-- | If the event is of type DecisionTaskScheduled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data DecisionTaskScheduledEventAttributes = DecisionTaskScheduledEventAttributes
    { _dtseaTaskList :: TaskList
      -- ^ The name of the task list in which the decision task was
      -- scheduled.
    , _dtseaStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration for this decision task. The task is
      -- considered timed out if it does not completed within this
      -- duration. The valid values are integers greater than or equal to
      -- 0. An integer value can be used to specify the duration in
      -- seconds while NONE can be used to specify unlimited duration.
    } deriving (Show, Generic)

-- | The name of the task list in which the decision task was scheduled.
dtseaTaskList :: Lens' DecisionTaskScheduledEventAttributes (TaskList)
dtseaTaskList = lens _dtseaTaskList (\s a -> s { _dtseaTaskList = a })
{-# INLINE dtseaTaskList #-}

-- | The maximum duration for this decision task. The task is considered timed
-- out if it does not completed within this duration. The valid values are
-- integers greater than or equal to 0. An integer value can be used to
-- specify the duration in seconds while NONE can be used to specify unlimited
-- duration.
dtseaStartToCloseTimeout :: Lens' DecisionTaskScheduledEventAttributes (Maybe Text)
dtseaStartToCloseTimeout = lens _dtseaStartToCloseTimeout (\s a -> s { _dtseaStartToCloseTimeout = a })
{-# INLINE dtseaStartToCloseTimeout #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DecisionTaskScheduledEventAttributes' data type to populate a request.
mkDecisionTaskScheduledEventAttributes :: TaskList -- ^ 'dtseaTaskList'
                                       -> DecisionTaskScheduledEventAttributes
mkDecisionTaskScheduledEventAttributes p1 = DecisionTaskScheduledEventAttributes
    { _dtseaTaskList = p1
    , _dtseaStartToCloseTimeout = Nothing
    }
{-# INLINE mkDecisionTaskScheduledEventAttributes #-}

instance FromJSON DecisionTaskScheduledEventAttributes

instance ToJSON DecisionTaskScheduledEventAttributes

-- | If the event is of type DecisionTaskStarted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data DecisionTaskStartedEventAttributes = DecisionTaskStartedEventAttributes
    { _dtsebIdentity :: Maybe Text
      -- ^ Identity of the decider making the request. This enables
      -- diagnostic tracing when problems arise. The form of this identity
      -- is user defined.
    , _dtsebScheduledEventId :: Integer
      -- ^ The id of the DecisionTaskScheduled event that was recorded when
      -- this decision task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    } deriving (Show, Generic)

-- | Identity of the decider making the request. This enables diagnostic tracing
-- when problems arise. The form of this identity is user defined.
dtsebIdentity :: Lens' DecisionTaskStartedEventAttributes (Maybe Text)
dtsebIdentity = lens _dtsebIdentity (\s a -> s { _dtsebIdentity = a })
{-# INLINE dtsebIdentity #-}

-- | The id of the DecisionTaskScheduled event that was recorded when this
-- decision task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
dtsebScheduledEventId :: Lens' DecisionTaskStartedEventAttributes (Integer)
dtsebScheduledEventId = lens _dtsebScheduledEventId (\s a -> s { _dtsebScheduledEventId = a })
{-# INLINE dtsebScheduledEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DecisionTaskStartedEventAttributes' data type to populate a request.
mkDecisionTaskStartedEventAttributes :: Integer -- ^ 'dtsebScheduledEventId'
                                     -> DecisionTaskStartedEventAttributes
mkDecisionTaskStartedEventAttributes p1 = DecisionTaskStartedEventAttributes
    { _dtsebIdentity = Nothing
    , _dtsebScheduledEventId = p2
    }
{-# INLINE mkDecisionTaskStartedEventAttributes #-}

instance FromJSON DecisionTaskStartedEventAttributes

instance ToJSON DecisionTaskStartedEventAttributes

-- | If the event is of type DecisionTaskTimedOut then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data DecisionTaskTimedOutEventAttributes = DecisionTaskTimedOutEventAttributes
    { _dttoeaTimeoutType :: DecisionTaskTimeoutType
      -- ^ The type of timeout that expired before the decision task could
      -- be completed.
    , _dttoeaScheduledEventId :: Integer
      -- ^ The id of the DecisionTaskScheduled event that was recorded when
      -- this decision task was scheduled. This information can be useful
      -- for diagnosing problems by tracing back the chain of events
      -- leading up to this event.
    , _dttoeaStartedEventId :: Integer
      -- ^ The Id of the DecisionTaskStarted event recorded when this
      -- decision task was started. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    } deriving (Show, Generic)

-- | The type of timeout that expired before the decision task could be
-- completed.
dttoeaTimeoutType :: Lens' DecisionTaskTimedOutEventAttributes (DecisionTaskTimeoutType)
dttoeaTimeoutType = lens _dttoeaTimeoutType (\s a -> s { _dttoeaTimeoutType = a })
{-# INLINE dttoeaTimeoutType #-}

-- | The id of the DecisionTaskScheduled event that was recorded when this
-- decision task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
dttoeaScheduledEventId :: Lens' DecisionTaskTimedOutEventAttributes (Integer)
dttoeaScheduledEventId = lens _dttoeaScheduledEventId (\s a -> s { _dttoeaScheduledEventId = a })
{-# INLINE dttoeaScheduledEventId #-}

-- | The Id of the DecisionTaskStarted event recorded when this decision task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
dttoeaStartedEventId :: Lens' DecisionTaskTimedOutEventAttributes (Integer)
dttoeaStartedEventId = lens _dttoeaStartedEventId (\s a -> s { _dttoeaStartedEventId = a })
{-# INLINE dttoeaStartedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DecisionTaskTimedOutEventAttributes' data type to populate a request.
mkDecisionTaskTimedOutEventAttributes :: DecisionTaskTimeoutType -- ^ 'dttoeaTimeoutType'
                                      -> Integer -- ^ 'dttoeaScheduledEventId'
                                      -> Integer -- ^ 'dttoeaStartedEventId'
                                      -> DecisionTaskTimedOutEventAttributes
mkDecisionTaskTimedOutEventAttributes p1 p2 p3 = DecisionTaskTimedOutEventAttributes
    { _dttoeaTimeoutType = p1
    , _dttoeaScheduledEventId = p2
    , _dttoeaStartedEventId = p3
    }
{-# INLINE mkDecisionTaskTimedOutEventAttributes #-}

instance FromJSON DecisionTaskTimedOutEventAttributes

instance ToJSON DecisionTaskTimedOutEventAttributes

-- | Contains general information about a domain.
data DomainInfo = DomainInfo
    { _diName :: Text
      -- ^ The name of the domain. This name is unique within the account.
    , _diStatus :: RegistrationStatus
      -- ^ The status of the domain: REGISTERED: The domain is properly
      -- registered and available. You can use this domain for registering
      -- types and creating new workflow executions. DEPRECATED: The
      -- domain was deprecated using DeprecateDomain, but is still in use.
      -- You should not create new workflow executions in this domain.
    , _diDescription :: Maybe Text
      -- ^ The description of the domain provided through RegisterDomain.
    } deriving (Show, Generic)

-- | The name of the domain. This name is unique within the account.
diName :: Lens' DomainInfo (Text)
diName = lens _diName (\s a -> s { _diName = a })
{-# INLINE diName #-}

-- | The status of the domain: REGISTERED: The domain is properly registered and
-- available. You can use this domain for registering types and creating new
-- workflow executions. DEPRECATED: The domain was deprecated using
-- DeprecateDomain, but is still in use. You should not create new workflow
-- executions in this domain.
diStatus :: Lens' DomainInfo (RegistrationStatus)
diStatus = lens _diStatus (\s a -> s { _diStatus = a })
{-# INLINE diStatus #-}

-- | The description of the domain provided through RegisterDomain.
diDescription :: Lens' DomainInfo (Maybe Text)
diDescription = lens _diDescription (\s a -> s { _diDescription = a })
{-# INLINE diDescription #-}

instance FromJSON DomainInfo

-- | If specified, only workflow executions that meet the start time criteria of
-- the filter are counted. startTimeFilter and closeTimeFilter are mutually
-- exclusive. You must specify one of these in a request but not both.
data ExecutionTimeFilter = ExecutionTimeFilter
    { _etfOldestDate :: POSIX
      -- ^ Specifies the oldest start or close date and time to return.
    , _etfLatestDate :: Maybe POSIX
      -- ^ Specifies the latest start or close date and time to return.
    } deriving (Show, Generic)

-- | Specifies the oldest start or close date and time to return.
etfOldestDate :: Lens' ExecutionTimeFilter (POSIX)
etfOldestDate = lens _etfOldestDate (\s a -> s { _etfOldestDate = a })
{-# INLINE etfOldestDate #-}

-- | Specifies the latest start or close date and time to return.
etfLatestDate :: Lens' ExecutionTimeFilter (Maybe POSIX)
etfLatestDate = lens _etfLatestDate (\s a -> s { _etfLatestDate = a })
{-# INLINE etfLatestDate #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ExecutionTimeFilter' data type to populate a request.
mkExecutionTimeFilter :: POSIX -- ^ 'etfOldestDate'
                      -> ExecutionTimeFilter
mkExecutionTimeFilter p1 = ExecutionTimeFilter
    { _etfOldestDate = p1
    , _etfLatestDate = Nothing
    }
{-# INLINE mkExecutionTimeFilter #-}

instance ToJSON ExecutionTimeFilter

-- | If the event is of type ExternalWorkflowExecutionCancelRequested then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
data ExternalWorkflowExecutionCancelRequestedEventAttributes = ExternalWorkflowExecutionCancelRequestedEventAttributes
    { _ewecreaWorkflowExecution :: WorkflowExecution
      -- ^ The external workflow execution to which the cancellation request
      -- was delivered.
    , _ewecreaInitiatedEventId :: Integer
      -- ^ The id of the RequestCancelExternalWorkflowExecutionInitiated
      -- event corresponding to the RequestCancelExternalWorkflowExecution
      -- decision to cancel this external workflow execution. This
      -- information can be useful for diagnosing problems by tracing back
      -- the chain of events leading up to this event.
    } deriving (Show, Generic)

-- | The external workflow execution to which the cancellation request was
-- delivered.
ewecreaWorkflowExecution :: Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes (WorkflowExecution)
ewecreaWorkflowExecution = lens _ewecreaWorkflowExecution (\s a -> s { _ewecreaWorkflowExecution = a })
{-# INLINE ewecreaWorkflowExecution #-}

-- | The id of the RequestCancelExternalWorkflowExecutionInitiated event
-- corresponding to the RequestCancelExternalWorkflowExecution decision to
-- cancel this external workflow execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
ewecreaInitiatedEventId :: Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes (Integer)
ewecreaInitiatedEventId = lens _ewecreaInitiatedEventId (\s a -> s { _ewecreaInitiatedEventId = a })
{-# INLINE ewecreaInitiatedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ExternalWorkflowExecutionCancelRequestedEventAttributes' data type to populate a request.
mkExternalWorkflowExecutionCancelRequestedEventAttributes :: WorkflowExecution -- ^ 'ewecreaWorkflowExecution'
                                                          -> Integer -- ^ 'ewecreaInitiatedEventId'
                                                          -> ExternalWorkflowExecutionCancelRequestedEventAttributes
mkExternalWorkflowExecutionCancelRequestedEventAttributes p1 p2 = ExternalWorkflowExecutionCancelRequestedEventAttributes
    { _ewecreaWorkflowExecution = p1
    , _ewecreaInitiatedEventId = p2
    }
{-# INLINE mkExternalWorkflowExecutionCancelRequestedEventAttributes #-}

instance FromJSON ExternalWorkflowExecutionCancelRequestedEventAttributes

instance ToJSON ExternalWorkflowExecutionCancelRequestedEventAttributes

-- | If the event is of type ExternalWorkflowExecutionSignaled then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
data ExternalWorkflowExecutionSignaledEventAttributes = ExternalWorkflowExecutionSignaledEventAttributes
    { _eweseaWorkflowExecution :: WorkflowExecution
      -- ^ The external workflow execution that the signal was delivered to.
    , _eweseaInitiatedEventId :: Integer
      -- ^ The id of the SignalExternalWorkflowExecutionInitiated event
      -- corresponding to the SignalExternalWorkflowExecution decision to
      -- request this signal. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    } deriving (Show, Generic)

-- | The external workflow execution that the signal was delivered to.
eweseaWorkflowExecution :: Lens' ExternalWorkflowExecutionSignaledEventAttributes (WorkflowExecution)
eweseaWorkflowExecution = lens _eweseaWorkflowExecution (\s a -> s { _eweseaWorkflowExecution = a })
{-# INLINE eweseaWorkflowExecution #-}

-- | The id of the SignalExternalWorkflowExecutionInitiated event corresponding
-- to the SignalExternalWorkflowExecution decision to request this signal.
-- This information can be useful for diagnosing problems by tracing back the
-- chain of events leading up to this event.
eweseaInitiatedEventId :: Lens' ExternalWorkflowExecutionSignaledEventAttributes (Integer)
eweseaInitiatedEventId = lens _eweseaInitiatedEventId (\s a -> s { _eweseaInitiatedEventId = a })
{-# INLINE eweseaInitiatedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ExternalWorkflowExecutionSignaledEventAttributes' data type to populate a request.
mkExternalWorkflowExecutionSignaledEventAttributes :: WorkflowExecution -- ^ 'eweseaWorkflowExecution'
                                                   -> Integer -- ^ 'eweseaInitiatedEventId'
                                                   -> ExternalWorkflowExecutionSignaledEventAttributes
mkExternalWorkflowExecutionSignaledEventAttributes p1 p2 = ExternalWorkflowExecutionSignaledEventAttributes
    { _eweseaWorkflowExecution = p1
    , _eweseaInitiatedEventId = p2
    }
{-# INLINE mkExternalWorkflowExecutionSignaledEventAttributes #-}

instance FromJSON ExternalWorkflowExecutionSignaledEventAttributes

instance ToJSON ExternalWorkflowExecutionSignaledEventAttributes

-- | Provides details of the FailWorkflowExecution decision. It is not set for
-- other decision types.
data FailWorkflowExecutionDecisionAttributes = FailWorkflowExecutionDecisionAttributes
    { _fwedaReason :: Maybe Text
      -- ^ A descriptive reason for the failure that may help in
      -- diagnostics.
    , _fwedaDetails :: Maybe Text
      -- ^ Optional details of the failure.
    } deriving (Show, Generic)

-- | A descriptive reason for the failure that may help in diagnostics.
fwedaReason :: Lens' FailWorkflowExecutionDecisionAttributes (Maybe Text)
fwedaReason = lens _fwedaReason (\s a -> s { _fwedaReason = a })
{-# INLINE fwedaReason #-}

-- | Optional details of the failure.
fwedaDetails :: Lens' FailWorkflowExecutionDecisionAttributes (Maybe Text)
fwedaDetails = lens _fwedaDetails (\s a -> s { _fwedaDetails = a })
{-# INLINE fwedaDetails #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'FailWorkflowExecutionDecisionAttributes' data type to populate a request.
mkFailWorkflowExecutionDecisionAttributes :: FailWorkflowExecutionDecisionAttributes
mkFailWorkflowExecutionDecisionAttributes = FailWorkflowExecutionDecisionAttributes
    { _fwedaReason = Nothing
    , _fwedaDetails = Nothing
    }
{-# INLINE mkFailWorkflowExecutionDecisionAttributes #-}

instance FromJSON FailWorkflowExecutionDecisionAttributes

instance ToJSON FailWorkflowExecutionDecisionAttributes

-- | If the event is of type FailWorkflowExecutionFailed then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data FailWorkflowExecutionFailedEventAttributes = FailWorkflowExecutionFailedEventAttributes
    { _fwefeaCause :: FailWorkflowExecutionFailedCause
      -- ^ The cause of the failure. This information is generated by the
      -- system and can be useful for diagnostic purposes. If cause is set
      -- to OPERATION_NOT_PERMITTED, the decision failed because it lacked
      -- sufficient permissions. For details and example IAM policies, see
      -- Using IAM to Manage Access to Amazon SWF Workflows.
    , _fwefeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the FailWorkflowExecution decision
      -- to fail this execution. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    } deriving (Show, Generic)

-- | The cause of the failure. This information is generated by the system and
-- can be useful for diagnostic purposes. If cause is set to
-- OPERATION_NOT_PERMITTED, the decision failed because it lacked sufficient
-- permissions. For details and example IAM policies, see Using IAM to Manage
-- Access to Amazon SWF Workflows.
fwefeaCause :: Lens' FailWorkflowExecutionFailedEventAttributes (FailWorkflowExecutionFailedCause)
fwefeaCause = lens _fwefeaCause (\s a -> s { _fwefeaCause = a })
{-# INLINE fwefeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the FailWorkflowExecution decision to fail this
-- execution. This information can be useful for diagnosing problems by
-- tracing back the cause of events.
fwefeaDecisionTaskCompletedEventId :: Lens' FailWorkflowExecutionFailedEventAttributes (Integer)
fwefeaDecisionTaskCompletedEventId = lens _fwefeaDecisionTaskCompletedEventId (\s a -> s { _fwefeaDecisionTaskCompletedEventId = a })
{-# INLINE fwefeaDecisionTaskCompletedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'FailWorkflowExecutionFailedEventAttributes' data type to populate a request.
mkFailWorkflowExecutionFailedEventAttributes :: FailWorkflowExecutionFailedCause -- ^ 'fwefeaCause'
                                             -> Integer -- ^ 'fwefeaDecisionTaskCompletedEventId'
                                             -> FailWorkflowExecutionFailedEventAttributes
mkFailWorkflowExecutionFailedEventAttributes p1 p2 = FailWorkflowExecutionFailedEventAttributes
    { _fwefeaCause = p1
    , _fwefeaDecisionTaskCompletedEventId = p2
    }
{-# INLINE mkFailWorkflowExecutionFailedEventAttributes #-}

instance FromJSON FailWorkflowExecutionFailedEventAttributes

instance ToJSON FailWorkflowExecutionFailedEventAttributes

-- | Event within a workflow execution. A history event can be one of these
-- types: WorkflowExecutionStarted: The workflow execution was started.
-- WorkflowExecutionCompleted: The workflow execution was closed due to
-- successful completion. WorkflowExecutionFailed: The workflow execution
-- closed due to a failure. WorkflowExecutionTimedOut: The workflow execution
-- was closed because a time out was exceeded. WorkflowExecutionCanceled: The
-- workflow execution was successfully canceled and closed.
-- WorkflowExecutionTerminated: The workflow execution was terminated.
-- WorkflowExecutionContinuedAsNew: The workflow execution was closed and a
-- new execution of the same type was created with the same workflowId.
-- WorkflowExecutionCancelRequested: A request to cancel this workflow
-- execution was made. DecisionTaskScheduled: A decision task was scheduled
-- for the workflow execution. DecisionTaskStarted: The decision task was
-- dispatched to a decider. DecisionTaskCompleted: The decider successfully
-- completed a decision task by calling RespondDecisionTaskCompleted.
-- DecisionTaskFailed: The decider failed a decision task by calling
-- RespondDecisionTaskFailed. --> DecisionTaskTimedOut: The decision task
-- timed out. ActivityTaskScheduled: An activity task was scheduled for
-- execution. ScheduleActivityTaskFailed: Failed to process
-- ScheduleActivityTask decision. This happens when the decision is not
-- configured properly, for example the activity type specified is not
-- registered. ActivityTaskStarted: The scheduled activity task was dispatched
-- to a worker. ActivityTaskCompleted: An activity worker successfully
-- completed an activity task by calling RespondActivityTaskCompleted.
-- ActivityTaskFailed: An activity worker failed an activity task by calling
-- RespondActivityTaskFailed. ActivityTaskTimedOut: The activity task timed
-- out. ActivityTaskCanceled: The activity task was successfully canceled.
-- ActivityTaskHeartbeatRecorded: A call to RecordActivityTaskHeartbeat was
-- successfully processed by the system. --> ActivityTaskCancelRequested: A
-- RequestCancelActivityTask decision was received by the system.
-- RequestCancelActivityTaskFailed: Failed to process
-- RequestCancelActivityTask decision. This happens when the decision is not
-- configured properly. WorkflowExecutionSignaled: An external signal was
-- received for the workflow execution. MarkerRecorded: A marker was recorded
-- in the workflow history as the result of a RecordMarker decision.
-- TimerStarted: A timer was started for the workflow execution due to a
-- StartTimer decision. StartTimerFailed: Failed to process StartTimer
-- decision. This happens when the decision is not configured properly, for
-- example a timer already exists with the specified timer Id. TimerFired: A
-- timer, previously started for this workflow execution, fired.
-- TimerCanceled: A timer, previously started for this workflow execution, was
-- successfully canceled. CancelTimerFailed: Failed to process CancelTimer
-- decision. This happens when the decision is not configured properly, for
-- example no timer exists with the specified timer Id.
-- StartChildWorkflowExecutionInitiated: A request was made to start a child
-- workflow execution. StartChildWorkflowExecutionFailed: Failed to process
-- StartChildWorkflowExecution decision. This happens when the decision is not
-- configured properly, for example the workflow type specified is not
-- registered. ChildWorkflowExecutionStarted: A child workflow execution was
-- successfully started. ChildWorkflowExecutionCompleted: A child workflow
-- execution, started by this workflow execution, completed successfully and
-- was closed. ChildWorkflowExecutionFailed: A child workflow execution,
-- started by this workflow execution, failed to complete successfully and was
-- closed. ChildWorkflowExecutionTimedOut: A child workflow execution, started
-- by this workflow execution, timed out and was closed.
-- ChildWorkflowExecutionCanceled: A child workflow execution, started by this
-- workflow execution, was canceled and closed.
-- ChildWorkflowExecutionTerminated: A child workflow execution, started by
-- this workflow execution, was terminated.
-- SignalExternalWorkflowExecutionInitiated: A request to signal an external
-- workflow was made. ExternalWorkflowExecutionSignaled: A signal, requested
-- by this workflow execution, was successfully delivered to the target
-- external workflow execution. SignalExternalWorkflowExecutionFailed: The
-- request to signal an external workflow execution failed.
-- RequestCancelExternalWorkflowExecutionInitiated: A request was made to
-- request the cancellation of an external workflow execution.
-- ExternalWorkflowExecutionCancelRequested: Request to cancel an external
-- workflow execution was successfully delivered to the target execution.
-- RequestCancelExternalWorkflowExecutionFailed: Request to cancel an external
-- workflow execution failed.
data HistoryEvent = HistoryEvent
    { _heEventTimestamp :: POSIX
      -- ^ The date and time when the event occurred.
    , _heEventType :: EventType
      -- ^ The type of the history event.
    , _heEventId :: Integer
      -- ^ The system generated id of the event. This id uniquely identifies
      -- the event with in the workflow execution history.
    , _heWorkflowExecutionStartedEventAttributes :: Maybe WorkflowExecutionStartedEventAttributes
      -- ^ If the event is of type WorkflowExecutionStarted then this member
      -- is set and provides detailed information about the event. It is
      -- not set for other event types.
    , _heWorkflowExecutionCompletedEventAttributes :: Maybe WorkflowExecutionCompletedEventAttributes
      -- ^ If the event is of type WorkflowExecutionCompleted then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heCompleteWorkflowExecutionFailedEventAttributes :: Maybe CompleteWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type CompleteWorkflowExecutionFailed then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heWorkflowExecutionFailedEventAttributes :: Maybe WorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type WorkflowExecutionFailed then this member
      -- is set and provides detailed information about the event. It is
      -- not set for other event types.
    , _heFailWorkflowExecutionFailedEventAttributes :: Maybe FailWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type FailWorkflowExecutionFailed then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heWorkflowExecutionTimedOutEventAttributes :: Maybe WorkflowExecutionTimedOutEventAttributes
      -- ^ If the event is of type WorkflowExecutionTimedOut then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heWorkflowExecutionCanceledEventAttributes :: Maybe WorkflowExecutionCanceledEventAttributes
      -- ^ If the event is of type WorkflowExecutionCanceled then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heCancelWorkflowExecutionFailedEventAttributes :: Maybe CancelWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type CancelWorkflowExecutionFailed then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heWorkflowExecutionContinuedAsNewEventAttributes :: Maybe WorkflowExecutionContinuedAsNewEventAttributes
      -- ^ If the event is of type WorkflowExecutionContinuedAsNew then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heContinueAsNewWorkflowExecutionFailedEventAttributes :: Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type ContinueAsNewWorkflowExecutionFailed then
      -- this member is set and provides detailed information about the
      -- event. It is not set for other event types.
    , _heWorkflowExecutionTerminatedEventAttributes :: Maybe WorkflowExecutionTerminatedEventAttributes
      -- ^ If the event is of type WorkflowExecutionTerminated then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heWorkflowExecutionCancelRequestedEventAttributes :: Maybe WorkflowExecutionCancelRequestedEventAttributes
      -- ^ If the event is of type WorkflowExecutionCancelRequested then
      -- this member is set and provides detailed information about the
      -- event. It is not set for other event types.
    , _heDecisionTaskScheduledEventAttributes :: Maybe DecisionTaskScheduledEventAttributes
      -- ^ If the event is of type DecisionTaskScheduled then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heDecisionTaskStartedEventAttributes :: Maybe DecisionTaskStartedEventAttributes
      -- ^ If the event is of type DecisionTaskStarted then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heDecisionTaskCompletedEventAttributes :: Maybe DecisionTaskCompletedEventAttributes
      -- ^ If the event is of type DecisionTaskCompleted then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heDecisionTaskTimedOutEventAttributes :: Maybe DecisionTaskTimedOutEventAttributes
      -- ^ If the event is of type DecisionTaskTimedOut then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heActivityTaskScheduledEventAttributes :: Maybe ActivityTaskScheduledEventAttributes
      -- ^ If the event is of type ActivityTaskScheduled then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heActivityTaskStartedEventAttributes :: Maybe ActivityTaskStartedEventAttributes
      -- ^ If the event is of type ActivityTaskStarted then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heActivityTaskCompletedEventAttributes :: Maybe ActivityTaskCompletedEventAttributes
      -- ^ If the event is of type ActivityTaskCompleted then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heActivityTaskFailedEventAttributes :: Maybe ActivityTaskFailedEventAttributes
      -- ^ If the event is of type ActivityTaskFailed then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heActivityTaskTimedOutEventAttributes :: Maybe ActivityTaskTimedOutEventAttributes
      -- ^ If the event is of type ActivityTaskTimedOut then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heActivityTaskCanceledEventAttributes :: Maybe ActivityTaskCanceledEventAttributes
      -- ^ If the event is of type ActivityTaskCanceled then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heActivityTaskCancelRequestedEventAttributes :: Maybe ActivityTaskCancelRequestedEventAttributes
      -- ^ If the event is of type ActivityTaskcancelRequested then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heWorkflowExecutionSignaledEventAttributes :: Maybe WorkflowExecutionSignaledEventAttributes
      -- ^ If the event is of type WorkflowExecutionSignaled then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heMarkerRecordedEventAttributes :: Maybe MarkerRecordedEventAttributes
      -- ^ If the event is of type MarkerRecorded then this member is set
      -- and provides detailed information about the event. It is not set
      -- for other event types.
    , _heRecordMarkerFailedEventAttributes :: Maybe RecordMarkerFailedEventAttributes
      -- ^ If the event is of type DecisionTaskFailed then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heTimerStartedEventAttributes :: Maybe TimerStartedEventAttributes
      -- ^ If the event is of type TimerStarted then this member is set and
      -- provides detailed information about the event. It is not set for
      -- other event types.
    , _heTimerFiredEventAttributes :: Maybe TimerFiredEventAttributes
      -- ^ If the event is of type TimerFired then this member is set and
      -- provides detailed information about the event. It is not set for
      -- other event types.
    , _heTimerCanceledEventAttributes :: Maybe TimerCanceledEventAttributes
      -- ^ If the event is of type TimerCanceled then this member is set and
      -- provides detailed information about the event. It is not set for
      -- other event types.
    , _heStartChildWorkflowExecutionInitiatedEventAttributes :: Maybe StartChildWorkflowExecutionInitiatedEventAttributes
      -- ^ If the event is of type StartChildWorkflowExecutionInitiated then
      -- this member is set and provides detailed information about the
      -- event. It is not set for other event types.
    , _heChildWorkflowExecutionStartedEventAttributes :: Maybe ChildWorkflowExecutionStartedEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionStarted then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heChildWorkflowExecutionCompletedEventAttributes :: Maybe ChildWorkflowExecutionCompletedEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionCompleted then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heChildWorkflowExecutionFailedEventAttributes :: Maybe ChildWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionFailed then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heChildWorkflowExecutionTimedOutEventAttributes :: Maybe ChildWorkflowExecutionTimedOutEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionTimedOut then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heChildWorkflowExecutionCanceledEventAttributes :: Maybe ChildWorkflowExecutionCanceledEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionCanceled then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heChildWorkflowExecutionTerminatedEventAttributes :: Maybe ChildWorkflowExecutionTerminatedEventAttributes
      -- ^ If the event is of type ChildWorkflowExecutionTerminated then
      -- this member is set and provides detailed information about the
      -- event. It is not set for other event types.
    , _heSignalExternalWorkflowExecutionInitiatedEventAttributes :: Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes
      -- ^ If the event is of type SignalExternalWorkflowExecutionInitiated
      -- then this member is set and provides detailed information about
      -- the event. It is not set for other event types.
    , _heExternalWorkflowExecutionSignaledEventAttributes :: Maybe ExternalWorkflowExecutionSignaledEventAttributes
      -- ^ If the event is of type ExternalWorkflowExecutionSignaled then
      -- this member is set and provides detailed information about the
      -- event. It is not set for other event types.
    , _heSignalExternalWorkflowExecutionFailedEventAttributes :: Maybe SignalExternalWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type SignalExternalWorkflowExecutionFailed
      -- then this member is set and provides detailed information about
      -- the event. It is not set for other event types.
    , _heExternalWorkflowExecutionCancelRequestedEventAttributes :: Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes
      -- ^ If the event is of type ExternalWorkflowExecutionCancelRequested
      -- then this member is set and provides detailed information about
      -- the event. It is not set for other event types.
    , _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
      -- ^ If the event is of type
      -- RequestCancelExternalWorkflowExecutionInitiated then this member
      -- is set and provides detailed information about the event. It is
      -- not set for other event types.
    , _heRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type
      -- RequestCancelExternalWorkflowExecutionFailed then this member is
      -- set and provides detailed information about the event. It is not
      -- set for other event types.
    , _heScheduleActivityTaskFailedEventAttributes :: Maybe ScheduleActivityTaskFailedEventAttributes
      -- ^ If the event is of type ScheduleActivityTaskFailed then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heRequestCancelActivityTaskFailedEventAttributes :: Maybe RequestCancelActivityTaskFailedEventAttributes
      -- ^ If the event is of type RequestCancelActivityTaskFailed then this
      -- member is set and provides detailed information about the event.
      -- It is not set for other event types.
    , _heStartTimerFailedEventAttributes :: Maybe StartTimerFailedEventAttributes
      -- ^ If the event is of type StartTimerFailed then this member is set
      -- and provides detailed information about the event. It is not set
      -- for other event types.
    , _heCancelTimerFailedEventAttributes :: Maybe CancelTimerFailedEventAttributes
      -- ^ If the event is of type CancelTimerFailed then this member is set
      -- and provides detailed information about the event. It is not set
      -- for other event types.
    , _heStartChildWorkflowExecutionFailedEventAttributes :: Maybe StartChildWorkflowExecutionFailedEventAttributes
      -- ^ If the event is of type StartChildWorkflowExecutionFailed then
      -- this member is set and provides detailed information about the
      -- event. It is not set for other event types.
    } deriving (Show, Generic)

-- | The date and time when the event occurred.
heEventTimestamp :: Lens' HistoryEvent (POSIX)
heEventTimestamp = lens _heEventTimestamp (\s a -> s { _heEventTimestamp = a })
{-# INLINE heEventTimestamp #-}

-- | The type of the history event.
heEventType :: Lens' HistoryEvent (EventType)
heEventType = lens _heEventType (\s a -> s { _heEventType = a })
{-# INLINE heEventType #-}

-- | The system generated id of the event. This id uniquely identifies the event
-- with in the workflow execution history.
heEventId :: Lens' HistoryEvent (Integer)
heEventId = lens _heEventId (\s a -> s { _heEventId = a })
{-# INLINE heEventId #-}

-- | If the event is of type WorkflowExecutionStarted then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionStartedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionStartedEventAttributes)
heWorkflowExecutionStartedEventAttributes = lens _heWorkflowExecutionStartedEventAttributes (\s a -> s { _heWorkflowExecutionStartedEventAttributes = a })
{-# INLINE heWorkflowExecutionStartedEventAttributes #-}

-- | If the event is of type WorkflowExecutionCompleted then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionCompletedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionCompletedEventAttributes)
heWorkflowExecutionCompletedEventAttributes = lens _heWorkflowExecutionCompletedEventAttributes (\s a -> s { _heWorkflowExecutionCompletedEventAttributes = a })
{-# INLINE heWorkflowExecutionCompletedEventAttributes #-}

-- | If the event is of type CompleteWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heCompleteWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe CompleteWorkflowExecutionFailedEventAttributes)
heCompleteWorkflowExecutionFailedEventAttributes = lens _heCompleteWorkflowExecutionFailedEventAttributes (\s a -> s { _heCompleteWorkflowExecutionFailedEventAttributes = a })
{-# INLINE heCompleteWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type WorkflowExecutionFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionFailedEventAttributes)
heWorkflowExecutionFailedEventAttributes = lens _heWorkflowExecutionFailedEventAttributes (\s a -> s { _heWorkflowExecutionFailedEventAttributes = a })
{-# INLINE heWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type FailWorkflowExecutionFailed then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heFailWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe FailWorkflowExecutionFailedEventAttributes)
heFailWorkflowExecutionFailedEventAttributes = lens _heFailWorkflowExecutionFailedEventAttributes (\s a -> s { _heFailWorkflowExecutionFailedEventAttributes = a })
{-# INLINE heFailWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type WorkflowExecutionTimedOut then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionTimedOutEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionTimedOutEventAttributes)
heWorkflowExecutionTimedOutEventAttributes = lens _heWorkflowExecutionTimedOutEventAttributes (\s a -> s { _heWorkflowExecutionTimedOutEventAttributes = a })
{-# INLINE heWorkflowExecutionTimedOutEventAttributes #-}

-- | If the event is of type WorkflowExecutionCanceled then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionCanceledEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionCanceledEventAttributes)
heWorkflowExecutionCanceledEventAttributes = lens _heWorkflowExecutionCanceledEventAttributes (\s a -> s { _heWorkflowExecutionCanceledEventAttributes = a })
{-# INLINE heWorkflowExecutionCanceledEventAttributes #-}

-- | If the event is of type CancelWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heCancelWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe CancelWorkflowExecutionFailedEventAttributes)
heCancelWorkflowExecutionFailedEventAttributes = lens _heCancelWorkflowExecutionFailedEventAttributes (\s a -> s { _heCancelWorkflowExecutionFailedEventAttributes = a })
{-# INLINE heCancelWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type WorkflowExecutionContinuedAsNew then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heWorkflowExecutionContinuedAsNewEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionContinuedAsNewEventAttributes)
heWorkflowExecutionContinuedAsNewEventAttributes = lens _heWorkflowExecutionContinuedAsNewEventAttributes (\s a -> s { _heWorkflowExecutionContinuedAsNewEventAttributes = a })
{-# INLINE heWorkflowExecutionContinuedAsNewEventAttributes #-}

-- | If the event is of type ContinueAsNewWorkflowExecutionFailed then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heContinueAsNewWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes)
heContinueAsNewWorkflowExecutionFailedEventAttributes = lens _heContinueAsNewWorkflowExecutionFailedEventAttributes (\s a -> s { _heContinueAsNewWorkflowExecutionFailedEventAttributes = a })
{-# INLINE heContinueAsNewWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type WorkflowExecutionTerminated then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionTerminatedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionTerminatedEventAttributes)
heWorkflowExecutionTerminatedEventAttributes = lens _heWorkflowExecutionTerminatedEventAttributes (\s a -> s { _heWorkflowExecutionTerminatedEventAttributes = a })
{-# INLINE heWorkflowExecutionTerminatedEventAttributes #-}

-- | If the event is of type WorkflowExecutionCancelRequested then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
heWorkflowExecutionCancelRequestedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionCancelRequestedEventAttributes)
heWorkflowExecutionCancelRequestedEventAttributes = lens _heWorkflowExecutionCancelRequestedEventAttributes (\s a -> s { _heWorkflowExecutionCancelRequestedEventAttributes = a })
{-# INLINE heWorkflowExecutionCancelRequestedEventAttributes #-}

-- | If the event is of type DecisionTaskScheduled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heDecisionTaskScheduledEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskScheduledEventAttributes)
heDecisionTaskScheduledEventAttributes = lens _heDecisionTaskScheduledEventAttributes (\s a -> s { _heDecisionTaskScheduledEventAttributes = a })
{-# INLINE heDecisionTaskScheduledEventAttributes #-}

-- | If the event is of type DecisionTaskStarted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heDecisionTaskStartedEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskStartedEventAttributes)
heDecisionTaskStartedEventAttributes = lens _heDecisionTaskStartedEventAttributes (\s a -> s { _heDecisionTaskStartedEventAttributes = a })
{-# INLINE heDecisionTaskStartedEventAttributes #-}

-- | If the event is of type DecisionTaskCompleted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heDecisionTaskCompletedEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskCompletedEventAttributes)
heDecisionTaskCompletedEventAttributes = lens _heDecisionTaskCompletedEventAttributes (\s a -> s { _heDecisionTaskCompletedEventAttributes = a })
{-# INLINE heDecisionTaskCompletedEventAttributes #-}

-- | If the event is of type DecisionTaskTimedOut then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heDecisionTaskTimedOutEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskTimedOutEventAttributes)
heDecisionTaskTimedOutEventAttributes = lens _heDecisionTaskTimedOutEventAttributes (\s a -> s { _heDecisionTaskTimedOutEventAttributes = a })
{-# INLINE heDecisionTaskTimedOutEventAttributes #-}

-- | If the event is of type ActivityTaskScheduled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskScheduledEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskScheduledEventAttributes)
heActivityTaskScheduledEventAttributes = lens _heActivityTaskScheduledEventAttributes (\s a -> s { _heActivityTaskScheduledEventAttributes = a })
{-# INLINE heActivityTaskScheduledEventAttributes #-}

-- | If the event is of type ActivityTaskStarted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskStartedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskStartedEventAttributes)
heActivityTaskStartedEventAttributes = lens _heActivityTaskStartedEventAttributes (\s a -> s { _heActivityTaskStartedEventAttributes = a })
{-# INLINE heActivityTaskStartedEventAttributes #-}

-- | If the event is of type ActivityTaskCompleted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskCompletedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskCompletedEventAttributes)
heActivityTaskCompletedEventAttributes = lens _heActivityTaskCompletedEventAttributes (\s a -> s { _heActivityTaskCompletedEventAttributes = a })
{-# INLINE heActivityTaskCompletedEventAttributes #-}

-- | If the event is of type ActivityTaskFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskFailedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskFailedEventAttributes)
heActivityTaskFailedEventAttributes = lens _heActivityTaskFailedEventAttributes (\s a -> s { _heActivityTaskFailedEventAttributes = a })
{-# INLINE heActivityTaskFailedEventAttributes #-}

-- | If the event is of type ActivityTaskTimedOut then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskTimedOutEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskTimedOutEventAttributes)
heActivityTaskTimedOutEventAttributes = lens _heActivityTaskTimedOutEventAttributes (\s a -> s { _heActivityTaskTimedOutEventAttributes = a })
{-# INLINE heActivityTaskTimedOutEventAttributes #-}

-- | If the event is of type ActivityTaskCanceled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskCanceledEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskCanceledEventAttributes)
heActivityTaskCanceledEventAttributes = lens _heActivityTaskCanceledEventAttributes (\s a -> s { _heActivityTaskCanceledEventAttributes = a })
{-# INLINE heActivityTaskCanceledEventAttributes #-}

-- | If the event is of type ActivityTaskcancelRequested then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskCancelRequestedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskCancelRequestedEventAttributes)
heActivityTaskCancelRequestedEventAttributes = lens _heActivityTaskCancelRequestedEventAttributes (\s a -> s { _heActivityTaskCancelRequestedEventAttributes = a })
{-# INLINE heActivityTaskCancelRequestedEventAttributes #-}

-- | If the event is of type WorkflowExecutionSignaled then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionSignaledEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionSignaledEventAttributes)
heWorkflowExecutionSignaledEventAttributes = lens _heWorkflowExecutionSignaledEventAttributes (\s a -> s { _heWorkflowExecutionSignaledEventAttributes = a })
{-# INLINE heWorkflowExecutionSignaledEventAttributes #-}

-- | If the event is of type MarkerRecorded then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heMarkerRecordedEventAttributes :: Lens' HistoryEvent (Maybe MarkerRecordedEventAttributes)
heMarkerRecordedEventAttributes = lens _heMarkerRecordedEventAttributes (\s a -> s { _heMarkerRecordedEventAttributes = a })
{-# INLINE heMarkerRecordedEventAttributes #-}

-- | If the event is of type DecisionTaskFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heRecordMarkerFailedEventAttributes :: Lens' HistoryEvent (Maybe RecordMarkerFailedEventAttributes)
heRecordMarkerFailedEventAttributes = lens _heRecordMarkerFailedEventAttributes (\s a -> s { _heRecordMarkerFailedEventAttributes = a })
{-# INLINE heRecordMarkerFailedEventAttributes #-}

-- | If the event is of type TimerStarted then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heTimerStartedEventAttributes :: Lens' HistoryEvent (Maybe TimerStartedEventAttributes)
heTimerStartedEventAttributes = lens _heTimerStartedEventAttributes (\s a -> s { _heTimerStartedEventAttributes = a })
{-# INLINE heTimerStartedEventAttributes #-}

-- | If the event is of type TimerFired then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heTimerFiredEventAttributes :: Lens' HistoryEvent (Maybe TimerFiredEventAttributes)
heTimerFiredEventAttributes = lens _heTimerFiredEventAttributes (\s a -> s { _heTimerFiredEventAttributes = a })
{-# INLINE heTimerFiredEventAttributes #-}

-- | If the event is of type TimerCanceled then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heTimerCanceledEventAttributes :: Lens' HistoryEvent (Maybe TimerCanceledEventAttributes)
heTimerCanceledEventAttributes = lens _heTimerCanceledEventAttributes (\s a -> s { _heTimerCanceledEventAttributes = a })
{-# INLINE heTimerCanceledEventAttributes #-}

-- | If the event is of type StartChildWorkflowExecutionInitiated then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heStartChildWorkflowExecutionInitiatedEventAttributes :: Lens' HistoryEvent (Maybe StartChildWorkflowExecutionInitiatedEventAttributes)
heStartChildWorkflowExecutionInitiatedEventAttributes = lens _heStartChildWorkflowExecutionInitiatedEventAttributes (\s a -> s { _heStartChildWorkflowExecutionInitiatedEventAttributes = a })
{-# INLINE heStartChildWorkflowExecutionInitiatedEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionStarted then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionStartedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionStartedEventAttributes)
heChildWorkflowExecutionStartedEventAttributes = lens _heChildWorkflowExecutionStartedEventAttributes (\s a -> s { _heChildWorkflowExecutionStartedEventAttributes = a })
{-# INLINE heChildWorkflowExecutionStartedEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionCompleted then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionCompletedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionCompletedEventAttributes)
heChildWorkflowExecutionCompletedEventAttributes = lens _heChildWorkflowExecutionCompletedEventAttributes (\s a -> s { _heChildWorkflowExecutionCompletedEventAttributes = a })
{-# INLINE heChildWorkflowExecutionCompletedEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionFailedEventAttributes)
heChildWorkflowExecutionFailedEventAttributes = lens _heChildWorkflowExecutionFailedEventAttributes (\s a -> s { _heChildWorkflowExecutionFailedEventAttributes = a })
{-# INLINE heChildWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionTimedOut then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionTimedOutEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionTimedOutEventAttributes)
heChildWorkflowExecutionTimedOutEventAttributes = lens _heChildWorkflowExecutionTimedOutEventAttributes (\s a -> s { _heChildWorkflowExecutionTimedOutEventAttributes = a })
{-# INLINE heChildWorkflowExecutionTimedOutEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionCanceled then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionCanceledEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionCanceledEventAttributes)
heChildWorkflowExecutionCanceledEventAttributes = lens _heChildWorkflowExecutionCanceledEventAttributes (\s a -> s { _heChildWorkflowExecutionCanceledEventAttributes = a })
{-# INLINE heChildWorkflowExecutionCanceledEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionTerminated then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionTerminatedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionTerminatedEventAttributes)
heChildWorkflowExecutionTerminatedEventAttributes = lens _heChildWorkflowExecutionTerminatedEventAttributes (\s a -> s { _heChildWorkflowExecutionTerminatedEventAttributes = a })
{-# INLINE heChildWorkflowExecutionTerminatedEventAttributes #-}

-- | If the event is of type SignalExternalWorkflowExecutionInitiated then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heSignalExternalWorkflowExecutionInitiatedEventAttributes :: Lens' HistoryEvent (Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes)
heSignalExternalWorkflowExecutionInitiatedEventAttributes = lens _heSignalExternalWorkflowExecutionInitiatedEventAttributes (\s a -> s { _heSignalExternalWorkflowExecutionInitiatedEventAttributes = a })
{-# INLINE heSignalExternalWorkflowExecutionInitiatedEventAttributes #-}

-- | If the event is of type ExternalWorkflowExecutionSignaled then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
heExternalWorkflowExecutionSignaledEventAttributes :: Lens' HistoryEvent (Maybe ExternalWorkflowExecutionSignaledEventAttributes)
heExternalWorkflowExecutionSignaledEventAttributes = lens _heExternalWorkflowExecutionSignaledEventAttributes (\s a -> s { _heExternalWorkflowExecutionSignaledEventAttributes = a })
{-# INLINE heExternalWorkflowExecutionSignaledEventAttributes #-}

-- | If the event is of type SignalExternalWorkflowExecutionFailed then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heSignalExternalWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe SignalExternalWorkflowExecutionFailedEventAttributes)
heSignalExternalWorkflowExecutionFailedEventAttributes = lens _heSignalExternalWorkflowExecutionFailedEventAttributes (\s a -> s { _heSignalExternalWorkflowExecutionFailedEventAttributes = a })
{-# INLINE heSignalExternalWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type ExternalWorkflowExecutionCancelRequested then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heExternalWorkflowExecutionCancelRequestedEventAttributes :: Lens' HistoryEvent (Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes)
heExternalWorkflowExecutionCancelRequestedEventAttributes = lens _heExternalWorkflowExecutionCancelRequestedEventAttributes (\s a -> s { _heExternalWorkflowExecutionCancelRequestedEventAttributes = a })
{-# INLINE heExternalWorkflowExecutionCancelRequestedEventAttributes #-}

-- | If the event is of type RequestCancelExternalWorkflowExecutionInitiated
-- then this member is set and provides detailed information about the event.
-- It is not set for other event types.
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Lens' HistoryEvent (Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes = lens _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes (\s a -> s { _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes = a })
{-# INLINE heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes #-}

-- | If the event is of type RequestCancelExternalWorkflowExecutionFailed then
-- this member is set and provides detailed information about the event. It is
-- not set for other event types.
heRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes)
heRequestCancelExternalWorkflowExecutionFailedEventAttributes = lens _heRequestCancelExternalWorkflowExecutionFailedEventAttributes (\s a -> s { _heRequestCancelExternalWorkflowExecutionFailedEventAttributes = a })
{-# INLINE heRequestCancelExternalWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type ScheduleActivityTaskFailed then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heScheduleActivityTaskFailedEventAttributes :: Lens' HistoryEvent (Maybe ScheduleActivityTaskFailedEventAttributes)
heScheduleActivityTaskFailedEventAttributes = lens _heScheduleActivityTaskFailedEventAttributes (\s a -> s { _heScheduleActivityTaskFailedEventAttributes = a })
{-# INLINE heScheduleActivityTaskFailedEventAttributes #-}

-- | If the event is of type RequestCancelActivityTaskFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heRequestCancelActivityTaskFailedEventAttributes :: Lens' HistoryEvent (Maybe RequestCancelActivityTaskFailedEventAttributes)
heRequestCancelActivityTaskFailedEventAttributes = lens _heRequestCancelActivityTaskFailedEventAttributes (\s a -> s { _heRequestCancelActivityTaskFailedEventAttributes = a })
{-# INLINE heRequestCancelActivityTaskFailedEventAttributes #-}

-- | If the event is of type StartTimerFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heStartTimerFailedEventAttributes :: Lens' HistoryEvent (Maybe StartTimerFailedEventAttributes)
heStartTimerFailedEventAttributes = lens _heStartTimerFailedEventAttributes (\s a -> s { _heStartTimerFailedEventAttributes = a })
{-# INLINE heStartTimerFailedEventAttributes #-}

-- | If the event is of type CancelTimerFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heCancelTimerFailedEventAttributes :: Lens' HistoryEvent (Maybe CancelTimerFailedEventAttributes)
heCancelTimerFailedEventAttributes = lens _heCancelTimerFailedEventAttributes (\s a -> s { _heCancelTimerFailedEventAttributes = a })
{-# INLINE heCancelTimerFailedEventAttributes #-}

-- | If the event is of type StartChildWorkflowExecutionFailed then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
heStartChildWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe StartChildWorkflowExecutionFailedEventAttributes)
heStartChildWorkflowExecutionFailedEventAttributes = lens _heStartChildWorkflowExecutionFailedEventAttributes (\s a -> s { _heStartChildWorkflowExecutionFailedEventAttributes = a })
{-# INLINE heStartChildWorkflowExecutionFailedEventAttributes #-}

instance FromJSON HistoryEvent

-- | If the event is of type MarkerRecorded then this member is set and provides
-- detailed information about the event. It is not set for other event types.
data MarkerRecordedEventAttributes = MarkerRecordedEventAttributes
    { _mreaMarkerName :: Text
      -- ^ The name of the marker.
    , _mreaDetails :: Maybe Text
      -- ^ Details of the marker (if any).
    , _mreaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the RecordMarker decision that
      -- requested this marker. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    } deriving (Show, Generic)

-- | The name of the marker.
mreaMarkerName :: Lens' MarkerRecordedEventAttributes (Text)
mreaMarkerName = lens _mreaMarkerName (\s a -> s { _mreaMarkerName = a })
{-# INLINE mreaMarkerName #-}

-- | Details of the marker (if any).
mreaDetails :: Lens' MarkerRecordedEventAttributes (Maybe Text)
mreaDetails = lens _mreaDetails (\s a -> s { _mreaDetails = a })
{-# INLINE mreaDetails #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RecordMarker decision that requested this marker.
-- This information can be useful for diagnosing problems by tracing back the
-- cause of events.
mreaDecisionTaskCompletedEventId :: Lens' MarkerRecordedEventAttributes (Integer)
mreaDecisionTaskCompletedEventId = lens _mreaDecisionTaskCompletedEventId (\s a -> s { _mreaDecisionTaskCompletedEventId = a })
{-# INLINE mreaDecisionTaskCompletedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'MarkerRecordedEventAttributes' data type to populate a request.
mkMarkerRecordedEventAttributes :: Text -- ^ 'mreaMarkerName'
                                -> Integer -- ^ 'mreaDecisionTaskCompletedEventId'
                                -> MarkerRecordedEventAttributes
mkMarkerRecordedEventAttributes p1 p2 = MarkerRecordedEventAttributes
    { _mreaMarkerName = p1
    , _mreaDetails = Nothing
    , _mreaDecisionTaskCompletedEventId = p3
    }
{-# INLINE mkMarkerRecordedEventAttributes #-}

instance FromJSON MarkerRecordedEventAttributes

instance ToJSON MarkerRecordedEventAttributes

-- | Provides details of the RecordMarker decision. It is not set for other
-- decision types.
data RecordMarkerDecisionAttributes = RecordMarkerDecisionAttributes
    { _rmdaMarkerName :: Text
      -- ^ The name of the marker. This file is required.
    , _rmdaDetails :: Maybe Text
      -- ^ Optional details of the marker.
    } deriving (Show, Generic)

-- | The name of the marker. This file is required.
rmdaMarkerName :: Lens' RecordMarkerDecisionAttributes (Text)
rmdaMarkerName = lens _rmdaMarkerName (\s a -> s { _rmdaMarkerName = a })
{-# INLINE rmdaMarkerName #-}

-- | Optional details of the marker.
rmdaDetails :: Lens' RecordMarkerDecisionAttributes (Maybe Text)
rmdaDetails = lens _rmdaDetails (\s a -> s { _rmdaDetails = a })
{-# INLINE rmdaDetails #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RecordMarkerDecisionAttributes' data type to populate a request.
mkRecordMarkerDecisionAttributes :: Text -- ^ 'rmdaMarkerName'
                                 -> RecordMarkerDecisionAttributes
mkRecordMarkerDecisionAttributes p1 = RecordMarkerDecisionAttributes
    { _rmdaMarkerName = p1
    , _rmdaDetails = Nothing
    }
{-# INLINE mkRecordMarkerDecisionAttributes #-}

instance FromJSON RecordMarkerDecisionAttributes

instance ToJSON RecordMarkerDecisionAttributes

-- | If the event is of type DecisionTaskFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data RecordMarkerFailedEventAttributes = RecordMarkerFailedEventAttributes
    { _rmfeaMarkerName :: Text
      -- ^ The marker's name.
    , _rmfeaCause :: RecordMarkerFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _rmfeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the RecordMarkerFailed decision
      -- for this cancellation request. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    } deriving (Show, Generic)

-- | The marker's name.
rmfeaMarkerName :: Lens' RecordMarkerFailedEventAttributes (Text)
rmfeaMarkerName = lens _rmfeaMarkerName (\s a -> s { _rmfeaMarkerName = a })
{-# INLINE rmfeaMarkerName #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
rmfeaCause :: Lens' RecordMarkerFailedEventAttributes (RecordMarkerFailedCause)
rmfeaCause = lens _rmfeaCause (\s a -> s { _rmfeaCause = a })
{-# INLINE rmfeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RecordMarkerFailed decision for this cancellation
-- request. This information can be useful for diagnosing problems by tracing
-- back the cause of events.
rmfeaDecisionTaskCompletedEventId :: Lens' RecordMarkerFailedEventAttributes (Integer)
rmfeaDecisionTaskCompletedEventId = lens _rmfeaDecisionTaskCompletedEventId (\s a -> s { _rmfeaDecisionTaskCompletedEventId = a })
{-# INLINE rmfeaDecisionTaskCompletedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RecordMarkerFailedEventAttributes' data type to populate a request.
mkRecordMarkerFailedEventAttributes :: Text -- ^ 'rmfeaMarkerName'
                                    -> RecordMarkerFailedCause -- ^ 'rmfeaCause'
                                    -> Integer -- ^ 'rmfeaDecisionTaskCompletedEventId'
                                    -> RecordMarkerFailedEventAttributes
mkRecordMarkerFailedEventAttributes p1 p2 p3 = RecordMarkerFailedEventAttributes
    { _rmfeaMarkerName = p1
    , _rmfeaCause = p2
    , _rmfeaDecisionTaskCompletedEventId = p3
    }
{-# INLINE mkRecordMarkerFailedEventAttributes #-}

instance FromJSON RecordMarkerFailedEventAttributes

instance ToJSON RecordMarkerFailedEventAttributes

-- | If the event is of type RequestCancelActivityTaskFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data RequestCancelActivityTaskFailedEventAttributes = RequestCancelActivityTaskFailedEventAttributes
    { _rcatfeaActivityId :: Text
      -- ^ The activityId provided in the RequestCancelActivityTask decision
      -- that failed.
    , _rcatfeaCause :: RequestCancelActivityTaskFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _rcatfeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the RequestCancelActivityTask
      -- decision for this cancellation request. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    } deriving (Show, Generic)

-- | The activityId provided in the RequestCancelActivityTask decision that
-- failed.
rcatfeaActivityId :: Lens' RequestCancelActivityTaskFailedEventAttributes (Text)
rcatfeaActivityId = lens _rcatfeaActivityId (\s a -> s { _rcatfeaActivityId = a })
{-# INLINE rcatfeaActivityId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
rcatfeaCause :: Lens' RequestCancelActivityTaskFailedEventAttributes (RequestCancelActivityTaskFailedCause)
rcatfeaCause = lens _rcatfeaCause (\s a -> s { _rcatfeaCause = a })
{-# INLINE rcatfeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RequestCancelActivityTask decision for this
-- cancellation request. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
rcatfeaDecisionTaskCompletedEventId :: Lens' RequestCancelActivityTaskFailedEventAttributes (Integer)
rcatfeaDecisionTaskCompletedEventId = lens _rcatfeaDecisionTaskCompletedEventId (\s a -> s { _rcatfeaDecisionTaskCompletedEventId = a })
{-# INLINE rcatfeaDecisionTaskCompletedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RequestCancelActivityTaskFailedEventAttributes' data type to populate a request.
mkRequestCancelActivityTaskFailedEventAttributes :: Text -- ^ 'rcatfeaActivityId'
                                                 -> RequestCancelActivityTaskFailedCause -- ^ 'rcatfeaCause'
                                                 -> Integer -- ^ 'rcatfeaDecisionTaskCompletedEventId'
                                                 -> RequestCancelActivityTaskFailedEventAttributes
mkRequestCancelActivityTaskFailedEventAttributes p1 p2 p3 = RequestCancelActivityTaskFailedEventAttributes
    { _rcatfeaActivityId = p1
    , _rcatfeaCause = p2
    , _rcatfeaDecisionTaskCompletedEventId = p3
    }
{-# INLINE mkRequestCancelActivityTaskFailedEventAttributes #-}

instance FromJSON RequestCancelActivityTaskFailedEventAttributes

instance ToJSON RequestCancelActivityTaskFailedEventAttributes

-- | Provides details of the RequestCancelExternalWorkflowExecution decision. It
-- is not set for other decision types.
data RequestCancelExternalWorkflowExecutionDecisionAttributes = RequestCancelExternalWorkflowExecutionDecisionAttributes
    { _rcewedaWorkflowId :: Text
      -- ^ The workflowId of the external workflow execution to cancel. This
      -- field is required.
    , _rcewedaRunId :: Maybe Text
      -- ^ The runId of the external workflow execution to cancel.
    , _rcewedaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent workflow tasks.
    } deriving (Show, Generic)

-- | The workflowId of the external workflow execution to cancel. This field is
-- required.
rcewedaWorkflowId :: Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Text)
rcewedaWorkflowId = lens _rcewedaWorkflowId (\s a -> s { _rcewedaWorkflowId = a })
{-# INLINE rcewedaWorkflowId #-}

-- | The runId of the external workflow execution to cancel.
rcewedaRunId :: Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Maybe Text)
rcewedaRunId = lens _rcewedaRunId (\s a -> s { _rcewedaRunId = a })
{-# INLINE rcewedaRunId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
rcewedaControl :: Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Maybe Text)
rcewedaControl = lens _rcewedaControl (\s a -> s { _rcewedaControl = a })
{-# INLINE rcewedaControl #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RequestCancelExternalWorkflowExecutionDecisionAttributes' data type to populate a request.
mkRequestCancelExternalWorkflowExecutionDecisionAttributes :: Text -- ^ 'rcewedaWorkflowId'
                                                           -> RequestCancelExternalWorkflowExecutionDecisionAttributes
mkRequestCancelExternalWorkflowExecutionDecisionAttributes p1 = RequestCancelExternalWorkflowExecutionDecisionAttributes
    { _rcewedaWorkflowId = p1
    , _rcewedaRunId = Nothing
    , _rcewedaControl = Nothing
    }
{-# INLINE mkRequestCancelExternalWorkflowExecutionDecisionAttributes #-}

instance FromJSON RequestCancelExternalWorkflowExecutionDecisionAttributes

instance ToJSON RequestCancelExternalWorkflowExecutionDecisionAttributes

-- | If the event is of type RequestCancelExternalWorkflowExecutionFailed then
-- this member is set and provides detailed information about the event. It is
-- not set for other event types.
data RequestCancelExternalWorkflowExecutionFailedEventAttributes = RequestCancelExternalWorkflowExecutionFailedEventAttributes
    { _rcewefeaWorkflowId :: Text
      -- ^ The workflowId of the external workflow to which the cancel
      -- request was to be delivered.
    , _rcewefeaRunId :: Maybe Text
      -- ^ The runId of the external workflow execution.
    , _rcewefeaCause :: RequestCancelExternalWorkflowExecutionFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _rcewefeaInitiatedEventId :: Integer
      -- ^ The id of the RequestCancelExternalWorkflowExecutionInitiated
      -- event corresponding to the RequestCancelExternalWorkflowExecution
      -- decision to cancel this external workflow execution. This
      -- information can be useful for diagnosing problems by tracing back
      -- the chain of events leading up to this event.
    , _rcewefeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the
      -- RequestCancelExternalWorkflowExecution decision for this
      -- cancellation request. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    , _rcewefeaControl :: Maybe Text
    } deriving (Show, Generic)

-- | The workflowId of the external workflow to which the cancel request was to
-- be delivered.
rcewefeaWorkflowId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (Text)
rcewefeaWorkflowId = lens _rcewefeaWorkflowId (\s a -> s { _rcewefeaWorkflowId = a })
{-# INLINE rcewefeaWorkflowId #-}

-- | The runId of the external workflow execution.
rcewefeaRunId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
rcewefeaRunId = lens _rcewefeaRunId (\s a -> s { _rcewefeaRunId = a })
{-# INLINE rcewefeaRunId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
rcewefeaCause :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (RequestCancelExternalWorkflowExecutionFailedCause)
rcewefeaCause = lens _rcewefeaCause (\s a -> s { _rcewefeaCause = a })
{-# INLINE rcewefeaCause #-}

-- | The id of the RequestCancelExternalWorkflowExecutionInitiated event
-- corresponding to the RequestCancelExternalWorkflowExecution decision to
-- cancel this external workflow execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
rcewefeaInitiatedEventId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (Integer)
rcewefeaInitiatedEventId = lens _rcewefeaInitiatedEventId (\s a -> s { _rcewefeaInitiatedEventId = a })
{-# INLINE rcewefeaInitiatedEventId #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RequestCancelExternalWorkflowExecution decision
-- for this cancellation request. This information can be useful for
-- diagnosing problems by tracing back the cause of events.
rcewefeaDecisionTaskCompletedEventId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (Integer)
rcewefeaDecisionTaskCompletedEventId = lens _rcewefeaDecisionTaskCompletedEventId (\s a -> s { _rcewefeaDecisionTaskCompletedEventId = a })
{-# INLINE rcewefeaDecisionTaskCompletedEventId #-}

rcewefeaControl :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
rcewefeaControl = lens _rcewefeaControl (\s a -> s { _rcewefeaControl = a })
{-# INLINE rcewefeaControl #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RequestCancelExternalWorkflowExecutionFailedEventAttributes' data type to populate a request.
mkRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Text -- ^ 'rcewefeaWorkflowId'
                                                              -> RequestCancelExternalWorkflowExecutionFailedCause -- ^ 'rcewefeaCause'
                                                              -> Integer -- ^ 'rcewefeaInitiatedEventId'
                                                              -> Integer -- ^ 'rcewefeaDecisionTaskCompletedEventId'
                                                              -> RequestCancelExternalWorkflowExecutionFailedEventAttributes
mkRequestCancelExternalWorkflowExecutionFailedEventAttributes p1 p2 p3 p4 = RequestCancelExternalWorkflowExecutionFailedEventAttributes
    { _rcewefeaWorkflowId = p1
    , _rcewefeaRunId = Nothing
    , _rcewefeaCause = p3
    , _rcewefeaInitiatedEventId = p4
    , _rcewefeaDecisionTaskCompletedEventId = p5
    , _rcewefeaControl = Nothing
    }
{-# INLINE mkRequestCancelExternalWorkflowExecutionFailedEventAttributes #-}

instance FromJSON RequestCancelExternalWorkflowExecutionFailedEventAttributes

instance ToJSON RequestCancelExternalWorkflowExecutionFailedEventAttributes

-- | If the event is of type RequestCancelExternalWorkflowExecutionInitiated
-- then this member is set and provides detailed information about the event.
-- It is not set for other event types.
data RequestCancelExternalWorkflowExecutionInitiatedEventAttributes = RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    { _rceweieaWorkflowId :: Text
      -- ^ The workflowId of the external workflow execution to be canceled.
    , _rceweieaRunId :: Maybe Text
      -- ^ The runId of the external workflow execution to be canceled.
    , _rceweieaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the
      -- RequestCancelExternalWorkflowExecution decision for this
      -- cancellation request. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    , _rceweieaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent workflow tasks.
    } deriving (Show, Generic)

-- | The workflowId of the external workflow execution to be canceled.
rceweieaWorkflowId :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Text)
rceweieaWorkflowId = lens _rceweieaWorkflowId (\s a -> s { _rceweieaWorkflowId = a })
{-# INLINE rceweieaWorkflowId #-}

-- | The runId of the external workflow execution to be canceled.
rceweieaRunId :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
rceweieaRunId = lens _rceweieaRunId (\s a -> s { _rceweieaRunId = a })
{-# INLINE rceweieaRunId #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RequestCancelExternalWorkflowExecution decision
-- for this cancellation request. This information can be useful for
-- diagnosing problems by tracing back the cause of events.
rceweieaDecisionTaskCompletedEventId :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Integer)
rceweieaDecisionTaskCompletedEventId = lens _rceweieaDecisionTaskCompletedEventId (\s a -> s { _rceweieaDecisionTaskCompletedEventId = a })
{-# INLINE rceweieaDecisionTaskCompletedEventId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
rceweieaControl :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
rceweieaControl = lens _rceweieaControl (\s a -> s { _rceweieaControl = a })
{-# INLINE rceweieaControl #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RequestCancelExternalWorkflowExecutionInitiatedEventAttributes' data type to populate a request.
mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Text -- ^ 'rceweieaWorkflowId'
                                                                 -> Integer -- ^ 'rceweieaDecisionTaskCompletedEventId'
                                                                 -> RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes p1 p2 = RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    { _rceweieaWorkflowId = p1
    , _rceweieaRunId = Nothing
    , _rceweieaDecisionTaskCompletedEventId = p3
    , _rceweieaControl = Nothing
    }
{-# INLINE mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes #-}

instance FromJSON RequestCancelExternalWorkflowExecutionInitiatedEventAttributes

instance ToJSON RequestCancelExternalWorkflowExecutionInitiatedEventAttributes

-- | Provides details of the ScheduleActivityTask decision. It is not set for
-- other decision types.
data ScheduleActivityTaskDecisionAttributes = ScheduleActivityTaskDecisionAttributes
    { _satdaActivityType :: ActivityType
      -- ^ The type of the activity task to schedule. This field is
      -- required.
    , _satdaActivityId :: Text
      -- ^ The activityId of the activity task. This field is required. The
      -- specified string must not start or end with whitespace. It must
      -- not contain a : (colon), / (slash), | (vertical bar), or any
      -- control characters (\u0000-\u001f | \u007f - \u009f). Also, it
      -- must not contain the literal string &quot;arn&quot;.
    , _satdaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent workflow tasks. This data is not sent to
      -- the activity.
    , _satdaInput :: Maybe Text
      -- ^ The input provided to the activity task.
    , _satdaScheduleToCloseTimeout :: Maybe Text
      -- ^ The maximum duration for this activity task. The valid values are
      -- integers greater than or equal to 0. An integer value can be used
      -- to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration. A schedule-to-close timeout for this
      -- activity task must be specified either as a default for the
      -- activity type or through this field. If neither this field is set
      -- nor a default schedule-to-close timeout was specified at
      -- registration time then a fault will be returned.
    , _satdaTaskList :: Maybe TaskList
      -- ^ If set, specifies the name of the task list in which to schedule
      -- the activity task. If not specified, the defaultTaskList
      -- registered with the activity type will be used. A task list for
      -- this activity task must be specified either as a default for the
      -- activity type or through this field. If neither this field is set
      -- nor a default task list was specified at registration time then a
      -- fault will be returned. The specified string must not start or
      -- end with whitespace. It must not contain a : (colon), / (slash),
      -- | (vertical bar), or any control characters (\u0000-\u001f |
      -- \u007f - \u009f). Also, it must not contain the literal string
      -- &quot;arn&quot;.
    , _satdaScheduleToStartTimeout :: Maybe Text
      -- ^ If set, specifies the maximum duration the activity task can wait
      -- to be assigned to a worker. This overrides the default
      -- schedule-to-start timeout specified when registering the activity
      -- type using RegisterActivityType. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify
      -- unlimited duration. A schedule-to-start timeout for this activity
      -- task must be specified either as a default for the activity type
      -- or through this field. If neither this field is set nor a default
      -- schedule-to-start timeout was specified at registration time then
      -- a fault will be returned.
    , _satdaStartToCloseTimeout :: Maybe Text
      -- ^ If set, specifies the maximum duration a worker may take to
      -- process this activity task. This overrides the default
      -- start-to-close timeout specified when registering the activity
      -- type using RegisterActivityType. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify
      -- unlimited duration. A start-to-close timeout for this activity
      -- task must be specified either as a default for the activity type
      -- or through this field. If neither this field is set nor a default
      -- start-to-close timeout was specified at registration time then a
      -- fault will be returned.
    , _satdaHeartbeatTimeout :: Maybe Text
      -- ^ If set, specifies the maximum time before which a worker
      -- processing a task of this type must report progress by calling
      -- RecordActivityTaskHeartbeat. If the timeout is exceeded, the
      -- activity task is automatically timed out. If the worker
      -- subsequently attempts to record a heartbeat or returns a result,
      -- it will be ignored. This overrides the default heartbeat timeout
      -- specified when registering the activity type using
      -- RegisterActivityType. The valid values are integers greater than
      -- or equal to 0. An integer value can be used to specify the
      -- duration in seconds while NONE can be used to specify unlimited
      -- duration.
    } deriving (Show, Generic)

-- | The type of the activity task to schedule. This field is required.
satdaActivityType :: Lens' ScheduleActivityTaskDecisionAttributes (ActivityType)
satdaActivityType = lens _satdaActivityType (\s a -> s { _satdaActivityType = a })
{-# INLINE satdaActivityType #-}

-- | The activityId of the activity task. This field is required. The specified
-- string must not start or end with whitespace. It must not contain a :
-- (colon), / (slash), | (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string &quot;arn&quot;.
satdaActivityId :: Lens' ScheduleActivityTaskDecisionAttributes (Text)
satdaActivityId = lens _satdaActivityId (\s a -> s { _satdaActivityId = a })
{-# INLINE satdaActivityId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks. This data is not sent to the activity.
satdaControl :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaControl = lens _satdaControl (\s a -> s { _satdaControl = a })
{-# INLINE satdaControl #-}

-- | The input provided to the activity task.
satdaInput :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaInput = lens _satdaInput (\s a -> s { _satdaInput = a })
{-# INLINE satdaInput #-}

-- | The maximum duration for this activity task. The valid values are integers
-- greater than or equal to 0. An integer value can be used to specify the
-- duration in seconds while NONE can be used to specify unlimited duration. A
-- schedule-to-close timeout for this activity task must be specified either
-- as a default for the activity type or through this field. If neither this
-- field is set nor a default schedule-to-close timeout was specified at
-- registration time then a fault will be returned.
satdaScheduleToCloseTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaScheduleToCloseTimeout = lens _satdaScheduleToCloseTimeout (\s a -> s { _satdaScheduleToCloseTimeout = a })
{-# INLINE satdaScheduleToCloseTimeout #-}

-- | If set, specifies the name of the task list in which to schedule the
-- activity task. If not specified, the defaultTaskList registered with the
-- activity type will be used. A task list for this activity task must be
-- specified either as a default for the activity type or through this field.
-- If neither this field is set nor a default task list was specified at
-- registration time then a fault will be returned. The specified string must
-- not start or end with whitespace. It must not contain a : (colon), /
-- (slash), | (vertical bar), or any control characters (\u0000-\u001f |
-- \u007f - \u009f). Also, it must not contain the literal string
-- &quot;arn&quot;.
satdaTaskList :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe TaskList)
satdaTaskList = lens _satdaTaskList (\s a -> s { _satdaTaskList = a })
{-# INLINE satdaTaskList #-}

-- | If set, specifies the maximum duration the activity task can wait to be
-- assigned to a worker. This overrides the default schedule-to-start timeout
-- specified when registering the activity type using RegisterActivityType.
-- The valid values are integers greater than or equal to 0. An integer value
-- can be used to specify the duration in seconds while NONE can be used to
-- specify unlimited duration. A schedule-to-start timeout for this activity
-- task must be specified either as a default for the activity type or through
-- this field. If neither this field is set nor a default schedule-to-start
-- timeout was specified at registration time then a fault will be returned.
satdaScheduleToStartTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaScheduleToStartTimeout = lens _satdaScheduleToStartTimeout (\s a -> s { _satdaScheduleToStartTimeout = a })
{-# INLINE satdaScheduleToStartTimeout #-}

-- | If set, specifies the maximum duration a worker may take to process this
-- activity task. This overrides the default start-to-close timeout specified
-- when registering the activity type using RegisterActivityType. The valid
-- values are integers greater than or equal to 0. An integer value can be
-- used to specify the duration in seconds while NONE can be used to specify
-- unlimited duration. A start-to-close timeout for this activity task must be
-- specified either as a default for the activity type or through this field.
-- If neither this field is set nor a default start-to-close timeout was
-- specified at registration time then a fault will be returned.
satdaStartToCloseTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaStartToCloseTimeout = lens _satdaStartToCloseTimeout (\s a -> s { _satdaStartToCloseTimeout = a })
{-# INLINE satdaStartToCloseTimeout #-}

-- | If set, specifies the maximum time before which a worker processing a task
-- of this type must report progress by calling RecordActivityTaskHeartbeat.
-- If the timeout is exceeded, the activity task is automatically timed out.
-- If the worker subsequently attempts to record a heartbeat or returns a
-- result, it will be ignored. This overrides the default heartbeat timeout
-- specified when registering the activity type using RegisterActivityType.
-- The valid values are integers greater than or equal to 0. An integer value
-- can be used to specify the duration in seconds while NONE can be used to
-- specify unlimited duration.
satdaHeartbeatTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaHeartbeatTimeout = lens _satdaHeartbeatTimeout (\s a -> s { _satdaHeartbeatTimeout = a })
{-# INLINE satdaHeartbeatTimeout #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ScheduleActivityTaskDecisionAttributes' data type to populate a request.
mkScheduleActivityTaskDecisionAttributes :: ActivityType -- ^ 'satdaActivityType'
                                         -> Text -- ^ 'satdaActivityId'
                                         -> ScheduleActivityTaskDecisionAttributes
mkScheduleActivityTaskDecisionAttributes p1 p2 = ScheduleActivityTaskDecisionAttributes
    { _satdaActivityType = p1
    , _satdaActivityId = p2
    , _satdaControl = Nothing
    , _satdaInput = Nothing
    , _satdaScheduleToCloseTimeout = Nothing
    , _satdaTaskList = Nothing
    , _satdaScheduleToStartTimeout = Nothing
    , _satdaStartToCloseTimeout = Nothing
    , _satdaHeartbeatTimeout = Nothing
    }
{-# INLINE mkScheduleActivityTaskDecisionAttributes #-}

instance FromJSON ScheduleActivityTaskDecisionAttributes

instance ToJSON ScheduleActivityTaskDecisionAttributes

-- | If the event is of type ScheduleActivityTaskFailed then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data ScheduleActivityTaskFailedEventAttributes = ScheduleActivityTaskFailedEventAttributes
    { _satfeaActivityType :: ActivityType
      -- ^ The activity type provided in the ScheduleActivityTask decision
      -- that failed.
    , _satfeaActivityId :: Text
      -- ^ The activityId provided in the ScheduleActivityTask decision that
      -- failed.
    , _satfeaCause :: ScheduleActivityTaskFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _satfeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision that resulted in the scheduling of this activity task.
      -- This information can be useful for diagnosing problems by tracing
      -- back the chain of events leading up to this event.
    } deriving (Show, Generic)

-- | The activity type provided in the ScheduleActivityTask decision that
-- failed.
satfeaActivityType :: Lens' ScheduleActivityTaskFailedEventAttributes (ActivityType)
satfeaActivityType = lens _satfeaActivityType (\s a -> s { _satfeaActivityType = a })
{-# INLINE satfeaActivityType #-}

-- | The activityId provided in the ScheduleActivityTask decision that failed.
satfeaActivityId :: Lens' ScheduleActivityTaskFailedEventAttributes (Text)
satfeaActivityId = lens _satfeaActivityId (\s a -> s { _satfeaActivityId = a })
{-# INLINE satfeaActivityId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
satfeaCause :: Lens' ScheduleActivityTaskFailedEventAttributes (ScheduleActivityTaskFailedCause)
satfeaCause = lens _satfeaCause (\s a -> s { _satfeaCause = a })
{-# INLINE satfeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- that resulted in the scheduling of this activity task. This information can
-- be useful for diagnosing problems by tracing back the chain of events
-- leading up to this event.
satfeaDecisionTaskCompletedEventId :: Lens' ScheduleActivityTaskFailedEventAttributes (Integer)
satfeaDecisionTaskCompletedEventId = lens _satfeaDecisionTaskCompletedEventId (\s a -> s { _satfeaDecisionTaskCompletedEventId = a })
{-# INLINE satfeaDecisionTaskCompletedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ScheduleActivityTaskFailedEventAttributes' data type to populate a request.
mkScheduleActivityTaskFailedEventAttributes :: ActivityType -- ^ 'satfeaActivityType'
                                            -> Text -- ^ 'satfeaActivityId'
                                            -> ScheduleActivityTaskFailedCause -- ^ 'satfeaCause'
                                            -> Integer -- ^ 'satfeaDecisionTaskCompletedEventId'
                                            -> ScheduleActivityTaskFailedEventAttributes
mkScheduleActivityTaskFailedEventAttributes p1 p2 p3 p4 = ScheduleActivityTaskFailedEventAttributes
    { _satfeaActivityType = p1
    , _satfeaActivityId = p2
    , _satfeaCause = p3
    , _satfeaDecisionTaskCompletedEventId = p4
    }
{-# INLINE mkScheduleActivityTaskFailedEventAttributes #-}

instance FromJSON ScheduleActivityTaskFailedEventAttributes

instance ToJSON ScheduleActivityTaskFailedEventAttributes

-- | Provides details of the SignalExternalWorkflowExecution decision. It is not
-- set for other decision types.
data SignalExternalWorkflowExecutionDecisionAttributes = SignalExternalWorkflowExecutionDecisionAttributes
    { _sewedaWorkflowId :: Text
      -- ^ The workflowId of the workflow execution to be signaled. This
      -- field is required.
    , _sewedaRunId :: Maybe Text
      -- ^ The runId of the workflow execution to be signaled.
    , _sewedaSignalName :: Text
      -- ^ The name of the signal.The target workflow execution will use the
      -- signal name and input to process the signal. This field is
      -- required.
    , _sewedaInput :: Maybe Text
      -- ^ Optional input to be provided with the signal.The target workflow
      -- execution will use the signal name and input to process the
      -- signal.
    , _sewedaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent decision tasks.
    } deriving (Show, Generic)

-- | The workflowId of the workflow execution to be signaled. This field is
-- required.
sewedaWorkflowId :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Text)
sewedaWorkflowId = lens _sewedaWorkflowId (\s a -> s { _sewedaWorkflowId = a })
{-# INLINE sewedaWorkflowId #-}

-- | The runId of the workflow execution to be signaled.
sewedaRunId :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Maybe Text)
sewedaRunId = lens _sewedaRunId (\s a -> s { _sewedaRunId = a })
{-# INLINE sewedaRunId #-}

-- | The name of the signal.The target workflow execution will use the signal
-- name and input to process the signal. This field is required.
sewedaSignalName :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Text)
sewedaSignalName = lens _sewedaSignalName (\s a -> s { _sewedaSignalName = a })
{-# INLINE sewedaSignalName #-}

-- | Optional input to be provided with the signal.The target workflow execution
-- will use the signal name and input to process the signal.
sewedaInput :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Maybe Text)
sewedaInput = lens _sewedaInput (\s a -> s { _sewedaInput = a })
{-# INLINE sewedaInput #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent decision tasks.
sewedaControl :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Maybe Text)
sewedaControl = lens _sewedaControl (\s a -> s { _sewedaControl = a })
{-# INLINE sewedaControl #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SignalExternalWorkflowExecutionDecisionAttributes' data type to populate a request.
mkSignalExternalWorkflowExecutionDecisionAttributes :: Text -- ^ 'sewedaWorkflowId'
                                                    -> Text -- ^ 'sewedaSignalName'
                                                    -> SignalExternalWorkflowExecutionDecisionAttributes
mkSignalExternalWorkflowExecutionDecisionAttributes p1 p2 = SignalExternalWorkflowExecutionDecisionAttributes
    { _sewedaWorkflowId = p1
    , _sewedaRunId = Nothing
    , _sewedaSignalName = p3
    , _sewedaInput = Nothing
    , _sewedaControl = Nothing
    }
{-# INLINE mkSignalExternalWorkflowExecutionDecisionAttributes #-}

instance FromJSON SignalExternalWorkflowExecutionDecisionAttributes

instance ToJSON SignalExternalWorkflowExecutionDecisionAttributes

-- | If the event is of type SignalExternalWorkflowExecutionFailed then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
data SignalExternalWorkflowExecutionFailedEventAttributes = SignalExternalWorkflowExecutionFailedEventAttributes
    { _sewefeaWorkflowId :: Text
      -- ^ The workflowId of the external workflow execution that the signal
      -- was being delivered to.
    , _sewefeaRunId :: Maybe Text
      -- ^ The runId of the external workflow execution that the signal was
      -- being delivered to.
    , _sewefeaCause :: SignalExternalWorkflowExecutionFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _sewefeaInitiatedEventId :: Integer
      -- ^ The id of the SignalExternalWorkflowExecutionInitiated event
      -- corresponding to the SignalExternalWorkflowExecution decision to
      -- request this signal. This information can be useful for
      -- diagnosing problems by tracing back the chain of events leading
      -- up to this event.
    , _sewefeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the
      -- SignalExternalWorkflowExecution decision for this signal. This
      -- information can be useful for diagnosing problems by tracing back
      -- the cause of events leading up to this event.
    , _sewefeaControl :: Maybe Text
    } deriving (Show, Generic)

-- | The workflowId of the external workflow execution that the signal was being
-- delivered to.
sewefeaWorkflowId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (Text)
sewefeaWorkflowId = lens _sewefeaWorkflowId (\s a -> s { _sewefeaWorkflowId = a })
{-# INLINE sewefeaWorkflowId #-}

-- | The runId of the external workflow execution that the signal was being
-- delivered to.
sewefeaRunId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
sewefeaRunId = lens _sewefeaRunId (\s a -> s { _sewefeaRunId = a })
{-# INLINE sewefeaRunId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
sewefeaCause :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (SignalExternalWorkflowExecutionFailedCause)
sewefeaCause = lens _sewefeaCause (\s a -> s { _sewefeaCause = a })
{-# INLINE sewefeaCause #-}

-- | The id of the SignalExternalWorkflowExecutionInitiated event corresponding
-- to the SignalExternalWorkflowExecution decision to request this signal.
-- This information can be useful for diagnosing problems by tracing back the
-- chain of events leading up to this event.
sewefeaInitiatedEventId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (Integer)
sewefeaInitiatedEventId = lens _sewefeaInitiatedEventId (\s a -> s { _sewefeaInitiatedEventId = a })
{-# INLINE sewefeaInitiatedEventId #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the SignalExternalWorkflowExecution decision for this
-- signal. This information can be useful for diagnosing problems by tracing
-- back the cause of events leading up to this event.
sewefeaDecisionTaskCompletedEventId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (Integer)
sewefeaDecisionTaskCompletedEventId = lens _sewefeaDecisionTaskCompletedEventId (\s a -> s { _sewefeaDecisionTaskCompletedEventId = a })
{-# INLINE sewefeaDecisionTaskCompletedEventId #-}

sewefeaControl :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
sewefeaControl = lens _sewefeaControl (\s a -> s { _sewefeaControl = a })
{-# INLINE sewefeaControl #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SignalExternalWorkflowExecutionFailedEventAttributes' data type to populate a request.
mkSignalExternalWorkflowExecutionFailedEventAttributes :: Text -- ^ 'sewefeaWorkflowId'
                                                       -> SignalExternalWorkflowExecutionFailedCause -- ^ 'sewefeaCause'
                                                       -> Integer -- ^ 'sewefeaInitiatedEventId'
                                                       -> Integer -- ^ 'sewefeaDecisionTaskCompletedEventId'
                                                       -> SignalExternalWorkflowExecutionFailedEventAttributes
mkSignalExternalWorkflowExecutionFailedEventAttributes p1 p2 p3 p4 = SignalExternalWorkflowExecutionFailedEventAttributes
    { _sewefeaWorkflowId = p1
    , _sewefeaRunId = Nothing
    , _sewefeaCause = p3
    , _sewefeaInitiatedEventId = p4
    , _sewefeaDecisionTaskCompletedEventId = p5
    , _sewefeaControl = Nothing
    }
{-# INLINE mkSignalExternalWorkflowExecutionFailedEventAttributes #-}

instance FromJSON SignalExternalWorkflowExecutionFailedEventAttributes

instance ToJSON SignalExternalWorkflowExecutionFailedEventAttributes

-- | If the event is of type SignalExternalWorkflowExecutionInitiated then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
data SignalExternalWorkflowExecutionInitiatedEventAttributes = SignalExternalWorkflowExecutionInitiatedEventAttributes
    { _seweieaWorkflowId :: Text
      -- ^ The workflowId of the external workflow execution.
    , _seweieaRunId :: Maybe Text
      -- ^ The runId of the external workflow execution to send the signal
      -- to.
    , _seweieaSignalName :: Text
      -- ^ The name of the signal.
    , _seweieaInput :: Maybe Text
      -- ^ Input provided to the signal (if any).
    , _seweieaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the
      -- SignalExternalWorkflowExecution decision for this signal. This
      -- information can be useful for diagnosing problems by tracing back
      -- the cause of events leading up to this event.
    , _seweieaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent decision tasks.
    } deriving (Show, Generic)

-- | The workflowId of the external workflow execution.
seweieaWorkflowId :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Text)
seweieaWorkflowId = lens _seweieaWorkflowId (\s a -> s { _seweieaWorkflowId = a })
{-# INLINE seweieaWorkflowId #-}

-- | The runId of the external workflow execution to send the signal to.
seweieaRunId :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
seweieaRunId = lens _seweieaRunId (\s a -> s { _seweieaRunId = a })
{-# INLINE seweieaRunId #-}

-- | The name of the signal.
seweieaSignalName :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Text)
seweieaSignalName = lens _seweieaSignalName (\s a -> s { _seweieaSignalName = a })
{-# INLINE seweieaSignalName #-}

-- | Input provided to the signal (if any).
seweieaInput :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
seweieaInput = lens _seweieaInput (\s a -> s { _seweieaInput = a })
{-# INLINE seweieaInput #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the SignalExternalWorkflowExecution decision for this
-- signal. This information can be useful for diagnosing problems by tracing
-- back the cause of events leading up to this event.
seweieaDecisionTaskCompletedEventId :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Integer)
seweieaDecisionTaskCompletedEventId = lens _seweieaDecisionTaskCompletedEventId (\s a -> s { _seweieaDecisionTaskCompletedEventId = a })
{-# INLINE seweieaDecisionTaskCompletedEventId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent decision tasks.
seweieaControl :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
seweieaControl = lens _seweieaControl (\s a -> s { _seweieaControl = a })
{-# INLINE seweieaControl #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SignalExternalWorkflowExecutionInitiatedEventAttributes' data type to populate a request.
mkSignalExternalWorkflowExecutionInitiatedEventAttributes :: Text -- ^ 'seweieaWorkflowId'
                                                          -> Text -- ^ 'seweieaSignalName'
                                                          -> Integer -- ^ 'seweieaDecisionTaskCompletedEventId'
                                                          -> SignalExternalWorkflowExecutionInitiatedEventAttributes
mkSignalExternalWorkflowExecutionInitiatedEventAttributes p1 p2 p3 = SignalExternalWorkflowExecutionInitiatedEventAttributes
    { _seweieaWorkflowId = p1
    , _seweieaRunId = Nothing
    , _seweieaSignalName = p3
    , _seweieaInput = Nothing
    , _seweieaDecisionTaskCompletedEventId = p5
    , _seweieaControl = Nothing
    }
{-# INLINE mkSignalExternalWorkflowExecutionInitiatedEventAttributes #-}

instance FromJSON SignalExternalWorkflowExecutionInitiatedEventAttributes

instance ToJSON SignalExternalWorkflowExecutionInitiatedEventAttributes

-- | Provides details of the StartChildWorkflowExecution decision. It is not set
-- for other decision types.
data StartChildWorkflowExecutionDecisionAttributes = StartChildWorkflowExecutionDecisionAttributes
    { _scwedaWorkflowType :: WorkflowType
      -- ^ The type of the workflow execution to be started. This field is
      -- required.
    , _scwedaWorkflowId :: Text
      -- ^ The workflowId of the workflow execution. This field is required.
      -- The specified string must not start or end with whitespace. It
      -- must not contain a : (colon), / (slash), | (vertical bar), or any
      -- control characters (\u0000-\u001f | \u007f - \u009f). Also, it
      -- must not contain the literal string &quot;arn&quot;.
    , _scwedaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent workflow tasks. This data is not sent to
      -- the child workflow execution.
    , _scwedaInput :: Maybe Text
      -- ^ The input to be provided to the workflow execution.
    , _scwedaExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The total duration for this workflow execution. This overrides
      -- the defaultExecutionStartToCloseTimeout specified when
      -- registering the workflow type. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify
      -- unlimited duration. An execution start-to-close timeout for this
      -- workflow execution must be specified either as a default for the
      -- workflow type or through this parameter. If neither this
      -- parameter is set nor a default execution start-to-close timeout
      -- was specified at registration time then a fault will be returned.
    , _scwedaTaskList :: Maybe TaskList
      -- ^ The name of the task list to be used for decision tasks of the
      -- child workflow execution. A task list for this workflow execution
      -- must be specified either as a default for the workflow type or
      -- through this parameter. If neither this parameter is set nor a
      -- default task list was specified at registration time then a fault
      -- will be returned. The specified string must not start or end with
      -- whitespace. It must not contain a : (colon), / (slash), |
      -- (vertical bar), or any control characters (\u0000-\u001f | \u007f
      -- - \u009f). Also, it must not contain the literal string
      -- &quot;arn&quot;.
    , _scwedaTaskStartToCloseTimeout :: Maybe Text
      -- ^ Specifies the maximum duration of decision tasks for this
      -- workflow execution. This parameter overrides the
      -- defaultTaskStartToCloseTimout specified when registering the
      -- workflow type using RegisterWorkflowType. The valid values are
      -- integers greater than or equal to 0. An integer value can be used
      -- to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration. A task start-to-close timeout for
      -- this workflow execution must be specified either as a default for
      -- the workflow type or through this parameter. If neither this
      -- parameter is set nor a default task start-to-close timeout was
      -- specified at registration time then a fault will be returned.
    , _scwedaChildPolicy :: Maybe ChildPolicy
      -- ^ If set, specifies the policy to use for the child workflow
      -- executions if the workflow execution being started is terminated
      -- by calling the TerminateWorkflowExecution action explicitly or
      -- due to an expired timeout. This policy overrides the default
      -- child policy specified when registering the workflow type using
      -- RegisterWorkflowType. The supported child policies are:
      -- TERMINATE: the child executions will be terminated.
      -- REQUEST_CANCEL: a request to cancel will be attempted for each
      -- child execution by recording a WorkflowExecutionCancelRequested
      -- event in its history. It is up to the decider to take appropriate
      -- actions when it receives an execution history with this event.
      -- ABANDON: no action will be taken. The child executions will
      -- continue to run. A child policy for the workflow execution being
      -- started must be specified either as a default registered for its
      -- workflow type or through this field. If neither this field is set
      -- nor a default child policy was specified at registration time
      -- then a fault will be returned.
    , _scwedaTagList :: [Text]
      -- ^ The list of tags to associate with the child workflow execution.
      -- A maximum of 5 tags can be specified. You can list workflow
      -- executions with a specific tag by calling
      -- ListOpenWorkflowExecutions or ListClosedWorkflowExecutions and
      -- specifying a TagFilter.
    } deriving (Show, Generic)

-- | The type of the workflow execution to be started. This field is required.
scwedaWorkflowType :: Lens' StartChildWorkflowExecutionDecisionAttributes (WorkflowType)
scwedaWorkflowType = lens _scwedaWorkflowType (\s a -> s { _scwedaWorkflowType = a })
{-# INLINE scwedaWorkflowType #-}

-- | The workflowId of the workflow execution. This field is required. The
-- specified string must not start or end with whitespace. It must not contain
-- a : (colon), / (slash), | (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string &quot;arn&quot;.
scwedaWorkflowId :: Lens' StartChildWorkflowExecutionDecisionAttributes (Text)
scwedaWorkflowId = lens _scwedaWorkflowId (\s a -> s { _scwedaWorkflowId = a })
{-# INLINE scwedaWorkflowId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks. This data is not sent to the child workflow
-- execution.
scwedaControl :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaControl = lens _scwedaControl (\s a -> s { _scwedaControl = a })
{-# INLINE scwedaControl #-}

-- | The input to be provided to the workflow execution.
scwedaInput :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaInput = lens _scwedaInput (\s a -> s { _scwedaInput = a })
{-# INLINE scwedaInput #-}

-- | The total duration for this workflow execution. This overrides the
-- defaultExecutionStartToCloseTimeout specified when registering the workflow
-- type. The valid values are integers greater than or equal to 0. An integer
-- value can be used to specify the duration in seconds while NONE can be used
-- to specify unlimited duration. An execution start-to-close timeout for this
-- workflow execution must be specified either as a default for the workflow
-- type or through this parameter. If neither this parameter is set nor a
-- default execution start-to-close timeout was specified at registration time
-- then a fault will be returned.
scwedaExecutionStartToCloseTimeout :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaExecutionStartToCloseTimeout = lens _scwedaExecutionStartToCloseTimeout (\s a -> s { _scwedaExecutionStartToCloseTimeout = a })
{-# INLINE scwedaExecutionStartToCloseTimeout #-}

-- | The name of the task list to be used for decision tasks of the child
-- workflow execution. A task list for this workflow execution must be
-- specified either as a default for the workflow type or through this
-- parameter. If neither this parameter is set nor a default task list was
-- specified at registration time then a fault will be returned. The specified
-- string must not start or end with whitespace. It must not contain a :
-- (colon), / (slash), | (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string &quot;arn&quot;.
scwedaTaskList :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe TaskList)
scwedaTaskList = lens _scwedaTaskList (\s a -> s { _scwedaTaskList = a })
{-# INLINE scwedaTaskList #-}

-- | Specifies the maximum duration of decision tasks for this workflow
-- execution. This parameter overrides the defaultTaskStartToCloseTimout
-- specified when registering the workflow type using RegisterWorkflowType.
-- The valid values are integers greater than or equal to 0. An integer value
-- can be used to specify the duration in seconds while NONE can be used to
-- specify unlimited duration. A task start-to-close timeout for this workflow
-- execution must be specified either as a default for the workflow type or
-- through this parameter. If neither this parameter is set nor a default task
-- start-to-close timeout was specified at registration time then a fault will
-- be returned.
scwedaTaskStartToCloseTimeout :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaTaskStartToCloseTimeout = lens _scwedaTaskStartToCloseTimeout (\s a -> s { _scwedaTaskStartToCloseTimeout = a })
{-# INLINE scwedaTaskStartToCloseTimeout #-}

-- | If set, specifies the policy to use for the child workflow executions if
-- the workflow execution being started is terminated by calling the
-- TerminateWorkflowExecution action explicitly or due to an expired timeout.
-- This policy overrides the default child policy specified when registering
-- the workflow type using RegisterWorkflowType. The supported child policies
-- are: TERMINATE: the child executions will be terminated. REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a
-- WorkflowExecutionCancelRequested event in its history. It is up to the
-- decider to take appropriate actions when it receives an execution history
-- with this event. ABANDON: no action will be taken. The child executions
-- will continue to run. A child policy for the workflow execution being
-- started must be specified either as a default registered for its workflow
-- type or through this field. If neither this field is set nor a default
-- child policy was specified at registration time then a fault will be
-- returned.
scwedaChildPolicy :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe ChildPolicy)
scwedaChildPolicy = lens _scwedaChildPolicy (\s a -> s { _scwedaChildPolicy = a })
{-# INLINE scwedaChildPolicy #-}

-- | The list of tags to associate with the child workflow execution. A maximum
-- of 5 tags can be specified. You can list workflow executions with a
-- specific tag by calling ListOpenWorkflowExecutions or
-- ListClosedWorkflowExecutions and specifying a TagFilter.
scwedaTagList :: Lens' StartChildWorkflowExecutionDecisionAttributes ([Text])
scwedaTagList = lens _scwedaTagList (\s a -> s { _scwedaTagList = a })
{-# INLINE scwedaTagList #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StartChildWorkflowExecutionDecisionAttributes' data type to populate a request.
mkStartChildWorkflowExecutionDecisionAttributes :: WorkflowType -- ^ 'scwedaWorkflowType'
                                                -> Text -- ^ 'scwedaWorkflowId'
                                                -> StartChildWorkflowExecutionDecisionAttributes
mkStartChildWorkflowExecutionDecisionAttributes p1 p2 = StartChildWorkflowExecutionDecisionAttributes
    { _scwedaWorkflowType = p1
    , _scwedaWorkflowId = p2
    , _scwedaControl = Nothing
    , _scwedaInput = Nothing
    , _scwedaExecutionStartToCloseTimeout = Nothing
    , _scwedaTaskList = Nothing
    , _scwedaTaskStartToCloseTimeout = Nothing
    , _scwedaChildPolicy = Nothing
    , _scwedaTagList = mempty
    }
{-# INLINE mkStartChildWorkflowExecutionDecisionAttributes #-}

instance FromJSON StartChildWorkflowExecutionDecisionAttributes

instance ToJSON StartChildWorkflowExecutionDecisionAttributes

-- | If the event is of type StartChildWorkflowExecutionFailed then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
data StartChildWorkflowExecutionFailedEventAttributes = StartChildWorkflowExecutionFailedEventAttributes
    { _scwefeaWorkflowType :: WorkflowType
      -- ^ The workflow type provided in the StartChildWorkflowExecution
      -- Decision that failed.
    , _scwefeaCause :: StartChildWorkflowExecutionFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _scwefeaWorkflowId :: Text
      -- ^ The workflowId of the child workflow execution.
    , _scwefeaInitiatedEventId :: Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this child workflow execution. This information can be
      -- useful for diagnosing problems by tracing back the chain of
      -- events leading up to this event.
    , _scwefeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the StartChildWorkflowExecution
      -- Decision to request this child workflow execution. This
      -- information can be useful for diagnosing problems by tracing back
      -- the cause of events.
    , _scwefeaControl :: Maybe Text
    } deriving (Show, Generic)

-- | The workflow type provided in the StartChildWorkflowExecution Decision that
-- failed.
scwefeaWorkflowType :: Lens' StartChildWorkflowExecutionFailedEventAttributes (WorkflowType)
scwefeaWorkflowType = lens _scwefeaWorkflowType (\s a -> s { _scwefeaWorkflowType = a })
{-# INLINE scwefeaWorkflowType #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
scwefeaCause :: Lens' StartChildWorkflowExecutionFailedEventAttributes (StartChildWorkflowExecutionFailedCause)
scwefeaCause = lens _scwefeaCause (\s a -> s { _scwefeaCause = a })
{-# INLINE scwefeaCause #-}

-- | The workflowId of the child workflow execution.
scwefeaWorkflowId :: Lens' StartChildWorkflowExecutionFailedEventAttributes (Text)
scwefeaWorkflowId = lens _scwefeaWorkflowId (\s a -> s { _scwefeaWorkflowId = a })
{-# INLINE scwefeaWorkflowId #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
scwefeaInitiatedEventId :: Lens' StartChildWorkflowExecutionFailedEventAttributes (Integer)
scwefeaInitiatedEventId = lens _scwefeaInitiatedEventId (\s a -> s { _scwefeaInitiatedEventId = a })
{-# INLINE scwefeaInitiatedEventId #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the StartChildWorkflowExecution Decision to request
-- this child workflow execution. This information can be useful for
-- diagnosing problems by tracing back the cause of events.
scwefeaDecisionTaskCompletedEventId :: Lens' StartChildWorkflowExecutionFailedEventAttributes (Integer)
scwefeaDecisionTaskCompletedEventId = lens _scwefeaDecisionTaskCompletedEventId (\s a -> s { _scwefeaDecisionTaskCompletedEventId = a })
{-# INLINE scwefeaDecisionTaskCompletedEventId #-}

scwefeaControl :: Lens' StartChildWorkflowExecutionFailedEventAttributes (Maybe Text)
scwefeaControl = lens _scwefeaControl (\s a -> s { _scwefeaControl = a })
{-# INLINE scwefeaControl #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StartChildWorkflowExecutionFailedEventAttributes' data type to populate a request.
mkStartChildWorkflowExecutionFailedEventAttributes :: WorkflowType -- ^ 'scwefeaWorkflowType'
                                                   -> StartChildWorkflowExecutionFailedCause -- ^ 'scwefeaCause'
                                                   -> Text -- ^ 'scwefeaWorkflowId'
                                                   -> Integer -- ^ 'scwefeaInitiatedEventId'
                                                   -> Integer -- ^ 'scwefeaDecisionTaskCompletedEventId'
                                                   -> StartChildWorkflowExecutionFailedEventAttributes
mkStartChildWorkflowExecutionFailedEventAttributes p1 p2 p3 p4 p5 = StartChildWorkflowExecutionFailedEventAttributes
    { _scwefeaWorkflowType = p1
    , _scwefeaCause = p2
    , _scwefeaWorkflowId = p3
    , _scwefeaInitiatedEventId = p4
    , _scwefeaDecisionTaskCompletedEventId = p5
    , _scwefeaControl = Nothing
    }
{-# INLINE mkStartChildWorkflowExecutionFailedEventAttributes #-}

instance FromJSON StartChildWorkflowExecutionFailedEventAttributes

instance ToJSON StartChildWorkflowExecutionFailedEventAttributes

-- | If the event is of type StartChildWorkflowExecutionInitiated then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
data StartChildWorkflowExecutionInitiatedEventAttributes = StartChildWorkflowExecutionInitiatedEventAttributes
    { _scweieaWorkflowId :: Text
      -- ^ The workflowId of the child workflow execution.
    , _scweieaWorkflowType :: WorkflowType
      -- ^ The type of the child workflow execution.
    , _scweieaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent decision tasks. This data is not sent to
      -- the activity.
    , _scweieaInput :: Maybe Text
      -- ^ The inputs provided to the child workflow execution (if any).
    , _scweieaExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration for the child workflow execution. If the
      -- workflow execution is not closed within this duration, it will be
      -- timed out and force terminated. The valid values are integers
      -- greater than or equal to 0. An integer value can be used to
      -- specify the duration in seconds while NONE can be used to specify
      -- unlimited duration.
    , _scweieaTaskList :: TaskList
      -- ^ The name of the task list used for the decision tasks of the
      -- child workflow execution.
    , _scweieaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the StartChildWorkflowExecution
      -- Decision to request this child workflow execution. This
      -- information can be useful for diagnosing problems by tracing back
      -- the cause of events.
    , _scweieaChildPolicy :: ChildPolicy
      -- ^ The policy to use for the child workflow executions if this
      -- execution gets terminated by explicitly calling the
      -- TerminateWorkflowExecution action or due to an expired timeout.
      -- The supported child policies are: TERMINATE: the child executions
      -- will be terminated. REQUEST_CANCEL: a request to cancel will be
      -- attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up
      -- to the decider to take appropriate actions when it receives an
      -- execution history with this event. ABANDON: no action will be
      -- taken. The child executions will continue to run.
    , _scweieaTaskStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration allowed for the decision tasks for this
      -- workflow execution. The valid values are integers greater than or
      -- equal to 0. An integer value can be used to specify the duration
      -- in seconds while NONE can be used to specify unlimited duration.
    , _scweieaTagList :: [Text]
      -- ^ The list of tags to associated with the child workflow execution.
    } deriving (Show, Generic)

-- | The workflowId of the child workflow execution.
scweieaWorkflowId :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Text)
scweieaWorkflowId = lens _scweieaWorkflowId (\s a -> s { _scweieaWorkflowId = a })
{-# INLINE scweieaWorkflowId #-}

-- | The type of the child workflow execution.
scweieaWorkflowType :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (WorkflowType)
scweieaWorkflowType = lens _scweieaWorkflowType (\s a -> s { _scweieaWorkflowType = a })
{-# INLINE scweieaWorkflowType #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent decision tasks. This data is not sent to the activity.
scweieaControl :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaControl = lens _scweieaControl (\s a -> s { _scweieaControl = a })
{-# INLINE scweieaControl #-}

-- | The inputs provided to the child workflow execution (if any).
scweieaInput :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaInput = lens _scweieaInput (\s a -> s { _scweieaInput = a })
{-# INLINE scweieaInput #-}

-- | The maximum duration for the child workflow execution. If the workflow
-- execution is not closed within this duration, it will be timed out and
-- force terminated. The valid values are integers greater than or equal to 0.
-- An integer value can be used to specify the duration in seconds while NONE
-- can be used to specify unlimited duration.
scweieaExecutionStartToCloseTimeout :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaExecutionStartToCloseTimeout = lens _scweieaExecutionStartToCloseTimeout (\s a -> s { _scweieaExecutionStartToCloseTimeout = a })
{-# INLINE scweieaExecutionStartToCloseTimeout #-}

-- | The name of the task list used for the decision tasks of the child workflow
-- execution.
scweieaTaskList :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (TaskList)
scweieaTaskList = lens _scweieaTaskList (\s a -> s { _scweieaTaskList = a })
{-# INLINE scweieaTaskList #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the StartChildWorkflowExecution Decision to request
-- this child workflow execution. This information can be useful for
-- diagnosing problems by tracing back the cause of events.
scweieaDecisionTaskCompletedEventId :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Integer)
scweieaDecisionTaskCompletedEventId = lens _scweieaDecisionTaskCompletedEventId (\s a -> s { _scweieaDecisionTaskCompletedEventId = a })
{-# INLINE scweieaDecisionTaskCompletedEventId #-}

-- | The policy to use for the child workflow executions if this execution gets
-- terminated by explicitly calling the TerminateWorkflowExecution action or
-- due to an expired timeout. The supported child policies are: TERMINATE: the
-- child executions will be terminated. REQUEST_CANCEL: a request to cancel
-- will be attempted for each child execution by recording a
-- WorkflowExecutionCancelRequested event in its history. It is up to the
-- decider to take appropriate actions when it receives an execution history
-- with this event. ABANDON: no action will be taken. The child executions
-- will continue to run.
scweieaChildPolicy :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (ChildPolicy)
scweieaChildPolicy = lens _scweieaChildPolicy (\s a -> s { _scweieaChildPolicy = a })
{-# INLINE scweieaChildPolicy #-}

-- | The maximum duration allowed for the decision tasks for this workflow
-- execution. The valid values are integers greater than or equal to 0. An
-- integer value can be used to specify the duration in seconds while NONE can
-- be used to specify unlimited duration.
scweieaTaskStartToCloseTimeout :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaTaskStartToCloseTimeout = lens _scweieaTaskStartToCloseTimeout (\s a -> s { _scweieaTaskStartToCloseTimeout = a })
{-# INLINE scweieaTaskStartToCloseTimeout #-}

-- | The list of tags to associated with the child workflow execution.
scweieaTagList :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes ([Text])
scweieaTagList = lens _scweieaTagList (\s a -> s { _scweieaTagList = a })
{-# INLINE scweieaTagList #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StartChildWorkflowExecutionInitiatedEventAttributes' data type to populate a request.
mkStartChildWorkflowExecutionInitiatedEventAttributes :: Text -- ^ 'scweieaWorkflowId'
                                                      -> WorkflowType -- ^ 'scweieaWorkflowType'
                                                      -> TaskList -- ^ 'scweieaTaskList'
                                                      -> Integer -- ^ 'scweieaDecisionTaskCompletedEventId'
                                                      -> ChildPolicy -- ^ 'scweieaChildPolicy'
                                                      -> StartChildWorkflowExecutionInitiatedEventAttributes
mkStartChildWorkflowExecutionInitiatedEventAttributes p1 p2 p3 p4 p5 = StartChildWorkflowExecutionInitiatedEventAttributes
    { _scweieaWorkflowId = p1
    , _scweieaWorkflowType = p2
    , _scweieaControl = Nothing
    , _scweieaInput = Nothing
    , _scweieaExecutionStartToCloseTimeout = Nothing
    , _scweieaTaskList = p6
    , _scweieaDecisionTaskCompletedEventId = p7
    , _scweieaChildPolicy = p8
    , _scweieaTaskStartToCloseTimeout = Nothing
    , _scweieaTagList = mempty
    }
{-# INLINE mkStartChildWorkflowExecutionInitiatedEventAttributes #-}

instance FromJSON StartChildWorkflowExecutionInitiatedEventAttributes

instance ToJSON StartChildWorkflowExecutionInitiatedEventAttributes

-- | Provides details of the StartTimer decision. It is not set for other
-- decision types.
data StartTimerDecisionAttributes = StartTimerDecisionAttributes
    { _stdaTimerId :: Text
      -- ^ The unique Id of the timer. This field is required. The specified
      -- string must not start or end with whitespace. It must not contain
      -- a : (colon), / (slash), | (vertical bar), or any control
      -- characters (\u0000-\u001f | \u007f - \u009f). Also, it must not
      -- contain the literal string &quot;arn&quot;.
    , _stdaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent workflow tasks.
    , _stdaStartToFireTimeout :: Text
      -- ^ The duration to wait before firing the timer. This field is
      -- required. The duration is specified in seconds. The valid values
      -- are integers greater than or equal to 0.
    } deriving (Show, Generic)

-- | The unique Id of the timer. This field is required. The specified string
-- must not start or end with whitespace. It must not contain a : (colon), /
-- (slash), | (vertical bar), or any control characters (\u0000-\u001f |
-- \u007f - \u009f). Also, it must not contain the literal string
-- &quot;arn&quot;.
stdaTimerId :: Lens' StartTimerDecisionAttributes (Text)
stdaTimerId = lens _stdaTimerId (\s a -> s { _stdaTimerId = a })
{-# INLINE stdaTimerId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
stdaControl :: Lens' StartTimerDecisionAttributes (Maybe Text)
stdaControl = lens _stdaControl (\s a -> s { _stdaControl = a })
{-# INLINE stdaControl #-}

-- | The duration to wait before firing the timer. This field is required. The
-- duration is specified in seconds. The valid values are integers greater
-- than or equal to 0.
stdaStartToFireTimeout :: Lens' StartTimerDecisionAttributes (Text)
stdaStartToFireTimeout = lens _stdaStartToFireTimeout (\s a -> s { _stdaStartToFireTimeout = a })
{-# INLINE stdaStartToFireTimeout #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StartTimerDecisionAttributes' data type to populate a request.
mkStartTimerDecisionAttributes :: Text -- ^ 'stdaTimerId'
                               -> Text -- ^ 'stdaStartToFireTimeout'
                               -> StartTimerDecisionAttributes
mkStartTimerDecisionAttributes p1 p2 = StartTimerDecisionAttributes
    { _stdaTimerId = p1
    , _stdaControl = Nothing
    , _stdaStartToFireTimeout = p3
    }
{-# INLINE mkStartTimerDecisionAttributes #-}

instance FromJSON StartTimerDecisionAttributes

instance ToJSON StartTimerDecisionAttributes

-- | If the event is of type StartTimerFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data StartTimerFailedEventAttributes = StartTimerFailedEventAttributes
    { _stfeaTimerId :: Text
      -- ^ The timerId provided in the StartTimer decision that failed.
    , _stfeaCause :: StartTimerFailedCause
      -- ^ The cause of the failure to process the decision. This
      -- information is generated by the system and can be useful for
      -- diagnostic purposes. If cause is set to OPERATION_NOT_PERMITTED,
      -- the decision failed because it lacked sufficient permissions. For
      -- details and example IAM policies, see Using IAM to Manage Access
      -- to Amazon SWF Workflows.
    , _stfeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the StartTimer decision for this
      -- activity task. This information can be useful for diagnosing
      -- problems by tracing back the cause of events.
    } deriving (Show, Generic)

-- | The timerId provided in the StartTimer decision that failed.
stfeaTimerId :: Lens' StartTimerFailedEventAttributes (Text)
stfeaTimerId = lens _stfeaTimerId (\s a -> s { _stfeaTimerId = a })
{-# INLINE stfeaTimerId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
stfeaCause :: Lens' StartTimerFailedEventAttributes (StartTimerFailedCause)
stfeaCause = lens _stfeaCause (\s a -> s { _stfeaCause = a })
{-# INLINE stfeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the StartTimer decision for this activity task. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
stfeaDecisionTaskCompletedEventId :: Lens' StartTimerFailedEventAttributes (Integer)
stfeaDecisionTaskCompletedEventId = lens _stfeaDecisionTaskCompletedEventId (\s a -> s { _stfeaDecisionTaskCompletedEventId = a })
{-# INLINE stfeaDecisionTaskCompletedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StartTimerFailedEventAttributes' data type to populate a request.
mkStartTimerFailedEventAttributes :: Text -- ^ 'stfeaTimerId'
                                  -> StartTimerFailedCause -- ^ 'stfeaCause'
                                  -> Integer -- ^ 'stfeaDecisionTaskCompletedEventId'
                                  -> StartTimerFailedEventAttributes
mkStartTimerFailedEventAttributes p1 p2 p3 = StartTimerFailedEventAttributes
    { _stfeaTimerId = p1
    , _stfeaCause = p2
    , _stfeaDecisionTaskCompletedEventId = p3
    }
{-# INLINE mkStartTimerFailedEventAttributes #-}

instance FromJSON StartTimerFailedEventAttributes

instance ToJSON StartTimerFailedEventAttributes

-- | If the event is of type TimerCanceled then this member is set and provides
-- detailed information about the event. It is not set for other event types.
data TimerCanceledEventAttributes = TimerCanceledEventAttributes
    { _tceaTimerId :: Text
      -- ^ The unique Id of the timer that was canceled.
    , _tceaStartedEventId :: Integer
      -- ^ The id of the TimerStarted event that was recorded when this
      -- timer was started. This information can be useful for diagnosing
      -- problems by tracing back the chain of events leading up to this
      -- event.
    , _tceaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the CancelTimer decision to cancel
      -- this timer. This information can be useful for diagnosing
      -- problems by tracing back the cause of events.
    } deriving (Show, Generic)

-- | The unique Id of the timer that was canceled.
tceaTimerId :: Lens' TimerCanceledEventAttributes (Text)
tceaTimerId = lens _tceaTimerId (\s a -> s { _tceaTimerId = a })
{-# INLINE tceaTimerId #-}

-- | The id of the TimerStarted event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
tceaStartedEventId :: Lens' TimerCanceledEventAttributes (Integer)
tceaStartedEventId = lens _tceaStartedEventId (\s a -> s { _tceaStartedEventId = a })
{-# INLINE tceaStartedEventId #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CancelTimer decision to cancel this timer. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
tceaDecisionTaskCompletedEventId :: Lens' TimerCanceledEventAttributes (Integer)
tceaDecisionTaskCompletedEventId = lens _tceaDecisionTaskCompletedEventId (\s a -> s { _tceaDecisionTaskCompletedEventId = a })
{-# INLINE tceaDecisionTaskCompletedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TimerCanceledEventAttributes' data type to populate a request.
mkTimerCanceledEventAttributes :: Text -- ^ 'tceaTimerId'
                               -> Integer -- ^ 'tceaStartedEventId'
                               -> Integer -- ^ 'tceaDecisionTaskCompletedEventId'
                               -> TimerCanceledEventAttributes
mkTimerCanceledEventAttributes p1 p2 p3 = TimerCanceledEventAttributes
    { _tceaTimerId = p1
    , _tceaStartedEventId = p2
    , _tceaDecisionTaskCompletedEventId = p3
    }
{-# INLINE mkTimerCanceledEventAttributes #-}

instance FromJSON TimerCanceledEventAttributes

instance ToJSON TimerCanceledEventAttributes

-- | If the event is of type TimerFired then this member is set and provides
-- detailed information about the event. It is not set for other event types.
data TimerFiredEventAttributes = TimerFiredEventAttributes
    { _tfeaTimerId :: Text
      -- ^ The unique Id of the timer that fired.
    , _tfeaStartedEventId :: Integer
      -- ^ The id of the TimerStarted event that was recorded when this
      -- timer was started. This information can be useful for diagnosing
      -- problems by tracing back the chain of events leading up to this
      -- event.
    } deriving (Show, Generic)

-- | The unique Id of the timer that fired.
tfeaTimerId :: Lens' TimerFiredEventAttributes (Text)
tfeaTimerId = lens _tfeaTimerId (\s a -> s { _tfeaTimerId = a })
{-# INLINE tfeaTimerId #-}

-- | The id of the TimerStarted event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
tfeaStartedEventId :: Lens' TimerFiredEventAttributes (Integer)
tfeaStartedEventId = lens _tfeaStartedEventId (\s a -> s { _tfeaStartedEventId = a })
{-# INLINE tfeaStartedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TimerFiredEventAttributes' data type to populate a request.
mkTimerFiredEventAttributes :: Text -- ^ 'tfeaTimerId'
                            -> Integer -- ^ 'tfeaStartedEventId'
                            -> TimerFiredEventAttributes
mkTimerFiredEventAttributes p1 p2 = TimerFiredEventAttributes
    { _tfeaTimerId = p1
    , _tfeaStartedEventId = p2
    }
{-# INLINE mkTimerFiredEventAttributes #-}

instance FromJSON TimerFiredEventAttributes

instance ToJSON TimerFiredEventAttributes

-- | If the event is of type TimerStarted then this member is set and provides
-- detailed information about the event. It is not set for other event types.
data TimerStartedEventAttributes = TimerStartedEventAttributes
    { _tseaTimerId :: Text
      -- ^ The unique Id of the timer that was started.
    , _tseaControl :: Maybe Text
      -- ^ Optional data attached to the event that can be used by the
      -- decider in subsequent workflow tasks.
    , _tseaStartToFireTimeout :: Text
      -- ^ The duration of time after which the timer will fire. The
      -- duration is specified in seconds. The valid values are integers
      -- greater than or equal to 0.
    , _tseaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the StartTimer decision for this
      -- activity task. This information can be useful for diagnosing
      -- problems by tracing back the cause of events.
    } deriving (Show, Generic)

-- | The unique Id of the timer that was started.
tseaTimerId :: Lens' TimerStartedEventAttributes (Text)
tseaTimerId = lens _tseaTimerId (\s a -> s { _tseaTimerId = a })
{-# INLINE tseaTimerId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
tseaControl :: Lens' TimerStartedEventAttributes (Maybe Text)
tseaControl = lens _tseaControl (\s a -> s { _tseaControl = a })
{-# INLINE tseaControl #-}

-- | The duration of time after which the timer will fire. The duration is
-- specified in seconds. The valid values are integers greater than or equal
-- to 0.
tseaStartToFireTimeout :: Lens' TimerStartedEventAttributes (Text)
tseaStartToFireTimeout = lens _tseaStartToFireTimeout (\s a -> s { _tseaStartToFireTimeout = a })
{-# INLINE tseaStartToFireTimeout #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the StartTimer decision for this activity task. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
tseaDecisionTaskCompletedEventId :: Lens' TimerStartedEventAttributes (Integer)
tseaDecisionTaskCompletedEventId = lens _tseaDecisionTaskCompletedEventId (\s a -> s { _tseaDecisionTaskCompletedEventId = a })
{-# INLINE tseaDecisionTaskCompletedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TimerStartedEventAttributes' data type to populate a request.
mkTimerStartedEventAttributes :: Text -- ^ 'tseaTimerId'
                              -> Text -- ^ 'tseaStartToFireTimeout'
                              -> Integer -- ^ 'tseaDecisionTaskCompletedEventId'
                              -> TimerStartedEventAttributes
mkTimerStartedEventAttributes p1 p2 p3 = TimerStartedEventAttributes
    { _tseaTimerId = p1
    , _tseaControl = Nothing
    , _tseaStartToFireTimeout = p3
    , _tseaDecisionTaskCompletedEventId = p4
    }
{-# INLINE mkTimerStartedEventAttributes #-}

instance FromJSON TimerStartedEventAttributes

instance ToJSON TimerStartedEventAttributes

-- | The workflow execution to describe.
data WorkflowExecution = WorkflowExecution
    { _weWorkflowId :: Text
      -- ^ The user defined identifier associated with the workflow
      -- execution.
    , _weRunId :: Text
      -- ^ A system generated unique identifier for the workflow execution.
    } deriving (Show, Generic)

-- | The user defined identifier associated with the workflow execution.
weWorkflowId :: Lens' WorkflowExecution (Text)
weWorkflowId = lens _weWorkflowId (\s a -> s { _weWorkflowId = a })
{-# INLINE weWorkflowId #-}

-- | A system generated unique identifier for the workflow execution.
weRunId :: Lens' WorkflowExecution (Text)
weRunId = lens _weRunId (\s a -> s { _weRunId = a })
{-# INLINE weRunId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WorkflowExecution' data type to populate a request.
mkWorkflowExecution :: Text -- ^ 'weWorkflowId'
                    -> Text -- ^ 'weRunId'
                    -> WorkflowExecution
mkWorkflowExecution p1 p2 = WorkflowExecution
    { _weWorkflowId = p1
    , _weRunId = p2
    }
{-# INLINE mkWorkflowExecution #-}

instance FromJSON WorkflowExecution

instance ToJSON WorkflowExecution

-- | If the event is of type WorkflowExecutionCancelRequested then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
data WorkflowExecutionCancelRequestedEventAttributes = WorkflowExecutionCancelRequestedEventAttributes
    { _wecreaExternalWorkflowExecution :: Maybe WorkflowExecution
      -- ^ The external workflow execution for which the cancellation was
      -- requested.
    , _wecreaExternalInitiatedEventId :: Maybe Integer
      -- ^ The id of the RequestCancelExternalWorkflowExecutionInitiated
      -- event corresponding to the RequestCancelExternalWorkflowExecution
      -- decision to cancel this workflow execution.The source event with
      -- this Id can be found in the history of the source workflow
      -- execution. This information can be useful for diagnosing problems
      -- by tracing back the chain of events leading up to this event.
    , _wecreaCause :: Maybe WorkflowExecutionCancelRequestedCause
      -- ^ If set, indicates that the request to cancel the workflow
      -- execution was automatically generated, and specifies the cause.
      -- This happens if the parent workflow execution times out or is
      -- terminated, and the child policy is set to cancel child
      -- executions.
    } deriving (Show, Generic)

-- | The external workflow execution for which the cancellation was requested.
wecreaExternalWorkflowExecution :: Lens' WorkflowExecutionCancelRequestedEventAttributes (Maybe WorkflowExecution)
wecreaExternalWorkflowExecution = lens _wecreaExternalWorkflowExecution (\s a -> s { _wecreaExternalWorkflowExecution = a })
{-# INLINE wecreaExternalWorkflowExecution #-}

-- | The id of the RequestCancelExternalWorkflowExecutionInitiated event
-- corresponding to the RequestCancelExternalWorkflowExecution decision to
-- cancel this workflow execution.The source event with this Id can be found
-- in the history of the source workflow execution. This information can be
-- useful for diagnosing problems by tracing back the chain of events leading
-- up to this event.
wecreaExternalInitiatedEventId :: Lens' WorkflowExecutionCancelRequestedEventAttributes (Maybe Integer)
wecreaExternalInitiatedEventId = lens _wecreaExternalInitiatedEventId (\s a -> s { _wecreaExternalInitiatedEventId = a })
{-# INLINE wecreaExternalInitiatedEventId #-}

-- | If set, indicates that the request to cancel the workflow execution was
-- automatically generated, and specifies the cause. This happens if the
-- parent workflow execution times out or is terminated, and the child policy
-- is set to cancel child executions.
wecreaCause :: Lens' WorkflowExecutionCancelRequestedEventAttributes (Maybe WorkflowExecutionCancelRequestedCause)
wecreaCause = lens _wecreaCause (\s a -> s { _wecreaCause = a })
{-# INLINE wecreaCause #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WorkflowExecutionCancelRequestedEventAttributes' data type to populate a request.
mkWorkflowExecutionCancelRequestedEventAttributes :: WorkflowExecutionCancelRequestedEventAttributes
mkWorkflowExecutionCancelRequestedEventAttributes = WorkflowExecutionCancelRequestedEventAttributes
    { _wecreaExternalWorkflowExecution = Nothing
    , _wecreaExternalInitiatedEventId = Nothing
    , _wecreaCause = Nothing
    }
{-# INLINE mkWorkflowExecutionCancelRequestedEventAttributes #-}

instance FromJSON WorkflowExecutionCancelRequestedEventAttributes

instance ToJSON WorkflowExecutionCancelRequestedEventAttributes

-- | If the event is of type WorkflowExecutionCanceled then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionCanceledEventAttributes = WorkflowExecutionCanceledEventAttributes
    { _wecebDetails :: Maybe Text
      -- ^ Details for the cancellation (if any).
    , _wecebDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the CancelWorkflowExecution
      -- decision for this cancellation request. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    } deriving (Show, Generic)

-- | Details for the cancellation (if any).
wecebDetails :: Lens' WorkflowExecutionCanceledEventAttributes (Maybe Text)
wecebDetails = lens _wecebDetails (\s a -> s { _wecebDetails = a })
{-# INLINE wecebDetails #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CancelWorkflowExecution decision for this
-- cancellation request. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
wecebDecisionTaskCompletedEventId :: Lens' WorkflowExecutionCanceledEventAttributes (Integer)
wecebDecisionTaskCompletedEventId = lens _wecebDecisionTaskCompletedEventId (\s a -> s { _wecebDecisionTaskCompletedEventId = a })
{-# INLINE wecebDecisionTaskCompletedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WorkflowExecutionCanceledEventAttributes' data type to populate a request.
mkWorkflowExecutionCanceledEventAttributes :: Integer -- ^ 'wecebDecisionTaskCompletedEventId'
                                           -> WorkflowExecutionCanceledEventAttributes
mkWorkflowExecutionCanceledEventAttributes p1 = WorkflowExecutionCanceledEventAttributes
    { _wecebDetails = Nothing
    , _wecebDecisionTaskCompletedEventId = p2
    }
{-# INLINE mkWorkflowExecutionCanceledEventAttributes #-}

instance FromJSON WorkflowExecutionCanceledEventAttributes

instance ToJSON WorkflowExecutionCanceledEventAttributes

-- | If the event is of type WorkflowExecutionCompleted then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionCompletedEventAttributes = WorkflowExecutionCompletedEventAttributes
    { _weceaResult :: Maybe Text
      -- ^ The result produced by the workflow execution upon successful
      -- completion.
    , _weceaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the CompleteWorkflowExecution
      -- decision to complete this execution. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    } deriving (Show, Generic)

-- | The result produced by the workflow execution upon successful completion.
weceaResult :: Lens' WorkflowExecutionCompletedEventAttributes (Maybe Text)
weceaResult = lens _weceaResult (\s a -> s { _weceaResult = a })
{-# INLINE weceaResult #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CompleteWorkflowExecution decision to complete
-- this execution. This information can be useful for diagnosing problems by
-- tracing back the cause of events.
weceaDecisionTaskCompletedEventId :: Lens' WorkflowExecutionCompletedEventAttributes (Integer)
weceaDecisionTaskCompletedEventId = lens _weceaDecisionTaskCompletedEventId (\s a -> s { _weceaDecisionTaskCompletedEventId = a })
{-# INLINE weceaDecisionTaskCompletedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WorkflowExecutionCompletedEventAttributes' data type to populate a request.
mkWorkflowExecutionCompletedEventAttributes :: Integer -- ^ 'weceaDecisionTaskCompletedEventId'
                                            -> WorkflowExecutionCompletedEventAttributes
mkWorkflowExecutionCompletedEventAttributes p1 = WorkflowExecutionCompletedEventAttributes
    { _weceaResult = Nothing
    , _weceaDecisionTaskCompletedEventId = p2
    }
{-# INLINE mkWorkflowExecutionCompletedEventAttributes #-}

instance FromJSON WorkflowExecutionCompletedEventAttributes

instance ToJSON WorkflowExecutionCompletedEventAttributes

-- | The configuration settings for this workflow execution including timeout
-- values, tasklist etc.
data WorkflowExecutionConfiguration = WorkflowExecutionConfiguration
    { _wehTaskStartToCloseTimeout :: Text
      -- ^ The maximum duration allowed for decision tasks for this workflow
      -- execution. The valid values are integers greater than or equal to
      -- 0. An integer value can be used to specify the duration in
      -- seconds while NONE can be used to specify unlimited duration.
    , _wehExecutionStartToCloseTimeout :: Text
      -- ^ The total duration for this workflow execution. The valid values
      -- are integers greater than or equal to 0. An integer value can be
      -- used to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration.
    , _wehTaskList :: TaskList
      -- ^ The task list used for the decision tasks generated for this
      -- workflow execution.
    , _wehChildPolicy :: ChildPolicy
      -- ^ The policy to use for the child workflow executions if this
      -- workflow execution is terminated, by calling the
      -- TerminateWorkflowExecution action explicitly or due to an expired
      -- timeout. The supported child policies are: TERMINATE: the child
      -- executions will be terminated. REQUEST_CANCEL: a request to
      -- cancel will be attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up
      -- to the decider to take appropriate actions when it receives an
      -- execution history with this event. ABANDON: no action will be
      -- taken. The child executions will continue to run.
    } deriving (Show, Generic)

-- | The maximum duration allowed for decision tasks for this workflow
-- execution. The valid values are integers greater than or equal to 0. An
-- integer value can be used to specify the duration in seconds while NONE can
-- be used to specify unlimited duration.
wehTaskStartToCloseTimeout :: Lens' WorkflowExecutionConfiguration (Text)
wehTaskStartToCloseTimeout = lens _wehTaskStartToCloseTimeout (\s a -> s { _wehTaskStartToCloseTimeout = a })
{-# INLINE wehTaskStartToCloseTimeout #-}

-- | The total duration for this workflow execution. The valid values are
-- integers greater than or equal to 0. An integer value can be used to
-- specify the duration in seconds while NONE can be used to specify unlimited
-- duration.
wehExecutionStartToCloseTimeout :: Lens' WorkflowExecutionConfiguration (Text)
wehExecutionStartToCloseTimeout = lens _wehExecutionStartToCloseTimeout (\s a -> s { _wehExecutionStartToCloseTimeout = a })
{-# INLINE wehExecutionStartToCloseTimeout #-}

-- | The task list used for the decision tasks generated for this workflow
-- execution.
wehTaskList :: Lens' WorkflowExecutionConfiguration (TaskList)
wehTaskList = lens _wehTaskList (\s a -> s { _wehTaskList = a })
{-# INLINE wehTaskList #-}

-- | The policy to use for the child workflow executions if this workflow
-- execution is terminated, by calling the TerminateWorkflowExecution action
-- explicitly or due to an expired timeout. The supported child policies are:
-- TERMINATE: the child executions will be terminated. REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a
-- WorkflowExecutionCancelRequested event in its history. It is up to the
-- decider to take appropriate actions when it receives an execution history
-- with this event. ABANDON: no action will be taken. The child executions
-- will continue to run.
wehChildPolicy :: Lens' WorkflowExecutionConfiguration (ChildPolicy)
wehChildPolicy = lens _wehChildPolicy (\s a -> s { _wehChildPolicy = a })
{-# INLINE wehChildPolicy #-}

instance FromJSON WorkflowExecutionConfiguration

-- | If the event is of type WorkflowExecutionContinuedAsNew then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
data WorkflowExecutionContinuedAsNewEventAttributes = WorkflowExecutionContinuedAsNewEventAttributes
    { _wecaneaInput :: Maybe Text
      -- ^ The input provided to the new workflow execution.
    , _wecaneaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the ContinueAsNewWorkflowExecution
      -- decision that started this execution. This information can be
      -- useful for diagnosing problems by tracing back the cause of
      -- events.
    , _wecaneaNewExecutionRunId :: Text
      -- ^ The runId of the new workflow execution.
    , _wecaneaExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The total duration allowed for the new workflow execution. The
      -- valid values are integers greater than or equal to 0. An integer
      -- value can be used to specify the duration in seconds while NONE
      -- can be used to specify unlimited duration.
    , _wecaneaTaskList :: TaskList
      -- ^ Represents a task list.
    , _wecaneaTaskStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration of decision tasks for the new workflow
      -- execution. The valid values are integers greater than or equal to
      -- 0. An integer value can be used to specify the duration in
      -- seconds while NONE can be used to specify unlimited duration.
    , _wecaneaChildPolicy :: ChildPolicy
      -- ^ The policy to use for the child workflow executions of the new
      -- execution if it is terminated by calling the
      -- TerminateWorkflowExecution action explicitly or due to an expired
      -- timeout. The supported child policies are: TERMINATE: the child
      -- executions will be terminated. REQUEST_CANCEL: a request to
      -- cancel will be attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up
      -- to the decider to take appropriate actions when it receives an
      -- execution history with this event. ABANDON: no action will be
      -- taken. The child executions will continue to run.
    , _wecaneaTagList :: [Text]
      -- ^ The list of tags associated with the new workflow execution.
    , _wecaneaWorkflowType :: WorkflowType
      -- ^ Represents a workflow type.
    } deriving (Show, Generic)

-- | The input provided to the new workflow execution.
wecaneaInput :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaInput = lens _wecaneaInput (\s a -> s { _wecaneaInput = a })
{-# INLINE wecaneaInput #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the ContinueAsNewWorkflowExecution decision that
-- started this execution. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
wecaneaDecisionTaskCompletedEventId :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Integer)
wecaneaDecisionTaskCompletedEventId = lens _wecaneaDecisionTaskCompletedEventId (\s a -> s { _wecaneaDecisionTaskCompletedEventId = a })
{-# INLINE wecaneaDecisionTaskCompletedEventId #-}

-- | The runId of the new workflow execution.
wecaneaNewExecutionRunId :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Text)
wecaneaNewExecutionRunId = lens _wecaneaNewExecutionRunId (\s a -> s { _wecaneaNewExecutionRunId = a })
{-# INLINE wecaneaNewExecutionRunId #-}

-- | The total duration allowed for the new workflow execution. The valid values
-- are integers greater than or equal to 0. An integer value can be used to
-- specify the duration in seconds while NONE can be used to specify unlimited
-- duration.
wecaneaExecutionStartToCloseTimeout :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaExecutionStartToCloseTimeout = lens _wecaneaExecutionStartToCloseTimeout (\s a -> s { _wecaneaExecutionStartToCloseTimeout = a })
{-# INLINE wecaneaExecutionStartToCloseTimeout #-}

-- | Represents a task list.
wecaneaTaskList :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (TaskList)
wecaneaTaskList = lens _wecaneaTaskList (\s a -> s { _wecaneaTaskList = a })
{-# INLINE wecaneaTaskList #-}

-- | The maximum duration of decision tasks for the new workflow execution. The
-- valid values are integers greater than or equal to 0. An integer value can
-- be used to specify the duration in seconds while NONE can be used to
-- specify unlimited duration.
wecaneaTaskStartToCloseTimeout :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaTaskStartToCloseTimeout = lens _wecaneaTaskStartToCloseTimeout (\s a -> s { _wecaneaTaskStartToCloseTimeout = a })
{-# INLINE wecaneaTaskStartToCloseTimeout #-}

-- | The policy to use for the child workflow executions of the new execution if
-- it is terminated by calling the TerminateWorkflowExecution action
-- explicitly or due to an expired timeout. The supported child policies are:
-- TERMINATE: the child executions will be terminated. REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a
-- WorkflowExecutionCancelRequested event in its history. It is up to the
-- decider to take appropriate actions when it receives an execution history
-- with this event. ABANDON: no action will be taken. The child executions
-- will continue to run.
wecaneaChildPolicy :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (ChildPolicy)
wecaneaChildPolicy = lens _wecaneaChildPolicy (\s a -> s { _wecaneaChildPolicy = a })
{-# INLINE wecaneaChildPolicy #-}

-- | The list of tags associated with the new workflow execution.
wecaneaTagList :: Lens' WorkflowExecutionContinuedAsNewEventAttributes ([Text])
wecaneaTagList = lens _wecaneaTagList (\s a -> s { _wecaneaTagList = a })
{-# INLINE wecaneaTagList #-}

-- | Represents a workflow type.
wecaneaWorkflowType :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (WorkflowType)
wecaneaWorkflowType = lens _wecaneaWorkflowType (\s a -> s { _wecaneaWorkflowType = a })
{-# INLINE wecaneaWorkflowType #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WorkflowExecutionContinuedAsNewEventAttributes' data type to populate a request.
mkWorkflowExecutionContinuedAsNewEventAttributes :: Integer -- ^ 'wecaneaDecisionTaskCompletedEventId'
                                                 -> Text -- ^ 'wecaneaNewExecutionRunId'
                                                 -> TaskList -- ^ 'wecaneaTaskList'
                                                 -> ChildPolicy -- ^ 'wecaneaChildPolicy'
                                                 -> WorkflowType -- ^ 'wecaneaWorkflowType'
                                                 -> WorkflowExecutionContinuedAsNewEventAttributes
mkWorkflowExecutionContinuedAsNewEventAttributes p1 p2 p3 p4 p5 = WorkflowExecutionContinuedAsNewEventAttributes
    { _wecaneaInput = Nothing
    , _wecaneaDecisionTaskCompletedEventId = p2
    , _wecaneaNewExecutionRunId = p3
    , _wecaneaExecutionStartToCloseTimeout = Nothing
    , _wecaneaTaskList = p5
    , _wecaneaTaskStartToCloseTimeout = Nothing
    , _wecaneaChildPolicy = p7
    , _wecaneaTagList = mempty
    , _wecaneaWorkflowType = p9
    }
{-# INLINE mkWorkflowExecutionContinuedAsNewEventAttributes #-}

instance FromJSON WorkflowExecutionContinuedAsNewEventAttributes

instance ToJSON WorkflowExecutionContinuedAsNewEventAttributes

-- | If the event is of type WorkflowExecutionFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionFailedEventAttributes = WorkflowExecutionFailedEventAttributes
    { _wefeaReason :: Maybe Text
      -- ^ The descriptive reason provided for the failure (if any).
    , _wefeaDetails :: Maybe Text
      -- ^ The details of the failure (if any).
    , _wefeaDecisionTaskCompletedEventId :: Integer
      -- ^ The id of the DecisionTaskCompleted event corresponding to the
      -- decision task that resulted in the FailWorkflowExecution decision
      -- to fail this execution. This information can be useful for
      -- diagnosing problems by tracing back the cause of events.
    } deriving (Show, Generic)

-- | The descriptive reason provided for the failure (if any).
wefeaReason :: Lens' WorkflowExecutionFailedEventAttributes (Maybe Text)
wefeaReason = lens _wefeaReason (\s a -> s { _wefeaReason = a })
{-# INLINE wefeaReason #-}

-- | The details of the failure (if any).
wefeaDetails :: Lens' WorkflowExecutionFailedEventAttributes (Maybe Text)
wefeaDetails = lens _wefeaDetails (\s a -> s { _wefeaDetails = a })
{-# INLINE wefeaDetails #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the FailWorkflowExecution decision to fail this
-- execution. This information can be useful for diagnosing problems by
-- tracing back the cause of events.
wefeaDecisionTaskCompletedEventId :: Lens' WorkflowExecutionFailedEventAttributes (Integer)
wefeaDecisionTaskCompletedEventId = lens _wefeaDecisionTaskCompletedEventId (\s a -> s { _wefeaDecisionTaskCompletedEventId = a })
{-# INLINE wefeaDecisionTaskCompletedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WorkflowExecutionFailedEventAttributes' data type to populate a request.
mkWorkflowExecutionFailedEventAttributes :: Integer -- ^ 'wefeaDecisionTaskCompletedEventId'
                                         -> WorkflowExecutionFailedEventAttributes
mkWorkflowExecutionFailedEventAttributes p1 = WorkflowExecutionFailedEventAttributes
    { _wefeaReason = Nothing
    , _wefeaDetails = Nothing
    , _wefeaDecisionTaskCompletedEventId = p3
    }
{-# INLINE mkWorkflowExecutionFailedEventAttributes #-}

instance FromJSON WorkflowExecutionFailedEventAttributes

instance ToJSON WorkflowExecutionFailedEventAttributes

-- | Information about the workflow execution.
data WorkflowExecutionInfo = WorkflowExecutionInfo
    { _weiExecution :: WorkflowExecution
      -- ^ The workflow execution this information is about.
    , _weiWorkflowType :: WorkflowType
      -- ^ The type of the workflow execution.
    , _weiStartTimestamp :: POSIX
      -- ^ The time when the execution was started.
    , _weiCloseTimestamp :: Maybe POSIX
      -- ^ The time when the workflow execution was closed. Set only if the
      -- execution status is CLOSED.
    , _weiExecutionStatus :: ExecutionStatus
      -- ^ The current status of the execution.
    , _weiCloseStatus :: Maybe CloseStatus
      -- ^ If the execution status is closed then this specifies how the
      -- execution was closed: COMPLETED: the execution was successfully
      -- completed. CANCELED: the execution was canceled.Cancellation
      -- allows the implementation to gracefully clean up before the
      -- execution is closed. TERMINATED: the execution was force
      -- terminated. FAILED: the execution failed to complete. TIMED_OUT:
      -- the execution did not complete in the alloted time and was
      -- automatically timed out. CONTINUED_AS_NEW: the execution is
      -- logically continued. This means the current execution was
      -- completed and a new execution was started to carry on the
      -- workflow.
    , _weiParent :: Maybe WorkflowExecution
      -- ^ If this workflow execution is a child of another execution then
      -- contains the workflow execution that started this execution.
    , _weiTagList :: [Text]
      -- ^ The list of tags associated with the workflow execution. Tags can
      -- be used to identify and list workflow executions of interest
      -- through the visibility APIs. A workflow execution can have a
      -- maximum of 5 tags.
    , _weiCancelRequested :: Maybe Bool
      -- ^ Set to true if a cancellation is requested for this workflow
      -- execution.
    } deriving (Show, Generic)

-- | The workflow execution this information is about.
weiExecution :: Lens' WorkflowExecutionInfo (WorkflowExecution)
weiExecution = lens _weiExecution (\s a -> s { _weiExecution = a })
{-# INLINE weiExecution #-}

-- | The type of the workflow execution.
weiWorkflowType :: Lens' WorkflowExecutionInfo (WorkflowType)
weiWorkflowType = lens _weiWorkflowType (\s a -> s { _weiWorkflowType = a })
{-# INLINE weiWorkflowType #-}

-- | The time when the execution was started.
weiStartTimestamp :: Lens' WorkflowExecutionInfo (POSIX)
weiStartTimestamp = lens _weiStartTimestamp (\s a -> s { _weiStartTimestamp = a })
{-# INLINE weiStartTimestamp #-}

-- | The time when the workflow execution was closed. Set only if the execution
-- status is CLOSED.
weiCloseTimestamp :: Lens' WorkflowExecutionInfo (Maybe POSIX)
weiCloseTimestamp = lens _weiCloseTimestamp (\s a -> s { _weiCloseTimestamp = a })
{-# INLINE weiCloseTimestamp #-}

-- | The current status of the execution.
weiExecutionStatus :: Lens' WorkflowExecutionInfo (ExecutionStatus)
weiExecutionStatus = lens _weiExecutionStatus (\s a -> s { _weiExecutionStatus = a })
{-# INLINE weiExecutionStatus #-}

-- | If the execution status is closed then this specifies how the execution was
-- closed: COMPLETED: the execution was successfully completed. CANCELED: the
-- execution was canceled.Cancellation allows the implementation to gracefully
-- clean up before the execution is closed. TERMINATED: the execution was
-- force terminated. FAILED: the execution failed to complete. TIMED_OUT: the
-- execution did not complete in the alloted time and was automatically timed
-- out. CONTINUED_AS_NEW: the execution is logically continued. This means the
-- current execution was completed and a new execution was started to carry on
-- the workflow.
weiCloseStatus :: Lens' WorkflowExecutionInfo (Maybe CloseStatus)
weiCloseStatus = lens _weiCloseStatus (\s a -> s { _weiCloseStatus = a })
{-# INLINE weiCloseStatus #-}

-- | If this workflow execution is a child of another execution then contains
-- the workflow execution that started this execution.
weiParent :: Lens' WorkflowExecutionInfo (Maybe WorkflowExecution)
weiParent = lens _weiParent (\s a -> s { _weiParent = a })
{-# INLINE weiParent #-}

-- | The list of tags associated with the workflow execution. Tags can be used
-- to identify and list workflow executions of interest through the visibility
-- APIs. A workflow execution can have a maximum of 5 tags.
weiTagList :: Lens' WorkflowExecutionInfo ([Text])
weiTagList = lens _weiTagList (\s a -> s { _weiTagList = a })
{-# INLINE weiTagList #-}

-- | Set to true if a cancellation is requested for this workflow execution.
weiCancelRequested :: Lens' WorkflowExecutionInfo (Maybe Bool)
weiCancelRequested = lens _weiCancelRequested (\s a -> s { _weiCancelRequested = a })
{-# INLINE weiCancelRequested #-}

instance FromJSON WorkflowExecutionInfo

-- | The number of tasks for this workflow execution. This includes open and
-- closed tasks of all types.
data WorkflowExecutionOpenCounts = WorkflowExecutionOpenCounts
    { _weocOpenActivityTasks :: Integer
      -- ^ The count of activity tasks whose status is OPEN.
    , _weocOpenDecisionTasks :: Integer
      -- ^ The count of decision tasks whose status is OPEN. A workflow
      -- execution can have at most one open decision task.
    , _weocOpenTimers :: Integer
      -- ^ The count of timers started by this workflow execution that have
      -- not fired yet.
    , _weocOpenChildWorkflowExecutions :: Integer
      -- ^ The count of child workflow executions whose status is OPEN.
    } deriving (Show, Generic)

-- | The count of activity tasks whose status is OPEN.
weocOpenActivityTasks :: Lens' WorkflowExecutionOpenCounts (Integer)
weocOpenActivityTasks = lens _weocOpenActivityTasks (\s a -> s { _weocOpenActivityTasks = a })
{-# INLINE weocOpenActivityTasks #-}

-- | The count of decision tasks whose status is OPEN. A workflow execution can
-- have at most one open decision task.
weocOpenDecisionTasks :: Lens' WorkflowExecutionOpenCounts (Integer)
weocOpenDecisionTasks = lens _weocOpenDecisionTasks (\s a -> s { _weocOpenDecisionTasks = a })
{-# INLINE weocOpenDecisionTasks #-}

-- | The count of timers started by this workflow execution that have not fired
-- yet.
weocOpenTimers :: Lens' WorkflowExecutionOpenCounts (Integer)
weocOpenTimers = lens _weocOpenTimers (\s a -> s { _weocOpenTimers = a })
{-# INLINE weocOpenTimers #-}

-- | The count of child workflow executions whose status is OPEN.
weocOpenChildWorkflowExecutions :: Lens' WorkflowExecutionOpenCounts (Integer)
weocOpenChildWorkflowExecutions = lens _weocOpenChildWorkflowExecutions (\s a -> s { _weocOpenChildWorkflowExecutions = a })
{-# INLINE weocOpenChildWorkflowExecutions #-}

instance FromJSON WorkflowExecutionOpenCounts

-- | If the event is of type WorkflowExecutionSignaled then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionSignaledEventAttributes = WorkflowExecutionSignaledEventAttributes
    { _wesebSignalName :: Text
      -- ^ The name of the signal received. The decider can use the signal
      -- name and inputs to determine how to the process the signal.
    , _wesebInput :: Maybe Text
      -- ^ Inputs provided with the signal (if any). The decider can use the
      -- signal name and inputs to determine how to process the signal.
    , _wesebExternalWorkflowExecution :: Maybe WorkflowExecution
      -- ^ The workflow execution that sent the signal. This is set only of
      -- the signal was sent by another workflow execution.
    , _wesebExternalInitiatedEventId :: Maybe Integer
      -- ^ The id of the SignalExternalWorkflowExecutionInitiated event
      -- corresponding to the SignalExternalWorkflow decision to signal
      -- this workflow execution.The source event with this Id can be
      -- found in the history of the source workflow execution. This
      -- information can be useful for diagnosing problems by tracing back
      -- the chain of events leading up to this event. This field is set
      -- only if the signal was initiated by another workflow execution.
    } deriving (Show, Generic)

-- | The name of the signal received. The decider can use the signal name and
-- inputs to determine how to the process the signal.
wesebSignalName :: Lens' WorkflowExecutionSignaledEventAttributes (Text)
wesebSignalName = lens _wesebSignalName (\s a -> s { _wesebSignalName = a })
{-# INLINE wesebSignalName #-}

-- | Inputs provided with the signal (if any). The decider can use the signal
-- name and inputs to determine how to process the signal.
wesebInput :: Lens' WorkflowExecutionSignaledEventAttributes (Maybe Text)
wesebInput = lens _wesebInput (\s a -> s { _wesebInput = a })
{-# INLINE wesebInput #-}

-- | The workflow execution that sent the signal. This is set only of the signal
-- was sent by another workflow execution.
wesebExternalWorkflowExecution :: Lens' WorkflowExecutionSignaledEventAttributes (Maybe WorkflowExecution)
wesebExternalWorkflowExecution = lens _wesebExternalWorkflowExecution (\s a -> s { _wesebExternalWorkflowExecution = a })
{-# INLINE wesebExternalWorkflowExecution #-}

-- | The id of the SignalExternalWorkflowExecutionInitiated event corresponding
-- to the SignalExternalWorkflow decision to signal this workflow
-- execution.The source event with this Id can be found in the history of the
-- source workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event. This
-- field is set only if the signal was initiated by another workflow
-- execution.
wesebExternalInitiatedEventId :: Lens' WorkflowExecutionSignaledEventAttributes (Maybe Integer)
wesebExternalInitiatedEventId = lens _wesebExternalInitiatedEventId (\s a -> s { _wesebExternalInitiatedEventId = a })
{-# INLINE wesebExternalInitiatedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WorkflowExecutionSignaledEventAttributes' data type to populate a request.
mkWorkflowExecutionSignaledEventAttributes :: Text -- ^ 'wesebSignalName'
                                           -> WorkflowExecutionSignaledEventAttributes
mkWorkflowExecutionSignaledEventAttributes p1 = WorkflowExecutionSignaledEventAttributes
    { _wesebSignalName = p1
    , _wesebInput = Nothing
    , _wesebExternalWorkflowExecution = Nothing
    , _wesebExternalInitiatedEventId = Nothing
    }
{-# INLINE mkWorkflowExecutionSignaledEventAttributes #-}

instance FromJSON WorkflowExecutionSignaledEventAttributes

instance ToJSON WorkflowExecutionSignaledEventAttributes

-- | If the event is of type WorkflowExecutionStarted then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionStartedEventAttributes = WorkflowExecutionStartedEventAttributes
    { _weseaInput :: Maybe Text
      -- ^ The input provided to the workflow execution (if any).
    , _weseaExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration for this workflow execution. The valid
      -- values are integers greater than or equal to 0. An integer value
      -- can be used to specify the duration in seconds while NONE can be
      -- used to specify unlimited duration.
    , _weseaTaskStartToCloseTimeout :: Maybe Text
      -- ^ The maximum duration of decision tasks for this workflow type.
      -- The valid values are integers greater than or equal to 0. An
      -- integer value can be used to specify the duration in seconds
      -- while NONE can be used to specify unlimited duration.
    , _weseaChildPolicy :: ChildPolicy
      -- ^ The policy to use for the child workflow executions if this
      -- workflow execution is terminated, by calling the
      -- TerminateWorkflowExecution action explicitly or due to an expired
      -- timeout. The supported child policies are: TERMINATE: the child
      -- executions will be terminated. REQUEST_CANCEL: a request to
      -- cancel will be attempted for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up
      -- to the decider to take appropriate actions when it receives an
      -- execution history with this event. ABANDON: no action will be
      -- taken. The child executions will continue to run.
    , _weseaTaskList :: TaskList
      -- ^ The name of the task list for scheduling the decision tasks for
      -- this workflow execution.
    , _weseaWorkflowType :: WorkflowType
      -- ^ The workflow type of this execution.
    , _weseaTagList :: [Text]
      -- ^ The list of tags associated with this workflow execution. An
      -- execution can have up to 5 tags.
    , _weseaContinuedExecutionRunId :: Maybe Text
      -- ^ If this workflow execution was started due to a
      -- ContinueAsNewWorkflowExecution decision, then it contains the
      -- runId of the previous workflow execution that was closed and
      -- continued as this execution.
    , _weseaParentWorkflowExecution :: Maybe WorkflowExecution
      -- ^ The source workflow execution that started this workflow
      -- execution. The member is not set if the workflow execution was
      -- not started by a workflow.
    , _weseaParentInitiatedEventId :: Maybe Integer
      -- ^ The id of the StartChildWorkflowExecutionInitiated event
      -- corresponding to the StartChildWorkflowExecution Decision to
      -- start this workflow execution. The source event with this Id can
      -- be found in the history of the source workflow execution. This
      -- information can be useful for diagnosing problems by tracing back
      -- the chain of events leading up to this event.
    } deriving (Show, Generic)

-- | The input provided to the workflow execution (if any).
weseaInput :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaInput = lens _weseaInput (\s a -> s { _weseaInput = a })
{-# INLINE weseaInput #-}

-- | The maximum duration for this workflow execution. The valid values are
-- integers greater than or equal to 0. An integer value can be used to
-- specify the duration in seconds while NONE can be used to specify unlimited
-- duration.
weseaExecutionStartToCloseTimeout :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaExecutionStartToCloseTimeout = lens _weseaExecutionStartToCloseTimeout (\s a -> s { _weseaExecutionStartToCloseTimeout = a })
{-# INLINE weseaExecutionStartToCloseTimeout #-}

-- | The maximum duration of decision tasks for this workflow type. The valid
-- values are integers greater than or equal to 0. An integer value can be
-- used to specify the duration in seconds while NONE can be used to specify
-- unlimited duration.
weseaTaskStartToCloseTimeout :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaTaskStartToCloseTimeout = lens _weseaTaskStartToCloseTimeout (\s a -> s { _weseaTaskStartToCloseTimeout = a })
{-# INLINE weseaTaskStartToCloseTimeout #-}

-- | The policy to use for the child workflow executions if this workflow
-- execution is terminated, by calling the TerminateWorkflowExecution action
-- explicitly or due to an expired timeout. The supported child policies are:
-- TERMINATE: the child executions will be terminated. REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a
-- WorkflowExecutionCancelRequested event in its history. It is up to the
-- decider to take appropriate actions when it receives an execution history
-- with this event. ABANDON: no action will be taken. The child executions
-- will continue to run.
weseaChildPolicy :: Lens' WorkflowExecutionStartedEventAttributes (ChildPolicy)
weseaChildPolicy = lens _weseaChildPolicy (\s a -> s { _weseaChildPolicy = a })
{-# INLINE weseaChildPolicy #-}

-- | The name of the task list for scheduling the decision tasks for this
-- workflow execution.
weseaTaskList :: Lens' WorkflowExecutionStartedEventAttributes (TaskList)
weseaTaskList = lens _weseaTaskList (\s a -> s { _weseaTaskList = a })
{-# INLINE weseaTaskList #-}

-- | The workflow type of this execution.
weseaWorkflowType :: Lens' WorkflowExecutionStartedEventAttributes (WorkflowType)
weseaWorkflowType = lens _weseaWorkflowType (\s a -> s { _weseaWorkflowType = a })
{-# INLINE weseaWorkflowType #-}

-- | The list of tags associated with this workflow execution. An execution can
-- have up to 5 tags.
weseaTagList :: Lens' WorkflowExecutionStartedEventAttributes ([Text])
weseaTagList = lens _weseaTagList (\s a -> s { _weseaTagList = a })
{-# INLINE weseaTagList #-}

-- | If this workflow execution was started due to a
-- ContinueAsNewWorkflowExecution decision, then it contains the runId of the
-- previous workflow execution that was closed and continued as this
-- execution.
weseaContinuedExecutionRunId :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaContinuedExecutionRunId = lens _weseaContinuedExecutionRunId (\s a -> s { _weseaContinuedExecutionRunId = a })
{-# INLINE weseaContinuedExecutionRunId #-}

-- | The source workflow execution that started this workflow execution. The
-- member is not set if the workflow execution was not started by a workflow.
weseaParentWorkflowExecution :: Lens' WorkflowExecutionStartedEventAttributes (Maybe WorkflowExecution)
weseaParentWorkflowExecution = lens _weseaParentWorkflowExecution (\s a -> s { _weseaParentWorkflowExecution = a })
{-# INLINE weseaParentWorkflowExecution #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this workflow execution.
-- The source event with this Id can be found in the history of the source
-- workflow execution. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
weseaParentInitiatedEventId :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Integer)
weseaParentInitiatedEventId = lens _weseaParentInitiatedEventId (\s a -> s { _weseaParentInitiatedEventId = a })
{-# INLINE weseaParentInitiatedEventId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WorkflowExecutionStartedEventAttributes' data type to populate a request.
mkWorkflowExecutionStartedEventAttributes :: ChildPolicy -- ^ 'weseaChildPolicy'
                                          -> TaskList -- ^ 'weseaTaskList'
                                          -> WorkflowType -- ^ 'weseaWorkflowType'
                                          -> WorkflowExecutionStartedEventAttributes
mkWorkflowExecutionStartedEventAttributes p1 p2 p3 = WorkflowExecutionStartedEventAttributes
    { _weseaInput = Nothing
    , _weseaExecutionStartToCloseTimeout = Nothing
    , _weseaTaskStartToCloseTimeout = Nothing
    , _weseaChildPolicy = p4
    , _weseaTaskList = p5
    , _weseaWorkflowType = p6
    , _weseaTagList = mempty
    , _weseaContinuedExecutionRunId = Nothing
    , _weseaParentWorkflowExecution = Nothing
    , _weseaParentInitiatedEventId = Nothing
    }
{-# INLINE mkWorkflowExecutionStartedEventAttributes #-}

instance FromJSON WorkflowExecutionStartedEventAttributes

instance ToJSON WorkflowExecutionStartedEventAttributes

-- | If the event is of type WorkflowExecutionTerminated then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionTerminatedEventAttributes = WorkflowExecutionTerminatedEventAttributes
    { _weteaReason :: Maybe Text
      -- ^ The reason provided for the termination (if any).
    , _weteaDetails :: Maybe Text
      -- ^ The details provided for the termination (if any).
    , _weteaChildPolicy :: ChildPolicy
      -- ^ The policy used for the child workflow executions of this
      -- workflow execution. The supported child policies are: TERMINATE:
      -- the child executions will be terminated. REQUEST_CANCEL: a
      -- request to cancel will be attempted for each child execution by
      -- recording a WorkflowExecutionCancelRequested event in its
      -- history. It is up to the decider to take appropriate actions when
      -- it receives an execution history with this event. ABANDON: no
      -- action will be taken. The child executions will continue to run.
    , _weteaCause :: Maybe WorkflowExecutionTerminatedCause
      -- ^ If set, indicates that the workflow execution was automatically
      -- terminated, and specifies the cause. This happens if the parent
      -- workflow execution times out or is terminated and the child
      -- policy is set to terminate child executions.
    } deriving (Show, Generic)

-- | The reason provided for the termination (if any).
weteaReason :: Lens' WorkflowExecutionTerminatedEventAttributes (Maybe Text)
weteaReason = lens _weteaReason (\s a -> s { _weteaReason = a })
{-# INLINE weteaReason #-}

-- | The details provided for the termination (if any).
weteaDetails :: Lens' WorkflowExecutionTerminatedEventAttributes (Maybe Text)
weteaDetails = lens _weteaDetails (\s a -> s { _weteaDetails = a })
{-# INLINE weteaDetails #-}

-- | The policy used for the child workflow executions of this workflow
-- execution. The supported child policies are: TERMINATE: the child
-- executions will be terminated. REQUEST_CANCEL: a request to cancel will be
-- attempted for each child execution by recording a
-- WorkflowExecutionCancelRequested event in its history. It is up to the
-- decider to take appropriate actions when it receives an execution history
-- with this event. ABANDON: no action will be taken. The child executions
-- will continue to run.
weteaChildPolicy :: Lens' WorkflowExecutionTerminatedEventAttributes (ChildPolicy)
weteaChildPolicy = lens _weteaChildPolicy (\s a -> s { _weteaChildPolicy = a })
{-# INLINE weteaChildPolicy #-}

-- | If set, indicates that the workflow execution was automatically terminated,
-- and specifies the cause. This happens if the parent workflow execution
-- times out or is terminated and the child policy is set to terminate child
-- executions.
weteaCause :: Lens' WorkflowExecutionTerminatedEventAttributes (Maybe WorkflowExecutionTerminatedCause)
weteaCause = lens _weteaCause (\s a -> s { _weteaCause = a })
{-# INLINE weteaCause #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WorkflowExecutionTerminatedEventAttributes' data type to populate a request.
mkWorkflowExecutionTerminatedEventAttributes :: ChildPolicy -- ^ 'weteaChildPolicy'
                                             -> WorkflowExecutionTerminatedEventAttributes
mkWorkflowExecutionTerminatedEventAttributes p1 = WorkflowExecutionTerminatedEventAttributes
    { _weteaReason = Nothing
    , _weteaDetails = Nothing
    , _weteaChildPolicy = p3
    , _weteaCause = Nothing
    }
{-# INLINE mkWorkflowExecutionTerminatedEventAttributes #-}

instance FromJSON WorkflowExecutionTerminatedEventAttributes

instance ToJSON WorkflowExecutionTerminatedEventAttributes

-- | If the event is of type WorkflowExecutionTimedOut then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
data WorkflowExecutionTimedOutEventAttributes = WorkflowExecutionTimedOutEventAttributes
    { _wetoeaTimeoutType :: WorkflowExecutionTimeoutType
      -- ^ The type of timeout that caused this event.
    , _wetoeaChildPolicy :: ChildPolicy
      -- ^ The policy used for the child workflow executions of this
      -- workflow execution. The supported child policies are: TERMINATE:
      -- the child executions will be terminated. REQUEST_CANCEL: a
      -- request to cancel will be attempted for each child execution by
      -- recording a WorkflowExecutionCancelRequested event in its
      -- history. It is up to the decider to take appropriate actions when
      -- it receives an execution history with this event. ABANDON: no
      -- action will be taken. The child executions will continue to run.
    } deriving (Show, Generic)

-- | The type of timeout that caused this event.
wetoeaTimeoutType :: Lens' WorkflowExecutionTimedOutEventAttributes (WorkflowExecutionTimeoutType)
wetoeaTimeoutType = lens _wetoeaTimeoutType (\s a -> s { _wetoeaTimeoutType = a })
{-# INLINE wetoeaTimeoutType #-}

-- | The policy used for the child workflow executions of this workflow
-- execution. The supported child policies are: TERMINATE: the child
-- executions will be terminated. REQUEST_CANCEL: a request to cancel will be
-- attempted for each child execution by recording a
-- WorkflowExecutionCancelRequested event in its history. It is up to the
-- decider to take appropriate actions when it receives an execution history
-- with this event. ABANDON: no action will be taken. The child executions
-- will continue to run.
wetoeaChildPolicy :: Lens' WorkflowExecutionTimedOutEventAttributes (ChildPolicy)
wetoeaChildPolicy = lens _wetoeaChildPolicy (\s a -> s { _wetoeaChildPolicy = a })
{-# INLINE wetoeaChildPolicy #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WorkflowExecutionTimedOutEventAttributes' data type to populate a request.
mkWorkflowExecutionTimedOutEventAttributes :: WorkflowExecutionTimeoutType -- ^ 'wetoeaTimeoutType'
                                           -> ChildPolicy -- ^ 'wetoeaChildPolicy'
                                           -> WorkflowExecutionTimedOutEventAttributes
mkWorkflowExecutionTimedOutEventAttributes p1 p2 = WorkflowExecutionTimedOutEventAttributes
    { _wetoeaTimeoutType = p1
    , _wetoeaChildPolicy = p2
    }
{-# INLINE mkWorkflowExecutionTimedOutEventAttributes #-}

instance FromJSON WorkflowExecutionTimedOutEventAttributes

instance ToJSON WorkflowExecutionTimedOutEventAttributes

-- | The workflow type to deprecate.
data WorkflowType = WorkflowType
    { _wtName :: Text
      -- ^ The name of the workflow type. This field is required. The
      -- combination of workflow type name and version must be unique with
      -- in a domain.
    , _wtVersion :: Text
      -- ^ The version of the workflow type. This field is required. The
      -- combination of workflow type name and version must be unique with
      -- in a domain.
    } deriving (Show, Generic)

-- | The name of the workflow type. This field is required. The combination of
-- workflow type name and version must be unique with in a domain.
wtName :: Lens' WorkflowType (Text)
wtName = lens _wtName (\s a -> s { _wtName = a })
{-# INLINE wtName #-}

-- | The version of the workflow type. This field is required. The combination
-- of workflow type name and version must be unique with in a domain.
wtVersion :: Lens' WorkflowType (Text)
wtVersion = lens _wtVersion (\s a -> s { _wtVersion = a })
{-# INLINE wtVersion #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WorkflowType' data type to populate a request.
mkWorkflowType :: Text -- ^ 'wtName'
               -> Text -- ^ 'wtVersion'
               -> WorkflowType
mkWorkflowType p1 p2 = WorkflowType
    { _wtName = p1
    , _wtVersion = p2
    }
{-# INLINE mkWorkflowType #-}

instance FromJSON WorkflowType

instance ToJSON WorkflowType

-- | Configuration settings of the workflow type registered through
-- RegisterWorkflowType.
data WorkflowTypeConfiguration = WorkflowTypeConfiguration
    { _wtcDefaultTaskStartToCloseTimeout :: Maybe Text
      -- ^ The optional default maximum duration, specified when registering
      -- the workflow type, that a decision task for executions of this
      -- workflow type might take before returning completion or failure.
      -- If the task does not close in the specified time then the task is
      -- automatically timed out and rescheduled. If the decider
      -- eventually reports a completion or failure, it is ignored. This
      -- default can be overridden when starting a workflow execution
      -- using the StartWorkflowExecution action or the
      -- StartChildWorkflowExecution Decision. The valid values are
      -- integers greater than or equal to 0. An integer value can be used
      -- to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration.
    , _wtcDefaultExecutionStartToCloseTimeout :: Maybe Text
      -- ^ The optional default maximum duration, specified when registering
      -- the workflow type, for executions of this workflow type. This
      -- default can be overridden when starting a workflow execution
      -- using the StartWorkflowExecution action or the
      -- StartChildWorkflowExecution Decision. The valid values are
      -- integers greater than or equal to 0. An integer value can be used
      -- to specify the duration in seconds while NONE can be used to
      -- specify unlimited duration.
    , _wtcDefaultTaskList :: Maybe TaskList
      -- ^ The optional default task list, specified when registering the
      -- workflow type, for decisions tasks scheduled for workflow
      -- executions of this type. This default can be overridden when
      -- starting a workflow execution using the StartWorkflowExecution
      -- action or the StartChildWorkflowExecution Decision.
    , _wtcDefaultChildPolicy :: Maybe ChildPolicy
      -- ^ The optional default policy to use for the child workflow
      -- executions when a workflow execution of this type is terminated,
      -- by calling the TerminateWorkflowExecution action explicitly or
      -- due to an expired timeout. This default can be overridden when
      -- starting a workflow execution using the StartWorkflowExecution
      -- action or the StartChildWorkflowExecution Decision. The supported
      -- child policies are: TERMINATE: the child executions will be
      -- terminated. REQUEST_CANCEL: a request to cancel will be attempted
      -- for each child execution by recording a
      -- WorkflowExecutionCancelRequested event in its history. It is up
      -- to the decider to take appropriate actions when it receives an
      -- execution history with this event. ABANDON: no action will be
      -- taken. The child executions will continue to run.
    } deriving (Show, Generic)

-- | The optional default maximum duration, specified when registering the
-- workflow type, that a decision task for executions of this workflow type
-- might take before returning completion or failure. If the task does not
-- close in the specified time then the task is automatically timed out and
-- rescheduled. If the decider eventually reports a completion or failure, it
-- is ignored. This default can be overridden when starting a workflow
-- execution using the StartWorkflowExecution action or the
-- StartChildWorkflowExecution Decision. The valid values are integers greater
-- than or equal to 0. An integer value can be used to specify the duration in
-- seconds while NONE can be used to specify unlimited duration.
wtcDefaultTaskStartToCloseTimeout :: Lens' WorkflowTypeConfiguration (Maybe Text)
wtcDefaultTaskStartToCloseTimeout = lens _wtcDefaultTaskStartToCloseTimeout (\s a -> s { _wtcDefaultTaskStartToCloseTimeout = a })
{-# INLINE wtcDefaultTaskStartToCloseTimeout #-}

-- | The optional default maximum duration, specified when registering the
-- workflow type, for executions of this workflow type. This default can be
-- overridden when starting a workflow execution using the
-- StartWorkflowExecution action or the StartChildWorkflowExecution Decision.
-- The valid values are integers greater than or equal to 0. An integer value
-- can be used to specify the duration in seconds while NONE can be used to
-- specify unlimited duration.
wtcDefaultExecutionStartToCloseTimeout :: Lens' WorkflowTypeConfiguration (Maybe Text)
wtcDefaultExecutionStartToCloseTimeout = lens _wtcDefaultExecutionStartToCloseTimeout (\s a -> s { _wtcDefaultExecutionStartToCloseTimeout = a })
{-# INLINE wtcDefaultExecutionStartToCloseTimeout #-}

-- | The optional default task list, specified when registering the workflow
-- type, for decisions tasks scheduled for workflow executions of this type.
-- This default can be overridden when starting a workflow execution using the
-- StartWorkflowExecution action or the StartChildWorkflowExecution Decision.
wtcDefaultTaskList :: Lens' WorkflowTypeConfiguration (Maybe TaskList)
wtcDefaultTaskList = lens _wtcDefaultTaskList (\s a -> s { _wtcDefaultTaskList = a })
{-# INLINE wtcDefaultTaskList #-}

-- | The optional default policy to use for the child workflow executions when a
-- workflow execution of this type is terminated, by calling the
-- TerminateWorkflowExecution action explicitly or due to an expired timeout.
-- This default can be overridden when starting a workflow execution using the
-- StartWorkflowExecution action or the StartChildWorkflowExecution Decision.
-- The supported child policies are: TERMINATE: the child executions will be
-- terminated. REQUEST_CANCEL: a request to cancel will be attempted for each
-- child execution by recording a WorkflowExecutionCancelRequested event in
-- its history. It is up to the decider to take appropriate actions when it
-- receives an execution history with this event. ABANDON: no action will be
-- taken. The child executions will continue to run.
wtcDefaultChildPolicy :: Lens' WorkflowTypeConfiguration (Maybe ChildPolicy)
wtcDefaultChildPolicy = lens _wtcDefaultChildPolicy (\s a -> s { _wtcDefaultChildPolicy = a })
{-# INLINE wtcDefaultChildPolicy #-}

instance FromJSON WorkflowTypeConfiguration

-- | If specified, indicates the type of the workflow executions to be counted.
-- closeStatusFilter, executionFilter, typeFilter and tagFilter are mutually
-- exclusive. You can specify at most one of these in a request.
data WorkflowTypeFilter = WorkflowTypeFilter
    { _wtfName :: Text
      -- ^ Name of the workflow type. This field is required.
    , _wtfVersion :: Maybe Text
      -- ^ Version of the workflow type.
    } deriving (Show, Generic)

-- | Name of the workflow type. This field is required.
wtfName :: Lens' WorkflowTypeFilter (Text)
wtfName = lens _wtfName (\s a -> s { _wtfName = a })
{-# INLINE wtfName #-}

-- | Version of the workflow type.
wtfVersion :: Lens' WorkflowTypeFilter (Maybe Text)
wtfVersion = lens _wtfVersion (\s a -> s { _wtfVersion = a })
{-# INLINE wtfVersion #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'WorkflowTypeFilter' data type to populate a request.
mkWorkflowTypeFilter :: Text -- ^ 'wtfName'
                     -> WorkflowTypeFilter
mkWorkflowTypeFilter p1 = WorkflowTypeFilter
    { _wtfName = p1
    , _wtfVersion = Nothing
    }
{-# INLINE mkWorkflowTypeFilter #-}

instance ToJSON WorkflowTypeFilter

-- | General information about the workflow type. The status of the workflow
-- type (returned in the WorkflowTypeInfo structure) can be one of the
-- following. REGISTERED: The type is registered and available. Workers
-- supporting this type should be running. DEPRECATED: The type was deprecated
-- using DeprecateWorkflowType, but is still in use. You should keep workers
-- supporting this type running. You cannot create new workflow executions of
-- this type.
data WorkflowTypeInfo = WorkflowTypeInfo
    { _wtiWorkflowType :: WorkflowType
      -- ^ The workflow type this information is about.
    , _wtiStatus :: RegistrationStatus
      -- ^ The current status of the workflow type.
    , _wtiDescription :: Maybe Text
      -- ^ The description of the type registered through
      -- RegisterWorkflowType.
    , _wtiCreationDate :: POSIX
      -- ^ The date when this type was registered.
    , _wtiDeprecationDate :: Maybe POSIX
      -- ^ If the type is in deprecated state, then it is set to the date
      -- when the type was deprecated.
    } deriving (Show, Generic)

-- | The workflow type this information is about.
wtiWorkflowType :: Lens' WorkflowTypeInfo (WorkflowType)
wtiWorkflowType = lens _wtiWorkflowType (\s a -> s { _wtiWorkflowType = a })
{-# INLINE wtiWorkflowType #-}

-- | The current status of the workflow type.
wtiStatus :: Lens' WorkflowTypeInfo (RegistrationStatus)
wtiStatus = lens _wtiStatus (\s a -> s { _wtiStatus = a })
{-# INLINE wtiStatus #-}

-- | The description of the type registered through RegisterWorkflowType.
wtiDescription :: Lens' WorkflowTypeInfo (Maybe Text)
wtiDescription = lens _wtiDescription (\s a -> s { _wtiDescription = a })
{-# INLINE wtiDescription #-}

-- | The date when this type was registered.
wtiCreationDate :: Lens' WorkflowTypeInfo (POSIX)
wtiCreationDate = lens _wtiCreationDate (\s a -> s { _wtiCreationDate = a })
{-# INLINE wtiCreationDate #-}

-- | If the type is in deprecated state, then it is set to the date when the
-- type was deprecated.
wtiDeprecationDate :: Lens' WorkflowTypeInfo (Maybe POSIX)
wtiDeprecationDate = lens _wtiDeprecationDate (\s a -> s { _wtiDeprecationDate = a })
{-# INLINE wtiDeprecationDate #-}

instance FromJSON WorkflowTypeInfo
