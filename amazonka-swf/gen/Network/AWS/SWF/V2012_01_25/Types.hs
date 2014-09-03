{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
    , CancelTimerDecisionAttributes (..)
    , ctdaTimerId

    -- * CancelWorkflowExecutionDecisionAttributes
    , CancelWorkflowExecutionDecisionAttributes (..)
    , cwedbDetails

    -- * CloseStatusFilter
    , CloseStatusFilter (..)
    , csfStatus

    -- * CompleteWorkflowExecutionDecisionAttributes
    , CompleteWorkflowExecutionDecisionAttributes (..)
    , cwedaResult

    -- * DomainConfiguration
    , DomainConfiguration (..)
    , dcWorkflowExecutionRetentionPeriodInDays

    -- * RequestCancelActivityTaskDecisionAttributes
    , RequestCancelActivityTaskDecisionAttributes (..)
    , rcatdaActivityId

    -- * TagFilter
    , TagFilter (..)
    , tfTag

    -- * TaskList
    , TaskList (..)
    , tlName

    -- * WorkflowExecutionFilter
    , WorkflowExecutionFilter (..)
    , wefWorkflowId

    -- * ActivityTaskCancelRequestedEventAttributes
    , ActivityTaskCancelRequestedEventAttributes (..)
    , atcreaDecisionTaskCompletedEventId
    , atcreaActivityId

    -- * ActivityTaskCanceledEventAttributes
    , ActivityTaskCanceledEventAttributes (..)
    , atcebDetails
    , atcebScheduledEventId
    , atcebStartedEventId
    , atcebLatestCancelRequestedEventId

    -- * ActivityTaskCompletedEventAttributes
    , ActivityTaskCompletedEventAttributes (..)
    , atceaResult
    , atceaScheduledEventId
    , atceaStartedEventId

    -- * ActivityTaskFailedEventAttributes
    , ActivityTaskFailedEventAttributes (..)
    , atfeaReason
    , atfeaDetails
    , atfeaScheduledEventId
    , atfeaStartedEventId

    -- * ActivityTaskScheduledEventAttributes
    , ActivityTaskScheduledEventAttributes (..)
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
    , ActivityTaskStartedEventAttributes (..)
    , atsebIdentity
    , atsebScheduledEventId

    -- * ActivityTaskTimedOutEventAttributes
    , ActivityTaskTimedOutEventAttributes (..)
    , attoeaTimeoutType
    , attoeaScheduledEventId
    , attoeaStartedEventId
    , attoeaDetails

    -- * ActivityType
    , ActivityType (..)
    , atName
    , atVersion

    -- * ActivityTypeConfiguration
    , ActivityTypeConfiguration (..)
    , atcDefaultTaskStartToCloseTimeout
    , atcDefaultTaskHeartbeatTimeout
    , atcDefaultTaskList
    , atcDefaultTaskScheduleToStartTimeout
    , atcDefaultTaskScheduleToCloseTimeout

    -- * ActivityTypeInfo
    , ActivityTypeInfo (..)
    , atiActivityType
    , atiStatus
    , atiDescription
    , atiCreationDate
    , atiDeprecationDate

    -- * CancelTimerFailedEventAttributes
    , CancelTimerFailedEventAttributes (..)
    , ctfeaTimerId
    , ctfeaCause
    , ctfeaDecisionTaskCompletedEventId

    -- * CancelWorkflowExecutionFailedEventAttributes
    , CancelWorkflowExecutionFailedEventAttributes (..)
    , cwefebCause
    , cwefebDecisionTaskCompletedEventId

    -- * ChildWorkflowExecutionCanceledEventAttributes
    , ChildWorkflowExecutionCanceledEventAttributes (..)
    , cwecebWorkflowExecution
    , cwecebWorkflowType
    , cwecebDetails
    , cwecebInitiatedEventId
    , cwecebStartedEventId

    -- * ChildWorkflowExecutionCompletedEventAttributes
    , ChildWorkflowExecutionCompletedEventAttributes (..)
    , cweceaWorkflowExecution
    , cweceaWorkflowType
    , cweceaResult
    , cweceaInitiatedEventId
    , cweceaStartedEventId

    -- * ChildWorkflowExecutionFailedEventAttributes
    , ChildWorkflowExecutionFailedEventAttributes (..)
    , cwefecWorkflowExecution
    , cwefecWorkflowType
    , cwefecReason
    , cwefecDetails
    , cwefecInitiatedEventId
    , cwefecStartedEventId

    -- * ChildWorkflowExecutionStartedEventAttributes
    , ChildWorkflowExecutionStartedEventAttributes (..)
    , cweseaWorkflowExecution
    , cweseaWorkflowType
    , cweseaInitiatedEventId

    -- * ChildWorkflowExecutionTerminatedEventAttributes
    , ChildWorkflowExecutionTerminatedEventAttributes (..)
    , cweteaWorkflowExecution
    , cweteaWorkflowType
    , cweteaInitiatedEventId
    , cweteaStartedEventId

    -- * ChildWorkflowExecutionTimedOutEventAttributes
    , ChildWorkflowExecutionTimedOutEventAttributes (..)
    , cwetoeaWorkflowExecution
    , cwetoeaWorkflowType
    , cwetoeaTimeoutType
    , cwetoeaInitiatedEventId
    , cwetoeaStartedEventId

    -- * CompleteWorkflowExecutionFailedEventAttributes
    , CompleteWorkflowExecutionFailedEventAttributes (..)
    , cwefeaCause
    , cwefeaDecisionTaskCompletedEventId

    -- * ContinueAsNewWorkflowExecutionDecisionAttributes
    , ContinueAsNewWorkflowExecutionDecisionAttributes (..)
    , canwedaInput
    , canwedaExecutionStartToCloseTimeout
    , canwedaTaskList
    , canwedaTaskStartToCloseTimeout
    , canwedaChildPolicy
    , canwedaTagList
    , canwedaWorkflowTypeVersion

    -- * ContinueAsNewWorkflowExecutionFailedEventAttributes
    , ContinueAsNewWorkflowExecutionFailedEventAttributes (..)
    , canwefeaCause
    , canwefeaDecisionTaskCompletedEventId

    -- * Decision
    , Decision (..)
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
    , DecisionTaskCompletedEventAttributes (..)
    , dtceaExecutionContext
    , dtceaScheduledEventId
    , dtceaStartedEventId

    -- * DecisionTaskScheduledEventAttributes
    , DecisionTaskScheduledEventAttributes (..)
    , dtseaTaskList
    , dtseaStartToCloseTimeout

    -- * DecisionTaskStartedEventAttributes
    , DecisionTaskStartedEventAttributes (..)
    , dtsebIdentity
    , dtsebScheduledEventId

    -- * DecisionTaskTimedOutEventAttributes
    , DecisionTaskTimedOutEventAttributes (..)
    , dttoeaTimeoutType
    , dttoeaScheduledEventId
    , dttoeaStartedEventId

    -- * DomainInfo
    , DomainInfo (..)
    , diName
    , diStatus
    , diDescription

    -- * ExecutionTimeFilter
    , ExecutionTimeFilter (..)
    , etfOldestDate
    , etfLatestDate

    -- * ExternalWorkflowExecutionCancelRequestedEventAttributes
    , ExternalWorkflowExecutionCancelRequestedEventAttributes (..)
    , ewecreaWorkflowExecution
    , ewecreaInitiatedEventId

    -- * ExternalWorkflowExecutionSignaledEventAttributes
    , ExternalWorkflowExecutionSignaledEventAttributes (..)
    , eweseaWorkflowExecution
    , eweseaInitiatedEventId

    -- * FailWorkflowExecutionDecisionAttributes
    , FailWorkflowExecutionDecisionAttributes (..)
    , fwedaReason
    , fwedaDetails

    -- * FailWorkflowExecutionFailedEventAttributes
    , FailWorkflowExecutionFailedEventAttributes (..)
    , fwefeaCause
    , fwefeaDecisionTaskCompletedEventId

    -- * HistoryEvent
    , HistoryEvent (..)
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
    , MarkerRecordedEventAttributes (..)
    , mreaMarkerName
    , mreaDetails
    , mreaDecisionTaskCompletedEventId

    -- * RecordMarkerDecisionAttributes
    , RecordMarkerDecisionAttributes (..)
    , rmdaMarkerName
    , rmdaDetails

    -- * RecordMarkerFailedEventAttributes
    , RecordMarkerFailedEventAttributes (..)
    , rmfeaMarkerName
    , rmfeaCause
    , rmfeaDecisionTaskCompletedEventId

    -- * RequestCancelActivityTaskFailedEventAttributes
    , RequestCancelActivityTaskFailedEventAttributes (..)
    , rcatfeaActivityId
    , rcatfeaCause
    , rcatfeaDecisionTaskCompletedEventId

    -- * RequestCancelExternalWorkflowExecutionDecisionAttributes
    , RequestCancelExternalWorkflowExecutionDecisionAttributes (..)
    , rcewedaWorkflowId
    , rcewedaRunId
    , rcewedaControl

    -- * RequestCancelExternalWorkflowExecutionFailedEventAttributes
    , RequestCancelExternalWorkflowExecutionFailedEventAttributes (..)
    , rcewefeaWorkflowId
    , rcewefeaRunId
    , rcewefeaCause
    , rcewefeaInitiatedEventId
    , rcewefeaDecisionTaskCompletedEventId
    , rcewefeaControl

    -- * RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (..)
    , rceweieaWorkflowId
    , rceweieaRunId
    , rceweieaDecisionTaskCompletedEventId
    , rceweieaControl

    -- * ScheduleActivityTaskDecisionAttributes
    , ScheduleActivityTaskDecisionAttributes (..)
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
    , ScheduleActivityTaskFailedEventAttributes (..)
    , satfeaActivityType
    , satfeaActivityId
    , satfeaCause
    , satfeaDecisionTaskCompletedEventId

    -- * SignalExternalWorkflowExecutionDecisionAttributes
    , SignalExternalWorkflowExecutionDecisionAttributes (..)
    , sewedaWorkflowId
    , sewedaRunId
    , sewedaSignalName
    , sewedaInput
    , sewedaControl

    -- * SignalExternalWorkflowExecutionFailedEventAttributes
    , SignalExternalWorkflowExecutionFailedEventAttributes (..)
    , sewefeaWorkflowId
    , sewefeaRunId
    , sewefeaCause
    , sewefeaInitiatedEventId
    , sewefeaDecisionTaskCompletedEventId
    , sewefeaControl

    -- * SignalExternalWorkflowExecutionInitiatedEventAttributes
    , SignalExternalWorkflowExecutionInitiatedEventAttributes (..)
    , seweieaWorkflowId
    , seweieaRunId
    , seweieaSignalName
    , seweieaInput
    , seweieaDecisionTaskCompletedEventId
    , seweieaControl

    -- * StartChildWorkflowExecutionDecisionAttributes
    , StartChildWorkflowExecutionDecisionAttributes (..)
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
    , StartChildWorkflowExecutionFailedEventAttributes (..)
    , scwefeaWorkflowType
    , scwefeaCause
    , scwefeaWorkflowId
    , scwefeaInitiatedEventId
    , scwefeaDecisionTaskCompletedEventId
    , scwefeaControl

    -- * StartChildWorkflowExecutionInitiatedEventAttributes
    , StartChildWorkflowExecutionInitiatedEventAttributes (..)
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
    , StartTimerDecisionAttributes (..)
    , stdaTimerId
    , stdaControl
    , stdaStartToFireTimeout

    -- * StartTimerFailedEventAttributes
    , StartTimerFailedEventAttributes (..)
    , stfeaTimerId
    , stfeaCause
    , stfeaDecisionTaskCompletedEventId

    -- * TimerCanceledEventAttributes
    , TimerCanceledEventAttributes (..)
    , tceaTimerId
    , tceaStartedEventId
    , tceaDecisionTaskCompletedEventId

    -- * TimerFiredEventAttributes
    , TimerFiredEventAttributes (..)
    , tfeaTimerId
    , tfeaStartedEventId

    -- * TimerStartedEventAttributes
    , TimerStartedEventAttributes (..)
    , tseaTimerId
    , tseaControl
    , tseaStartToFireTimeout
    , tseaDecisionTaskCompletedEventId

    -- * WorkflowExecution
    , WorkflowExecution (..)
    , weWorkflowId
    , weRunId

    -- * WorkflowExecutionCancelRequestedEventAttributes
    , WorkflowExecutionCancelRequestedEventAttributes (..)
    , wecreaExternalWorkflowExecution
    , wecreaExternalInitiatedEventId
    , wecreaCause

    -- * WorkflowExecutionCanceledEventAttributes
    , WorkflowExecutionCanceledEventAttributes (..)
    , wecebDetails
    , wecebDecisionTaskCompletedEventId

    -- * WorkflowExecutionCompletedEventAttributes
    , WorkflowExecutionCompletedEventAttributes (..)
    , weceaResult
    , weceaDecisionTaskCompletedEventId

    -- * WorkflowExecutionConfiguration
    , WorkflowExecutionConfiguration (..)
    , wehTaskStartToCloseTimeout
    , wehExecutionStartToCloseTimeout
    , wehTaskList
    , wehChildPolicy

    -- * WorkflowExecutionContinuedAsNewEventAttributes
    , WorkflowExecutionContinuedAsNewEventAttributes (..)
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
    , WorkflowExecutionFailedEventAttributes (..)
    , wefeaReason
    , wefeaDetails
    , wefeaDecisionTaskCompletedEventId

    -- * WorkflowExecutionInfo
    , WorkflowExecutionInfo (..)
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
    , WorkflowExecutionOpenCounts (..)
    , weocOpenActivityTasks
    , weocOpenDecisionTasks
    , weocOpenTimers
    , weocOpenChildWorkflowExecutions

    -- * WorkflowExecutionSignaledEventAttributes
    , WorkflowExecutionSignaledEventAttributes (..)
    , wesebSignalName
    , wesebInput
    , wesebExternalWorkflowExecution
    , wesebExternalInitiatedEventId

    -- * WorkflowExecutionStartedEventAttributes
    , WorkflowExecutionStartedEventAttributes (..)
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
    , WorkflowExecutionTerminatedEventAttributes (..)
    , weteaReason
    , weteaDetails
    , weteaChildPolicy
    , weteaCause

    -- * WorkflowExecutionTimedOutEventAttributes
    , WorkflowExecutionTimedOutEventAttributes (..)
    , wetoeaTimeoutType
    , wetoeaChildPolicy

    -- * WorkflowType
    , WorkflowType (..)
    , wtName
    , wtVersion

    -- * WorkflowTypeConfiguration
    , WorkflowTypeConfiguration (..)
    , wtcDefaultTaskStartToCloseTimeout
    , wtcDefaultExecutionStartToCloseTimeout
    , wtcDefaultTaskList
    , wtcDefaultChildPolicy

    -- * WorkflowTypeFilter
    , WorkflowTypeFilter (..)
    , wtfName
    , wtfVersion

    -- * WorkflowTypeInfo
    , WorkflowTypeInfo (..)
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
ctdaTimerId
    :: Functor f
    => (Text
    -> f (Text))
    -> CancelTimerDecisionAttributes
    -> f CancelTimerDecisionAttributes
ctdaTimerId f x =
    (\y -> x { _ctdaTimerId = y })
       <$> f (_ctdaTimerId x)
{-# INLINE ctdaTimerId #-}

instance FromJSON CancelTimerDecisionAttributes

instance ToJSON CancelTimerDecisionAttributes

-- | Provides details of the CancelWorkflowExecution decision. It is not set for
-- other decision types.
newtype CancelWorkflowExecutionDecisionAttributes = CancelWorkflowExecutionDecisionAttributes
    { _cwedbDetails :: Maybe Text
      -- ^ Optional details of the cancellation.
    } deriving (Show, Generic)

-- | Optional details of the cancellation.
cwedbDetails
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CancelWorkflowExecutionDecisionAttributes
    -> f CancelWorkflowExecutionDecisionAttributes
cwedbDetails f x =
    (\y -> x { _cwedbDetails = y })
       <$> f (_cwedbDetails x)
{-# INLINE cwedbDetails #-}

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
csfStatus
    :: Functor f
    => (CloseStatus
    -> f (CloseStatus))
    -> CloseStatusFilter
    -> f CloseStatusFilter
csfStatus f x =
    (\y -> x { _csfStatus = y })
       <$> f (_csfStatus x)
{-# INLINE csfStatus #-}

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
cwedaResult
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CompleteWorkflowExecutionDecisionAttributes
    -> f CompleteWorkflowExecutionDecisionAttributes
cwedaResult f x =
    (\y -> x { _cwedaResult = y })
       <$> f (_cwedaResult x)
{-# INLINE cwedaResult #-}

instance FromJSON CompleteWorkflowExecutionDecisionAttributes

instance ToJSON CompleteWorkflowExecutionDecisionAttributes

-- | Contains the configuration settings of a domain.
newtype DomainConfiguration = DomainConfiguration
    { _dcWorkflowExecutionRetentionPeriodInDays :: Text
      -- ^ The retention period for workflow executions in this domain.
    } deriving (Show, Generic)

-- | The retention period for workflow executions in this domain.
dcWorkflowExecutionRetentionPeriodInDays
    :: Functor f
    => (Text
    -> f (Text))
    -> DomainConfiguration
    -> f DomainConfiguration
dcWorkflowExecutionRetentionPeriodInDays f x =
    (\y -> x { _dcWorkflowExecutionRetentionPeriodInDays = y })
       <$> f (_dcWorkflowExecutionRetentionPeriodInDays x)
{-# INLINE dcWorkflowExecutionRetentionPeriodInDays #-}

instance FromJSON DomainConfiguration

-- | Provides details of the RequestCancelActivityTask decision. It is not set
-- for other decision types.
newtype RequestCancelActivityTaskDecisionAttributes = RequestCancelActivityTaskDecisionAttributes
    { _rcatdaActivityId :: Text
      -- ^ The activityId of the activity task to be canceled.
    } deriving (Show, Generic)

-- | The activityId of the activity task to be canceled.
rcatdaActivityId
    :: Functor f
    => (Text
    -> f (Text))
    -> RequestCancelActivityTaskDecisionAttributes
    -> f RequestCancelActivityTaskDecisionAttributes
rcatdaActivityId f x =
    (\y -> x { _rcatdaActivityId = y })
       <$> f (_rcatdaActivityId x)
{-# INLINE rcatdaActivityId #-}

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
tfTag
    :: Functor f
    => (Text
    -> f (Text))
    -> TagFilter
    -> f TagFilter
tfTag f x =
    (\y -> x { _tfTag = y })
       <$> f (_tfTag x)
{-# INLINE tfTag #-}

instance ToJSON TagFilter

-- | The name of the task list.
newtype TaskList = TaskList
    { _tlName :: Text
      -- ^ The name of the task list.
    } deriving (Show, Generic)

-- | The name of the task list.
tlName
    :: Functor f
    => (Text
    -> f (Text))
    -> TaskList
    -> f TaskList
tlName f x =
    (\y -> x { _tlName = y })
       <$> f (_tlName x)
{-# INLINE tlName #-}

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
wefWorkflowId
    :: Functor f
    => (Text
    -> f (Text))
    -> WorkflowExecutionFilter
    -> f WorkflowExecutionFilter
wefWorkflowId f x =
    (\y -> x { _wefWorkflowId = y })
       <$> f (_wefWorkflowId x)
{-# INLINE wefWorkflowId #-}

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
atcreaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ActivityTaskCancelRequestedEventAttributes
    -> f ActivityTaskCancelRequestedEventAttributes
atcreaDecisionTaskCompletedEventId f x =
    (\y -> x { _atcreaDecisionTaskCompletedEventId = y })
       <$> f (_atcreaDecisionTaskCompletedEventId x)
{-# INLINE atcreaDecisionTaskCompletedEventId #-}

-- | The unique ID of the task.
atcreaActivityId
    :: Functor f
    => (Text
    -> f (Text))
    -> ActivityTaskCancelRequestedEventAttributes
    -> f ActivityTaskCancelRequestedEventAttributes
atcreaActivityId f x =
    (\y -> x { _atcreaActivityId = y })
       <$> f (_atcreaActivityId x)
{-# INLINE atcreaActivityId #-}

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
atcebDetails
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTaskCanceledEventAttributes
    -> f ActivityTaskCanceledEventAttributes
atcebDetails f x =
    (\y -> x { _atcebDetails = y })
       <$> f (_atcebDetails x)
{-# INLINE atcebDetails #-}

-- | The id of the ActivityTaskScheduled event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atcebScheduledEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ActivityTaskCanceledEventAttributes
    -> f ActivityTaskCanceledEventAttributes
atcebScheduledEventId f x =
    (\y -> x { _atcebScheduledEventId = y })
       <$> f (_atcebScheduledEventId x)
{-# INLINE atcebScheduledEventId #-}

-- | The Id of the ActivityTaskStarted event recorded when this activity task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
atcebStartedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ActivityTaskCanceledEventAttributes
    -> f ActivityTaskCanceledEventAttributes
atcebStartedEventId f x =
    (\y -> x { _atcebStartedEventId = y })
       <$> f (_atcebStartedEventId x)
{-# INLINE atcebStartedEventId #-}

-- | If set, contains the Id of the last ActivityTaskCancelRequested event
-- recorded for this activity task. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
atcebLatestCancelRequestedEventId
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ActivityTaskCanceledEventAttributes
    -> f ActivityTaskCanceledEventAttributes
atcebLatestCancelRequestedEventId f x =
    (\y -> x { _atcebLatestCancelRequestedEventId = y })
       <$> f (_atcebLatestCancelRequestedEventId x)
{-# INLINE atcebLatestCancelRequestedEventId #-}

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
atceaResult
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTaskCompletedEventAttributes
    -> f ActivityTaskCompletedEventAttributes
atceaResult f x =
    (\y -> x { _atceaResult = y })
       <$> f (_atceaResult x)
{-# INLINE atceaResult #-}

-- | The id of the ActivityTaskScheduled event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atceaScheduledEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ActivityTaskCompletedEventAttributes
    -> f ActivityTaskCompletedEventAttributes
atceaScheduledEventId f x =
    (\y -> x { _atceaScheduledEventId = y })
       <$> f (_atceaScheduledEventId x)
{-# INLINE atceaScheduledEventId #-}

-- | The Id of the ActivityTaskStarted event recorded when this activity task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
atceaStartedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ActivityTaskCompletedEventAttributes
    -> f ActivityTaskCompletedEventAttributes
atceaStartedEventId f x =
    (\y -> x { _atceaStartedEventId = y })
       <$> f (_atceaStartedEventId x)
{-# INLINE atceaStartedEventId #-}

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
atfeaReason
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTaskFailedEventAttributes
    -> f ActivityTaskFailedEventAttributes
atfeaReason f x =
    (\y -> x { _atfeaReason = y })
       <$> f (_atfeaReason x)
{-# INLINE atfeaReason #-}

-- | The details of the failure (if any).
atfeaDetails
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTaskFailedEventAttributes
    -> f ActivityTaskFailedEventAttributes
atfeaDetails f x =
    (\y -> x { _atfeaDetails = y })
       <$> f (_atfeaDetails x)
{-# INLINE atfeaDetails #-}

-- | The id of the ActivityTaskScheduled event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atfeaScheduledEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ActivityTaskFailedEventAttributes
    -> f ActivityTaskFailedEventAttributes
atfeaScheduledEventId f x =
    (\y -> x { _atfeaScheduledEventId = y })
       <$> f (_atfeaScheduledEventId x)
{-# INLINE atfeaScheduledEventId #-}

-- | The Id of the ActivityTaskStarted event recorded when this activity task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
atfeaStartedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ActivityTaskFailedEventAttributes
    -> f ActivityTaskFailedEventAttributes
atfeaStartedEventId f x =
    (\y -> x { _atfeaStartedEventId = y })
       <$> f (_atfeaStartedEventId x)
{-# INLINE atfeaStartedEventId #-}

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
atseaActivityType
    :: Functor f
    => (ActivityType
    -> f (ActivityType))
    -> ActivityTaskScheduledEventAttributes
    -> f ActivityTaskScheduledEventAttributes
atseaActivityType f x =
    (\y -> x { _atseaActivityType = y })
       <$> f (_atseaActivityType x)
{-# INLINE atseaActivityType #-}

-- | The unique id of the activity task.
atseaActivityId
    :: Functor f
    => (Text
    -> f (Text))
    -> ActivityTaskScheduledEventAttributes
    -> f ActivityTaskScheduledEventAttributes
atseaActivityId f x =
    (\y -> x { _atseaActivityId = y })
       <$> f (_atseaActivityId x)
{-# INLINE atseaActivityId #-}

-- | The input provided to the activity task.
atseaInput
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTaskScheduledEventAttributes
    -> f ActivityTaskScheduledEventAttributes
atseaInput f x =
    (\y -> x { _atseaInput = y })
       <$> f (_atseaInput x)
{-# INLINE atseaInput #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks. This data is not sent to the activity.
atseaControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTaskScheduledEventAttributes
    -> f ActivityTaskScheduledEventAttributes
atseaControl f x =
    (\y -> x { _atseaControl = y })
       <$> f (_atseaControl x)
{-# INLINE atseaControl #-}

-- | The maximum amount of time the activity task can wait to be assigned to a
-- worker.
atseaScheduleToStartTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTaskScheduledEventAttributes
    -> f ActivityTaskScheduledEventAttributes
atseaScheduleToStartTimeout f x =
    (\y -> x { _atseaScheduleToStartTimeout = y })
       <$> f (_atseaScheduleToStartTimeout x)
{-# INLINE atseaScheduleToStartTimeout #-}

-- | The maximum amount of time for this activity task.
atseaScheduleToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTaskScheduledEventAttributes
    -> f ActivityTaskScheduledEventAttributes
atseaScheduleToCloseTimeout f x =
    (\y -> x { _atseaScheduleToCloseTimeout = y })
       <$> f (_atseaScheduleToCloseTimeout x)
{-# INLINE atseaScheduleToCloseTimeout #-}

-- | The maximum amount of time a worker may take to process the activity task.
atseaStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTaskScheduledEventAttributes
    -> f ActivityTaskScheduledEventAttributes
atseaStartToCloseTimeout f x =
    (\y -> x { _atseaStartToCloseTimeout = y })
       <$> f (_atseaStartToCloseTimeout x)
{-# INLINE atseaStartToCloseTimeout #-}

-- | The task list in which the activity task has been scheduled.
atseaTaskList
    :: Functor f
    => (TaskList
    -> f (TaskList))
    -> ActivityTaskScheduledEventAttributes
    -> f ActivityTaskScheduledEventAttributes
atseaTaskList f x =
    (\y -> x { _atseaTaskList = y })
       <$> f (_atseaTaskList x)
{-# INLINE atseaTaskList #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- that resulted in the scheduling of this activity task. This information can
-- be useful for diagnosing problems by tracing back the chain of events
-- leading up to this event.
atseaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ActivityTaskScheduledEventAttributes
    -> f ActivityTaskScheduledEventAttributes
atseaDecisionTaskCompletedEventId f x =
    (\y -> x { _atseaDecisionTaskCompletedEventId = y })
       <$> f (_atseaDecisionTaskCompletedEventId x)
{-# INLINE atseaDecisionTaskCompletedEventId #-}

-- | The maximum time before which the worker processing this task must report
-- progress by calling RecordActivityTaskHeartbeat. If the timeout is
-- exceeded, the activity task is automatically timed out. If the worker
-- subsequently attempts to record a heartbeat or return a result, it will be
-- ignored.
atseaHeartbeatTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTaskScheduledEventAttributes
    -> f ActivityTaskScheduledEventAttributes
atseaHeartbeatTimeout f x =
    (\y -> x { _atseaHeartbeatTimeout = y })
       <$> f (_atseaHeartbeatTimeout x)
{-# INLINE atseaHeartbeatTimeout #-}

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
atsebIdentity
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTaskStartedEventAttributes
    -> f ActivityTaskStartedEventAttributes
atsebIdentity f x =
    (\y -> x { _atsebIdentity = y })
       <$> f (_atsebIdentity x)
{-# INLINE atsebIdentity #-}

-- | The id of the ActivityTaskScheduled event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atsebScheduledEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ActivityTaskStartedEventAttributes
    -> f ActivityTaskStartedEventAttributes
atsebScheduledEventId f x =
    (\y -> x { _atsebScheduledEventId = y })
       <$> f (_atsebScheduledEventId x)
{-# INLINE atsebScheduledEventId #-}

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
attoeaTimeoutType
    :: Functor f
    => (ActivityTaskTimeoutType
    -> f (ActivityTaskTimeoutType))
    -> ActivityTaskTimedOutEventAttributes
    -> f ActivityTaskTimedOutEventAttributes
attoeaTimeoutType f x =
    (\y -> x { _attoeaTimeoutType = y })
       <$> f (_attoeaTimeoutType x)
{-# INLINE attoeaTimeoutType #-}

-- | The id of the ActivityTaskScheduled event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
attoeaScheduledEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ActivityTaskTimedOutEventAttributes
    -> f ActivityTaskTimedOutEventAttributes
attoeaScheduledEventId f x =
    (\y -> x { _attoeaScheduledEventId = y })
       <$> f (_attoeaScheduledEventId x)
{-# INLINE attoeaScheduledEventId #-}

-- | The Id of the ActivityTaskStarted event recorded when this activity task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
attoeaStartedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ActivityTaskTimedOutEventAttributes
    -> f ActivityTaskTimedOutEventAttributes
attoeaStartedEventId f x =
    (\y -> x { _attoeaStartedEventId = y })
       <$> f (_attoeaStartedEventId x)
{-# INLINE attoeaStartedEventId #-}

-- | Contains the content of the details parameter for the last call made by the
-- activity to RecordActivityTaskHeartbeat.
attoeaDetails
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTaskTimedOutEventAttributes
    -> f ActivityTaskTimedOutEventAttributes
attoeaDetails f x =
    (\y -> x { _attoeaDetails = y })
       <$> f (_attoeaDetails x)
{-# INLINE attoeaDetails #-}

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
atName
    :: Functor f
    => (Text
    -> f (Text))
    -> ActivityType
    -> f ActivityType
atName f x =
    (\y -> x { _atName = y })
       <$> f (_atName x)
{-# INLINE atName #-}

-- | The version of this activity. The combination of activity type name and
-- version must be unique with in a domain.
atVersion
    :: Functor f
    => (Text
    -> f (Text))
    -> ActivityType
    -> f ActivityType
atVersion f x =
    (\y -> x { _atVersion = y })
       <$> f (_atVersion x)
{-# INLINE atVersion #-}

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
atcDefaultTaskStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTypeConfiguration
    -> f ActivityTypeConfiguration
atcDefaultTaskStartToCloseTimeout f x =
    (\y -> x { _atcDefaultTaskStartToCloseTimeout = y })
       <$> f (_atcDefaultTaskStartToCloseTimeout x)
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
atcDefaultTaskHeartbeatTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTypeConfiguration
    -> f ActivityTypeConfiguration
atcDefaultTaskHeartbeatTimeout f x =
    (\y -> x { _atcDefaultTaskHeartbeatTimeout = y })
       <$> f (_atcDefaultTaskHeartbeatTimeout x)
{-# INLINE atcDefaultTaskHeartbeatTimeout #-}

-- | The optional default task list specified for this activity type at
-- registration. This default task list is used if a task list is not provided
-- when a task is scheduled through the ScheduleActivityTask Decision. You can
-- override this default when scheduling a task through the
-- ScheduleActivityTask Decision.
atcDefaultTaskList
    :: Functor f
    => (Maybe TaskList
    -> f (Maybe TaskList))
    -> ActivityTypeConfiguration
    -> f ActivityTypeConfiguration
atcDefaultTaskList f x =
    (\y -> x { _atcDefaultTaskList = y })
       <$> f (_atcDefaultTaskList x)
{-# INLINE atcDefaultTaskList #-}

-- | The optional default maximum duration, specified when registering the
-- activity type, that a task of an activity type can wait before being
-- assigned to a worker. You can override this default when scheduling a task
-- through the ScheduleActivityTask Decision. The valid values are integers
-- greater than or equal to 0. An integer value can be used to specify the
-- duration in seconds while NONE can be used to specify unlimited duration.
atcDefaultTaskScheduleToStartTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTypeConfiguration
    -> f ActivityTypeConfiguration
atcDefaultTaskScheduleToStartTimeout f x =
    (\y -> x { _atcDefaultTaskScheduleToStartTimeout = y })
       <$> f (_atcDefaultTaskScheduleToStartTimeout x)
{-# INLINE atcDefaultTaskScheduleToStartTimeout #-}

-- | The optional default maximum duration, specified when registering the
-- activity type, for tasks of this activity type. You can override this
-- default when scheduling a task through the ScheduleActivityTask Decision.
-- The valid values are integers greater than or equal to 0. An integer value
-- can be used to specify the duration in seconds while NONE can be used to
-- specify unlimited duration.
atcDefaultTaskScheduleToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTypeConfiguration
    -> f ActivityTypeConfiguration
atcDefaultTaskScheduleToCloseTimeout f x =
    (\y -> x { _atcDefaultTaskScheduleToCloseTimeout = y })
       <$> f (_atcDefaultTaskScheduleToCloseTimeout x)
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
atiActivityType
    :: Functor f
    => (ActivityType
    -> f (ActivityType))
    -> ActivityTypeInfo
    -> f ActivityTypeInfo
atiActivityType f x =
    (\y -> x { _atiActivityType = y })
       <$> f (_atiActivityType x)
{-# INLINE atiActivityType #-}

-- | The current status of the activity type.
atiStatus
    :: Functor f
    => (RegistrationStatus
    -> f (RegistrationStatus))
    -> ActivityTypeInfo
    -> f ActivityTypeInfo
atiStatus f x =
    (\y -> x { _atiStatus = y })
       <$> f (_atiStatus x)
{-# INLINE atiStatus #-}

-- | The description of the activity type provided in RegisterActivityType.
atiDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ActivityTypeInfo
    -> f ActivityTypeInfo
atiDescription f x =
    (\y -> x { _atiDescription = y })
       <$> f (_atiDescription x)
{-# INLINE atiDescription #-}

-- | The date and time this activity type was created through
-- RegisterActivityType.
atiCreationDate
    :: Functor f
    => (POSIX
    -> f (POSIX))
    -> ActivityTypeInfo
    -> f ActivityTypeInfo
atiCreationDate f x =
    (\y -> x { _atiCreationDate = y })
       <$> f (_atiCreationDate x)
{-# INLINE atiCreationDate #-}

-- | If DEPRECATED, the date and time DeprecateActivityType was called.
atiDeprecationDate
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> ActivityTypeInfo
    -> f ActivityTypeInfo
atiDeprecationDate f x =
    (\y -> x { _atiDeprecationDate = y })
       <$> f (_atiDeprecationDate x)
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
ctfeaTimerId
    :: Functor f
    => (Text
    -> f (Text))
    -> CancelTimerFailedEventAttributes
    -> f CancelTimerFailedEventAttributes
ctfeaTimerId f x =
    (\y -> x { _ctfeaTimerId = y })
       <$> f (_ctfeaTimerId x)
{-# INLINE ctfeaTimerId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
ctfeaCause
    :: Functor f
    => (CancelTimerFailedCause
    -> f (CancelTimerFailedCause))
    -> CancelTimerFailedEventAttributes
    -> f CancelTimerFailedEventAttributes
ctfeaCause f x =
    (\y -> x { _ctfeaCause = y })
       <$> f (_ctfeaCause x)
{-# INLINE ctfeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CancelTimer decision to cancel this timer. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
ctfeaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> CancelTimerFailedEventAttributes
    -> f CancelTimerFailedEventAttributes
ctfeaDecisionTaskCompletedEventId f x =
    (\y -> x { _ctfeaDecisionTaskCompletedEventId = y })
       <$> f (_ctfeaDecisionTaskCompletedEventId x)
{-# INLINE ctfeaDecisionTaskCompletedEventId #-}

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
cwefebCause
    :: Functor f
    => (CancelWorkflowExecutionFailedCause
    -> f (CancelWorkflowExecutionFailedCause))
    -> CancelWorkflowExecutionFailedEventAttributes
    -> f CancelWorkflowExecutionFailedEventAttributes
cwefebCause f x =
    (\y -> x { _cwefebCause = y })
       <$> f (_cwefebCause x)
{-# INLINE cwefebCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CancelWorkflowExecution decision for this
-- cancellation request. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
cwefebDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> CancelWorkflowExecutionFailedEventAttributes
    -> f CancelWorkflowExecutionFailedEventAttributes
cwefebDecisionTaskCompletedEventId f x =
    (\y -> x { _cwefebDecisionTaskCompletedEventId = y })
       <$> f (_cwefebDecisionTaskCompletedEventId x)
{-# INLINE cwefebDecisionTaskCompletedEventId #-}

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
cwecebWorkflowExecution
    :: Functor f
    => (WorkflowExecution
    -> f (WorkflowExecution))
    -> ChildWorkflowExecutionCanceledEventAttributes
    -> f ChildWorkflowExecutionCanceledEventAttributes
cwecebWorkflowExecution f x =
    (\y -> x { _cwecebWorkflowExecution = y })
       <$> f (_cwecebWorkflowExecution x)
{-# INLINE cwecebWorkflowExecution #-}

-- | The type of the child workflow execution.
cwecebWorkflowType
    :: Functor f
    => (WorkflowType
    -> f (WorkflowType))
    -> ChildWorkflowExecutionCanceledEventAttributes
    -> f ChildWorkflowExecutionCanceledEventAttributes
cwecebWorkflowType f x =
    (\y -> x { _cwecebWorkflowType = y })
       <$> f (_cwecebWorkflowType x)
{-# INLINE cwecebWorkflowType #-}

-- | Details of the cancellation (if provided).
cwecebDetails
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ChildWorkflowExecutionCanceledEventAttributes
    -> f ChildWorkflowExecutionCanceledEventAttributes
cwecebDetails f x =
    (\y -> x { _cwecebDetails = y })
       <$> f (_cwecebDetails x)
{-# INLINE cwecebDetails #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cwecebInitiatedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ChildWorkflowExecutionCanceledEventAttributes
    -> f ChildWorkflowExecutionCanceledEventAttributes
cwecebInitiatedEventId f x =
    (\y -> x { _cwecebInitiatedEventId = y })
       <$> f (_cwecebInitiatedEventId x)
{-# INLINE cwecebInitiatedEventId #-}

-- | The Id of the ChildWorkflowExecutionStarted event recorded when this child
-- workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
cwecebStartedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ChildWorkflowExecutionCanceledEventAttributes
    -> f ChildWorkflowExecutionCanceledEventAttributes
cwecebStartedEventId f x =
    (\y -> x { _cwecebStartedEventId = y })
       <$> f (_cwecebStartedEventId x)
{-# INLINE cwecebStartedEventId #-}

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
cweceaWorkflowExecution
    :: Functor f
    => (WorkflowExecution
    -> f (WorkflowExecution))
    -> ChildWorkflowExecutionCompletedEventAttributes
    -> f ChildWorkflowExecutionCompletedEventAttributes
cweceaWorkflowExecution f x =
    (\y -> x { _cweceaWorkflowExecution = y })
       <$> f (_cweceaWorkflowExecution x)
{-# INLINE cweceaWorkflowExecution #-}

-- | The type of the child workflow execution.
cweceaWorkflowType
    :: Functor f
    => (WorkflowType
    -> f (WorkflowType))
    -> ChildWorkflowExecutionCompletedEventAttributes
    -> f ChildWorkflowExecutionCompletedEventAttributes
cweceaWorkflowType f x =
    (\y -> x { _cweceaWorkflowType = y })
       <$> f (_cweceaWorkflowType x)
{-# INLINE cweceaWorkflowType #-}

-- | The result of the child workflow execution (if any).
cweceaResult
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ChildWorkflowExecutionCompletedEventAttributes
    -> f ChildWorkflowExecutionCompletedEventAttributes
cweceaResult f x =
    (\y -> x { _cweceaResult = y })
       <$> f (_cweceaResult x)
{-# INLINE cweceaResult #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cweceaInitiatedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ChildWorkflowExecutionCompletedEventAttributes
    -> f ChildWorkflowExecutionCompletedEventAttributes
cweceaInitiatedEventId f x =
    (\y -> x { _cweceaInitiatedEventId = y })
       <$> f (_cweceaInitiatedEventId x)
{-# INLINE cweceaInitiatedEventId #-}

-- | The Id of the ChildWorkflowExecutionStarted event recorded when this child
-- workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
cweceaStartedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ChildWorkflowExecutionCompletedEventAttributes
    -> f ChildWorkflowExecutionCompletedEventAttributes
cweceaStartedEventId f x =
    (\y -> x { _cweceaStartedEventId = y })
       <$> f (_cweceaStartedEventId x)
{-# INLINE cweceaStartedEventId #-}

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
cwefecWorkflowExecution
    :: Functor f
    => (WorkflowExecution
    -> f (WorkflowExecution))
    -> ChildWorkflowExecutionFailedEventAttributes
    -> f ChildWorkflowExecutionFailedEventAttributes
cwefecWorkflowExecution f x =
    (\y -> x { _cwefecWorkflowExecution = y })
       <$> f (_cwefecWorkflowExecution x)
{-# INLINE cwefecWorkflowExecution #-}

-- | The type of the child workflow execution.
cwefecWorkflowType
    :: Functor f
    => (WorkflowType
    -> f (WorkflowType))
    -> ChildWorkflowExecutionFailedEventAttributes
    -> f ChildWorkflowExecutionFailedEventAttributes
cwefecWorkflowType f x =
    (\y -> x { _cwefecWorkflowType = y })
       <$> f (_cwefecWorkflowType x)
{-# INLINE cwefecWorkflowType #-}

-- | The reason for the failure (if provided).
cwefecReason
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ChildWorkflowExecutionFailedEventAttributes
    -> f ChildWorkflowExecutionFailedEventAttributes
cwefecReason f x =
    (\y -> x { _cwefecReason = y })
       <$> f (_cwefecReason x)
{-# INLINE cwefecReason #-}

-- | The details of the failure (if provided).
cwefecDetails
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ChildWorkflowExecutionFailedEventAttributes
    -> f ChildWorkflowExecutionFailedEventAttributes
cwefecDetails f x =
    (\y -> x { _cwefecDetails = y })
       <$> f (_cwefecDetails x)
{-# INLINE cwefecDetails #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cwefecInitiatedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ChildWorkflowExecutionFailedEventAttributes
    -> f ChildWorkflowExecutionFailedEventAttributes
cwefecInitiatedEventId f x =
    (\y -> x { _cwefecInitiatedEventId = y })
       <$> f (_cwefecInitiatedEventId x)
{-# INLINE cwefecInitiatedEventId #-}

-- | The Id of the ChildWorkflowExecutionStarted event recorded when this child
-- workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
cwefecStartedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ChildWorkflowExecutionFailedEventAttributes
    -> f ChildWorkflowExecutionFailedEventAttributes
cwefecStartedEventId f x =
    (\y -> x { _cwefecStartedEventId = y })
       <$> f (_cwefecStartedEventId x)
{-# INLINE cwefecStartedEventId #-}

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
cweseaWorkflowExecution
    :: Functor f
    => (WorkflowExecution
    -> f (WorkflowExecution))
    -> ChildWorkflowExecutionStartedEventAttributes
    -> f ChildWorkflowExecutionStartedEventAttributes
cweseaWorkflowExecution f x =
    (\y -> x { _cweseaWorkflowExecution = y })
       <$> f (_cweseaWorkflowExecution x)
{-# INLINE cweseaWorkflowExecution #-}

-- | The type of the child workflow execution.
cweseaWorkflowType
    :: Functor f
    => (WorkflowType
    -> f (WorkflowType))
    -> ChildWorkflowExecutionStartedEventAttributes
    -> f ChildWorkflowExecutionStartedEventAttributes
cweseaWorkflowType f x =
    (\y -> x { _cweseaWorkflowType = y })
       <$> f (_cweseaWorkflowType x)
{-# INLINE cweseaWorkflowType #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cweseaInitiatedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ChildWorkflowExecutionStartedEventAttributes
    -> f ChildWorkflowExecutionStartedEventAttributes
cweseaInitiatedEventId f x =
    (\y -> x { _cweseaInitiatedEventId = y })
       <$> f (_cweseaInitiatedEventId x)
{-# INLINE cweseaInitiatedEventId #-}

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
cweteaWorkflowExecution
    :: Functor f
    => (WorkflowExecution
    -> f (WorkflowExecution))
    -> ChildWorkflowExecutionTerminatedEventAttributes
    -> f ChildWorkflowExecutionTerminatedEventAttributes
cweteaWorkflowExecution f x =
    (\y -> x { _cweteaWorkflowExecution = y })
       <$> f (_cweteaWorkflowExecution x)
{-# INLINE cweteaWorkflowExecution #-}

-- | The type of the child workflow execution.
cweteaWorkflowType
    :: Functor f
    => (WorkflowType
    -> f (WorkflowType))
    -> ChildWorkflowExecutionTerminatedEventAttributes
    -> f ChildWorkflowExecutionTerminatedEventAttributes
cweteaWorkflowType f x =
    (\y -> x { _cweteaWorkflowType = y })
       <$> f (_cweteaWorkflowType x)
{-# INLINE cweteaWorkflowType #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cweteaInitiatedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ChildWorkflowExecutionTerminatedEventAttributes
    -> f ChildWorkflowExecutionTerminatedEventAttributes
cweteaInitiatedEventId f x =
    (\y -> x { _cweteaInitiatedEventId = y })
       <$> f (_cweteaInitiatedEventId x)
{-# INLINE cweteaInitiatedEventId #-}

-- | The Id of the ChildWorkflowExecutionStarted event recorded when this child
-- workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
cweteaStartedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ChildWorkflowExecutionTerminatedEventAttributes
    -> f ChildWorkflowExecutionTerminatedEventAttributes
cweteaStartedEventId f x =
    (\y -> x { _cweteaStartedEventId = y })
       <$> f (_cweteaStartedEventId x)
{-# INLINE cweteaStartedEventId #-}

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
cwetoeaWorkflowExecution
    :: Functor f
    => (WorkflowExecution
    -> f (WorkflowExecution))
    -> ChildWorkflowExecutionTimedOutEventAttributes
    -> f ChildWorkflowExecutionTimedOutEventAttributes
cwetoeaWorkflowExecution f x =
    (\y -> x { _cwetoeaWorkflowExecution = y })
       <$> f (_cwetoeaWorkflowExecution x)
{-# INLINE cwetoeaWorkflowExecution #-}

-- | The type of the child workflow execution.
cwetoeaWorkflowType
    :: Functor f
    => (WorkflowType
    -> f (WorkflowType))
    -> ChildWorkflowExecutionTimedOutEventAttributes
    -> f ChildWorkflowExecutionTimedOutEventAttributes
cwetoeaWorkflowType f x =
    (\y -> x { _cwetoeaWorkflowType = y })
       <$> f (_cwetoeaWorkflowType x)
{-# INLINE cwetoeaWorkflowType #-}

-- | The type of the timeout that caused the child workflow execution to time
-- out.
cwetoeaTimeoutType
    :: Functor f
    => (WorkflowExecutionTimeoutType
    -> f (WorkflowExecutionTimeoutType))
    -> ChildWorkflowExecutionTimedOutEventAttributes
    -> f ChildWorkflowExecutionTimedOutEventAttributes
cwetoeaTimeoutType f x =
    (\y -> x { _cwetoeaTimeoutType = y })
       <$> f (_cwetoeaTimeoutType x)
{-# INLINE cwetoeaTimeoutType #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cwetoeaInitiatedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ChildWorkflowExecutionTimedOutEventAttributes
    -> f ChildWorkflowExecutionTimedOutEventAttributes
cwetoeaInitiatedEventId f x =
    (\y -> x { _cwetoeaInitiatedEventId = y })
       <$> f (_cwetoeaInitiatedEventId x)
{-# INLINE cwetoeaInitiatedEventId #-}

-- | The Id of the ChildWorkflowExecutionStarted event recorded when this child
-- workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
cwetoeaStartedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ChildWorkflowExecutionTimedOutEventAttributes
    -> f ChildWorkflowExecutionTimedOutEventAttributes
cwetoeaStartedEventId f x =
    (\y -> x { _cwetoeaStartedEventId = y })
       <$> f (_cwetoeaStartedEventId x)
{-# INLINE cwetoeaStartedEventId #-}

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
cwefeaCause
    :: Functor f
    => (CompleteWorkflowExecutionFailedCause
    -> f (CompleteWorkflowExecutionFailedCause))
    -> CompleteWorkflowExecutionFailedEventAttributes
    -> f CompleteWorkflowExecutionFailedEventAttributes
cwefeaCause f x =
    (\y -> x { _cwefeaCause = y })
       <$> f (_cwefeaCause x)
{-# INLINE cwefeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CompleteWorkflowExecution decision to complete
-- this execution. This information can be useful for diagnosing problems by
-- tracing back the cause of events.
cwefeaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> CompleteWorkflowExecutionFailedEventAttributes
    -> f CompleteWorkflowExecutionFailedEventAttributes
cwefeaDecisionTaskCompletedEventId f x =
    (\y -> x { _cwefeaDecisionTaskCompletedEventId = y })
       <$> f (_cwefeaDecisionTaskCompletedEventId x)
{-# INLINE cwefeaDecisionTaskCompletedEventId #-}

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
canwedaInput
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ContinueAsNewWorkflowExecutionDecisionAttributes
    -> f ContinueAsNewWorkflowExecutionDecisionAttributes
canwedaInput f x =
    (\y -> x { _canwedaInput = y })
       <$> f (_canwedaInput x)
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
canwedaExecutionStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ContinueAsNewWorkflowExecutionDecisionAttributes
    -> f ContinueAsNewWorkflowExecutionDecisionAttributes
canwedaExecutionStartToCloseTimeout f x =
    (\y -> x { _canwedaExecutionStartToCloseTimeout = y })
       <$> f (_canwedaExecutionStartToCloseTimeout x)
{-# INLINE canwedaExecutionStartToCloseTimeout #-}

-- | Represents a task list.
canwedaTaskList
    :: Functor f
    => (Maybe TaskList
    -> f (Maybe TaskList))
    -> ContinueAsNewWorkflowExecutionDecisionAttributes
    -> f ContinueAsNewWorkflowExecutionDecisionAttributes
canwedaTaskList f x =
    (\y -> x { _canwedaTaskList = y })
       <$> f (_canwedaTaskList x)
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
canwedaTaskStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ContinueAsNewWorkflowExecutionDecisionAttributes
    -> f ContinueAsNewWorkflowExecutionDecisionAttributes
canwedaTaskStartToCloseTimeout f x =
    (\y -> x { _canwedaTaskStartToCloseTimeout = y })
       <$> f (_canwedaTaskStartToCloseTimeout x)
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
canwedaChildPolicy
    :: Functor f
    => (Maybe ChildPolicy
    -> f (Maybe ChildPolicy))
    -> ContinueAsNewWorkflowExecutionDecisionAttributes
    -> f ContinueAsNewWorkflowExecutionDecisionAttributes
canwedaChildPolicy f x =
    (\y -> x { _canwedaChildPolicy = y })
       <$> f (_canwedaChildPolicy x)
{-# INLINE canwedaChildPolicy #-}

-- | The list of tags to associate with the new workflow execution. A maximum of
-- 5 tags can be specified. You can list workflow executions with a specific
-- tag by calling ListOpenWorkflowExecutions or ListClosedWorkflowExecutions
-- and specifying a TagFilter.
canwedaTagList
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ContinueAsNewWorkflowExecutionDecisionAttributes
    -> f ContinueAsNewWorkflowExecutionDecisionAttributes
canwedaTagList f x =
    (\y -> x { _canwedaTagList = y })
       <$> f (_canwedaTagList x)
{-# INLINE canwedaTagList #-}

canwedaWorkflowTypeVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ContinueAsNewWorkflowExecutionDecisionAttributes
    -> f ContinueAsNewWorkflowExecutionDecisionAttributes
canwedaWorkflowTypeVersion f x =
    (\y -> x { _canwedaWorkflowTypeVersion = y })
       <$> f (_canwedaWorkflowTypeVersion x)
{-# INLINE canwedaWorkflowTypeVersion #-}

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
canwefeaCause
    :: Functor f
    => (ContinueAsNewWorkflowExecutionFailedCause
    -> f (ContinueAsNewWorkflowExecutionFailedCause))
    -> ContinueAsNewWorkflowExecutionFailedEventAttributes
    -> f ContinueAsNewWorkflowExecutionFailedEventAttributes
canwefeaCause f x =
    (\y -> x { _canwefeaCause = y })
       <$> f (_canwefeaCause x)
{-# INLINE canwefeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the ContinueAsNewWorkflowExecution decision that
-- started this execution. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
canwefeaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ContinueAsNewWorkflowExecutionFailedEventAttributes
    -> f ContinueAsNewWorkflowExecutionFailedEventAttributes
canwefeaDecisionTaskCompletedEventId f x =
    (\y -> x { _canwefeaDecisionTaskCompletedEventId = y })
       <$> f (_canwefeaDecisionTaskCompletedEventId x)
{-# INLINE canwefeaDecisionTaskCompletedEventId #-}

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
ddddrDecisionType
    :: Functor f
    => (DecisionType
    -> f (DecisionType))
    -> Decision
    -> f Decision
ddddrDecisionType f x =
    (\y -> x { _ddddrDecisionType = y })
       <$> f (_ddddrDecisionType x)
{-# INLINE ddddrDecisionType #-}

-- | Provides details of the ScheduleActivityTask decision. It is not set for
-- other decision types.
ddddrScheduleActivityTaskDecisionAttributes
    :: Functor f
    => (Maybe ScheduleActivityTaskDecisionAttributes
    -> f (Maybe ScheduleActivityTaskDecisionAttributes))
    -> Decision
    -> f Decision
ddddrScheduleActivityTaskDecisionAttributes f x =
    (\y -> x { _ddddrScheduleActivityTaskDecisionAttributes = y })
       <$> f (_ddddrScheduleActivityTaskDecisionAttributes x)
{-# INLINE ddddrScheduleActivityTaskDecisionAttributes #-}

-- | Provides details of the RequestCancelActivityTask decision. It is not set
-- for other decision types.
ddddrRequestCancelActivityTaskDecisionAttributes
    :: Functor f
    => (Maybe RequestCancelActivityTaskDecisionAttributes
    -> f (Maybe RequestCancelActivityTaskDecisionAttributes))
    -> Decision
    -> f Decision
ddddrRequestCancelActivityTaskDecisionAttributes f x =
    (\y -> x { _ddddrRequestCancelActivityTaskDecisionAttributes = y })
       <$> f (_ddddrRequestCancelActivityTaskDecisionAttributes x)
{-# INLINE ddddrRequestCancelActivityTaskDecisionAttributes #-}

-- | Provides details of the CompleteWorkflowExecution decision. It is not set
-- for other decision types.
ddddrCompleteWorkflowExecutionDecisionAttributes
    :: Functor f
    => (Maybe CompleteWorkflowExecutionDecisionAttributes
    -> f (Maybe CompleteWorkflowExecutionDecisionAttributes))
    -> Decision
    -> f Decision
ddddrCompleteWorkflowExecutionDecisionAttributes f x =
    (\y -> x { _ddddrCompleteWorkflowExecutionDecisionAttributes = y })
       <$> f (_ddddrCompleteWorkflowExecutionDecisionAttributes x)
{-# INLINE ddddrCompleteWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the FailWorkflowExecution decision. It is not set for
-- other decision types.
ddddrFailWorkflowExecutionDecisionAttributes
    :: Functor f
    => (Maybe FailWorkflowExecutionDecisionAttributes
    -> f (Maybe FailWorkflowExecutionDecisionAttributes))
    -> Decision
    -> f Decision
ddddrFailWorkflowExecutionDecisionAttributes f x =
    (\y -> x { _ddddrFailWorkflowExecutionDecisionAttributes = y })
       <$> f (_ddddrFailWorkflowExecutionDecisionAttributes x)
{-# INLINE ddddrFailWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the CancelWorkflowExecution decision. It is not set for
-- other decision types.
ddddrCancelWorkflowExecutionDecisionAttributes
    :: Functor f
    => (Maybe CancelWorkflowExecutionDecisionAttributes
    -> f (Maybe CancelWorkflowExecutionDecisionAttributes))
    -> Decision
    -> f Decision
ddddrCancelWorkflowExecutionDecisionAttributes f x =
    (\y -> x { _ddddrCancelWorkflowExecutionDecisionAttributes = y })
       <$> f (_ddddrCancelWorkflowExecutionDecisionAttributes x)
{-# INLINE ddddrCancelWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the ContinueAsNewWorkflowExecution decision. It is not
-- set for other decision types.
ddddrContinueAsNewWorkflowExecutionDecisionAttributes
    :: Functor f
    => (Maybe ContinueAsNewWorkflowExecutionDecisionAttributes
    -> f (Maybe ContinueAsNewWorkflowExecutionDecisionAttributes))
    -> Decision
    -> f Decision
ddddrContinueAsNewWorkflowExecutionDecisionAttributes f x =
    (\y -> x { _ddddrContinueAsNewWorkflowExecutionDecisionAttributes = y })
       <$> f (_ddddrContinueAsNewWorkflowExecutionDecisionAttributes x)
{-# INLINE ddddrContinueAsNewWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the RecordMarker decision. It is not set for other
-- decision types.
ddddrRecordMarkerDecisionAttributes
    :: Functor f
    => (Maybe RecordMarkerDecisionAttributes
    -> f (Maybe RecordMarkerDecisionAttributes))
    -> Decision
    -> f Decision
ddddrRecordMarkerDecisionAttributes f x =
    (\y -> x { _ddddrRecordMarkerDecisionAttributes = y })
       <$> f (_ddddrRecordMarkerDecisionAttributes x)
{-# INLINE ddddrRecordMarkerDecisionAttributes #-}

-- | Provides details of the StartTimer decision. It is not set for other
-- decision types.
ddddrStartTimerDecisionAttributes
    :: Functor f
    => (Maybe StartTimerDecisionAttributes
    -> f (Maybe StartTimerDecisionAttributes))
    -> Decision
    -> f Decision
ddddrStartTimerDecisionAttributes f x =
    (\y -> x { _ddddrStartTimerDecisionAttributes = y })
       <$> f (_ddddrStartTimerDecisionAttributes x)
{-# INLINE ddddrStartTimerDecisionAttributes #-}

-- | Provides details of the CancelTimer decision. It is not set for other
-- decision types.
ddddrCancelTimerDecisionAttributes
    :: Functor f
    => (Maybe CancelTimerDecisionAttributes
    -> f (Maybe CancelTimerDecisionAttributes))
    -> Decision
    -> f Decision
ddddrCancelTimerDecisionAttributes f x =
    (\y -> x { _ddddrCancelTimerDecisionAttributes = y })
       <$> f (_ddddrCancelTimerDecisionAttributes x)
{-# INLINE ddddrCancelTimerDecisionAttributes #-}

-- | Provides details of the SignalExternalWorkflowExecution decision. It is not
-- set for other decision types.
ddddrSignalExternalWorkflowExecutionDecisionAttributes
    :: Functor f
    => (Maybe SignalExternalWorkflowExecutionDecisionAttributes
    -> f (Maybe SignalExternalWorkflowExecutionDecisionAttributes))
    -> Decision
    -> f Decision
ddddrSignalExternalWorkflowExecutionDecisionAttributes f x =
    (\y -> x { _ddddrSignalExternalWorkflowExecutionDecisionAttributes = y })
       <$> f (_ddddrSignalExternalWorkflowExecutionDecisionAttributes x)
{-# INLINE ddddrSignalExternalWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the RequestCancelExternalWorkflowExecution decision. It
-- is not set for other decision types.
ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes
    :: Functor f
    => (Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes
    -> f (Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes))
    -> Decision
    -> f Decision
ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes f x =
    (\y -> x { _ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes = y })
       <$> f (_ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes x)
{-# INLINE ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the StartChildWorkflowExecution decision. It is not set
-- for other decision types.
ddddrStartChildWorkflowExecutionDecisionAttributes
    :: Functor f
    => (Maybe StartChildWorkflowExecutionDecisionAttributes
    -> f (Maybe StartChildWorkflowExecutionDecisionAttributes))
    -> Decision
    -> f Decision
ddddrStartChildWorkflowExecutionDecisionAttributes f x =
    (\y -> x { _ddddrStartChildWorkflowExecutionDecisionAttributes = y })
       <$> f (_ddddrStartChildWorkflowExecutionDecisionAttributes x)
{-# INLINE ddddrStartChildWorkflowExecutionDecisionAttributes #-}

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
dtceaExecutionContext
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DecisionTaskCompletedEventAttributes
    -> f DecisionTaskCompletedEventAttributes
dtceaExecutionContext f x =
    (\y -> x { _dtceaExecutionContext = y })
       <$> f (_dtceaExecutionContext x)
{-# INLINE dtceaExecutionContext #-}

-- | The id of the DecisionTaskScheduled event that was recorded when this
-- decision task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
dtceaScheduledEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> DecisionTaskCompletedEventAttributes
    -> f DecisionTaskCompletedEventAttributes
dtceaScheduledEventId f x =
    (\y -> x { _dtceaScheduledEventId = y })
       <$> f (_dtceaScheduledEventId x)
{-# INLINE dtceaScheduledEventId #-}

-- | The Id of the DecisionTaskStarted event recorded when this decision task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
dtceaStartedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> DecisionTaskCompletedEventAttributes
    -> f DecisionTaskCompletedEventAttributes
dtceaStartedEventId f x =
    (\y -> x { _dtceaStartedEventId = y })
       <$> f (_dtceaStartedEventId x)
{-# INLINE dtceaStartedEventId #-}

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
dtseaTaskList
    :: Functor f
    => (TaskList
    -> f (TaskList))
    -> DecisionTaskScheduledEventAttributes
    -> f DecisionTaskScheduledEventAttributes
dtseaTaskList f x =
    (\y -> x { _dtseaTaskList = y })
       <$> f (_dtseaTaskList x)
{-# INLINE dtseaTaskList #-}

-- | The maximum duration for this decision task. The task is considered timed
-- out if it does not completed within this duration. The valid values are
-- integers greater than or equal to 0. An integer value can be used to
-- specify the duration in seconds while NONE can be used to specify unlimited
-- duration.
dtseaStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DecisionTaskScheduledEventAttributes
    -> f DecisionTaskScheduledEventAttributes
dtseaStartToCloseTimeout f x =
    (\y -> x { _dtseaStartToCloseTimeout = y })
       <$> f (_dtseaStartToCloseTimeout x)
{-# INLINE dtseaStartToCloseTimeout #-}

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
dtsebIdentity
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DecisionTaskStartedEventAttributes
    -> f DecisionTaskStartedEventAttributes
dtsebIdentity f x =
    (\y -> x { _dtsebIdentity = y })
       <$> f (_dtsebIdentity x)
{-# INLINE dtsebIdentity #-}

-- | The id of the DecisionTaskScheduled event that was recorded when this
-- decision task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
dtsebScheduledEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> DecisionTaskStartedEventAttributes
    -> f DecisionTaskStartedEventAttributes
dtsebScheduledEventId f x =
    (\y -> x { _dtsebScheduledEventId = y })
       <$> f (_dtsebScheduledEventId x)
{-# INLINE dtsebScheduledEventId #-}

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
dttoeaTimeoutType
    :: Functor f
    => (DecisionTaskTimeoutType
    -> f (DecisionTaskTimeoutType))
    -> DecisionTaskTimedOutEventAttributes
    -> f DecisionTaskTimedOutEventAttributes
dttoeaTimeoutType f x =
    (\y -> x { _dttoeaTimeoutType = y })
       <$> f (_dttoeaTimeoutType x)
{-# INLINE dttoeaTimeoutType #-}

-- | The id of the DecisionTaskScheduled event that was recorded when this
-- decision task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
dttoeaScheduledEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> DecisionTaskTimedOutEventAttributes
    -> f DecisionTaskTimedOutEventAttributes
dttoeaScheduledEventId f x =
    (\y -> x { _dttoeaScheduledEventId = y })
       <$> f (_dttoeaScheduledEventId x)
{-# INLINE dttoeaScheduledEventId #-}

-- | The Id of the DecisionTaskStarted event recorded when this decision task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
dttoeaStartedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> DecisionTaskTimedOutEventAttributes
    -> f DecisionTaskTimedOutEventAttributes
dttoeaStartedEventId f x =
    (\y -> x { _dttoeaStartedEventId = y })
       <$> f (_dttoeaStartedEventId x)
{-# INLINE dttoeaStartedEventId #-}

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
diName
    :: Functor f
    => (Text
    -> f (Text))
    -> DomainInfo
    -> f DomainInfo
diName f x =
    (\y -> x { _diName = y })
       <$> f (_diName x)
{-# INLINE diName #-}

-- | The status of the domain: REGISTERED: The domain is properly registered and
-- available. You can use this domain for registering types and creating new
-- workflow executions. DEPRECATED: The domain was deprecated using
-- DeprecateDomain, but is still in use. You should not create new workflow
-- executions in this domain.
diStatus
    :: Functor f
    => (RegistrationStatus
    -> f (RegistrationStatus))
    -> DomainInfo
    -> f DomainInfo
diStatus f x =
    (\y -> x { _diStatus = y })
       <$> f (_diStatus x)
{-# INLINE diStatus #-}

-- | The description of the domain provided through RegisterDomain.
diDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DomainInfo
    -> f DomainInfo
diDescription f x =
    (\y -> x { _diDescription = y })
       <$> f (_diDescription x)
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
etfOldestDate
    :: Functor f
    => (POSIX
    -> f (POSIX))
    -> ExecutionTimeFilter
    -> f ExecutionTimeFilter
etfOldestDate f x =
    (\y -> x { _etfOldestDate = y })
       <$> f (_etfOldestDate x)
{-# INLINE etfOldestDate #-}

-- | Specifies the latest start or close date and time to return.
etfLatestDate
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> ExecutionTimeFilter
    -> f ExecutionTimeFilter
etfLatestDate f x =
    (\y -> x { _etfLatestDate = y })
       <$> f (_etfLatestDate x)
{-# INLINE etfLatestDate #-}

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
ewecreaWorkflowExecution
    :: Functor f
    => (WorkflowExecution
    -> f (WorkflowExecution))
    -> ExternalWorkflowExecutionCancelRequestedEventAttributes
    -> f ExternalWorkflowExecutionCancelRequestedEventAttributes
ewecreaWorkflowExecution f x =
    (\y -> x { _ewecreaWorkflowExecution = y })
       <$> f (_ewecreaWorkflowExecution x)
{-# INLINE ewecreaWorkflowExecution #-}

-- | The id of the RequestCancelExternalWorkflowExecutionInitiated event
-- corresponding to the RequestCancelExternalWorkflowExecution decision to
-- cancel this external workflow execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
ewecreaInitiatedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ExternalWorkflowExecutionCancelRequestedEventAttributes
    -> f ExternalWorkflowExecutionCancelRequestedEventAttributes
ewecreaInitiatedEventId f x =
    (\y -> x { _ewecreaInitiatedEventId = y })
       <$> f (_ewecreaInitiatedEventId x)
{-# INLINE ewecreaInitiatedEventId #-}

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
eweseaWorkflowExecution
    :: Functor f
    => (WorkflowExecution
    -> f (WorkflowExecution))
    -> ExternalWorkflowExecutionSignaledEventAttributes
    -> f ExternalWorkflowExecutionSignaledEventAttributes
eweseaWorkflowExecution f x =
    (\y -> x { _eweseaWorkflowExecution = y })
       <$> f (_eweseaWorkflowExecution x)
{-# INLINE eweseaWorkflowExecution #-}

-- | The id of the SignalExternalWorkflowExecutionInitiated event corresponding
-- to the SignalExternalWorkflowExecution decision to request this signal.
-- This information can be useful for diagnosing problems by tracing back the
-- chain of events leading up to this event.
eweseaInitiatedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ExternalWorkflowExecutionSignaledEventAttributes
    -> f ExternalWorkflowExecutionSignaledEventAttributes
eweseaInitiatedEventId f x =
    (\y -> x { _eweseaInitiatedEventId = y })
       <$> f (_eweseaInitiatedEventId x)
{-# INLINE eweseaInitiatedEventId #-}

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
fwedaReason
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> FailWorkflowExecutionDecisionAttributes
    -> f FailWorkflowExecutionDecisionAttributes
fwedaReason f x =
    (\y -> x { _fwedaReason = y })
       <$> f (_fwedaReason x)
{-# INLINE fwedaReason #-}

-- | Optional details of the failure.
fwedaDetails
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> FailWorkflowExecutionDecisionAttributes
    -> f FailWorkflowExecutionDecisionAttributes
fwedaDetails f x =
    (\y -> x { _fwedaDetails = y })
       <$> f (_fwedaDetails x)
{-# INLINE fwedaDetails #-}

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
fwefeaCause
    :: Functor f
    => (FailWorkflowExecutionFailedCause
    -> f (FailWorkflowExecutionFailedCause))
    -> FailWorkflowExecutionFailedEventAttributes
    -> f FailWorkflowExecutionFailedEventAttributes
fwefeaCause f x =
    (\y -> x { _fwefeaCause = y })
       <$> f (_fwefeaCause x)
{-# INLINE fwefeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the FailWorkflowExecution decision to fail this
-- execution. This information can be useful for diagnosing problems by
-- tracing back the cause of events.
fwefeaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> FailWorkflowExecutionFailedEventAttributes
    -> f FailWorkflowExecutionFailedEventAttributes
fwefeaDecisionTaskCompletedEventId f x =
    (\y -> x { _fwefeaDecisionTaskCompletedEventId = y })
       <$> f (_fwefeaDecisionTaskCompletedEventId x)
{-# INLINE fwefeaDecisionTaskCompletedEventId #-}

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
heEventTimestamp
    :: Functor f
    => (POSIX
    -> f (POSIX))
    -> HistoryEvent
    -> f HistoryEvent
heEventTimestamp f x =
    (\y -> x { _heEventTimestamp = y })
       <$> f (_heEventTimestamp x)
{-# INLINE heEventTimestamp #-}

-- | The type of the history event.
heEventType
    :: Functor f
    => (EventType
    -> f (EventType))
    -> HistoryEvent
    -> f HistoryEvent
heEventType f x =
    (\y -> x { _heEventType = y })
       <$> f (_heEventType x)
{-# INLINE heEventType #-}

-- | The system generated id of the event. This id uniquely identifies the event
-- with in the workflow execution history.
heEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> HistoryEvent
    -> f HistoryEvent
heEventId f x =
    (\y -> x { _heEventId = y })
       <$> f (_heEventId x)
{-# INLINE heEventId #-}

-- | If the event is of type WorkflowExecutionStarted then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionStartedEventAttributes
    :: Functor f
    => (Maybe WorkflowExecutionStartedEventAttributes
    -> f (Maybe WorkflowExecutionStartedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heWorkflowExecutionStartedEventAttributes f x =
    (\y -> x { _heWorkflowExecutionStartedEventAttributes = y })
       <$> f (_heWorkflowExecutionStartedEventAttributes x)
{-# INLINE heWorkflowExecutionStartedEventAttributes #-}

-- | If the event is of type WorkflowExecutionCompleted then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionCompletedEventAttributes
    :: Functor f
    => (Maybe WorkflowExecutionCompletedEventAttributes
    -> f (Maybe WorkflowExecutionCompletedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heWorkflowExecutionCompletedEventAttributes f x =
    (\y -> x { _heWorkflowExecutionCompletedEventAttributes = y })
       <$> f (_heWorkflowExecutionCompletedEventAttributes x)
{-# INLINE heWorkflowExecutionCompletedEventAttributes #-}

-- | If the event is of type CompleteWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heCompleteWorkflowExecutionFailedEventAttributes
    :: Functor f
    => (Maybe CompleteWorkflowExecutionFailedEventAttributes
    -> f (Maybe CompleteWorkflowExecutionFailedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heCompleteWorkflowExecutionFailedEventAttributes f x =
    (\y -> x { _heCompleteWorkflowExecutionFailedEventAttributes = y })
       <$> f (_heCompleteWorkflowExecutionFailedEventAttributes x)
{-# INLINE heCompleteWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type WorkflowExecutionFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionFailedEventAttributes
    :: Functor f
    => (Maybe WorkflowExecutionFailedEventAttributes
    -> f (Maybe WorkflowExecutionFailedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heWorkflowExecutionFailedEventAttributes f x =
    (\y -> x { _heWorkflowExecutionFailedEventAttributes = y })
       <$> f (_heWorkflowExecutionFailedEventAttributes x)
{-# INLINE heWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type FailWorkflowExecutionFailed then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heFailWorkflowExecutionFailedEventAttributes
    :: Functor f
    => (Maybe FailWorkflowExecutionFailedEventAttributes
    -> f (Maybe FailWorkflowExecutionFailedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heFailWorkflowExecutionFailedEventAttributes f x =
    (\y -> x { _heFailWorkflowExecutionFailedEventAttributes = y })
       <$> f (_heFailWorkflowExecutionFailedEventAttributes x)
{-# INLINE heFailWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type WorkflowExecutionTimedOut then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionTimedOutEventAttributes
    :: Functor f
    => (Maybe WorkflowExecutionTimedOutEventAttributes
    -> f (Maybe WorkflowExecutionTimedOutEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heWorkflowExecutionTimedOutEventAttributes f x =
    (\y -> x { _heWorkflowExecutionTimedOutEventAttributes = y })
       <$> f (_heWorkflowExecutionTimedOutEventAttributes x)
{-# INLINE heWorkflowExecutionTimedOutEventAttributes #-}

-- | If the event is of type WorkflowExecutionCanceled then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionCanceledEventAttributes
    :: Functor f
    => (Maybe WorkflowExecutionCanceledEventAttributes
    -> f (Maybe WorkflowExecutionCanceledEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heWorkflowExecutionCanceledEventAttributes f x =
    (\y -> x { _heWorkflowExecutionCanceledEventAttributes = y })
       <$> f (_heWorkflowExecutionCanceledEventAttributes x)
{-# INLINE heWorkflowExecutionCanceledEventAttributes #-}

-- | If the event is of type CancelWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heCancelWorkflowExecutionFailedEventAttributes
    :: Functor f
    => (Maybe CancelWorkflowExecutionFailedEventAttributes
    -> f (Maybe CancelWorkflowExecutionFailedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heCancelWorkflowExecutionFailedEventAttributes f x =
    (\y -> x { _heCancelWorkflowExecutionFailedEventAttributes = y })
       <$> f (_heCancelWorkflowExecutionFailedEventAttributes x)
{-# INLINE heCancelWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type WorkflowExecutionContinuedAsNew then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heWorkflowExecutionContinuedAsNewEventAttributes
    :: Functor f
    => (Maybe WorkflowExecutionContinuedAsNewEventAttributes
    -> f (Maybe WorkflowExecutionContinuedAsNewEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heWorkflowExecutionContinuedAsNewEventAttributes f x =
    (\y -> x { _heWorkflowExecutionContinuedAsNewEventAttributes = y })
       <$> f (_heWorkflowExecutionContinuedAsNewEventAttributes x)
{-# INLINE heWorkflowExecutionContinuedAsNewEventAttributes #-}

-- | If the event is of type ContinueAsNewWorkflowExecutionFailed then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heContinueAsNewWorkflowExecutionFailedEventAttributes
    :: Functor f
    => (Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes
    -> f (Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heContinueAsNewWorkflowExecutionFailedEventAttributes f x =
    (\y -> x { _heContinueAsNewWorkflowExecutionFailedEventAttributes = y })
       <$> f (_heContinueAsNewWorkflowExecutionFailedEventAttributes x)
{-# INLINE heContinueAsNewWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type WorkflowExecutionTerminated then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionTerminatedEventAttributes
    :: Functor f
    => (Maybe WorkflowExecutionTerminatedEventAttributes
    -> f (Maybe WorkflowExecutionTerminatedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heWorkflowExecutionTerminatedEventAttributes f x =
    (\y -> x { _heWorkflowExecutionTerminatedEventAttributes = y })
       <$> f (_heWorkflowExecutionTerminatedEventAttributes x)
{-# INLINE heWorkflowExecutionTerminatedEventAttributes #-}

-- | If the event is of type WorkflowExecutionCancelRequested then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
heWorkflowExecutionCancelRequestedEventAttributes
    :: Functor f
    => (Maybe WorkflowExecutionCancelRequestedEventAttributes
    -> f (Maybe WorkflowExecutionCancelRequestedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heWorkflowExecutionCancelRequestedEventAttributes f x =
    (\y -> x { _heWorkflowExecutionCancelRequestedEventAttributes = y })
       <$> f (_heWorkflowExecutionCancelRequestedEventAttributes x)
{-# INLINE heWorkflowExecutionCancelRequestedEventAttributes #-}

-- | If the event is of type DecisionTaskScheduled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heDecisionTaskScheduledEventAttributes
    :: Functor f
    => (Maybe DecisionTaskScheduledEventAttributes
    -> f (Maybe DecisionTaskScheduledEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heDecisionTaskScheduledEventAttributes f x =
    (\y -> x { _heDecisionTaskScheduledEventAttributes = y })
       <$> f (_heDecisionTaskScheduledEventAttributes x)
{-# INLINE heDecisionTaskScheduledEventAttributes #-}

-- | If the event is of type DecisionTaskStarted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heDecisionTaskStartedEventAttributes
    :: Functor f
    => (Maybe DecisionTaskStartedEventAttributes
    -> f (Maybe DecisionTaskStartedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heDecisionTaskStartedEventAttributes f x =
    (\y -> x { _heDecisionTaskStartedEventAttributes = y })
       <$> f (_heDecisionTaskStartedEventAttributes x)
{-# INLINE heDecisionTaskStartedEventAttributes #-}

-- | If the event is of type DecisionTaskCompleted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heDecisionTaskCompletedEventAttributes
    :: Functor f
    => (Maybe DecisionTaskCompletedEventAttributes
    -> f (Maybe DecisionTaskCompletedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heDecisionTaskCompletedEventAttributes f x =
    (\y -> x { _heDecisionTaskCompletedEventAttributes = y })
       <$> f (_heDecisionTaskCompletedEventAttributes x)
{-# INLINE heDecisionTaskCompletedEventAttributes #-}

-- | If the event is of type DecisionTaskTimedOut then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heDecisionTaskTimedOutEventAttributes
    :: Functor f
    => (Maybe DecisionTaskTimedOutEventAttributes
    -> f (Maybe DecisionTaskTimedOutEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heDecisionTaskTimedOutEventAttributes f x =
    (\y -> x { _heDecisionTaskTimedOutEventAttributes = y })
       <$> f (_heDecisionTaskTimedOutEventAttributes x)
{-# INLINE heDecisionTaskTimedOutEventAttributes #-}

-- | If the event is of type ActivityTaskScheduled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskScheduledEventAttributes
    :: Functor f
    => (Maybe ActivityTaskScheduledEventAttributes
    -> f (Maybe ActivityTaskScheduledEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heActivityTaskScheduledEventAttributes f x =
    (\y -> x { _heActivityTaskScheduledEventAttributes = y })
       <$> f (_heActivityTaskScheduledEventAttributes x)
{-# INLINE heActivityTaskScheduledEventAttributes #-}

-- | If the event is of type ActivityTaskStarted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskStartedEventAttributes
    :: Functor f
    => (Maybe ActivityTaskStartedEventAttributes
    -> f (Maybe ActivityTaskStartedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heActivityTaskStartedEventAttributes f x =
    (\y -> x { _heActivityTaskStartedEventAttributes = y })
       <$> f (_heActivityTaskStartedEventAttributes x)
{-# INLINE heActivityTaskStartedEventAttributes #-}

-- | If the event is of type ActivityTaskCompleted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskCompletedEventAttributes
    :: Functor f
    => (Maybe ActivityTaskCompletedEventAttributes
    -> f (Maybe ActivityTaskCompletedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heActivityTaskCompletedEventAttributes f x =
    (\y -> x { _heActivityTaskCompletedEventAttributes = y })
       <$> f (_heActivityTaskCompletedEventAttributes x)
{-# INLINE heActivityTaskCompletedEventAttributes #-}

-- | If the event is of type ActivityTaskFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskFailedEventAttributes
    :: Functor f
    => (Maybe ActivityTaskFailedEventAttributes
    -> f (Maybe ActivityTaskFailedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heActivityTaskFailedEventAttributes f x =
    (\y -> x { _heActivityTaskFailedEventAttributes = y })
       <$> f (_heActivityTaskFailedEventAttributes x)
{-# INLINE heActivityTaskFailedEventAttributes #-}

-- | If the event is of type ActivityTaskTimedOut then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskTimedOutEventAttributes
    :: Functor f
    => (Maybe ActivityTaskTimedOutEventAttributes
    -> f (Maybe ActivityTaskTimedOutEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heActivityTaskTimedOutEventAttributes f x =
    (\y -> x { _heActivityTaskTimedOutEventAttributes = y })
       <$> f (_heActivityTaskTimedOutEventAttributes x)
{-# INLINE heActivityTaskTimedOutEventAttributes #-}

-- | If the event is of type ActivityTaskCanceled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskCanceledEventAttributes
    :: Functor f
    => (Maybe ActivityTaskCanceledEventAttributes
    -> f (Maybe ActivityTaskCanceledEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heActivityTaskCanceledEventAttributes f x =
    (\y -> x { _heActivityTaskCanceledEventAttributes = y })
       <$> f (_heActivityTaskCanceledEventAttributes x)
{-# INLINE heActivityTaskCanceledEventAttributes #-}

-- | If the event is of type ActivityTaskcancelRequested then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskCancelRequestedEventAttributes
    :: Functor f
    => (Maybe ActivityTaskCancelRequestedEventAttributes
    -> f (Maybe ActivityTaskCancelRequestedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heActivityTaskCancelRequestedEventAttributes f x =
    (\y -> x { _heActivityTaskCancelRequestedEventAttributes = y })
       <$> f (_heActivityTaskCancelRequestedEventAttributes x)
{-# INLINE heActivityTaskCancelRequestedEventAttributes #-}

-- | If the event is of type WorkflowExecutionSignaled then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionSignaledEventAttributes
    :: Functor f
    => (Maybe WorkflowExecutionSignaledEventAttributes
    -> f (Maybe WorkflowExecutionSignaledEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heWorkflowExecutionSignaledEventAttributes f x =
    (\y -> x { _heWorkflowExecutionSignaledEventAttributes = y })
       <$> f (_heWorkflowExecutionSignaledEventAttributes x)
{-# INLINE heWorkflowExecutionSignaledEventAttributes #-}

-- | If the event is of type MarkerRecorded then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heMarkerRecordedEventAttributes
    :: Functor f
    => (Maybe MarkerRecordedEventAttributes
    -> f (Maybe MarkerRecordedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heMarkerRecordedEventAttributes f x =
    (\y -> x { _heMarkerRecordedEventAttributes = y })
       <$> f (_heMarkerRecordedEventAttributes x)
{-# INLINE heMarkerRecordedEventAttributes #-}

-- | If the event is of type DecisionTaskFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heRecordMarkerFailedEventAttributes
    :: Functor f
    => (Maybe RecordMarkerFailedEventAttributes
    -> f (Maybe RecordMarkerFailedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heRecordMarkerFailedEventAttributes f x =
    (\y -> x { _heRecordMarkerFailedEventAttributes = y })
       <$> f (_heRecordMarkerFailedEventAttributes x)
{-# INLINE heRecordMarkerFailedEventAttributes #-}

-- | If the event is of type TimerStarted then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heTimerStartedEventAttributes
    :: Functor f
    => (Maybe TimerStartedEventAttributes
    -> f (Maybe TimerStartedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heTimerStartedEventAttributes f x =
    (\y -> x { _heTimerStartedEventAttributes = y })
       <$> f (_heTimerStartedEventAttributes x)
{-# INLINE heTimerStartedEventAttributes #-}

-- | If the event is of type TimerFired then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heTimerFiredEventAttributes
    :: Functor f
    => (Maybe TimerFiredEventAttributes
    -> f (Maybe TimerFiredEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heTimerFiredEventAttributes f x =
    (\y -> x { _heTimerFiredEventAttributes = y })
       <$> f (_heTimerFiredEventAttributes x)
{-# INLINE heTimerFiredEventAttributes #-}

-- | If the event is of type TimerCanceled then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heTimerCanceledEventAttributes
    :: Functor f
    => (Maybe TimerCanceledEventAttributes
    -> f (Maybe TimerCanceledEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heTimerCanceledEventAttributes f x =
    (\y -> x { _heTimerCanceledEventAttributes = y })
       <$> f (_heTimerCanceledEventAttributes x)
{-# INLINE heTimerCanceledEventAttributes #-}

-- | If the event is of type StartChildWorkflowExecutionInitiated then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heStartChildWorkflowExecutionInitiatedEventAttributes
    :: Functor f
    => (Maybe StartChildWorkflowExecutionInitiatedEventAttributes
    -> f (Maybe StartChildWorkflowExecutionInitiatedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heStartChildWorkflowExecutionInitiatedEventAttributes f x =
    (\y -> x { _heStartChildWorkflowExecutionInitiatedEventAttributes = y })
       <$> f (_heStartChildWorkflowExecutionInitiatedEventAttributes x)
{-# INLINE heStartChildWorkflowExecutionInitiatedEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionStarted then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionStartedEventAttributes
    :: Functor f
    => (Maybe ChildWorkflowExecutionStartedEventAttributes
    -> f (Maybe ChildWorkflowExecutionStartedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heChildWorkflowExecutionStartedEventAttributes f x =
    (\y -> x { _heChildWorkflowExecutionStartedEventAttributes = y })
       <$> f (_heChildWorkflowExecutionStartedEventAttributes x)
{-# INLINE heChildWorkflowExecutionStartedEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionCompleted then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionCompletedEventAttributes
    :: Functor f
    => (Maybe ChildWorkflowExecutionCompletedEventAttributes
    -> f (Maybe ChildWorkflowExecutionCompletedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heChildWorkflowExecutionCompletedEventAttributes f x =
    (\y -> x { _heChildWorkflowExecutionCompletedEventAttributes = y })
       <$> f (_heChildWorkflowExecutionCompletedEventAttributes x)
{-# INLINE heChildWorkflowExecutionCompletedEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionFailedEventAttributes
    :: Functor f
    => (Maybe ChildWorkflowExecutionFailedEventAttributes
    -> f (Maybe ChildWorkflowExecutionFailedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heChildWorkflowExecutionFailedEventAttributes f x =
    (\y -> x { _heChildWorkflowExecutionFailedEventAttributes = y })
       <$> f (_heChildWorkflowExecutionFailedEventAttributes x)
{-# INLINE heChildWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionTimedOut then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionTimedOutEventAttributes
    :: Functor f
    => (Maybe ChildWorkflowExecutionTimedOutEventAttributes
    -> f (Maybe ChildWorkflowExecutionTimedOutEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heChildWorkflowExecutionTimedOutEventAttributes f x =
    (\y -> x { _heChildWorkflowExecutionTimedOutEventAttributes = y })
       <$> f (_heChildWorkflowExecutionTimedOutEventAttributes x)
{-# INLINE heChildWorkflowExecutionTimedOutEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionCanceled then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionCanceledEventAttributes
    :: Functor f
    => (Maybe ChildWorkflowExecutionCanceledEventAttributes
    -> f (Maybe ChildWorkflowExecutionCanceledEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heChildWorkflowExecutionCanceledEventAttributes f x =
    (\y -> x { _heChildWorkflowExecutionCanceledEventAttributes = y })
       <$> f (_heChildWorkflowExecutionCanceledEventAttributes x)
{-# INLINE heChildWorkflowExecutionCanceledEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionTerminated then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionTerminatedEventAttributes
    :: Functor f
    => (Maybe ChildWorkflowExecutionTerminatedEventAttributes
    -> f (Maybe ChildWorkflowExecutionTerminatedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heChildWorkflowExecutionTerminatedEventAttributes f x =
    (\y -> x { _heChildWorkflowExecutionTerminatedEventAttributes = y })
       <$> f (_heChildWorkflowExecutionTerminatedEventAttributes x)
{-# INLINE heChildWorkflowExecutionTerminatedEventAttributes #-}

-- | If the event is of type SignalExternalWorkflowExecutionInitiated then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heSignalExternalWorkflowExecutionInitiatedEventAttributes
    :: Functor f
    => (Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes
    -> f (Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heSignalExternalWorkflowExecutionInitiatedEventAttributes f x =
    (\y -> x { _heSignalExternalWorkflowExecutionInitiatedEventAttributes = y })
       <$> f (_heSignalExternalWorkflowExecutionInitiatedEventAttributes x)
{-# INLINE heSignalExternalWorkflowExecutionInitiatedEventAttributes #-}

-- | If the event is of type ExternalWorkflowExecutionSignaled then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
heExternalWorkflowExecutionSignaledEventAttributes
    :: Functor f
    => (Maybe ExternalWorkflowExecutionSignaledEventAttributes
    -> f (Maybe ExternalWorkflowExecutionSignaledEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heExternalWorkflowExecutionSignaledEventAttributes f x =
    (\y -> x { _heExternalWorkflowExecutionSignaledEventAttributes = y })
       <$> f (_heExternalWorkflowExecutionSignaledEventAttributes x)
{-# INLINE heExternalWorkflowExecutionSignaledEventAttributes #-}

-- | If the event is of type SignalExternalWorkflowExecutionFailed then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heSignalExternalWorkflowExecutionFailedEventAttributes
    :: Functor f
    => (Maybe SignalExternalWorkflowExecutionFailedEventAttributes
    -> f (Maybe SignalExternalWorkflowExecutionFailedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heSignalExternalWorkflowExecutionFailedEventAttributes f x =
    (\y -> x { _heSignalExternalWorkflowExecutionFailedEventAttributes = y })
       <$> f (_heSignalExternalWorkflowExecutionFailedEventAttributes x)
{-# INLINE heSignalExternalWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type ExternalWorkflowExecutionCancelRequested then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heExternalWorkflowExecutionCancelRequestedEventAttributes
    :: Functor f
    => (Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes
    -> f (Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heExternalWorkflowExecutionCancelRequestedEventAttributes f x =
    (\y -> x { _heExternalWorkflowExecutionCancelRequestedEventAttributes = y })
       <$> f (_heExternalWorkflowExecutionCancelRequestedEventAttributes x)
{-# INLINE heExternalWorkflowExecutionCancelRequestedEventAttributes #-}

-- | If the event is of type RequestCancelExternalWorkflowExecutionInitiated
-- then this member is set and provides detailed information about the event.
-- It is not set for other event types.
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    :: Functor f
    => (Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    -> f (Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes f x =
    (\y -> x { _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes = y })
       <$> f (_heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes x)
{-# INLINE heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes #-}

-- | If the event is of type RequestCancelExternalWorkflowExecutionFailed then
-- this member is set and provides detailed information about the event. It is
-- not set for other event types.
heRequestCancelExternalWorkflowExecutionFailedEventAttributes
    :: Functor f
    => (Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes
    -> f (Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heRequestCancelExternalWorkflowExecutionFailedEventAttributes f x =
    (\y -> x { _heRequestCancelExternalWorkflowExecutionFailedEventAttributes = y })
       <$> f (_heRequestCancelExternalWorkflowExecutionFailedEventAttributes x)
{-# INLINE heRequestCancelExternalWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type ScheduleActivityTaskFailed then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heScheduleActivityTaskFailedEventAttributes
    :: Functor f
    => (Maybe ScheduleActivityTaskFailedEventAttributes
    -> f (Maybe ScheduleActivityTaskFailedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heScheduleActivityTaskFailedEventAttributes f x =
    (\y -> x { _heScheduleActivityTaskFailedEventAttributes = y })
       <$> f (_heScheduleActivityTaskFailedEventAttributes x)
{-# INLINE heScheduleActivityTaskFailedEventAttributes #-}

-- | If the event is of type RequestCancelActivityTaskFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heRequestCancelActivityTaskFailedEventAttributes
    :: Functor f
    => (Maybe RequestCancelActivityTaskFailedEventAttributes
    -> f (Maybe RequestCancelActivityTaskFailedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heRequestCancelActivityTaskFailedEventAttributes f x =
    (\y -> x { _heRequestCancelActivityTaskFailedEventAttributes = y })
       <$> f (_heRequestCancelActivityTaskFailedEventAttributes x)
{-# INLINE heRequestCancelActivityTaskFailedEventAttributes #-}

-- | If the event is of type StartTimerFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heStartTimerFailedEventAttributes
    :: Functor f
    => (Maybe StartTimerFailedEventAttributes
    -> f (Maybe StartTimerFailedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heStartTimerFailedEventAttributes f x =
    (\y -> x { _heStartTimerFailedEventAttributes = y })
       <$> f (_heStartTimerFailedEventAttributes x)
{-# INLINE heStartTimerFailedEventAttributes #-}

-- | If the event is of type CancelTimerFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heCancelTimerFailedEventAttributes
    :: Functor f
    => (Maybe CancelTimerFailedEventAttributes
    -> f (Maybe CancelTimerFailedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heCancelTimerFailedEventAttributes f x =
    (\y -> x { _heCancelTimerFailedEventAttributes = y })
       <$> f (_heCancelTimerFailedEventAttributes x)
{-# INLINE heCancelTimerFailedEventAttributes #-}

-- | If the event is of type StartChildWorkflowExecutionFailed then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
heStartChildWorkflowExecutionFailedEventAttributes
    :: Functor f
    => (Maybe StartChildWorkflowExecutionFailedEventAttributes
    -> f (Maybe StartChildWorkflowExecutionFailedEventAttributes))
    -> HistoryEvent
    -> f HistoryEvent
heStartChildWorkflowExecutionFailedEventAttributes f x =
    (\y -> x { _heStartChildWorkflowExecutionFailedEventAttributes = y })
       <$> f (_heStartChildWorkflowExecutionFailedEventAttributes x)
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
mreaMarkerName
    :: Functor f
    => (Text
    -> f (Text))
    -> MarkerRecordedEventAttributes
    -> f MarkerRecordedEventAttributes
mreaMarkerName f x =
    (\y -> x { _mreaMarkerName = y })
       <$> f (_mreaMarkerName x)
{-# INLINE mreaMarkerName #-}

-- | Details of the marker (if any).
mreaDetails
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> MarkerRecordedEventAttributes
    -> f MarkerRecordedEventAttributes
mreaDetails f x =
    (\y -> x { _mreaDetails = y })
       <$> f (_mreaDetails x)
{-# INLINE mreaDetails #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RecordMarker decision that requested this marker.
-- This information can be useful for diagnosing problems by tracing back the
-- cause of events.
mreaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> MarkerRecordedEventAttributes
    -> f MarkerRecordedEventAttributes
mreaDecisionTaskCompletedEventId f x =
    (\y -> x { _mreaDecisionTaskCompletedEventId = y })
       <$> f (_mreaDecisionTaskCompletedEventId x)
{-# INLINE mreaDecisionTaskCompletedEventId #-}

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
rmdaMarkerName
    :: Functor f
    => (Text
    -> f (Text))
    -> RecordMarkerDecisionAttributes
    -> f RecordMarkerDecisionAttributes
rmdaMarkerName f x =
    (\y -> x { _rmdaMarkerName = y })
       <$> f (_rmdaMarkerName x)
{-# INLINE rmdaMarkerName #-}

-- | Optional details of the marker.
rmdaDetails
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RecordMarkerDecisionAttributes
    -> f RecordMarkerDecisionAttributes
rmdaDetails f x =
    (\y -> x { _rmdaDetails = y })
       <$> f (_rmdaDetails x)
{-# INLINE rmdaDetails #-}

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
rmfeaMarkerName
    :: Functor f
    => (Text
    -> f (Text))
    -> RecordMarkerFailedEventAttributes
    -> f RecordMarkerFailedEventAttributes
rmfeaMarkerName f x =
    (\y -> x { _rmfeaMarkerName = y })
       <$> f (_rmfeaMarkerName x)
{-# INLINE rmfeaMarkerName #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
rmfeaCause
    :: Functor f
    => (RecordMarkerFailedCause
    -> f (RecordMarkerFailedCause))
    -> RecordMarkerFailedEventAttributes
    -> f RecordMarkerFailedEventAttributes
rmfeaCause f x =
    (\y -> x { _rmfeaCause = y })
       <$> f (_rmfeaCause x)
{-# INLINE rmfeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RecordMarkerFailed decision for this cancellation
-- request. This information can be useful for diagnosing problems by tracing
-- back the cause of events.
rmfeaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> RecordMarkerFailedEventAttributes
    -> f RecordMarkerFailedEventAttributes
rmfeaDecisionTaskCompletedEventId f x =
    (\y -> x { _rmfeaDecisionTaskCompletedEventId = y })
       <$> f (_rmfeaDecisionTaskCompletedEventId x)
{-# INLINE rmfeaDecisionTaskCompletedEventId #-}

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
rcatfeaActivityId
    :: Functor f
    => (Text
    -> f (Text))
    -> RequestCancelActivityTaskFailedEventAttributes
    -> f RequestCancelActivityTaskFailedEventAttributes
rcatfeaActivityId f x =
    (\y -> x { _rcatfeaActivityId = y })
       <$> f (_rcatfeaActivityId x)
{-# INLINE rcatfeaActivityId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
rcatfeaCause
    :: Functor f
    => (RequestCancelActivityTaskFailedCause
    -> f (RequestCancelActivityTaskFailedCause))
    -> RequestCancelActivityTaskFailedEventAttributes
    -> f RequestCancelActivityTaskFailedEventAttributes
rcatfeaCause f x =
    (\y -> x { _rcatfeaCause = y })
       <$> f (_rcatfeaCause x)
{-# INLINE rcatfeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RequestCancelActivityTask decision for this
-- cancellation request. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
rcatfeaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> RequestCancelActivityTaskFailedEventAttributes
    -> f RequestCancelActivityTaskFailedEventAttributes
rcatfeaDecisionTaskCompletedEventId f x =
    (\y -> x { _rcatfeaDecisionTaskCompletedEventId = y })
       <$> f (_rcatfeaDecisionTaskCompletedEventId x)
{-# INLINE rcatfeaDecisionTaskCompletedEventId #-}

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
rcewedaWorkflowId
    :: Functor f
    => (Text
    -> f (Text))
    -> RequestCancelExternalWorkflowExecutionDecisionAttributes
    -> f RequestCancelExternalWorkflowExecutionDecisionAttributes
rcewedaWorkflowId f x =
    (\y -> x { _rcewedaWorkflowId = y })
       <$> f (_rcewedaWorkflowId x)
{-# INLINE rcewedaWorkflowId #-}

-- | The runId of the external workflow execution to cancel.
rcewedaRunId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RequestCancelExternalWorkflowExecutionDecisionAttributes
    -> f RequestCancelExternalWorkflowExecutionDecisionAttributes
rcewedaRunId f x =
    (\y -> x { _rcewedaRunId = y })
       <$> f (_rcewedaRunId x)
{-# INLINE rcewedaRunId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
rcewedaControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RequestCancelExternalWorkflowExecutionDecisionAttributes
    -> f RequestCancelExternalWorkflowExecutionDecisionAttributes
rcewedaControl f x =
    (\y -> x { _rcewedaControl = y })
       <$> f (_rcewedaControl x)
{-# INLINE rcewedaControl #-}

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
rcewefeaWorkflowId
    :: Functor f
    => (Text
    -> f (Text))
    -> RequestCancelExternalWorkflowExecutionFailedEventAttributes
    -> f RequestCancelExternalWorkflowExecutionFailedEventAttributes
rcewefeaWorkflowId f x =
    (\y -> x { _rcewefeaWorkflowId = y })
       <$> f (_rcewefeaWorkflowId x)
{-# INLINE rcewefeaWorkflowId #-}

-- | The runId of the external workflow execution.
rcewefeaRunId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RequestCancelExternalWorkflowExecutionFailedEventAttributes
    -> f RequestCancelExternalWorkflowExecutionFailedEventAttributes
rcewefeaRunId f x =
    (\y -> x { _rcewefeaRunId = y })
       <$> f (_rcewefeaRunId x)
{-# INLINE rcewefeaRunId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
rcewefeaCause
    :: Functor f
    => (RequestCancelExternalWorkflowExecutionFailedCause
    -> f (RequestCancelExternalWorkflowExecutionFailedCause))
    -> RequestCancelExternalWorkflowExecutionFailedEventAttributes
    -> f RequestCancelExternalWorkflowExecutionFailedEventAttributes
rcewefeaCause f x =
    (\y -> x { _rcewefeaCause = y })
       <$> f (_rcewefeaCause x)
{-# INLINE rcewefeaCause #-}

-- | The id of the RequestCancelExternalWorkflowExecutionInitiated event
-- corresponding to the RequestCancelExternalWorkflowExecution decision to
-- cancel this external workflow execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
rcewefeaInitiatedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> RequestCancelExternalWorkflowExecutionFailedEventAttributes
    -> f RequestCancelExternalWorkflowExecutionFailedEventAttributes
rcewefeaInitiatedEventId f x =
    (\y -> x { _rcewefeaInitiatedEventId = y })
       <$> f (_rcewefeaInitiatedEventId x)
{-# INLINE rcewefeaInitiatedEventId #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RequestCancelExternalWorkflowExecution decision
-- for this cancellation request. This information can be useful for
-- diagnosing problems by tracing back the cause of events.
rcewefeaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> RequestCancelExternalWorkflowExecutionFailedEventAttributes
    -> f RequestCancelExternalWorkflowExecutionFailedEventAttributes
rcewefeaDecisionTaskCompletedEventId f x =
    (\y -> x { _rcewefeaDecisionTaskCompletedEventId = y })
       <$> f (_rcewefeaDecisionTaskCompletedEventId x)
{-# INLINE rcewefeaDecisionTaskCompletedEventId #-}

rcewefeaControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RequestCancelExternalWorkflowExecutionFailedEventAttributes
    -> f RequestCancelExternalWorkflowExecutionFailedEventAttributes
rcewefeaControl f x =
    (\y -> x { _rcewefeaControl = y })
       <$> f (_rcewefeaControl x)
{-# INLINE rcewefeaControl #-}

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
rceweieaWorkflowId
    :: Functor f
    => (Text
    -> f (Text))
    -> RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    -> f RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
rceweieaWorkflowId f x =
    (\y -> x { _rceweieaWorkflowId = y })
       <$> f (_rceweieaWorkflowId x)
{-# INLINE rceweieaWorkflowId #-}

-- | The runId of the external workflow execution to be canceled.
rceweieaRunId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    -> f RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
rceweieaRunId f x =
    (\y -> x { _rceweieaRunId = y })
       <$> f (_rceweieaRunId x)
{-# INLINE rceweieaRunId #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RequestCancelExternalWorkflowExecution decision
-- for this cancellation request. This information can be useful for
-- diagnosing problems by tracing back the cause of events.
rceweieaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    -> f RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
rceweieaDecisionTaskCompletedEventId f x =
    (\y -> x { _rceweieaDecisionTaskCompletedEventId = y })
       <$> f (_rceweieaDecisionTaskCompletedEventId x)
{-# INLINE rceweieaDecisionTaskCompletedEventId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
rceweieaControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    -> f RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
rceweieaControl f x =
    (\y -> x { _rceweieaControl = y })
       <$> f (_rceweieaControl x)
{-# INLINE rceweieaControl #-}

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
satdaActivityType
    :: Functor f
    => (ActivityType
    -> f (ActivityType))
    -> ScheduleActivityTaskDecisionAttributes
    -> f ScheduleActivityTaskDecisionAttributes
satdaActivityType f x =
    (\y -> x { _satdaActivityType = y })
       <$> f (_satdaActivityType x)
{-# INLINE satdaActivityType #-}

-- | The activityId of the activity task. This field is required. The specified
-- string must not start or end with whitespace. It must not contain a :
-- (colon), / (slash), | (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string &quot;arn&quot;.
satdaActivityId
    :: Functor f
    => (Text
    -> f (Text))
    -> ScheduleActivityTaskDecisionAttributes
    -> f ScheduleActivityTaskDecisionAttributes
satdaActivityId f x =
    (\y -> x { _satdaActivityId = y })
       <$> f (_satdaActivityId x)
{-# INLINE satdaActivityId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks. This data is not sent to the activity.
satdaControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ScheduleActivityTaskDecisionAttributes
    -> f ScheduleActivityTaskDecisionAttributes
satdaControl f x =
    (\y -> x { _satdaControl = y })
       <$> f (_satdaControl x)
{-# INLINE satdaControl #-}

-- | The input provided to the activity task.
satdaInput
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ScheduleActivityTaskDecisionAttributes
    -> f ScheduleActivityTaskDecisionAttributes
satdaInput f x =
    (\y -> x { _satdaInput = y })
       <$> f (_satdaInput x)
{-# INLINE satdaInput #-}

-- | The maximum duration for this activity task. The valid values are integers
-- greater than or equal to 0. An integer value can be used to specify the
-- duration in seconds while NONE can be used to specify unlimited duration. A
-- schedule-to-close timeout for this activity task must be specified either
-- as a default for the activity type or through this field. If neither this
-- field is set nor a default schedule-to-close timeout was specified at
-- registration time then a fault will be returned.
satdaScheduleToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ScheduleActivityTaskDecisionAttributes
    -> f ScheduleActivityTaskDecisionAttributes
satdaScheduleToCloseTimeout f x =
    (\y -> x { _satdaScheduleToCloseTimeout = y })
       <$> f (_satdaScheduleToCloseTimeout x)
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
satdaTaskList
    :: Functor f
    => (Maybe TaskList
    -> f (Maybe TaskList))
    -> ScheduleActivityTaskDecisionAttributes
    -> f ScheduleActivityTaskDecisionAttributes
satdaTaskList f x =
    (\y -> x { _satdaTaskList = y })
       <$> f (_satdaTaskList x)
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
satdaScheduleToStartTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ScheduleActivityTaskDecisionAttributes
    -> f ScheduleActivityTaskDecisionAttributes
satdaScheduleToStartTimeout f x =
    (\y -> x { _satdaScheduleToStartTimeout = y })
       <$> f (_satdaScheduleToStartTimeout x)
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
satdaStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ScheduleActivityTaskDecisionAttributes
    -> f ScheduleActivityTaskDecisionAttributes
satdaStartToCloseTimeout f x =
    (\y -> x { _satdaStartToCloseTimeout = y })
       <$> f (_satdaStartToCloseTimeout x)
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
satdaHeartbeatTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ScheduleActivityTaskDecisionAttributes
    -> f ScheduleActivityTaskDecisionAttributes
satdaHeartbeatTimeout f x =
    (\y -> x { _satdaHeartbeatTimeout = y })
       <$> f (_satdaHeartbeatTimeout x)
{-# INLINE satdaHeartbeatTimeout #-}

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
satfeaActivityType
    :: Functor f
    => (ActivityType
    -> f (ActivityType))
    -> ScheduleActivityTaskFailedEventAttributes
    -> f ScheduleActivityTaskFailedEventAttributes
satfeaActivityType f x =
    (\y -> x { _satfeaActivityType = y })
       <$> f (_satfeaActivityType x)
{-# INLINE satfeaActivityType #-}

-- | The activityId provided in the ScheduleActivityTask decision that failed.
satfeaActivityId
    :: Functor f
    => (Text
    -> f (Text))
    -> ScheduleActivityTaskFailedEventAttributes
    -> f ScheduleActivityTaskFailedEventAttributes
satfeaActivityId f x =
    (\y -> x { _satfeaActivityId = y })
       <$> f (_satfeaActivityId x)
{-# INLINE satfeaActivityId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
satfeaCause
    :: Functor f
    => (ScheduleActivityTaskFailedCause
    -> f (ScheduleActivityTaskFailedCause))
    -> ScheduleActivityTaskFailedEventAttributes
    -> f ScheduleActivityTaskFailedEventAttributes
satfeaCause f x =
    (\y -> x { _satfeaCause = y })
       <$> f (_satfeaCause x)
{-# INLINE satfeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- that resulted in the scheduling of this activity task. This information can
-- be useful for diagnosing problems by tracing back the chain of events
-- leading up to this event.
satfeaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ScheduleActivityTaskFailedEventAttributes
    -> f ScheduleActivityTaskFailedEventAttributes
satfeaDecisionTaskCompletedEventId f x =
    (\y -> x { _satfeaDecisionTaskCompletedEventId = y })
       <$> f (_satfeaDecisionTaskCompletedEventId x)
{-# INLINE satfeaDecisionTaskCompletedEventId #-}

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
sewedaWorkflowId
    :: Functor f
    => (Text
    -> f (Text))
    -> SignalExternalWorkflowExecutionDecisionAttributes
    -> f SignalExternalWorkflowExecutionDecisionAttributes
sewedaWorkflowId f x =
    (\y -> x { _sewedaWorkflowId = y })
       <$> f (_sewedaWorkflowId x)
{-# INLINE sewedaWorkflowId #-}

-- | The runId of the workflow execution to be signaled.
sewedaRunId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SignalExternalWorkflowExecutionDecisionAttributes
    -> f SignalExternalWorkflowExecutionDecisionAttributes
sewedaRunId f x =
    (\y -> x { _sewedaRunId = y })
       <$> f (_sewedaRunId x)
{-# INLINE sewedaRunId #-}

-- | The name of the signal.The target workflow execution will use the signal
-- name and input to process the signal. This field is required.
sewedaSignalName
    :: Functor f
    => (Text
    -> f (Text))
    -> SignalExternalWorkflowExecutionDecisionAttributes
    -> f SignalExternalWorkflowExecutionDecisionAttributes
sewedaSignalName f x =
    (\y -> x { _sewedaSignalName = y })
       <$> f (_sewedaSignalName x)
{-# INLINE sewedaSignalName #-}

-- | Optional input to be provided with the signal.The target workflow execution
-- will use the signal name and input to process the signal.
sewedaInput
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SignalExternalWorkflowExecutionDecisionAttributes
    -> f SignalExternalWorkflowExecutionDecisionAttributes
sewedaInput f x =
    (\y -> x { _sewedaInput = y })
       <$> f (_sewedaInput x)
{-# INLINE sewedaInput #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent decision tasks.
sewedaControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SignalExternalWorkflowExecutionDecisionAttributes
    -> f SignalExternalWorkflowExecutionDecisionAttributes
sewedaControl f x =
    (\y -> x { _sewedaControl = y })
       <$> f (_sewedaControl x)
{-# INLINE sewedaControl #-}

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
sewefeaWorkflowId
    :: Functor f
    => (Text
    -> f (Text))
    -> SignalExternalWorkflowExecutionFailedEventAttributes
    -> f SignalExternalWorkflowExecutionFailedEventAttributes
sewefeaWorkflowId f x =
    (\y -> x { _sewefeaWorkflowId = y })
       <$> f (_sewefeaWorkflowId x)
{-# INLINE sewefeaWorkflowId #-}

-- | The runId of the external workflow execution that the signal was being
-- delivered to.
sewefeaRunId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SignalExternalWorkflowExecutionFailedEventAttributes
    -> f SignalExternalWorkflowExecutionFailedEventAttributes
sewefeaRunId f x =
    (\y -> x { _sewefeaRunId = y })
       <$> f (_sewefeaRunId x)
{-# INLINE sewefeaRunId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
sewefeaCause
    :: Functor f
    => (SignalExternalWorkflowExecutionFailedCause
    -> f (SignalExternalWorkflowExecutionFailedCause))
    -> SignalExternalWorkflowExecutionFailedEventAttributes
    -> f SignalExternalWorkflowExecutionFailedEventAttributes
sewefeaCause f x =
    (\y -> x { _sewefeaCause = y })
       <$> f (_sewefeaCause x)
{-# INLINE sewefeaCause #-}

-- | The id of the SignalExternalWorkflowExecutionInitiated event corresponding
-- to the SignalExternalWorkflowExecution decision to request this signal.
-- This information can be useful for diagnosing problems by tracing back the
-- chain of events leading up to this event.
sewefeaInitiatedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> SignalExternalWorkflowExecutionFailedEventAttributes
    -> f SignalExternalWorkflowExecutionFailedEventAttributes
sewefeaInitiatedEventId f x =
    (\y -> x { _sewefeaInitiatedEventId = y })
       <$> f (_sewefeaInitiatedEventId x)
{-# INLINE sewefeaInitiatedEventId #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the SignalExternalWorkflowExecution decision for this
-- signal. This information can be useful for diagnosing problems by tracing
-- back the cause of events leading up to this event.
sewefeaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> SignalExternalWorkflowExecutionFailedEventAttributes
    -> f SignalExternalWorkflowExecutionFailedEventAttributes
sewefeaDecisionTaskCompletedEventId f x =
    (\y -> x { _sewefeaDecisionTaskCompletedEventId = y })
       <$> f (_sewefeaDecisionTaskCompletedEventId x)
{-# INLINE sewefeaDecisionTaskCompletedEventId #-}

sewefeaControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SignalExternalWorkflowExecutionFailedEventAttributes
    -> f SignalExternalWorkflowExecutionFailedEventAttributes
sewefeaControl f x =
    (\y -> x { _sewefeaControl = y })
       <$> f (_sewefeaControl x)
{-# INLINE sewefeaControl #-}

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
seweieaWorkflowId
    :: Functor f
    => (Text
    -> f (Text))
    -> SignalExternalWorkflowExecutionInitiatedEventAttributes
    -> f SignalExternalWorkflowExecutionInitiatedEventAttributes
seweieaWorkflowId f x =
    (\y -> x { _seweieaWorkflowId = y })
       <$> f (_seweieaWorkflowId x)
{-# INLINE seweieaWorkflowId #-}

-- | The runId of the external workflow execution to send the signal to.
seweieaRunId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SignalExternalWorkflowExecutionInitiatedEventAttributes
    -> f SignalExternalWorkflowExecutionInitiatedEventAttributes
seweieaRunId f x =
    (\y -> x { _seweieaRunId = y })
       <$> f (_seweieaRunId x)
{-# INLINE seweieaRunId #-}

-- | The name of the signal.
seweieaSignalName
    :: Functor f
    => (Text
    -> f (Text))
    -> SignalExternalWorkflowExecutionInitiatedEventAttributes
    -> f SignalExternalWorkflowExecutionInitiatedEventAttributes
seweieaSignalName f x =
    (\y -> x { _seweieaSignalName = y })
       <$> f (_seweieaSignalName x)
{-# INLINE seweieaSignalName #-}

-- | Input provided to the signal (if any).
seweieaInput
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SignalExternalWorkflowExecutionInitiatedEventAttributes
    -> f SignalExternalWorkflowExecutionInitiatedEventAttributes
seweieaInput f x =
    (\y -> x { _seweieaInput = y })
       <$> f (_seweieaInput x)
{-# INLINE seweieaInput #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the SignalExternalWorkflowExecution decision for this
-- signal. This information can be useful for diagnosing problems by tracing
-- back the cause of events leading up to this event.
seweieaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> SignalExternalWorkflowExecutionInitiatedEventAttributes
    -> f SignalExternalWorkflowExecutionInitiatedEventAttributes
seweieaDecisionTaskCompletedEventId f x =
    (\y -> x { _seweieaDecisionTaskCompletedEventId = y })
       <$> f (_seweieaDecisionTaskCompletedEventId x)
{-# INLINE seweieaDecisionTaskCompletedEventId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent decision tasks.
seweieaControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SignalExternalWorkflowExecutionInitiatedEventAttributes
    -> f SignalExternalWorkflowExecutionInitiatedEventAttributes
seweieaControl f x =
    (\y -> x { _seweieaControl = y })
       <$> f (_seweieaControl x)
{-# INLINE seweieaControl #-}

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
scwedaWorkflowType
    :: Functor f
    => (WorkflowType
    -> f (WorkflowType))
    -> StartChildWorkflowExecutionDecisionAttributes
    -> f StartChildWorkflowExecutionDecisionAttributes
scwedaWorkflowType f x =
    (\y -> x { _scwedaWorkflowType = y })
       <$> f (_scwedaWorkflowType x)
{-# INLINE scwedaWorkflowType #-}

-- | The workflowId of the workflow execution. This field is required. The
-- specified string must not start or end with whitespace. It must not contain
-- a : (colon), / (slash), | (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string &quot;arn&quot;.
scwedaWorkflowId
    :: Functor f
    => (Text
    -> f (Text))
    -> StartChildWorkflowExecutionDecisionAttributes
    -> f StartChildWorkflowExecutionDecisionAttributes
scwedaWorkflowId f x =
    (\y -> x { _scwedaWorkflowId = y })
       <$> f (_scwedaWorkflowId x)
{-# INLINE scwedaWorkflowId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks. This data is not sent to the child workflow
-- execution.
scwedaControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StartChildWorkflowExecutionDecisionAttributes
    -> f StartChildWorkflowExecutionDecisionAttributes
scwedaControl f x =
    (\y -> x { _scwedaControl = y })
       <$> f (_scwedaControl x)
{-# INLINE scwedaControl #-}

-- | The input to be provided to the workflow execution.
scwedaInput
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StartChildWorkflowExecutionDecisionAttributes
    -> f StartChildWorkflowExecutionDecisionAttributes
scwedaInput f x =
    (\y -> x { _scwedaInput = y })
       <$> f (_scwedaInput x)
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
scwedaExecutionStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StartChildWorkflowExecutionDecisionAttributes
    -> f StartChildWorkflowExecutionDecisionAttributes
scwedaExecutionStartToCloseTimeout f x =
    (\y -> x { _scwedaExecutionStartToCloseTimeout = y })
       <$> f (_scwedaExecutionStartToCloseTimeout x)
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
scwedaTaskList
    :: Functor f
    => (Maybe TaskList
    -> f (Maybe TaskList))
    -> StartChildWorkflowExecutionDecisionAttributes
    -> f StartChildWorkflowExecutionDecisionAttributes
scwedaTaskList f x =
    (\y -> x { _scwedaTaskList = y })
       <$> f (_scwedaTaskList x)
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
scwedaTaskStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StartChildWorkflowExecutionDecisionAttributes
    -> f StartChildWorkflowExecutionDecisionAttributes
scwedaTaskStartToCloseTimeout f x =
    (\y -> x { _scwedaTaskStartToCloseTimeout = y })
       <$> f (_scwedaTaskStartToCloseTimeout x)
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
scwedaChildPolicy
    :: Functor f
    => (Maybe ChildPolicy
    -> f (Maybe ChildPolicy))
    -> StartChildWorkflowExecutionDecisionAttributes
    -> f StartChildWorkflowExecutionDecisionAttributes
scwedaChildPolicy f x =
    (\y -> x { _scwedaChildPolicy = y })
       <$> f (_scwedaChildPolicy x)
{-# INLINE scwedaChildPolicy #-}

-- | The list of tags to associate with the child workflow execution. A maximum
-- of 5 tags can be specified. You can list workflow executions with a
-- specific tag by calling ListOpenWorkflowExecutions or
-- ListClosedWorkflowExecutions and specifying a TagFilter.
scwedaTagList
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> StartChildWorkflowExecutionDecisionAttributes
    -> f StartChildWorkflowExecutionDecisionAttributes
scwedaTagList f x =
    (\y -> x { _scwedaTagList = y })
       <$> f (_scwedaTagList x)
{-# INLINE scwedaTagList #-}

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
scwefeaWorkflowType
    :: Functor f
    => (WorkflowType
    -> f (WorkflowType))
    -> StartChildWorkflowExecutionFailedEventAttributes
    -> f StartChildWorkflowExecutionFailedEventAttributes
scwefeaWorkflowType f x =
    (\y -> x { _scwefeaWorkflowType = y })
       <$> f (_scwefeaWorkflowType x)
{-# INLINE scwefeaWorkflowType #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
scwefeaCause
    :: Functor f
    => (StartChildWorkflowExecutionFailedCause
    -> f (StartChildWorkflowExecutionFailedCause))
    -> StartChildWorkflowExecutionFailedEventAttributes
    -> f StartChildWorkflowExecutionFailedEventAttributes
scwefeaCause f x =
    (\y -> x { _scwefeaCause = y })
       <$> f (_scwefeaCause x)
{-# INLINE scwefeaCause #-}

-- | The workflowId of the child workflow execution.
scwefeaWorkflowId
    :: Functor f
    => (Text
    -> f (Text))
    -> StartChildWorkflowExecutionFailedEventAttributes
    -> f StartChildWorkflowExecutionFailedEventAttributes
scwefeaWorkflowId f x =
    (\y -> x { _scwefeaWorkflowId = y })
       <$> f (_scwefeaWorkflowId x)
{-# INLINE scwefeaWorkflowId #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
scwefeaInitiatedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> StartChildWorkflowExecutionFailedEventAttributes
    -> f StartChildWorkflowExecutionFailedEventAttributes
scwefeaInitiatedEventId f x =
    (\y -> x { _scwefeaInitiatedEventId = y })
       <$> f (_scwefeaInitiatedEventId x)
{-# INLINE scwefeaInitiatedEventId #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the StartChildWorkflowExecution Decision to request
-- this child workflow execution. This information can be useful for
-- diagnosing problems by tracing back the cause of events.
scwefeaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> StartChildWorkflowExecutionFailedEventAttributes
    -> f StartChildWorkflowExecutionFailedEventAttributes
scwefeaDecisionTaskCompletedEventId f x =
    (\y -> x { _scwefeaDecisionTaskCompletedEventId = y })
       <$> f (_scwefeaDecisionTaskCompletedEventId x)
{-# INLINE scwefeaDecisionTaskCompletedEventId #-}

scwefeaControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StartChildWorkflowExecutionFailedEventAttributes
    -> f StartChildWorkflowExecutionFailedEventAttributes
scwefeaControl f x =
    (\y -> x { _scwefeaControl = y })
       <$> f (_scwefeaControl x)
{-# INLINE scwefeaControl #-}

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
scweieaWorkflowId
    :: Functor f
    => (Text
    -> f (Text))
    -> StartChildWorkflowExecutionInitiatedEventAttributes
    -> f StartChildWorkflowExecutionInitiatedEventAttributes
scweieaWorkflowId f x =
    (\y -> x { _scweieaWorkflowId = y })
       <$> f (_scweieaWorkflowId x)
{-# INLINE scweieaWorkflowId #-}

-- | The type of the child workflow execution.
scweieaWorkflowType
    :: Functor f
    => (WorkflowType
    -> f (WorkflowType))
    -> StartChildWorkflowExecutionInitiatedEventAttributes
    -> f StartChildWorkflowExecutionInitiatedEventAttributes
scweieaWorkflowType f x =
    (\y -> x { _scweieaWorkflowType = y })
       <$> f (_scweieaWorkflowType x)
{-# INLINE scweieaWorkflowType #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent decision tasks. This data is not sent to the activity.
scweieaControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StartChildWorkflowExecutionInitiatedEventAttributes
    -> f StartChildWorkflowExecutionInitiatedEventAttributes
scweieaControl f x =
    (\y -> x { _scweieaControl = y })
       <$> f (_scweieaControl x)
{-# INLINE scweieaControl #-}

-- | The inputs provided to the child workflow execution (if any).
scweieaInput
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StartChildWorkflowExecutionInitiatedEventAttributes
    -> f StartChildWorkflowExecutionInitiatedEventAttributes
scweieaInput f x =
    (\y -> x { _scweieaInput = y })
       <$> f (_scweieaInput x)
{-# INLINE scweieaInput #-}

-- | The maximum duration for the child workflow execution. If the workflow
-- execution is not closed within this duration, it will be timed out and
-- force terminated. The valid values are integers greater than or equal to 0.
-- An integer value can be used to specify the duration in seconds while NONE
-- can be used to specify unlimited duration.
scweieaExecutionStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StartChildWorkflowExecutionInitiatedEventAttributes
    -> f StartChildWorkflowExecutionInitiatedEventAttributes
scweieaExecutionStartToCloseTimeout f x =
    (\y -> x { _scweieaExecutionStartToCloseTimeout = y })
       <$> f (_scweieaExecutionStartToCloseTimeout x)
{-# INLINE scweieaExecutionStartToCloseTimeout #-}

-- | The name of the task list used for the decision tasks of the child workflow
-- execution.
scweieaTaskList
    :: Functor f
    => (TaskList
    -> f (TaskList))
    -> StartChildWorkflowExecutionInitiatedEventAttributes
    -> f StartChildWorkflowExecutionInitiatedEventAttributes
scweieaTaskList f x =
    (\y -> x { _scweieaTaskList = y })
       <$> f (_scweieaTaskList x)
{-# INLINE scweieaTaskList #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the StartChildWorkflowExecution Decision to request
-- this child workflow execution. This information can be useful for
-- diagnosing problems by tracing back the cause of events.
scweieaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> StartChildWorkflowExecutionInitiatedEventAttributes
    -> f StartChildWorkflowExecutionInitiatedEventAttributes
scweieaDecisionTaskCompletedEventId f x =
    (\y -> x { _scweieaDecisionTaskCompletedEventId = y })
       <$> f (_scweieaDecisionTaskCompletedEventId x)
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
scweieaChildPolicy
    :: Functor f
    => (ChildPolicy
    -> f (ChildPolicy))
    -> StartChildWorkflowExecutionInitiatedEventAttributes
    -> f StartChildWorkflowExecutionInitiatedEventAttributes
scweieaChildPolicy f x =
    (\y -> x { _scweieaChildPolicy = y })
       <$> f (_scweieaChildPolicy x)
{-# INLINE scweieaChildPolicy #-}

-- | The maximum duration allowed for the decision tasks for this workflow
-- execution. The valid values are integers greater than or equal to 0. An
-- integer value can be used to specify the duration in seconds while NONE can
-- be used to specify unlimited duration.
scweieaTaskStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StartChildWorkflowExecutionInitiatedEventAttributes
    -> f StartChildWorkflowExecutionInitiatedEventAttributes
scweieaTaskStartToCloseTimeout f x =
    (\y -> x { _scweieaTaskStartToCloseTimeout = y })
       <$> f (_scweieaTaskStartToCloseTimeout x)
{-# INLINE scweieaTaskStartToCloseTimeout #-}

-- | The list of tags to associated with the child workflow execution.
scweieaTagList
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> StartChildWorkflowExecutionInitiatedEventAttributes
    -> f StartChildWorkflowExecutionInitiatedEventAttributes
scweieaTagList f x =
    (\y -> x { _scweieaTagList = y })
       <$> f (_scweieaTagList x)
{-# INLINE scweieaTagList #-}

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
stdaTimerId
    :: Functor f
    => (Text
    -> f (Text))
    -> StartTimerDecisionAttributes
    -> f StartTimerDecisionAttributes
stdaTimerId f x =
    (\y -> x { _stdaTimerId = y })
       <$> f (_stdaTimerId x)
{-# INLINE stdaTimerId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
stdaControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StartTimerDecisionAttributes
    -> f StartTimerDecisionAttributes
stdaControl f x =
    (\y -> x { _stdaControl = y })
       <$> f (_stdaControl x)
{-# INLINE stdaControl #-}

-- | The duration to wait before firing the timer. This field is required. The
-- duration is specified in seconds. The valid values are integers greater
-- than or equal to 0.
stdaStartToFireTimeout
    :: Functor f
    => (Text
    -> f (Text))
    -> StartTimerDecisionAttributes
    -> f StartTimerDecisionAttributes
stdaStartToFireTimeout f x =
    (\y -> x { _stdaStartToFireTimeout = y })
       <$> f (_stdaStartToFireTimeout x)
{-# INLINE stdaStartToFireTimeout #-}

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
stfeaTimerId
    :: Functor f
    => (Text
    -> f (Text))
    -> StartTimerFailedEventAttributes
    -> f StartTimerFailedEventAttributes
stfeaTimerId f x =
    (\y -> x { _stfeaTimerId = y })
       <$> f (_stfeaTimerId x)
{-# INLINE stfeaTimerId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
stfeaCause
    :: Functor f
    => (StartTimerFailedCause
    -> f (StartTimerFailedCause))
    -> StartTimerFailedEventAttributes
    -> f StartTimerFailedEventAttributes
stfeaCause f x =
    (\y -> x { _stfeaCause = y })
       <$> f (_stfeaCause x)
{-# INLINE stfeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the StartTimer decision for this activity task. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
stfeaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> StartTimerFailedEventAttributes
    -> f StartTimerFailedEventAttributes
stfeaDecisionTaskCompletedEventId f x =
    (\y -> x { _stfeaDecisionTaskCompletedEventId = y })
       <$> f (_stfeaDecisionTaskCompletedEventId x)
{-# INLINE stfeaDecisionTaskCompletedEventId #-}

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
tceaTimerId
    :: Functor f
    => (Text
    -> f (Text))
    -> TimerCanceledEventAttributes
    -> f TimerCanceledEventAttributes
tceaTimerId f x =
    (\y -> x { _tceaTimerId = y })
       <$> f (_tceaTimerId x)
{-# INLINE tceaTimerId #-}

-- | The id of the TimerStarted event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
tceaStartedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> TimerCanceledEventAttributes
    -> f TimerCanceledEventAttributes
tceaStartedEventId f x =
    (\y -> x { _tceaStartedEventId = y })
       <$> f (_tceaStartedEventId x)
{-# INLINE tceaStartedEventId #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CancelTimer decision to cancel this timer. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
tceaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> TimerCanceledEventAttributes
    -> f TimerCanceledEventAttributes
tceaDecisionTaskCompletedEventId f x =
    (\y -> x { _tceaDecisionTaskCompletedEventId = y })
       <$> f (_tceaDecisionTaskCompletedEventId x)
{-# INLINE tceaDecisionTaskCompletedEventId #-}

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
tfeaTimerId
    :: Functor f
    => (Text
    -> f (Text))
    -> TimerFiredEventAttributes
    -> f TimerFiredEventAttributes
tfeaTimerId f x =
    (\y -> x { _tfeaTimerId = y })
       <$> f (_tfeaTimerId x)
{-# INLINE tfeaTimerId #-}

-- | The id of the TimerStarted event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
tfeaStartedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> TimerFiredEventAttributes
    -> f TimerFiredEventAttributes
tfeaStartedEventId f x =
    (\y -> x { _tfeaStartedEventId = y })
       <$> f (_tfeaStartedEventId x)
{-# INLINE tfeaStartedEventId #-}

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
tseaTimerId
    :: Functor f
    => (Text
    -> f (Text))
    -> TimerStartedEventAttributes
    -> f TimerStartedEventAttributes
tseaTimerId f x =
    (\y -> x { _tseaTimerId = y })
       <$> f (_tseaTimerId x)
{-# INLINE tseaTimerId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
tseaControl
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TimerStartedEventAttributes
    -> f TimerStartedEventAttributes
tseaControl f x =
    (\y -> x { _tseaControl = y })
       <$> f (_tseaControl x)
{-# INLINE tseaControl #-}

-- | The duration of time after which the timer will fire. The duration is
-- specified in seconds. The valid values are integers greater than or equal
-- to 0.
tseaStartToFireTimeout
    :: Functor f
    => (Text
    -> f (Text))
    -> TimerStartedEventAttributes
    -> f TimerStartedEventAttributes
tseaStartToFireTimeout f x =
    (\y -> x { _tseaStartToFireTimeout = y })
       <$> f (_tseaStartToFireTimeout x)
{-# INLINE tseaStartToFireTimeout #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the StartTimer decision for this activity task. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
tseaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> TimerStartedEventAttributes
    -> f TimerStartedEventAttributes
tseaDecisionTaskCompletedEventId f x =
    (\y -> x { _tseaDecisionTaskCompletedEventId = y })
       <$> f (_tseaDecisionTaskCompletedEventId x)
{-# INLINE tseaDecisionTaskCompletedEventId #-}

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
weWorkflowId
    :: Functor f
    => (Text
    -> f (Text))
    -> WorkflowExecution
    -> f WorkflowExecution
weWorkflowId f x =
    (\y -> x { _weWorkflowId = y })
       <$> f (_weWorkflowId x)
{-# INLINE weWorkflowId #-}

-- | A system generated unique identifier for the workflow execution.
weRunId
    :: Functor f
    => (Text
    -> f (Text))
    -> WorkflowExecution
    -> f WorkflowExecution
weRunId f x =
    (\y -> x { _weRunId = y })
       <$> f (_weRunId x)
{-# INLINE weRunId #-}

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
wecreaExternalWorkflowExecution
    :: Functor f
    => (Maybe WorkflowExecution
    -> f (Maybe WorkflowExecution))
    -> WorkflowExecutionCancelRequestedEventAttributes
    -> f WorkflowExecutionCancelRequestedEventAttributes
wecreaExternalWorkflowExecution f x =
    (\y -> x { _wecreaExternalWorkflowExecution = y })
       <$> f (_wecreaExternalWorkflowExecution x)
{-# INLINE wecreaExternalWorkflowExecution #-}

-- | The id of the RequestCancelExternalWorkflowExecutionInitiated event
-- corresponding to the RequestCancelExternalWorkflowExecution decision to
-- cancel this workflow execution.The source event with this Id can be found
-- in the history of the source workflow execution. This information can be
-- useful for diagnosing problems by tracing back the chain of events leading
-- up to this event.
wecreaExternalInitiatedEventId
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> WorkflowExecutionCancelRequestedEventAttributes
    -> f WorkflowExecutionCancelRequestedEventAttributes
wecreaExternalInitiatedEventId f x =
    (\y -> x { _wecreaExternalInitiatedEventId = y })
       <$> f (_wecreaExternalInitiatedEventId x)
{-# INLINE wecreaExternalInitiatedEventId #-}

-- | If set, indicates that the request to cancel the workflow execution was
-- automatically generated, and specifies the cause. This happens if the
-- parent workflow execution times out or is terminated, and the child policy
-- is set to cancel child executions.
wecreaCause
    :: Functor f
    => (Maybe WorkflowExecutionCancelRequestedCause
    -> f (Maybe WorkflowExecutionCancelRequestedCause))
    -> WorkflowExecutionCancelRequestedEventAttributes
    -> f WorkflowExecutionCancelRequestedEventAttributes
wecreaCause f x =
    (\y -> x { _wecreaCause = y })
       <$> f (_wecreaCause x)
{-# INLINE wecreaCause #-}

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
wecebDetails
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowExecutionCanceledEventAttributes
    -> f WorkflowExecutionCanceledEventAttributes
wecebDetails f x =
    (\y -> x { _wecebDetails = y })
       <$> f (_wecebDetails x)
{-# INLINE wecebDetails #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CancelWorkflowExecution decision for this
-- cancellation request. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
wecebDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> WorkflowExecutionCanceledEventAttributes
    -> f WorkflowExecutionCanceledEventAttributes
wecebDecisionTaskCompletedEventId f x =
    (\y -> x { _wecebDecisionTaskCompletedEventId = y })
       <$> f (_wecebDecisionTaskCompletedEventId x)
{-# INLINE wecebDecisionTaskCompletedEventId #-}

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
weceaResult
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowExecutionCompletedEventAttributes
    -> f WorkflowExecutionCompletedEventAttributes
weceaResult f x =
    (\y -> x { _weceaResult = y })
       <$> f (_weceaResult x)
{-# INLINE weceaResult #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CompleteWorkflowExecution decision to complete
-- this execution. This information can be useful for diagnosing problems by
-- tracing back the cause of events.
weceaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> WorkflowExecutionCompletedEventAttributes
    -> f WorkflowExecutionCompletedEventAttributes
weceaDecisionTaskCompletedEventId f x =
    (\y -> x { _weceaDecisionTaskCompletedEventId = y })
       <$> f (_weceaDecisionTaskCompletedEventId x)
{-# INLINE weceaDecisionTaskCompletedEventId #-}

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
wehTaskStartToCloseTimeout
    :: Functor f
    => (Text
    -> f (Text))
    -> WorkflowExecutionConfiguration
    -> f WorkflowExecutionConfiguration
wehTaskStartToCloseTimeout f x =
    (\y -> x { _wehTaskStartToCloseTimeout = y })
       <$> f (_wehTaskStartToCloseTimeout x)
{-# INLINE wehTaskStartToCloseTimeout #-}

-- | The total duration for this workflow execution. The valid values are
-- integers greater than or equal to 0. An integer value can be used to
-- specify the duration in seconds while NONE can be used to specify unlimited
-- duration.
wehExecutionStartToCloseTimeout
    :: Functor f
    => (Text
    -> f (Text))
    -> WorkflowExecutionConfiguration
    -> f WorkflowExecutionConfiguration
wehExecutionStartToCloseTimeout f x =
    (\y -> x { _wehExecutionStartToCloseTimeout = y })
       <$> f (_wehExecutionStartToCloseTimeout x)
{-# INLINE wehExecutionStartToCloseTimeout #-}

-- | The task list used for the decision tasks generated for this workflow
-- execution.
wehTaskList
    :: Functor f
    => (TaskList
    -> f (TaskList))
    -> WorkflowExecutionConfiguration
    -> f WorkflowExecutionConfiguration
wehTaskList f x =
    (\y -> x { _wehTaskList = y })
       <$> f (_wehTaskList x)
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
wehChildPolicy
    :: Functor f
    => (ChildPolicy
    -> f (ChildPolicy))
    -> WorkflowExecutionConfiguration
    -> f WorkflowExecutionConfiguration
wehChildPolicy f x =
    (\y -> x { _wehChildPolicy = y })
       <$> f (_wehChildPolicy x)
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
wecaneaInput
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowExecutionContinuedAsNewEventAttributes
    -> f WorkflowExecutionContinuedAsNewEventAttributes
wecaneaInput f x =
    (\y -> x { _wecaneaInput = y })
       <$> f (_wecaneaInput x)
{-# INLINE wecaneaInput #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the ContinueAsNewWorkflowExecution decision that
-- started this execution. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
wecaneaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> WorkflowExecutionContinuedAsNewEventAttributes
    -> f WorkflowExecutionContinuedAsNewEventAttributes
wecaneaDecisionTaskCompletedEventId f x =
    (\y -> x { _wecaneaDecisionTaskCompletedEventId = y })
       <$> f (_wecaneaDecisionTaskCompletedEventId x)
{-# INLINE wecaneaDecisionTaskCompletedEventId #-}

-- | The runId of the new workflow execution.
wecaneaNewExecutionRunId
    :: Functor f
    => (Text
    -> f (Text))
    -> WorkflowExecutionContinuedAsNewEventAttributes
    -> f WorkflowExecutionContinuedAsNewEventAttributes
wecaneaNewExecutionRunId f x =
    (\y -> x { _wecaneaNewExecutionRunId = y })
       <$> f (_wecaneaNewExecutionRunId x)
{-# INLINE wecaneaNewExecutionRunId #-}

-- | The total duration allowed for the new workflow execution. The valid values
-- are integers greater than or equal to 0. An integer value can be used to
-- specify the duration in seconds while NONE can be used to specify unlimited
-- duration.
wecaneaExecutionStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowExecutionContinuedAsNewEventAttributes
    -> f WorkflowExecutionContinuedAsNewEventAttributes
wecaneaExecutionStartToCloseTimeout f x =
    (\y -> x { _wecaneaExecutionStartToCloseTimeout = y })
       <$> f (_wecaneaExecutionStartToCloseTimeout x)
{-# INLINE wecaneaExecutionStartToCloseTimeout #-}

-- | Represents a task list.
wecaneaTaskList
    :: Functor f
    => (TaskList
    -> f (TaskList))
    -> WorkflowExecutionContinuedAsNewEventAttributes
    -> f WorkflowExecutionContinuedAsNewEventAttributes
wecaneaTaskList f x =
    (\y -> x { _wecaneaTaskList = y })
       <$> f (_wecaneaTaskList x)
{-# INLINE wecaneaTaskList #-}

-- | The maximum duration of decision tasks for the new workflow execution. The
-- valid values are integers greater than or equal to 0. An integer value can
-- be used to specify the duration in seconds while NONE can be used to
-- specify unlimited duration.
wecaneaTaskStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowExecutionContinuedAsNewEventAttributes
    -> f WorkflowExecutionContinuedAsNewEventAttributes
wecaneaTaskStartToCloseTimeout f x =
    (\y -> x { _wecaneaTaskStartToCloseTimeout = y })
       <$> f (_wecaneaTaskStartToCloseTimeout x)
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
wecaneaChildPolicy
    :: Functor f
    => (ChildPolicy
    -> f (ChildPolicy))
    -> WorkflowExecutionContinuedAsNewEventAttributes
    -> f WorkflowExecutionContinuedAsNewEventAttributes
wecaneaChildPolicy f x =
    (\y -> x { _wecaneaChildPolicy = y })
       <$> f (_wecaneaChildPolicy x)
{-# INLINE wecaneaChildPolicy #-}

-- | The list of tags associated with the new workflow execution.
wecaneaTagList
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> WorkflowExecutionContinuedAsNewEventAttributes
    -> f WorkflowExecutionContinuedAsNewEventAttributes
wecaneaTagList f x =
    (\y -> x { _wecaneaTagList = y })
       <$> f (_wecaneaTagList x)
{-# INLINE wecaneaTagList #-}

-- | Represents a workflow type.
wecaneaWorkflowType
    :: Functor f
    => (WorkflowType
    -> f (WorkflowType))
    -> WorkflowExecutionContinuedAsNewEventAttributes
    -> f WorkflowExecutionContinuedAsNewEventAttributes
wecaneaWorkflowType f x =
    (\y -> x { _wecaneaWorkflowType = y })
       <$> f (_wecaneaWorkflowType x)
{-# INLINE wecaneaWorkflowType #-}

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
wefeaReason
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowExecutionFailedEventAttributes
    -> f WorkflowExecutionFailedEventAttributes
wefeaReason f x =
    (\y -> x { _wefeaReason = y })
       <$> f (_wefeaReason x)
{-# INLINE wefeaReason #-}

-- | The details of the failure (if any).
wefeaDetails
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowExecutionFailedEventAttributes
    -> f WorkflowExecutionFailedEventAttributes
wefeaDetails f x =
    (\y -> x { _wefeaDetails = y })
       <$> f (_wefeaDetails x)
{-# INLINE wefeaDetails #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the FailWorkflowExecution decision to fail this
-- execution. This information can be useful for diagnosing problems by
-- tracing back the cause of events.
wefeaDecisionTaskCompletedEventId
    :: Functor f
    => (Integer
    -> f (Integer))
    -> WorkflowExecutionFailedEventAttributes
    -> f WorkflowExecutionFailedEventAttributes
wefeaDecisionTaskCompletedEventId f x =
    (\y -> x { _wefeaDecisionTaskCompletedEventId = y })
       <$> f (_wefeaDecisionTaskCompletedEventId x)
{-# INLINE wefeaDecisionTaskCompletedEventId #-}

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
weiExecution
    :: Functor f
    => (WorkflowExecution
    -> f (WorkflowExecution))
    -> WorkflowExecutionInfo
    -> f WorkflowExecutionInfo
weiExecution f x =
    (\y -> x { _weiExecution = y })
       <$> f (_weiExecution x)
{-# INLINE weiExecution #-}

-- | The type of the workflow execution.
weiWorkflowType
    :: Functor f
    => (WorkflowType
    -> f (WorkflowType))
    -> WorkflowExecutionInfo
    -> f WorkflowExecutionInfo
weiWorkflowType f x =
    (\y -> x { _weiWorkflowType = y })
       <$> f (_weiWorkflowType x)
{-# INLINE weiWorkflowType #-}

-- | The time when the execution was started.
weiStartTimestamp
    :: Functor f
    => (POSIX
    -> f (POSIX))
    -> WorkflowExecutionInfo
    -> f WorkflowExecutionInfo
weiStartTimestamp f x =
    (\y -> x { _weiStartTimestamp = y })
       <$> f (_weiStartTimestamp x)
{-# INLINE weiStartTimestamp #-}

-- | The time when the workflow execution was closed. Set only if the execution
-- status is CLOSED.
weiCloseTimestamp
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> WorkflowExecutionInfo
    -> f WorkflowExecutionInfo
weiCloseTimestamp f x =
    (\y -> x { _weiCloseTimestamp = y })
       <$> f (_weiCloseTimestamp x)
{-# INLINE weiCloseTimestamp #-}

-- | The current status of the execution.
weiExecutionStatus
    :: Functor f
    => (ExecutionStatus
    -> f (ExecutionStatus))
    -> WorkflowExecutionInfo
    -> f WorkflowExecutionInfo
weiExecutionStatus f x =
    (\y -> x { _weiExecutionStatus = y })
       <$> f (_weiExecutionStatus x)
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
weiCloseStatus
    :: Functor f
    => (Maybe CloseStatus
    -> f (Maybe CloseStatus))
    -> WorkflowExecutionInfo
    -> f WorkflowExecutionInfo
weiCloseStatus f x =
    (\y -> x { _weiCloseStatus = y })
       <$> f (_weiCloseStatus x)
{-# INLINE weiCloseStatus #-}

-- | If this workflow execution is a child of another execution then contains
-- the workflow execution that started this execution.
weiParent
    :: Functor f
    => (Maybe WorkflowExecution
    -> f (Maybe WorkflowExecution))
    -> WorkflowExecutionInfo
    -> f WorkflowExecutionInfo
weiParent f x =
    (\y -> x { _weiParent = y })
       <$> f (_weiParent x)
{-# INLINE weiParent #-}

-- | The list of tags associated with the workflow execution. Tags can be used
-- to identify and list workflow executions of interest through the visibility
-- APIs. A workflow execution can have a maximum of 5 tags.
weiTagList
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> WorkflowExecutionInfo
    -> f WorkflowExecutionInfo
weiTagList f x =
    (\y -> x { _weiTagList = y })
       <$> f (_weiTagList x)
{-# INLINE weiTagList #-}

-- | Set to true if a cancellation is requested for this workflow execution.
weiCancelRequested
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> WorkflowExecutionInfo
    -> f WorkflowExecutionInfo
weiCancelRequested f x =
    (\y -> x { _weiCancelRequested = y })
       <$> f (_weiCancelRequested x)
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
weocOpenActivityTasks
    :: Functor f
    => (Integer
    -> f (Integer))
    -> WorkflowExecutionOpenCounts
    -> f WorkflowExecutionOpenCounts
weocOpenActivityTasks f x =
    (\y -> x { _weocOpenActivityTasks = y })
       <$> f (_weocOpenActivityTasks x)
{-# INLINE weocOpenActivityTasks #-}

-- | The count of decision tasks whose status is OPEN. A workflow execution can
-- have at most one open decision task.
weocOpenDecisionTasks
    :: Functor f
    => (Integer
    -> f (Integer))
    -> WorkflowExecutionOpenCounts
    -> f WorkflowExecutionOpenCounts
weocOpenDecisionTasks f x =
    (\y -> x { _weocOpenDecisionTasks = y })
       <$> f (_weocOpenDecisionTasks x)
{-# INLINE weocOpenDecisionTasks #-}

-- | The count of timers started by this workflow execution that have not fired
-- yet.
weocOpenTimers
    :: Functor f
    => (Integer
    -> f (Integer))
    -> WorkflowExecutionOpenCounts
    -> f WorkflowExecutionOpenCounts
weocOpenTimers f x =
    (\y -> x { _weocOpenTimers = y })
       <$> f (_weocOpenTimers x)
{-# INLINE weocOpenTimers #-}

-- | The count of child workflow executions whose status is OPEN.
weocOpenChildWorkflowExecutions
    :: Functor f
    => (Integer
    -> f (Integer))
    -> WorkflowExecutionOpenCounts
    -> f WorkflowExecutionOpenCounts
weocOpenChildWorkflowExecutions f x =
    (\y -> x { _weocOpenChildWorkflowExecutions = y })
       <$> f (_weocOpenChildWorkflowExecutions x)
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
wesebSignalName
    :: Functor f
    => (Text
    -> f (Text))
    -> WorkflowExecutionSignaledEventAttributes
    -> f WorkflowExecutionSignaledEventAttributes
wesebSignalName f x =
    (\y -> x { _wesebSignalName = y })
       <$> f (_wesebSignalName x)
{-# INLINE wesebSignalName #-}

-- | Inputs provided with the signal (if any). The decider can use the signal
-- name and inputs to determine how to process the signal.
wesebInput
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowExecutionSignaledEventAttributes
    -> f WorkflowExecutionSignaledEventAttributes
wesebInput f x =
    (\y -> x { _wesebInput = y })
       <$> f (_wesebInput x)
{-# INLINE wesebInput #-}

-- | The workflow execution that sent the signal. This is set only of the signal
-- was sent by another workflow execution.
wesebExternalWorkflowExecution
    :: Functor f
    => (Maybe WorkflowExecution
    -> f (Maybe WorkflowExecution))
    -> WorkflowExecutionSignaledEventAttributes
    -> f WorkflowExecutionSignaledEventAttributes
wesebExternalWorkflowExecution f x =
    (\y -> x { _wesebExternalWorkflowExecution = y })
       <$> f (_wesebExternalWorkflowExecution x)
{-# INLINE wesebExternalWorkflowExecution #-}

-- | The id of the SignalExternalWorkflowExecutionInitiated event corresponding
-- to the SignalExternalWorkflow decision to signal this workflow
-- execution.The source event with this Id can be found in the history of the
-- source workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event. This
-- field is set only if the signal was initiated by another workflow
-- execution.
wesebExternalInitiatedEventId
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> WorkflowExecutionSignaledEventAttributes
    -> f WorkflowExecutionSignaledEventAttributes
wesebExternalInitiatedEventId f x =
    (\y -> x { _wesebExternalInitiatedEventId = y })
       <$> f (_wesebExternalInitiatedEventId x)
{-# INLINE wesebExternalInitiatedEventId #-}

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
weseaInput
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowExecutionStartedEventAttributes
    -> f WorkflowExecutionStartedEventAttributes
weseaInput f x =
    (\y -> x { _weseaInput = y })
       <$> f (_weseaInput x)
{-# INLINE weseaInput #-}

-- | The maximum duration for this workflow execution. The valid values are
-- integers greater than or equal to 0. An integer value can be used to
-- specify the duration in seconds while NONE can be used to specify unlimited
-- duration.
weseaExecutionStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowExecutionStartedEventAttributes
    -> f WorkflowExecutionStartedEventAttributes
weseaExecutionStartToCloseTimeout f x =
    (\y -> x { _weseaExecutionStartToCloseTimeout = y })
       <$> f (_weseaExecutionStartToCloseTimeout x)
{-# INLINE weseaExecutionStartToCloseTimeout #-}

-- | The maximum duration of decision tasks for this workflow type. The valid
-- values are integers greater than or equal to 0. An integer value can be
-- used to specify the duration in seconds while NONE can be used to specify
-- unlimited duration.
weseaTaskStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowExecutionStartedEventAttributes
    -> f WorkflowExecutionStartedEventAttributes
weseaTaskStartToCloseTimeout f x =
    (\y -> x { _weseaTaskStartToCloseTimeout = y })
       <$> f (_weseaTaskStartToCloseTimeout x)
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
weseaChildPolicy
    :: Functor f
    => (ChildPolicy
    -> f (ChildPolicy))
    -> WorkflowExecutionStartedEventAttributes
    -> f WorkflowExecutionStartedEventAttributes
weseaChildPolicy f x =
    (\y -> x { _weseaChildPolicy = y })
       <$> f (_weseaChildPolicy x)
{-# INLINE weseaChildPolicy #-}

-- | The name of the task list for scheduling the decision tasks for this
-- workflow execution.
weseaTaskList
    :: Functor f
    => (TaskList
    -> f (TaskList))
    -> WorkflowExecutionStartedEventAttributes
    -> f WorkflowExecutionStartedEventAttributes
weseaTaskList f x =
    (\y -> x { _weseaTaskList = y })
       <$> f (_weseaTaskList x)
{-# INLINE weseaTaskList #-}

-- | The workflow type of this execution.
weseaWorkflowType
    :: Functor f
    => (WorkflowType
    -> f (WorkflowType))
    -> WorkflowExecutionStartedEventAttributes
    -> f WorkflowExecutionStartedEventAttributes
weseaWorkflowType f x =
    (\y -> x { _weseaWorkflowType = y })
       <$> f (_weseaWorkflowType x)
{-# INLINE weseaWorkflowType #-}

-- | The list of tags associated with this workflow execution. An execution can
-- have up to 5 tags.
weseaTagList
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> WorkflowExecutionStartedEventAttributes
    -> f WorkflowExecutionStartedEventAttributes
weseaTagList f x =
    (\y -> x { _weseaTagList = y })
       <$> f (_weseaTagList x)
{-# INLINE weseaTagList #-}

-- | If this workflow execution was started due to a
-- ContinueAsNewWorkflowExecution decision, then it contains the runId of the
-- previous workflow execution that was closed and continued as this
-- execution.
weseaContinuedExecutionRunId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowExecutionStartedEventAttributes
    -> f WorkflowExecutionStartedEventAttributes
weseaContinuedExecutionRunId f x =
    (\y -> x { _weseaContinuedExecutionRunId = y })
       <$> f (_weseaContinuedExecutionRunId x)
{-# INLINE weseaContinuedExecutionRunId #-}

-- | The source workflow execution that started this workflow execution. The
-- member is not set if the workflow execution was not started by a workflow.
weseaParentWorkflowExecution
    :: Functor f
    => (Maybe WorkflowExecution
    -> f (Maybe WorkflowExecution))
    -> WorkflowExecutionStartedEventAttributes
    -> f WorkflowExecutionStartedEventAttributes
weseaParentWorkflowExecution f x =
    (\y -> x { _weseaParentWorkflowExecution = y })
       <$> f (_weseaParentWorkflowExecution x)
{-# INLINE weseaParentWorkflowExecution #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this workflow execution.
-- The source event with this Id can be found in the history of the source
-- workflow execution. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
weseaParentInitiatedEventId
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> WorkflowExecutionStartedEventAttributes
    -> f WorkflowExecutionStartedEventAttributes
weseaParentInitiatedEventId f x =
    (\y -> x { _weseaParentInitiatedEventId = y })
       <$> f (_weseaParentInitiatedEventId x)
{-# INLINE weseaParentInitiatedEventId #-}

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
weteaReason
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowExecutionTerminatedEventAttributes
    -> f WorkflowExecutionTerminatedEventAttributes
weteaReason f x =
    (\y -> x { _weteaReason = y })
       <$> f (_weteaReason x)
{-# INLINE weteaReason #-}

-- | The details provided for the termination (if any).
weteaDetails
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowExecutionTerminatedEventAttributes
    -> f WorkflowExecutionTerminatedEventAttributes
weteaDetails f x =
    (\y -> x { _weteaDetails = y })
       <$> f (_weteaDetails x)
{-# INLINE weteaDetails #-}

-- | The policy used for the child workflow executions of this workflow
-- execution. The supported child policies are: TERMINATE: the child
-- executions will be terminated. REQUEST_CANCEL: a request to cancel will be
-- attempted for each child execution by recording a
-- WorkflowExecutionCancelRequested event in its history. It is up to the
-- decider to take appropriate actions when it receives an execution history
-- with this event. ABANDON: no action will be taken. The child executions
-- will continue to run.
weteaChildPolicy
    :: Functor f
    => (ChildPolicy
    -> f (ChildPolicy))
    -> WorkflowExecutionTerminatedEventAttributes
    -> f WorkflowExecutionTerminatedEventAttributes
weteaChildPolicy f x =
    (\y -> x { _weteaChildPolicy = y })
       <$> f (_weteaChildPolicy x)
{-# INLINE weteaChildPolicy #-}

-- | If set, indicates that the workflow execution was automatically terminated,
-- and specifies the cause. This happens if the parent workflow execution
-- times out or is terminated and the child policy is set to terminate child
-- executions.
weteaCause
    :: Functor f
    => (Maybe WorkflowExecutionTerminatedCause
    -> f (Maybe WorkflowExecutionTerminatedCause))
    -> WorkflowExecutionTerminatedEventAttributes
    -> f WorkflowExecutionTerminatedEventAttributes
weteaCause f x =
    (\y -> x { _weteaCause = y })
       <$> f (_weteaCause x)
{-# INLINE weteaCause #-}

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
wetoeaTimeoutType
    :: Functor f
    => (WorkflowExecutionTimeoutType
    -> f (WorkflowExecutionTimeoutType))
    -> WorkflowExecutionTimedOutEventAttributes
    -> f WorkflowExecutionTimedOutEventAttributes
wetoeaTimeoutType f x =
    (\y -> x { _wetoeaTimeoutType = y })
       <$> f (_wetoeaTimeoutType x)
{-# INLINE wetoeaTimeoutType #-}

-- | The policy used for the child workflow executions of this workflow
-- execution. The supported child policies are: TERMINATE: the child
-- executions will be terminated. REQUEST_CANCEL: a request to cancel will be
-- attempted for each child execution by recording a
-- WorkflowExecutionCancelRequested event in its history. It is up to the
-- decider to take appropriate actions when it receives an execution history
-- with this event. ABANDON: no action will be taken. The child executions
-- will continue to run.
wetoeaChildPolicy
    :: Functor f
    => (ChildPolicy
    -> f (ChildPolicy))
    -> WorkflowExecutionTimedOutEventAttributes
    -> f WorkflowExecutionTimedOutEventAttributes
wetoeaChildPolicy f x =
    (\y -> x { _wetoeaChildPolicy = y })
       <$> f (_wetoeaChildPolicy x)
{-# INLINE wetoeaChildPolicy #-}

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
wtName
    :: Functor f
    => (Text
    -> f (Text))
    -> WorkflowType
    -> f WorkflowType
wtName f x =
    (\y -> x { _wtName = y })
       <$> f (_wtName x)
{-# INLINE wtName #-}

-- | The version of the workflow type. This field is required. The combination
-- of workflow type name and version must be unique with in a domain.
wtVersion
    :: Functor f
    => (Text
    -> f (Text))
    -> WorkflowType
    -> f WorkflowType
wtVersion f x =
    (\y -> x { _wtVersion = y })
       <$> f (_wtVersion x)
{-# INLINE wtVersion #-}

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
wtcDefaultTaskStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowTypeConfiguration
    -> f WorkflowTypeConfiguration
wtcDefaultTaskStartToCloseTimeout f x =
    (\y -> x { _wtcDefaultTaskStartToCloseTimeout = y })
       <$> f (_wtcDefaultTaskStartToCloseTimeout x)
{-# INLINE wtcDefaultTaskStartToCloseTimeout #-}

-- | The optional default maximum duration, specified when registering the
-- workflow type, for executions of this workflow type. This default can be
-- overridden when starting a workflow execution using the
-- StartWorkflowExecution action or the StartChildWorkflowExecution Decision.
-- The valid values are integers greater than or equal to 0. An integer value
-- can be used to specify the duration in seconds while NONE can be used to
-- specify unlimited duration.
wtcDefaultExecutionStartToCloseTimeout
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowTypeConfiguration
    -> f WorkflowTypeConfiguration
wtcDefaultExecutionStartToCloseTimeout f x =
    (\y -> x { _wtcDefaultExecutionStartToCloseTimeout = y })
       <$> f (_wtcDefaultExecutionStartToCloseTimeout x)
{-# INLINE wtcDefaultExecutionStartToCloseTimeout #-}

-- | The optional default task list, specified when registering the workflow
-- type, for decisions tasks scheduled for workflow executions of this type.
-- This default can be overridden when starting a workflow execution using the
-- StartWorkflowExecution action or the StartChildWorkflowExecution Decision.
wtcDefaultTaskList
    :: Functor f
    => (Maybe TaskList
    -> f (Maybe TaskList))
    -> WorkflowTypeConfiguration
    -> f WorkflowTypeConfiguration
wtcDefaultTaskList f x =
    (\y -> x { _wtcDefaultTaskList = y })
       <$> f (_wtcDefaultTaskList x)
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
wtcDefaultChildPolicy
    :: Functor f
    => (Maybe ChildPolicy
    -> f (Maybe ChildPolicy))
    -> WorkflowTypeConfiguration
    -> f WorkflowTypeConfiguration
wtcDefaultChildPolicy f x =
    (\y -> x { _wtcDefaultChildPolicy = y })
       <$> f (_wtcDefaultChildPolicy x)
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
wtfName
    :: Functor f
    => (Text
    -> f (Text))
    -> WorkflowTypeFilter
    -> f WorkflowTypeFilter
wtfName f x =
    (\y -> x { _wtfName = y })
       <$> f (_wtfName x)
{-# INLINE wtfName #-}

-- | Version of the workflow type.
wtfVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowTypeFilter
    -> f WorkflowTypeFilter
wtfVersion f x =
    (\y -> x { _wtfVersion = y })
       <$> f (_wtfVersion x)
{-# INLINE wtfVersion #-}

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
wtiWorkflowType
    :: Functor f
    => (WorkflowType
    -> f (WorkflowType))
    -> WorkflowTypeInfo
    -> f WorkflowTypeInfo
wtiWorkflowType f x =
    (\y -> x { _wtiWorkflowType = y })
       <$> f (_wtiWorkflowType x)
{-# INLINE wtiWorkflowType #-}

-- | The current status of the workflow type.
wtiStatus
    :: Functor f
    => (RegistrationStatus
    -> f (RegistrationStatus))
    -> WorkflowTypeInfo
    -> f WorkflowTypeInfo
wtiStatus f x =
    (\y -> x { _wtiStatus = y })
       <$> f (_wtiStatus x)
{-# INLINE wtiStatus #-}

-- | The description of the type registered through RegisterWorkflowType.
wtiDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> WorkflowTypeInfo
    -> f WorkflowTypeInfo
wtiDescription f x =
    (\y -> x { _wtiDescription = y })
       <$> f (_wtiDescription x)
{-# INLINE wtiDescription #-}

-- | The date when this type was registered.
wtiCreationDate
    :: Functor f
    => (POSIX
    -> f (POSIX))
    -> WorkflowTypeInfo
    -> f WorkflowTypeInfo
wtiCreationDate f x =
    (\y -> x { _wtiCreationDate = y })
       <$> f (_wtiCreationDate x)
{-# INLINE wtiCreationDate #-}

-- | If the type is in deprecated state, then it is set to the date when the
-- type was deprecated.
wtiDeprecationDate
    :: Functor f
    => (Maybe POSIX
    -> f (Maybe POSIX))
    -> WorkflowTypeInfo
    -> f WorkflowTypeInfo
wtiDeprecationDate f x =
    (\y -> x { _wtiDeprecationDate = y })
       <$> f (_wtiDeprecationDate x)
{-# INLINE wtiDeprecationDate #-}

instance FromJSON WorkflowTypeInfo
