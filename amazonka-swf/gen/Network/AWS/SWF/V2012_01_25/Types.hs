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
ctdaTimerId :: Lens' CancelTimerDecisionAttributes (Text)
ctdaTimerId f x =
    f (_ctdaTimerId x)
        <&> \y -> x { _ctdaTimerId = y }
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
cwedbDetails :: Lens' CancelWorkflowExecutionDecisionAttributes (Maybe Text)
cwedbDetails f x =
    f (_cwedbDetails x)
        <&> \y -> x { _cwedbDetails = y }
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
csfStatus :: Lens' CloseStatusFilter (CloseStatus)
csfStatus f x =
    f (_csfStatus x)
        <&> \y -> x { _csfStatus = y }
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
cwedaResult :: Lens' CompleteWorkflowExecutionDecisionAttributes (Maybe Text)
cwedaResult f x =
    f (_cwedaResult x)
        <&> \y -> x { _cwedaResult = y }
{-# INLINE cwedaResult #-}

instance FromJSON CompleteWorkflowExecutionDecisionAttributes

instance ToJSON CompleteWorkflowExecutionDecisionAttributes

-- | Contains the configuration settings of a domain.
newtype DomainConfiguration = DomainConfiguration
    { _dcWorkflowExecutionRetentionPeriodInDays :: Text
      -- ^ The retention period for workflow executions in this domain.
    } deriving (Show, Generic)

-- | The retention period for workflow executions in this domain.
dcWorkflowExecutionRetentionPeriodInDays :: Lens' DomainConfiguration (Text)
dcWorkflowExecutionRetentionPeriodInDays f x =
    f (_dcWorkflowExecutionRetentionPeriodInDays x)
        <&> \y -> x { _dcWorkflowExecutionRetentionPeriodInDays = y }
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
rcatdaActivityId f x =
    f (_rcatdaActivityId x)
        <&> \y -> x { _rcatdaActivityId = y }
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
tfTag :: Lens' TagFilter (Text)
tfTag f x =
    f (_tfTag x)
        <&> \y -> x { _tfTag = y }
{-# INLINE tfTag #-}

instance ToJSON TagFilter

-- | The name of the task list.
newtype TaskList = TaskList
    { _tlName :: Text
      -- ^ The name of the task list.
    } deriving (Show, Generic)

-- | The name of the task list.
tlName :: Lens' TaskList (Text)
tlName f x =
    f (_tlName x)
        <&> \y -> x { _tlName = y }
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
wefWorkflowId :: Lens' WorkflowExecutionFilter (Text)
wefWorkflowId f x =
    f (_wefWorkflowId x)
        <&> \y -> x { _wefWorkflowId = y }
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
atcreaDecisionTaskCompletedEventId :: Lens' ActivityTaskCancelRequestedEventAttributes (Integer)
atcreaDecisionTaskCompletedEventId f x =
    f (_atcreaDecisionTaskCompletedEventId x)
        <&> \y -> x { _atcreaDecisionTaskCompletedEventId = y }
{-# INLINE atcreaDecisionTaskCompletedEventId #-}

-- | The unique ID of the task.
atcreaActivityId :: Lens' ActivityTaskCancelRequestedEventAttributes (Text)
atcreaActivityId f x =
    f (_atcreaActivityId x)
        <&> \y -> x { _atcreaActivityId = y }
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
atcebDetails :: Lens' ActivityTaskCanceledEventAttributes (Maybe Text)
atcebDetails f x =
    f (_atcebDetails x)
        <&> \y -> x { _atcebDetails = y }
{-# INLINE atcebDetails #-}

-- | The id of the ActivityTaskScheduled event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atcebScheduledEventId :: Lens' ActivityTaskCanceledEventAttributes (Integer)
atcebScheduledEventId f x =
    f (_atcebScheduledEventId x)
        <&> \y -> x { _atcebScheduledEventId = y }
{-# INLINE atcebScheduledEventId #-}

-- | The Id of the ActivityTaskStarted event recorded when this activity task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
atcebStartedEventId :: Lens' ActivityTaskCanceledEventAttributes (Integer)
atcebStartedEventId f x =
    f (_atcebStartedEventId x)
        <&> \y -> x { _atcebStartedEventId = y }
{-# INLINE atcebStartedEventId #-}

-- | If set, contains the Id of the last ActivityTaskCancelRequested event
-- recorded for this activity task. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
atcebLatestCancelRequestedEventId :: Lens' ActivityTaskCanceledEventAttributes (Maybe Integer)
atcebLatestCancelRequestedEventId f x =
    f (_atcebLatestCancelRequestedEventId x)
        <&> \y -> x { _atcebLatestCancelRequestedEventId = y }
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
atceaResult :: Lens' ActivityTaskCompletedEventAttributes (Maybe Text)
atceaResult f x =
    f (_atceaResult x)
        <&> \y -> x { _atceaResult = y }
{-# INLINE atceaResult #-}

-- | The id of the ActivityTaskScheduled event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atceaScheduledEventId :: Lens' ActivityTaskCompletedEventAttributes (Integer)
atceaScheduledEventId f x =
    f (_atceaScheduledEventId x)
        <&> \y -> x { _atceaScheduledEventId = y }
{-# INLINE atceaScheduledEventId #-}

-- | The Id of the ActivityTaskStarted event recorded when this activity task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
atceaStartedEventId :: Lens' ActivityTaskCompletedEventAttributes (Integer)
atceaStartedEventId f x =
    f (_atceaStartedEventId x)
        <&> \y -> x { _atceaStartedEventId = y }
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
atfeaReason :: Lens' ActivityTaskFailedEventAttributes (Maybe Text)
atfeaReason f x =
    f (_atfeaReason x)
        <&> \y -> x { _atfeaReason = y }
{-# INLINE atfeaReason #-}

-- | The details of the failure (if any).
atfeaDetails :: Lens' ActivityTaskFailedEventAttributes (Maybe Text)
atfeaDetails f x =
    f (_atfeaDetails x)
        <&> \y -> x { _atfeaDetails = y }
{-# INLINE atfeaDetails #-}

-- | The id of the ActivityTaskScheduled event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atfeaScheduledEventId :: Lens' ActivityTaskFailedEventAttributes (Integer)
atfeaScheduledEventId f x =
    f (_atfeaScheduledEventId x)
        <&> \y -> x { _atfeaScheduledEventId = y }
{-# INLINE atfeaScheduledEventId #-}

-- | The Id of the ActivityTaskStarted event recorded when this activity task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
atfeaStartedEventId :: Lens' ActivityTaskFailedEventAttributes (Integer)
atfeaStartedEventId f x =
    f (_atfeaStartedEventId x)
        <&> \y -> x { _atfeaStartedEventId = y }
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
atseaActivityType :: Lens' ActivityTaskScheduledEventAttributes (ActivityType)
atseaActivityType f x =
    f (_atseaActivityType x)
        <&> \y -> x { _atseaActivityType = y }
{-# INLINE atseaActivityType #-}

-- | The unique id of the activity task.
atseaActivityId :: Lens' ActivityTaskScheduledEventAttributes (Text)
atseaActivityId f x =
    f (_atseaActivityId x)
        <&> \y -> x { _atseaActivityId = y }
{-# INLINE atseaActivityId #-}

-- | The input provided to the activity task.
atseaInput :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaInput f x =
    f (_atseaInput x)
        <&> \y -> x { _atseaInput = y }
{-# INLINE atseaInput #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks. This data is not sent to the activity.
atseaControl :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaControl f x =
    f (_atseaControl x)
        <&> \y -> x { _atseaControl = y }
{-# INLINE atseaControl #-}

-- | The maximum amount of time the activity task can wait to be assigned to a
-- worker.
atseaScheduleToStartTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaScheduleToStartTimeout f x =
    f (_atseaScheduleToStartTimeout x)
        <&> \y -> x { _atseaScheduleToStartTimeout = y }
{-# INLINE atseaScheduleToStartTimeout #-}

-- | The maximum amount of time for this activity task.
atseaScheduleToCloseTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaScheduleToCloseTimeout f x =
    f (_atseaScheduleToCloseTimeout x)
        <&> \y -> x { _atseaScheduleToCloseTimeout = y }
{-# INLINE atseaScheduleToCloseTimeout #-}

-- | The maximum amount of time a worker may take to process the activity task.
atseaStartToCloseTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaStartToCloseTimeout f x =
    f (_atseaStartToCloseTimeout x)
        <&> \y -> x { _atseaStartToCloseTimeout = y }
{-# INLINE atseaStartToCloseTimeout #-}

-- | The task list in which the activity task has been scheduled.
atseaTaskList :: Lens' ActivityTaskScheduledEventAttributes (TaskList)
atseaTaskList f x =
    f (_atseaTaskList x)
        <&> \y -> x { _atseaTaskList = y }
{-# INLINE atseaTaskList #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- that resulted in the scheduling of this activity task. This information can
-- be useful for diagnosing problems by tracing back the chain of events
-- leading up to this event.
atseaDecisionTaskCompletedEventId :: Lens' ActivityTaskScheduledEventAttributes (Integer)
atseaDecisionTaskCompletedEventId f x =
    f (_atseaDecisionTaskCompletedEventId x)
        <&> \y -> x { _atseaDecisionTaskCompletedEventId = y }
{-# INLINE atseaDecisionTaskCompletedEventId #-}

-- | The maximum time before which the worker processing this task must report
-- progress by calling RecordActivityTaskHeartbeat. If the timeout is
-- exceeded, the activity task is automatically timed out. If the worker
-- subsequently attempts to record a heartbeat or return a result, it will be
-- ignored.
atseaHeartbeatTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaHeartbeatTimeout f x =
    f (_atseaHeartbeatTimeout x)
        <&> \y -> x { _atseaHeartbeatTimeout = y }
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
atsebIdentity :: Lens' ActivityTaskStartedEventAttributes (Maybe Text)
atsebIdentity f x =
    f (_atsebIdentity x)
        <&> \y -> x { _atsebIdentity = y }
{-# INLINE atsebIdentity #-}

-- | The id of the ActivityTaskScheduled event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atsebScheduledEventId :: Lens' ActivityTaskStartedEventAttributes (Integer)
atsebScheduledEventId f x =
    f (_atsebScheduledEventId x)
        <&> \y -> x { _atsebScheduledEventId = y }
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
attoeaTimeoutType :: Lens' ActivityTaskTimedOutEventAttributes (ActivityTaskTimeoutType)
attoeaTimeoutType f x =
    f (_attoeaTimeoutType x)
        <&> \y -> x { _attoeaTimeoutType = y }
{-# INLINE attoeaTimeoutType #-}

-- | The id of the ActivityTaskScheduled event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
attoeaScheduledEventId :: Lens' ActivityTaskTimedOutEventAttributes (Integer)
attoeaScheduledEventId f x =
    f (_attoeaScheduledEventId x)
        <&> \y -> x { _attoeaScheduledEventId = y }
{-# INLINE attoeaScheduledEventId #-}

-- | The Id of the ActivityTaskStarted event recorded when this activity task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
attoeaStartedEventId :: Lens' ActivityTaskTimedOutEventAttributes (Integer)
attoeaStartedEventId f x =
    f (_attoeaStartedEventId x)
        <&> \y -> x { _attoeaStartedEventId = y }
{-# INLINE attoeaStartedEventId #-}

-- | Contains the content of the details parameter for the last call made by the
-- activity to RecordActivityTaskHeartbeat.
attoeaDetails :: Lens' ActivityTaskTimedOutEventAttributes (Maybe Text)
attoeaDetails f x =
    f (_attoeaDetails x)
        <&> \y -> x { _attoeaDetails = y }
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
atName :: Lens' ActivityType (Text)
atName f x =
    f (_atName x)
        <&> \y -> x { _atName = y }
{-# INLINE atName #-}

-- | The version of this activity. The combination of activity type name and
-- version must be unique with in a domain.
atVersion :: Lens' ActivityType (Text)
atVersion f x =
    f (_atVersion x)
        <&> \y -> x { _atVersion = y }
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
atcDefaultTaskStartToCloseTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskStartToCloseTimeout f x =
    f (_atcDefaultTaskStartToCloseTimeout x)
        <&> \y -> x { _atcDefaultTaskStartToCloseTimeout = y }
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
atcDefaultTaskHeartbeatTimeout f x =
    f (_atcDefaultTaskHeartbeatTimeout x)
        <&> \y -> x { _atcDefaultTaskHeartbeatTimeout = y }
{-# INLINE atcDefaultTaskHeartbeatTimeout #-}

-- | The optional default task list specified for this activity type at
-- registration. This default task list is used if a task list is not provided
-- when a task is scheduled through the ScheduleActivityTask Decision. You can
-- override this default when scheduling a task through the
-- ScheduleActivityTask Decision.
atcDefaultTaskList :: Lens' ActivityTypeConfiguration (Maybe TaskList)
atcDefaultTaskList f x =
    f (_atcDefaultTaskList x)
        <&> \y -> x { _atcDefaultTaskList = y }
{-# INLINE atcDefaultTaskList #-}

-- | The optional default maximum duration, specified when registering the
-- activity type, that a task of an activity type can wait before being
-- assigned to a worker. You can override this default when scheduling a task
-- through the ScheduleActivityTask Decision. The valid values are integers
-- greater than or equal to 0. An integer value can be used to specify the
-- duration in seconds while NONE can be used to specify unlimited duration.
atcDefaultTaskScheduleToStartTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskScheduleToStartTimeout f x =
    f (_atcDefaultTaskScheduleToStartTimeout x)
        <&> \y -> x { _atcDefaultTaskScheduleToStartTimeout = y }
{-# INLINE atcDefaultTaskScheduleToStartTimeout #-}

-- | The optional default maximum duration, specified when registering the
-- activity type, for tasks of this activity type. You can override this
-- default when scheduling a task through the ScheduleActivityTask Decision.
-- The valid values are integers greater than or equal to 0. An integer value
-- can be used to specify the duration in seconds while NONE can be used to
-- specify unlimited duration.
atcDefaultTaskScheduleToCloseTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskScheduleToCloseTimeout f x =
    f (_atcDefaultTaskScheduleToCloseTimeout x)
        <&> \y -> x { _atcDefaultTaskScheduleToCloseTimeout = y }
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
atiActivityType f x =
    f (_atiActivityType x)
        <&> \y -> x { _atiActivityType = y }
{-# INLINE atiActivityType #-}

-- | The current status of the activity type.
atiStatus :: Lens' ActivityTypeInfo (RegistrationStatus)
atiStatus f x =
    f (_atiStatus x)
        <&> \y -> x { _atiStatus = y }
{-# INLINE atiStatus #-}

-- | The description of the activity type provided in RegisterActivityType.
atiDescription :: Lens' ActivityTypeInfo (Maybe Text)
atiDescription f x =
    f (_atiDescription x)
        <&> \y -> x { _atiDescription = y }
{-# INLINE atiDescription #-}

-- | The date and time this activity type was created through
-- RegisterActivityType.
atiCreationDate :: Lens' ActivityTypeInfo (POSIX)
atiCreationDate f x =
    f (_atiCreationDate x)
        <&> \y -> x { _atiCreationDate = y }
{-# INLINE atiCreationDate #-}

-- | If DEPRECATED, the date and time DeprecateActivityType was called.
atiDeprecationDate :: Lens' ActivityTypeInfo (Maybe POSIX)
atiDeprecationDate f x =
    f (_atiDeprecationDate x)
        <&> \y -> x { _atiDeprecationDate = y }
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
ctfeaTimerId f x =
    f (_ctfeaTimerId x)
        <&> \y -> x { _ctfeaTimerId = y }
{-# INLINE ctfeaTimerId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
ctfeaCause :: Lens' CancelTimerFailedEventAttributes (CancelTimerFailedCause)
ctfeaCause f x =
    f (_ctfeaCause x)
        <&> \y -> x { _ctfeaCause = y }
{-# INLINE ctfeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CancelTimer decision to cancel this timer. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
ctfeaDecisionTaskCompletedEventId :: Lens' CancelTimerFailedEventAttributes (Integer)
ctfeaDecisionTaskCompletedEventId f x =
    f (_ctfeaDecisionTaskCompletedEventId x)
        <&> \y -> x { _ctfeaDecisionTaskCompletedEventId = y }
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
cwefebCause :: Lens' CancelWorkflowExecutionFailedEventAttributes (CancelWorkflowExecutionFailedCause)
cwefebCause f x =
    f (_cwefebCause x)
        <&> \y -> x { _cwefebCause = y }
{-# INLINE cwefebCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CancelWorkflowExecution decision for this
-- cancellation request. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
cwefebDecisionTaskCompletedEventId :: Lens' CancelWorkflowExecutionFailedEventAttributes (Integer)
cwefebDecisionTaskCompletedEventId f x =
    f (_cwefebDecisionTaskCompletedEventId x)
        <&> \y -> x { _cwefebDecisionTaskCompletedEventId = y }
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
cwecebWorkflowExecution :: Lens' ChildWorkflowExecutionCanceledEventAttributes (WorkflowExecution)
cwecebWorkflowExecution f x =
    f (_cwecebWorkflowExecution x)
        <&> \y -> x { _cwecebWorkflowExecution = y }
{-# INLINE cwecebWorkflowExecution #-}

-- | The type of the child workflow execution.
cwecebWorkflowType :: Lens' ChildWorkflowExecutionCanceledEventAttributes (WorkflowType)
cwecebWorkflowType f x =
    f (_cwecebWorkflowType x)
        <&> \y -> x { _cwecebWorkflowType = y }
{-# INLINE cwecebWorkflowType #-}

-- | Details of the cancellation (if provided).
cwecebDetails :: Lens' ChildWorkflowExecutionCanceledEventAttributes (Maybe Text)
cwecebDetails f x =
    f (_cwecebDetails x)
        <&> \y -> x { _cwecebDetails = y }
{-# INLINE cwecebDetails #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cwecebInitiatedEventId :: Lens' ChildWorkflowExecutionCanceledEventAttributes (Integer)
cwecebInitiatedEventId f x =
    f (_cwecebInitiatedEventId x)
        <&> \y -> x { _cwecebInitiatedEventId = y }
{-# INLINE cwecebInitiatedEventId #-}

-- | The Id of the ChildWorkflowExecutionStarted event recorded when this child
-- workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
cwecebStartedEventId :: Lens' ChildWorkflowExecutionCanceledEventAttributes (Integer)
cwecebStartedEventId f x =
    f (_cwecebStartedEventId x)
        <&> \y -> x { _cwecebStartedEventId = y }
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
cweceaWorkflowExecution :: Lens' ChildWorkflowExecutionCompletedEventAttributes (WorkflowExecution)
cweceaWorkflowExecution f x =
    f (_cweceaWorkflowExecution x)
        <&> \y -> x { _cweceaWorkflowExecution = y }
{-# INLINE cweceaWorkflowExecution #-}

-- | The type of the child workflow execution.
cweceaWorkflowType :: Lens' ChildWorkflowExecutionCompletedEventAttributes (WorkflowType)
cweceaWorkflowType f x =
    f (_cweceaWorkflowType x)
        <&> \y -> x { _cweceaWorkflowType = y }
{-# INLINE cweceaWorkflowType #-}

-- | The result of the child workflow execution (if any).
cweceaResult :: Lens' ChildWorkflowExecutionCompletedEventAttributes (Maybe Text)
cweceaResult f x =
    f (_cweceaResult x)
        <&> \y -> x { _cweceaResult = y }
{-# INLINE cweceaResult #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cweceaInitiatedEventId :: Lens' ChildWorkflowExecutionCompletedEventAttributes (Integer)
cweceaInitiatedEventId f x =
    f (_cweceaInitiatedEventId x)
        <&> \y -> x { _cweceaInitiatedEventId = y }
{-# INLINE cweceaInitiatedEventId #-}

-- | The Id of the ChildWorkflowExecutionStarted event recorded when this child
-- workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
cweceaStartedEventId :: Lens' ChildWorkflowExecutionCompletedEventAttributes (Integer)
cweceaStartedEventId f x =
    f (_cweceaStartedEventId x)
        <&> \y -> x { _cweceaStartedEventId = y }
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
cwefecWorkflowExecution :: Lens' ChildWorkflowExecutionFailedEventAttributes (WorkflowExecution)
cwefecWorkflowExecution f x =
    f (_cwefecWorkflowExecution x)
        <&> \y -> x { _cwefecWorkflowExecution = y }
{-# INLINE cwefecWorkflowExecution #-}

-- | The type of the child workflow execution.
cwefecWorkflowType :: Lens' ChildWorkflowExecutionFailedEventAttributes (WorkflowType)
cwefecWorkflowType f x =
    f (_cwefecWorkflowType x)
        <&> \y -> x { _cwefecWorkflowType = y }
{-# INLINE cwefecWorkflowType #-}

-- | The reason for the failure (if provided).
cwefecReason :: Lens' ChildWorkflowExecutionFailedEventAttributes (Maybe Text)
cwefecReason f x =
    f (_cwefecReason x)
        <&> \y -> x { _cwefecReason = y }
{-# INLINE cwefecReason #-}

-- | The details of the failure (if provided).
cwefecDetails :: Lens' ChildWorkflowExecutionFailedEventAttributes (Maybe Text)
cwefecDetails f x =
    f (_cwefecDetails x)
        <&> \y -> x { _cwefecDetails = y }
{-# INLINE cwefecDetails #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cwefecInitiatedEventId :: Lens' ChildWorkflowExecutionFailedEventAttributes (Integer)
cwefecInitiatedEventId f x =
    f (_cwefecInitiatedEventId x)
        <&> \y -> x { _cwefecInitiatedEventId = y }
{-# INLINE cwefecInitiatedEventId #-}

-- | The Id of the ChildWorkflowExecutionStarted event recorded when this child
-- workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
cwefecStartedEventId :: Lens' ChildWorkflowExecutionFailedEventAttributes (Integer)
cwefecStartedEventId f x =
    f (_cwefecStartedEventId x)
        <&> \y -> x { _cwefecStartedEventId = y }
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
cweseaWorkflowExecution :: Lens' ChildWorkflowExecutionStartedEventAttributes (WorkflowExecution)
cweseaWorkflowExecution f x =
    f (_cweseaWorkflowExecution x)
        <&> \y -> x { _cweseaWorkflowExecution = y }
{-# INLINE cweseaWorkflowExecution #-}

-- | The type of the child workflow execution.
cweseaWorkflowType :: Lens' ChildWorkflowExecutionStartedEventAttributes (WorkflowType)
cweseaWorkflowType f x =
    f (_cweseaWorkflowType x)
        <&> \y -> x { _cweseaWorkflowType = y }
{-# INLINE cweseaWorkflowType #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cweseaInitiatedEventId :: Lens' ChildWorkflowExecutionStartedEventAttributes (Integer)
cweseaInitiatedEventId f x =
    f (_cweseaInitiatedEventId x)
        <&> \y -> x { _cweseaInitiatedEventId = y }
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
cweteaWorkflowExecution :: Lens' ChildWorkflowExecutionTerminatedEventAttributes (WorkflowExecution)
cweteaWorkflowExecution f x =
    f (_cweteaWorkflowExecution x)
        <&> \y -> x { _cweteaWorkflowExecution = y }
{-# INLINE cweteaWorkflowExecution #-}

-- | The type of the child workflow execution.
cweteaWorkflowType :: Lens' ChildWorkflowExecutionTerminatedEventAttributes (WorkflowType)
cweteaWorkflowType f x =
    f (_cweteaWorkflowType x)
        <&> \y -> x { _cweteaWorkflowType = y }
{-# INLINE cweteaWorkflowType #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cweteaInitiatedEventId :: Lens' ChildWorkflowExecutionTerminatedEventAttributes (Integer)
cweteaInitiatedEventId f x =
    f (_cweteaInitiatedEventId x)
        <&> \y -> x { _cweteaInitiatedEventId = y }
{-# INLINE cweteaInitiatedEventId #-}

-- | The Id of the ChildWorkflowExecutionStarted event recorded when this child
-- workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
cweteaStartedEventId :: Lens' ChildWorkflowExecutionTerminatedEventAttributes (Integer)
cweteaStartedEventId f x =
    f (_cweteaStartedEventId x)
        <&> \y -> x { _cweteaStartedEventId = y }
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
cwetoeaWorkflowExecution :: Lens' ChildWorkflowExecutionTimedOutEventAttributes (WorkflowExecution)
cwetoeaWorkflowExecution f x =
    f (_cwetoeaWorkflowExecution x)
        <&> \y -> x { _cwetoeaWorkflowExecution = y }
{-# INLINE cwetoeaWorkflowExecution #-}

-- | The type of the child workflow execution.
cwetoeaWorkflowType :: Lens' ChildWorkflowExecutionTimedOutEventAttributes (WorkflowType)
cwetoeaWorkflowType f x =
    f (_cwetoeaWorkflowType x)
        <&> \y -> x { _cwetoeaWorkflowType = y }
{-# INLINE cwetoeaWorkflowType #-}

-- | The type of the timeout that caused the child workflow execution to time
-- out.
cwetoeaTimeoutType :: Lens' ChildWorkflowExecutionTimedOutEventAttributes (WorkflowExecutionTimeoutType)
cwetoeaTimeoutType f x =
    f (_cwetoeaTimeoutType x)
        <&> \y -> x { _cwetoeaTimeoutType = y }
{-# INLINE cwetoeaTimeoutType #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
cwetoeaInitiatedEventId :: Lens' ChildWorkflowExecutionTimedOutEventAttributes (Integer)
cwetoeaInitiatedEventId f x =
    f (_cwetoeaInitiatedEventId x)
        <&> \y -> x { _cwetoeaInitiatedEventId = y }
{-# INLINE cwetoeaInitiatedEventId #-}

-- | The Id of the ChildWorkflowExecutionStarted event recorded when this child
-- workflow execution was started. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
cwetoeaStartedEventId :: Lens' ChildWorkflowExecutionTimedOutEventAttributes (Integer)
cwetoeaStartedEventId f x =
    f (_cwetoeaStartedEventId x)
        <&> \y -> x { _cwetoeaStartedEventId = y }
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
cwefeaCause :: Lens' CompleteWorkflowExecutionFailedEventAttributes (CompleteWorkflowExecutionFailedCause)
cwefeaCause f x =
    f (_cwefeaCause x)
        <&> \y -> x { _cwefeaCause = y }
{-# INLINE cwefeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CompleteWorkflowExecution decision to complete
-- this execution. This information can be useful for diagnosing problems by
-- tracing back the cause of events.
cwefeaDecisionTaskCompletedEventId :: Lens' CompleteWorkflowExecutionFailedEventAttributes (Integer)
cwefeaDecisionTaskCompletedEventId f x =
    f (_cwefeaDecisionTaskCompletedEventId x)
        <&> \y -> x { _cwefeaDecisionTaskCompletedEventId = y }
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
canwedaInput :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaInput f x =
    f (_canwedaInput x)
        <&> \y -> x { _canwedaInput = y }
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
canwedaExecutionStartToCloseTimeout f x =
    f (_canwedaExecutionStartToCloseTimeout x)
        <&> \y -> x { _canwedaExecutionStartToCloseTimeout = y }
{-# INLINE canwedaExecutionStartToCloseTimeout #-}

-- | Represents a task list.
canwedaTaskList :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe TaskList)
canwedaTaskList f x =
    f (_canwedaTaskList x)
        <&> \y -> x { _canwedaTaskList = y }
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
canwedaTaskStartToCloseTimeout f x =
    f (_canwedaTaskStartToCloseTimeout x)
        <&> \y -> x { _canwedaTaskStartToCloseTimeout = y }
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
canwedaChildPolicy f x =
    f (_canwedaChildPolicy x)
        <&> \y -> x { _canwedaChildPolicy = y }
{-# INLINE canwedaChildPolicy #-}

-- | The list of tags to associate with the new workflow execution. A maximum of
-- 5 tags can be specified. You can list workflow executions with a specific
-- tag by calling ListOpenWorkflowExecutions or ListClosedWorkflowExecutions
-- and specifying a TagFilter.
canwedaTagList :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes ([Text])
canwedaTagList f x =
    f (_canwedaTagList x)
        <&> \y -> x { _canwedaTagList = y }
{-# INLINE canwedaTagList #-}

canwedaWorkflowTypeVersion :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaWorkflowTypeVersion f x =
    f (_canwedaWorkflowTypeVersion x)
        <&> \y -> x { _canwedaWorkflowTypeVersion = y }
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
canwefeaCause :: Lens' ContinueAsNewWorkflowExecutionFailedEventAttributes (ContinueAsNewWorkflowExecutionFailedCause)
canwefeaCause f x =
    f (_canwefeaCause x)
        <&> \y -> x { _canwefeaCause = y }
{-# INLINE canwefeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the ContinueAsNewWorkflowExecution decision that
-- started this execution. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
canwefeaDecisionTaskCompletedEventId :: Lens' ContinueAsNewWorkflowExecutionFailedEventAttributes (Integer)
canwefeaDecisionTaskCompletedEventId f x =
    f (_canwefeaDecisionTaskCompletedEventId x)
        <&> \y -> x { _canwefeaDecisionTaskCompletedEventId = y }
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
ddddrDecisionType :: Lens' Decision (DecisionType)
ddddrDecisionType f x =
    f (_ddddrDecisionType x)
        <&> \y -> x { _ddddrDecisionType = y }
{-# INLINE ddddrDecisionType #-}

-- | Provides details of the ScheduleActivityTask decision. It is not set for
-- other decision types.
ddddrScheduleActivityTaskDecisionAttributes :: Lens' Decision (Maybe ScheduleActivityTaskDecisionAttributes)
ddddrScheduleActivityTaskDecisionAttributes f x =
    f (_ddddrScheduleActivityTaskDecisionAttributes x)
        <&> \y -> x { _ddddrScheduleActivityTaskDecisionAttributes = y }
{-# INLINE ddddrScheduleActivityTaskDecisionAttributes #-}

-- | Provides details of the RequestCancelActivityTask decision. It is not set
-- for other decision types.
ddddrRequestCancelActivityTaskDecisionAttributes :: Lens' Decision (Maybe RequestCancelActivityTaskDecisionAttributes)
ddddrRequestCancelActivityTaskDecisionAttributes f x =
    f (_ddddrRequestCancelActivityTaskDecisionAttributes x)
        <&> \y -> x { _ddddrRequestCancelActivityTaskDecisionAttributes = y }
{-# INLINE ddddrRequestCancelActivityTaskDecisionAttributes #-}

-- | Provides details of the CompleteWorkflowExecution decision. It is not set
-- for other decision types.
ddddrCompleteWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe CompleteWorkflowExecutionDecisionAttributes)
ddddrCompleteWorkflowExecutionDecisionAttributes f x =
    f (_ddddrCompleteWorkflowExecutionDecisionAttributes x)
        <&> \y -> x { _ddddrCompleteWorkflowExecutionDecisionAttributes = y }
{-# INLINE ddddrCompleteWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the FailWorkflowExecution decision. It is not set for
-- other decision types.
ddddrFailWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe FailWorkflowExecutionDecisionAttributes)
ddddrFailWorkflowExecutionDecisionAttributes f x =
    f (_ddddrFailWorkflowExecutionDecisionAttributes x)
        <&> \y -> x { _ddddrFailWorkflowExecutionDecisionAttributes = y }
{-# INLINE ddddrFailWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the CancelWorkflowExecution decision. It is not set for
-- other decision types.
ddddrCancelWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe CancelWorkflowExecutionDecisionAttributes)
ddddrCancelWorkflowExecutionDecisionAttributes f x =
    f (_ddddrCancelWorkflowExecutionDecisionAttributes x)
        <&> \y -> x { _ddddrCancelWorkflowExecutionDecisionAttributes = y }
{-# INLINE ddddrCancelWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the ContinueAsNewWorkflowExecution decision. It is not
-- set for other decision types.
ddddrContinueAsNewWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe ContinueAsNewWorkflowExecutionDecisionAttributes)
ddddrContinueAsNewWorkflowExecutionDecisionAttributes f x =
    f (_ddddrContinueAsNewWorkflowExecutionDecisionAttributes x)
        <&> \y -> x { _ddddrContinueAsNewWorkflowExecutionDecisionAttributes = y }
{-# INLINE ddddrContinueAsNewWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the RecordMarker decision. It is not set for other
-- decision types.
ddddrRecordMarkerDecisionAttributes :: Lens' Decision (Maybe RecordMarkerDecisionAttributes)
ddddrRecordMarkerDecisionAttributes f x =
    f (_ddddrRecordMarkerDecisionAttributes x)
        <&> \y -> x { _ddddrRecordMarkerDecisionAttributes = y }
{-# INLINE ddddrRecordMarkerDecisionAttributes #-}

-- | Provides details of the StartTimer decision. It is not set for other
-- decision types.
ddddrStartTimerDecisionAttributes :: Lens' Decision (Maybe StartTimerDecisionAttributes)
ddddrStartTimerDecisionAttributes f x =
    f (_ddddrStartTimerDecisionAttributes x)
        <&> \y -> x { _ddddrStartTimerDecisionAttributes = y }
{-# INLINE ddddrStartTimerDecisionAttributes #-}

-- | Provides details of the CancelTimer decision. It is not set for other
-- decision types.
ddddrCancelTimerDecisionAttributes :: Lens' Decision (Maybe CancelTimerDecisionAttributes)
ddddrCancelTimerDecisionAttributes f x =
    f (_ddddrCancelTimerDecisionAttributes x)
        <&> \y -> x { _ddddrCancelTimerDecisionAttributes = y }
{-# INLINE ddddrCancelTimerDecisionAttributes #-}

-- | Provides details of the SignalExternalWorkflowExecution decision. It is not
-- set for other decision types.
ddddrSignalExternalWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe SignalExternalWorkflowExecutionDecisionAttributes)
ddddrSignalExternalWorkflowExecutionDecisionAttributes f x =
    f (_ddddrSignalExternalWorkflowExecutionDecisionAttributes x)
        <&> \y -> x { _ddddrSignalExternalWorkflowExecutionDecisionAttributes = y }
{-# INLINE ddddrSignalExternalWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the RequestCancelExternalWorkflowExecution decision. It
-- is not set for other decision types.
ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes)
ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes f x =
    f (_ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes x)
        <&> \y -> x { _ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes = y }
{-# INLINE ddddrRequestCancelExternalWorkflowExecutionDecisionAttributes #-}

-- | Provides details of the StartChildWorkflowExecution decision. It is not set
-- for other decision types.
ddddrStartChildWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe StartChildWorkflowExecutionDecisionAttributes)
ddddrStartChildWorkflowExecutionDecisionAttributes f x =
    f (_ddddrStartChildWorkflowExecutionDecisionAttributes x)
        <&> \y -> x { _ddddrStartChildWorkflowExecutionDecisionAttributes = y }
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
dtceaExecutionContext :: Lens' DecisionTaskCompletedEventAttributes (Maybe Text)
dtceaExecutionContext f x =
    f (_dtceaExecutionContext x)
        <&> \y -> x { _dtceaExecutionContext = y }
{-# INLINE dtceaExecutionContext #-}

-- | The id of the DecisionTaskScheduled event that was recorded when this
-- decision task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
dtceaScheduledEventId :: Lens' DecisionTaskCompletedEventAttributes (Integer)
dtceaScheduledEventId f x =
    f (_dtceaScheduledEventId x)
        <&> \y -> x { _dtceaScheduledEventId = y }
{-# INLINE dtceaScheduledEventId #-}

-- | The Id of the DecisionTaskStarted event recorded when this decision task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
dtceaStartedEventId :: Lens' DecisionTaskCompletedEventAttributes (Integer)
dtceaStartedEventId f x =
    f (_dtceaStartedEventId x)
        <&> \y -> x { _dtceaStartedEventId = y }
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
dtseaTaskList :: Lens' DecisionTaskScheduledEventAttributes (TaskList)
dtseaTaskList f x =
    f (_dtseaTaskList x)
        <&> \y -> x { _dtseaTaskList = y }
{-# INLINE dtseaTaskList #-}

-- | The maximum duration for this decision task. The task is considered timed
-- out if it does not completed within this duration. The valid values are
-- integers greater than or equal to 0. An integer value can be used to
-- specify the duration in seconds while NONE can be used to specify unlimited
-- duration.
dtseaStartToCloseTimeout :: Lens' DecisionTaskScheduledEventAttributes (Maybe Text)
dtseaStartToCloseTimeout f x =
    f (_dtseaStartToCloseTimeout x)
        <&> \y -> x { _dtseaStartToCloseTimeout = y }
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
dtsebIdentity :: Lens' DecisionTaskStartedEventAttributes (Maybe Text)
dtsebIdentity f x =
    f (_dtsebIdentity x)
        <&> \y -> x { _dtsebIdentity = y }
{-# INLINE dtsebIdentity #-}

-- | The id of the DecisionTaskScheduled event that was recorded when this
-- decision task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
dtsebScheduledEventId :: Lens' DecisionTaskStartedEventAttributes (Integer)
dtsebScheduledEventId f x =
    f (_dtsebScheduledEventId x)
        <&> \y -> x { _dtsebScheduledEventId = y }
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
dttoeaTimeoutType :: Lens' DecisionTaskTimedOutEventAttributes (DecisionTaskTimeoutType)
dttoeaTimeoutType f x =
    f (_dttoeaTimeoutType x)
        <&> \y -> x { _dttoeaTimeoutType = y }
{-# INLINE dttoeaTimeoutType #-}

-- | The id of the DecisionTaskScheduled event that was recorded when this
-- decision task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
dttoeaScheduledEventId :: Lens' DecisionTaskTimedOutEventAttributes (Integer)
dttoeaScheduledEventId f x =
    f (_dttoeaScheduledEventId x)
        <&> \y -> x { _dttoeaScheduledEventId = y }
{-# INLINE dttoeaScheduledEventId #-}

-- | The Id of the DecisionTaskStarted event recorded when this decision task
-- was started. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
dttoeaStartedEventId :: Lens' DecisionTaskTimedOutEventAttributes (Integer)
dttoeaStartedEventId f x =
    f (_dttoeaStartedEventId x)
        <&> \y -> x { _dttoeaStartedEventId = y }
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
diName :: Lens' DomainInfo (Text)
diName f x =
    f (_diName x)
        <&> \y -> x { _diName = y }
{-# INLINE diName #-}

-- | The status of the domain: REGISTERED: The domain is properly registered and
-- available. You can use this domain for registering types and creating new
-- workflow executions. DEPRECATED: The domain was deprecated using
-- DeprecateDomain, but is still in use. You should not create new workflow
-- executions in this domain.
diStatus :: Lens' DomainInfo (RegistrationStatus)
diStatus f x =
    f (_diStatus x)
        <&> \y -> x { _diStatus = y }
{-# INLINE diStatus #-}

-- | The description of the domain provided through RegisterDomain.
diDescription :: Lens' DomainInfo (Maybe Text)
diDescription f x =
    f (_diDescription x)
        <&> \y -> x { _diDescription = y }
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
etfOldestDate f x =
    f (_etfOldestDate x)
        <&> \y -> x { _etfOldestDate = y }
{-# INLINE etfOldestDate #-}

-- | Specifies the latest start or close date and time to return.
etfLatestDate :: Lens' ExecutionTimeFilter (Maybe POSIX)
etfLatestDate f x =
    f (_etfLatestDate x)
        <&> \y -> x { _etfLatestDate = y }
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
ewecreaWorkflowExecution :: Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes (WorkflowExecution)
ewecreaWorkflowExecution f x =
    f (_ewecreaWorkflowExecution x)
        <&> \y -> x { _ewecreaWorkflowExecution = y }
{-# INLINE ewecreaWorkflowExecution #-}

-- | The id of the RequestCancelExternalWorkflowExecutionInitiated event
-- corresponding to the RequestCancelExternalWorkflowExecution decision to
-- cancel this external workflow execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
ewecreaInitiatedEventId :: Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes (Integer)
ewecreaInitiatedEventId f x =
    f (_ewecreaInitiatedEventId x)
        <&> \y -> x { _ewecreaInitiatedEventId = y }
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
eweseaWorkflowExecution :: Lens' ExternalWorkflowExecutionSignaledEventAttributes (WorkflowExecution)
eweseaWorkflowExecution f x =
    f (_eweseaWorkflowExecution x)
        <&> \y -> x { _eweseaWorkflowExecution = y }
{-# INLINE eweseaWorkflowExecution #-}

-- | The id of the SignalExternalWorkflowExecutionInitiated event corresponding
-- to the SignalExternalWorkflowExecution decision to request this signal.
-- This information can be useful for diagnosing problems by tracing back the
-- chain of events leading up to this event.
eweseaInitiatedEventId :: Lens' ExternalWorkflowExecutionSignaledEventAttributes (Integer)
eweseaInitiatedEventId f x =
    f (_eweseaInitiatedEventId x)
        <&> \y -> x { _eweseaInitiatedEventId = y }
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
fwedaReason :: Lens' FailWorkflowExecutionDecisionAttributes (Maybe Text)
fwedaReason f x =
    f (_fwedaReason x)
        <&> \y -> x { _fwedaReason = y }
{-# INLINE fwedaReason #-}

-- | Optional details of the failure.
fwedaDetails :: Lens' FailWorkflowExecutionDecisionAttributes (Maybe Text)
fwedaDetails f x =
    f (_fwedaDetails x)
        <&> \y -> x { _fwedaDetails = y }
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
fwefeaCause :: Lens' FailWorkflowExecutionFailedEventAttributes (FailWorkflowExecutionFailedCause)
fwefeaCause f x =
    f (_fwefeaCause x)
        <&> \y -> x { _fwefeaCause = y }
{-# INLINE fwefeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the FailWorkflowExecution decision to fail this
-- execution. This information can be useful for diagnosing problems by
-- tracing back the cause of events.
fwefeaDecisionTaskCompletedEventId :: Lens' FailWorkflowExecutionFailedEventAttributes (Integer)
fwefeaDecisionTaskCompletedEventId f x =
    f (_fwefeaDecisionTaskCompletedEventId x)
        <&> \y -> x { _fwefeaDecisionTaskCompletedEventId = y }
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
heEventTimestamp :: Lens' HistoryEvent (POSIX)
heEventTimestamp f x =
    f (_heEventTimestamp x)
        <&> \y -> x { _heEventTimestamp = y }
{-# INLINE heEventTimestamp #-}

-- | The type of the history event.
heEventType :: Lens' HistoryEvent (EventType)
heEventType f x =
    f (_heEventType x)
        <&> \y -> x { _heEventType = y }
{-# INLINE heEventType #-}

-- | The system generated id of the event. This id uniquely identifies the event
-- with in the workflow execution history.
heEventId :: Lens' HistoryEvent (Integer)
heEventId f x =
    f (_heEventId x)
        <&> \y -> x { _heEventId = y }
{-# INLINE heEventId #-}

-- | If the event is of type WorkflowExecutionStarted then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionStartedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionStartedEventAttributes)
heWorkflowExecutionStartedEventAttributes f x =
    f (_heWorkflowExecutionStartedEventAttributes x)
        <&> \y -> x { _heWorkflowExecutionStartedEventAttributes = y }
{-# INLINE heWorkflowExecutionStartedEventAttributes #-}

-- | If the event is of type WorkflowExecutionCompleted then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionCompletedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionCompletedEventAttributes)
heWorkflowExecutionCompletedEventAttributes f x =
    f (_heWorkflowExecutionCompletedEventAttributes x)
        <&> \y -> x { _heWorkflowExecutionCompletedEventAttributes = y }
{-# INLINE heWorkflowExecutionCompletedEventAttributes #-}

-- | If the event is of type CompleteWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heCompleteWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe CompleteWorkflowExecutionFailedEventAttributes)
heCompleteWorkflowExecutionFailedEventAttributes f x =
    f (_heCompleteWorkflowExecutionFailedEventAttributes x)
        <&> \y -> x { _heCompleteWorkflowExecutionFailedEventAttributes = y }
{-# INLINE heCompleteWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type WorkflowExecutionFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionFailedEventAttributes)
heWorkflowExecutionFailedEventAttributes f x =
    f (_heWorkflowExecutionFailedEventAttributes x)
        <&> \y -> x { _heWorkflowExecutionFailedEventAttributes = y }
{-# INLINE heWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type FailWorkflowExecutionFailed then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heFailWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe FailWorkflowExecutionFailedEventAttributes)
heFailWorkflowExecutionFailedEventAttributes f x =
    f (_heFailWorkflowExecutionFailedEventAttributes x)
        <&> \y -> x { _heFailWorkflowExecutionFailedEventAttributes = y }
{-# INLINE heFailWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type WorkflowExecutionTimedOut then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionTimedOutEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionTimedOutEventAttributes)
heWorkflowExecutionTimedOutEventAttributes f x =
    f (_heWorkflowExecutionTimedOutEventAttributes x)
        <&> \y -> x { _heWorkflowExecutionTimedOutEventAttributes = y }
{-# INLINE heWorkflowExecutionTimedOutEventAttributes #-}

-- | If the event is of type WorkflowExecutionCanceled then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionCanceledEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionCanceledEventAttributes)
heWorkflowExecutionCanceledEventAttributes f x =
    f (_heWorkflowExecutionCanceledEventAttributes x)
        <&> \y -> x { _heWorkflowExecutionCanceledEventAttributes = y }
{-# INLINE heWorkflowExecutionCanceledEventAttributes #-}

-- | If the event is of type CancelWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heCancelWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe CancelWorkflowExecutionFailedEventAttributes)
heCancelWorkflowExecutionFailedEventAttributes f x =
    f (_heCancelWorkflowExecutionFailedEventAttributes x)
        <&> \y -> x { _heCancelWorkflowExecutionFailedEventAttributes = y }
{-# INLINE heCancelWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type WorkflowExecutionContinuedAsNew then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heWorkflowExecutionContinuedAsNewEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionContinuedAsNewEventAttributes)
heWorkflowExecutionContinuedAsNewEventAttributes f x =
    f (_heWorkflowExecutionContinuedAsNewEventAttributes x)
        <&> \y -> x { _heWorkflowExecutionContinuedAsNewEventAttributes = y }
{-# INLINE heWorkflowExecutionContinuedAsNewEventAttributes #-}

-- | If the event is of type ContinueAsNewWorkflowExecutionFailed then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heContinueAsNewWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes)
heContinueAsNewWorkflowExecutionFailedEventAttributes f x =
    f (_heContinueAsNewWorkflowExecutionFailedEventAttributes x)
        <&> \y -> x { _heContinueAsNewWorkflowExecutionFailedEventAttributes = y }
{-# INLINE heContinueAsNewWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type WorkflowExecutionTerminated then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionTerminatedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionTerminatedEventAttributes)
heWorkflowExecutionTerminatedEventAttributes f x =
    f (_heWorkflowExecutionTerminatedEventAttributes x)
        <&> \y -> x { _heWorkflowExecutionTerminatedEventAttributes = y }
{-# INLINE heWorkflowExecutionTerminatedEventAttributes #-}

-- | If the event is of type WorkflowExecutionCancelRequested then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
heWorkflowExecutionCancelRequestedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionCancelRequestedEventAttributes)
heWorkflowExecutionCancelRequestedEventAttributes f x =
    f (_heWorkflowExecutionCancelRequestedEventAttributes x)
        <&> \y -> x { _heWorkflowExecutionCancelRequestedEventAttributes = y }
{-# INLINE heWorkflowExecutionCancelRequestedEventAttributes #-}

-- | If the event is of type DecisionTaskScheduled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heDecisionTaskScheduledEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskScheduledEventAttributes)
heDecisionTaskScheduledEventAttributes f x =
    f (_heDecisionTaskScheduledEventAttributes x)
        <&> \y -> x { _heDecisionTaskScheduledEventAttributes = y }
{-# INLINE heDecisionTaskScheduledEventAttributes #-}

-- | If the event is of type DecisionTaskStarted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heDecisionTaskStartedEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskStartedEventAttributes)
heDecisionTaskStartedEventAttributes f x =
    f (_heDecisionTaskStartedEventAttributes x)
        <&> \y -> x { _heDecisionTaskStartedEventAttributes = y }
{-# INLINE heDecisionTaskStartedEventAttributes #-}

-- | If the event is of type DecisionTaskCompleted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heDecisionTaskCompletedEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskCompletedEventAttributes)
heDecisionTaskCompletedEventAttributes f x =
    f (_heDecisionTaskCompletedEventAttributes x)
        <&> \y -> x { _heDecisionTaskCompletedEventAttributes = y }
{-# INLINE heDecisionTaskCompletedEventAttributes #-}

-- | If the event is of type DecisionTaskTimedOut then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heDecisionTaskTimedOutEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskTimedOutEventAttributes)
heDecisionTaskTimedOutEventAttributes f x =
    f (_heDecisionTaskTimedOutEventAttributes x)
        <&> \y -> x { _heDecisionTaskTimedOutEventAttributes = y }
{-# INLINE heDecisionTaskTimedOutEventAttributes #-}

-- | If the event is of type ActivityTaskScheduled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskScheduledEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskScheduledEventAttributes)
heActivityTaskScheduledEventAttributes f x =
    f (_heActivityTaskScheduledEventAttributes x)
        <&> \y -> x { _heActivityTaskScheduledEventAttributes = y }
{-# INLINE heActivityTaskScheduledEventAttributes #-}

-- | If the event is of type ActivityTaskStarted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskStartedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskStartedEventAttributes)
heActivityTaskStartedEventAttributes f x =
    f (_heActivityTaskStartedEventAttributes x)
        <&> \y -> x { _heActivityTaskStartedEventAttributes = y }
{-# INLINE heActivityTaskStartedEventAttributes #-}

-- | If the event is of type ActivityTaskCompleted then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskCompletedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskCompletedEventAttributes)
heActivityTaskCompletedEventAttributes f x =
    f (_heActivityTaskCompletedEventAttributes x)
        <&> \y -> x { _heActivityTaskCompletedEventAttributes = y }
{-# INLINE heActivityTaskCompletedEventAttributes #-}

-- | If the event is of type ActivityTaskFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskFailedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskFailedEventAttributes)
heActivityTaskFailedEventAttributes f x =
    f (_heActivityTaskFailedEventAttributes x)
        <&> \y -> x { _heActivityTaskFailedEventAttributes = y }
{-# INLINE heActivityTaskFailedEventAttributes #-}

-- | If the event is of type ActivityTaskTimedOut then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskTimedOutEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskTimedOutEventAttributes)
heActivityTaskTimedOutEventAttributes f x =
    f (_heActivityTaskTimedOutEventAttributes x)
        <&> \y -> x { _heActivityTaskTimedOutEventAttributes = y }
{-# INLINE heActivityTaskTimedOutEventAttributes #-}

-- | If the event is of type ActivityTaskCanceled then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskCanceledEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskCanceledEventAttributes)
heActivityTaskCanceledEventAttributes f x =
    f (_heActivityTaskCanceledEventAttributes x)
        <&> \y -> x { _heActivityTaskCanceledEventAttributes = y }
{-# INLINE heActivityTaskCanceledEventAttributes #-}

-- | If the event is of type ActivityTaskcancelRequested then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskCancelRequestedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskCancelRequestedEventAttributes)
heActivityTaskCancelRequestedEventAttributes f x =
    f (_heActivityTaskCancelRequestedEventAttributes x)
        <&> \y -> x { _heActivityTaskCancelRequestedEventAttributes = y }
{-# INLINE heActivityTaskCancelRequestedEventAttributes #-}

-- | If the event is of type WorkflowExecutionSignaled then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionSignaledEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionSignaledEventAttributes)
heWorkflowExecutionSignaledEventAttributes f x =
    f (_heWorkflowExecutionSignaledEventAttributes x)
        <&> \y -> x { _heWorkflowExecutionSignaledEventAttributes = y }
{-# INLINE heWorkflowExecutionSignaledEventAttributes #-}

-- | If the event is of type MarkerRecorded then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heMarkerRecordedEventAttributes :: Lens' HistoryEvent (Maybe MarkerRecordedEventAttributes)
heMarkerRecordedEventAttributes f x =
    f (_heMarkerRecordedEventAttributes x)
        <&> \y -> x { _heMarkerRecordedEventAttributes = y }
{-# INLINE heMarkerRecordedEventAttributes #-}

-- | If the event is of type DecisionTaskFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heRecordMarkerFailedEventAttributes :: Lens' HistoryEvent (Maybe RecordMarkerFailedEventAttributes)
heRecordMarkerFailedEventAttributes f x =
    f (_heRecordMarkerFailedEventAttributes x)
        <&> \y -> x { _heRecordMarkerFailedEventAttributes = y }
{-# INLINE heRecordMarkerFailedEventAttributes #-}

-- | If the event is of type TimerStarted then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heTimerStartedEventAttributes :: Lens' HistoryEvent (Maybe TimerStartedEventAttributes)
heTimerStartedEventAttributes f x =
    f (_heTimerStartedEventAttributes x)
        <&> \y -> x { _heTimerStartedEventAttributes = y }
{-# INLINE heTimerStartedEventAttributes #-}

-- | If the event is of type TimerFired then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heTimerFiredEventAttributes :: Lens' HistoryEvent (Maybe TimerFiredEventAttributes)
heTimerFiredEventAttributes f x =
    f (_heTimerFiredEventAttributes x)
        <&> \y -> x { _heTimerFiredEventAttributes = y }
{-# INLINE heTimerFiredEventAttributes #-}

-- | If the event is of type TimerCanceled then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heTimerCanceledEventAttributes :: Lens' HistoryEvent (Maybe TimerCanceledEventAttributes)
heTimerCanceledEventAttributes f x =
    f (_heTimerCanceledEventAttributes x)
        <&> \y -> x { _heTimerCanceledEventAttributes = y }
{-# INLINE heTimerCanceledEventAttributes #-}

-- | If the event is of type StartChildWorkflowExecutionInitiated then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heStartChildWorkflowExecutionInitiatedEventAttributes :: Lens' HistoryEvent (Maybe StartChildWorkflowExecutionInitiatedEventAttributes)
heStartChildWorkflowExecutionInitiatedEventAttributes f x =
    f (_heStartChildWorkflowExecutionInitiatedEventAttributes x)
        <&> \y -> x { _heStartChildWorkflowExecutionInitiatedEventAttributes = y }
{-# INLINE heStartChildWorkflowExecutionInitiatedEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionStarted then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionStartedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionStartedEventAttributes)
heChildWorkflowExecutionStartedEventAttributes f x =
    f (_heChildWorkflowExecutionStartedEventAttributes x)
        <&> \y -> x { _heChildWorkflowExecutionStartedEventAttributes = y }
{-# INLINE heChildWorkflowExecutionStartedEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionCompleted then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionCompletedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionCompletedEventAttributes)
heChildWorkflowExecutionCompletedEventAttributes f x =
    f (_heChildWorkflowExecutionCompletedEventAttributes x)
        <&> \y -> x { _heChildWorkflowExecutionCompletedEventAttributes = y }
{-# INLINE heChildWorkflowExecutionCompletedEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionFailedEventAttributes)
heChildWorkflowExecutionFailedEventAttributes f x =
    f (_heChildWorkflowExecutionFailedEventAttributes x)
        <&> \y -> x { _heChildWorkflowExecutionFailedEventAttributes = y }
{-# INLINE heChildWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionTimedOut then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionTimedOutEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionTimedOutEventAttributes)
heChildWorkflowExecutionTimedOutEventAttributes f x =
    f (_heChildWorkflowExecutionTimedOutEventAttributes x)
        <&> \y -> x { _heChildWorkflowExecutionTimedOutEventAttributes = y }
{-# INLINE heChildWorkflowExecutionTimedOutEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionCanceled then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionCanceledEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionCanceledEventAttributes)
heChildWorkflowExecutionCanceledEventAttributes f x =
    f (_heChildWorkflowExecutionCanceledEventAttributes x)
        <&> \y -> x { _heChildWorkflowExecutionCanceledEventAttributes = y }
{-# INLINE heChildWorkflowExecutionCanceledEventAttributes #-}

-- | If the event is of type ChildWorkflowExecutionTerminated then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionTerminatedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionTerminatedEventAttributes)
heChildWorkflowExecutionTerminatedEventAttributes f x =
    f (_heChildWorkflowExecutionTerminatedEventAttributes x)
        <&> \y -> x { _heChildWorkflowExecutionTerminatedEventAttributes = y }
{-# INLINE heChildWorkflowExecutionTerminatedEventAttributes #-}

-- | If the event is of type SignalExternalWorkflowExecutionInitiated then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heSignalExternalWorkflowExecutionInitiatedEventAttributes :: Lens' HistoryEvent (Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes)
heSignalExternalWorkflowExecutionInitiatedEventAttributes f x =
    f (_heSignalExternalWorkflowExecutionInitiatedEventAttributes x)
        <&> \y -> x { _heSignalExternalWorkflowExecutionInitiatedEventAttributes = y }
{-# INLINE heSignalExternalWorkflowExecutionInitiatedEventAttributes #-}

-- | If the event is of type ExternalWorkflowExecutionSignaled then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
heExternalWorkflowExecutionSignaledEventAttributes :: Lens' HistoryEvent (Maybe ExternalWorkflowExecutionSignaledEventAttributes)
heExternalWorkflowExecutionSignaledEventAttributes f x =
    f (_heExternalWorkflowExecutionSignaledEventAttributes x)
        <&> \y -> x { _heExternalWorkflowExecutionSignaledEventAttributes = y }
{-# INLINE heExternalWorkflowExecutionSignaledEventAttributes #-}

-- | If the event is of type SignalExternalWorkflowExecutionFailed then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heSignalExternalWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe SignalExternalWorkflowExecutionFailedEventAttributes)
heSignalExternalWorkflowExecutionFailedEventAttributes f x =
    f (_heSignalExternalWorkflowExecutionFailedEventAttributes x)
        <&> \y -> x { _heSignalExternalWorkflowExecutionFailedEventAttributes = y }
{-# INLINE heSignalExternalWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type ExternalWorkflowExecutionCancelRequested then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heExternalWorkflowExecutionCancelRequestedEventAttributes :: Lens' HistoryEvent (Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes)
heExternalWorkflowExecutionCancelRequestedEventAttributes f x =
    f (_heExternalWorkflowExecutionCancelRequestedEventAttributes x)
        <&> \y -> x { _heExternalWorkflowExecutionCancelRequestedEventAttributes = y }
{-# INLINE heExternalWorkflowExecutionCancelRequestedEventAttributes #-}

-- | If the event is of type RequestCancelExternalWorkflowExecutionInitiated
-- then this member is set and provides detailed information about the event.
-- It is not set for other event types.
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Lens' HistoryEvent (Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes f x =
    f (_heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes x)
        <&> \y -> x { _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes = y }
{-# INLINE heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes #-}

-- | If the event is of type RequestCancelExternalWorkflowExecutionFailed then
-- this member is set and provides detailed information about the event. It is
-- not set for other event types.
heRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes)
heRequestCancelExternalWorkflowExecutionFailedEventAttributes f x =
    f (_heRequestCancelExternalWorkflowExecutionFailedEventAttributes x)
        <&> \y -> x { _heRequestCancelExternalWorkflowExecutionFailedEventAttributes = y }
{-# INLINE heRequestCancelExternalWorkflowExecutionFailedEventAttributes #-}

-- | If the event is of type ScheduleActivityTaskFailed then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heScheduleActivityTaskFailedEventAttributes :: Lens' HistoryEvent (Maybe ScheduleActivityTaskFailedEventAttributes)
heScheduleActivityTaskFailedEventAttributes f x =
    f (_heScheduleActivityTaskFailedEventAttributes x)
        <&> \y -> x { _heScheduleActivityTaskFailedEventAttributes = y }
{-# INLINE heScheduleActivityTaskFailedEventAttributes #-}

-- | If the event is of type RequestCancelActivityTaskFailed then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heRequestCancelActivityTaskFailedEventAttributes :: Lens' HistoryEvent (Maybe RequestCancelActivityTaskFailedEventAttributes)
heRequestCancelActivityTaskFailedEventAttributes f x =
    f (_heRequestCancelActivityTaskFailedEventAttributes x)
        <&> \y -> x { _heRequestCancelActivityTaskFailedEventAttributes = y }
{-# INLINE heRequestCancelActivityTaskFailedEventAttributes #-}

-- | If the event is of type StartTimerFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heStartTimerFailedEventAttributes :: Lens' HistoryEvent (Maybe StartTimerFailedEventAttributes)
heStartTimerFailedEventAttributes f x =
    f (_heStartTimerFailedEventAttributes x)
        <&> \y -> x { _heStartTimerFailedEventAttributes = y }
{-# INLINE heStartTimerFailedEventAttributes #-}

-- | If the event is of type CancelTimerFailed then this member is set and
-- provides detailed information about the event. It is not set for other
-- event types.
heCancelTimerFailedEventAttributes :: Lens' HistoryEvent (Maybe CancelTimerFailedEventAttributes)
heCancelTimerFailedEventAttributes f x =
    f (_heCancelTimerFailedEventAttributes x)
        <&> \y -> x { _heCancelTimerFailedEventAttributes = y }
{-# INLINE heCancelTimerFailedEventAttributes #-}

-- | If the event is of type StartChildWorkflowExecutionFailed then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
heStartChildWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe StartChildWorkflowExecutionFailedEventAttributes)
heStartChildWorkflowExecutionFailedEventAttributes f x =
    f (_heStartChildWorkflowExecutionFailedEventAttributes x)
        <&> \y -> x { _heStartChildWorkflowExecutionFailedEventAttributes = y }
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
mreaMarkerName f x =
    f (_mreaMarkerName x)
        <&> \y -> x { _mreaMarkerName = y }
{-# INLINE mreaMarkerName #-}

-- | Details of the marker (if any).
mreaDetails :: Lens' MarkerRecordedEventAttributes (Maybe Text)
mreaDetails f x =
    f (_mreaDetails x)
        <&> \y -> x { _mreaDetails = y }
{-# INLINE mreaDetails #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RecordMarker decision that requested this marker.
-- This information can be useful for diagnosing problems by tracing back the
-- cause of events.
mreaDecisionTaskCompletedEventId :: Lens' MarkerRecordedEventAttributes (Integer)
mreaDecisionTaskCompletedEventId f x =
    f (_mreaDecisionTaskCompletedEventId x)
        <&> \y -> x { _mreaDecisionTaskCompletedEventId = y }
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
rmdaMarkerName :: Lens' RecordMarkerDecisionAttributes (Text)
rmdaMarkerName f x =
    f (_rmdaMarkerName x)
        <&> \y -> x { _rmdaMarkerName = y }
{-# INLINE rmdaMarkerName #-}

-- | Optional details of the marker.
rmdaDetails :: Lens' RecordMarkerDecisionAttributes (Maybe Text)
rmdaDetails f x =
    f (_rmdaDetails x)
        <&> \y -> x { _rmdaDetails = y }
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
rmfeaMarkerName :: Lens' RecordMarkerFailedEventAttributes (Text)
rmfeaMarkerName f x =
    f (_rmfeaMarkerName x)
        <&> \y -> x { _rmfeaMarkerName = y }
{-# INLINE rmfeaMarkerName #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
rmfeaCause :: Lens' RecordMarkerFailedEventAttributes (RecordMarkerFailedCause)
rmfeaCause f x =
    f (_rmfeaCause x)
        <&> \y -> x { _rmfeaCause = y }
{-# INLINE rmfeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RecordMarkerFailed decision for this cancellation
-- request. This information can be useful for diagnosing problems by tracing
-- back the cause of events.
rmfeaDecisionTaskCompletedEventId :: Lens' RecordMarkerFailedEventAttributes (Integer)
rmfeaDecisionTaskCompletedEventId f x =
    f (_rmfeaDecisionTaskCompletedEventId x)
        <&> \y -> x { _rmfeaDecisionTaskCompletedEventId = y }
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
rcatfeaActivityId :: Lens' RequestCancelActivityTaskFailedEventAttributes (Text)
rcatfeaActivityId f x =
    f (_rcatfeaActivityId x)
        <&> \y -> x { _rcatfeaActivityId = y }
{-# INLINE rcatfeaActivityId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
rcatfeaCause :: Lens' RequestCancelActivityTaskFailedEventAttributes (RequestCancelActivityTaskFailedCause)
rcatfeaCause f x =
    f (_rcatfeaCause x)
        <&> \y -> x { _rcatfeaCause = y }
{-# INLINE rcatfeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RequestCancelActivityTask decision for this
-- cancellation request. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
rcatfeaDecisionTaskCompletedEventId :: Lens' RequestCancelActivityTaskFailedEventAttributes (Integer)
rcatfeaDecisionTaskCompletedEventId f x =
    f (_rcatfeaDecisionTaskCompletedEventId x)
        <&> \y -> x { _rcatfeaDecisionTaskCompletedEventId = y }
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
rcewedaWorkflowId :: Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Text)
rcewedaWorkflowId f x =
    f (_rcewedaWorkflowId x)
        <&> \y -> x { _rcewedaWorkflowId = y }
{-# INLINE rcewedaWorkflowId #-}

-- | The runId of the external workflow execution to cancel.
rcewedaRunId :: Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Maybe Text)
rcewedaRunId f x =
    f (_rcewedaRunId x)
        <&> \y -> x { _rcewedaRunId = y }
{-# INLINE rcewedaRunId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
rcewedaControl :: Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Maybe Text)
rcewedaControl f x =
    f (_rcewedaControl x)
        <&> \y -> x { _rcewedaControl = y }
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
rcewefeaWorkflowId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (Text)
rcewefeaWorkflowId f x =
    f (_rcewefeaWorkflowId x)
        <&> \y -> x { _rcewefeaWorkflowId = y }
{-# INLINE rcewefeaWorkflowId #-}

-- | The runId of the external workflow execution.
rcewefeaRunId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
rcewefeaRunId f x =
    f (_rcewefeaRunId x)
        <&> \y -> x { _rcewefeaRunId = y }
{-# INLINE rcewefeaRunId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
rcewefeaCause :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (RequestCancelExternalWorkflowExecutionFailedCause)
rcewefeaCause f x =
    f (_rcewefeaCause x)
        <&> \y -> x { _rcewefeaCause = y }
{-# INLINE rcewefeaCause #-}

-- | The id of the RequestCancelExternalWorkflowExecutionInitiated event
-- corresponding to the RequestCancelExternalWorkflowExecution decision to
-- cancel this external workflow execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
rcewefeaInitiatedEventId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (Integer)
rcewefeaInitiatedEventId f x =
    f (_rcewefeaInitiatedEventId x)
        <&> \y -> x { _rcewefeaInitiatedEventId = y }
{-# INLINE rcewefeaInitiatedEventId #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RequestCancelExternalWorkflowExecution decision
-- for this cancellation request. This information can be useful for
-- diagnosing problems by tracing back the cause of events.
rcewefeaDecisionTaskCompletedEventId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (Integer)
rcewefeaDecisionTaskCompletedEventId f x =
    f (_rcewefeaDecisionTaskCompletedEventId x)
        <&> \y -> x { _rcewefeaDecisionTaskCompletedEventId = y }
{-# INLINE rcewefeaDecisionTaskCompletedEventId #-}

rcewefeaControl :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
rcewefeaControl f x =
    f (_rcewefeaControl x)
        <&> \y -> x { _rcewefeaControl = y }
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
rceweieaWorkflowId :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Text)
rceweieaWorkflowId f x =
    f (_rceweieaWorkflowId x)
        <&> \y -> x { _rceweieaWorkflowId = y }
{-# INLINE rceweieaWorkflowId #-}

-- | The runId of the external workflow execution to be canceled.
rceweieaRunId :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
rceweieaRunId f x =
    f (_rceweieaRunId x)
        <&> \y -> x { _rceweieaRunId = y }
{-# INLINE rceweieaRunId #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the RequestCancelExternalWorkflowExecution decision
-- for this cancellation request. This information can be useful for
-- diagnosing problems by tracing back the cause of events.
rceweieaDecisionTaskCompletedEventId :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Integer)
rceweieaDecisionTaskCompletedEventId f x =
    f (_rceweieaDecisionTaskCompletedEventId x)
        <&> \y -> x { _rceweieaDecisionTaskCompletedEventId = y }
{-# INLINE rceweieaDecisionTaskCompletedEventId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
rceweieaControl :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
rceweieaControl f x =
    f (_rceweieaControl x)
        <&> \y -> x { _rceweieaControl = y }
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
satdaActivityType :: Lens' ScheduleActivityTaskDecisionAttributes (ActivityType)
satdaActivityType f x =
    f (_satdaActivityType x)
        <&> \y -> x { _satdaActivityType = y }
{-# INLINE satdaActivityType #-}

-- | The activityId of the activity task. This field is required. The specified
-- string must not start or end with whitespace. It must not contain a :
-- (colon), / (slash), | (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string &quot;arn&quot;.
satdaActivityId :: Lens' ScheduleActivityTaskDecisionAttributes (Text)
satdaActivityId f x =
    f (_satdaActivityId x)
        <&> \y -> x { _satdaActivityId = y }
{-# INLINE satdaActivityId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks. This data is not sent to the activity.
satdaControl :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaControl f x =
    f (_satdaControl x)
        <&> \y -> x { _satdaControl = y }
{-# INLINE satdaControl #-}

-- | The input provided to the activity task.
satdaInput :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaInput f x =
    f (_satdaInput x)
        <&> \y -> x { _satdaInput = y }
{-# INLINE satdaInput #-}

-- | The maximum duration for this activity task. The valid values are integers
-- greater than or equal to 0. An integer value can be used to specify the
-- duration in seconds while NONE can be used to specify unlimited duration. A
-- schedule-to-close timeout for this activity task must be specified either
-- as a default for the activity type or through this field. If neither this
-- field is set nor a default schedule-to-close timeout was specified at
-- registration time then a fault will be returned.
satdaScheduleToCloseTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaScheduleToCloseTimeout f x =
    f (_satdaScheduleToCloseTimeout x)
        <&> \y -> x { _satdaScheduleToCloseTimeout = y }
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
satdaTaskList f x =
    f (_satdaTaskList x)
        <&> \y -> x { _satdaTaskList = y }
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
satdaScheduleToStartTimeout f x =
    f (_satdaScheduleToStartTimeout x)
        <&> \y -> x { _satdaScheduleToStartTimeout = y }
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
satdaStartToCloseTimeout f x =
    f (_satdaStartToCloseTimeout x)
        <&> \y -> x { _satdaStartToCloseTimeout = y }
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
satdaHeartbeatTimeout f x =
    f (_satdaHeartbeatTimeout x)
        <&> \y -> x { _satdaHeartbeatTimeout = y }
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
satfeaActivityType :: Lens' ScheduleActivityTaskFailedEventAttributes (ActivityType)
satfeaActivityType f x =
    f (_satfeaActivityType x)
        <&> \y -> x { _satfeaActivityType = y }
{-# INLINE satfeaActivityType #-}

-- | The activityId provided in the ScheduleActivityTask decision that failed.
satfeaActivityId :: Lens' ScheduleActivityTaskFailedEventAttributes (Text)
satfeaActivityId f x =
    f (_satfeaActivityId x)
        <&> \y -> x { _satfeaActivityId = y }
{-# INLINE satfeaActivityId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
satfeaCause :: Lens' ScheduleActivityTaskFailedEventAttributes (ScheduleActivityTaskFailedCause)
satfeaCause f x =
    f (_satfeaCause x)
        <&> \y -> x { _satfeaCause = y }
{-# INLINE satfeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- that resulted in the scheduling of this activity task. This information can
-- be useful for diagnosing problems by tracing back the chain of events
-- leading up to this event.
satfeaDecisionTaskCompletedEventId :: Lens' ScheduleActivityTaskFailedEventAttributes (Integer)
satfeaDecisionTaskCompletedEventId f x =
    f (_satfeaDecisionTaskCompletedEventId x)
        <&> \y -> x { _satfeaDecisionTaskCompletedEventId = y }
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
sewedaWorkflowId :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Text)
sewedaWorkflowId f x =
    f (_sewedaWorkflowId x)
        <&> \y -> x { _sewedaWorkflowId = y }
{-# INLINE sewedaWorkflowId #-}

-- | The runId of the workflow execution to be signaled.
sewedaRunId :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Maybe Text)
sewedaRunId f x =
    f (_sewedaRunId x)
        <&> \y -> x { _sewedaRunId = y }
{-# INLINE sewedaRunId #-}

-- | The name of the signal.The target workflow execution will use the signal
-- name and input to process the signal. This field is required.
sewedaSignalName :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Text)
sewedaSignalName f x =
    f (_sewedaSignalName x)
        <&> \y -> x { _sewedaSignalName = y }
{-# INLINE sewedaSignalName #-}

-- | Optional input to be provided with the signal.The target workflow execution
-- will use the signal name and input to process the signal.
sewedaInput :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Maybe Text)
sewedaInput f x =
    f (_sewedaInput x)
        <&> \y -> x { _sewedaInput = y }
{-# INLINE sewedaInput #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent decision tasks.
sewedaControl :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Maybe Text)
sewedaControl f x =
    f (_sewedaControl x)
        <&> \y -> x { _sewedaControl = y }
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
sewefeaWorkflowId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (Text)
sewefeaWorkflowId f x =
    f (_sewefeaWorkflowId x)
        <&> \y -> x { _sewefeaWorkflowId = y }
{-# INLINE sewefeaWorkflowId #-}

-- | The runId of the external workflow execution that the signal was being
-- delivered to.
sewefeaRunId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
sewefeaRunId f x =
    f (_sewefeaRunId x)
        <&> \y -> x { _sewefeaRunId = y }
{-# INLINE sewefeaRunId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
sewefeaCause :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (SignalExternalWorkflowExecutionFailedCause)
sewefeaCause f x =
    f (_sewefeaCause x)
        <&> \y -> x { _sewefeaCause = y }
{-# INLINE sewefeaCause #-}

-- | The id of the SignalExternalWorkflowExecutionInitiated event corresponding
-- to the SignalExternalWorkflowExecution decision to request this signal.
-- This information can be useful for diagnosing problems by tracing back the
-- chain of events leading up to this event.
sewefeaInitiatedEventId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (Integer)
sewefeaInitiatedEventId f x =
    f (_sewefeaInitiatedEventId x)
        <&> \y -> x { _sewefeaInitiatedEventId = y }
{-# INLINE sewefeaInitiatedEventId #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the SignalExternalWorkflowExecution decision for this
-- signal. This information can be useful for diagnosing problems by tracing
-- back the cause of events leading up to this event.
sewefeaDecisionTaskCompletedEventId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (Integer)
sewefeaDecisionTaskCompletedEventId f x =
    f (_sewefeaDecisionTaskCompletedEventId x)
        <&> \y -> x { _sewefeaDecisionTaskCompletedEventId = y }
{-# INLINE sewefeaDecisionTaskCompletedEventId #-}

sewefeaControl :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
sewefeaControl f x =
    f (_sewefeaControl x)
        <&> \y -> x { _sewefeaControl = y }
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
seweieaWorkflowId :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Text)
seweieaWorkflowId f x =
    f (_seweieaWorkflowId x)
        <&> \y -> x { _seweieaWorkflowId = y }
{-# INLINE seweieaWorkflowId #-}

-- | The runId of the external workflow execution to send the signal to.
seweieaRunId :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
seweieaRunId f x =
    f (_seweieaRunId x)
        <&> \y -> x { _seweieaRunId = y }
{-# INLINE seweieaRunId #-}

-- | The name of the signal.
seweieaSignalName :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Text)
seweieaSignalName f x =
    f (_seweieaSignalName x)
        <&> \y -> x { _seweieaSignalName = y }
{-# INLINE seweieaSignalName #-}

-- | Input provided to the signal (if any).
seweieaInput :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
seweieaInput f x =
    f (_seweieaInput x)
        <&> \y -> x { _seweieaInput = y }
{-# INLINE seweieaInput #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the SignalExternalWorkflowExecution decision for this
-- signal. This information can be useful for diagnosing problems by tracing
-- back the cause of events leading up to this event.
seweieaDecisionTaskCompletedEventId :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Integer)
seweieaDecisionTaskCompletedEventId f x =
    f (_seweieaDecisionTaskCompletedEventId x)
        <&> \y -> x { _seweieaDecisionTaskCompletedEventId = y }
{-# INLINE seweieaDecisionTaskCompletedEventId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent decision tasks.
seweieaControl :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
seweieaControl f x =
    f (_seweieaControl x)
        <&> \y -> x { _seweieaControl = y }
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
scwedaWorkflowType :: Lens' StartChildWorkflowExecutionDecisionAttributes (WorkflowType)
scwedaWorkflowType f x =
    f (_scwedaWorkflowType x)
        <&> \y -> x { _scwedaWorkflowType = y }
{-# INLINE scwedaWorkflowType #-}

-- | The workflowId of the workflow execution. This field is required. The
-- specified string must not start or end with whitespace. It must not contain
-- a : (colon), / (slash), | (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string &quot;arn&quot;.
scwedaWorkflowId :: Lens' StartChildWorkflowExecutionDecisionAttributes (Text)
scwedaWorkflowId f x =
    f (_scwedaWorkflowId x)
        <&> \y -> x { _scwedaWorkflowId = y }
{-# INLINE scwedaWorkflowId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks. This data is not sent to the child workflow
-- execution.
scwedaControl :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaControl f x =
    f (_scwedaControl x)
        <&> \y -> x { _scwedaControl = y }
{-# INLINE scwedaControl #-}

-- | The input to be provided to the workflow execution.
scwedaInput :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaInput f x =
    f (_scwedaInput x)
        <&> \y -> x { _scwedaInput = y }
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
scwedaExecutionStartToCloseTimeout f x =
    f (_scwedaExecutionStartToCloseTimeout x)
        <&> \y -> x { _scwedaExecutionStartToCloseTimeout = y }
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
scwedaTaskList f x =
    f (_scwedaTaskList x)
        <&> \y -> x { _scwedaTaskList = y }
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
scwedaTaskStartToCloseTimeout f x =
    f (_scwedaTaskStartToCloseTimeout x)
        <&> \y -> x { _scwedaTaskStartToCloseTimeout = y }
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
scwedaChildPolicy f x =
    f (_scwedaChildPolicy x)
        <&> \y -> x { _scwedaChildPolicy = y }
{-# INLINE scwedaChildPolicy #-}

-- | The list of tags to associate with the child workflow execution. A maximum
-- of 5 tags can be specified. You can list workflow executions with a
-- specific tag by calling ListOpenWorkflowExecutions or
-- ListClosedWorkflowExecutions and specifying a TagFilter.
scwedaTagList :: Lens' StartChildWorkflowExecutionDecisionAttributes ([Text])
scwedaTagList f x =
    f (_scwedaTagList x)
        <&> \y -> x { _scwedaTagList = y }
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
scwefeaWorkflowType :: Lens' StartChildWorkflowExecutionFailedEventAttributes (WorkflowType)
scwefeaWorkflowType f x =
    f (_scwefeaWorkflowType x)
        <&> \y -> x { _scwefeaWorkflowType = y }
{-# INLINE scwefeaWorkflowType #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
scwefeaCause :: Lens' StartChildWorkflowExecutionFailedEventAttributes (StartChildWorkflowExecutionFailedCause)
scwefeaCause f x =
    f (_scwefeaCause x)
        <&> \y -> x { _scwefeaCause = y }
{-# INLINE scwefeaCause #-}

-- | The workflowId of the child workflow execution.
scwefeaWorkflowId :: Lens' StartChildWorkflowExecutionFailedEventAttributes (Text)
scwefeaWorkflowId f x =
    f (_scwefeaWorkflowId x)
        <&> \y -> x { _scwefeaWorkflowId = y }
{-# INLINE scwefeaWorkflowId #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this child workflow
-- execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
scwefeaInitiatedEventId :: Lens' StartChildWorkflowExecutionFailedEventAttributes (Integer)
scwefeaInitiatedEventId f x =
    f (_scwefeaInitiatedEventId x)
        <&> \y -> x { _scwefeaInitiatedEventId = y }
{-# INLINE scwefeaInitiatedEventId #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the StartChildWorkflowExecution Decision to request
-- this child workflow execution. This information can be useful for
-- diagnosing problems by tracing back the cause of events.
scwefeaDecisionTaskCompletedEventId :: Lens' StartChildWorkflowExecutionFailedEventAttributes (Integer)
scwefeaDecisionTaskCompletedEventId f x =
    f (_scwefeaDecisionTaskCompletedEventId x)
        <&> \y -> x { _scwefeaDecisionTaskCompletedEventId = y }
{-# INLINE scwefeaDecisionTaskCompletedEventId #-}

scwefeaControl :: Lens' StartChildWorkflowExecutionFailedEventAttributes (Maybe Text)
scwefeaControl f x =
    f (_scwefeaControl x)
        <&> \y -> x { _scwefeaControl = y }
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
scweieaWorkflowId :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Text)
scweieaWorkflowId f x =
    f (_scweieaWorkflowId x)
        <&> \y -> x { _scweieaWorkflowId = y }
{-# INLINE scweieaWorkflowId #-}

-- | The type of the child workflow execution.
scweieaWorkflowType :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (WorkflowType)
scweieaWorkflowType f x =
    f (_scweieaWorkflowType x)
        <&> \y -> x { _scweieaWorkflowType = y }
{-# INLINE scweieaWorkflowType #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent decision tasks. This data is not sent to the activity.
scweieaControl :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaControl f x =
    f (_scweieaControl x)
        <&> \y -> x { _scweieaControl = y }
{-# INLINE scweieaControl #-}

-- | The inputs provided to the child workflow execution (if any).
scweieaInput :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaInput f x =
    f (_scweieaInput x)
        <&> \y -> x { _scweieaInput = y }
{-# INLINE scweieaInput #-}

-- | The maximum duration for the child workflow execution. If the workflow
-- execution is not closed within this duration, it will be timed out and
-- force terminated. The valid values are integers greater than or equal to 0.
-- An integer value can be used to specify the duration in seconds while NONE
-- can be used to specify unlimited duration.
scweieaExecutionStartToCloseTimeout :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaExecutionStartToCloseTimeout f x =
    f (_scweieaExecutionStartToCloseTimeout x)
        <&> \y -> x { _scweieaExecutionStartToCloseTimeout = y }
{-# INLINE scweieaExecutionStartToCloseTimeout #-}

-- | The name of the task list used for the decision tasks of the child workflow
-- execution.
scweieaTaskList :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (TaskList)
scweieaTaskList f x =
    f (_scweieaTaskList x)
        <&> \y -> x { _scweieaTaskList = y }
{-# INLINE scweieaTaskList #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the StartChildWorkflowExecution Decision to request
-- this child workflow execution. This information can be useful for
-- diagnosing problems by tracing back the cause of events.
scweieaDecisionTaskCompletedEventId :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Integer)
scweieaDecisionTaskCompletedEventId f x =
    f (_scweieaDecisionTaskCompletedEventId x)
        <&> \y -> x { _scweieaDecisionTaskCompletedEventId = y }
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
scweieaChildPolicy f x =
    f (_scweieaChildPolicy x)
        <&> \y -> x { _scweieaChildPolicy = y }
{-# INLINE scweieaChildPolicy #-}

-- | The maximum duration allowed for the decision tasks for this workflow
-- execution. The valid values are integers greater than or equal to 0. An
-- integer value can be used to specify the duration in seconds while NONE can
-- be used to specify unlimited duration.
scweieaTaskStartToCloseTimeout :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaTaskStartToCloseTimeout f x =
    f (_scweieaTaskStartToCloseTimeout x)
        <&> \y -> x { _scweieaTaskStartToCloseTimeout = y }
{-# INLINE scweieaTaskStartToCloseTimeout #-}

-- | The list of tags to associated with the child workflow execution.
scweieaTagList :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes ([Text])
scweieaTagList f x =
    f (_scweieaTagList x)
        <&> \y -> x { _scweieaTagList = y }
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
stdaTimerId :: Lens' StartTimerDecisionAttributes (Text)
stdaTimerId f x =
    f (_stdaTimerId x)
        <&> \y -> x { _stdaTimerId = y }
{-# INLINE stdaTimerId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
stdaControl :: Lens' StartTimerDecisionAttributes (Maybe Text)
stdaControl f x =
    f (_stdaControl x)
        <&> \y -> x { _stdaControl = y }
{-# INLINE stdaControl #-}

-- | The duration to wait before firing the timer. This field is required. The
-- duration is specified in seconds. The valid values are integers greater
-- than or equal to 0.
stdaStartToFireTimeout :: Lens' StartTimerDecisionAttributes (Text)
stdaStartToFireTimeout f x =
    f (_stdaStartToFireTimeout x)
        <&> \y -> x { _stdaStartToFireTimeout = y }
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
stfeaTimerId :: Lens' StartTimerFailedEventAttributes (Text)
stfeaTimerId f x =
    f (_stfeaTimerId x)
        <&> \y -> x { _stfeaTimerId = y }
{-# INLINE stfeaTimerId #-}

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes. If cause
-- is set to OPERATION_NOT_PERMITTED, the decision failed because it lacked
-- sufficient permissions. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows.
stfeaCause :: Lens' StartTimerFailedEventAttributes (StartTimerFailedCause)
stfeaCause f x =
    f (_stfeaCause x)
        <&> \y -> x { _stfeaCause = y }
{-# INLINE stfeaCause #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the StartTimer decision for this activity task. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
stfeaDecisionTaskCompletedEventId :: Lens' StartTimerFailedEventAttributes (Integer)
stfeaDecisionTaskCompletedEventId f x =
    f (_stfeaDecisionTaskCompletedEventId x)
        <&> \y -> x { _stfeaDecisionTaskCompletedEventId = y }
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
tceaTimerId :: Lens' TimerCanceledEventAttributes (Text)
tceaTimerId f x =
    f (_tceaTimerId x)
        <&> \y -> x { _tceaTimerId = y }
{-# INLINE tceaTimerId #-}

-- | The id of the TimerStarted event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
tceaStartedEventId :: Lens' TimerCanceledEventAttributes (Integer)
tceaStartedEventId f x =
    f (_tceaStartedEventId x)
        <&> \y -> x { _tceaStartedEventId = y }
{-# INLINE tceaStartedEventId #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CancelTimer decision to cancel this timer. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
tceaDecisionTaskCompletedEventId :: Lens' TimerCanceledEventAttributes (Integer)
tceaDecisionTaskCompletedEventId f x =
    f (_tceaDecisionTaskCompletedEventId x)
        <&> \y -> x { _tceaDecisionTaskCompletedEventId = y }
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
tfeaTimerId :: Lens' TimerFiredEventAttributes (Text)
tfeaTimerId f x =
    f (_tfeaTimerId x)
        <&> \y -> x { _tfeaTimerId = y }
{-# INLINE tfeaTimerId #-}

-- | The id of the TimerStarted event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
tfeaStartedEventId :: Lens' TimerFiredEventAttributes (Integer)
tfeaStartedEventId f x =
    f (_tfeaStartedEventId x)
        <&> \y -> x { _tfeaStartedEventId = y }
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
tseaTimerId :: Lens' TimerStartedEventAttributes (Text)
tseaTimerId f x =
    f (_tseaTimerId x)
        <&> \y -> x { _tseaTimerId = y }
{-# INLINE tseaTimerId #-}

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
tseaControl :: Lens' TimerStartedEventAttributes (Maybe Text)
tseaControl f x =
    f (_tseaControl x)
        <&> \y -> x { _tseaControl = y }
{-# INLINE tseaControl #-}

-- | The duration of time after which the timer will fire. The duration is
-- specified in seconds. The valid values are integers greater than or equal
-- to 0.
tseaStartToFireTimeout :: Lens' TimerStartedEventAttributes (Text)
tseaStartToFireTimeout f x =
    f (_tseaStartToFireTimeout x)
        <&> \y -> x { _tseaStartToFireTimeout = y }
{-# INLINE tseaStartToFireTimeout #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the StartTimer decision for this activity task. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
tseaDecisionTaskCompletedEventId :: Lens' TimerStartedEventAttributes (Integer)
tseaDecisionTaskCompletedEventId f x =
    f (_tseaDecisionTaskCompletedEventId x)
        <&> \y -> x { _tseaDecisionTaskCompletedEventId = y }
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
weWorkflowId :: Lens' WorkflowExecution (Text)
weWorkflowId f x =
    f (_weWorkflowId x)
        <&> \y -> x { _weWorkflowId = y }
{-# INLINE weWorkflowId #-}

-- | A system generated unique identifier for the workflow execution.
weRunId :: Lens' WorkflowExecution (Text)
weRunId f x =
    f (_weRunId x)
        <&> \y -> x { _weRunId = y }
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
wecreaExternalWorkflowExecution :: Lens' WorkflowExecutionCancelRequestedEventAttributes (Maybe WorkflowExecution)
wecreaExternalWorkflowExecution f x =
    f (_wecreaExternalWorkflowExecution x)
        <&> \y -> x { _wecreaExternalWorkflowExecution = y }
{-# INLINE wecreaExternalWorkflowExecution #-}

-- | The id of the RequestCancelExternalWorkflowExecutionInitiated event
-- corresponding to the RequestCancelExternalWorkflowExecution decision to
-- cancel this workflow execution.The source event with this Id can be found
-- in the history of the source workflow execution. This information can be
-- useful for diagnosing problems by tracing back the chain of events leading
-- up to this event.
wecreaExternalInitiatedEventId :: Lens' WorkflowExecutionCancelRequestedEventAttributes (Maybe Integer)
wecreaExternalInitiatedEventId f x =
    f (_wecreaExternalInitiatedEventId x)
        <&> \y -> x { _wecreaExternalInitiatedEventId = y }
{-# INLINE wecreaExternalInitiatedEventId #-}

-- | If set, indicates that the request to cancel the workflow execution was
-- automatically generated, and specifies the cause. This happens if the
-- parent workflow execution times out or is terminated, and the child policy
-- is set to cancel child executions.
wecreaCause :: Lens' WorkflowExecutionCancelRequestedEventAttributes (Maybe WorkflowExecutionCancelRequestedCause)
wecreaCause f x =
    f (_wecreaCause x)
        <&> \y -> x { _wecreaCause = y }
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
wecebDetails :: Lens' WorkflowExecutionCanceledEventAttributes (Maybe Text)
wecebDetails f x =
    f (_wecebDetails x)
        <&> \y -> x { _wecebDetails = y }
{-# INLINE wecebDetails #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CancelWorkflowExecution decision for this
-- cancellation request. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
wecebDecisionTaskCompletedEventId :: Lens' WorkflowExecutionCanceledEventAttributes (Integer)
wecebDecisionTaskCompletedEventId f x =
    f (_wecebDecisionTaskCompletedEventId x)
        <&> \y -> x { _wecebDecisionTaskCompletedEventId = y }
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
weceaResult :: Lens' WorkflowExecutionCompletedEventAttributes (Maybe Text)
weceaResult f x =
    f (_weceaResult x)
        <&> \y -> x { _weceaResult = y }
{-# INLINE weceaResult #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the CompleteWorkflowExecution decision to complete
-- this execution. This information can be useful for diagnosing problems by
-- tracing back the cause of events.
weceaDecisionTaskCompletedEventId :: Lens' WorkflowExecutionCompletedEventAttributes (Integer)
weceaDecisionTaskCompletedEventId f x =
    f (_weceaDecisionTaskCompletedEventId x)
        <&> \y -> x { _weceaDecisionTaskCompletedEventId = y }
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
wehTaskStartToCloseTimeout :: Lens' WorkflowExecutionConfiguration (Text)
wehTaskStartToCloseTimeout f x =
    f (_wehTaskStartToCloseTimeout x)
        <&> \y -> x { _wehTaskStartToCloseTimeout = y }
{-# INLINE wehTaskStartToCloseTimeout #-}

-- | The total duration for this workflow execution. The valid values are
-- integers greater than or equal to 0. An integer value can be used to
-- specify the duration in seconds while NONE can be used to specify unlimited
-- duration.
wehExecutionStartToCloseTimeout :: Lens' WorkflowExecutionConfiguration (Text)
wehExecutionStartToCloseTimeout f x =
    f (_wehExecutionStartToCloseTimeout x)
        <&> \y -> x { _wehExecutionStartToCloseTimeout = y }
{-# INLINE wehExecutionStartToCloseTimeout #-}

-- | The task list used for the decision tasks generated for this workflow
-- execution.
wehTaskList :: Lens' WorkflowExecutionConfiguration (TaskList)
wehTaskList f x =
    f (_wehTaskList x)
        <&> \y -> x { _wehTaskList = y }
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
wehChildPolicy f x =
    f (_wehChildPolicy x)
        <&> \y -> x { _wehChildPolicy = y }
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
wecaneaInput f x =
    f (_wecaneaInput x)
        <&> \y -> x { _wecaneaInput = y }
{-# INLINE wecaneaInput #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the ContinueAsNewWorkflowExecution decision that
-- started this execution. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
wecaneaDecisionTaskCompletedEventId :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Integer)
wecaneaDecisionTaskCompletedEventId f x =
    f (_wecaneaDecisionTaskCompletedEventId x)
        <&> \y -> x { _wecaneaDecisionTaskCompletedEventId = y }
{-# INLINE wecaneaDecisionTaskCompletedEventId #-}

-- | The runId of the new workflow execution.
wecaneaNewExecutionRunId :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Text)
wecaneaNewExecutionRunId f x =
    f (_wecaneaNewExecutionRunId x)
        <&> \y -> x { _wecaneaNewExecutionRunId = y }
{-# INLINE wecaneaNewExecutionRunId #-}

-- | The total duration allowed for the new workflow execution. The valid values
-- are integers greater than or equal to 0. An integer value can be used to
-- specify the duration in seconds while NONE can be used to specify unlimited
-- duration.
wecaneaExecutionStartToCloseTimeout :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaExecutionStartToCloseTimeout f x =
    f (_wecaneaExecutionStartToCloseTimeout x)
        <&> \y -> x { _wecaneaExecutionStartToCloseTimeout = y }
{-# INLINE wecaneaExecutionStartToCloseTimeout #-}

-- | Represents a task list.
wecaneaTaskList :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (TaskList)
wecaneaTaskList f x =
    f (_wecaneaTaskList x)
        <&> \y -> x { _wecaneaTaskList = y }
{-# INLINE wecaneaTaskList #-}

-- | The maximum duration of decision tasks for the new workflow execution. The
-- valid values are integers greater than or equal to 0. An integer value can
-- be used to specify the duration in seconds while NONE can be used to
-- specify unlimited duration.
wecaneaTaskStartToCloseTimeout :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaTaskStartToCloseTimeout f x =
    f (_wecaneaTaskStartToCloseTimeout x)
        <&> \y -> x { _wecaneaTaskStartToCloseTimeout = y }
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
wecaneaChildPolicy f x =
    f (_wecaneaChildPolicy x)
        <&> \y -> x { _wecaneaChildPolicy = y }
{-# INLINE wecaneaChildPolicy #-}

-- | The list of tags associated with the new workflow execution.
wecaneaTagList :: Lens' WorkflowExecutionContinuedAsNewEventAttributes ([Text])
wecaneaTagList f x =
    f (_wecaneaTagList x)
        <&> \y -> x { _wecaneaTagList = y }
{-# INLINE wecaneaTagList #-}

-- | Represents a workflow type.
wecaneaWorkflowType :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (WorkflowType)
wecaneaWorkflowType f x =
    f (_wecaneaWorkflowType x)
        <&> \y -> x { _wecaneaWorkflowType = y }
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
wefeaReason :: Lens' WorkflowExecutionFailedEventAttributes (Maybe Text)
wefeaReason f x =
    f (_wefeaReason x)
        <&> \y -> x { _wefeaReason = y }
{-# INLINE wefeaReason #-}

-- | The details of the failure (if any).
wefeaDetails :: Lens' WorkflowExecutionFailedEventAttributes (Maybe Text)
wefeaDetails f x =
    f (_wefeaDetails x)
        <&> \y -> x { _wefeaDetails = y }
{-# INLINE wefeaDetails #-}

-- | The id of the DecisionTaskCompleted event corresponding to the decision
-- task that resulted in the FailWorkflowExecution decision to fail this
-- execution. This information can be useful for diagnosing problems by
-- tracing back the cause of events.
wefeaDecisionTaskCompletedEventId :: Lens' WorkflowExecutionFailedEventAttributes (Integer)
wefeaDecisionTaskCompletedEventId f x =
    f (_wefeaDecisionTaskCompletedEventId x)
        <&> \y -> x { _wefeaDecisionTaskCompletedEventId = y }
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
weiExecution :: Lens' WorkflowExecutionInfo (WorkflowExecution)
weiExecution f x =
    f (_weiExecution x)
        <&> \y -> x { _weiExecution = y }
{-# INLINE weiExecution #-}

-- | The type of the workflow execution.
weiWorkflowType :: Lens' WorkflowExecutionInfo (WorkflowType)
weiWorkflowType f x =
    f (_weiWorkflowType x)
        <&> \y -> x { _weiWorkflowType = y }
{-# INLINE weiWorkflowType #-}

-- | The time when the execution was started.
weiStartTimestamp :: Lens' WorkflowExecutionInfo (POSIX)
weiStartTimestamp f x =
    f (_weiStartTimestamp x)
        <&> \y -> x { _weiStartTimestamp = y }
{-# INLINE weiStartTimestamp #-}

-- | The time when the workflow execution was closed. Set only if the execution
-- status is CLOSED.
weiCloseTimestamp :: Lens' WorkflowExecutionInfo (Maybe POSIX)
weiCloseTimestamp f x =
    f (_weiCloseTimestamp x)
        <&> \y -> x { _weiCloseTimestamp = y }
{-# INLINE weiCloseTimestamp #-}

-- | The current status of the execution.
weiExecutionStatus :: Lens' WorkflowExecutionInfo (ExecutionStatus)
weiExecutionStatus f x =
    f (_weiExecutionStatus x)
        <&> \y -> x { _weiExecutionStatus = y }
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
weiCloseStatus f x =
    f (_weiCloseStatus x)
        <&> \y -> x { _weiCloseStatus = y }
{-# INLINE weiCloseStatus #-}

-- | If this workflow execution is a child of another execution then contains
-- the workflow execution that started this execution.
weiParent :: Lens' WorkflowExecutionInfo (Maybe WorkflowExecution)
weiParent f x =
    f (_weiParent x)
        <&> \y -> x { _weiParent = y }
{-# INLINE weiParent #-}

-- | The list of tags associated with the workflow execution. Tags can be used
-- to identify and list workflow executions of interest through the visibility
-- APIs. A workflow execution can have a maximum of 5 tags.
weiTagList :: Lens' WorkflowExecutionInfo ([Text])
weiTagList f x =
    f (_weiTagList x)
        <&> \y -> x { _weiTagList = y }
{-# INLINE weiTagList #-}

-- | Set to true if a cancellation is requested for this workflow execution.
weiCancelRequested :: Lens' WorkflowExecutionInfo (Maybe Bool)
weiCancelRequested f x =
    f (_weiCancelRequested x)
        <&> \y -> x { _weiCancelRequested = y }
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
weocOpenActivityTasks f x =
    f (_weocOpenActivityTasks x)
        <&> \y -> x { _weocOpenActivityTasks = y }
{-# INLINE weocOpenActivityTasks #-}

-- | The count of decision tasks whose status is OPEN. A workflow execution can
-- have at most one open decision task.
weocOpenDecisionTasks :: Lens' WorkflowExecutionOpenCounts (Integer)
weocOpenDecisionTasks f x =
    f (_weocOpenDecisionTasks x)
        <&> \y -> x { _weocOpenDecisionTasks = y }
{-# INLINE weocOpenDecisionTasks #-}

-- | The count of timers started by this workflow execution that have not fired
-- yet.
weocOpenTimers :: Lens' WorkflowExecutionOpenCounts (Integer)
weocOpenTimers f x =
    f (_weocOpenTimers x)
        <&> \y -> x { _weocOpenTimers = y }
{-# INLINE weocOpenTimers #-}

-- | The count of child workflow executions whose status is OPEN.
weocOpenChildWorkflowExecutions :: Lens' WorkflowExecutionOpenCounts (Integer)
weocOpenChildWorkflowExecutions f x =
    f (_weocOpenChildWorkflowExecutions x)
        <&> \y -> x { _weocOpenChildWorkflowExecutions = y }
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
wesebSignalName f x =
    f (_wesebSignalName x)
        <&> \y -> x { _wesebSignalName = y }
{-# INLINE wesebSignalName #-}

-- | Inputs provided with the signal (if any). The decider can use the signal
-- name and inputs to determine how to process the signal.
wesebInput :: Lens' WorkflowExecutionSignaledEventAttributes (Maybe Text)
wesebInput f x =
    f (_wesebInput x)
        <&> \y -> x { _wesebInput = y }
{-# INLINE wesebInput #-}

-- | The workflow execution that sent the signal. This is set only of the signal
-- was sent by another workflow execution.
wesebExternalWorkflowExecution :: Lens' WorkflowExecutionSignaledEventAttributes (Maybe WorkflowExecution)
wesebExternalWorkflowExecution f x =
    f (_wesebExternalWorkflowExecution x)
        <&> \y -> x { _wesebExternalWorkflowExecution = y }
{-# INLINE wesebExternalWorkflowExecution #-}

-- | The id of the SignalExternalWorkflowExecutionInitiated event corresponding
-- to the SignalExternalWorkflow decision to signal this workflow
-- execution.The source event with this Id can be found in the history of the
-- source workflow execution. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event. This
-- field is set only if the signal was initiated by another workflow
-- execution.
wesebExternalInitiatedEventId :: Lens' WorkflowExecutionSignaledEventAttributes (Maybe Integer)
wesebExternalInitiatedEventId f x =
    f (_wesebExternalInitiatedEventId x)
        <&> \y -> x { _wesebExternalInitiatedEventId = y }
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
weseaInput :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaInput f x =
    f (_weseaInput x)
        <&> \y -> x { _weseaInput = y }
{-# INLINE weseaInput #-}

-- | The maximum duration for this workflow execution. The valid values are
-- integers greater than or equal to 0. An integer value can be used to
-- specify the duration in seconds while NONE can be used to specify unlimited
-- duration.
weseaExecutionStartToCloseTimeout :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaExecutionStartToCloseTimeout f x =
    f (_weseaExecutionStartToCloseTimeout x)
        <&> \y -> x { _weseaExecutionStartToCloseTimeout = y }
{-# INLINE weseaExecutionStartToCloseTimeout #-}

-- | The maximum duration of decision tasks for this workflow type. The valid
-- values are integers greater than or equal to 0. An integer value can be
-- used to specify the duration in seconds while NONE can be used to specify
-- unlimited duration.
weseaTaskStartToCloseTimeout :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaTaskStartToCloseTimeout f x =
    f (_weseaTaskStartToCloseTimeout x)
        <&> \y -> x { _weseaTaskStartToCloseTimeout = y }
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
weseaChildPolicy f x =
    f (_weseaChildPolicy x)
        <&> \y -> x { _weseaChildPolicy = y }
{-# INLINE weseaChildPolicy #-}

-- | The name of the task list for scheduling the decision tasks for this
-- workflow execution.
weseaTaskList :: Lens' WorkflowExecutionStartedEventAttributes (TaskList)
weseaTaskList f x =
    f (_weseaTaskList x)
        <&> \y -> x { _weseaTaskList = y }
{-# INLINE weseaTaskList #-}

-- | The workflow type of this execution.
weseaWorkflowType :: Lens' WorkflowExecutionStartedEventAttributes (WorkflowType)
weseaWorkflowType f x =
    f (_weseaWorkflowType x)
        <&> \y -> x { _weseaWorkflowType = y }
{-# INLINE weseaWorkflowType #-}

-- | The list of tags associated with this workflow execution. An execution can
-- have up to 5 tags.
weseaTagList :: Lens' WorkflowExecutionStartedEventAttributes ([Text])
weseaTagList f x =
    f (_weseaTagList x)
        <&> \y -> x { _weseaTagList = y }
{-# INLINE weseaTagList #-}

-- | If this workflow execution was started due to a
-- ContinueAsNewWorkflowExecution decision, then it contains the runId of the
-- previous workflow execution that was closed and continued as this
-- execution.
weseaContinuedExecutionRunId :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaContinuedExecutionRunId f x =
    f (_weseaContinuedExecutionRunId x)
        <&> \y -> x { _weseaContinuedExecutionRunId = y }
{-# INLINE weseaContinuedExecutionRunId #-}

-- | The source workflow execution that started this workflow execution. The
-- member is not set if the workflow execution was not started by a workflow.
weseaParentWorkflowExecution :: Lens' WorkflowExecutionStartedEventAttributes (Maybe WorkflowExecution)
weseaParentWorkflowExecution f x =
    f (_weseaParentWorkflowExecution x)
        <&> \y -> x { _weseaParentWorkflowExecution = y }
{-# INLINE weseaParentWorkflowExecution #-}

-- | The id of the StartChildWorkflowExecutionInitiated event corresponding to
-- the StartChildWorkflowExecution Decision to start this workflow execution.
-- The source event with this Id can be found in the history of the source
-- workflow execution. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
weseaParentInitiatedEventId :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Integer)
weseaParentInitiatedEventId f x =
    f (_weseaParentInitiatedEventId x)
        <&> \y -> x { _weseaParentInitiatedEventId = y }
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
weteaReason :: Lens' WorkflowExecutionTerminatedEventAttributes (Maybe Text)
weteaReason f x =
    f (_weteaReason x)
        <&> \y -> x { _weteaReason = y }
{-# INLINE weteaReason #-}

-- | The details provided for the termination (if any).
weteaDetails :: Lens' WorkflowExecutionTerminatedEventAttributes (Maybe Text)
weteaDetails f x =
    f (_weteaDetails x)
        <&> \y -> x { _weteaDetails = y }
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
weteaChildPolicy f x =
    f (_weteaChildPolicy x)
        <&> \y -> x { _weteaChildPolicy = y }
{-# INLINE weteaChildPolicy #-}

-- | If set, indicates that the workflow execution was automatically terminated,
-- and specifies the cause. This happens if the parent workflow execution
-- times out or is terminated and the child policy is set to terminate child
-- executions.
weteaCause :: Lens' WorkflowExecutionTerminatedEventAttributes (Maybe WorkflowExecutionTerminatedCause)
weteaCause f x =
    f (_weteaCause x)
        <&> \y -> x { _weteaCause = y }
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
wetoeaTimeoutType :: Lens' WorkflowExecutionTimedOutEventAttributes (WorkflowExecutionTimeoutType)
wetoeaTimeoutType f x =
    f (_wetoeaTimeoutType x)
        <&> \y -> x { _wetoeaTimeoutType = y }
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
wetoeaChildPolicy f x =
    f (_wetoeaChildPolicy x)
        <&> \y -> x { _wetoeaChildPolicy = y }
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
wtName :: Lens' WorkflowType (Text)
wtName f x =
    f (_wtName x)
        <&> \y -> x { _wtName = y }
{-# INLINE wtName #-}

-- | The version of the workflow type. This field is required. The combination
-- of workflow type name and version must be unique with in a domain.
wtVersion :: Lens' WorkflowType (Text)
wtVersion f x =
    f (_wtVersion x)
        <&> \y -> x { _wtVersion = y }
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
wtcDefaultTaskStartToCloseTimeout :: Lens' WorkflowTypeConfiguration (Maybe Text)
wtcDefaultTaskStartToCloseTimeout f x =
    f (_wtcDefaultTaskStartToCloseTimeout x)
        <&> \y -> x { _wtcDefaultTaskStartToCloseTimeout = y }
{-# INLINE wtcDefaultTaskStartToCloseTimeout #-}

-- | The optional default maximum duration, specified when registering the
-- workflow type, for executions of this workflow type. This default can be
-- overridden when starting a workflow execution using the
-- StartWorkflowExecution action or the StartChildWorkflowExecution Decision.
-- The valid values are integers greater than or equal to 0. An integer value
-- can be used to specify the duration in seconds while NONE can be used to
-- specify unlimited duration.
wtcDefaultExecutionStartToCloseTimeout :: Lens' WorkflowTypeConfiguration (Maybe Text)
wtcDefaultExecutionStartToCloseTimeout f x =
    f (_wtcDefaultExecutionStartToCloseTimeout x)
        <&> \y -> x { _wtcDefaultExecutionStartToCloseTimeout = y }
{-# INLINE wtcDefaultExecutionStartToCloseTimeout #-}

-- | The optional default task list, specified when registering the workflow
-- type, for decisions tasks scheduled for workflow executions of this type.
-- This default can be overridden when starting a workflow execution using the
-- StartWorkflowExecution action or the StartChildWorkflowExecution Decision.
wtcDefaultTaskList :: Lens' WorkflowTypeConfiguration (Maybe TaskList)
wtcDefaultTaskList f x =
    f (_wtcDefaultTaskList x)
        <&> \y -> x { _wtcDefaultTaskList = y }
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
wtcDefaultChildPolicy f x =
    f (_wtcDefaultChildPolicy x)
        <&> \y -> x { _wtcDefaultChildPolicy = y }
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
wtfName f x =
    f (_wtfName x)
        <&> \y -> x { _wtfName = y }
{-# INLINE wtfName #-}

-- | Version of the workflow type.
wtfVersion :: Lens' WorkflowTypeFilter (Maybe Text)
wtfVersion f x =
    f (_wtfVersion x)
        <&> \y -> x { _wtfVersion = y }
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
wtiWorkflowType :: Lens' WorkflowTypeInfo (WorkflowType)
wtiWorkflowType f x =
    f (_wtiWorkflowType x)
        <&> \y -> x { _wtiWorkflowType = y }
{-# INLINE wtiWorkflowType #-}

-- | The current status of the workflow type.
wtiStatus :: Lens' WorkflowTypeInfo (RegistrationStatus)
wtiStatus f x =
    f (_wtiStatus x)
        <&> \y -> x { _wtiStatus = y }
{-# INLINE wtiStatus #-}

-- | The description of the type registered through RegisterWorkflowType.
wtiDescription :: Lens' WorkflowTypeInfo (Maybe Text)
wtiDescription f x =
    f (_wtiDescription x)
        <&> \y -> x { _wtiDescription = y }
{-# INLINE wtiDescription #-}

-- | The date when this type was registered.
wtiCreationDate :: Lens' WorkflowTypeInfo (POSIX)
wtiCreationDate f x =
    f (_wtiCreationDate x)
        <&> \y -> x { _wtiCreationDate = y }
{-# INLINE wtiCreationDate #-}

-- | If the type is in deprecated state, then it is set to the date when the
-- type was deprecated.
wtiDeprecationDate :: Lens' WorkflowTypeInfo (Maybe POSIX)
wtiDeprecationDate f x =
    f (_wtiDeprecationDate x)
        <&> \y -> x { _wtiDeprecationDate = y }
{-# INLINE wtiDeprecationDate #-}

instance FromJSON WorkflowTypeInfo
