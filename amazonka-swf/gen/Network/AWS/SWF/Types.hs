{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
    -- ** Error
    , JSONError (..)

    -- * WorkflowExecutionCancelRequestedEventAttributes
    , WorkflowExecutionCancelRequestedEventAttributes
    , workflowExecutionCancelRequestedEventAttributes
    , wecreaCause
    , wecreaExternalInitiatedEventId
    , wecreaExternalWorkflowExecution

    -- * RequestCancelExternalWorkflowExecutionDecisionAttributes
    , RequestCancelExternalWorkflowExecutionDecisionAttributes
    , requestCancelExternalWorkflowExecutionDecisionAttributes
    , rcewedaControl
    , rcewedaRunId
    , rcewedaWorkflowId

    -- * DecisionTaskScheduledEventAttributes
    , DecisionTaskScheduledEventAttributes
    , decisionTaskScheduledEventAttributes
    , dtseaStartToCloseTimeout
    , dtseaTaskList

    -- * WorkflowExecutionCompletedEventAttributes
    , WorkflowExecutionCompletedEventAttributes
    , workflowExecutionCompletedEventAttributes
    , weceaDecisionTaskCompletedEventId
    , weceaResult

    -- * ExecutionTimeFilter
    , ExecutionTimeFilter
    , executionTimeFilter
    , etfLatestDate
    , etfOldestDate

    -- * StartTimerFailedEventAttributes
    , StartTimerFailedEventAttributes
    , startTimerFailedEventAttributes
    , stfeaCause
    , stfeaDecisionTaskCompletedEventId
    , stfeaTimerId

    -- * RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , requestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , rceweieaControl
    , rceweieaDecisionTaskCompletedEventId
    , rceweieaRunId
    , rceweieaWorkflowId

    -- * RecordMarkerFailedEventAttributes
    , RecordMarkerFailedEventAttributes
    , recordMarkerFailedEventAttributes
    , rmfeaCause
    , rmfeaDecisionTaskCompletedEventId
    , rmfeaMarkerName

    -- * WorkflowExecutionCount
    , WorkflowExecutionCount
    , workflowExecutionCount
    , wecCount
    , wecTruncated

    -- * ActivityTaskScheduledEventAttributes
    , ActivityTaskScheduledEventAttributes
    , activityTaskScheduledEventAttributes
    , atseaActivityId
    , atseaActivityType
    , atseaControl
    , atseaDecisionTaskCompletedEventId
    , atseaHeartbeatTimeout
    , atseaInput
    , atseaScheduleToCloseTimeout
    , atseaScheduleToStartTimeout
    , atseaStartToCloseTimeout
    , atseaTaskList

    -- * CloseStatusFilter
    , CloseStatusFilter
    , closeStatusFilter
    , csfStatus

    -- * WorkflowExecutionTimeoutType
    , WorkflowExecutionTimeoutType (..)

    -- * ScheduleActivityTaskDecisionAttributes
    , ScheduleActivityTaskDecisionAttributes
    , scheduleActivityTaskDecisionAttributes
    , satdaActivityId
    , satdaActivityType
    , satdaControl
    , satdaHeartbeatTimeout
    , satdaInput
    , satdaScheduleToCloseTimeout
    , satdaScheduleToStartTimeout
    , satdaStartToCloseTimeout
    , satdaTaskList

    -- * ActivityTypeConfiguration
    , ActivityTypeConfiguration
    , activityTypeConfiguration
    , atcDefaultTaskHeartbeatTimeout
    , atcDefaultTaskList
    , atcDefaultTaskScheduleToCloseTimeout
    , atcDefaultTaskScheduleToStartTimeout
    , atcDefaultTaskStartToCloseTimeout

    -- * ActivityType
    , ActivityType
    , activityType
    , atName
    , atVersion

    -- * WorkflowTypeInfo
    , WorkflowTypeInfo
    , workflowTypeInfo
    , wtiCreationDate
    , wtiDeprecationDate
    , wtiDescription
    , wtiStatus
    , wtiWorkflowType

    -- * ChildWorkflowExecutionCompletedEventAttributes
    , ChildWorkflowExecutionCompletedEventAttributes
    , childWorkflowExecutionCompletedEventAttributes
    , cweceaInitiatedEventId
    , cweceaResult
    , cweceaStartedEventId
    , cweceaWorkflowExecution
    , cweceaWorkflowType

    -- * WorkflowExecutionOpenCounts
    , WorkflowExecutionOpenCounts
    , workflowExecutionOpenCounts
    , weocOpenActivityTasks
    , weocOpenChildWorkflowExecutions
    , weocOpenDecisionTasks
    , weocOpenTimers

    -- * RequestCancelActivityTaskFailedCause
    , RequestCancelActivityTaskFailedCause (..)

    -- * ScheduleActivityTaskFailedEventAttributes
    , ScheduleActivityTaskFailedEventAttributes
    , scheduleActivityTaskFailedEventAttributes
    , satfeaActivityId
    , satfeaActivityType
    , satfeaCause
    , satfeaDecisionTaskCompletedEventId

    -- * MarkerRecordedEventAttributes
    , MarkerRecordedEventAttributes
    , markerRecordedEventAttributes
    , mreaDecisionTaskCompletedEventId
    , mreaDetails
    , mreaMarkerName

    -- * SignalExternalWorkflowExecutionDecisionAttributes
    , SignalExternalWorkflowExecutionDecisionAttributes
    , signalExternalWorkflowExecutionDecisionAttributes
    , sewedaControl
    , sewedaInput
    , sewedaRunId
    , sewedaSignalName
    , sewedaWorkflowId

    -- * WorkflowExecutionTerminatedCause
    , WorkflowExecutionTerminatedCause (..)

    -- * CancelWorkflowExecutionFailedCause
    , CancelWorkflowExecutionFailedCause (..)

    -- * SignalExternalWorkflowExecutionFailedCause
    , SignalExternalWorkflowExecutionFailedCause (..)

    -- * RecordMarkerDecisionAttributes
    , RecordMarkerDecisionAttributes
    , recordMarkerDecisionAttributes
    , rmdaDetails
    , rmdaMarkerName

    -- * CompleteWorkflowExecutionFailedEventAttributes
    , CompleteWorkflowExecutionFailedEventAttributes
    , completeWorkflowExecutionFailedEventAttributes
    , cwefeaCause
    , cwefeaDecisionTaskCompletedEventId

    -- * StartTimerDecisionAttributes
    , StartTimerDecisionAttributes
    , startTimerDecisionAttributes
    , stdaControl
    , stdaStartToFireTimeout
    , stdaTimerId

    -- * DecisionType
    , DecisionType (..)

    -- * RequestCancelExternalWorkflowExecutionFailedEventAttributes
    , RequestCancelExternalWorkflowExecutionFailedEventAttributes
    , requestCancelExternalWorkflowExecutionFailedEventAttributes
    , rcewefeaCause
    , rcewefeaControl
    , rcewefeaDecisionTaskCompletedEventId
    , rcewefeaInitiatedEventId
    , rcewefeaRunId
    , rcewefeaWorkflowId

    -- * ActivityTypeInfo
    , ActivityTypeInfo
    , activityTypeInfo
    , atiActivityType
    , atiCreationDate
    , atiDeprecationDate
    , atiDescription
    , atiStatus

    -- * TimerCanceledEventAttributes
    , TimerCanceledEventAttributes
    , timerCanceledEventAttributes
    , tceaDecisionTaskCompletedEventId
    , tceaStartedEventId
    , tceaTimerId

    -- * WorkflowExecutionStartedEventAttributes
    , WorkflowExecutionStartedEventAttributes
    , workflowExecutionStartedEventAttributes
    , weseaChildPolicy
    , weseaContinuedExecutionRunId
    , weseaExecutionStartToCloseTimeout
    , weseaInput
    , weseaParentInitiatedEventId
    , weseaParentWorkflowExecution
    , weseaTagList
    , weseaTaskList
    , weseaTaskStartToCloseTimeout
    , weseaWorkflowType

    -- * WorkflowTypeConfiguration
    , WorkflowTypeConfiguration
    , workflowTypeConfiguration
    , wtcDefaultChildPolicy
    , wtcDefaultExecutionStartToCloseTimeout
    , wtcDefaultTaskList
    , wtcDefaultTaskStartToCloseTimeout

    -- * ActivityTaskTimeoutType
    , ActivityTaskTimeoutType (..)

    -- * WorkflowType
    , WorkflowType
    , workflowType
    , wtName
    , wtVersion

    -- * ActivityTaskCompletedEventAttributes
    , ActivityTaskCompletedEventAttributes
    , activityTaskCompletedEventAttributes
    , atceaResult
    , atceaScheduledEventId
    , atceaStartedEventId

    -- * ExecutionStatus
    , ExecutionStatus (..)

    -- * DecisionTaskTimeoutType
    , DecisionTaskTimeoutType (..)

    -- * WorkflowExecutionCancelRequestedCause
    , WorkflowExecutionCancelRequestedCause (..)

    -- * StartChildWorkflowExecutionFailedCause
    , StartChildWorkflowExecutionFailedCause (..)

    -- * DecisionTaskTimedOutEventAttributes
    , DecisionTaskTimedOutEventAttributes
    , decisionTaskTimedOutEventAttributes
    , dttoeaScheduledEventId
    , dttoeaStartedEventId
    , dttoeaTimeoutType

    -- * ChildWorkflowExecutionStartedEventAttributes
    , ChildWorkflowExecutionStartedEventAttributes
    , childWorkflowExecutionStartedEventAttributes
    , cweseaInitiatedEventId
    , cweseaWorkflowExecution
    , cweseaWorkflowType

    -- * CancelTimerFailedEventAttributes
    , CancelTimerFailedEventAttributes
    , cancelTimerFailedEventAttributes
    , ctfeaCause
    , ctfeaDecisionTaskCompletedEventId
    , ctfeaTimerId

    -- * FailWorkflowExecutionFailedCause
    , FailWorkflowExecutionFailedCause (..)

    -- * WorkflowExecutionFilter
    , WorkflowExecutionFilter
    , workflowExecutionFilter
    , wefWorkflowId

    -- * ActivityTaskCanceledEventAttributes
    , ActivityTaskCanceledEventAttributes
    , activityTaskCanceledEventAttributes
    , atcea1Details
    , atcea1LatestCancelRequestedEventId
    , atcea1ScheduledEventId
    , atcea1StartedEventId

    -- * WorkflowExecutionInfos
    , WorkflowExecutionInfos
    , workflowExecutionInfos
    , weiExecutionInfos
    , weiNextPageToken

    -- * StartChildWorkflowExecutionDecisionAttributes
    , StartChildWorkflowExecutionDecisionAttributes
    , startChildWorkflowExecutionDecisionAttributes
    , scwedaChildPolicy
    , scwedaControl
    , scwedaExecutionStartToCloseTimeout
    , scwedaInput
    , scwedaTagList
    , scwedaTaskList
    , scwedaTaskStartToCloseTimeout
    , scwedaWorkflowId
    , scwedaWorkflowType

    -- * ContinueAsNewWorkflowExecutionFailedCause
    , ContinueAsNewWorkflowExecutionFailedCause (..)

    -- * FailWorkflowExecutionDecisionAttributes
    , FailWorkflowExecutionDecisionAttributes
    , failWorkflowExecutionDecisionAttributes
    , fwedaDetails
    , fwedaReason

    -- * EventType
    , EventType (..)

    -- * ActivityTaskTimedOutEventAttributes
    , ActivityTaskTimedOutEventAttributes
    , activityTaskTimedOutEventAttributes
    , attoeaDetails
    , attoeaScheduledEventId
    , attoeaStartedEventId
    , attoeaTimeoutType

    -- * RequestCancelActivityTaskFailedEventAttributes
    , RequestCancelActivityTaskFailedEventAttributes
    , requestCancelActivityTaskFailedEventAttributes
    , rcatfeaActivityId
    , rcatfeaCause
    , rcatfeaDecisionTaskCompletedEventId

    -- * CompleteWorkflowExecutionDecisionAttributes
    , CompleteWorkflowExecutionDecisionAttributes
    , completeWorkflowExecutionDecisionAttributes
    , cwedaResult

    -- * DecisionTaskStartedEventAttributes
    , DecisionTaskStartedEventAttributes
    , decisionTaskStartedEventAttributes
    , dtseaIdentity
    , dtseaScheduledEventId

    -- * ChildWorkflowExecutionTimedOutEventAttributes
    , ChildWorkflowExecutionTimedOutEventAttributes
    , childWorkflowExecutionTimedOutEventAttributes
    , cwetoeaInitiatedEventId
    , cwetoeaStartedEventId
    , cwetoeaTimeoutType
    , cwetoeaWorkflowExecution
    , cwetoeaWorkflowType

    -- * StartChildWorkflowExecutionInitiatedEventAttributes
    , StartChildWorkflowExecutionInitiatedEventAttributes
    , startChildWorkflowExecutionInitiatedEventAttributes
    , scweieaChildPolicy
    , scweieaControl
    , scweieaDecisionTaskCompletedEventId
    , scweieaExecutionStartToCloseTimeout
    , scweieaInput
    , scweieaTagList
    , scweieaTaskList
    , scweieaTaskStartToCloseTimeout
    , scweieaWorkflowId
    , scweieaWorkflowType

    -- * CancelWorkflowExecutionFailedEventAttributes
    , CancelWorkflowExecutionFailedEventAttributes
    , cancelWorkflowExecutionFailedEventAttributes
    , cwefea1Cause
    , cwefea1DecisionTaskCompletedEventId

    -- * WorkflowExecutionTerminatedEventAttributes
    , WorkflowExecutionTerminatedEventAttributes
    , workflowExecutionTerminatedEventAttributes
    , weteaCause
    , weteaChildPolicy
    , weteaDetails
    , weteaReason

    -- * TaskList
    , TaskList
    , taskList
    , tlName

    -- * ScheduleActivityTaskFailedCause
    , ScheduleActivityTaskFailedCause (..)

    -- * ChildWorkflowExecutionCanceledEventAttributes
    , ChildWorkflowExecutionCanceledEventAttributes
    , childWorkflowExecutionCanceledEventAttributes
    , cwecea1Details
    , cwecea1InitiatedEventId
    , cwecea1StartedEventId
    , cwecea1WorkflowExecution
    , cwecea1WorkflowType

    -- * WorkflowExecutionInfo
    , WorkflowExecutionInfo
    , workflowExecutionInfo
    , weiCancelRequested
    , weiCloseStatus
    , weiCloseTimestamp
    , weiExecution
    , weiExecutionStatus
    , weiParent
    , weiStartTimestamp
    , weiTagList
    , weiWorkflowType

    -- * SignalExternalWorkflowExecutionFailedEventAttributes
    , SignalExternalWorkflowExecutionFailedEventAttributes
    , signalExternalWorkflowExecutionFailedEventAttributes
    , sewefeaCause
    , sewefeaControl
    , sewefeaDecisionTaskCompletedEventId
    , sewefeaInitiatedEventId
    , sewefeaRunId
    , sewefeaWorkflowId

    -- * TagFilter
    , TagFilter
    , tagFilter
    , tfTag

    -- * ChildPolicy
    , ChildPolicy (..)

    -- * ActivityTaskStartedEventAttributes
    , ActivityTaskStartedEventAttributes
    , activityTaskStartedEventAttributes
    , atseaIdentity
    , atseaScheduledEventId

    -- * CloseStatus
    , CloseStatus (..)

    -- * CompleteWorkflowExecutionFailedCause
    , CompleteWorkflowExecutionFailedCause (..)

    -- * StartTimerFailedCause
    , StartTimerFailedCause (..)

    -- * ActivityTaskCancelRequestedEventAttributes
    , ActivityTaskCancelRequestedEventAttributes
    , activityTaskCancelRequestedEventAttributes
    , atcreaActivityId
    , atcreaDecisionTaskCompletedEventId

    -- * WorkflowExecutionTimedOutEventAttributes
    , WorkflowExecutionTimedOutEventAttributes
    , workflowExecutionTimedOutEventAttributes
    , wetoeaChildPolicy
    , wetoeaTimeoutType

    -- * ChildWorkflowExecutionTerminatedEventAttributes
    , ChildWorkflowExecutionTerminatedEventAttributes
    , childWorkflowExecutionTerminatedEventAttributes
    , cweteaInitiatedEventId
    , cweteaStartedEventId
    , cweteaWorkflowExecution
    , cweteaWorkflowType

    -- * WorkflowExecutionCanceledEventAttributes
    , WorkflowExecutionCanceledEventAttributes
    , workflowExecutionCanceledEventAttributes
    , wecea1DecisionTaskCompletedEventId
    , wecea1Details

    -- * WorkflowExecutionSignaledEventAttributes
    , WorkflowExecutionSignaledEventAttributes
    , workflowExecutionSignaledEventAttributes
    , wesea1ExternalInitiatedEventId
    , wesea1ExternalWorkflowExecution
    , wesea1Input
    , wesea1SignalName

    -- * RecordMarkerFailedCause
    , RecordMarkerFailedCause (..)

    -- * RegistrationStatus
    , RegistrationStatus (..)

    -- * TimerStartedEventAttributes
    , TimerStartedEventAttributes
    , timerStartedEventAttributes
    , tseaControl
    , tseaDecisionTaskCompletedEventId
    , tseaStartToFireTimeout
    , tseaTimerId

    -- * RequestCancelActivityTaskDecisionAttributes
    , RequestCancelActivityTaskDecisionAttributes
    , requestCancelActivityTaskDecisionAttributes
    , rcatdaActivityId

    -- * Decision
    , Decision
    , decision
    , dCancelTimerDecisionAttributes
    , dCancelWorkflowExecutionDecisionAttributes
    , dCompleteWorkflowExecutionDecisionAttributes
    , dContinueAsNewWorkflowExecutionDecisionAttributes
    , dDecisionType
    , dFailWorkflowExecutionDecisionAttributes
    , dRecordMarkerDecisionAttributes
    , dRequestCancelActivityTaskDecisionAttributes
    , dRequestCancelExternalWorkflowExecutionDecisionAttributes
    , dScheduleActivityTaskDecisionAttributes
    , dSignalExternalWorkflowExecutionDecisionAttributes
    , dStartChildWorkflowExecutionDecisionAttributes
    , dStartTimerDecisionAttributes

    -- * TimerFiredEventAttributes
    , TimerFiredEventAttributes
    , timerFiredEventAttributes
    , tfeaStartedEventId
    , tfeaTimerId

    -- * DomainConfiguration
    , DomainConfiguration
    , domainConfiguration
    , dcWorkflowExecutionRetentionPeriodInDays

    -- * ExternalWorkflowExecutionSignaledEventAttributes
    , ExternalWorkflowExecutionSignaledEventAttributes
    , externalWorkflowExecutionSignaledEventAttributes
    , eweseaInitiatedEventId
    , eweseaWorkflowExecution

    -- * CancelWorkflowExecutionDecisionAttributes
    , CancelWorkflowExecutionDecisionAttributes
    , cancelWorkflowExecutionDecisionAttributes
    , cwedaDetails

    -- * ActivityTaskFailedEventAttributes
    , ActivityTaskFailedEventAttributes
    , activityTaskFailedEventAttributes
    , atfeaDetails
    , atfeaReason
    , atfeaScheduledEventId
    , atfeaStartedEventId

    -- * FailWorkflowExecutionFailedEventAttributes
    , FailWorkflowExecutionFailedEventAttributes
    , failWorkflowExecutionFailedEventAttributes
    , fwefeaCause
    , fwefeaDecisionTaskCompletedEventId

    -- * StartChildWorkflowExecutionFailedEventAttributes
    , StartChildWorkflowExecutionFailedEventAttributes
    , startChildWorkflowExecutionFailedEventAttributes
    , scwefeaCause
    , scwefeaControl
    , scwefeaDecisionTaskCompletedEventId
    , scwefeaInitiatedEventId
    , scwefeaWorkflowId
    , scwefeaWorkflowType

    -- * WorkflowTypeFilter
    , WorkflowTypeFilter
    , workflowTypeFilter
    , wtfName
    , wtfVersion

    -- * CancelTimerFailedCause
    , CancelTimerFailedCause (..)

    -- * DecisionTaskCompletedEventAttributes
    , DecisionTaskCompletedEventAttributes
    , decisionTaskCompletedEventAttributes
    , dtceaExecutionContext
    , dtceaScheduledEventId
    , dtceaStartedEventId

    -- * ChildWorkflowExecutionFailedEventAttributes
    , ChildWorkflowExecutionFailedEventAttributes
    , childWorkflowExecutionFailedEventAttributes
    , cwefeaDetails
    , cwefeaInitiatedEventId
    , cwefeaReason
    , cwefeaStartedEventId
    , cwefeaWorkflowExecution
    , cwefeaWorkflowType

    -- * DomainInfo
    , DomainInfo
    , domainInfo
    , diDescription
    , diName
    , diStatus

    -- * HistoryEvent
    , HistoryEvent
    , historyEvent
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
    , heEventId
    , heEventTimestamp
    , heEventType
    , heExternalWorkflowExecutionCancelRequestedEventAttributes
    , heExternalWorkflowExecutionSignaledEventAttributes
    , heFailWorkflowExecutionFailedEventAttributes
    , heMarkerRecordedEventAttributes
    , heRecordMarkerFailedEventAttributes
    , heRequestCancelActivityTaskFailedEventAttributes
    , heRequestCancelExternalWorkflowExecutionFailedEventAttributes
    , heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , heScheduleActivityTaskFailedEventAttributes
    , heSignalExternalWorkflowExecutionFailedEventAttributes
    , heSignalExternalWorkflowExecutionInitiatedEventAttributes
    , heStartChildWorkflowExecutionFailedEventAttributes
    , heStartChildWorkflowExecutionInitiatedEventAttributes
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

    -- * ContinueAsNewWorkflowExecutionFailedEventAttributes
    , ContinueAsNewWorkflowExecutionFailedEventAttributes
    , continueAsNewWorkflowExecutionFailedEventAttributes
    , canwefeaCause
    , canwefeaDecisionTaskCompletedEventId

    -- * SignalExternalWorkflowExecutionInitiatedEventAttributes
    , SignalExternalWorkflowExecutionInitiatedEventAttributes
    , signalExternalWorkflowExecutionInitiatedEventAttributes
    , seweieaControl
    , seweieaDecisionTaskCompletedEventId
    , seweieaInput
    , seweieaRunId
    , seweieaSignalName
    , seweieaWorkflowId

    -- * CancelTimerDecisionAttributes
    , CancelTimerDecisionAttributes
    , cancelTimerDecisionAttributes
    , ctdaTimerId

    -- * WorkflowExecutionFailedEventAttributes
    , WorkflowExecutionFailedEventAttributes
    , workflowExecutionFailedEventAttributes
    , wefeaDecisionTaskCompletedEventId
    , wefeaDetails
    , wefeaReason

    -- * WorkflowExecutionConfiguration
    , WorkflowExecutionConfiguration
    , workflowExecutionConfiguration
    , wecChildPolicy
    , wecExecutionStartToCloseTimeout
    , wecTaskList
    , wecTaskStartToCloseTimeout

    -- * WorkflowExecution
    , WorkflowExecution
    , workflowExecution
    , weRunId
    , weWorkflowId

    -- * RequestCancelExternalWorkflowExecutionFailedCause
    , RequestCancelExternalWorkflowExecutionFailedCause (..)

    -- * ContinueAsNewWorkflowExecutionDecisionAttributes
    , ContinueAsNewWorkflowExecutionDecisionAttributes
    , continueAsNewWorkflowExecutionDecisionAttributes
    , canwedaChildPolicy
    , canwedaExecutionStartToCloseTimeout
    , canwedaInput
    , canwedaTagList
    , canwedaTaskList
    , canwedaTaskStartToCloseTimeout
    , canwedaWorkflowTypeVersion

    -- * ExternalWorkflowExecutionCancelRequestedEventAttributes
    , ExternalWorkflowExecutionCancelRequestedEventAttributes
    , externalWorkflowExecutionCancelRequestedEventAttributes
    , ewecreaInitiatedEventId
    , ewecreaWorkflowExecution

    -- * PendingTaskCount
    , PendingTaskCount
    , pendingTaskCount
    , ptcCount
    , ptcTruncated

    -- * WorkflowExecutionContinuedAsNewEventAttributes
    , WorkflowExecutionContinuedAsNewEventAttributes
    , workflowExecutionContinuedAsNewEventAttributes
    , wecaneaChildPolicy
    , wecaneaDecisionTaskCompletedEventId
    , wecaneaExecutionStartToCloseTimeout
    , wecaneaInput
    , wecaneaNewExecutionRunId
    , wecaneaTagList
    , wecaneaTaskList
    , wecaneaTaskStartToCloseTimeout
    , wecaneaWorkflowType
    ) where

import Data.Char (isUpper)
import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2012-01-25@ of the Amazon Simple Workflow Service service.
data SWF

instance AWSService SWF where
    type Sg SWF = V4
    type Er SWF = JSONError

    service = Service
        { _svcAbbrev       = "SWF"
        , _svcPrefix       = "swf"
        , _svcVersion      = "2012-01-25"
        , _svcTargetPrefix = Just "SimpleWorkflowService"
        , _svcJSONVersion  = Just "1.0"
        , _svcHandle       = jsonError statusSuccess
        , _svcDelay        = delay
        , _svcRetry        = retry
        }
    {-# INLINE service #-}

delay :: Delay
delay = Exp 0.05 2 5
{-# INLINE delay #-}

retry :: AWSErrorCode -> Status -> a -> Bool
retry (statusCode -> s) (awsErrorCode -> e)
    | s == 500  = True -- General Server Error
    | s == 509  = True -- Limit Exceeded
    | s == 503  = True -- Service Unavailable
    | s == 400  = "Throttling" == e -- Throttling
    | otherwise = False
{-# INLINE retry #-}

data WorkflowExecutionCancelRequestedEventAttributes = WorkflowExecutionCancelRequestedEventAttributes
    { _wecreaCause                     :: Maybe WorkflowExecutionCancelRequestedCause
    , _wecreaExternalInitiatedEventId  :: Maybe Integer
    , _wecreaExternalWorkflowExecution :: Maybe WorkflowExecution
    } deriving (Eq, Show)

-- | 'WorkflowExecutionCancelRequestedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wecreaCause' @::@ 'Maybe' 'WorkflowExecutionCancelRequestedCause'
--
-- * 'wecreaExternalInitiatedEventId' @::@ 'Maybe' 'Integer'
--
-- * 'wecreaExternalWorkflowExecution' @::@ 'Maybe' 'WorkflowExecution'
--
workflowExecutionCancelRequestedEventAttributes :: WorkflowExecutionCancelRequestedEventAttributes
workflowExecutionCancelRequestedEventAttributes = WorkflowExecutionCancelRequestedEventAttributes
    { _wecreaExternalWorkflowExecution = Nothing
    , _wecreaExternalInitiatedEventId  = Nothing
    , _wecreaCause                     = Nothing
    }

-- | If set, indicates that the request to cancel the workflow execution was
-- automatically generated, and specifies the cause. This happens if the parent
-- workflow execution times out or is terminated, and the child policy is set to
-- cancel child executions.
wecreaCause :: Lens' WorkflowExecutionCancelRequestedEventAttributes (Maybe WorkflowExecutionCancelRequestedCause)
wecreaCause = lens _wecreaCause (\s a -> s { _wecreaCause = a })

-- | The id of the 'RequestCancelExternalWorkflowExecutionInitiated' event
-- corresponding to the 'RequestCancelExternalWorkflowExecution' decision to
-- cancel this workflow execution.The source event with this Id can be found in
-- the history of the source workflow execution. This information can be useful
-- for diagnosing problems by tracing back the chain of events leading up to
-- this event.
wecreaExternalInitiatedEventId :: Lens' WorkflowExecutionCancelRequestedEventAttributes (Maybe Integer)
wecreaExternalInitiatedEventId =
    lens _wecreaExternalInitiatedEventId
        (\s a -> s { _wecreaExternalInitiatedEventId = a })

-- | The external workflow execution for which the cancellation was requested.
wecreaExternalWorkflowExecution :: Lens' WorkflowExecutionCancelRequestedEventAttributes (Maybe WorkflowExecution)
wecreaExternalWorkflowExecution =
    lens _wecreaExternalWorkflowExecution
        (\s a -> s { _wecreaExternalWorkflowExecution = a })

instance FromJSON WorkflowExecutionCancelRequestedEventAttributes where
    parseJSON = withObject "WorkflowExecutionCancelRequestedEventAttributes" $ \o -> WorkflowExecutionCancelRequestedEventAttributes
        <$> o .:? "cause"
        <*> o .:? "externalInitiatedEventId"
        <*> o .:? "externalWorkflowExecution"

instance ToJSON WorkflowExecutionCancelRequestedEventAttributes where
    toJSON WorkflowExecutionCancelRequestedEventAttributes{..} = object
        [ "externalWorkflowExecution" .= _wecreaExternalWorkflowExecution
        , "externalInitiatedEventId"  .= _wecreaExternalInitiatedEventId
        , "cause"                     .= _wecreaCause
        ]

data RequestCancelExternalWorkflowExecutionDecisionAttributes = RequestCancelExternalWorkflowExecutionDecisionAttributes
    { _rcewedaControl    :: Maybe Text
    , _rcewedaRunId      :: Maybe Text
    , _rcewedaWorkflowId :: Text
    } deriving (Eq, Ord, Show)

-- | 'RequestCancelExternalWorkflowExecutionDecisionAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcewedaControl' @::@ 'Maybe' 'Text'
--
-- * 'rcewedaRunId' @::@ 'Maybe' 'Text'
--
-- * 'rcewedaWorkflowId' @::@ 'Text'
--
requestCancelExternalWorkflowExecutionDecisionAttributes :: Text -- ^ 'rcewedaWorkflowId'
                                                         -> RequestCancelExternalWorkflowExecutionDecisionAttributes
requestCancelExternalWorkflowExecutionDecisionAttributes p1 = RequestCancelExternalWorkflowExecutionDecisionAttributes
    { _rcewedaWorkflowId = p1
    , _rcewedaRunId      = Nothing
    , _rcewedaControl    = Nothing
    }

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
rcewedaControl :: Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Maybe Text)
rcewedaControl = lens _rcewedaControl (\s a -> s { _rcewedaControl = a })

-- | The 'runId' of the external workflow execution to cancel.
rcewedaRunId :: Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Maybe Text)
rcewedaRunId = lens _rcewedaRunId (\s a -> s { _rcewedaRunId = a })

-- | The 'workflowId' of the external workflow execution to cancel. This field is
-- required.
rcewedaWorkflowId :: Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes Text
rcewedaWorkflowId =
    lens _rcewedaWorkflowId (\s a -> s { _rcewedaWorkflowId = a })

instance FromJSON RequestCancelExternalWorkflowExecutionDecisionAttributes where
    parseJSON = withObject "RequestCancelExternalWorkflowExecutionDecisionAttributes" $ \o -> RequestCancelExternalWorkflowExecutionDecisionAttributes
        <$> o .:? "control"
        <*> o .:? "runId"
        <*> o .:  "workflowId"

instance ToJSON RequestCancelExternalWorkflowExecutionDecisionAttributes where
    toJSON RequestCancelExternalWorkflowExecutionDecisionAttributes{..} = object
        [ "workflowId" .= _rcewedaWorkflowId
        , "runId"      .= _rcewedaRunId
        , "control"    .= _rcewedaControl
        ]

data DecisionTaskScheduledEventAttributes = DecisionTaskScheduledEventAttributes
    { _dtseaStartToCloseTimeout :: Maybe Text
    , _dtseaTaskList            :: TaskList
    } deriving (Eq, Show)

-- | 'DecisionTaskScheduledEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtseaStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'dtseaTaskList' @::@ 'TaskList'
--
decisionTaskScheduledEventAttributes :: TaskList -- ^ 'dtseaTaskList'
                                     -> DecisionTaskScheduledEventAttributes
decisionTaskScheduledEventAttributes p1 = DecisionTaskScheduledEventAttributes
    { _dtseaTaskList            = p1
    , _dtseaStartToCloseTimeout = Nothing
    }

-- | The maximum duration for this decision task. The task is considered timed
-- out if it does not completed within this duration.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
dtseaStartToCloseTimeout :: Lens' DecisionTaskScheduledEventAttributes (Maybe Text)
dtseaStartToCloseTimeout =
    lens _dtseaStartToCloseTimeout
        (\s a -> s { _dtseaStartToCloseTimeout = a })

-- | The name of the task list in which the decision task was scheduled.
dtseaTaskList :: Lens' DecisionTaskScheduledEventAttributes TaskList
dtseaTaskList = lens _dtseaTaskList (\s a -> s { _dtseaTaskList = a })

instance FromJSON DecisionTaskScheduledEventAttributes where
    parseJSON = withObject "DecisionTaskScheduledEventAttributes" $ \o -> DecisionTaskScheduledEventAttributes
        <$> o .:? "startToCloseTimeout"
        <*> o .:  "taskList"

instance ToJSON DecisionTaskScheduledEventAttributes where
    toJSON DecisionTaskScheduledEventAttributes{..} = object
        [ "taskList"            .= _dtseaTaskList
        , "startToCloseTimeout" .= _dtseaStartToCloseTimeout
        ]

data WorkflowExecutionCompletedEventAttributes = WorkflowExecutionCompletedEventAttributes
    { _weceaDecisionTaskCompletedEventId :: Integer
    , _weceaResult                       :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'WorkflowExecutionCompletedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'weceaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'weceaResult' @::@ 'Maybe' 'Text'
--
workflowExecutionCompletedEventAttributes :: Integer -- ^ 'weceaDecisionTaskCompletedEventId'
                                          -> WorkflowExecutionCompletedEventAttributes
workflowExecutionCompletedEventAttributes p1 = WorkflowExecutionCompletedEventAttributes
    { _weceaDecisionTaskCompletedEventId = p1
    , _weceaResult                       = Nothing
    }

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'CompleteWorkflowExecution' decision to complete this
-- execution. This information can be useful for diagnosing problems by tracing
-- back the cause of events.
weceaDecisionTaskCompletedEventId :: Lens' WorkflowExecutionCompletedEventAttributes Integer
weceaDecisionTaskCompletedEventId =
    lens _weceaDecisionTaskCompletedEventId
        (\s a -> s { _weceaDecisionTaskCompletedEventId = a })

-- | The result produced by the workflow execution upon successful completion.
weceaResult :: Lens' WorkflowExecutionCompletedEventAttributes (Maybe Text)
weceaResult = lens _weceaResult (\s a -> s { _weceaResult = a })

instance FromJSON WorkflowExecutionCompletedEventAttributes where
    parseJSON = withObject "WorkflowExecutionCompletedEventAttributes" $ \o -> WorkflowExecutionCompletedEventAttributes
        <$> o .:  "decisionTaskCompletedEventId"
        <*> o .:? "result"

instance ToJSON WorkflowExecutionCompletedEventAttributes where
    toJSON WorkflowExecutionCompletedEventAttributes{..} = object
        [ "result"                       .= _weceaResult
        , "decisionTaskCompletedEventId" .= _weceaDecisionTaskCompletedEventId
        ]

data ExecutionTimeFilter = ExecutionTimeFilter
    { _etfLatestDate :: Maybe POSIX
    , _etfOldestDate :: POSIX
    } deriving (Eq, Ord, Show)

-- | 'ExecutionTimeFilter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etfLatestDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'etfOldestDate' @::@ 'UTCTime'
--
executionTimeFilter :: UTCTime -- ^ 'etfOldestDate'
                    -> ExecutionTimeFilter
executionTimeFilter p1 = ExecutionTimeFilter
    { _etfOldestDate = withIso _Time (const id) p1
    , _etfLatestDate = Nothing
    }

-- | Specifies the latest start or close date and time to return.
etfLatestDate :: Lens' ExecutionTimeFilter (Maybe UTCTime)
etfLatestDate = lens _etfLatestDate (\s a -> s { _etfLatestDate = a }) . mapping _Time

-- | Specifies the oldest start or close date and time to return.
etfOldestDate :: Lens' ExecutionTimeFilter UTCTime
etfOldestDate = lens _etfOldestDate (\s a -> s { _etfOldestDate = a }) . _Time

instance FromJSON ExecutionTimeFilter where
    parseJSON = withObject "ExecutionTimeFilter" $ \o -> ExecutionTimeFilter
        <$> o .:? "latestDate"
        <*> o .:  "oldestDate"

instance ToJSON ExecutionTimeFilter where
    toJSON ExecutionTimeFilter{..} = object
        [ "oldestDate" .= _etfOldestDate
        , "latestDate" .= _etfLatestDate
        ]

data StartTimerFailedEventAttributes = StartTimerFailedEventAttributes
    { _stfeaCause                        :: StartTimerFailedCause
    , _stfeaDecisionTaskCompletedEventId :: Integer
    , _stfeaTimerId                      :: Text
    } deriving (Eq, Show)

-- | 'StartTimerFailedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stfeaCause' @::@ 'StartTimerFailedCause'
--
-- * 'stfeaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'stfeaTimerId' @::@ 'Text'
--
startTimerFailedEventAttributes :: Text -- ^ 'stfeaTimerId'
                                -> StartTimerFailedCause -- ^ 'stfeaCause'
                                -> Integer -- ^ 'stfeaDecisionTaskCompletedEventId'
                                -> StartTimerFailedEventAttributes
startTimerFailedEventAttributes p1 p2 p3 = StartTimerFailedEventAttributes
    { _stfeaTimerId                      = p1
    , _stfeaCause                        = p2
    , _stfeaDecisionTaskCompletedEventId = p3
    }

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes.
stfeaCause :: Lens' StartTimerFailedEventAttributes StartTimerFailedCause
stfeaCause = lens _stfeaCause (\s a -> s { _stfeaCause = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'StartTimer' decision for this activity task. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
stfeaDecisionTaskCompletedEventId :: Lens' StartTimerFailedEventAttributes Integer
stfeaDecisionTaskCompletedEventId =
    lens _stfeaDecisionTaskCompletedEventId
        (\s a -> s { _stfeaDecisionTaskCompletedEventId = a })

-- | The timerId provided in the 'StartTimer' decision that failed.
stfeaTimerId :: Lens' StartTimerFailedEventAttributes Text
stfeaTimerId = lens _stfeaTimerId (\s a -> s { _stfeaTimerId = a })

instance FromJSON StartTimerFailedEventAttributes where
    parseJSON = withObject "StartTimerFailedEventAttributes" $ \o -> StartTimerFailedEventAttributes
        <$> o .:  "cause"
        <*> o .:  "decisionTaskCompletedEventId"
        <*> o .:  "timerId"

instance ToJSON StartTimerFailedEventAttributes where
    toJSON StartTimerFailedEventAttributes{..} = object
        [ "timerId"                      .= _stfeaTimerId
        , "cause"                        .= _stfeaCause
        , "decisionTaskCompletedEventId" .= _stfeaDecisionTaskCompletedEventId
        ]

data RequestCancelExternalWorkflowExecutionInitiatedEventAttributes = RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    { _rceweieaControl                      :: Maybe Text
    , _rceweieaDecisionTaskCompletedEventId :: Integer
    , _rceweieaRunId                        :: Maybe Text
    , _rceweieaWorkflowId                   :: Text
    } deriving (Eq, Ord, Show)

-- | 'RequestCancelExternalWorkflowExecutionInitiatedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rceweieaControl' @::@ 'Maybe' 'Text'
--
-- * 'rceweieaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'rceweieaRunId' @::@ 'Maybe' 'Text'
--
-- * 'rceweieaWorkflowId' @::@ 'Text'
--
requestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Text -- ^ 'rceweieaWorkflowId'
                                                               -> Integer -- ^ 'rceweieaDecisionTaskCompletedEventId'
                                                               -> RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
requestCancelExternalWorkflowExecutionInitiatedEventAttributes p1 p2 = RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    { _rceweieaWorkflowId                   = p1
    , _rceweieaDecisionTaskCompletedEventId = p2
    , _rceweieaRunId                        = Nothing
    , _rceweieaControl                      = Nothing
    }

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
rceweieaControl :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
rceweieaControl = lens _rceweieaControl (\s a -> s { _rceweieaControl = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'RequestCancelExternalWorkflowExecution' decision for this
-- cancellation request. This information can be useful for diagnosing problems
-- by tracing back the cause of events.
rceweieaDecisionTaskCompletedEventId :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes Integer
rceweieaDecisionTaskCompletedEventId =
    lens _rceweieaDecisionTaskCompletedEventId
        (\s a -> s { _rceweieaDecisionTaskCompletedEventId = a })

-- | The 'runId' of the external workflow execution to be canceled.
rceweieaRunId :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
rceweieaRunId = lens _rceweieaRunId (\s a -> s { _rceweieaRunId = a })

-- | The 'workflowId' of the external workflow execution to be canceled.
rceweieaWorkflowId :: Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes Text
rceweieaWorkflowId =
    lens _rceweieaWorkflowId (\s a -> s { _rceweieaWorkflowId = a })

instance FromJSON RequestCancelExternalWorkflowExecutionInitiatedEventAttributes where
    parseJSON = withObject "RequestCancelExternalWorkflowExecutionInitiatedEventAttributes" $ \o -> RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
        <$> o .:? "control"
        <*> o .:  "decisionTaskCompletedEventId"
        <*> o .:? "runId"
        <*> o .:  "workflowId"

instance ToJSON RequestCancelExternalWorkflowExecutionInitiatedEventAttributes where
    toJSON RequestCancelExternalWorkflowExecutionInitiatedEventAttributes{..} = object
        [ "workflowId"                   .= _rceweieaWorkflowId
        , "runId"                        .= _rceweieaRunId
        , "decisionTaskCompletedEventId" .= _rceweieaDecisionTaskCompletedEventId
        , "control"                      .= _rceweieaControl
        ]

data RecordMarkerFailedEventAttributes = RecordMarkerFailedEventAttributes
    { _rmfeaCause                        :: RecordMarkerFailedCause
    , _rmfeaDecisionTaskCompletedEventId :: Integer
    , _rmfeaMarkerName                   :: Text
    } deriving (Eq, Show)

-- | 'RecordMarkerFailedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rmfeaCause' @::@ 'RecordMarkerFailedCause'
--
-- * 'rmfeaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'rmfeaMarkerName' @::@ 'Text'
--
recordMarkerFailedEventAttributes :: Text -- ^ 'rmfeaMarkerName'
                                  -> RecordMarkerFailedCause -- ^ 'rmfeaCause'
                                  -> Integer -- ^ 'rmfeaDecisionTaskCompletedEventId'
                                  -> RecordMarkerFailedEventAttributes
recordMarkerFailedEventAttributes p1 p2 p3 = RecordMarkerFailedEventAttributes
    { _rmfeaMarkerName                   = p1
    , _rmfeaCause                        = p2
    , _rmfeaDecisionTaskCompletedEventId = p3
    }

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes.
rmfeaCause :: Lens' RecordMarkerFailedEventAttributes RecordMarkerFailedCause
rmfeaCause = lens _rmfeaCause (\s a -> s { _rmfeaCause = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'RecordMarkerFailed' decision for this cancellation
-- request. This information can be useful for diagnosing problems by tracing
-- back the cause of events.
rmfeaDecisionTaskCompletedEventId :: Lens' RecordMarkerFailedEventAttributes Integer
rmfeaDecisionTaskCompletedEventId =
    lens _rmfeaDecisionTaskCompletedEventId
        (\s a -> s { _rmfeaDecisionTaskCompletedEventId = a })

-- | The marker's name.
rmfeaMarkerName :: Lens' RecordMarkerFailedEventAttributes Text
rmfeaMarkerName = lens _rmfeaMarkerName (\s a -> s { _rmfeaMarkerName = a })

instance FromJSON RecordMarkerFailedEventAttributes where
    parseJSON = withObject "RecordMarkerFailedEventAttributes" $ \o -> RecordMarkerFailedEventAttributes
        <$> o .:  "cause"
        <*> o .:  "decisionTaskCompletedEventId"
        <*> o .:  "markerName"

instance ToJSON RecordMarkerFailedEventAttributes where
    toJSON RecordMarkerFailedEventAttributes{..} = object
        [ "markerName"                   .= _rmfeaMarkerName
        , "cause"                        .= _rmfeaCause
        , "decisionTaskCompletedEventId" .= _rmfeaDecisionTaskCompletedEventId
        ]

data WorkflowExecutionCount = WorkflowExecutionCount
    { _wecCount     :: Nat
    , _wecTruncated :: Maybe Bool
    } deriving (Eq, Ord, Show)

-- | 'WorkflowExecutionCount' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wecCount' @::@ 'Natural'
--
-- * 'wecTruncated' @::@ 'Maybe' 'Bool'
--
workflowExecutionCount :: Natural -- ^ 'wecCount'
                       -> WorkflowExecutionCount
workflowExecutionCount p1 = WorkflowExecutionCount
    { _wecCount     = withIso _Nat (const id) p1
    , _wecTruncated = Nothing
    }

-- | The number of workflow executions.
wecCount :: Lens' WorkflowExecutionCount Natural
wecCount = lens _wecCount (\s a -> s { _wecCount = a }) . _Nat

-- | If set to true, indicates that the actual count was more than the maximum
-- supported by this API and the count returned is the truncated value.
wecTruncated :: Lens' WorkflowExecutionCount (Maybe Bool)
wecTruncated = lens _wecTruncated (\s a -> s { _wecTruncated = a })

instance FromJSON WorkflowExecutionCount where
    parseJSON = withObject "WorkflowExecutionCount" $ \o -> WorkflowExecutionCount
        <$> o .:  "count"
        <*> o .:? "truncated"

instance ToJSON WorkflowExecutionCount where
    toJSON WorkflowExecutionCount{..} = object
        [ "count"     .= _wecCount
        , "truncated" .= _wecTruncated
        ]

data ActivityTaskScheduledEventAttributes = ActivityTaskScheduledEventAttributes
    { _atseaActivityId                   :: Text
    , _atseaActivityType                 :: ActivityType
    , _atseaControl                      :: Maybe Text
    , _atseaDecisionTaskCompletedEventId :: Integer
    , _atseaHeartbeatTimeout             :: Maybe Text
    , _atseaInput                        :: Maybe Text
    , _atseaScheduleToCloseTimeout       :: Maybe Text
    , _atseaScheduleToStartTimeout       :: Maybe Text
    , _atseaStartToCloseTimeout          :: Maybe Text
    , _atseaTaskList                     :: TaskList
    } deriving (Eq, Show)

-- | 'ActivityTaskScheduledEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atseaActivityId' @::@ 'Text'
--
-- * 'atseaActivityType' @::@ 'ActivityType'
--
-- * 'atseaControl' @::@ 'Maybe' 'Text'
--
-- * 'atseaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'atseaHeartbeatTimeout' @::@ 'Maybe' 'Text'
--
-- * 'atseaInput' @::@ 'Maybe' 'Text'
--
-- * 'atseaScheduleToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'atseaScheduleToStartTimeout' @::@ 'Maybe' 'Text'
--
-- * 'atseaStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'atseaTaskList' @::@ 'TaskList'
--
activityTaskScheduledEventAttributes :: ActivityType -- ^ 'atseaActivityType'
                                     -> Text -- ^ 'atseaActivityId'
                                     -> TaskList -- ^ 'atseaTaskList'
                                     -> Integer -- ^ 'atseaDecisionTaskCompletedEventId'
                                     -> ActivityTaskScheduledEventAttributes
activityTaskScheduledEventAttributes p1 p2 p3 p4 = ActivityTaskScheduledEventAttributes
    { _atseaActivityType                 = p1
    , _atseaActivityId                   = p2
    , _atseaTaskList                     = p3
    , _atseaDecisionTaskCompletedEventId = p4
    , _atseaInput                        = Nothing
    , _atseaControl                      = Nothing
    , _atseaScheduleToStartTimeout       = Nothing
    , _atseaScheduleToCloseTimeout       = Nothing
    , _atseaStartToCloseTimeout          = Nothing
    , _atseaHeartbeatTimeout             = Nothing
    }

-- | The unique id of the activity task.
atseaActivityId :: Lens' ActivityTaskScheduledEventAttributes Text
atseaActivityId = lens _atseaActivityId (\s a -> s { _atseaActivityId = a })

-- | The type of the activity task.
atseaActivityType :: Lens' ActivityTaskScheduledEventAttributes ActivityType
atseaActivityType =
    lens _atseaActivityType (\s a -> s { _atseaActivityType = a })

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks. This data is not sent to the activity.
atseaControl :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaControl = lens _atseaControl (\s a -> s { _atseaControl = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision that
-- resulted in the scheduling of this activity task. This information can be
-- useful for diagnosing problems by tracing back the chain of events leading up
-- to this event.
atseaDecisionTaskCompletedEventId :: Lens' ActivityTaskScheduledEventAttributes Integer
atseaDecisionTaskCompletedEventId =
    lens _atseaDecisionTaskCompletedEventId
        (\s a -> s { _atseaDecisionTaskCompletedEventId = a })

-- | The maximum time before which the worker processing this task must report
-- progress by calling 'RecordActivityTaskHeartbeat'. If the timeout is exceeded,
-- the activity task is automatically timed out. If the worker subsequently
-- attempts to record a heartbeat or return a result, it will be ignored.
atseaHeartbeatTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaHeartbeatTimeout =
    lens _atseaHeartbeatTimeout (\s a -> s { _atseaHeartbeatTimeout = a })

-- | The input provided to the activity task.
atseaInput :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaInput = lens _atseaInput (\s a -> s { _atseaInput = a })

-- | The maximum amount of time for this activity task.
atseaScheduleToCloseTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaScheduleToCloseTimeout =
    lens _atseaScheduleToCloseTimeout
        (\s a -> s { _atseaScheduleToCloseTimeout = a })

-- | The maximum amount of time the activity task can wait to be assigned to a
-- worker.
atseaScheduleToStartTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaScheduleToStartTimeout =
    lens _atseaScheduleToStartTimeout
        (\s a -> s { _atseaScheduleToStartTimeout = a })

-- | The maximum amount of time a worker may take to process the activity task.
atseaStartToCloseTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaStartToCloseTimeout =
    lens _atseaStartToCloseTimeout
        (\s a -> s { _atseaStartToCloseTimeout = a })

-- | The task list in which the activity task has been scheduled.
atseaTaskList :: Lens' ActivityTaskScheduledEventAttributes TaskList
atseaTaskList = lens _atseaTaskList (\s a -> s { _atseaTaskList = a })

instance FromJSON ActivityTaskScheduledEventAttributes where
    parseJSON = withObject "ActivityTaskScheduledEventAttributes" $ \o -> ActivityTaskScheduledEventAttributes
        <$> o .:  "activityId"
        <*> o .:  "activityType"
        <*> o .:? "control"
        <*> o .:  "decisionTaskCompletedEventId"
        <*> o .:? "heartbeatTimeout"
        <*> o .:? "input"
        <*> o .:? "scheduleToCloseTimeout"
        <*> o .:? "scheduleToStartTimeout"
        <*> o .:? "startToCloseTimeout"
        <*> o .:  "taskList"

instance ToJSON ActivityTaskScheduledEventAttributes where
    toJSON ActivityTaskScheduledEventAttributes{..} = object
        [ "activityType"                 .= _atseaActivityType
        , "activityId"                   .= _atseaActivityId
        , "input"                        .= _atseaInput
        , "control"                      .= _atseaControl
        , "scheduleToStartTimeout"       .= _atseaScheduleToStartTimeout
        , "scheduleToCloseTimeout"       .= _atseaScheduleToCloseTimeout
        , "startToCloseTimeout"          .= _atseaStartToCloseTimeout
        , "taskList"                     .= _atseaTaskList
        , "decisionTaskCompletedEventId" .= _atseaDecisionTaskCompletedEventId
        , "heartbeatTimeout"             .= _atseaHeartbeatTimeout
        ]

newtype CloseStatusFilter = CloseStatusFilter
    { _csfStatus :: CloseStatus
    } deriving (Eq, Show)

-- | 'CloseStatusFilter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csfStatus' @::@ 'CloseStatus'
--
closeStatusFilter :: CloseStatus -- ^ 'csfStatus'
                  -> CloseStatusFilter
closeStatusFilter p1 = CloseStatusFilter
    { _csfStatus = p1
    }

-- | The close status that must match the close status of an execution for it to
-- meet the criteria of this filter. This field is required.
csfStatus :: Lens' CloseStatusFilter CloseStatus
csfStatus = lens _csfStatus (\s a -> s { _csfStatus = a })

instance FromJSON CloseStatusFilter where
    parseJSON = withObject "CloseStatusFilter" $ \o -> CloseStatusFilter
        <$> o .:  "status"

instance ToJSON CloseStatusFilter where
    toJSON CloseStatusFilter{..} = object
        [ "status" .= _csfStatus
        ]

data WorkflowExecutionTimeoutType
    = StartToClose -- ^ START_TO_CLOSE
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable WorkflowExecutionTimeoutType

instance FromText WorkflowExecutionTimeoutType where
    parser = takeText >>= \case
        "START_TO_CLOSE" -> pure StartToClose
        e                -> fail $
            "Failure parsing WorkflowExecutionTimeoutType from " ++ show e

instance ToText WorkflowExecutionTimeoutType where
    toText StartToClose = "START_TO_CLOSE"

instance ToByteString WorkflowExecutionTimeoutType
instance ToHeader     WorkflowExecutionTimeoutType
instance ToQuery      WorkflowExecutionTimeoutType

instance FromJSON WorkflowExecutionTimeoutType where
    parseJSON = parseJSONText "WorkflowExecutionTimeoutType"

instance ToJSON WorkflowExecutionTimeoutType where
    toJSON = toJSONText

data ScheduleActivityTaskDecisionAttributes = ScheduleActivityTaskDecisionAttributes
    { _satdaActivityId             :: Text
    , _satdaActivityType           :: ActivityType
    , _satdaControl                :: Maybe Text
    , _satdaHeartbeatTimeout       :: Maybe Text
    , _satdaInput                  :: Maybe Text
    , _satdaScheduleToCloseTimeout :: Maybe Text
    , _satdaScheduleToStartTimeout :: Maybe Text
    , _satdaStartToCloseTimeout    :: Maybe Text
    , _satdaTaskList               :: Maybe TaskList
    } deriving (Eq, Show)

-- | 'ScheduleActivityTaskDecisionAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'satdaActivityId' @::@ 'Text'
--
-- * 'satdaActivityType' @::@ 'ActivityType'
--
-- * 'satdaControl' @::@ 'Maybe' 'Text'
--
-- * 'satdaHeartbeatTimeout' @::@ 'Maybe' 'Text'
--
-- * 'satdaInput' @::@ 'Maybe' 'Text'
--
-- * 'satdaScheduleToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'satdaScheduleToStartTimeout' @::@ 'Maybe' 'Text'
--
-- * 'satdaStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'satdaTaskList' @::@ 'Maybe' 'TaskList'
--
scheduleActivityTaskDecisionAttributes :: ActivityType -- ^ 'satdaActivityType'
                                       -> Text -- ^ 'satdaActivityId'
                                       -> ScheduleActivityTaskDecisionAttributes
scheduleActivityTaskDecisionAttributes p1 p2 = ScheduleActivityTaskDecisionAttributes
    { _satdaActivityType           = p1
    , _satdaActivityId             = p2
    , _satdaControl                = Nothing
    , _satdaInput                  = Nothing
    , _satdaScheduleToCloseTimeout = Nothing
    , _satdaTaskList               = Nothing
    , _satdaScheduleToStartTimeout = Nothing
    , _satdaStartToCloseTimeout    = Nothing
    , _satdaHeartbeatTimeout       = Nothing
    }

-- | The 'activityId' of the activity task. This field is required.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a ':' (colon), '/' (slash), '|' (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string "arn".
satdaActivityId :: Lens' ScheduleActivityTaskDecisionAttributes Text
satdaActivityId = lens _satdaActivityId (\s a -> s { _satdaActivityId = a })

-- | The type of the activity task to schedule. This field is required.
satdaActivityType :: Lens' ScheduleActivityTaskDecisionAttributes ActivityType
satdaActivityType =
    lens _satdaActivityType (\s a -> s { _satdaActivityType = a })

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks. This data is not sent to the activity.
satdaControl :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaControl = lens _satdaControl (\s a -> s { _satdaControl = a })

-- | If set, specifies the maximum time before which a worker processing a task
-- of this type must report progress by calling 'RecordActivityTaskHeartbeat'. If
-- the timeout is exceeded, the activity task is automatically timed out. If the
-- worker subsequently attempts to record a heartbeat or returns a result, it
-- will be ignored. This overrides the default heartbeat timeout specified when
-- registering the activity type using 'RegisterActivityType'.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
satdaHeartbeatTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaHeartbeatTimeout =
    lens _satdaHeartbeatTimeout (\s a -> s { _satdaHeartbeatTimeout = a })

-- | The input provided to the activity task.
satdaInput :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaInput = lens _satdaInput (\s a -> s { _satdaInput = a })

-- | The maximum duration for this activity task.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
satdaScheduleToCloseTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaScheduleToCloseTimeout =
    lens _satdaScheduleToCloseTimeout
        (\s a -> s { _satdaScheduleToCloseTimeout = a })

-- | If set, specifies the maximum duration the activity task can wait to be
-- assigned to a worker. This overrides the default schedule-to-start timeout
-- specified when registering the activity type using 'RegisterActivityType'.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
satdaScheduleToStartTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaScheduleToStartTimeout =
    lens _satdaScheduleToStartTimeout
        (\s a -> s { _satdaScheduleToStartTimeout = a })

-- | If set, specifies the maximum duration a worker may take to process this
-- activity task. This overrides the default start-to-close timeout specified
-- when registering the activity type using 'RegisterActivityType'.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
satdaStartToCloseTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaStartToCloseTimeout =
    lens _satdaStartToCloseTimeout
        (\s a -> s { _satdaStartToCloseTimeout = a })

-- | If set, specifies the name of the task list in which to schedule the
-- activity task. If not specified, the 'defaultTaskList' registered with the
-- activity type will be used.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a ':' (colon), '/' (slash), '|' (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string "arn".
satdaTaskList :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe TaskList)
satdaTaskList = lens _satdaTaskList (\s a -> s { _satdaTaskList = a })

instance FromJSON ScheduleActivityTaskDecisionAttributes where
    parseJSON = withObject "ScheduleActivityTaskDecisionAttributes" $ \o -> ScheduleActivityTaskDecisionAttributes
        <$> o .:  "activityId"
        <*> o .:  "activityType"
        <*> o .:? "control"
        <*> o .:? "heartbeatTimeout"
        <*> o .:? "input"
        <*> o .:? "scheduleToCloseTimeout"
        <*> o .:? "scheduleToStartTimeout"
        <*> o .:? "startToCloseTimeout"
        <*> o .:? "taskList"

instance ToJSON ScheduleActivityTaskDecisionAttributes where
    toJSON ScheduleActivityTaskDecisionAttributes{..} = object
        [ "activityType"           .= _satdaActivityType
        , "activityId"             .= _satdaActivityId
        , "control"                .= _satdaControl
        , "input"                  .= _satdaInput
        , "scheduleToCloseTimeout" .= _satdaScheduleToCloseTimeout
        , "taskList"               .= _satdaTaskList
        , "scheduleToStartTimeout" .= _satdaScheduleToStartTimeout
        , "startToCloseTimeout"    .= _satdaStartToCloseTimeout
        , "heartbeatTimeout"       .= _satdaHeartbeatTimeout
        ]

data ActivityTypeConfiguration = ActivityTypeConfiguration
    { _atcDefaultTaskHeartbeatTimeout       :: Maybe Text
    , _atcDefaultTaskList                   :: Maybe TaskList
    , _atcDefaultTaskScheduleToCloseTimeout :: Maybe Text
    , _atcDefaultTaskScheduleToStartTimeout :: Maybe Text
    , _atcDefaultTaskStartToCloseTimeout    :: Maybe Text
    } deriving (Eq, Show)

-- | 'ActivityTypeConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atcDefaultTaskHeartbeatTimeout' @::@ 'Maybe' 'Text'
--
-- * 'atcDefaultTaskList' @::@ 'Maybe' 'TaskList'
--
-- * 'atcDefaultTaskScheduleToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'atcDefaultTaskScheduleToStartTimeout' @::@ 'Maybe' 'Text'
--
-- * 'atcDefaultTaskStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
activityTypeConfiguration :: ActivityTypeConfiguration
activityTypeConfiguration = ActivityTypeConfiguration
    { _atcDefaultTaskStartToCloseTimeout    = Nothing
    , _atcDefaultTaskHeartbeatTimeout       = Nothing
    , _atcDefaultTaskList                   = Nothing
    , _atcDefaultTaskScheduleToStartTimeout = Nothing
    , _atcDefaultTaskScheduleToCloseTimeout = Nothing
    }

-- | The optional default maximum time, specified when registering the activity
-- type, before which a worker processing a task must report progress by calling 'RecordActivityTaskHeartbeat'. You can override this default when scheduling a
-- task through the 'ScheduleActivityTask' 'Decision'. If the activity worker
-- subsequently attempts to record a heartbeat or returns a result, the activity
-- worker receives an 'UnknownResource' fault. In this case, Amazon SWF no longer
-- considers the activity task to be valid; the activity worker should clean up
-- the activity task.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
atcDefaultTaskHeartbeatTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskHeartbeatTimeout =
    lens _atcDefaultTaskHeartbeatTimeout
        (\s a -> s { _atcDefaultTaskHeartbeatTimeout = a })

-- | The optional default task list specified for this activity type at
-- registration. This default task list is used if a task list is not provided
-- when a task is scheduled through the 'ScheduleActivityTask' 'Decision'. You can
-- override this default when scheduling a task through the 'ScheduleActivityTask' 'Decision'.
atcDefaultTaskList :: Lens' ActivityTypeConfiguration (Maybe TaskList)
atcDefaultTaskList =
    lens _atcDefaultTaskList (\s a -> s { _atcDefaultTaskList = a })

-- | The optional default maximum duration, specified when registering the
-- activity type, for tasks of this activity type. You can override this default
-- when scheduling a task through the 'ScheduleActivityTask' 'Decision'.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
atcDefaultTaskScheduleToCloseTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskScheduleToCloseTimeout =
    lens _atcDefaultTaskScheduleToCloseTimeout
        (\s a -> s { _atcDefaultTaskScheduleToCloseTimeout = a })

-- | The optional default maximum duration, specified when registering the
-- activity type, that a task of an activity type can wait before being assigned
-- to a worker. You can override this default when scheduling a task through the 'ScheduleActivityTask' 'Decision'.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
atcDefaultTaskScheduleToStartTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskScheduleToStartTimeout =
    lens _atcDefaultTaskScheduleToStartTimeout
        (\s a -> s { _atcDefaultTaskScheduleToStartTimeout = a })

-- | The optional default maximum duration for tasks of an activity type
-- specified when registering the activity type. You can override this default
-- when scheduling a task through the 'ScheduleActivityTask' 'Decision'.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
atcDefaultTaskStartToCloseTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskStartToCloseTimeout =
    lens _atcDefaultTaskStartToCloseTimeout
        (\s a -> s { _atcDefaultTaskStartToCloseTimeout = a })

instance FromJSON ActivityTypeConfiguration where
    parseJSON = withObject "ActivityTypeConfiguration" $ \o -> ActivityTypeConfiguration
        <$> o .:? "defaultTaskHeartbeatTimeout"
        <*> o .:? "defaultTaskList"
        <*> o .:? "defaultTaskScheduleToCloseTimeout"
        <*> o .:? "defaultTaskScheduleToStartTimeout"
        <*> o .:? "defaultTaskStartToCloseTimeout"

instance ToJSON ActivityTypeConfiguration where
    toJSON ActivityTypeConfiguration{..} = object
        [ "defaultTaskStartToCloseTimeout"    .= _atcDefaultTaskStartToCloseTimeout
        , "defaultTaskHeartbeatTimeout"       .= _atcDefaultTaskHeartbeatTimeout
        , "defaultTaskList"                   .= _atcDefaultTaskList
        , "defaultTaskScheduleToStartTimeout" .= _atcDefaultTaskScheduleToStartTimeout
        , "defaultTaskScheduleToCloseTimeout" .= _atcDefaultTaskScheduleToCloseTimeout
        ]

data ActivityType = ActivityType
    { _atName    :: Text
    , _atVersion :: Text
    } deriving (Eq, Ord, Show)

-- | 'ActivityType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atName' @::@ 'Text'
--
-- * 'atVersion' @::@ 'Text'
--
activityType :: Text -- ^ 'atName'
             -> Text -- ^ 'atVersion'
             -> ActivityType
activityType p1 p2 = ActivityType
    { _atName    = p1
    , _atVersion = p2
    }

-- | The name of this activity.
atName :: Lens' ActivityType Text
atName = lens _atName (\s a -> s { _atName = a })

-- | The version of this activity.
atVersion :: Lens' ActivityType Text
atVersion = lens _atVersion (\s a -> s { _atVersion = a })

instance FromJSON ActivityType where
    parseJSON = withObject "ActivityType" $ \o -> ActivityType
        <$> o .:  "name"
        <*> o .:  "version"

instance ToJSON ActivityType where
    toJSON ActivityType{..} = object
        [ "name"    .= _atName
        , "version" .= _atVersion
        ]

data WorkflowTypeInfo = WorkflowTypeInfo
    { _wtiCreationDate    :: POSIX
    , _wtiDeprecationDate :: Maybe POSIX
    , _wtiDescription     :: Maybe Text
    , _wtiStatus          :: RegistrationStatus
    , _wtiWorkflowType    :: WorkflowType
    } deriving (Eq, Show)

-- | 'WorkflowTypeInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wtiCreationDate' @::@ 'UTCTime'
--
-- * 'wtiDeprecationDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'wtiDescription' @::@ 'Maybe' 'Text'
--
-- * 'wtiStatus' @::@ 'RegistrationStatus'
--
-- * 'wtiWorkflowType' @::@ 'WorkflowType'
--
workflowTypeInfo :: WorkflowType -- ^ 'wtiWorkflowType'
                 -> RegistrationStatus -- ^ 'wtiStatus'
                 -> UTCTime -- ^ 'wtiCreationDate'
                 -> WorkflowTypeInfo
workflowTypeInfo p1 p2 p3 = WorkflowTypeInfo
    { _wtiWorkflowType    = p1
    , _wtiStatus          = p2
    , _wtiCreationDate    = withIso _Time (const id) p3
    , _wtiDescription     = Nothing
    , _wtiDeprecationDate = Nothing
    }

-- | The date when this type was registered.
wtiCreationDate :: Lens' WorkflowTypeInfo UTCTime
wtiCreationDate = lens _wtiCreationDate (\s a -> s { _wtiCreationDate = a }) . _Time

-- | If the type is in deprecated state, then it is set to the date when the type
-- was deprecated.
wtiDeprecationDate :: Lens' WorkflowTypeInfo (Maybe UTCTime)
wtiDeprecationDate =
    lens _wtiDeprecationDate (\s a -> s { _wtiDeprecationDate = a })
        . mapping _Time

-- | The description of the type registered through 'RegisterWorkflowType'.
wtiDescription :: Lens' WorkflowTypeInfo (Maybe Text)
wtiDescription = lens _wtiDescription (\s a -> s { _wtiDescription = a })

-- | The current status of the workflow type.
wtiStatus :: Lens' WorkflowTypeInfo RegistrationStatus
wtiStatus = lens _wtiStatus (\s a -> s { _wtiStatus = a })

-- | The workflow type this information is about.
wtiWorkflowType :: Lens' WorkflowTypeInfo WorkflowType
wtiWorkflowType = lens _wtiWorkflowType (\s a -> s { _wtiWorkflowType = a })

instance FromJSON WorkflowTypeInfo where
    parseJSON = withObject "WorkflowTypeInfo" $ \o -> WorkflowTypeInfo
        <$> o .:  "creationDate"
        <*> o .:? "deprecationDate"
        <*> o .:? "description"
        <*> o .:  "status"
        <*> o .:  "workflowType"

instance ToJSON WorkflowTypeInfo where
    toJSON WorkflowTypeInfo{..} = object
        [ "workflowType"    .= _wtiWorkflowType
        , "status"          .= _wtiStatus
        , "description"     .= _wtiDescription
        , "creationDate"    .= _wtiCreationDate
        , "deprecationDate" .= _wtiDeprecationDate
        ]

data ChildWorkflowExecutionCompletedEventAttributes = ChildWorkflowExecutionCompletedEventAttributes
    { _cweceaInitiatedEventId  :: Integer
    , _cweceaResult            :: Maybe Text
    , _cweceaStartedEventId    :: Integer
    , _cweceaWorkflowExecution :: WorkflowExecution
    , _cweceaWorkflowType      :: WorkflowType
    } deriving (Eq, Show)

-- | 'ChildWorkflowExecutionCompletedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cweceaInitiatedEventId' @::@ 'Integer'
--
-- * 'cweceaResult' @::@ 'Maybe' 'Text'
--
-- * 'cweceaStartedEventId' @::@ 'Integer'
--
-- * 'cweceaWorkflowExecution' @::@ 'WorkflowExecution'
--
-- * 'cweceaWorkflowType' @::@ 'WorkflowType'
--
childWorkflowExecutionCompletedEventAttributes :: WorkflowExecution -- ^ 'cweceaWorkflowExecution'
                                               -> WorkflowType -- ^ 'cweceaWorkflowType'
                                               -> Integer -- ^ 'cweceaInitiatedEventId'
                                               -> Integer -- ^ 'cweceaStartedEventId'
                                               -> ChildWorkflowExecutionCompletedEventAttributes
childWorkflowExecutionCompletedEventAttributes p1 p2 p3 p4 = ChildWorkflowExecutionCompletedEventAttributes
    { _cweceaWorkflowExecution = p1
    , _cweceaWorkflowType      = p2
    , _cweceaInitiatedEventId  = p3
    , _cweceaStartedEventId    = p4
    , _cweceaResult            = Nothing
    }

-- | The id of the 'StartChildWorkflowExecutionInitiated' event corresponding to
-- the 'StartChildWorkflowExecution' 'Decision' to start this child workflow
-- execution. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
cweceaInitiatedEventId :: Lens' ChildWorkflowExecutionCompletedEventAttributes Integer
cweceaInitiatedEventId =
    lens _cweceaInitiatedEventId (\s a -> s { _cweceaInitiatedEventId = a })

-- | The result of the child workflow execution (if any).
cweceaResult :: Lens' ChildWorkflowExecutionCompletedEventAttributes (Maybe Text)
cweceaResult = lens _cweceaResult (\s a -> s { _cweceaResult = a })

-- | The Id of the 'ChildWorkflowExecutionStarted' event recorded when this child
-- workflow execution was started. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
cweceaStartedEventId :: Lens' ChildWorkflowExecutionCompletedEventAttributes Integer
cweceaStartedEventId =
    lens _cweceaStartedEventId (\s a -> s { _cweceaStartedEventId = a })

-- | The child workflow execution that was completed.
cweceaWorkflowExecution :: Lens' ChildWorkflowExecutionCompletedEventAttributes WorkflowExecution
cweceaWorkflowExecution =
    lens _cweceaWorkflowExecution (\s a -> s { _cweceaWorkflowExecution = a })

-- | The type of the child workflow execution.
cweceaWorkflowType :: Lens' ChildWorkflowExecutionCompletedEventAttributes WorkflowType
cweceaWorkflowType =
    lens _cweceaWorkflowType (\s a -> s { _cweceaWorkflowType = a })

instance FromJSON ChildWorkflowExecutionCompletedEventAttributes where
    parseJSON = withObject "ChildWorkflowExecutionCompletedEventAttributes" $ \o -> ChildWorkflowExecutionCompletedEventAttributes
        <$> o .:  "initiatedEventId"
        <*> o .:? "result"
        <*> o .:  "startedEventId"
        <*> o .:  "workflowExecution"
        <*> o .:  "workflowType"

instance ToJSON ChildWorkflowExecutionCompletedEventAttributes where
    toJSON ChildWorkflowExecutionCompletedEventAttributes{..} = object
        [ "workflowExecution" .= _cweceaWorkflowExecution
        , "workflowType"      .= _cweceaWorkflowType
        , "result"            .= _cweceaResult
        , "initiatedEventId"  .= _cweceaInitiatedEventId
        , "startedEventId"    .= _cweceaStartedEventId
        ]

data WorkflowExecutionOpenCounts = WorkflowExecutionOpenCounts
    { _weocOpenActivityTasks           :: Nat
    , _weocOpenChildWorkflowExecutions :: Nat
    , _weocOpenDecisionTasks           :: Nat
    , _weocOpenTimers                  :: Nat
    } deriving (Eq, Ord, Show)

-- | 'WorkflowExecutionOpenCounts' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'weocOpenActivityTasks' @::@ 'Natural'
--
-- * 'weocOpenChildWorkflowExecutions' @::@ 'Natural'
--
-- * 'weocOpenDecisionTasks' @::@ 'Natural'
--
-- * 'weocOpenTimers' @::@ 'Natural'
--
workflowExecutionOpenCounts :: Natural -- ^ 'weocOpenActivityTasks'
                            -> Natural -- ^ 'weocOpenDecisionTasks'
                            -> Natural -- ^ 'weocOpenTimers'
                            -> Natural -- ^ 'weocOpenChildWorkflowExecutions'
                            -> WorkflowExecutionOpenCounts
workflowExecutionOpenCounts p1 p2 p3 p4 = WorkflowExecutionOpenCounts
    { _weocOpenActivityTasks           = withIso _Nat (const id) p1
    , _weocOpenDecisionTasks           = withIso _Nat (const id) p2
    , _weocOpenTimers                  = withIso _Nat (const id) p3
    , _weocOpenChildWorkflowExecutions = withIso _Nat (const id) p4
    }

-- | The count of activity tasks whose status is OPEN.
weocOpenActivityTasks :: Lens' WorkflowExecutionOpenCounts Natural
weocOpenActivityTasks =
    lens _weocOpenActivityTasks (\s a -> s { _weocOpenActivityTasks = a })
        . _Nat

-- | The count of child workflow executions whose status is OPEN.
weocOpenChildWorkflowExecutions :: Lens' WorkflowExecutionOpenCounts Natural
weocOpenChildWorkflowExecutions =
    lens _weocOpenChildWorkflowExecutions
        (\s a -> s { _weocOpenChildWorkflowExecutions = a })
            . _Nat

-- | The count of decision tasks whose status is OPEN. A workflow execution can
-- have at most one open decision task.
weocOpenDecisionTasks :: Lens' WorkflowExecutionOpenCounts Natural
weocOpenDecisionTasks =
    lens _weocOpenDecisionTasks (\s a -> s { _weocOpenDecisionTasks = a })
        . _Nat

-- | The count of timers started by this workflow execution that have not fired
-- yet.
weocOpenTimers :: Lens' WorkflowExecutionOpenCounts Natural
weocOpenTimers = lens _weocOpenTimers (\s a -> s { _weocOpenTimers = a }) . _Nat

instance FromJSON WorkflowExecutionOpenCounts where
    parseJSON = withObject "WorkflowExecutionOpenCounts" $ \o -> WorkflowExecutionOpenCounts
        <$> o .:  "openActivityTasks"
        <*> o .:  "openChildWorkflowExecutions"
        <*> o .:  "openDecisionTasks"
        <*> o .:  "openTimers"

instance ToJSON WorkflowExecutionOpenCounts where
    toJSON WorkflowExecutionOpenCounts{..} = object
        [ "openActivityTasks"           .= _weocOpenActivityTasks
        , "openDecisionTasks"           .= _weocOpenDecisionTasks
        , "openTimers"                  .= _weocOpenTimers
        , "openChildWorkflowExecutions" .= _weocOpenChildWorkflowExecutions
        ]

data RequestCancelActivityTaskFailedCause
    = ActivityIdUnknown     -- ^ ACTIVITY_ID_UNKNOWN
    | OperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable RequestCancelActivityTaskFailedCause

instance FromText RequestCancelActivityTaskFailedCause where
    parser = takeText >>= \case
        "ACTIVITY_ID_UNKNOWN"     -> pure ActivityIdUnknown
        "OPERATION_NOT_PERMITTED" -> pure OperationNotPermitted
        e                         -> fail $
            "Failure parsing RequestCancelActivityTaskFailedCause from " ++ show e

instance ToText RequestCancelActivityTaskFailedCause where
    toText = \case
        ActivityIdUnknown     -> "ACTIVITY_ID_UNKNOWN"
        OperationNotPermitted -> "OPERATION_NOT_PERMITTED"

instance ToByteString RequestCancelActivityTaskFailedCause
instance ToHeader     RequestCancelActivityTaskFailedCause
instance ToQuery      RequestCancelActivityTaskFailedCause

instance FromJSON RequestCancelActivityTaskFailedCause where
    parseJSON = parseJSONText "RequestCancelActivityTaskFailedCause"

instance ToJSON RequestCancelActivityTaskFailedCause where
    toJSON = toJSONText

data ScheduleActivityTaskFailedEventAttributes = ScheduleActivityTaskFailedEventAttributes
    { _satfeaActivityId                   :: Text
    , _satfeaActivityType                 :: ActivityType
    , _satfeaCause                        :: ScheduleActivityTaskFailedCause
    , _satfeaDecisionTaskCompletedEventId :: Integer
    } deriving (Eq, Show)

-- | 'ScheduleActivityTaskFailedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'satfeaActivityId' @::@ 'Text'
--
-- * 'satfeaActivityType' @::@ 'ActivityType'
--
-- * 'satfeaCause' @::@ 'ScheduleActivityTaskFailedCause'
--
-- * 'satfeaDecisionTaskCompletedEventId' @::@ 'Integer'
--
scheduleActivityTaskFailedEventAttributes :: ActivityType -- ^ 'satfeaActivityType'
                                          -> Text -- ^ 'satfeaActivityId'
                                          -> ScheduleActivityTaskFailedCause -- ^ 'satfeaCause'
                                          -> Integer -- ^ 'satfeaDecisionTaskCompletedEventId'
                                          -> ScheduleActivityTaskFailedEventAttributes
scheduleActivityTaskFailedEventAttributes p1 p2 p3 p4 = ScheduleActivityTaskFailedEventAttributes
    { _satfeaActivityType                 = p1
    , _satfeaActivityId                   = p2
    , _satfeaCause                        = p3
    , _satfeaDecisionTaskCompletedEventId = p4
    }

-- | The activityId provided in the 'ScheduleActivityTask' decision that failed.
satfeaActivityId :: Lens' ScheduleActivityTaskFailedEventAttributes Text
satfeaActivityId = lens _satfeaActivityId (\s a -> s { _satfeaActivityId = a })

-- | The activity type provided in the 'ScheduleActivityTask' decision that failed.
satfeaActivityType :: Lens' ScheduleActivityTaskFailedEventAttributes ActivityType
satfeaActivityType =
    lens _satfeaActivityType (\s a -> s { _satfeaActivityType = a })

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes.
satfeaCause :: Lens' ScheduleActivityTaskFailedEventAttributes ScheduleActivityTaskFailedCause
satfeaCause = lens _satfeaCause (\s a -> s { _satfeaCause = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision that
-- resulted in the scheduling of this activity task. This information can be
-- useful for diagnosing problems by tracing back the chain of events leading up
-- to this event.
satfeaDecisionTaskCompletedEventId :: Lens' ScheduleActivityTaskFailedEventAttributes Integer
satfeaDecisionTaskCompletedEventId =
    lens _satfeaDecisionTaskCompletedEventId
        (\s a -> s { _satfeaDecisionTaskCompletedEventId = a })

instance FromJSON ScheduleActivityTaskFailedEventAttributes where
    parseJSON = withObject "ScheduleActivityTaskFailedEventAttributes" $ \o -> ScheduleActivityTaskFailedEventAttributes
        <$> o .:  "activityId"
        <*> o .:  "activityType"
        <*> o .:  "cause"
        <*> o .:  "decisionTaskCompletedEventId"

instance ToJSON ScheduleActivityTaskFailedEventAttributes where
    toJSON ScheduleActivityTaskFailedEventAttributes{..} = object
        [ "activityType"                 .= _satfeaActivityType
        , "activityId"                   .= _satfeaActivityId
        , "cause"                        .= _satfeaCause
        , "decisionTaskCompletedEventId" .= _satfeaDecisionTaskCompletedEventId
        ]

data MarkerRecordedEventAttributes = MarkerRecordedEventAttributes
    { _mreaDecisionTaskCompletedEventId :: Integer
    , _mreaDetails                      :: Maybe Text
    , _mreaMarkerName                   :: Text
    } deriving (Eq, Ord, Show)

-- | 'MarkerRecordedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mreaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'mreaDetails' @::@ 'Maybe' 'Text'
--
-- * 'mreaMarkerName' @::@ 'Text'
--
markerRecordedEventAttributes :: Text -- ^ 'mreaMarkerName'
                              -> Integer -- ^ 'mreaDecisionTaskCompletedEventId'
                              -> MarkerRecordedEventAttributes
markerRecordedEventAttributes p1 p2 = MarkerRecordedEventAttributes
    { _mreaMarkerName                   = p1
    , _mreaDecisionTaskCompletedEventId = p2
    , _mreaDetails                      = Nothing
    }

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'RecordMarker' decision that requested this marker. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
mreaDecisionTaskCompletedEventId :: Lens' MarkerRecordedEventAttributes Integer
mreaDecisionTaskCompletedEventId =
    lens _mreaDecisionTaskCompletedEventId
        (\s a -> s { _mreaDecisionTaskCompletedEventId = a })

-- | Details of the marker (if any).
mreaDetails :: Lens' MarkerRecordedEventAttributes (Maybe Text)
mreaDetails = lens _mreaDetails (\s a -> s { _mreaDetails = a })

-- | The name of the marker.
mreaMarkerName :: Lens' MarkerRecordedEventAttributes Text
mreaMarkerName = lens _mreaMarkerName (\s a -> s { _mreaMarkerName = a })

instance FromJSON MarkerRecordedEventAttributes where
    parseJSON = withObject "MarkerRecordedEventAttributes" $ \o -> MarkerRecordedEventAttributes
        <$> o .:  "decisionTaskCompletedEventId"
        <*> o .:? "details"
        <*> o .:  "markerName"

instance ToJSON MarkerRecordedEventAttributes where
    toJSON MarkerRecordedEventAttributes{..} = object
        [ "markerName"                   .= _mreaMarkerName
        , "details"                      .= _mreaDetails
        , "decisionTaskCompletedEventId" .= _mreaDecisionTaskCompletedEventId
        ]

data SignalExternalWorkflowExecutionDecisionAttributes = SignalExternalWorkflowExecutionDecisionAttributes
    { _sewedaControl    :: Maybe Text
    , _sewedaInput      :: Maybe Text
    , _sewedaRunId      :: Maybe Text
    , _sewedaSignalName :: Text
    , _sewedaWorkflowId :: Text
    } deriving (Eq, Ord, Show)

-- | 'SignalExternalWorkflowExecutionDecisionAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sewedaControl' @::@ 'Maybe' 'Text'
--
-- * 'sewedaInput' @::@ 'Maybe' 'Text'
--
-- * 'sewedaRunId' @::@ 'Maybe' 'Text'
--
-- * 'sewedaSignalName' @::@ 'Text'
--
-- * 'sewedaWorkflowId' @::@ 'Text'
--
signalExternalWorkflowExecutionDecisionAttributes :: Text -- ^ 'sewedaWorkflowId'
                                                  -> Text -- ^ 'sewedaSignalName'
                                                  -> SignalExternalWorkflowExecutionDecisionAttributes
signalExternalWorkflowExecutionDecisionAttributes p1 p2 = SignalExternalWorkflowExecutionDecisionAttributes
    { _sewedaWorkflowId = p1
    , _sewedaSignalName = p2
    , _sewedaRunId      = Nothing
    , _sewedaInput      = Nothing
    , _sewedaControl    = Nothing
    }

-- | Optional data attached to the event that can be used by the decider in
-- subsequent decision tasks.
sewedaControl :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Maybe Text)
sewedaControl = lens _sewedaControl (\s a -> s { _sewedaControl = a })

-- | Optional input to be provided with the signal.The target workflow execution
-- will use the signal name and input to process the signal.
sewedaInput :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Maybe Text)
sewedaInput = lens _sewedaInput (\s a -> s { _sewedaInput = a })

-- | The 'runId' of the workflow execution to be signaled.
sewedaRunId :: Lens' SignalExternalWorkflowExecutionDecisionAttributes (Maybe Text)
sewedaRunId = lens _sewedaRunId (\s a -> s { _sewedaRunId = a })

-- | The name of the signal.The target workflow execution will use the signal
-- name and input to process the signal. This field is required.
sewedaSignalName :: Lens' SignalExternalWorkflowExecutionDecisionAttributes Text
sewedaSignalName = lens _sewedaSignalName (\s a -> s { _sewedaSignalName = a })

-- | The 'workflowId' of the workflow execution to be signaled. This field is
-- required.
sewedaWorkflowId :: Lens' SignalExternalWorkflowExecutionDecisionAttributes Text
sewedaWorkflowId = lens _sewedaWorkflowId (\s a -> s { _sewedaWorkflowId = a })

instance FromJSON SignalExternalWorkflowExecutionDecisionAttributes where
    parseJSON = withObject "SignalExternalWorkflowExecutionDecisionAttributes" $ \o -> SignalExternalWorkflowExecutionDecisionAttributes
        <$> o .:? "control"
        <*> o .:? "input"
        <*> o .:? "runId"
        <*> o .:  "signalName"
        <*> o .:  "workflowId"

instance ToJSON SignalExternalWorkflowExecutionDecisionAttributes where
    toJSON SignalExternalWorkflowExecutionDecisionAttributes{..} = object
        [ "workflowId" .= _sewedaWorkflowId
        , "runId"      .= _sewedaRunId
        , "signalName" .= _sewedaSignalName
        , "input"      .= _sewedaInput
        , "control"    .= _sewedaControl
        ]

data WorkflowExecutionTerminatedCause
    = ChildPolicyApplied -- ^ CHILD_POLICY_APPLIED
    | EventLimitExceeded -- ^ EVENT_LIMIT_EXCEEDED
    | OperatorInitiated  -- ^ OPERATOR_INITIATED
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable WorkflowExecutionTerminatedCause

instance FromText WorkflowExecutionTerminatedCause where
    parser = takeText >>= \case
        "CHILD_POLICY_APPLIED" -> pure ChildPolicyApplied
        "EVENT_LIMIT_EXCEEDED" -> pure EventLimitExceeded
        "OPERATOR_INITIATED"   -> pure OperatorInitiated
        e                      -> fail $
            "Failure parsing WorkflowExecutionTerminatedCause from " ++ show e

instance ToText WorkflowExecutionTerminatedCause where
    toText = \case
        ChildPolicyApplied -> "CHILD_POLICY_APPLIED"
        EventLimitExceeded -> "EVENT_LIMIT_EXCEEDED"
        OperatorInitiated  -> "OPERATOR_INITIATED"

instance ToByteString WorkflowExecutionTerminatedCause
instance ToHeader     WorkflowExecutionTerminatedCause
instance ToQuery      WorkflowExecutionTerminatedCause

instance FromJSON WorkflowExecutionTerminatedCause where
    parseJSON = parseJSONText "WorkflowExecutionTerminatedCause"

instance ToJSON WorkflowExecutionTerminatedCause where
    toJSON = toJSONText

data CancelWorkflowExecutionFailedCause
    = CWEFCOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | CWEFCUnhandledDecision     -- ^ UNHANDLED_DECISION
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable CancelWorkflowExecutionFailedCause

instance FromText CancelWorkflowExecutionFailedCause where
    parser = takeText >>= \case
        "OPERATION_NOT_PERMITTED" -> pure CWEFCOperationNotPermitted
        "UNHANDLED_DECISION"      -> pure CWEFCUnhandledDecision
        e                         -> fail $
            "Failure parsing CancelWorkflowExecutionFailedCause from " ++ show e

instance ToText CancelWorkflowExecutionFailedCause where
    toText = \case
        CWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        CWEFCUnhandledDecision     -> "UNHANDLED_DECISION"

instance ToByteString CancelWorkflowExecutionFailedCause
instance ToHeader     CancelWorkflowExecutionFailedCause
instance ToQuery      CancelWorkflowExecutionFailedCause

instance FromJSON CancelWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "CancelWorkflowExecutionFailedCause"

instance ToJSON CancelWorkflowExecutionFailedCause where
    toJSON = toJSONText

data SignalExternalWorkflowExecutionFailedCause
    = SEWEFCOperationNotPermitted                       -- ^ OPERATION_NOT_PERMITTED
    | SEWEFCSignalExternalWorkflowExecutionRateExceeded -- ^ SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED
    | SEWEFCUnknownExternalWorkflowExecution            -- ^ UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable SignalExternalWorkflowExecutionFailedCause

instance FromText SignalExternalWorkflowExecutionFailedCause where
    parser = takeText >>= \case
        "OPERATION_NOT_PERMITTED"                          -> pure SEWEFCOperationNotPermitted
        "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED" -> pure SEWEFCSignalExternalWorkflowExecutionRateExceeded
        "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"              -> pure SEWEFCUnknownExternalWorkflowExecution
        e                                                  -> fail $
            "Failure parsing SignalExternalWorkflowExecutionFailedCause from " ++ show e

instance ToText SignalExternalWorkflowExecutionFailedCause where
    toText = \case
        SEWEFCOperationNotPermitted                       -> "OPERATION_NOT_PERMITTED"
        SEWEFCSignalExternalWorkflowExecutionRateExceeded -> "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
        SEWEFCUnknownExternalWorkflowExecution            -> "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance ToByteString SignalExternalWorkflowExecutionFailedCause
instance ToHeader     SignalExternalWorkflowExecutionFailedCause
instance ToQuery      SignalExternalWorkflowExecutionFailedCause

instance FromJSON SignalExternalWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "SignalExternalWorkflowExecutionFailedCause"

instance ToJSON SignalExternalWorkflowExecutionFailedCause where
    toJSON = toJSONText

data RecordMarkerDecisionAttributes = RecordMarkerDecisionAttributes
    { _rmdaDetails    :: Maybe Text
    , _rmdaMarkerName :: Text
    } deriving (Eq, Ord, Show)

-- | 'RecordMarkerDecisionAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rmdaDetails' @::@ 'Maybe' 'Text'
--
-- * 'rmdaMarkerName' @::@ 'Text'
--
recordMarkerDecisionAttributes :: Text -- ^ 'rmdaMarkerName'
                               -> RecordMarkerDecisionAttributes
recordMarkerDecisionAttributes p1 = RecordMarkerDecisionAttributes
    { _rmdaMarkerName = p1
    , _rmdaDetails    = Nothing
    }

-- | Optional details of the marker.
rmdaDetails :: Lens' RecordMarkerDecisionAttributes (Maybe Text)
rmdaDetails = lens _rmdaDetails (\s a -> s { _rmdaDetails = a })

-- | The name of the marker. This file is required.
rmdaMarkerName :: Lens' RecordMarkerDecisionAttributes Text
rmdaMarkerName = lens _rmdaMarkerName (\s a -> s { _rmdaMarkerName = a })

instance FromJSON RecordMarkerDecisionAttributes where
    parseJSON = withObject "RecordMarkerDecisionAttributes" $ \o -> RecordMarkerDecisionAttributes
        <$> o .:? "details"
        <*> o .:  "markerName"

instance ToJSON RecordMarkerDecisionAttributes where
    toJSON RecordMarkerDecisionAttributes{..} = object
        [ "markerName" .= _rmdaMarkerName
        , "details"    .= _rmdaDetails
        ]

data CompleteWorkflowExecutionFailedEventAttributes = CompleteWorkflowExecutionFailedEventAttributes
    { _cwefeaCause                        :: CompleteWorkflowExecutionFailedCause
    , _cwefeaDecisionTaskCompletedEventId :: Integer
    } deriving (Eq, Show)

-- | 'CompleteWorkflowExecutionFailedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwefeaCause' @::@ 'CompleteWorkflowExecutionFailedCause'
--
-- * 'cwefeaDecisionTaskCompletedEventId' @::@ 'Integer'
--
completeWorkflowExecutionFailedEventAttributes :: CompleteWorkflowExecutionFailedCause -- ^ 'cwefeaCause'
                                               -> Integer -- ^ 'cwefeaDecisionTaskCompletedEventId'
                                               -> CompleteWorkflowExecutionFailedEventAttributes
completeWorkflowExecutionFailedEventAttributes p1 p2 = CompleteWorkflowExecutionFailedEventAttributes
    { _cwefeaCause                        = p1
    , _cwefeaDecisionTaskCompletedEventId = p2
    }

-- | The cause of the failure. This information is generated by the system and can
-- be useful for diagnostic purposes.
cwefeaCause :: Lens' CompleteWorkflowExecutionFailedEventAttributes CompleteWorkflowExecutionFailedCause
cwefeaCause = lens _cwefeaCause (\s a -> s { _cwefeaCause = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'CompleteWorkflowExecution' decision to complete this
-- execution. This information can be useful for diagnosing problems by tracing
-- back the cause of events.
cwefeaDecisionTaskCompletedEventId :: Lens' CompleteWorkflowExecutionFailedEventAttributes Integer
cwefeaDecisionTaskCompletedEventId =
    lens _cwefeaDecisionTaskCompletedEventId
        (\s a -> s { _cwefeaDecisionTaskCompletedEventId = a })

instance FromJSON CompleteWorkflowExecutionFailedEventAttributes where
    parseJSON = withObject "CompleteWorkflowExecutionFailedEventAttributes" $ \o -> CompleteWorkflowExecutionFailedEventAttributes
        <$> o .:  "cause"
        <*> o .:  "decisionTaskCompletedEventId"

instance ToJSON CompleteWorkflowExecutionFailedEventAttributes where
    toJSON CompleteWorkflowExecutionFailedEventAttributes{..} = object
        [ "cause"                        .= _cwefeaCause
        , "decisionTaskCompletedEventId" .= _cwefeaDecisionTaskCompletedEventId
        ]

data StartTimerDecisionAttributes = StartTimerDecisionAttributes
    { _stdaControl            :: Maybe Text
    , _stdaStartToFireTimeout :: Text
    , _stdaTimerId            :: Text
    } deriving (Eq, Ord, Show)

-- | 'StartTimerDecisionAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stdaControl' @::@ 'Maybe' 'Text'
--
-- * 'stdaStartToFireTimeout' @::@ 'Text'
--
-- * 'stdaTimerId' @::@ 'Text'
--
startTimerDecisionAttributes :: Text -- ^ 'stdaTimerId'
                             -> Text -- ^ 'stdaStartToFireTimeout'
                             -> StartTimerDecisionAttributes
startTimerDecisionAttributes p1 p2 = StartTimerDecisionAttributes
    { _stdaTimerId            = p1
    , _stdaStartToFireTimeout = p2
    , _stdaControl            = Nothing
    }

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
stdaControl :: Lens' StartTimerDecisionAttributes (Maybe Text)
stdaControl = lens _stdaControl (\s a -> s { _stdaControl = a })

-- | The duration to wait before firing the timer. This field is required.
--
-- The duration is specified in seconds. The valid values are integers greater
-- than or equal to 0.
stdaStartToFireTimeout :: Lens' StartTimerDecisionAttributes Text
stdaStartToFireTimeout =
    lens _stdaStartToFireTimeout (\s a -> s { _stdaStartToFireTimeout = a })

-- | The unique Id of the timer. This field is required.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a ':' (colon), '/' (slash), '|' (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string "arn".
stdaTimerId :: Lens' StartTimerDecisionAttributes Text
stdaTimerId = lens _stdaTimerId (\s a -> s { _stdaTimerId = a })

instance FromJSON StartTimerDecisionAttributes where
    parseJSON = withObject "StartTimerDecisionAttributes" $ \o -> StartTimerDecisionAttributes
        <$> o .:? "control"
        <*> o .:  "startToFireTimeout"
        <*> o .:  "timerId"

instance ToJSON StartTimerDecisionAttributes where
    toJSON StartTimerDecisionAttributes{..} = object
        [ "timerId"            .= _stdaTimerId
        , "control"            .= _stdaControl
        , "startToFireTimeout" .= _stdaStartToFireTimeout
        ]

data DecisionType
    = CancelTimer                            -- ^ CancelTimer
    | CancelWorkflowExecution                -- ^ CancelWorkflowExecution
    | CompleteWorkflowExecution              -- ^ CompleteWorkflowExecution
    | ContinueAsNewWorkflowExecution         -- ^ ContinueAsNewWorkflowExecution
    | FailWorkflowExecution                  -- ^ FailWorkflowExecution
    | RecordMarker                           -- ^ RecordMarker
    | RequestCancelActivityTask              -- ^ RequestCancelActivityTask
    | RequestCancelExternalWorkflowExecution -- ^ RequestCancelExternalWorkflowExecution
    | ScheduleActivityTask                   -- ^ ScheduleActivityTask
    | SignalExternalWorkflowExecution        -- ^ SignalExternalWorkflowExecution
    | StartChildWorkflowExecution            -- ^ StartChildWorkflowExecution
    | StartTimer                             -- ^ StartTimer
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable DecisionType

instance FromText DecisionType where
    parser = takeText >>= \case
        "CancelTimer"                            -> pure CancelTimer
        "CancelWorkflowExecution"                -> pure CancelWorkflowExecution
        "CompleteWorkflowExecution"              -> pure CompleteWorkflowExecution
        "ContinueAsNewWorkflowExecution"         -> pure ContinueAsNewWorkflowExecution
        "FailWorkflowExecution"                  -> pure FailWorkflowExecution
        "RecordMarker"                           -> pure RecordMarker
        "RequestCancelActivityTask"              -> pure RequestCancelActivityTask
        "RequestCancelExternalWorkflowExecution" -> pure RequestCancelExternalWorkflowExecution
        "ScheduleActivityTask"                   -> pure ScheduleActivityTask
        "SignalExternalWorkflowExecution"        -> pure SignalExternalWorkflowExecution
        "StartChildWorkflowExecution"            -> pure StartChildWorkflowExecution
        "StartTimer"                             -> pure StartTimer
        e                                        -> fail $
            "Failure parsing DecisionType from " ++ show e

instance ToText DecisionType where
    toText = \case
        CancelTimer                            -> "CancelTimer"
        CancelWorkflowExecution                -> "CancelWorkflowExecution"
        CompleteWorkflowExecution              -> "CompleteWorkflowExecution"
        ContinueAsNewWorkflowExecution         -> "ContinueAsNewWorkflowExecution"
        FailWorkflowExecution                  -> "FailWorkflowExecution"
        RecordMarker                           -> "RecordMarker"
        RequestCancelActivityTask              -> "RequestCancelActivityTask"
        RequestCancelExternalWorkflowExecution -> "RequestCancelExternalWorkflowExecution"
        ScheduleActivityTask                   -> "ScheduleActivityTask"
        SignalExternalWorkflowExecution        -> "SignalExternalWorkflowExecution"
        StartChildWorkflowExecution            -> "StartChildWorkflowExecution"
        StartTimer                             -> "StartTimer"

instance ToByteString DecisionType
instance ToHeader     DecisionType
instance ToQuery      DecisionType

instance FromJSON DecisionType where
    parseJSON = parseJSONText "DecisionType"

instance ToJSON DecisionType where
    toJSON = toJSONText

data RequestCancelExternalWorkflowExecutionFailedEventAttributes = RequestCancelExternalWorkflowExecutionFailedEventAttributes
    { _rcewefeaCause                        :: RequestCancelExternalWorkflowExecutionFailedCause
    , _rcewefeaControl                      :: Maybe Text
    , _rcewefeaDecisionTaskCompletedEventId :: Integer
    , _rcewefeaInitiatedEventId             :: Integer
    , _rcewefeaRunId                        :: Maybe Text
    , _rcewefeaWorkflowId                   :: Text
    } deriving (Eq, Show)

-- | 'RequestCancelExternalWorkflowExecutionFailedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcewefeaCause' @::@ 'RequestCancelExternalWorkflowExecutionFailedCause'
--
-- * 'rcewefeaControl' @::@ 'Maybe' 'Text'
--
-- * 'rcewefeaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'rcewefeaInitiatedEventId' @::@ 'Integer'
--
-- * 'rcewefeaRunId' @::@ 'Maybe' 'Text'
--
-- * 'rcewefeaWorkflowId' @::@ 'Text'
--
requestCancelExternalWorkflowExecutionFailedEventAttributes :: Text -- ^ 'rcewefeaWorkflowId'
                                                            -> RequestCancelExternalWorkflowExecutionFailedCause -- ^ 'rcewefeaCause'
                                                            -> Integer -- ^ 'rcewefeaInitiatedEventId'
                                                            -> Integer -- ^ 'rcewefeaDecisionTaskCompletedEventId'
                                                            -> RequestCancelExternalWorkflowExecutionFailedEventAttributes
requestCancelExternalWorkflowExecutionFailedEventAttributes p1 p2 p3 p4 = RequestCancelExternalWorkflowExecutionFailedEventAttributes
    { _rcewefeaWorkflowId                   = p1
    , _rcewefeaCause                        = p2
    , _rcewefeaInitiatedEventId             = p3
    , _rcewefeaDecisionTaskCompletedEventId = p4
    , _rcewefeaRunId                        = Nothing
    , _rcewefeaControl                      = Nothing
    }

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes.
rcewefeaCause :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes RequestCancelExternalWorkflowExecutionFailedCause
rcewefeaCause = lens _rcewefeaCause (\s a -> s { _rcewefeaCause = a })

rcewefeaControl :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
rcewefeaControl = lens _rcewefeaControl (\s a -> s { _rcewefeaControl = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'RequestCancelExternalWorkflowExecution' decision for this
-- cancellation request. This information can be useful for diagnosing problems
-- by tracing back the cause of events.
rcewefeaDecisionTaskCompletedEventId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes Integer
rcewefeaDecisionTaskCompletedEventId =
    lens _rcewefeaDecisionTaskCompletedEventId
        (\s a -> s { _rcewefeaDecisionTaskCompletedEventId = a })

-- | The id of the 'RequestCancelExternalWorkflowExecutionInitiated' event
-- corresponding to the 'RequestCancelExternalWorkflowExecution' decision to
-- cancel this external workflow execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
rcewefeaInitiatedEventId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes Integer
rcewefeaInitiatedEventId =
    lens _rcewefeaInitiatedEventId
        (\s a -> s { _rcewefeaInitiatedEventId = a })

-- | The 'runId' of the external workflow execution.
rcewefeaRunId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
rcewefeaRunId = lens _rcewefeaRunId (\s a -> s { _rcewefeaRunId = a })

-- | The 'workflowId' of the external workflow to which the cancel request was to
-- be delivered.
rcewefeaWorkflowId :: Lens' RequestCancelExternalWorkflowExecutionFailedEventAttributes Text
rcewefeaWorkflowId =
    lens _rcewefeaWorkflowId (\s a -> s { _rcewefeaWorkflowId = a })

instance FromJSON RequestCancelExternalWorkflowExecutionFailedEventAttributes where
    parseJSON = withObject "RequestCancelExternalWorkflowExecutionFailedEventAttributes" $ \o -> RequestCancelExternalWorkflowExecutionFailedEventAttributes
        <$> o .:  "cause"
        <*> o .:? "control"
        <*> o .:  "decisionTaskCompletedEventId"
        <*> o .:  "initiatedEventId"
        <*> o .:? "runId"
        <*> o .:  "workflowId"

instance ToJSON RequestCancelExternalWorkflowExecutionFailedEventAttributes where
    toJSON RequestCancelExternalWorkflowExecutionFailedEventAttributes{..} = object
        [ "workflowId"                   .= _rcewefeaWorkflowId
        , "runId"                        .= _rcewefeaRunId
        , "cause"                        .= _rcewefeaCause
        , "initiatedEventId"             .= _rcewefeaInitiatedEventId
        , "decisionTaskCompletedEventId" .= _rcewefeaDecisionTaskCompletedEventId
        , "control"                      .= _rcewefeaControl
        ]

data ActivityTypeInfo = ActivityTypeInfo
    { _atiActivityType    :: ActivityType
    , _atiCreationDate    :: POSIX
    , _atiDeprecationDate :: Maybe POSIX
    , _atiDescription     :: Maybe Text
    , _atiStatus          :: RegistrationStatus
    } deriving (Eq, Show)

-- | 'ActivityTypeInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atiActivityType' @::@ 'ActivityType'
--
-- * 'atiCreationDate' @::@ 'UTCTime'
--
-- * 'atiDeprecationDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'atiDescription' @::@ 'Maybe' 'Text'
--
-- * 'atiStatus' @::@ 'RegistrationStatus'
--
activityTypeInfo :: ActivityType -- ^ 'atiActivityType'
                 -> RegistrationStatus -- ^ 'atiStatus'
                 -> UTCTime -- ^ 'atiCreationDate'
                 -> ActivityTypeInfo
activityTypeInfo p1 p2 p3 = ActivityTypeInfo
    { _atiActivityType    = p1
    , _atiStatus          = p2
    , _atiCreationDate    = withIso _Time (const id) p3
    , _atiDescription     = Nothing
    , _atiDeprecationDate = Nothing
    }

-- | The 'ActivityType' type structure representing the activity type.
atiActivityType :: Lens' ActivityTypeInfo ActivityType
atiActivityType = lens _atiActivityType (\s a -> s { _atiActivityType = a })

-- | The date and time this activity type was created through 'RegisterActivityType'
-- .
atiCreationDate :: Lens' ActivityTypeInfo UTCTime
atiCreationDate = lens _atiCreationDate (\s a -> s { _atiCreationDate = a }) . _Time

-- | If DEPRECATED, the date and time 'DeprecateActivityType' was called.
atiDeprecationDate :: Lens' ActivityTypeInfo (Maybe UTCTime)
atiDeprecationDate =
    lens _atiDeprecationDate (\s a -> s { _atiDeprecationDate = a })
        . mapping _Time

-- | The description of the activity type provided in 'RegisterActivityType'.
atiDescription :: Lens' ActivityTypeInfo (Maybe Text)
atiDescription = lens _atiDescription (\s a -> s { _atiDescription = a })

-- | The current status of the activity type.
atiStatus :: Lens' ActivityTypeInfo RegistrationStatus
atiStatus = lens _atiStatus (\s a -> s { _atiStatus = a })

instance FromJSON ActivityTypeInfo where
    parseJSON = withObject "ActivityTypeInfo" $ \o -> ActivityTypeInfo
        <$> o .:  "activityType"
        <*> o .:  "creationDate"
        <*> o .:? "deprecationDate"
        <*> o .:? "description"
        <*> o .:  "status"

instance ToJSON ActivityTypeInfo where
    toJSON ActivityTypeInfo{..} = object
        [ "activityType"    .= _atiActivityType
        , "status"          .= _atiStatus
        , "description"     .= _atiDescription
        , "creationDate"    .= _atiCreationDate
        , "deprecationDate" .= _atiDeprecationDate
        ]

data TimerCanceledEventAttributes = TimerCanceledEventAttributes
    { _tceaDecisionTaskCompletedEventId :: Integer
    , _tceaStartedEventId               :: Integer
    , _tceaTimerId                      :: Text
    } deriving (Eq, Ord, Show)

-- | 'TimerCanceledEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tceaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'tceaStartedEventId' @::@ 'Integer'
--
-- * 'tceaTimerId' @::@ 'Text'
--
timerCanceledEventAttributes :: Text -- ^ 'tceaTimerId'
                             -> Integer -- ^ 'tceaStartedEventId'
                             -> Integer -- ^ 'tceaDecisionTaskCompletedEventId'
                             -> TimerCanceledEventAttributes
timerCanceledEventAttributes p1 p2 p3 = TimerCanceledEventAttributes
    { _tceaTimerId                      = p1
    , _tceaStartedEventId               = p2
    , _tceaDecisionTaskCompletedEventId = p3
    }

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'CancelTimer' decision to cancel this timer. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
tceaDecisionTaskCompletedEventId :: Lens' TimerCanceledEventAttributes Integer
tceaDecisionTaskCompletedEventId =
    lens _tceaDecisionTaskCompletedEventId
        (\s a -> s { _tceaDecisionTaskCompletedEventId = a })

-- | The id of the 'TimerStarted' event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
tceaStartedEventId :: Lens' TimerCanceledEventAttributes Integer
tceaStartedEventId =
    lens _tceaStartedEventId (\s a -> s { _tceaStartedEventId = a })

-- | The unique Id of the timer that was canceled.
tceaTimerId :: Lens' TimerCanceledEventAttributes Text
tceaTimerId = lens _tceaTimerId (\s a -> s { _tceaTimerId = a })

instance FromJSON TimerCanceledEventAttributes where
    parseJSON = withObject "TimerCanceledEventAttributes" $ \o -> TimerCanceledEventAttributes
        <$> o .:  "decisionTaskCompletedEventId"
        <*> o .:  "startedEventId"
        <*> o .:  "timerId"

instance ToJSON TimerCanceledEventAttributes where
    toJSON TimerCanceledEventAttributes{..} = object
        [ "timerId"                      .= _tceaTimerId
        , "startedEventId"               .= _tceaStartedEventId
        , "decisionTaskCompletedEventId" .= _tceaDecisionTaskCompletedEventId
        ]

data WorkflowExecutionStartedEventAttributes = WorkflowExecutionStartedEventAttributes
    { _weseaChildPolicy                  :: ChildPolicy
    , _weseaContinuedExecutionRunId      :: Maybe Text
    , _weseaExecutionStartToCloseTimeout :: Maybe Text
    , _weseaInput                        :: Maybe Text
    , _weseaParentInitiatedEventId       :: Maybe Integer
    , _weseaParentWorkflowExecution      :: Maybe WorkflowExecution
    , _weseaTagList                      :: List "tagList" Text
    , _weseaTaskList                     :: TaskList
    , _weseaTaskStartToCloseTimeout      :: Maybe Text
    , _weseaWorkflowType                 :: WorkflowType
    } deriving (Eq, Show)

-- | 'WorkflowExecutionStartedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'weseaChildPolicy' @::@ 'ChildPolicy'
--
-- * 'weseaContinuedExecutionRunId' @::@ 'Maybe' 'Text'
--
-- * 'weseaExecutionStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'weseaInput' @::@ 'Maybe' 'Text'
--
-- * 'weseaParentInitiatedEventId' @::@ 'Maybe' 'Integer'
--
-- * 'weseaParentWorkflowExecution' @::@ 'Maybe' 'WorkflowExecution'
--
-- * 'weseaTagList' @::@ ['Text']
--
-- * 'weseaTaskList' @::@ 'TaskList'
--
-- * 'weseaTaskStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'weseaWorkflowType' @::@ 'WorkflowType'
--
workflowExecutionStartedEventAttributes :: ChildPolicy -- ^ 'weseaChildPolicy'
                                        -> TaskList -- ^ 'weseaTaskList'
                                        -> WorkflowType -- ^ 'weseaWorkflowType'
                                        -> WorkflowExecutionStartedEventAttributes
workflowExecutionStartedEventAttributes p1 p2 p3 = WorkflowExecutionStartedEventAttributes
    { _weseaChildPolicy                  = p1
    , _weseaTaskList                     = p2
    , _weseaWorkflowType                 = p3
    , _weseaInput                        = Nothing
    , _weseaExecutionStartToCloseTimeout = Nothing
    , _weseaTaskStartToCloseTimeout      = Nothing
    , _weseaTagList                      = mempty
    , _weseaContinuedExecutionRunId      = Nothing
    , _weseaParentWorkflowExecution      = Nothing
    , _weseaParentInitiatedEventId       = Nothing
    }

-- | The policy to use for the child workflow executions if this workflow
-- execution is terminated, by calling the 'TerminateWorkflowExecution' action
-- explicitly or due to an expired timeout. The supported child policies are:   TERMINATE:
-- the child executions will be terminated.  REQUEST_CANCEL: a request to
-- cancel will be attempted for each child execution by recording a 'WorkflowExecutionCancelRequested' event in its history. It is up to the decider to take appropriate actions
-- when it receives an execution history with this event.   ABANDON: no action
-- will be taken. The child executions will continue to run.
weseaChildPolicy :: Lens' WorkflowExecutionStartedEventAttributes ChildPolicy
weseaChildPolicy = lens _weseaChildPolicy (\s a -> s { _weseaChildPolicy = a })

-- | If this workflow execution was started due to a 'ContinueAsNewWorkflowExecution' decision, then it contains the 'runId' of the previous workflow execution that
-- was closed and continued as this execution.
weseaContinuedExecutionRunId :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaContinuedExecutionRunId =
    lens _weseaContinuedExecutionRunId
        (\s a -> s { _weseaContinuedExecutionRunId = a })

-- | The maximum duration for this workflow execution.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
weseaExecutionStartToCloseTimeout :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaExecutionStartToCloseTimeout =
    lens _weseaExecutionStartToCloseTimeout
        (\s a -> s { _weseaExecutionStartToCloseTimeout = a })

-- | The input provided to the workflow execution (if any).
weseaInput :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaInput = lens _weseaInput (\s a -> s { _weseaInput = a })

-- | The id of the 'StartChildWorkflowExecutionInitiated' event corresponding to
-- the 'StartChildWorkflowExecution' 'Decision' to start this workflow execution.
-- The source event with this Id can be found in the history of the source
-- workflow execution. This information can be useful for diagnosing problems by
-- tracing back the chain of events leading up to this event.
weseaParentInitiatedEventId :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Integer)
weseaParentInitiatedEventId =
    lens _weseaParentInitiatedEventId
        (\s a -> s { _weseaParentInitiatedEventId = a })

-- | The source workflow execution that started this workflow execution. The
-- member is not set if the workflow execution was not started by a workflow.
weseaParentWorkflowExecution :: Lens' WorkflowExecutionStartedEventAttributes (Maybe WorkflowExecution)
weseaParentWorkflowExecution =
    lens _weseaParentWorkflowExecution
        (\s a -> s { _weseaParentWorkflowExecution = a })

-- | The list of tags associated with this workflow execution. An execution can
-- have up to 5 tags.
weseaTagList :: Lens' WorkflowExecutionStartedEventAttributes [Text]
weseaTagList = lens _weseaTagList (\s a -> s { _weseaTagList = a }) . _List

-- | The name of the task list for scheduling the decision tasks for this
-- workflow execution.
weseaTaskList :: Lens' WorkflowExecutionStartedEventAttributes TaskList
weseaTaskList = lens _weseaTaskList (\s a -> s { _weseaTaskList = a })

-- | The maximum duration of decision tasks for this workflow type.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
weseaTaskStartToCloseTimeout :: Lens' WorkflowExecutionStartedEventAttributes (Maybe Text)
weseaTaskStartToCloseTimeout =
    lens _weseaTaskStartToCloseTimeout
        (\s a -> s { _weseaTaskStartToCloseTimeout = a })

-- | The workflow type of this execution.
weseaWorkflowType :: Lens' WorkflowExecutionStartedEventAttributes WorkflowType
weseaWorkflowType =
    lens _weseaWorkflowType (\s a -> s { _weseaWorkflowType = a })

instance FromJSON WorkflowExecutionStartedEventAttributes where
    parseJSON = withObject "WorkflowExecutionStartedEventAttributes" $ \o -> WorkflowExecutionStartedEventAttributes
        <$> o .:  "childPolicy"
        <*> o .:? "continuedExecutionRunId"
        <*> o .:? "executionStartToCloseTimeout"
        <*> o .:? "input"
        <*> o .:? "parentInitiatedEventId"
        <*> o .:? "parentWorkflowExecution"
        <*> o .:? "tagList" .!= mempty
        <*> o .:  "taskList"
        <*> o .:? "taskStartToCloseTimeout"
        <*> o .:  "workflowType"

instance ToJSON WorkflowExecutionStartedEventAttributes where
    toJSON WorkflowExecutionStartedEventAttributes{..} = object
        [ "input"                        .= _weseaInput
        , "executionStartToCloseTimeout" .= _weseaExecutionStartToCloseTimeout
        , "taskStartToCloseTimeout"      .= _weseaTaskStartToCloseTimeout
        , "childPolicy"                  .= _weseaChildPolicy
        , "taskList"                     .= _weseaTaskList
        , "workflowType"                 .= _weseaWorkflowType
        , "tagList"                      .= _weseaTagList
        , "continuedExecutionRunId"      .= _weseaContinuedExecutionRunId
        , "parentWorkflowExecution"      .= _weseaParentWorkflowExecution
        , "parentInitiatedEventId"       .= _weseaParentInitiatedEventId
        ]

data WorkflowTypeConfiguration = WorkflowTypeConfiguration
    { _wtcDefaultChildPolicy                  :: Maybe ChildPolicy
    , _wtcDefaultExecutionStartToCloseTimeout :: Maybe Text
    , _wtcDefaultTaskList                     :: Maybe TaskList
    , _wtcDefaultTaskStartToCloseTimeout      :: Maybe Text
    } deriving (Eq, Show)

-- | 'WorkflowTypeConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wtcDefaultChildPolicy' @::@ 'Maybe' 'ChildPolicy'
--
-- * 'wtcDefaultExecutionStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'wtcDefaultTaskList' @::@ 'Maybe' 'TaskList'
--
-- * 'wtcDefaultTaskStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
workflowTypeConfiguration :: WorkflowTypeConfiguration
workflowTypeConfiguration = WorkflowTypeConfiguration
    { _wtcDefaultTaskStartToCloseTimeout      = Nothing
    , _wtcDefaultExecutionStartToCloseTimeout = Nothing
    , _wtcDefaultTaskList                     = Nothing
    , _wtcDefaultChildPolicy                  = Nothing
    }

-- | The optional default policy to use for the child workflow executions when a
-- workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be
-- overridden when starting a workflow execution using the 'StartWorkflowExecution'
-- action or the 'StartChildWorkflowExecution' 'Decision'. The supported child
-- policies are:
--
-- TERMINATE: the child executions will be terminated.  REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a 'WorkflowExecutionCancelRequested' event in its history. It is up to the decider to take appropriate actions
-- when it receives an execution history with this event.   ABANDON: no action
-- will be taken. The child executions will continue to run.
wtcDefaultChildPolicy :: Lens' WorkflowTypeConfiguration (Maybe ChildPolicy)
wtcDefaultChildPolicy =
    lens _wtcDefaultChildPolicy (\s a -> s { _wtcDefaultChildPolicy = a })

-- | The optional default maximum duration, specified when registering the
-- workflow type, for executions of this workflow type. This default can be
-- overridden when starting a workflow execution using the 'StartWorkflowExecution'
-- action or the 'StartChildWorkflowExecution' 'Decision'.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
wtcDefaultExecutionStartToCloseTimeout :: Lens' WorkflowTypeConfiguration (Maybe Text)
wtcDefaultExecutionStartToCloseTimeout =
    lens _wtcDefaultExecutionStartToCloseTimeout
        (\s a -> s { _wtcDefaultExecutionStartToCloseTimeout = a })

-- | The optional default task list, specified when registering the workflow
-- type, for decisions tasks scheduled for workflow executions of this type.
-- This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the 'StartChildWorkflowExecution' 'Decision'.
wtcDefaultTaskList :: Lens' WorkflowTypeConfiguration (Maybe TaskList)
wtcDefaultTaskList =
    lens _wtcDefaultTaskList (\s a -> s { _wtcDefaultTaskList = a })

-- | The optional default maximum duration, specified when registering the
-- workflow type, that a decision task for executions of this workflow type
-- might take before returning completion or failure. If the task does not close
-- in the specified time then the task is automatically timed out and
-- rescheduled. If the decider eventually reports a completion or failure, it is
-- ignored. This default can be overridden when starting a workflow execution
-- using the 'StartWorkflowExecution' action or the 'StartChildWorkflowExecution' 'Decision'.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
wtcDefaultTaskStartToCloseTimeout :: Lens' WorkflowTypeConfiguration (Maybe Text)
wtcDefaultTaskStartToCloseTimeout =
    lens _wtcDefaultTaskStartToCloseTimeout
        (\s a -> s { _wtcDefaultTaskStartToCloseTimeout = a })

instance FromJSON WorkflowTypeConfiguration where
    parseJSON = withObject "WorkflowTypeConfiguration" $ \o -> WorkflowTypeConfiguration
        <$> o .:? "defaultChildPolicy"
        <*> o .:? "defaultExecutionStartToCloseTimeout"
        <*> o .:? "defaultTaskList"
        <*> o .:? "defaultTaskStartToCloseTimeout"

instance ToJSON WorkflowTypeConfiguration where
    toJSON WorkflowTypeConfiguration{..} = object
        [ "defaultTaskStartToCloseTimeout"      .= _wtcDefaultTaskStartToCloseTimeout
        , "defaultExecutionStartToCloseTimeout" .= _wtcDefaultExecutionStartToCloseTimeout
        , "defaultTaskList"                     .= _wtcDefaultTaskList
        , "defaultChildPolicy"                  .= _wtcDefaultChildPolicy
        ]

data ActivityTaskTimeoutType
    = ATTTHeartbeat       -- ^ HEARTBEAT
    | ATTTScheduleToClose -- ^ SCHEDULE_TO_CLOSE
    | ATTTScheduleToStart -- ^ SCHEDULE_TO_START
    | ATTTStartToClose    -- ^ START_TO_CLOSE
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ActivityTaskTimeoutType

instance FromText ActivityTaskTimeoutType where
    parser = takeText >>= \case
        "HEARTBEAT"         -> pure ATTTHeartbeat
        "SCHEDULE_TO_CLOSE" -> pure ATTTScheduleToClose
        "SCHEDULE_TO_START" -> pure ATTTScheduleToStart
        "START_TO_CLOSE"    -> pure ATTTStartToClose
        e                   -> fail $
            "Failure parsing ActivityTaskTimeoutType from " ++ show e

instance ToText ActivityTaskTimeoutType where
    toText = \case
        ATTTHeartbeat       -> "HEARTBEAT"
        ATTTScheduleToClose -> "SCHEDULE_TO_CLOSE"
        ATTTScheduleToStart -> "SCHEDULE_TO_START"
        ATTTStartToClose    -> "START_TO_CLOSE"

instance ToByteString ActivityTaskTimeoutType
instance ToHeader     ActivityTaskTimeoutType
instance ToQuery      ActivityTaskTimeoutType

instance FromJSON ActivityTaskTimeoutType where
    parseJSON = parseJSONText "ActivityTaskTimeoutType"

instance ToJSON ActivityTaskTimeoutType where
    toJSON = toJSONText

data WorkflowType = WorkflowType
    { _wtName    :: Text
    , _wtVersion :: Text
    } deriving (Eq, Ord, Show)

-- | 'WorkflowType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wtName' @::@ 'Text'
--
-- * 'wtVersion' @::@ 'Text'
--
workflowType :: Text -- ^ 'wtName'
             -> Text -- ^ 'wtVersion'
             -> WorkflowType
workflowType p1 p2 = WorkflowType
    { _wtName    = p1
    , _wtVersion = p2
    }

-- | The name of the workflow type. This field is required.
wtName :: Lens' WorkflowType Text
wtName = lens _wtName (\s a -> s { _wtName = a })

-- | The version of the workflow type. This field is required.
wtVersion :: Lens' WorkflowType Text
wtVersion = lens _wtVersion (\s a -> s { _wtVersion = a })

instance FromJSON WorkflowType where
    parseJSON = withObject "WorkflowType" $ \o -> WorkflowType
        <$> o .:  "name"
        <*> o .:  "version"

instance ToJSON WorkflowType where
    toJSON WorkflowType{..} = object
        [ "name"    .= _wtName
        , "version" .= _wtVersion
        ]

data ActivityTaskCompletedEventAttributes = ActivityTaskCompletedEventAttributes
    { _atceaResult           :: Maybe Text
    , _atceaScheduledEventId :: Integer
    , _atceaStartedEventId   :: Integer
    } deriving (Eq, Ord, Show)

-- | 'ActivityTaskCompletedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atceaResult' @::@ 'Maybe' 'Text'
--
-- * 'atceaScheduledEventId' @::@ 'Integer'
--
-- * 'atceaStartedEventId' @::@ 'Integer'
--
activityTaskCompletedEventAttributes :: Integer -- ^ 'atceaScheduledEventId'
                                     -> Integer -- ^ 'atceaStartedEventId'
                                     -> ActivityTaskCompletedEventAttributes
activityTaskCompletedEventAttributes p1 p2 = ActivityTaskCompletedEventAttributes
    { _atceaScheduledEventId = p1
    , _atceaStartedEventId   = p2
    , _atceaResult           = Nothing
    }

-- | The results of the activity task (if any).
atceaResult :: Lens' ActivityTaskCompletedEventAttributes (Maybe Text)
atceaResult = lens _atceaResult (\s a -> s { _atceaResult = a })

-- | The id of the 'ActivityTaskScheduled' event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atceaScheduledEventId :: Lens' ActivityTaskCompletedEventAttributes Integer
atceaScheduledEventId =
    lens _atceaScheduledEventId (\s a -> s { _atceaScheduledEventId = a })

-- | The Id of the 'ActivityTaskStarted' event recorded when this activity task was
-- started. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
atceaStartedEventId :: Lens' ActivityTaskCompletedEventAttributes Integer
atceaStartedEventId =
    lens _atceaStartedEventId (\s a -> s { _atceaStartedEventId = a })

instance FromJSON ActivityTaskCompletedEventAttributes where
    parseJSON = withObject "ActivityTaskCompletedEventAttributes" $ \o -> ActivityTaskCompletedEventAttributes
        <$> o .:? "result"
        <*> o .:  "scheduledEventId"
        <*> o .:  "startedEventId"

instance ToJSON ActivityTaskCompletedEventAttributes where
    toJSON ActivityTaskCompletedEventAttributes{..} = object
        [ "result"           .= _atceaResult
        , "scheduledEventId" .= _atceaScheduledEventId
        , "startedEventId"   .= _atceaStartedEventId
        ]

data ExecutionStatus
    = Closed -- ^ CLOSED
    | Open   -- ^ OPEN
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ExecutionStatus

instance FromText ExecutionStatus where
    parser = takeText >>= \case
        "CLOSED" -> pure Closed
        "OPEN"   -> pure Open
        e        -> fail $
            "Failure parsing ExecutionStatus from " ++ show e

instance ToText ExecutionStatus where
    toText = \case
        Closed -> "CLOSED"
        Open   -> "OPEN"

instance ToByteString ExecutionStatus
instance ToHeader     ExecutionStatus
instance ToQuery      ExecutionStatus

instance FromJSON ExecutionStatus where
    parseJSON = parseJSONText "ExecutionStatus"

instance ToJSON ExecutionStatus where
    toJSON = toJSONText

data DecisionTaskTimeoutType
    = DTTTStartToClose -- ^ START_TO_CLOSE
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable DecisionTaskTimeoutType

instance FromText DecisionTaskTimeoutType where
    parser = takeText >>= \case
        "START_TO_CLOSE" -> pure DTTTStartToClose
        e                -> fail $
            "Failure parsing DecisionTaskTimeoutType from " ++ show e

instance ToText DecisionTaskTimeoutType where
    toText DTTTStartToClose = "START_TO_CLOSE"

instance ToByteString DecisionTaskTimeoutType
instance ToHeader     DecisionTaskTimeoutType
instance ToQuery      DecisionTaskTimeoutType

instance FromJSON DecisionTaskTimeoutType where
    parseJSON = parseJSONText "DecisionTaskTimeoutType"

instance ToJSON DecisionTaskTimeoutType where
    toJSON = toJSONText

data WorkflowExecutionCancelRequestedCause
    = WECRCChildPolicyApplied -- ^ CHILD_POLICY_APPLIED
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable WorkflowExecutionCancelRequestedCause

instance FromText WorkflowExecutionCancelRequestedCause where
    parser = takeText >>= \case
        "CHILD_POLICY_APPLIED" -> pure WECRCChildPolicyApplied
        e                      -> fail $
            "Failure parsing WorkflowExecutionCancelRequestedCause from " ++ show e

instance ToText WorkflowExecutionCancelRequestedCause where
    toText WECRCChildPolicyApplied = "CHILD_POLICY_APPLIED"

instance ToByteString WorkflowExecutionCancelRequestedCause
instance ToHeader     WorkflowExecutionCancelRequestedCause
instance ToQuery      WorkflowExecutionCancelRequestedCause

instance FromJSON WorkflowExecutionCancelRequestedCause where
    parseJSON = parseJSONText "WorkflowExecutionCancelRequestedCause"

instance ToJSON WorkflowExecutionCancelRequestedCause where
    toJSON = toJSONText

data StartChildWorkflowExecutionFailedCause
    = SCWEFCChildCreationRateExceeded                    -- ^ CHILD_CREATION_RATE_EXCEEDED
    | SCWEFCDefaultChildPolicyUndefined                  -- ^ DEFAULT_CHILD_POLICY_UNDEFINED
    | SCWEFCDefaultExecutionStartToCloseTimeoutUndefined -- ^ DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | SCWEFCDefaultTaskListUndefined                     -- ^ DEFAULT_TASK_LIST_UNDEFINED
    | SCWEFCDefaultTaskStartToCloseTimeoutUndefined      -- ^ DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | SCWEFCOpenChildrenLimitExceeded                    -- ^ OPEN_CHILDREN_LIMIT_EXCEEDED
    | SCWEFCOpenWorkflowsLimitExceeded                   -- ^ OPEN_WORKFLOWS_LIMIT_EXCEEDED
    | SCWEFCOperationNotPermitted                        -- ^ OPERATION_NOT_PERMITTED
    | SCWEFCWorkflowAlreadyRunning                       -- ^ WORKFLOW_ALREADY_RUNNING
    | SCWEFCWorkflowTypeDeprecated                       -- ^ WORKFLOW_TYPE_DEPRECATED
    | SCWEFCWorkflowTypeDoesNotExist                     -- ^ WORKFLOW_TYPE_DOES_NOT_EXIST
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable StartChildWorkflowExecutionFailedCause

instance FromText StartChildWorkflowExecutionFailedCause where
    parser = takeText >>= \case
        "CHILD_CREATION_RATE_EXCEEDED"                       -> pure SCWEFCChildCreationRateExceeded
        "DEFAULT_CHILD_POLICY_UNDEFINED"                     -> pure SCWEFCDefaultChildPolicyUndefined
        "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED" -> pure SCWEFCDefaultExecutionStartToCloseTimeoutUndefined
        "DEFAULT_TASK_LIST_UNDEFINED"                        -> pure SCWEFCDefaultTaskListUndefined
        "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"      -> pure SCWEFCDefaultTaskStartToCloseTimeoutUndefined
        "OPEN_CHILDREN_LIMIT_EXCEEDED"                       -> pure SCWEFCOpenChildrenLimitExceeded
        "OPEN_WORKFLOWS_LIMIT_EXCEEDED"                      -> pure SCWEFCOpenWorkflowsLimitExceeded
        "OPERATION_NOT_PERMITTED"                            -> pure SCWEFCOperationNotPermitted
        "WORKFLOW_ALREADY_RUNNING"                           -> pure SCWEFCWorkflowAlreadyRunning
        "WORKFLOW_TYPE_DEPRECATED"                           -> pure SCWEFCWorkflowTypeDeprecated
        "WORKFLOW_TYPE_DOES_NOT_EXIST"                       -> pure SCWEFCWorkflowTypeDoesNotExist
        e                                                    -> fail $
            "Failure parsing StartChildWorkflowExecutionFailedCause from " ++ show e

instance ToText StartChildWorkflowExecutionFailedCause where
    toText = \case
        SCWEFCChildCreationRateExceeded                    -> "CHILD_CREATION_RATE_EXCEEDED"
        SCWEFCDefaultChildPolicyUndefined                  -> "DEFAULT_CHILD_POLICY_UNDEFINED"
        SCWEFCDefaultExecutionStartToCloseTimeoutUndefined -> "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
        SCWEFCDefaultTaskListUndefined                     -> "DEFAULT_TASK_LIST_UNDEFINED"
        SCWEFCDefaultTaskStartToCloseTimeoutUndefined      -> "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
        SCWEFCOpenChildrenLimitExceeded                    -> "OPEN_CHILDREN_LIMIT_EXCEEDED"
        SCWEFCOpenWorkflowsLimitExceeded                   -> "OPEN_WORKFLOWS_LIMIT_EXCEEDED"
        SCWEFCOperationNotPermitted                        -> "OPERATION_NOT_PERMITTED"
        SCWEFCWorkflowAlreadyRunning                       -> "WORKFLOW_ALREADY_RUNNING"
        SCWEFCWorkflowTypeDeprecated                       -> "WORKFLOW_TYPE_DEPRECATED"
        SCWEFCWorkflowTypeDoesNotExist                     -> "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance ToByteString StartChildWorkflowExecutionFailedCause
instance ToHeader     StartChildWorkflowExecutionFailedCause
instance ToQuery      StartChildWorkflowExecutionFailedCause

instance FromJSON StartChildWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "StartChildWorkflowExecutionFailedCause"

instance ToJSON StartChildWorkflowExecutionFailedCause where
    toJSON = toJSONText

data DecisionTaskTimedOutEventAttributes = DecisionTaskTimedOutEventAttributes
    { _dttoeaScheduledEventId :: Integer
    , _dttoeaStartedEventId   :: Integer
    , _dttoeaTimeoutType      :: DecisionTaskTimeoutType
    } deriving (Eq, Show)

-- | 'DecisionTaskTimedOutEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dttoeaScheduledEventId' @::@ 'Integer'
--
-- * 'dttoeaStartedEventId' @::@ 'Integer'
--
-- * 'dttoeaTimeoutType' @::@ 'DecisionTaskTimeoutType'
--
decisionTaskTimedOutEventAttributes :: DecisionTaskTimeoutType -- ^ 'dttoeaTimeoutType'
                                    -> Integer -- ^ 'dttoeaScheduledEventId'
                                    -> Integer -- ^ 'dttoeaStartedEventId'
                                    -> DecisionTaskTimedOutEventAttributes
decisionTaskTimedOutEventAttributes p1 p2 p3 = DecisionTaskTimedOutEventAttributes
    { _dttoeaTimeoutType      = p1
    , _dttoeaScheduledEventId = p2
    , _dttoeaStartedEventId   = p3
    }

-- | The id of the 'DecisionTaskScheduled' event that was recorded when this
-- decision task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
dttoeaScheduledEventId :: Lens' DecisionTaskTimedOutEventAttributes Integer
dttoeaScheduledEventId =
    lens _dttoeaScheduledEventId (\s a -> s { _dttoeaScheduledEventId = a })

-- | The Id of the 'DecisionTaskStarted' event recorded when this decision task was
-- started. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
dttoeaStartedEventId :: Lens' DecisionTaskTimedOutEventAttributes Integer
dttoeaStartedEventId =
    lens _dttoeaStartedEventId (\s a -> s { _dttoeaStartedEventId = a })

-- | The type of timeout that expired before the decision task could be
-- completed.
dttoeaTimeoutType :: Lens' DecisionTaskTimedOutEventAttributes DecisionTaskTimeoutType
dttoeaTimeoutType =
    lens _dttoeaTimeoutType (\s a -> s { _dttoeaTimeoutType = a })

instance FromJSON DecisionTaskTimedOutEventAttributes where
    parseJSON = withObject "DecisionTaskTimedOutEventAttributes" $ \o -> DecisionTaskTimedOutEventAttributes
        <$> o .:  "scheduledEventId"
        <*> o .:  "startedEventId"
        <*> o .:  "timeoutType"

instance ToJSON DecisionTaskTimedOutEventAttributes where
    toJSON DecisionTaskTimedOutEventAttributes{..} = object
        [ "timeoutType"      .= _dttoeaTimeoutType
        , "scheduledEventId" .= _dttoeaScheduledEventId
        , "startedEventId"   .= _dttoeaStartedEventId
        ]

data ChildWorkflowExecutionStartedEventAttributes = ChildWorkflowExecutionStartedEventAttributes
    { _cweseaInitiatedEventId  :: Integer
    , _cweseaWorkflowExecution :: WorkflowExecution
    , _cweseaWorkflowType      :: WorkflowType
    } deriving (Eq, Show)

-- | 'ChildWorkflowExecutionStartedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cweseaInitiatedEventId' @::@ 'Integer'
--
-- * 'cweseaWorkflowExecution' @::@ 'WorkflowExecution'
--
-- * 'cweseaWorkflowType' @::@ 'WorkflowType'
--
childWorkflowExecutionStartedEventAttributes :: WorkflowExecution -- ^ 'cweseaWorkflowExecution'
                                             -> WorkflowType -- ^ 'cweseaWorkflowType'
                                             -> Integer -- ^ 'cweseaInitiatedEventId'
                                             -> ChildWorkflowExecutionStartedEventAttributes
childWorkflowExecutionStartedEventAttributes p1 p2 p3 = ChildWorkflowExecutionStartedEventAttributes
    { _cweseaWorkflowExecution = p1
    , _cweseaWorkflowType      = p2
    , _cweseaInitiatedEventId  = p3
    }

-- | The id of the 'StartChildWorkflowExecutionInitiated' event corresponding to
-- the 'StartChildWorkflowExecution' 'Decision' to start this child workflow
-- execution. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
cweseaInitiatedEventId :: Lens' ChildWorkflowExecutionStartedEventAttributes Integer
cweseaInitiatedEventId =
    lens _cweseaInitiatedEventId (\s a -> s { _cweseaInitiatedEventId = a })

-- | The child workflow execution that was started.
cweseaWorkflowExecution :: Lens' ChildWorkflowExecutionStartedEventAttributes WorkflowExecution
cweseaWorkflowExecution =
    lens _cweseaWorkflowExecution (\s a -> s { _cweseaWorkflowExecution = a })

-- | The type of the child workflow execution.
cweseaWorkflowType :: Lens' ChildWorkflowExecutionStartedEventAttributes WorkflowType
cweseaWorkflowType =
    lens _cweseaWorkflowType (\s a -> s { _cweseaWorkflowType = a })

instance FromJSON ChildWorkflowExecutionStartedEventAttributes where
    parseJSON = withObject "ChildWorkflowExecutionStartedEventAttributes" $ \o -> ChildWorkflowExecutionStartedEventAttributes
        <$> o .:  "initiatedEventId"
        <*> o .:  "workflowExecution"
        <*> o .:  "workflowType"

instance ToJSON ChildWorkflowExecutionStartedEventAttributes where
    toJSON ChildWorkflowExecutionStartedEventAttributes{..} = object
        [ "workflowExecution" .= _cweseaWorkflowExecution
        , "workflowType"      .= _cweseaWorkflowType
        , "initiatedEventId"  .= _cweseaInitiatedEventId
        ]

data CancelTimerFailedEventAttributes = CancelTimerFailedEventAttributes
    { _ctfeaCause                        :: CancelTimerFailedCause
    , _ctfeaDecisionTaskCompletedEventId :: Integer
    , _ctfeaTimerId                      :: Text
    } deriving (Eq, Show)

-- | 'CancelTimerFailedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctfeaCause' @::@ 'CancelTimerFailedCause'
--
-- * 'ctfeaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'ctfeaTimerId' @::@ 'Text'
--
cancelTimerFailedEventAttributes :: Text -- ^ 'ctfeaTimerId'
                                 -> CancelTimerFailedCause -- ^ 'ctfeaCause'
                                 -> Integer -- ^ 'ctfeaDecisionTaskCompletedEventId'
                                 -> CancelTimerFailedEventAttributes
cancelTimerFailedEventAttributes p1 p2 p3 = CancelTimerFailedEventAttributes
    { _ctfeaTimerId                      = p1
    , _ctfeaCause                        = p2
    , _ctfeaDecisionTaskCompletedEventId = p3
    }

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes.
ctfeaCause :: Lens' CancelTimerFailedEventAttributes CancelTimerFailedCause
ctfeaCause = lens _ctfeaCause (\s a -> s { _ctfeaCause = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'CancelTimer' decision to cancel this timer. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
ctfeaDecisionTaskCompletedEventId :: Lens' CancelTimerFailedEventAttributes Integer
ctfeaDecisionTaskCompletedEventId =
    lens _ctfeaDecisionTaskCompletedEventId
        (\s a -> s { _ctfeaDecisionTaskCompletedEventId = a })

-- | The timerId provided in the 'CancelTimer' decision that failed.
ctfeaTimerId :: Lens' CancelTimerFailedEventAttributes Text
ctfeaTimerId = lens _ctfeaTimerId (\s a -> s { _ctfeaTimerId = a })

instance FromJSON CancelTimerFailedEventAttributes where
    parseJSON = withObject "CancelTimerFailedEventAttributes" $ \o -> CancelTimerFailedEventAttributes
        <$> o .:  "cause"
        <*> o .:  "decisionTaskCompletedEventId"
        <*> o .:  "timerId"

instance ToJSON CancelTimerFailedEventAttributes where
    toJSON CancelTimerFailedEventAttributes{..} = object
        [ "timerId"                      .= _ctfeaTimerId
        , "cause"                        .= _ctfeaCause
        , "decisionTaskCompletedEventId" .= _ctfeaDecisionTaskCompletedEventId
        ]

data FailWorkflowExecutionFailedCause
    = FWEFCOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | FWEFCUnhandledDecision     -- ^ UNHANDLED_DECISION
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable FailWorkflowExecutionFailedCause

instance FromText FailWorkflowExecutionFailedCause where
    parser = takeText >>= \case
        "OPERATION_NOT_PERMITTED" -> pure FWEFCOperationNotPermitted
        "UNHANDLED_DECISION"      -> pure FWEFCUnhandledDecision
        e                         -> fail $
            "Failure parsing FailWorkflowExecutionFailedCause from " ++ show e

instance ToText FailWorkflowExecutionFailedCause where
    toText = \case
        FWEFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        FWEFCUnhandledDecision     -> "UNHANDLED_DECISION"

instance ToByteString FailWorkflowExecutionFailedCause
instance ToHeader     FailWorkflowExecutionFailedCause
instance ToQuery      FailWorkflowExecutionFailedCause

instance FromJSON FailWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "FailWorkflowExecutionFailedCause"

instance ToJSON FailWorkflowExecutionFailedCause where
    toJSON = toJSONText

newtype WorkflowExecutionFilter = WorkflowExecutionFilter
    { _wefWorkflowId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'WorkflowExecutionFilter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wefWorkflowId' @::@ 'Text'
--
workflowExecutionFilter :: Text -- ^ 'wefWorkflowId'
                        -> WorkflowExecutionFilter
workflowExecutionFilter p1 = WorkflowExecutionFilter
    { _wefWorkflowId = p1
    }

-- | The workflowId to pass of match the criteria of this filter.
wefWorkflowId :: Lens' WorkflowExecutionFilter Text
wefWorkflowId = lens _wefWorkflowId (\s a -> s { _wefWorkflowId = a })

instance FromJSON WorkflowExecutionFilter where
    parseJSON = withObject "WorkflowExecutionFilter" $ \o -> WorkflowExecutionFilter
        <$> o .:  "workflowId"

instance ToJSON WorkflowExecutionFilter where
    toJSON WorkflowExecutionFilter{..} = object
        [ "workflowId" .= _wefWorkflowId
        ]

data ActivityTaskCanceledEventAttributes = ActivityTaskCanceledEventAttributes
    { _atcea1Details                      :: Maybe Text
    , _atcea1LatestCancelRequestedEventId :: Maybe Integer
    , _atcea1ScheduledEventId             :: Integer
    , _atcea1StartedEventId               :: Integer
    } deriving (Eq, Ord, Show)

-- | 'ActivityTaskCanceledEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atcea1Details' @::@ 'Maybe' 'Text'
--
-- * 'atcea1LatestCancelRequestedEventId' @::@ 'Maybe' 'Integer'
--
-- * 'atcea1ScheduledEventId' @::@ 'Integer'
--
-- * 'atcea1StartedEventId' @::@ 'Integer'
--
activityTaskCanceledEventAttributes :: Integer -- ^ 'atcea1ScheduledEventId'
                                    -> Integer -- ^ 'atcea1StartedEventId'
                                    -> ActivityTaskCanceledEventAttributes
activityTaskCanceledEventAttributes p1 p2 = ActivityTaskCanceledEventAttributes
    { _atcea1ScheduledEventId             = p1
    , _atcea1StartedEventId               = p2
    , _atcea1Details                      = Nothing
    , _atcea1LatestCancelRequestedEventId = Nothing
    }

-- | Details of the cancellation (if any).
atcea1Details :: Lens' ActivityTaskCanceledEventAttributes (Maybe Text)
atcea1Details = lens _atcea1Details (\s a -> s { _atcea1Details = a })

-- | If set, contains the Id of the last 'ActivityTaskCancelRequested' event
-- recorded for this activity task. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
atcea1LatestCancelRequestedEventId :: Lens' ActivityTaskCanceledEventAttributes (Maybe Integer)
atcea1LatestCancelRequestedEventId =
    lens _atcea1LatestCancelRequestedEventId
        (\s a -> s { _atcea1LatestCancelRequestedEventId = a })

-- | The id of the 'ActivityTaskScheduled' event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atcea1ScheduledEventId :: Lens' ActivityTaskCanceledEventAttributes Integer
atcea1ScheduledEventId =
    lens _atcea1ScheduledEventId (\s a -> s { _atcea1ScheduledEventId = a })

-- | The Id of the 'ActivityTaskStarted' event recorded when this activity task was
-- started. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
atcea1StartedEventId :: Lens' ActivityTaskCanceledEventAttributes Integer
atcea1StartedEventId =
    lens _atcea1StartedEventId (\s a -> s { _atcea1StartedEventId = a })

instance FromJSON ActivityTaskCanceledEventAttributes where
    parseJSON = withObject "ActivityTaskCanceledEventAttributes" $ \o -> ActivityTaskCanceledEventAttributes
        <$> o .:? "details"
        <*> o .:? "latestCancelRequestedEventId"
        <*> o .:  "scheduledEventId"
        <*> o .:  "startedEventId"

instance ToJSON ActivityTaskCanceledEventAttributes where
    toJSON ActivityTaskCanceledEventAttributes{..} = object
        [ "details"                      .= _atcea1Details
        , "scheduledEventId"             .= _atcea1ScheduledEventId
        , "startedEventId"               .= _atcea1StartedEventId
        , "latestCancelRequestedEventId" .= _atcea1LatestCancelRequestedEventId
        ]

data WorkflowExecutionInfos = WorkflowExecutionInfos
    { _weiExecutionInfos :: List "executionInfos" WorkflowExecutionInfo
    , _weiNextPageToken  :: Maybe Text
    } deriving (Eq, Show)

-- | 'WorkflowExecutionInfos' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'weiExecutionInfos' @::@ ['WorkflowExecutionInfo']
--
-- * 'weiNextPageToken' @::@ 'Maybe' 'Text'
--
workflowExecutionInfos :: WorkflowExecutionInfos
workflowExecutionInfos = WorkflowExecutionInfos
    { _weiExecutionInfos = mempty
    , _weiNextPageToken  = Nothing
    }

-- | The list of workflow information structures.
weiExecutionInfos :: Lens' WorkflowExecutionInfos [WorkflowExecutionInfo]
weiExecutionInfos =
    lens _weiExecutionInfos (\s a -> s { _weiExecutionInfos = a })
        . _List

-- | The token of the next page in the result. If set, the results have more than
-- one page. The next page can be retrieved by repeating the request with this
-- token and all other arguments unchanged.
weiNextPageToken :: Lens' WorkflowExecutionInfos (Maybe Text)
weiNextPageToken = lens _weiNextPageToken (\s a -> s { _weiNextPageToken = a })

instance FromJSON WorkflowExecutionInfos where
    parseJSON = withObject "WorkflowExecutionInfos" $ \o -> WorkflowExecutionInfos
        <$> o .:? "executionInfos" .!= mempty
        <*> o .:? "nextPageToken"

instance ToJSON WorkflowExecutionInfos where
    toJSON WorkflowExecutionInfos{..} = object
        [ "executionInfos" .= _weiExecutionInfos
        , "nextPageToken"  .= _weiNextPageToken
        ]

data StartChildWorkflowExecutionDecisionAttributes = StartChildWorkflowExecutionDecisionAttributes
    { _scwedaChildPolicy                  :: Maybe ChildPolicy
    , _scwedaControl                      :: Maybe Text
    , _scwedaExecutionStartToCloseTimeout :: Maybe Text
    , _scwedaInput                        :: Maybe Text
    , _scwedaTagList                      :: List "tagList" Text
    , _scwedaTaskList                     :: Maybe TaskList
    , _scwedaTaskStartToCloseTimeout      :: Maybe Text
    , _scwedaWorkflowId                   :: Text
    , _scwedaWorkflowType                 :: WorkflowType
    } deriving (Eq, Show)

-- | 'StartChildWorkflowExecutionDecisionAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scwedaChildPolicy' @::@ 'Maybe' 'ChildPolicy'
--
-- * 'scwedaControl' @::@ 'Maybe' 'Text'
--
-- * 'scwedaExecutionStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'scwedaInput' @::@ 'Maybe' 'Text'
--
-- * 'scwedaTagList' @::@ ['Text']
--
-- * 'scwedaTaskList' @::@ 'Maybe' 'TaskList'
--
-- * 'scwedaTaskStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'scwedaWorkflowId' @::@ 'Text'
--
-- * 'scwedaWorkflowType' @::@ 'WorkflowType'
--
startChildWorkflowExecutionDecisionAttributes :: WorkflowType -- ^ 'scwedaWorkflowType'
                                              -> Text -- ^ 'scwedaWorkflowId'
                                              -> StartChildWorkflowExecutionDecisionAttributes
startChildWorkflowExecutionDecisionAttributes p1 p2 = StartChildWorkflowExecutionDecisionAttributes
    { _scwedaWorkflowType                 = p1
    , _scwedaWorkflowId                   = p2
    , _scwedaControl                      = Nothing
    , _scwedaInput                        = Nothing
    , _scwedaExecutionStartToCloseTimeout = Nothing
    , _scwedaTaskList                     = Nothing
    , _scwedaTaskStartToCloseTimeout      = Nothing
    , _scwedaChildPolicy                  = Nothing
    , _scwedaTagList                      = mempty
    }

-- | If set, specifies the policy to use for the child workflow executions if the
-- workflow execution being started is terminated by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This policy overrides the
-- default child policy specified when registering the workflow type using 'RegisterWorkflowType'. The supported child policies are:
--
-- TERMINATE: the child executions will be terminated.  REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a 'WorkflowExecutionCancelRequested' event in its history. It is up to the decider to take appropriate actions
-- when it receives an execution history with this event.   ABANDON: no action
-- will be taken. The child executions will continue to run.
scwedaChildPolicy :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe ChildPolicy)
scwedaChildPolicy =
    lens _scwedaChildPolicy (\s a -> s { _scwedaChildPolicy = a })

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks. This data is not sent to the child workflow
-- execution.
scwedaControl :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaControl = lens _scwedaControl (\s a -> s { _scwedaControl = a })

-- | The total duration for this workflow execution. This overrides the
-- defaultExecutionStartToCloseTimeout specified when registering the workflow
-- type.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
scwedaExecutionStartToCloseTimeout :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaExecutionStartToCloseTimeout =
    lens _scwedaExecutionStartToCloseTimeout
        (\s a -> s { _scwedaExecutionStartToCloseTimeout = a })

-- | The input to be provided to the workflow execution.
scwedaInput :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaInput = lens _scwedaInput (\s a -> s { _scwedaInput = a })

-- | The list of tags to associate with the child workflow execution. A maximum
-- of 5 tags can be specified. You can list workflow executions with a specific
-- tag by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and
-- specifying a 'TagFilter'.
scwedaTagList :: Lens' StartChildWorkflowExecutionDecisionAttributes [Text]
scwedaTagList = lens _scwedaTagList (\s a -> s { _scwedaTagList = a }) . _List

-- | The name of the task list to be used for decision tasks of the child
-- workflow execution.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a ':' (colon), '/' (slash), '|' (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string "arn".
scwedaTaskList :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe TaskList)
scwedaTaskList = lens _scwedaTaskList (\s a -> s { _scwedaTaskList = a })

-- | Specifies the maximum duration of decision tasks for this workflow
-- execution. This parameter overrides the 'defaultTaskStartToCloseTimout'
-- specified when registering the workflow type using 'RegisterWorkflowType'.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
scwedaTaskStartToCloseTimeout :: Lens' StartChildWorkflowExecutionDecisionAttributes (Maybe Text)
scwedaTaskStartToCloseTimeout =
    lens _scwedaTaskStartToCloseTimeout
        (\s a -> s { _scwedaTaskStartToCloseTimeout = a })

-- | The 'workflowId' of the workflow execution. This field is required.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a ':' (colon), '/' (slash), '|' (vertical bar), or any control characters
-- (\u0000-\u001f | \u007f - \u009f). Also, it must not contain the literal
-- string "arn".
scwedaWorkflowId :: Lens' StartChildWorkflowExecutionDecisionAttributes Text
scwedaWorkflowId = lens _scwedaWorkflowId (\s a -> s { _scwedaWorkflowId = a })

-- | The type of the workflow execution to be started. This field is required.
scwedaWorkflowType :: Lens' StartChildWorkflowExecutionDecisionAttributes WorkflowType
scwedaWorkflowType =
    lens _scwedaWorkflowType (\s a -> s { _scwedaWorkflowType = a })

instance FromJSON StartChildWorkflowExecutionDecisionAttributes where
    parseJSON = withObject "StartChildWorkflowExecutionDecisionAttributes" $ \o -> StartChildWorkflowExecutionDecisionAttributes
        <$> o .:? "childPolicy"
        <*> o .:? "control"
        <*> o .:? "executionStartToCloseTimeout"
        <*> o .:? "input"
        <*> o .:? "tagList" .!= mempty
        <*> o .:? "taskList"
        <*> o .:? "taskStartToCloseTimeout"
        <*> o .:  "workflowId"
        <*> o .:  "workflowType"

instance ToJSON StartChildWorkflowExecutionDecisionAttributes where
    toJSON StartChildWorkflowExecutionDecisionAttributes{..} = object
        [ "workflowType"                 .= _scwedaWorkflowType
        , "workflowId"                   .= _scwedaWorkflowId
        , "control"                      .= _scwedaControl
        , "input"                        .= _scwedaInput
        , "executionStartToCloseTimeout" .= _scwedaExecutionStartToCloseTimeout
        , "taskList"                     .= _scwedaTaskList
        , "taskStartToCloseTimeout"      .= _scwedaTaskStartToCloseTimeout
        , "childPolicy"                  .= _scwedaChildPolicy
        , "tagList"                      .= _scwedaTagList
        ]

data ContinueAsNewWorkflowExecutionFailedCause
    = CANWEFCDefaultChildPolicyUndefined                  -- ^ DEFAULT_CHILD_POLICY_UNDEFINED
    | CANWEFCDefaultExecutionStartToCloseTimeoutUndefined -- ^ DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | CANWEFCDefaultTaskListUndefined                     -- ^ DEFAULT_TASK_LIST_UNDEFINED
    | CANWEFCDefaultTaskStartToCloseTimeoutUndefined      -- ^ DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | CANWEFCOperationNotPermitted                        -- ^ OPERATION_NOT_PERMITTED
    | CANWEFCUnhandledDecision                            -- ^ UNHANDLED_DECISION
    | CANWEFCWorkflowTypeDeprecated                       -- ^ WORKFLOW_TYPE_DEPRECATED
    | CANWEFCWorkflowTypeDoesNotExist                     -- ^ WORKFLOW_TYPE_DOES_NOT_EXIST
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ContinueAsNewWorkflowExecutionFailedCause

instance FromText ContinueAsNewWorkflowExecutionFailedCause where
    parser = takeText >>= \case
        "DEFAULT_CHILD_POLICY_UNDEFINED"                     -> pure CANWEFCDefaultChildPolicyUndefined
        "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED" -> pure CANWEFCDefaultExecutionStartToCloseTimeoutUndefined
        "DEFAULT_TASK_LIST_UNDEFINED"                        -> pure CANWEFCDefaultTaskListUndefined
        "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"      -> pure CANWEFCDefaultTaskStartToCloseTimeoutUndefined
        "OPERATION_NOT_PERMITTED"                            -> pure CANWEFCOperationNotPermitted
        "UNHANDLED_DECISION"                                 -> pure CANWEFCUnhandledDecision
        "WORKFLOW_TYPE_DEPRECATED"                           -> pure CANWEFCWorkflowTypeDeprecated
        "WORKFLOW_TYPE_DOES_NOT_EXIST"                       -> pure CANWEFCWorkflowTypeDoesNotExist
        e                                                    -> fail $
            "Failure parsing ContinueAsNewWorkflowExecutionFailedCause from " ++ show e

instance ToText ContinueAsNewWorkflowExecutionFailedCause where
    toText = \case
        CANWEFCDefaultChildPolicyUndefined                  -> "DEFAULT_CHILD_POLICY_UNDEFINED"
        CANWEFCDefaultExecutionStartToCloseTimeoutUndefined -> "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"
        CANWEFCDefaultTaskListUndefined                     -> "DEFAULT_TASK_LIST_UNDEFINED"
        CANWEFCDefaultTaskStartToCloseTimeoutUndefined      -> "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"
        CANWEFCOperationNotPermitted                        -> "OPERATION_NOT_PERMITTED"
        CANWEFCUnhandledDecision                            -> "UNHANDLED_DECISION"
        CANWEFCWorkflowTypeDeprecated                       -> "WORKFLOW_TYPE_DEPRECATED"
        CANWEFCWorkflowTypeDoesNotExist                     -> "WORKFLOW_TYPE_DOES_NOT_EXIST"

instance ToByteString ContinueAsNewWorkflowExecutionFailedCause
instance ToHeader     ContinueAsNewWorkflowExecutionFailedCause
instance ToQuery      ContinueAsNewWorkflowExecutionFailedCause

instance FromJSON ContinueAsNewWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "ContinueAsNewWorkflowExecutionFailedCause"

instance ToJSON ContinueAsNewWorkflowExecutionFailedCause where
    toJSON = toJSONText

data FailWorkflowExecutionDecisionAttributes = FailWorkflowExecutionDecisionAttributes
    { _fwedaDetails :: Maybe Text
    , _fwedaReason  :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'FailWorkflowExecutionDecisionAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fwedaDetails' @::@ 'Maybe' 'Text'
--
-- * 'fwedaReason' @::@ 'Maybe' 'Text'
--
failWorkflowExecutionDecisionAttributes :: FailWorkflowExecutionDecisionAttributes
failWorkflowExecutionDecisionAttributes = FailWorkflowExecutionDecisionAttributes
    { _fwedaReason  = Nothing
    , _fwedaDetails = Nothing
    }

-- | Optional details of the failure.
fwedaDetails :: Lens' FailWorkflowExecutionDecisionAttributes (Maybe Text)
fwedaDetails = lens _fwedaDetails (\s a -> s { _fwedaDetails = a })

-- | A descriptive reason for the failure that may help in diagnostics.
fwedaReason :: Lens' FailWorkflowExecutionDecisionAttributes (Maybe Text)
fwedaReason = lens _fwedaReason (\s a -> s { _fwedaReason = a })

instance FromJSON FailWorkflowExecutionDecisionAttributes where
    parseJSON = withObject "FailWorkflowExecutionDecisionAttributes" $ \o -> FailWorkflowExecutionDecisionAttributes
        <$> o .:? "details"
        <*> o .:? "reason"

instance ToJSON FailWorkflowExecutionDecisionAttributes where
    toJSON FailWorkflowExecutionDecisionAttributes{..} = object
        [ "reason"  .= _fwedaReason
        , "details" .= _fwedaDetails
        ]

data EventType
    = ActivityTaskCancelRequested                     -- ^ ActivityTaskCancelRequested
    | ActivityTaskCanceled                            -- ^ ActivityTaskCanceled
    | ActivityTaskCompleted                           -- ^ ActivityTaskCompleted
    | ActivityTaskFailed                              -- ^ ActivityTaskFailed
    | ActivityTaskScheduled                           -- ^ ActivityTaskScheduled
    | ActivityTaskStarted                             -- ^ ActivityTaskStarted
    | ActivityTaskTimedOut                            -- ^ ActivityTaskTimedOut
    | CancelTimerFailed                               -- ^ CancelTimerFailed
    | CancelWorkflowExecutionFailed                   -- ^ CancelWorkflowExecutionFailed
    | ChildWorkflowExecutionCanceled                  -- ^ ChildWorkflowExecutionCanceled
    | ChildWorkflowExecutionCompleted                 -- ^ ChildWorkflowExecutionCompleted
    | ChildWorkflowExecutionFailed                    -- ^ ChildWorkflowExecutionFailed
    | ChildWorkflowExecutionStarted                   -- ^ ChildWorkflowExecutionStarted
    | ChildWorkflowExecutionTerminated                -- ^ ChildWorkflowExecutionTerminated
    | ChildWorkflowExecutionTimedOut                  -- ^ ChildWorkflowExecutionTimedOut
    | CompleteWorkflowExecutionFailed                 -- ^ CompleteWorkflowExecutionFailed
    | ContinueAsNewWorkflowExecutionFailed            -- ^ ContinueAsNewWorkflowExecutionFailed
    | DecisionTaskCompleted                           -- ^ DecisionTaskCompleted
    | DecisionTaskScheduled                           -- ^ DecisionTaskScheduled
    | DecisionTaskStarted                             -- ^ DecisionTaskStarted
    | DecisionTaskTimedOut                            -- ^ DecisionTaskTimedOut
    | ExternalWorkflowExecutionCancelRequested        -- ^ ExternalWorkflowExecutionCancelRequested
    | ExternalWorkflowExecutionSignaled               -- ^ ExternalWorkflowExecutionSignaled
    | FailWorkflowExecutionFailed                     -- ^ FailWorkflowExecutionFailed
    | MarkerRecorded                                  -- ^ MarkerRecorded
    | RecordMarkerFailed                              -- ^ RecordMarkerFailed
    | RequestCancelActivityTaskFailed                 -- ^ RequestCancelActivityTaskFailed
    | RequestCancelExternalWorkflowExecutionFailed    -- ^ RequestCancelExternalWorkflowExecutionFailed
    | RequestCancelExternalWorkflowExecutionInitiated -- ^ RequestCancelExternalWorkflowExecutionInitiated
    | ScheduleActivityTaskFailed                      -- ^ ScheduleActivityTaskFailed
    | SignalExternalWorkflowExecutionFailed           -- ^ SignalExternalWorkflowExecutionFailed
    | SignalExternalWorkflowExecutionInitiated        -- ^ SignalExternalWorkflowExecutionInitiated
    | StartChildWorkflowExecutionFailed               -- ^ StartChildWorkflowExecutionFailed
    | StartChildWorkflowExecutionInitiated            -- ^ StartChildWorkflowExecutionInitiated
    | StartTimerFailed                                -- ^ StartTimerFailed
    | TimerCanceled                                   -- ^ TimerCanceled
    | TimerFired                                      -- ^ TimerFired
    | TimerStarted                                    -- ^ TimerStarted
    | WorkflowExecutionCancelRequested                -- ^ WorkflowExecutionCancelRequested
    | WorkflowExecutionCanceled                       -- ^ WorkflowExecutionCanceled
    | WorkflowExecutionCompleted                      -- ^ WorkflowExecutionCompleted
    | WorkflowExecutionContinuedAsNew                 -- ^ WorkflowExecutionContinuedAsNew
    | WorkflowExecutionFailed                         -- ^ WorkflowExecutionFailed
    | WorkflowExecutionSignaled                       -- ^ WorkflowExecutionSignaled
    | WorkflowExecutionStarted                        -- ^ WorkflowExecutionStarted
    | WorkflowExecutionTerminated                     -- ^ WorkflowExecutionTerminated
    | WorkflowExecutionTimedOut                       -- ^ WorkflowExecutionTimedOut
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable EventType

instance FromText EventType where
    parser = takeText >>= \case
        "ActivityTaskCancelRequested"                     -> pure ActivityTaskCancelRequested
        "ActivityTaskCanceled"                            -> pure ActivityTaskCanceled
        "ActivityTaskCompleted"                           -> pure ActivityTaskCompleted
        "ActivityTaskFailed"                              -> pure ActivityTaskFailed
        "ActivityTaskScheduled"                           -> pure ActivityTaskScheduled
        "ActivityTaskStarted"                             -> pure ActivityTaskStarted
        "ActivityTaskTimedOut"                            -> pure ActivityTaskTimedOut
        "CancelTimerFailed"                               -> pure CancelTimerFailed
        "CancelWorkflowExecutionFailed"                   -> pure CancelWorkflowExecutionFailed
        "ChildWorkflowExecutionCanceled"                  -> pure ChildWorkflowExecutionCanceled
        "ChildWorkflowExecutionCompleted"                 -> pure ChildWorkflowExecutionCompleted
        "ChildWorkflowExecutionFailed"                    -> pure ChildWorkflowExecutionFailed
        "ChildWorkflowExecutionStarted"                   -> pure ChildWorkflowExecutionStarted
        "ChildWorkflowExecutionTerminated"                -> pure ChildWorkflowExecutionTerminated
        "ChildWorkflowExecutionTimedOut"                  -> pure ChildWorkflowExecutionTimedOut
        "CompleteWorkflowExecutionFailed"                 -> pure CompleteWorkflowExecutionFailed
        "ContinueAsNewWorkflowExecutionFailed"            -> pure ContinueAsNewWorkflowExecutionFailed
        "DecisionTaskCompleted"                           -> pure DecisionTaskCompleted
        "DecisionTaskScheduled"                           -> pure DecisionTaskScheduled
        "DecisionTaskStarted"                             -> pure DecisionTaskStarted
        "DecisionTaskTimedOut"                            -> pure DecisionTaskTimedOut
        "ExternalWorkflowExecutionCancelRequested"        -> pure ExternalWorkflowExecutionCancelRequested
        "ExternalWorkflowExecutionSignaled"               -> pure ExternalWorkflowExecutionSignaled
        "FailWorkflowExecutionFailed"                     -> pure FailWorkflowExecutionFailed
        "MarkerRecorded"                                  -> pure MarkerRecorded
        "RecordMarkerFailed"                              -> pure RecordMarkerFailed
        "RequestCancelActivityTaskFailed"                 -> pure RequestCancelActivityTaskFailed
        "RequestCancelExternalWorkflowExecutionFailed"    -> pure RequestCancelExternalWorkflowExecutionFailed
        "RequestCancelExternalWorkflowExecutionInitiated" -> pure RequestCancelExternalWorkflowExecutionInitiated
        "ScheduleActivityTaskFailed"                      -> pure ScheduleActivityTaskFailed
        "SignalExternalWorkflowExecutionFailed"           -> pure SignalExternalWorkflowExecutionFailed
        "SignalExternalWorkflowExecutionInitiated"        -> pure SignalExternalWorkflowExecutionInitiated
        "StartChildWorkflowExecutionFailed"               -> pure StartChildWorkflowExecutionFailed
        "StartChildWorkflowExecutionInitiated"            -> pure StartChildWorkflowExecutionInitiated
        "StartTimerFailed"                                -> pure StartTimerFailed
        "TimerCanceled"                                   -> pure TimerCanceled
        "TimerFired"                                      -> pure TimerFired
        "TimerStarted"                                    -> pure TimerStarted
        "WorkflowExecutionCancelRequested"                -> pure WorkflowExecutionCancelRequested
        "WorkflowExecutionCanceled"                       -> pure WorkflowExecutionCanceled
        "WorkflowExecutionCompleted"                      -> pure WorkflowExecutionCompleted
        "WorkflowExecutionContinuedAsNew"                 -> pure WorkflowExecutionContinuedAsNew
        "WorkflowExecutionFailed"                         -> pure WorkflowExecutionFailed
        "WorkflowExecutionSignaled"                       -> pure WorkflowExecutionSignaled
        "WorkflowExecutionStarted"                        -> pure WorkflowExecutionStarted
        "WorkflowExecutionTerminated"                     -> pure WorkflowExecutionTerminated
        "WorkflowExecutionTimedOut"                       -> pure WorkflowExecutionTimedOut
        e                                                 -> fail $
            "Failure parsing EventType from " ++ show e

instance ToText EventType where
    toText = \case
        ActivityTaskCancelRequested                     -> "ActivityTaskCancelRequested"
        ActivityTaskCanceled                            -> "ActivityTaskCanceled"
        ActivityTaskCompleted                           -> "ActivityTaskCompleted"
        ActivityTaskFailed                              -> "ActivityTaskFailed"
        ActivityTaskScheduled                           -> "ActivityTaskScheduled"
        ActivityTaskStarted                             -> "ActivityTaskStarted"
        ActivityTaskTimedOut                            -> "ActivityTaskTimedOut"
        CancelTimerFailed                               -> "CancelTimerFailed"
        CancelWorkflowExecutionFailed                   -> "CancelWorkflowExecutionFailed"
        ChildWorkflowExecutionCanceled                  -> "ChildWorkflowExecutionCanceled"
        ChildWorkflowExecutionCompleted                 -> "ChildWorkflowExecutionCompleted"
        ChildWorkflowExecutionFailed                    -> "ChildWorkflowExecutionFailed"
        ChildWorkflowExecutionStarted                   -> "ChildWorkflowExecutionStarted"
        ChildWorkflowExecutionTerminated                -> "ChildWorkflowExecutionTerminated"
        ChildWorkflowExecutionTimedOut                  -> "ChildWorkflowExecutionTimedOut"
        CompleteWorkflowExecutionFailed                 -> "CompleteWorkflowExecutionFailed"
        ContinueAsNewWorkflowExecutionFailed            -> "ContinueAsNewWorkflowExecutionFailed"
        DecisionTaskCompleted                           -> "DecisionTaskCompleted"
        DecisionTaskScheduled                           -> "DecisionTaskScheduled"
        DecisionTaskStarted                             -> "DecisionTaskStarted"
        DecisionTaskTimedOut                            -> "DecisionTaskTimedOut"
        ExternalWorkflowExecutionCancelRequested        -> "ExternalWorkflowExecutionCancelRequested"
        ExternalWorkflowExecutionSignaled               -> "ExternalWorkflowExecutionSignaled"
        FailWorkflowExecutionFailed                     -> "FailWorkflowExecutionFailed"
        MarkerRecorded                                  -> "MarkerRecorded"
        RecordMarkerFailed                              -> "RecordMarkerFailed"
        RequestCancelActivityTaskFailed                 -> "RequestCancelActivityTaskFailed"
        RequestCancelExternalWorkflowExecutionFailed    -> "RequestCancelExternalWorkflowExecutionFailed"
        RequestCancelExternalWorkflowExecutionInitiated -> "RequestCancelExternalWorkflowExecutionInitiated"
        ScheduleActivityTaskFailed                      -> "ScheduleActivityTaskFailed"
        SignalExternalWorkflowExecutionFailed           -> "SignalExternalWorkflowExecutionFailed"
        SignalExternalWorkflowExecutionInitiated        -> "SignalExternalWorkflowExecutionInitiated"
        StartChildWorkflowExecutionFailed               -> "StartChildWorkflowExecutionFailed"
        StartChildWorkflowExecutionInitiated            -> "StartChildWorkflowExecutionInitiated"
        StartTimerFailed                                -> "StartTimerFailed"
        TimerCanceled                                   -> "TimerCanceled"
        TimerFired                                      -> "TimerFired"
        TimerStarted                                    -> "TimerStarted"
        WorkflowExecutionCancelRequested                -> "WorkflowExecutionCancelRequested"
        WorkflowExecutionCanceled                       -> "WorkflowExecutionCanceled"
        WorkflowExecutionCompleted                      -> "WorkflowExecutionCompleted"
        WorkflowExecutionContinuedAsNew                 -> "WorkflowExecutionContinuedAsNew"
        WorkflowExecutionFailed                         -> "WorkflowExecutionFailed"
        WorkflowExecutionSignaled                       -> "WorkflowExecutionSignaled"
        WorkflowExecutionStarted                        -> "WorkflowExecutionStarted"
        WorkflowExecutionTerminated                     -> "WorkflowExecutionTerminated"
        WorkflowExecutionTimedOut                       -> "WorkflowExecutionTimedOut"

instance ToByteString EventType
instance ToHeader     EventType
instance ToQuery      EventType

instance FromJSON EventType where
    parseJSON = parseJSONText "EventType"

instance ToJSON EventType where
    toJSON = toJSONText

data ActivityTaskTimedOutEventAttributes = ActivityTaskTimedOutEventAttributes
    { _attoeaDetails          :: Maybe Text
    , _attoeaScheduledEventId :: Integer
    , _attoeaStartedEventId   :: Integer
    , _attoeaTimeoutType      :: ActivityTaskTimeoutType
    } deriving (Eq, Show)

-- | 'ActivityTaskTimedOutEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attoeaDetails' @::@ 'Maybe' 'Text'
--
-- * 'attoeaScheduledEventId' @::@ 'Integer'
--
-- * 'attoeaStartedEventId' @::@ 'Integer'
--
-- * 'attoeaTimeoutType' @::@ 'ActivityTaskTimeoutType'
--
activityTaskTimedOutEventAttributes :: ActivityTaskTimeoutType -- ^ 'attoeaTimeoutType'
                                    -> Integer -- ^ 'attoeaScheduledEventId'
                                    -> Integer -- ^ 'attoeaStartedEventId'
                                    -> ActivityTaskTimedOutEventAttributes
activityTaskTimedOutEventAttributes p1 p2 p3 = ActivityTaskTimedOutEventAttributes
    { _attoeaTimeoutType      = p1
    , _attoeaScheduledEventId = p2
    , _attoeaStartedEventId   = p3
    , _attoeaDetails          = Nothing
    }

-- | Contains the content of the 'details' parameter for the last call made by the
-- activity to 'RecordActivityTaskHeartbeat'.
attoeaDetails :: Lens' ActivityTaskTimedOutEventAttributes (Maybe Text)
attoeaDetails = lens _attoeaDetails (\s a -> s { _attoeaDetails = a })

-- | The id of the 'ActivityTaskScheduled' event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
attoeaScheduledEventId :: Lens' ActivityTaskTimedOutEventAttributes Integer
attoeaScheduledEventId =
    lens _attoeaScheduledEventId (\s a -> s { _attoeaScheduledEventId = a })

-- | The Id of the 'ActivityTaskStarted' event recorded when this activity task was
-- started. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
attoeaStartedEventId :: Lens' ActivityTaskTimedOutEventAttributes Integer
attoeaStartedEventId =
    lens _attoeaStartedEventId (\s a -> s { _attoeaStartedEventId = a })

-- | The type of the timeout that caused this event.
attoeaTimeoutType :: Lens' ActivityTaskTimedOutEventAttributes ActivityTaskTimeoutType
attoeaTimeoutType =
    lens _attoeaTimeoutType (\s a -> s { _attoeaTimeoutType = a })

instance FromJSON ActivityTaskTimedOutEventAttributes where
    parseJSON = withObject "ActivityTaskTimedOutEventAttributes" $ \o -> ActivityTaskTimedOutEventAttributes
        <$> o .:? "details"
        <*> o .:  "scheduledEventId"
        <*> o .:  "startedEventId"
        <*> o .:  "timeoutType"

instance ToJSON ActivityTaskTimedOutEventAttributes where
    toJSON ActivityTaskTimedOutEventAttributes{..} = object
        [ "timeoutType"      .= _attoeaTimeoutType
        , "scheduledEventId" .= _attoeaScheduledEventId
        , "startedEventId"   .= _attoeaStartedEventId
        , "details"          .= _attoeaDetails
        ]

data RequestCancelActivityTaskFailedEventAttributes = RequestCancelActivityTaskFailedEventAttributes
    { _rcatfeaActivityId                   :: Text
    , _rcatfeaCause                        :: RequestCancelActivityTaskFailedCause
    , _rcatfeaDecisionTaskCompletedEventId :: Integer
    } deriving (Eq, Show)

-- | 'RequestCancelActivityTaskFailedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcatfeaActivityId' @::@ 'Text'
--
-- * 'rcatfeaCause' @::@ 'RequestCancelActivityTaskFailedCause'
--
-- * 'rcatfeaDecisionTaskCompletedEventId' @::@ 'Integer'
--
requestCancelActivityTaskFailedEventAttributes :: Text -- ^ 'rcatfeaActivityId'
                                               -> RequestCancelActivityTaskFailedCause -- ^ 'rcatfeaCause'
                                               -> Integer -- ^ 'rcatfeaDecisionTaskCompletedEventId'
                                               -> RequestCancelActivityTaskFailedEventAttributes
requestCancelActivityTaskFailedEventAttributes p1 p2 p3 = RequestCancelActivityTaskFailedEventAttributes
    { _rcatfeaActivityId                   = p1
    , _rcatfeaCause                        = p2
    , _rcatfeaDecisionTaskCompletedEventId = p3
    }

-- | The activityId provided in the 'RequestCancelActivityTask' decision that
-- failed.
rcatfeaActivityId :: Lens' RequestCancelActivityTaskFailedEventAttributes Text
rcatfeaActivityId =
    lens _rcatfeaActivityId (\s a -> s { _rcatfeaActivityId = a })

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes.
rcatfeaCause :: Lens' RequestCancelActivityTaskFailedEventAttributes RequestCancelActivityTaskFailedCause
rcatfeaCause = lens _rcatfeaCause (\s a -> s { _rcatfeaCause = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'RequestCancelActivityTask' decision for this cancellation
-- request. This information can be useful for diagnosing problems by tracing
-- back the cause of events.
rcatfeaDecisionTaskCompletedEventId :: Lens' RequestCancelActivityTaskFailedEventAttributes Integer
rcatfeaDecisionTaskCompletedEventId =
    lens _rcatfeaDecisionTaskCompletedEventId
        (\s a -> s { _rcatfeaDecisionTaskCompletedEventId = a })

instance FromJSON RequestCancelActivityTaskFailedEventAttributes where
    parseJSON = withObject "RequestCancelActivityTaskFailedEventAttributes" $ \o -> RequestCancelActivityTaskFailedEventAttributes
        <$> o .:  "activityId"
        <*> o .:  "cause"
        <*> o .:  "decisionTaskCompletedEventId"

instance ToJSON RequestCancelActivityTaskFailedEventAttributes where
    toJSON RequestCancelActivityTaskFailedEventAttributes{..} = object
        [ "activityId"                   .= _rcatfeaActivityId
        , "cause"                        .= _rcatfeaCause
        , "decisionTaskCompletedEventId" .= _rcatfeaDecisionTaskCompletedEventId
        ]

newtype CompleteWorkflowExecutionDecisionAttributes = CompleteWorkflowExecutionDecisionAttributes
    { _cwedaResult :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'CompleteWorkflowExecutionDecisionAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwedaResult' @::@ 'Maybe' 'Text'
--
completeWorkflowExecutionDecisionAttributes :: CompleteWorkflowExecutionDecisionAttributes
completeWorkflowExecutionDecisionAttributes = CompleteWorkflowExecutionDecisionAttributes
    { _cwedaResult = Nothing
    }

-- | The result of the workflow execution. The form of the result is
-- implementation defined.
cwedaResult :: Lens' CompleteWorkflowExecutionDecisionAttributes (Maybe Text)
cwedaResult = lens _cwedaResult (\s a -> s { _cwedaResult = a })

instance FromJSON CompleteWorkflowExecutionDecisionAttributes where
    parseJSON = withObject "CompleteWorkflowExecutionDecisionAttributes" $ \o -> CompleteWorkflowExecutionDecisionAttributes
        <$> o .:? "result"

instance ToJSON CompleteWorkflowExecutionDecisionAttributes where
    toJSON CompleteWorkflowExecutionDecisionAttributes{..} = object
        [ "result" .= _cwedaResult
        ]

data DecisionTaskStartedEventAttributes = DecisionTaskStartedEventAttributes
    { _dtseaIdentity         :: Maybe Text
    , _dtseaScheduledEventId :: Integer
    } deriving (Eq, Ord, Show)

-- | 'DecisionTaskStartedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtseaIdentity' @::@ 'Maybe' 'Text'
--
-- * 'dtseaScheduledEventId' @::@ 'Integer'
--
decisionTaskStartedEventAttributes :: Integer -- ^ 'dtseaScheduledEventId'
                                   -> DecisionTaskStartedEventAttributes
decisionTaskStartedEventAttributes p1 = DecisionTaskStartedEventAttributes
    { _dtseaScheduledEventId = p1
    , _dtseaIdentity         = Nothing
    }

-- | Identity of the decider making the request. This enables diagnostic tracing
-- when problems arise. The form of this identity is user defined.
dtseaIdentity :: Lens' DecisionTaskStartedEventAttributes (Maybe Text)
dtseaIdentity = lens _dtseaIdentity (\s a -> s { _dtseaIdentity = a })

-- | The id of the 'DecisionTaskScheduled' event that was recorded when this
-- decision task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
dtseaScheduledEventId :: Lens' DecisionTaskStartedEventAttributes Integer
dtseaScheduledEventId =
    lens _dtseaScheduledEventId (\s a -> s { _dtseaScheduledEventId = a })

instance FromJSON DecisionTaskStartedEventAttributes where
    parseJSON = withObject "DecisionTaskStartedEventAttributes" $ \o -> DecisionTaskStartedEventAttributes
        <$> o .:? "identity"
        <*> o .:  "scheduledEventId"

instance ToJSON DecisionTaskStartedEventAttributes where
    toJSON DecisionTaskStartedEventAttributes{..} = object
        [ "identity"         .= _dtseaIdentity
        , "scheduledEventId" .= _dtseaScheduledEventId
        ]

data ChildWorkflowExecutionTimedOutEventAttributes = ChildWorkflowExecutionTimedOutEventAttributes
    { _cwetoeaInitiatedEventId  :: Integer
    , _cwetoeaStartedEventId    :: Integer
    , _cwetoeaTimeoutType       :: WorkflowExecutionTimeoutType
    , _cwetoeaWorkflowExecution :: WorkflowExecution
    , _cwetoeaWorkflowType      :: WorkflowType
    } deriving (Eq, Show)

-- | 'ChildWorkflowExecutionTimedOutEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwetoeaInitiatedEventId' @::@ 'Integer'
--
-- * 'cwetoeaStartedEventId' @::@ 'Integer'
--
-- * 'cwetoeaTimeoutType' @::@ 'WorkflowExecutionTimeoutType'
--
-- * 'cwetoeaWorkflowExecution' @::@ 'WorkflowExecution'
--
-- * 'cwetoeaWorkflowType' @::@ 'WorkflowType'
--
childWorkflowExecutionTimedOutEventAttributes :: WorkflowExecution -- ^ 'cwetoeaWorkflowExecution'
                                              -> WorkflowType -- ^ 'cwetoeaWorkflowType'
                                              -> WorkflowExecutionTimeoutType -- ^ 'cwetoeaTimeoutType'
                                              -> Integer -- ^ 'cwetoeaInitiatedEventId'
                                              -> Integer -- ^ 'cwetoeaStartedEventId'
                                              -> ChildWorkflowExecutionTimedOutEventAttributes
childWorkflowExecutionTimedOutEventAttributes p1 p2 p3 p4 p5 = ChildWorkflowExecutionTimedOutEventAttributes
    { _cwetoeaWorkflowExecution = p1
    , _cwetoeaWorkflowType      = p2
    , _cwetoeaTimeoutType       = p3
    , _cwetoeaInitiatedEventId  = p4
    , _cwetoeaStartedEventId    = p5
    }

-- | The id of the 'StartChildWorkflowExecutionInitiated' event corresponding to
-- the 'StartChildWorkflowExecution' 'Decision' to start this child workflow
-- execution. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
cwetoeaInitiatedEventId :: Lens' ChildWorkflowExecutionTimedOutEventAttributes Integer
cwetoeaInitiatedEventId =
    lens _cwetoeaInitiatedEventId (\s a -> s { _cwetoeaInitiatedEventId = a })

-- | The Id of the 'ChildWorkflowExecutionStarted' event recorded when this child
-- workflow execution was started. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
cwetoeaStartedEventId :: Lens' ChildWorkflowExecutionTimedOutEventAttributes Integer
cwetoeaStartedEventId =
    lens _cwetoeaStartedEventId (\s a -> s { _cwetoeaStartedEventId = a })

-- | The type of the timeout that caused the child workflow execution to time
-- out.
cwetoeaTimeoutType :: Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowExecutionTimeoutType
cwetoeaTimeoutType =
    lens _cwetoeaTimeoutType (\s a -> s { _cwetoeaTimeoutType = a })

-- | The child workflow execution that timed out.
cwetoeaWorkflowExecution :: Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowExecution
cwetoeaWorkflowExecution =
    lens _cwetoeaWorkflowExecution
        (\s a -> s { _cwetoeaWorkflowExecution = a })

-- | The type of the child workflow execution.
cwetoeaWorkflowType :: Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowType
cwetoeaWorkflowType =
    lens _cwetoeaWorkflowType (\s a -> s { _cwetoeaWorkflowType = a })

instance FromJSON ChildWorkflowExecutionTimedOutEventAttributes where
    parseJSON = withObject "ChildWorkflowExecutionTimedOutEventAttributes" $ \o -> ChildWorkflowExecutionTimedOutEventAttributes
        <$> o .:  "initiatedEventId"
        <*> o .:  "startedEventId"
        <*> o .:  "timeoutType"
        <*> o .:  "workflowExecution"
        <*> o .:  "workflowType"

instance ToJSON ChildWorkflowExecutionTimedOutEventAttributes where
    toJSON ChildWorkflowExecutionTimedOutEventAttributes{..} = object
        [ "workflowExecution" .= _cwetoeaWorkflowExecution
        , "workflowType"      .= _cwetoeaWorkflowType
        , "timeoutType"       .= _cwetoeaTimeoutType
        , "initiatedEventId"  .= _cwetoeaInitiatedEventId
        , "startedEventId"    .= _cwetoeaStartedEventId
        ]

data StartChildWorkflowExecutionInitiatedEventAttributes = StartChildWorkflowExecutionInitiatedEventAttributes
    { _scweieaChildPolicy                  :: ChildPolicy
    , _scweieaControl                      :: Maybe Text
    , _scweieaDecisionTaskCompletedEventId :: Integer
    , _scweieaExecutionStartToCloseTimeout :: Maybe Text
    , _scweieaInput                        :: Maybe Text
    , _scweieaTagList                      :: List "tagList" Text
    , _scweieaTaskList                     :: TaskList
    , _scweieaTaskStartToCloseTimeout      :: Maybe Text
    , _scweieaWorkflowId                   :: Text
    , _scweieaWorkflowType                 :: WorkflowType
    } deriving (Eq, Show)

-- | 'StartChildWorkflowExecutionInitiatedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scweieaChildPolicy' @::@ 'ChildPolicy'
--
-- * 'scweieaControl' @::@ 'Maybe' 'Text'
--
-- * 'scweieaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'scweieaExecutionStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'scweieaInput' @::@ 'Maybe' 'Text'
--
-- * 'scweieaTagList' @::@ ['Text']
--
-- * 'scweieaTaskList' @::@ 'TaskList'
--
-- * 'scweieaTaskStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'scweieaWorkflowId' @::@ 'Text'
--
-- * 'scweieaWorkflowType' @::@ 'WorkflowType'
--
startChildWorkflowExecutionInitiatedEventAttributes :: Text -- ^ 'scweieaWorkflowId'
                                                    -> WorkflowType -- ^ 'scweieaWorkflowType'
                                                    -> TaskList -- ^ 'scweieaTaskList'
                                                    -> Integer -- ^ 'scweieaDecisionTaskCompletedEventId'
                                                    -> ChildPolicy -- ^ 'scweieaChildPolicy'
                                                    -> StartChildWorkflowExecutionInitiatedEventAttributes
startChildWorkflowExecutionInitiatedEventAttributes p1 p2 p3 p4 p5 = StartChildWorkflowExecutionInitiatedEventAttributes
    { _scweieaWorkflowId                   = p1
    , _scweieaWorkflowType                 = p2
    , _scweieaTaskList                     = p3
    , _scweieaDecisionTaskCompletedEventId = p4
    , _scweieaChildPolicy                  = p5
    , _scweieaControl                      = Nothing
    , _scweieaInput                        = Nothing
    , _scweieaExecutionStartToCloseTimeout = Nothing
    , _scweieaTaskStartToCloseTimeout      = Nothing
    , _scweieaTagList                      = mempty
    }

-- | The policy to use for the child workflow executions if this execution gets
-- terminated by explicitly calling the 'TerminateWorkflowExecution' action or due
-- to an expired timeout.
--
-- The supported child policies are:
--
-- TERMINATE: the child executions will be terminated.  REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a 'WorkflowExecutionCancelRequested' event in its history. It is up to the decider to take appropriate actions
-- when it receives an execution history with this event.   ABANDON: no action
-- will be taken. The child executions will continue to run.
scweieaChildPolicy :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes ChildPolicy
scweieaChildPolicy =
    lens _scweieaChildPolicy (\s a -> s { _scweieaChildPolicy = a })

-- | Optional data attached to the event that can be used by the decider in
-- subsequent decision tasks. This data is not sent to the activity.
scweieaControl :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaControl = lens _scweieaControl (\s a -> s { _scweieaControl = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'StartChildWorkflowExecution' 'Decision' to request this
-- child workflow execution. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
scweieaDecisionTaskCompletedEventId :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes Integer
scweieaDecisionTaskCompletedEventId =
    lens _scweieaDecisionTaskCompletedEventId
        (\s a -> s { _scweieaDecisionTaskCompletedEventId = a })

-- | The maximum duration for the child workflow execution. If the workflow
-- execution is not closed within this duration, it will be timed out and force
-- terminated.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
scweieaExecutionStartToCloseTimeout :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaExecutionStartToCloseTimeout =
    lens _scweieaExecutionStartToCloseTimeout
        (\s a -> s { _scweieaExecutionStartToCloseTimeout = a })

-- | The inputs provided to the child workflow execution (if any).
scweieaInput :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaInput = lens _scweieaInput (\s a -> s { _scweieaInput = a })

-- | The list of tags to associated with the child workflow execution.
scweieaTagList :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes [Text]
scweieaTagList = lens _scweieaTagList (\s a -> s { _scweieaTagList = a }) . _List

-- | The name of the task list used for the decision tasks of the child workflow
-- execution.
scweieaTaskList :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes TaskList
scweieaTaskList = lens _scweieaTaskList (\s a -> s { _scweieaTaskList = a })

-- | The maximum duration allowed for the decision tasks for this workflow
-- execution.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
scweieaTaskStartToCloseTimeout :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes (Maybe Text)
scweieaTaskStartToCloseTimeout =
    lens _scweieaTaskStartToCloseTimeout
        (\s a -> s { _scweieaTaskStartToCloseTimeout = a })

-- | The 'workflowId' of the child workflow execution.
scweieaWorkflowId :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes Text
scweieaWorkflowId =
    lens _scweieaWorkflowId (\s a -> s { _scweieaWorkflowId = a })

-- | The type of the child workflow execution.
scweieaWorkflowType :: Lens' StartChildWorkflowExecutionInitiatedEventAttributes WorkflowType
scweieaWorkflowType =
    lens _scweieaWorkflowType (\s a -> s { _scweieaWorkflowType = a })

instance FromJSON StartChildWorkflowExecutionInitiatedEventAttributes where
    parseJSON = withObject "StartChildWorkflowExecutionInitiatedEventAttributes" $ \o -> StartChildWorkflowExecutionInitiatedEventAttributes
        <$> o .:  "childPolicy"
        <*> o .:? "control"
        <*> o .:  "decisionTaskCompletedEventId"
        <*> o .:? "executionStartToCloseTimeout"
        <*> o .:? "input"
        <*> o .:? "tagList" .!= mempty
        <*> o .:  "taskList"
        <*> o .:? "taskStartToCloseTimeout"
        <*> o .:  "workflowId"
        <*> o .:  "workflowType"

instance ToJSON StartChildWorkflowExecutionInitiatedEventAttributes where
    toJSON StartChildWorkflowExecutionInitiatedEventAttributes{..} = object
        [ "workflowId"                   .= _scweieaWorkflowId
        , "workflowType"                 .= _scweieaWorkflowType
        , "control"                      .= _scweieaControl
        , "input"                        .= _scweieaInput
        , "executionStartToCloseTimeout" .= _scweieaExecutionStartToCloseTimeout
        , "taskList"                     .= _scweieaTaskList
        , "decisionTaskCompletedEventId" .= _scweieaDecisionTaskCompletedEventId
        , "childPolicy"                  .= _scweieaChildPolicy
        , "taskStartToCloseTimeout"      .= _scweieaTaskStartToCloseTimeout
        , "tagList"                      .= _scweieaTagList
        ]

data CancelWorkflowExecutionFailedEventAttributes = CancelWorkflowExecutionFailedEventAttributes
    { _cwefea1Cause                        :: CancelWorkflowExecutionFailedCause
    , _cwefea1DecisionTaskCompletedEventId :: Integer
    } deriving (Eq, Show)

-- | 'CancelWorkflowExecutionFailedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwefea1Cause' @::@ 'CancelWorkflowExecutionFailedCause'
--
-- * 'cwefea1DecisionTaskCompletedEventId' @::@ 'Integer'
--
cancelWorkflowExecutionFailedEventAttributes :: CancelWorkflowExecutionFailedCause -- ^ 'cwefea1Cause'
                                             -> Integer -- ^ 'cwefea1DecisionTaskCompletedEventId'
                                             -> CancelWorkflowExecutionFailedEventAttributes
cancelWorkflowExecutionFailedEventAttributes p1 p2 = CancelWorkflowExecutionFailedEventAttributes
    { _cwefea1Cause                        = p1
    , _cwefea1DecisionTaskCompletedEventId = p2
    }

-- | The cause of the failure. This information is generated by the system and can
-- be useful for diagnostic purposes.
cwefea1Cause :: Lens' CancelWorkflowExecutionFailedEventAttributes CancelWorkflowExecutionFailedCause
cwefea1Cause = lens _cwefea1Cause (\s a -> s { _cwefea1Cause = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'CancelWorkflowExecution' decision for this cancellation
-- request. This information can be useful for diagnosing problems by tracing
-- back the cause of events.
cwefea1DecisionTaskCompletedEventId :: Lens' CancelWorkflowExecutionFailedEventAttributes Integer
cwefea1DecisionTaskCompletedEventId =
    lens _cwefea1DecisionTaskCompletedEventId
        (\s a -> s { _cwefea1DecisionTaskCompletedEventId = a })

instance FromJSON CancelWorkflowExecutionFailedEventAttributes where
    parseJSON = withObject "CancelWorkflowExecutionFailedEventAttributes" $ \o -> CancelWorkflowExecutionFailedEventAttributes
        <$> o .:  "cause"
        <*> o .:  "decisionTaskCompletedEventId"

instance ToJSON CancelWorkflowExecutionFailedEventAttributes where
    toJSON CancelWorkflowExecutionFailedEventAttributes{..} = object
        [ "cause"                        .= _cwefea1Cause
        , "decisionTaskCompletedEventId" .= _cwefea1DecisionTaskCompletedEventId
        ]

data WorkflowExecutionTerminatedEventAttributes = WorkflowExecutionTerminatedEventAttributes
    { _weteaCause       :: Maybe WorkflowExecutionTerminatedCause
    , _weteaChildPolicy :: ChildPolicy
    , _weteaDetails     :: Maybe Text
    , _weteaReason      :: Maybe Text
    } deriving (Eq, Show)

-- | 'WorkflowExecutionTerminatedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'weteaCause' @::@ 'Maybe' 'WorkflowExecutionTerminatedCause'
--
-- * 'weteaChildPolicy' @::@ 'ChildPolicy'
--
-- * 'weteaDetails' @::@ 'Maybe' 'Text'
--
-- * 'weteaReason' @::@ 'Maybe' 'Text'
--
workflowExecutionTerminatedEventAttributes :: ChildPolicy -- ^ 'weteaChildPolicy'
                                           -> WorkflowExecutionTerminatedEventAttributes
workflowExecutionTerminatedEventAttributes p1 = WorkflowExecutionTerminatedEventAttributes
    { _weteaChildPolicy = p1
    , _weteaReason      = Nothing
    , _weteaDetails     = Nothing
    , _weteaCause       = Nothing
    }

-- | If set, indicates that the workflow execution was automatically terminated,
-- and specifies the cause. This happens if the parent workflow execution times
-- out or is terminated and the child policy is set to terminate child
-- executions.
weteaCause :: Lens' WorkflowExecutionTerminatedEventAttributes (Maybe WorkflowExecutionTerminatedCause)
weteaCause = lens _weteaCause (\s a -> s { _weteaCause = a })

-- | The policy used for the child workflow executions of this workflow
-- execution. The supported child policies are:
--
-- TERMINATE: the child executions will be terminated.  REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a 'WorkflowExecutionCancelRequested' event in its history. It is up to the decider to take appropriate actions
-- when it receives an execution history with this event.   ABANDON: no action
-- will be taken. The child executions will continue to run.
weteaChildPolicy :: Lens' WorkflowExecutionTerminatedEventAttributes ChildPolicy
weteaChildPolicy = lens _weteaChildPolicy (\s a -> s { _weteaChildPolicy = a })

-- | The details provided for the termination (if any).
weteaDetails :: Lens' WorkflowExecutionTerminatedEventAttributes (Maybe Text)
weteaDetails = lens _weteaDetails (\s a -> s { _weteaDetails = a })

-- | The reason provided for the termination (if any).
weteaReason :: Lens' WorkflowExecutionTerminatedEventAttributes (Maybe Text)
weteaReason = lens _weteaReason (\s a -> s { _weteaReason = a })

instance FromJSON WorkflowExecutionTerminatedEventAttributes where
    parseJSON = withObject "WorkflowExecutionTerminatedEventAttributes" $ \o -> WorkflowExecutionTerminatedEventAttributes
        <$> o .:? "cause"
        <*> o .:  "childPolicy"
        <*> o .:? "details"
        <*> o .:? "reason"

instance ToJSON WorkflowExecutionTerminatedEventAttributes where
    toJSON WorkflowExecutionTerminatedEventAttributes{..} = object
        [ "reason"      .= _weteaReason
        , "details"     .= _weteaDetails
        , "childPolicy" .= _weteaChildPolicy
        , "cause"       .= _weteaCause
        ]

newtype TaskList = TaskList
    { _tlName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'TaskList' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tlName' @::@ 'Text'
--
taskList :: Text -- ^ 'tlName'
         -> TaskList
taskList p1 = TaskList
    { _tlName = p1
    }

-- | The name of the task list.
tlName :: Lens' TaskList Text
tlName = lens _tlName (\s a -> s { _tlName = a })

instance FromJSON TaskList where
    parseJSON = withObject "TaskList" $ \o -> TaskList
        <$> o .:  "name"

instance ToJSON TaskList where
    toJSON TaskList{..} = object
        [ "name" .= _tlName
        ]

data ScheduleActivityTaskFailedCause
    = SATFCActivityCreationRateExceeded           -- ^ ACTIVITY_CREATION_RATE_EXCEEDED
    | SATFCActivityIdAlreadyInUse                 -- ^ ACTIVITY_ID_ALREADY_IN_USE
    | SATFCActivityTypeDeprecated                 -- ^ ACTIVITY_TYPE_DEPRECATED
    | SATFCActivityTypeDoesNotExist               -- ^ ACTIVITY_TYPE_DOES_NOT_EXIST
    | SATFCDefaultHeartbeatTimeoutUndefined       -- ^ DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED
    | SATFCDefaultScheduleToCloseTimeoutUndefined -- ^ DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED
    | SATFCDefaultScheduleToStartTimeoutUndefined -- ^ DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED
    | SATFCDefaultStartToCloseTimeoutUndefined    -- ^ DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED
    | SATFCDefaultTaskListUndefined               -- ^ DEFAULT_TASK_LIST_UNDEFINED
    | SATFCOpenActivitiesLimitExceeded            -- ^ OPEN_ACTIVITIES_LIMIT_EXCEEDED
    | SATFCOperationNotPermitted                  -- ^ OPERATION_NOT_PERMITTED
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ScheduleActivityTaskFailedCause

instance FromText ScheduleActivityTaskFailedCause where
    parser = takeText >>= \case
        "ACTIVITY_CREATION_RATE_EXCEEDED"             -> pure SATFCActivityCreationRateExceeded
        "ACTIVITY_ID_ALREADY_IN_USE"                  -> pure SATFCActivityIdAlreadyInUse
        "ACTIVITY_TYPE_DEPRECATED"                    -> pure SATFCActivityTypeDeprecated
        "ACTIVITY_TYPE_DOES_NOT_EXIST"                -> pure SATFCActivityTypeDoesNotExist
        "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED"         -> pure SATFCDefaultHeartbeatTimeoutUndefined
        "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED" -> pure SATFCDefaultScheduleToCloseTimeoutUndefined
        "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED" -> pure SATFCDefaultScheduleToStartTimeoutUndefined
        "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED"    -> pure SATFCDefaultStartToCloseTimeoutUndefined
        "DEFAULT_TASK_LIST_UNDEFINED"                 -> pure SATFCDefaultTaskListUndefined
        "OPEN_ACTIVITIES_LIMIT_EXCEEDED"              -> pure SATFCOpenActivitiesLimitExceeded
        "OPERATION_NOT_PERMITTED"                     -> pure SATFCOperationNotPermitted
        e                                             -> fail $
            "Failure parsing ScheduleActivityTaskFailedCause from " ++ show e

instance ToText ScheduleActivityTaskFailedCause where
    toText = \case
        SATFCActivityCreationRateExceeded           -> "ACTIVITY_CREATION_RATE_EXCEEDED"
        SATFCActivityIdAlreadyInUse                 -> "ACTIVITY_ID_ALREADY_IN_USE"
        SATFCActivityTypeDeprecated                 -> "ACTIVITY_TYPE_DEPRECATED"
        SATFCActivityTypeDoesNotExist               -> "ACTIVITY_TYPE_DOES_NOT_EXIST"
        SATFCDefaultHeartbeatTimeoutUndefined       -> "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED"
        SATFCDefaultScheduleToCloseTimeoutUndefined -> "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED"
        SATFCDefaultScheduleToStartTimeoutUndefined -> "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED"
        SATFCDefaultStartToCloseTimeoutUndefined    -> "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED"
        SATFCDefaultTaskListUndefined               -> "DEFAULT_TASK_LIST_UNDEFINED"
        SATFCOpenActivitiesLimitExceeded            -> "OPEN_ACTIVITIES_LIMIT_EXCEEDED"
        SATFCOperationNotPermitted                  -> "OPERATION_NOT_PERMITTED"

instance ToByteString ScheduleActivityTaskFailedCause
instance ToHeader     ScheduleActivityTaskFailedCause
instance ToQuery      ScheduleActivityTaskFailedCause

instance FromJSON ScheduleActivityTaskFailedCause where
    parseJSON = parseJSONText "ScheduleActivityTaskFailedCause"

instance ToJSON ScheduleActivityTaskFailedCause where
    toJSON = toJSONText

data ChildWorkflowExecutionCanceledEventAttributes = ChildWorkflowExecutionCanceledEventAttributes
    { _cwecea1Details           :: Maybe Text
    , _cwecea1InitiatedEventId  :: Integer
    , _cwecea1StartedEventId    :: Integer
    , _cwecea1WorkflowExecution :: WorkflowExecution
    , _cwecea1WorkflowType      :: WorkflowType
    } deriving (Eq, Show)

-- | 'ChildWorkflowExecutionCanceledEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwecea1Details' @::@ 'Maybe' 'Text'
--
-- * 'cwecea1InitiatedEventId' @::@ 'Integer'
--
-- * 'cwecea1StartedEventId' @::@ 'Integer'
--
-- * 'cwecea1WorkflowExecution' @::@ 'WorkflowExecution'
--
-- * 'cwecea1WorkflowType' @::@ 'WorkflowType'
--
childWorkflowExecutionCanceledEventAttributes :: WorkflowExecution -- ^ 'cwecea1WorkflowExecution'
                                              -> WorkflowType -- ^ 'cwecea1WorkflowType'
                                              -> Integer -- ^ 'cwecea1InitiatedEventId'
                                              -> Integer -- ^ 'cwecea1StartedEventId'
                                              -> ChildWorkflowExecutionCanceledEventAttributes
childWorkflowExecutionCanceledEventAttributes p1 p2 p3 p4 = ChildWorkflowExecutionCanceledEventAttributes
    { _cwecea1WorkflowExecution = p1
    , _cwecea1WorkflowType      = p2
    , _cwecea1InitiatedEventId  = p3
    , _cwecea1StartedEventId    = p4
    , _cwecea1Details           = Nothing
    }

-- | Details of the cancellation (if provided).
cwecea1Details :: Lens' ChildWorkflowExecutionCanceledEventAttributes (Maybe Text)
cwecea1Details = lens _cwecea1Details (\s a -> s { _cwecea1Details = a })

-- | The id of the 'StartChildWorkflowExecutionInitiated' event corresponding to
-- the 'StartChildWorkflowExecution' 'Decision' to start this child workflow
-- execution. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
cwecea1InitiatedEventId :: Lens' ChildWorkflowExecutionCanceledEventAttributes Integer
cwecea1InitiatedEventId =
    lens _cwecea1InitiatedEventId (\s a -> s { _cwecea1InitiatedEventId = a })

-- | The Id of the 'ChildWorkflowExecutionStarted' event recorded when this child
-- workflow execution was started. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
cwecea1StartedEventId :: Lens' ChildWorkflowExecutionCanceledEventAttributes Integer
cwecea1StartedEventId =
    lens _cwecea1StartedEventId (\s a -> s { _cwecea1StartedEventId = a })

-- | The child workflow execution that was canceled.
cwecea1WorkflowExecution :: Lens' ChildWorkflowExecutionCanceledEventAttributes WorkflowExecution
cwecea1WorkflowExecution =
    lens _cwecea1WorkflowExecution
        (\s a -> s { _cwecea1WorkflowExecution = a })

-- | The type of the child workflow execution.
cwecea1WorkflowType :: Lens' ChildWorkflowExecutionCanceledEventAttributes WorkflowType
cwecea1WorkflowType =
    lens _cwecea1WorkflowType (\s a -> s { _cwecea1WorkflowType = a })

instance FromJSON ChildWorkflowExecutionCanceledEventAttributes where
    parseJSON = withObject "ChildWorkflowExecutionCanceledEventAttributes" $ \o -> ChildWorkflowExecutionCanceledEventAttributes
        <$> o .:? "details"
        <*> o .:  "initiatedEventId"
        <*> o .:  "startedEventId"
        <*> o .:  "workflowExecution"
        <*> o .:  "workflowType"

instance ToJSON ChildWorkflowExecutionCanceledEventAttributes where
    toJSON ChildWorkflowExecutionCanceledEventAttributes{..} = object
        [ "workflowExecution" .= _cwecea1WorkflowExecution
        , "workflowType"      .= _cwecea1WorkflowType
        , "details"           .= _cwecea1Details
        , "initiatedEventId"  .= _cwecea1InitiatedEventId
        , "startedEventId"    .= _cwecea1StartedEventId
        ]

data WorkflowExecutionInfo = WorkflowExecutionInfo
    { _weiCancelRequested :: Maybe Bool
    , _weiCloseStatus     :: Maybe CloseStatus
    , _weiCloseTimestamp  :: Maybe POSIX
    , _weiExecution       :: WorkflowExecution
    , _weiExecutionStatus :: ExecutionStatus
    , _weiParent          :: Maybe WorkflowExecution
    , _weiStartTimestamp  :: POSIX
    , _weiTagList         :: List "tagList" Text
    , _weiWorkflowType    :: WorkflowType
    } deriving (Eq, Show)

-- | 'WorkflowExecutionInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'weiCancelRequested' @::@ 'Maybe' 'Bool'
--
-- * 'weiCloseStatus' @::@ 'Maybe' 'CloseStatus'
--
-- * 'weiCloseTimestamp' @::@ 'Maybe' 'UTCTime'
--
-- * 'weiExecution' @::@ 'WorkflowExecution'
--
-- * 'weiExecutionStatus' @::@ 'ExecutionStatus'
--
-- * 'weiParent' @::@ 'Maybe' 'WorkflowExecution'
--
-- * 'weiStartTimestamp' @::@ 'UTCTime'
--
-- * 'weiTagList' @::@ ['Text']
--
-- * 'weiWorkflowType' @::@ 'WorkflowType'
--
workflowExecutionInfo :: WorkflowExecution -- ^ 'weiExecution'
                      -> WorkflowType -- ^ 'weiWorkflowType'
                      -> UTCTime -- ^ 'weiStartTimestamp'
                      -> ExecutionStatus -- ^ 'weiExecutionStatus'
                      -> WorkflowExecutionInfo
workflowExecutionInfo p1 p2 p3 p4 = WorkflowExecutionInfo
    { _weiExecution       = p1
    , _weiWorkflowType    = p2
    , _weiStartTimestamp  = withIso _Time (const id) p3
    , _weiExecutionStatus = p4
    , _weiCloseTimestamp  = Nothing
    , _weiCloseStatus     = Nothing
    , _weiParent          = Nothing
    , _weiTagList         = mempty
    , _weiCancelRequested = Nothing
    }

-- | Set to true if a cancellation is requested for this workflow execution.
weiCancelRequested :: Lens' WorkflowExecutionInfo (Maybe Bool)
weiCancelRequested =
    lens _weiCancelRequested (\s a -> s { _weiCancelRequested = a })

-- | If the execution status is closed then this specifies how the execution was
-- closed:
--
-- COMPLETED: the execution was successfully completed. CANCELED: the
-- execution was canceled.Cancellation allows the implementation to gracefully
-- clean up before the execution is closed. TERMINATED: the execution was force
-- terminated. FAILED: the execution failed to complete. TIMED_OUT: the
-- execution did not complete in the alloted time and was automatically timed
-- out. CONTINUED_AS_NEW: the execution is logically continued. This means the
-- current execution was completed and a new execution was started to carry on
-- the workflow.
weiCloseStatus :: Lens' WorkflowExecutionInfo (Maybe CloseStatus)
weiCloseStatus = lens _weiCloseStatus (\s a -> s { _weiCloseStatus = a })

-- | The time when the workflow execution was closed. Set only if the execution
-- status is CLOSED.
weiCloseTimestamp :: Lens' WorkflowExecutionInfo (Maybe UTCTime)
weiCloseTimestamp =
    lens _weiCloseTimestamp (\s a -> s { _weiCloseTimestamp = a })
        . mapping _Time

-- | The workflow execution this information is about.
weiExecution :: Lens' WorkflowExecutionInfo WorkflowExecution
weiExecution = lens _weiExecution (\s a -> s { _weiExecution = a })

-- | The current status of the execution.
weiExecutionStatus :: Lens' WorkflowExecutionInfo ExecutionStatus
weiExecutionStatus =
    lens _weiExecutionStatus (\s a -> s { _weiExecutionStatus = a })

-- | If this workflow execution is a child of another execution then contains the
-- workflow execution that started this execution.
weiParent :: Lens' WorkflowExecutionInfo (Maybe WorkflowExecution)
weiParent = lens _weiParent (\s a -> s { _weiParent = a })

-- | The time when the execution was started.
weiStartTimestamp :: Lens' WorkflowExecutionInfo UTCTime
weiStartTimestamp =
    lens _weiStartTimestamp (\s a -> s { _weiStartTimestamp = a })
        . _Time

-- | The list of tags associated with the workflow execution. Tags can be used to
-- identify and list workflow executions of interest through the visibility
-- APIs. A workflow execution can have a maximum of 5 tags.
weiTagList :: Lens' WorkflowExecutionInfo [Text]
weiTagList = lens _weiTagList (\s a -> s { _weiTagList = a }) . _List

-- | The type of the workflow execution.
weiWorkflowType :: Lens' WorkflowExecutionInfo WorkflowType
weiWorkflowType = lens _weiWorkflowType (\s a -> s { _weiWorkflowType = a })

instance FromJSON WorkflowExecutionInfo where
    parseJSON = withObject "WorkflowExecutionInfo" $ \o -> WorkflowExecutionInfo
        <$> o .:? "cancelRequested"
        <*> o .:? "closeStatus"
        <*> o .:? "closeTimestamp"
        <*> o .:  "execution"
        <*> o .:  "executionStatus"
        <*> o .:? "parent"
        <*> o .:  "startTimestamp"
        <*> o .:? "tagList" .!= mempty
        <*> o .:  "workflowType"

instance ToJSON WorkflowExecutionInfo where
    toJSON WorkflowExecutionInfo{..} = object
        [ "execution"       .= _weiExecution
        , "workflowType"    .= _weiWorkflowType
        , "startTimestamp"  .= _weiStartTimestamp
        , "closeTimestamp"  .= _weiCloseTimestamp
        , "executionStatus" .= _weiExecutionStatus
        , "closeStatus"     .= _weiCloseStatus
        , "parent"          .= _weiParent
        , "tagList"         .= _weiTagList
        , "cancelRequested" .= _weiCancelRequested
        ]

data SignalExternalWorkflowExecutionFailedEventAttributes = SignalExternalWorkflowExecutionFailedEventAttributes
    { _sewefeaCause                        :: SignalExternalWorkflowExecutionFailedCause
    , _sewefeaControl                      :: Maybe Text
    , _sewefeaDecisionTaskCompletedEventId :: Integer
    , _sewefeaInitiatedEventId             :: Integer
    , _sewefeaRunId                        :: Maybe Text
    , _sewefeaWorkflowId                   :: Text
    } deriving (Eq, Show)

-- | 'SignalExternalWorkflowExecutionFailedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sewefeaCause' @::@ 'SignalExternalWorkflowExecutionFailedCause'
--
-- * 'sewefeaControl' @::@ 'Maybe' 'Text'
--
-- * 'sewefeaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'sewefeaInitiatedEventId' @::@ 'Integer'
--
-- * 'sewefeaRunId' @::@ 'Maybe' 'Text'
--
-- * 'sewefeaWorkflowId' @::@ 'Text'
--
signalExternalWorkflowExecutionFailedEventAttributes :: Text -- ^ 'sewefeaWorkflowId'
                                                     -> SignalExternalWorkflowExecutionFailedCause -- ^ 'sewefeaCause'
                                                     -> Integer -- ^ 'sewefeaInitiatedEventId'
                                                     -> Integer -- ^ 'sewefeaDecisionTaskCompletedEventId'
                                                     -> SignalExternalWorkflowExecutionFailedEventAttributes
signalExternalWorkflowExecutionFailedEventAttributes p1 p2 p3 p4 = SignalExternalWorkflowExecutionFailedEventAttributes
    { _sewefeaWorkflowId                   = p1
    , _sewefeaCause                        = p2
    , _sewefeaInitiatedEventId             = p3
    , _sewefeaDecisionTaskCompletedEventId = p4
    , _sewefeaRunId                        = Nothing
    , _sewefeaControl                      = Nothing
    }

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes.
sewefeaCause :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes SignalExternalWorkflowExecutionFailedCause
sewefeaCause = lens _sewefeaCause (\s a -> s { _sewefeaCause = a })

sewefeaControl :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
sewefeaControl = lens _sewefeaControl (\s a -> s { _sewefeaControl = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'SignalExternalWorkflowExecution' decision for this
-- signal. This information can be useful for diagnosing problems by tracing
-- back the cause of events leading up to this event.
sewefeaDecisionTaskCompletedEventId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes Integer
sewefeaDecisionTaskCompletedEventId =
    lens _sewefeaDecisionTaskCompletedEventId
        (\s a -> s { _sewefeaDecisionTaskCompletedEventId = a })

-- | The id of the 'SignalExternalWorkflowExecutionInitiated' event corresponding
-- to the 'SignalExternalWorkflowExecution' decision to request this signal. This
-- information can be useful for diagnosing problems by tracing back the chain
-- of events leading up to this event.
sewefeaInitiatedEventId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes Integer
sewefeaInitiatedEventId =
    lens _sewefeaInitiatedEventId (\s a -> s { _sewefeaInitiatedEventId = a })

-- | The 'runId' of the external workflow execution that the signal was being
-- delivered to.
sewefeaRunId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes (Maybe Text)
sewefeaRunId = lens _sewefeaRunId (\s a -> s { _sewefeaRunId = a })

-- | The 'workflowId' of the external workflow execution that the signal was being
-- delivered to.
sewefeaWorkflowId :: Lens' SignalExternalWorkflowExecutionFailedEventAttributes Text
sewefeaWorkflowId =
    lens _sewefeaWorkflowId (\s a -> s { _sewefeaWorkflowId = a })

instance FromJSON SignalExternalWorkflowExecutionFailedEventAttributes where
    parseJSON = withObject "SignalExternalWorkflowExecutionFailedEventAttributes" $ \o -> SignalExternalWorkflowExecutionFailedEventAttributes
        <$> o .:  "cause"
        <*> o .:? "control"
        <*> o .:  "decisionTaskCompletedEventId"
        <*> o .:  "initiatedEventId"
        <*> o .:? "runId"
        <*> o .:  "workflowId"

instance ToJSON SignalExternalWorkflowExecutionFailedEventAttributes where
    toJSON SignalExternalWorkflowExecutionFailedEventAttributes{..} = object
        [ "workflowId"                   .= _sewefeaWorkflowId
        , "runId"                        .= _sewefeaRunId
        , "cause"                        .= _sewefeaCause
        , "initiatedEventId"             .= _sewefeaInitiatedEventId
        , "decisionTaskCompletedEventId" .= _sewefeaDecisionTaskCompletedEventId
        , "control"                      .= _sewefeaControl
        ]

newtype TagFilter = TagFilter
    { _tfTag :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'TagFilter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tfTag' @::@ 'Text'
--
tagFilter :: Text -- ^ 'tfTag'
          -> TagFilter
tagFilter p1 = TagFilter
    { _tfTag = p1
    }

-- | Specifies the tag that must be associated with the execution for it to meet
-- the filter criteria. This field is required.
tfTag :: Lens' TagFilter Text
tfTag = lens _tfTag (\s a -> s { _tfTag = a })

instance FromJSON TagFilter where
    parseJSON = withObject "TagFilter" $ \o -> TagFilter
        <$> o .:  "tag"

instance ToJSON TagFilter where
    toJSON TagFilter{..} = object
        [ "tag" .= _tfTag
        ]

data ChildPolicy
    = Abandon       -- ^ ABANDON
    | RequestCancel -- ^ REQUEST_CANCEL
    | Terminate     -- ^ TERMINATE
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ChildPolicy

instance FromText ChildPolicy where
    parser = takeText >>= \case
        "ABANDON"        -> pure Abandon
        "REQUEST_CANCEL" -> pure RequestCancel
        "TERMINATE"      -> pure Terminate
        e                -> fail $
            "Failure parsing ChildPolicy from " ++ show e

instance ToText ChildPolicy where
    toText = \case
        Abandon       -> "ABANDON"
        RequestCancel -> "REQUEST_CANCEL"
        Terminate     -> "TERMINATE"

instance ToByteString ChildPolicy
instance ToHeader     ChildPolicy
instance ToQuery      ChildPolicy

instance FromJSON ChildPolicy where
    parseJSON = parseJSONText "ChildPolicy"

instance ToJSON ChildPolicy where
    toJSON = toJSONText

data ActivityTaskStartedEventAttributes = ActivityTaskStartedEventAttributes
    { _atseaIdentity         :: Maybe Text
    , _atseaScheduledEventId :: Integer
    } deriving (Eq, Ord, Show)

-- | 'ActivityTaskStartedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atseaIdentity' @::@ 'Maybe' 'Text'
--
-- * 'atseaScheduledEventId' @::@ 'Integer'
--
activityTaskStartedEventAttributes :: Integer -- ^ 'atseaScheduledEventId'
                                   -> ActivityTaskStartedEventAttributes
activityTaskStartedEventAttributes p1 = ActivityTaskStartedEventAttributes
    { _atseaScheduledEventId = p1
    , _atseaIdentity         = Nothing
    }

-- | Identity of the worker that was assigned this task. This aids diagnostics
-- when problems arise. The form of this identity is user defined.
atseaIdentity :: Lens' ActivityTaskStartedEventAttributes (Maybe Text)
atseaIdentity = lens _atseaIdentity (\s a -> s { _atseaIdentity = a })

-- | The id of the 'ActivityTaskScheduled' event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atseaScheduledEventId :: Lens' ActivityTaskStartedEventAttributes Integer
atseaScheduledEventId =
    lens _atseaScheduledEventId (\s a -> s { _atseaScheduledEventId = a })

instance FromJSON ActivityTaskStartedEventAttributes where
    parseJSON = withObject "ActivityTaskStartedEventAttributes" $ \o -> ActivityTaskStartedEventAttributes
        <$> o .:? "identity"
        <*> o .:  "scheduledEventId"

instance ToJSON ActivityTaskStartedEventAttributes where
    toJSON ActivityTaskStartedEventAttributes{..} = object
        [ "identity"         .= _atseaIdentity
        , "scheduledEventId" .= _atseaScheduledEventId
        ]

data CloseStatus
    = CSCanceled       -- ^ CANCELED
    | CSCompleted      -- ^ COMPLETED
    | CSContinuedAsNew -- ^ CONTINUED_AS_NEW
    | CSFailed         -- ^ FAILED
    | CSTerminated     -- ^ TERMINATED
    | CSTimedOut       -- ^ TIMED_OUT
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable CloseStatus

instance FromText CloseStatus where
    parser = takeText >>= \case
        "CANCELED"         -> pure CSCanceled
        "COMPLETED"        -> pure CSCompleted
        "CONTINUED_AS_NEW" -> pure CSContinuedAsNew
        "FAILED"           -> pure CSFailed
        "TERMINATED"       -> pure CSTerminated
        "TIMED_OUT"        -> pure CSTimedOut
        e                  -> fail $
            "Failure parsing CloseStatus from " ++ show e

instance ToText CloseStatus where
    toText = \case
        CSCanceled       -> "CANCELED"
        CSCompleted      -> "COMPLETED"
        CSContinuedAsNew -> "CONTINUED_AS_NEW"
        CSFailed         -> "FAILED"
        CSTerminated     -> "TERMINATED"
        CSTimedOut       -> "TIMED_OUT"

instance ToByteString CloseStatus
instance ToHeader     CloseStatus
instance ToQuery      CloseStatus

instance FromJSON CloseStatus where
    parseJSON = parseJSONText "CloseStatus"

instance ToJSON CloseStatus where
    toJSON = toJSONText

data CompleteWorkflowExecutionFailedCause
    = CompleteWorkflowExecutionFailedCauseOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | CompleteWorkflowExecutionFailedCauseUnhandledDecision     -- ^ UNHANDLED_DECISION
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable CompleteWorkflowExecutionFailedCause

instance FromText CompleteWorkflowExecutionFailedCause where
    parser = takeText >>= \case
        "OPERATION_NOT_PERMITTED" -> pure CompleteWorkflowExecutionFailedCauseOperationNotPermitted
        "UNHANDLED_DECISION"      -> pure CompleteWorkflowExecutionFailedCauseUnhandledDecision
        e                         -> fail $
            "Failure parsing CompleteWorkflowExecutionFailedCause from " ++ show e

instance ToText CompleteWorkflowExecutionFailedCause where
    toText = \case
        CompleteWorkflowExecutionFailedCauseOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        CompleteWorkflowExecutionFailedCauseUnhandledDecision     -> "UNHANDLED_DECISION"

instance ToByteString CompleteWorkflowExecutionFailedCause
instance ToHeader     CompleteWorkflowExecutionFailedCause
instance ToQuery      CompleteWorkflowExecutionFailedCause

instance FromJSON CompleteWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "CompleteWorkflowExecutionFailedCause"

instance ToJSON CompleteWorkflowExecutionFailedCause where
    toJSON = toJSONText

data StartTimerFailedCause
    = STFCOpenTimersLimitExceeded   -- ^ OPEN_TIMERS_LIMIT_EXCEEDED
    | STFCOperationNotPermitted     -- ^ OPERATION_NOT_PERMITTED
    | STFCTimerCreationRateExceeded -- ^ TIMER_CREATION_RATE_EXCEEDED
    | STFCTimerIdAlreadyInUse       -- ^ TIMER_ID_ALREADY_IN_USE
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable StartTimerFailedCause

instance FromText StartTimerFailedCause where
    parser = takeText >>= \case
        "OPEN_TIMERS_LIMIT_EXCEEDED"   -> pure STFCOpenTimersLimitExceeded
        "OPERATION_NOT_PERMITTED"      -> pure STFCOperationNotPermitted
        "TIMER_CREATION_RATE_EXCEEDED" -> pure STFCTimerCreationRateExceeded
        "TIMER_ID_ALREADY_IN_USE"      -> pure STFCTimerIdAlreadyInUse
        e                              -> fail $
            "Failure parsing StartTimerFailedCause from " ++ show e

instance ToText StartTimerFailedCause where
    toText = \case
        STFCOpenTimersLimitExceeded   -> "OPEN_TIMERS_LIMIT_EXCEEDED"
        STFCOperationNotPermitted     -> "OPERATION_NOT_PERMITTED"
        STFCTimerCreationRateExceeded -> "TIMER_CREATION_RATE_EXCEEDED"
        STFCTimerIdAlreadyInUse       -> "TIMER_ID_ALREADY_IN_USE"

instance ToByteString StartTimerFailedCause
instance ToHeader     StartTimerFailedCause
instance ToQuery      StartTimerFailedCause

instance FromJSON StartTimerFailedCause where
    parseJSON = parseJSONText "StartTimerFailedCause"

instance ToJSON StartTimerFailedCause where
    toJSON = toJSONText

data ActivityTaskCancelRequestedEventAttributes = ActivityTaskCancelRequestedEventAttributes
    { _atcreaActivityId                   :: Text
    , _atcreaDecisionTaskCompletedEventId :: Integer
    } deriving (Eq, Ord, Show)

-- | 'ActivityTaskCancelRequestedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atcreaActivityId' @::@ 'Text'
--
-- * 'atcreaDecisionTaskCompletedEventId' @::@ 'Integer'
--
activityTaskCancelRequestedEventAttributes :: Integer -- ^ 'atcreaDecisionTaskCompletedEventId'
                                           -> Text -- ^ 'atcreaActivityId'
                                           -> ActivityTaskCancelRequestedEventAttributes
activityTaskCancelRequestedEventAttributes p1 p2 = ActivityTaskCancelRequestedEventAttributes
    { _atcreaDecisionTaskCompletedEventId = p1
    , _atcreaActivityId                   = p2
    }

-- | The unique ID of the task.
atcreaActivityId :: Lens' ActivityTaskCancelRequestedEventAttributes Text
atcreaActivityId = lens _atcreaActivityId (\s a -> s { _atcreaActivityId = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'RequestCancelActivityTask' decision for this cancellation
-- request. This information can be useful for diagnosing problems by tracing
-- back the cause of events.
atcreaDecisionTaskCompletedEventId :: Lens' ActivityTaskCancelRequestedEventAttributes Integer
atcreaDecisionTaskCompletedEventId =
    lens _atcreaDecisionTaskCompletedEventId
        (\s a -> s { _atcreaDecisionTaskCompletedEventId = a })

instance FromJSON ActivityTaskCancelRequestedEventAttributes where
    parseJSON = withObject "ActivityTaskCancelRequestedEventAttributes" $ \o -> ActivityTaskCancelRequestedEventAttributes
        <$> o .:  "activityId"
        <*> o .:  "decisionTaskCompletedEventId"

instance ToJSON ActivityTaskCancelRequestedEventAttributes where
    toJSON ActivityTaskCancelRequestedEventAttributes{..} = object
        [ "decisionTaskCompletedEventId" .= _atcreaDecisionTaskCompletedEventId
        , "activityId"                   .= _atcreaActivityId
        ]

data WorkflowExecutionTimedOutEventAttributes = WorkflowExecutionTimedOutEventAttributes
    { _wetoeaChildPolicy :: ChildPolicy
    , _wetoeaTimeoutType :: WorkflowExecutionTimeoutType
    } deriving (Eq, Show)

-- | 'WorkflowExecutionTimedOutEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wetoeaChildPolicy' @::@ 'ChildPolicy'
--
-- * 'wetoeaTimeoutType' @::@ 'WorkflowExecutionTimeoutType'
--
workflowExecutionTimedOutEventAttributes :: WorkflowExecutionTimeoutType -- ^ 'wetoeaTimeoutType'
                                         -> ChildPolicy -- ^ 'wetoeaChildPolicy'
                                         -> WorkflowExecutionTimedOutEventAttributes
workflowExecutionTimedOutEventAttributes p1 p2 = WorkflowExecutionTimedOutEventAttributes
    { _wetoeaTimeoutType = p1
    , _wetoeaChildPolicy = p2
    }

-- | The policy used for the child workflow executions of this workflow
-- execution. The supported child policies are:   TERMINATE: the child
-- executions will be terminated.  REQUEST_CANCEL: a request to cancel will be
-- attempted for each child execution by recording a 'WorkflowExecutionCancelRequested' event in its history. It is up to the decider to take appropriate actions
-- when it receives an execution history with this event.   ABANDON: no action
-- will be taken. The child executions will continue to run.
wetoeaChildPolicy :: Lens' WorkflowExecutionTimedOutEventAttributes ChildPolicy
wetoeaChildPolicy =
    lens _wetoeaChildPolicy (\s a -> s { _wetoeaChildPolicy = a })

-- | The type of timeout that caused this event.
wetoeaTimeoutType :: Lens' WorkflowExecutionTimedOutEventAttributes WorkflowExecutionTimeoutType
wetoeaTimeoutType =
    lens _wetoeaTimeoutType (\s a -> s { _wetoeaTimeoutType = a })

instance FromJSON WorkflowExecutionTimedOutEventAttributes where
    parseJSON = withObject "WorkflowExecutionTimedOutEventAttributes" $ \o -> WorkflowExecutionTimedOutEventAttributes
        <$> o .:  "childPolicy"
        <*> o .:  "timeoutType"

instance ToJSON WorkflowExecutionTimedOutEventAttributes where
    toJSON WorkflowExecutionTimedOutEventAttributes{..} = object
        [ "timeoutType" .= _wetoeaTimeoutType
        , "childPolicy" .= _wetoeaChildPolicy
        ]

data ChildWorkflowExecutionTerminatedEventAttributes = ChildWorkflowExecutionTerminatedEventAttributes
    { _cweteaInitiatedEventId  :: Integer
    , _cweteaStartedEventId    :: Integer
    , _cweteaWorkflowExecution :: WorkflowExecution
    , _cweteaWorkflowType      :: WorkflowType
    } deriving (Eq, Show)

-- | 'ChildWorkflowExecutionTerminatedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cweteaInitiatedEventId' @::@ 'Integer'
--
-- * 'cweteaStartedEventId' @::@ 'Integer'
--
-- * 'cweteaWorkflowExecution' @::@ 'WorkflowExecution'
--
-- * 'cweteaWorkflowType' @::@ 'WorkflowType'
--
childWorkflowExecutionTerminatedEventAttributes :: WorkflowExecution -- ^ 'cweteaWorkflowExecution'
                                                -> WorkflowType -- ^ 'cweteaWorkflowType'
                                                -> Integer -- ^ 'cweteaInitiatedEventId'
                                                -> Integer -- ^ 'cweteaStartedEventId'
                                                -> ChildWorkflowExecutionTerminatedEventAttributes
childWorkflowExecutionTerminatedEventAttributes p1 p2 p3 p4 = ChildWorkflowExecutionTerminatedEventAttributes
    { _cweteaWorkflowExecution = p1
    , _cweteaWorkflowType      = p2
    , _cweteaInitiatedEventId  = p3
    , _cweteaStartedEventId    = p4
    }

-- | The id of the 'StartChildWorkflowExecutionInitiated' event corresponding to
-- the 'StartChildWorkflowExecution' 'Decision' to start this child workflow
-- execution. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
cweteaInitiatedEventId :: Lens' ChildWorkflowExecutionTerminatedEventAttributes Integer
cweteaInitiatedEventId =
    lens _cweteaInitiatedEventId (\s a -> s { _cweteaInitiatedEventId = a })

-- | The Id of the 'ChildWorkflowExecutionStarted' event recorded when this child
-- workflow execution was started. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
cweteaStartedEventId :: Lens' ChildWorkflowExecutionTerminatedEventAttributes Integer
cweteaStartedEventId =
    lens _cweteaStartedEventId (\s a -> s { _cweteaStartedEventId = a })

-- | The child workflow execution that was terminated.
cweteaWorkflowExecution :: Lens' ChildWorkflowExecutionTerminatedEventAttributes WorkflowExecution
cweteaWorkflowExecution =
    lens _cweteaWorkflowExecution (\s a -> s { _cweteaWorkflowExecution = a })

-- | The type of the child workflow execution.
cweteaWorkflowType :: Lens' ChildWorkflowExecutionTerminatedEventAttributes WorkflowType
cweteaWorkflowType =
    lens _cweteaWorkflowType (\s a -> s { _cweteaWorkflowType = a })

instance FromJSON ChildWorkflowExecutionTerminatedEventAttributes where
    parseJSON = withObject "ChildWorkflowExecutionTerminatedEventAttributes" $ \o -> ChildWorkflowExecutionTerminatedEventAttributes
        <$> o .:  "initiatedEventId"
        <*> o .:  "startedEventId"
        <*> o .:  "workflowExecution"
        <*> o .:  "workflowType"

instance ToJSON ChildWorkflowExecutionTerminatedEventAttributes where
    toJSON ChildWorkflowExecutionTerminatedEventAttributes{..} = object
        [ "workflowExecution" .= _cweteaWorkflowExecution
        , "workflowType"      .= _cweteaWorkflowType
        , "initiatedEventId"  .= _cweteaInitiatedEventId
        , "startedEventId"    .= _cweteaStartedEventId
        ]

data WorkflowExecutionCanceledEventAttributes = WorkflowExecutionCanceledEventAttributes
    { _wecea1DecisionTaskCompletedEventId :: Integer
    , _wecea1Details                      :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'WorkflowExecutionCanceledEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wecea1DecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'wecea1Details' @::@ 'Maybe' 'Text'
--
workflowExecutionCanceledEventAttributes :: Integer -- ^ 'wecea1DecisionTaskCompletedEventId'
                                         -> WorkflowExecutionCanceledEventAttributes
workflowExecutionCanceledEventAttributes p1 = WorkflowExecutionCanceledEventAttributes
    { _wecea1DecisionTaskCompletedEventId = p1
    , _wecea1Details                      = Nothing
    }

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'CancelWorkflowExecution' decision for this cancellation
-- request. This information can be useful for diagnosing problems by tracing
-- back the cause of events.
wecea1DecisionTaskCompletedEventId :: Lens' WorkflowExecutionCanceledEventAttributes Integer
wecea1DecisionTaskCompletedEventId =
    lens _wecea1DecisionTaskCompletedEventId
        (\s a -> s { _wecea1DecisionTaskCompletedEventId = a })

-- | Details for the cancellation (if any).
wecea1Details :: Lens' WorkflowExecutionCanceledEventAttributes (Maybe Text)
wecea1Details = lens _wecea1Details (\s a -> s { _wecea1Details = a })

instance FromJSON WorkflowExecutionCanceledEventAttributes where
    parseJSON = withObject "WorkflowExecutionCanceledEventAttributes" $ \o -> WorkflowExecutionCanceledEventAttributes
        <$> o .:  "decisionTaskCompletedEventId"
        <*> o .:? "details"

instance ToJSON WorkflowExecutionCanceledEventAttributes where
    toJSON WorkflowExecutionCanceledEventAttributes{..} = object
        [ "details"                      .= _wecea1Details
        , "decisionTaskCompletedEventId" .= _wecea1DecisionTaskCompletedEventId
        ]

data WorkflowExecutionSignaledEventAttributes = WorkflowExecutionSignaledEventAttributes
    { _wesea1ExternalInitiatedEventId  :: Maybe Integer
    , _wesea1ExternalWorkflowExecution :: Maybe WorkflowExecution
    , _wesea1Input                     :: Maybe Text
    , _wesea1SignalName                :: Text
    } deriving (Eq, Show)

-- | 'WorkflowExecutionSignaledEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wesea1ExternalInitiatedEventId' @::@ 'Maybe' 'Integer'
--
-- * 'wesea1ExternalWorkflowExecution' @::@ 'Maybe' 'WorkflowExecution'
--
-- * 'wesea1Input' @::@ 'Maybe' 'Text'
--
-- * 'wesea1SignalName' @::@ 'Text'
--
workflowExecutionSignaledEventAttributes :: Text -- ^ 'wesea1SignalName'
                                         -> WorkflowExecutionSignaledEventAttributes
workflowExecutionSignaledEventAttributes p1 = WorkflowExecutionSignaledEventAttributes
    { _wesea1SignalName                = p1
    , _wesea1Input                     = Nothing
    , _wesea1ExternalWorkflowExecution = Nothing
    , _wesea1ExternalInitiatedEventId  = Nothing
    }

-- | The id of the 'SignalExternalWorkflowExecutionInitiated' event corresponding
-- to the 'SignalExternalWorkflow' decision to signal this workflow execution.The
-- source event with this Id can be found in the history of the source workflow
-- execution. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event. This field is set only if
-- the signal was initiated by another workflow execution.
wesea1ExternalInitiatedEventId :: Lens' WorkflowExecutionSignaledEventAttributes (Maybe Integer)
wesea1ExternalInitiatedEventId =
    lens _wesea1ExternalInitiatedEventId
        (\s a -> s { _wesea1ExternalInitiatedEventId = a })

-- | The workflow execution that sent the signal. This is set only of the signal
-- was sent by another workflow execution.
wesea1ExternalWorkflowExecution :: Lens' WorkflowExecutionSignaledEventAttributes (Maybe WorkflowExecution)
wesea1ExternalWorkflowExecution =
    lens _wesea1ExternalWorkflowExecution
        (\s a -> s { _wesea1ExternalWorkflowExecution = a })

-- | Inputs provided with the signal (if any). The decider can use the signal
-- name and inputs to determine how to process the signal.
wesea1Input :: Lens' WorkflowExecutionSignaledEventAttributes (Maybe Text)
wesea1Input = lens _wesea1Input (\s a -> s { _wesea1Input = a })

-- | The name of the signal received. The decider can use the signal name and
-- inputs to determine how to the process the signal.
wesea1SignalName :: Lens' WorkflowExecutionSignaledEventAttributes Text
wesea1SignalName = lens _wesea1SignalName (\s a -> s { _wesea1SignalName = a })

instance FromJSON WorkflowExecutionSignaledEventAttributes where
    parseJSON = withObject "WorkflowExecutionSignaledEventAttributes" $ \o -> WorkflowExecutionSignaledEventAttributes
        <$> o .:? "externalInitiatedEventId"
        <*> o .:? "externalWorkflowExecution"
        <*> o .:? "input"
        <*> o .:  "signalName"

instance ToJSON WorkflowExecutionSignaledEventAttributes where
    toJSON WorkflowExecutionSignaledEventAttributes{..} = object
        [ "signalName"                .= _wesea1SignalName
        , "input"                     .= _wesea1Input
        , "externalWorkflowExecution" .= _wesea1ExternalWorkflowExecution
        , "externalInitiatedEventId"  .= _wesea1ExternalInitiatedEventId
        ]

data RecordMarkerFailedCause
    = RMFCOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable RecordMarkerFailedCause

instance FromText RecordMarkerFailedCause where
    parser = takeText >>= \case
        "OPERATION_NOT_PERMITTED" -> pure RMFCOperationNotPermitted
        e                         -> fail $
            "Failure parsing RecordMarkerFailedCause from " ++ show e

instance ToText RecordMarkerFailedCause where
    toText RMFCOperationNotPermitted = "OPERATION_NOT_PERMITTED"

instance ToByteString RecordMarkerFailedCause
instance ToHeader     RecordMarkerFailedCause
instance ToQuery      RecordMarkerFailedCause

instance FromJSON RecordMarkerFailedCause where
    parseJSON = parseJSONText "RecordMarkerFailedCause"

instance ToJSON RecordMarkerFailedCause where
    toJSON = toJSONText

data RegistrationStatus
    = Deprecated -- ^ DEPRECATED
    | Registered -- ^ REGISTERED
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable RegistrationStatus

instance FromText RegistrationStatus where
    parser = takeText >>= \case
        "DEPRECATED" -> pure Deprecated
        "REGISTERED" -> pure Registered
        e            -> fail $
            "Failure parsing RegistrationStatus from " ++ show e

instance ToText RegistrationStatus where
    toText = \case
        Deprecated -> "DEPRECATED"
        Registered -> "REGISTERED"

instance ToByteString RegistrationStatus
instance ToHeader     RegistrationStatus
instance ToQuery      RegistrationStatus

instance FromJSON RegistrationStatus where
    parseJSON = parseJSONText "RegistrationStatus"

instance ToJSON RegistrationStatus where
    toJSON = toJSONText

data TimerStartedEventAttributes = TimerStartedEventAttributes
    { _tseaControl                      :: Maybe Text
    , _tseaDecisionTaskCompletedEventId :: Integer
    , _tseaStartToFireTimeout           :: Text
    , _tseaTimerId                      :: Text
    } deriving (Eq, Ord, Show)

-- | 'TimerStartedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tseaControl' @::@ 'Maybe' 'Text'
--
-- * 'tseaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'tseaStartToFireTimeout' @::@ 'Text'
--
-- * 'tseaTimerId' @::@ 'Text'
--
timerStartedEventAttributes :: Text -- ^ 'tseaTimerId'
                            -> Text -- ^ 'tseaStartToFireTimeout'
                            -> Integer -- ^ 'tseaDecisionTaskCompletedEventId'
                            -> TimerStartedEventAttributes
timerStartedEventAttributes p1 p2 p3 = TimerStartedEventAttributes
    { _tseaTimerId                      = p1
    , _tseaStartToFireTimeout           = p2
    , _tseaDecisionTaskCompletedEventId = p3
    , _tseaControl                      = Nothing
    }

-- | Optional data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
tseaControl :: Lens' TimerStartedEventAttributes (Maybe Text)
tseaControl = lens _tseaControl (\s a -> s { _tseaControl = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'StartTimer' decision for this activity task. This
-- information can be useful for diagnosing problems by tracing back the cause
-- of events.
tseaDecisionTaskCompletedEventId :: Lens' TimerStartedEventAttributes Integer
tseaDecisionTaskCompletedEventId =
    lens _tseaDecisionTaskCompletedEventId
        (\s a -> s { _tseaDecisionTaskCompletedEventId = a })

-- | The duration of time after which the timer will fire.
--
-- The duration is specified in seconds. The valid values are integers greater
-- than or equal to 0.
tseaStartToFireTimeout :: Lens' TimerStartedEventAttributes Text
tseaStartToFireTimeout =
    lens _tseaStartToFireTimeout (\s a -> s { _tseaStartToFireTimeout = a })

-- | The unique Id of the timer that was started.
tseaTimerId :: Lens' TimerStartedEventAttributes Text
tseaTimerId = lens _tseaTimerId (\s a -> s { _tseaTimerId = a })

instance FromJSON TimerStartedEventAttributes where
    parseJSON = withObject "TimerStartedEventAttributes" $ \o -> TimerStartedEventAttributes
        <$> o .:? "control"
        <*> o .:  "decisionTaskCompletedEventId"
        <*> o .:  "startToFireTimeout"
        <*> o .:  "timerId"

instance ToJSON TimerStartedEventAttributes where
    toJSON TimerStartedEventAttributes{..} = object
        [ "timerId"                      .= _tseaTimerId
        , "control"                      .= _tseaControl
        , "startToFireTimeout"           .= _tseaStartToFireTimeout
        , "decisionTaskCompletedEventId" .= _tseaDecisionTaskCompletedEventId
        ]

newtype RequestCancelActivityTaskDecisionAttributes = RequestCancelActivityTaskDecisionAttributes
    { _rcatdaActivityId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'RequestCancelActivityTaskDecisionAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcatdaActivityId' @::@ 'Text'
--
requestCancelActivityTaskDecisionAttributes :: Text -- ^ 'rcatdaActivityId'
                                            -> RequestCancelActivityTaskDecisionAttributes
requestCancelActivityTaskDecisionAttributes p1 = RequestCancelActivityTaskDecisionAttributes
    { _rcatdaActivityId = p1
    }

-- | The 'activityId' of the activity task to be canceled.
rcatdaActivityId :: Lens' RequestCancelActivityTaskDecisionAttributes Text
rcatdaActivityId = lens _rcatdaActivityId (\s a -> s { _rcatdaActivityId = a })

instance FromJSON RequestCancelActivityTaskDecisionAttributes where
    parseJSON = withObject "RequestCancelActivityTaskDecisionAttributes" $ \o -> RequestCancelActivityTaskDecisionAttributes
        <$> o .:  "activityId"

instance ToJSON RequestCancelActivityTaskDecisionAttributes where
    toJSON RequestCancelActivityTaskDecisionAttributes{..} = object
        [ "activityId" .= _rcatdaActivityId
        ]

data Decision = Decision
    { _dCancelTimerDecisionAttributes                            :: Maybe CancelTimerDecisionAttributes
    , _dCancelWorkflowExecutionDecisionAttributes                :: Maybe CancelWorkflowExecutionDecisionAttributes
    , _dCompleteWorkflowExecutionDecisionAttributes              :: Maybe CompleteWorkflowExecutionDecisionAttributes
    , _dContinueAsNewWorkflowExecutionDecisionAttributes         :: Maybe ContinueAsNewWorkflowExecutionDecisionAttributes
    , _dDecisionType                                             :: DecisionType
    , _dFailWorkflowExecutionDecisionAttributes                  :: Maybe FailWorkflowExecutionDecisionAttributes
    , _dRecordMarkerDecisionAttributes                           :: Maybe RecordMarkerDecisionAttributes
    , _dRequestCancelActivityTaskDecisionAttributes              :: Maybe RequestCancelActivityTaskDecisionAttributes
    , _dRequestCancelExternalWorkflowExecutionDecisionAttributes :: Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes
    , _dScheduleActivityTaskDecisionAttributes                   :: Maybe ScheduleActivityTaskDecisionAttributes
    , _dSignalExternalWorkflowExecutionDecisionAttributes        :: Maybe SignalExternalWorkflowExecutionDecisionAttributes
    , _dStartChildWorkflowExecutionDecisionAttributes            :: Maybe StartChildWorkflowExecutionDecisionAttributes
    , _dStartTimerDecisionAttributes                             :: Maybe StartTimerDecisionAttributes
    } deriving (Eq, Show)

-- | 'Decision' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dCancelTimerDecisionAttributes' @::@ 'Maybe' 'CancelTimerDecisionAttributes'
--
-- * 'dCancelWorkflowExecutionDecisionAttributes' @::@ 'Maybe' 'CancelWorkflowExecutionDecisionAttributes'
--
-- * 'dCompleteWorkflowExecutionDecisionAttributes' @::@ 'Maybe' 'CompleteWorkflowExecutionDecisionAttributes'
--
-- * 'dContinueAsNewWorkflowExecutionDecisionAttributes' @::@ 'Maybe' 'ContinueAsNewWorkflowExecutionDecisionAttributes'
--
-- * 'dDecisionType' @::@ 'DecisionType'
--
-- * 'dFailWorkflowExecutionDecisionAttributes' @::@ 'Maybe' 'FailWorkflowExecutionDecisionAttributes'
--
-- * 'dRecordMarkerDecisionAttributes' @::@ 'Maybe' 'RecordMarkerDecisionAttributes'
--
-- * 'dRequestCancelActivityTaskDecisionAttributes' @::@ 'Maybe' 'RequestCancelActivityTaskDecisionAttributes'
--
-- * 'dRequestCancelExternalWorkflowExecutionDecisionAttributes' @::@ 'Maybe' 'RequestCancelExternalWorkflowExecutionDecisionAttributes'
--
-- * 'dScheduleActivityTaskDecisionAttributes' @::@ 'Maybe' 'ScheduleActivityTaskDecisionAttributes'
--
-- * 'dSignalExternalWorkflowExecutionDecisionAttributes' @::@ 'Maybe' 'SignalExternalWorkflowExecutionDecisionAttributes'
--
-- * 'dStartChildWorkflowExecutionDecisionAttributes' @::@ 'Maybe' 'StartChildWorkflowExecutionDecisionAttributes'
--
-- * 'dStartTimerDecisionAttributes' @::@ 'Maybe' 'StartTimerDecisionAttributes'
--
decision :: DecisionType -- ^ 'dDecisionType'
         -> Decision
decision p1 = Decision
    { _dDecisionType                                             = p1
    , _dScheduleActivityTaskDecisionAttributes                   = Nothing
    , _dRequestCancelActivityTaskDecisionAttributes              = Nothing
    , _dCompleteWorkflowExecutionDecisionAttributes              = Nothing
    , _dFailWorkflowExecutionDecisionAttributes                  = Nothing
    , _dCancelWorkflowExecutionDecisionAttributes                = Nothing
    , _dContinueAsNewWorkflowExecutionDecisionAttributes         = Nothing
    , _dRecordMarkerDecisionAttributes                           = Nothing
    , _dStartTimerDecisionAttributes                             = Nothing
    , _dCancelTimerDecisionAttributes                            = Nothing
    , _dSignalExternalWorkflowExecutionDecisionAttributes        = Nothing
    , _dRequestCancelExternalWorkflowExecutionDecisionAttributes = Nothing
    , _dStartChildWorkflowExecutionDecisionAttributes            = Nothing
    }

-- | Provides details of the 'CancelTimer' decision. It is not set for other
-- decision types.
dCancelTimerDecisionAttributes :: Lens' Decision (Maybe CancelTimerDecisionAttributes)
dCancelTimerDecisionAttributes =
    lens _dCancelTimerDecisionAttributes
        (\s a -> s { _dCancelTimerDecisionAttributes = a })

-- | Provides details of the 'CancelWorkflowExecution' decision. It is not set for
-- other decision types.
dCancelWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe CancelWorkflowExecutionDecisionAttributes)
dCancelWorkflowExecutionDecisionAttributes =
    lens _dCancelWorkflowExecutionDecisionAttributes
        (\s a -> s { _dCancelWorkflowExecutionDecisionAttributes = a })

-- | Provides details of the 'CompleteWorkflowExecution' decision. It is not set
-- for other decision types.
dCompleteWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe CompleteWorkflowExecutionDecisionAttributes)
dCompleteWorkflowExecutionDecisionAttributes =
    lens _dCompleteWorkflowExecutionDecisionAttributes
        (\s a -> s { _dCompleteWorkflowExecutionDecisionAttributes = a })

-- | Provides details of the 'ContinueAsNewWorkflowExecution' decision. It is not
-- set for other decision types.
dContinueAsNewWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe ContinueAsNewWorkflowExecutionDecisionAttributes)
dContinueAsNewWorkflowExecutionDecisionAttributes =
    lens _dContinueAsNewWorkflowExecutionDecisionAttributes
        (\s a -> s { _dContinueAsNewWorkflowExecutionDecisionAttributes = a })

-- | Specifies the type of the decision.
dDecisionType :: Lens' Decision DecisionType
dDecisionType = lens _dDecisionType (\s a -> s { _dDecisionType = a })

-- | Provides details of the 'FailWorkflowExecution' decision. It is not set for
-- other decision types.
dFailWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe FailWorkflowExecutionDecisionAttributes)
dFailWorkflowExecutionDecisionAttributes =
    lens _dFailWorkflowExecutionDecisionAttributes
        (\s a -> s { _dFailWorkflowExecutionDecisionAttributes = a })

-- | Provides details of the 'RecordMarker' decision. It is not set for other
-- decision types.
dRecordMarkerDecisionAttributes :: Lens' Decision (Maybe RecordMarkerDecisionAttributes)
dRecordMarkerDecisionAttributes =
    lens _dRecordMarkerDecisionAttributes
        (\s a -> s { _dRecordMarkerDecisionAttributes = a })

-- | Provides details of the 'RequestCancelActivityTask' decision. It is not set
-- for other decision types.
dRequestCancelActivityTaskDecisionAttributes :: Lens' Decision (Maybe RequestCancelActivityTaskDecisionAttributes)
dRequestCancelActivityTaskDecisionAttributes =
    lens _dRequestCancelActivityTaskDecisionAttributes
        (\s a -> s { _dRequestCancelActivityTaskDecisionAttributes = a })

-- | Provides details of the 'RequestCancelExternalWorkflowExecution' decision. It
-- is not set for other decision types.
dRequestCancelExternalWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe RequestCancelExternalWorkflowExecutionDecisionAttributes)
dRequestCancelExternalWorkflowExecutionDecisionAttributes =
    lens _dRequestCancelExternalWorkflowExecutionDecisionAttributes
        (\s a -> s { _dRequestCancelExternalWorkflowExecutionDecisionAttributes = a })

-- | Provides details of the 'ScheduleActivityTask' decision. It is not set for
-- other decision types.
dScheduleActivityTaskDecisionAttributes :: Lens' Decision (Maybe ScheduleActivityTaskDecisionAttributes)
dScheduleActivityTaskDecisionAttributes =
    lens _dScheduleActivityTaskDecisionAttributes
        (\s a -> s { _dScheduleActivityTaskDecisionAttributes = a })

-- | Provides details of the 'SignalExternalWorkflowExecution' decision. It is not
-- set for other decision types.
dSignalExternalWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe SignalExternalWorkflowExecutionDecisionAttributes)
dSignalExternalWorkflowExecutionDecisionAttributes =
    lens _dSignalExternalWorkflowExecutionDecisionAttributes
        (\s a -> s { _dSignalExternalWorkflowExecutionDecisionAttributes = a })

-- | Provides details of the 'StartChildWorkflowExecution' decision. It is not set
-- for other decision types.
dStartChildWorkflowExecutionDecisionAttributes :: Lens' Decision (Maybe StartChildWorkflowExecutionDecisionAttributes)
dStartChildWorkflowExecutionDecisionAttributes =
    lens _dStartChildWorkflowExecutionDecisionAttributes
        (\s a -> s { _dStartChildWorkflowExecutionDecisionAttributes = a })

-- | Provides details of the 'StartTimer' decision. It is not set for other
-- decision types.
dStartTimerDecisionAttributes :: Lens' Decision (Maybe StartTimerDecisionAttributes)
dStartTimerDecisionAttributes =
    lens _dStartTimerDecisionAttributes
        (\s a -> s { _dStartTimerDecisionAttributes = a })

instance FromJSON Decision where
    parseJSON = withObject "Decision" $ \o -> Decision
        <$> o .:? "cancelTimerDecisionAttributes"
        <*> o .:? "cancelWorkflowExecutionDecisionAttributes"
        <*> o .:? "completeWorkflowExecutionDecisionAttributes"
        <*> o .:? "continueAsNewWorkflowExecutionDecisionAttributes"
        <*> o .:  "decisionType"
        <*> o .:? "failWorkflowExecutionDecisionAttributes"
        <*> o .:? "recordMarkerDecisionAttributes"
        <*> o .:? "requestCancelActivityTaskDecisionAttributes"
        <*> o .:? "requestCancelExternalWorkflowExecutionDecisionAttributes"
        <*> o .:? "scheduleActivityTaskDecisionAttributes"
        <*> o .:? "signalExternalWorkflowExecutionDecisionAttributes"
        <*> o .:? "startChildWorkflowExecutionDecisionAttributes"
        <*> o .:? "startTimerDecisionAttributes"

instance ToJSON Decision where
    toJSON Decision{..} = object
        [ "decisionType"                                             .= _dDecisionType
        , "scheduleActivityTaskDecisionAttributes"                   .= _dScheduleActivityTaskDecisionAttributes
        , "requestCancelActivityTaskDecisionAttributes"              .= _dRequestCancelActivityTaskDecisionAttributes
        , "completeWorkflowExecutionDecisionAttributes"              .= _dCompleteWorkflowExecutionDecisionAttributes
        , "failWorkflowExecutionDecisionAttributes"                  .= _dFailWorkflowExecutionDecisionAttributes
        , "cancelWorkflowExecutionDecisionAttributes"                .= _dCancelWorkflowExecutionDecisionAttributes
        , "continueAsNewWorkflowExecutionDecisionAttributes"         .= _dContinueAsNewWorkflowExecutionDecisionAttributes
        , "recordMarkerDecisionAttributes"                           .= _dRecordMarkerDecisionAttributes
        , "startTimerDecisionAttributes"                             .= _dStartTimerDecisionAttributes
        , "cancelTimerDecisionAttributes"                            .= _dCancelTimerDecisionAttributes
        , "signalExternalWorkflowExecutionDecisionAttributes"        .= _dSignalExternalWorkflowExecutionDecisionAttributes
        , "requestCancelExternalWorkflowExecutionDecisionAttributes" .= _dRequestCancelExternalWorkflowExecutionDecisionAttributes
        , "startChildWorkflowExecutionDecisionAttributes"            .= _dStartChildWorkflowExecutionDecisionAttributes
        ]

data TimerFiredEventAttributes = TimerFiredEventAttributes
    { _tfeaStartedEventId :: Integer
    , _tfeaTimerId        :: Text
    } deriving (Eq, Ord, Show)

-- | 'TimerFiredEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tfeaStartedEventId' @::@ 'Integer'
--
-- * 'tfeaTimerId' @::@ 'Text'
--
timerFiredEventAttributes :: Text -- ^ 'tfeaTimerId'
                          -> Integer -- ^ 'tfeaStartedEventId'
                          -> TimerFiredEventAttributes
timerFiredEventAttributes p1 p2 = TimerFiredEventAttributes
    { _tfeaTimerId        = p1
    , _tfeaStartedEventId = p2
    }

-- | The id of the 'TimerStarted' event that was recorded when this timer was
-- started. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
tfeaStartedEventId :: Lens' TimerFiredEventAttributes Integer
tfeaStartedEventId =
    lens _tfeaStartedEventId (\s a -> s { _tfeaStartedEventId = a })

-- | The unique Id of the timer that fired.
tfeaTimerId :: Lens' TimerFiredEventAttributes Text
tfeaTimerId = lens _tfeaTimerId (\s a -> s { _tfeaTimerId = a })

instance FromJSON TimerFiredEventAttributes where
    parseJSON = withObject "TimerFiredEventAttributes" $ \o -> TimerFiredEventAttributes
        <$> o .:  "startedEventId"
        <*> o .:  "timerId"

instance ToJSON TimerFiredEventAttributes where
    toJSON TimerFiredEventAttributes{..} = object
        [ "timerId"        .= _tfeaTimerId
        , "startedEventId" .= _tfeaStartedEventId
        ]

newtype DomainConfiguration = DomainConfiguration
    { _dcWorkflowExecutionRetentionPeriodInDays :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DomainConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcWorkflowExecutionRetentionPeriodInDays' @::@ 'Text'
--
domainConfiguration :: Text -- ^ 'dcWorkflowExecutionRetentionPeriodInDays'
                    -> DomainConfiguration
domainConfiguration p1 = DomainConfiguration
    { _dcWorkflowExecutionRetentionPeriodInDays = p1
    }

-- | The retention period for workflow executions in this domain.
dcWorkflowExecutionRetentionPeriodInDays :: Lens' DomainConfiguration Text
dcWorkflowExecutionRetentionPeriodInDays =
    lens _dcWorkflowExecutionRetentionPeriodInDays
        (\s a -> s { _dcWorkflowExecutionRetentionPeriodInDays = a })

instance FromJSON DomainConfiguration where
    parseJSON = withObject "DomainConfiguration" $ \o -> DomainConfiguration
        <$> o .:  "workflowExecutionRetentionPeriodInDays"

instance ToJSON DomainConfiguration where
    toJSON DomainConfiguration{..} = object
        [ "workflowExecutionRetentionPeriodInDays" .= _dcWorkflowExecutionRetentionPeriodInDays
        ]

data ExternalWorkflowExecutionSignaledEventAttributes = ExternalWorkflowExecutionSignaledEventAttributes
    { _eweseaInitiatedEventId  :: Integer
    , _eweseaWorkflowExecution :: WorkflowExecution
    } deriving (Eq, Show)

-- | 'ExternalWorkflowExecutionSignaledEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eweseaInitiatedEventId' @::@ 'Integer'
--
-- * 'eweseaWorkflowExecution' @::@ 'WorkflowExecution'
--
externalWorkflowExecutionSignaledEventAttributes :: WorkflowExecution -- ^ 'eweseaWorkflowExecution'
                                                 -> Integer -- ^ 'eweseaInitiatedEventId'
                                                 -> ExternalWorkflowExecutionSignaledEventAttributes
externalWorkflowExecutionSignaledEventAttributes p1 p2 = ExternalWorkflowExecutionSignaledEventAttributes
    { _eweseaWorkflowExecution = p1
    , _eweseaInitiatedEventId  = p2
    }

-- | The id of the 'SignalExternalWorkflowExecutionInitiated' event corresponding
-- to the 'SignalExternalWorkflowExecution' decision to request this signal. This
-- information can be useful for diagnosing problems by tracing back the chain
-- of events leading up to this event.
eweseaInitiatedEventId :: Lens' ExternalWorkflowExecutionSignaledEventAttributes Integer
eweseaInitiatedEventId =
    lens _eweseaInitiatedEventId (\s a -> s { _eweseaInitiatedEventId = a })

-- | The external workflow execution that the signal was delivered to.
eweseaWorkflowExecution :: Lens' ExternalWorkflowExecutionSignaledEventAttributes WorkflowExecution
eweseaWorkflowExecution =
    lens _eweseaWorkflowExecution (\s a -> s { _eweseaWorkflowExecution = a })

instance FromJSON ExternalWorkflowExecutionSignaledEventAttributes where
    parseJSON = withObject "ExternalWorkflowExecutionSignaledEventAttributes" $ \o -> ExternalWorkflowExecutionSignaledEventAttributes
        <$> o .:  "initiatedEventId"
        <*> o .:  "workflowExecution"

instance ToJSON ExternalWorkflowExecutionSignaledEventAttributes where
    toJSON ExternalWorkflowExecutionSignaledEventAttributes{..} = object
        [ "workflowExecution" .= _eweseaWorkflowExecution
        , "initiatedEventId"  .= _eweseaInitiatedEventId
        ]

newtype CancelWorkflowExecutionDecisionAttributes = CancelWorkflowExecutionDecisionAttributes
    { _cwedaDetails :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'CancelWorkflowExecutionDecisionAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwedaDetails' @::@ 'Maybe' 'Text'
--
cancelWorkflowExecutionDecisionAttributes :: CancelWorkflowExecutionDecisionAttributes
cancelWorkflowExecutionDecisionAttributes = CancelWorkflowExecutionDecisionAttributes
    { _cwedaDetails = Nothing
    }

-- | Optional details of the cancellation.
cwedaDetails :: Lens' CancelWorkflowExecutionDecisionAttributes (Maybe Text)
cwedaDetails = lens _cwedaDetails (\s a -> s { _cwedaDetails = a })

instance FromJSON CancelWorkflowExecutionDecisionAttributes where
    parseJSON = withObject "CancelWorkflowExecutionDecisionAttributes" $ \o -> CancelWorkflowExecutionDecisionAttributes
        <$> o .:? "details"

instance ToJSON CancelWorkflowExecutionDecisionAttributes where
    toJSON CancelWorkflowExecutionDecisionAttributes{..} = object
        [ "details" .= _cwedaDetails
        ]

data ActivityTaskFailedEventAttributes = ActivityTaskFailedEventAttributes
    { _atfeaDetails          :: Maybe Text
    , _atfeaReason           :: Maybe Text
    , _atfeaScheduledEventId :: Integer
    , _atfeaStartedEventId   :: Integer
    } deriving (Eq, Ord, Show)

-- | 'ActivityTaskFailedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atfeaDetails' @::@ 'Maybe' 'Text'
--
-- * 'atfeaReason' @::@ 'Maybe' 'Text'
--
-- * 'atfeaScheduledEventId' @::@ 'Integer'
--
-- * 'atfeaStartedEventId' @::@ 'Integer'
--
activityTaskFailedEventAttributes :: Integer -- ^ 'atfeaScheduledEventId'
                                  -> Integer -- ^ 'atfeaStartedEventId'
                                  -> ActivityTaskFailedEventAttributes
activityTaskFailedEventAttributes p1 p2 = ActivityTaskFailedEventAttributes
    { _atfeaScheduledEventId = p1
    , _atfeaStartedEventId   = p2
    , _atfeaReason           = Nothing
    , _atfeaDetails          = Nothing
    }

-- | The details of the failure (if any).
atfeaDetails :: Lens' ActivityTaskFailedEventAttributes (Maybe Text)
atfeaDetails = lens _atfeaDetails (\s a -> s { _atfeaDetails = a })

-- | The reason provided for the failure (if any).
atfeaReason :: Lens' ActivityTaskFailedEventAttributes (Maybe Text)
atfeaReason = lens _atfeaReason (\s a -> s { _atfeaReason = a })

-- | The id of the 'ActivityTaskScheduled' event that was recorded when this
-- activity task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
atfeaScheduledEventId :: Lens' ActivityTaskFailedEventAttributes Integer
atfeaScheduledEventId =
    lens _atfeaScheduledEventId (\s a -> s { _atfeaScheduledEventId = a })

-- | The Id of the 'ActivityTaskStarted' event recorded when this activity task was
-- started. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
atfeaStartedEventId :: Lens' ActivityTaskFailedEventAttributes Integer
atfeaStartedEventId =
    lens _atfeaStartedEventId (\s a -> s { _atfeaStartedEventId = a })

instance FromJSON ActivityTaskFailedEventAttributes where
    parseJSON = withObject "ActivityTaskFailedEventAttributes" $ \o -> ActivityTaskFailedEventAttributes
        <$> o .:? "details"
        <*> o .:? "reason"
        <*> o .:  "scheduledEventId"
        <*> o .:  "startedEventId"

instance ToJSON ActivityTaskFailedEventAttributes where
    toJSON ActivityTaskFailedEventAttributes{..} = object
        [ "reason"           .= _atfeaReason
        , "details"          .= _atfeaDetails
        , "scheduledEventId" .= _atfeaScheduledEventId
        , "startedEventId"   .= _atfeaStartedEventId
        ]

data FailWorkflowExecutionFailedEventAttributes = FailWorkflowExecutionFailedEventAttributes
    { _fwefeaCause                        :: FailWorkflowExecutionFailedCause
    , _fwefeaDecisionTaskCompletedEventId :: Integer
    } deriving (Eq, Show)

-- | 'FailWorkflowExecutionFailedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fwefeaCause' @::@ 'FailWorkflowExecutionFailedCause'
--
-- * 'fwefeaDecisionTaskCompletedEventId' @::@ 'Integer'
--
failWorkflowExecutionFailedEventAttributes :: FailWorkflowExecutionFailedCause -- ^ 'fwefeaCause'
                                           -> Integer -- ^ 'fwefeaDecisionTaskCompletedEventId'
                                           -> FailWorkflowExecutionFailedEventAttributes
failWorkflowExecutionFailedEventAttributes p1 p2 = FailWorkflowExecutionFailedEventAttributes
    { _fwefeaCause                        = p1
    , _fwefeaDecisionTaskCompletedEventId = p2
    }

-- | The cause of the failure. This information is generated by the system and can
-- be useful for diagnostic purposes.
fwefeaCause :: Lens' FailWorkflowExecutionFailedEventAttributes FailWorkflowExecutionFailedCause
fwefeaCause = lens _fwefeaCause (\s a -> s { _fwefeaCause = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'FailWorkflowExecution' decision to fail this execution.
-- This information can be useful for diagnosing problems by tracing back the
-- cause of events.
fwefeaDecisionTaskCompletedEventId :: Lens' FailWorkflowExecutionFailedEventAttributes Integer
fwefeaDecisionTaskCompletedEventId =
    lens _fwefeaDecisionTaskCompletedEventId
        (\s a -> s { _fwefeaDecisionTaskCompletedEventId = a })

instance FromJSON FailWorkflowExecutionFailedEventAttributes where
    parseJSON = withObject "FailWorkflowExecutionFailedEventAttributes" $ \o -> FailWorkflowExecutionFailedEventAttributes
        <$> o .:  "cause"
        <*> o .:  "decisionTaskCompletedEventId"

instance ToJSON FailWorkflowExecutionFailedEventAttributes where
    toJSON FailWorkflowExecutionFailedEventAttributes{..} = object
        [ "cause"                        .= _fwefeaCause
        , "decisionTaskCompletedEventId" .= _fwefeaDecisionTaskCompletedEventId
        ]

data StartChildWorkflowExecutionFailedEventAttributes = StartChildWorkflowExecutionFailedEventAttributes
    { _scwefeaCause                        :: StartChildWorkflowExecutionFailedCause
    , _scwefeaControl                      :: Maybe Text
    , _scwefeaDecisionTaskCompletedEventId :: Integer
    , _scwefeaInitiatedEventId             :: Integer
    , _scwefeaWorkflowId                   :: Text
    , _scwefeaWorkflowType                 :: WorkflowType
    } deriving (Eq, Show)

-- | 'StartChildWorkflowExecutionFailedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scwefeaCause' @::@ 'StartChildWorkflowExecutionFailedCause'
--
-- * 'scwefeaControl' @::@ 'Maybe' 'Text'
--
-- * 'scwefeaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'scwefeaInitiatedEventId' @::@ 'Integer'
--
-- * 'scwefeaWorkflowId' @::@ 'Text'
--
-- * 'scwefeaWorkflowType' @::@ 'WorkflowType'
--
startChildWorkflowExecutionFailedEventAttributes :: WorkflowType -- ^ 'scwefeaWorkflowType'
                                                 -> StartChildWorkflowExecutionFailedCause -- ^ 'scwefeaCause'
                                                 -> Text -- ^ 'scwefeaWorkflowId'
                                                 -> Integer -- ^ 'scwefeaInitiatedEventId'
                                                 -> Integer -- ^ 'scwefeaDecisionTaskCompletedEventId'
                                                 -> StartChildWorkflowExecutionFailedEventAttributes
startChildWorkflowExecutionFailedEventAttributes p1 p2 p3 p4 p5 = StartChildWorkflowExecutionFailedEventAttributes
    { _scwefeaWorkflowType                 = p1
    , _scwefeaCause                        = p2
    , _scwefeaWorkflowId                   = p3
    , _scwefeaInitiatedEventId             = p4
    , _scwefeaDecisionTaskCompletedEventId = p5
    , _scwefeaControl                      = Nothing
    }

-- | The cause of the failure to process the decision. This information is
-- generated by the system and can be useful for diagnostic purposes.
scwefeaCause :: Lens' StartChildWorkflowExecutionFailedEventAttributes StartChildWorkflowExecutionFailedCause
scwefeaCause = lens _scwefeaCause (\s a -> s { _scwefeaCause = a })

scwefeaControl :: Lens' StartChildWorkflowExecutionFailedEventAttributes (Maybe Text)
scwefeaControl = lens _scwefeaControl (\s a -> s { _scwefeaControl = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'StartChildWorkflowExecution' 'Decision' to request this
-- child workflow execution. This information can be useful for diagnosing
-- problems by tracing back the cause of events.
scwefeaDecisionTaskCompletedEventId :: Lens' StartChildWorkflowExecutionFailedEventAttributes Integer
scwefeaDecisionTaskCompletedEventId =
    lens _scwefeaDecisionTaskCompletedEventId
        (\s a -> s { _scwefeaDecisionTaskCompletedEventId = a })

-- | The id of the 'StartChildWorkflowExecutionInitiated' event corresponding to
-- the 'StartChildWorkflowExecution' 'Decision' to start this child workflow
-- execution. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
scwefeaInitiatedEventId :: Lens' StartChildWorkflowExecutionFailedEventAttributes Integer
scwefeaInitiatedEventId =
    lens _scwefeaInitiatedEventId (\s a -> s { _scwefeaInitiatedEventId = a })

-- | The 'workflowId' of the child workflow execution.
scwefeaWorkflowId :: Lens' StartChildWorkflowExecutionFailedEventAttributes Text
scwefeaWorkflowId =
    lens _scwefeaWorkflowId (\s a -> s { _scwefeaWorkflowId = a })

-- | The workflow type provided in the 'StartChildWorkflowExecution' 'Decision' that
-- failed.
scwefeaWorkflowType :: Lens' StartChildWorkflowExecutionFailedEventAttributes WorkflowType
scwefeaWorkflowType =
    lens _scwefeaWorkflowType (\s a -> s { _scwefeaWorkflowType = a })

instance FromJSON StartChildWorkflowExecutionFailedEventAttributes where
    parseJSON = withObject "StartChildWorkflowExecutionFailedEventAttributes" $ \o -> StartChildWorkflowExecutionFailedEventAttributes
        <$> o .:  "cause"
        <*> o .:? "control"
        <*> o .:  "decisionTaskCompletedEventId"
        <*> o .:  "initiatedEventId"
        <*> o .:  "workflowId"
        <*> o .:  "workflowType"

instance ToJSON StartChildWorkflowExecutionFailedEventAttributes where
    toJSON StartChildWorkflowExecutionFailedEventAttributes{..} = object
        [ "workflowType"                 .= _scwefeaWorkflowType
        , "cause"                        .= _scwefeaCause
        , "workflowId"                   .= _scwefeaWorkflowId
        , "initiatedEventId"             .= _scwefeaInitiatedEventId
        , "decisionTaskCompletedEventId" .= _scwefeaDecisionTaskCompletedEventId
        , "control"                      .= _scwefeaControl
        ]

data WorkflowTypeFilter = WorkflowTypeFilter
    { _wtfName    :: Text
    , _wtfVersion :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'WorkflowTypeFilter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wtfName' @::@ 'Text'
--
-- * 'wtfVersion' @::@ 'Maybe' 'Text'
--
workflowTypeFilter :: Text -- ^ 'wtfName'
                   -> WorkflowTypeFilter
workflowTypeFilter p1 = WorkflowTypeFilter
    { _wtfName    = p1
    , _wtfVersion = Nothing
    }

-- | Name of the workflow type. This field is required.
wtfName :: Lens' WorkflowTypeFilter Text
wtfName = lens _wtfName (\s a -> s { _wtfName = a })

-- | Version of the workflow type.
wtfVersion :: Lens' WorkflowTypeFilter (Maybe Text)
wtfVersion = lens _wtfVersion (\s a -> s { _wtfVersion = a })

instance FromJSON WorkflowTypeFilter where
    parseJSON = withObject "WorkflowTypeFilter" $ \o -> WorkflowTypeFilter
        <$> o .:  "name"
        <*> o .:? "version"

instance ToJSON WorkflowTypeFilter where
    toJSON WorkflowTypeFilter{..} = object
        [ "name"    .= _wtfName
        , "version" .= _wtfVersion
        ]

data CancelTimerFailedCause
    = CTFCOperationNotPermitted -- ^ OPERATION_NOT_PERMITTED
    | CTFCTimerIdUnknown        -- ^ TIMER_ID_UNKNOWN
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable CancelTimerFailedCause

instance FromText CancelTimerFailedCause where
    parser = takeText >>= \case
        "OPERATION_NOT_PERMITTED" -> pure CTFCOperationNotPermitted
        "TIMER_ID_UNKNOWN"        -> pure CTFCTimerIdUnknown
        e                         -> fail $
            "Failure parsing CancelTimerFailedCause from " ++ show e

instance ToText CancelTimerFailedCause where
    toText = \case
        CTFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"
        CTFCTimerIdUnknown        -> "TIMER_ID_UNKNOWN"

instance ToByteString CancelTimerFailedCause
instance ToHeader     CancelTimerFailedCause
instance ToQuery      CancelTimerFailedCause

instance FromJSON CancelTimerFailedCause where
    parseJSON = parseJSONText "CancelTimerFailedCause"

instance ToJSON CancelTimerFailedCause where
    toJSON = toJSONText

data DecisionTaskCompletedEventAttributes = DecisionTaskCompletedEventAttributes
    { _dtceaExecutionContext :: Maybe Text
    , _dtceaScheduledEventId :: Integer
    , _dtceaStartedEventId   :: Integer
    } deriving (Eq, Ord, Show)

-- | 'DecisionTaskCompletedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtceaExecutionContext' @::@ 'Maybe' 'Text'
--
-- * 'dtceaScheduledEventId' @::@ 'Integer'
--
-- * 'dtceaStartedEventId' @::@ 'Integer'
--
decisionTaskCompletedEventAttributes :: Integer -- ^ 'dtceaScheduledEventId'
                                     -> Integer -- ^ 'dtceaStartedEventId'
                                     -> DecisionTaskCompletedEventAttributes
decisionTaskCompletedEventAttributes p1 p2 = DecisionTaskCompletedEventAttributes
    { _dtceaScheduledEventId = p1
    , _dtceaStartedEventId   = p2
    , _dtceaExecutionContext = Nothing
    }

-- | User defined context for the workflow execution.
dtceaExecutionContext :: Lens' DecisionTaskCompletedEventAttributes (Maybe Text)
dtceaExecutionContext =
    lens _dtceaExecutionContext (\s a -> s { _dtceaExecutionContext = a })

-- | The id of the 'DecisionTaskScheduled' event that was recorded when this
-- decision task was scheduled. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
dtceaScheduledEventId :: Lens' DecisionTaskCompletedEventAttributes Integer
dtceaScheduledEventId =
    lens _dtceaScheduledEventId (\s a -> s { _dtceaScheduledEventId = a })

-- | The Id of the 'DecisionTaskStarted' event recorded when this decision task was
-- started. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
dtceaStartedEventId :: Lens' DecisionTaskCompletedEventAttributes Integer
dtceaStartedEventId =
    lens _dtceaStartedEventId (\s a -> s { _dtceaStartedEventId = a })

instance FromJSON DecisionTaskCompletedEventAttributes where
    parseJSON = withObject "DecisionTaskCompletedEventAttributes" $ \o -> DecisionTaskCompletedEventAttributes
        <$> o .:? "executionContext"
        <*> o .:  "scheduledEventId"
        <*> o .:  "startedEventId"

instance ToJSON DecisionTaskCompletedEventAttributes where
    toJSON DecisionTaskCompletedEventAttributes{..} = object
        [ "executionContext" .= _dtceaExecutionContext
        , "scheduledEventId" .= _dtceaScheduledEventId
        , "startedEventId"   .= _dtceaStartedEventId
        ]

data ChildWorkflowExecutionFailedEventAttributes = ChildWorkflowExecutionFailedEventAttributes
    { _cwefeaDetails           :: Maybe Text
    , _cwefeaInitiatedEventId  :: Integer
    , _cwefeaReason            :: Maybe Text
    , _cwefeaStartedEventId    :: Integer
    , _cwefeaWorkflowExecution :: WorkflowExecution
    , _cwefeaWorkflowType      :: WorkflowType
    } deriving (Eq, Show)

-- | 'ChildWorkflowExecutionFailedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cwefeaDetails' @::@ 'Maybe' 'Text'
--
-- * 'cwefeaInitiatedEventId' @::@ 'Integer'
--
-- * 'cwefeaReason' @::@ 'Maybe' 'Text'
--
-- * 'cwefeaStartedEventId' @::@ 'Integer'
--
-- * 'cwefeaWorkflowExecution' @::@ 'WorkflowExecution'
--
-- * 'cwefeaWorkflowType' @::@ 'WorkflowType'
--
childWorkflowExecutionFailedEventAttributes :: WorkflowExecution -- ^ 'cwefeaWorkflowExecution'
                                            -> WorkflowType -- ^ 'cwefeaWorkflowType'
                                            -> Integer -- ^ 'cwefeaInitiatedEventId'
                                            -> Integer -- ^ 'cwefeaStartedEventId'
                                            -> ChildWorkflowExecutionFailedEventAttributes
childWorkflowExecutionFailedEventAttributes p1 p2 p3 p4 = ChildWorkflowExecutionFailedEventAttributes
    { _cwefeaWorkflowExecution = p1
    , _cwefeaWorkflowType      = p2
    , _cwefeaInitiatedEventId  = p3
    , _cwefeaStartedEventId    = p4
    , _cwefeaReason            = Nothing
    , _cwefeaDetails           = Nothing
    }

-- | The details of the failure (if provided).
cwefeaDetails :: Lens' ChildWorkflowExecutionFailedEventAttributes (Maybe Text)
cwefeaDetails = lens _cwefeaDetails (\s a -> s { _cwefeaDetails = a })

-- | The id of the 'StartChildWorkflowExecutionInitiated' event corresponding to
-- the 'StartChildWorkflowExecution' 'Decision' to start this child workflow
-- execution. This information can be useful for diagnosing problems by tracing
-- back the chain of events leading up to this event.
cwefeaInitiatedEventId :: Lens' ChildWorkflowExecutionFailedEventAttributes Integer
cwefeaInitiatedEventId =
    lens _cwefeaInitiatedEventId (\s a -> s { _cwefeaInitiatedEventId = a })

-- | The reason for the failure (if provided).
cwefeaReason :: Lens' ChildWorkflowExecutionFailedEventAttributes (Maybe Text)
cwefeaReason = lens _cwefeaReason (\s a -> s { _cwefeaReason = a })

-- | The Id of the 'ChildWorkflowExecutionStarted' event recorded when this child
-- workflow execution was started. This information can be useful for diagnosing
-- problems by tracing back the chain of events leading up to this event.
cwefeaStartedEventId :: Lens' ChildWorkflowExecutionFailedEventAttributes Integer
cwefeaStartedEventId =
    lens _cwefeaStartedEventId (\s a -> s { _cwefeaStartedEventId = a })

-- | The child workflow execution that failed.
cwefeaWorkflowExecution :: Lens' ChildWorkflowExecutionFailedEventAttributes WorkflowExecution
cwefeaWorkflowExecution =
    lens _cwefeaWorkflowExecution (\s a -> s { _cwefeaWorkflowExecution = a })

-- | The type of the child workflow execution.
cwefeaWorkflowType :: Lens' ChildWorkflowExecutionFailedEventAttributes WorkflowType
cwefeaWorkflowType =
    lens _cwefeaWorkflowType (\s a -> s { _cwefeaWorkflowType = a })

instance FromJSON ChildWorkflowExecutionFailedEventAttributes where
    parseJSON = withObject "ChildWorkflowExecutionFailedEventAttributes" $ \o -> ChildWorkflowExecutionFailedEventAttributes
        <$> o .:? "details"
        <*> o .:  "initiatedEventId"
        <*> o .:? "reason"
        <*> o .:  "startedEventId"
        <*> o .:  "workflowExecution"
        <*> o .:  "workflowType"

instance ToJSON ChildWorkflowExecutionFailedEventAttributes where
    toJSON ChildWorkflowExecutionFailedEventAttributes{..} = object
        [ "workflowExecution" .= _cwefeaWorkflowExecution
        , "workflowType"      .= _cwefeaWorkflowType
        , "reason"            .= _cwefeaReason
        , "details"           .= _cwefeaDetails
        , "initiatedEventId"  .= _cwefeaInitiatedEventId
        , "startedEventId"    .= _cwefeaStartedEventId
        ]

data DomainInfo = DomainInfo
    { _diDescription :: Maybe Text
    , _diName        :: Text
    , _diStatus      :: RegistrationStatus
    } deriving (Eq, Show)

-- | 'DomainInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diDescription' @::@ 'Maybe' 'Text'
--
-- * 'diName' @::@ 'Text'
--
-- * 'diStatus' @::@ 'RegistrationStatus'
--
domainInfo :: Text -- ^ 'diName'
           -> RegistrationStatus -- ^ 'diStatus'
           -> DomainInfo
domainInfo p1 p2 = DomainInfo
    { _diName        = p1
    , _diStatus      = p2
    , _diDescription = Nothing
    }

-- | The description of the domain provided through 'RegisterDomain'.
diDescription :: Lens' DomainInfo (Maybe Text)
diDescription = lens _diDescription (\s a -> s { _diDescription = a })

-- | The name of the domain. This name is unique within the account.
diName :: Lens' DomainInfo Text
diName = lens _diName (\s a -> s { _diName = a })

-- | The status of the domain:
--
-- REGISTERED: The domain is properly registered and available. You can use
-- this domain for registering types and creating new workflow executions.   DEPRECATED
-- : The domain was deprecated using 'DeprecateDomain', but is still in use. You
-- should not create new workflow executions in this domain.
diStatus :: Lens' DomainInfo RegistrationStatus
diStatus = lens _diStatus (\s a -> s { _diStatus = a })

instance FromJSON DomainInfo where
    parseJSON = withObject "DomainInfo" $ \o -> DomainInfo
        <$> o .:? "description"
        <*> o .:  "name"
        <*> o .:  "status"

instance ToJSON DomainInfo where
    toJSON DomainInfo{..} = object
        [ "name"        .= _diName
        , "status"      .= _diStatus
        , "description" .= _diDescription
        ]

data HistoryEvent = HistoryEvent
    { _heActivityTaskCancelRequestedEventAttributes                     :: Maybe ActivityTaskCancelRequestedEventAttributes
    , _heActivityTaskCanceledEventAttributes                            :: Maybe ActivityTaskCanceledEventAttributes
    , _heActivityTaskCompletedEventAttributes                           :: Maybe ActivityTaskCompletedEventAttributes
    , _heActivityTaskFailedEventAttributes                              :: Maybe ActivityTaskFailedEventAttributes
    , _heActivityTaskScheduledEventAttributes                           :: Maybe ActivityTaskScheduledEventAttributes
    , _heActivityTaskStartedEventAttributes                             :: Maybe ActivityTaskStartedEventAttributes
    , _heActivityTaskTimedOutEventAttributes                            :: Maybe ActivityTaskTimedOutEventAttributes
    , _heCancelTimerFailedEventAttributes                               :: Maybe CancelTimerFailedEventAttributes
    , _heCancelWorkflowExecutionFailedEventAttributes                   :: Maybe CancelWorkflowExecutionFailedEventAttributes
    , _heChildWorkflowExecutionCanceledEventAttributes                  :: Maybe ChildWorkflowExecutionCanceledEventAttributes
    , _heChildWorkflowExecutionCompletedEventAttributes                 :: Maybe ChildWorkflowExecutionCompletedEventAttributes
    , _heChildWorkflowExecutionFailedEventAttributes                    :: Maybe ChildWorkflowExecutionFailedEventAttributes
    , _heChildWorkflowExecutionStartedEventAttributes                   :: Maybe ChildWorkflowExecutionStartedEventAttributes
    , _heChildWorkflowExecutionTerminatedEventAttributes                :: Maybe ChildWorkflowExecutionTerminatedEventAttributes
    , _heChildWorkflowExecutionTimedOutEventAttributes                  :: Maybe ChildWorkflowExecutionTimedOutEventAttributes
    , _heCompleteWorkflowExecutionFailedEventAttributes                 :: Maybe CompleteWorkflowExecutionFailedEventAttributes
    , _heContinueAsNewWorkflowExecutionFailedEventAttributes            :: Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes
    , _heDecisionTaskCompletedEventAttributes                           :: Maybe DecisionTaskCompletedEventAttributes
    , _heDecisionTaskScheduledEventAttributes                           :: Maybe DecisionTaskScheduledEventAttributes
    , _heDecisionTaskStartedEventAttributes                             :: Maybe DecisionTaskStartedEventAttributes
    , _heDecisionTaskTimedOutEventAttributes                            :: Maybe DecisionTaskTimedOutEventAttributes
    , _heEventId                                                        :: Integer
    , _heEventTimestamp                                                 :: POSIX
    , _heEventType                                                      :: EventType
    , _heExternalWorkflowExecutionCancelRequestedEventAttributes        :: Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes
    , _heExternalWorkflowExecutionSignaledEventAttributes               :: Maybe ExternalWorkflowExecutionSignaledEventAttributes
    , _heFailWorkflowExecutionFailedEventAttributes                     :: Maybe FailWorkflowExecutionFailedEventAttributes
    , _heMarkerRecordedEventAttributes                                  :: Maybe MarkerRecordedEventAttributes
    , _heRecordMarkerFailedEventAttributes                              :: Maybe RecordMarkerFailedEventAttributes
    , _heRequestCancelActivityTaskFailedEventAttributes                 :: Maybe RequestCancelActivityTaskFailedEventAttributes
    , _heRequestCancelExternalWorkflowExecutionFailedEventAttributes    :: Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes
    , _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
    , _heScheduleActivityTaskFailedEventAttributes                      :: Maybe ScheduleActivityTaskFailedEventAttributes
    , _heSignalExternalWorkflowExecutionFailedEventAttributes           :: Maybe SignalExternalWorkflowExecutionFailedEventAttributes
    , _heSignalExternalWorkflowExecutionInitiatedEventAttributes        :: Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes
    , _heStartChildWorkflowExecutionFailedEventAttributes               :: Maybe StartChildWorkflowExecutionFailedEventAttributes
    , _heStartChildWorkflowExecutionInitiatedEventAttributes            :: Maybe StartChildWorkflowExecutionInitiatedEventAttributes
    , _heStartTimerFailedEventAttributes                                :: Maybe StartTimerFailedEventAttributes
    , _heTimerCanceledEventAttributes                                   :: Maybe TimerCanceledEventAttributes
    , _heTimerFiredEventAttributes                                      :: Maybe TimerFiredEventAttributes
    , _heTimerStartedEventAttributes                                    :: Maybe TimerStartedEventAttributes
    , _heWorkflowExecutionCancelRequestedEventAttributes                :: Maybe WorkflowExecutionCancelRequestedEventAttributes
    , _heWorkflowExecutionCanceledEventAttributes                       :: Maybe WorkflowExecutionCanceledEventAttributes
    , _heWorkflowExecutionCompletedEventAttributes                      :: Maybe WorkflowExecutionCompletedEventAttributes
    , _heWorkflowExecutionContinuedAsNewEventAttributes                 :: Maybe WorkflowExecutionContinuedAsNewEventAttributes
    , _heWorkflowExecutionFailedEventAttributes                         :: Maybe WorkflowExecutionFailedEventAttributes
    , _heWorkflowExecutionSignaledEventAttributes                       :: Maybe WorkflowExecutionSignaledEventAttributes
    , _heWorkflowExecutionStartedEventAttributes                        :: Maybe WorkflowExecutionStartedEventAttributes
    , _heWorkflowExecutionTerminatedEventAttributes                     :: Maybe WorkflowExecutionTerminatedEventAttributes
    , _heWorkflowExecutionTimedOutEventAttributes                       :: Maybe WorkflowExecutionTimedOutEventAttributes
    } deriving (Eq, Show)

-- | 'HistoryEvent' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'heActivityTaskCancelRequestedEventAttributes' @::@ 'Maybe' 'ActivityTaskCancelRequestedEventAttributes'
--
-- * 'heActivityTaskCanceledEventAttributes' @::@ 'Maybe' 'ActivityTaskCanceledEventAttributes'
--
-- * 'heActivityTaskCompletedEventAttributes' @::@ 'Maybe' 'ActivityTaskCompletedEventAttributes'
--
-- * 'heActivityTaskFailedEventAttributes' @::@ 'Maybe' 'ActivityTaskFailedEventAttributes'
--
-- * 'heActivityTaskScheduledEventAttributes' @::@ 'Maybe' 'ActivityTaskScheduledEventAttributes'
--
-- * 'heActivityTaskStartedEventAttributes' @::@ 'Maybe' 'ActivityTaskStartedEventAttributes'
--
-- * 'heActivityTaskTimedOutEventAttributes' @::@ 'Maybe' 'ActivityTaskTimedOutEventAttributes'
--
-- * 'heCancelTimerFailedEventAttributes' @::@ 'Maybe' 'CancelTimerFailedEventAttributes'
--
-- * 'heCancelWorkflowExecutionFailedEventAttributes' @::@ 'Maybe' 'CancelWorkflowExecutionFailedEventAttributes'
--
-- * 'heChildWorkflowExecutionCanceledEventAttributes' @::@ 'Maybe' 'ChildWorkflowExecutionCanceledEventAttributes'
--
-- * 'heChildWorkflowExecutionCompletedEventAttributes' @::@ 'Maybe' 'ChildWorkflowExecutionCompletedEventAttributes'
--
-- * 'heChildWorkflowExecutionFailedEventAttributes' @::@ 'Maybe' 'ChildWorkflowExecutionFailedEventAttributes'
--
-- * 'heChildWorkflowExecutionStartedEventAttributes' @::@ 'Maybe' 'ChildWorkflowExecutionStartedEventAttributes'
--
-- * 'heChildWorkflowExecutionTerminatedEventAttributes' @::@ 'Maybe' 'ChildWorkflowExecutionTerminatedEventAttributes'
--
-- * 'heChildWorkflowExecutionTimedOutEventAttributes' @::@ 'Maybe' 'ChildWorkflowExecutionTimedOutEventAttributes'
--
-- * 'heCompleteWorkflowExecutionFailedEventAttributes' @::@ 'Maybe' 'CompleteWorkflowExecutionFailedEventAttributes'
--
-- * 'heContinueAsNewWorkflowExecutionFailedEventAttributes' @::@ 'Maybe' 'ContinueAsNewWorkflowExecutionFailedEventAttributes'
--
-- * 'heDecisionTaskCompletedEventAttributes' @::@ 'Maybe' 'DecisionTaskCompletedEventAttributes'
--
-- * 'heDecisionTaskScheduledEventAttributes' @::@ 'Maybe' 'DecisionTaskScheduledEventAttributes'
--
-- * 'heDecisionTaskStartedEventAttributes' @::@ 'Maybe' 'DecisionTaskStartedEventAttributes'
--
-- * 'heDecisionTaskTimedOutEventAttributes' @::@ 'Maybe' 'DecisionTaskTimedOutEventAttributes'
--
-- * 'heEventId' @::@ 'Integer'
--
-- * 'heEventTimestamp' @::@ 'UTCTime'
--
-- * 'heEventType' @::@ 'EventType'
--
-- * 'heExternalWorkflowExecutionCancelRequestedEventAttributes' @::@ 'Maybe' 'ExternalWorkflowExecutionCancelRequestedEventAttributes'
--
-- * 'heExternalWorkflowExecutionSignaledEventAttributes' @::@ 'Maybe' 'ExternalWorkflowExecutionSignaledEventAttributes'
--
-- * 'heFailWorkflowExecutionFailedEventAttributes' @::@ 'Maybe' 'FailWorkflowExecutionFailedEventAttributes'
--
-- * 'heMarkerRecordedEventAttributes' @::@ 'Maybe' 'MarkerRecordedEventAttributes'
--
-- * 'heRecordMarkerFailedEventAttributes' @::@ 'Maybe' 'RecordMarkerFailedEventAttributes'
--
-- * 'heRequestCancelActivityTaskFailedEventAttributes' @::@ 'Maybe' 'RequestCancelActivityTaskFailedEventAttributes'
--
-- * 'heRequestCancelExternalWorkflowExecutionFailedEventAttributes' @::@ 'Maybe' 'RequestCancelExternalWorkflowExecutionFailedEventAttributes'
--
-- * 'heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes' @::@ 'Maybe' 'RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
--
-- * 'heScheduleActivityTaskFailedEventAttributes' @::@ 'Maybe' 'ScheduleActivityTaskFailedEventAttributes'
--
-- * 'heSignalExternalWorkflowExecutionFailedEventAttributes' @::@ 'Maybe' 'SignalExternalWorkflowExecutionFailedEventAttributes'
--
-- * 'heSignalExternalWorkflowExecutionInitiatedEventAttributes' @::@ 'Maybe' 'SignalExternalWorkflowExecutionInitiatedEventAttributes'
--
-- * 'heStartChildWorkflowExecutionFailedEventAttributes' @::@ 'Maybe' 'StartChildWorkflowExecutionFailedEventAttributes'
--
-- * 'heStartChildWorkflowExecutionInitiatedEventAttributes' @::@ 'Maybe' 'StartChildWorkflowExecutionInitiatedEventAttributes'
--
-- * 'heStartTimerFailedEventAttributes' @::@ 'Maybe' 'StartTimerFailedEventAttributes'
--
-- * 'heTimerCanceledEventAttributes' @::@ 'Maybe' 'TimerCanceledEventAttributes'
--
-- * 'heTimerFiredEventAttributes' @::@ 'Maybe' 'TimerFiredEventAttributes'
--
-- * 'heTimerStartedEventAttributes' @::@ 'Maybe' 'TimerStartedEventAttributes'
--
-- * 'heWorkflowExecutionCancelRequestedEventAttributes' @::@ 'Maybe' 'WorkflowExecutionCancelRequestedEventAttributes'
--
-- * 'heWorkflowExecutionCanceledEventAttributes' @::@ 'Maybe' 'WorkflowExecutionCanceledEventAttributes'
--
-- * 'heWorkflowExecutionCompletedEventAttributes' @::@ 'Maybe' 'WorkflowExecutionCompletedEventAttributes'
--
-- * 'heWorkflowExecutionContinuedAsNewEventAttributes' @::@ 'Maybe' 'WorkflowExecutionContinuedAsNewEventAttributes'
--
-- * 'heWorkflowExecutionFailedEventAttributes' @::@ 'Maybe' 'WorkflowExecutionFailedEventAttributes'
--
-- * 'heWorkflowExecutionSignaledEventAttributes' @::@ 'Maybe' 'WorkflowExecutionSignaledEventAttributes'
--
-- * 'heWorkflowExecutionStartedEventAttributes' @::@ 'Maybe' 'WorkflowExecutionStartedEventAttributes'
--
-- * 'heWorkflowExecutionTerminatedEventAttributes' @::@ 'Maybe' 'WorkflowExecutionTerminatedEventAttributes'
--
-- * 'heWorkflowExecutionTimedOutEventAttributes' @::@ 'Maybe' 'WorkflowExecutionTimedOutEventAttributes'
--
historyEvent :: UTCTime -- ^ 'heEventTimestamp'
             -> EventType -- ^ 'heEventType'
             -> Integer -- ^ 'heEventId'
             -> HistoryEvent
historyEvent p1 p2 p3 = HistoryEvent
    { _heEventTimestamp                                                 = withIso _Time (const id) p1
    , _heEventType                                                      = p2
    , _heEventId                                                        = p3
    , _heWorkflowExecutionStartedEventAttributes                        = Nothing
    , _heWorkflowExecutionCompletedEventAttributes                      = Nothing
    , _heCompleteWorkflowExecutionFailedEventAttributes                 = Nothing
    , _heWorkflowExecutionFailedEventAttributes                         = Nothing
    , _heFailWorkflowExecutionFailedEventAttributes                     = Nothing
    , _heWorkflowExecutionTimedOutEventAttributes                       = Nothing
    , _heWorkflowExecutionCanceledEventAttributes                       = Nothing
    , _heCancelWorkflowExecutionFailedEventAttributes                   = Nothing
    , _heWorkflowExecutionContinuedAsNewEventAttributes                 = Nothing
    , _heContinueAsNewWorkflowExecutionFailedEventAttributes            = Nothing
    , _heWorkflowExecutionTerminatedEventAttributes                     = Nothing
    , _heWorkflowExecutionCancelRequestedEventAttributes                = Nothing
    , _heDecisionTaskScheduledEventAttributes                           = Nothing
    , _heDecisionTaskStartedEventAttributes                             = Nothing
    , _heDecisionTaskCompletedEventAttributes                           = Nothing
    , _heDecisionTaskTimedOutEventAttributes                            = Nothing
    , _heActivityTaskScheduledEventAttributes                           = Nothing
    , _heActivityTaskStartedEventAttributes                             = Nothing
    , _heActivityTaskCompletedEventAttributes                           = Nothing
    , _heActivityTaskFailedEventAttributes                              = Nothing
    , _heActivityTaskTimedOutEventAttributes                            = Nothing
    , _heActivityTaskCanceledEventAttributes                            = Nothing
    , _heActivityTaskCancelRequestedEventAttributes                     = Nothing
    , _heWorkflowExecutionSignaledEventAttributes                       = Nothing
    , _heMarkerRecordedEventAttributes                                  = Nothing
    , _heRecordMarkerFailedEventAttributes                              = Nothing
    , _heTimerStartedEventAttributes                                    = Nothing
    , _heTimerFiredEventAttributes                                      = Nothing
    , _heTimerCanceledEventAttributes                                   = Nothing
    , _heStartChildWorkflowExecutionInitiatedEventAttributes            = Nothing
    , _heChildWorkflowExecutionStartedEventAttributes                   = Nothing
    , _heChildWorkflowExecutionCompletedEventAttributes                 = Nothing
    , _heChildWorkflowExecutionFailedEventAttributes                    = Nothing
    , _heChildWorkflowExecutionTimedOutEventAttributes                  = Nothing
    , _heChildWorkflowExecutionCanceledEventAttributes                  = Nothing
    , _heChildWorkflowExecutionTerminatedEventAttributes                = Nothing
    , _heSignalExternalWorkflowExecutionInitiatedEventAttributes        = Nothing
    , _heExternalWorkflowExecutionSignaledEventAttributes               = Nothing
    , _heSignalExternalWorkflowExecutionFailedEventAttributes           = Nothing
    , _heExternalWorkflowExecutionCancelRequestedEventAttributes        = Nothing
    , _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes = Nothing
    , _heRequestCancelExternalWorkflowExecutionFailedEventAttributes    = Nothing
    , _heScheduleActivityTaskFailedEventAttributes                      = Nothing
    , _heRequestCancelActivityTaskFailedEventAttributes                 = Nothing
    , _heStartTimerFailedEventAttributes                                = Nothing
    , _heCancelTimerFailedEventAttributes                               = Nothing
    , _heStartChildWorkflowExecutionFailedEventAttributes               = Nothing
    }

-- | If the event is of type 'ActivityTaskcancelRequested' then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heActivityTaskCancelRequestedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskCancelRequestedEventAttributes)
heActivityTaskCancelRequestedEventAttributes =
    lens _heActivityTaskCancelRequestedEventAttributes
        (\s a -> s { _heActivityTaskCancelRequestedEventAttributes = a })

-- | If the event is of type 'ActivityTaskCanceled' then this member is set and
-- provides detailed information about the event. It is not set for other event
-- types.
heActivityTaskCanceledEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskCanceledEventAttributes)
heActivityTaskCanceledEventAttributes =
    lens _heActivityTaskCanceledEventAttributes
        (\s a -> s { _heActivityTaskCanceledEventAttributes = a })

-- | If the event is of type 'ActivityTaskCompleted' then this member is set and
-- provides detailed information about the event. It is not set for other event
-- types.
heActivityTaskCompletedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskCompletedEventAttributes)
heActivityTaskCompletedEventAttributes =
    lens _heActivityTaskCompletedEventAttributes
        (\s a -> s { _heActivityTaskCompletedEventAttributes = a })

-- | If the event is of type 'ActivityTaskFailed' then this member is set and
-- provides detailed information about the event. It is not set for other event
-- types.
heActivityTaskFailedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskFailedEventAttributes)
heActivityTaskFailedEventAttributes =
    lens _heActivityTaskFailedEventAttributes
        (\s a -> s { _heActivityTaskFailedEventAttributes = a })

-- | If the event is of type 'ActivityTaskScheduled' then this member is set and
-- provides detailed information about the event. It is not set for other event
-- types.
heActivityTaskScheduledEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskScheduledEventAttributes)
heActivityTaskScheduledEventAttributes =
    lens _heActivityTaskScheduledEventAttributes
        (\s a -> s { _heActivityTaskScheduledEventAttributes = a })

-- | If the event is of type 'ActivityTaskStarted' then this member is set and
-- provides detailed information about the event. It is not set for other event
-- types.
heActivityTaskStartedEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskStartedEventAttributes)
heActivityTaskStartedEventAttributes =
    lens _heActivityTaskStartedEventAttributes
        (\s a -> s { _heActivityTaskStartedEventAttributes = a })

-- | If the event is of type 'ActivityTaskTimedOut' then this member is set and
-- provides detailed information about the event. It is not set for other event
-- types.
heActivityTaskTimedOutEventAttributes :: Lens' HistoryEvent (Maybe ActivityTaskTimedOutEventAttributes)
heActivityTaskTimedOutEventAttributes =
    lens _heActivityTaskTimedOutEventAttributes
        (\s a -> s { _heActivityTaskTimedOutEventAttributes = a })

-- | If the event is of type 'CancelTimerFailed' then this member is set and
-- provides detailed information about the event. It is not set for other event
-- types.
heCancelTimerFailedEventAttributes :: Lens' HistoryEvent (Maybe CancelTimerFailedEventAttributes)
heCancelTimerFailedEventAttributes =
    lens _heCancelTimerFailedEventAttributes
        (\s a -> s { _heCancelTimerFailedEventAttributes = a })

-- | If the event is of type 'CancelWorkflowExecutionFailed' then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heCancelWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe CancelWorkflowExecutionFailedEventAttributes)
heCancelWorkflowExecutionFailedEventAttributes =
    lens _heCancelWorkflowExecutionFailedEventAttributes
        (\s a -> s { _heCancelWorkflowExecutionFailedEventAttributes = a })

-- | If the event is of type 'ChildWorkflowExecutionCanceled' then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionCanceledEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionCanceledEventAttributes)
heChildWorkflowExecutionCanceledEventAttributes =
    lens _heChildWorkflowExecutionCanceledEventAttributes
        (\s a -> s { _heChildWorkflowExecutionCanceledEventAttributes = a })

-- | If the event is of type 'ChildWorkflowExecutionCompleted' then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionCompletedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionCompletedEventAttributes)
heChildWorkflowExecutionCompletedEventAttributes =
    lens _heChildWorkflowExecutionCompletedEventAttributes
        (\s a -> s { _heChildWorkflowExecutionCompletedEventAttributes = a })

-- | If the event is of type 'ChildWorkflowExecutionFailed' then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heChildWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionFailedEventAttributes)
heChildWorkflowExecutionFailedEventAttributes =
    lens _heChildWorkflowExecutionFailedEventAttributes
        (\s a -> s { _heChildWorkflowExecutionFailedEventAttributes = a })

-- | If the event is of type 'ChildWorkflowExecutionStarted' then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionStartedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionStartedEventAttributes)
heChildWorkflowExecutionStartedEventAttributes =
    lens _heChildWorkflowExecutionStartedEventAttributes
        (\s a -> s { _heChildWorkflowExecutionStartedEventAttributes = a })

-- | If the event is of type 'ChildWorkflowExecutionTerminated' then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionTerminatedEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionTerminatedEventAttributes)
heChildWorkflowExecutionTerminatedEventAttributes =
    lens _heChildWorkflowExecutionTerminatedEventAttributes
        (\s a -> s { _heChildWorkflowExecutionTerminatedEventAttributes = a })

-- | If the event is of type 'ChildWorkflowExecutionTimedOut' then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heChildWorkflowExecutionTimedOutEventAttributes :: Lens' HistoryEvent (Maybe ChildWorkflowExecutionTimedOutEventAttributes)
heChildWorkflowExecutionTimedOutEventAttributes =
    lens _heChildWorkflowExecutionTimedOutEventAttributes
        (\s a -> s { _heChildWorkflowExecutionTimedOutEventAttributes = a })

-- | If the event is of type 'CompleteWorkflowExecutionFailed' then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heCompleteWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe CompleteWorkflowExecutionFailedEventAttributes)
heCompleteWorkflowExecutionFailedEventAttributes =
    lens _heCompleteWorkflowExecutionFailedEventAttributes
        (\s a -> s { _heCompleteWorkflowExecutionFailedEventAttributes = a })

-- | If the event is of type 'ContinueAsNewWorkflowExecutionFailed' then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heContinueAsNewWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes)
heContinueAsNewWorkflowExecutionFailedEventAttributes =
    lens _heContinueAsNewWorkflowExecutionFailedEventAttributes
        (\s a -> s { _heContinueAsNewWorkflowExecutionFailedEventAttributes = a })

-- | If the event is of type 'DecisionTaskCompleted' then this member is set and
-- provides detailed information about the event. It is not set for other event
-- types.
heDecisionTaskCompletedEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskCompletedEventAttributes)
heDecisionTaskCompletedEventAttributes =
    lens _heDecisionTaskCompletedEventAttributes
        (\s a -> s { _heDecisionTaskCompletedEventAttributes = a })

-- | If the event is of type 'DecisionTaskScheduled' then this member is set and
-- provides detailed information about the event. It is not set for other event
-- types.
heDecisionTaskScheduledEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskScheduledEventAttributes)
heDecisionTaskScheduledEventAttributes =
    lens _heDecisionTaskScheduledEventAttributes
        (\s a -> s { _heDecisionTaskScheduledEventAttributes = a })

-- | If the event is of type 'DecisionTaskStarted' then this member is set and
-- provides detailed information about the event. It is not set for other event
-- types.
heDecisionTaskStartedEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskStartedEventAttributes)
heDecisionTaskStartedEventAttributes =
    lens _heDecisionTaskStartedEventAttributes
        (\s a -> s { _heDecisionTaskStartedEventAttributes = a })

-- | If the event is of type 'DecisionTaskTimedOut' then this member is set and
-- provides detailed information about the event. It is not set for other event
-- types.
heDecisionTaskTimedOutEventAttributes :: Lens' HistoryEvent (Maybe DecisionTaskTimedOutEventAttributes)
heDecisionTaskTimedOutEventAttributes =
    lens _heDecisionTaskTimedOutEventAttributes
        (\s a -> s { _heDecisionTaskTimedOutEventAttributes = a })

-- | The system generated id of the event. This id uniquely identifies the event
-- with in the workflow execution history.
heEventId :: Lens' HistoryEvent Integer
heEventId = lens _heEventId (\s a -> s { _heEventId = a })

-- | The date and time when the event occurred.
heEventTimestamp :: Lens' HistoryEvent UTCTime
heEventTimestamp = lens _heEventTimestamp (\s a -> s { _heEventTimestamp = a }) . _Time

-- | The type of the history event.
heEventType :: Lens' HistoryEvent EventType
heEventType = lens _heEventType (\s a -> s { _heEventType = a })

-- | If the event is of type 'ExternalWorkflowExecutionCancelRequested' then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heExternalWorkflowExecutionCancelRequestedEventAttributes :: Lens' HistoryEvent (Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes)
heExternalWorkflowExecutionCancelRequestedEventAttributes =
    lens _heExternalWorkflowExecutionCancelRequestedEventAttributes
        (\s a -> s { _heExternalWorkflowExecutionCancelRequestedEventAttributes = a })

-- | If the event is of type 'ExternalWorkflowExecutionSignaled' then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
heExternalWorkflowExecutionSignaledEventAttributes :: Lens' HistoryEvent (Maybe ExternalWorkflowExecutionSignaledEventAttributes)
heExternalWorkflowExecutionSignaledEventAttributes =
    lens _heExternalWorkflowExecutionSignaledEventAttributes
        (\s a -> s { _heExternalWorkflowExecutionSignaledEventAttributes = a })

-- | If the event is of type 'FailWorkflowExecutionFailed' then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heFailWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe FailWorkflowExecutionFailedEventAttributes)
heFailWorkflowExecutionFailedEventAttributes =
    lens _heFailWorkflowExecutionFailedEventAttributes
        (\s a -> s { _heFailWorkflowExecutionFailedEventAttributes = a })

-- | If the event is of type 'MarkerRecorded' then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heMarkerRecordedEventAttributes :: Lens' HistoryEvent (Maybe MarkerRecordedEventAttributes)
heMarkerRecordedEventAttributes =
    lens _heMarkerRecordedEventAttributes
        (\s a -> s { _heMarkerRecordedEventAttributes = a })

-- | If the event is of type 'DecisionTaskFailed' then this member is set and
-- provides detailed information about the event. It is not set for other event
-- types.
heRecordMarkerFailedEventAttributes :: Lens' HistoryEvent (Maybe RecordMarkerFailedEventAttributes)
heRecordMarkerFailedEventAttributes =
    lens _heRecordMarkerFailedEventAttributes
        (\s a -> s { _heRecordMarkerFailedEventAttributes = a })

-- | If the event is of type 'RequestCancelActivityTaskFailed' then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heRequestCancelActivityTaskFailedEventAttributes :: Lens' HistoryEvent (Maybe RequestCancelActivityTaskFailedEventAttributes)
heRequestCancelActivityTaskFailedEventAttributes =
    lens _heRequestCancelActivityTaskFailedEventAttributes
        (\s a -> s { _heRequestCancelActivityTaskFailedEventAttributes = a })

-- | If the event is of type 'RequestCancelExternalWorkflowExecutionFailed' then
-- this member is set and provides detailed information about the event. It is
-- not set for other event types.
heRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes)
heRequestCancelExternalWorkflowExecutionFailedEventAttributes =
    lens _heRequestCancelExternalWorkflowExecutionFailedEventAttributes
        (\s a -> s { _heRequestCancelExternalWorkflowExecutionFailedEventAttributes = a })

-- | If the event is of type 'RequestCancelExternalWorkflowExecutionInitiated' then
-- this member is set and provides detailed information about the event. It is
-- not set for other event types.
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Lens' HistoryEvent (Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes =
    lens _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes
        (\s a -> s { _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes = a })

-- | If the event is of type 'ScheduleActivityTaskFailed' then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heScheduleActivityTaskFailedEventAttributes :: Lens' HistoryEvent (Maybe ScheduleActivityTaskFailedEventAttributes)
heScheduleActivityTaskFailedEventAttributes =
    lens _heScheduleActivityTaskFailedEventAttributes
        (\s a -> s { _heScheduleActivityTaskFailedEventAttributes = a })

-- | If the event is of type 'SignalExternalWorkflowExecutionFailed' then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heSignalExternalWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe SignalExternalWorkflowExecutionFailedEventAttributes)
heSignalExternalWorkflowExecutionFailedEventAttributes =
    lens _heSignalExternalWorkflowExecutionFailedEventAttributes
        (\s a -> s { _heSignalExternalWorkflowExecutionFailedEventAttributes = a })

-- | If the event is of type 'SignalExternalWorkflowExecutionInitiated' then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heSignalExternalWorkflowExecutionInitiatedEventAttributes :: Lens' HistoryEvent (Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes)
heSignalExternalWorkflowExecutionInitiatedEventAttributes =
    lens _heSignalExternalWorkflowExecutionInitiatedEventAttributes
        (\s a -> s { _heSignalExternalWorkflowExecutionInitiatedEventAttributes = a })

-- | If the event is of type 'StartChildWorkflowExecutionFailed' then this member
-- is set and provides detailed information about the event. It is not set for
-- other event types.
heStartChildWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe StartChildWorkflowExecutionFailedEventAttributes)
heStartChildWorkflowExecutionFailedEventAttributes =
    lens _heStartChildWorkflowExecutionFailedEventAttributes
        (\s a -> s { _heStartChildWorkflowExecutionFailedEventAttributes = a })

-- | If the event is of type 'StartChildWorkflowExecutionInitiated' then this
-- member is set and provides detailed information about the event. It is not
-- set for other event types.
heStartChildWorkflowExecutionInitiatedEventAttributes :: Lens' HistoryEvent (Maybe StartChildWorkflowExecutionInitiatedEventAttributes)
heStartChildWorkflowExecutionInitiatedEventAttributes =
    lens _heStartChildWorkflowExecutionInitiatedEventAttributes
        (\s a -> s { _heStartChildWorkflowExecutionInitiatedEventAttributes = a })

-- | If the event is of type 'StartTimerFailed' then this member is set and
-- provides detailed information about the event. It is not set for other event
-- types.
heStartTimerFailedEventAttributes :: Lens' HistoryEvent (Maybe StartTimerFailedEventAttributes)
heStartTimerFailedEventAttributes =
    lens _heStartTimerFailedEventAttributes
        (\s a -> s { _heStartTimerFailedEventAttributes = a })

-- | If the event is of type 'TimerCanceled' then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heTimerCanceledEventAttributes :: Lens' HistoryEvent (Maybe TimerCanceledEventAttributes)
heTimerCanceledEventAttributes =
    lens _heTimerCanceledEventAttributes
        (\s a -> s { _heTimerCanceledEventAttributes = a })

-- | If the event is of type 'TimerFired' then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heTimerFiredEventAttributes :: Lens' HistoryEvent (Maybe TimerFiredEventAttributes)
heTimerFiredEventAttributes =
    lens _heTimerFiredEventAttributes
        (\s a -> s { _heTimerFiredEventAttributes = a })

-- | If the event is of type 'TimerStarted' then this member is set and provides
-- detailed information about the event. It is not set for other event types.
heTimerStartedEventAttributes :: Lens' HistoryEvent (Maybe TimerStartedEventAttributes)
heTimerStartedEventAttributes =
    lens _heTimerStartedEventAttributes
        (\s a -> s { _heTimerStartedEventAttributes = a })

-- | If the event is of type 'WorkflowExecutionCancelRequested' then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heWorkflowExecutionCancelRequestedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionCancelRequestedEventAttributes)
heWorkflowExecutionCancelRequestedEventAttributes =
    lens _heWorkflowExecutionCancelRequestedEventAttributes
        (\s a -> s { _heWorkflowExecutionCancelRequestedEventAttributes = a })

-- | If the event is of type 'WorkflowExecutionCanceled' then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionCanceledEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionCanceledEventAttributes)
heWorkflowExecutionCanceledEventAttributes =
    lens _heWorkflowExecutionCanceledEventAttributes
        (\s a -> s { _heWorkflowExecutionCanceledEventAttributes = a })

-- | If the event is of type 'WorkflowExecutionCompleted' then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionCompletedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionCompletedEventAttributes)
heWorkflowExecutionCompletedEventAttributes =
    lens _heWorkflowExecutionCompletedEventAttributes
        (\s a -> s { _heWorkflowExecutionCompletedEventAttributes = a })

-- | If the event is of type 'WorkflowExecutionContinuedAsNew' then this member is
-- set and provides detailed information about the event. It is not set for
-- other event types.
heWorkflowExecutionContinuedAsNewEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionContinuedAsNewEventAttributes)
heWorkflowExecutionContinuedAsNewEventAttributes =
    lens _heWorkflowExecutionContinuedAsNewEventAttributes
        (\s a -> s { _heWorkflowExecutionContinuedAsNewEventAttributes = a })

-- | If the event is of type 'WorkflowExecutionFailed' then this member is set and
-- provides detailed information about the event. It is not set for other event
-- types.
heWorkflowExecutionFailedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionFailedEventAttributes)
heWorkflowExecutionFailedEventAttributes =
    lens _heWorkflowExecutionFailedEventAttributes
        (\s a -> s { _heWorkflowExecutionFailedEventAttributes = a })

-- | If the event is of type 'WorkflowExecutionSignaled' then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionSignaledEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionSignaledEventAttributes)
heWorkflowExecutionSignaledEventAttributes =
    lens _heWorkflowExecutionSignaledEventAttributes
        (\s a -> s { _heWorkflowExecutionSignaledEventAttributes = a })

-- | If the event is of type 'WorkflowExecutionStarted' then this member is set and
-- provides detailed information about the event. It is not set for other event
-- types.
heWorkflowExecutionStartedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionStartedEventAttributes)
heWorkflowExecutionStartedEventAttributes =
    lens _heWorkflowExecutionStartedEventAttributes
        (\s a -> s { _heWorkflowExecutionStartedEventAttributes = a })

-- | If the event is of type 'WorkflowExecutionTerminated' then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionTerminatedEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionTerminatedEventAttributes)
heWorkflowExecutionTerminatedEventAttributes =
    lens _heWorkflowExecutionTerminatedEventAttributes
        (\s a -> s { _heWorkflowExecutionTerminatedEventAttributes = a })

-- | If the event is of type 'WorkflowExecutionTimedOut' then this member is set
-- and provides detailed information about the event. It is not set for other
-- event types.
heWorkflowExecutionTimedOutEventAttributes :: Lens' HistoryEvent (Maybe WorkflowExecutionTimedOutEventAttributes)
heWorkflowExecutionTimedOutEventAttributes =
    lens _heWorkflowExecutionTimedOutEventAttributes
        (\s a -> s { _heWorkflowExecutionTimedOutEventAttributes = a })

instance FromJSON HistoryEvent where
    parseJSON = withObject "HistoryEvent" $ \o -> HistoryEvent
        <$> o .:? "activityTaskCancelRequestedEventAttributes"
        <*> o .:? "activityTaskCanceledEventAttributes"
        <*> o .:? "activityTaskCompletedEventAttributes"
        <*> o .:? "activityTaskFailedEventAttributes"
        <*> o .:? "activityTaskScheduledEventAttributes"
        <*> o .:? "activityTaskStartedEventAttributes"
        <*> o .:? "activityTaskTimedOutEventAttributes"
        <*> o .:? "cancelTimerFailedEventAttributes"
        <*> o .:? "cancelWorkflowExecutionFailedEventAttributes"
        <*> o .:? "childWorkflowExecutionCanceledEventAttributes"
        <*> o .:? "childWorkflowExecutionCompletedEventAttributes"
        <*> o .:? "childWorkflowExecutionFailedEventAttributes"
        <*> o .:? "childWorkflowExecutionStartedEventAttributes"
        <*> o .:? "childWorkflowExecutionTerminatedEventAttributes"
        <*> o .:? "childWorkflowExecutionTimedOutEventAttributes"
        <*> o .:? "completeWorkflowExecutionFailedEventAttributes"
        <*> o .:? "continueAsNewWorkflowExecutionFailedEventAttributes"
        <*> o .:? "decisionTaskCompletedEventAttributes"
        <*> o .:? "decisionTaskScheduledEventAttributes"
        <*> o .:? "decisionTaskStartedEventAttributes"
        <*> o .:? "decisionTaskTimedOutEventAttributes"
        <*> o .:  "eventId"
        <*> o .:  "eventTimestamp"
        <*> o .:  "eventType"
        <*> o .:? "externalWorkflowExecutionCancelRequestedEventAttributes"
        <*> o .:? "externalWorkflowExecutionSignaledEventAttributes"
        <*> o .:? "failWorkflowExecutionFailedEventAttributes"
        <*> o .:? "markerRecordedEventAttributes"
        <*> o .:? "recordMarkerFailedEventAttributes"
        <*> o .:? "requestCancelActivityTaskFailedEventAttributes"
        <*> o .:? "requestCancelExternalWorkflowExecutionFailedEventAttributes"
        <*> o .:? "requestCancelExternalWorkflowExecutionInitiatedEventAttributes"
        <*> o .:? "scheduleActivityTaskFailedEventAttributes"
        <*> o .:? "signalExternalWorkflowExecutionFailedEventAttributes"
        <*> o .:? "signalExternalWorkflowExecutionInitiatedEventAttributes"
        <*> o .:? "startChildWorkflowExecutionFailedEventAttributes"
        <*> o .:? "startChildWorkflowExecutionInitiatedEventAttributes"
        <*> o .:? "startTimerFailedEventAttributes"
        <*> o .:? "timerCanceledEventAttributes"
        <*> o .:? "timerFiredEventAttributes"
        <*> o .:? "timerStartedEventAttributes"
        <*> o .:? "workflowExecutionCancelRequestedEventAttributes"
        <*> o .:? "workflowExecutionCanceledEventAttributes"
        <*> o .:? "workflowExecutionCompletedEventAttributes"
        <*> o .:? "workflowExecutionContinuedAsNewEventAttributes"
        <*> o .:? "workflowExecutionFailedEventAttributes"
        <*> o .:? "workflowExecutionSignaledEventAttributes"
        <*> o .:? "workflowExecutionStartedEventAttributes"
        <*> o .:? "workflowExecutionTerminatedEventAttributes"
        <*> o .:? "workflowExecutionTimedOutEventAttributes"

instance ToJSON HistoryEvent where
    toJSON HistoryEvent{..} = object
        [ "eventTimestamp"                                                 .= _heEventTimestamp
        , "eventType"                                                      .= _heEventType
        , "eventId"                                                        .= _heEventId
        , "workflowExecutionStartedEventAttributes"                        .= _heWorkflowExecutionStartedEventAttributes
        , "workflowExecutionCompletedEventAttributes"                      .= _heWorkflowExecutionCompletedEventAttributes
        , "completeWorkflowExecutionFailedEventAttributes"                 .= _heCompleteWorkflowExecutionFailedEventAttributes
        , "workflowExecutionFailedEventAttributes"                         .= _heWorkflowExecutionFailedEventAttributes
        , "failWorkflowExecutionFailedEventAttributes"                     .= _heFailWorkflowExecutionFailedEventAttributes
        , "workflowExecutionTimedOutEventAttributes"                       .= _heWorkflowExecutionTimedOutEventAttributes
        , "workflowExecutionCanceledEventAttributes"                       .= _heWorkflowExecutionCanceledEventAttributes
        , "cancelWorkflowExecutionFailedEventAttributes"                   .= _heCancelWorkflowExecutionFailedEventAttributes
        , "workflowExecutionContinuedAsNewEventAttributes"                 .= _heWorkflowExecutionContinuedAsNewEventAttributes
        , "continueAsNewWorkflowExecutionFailedEventAttributes"            .= _heContinueAsNewWorkflowExecutionFailedEventAttributes
        , "workflowExecutionTerminatedEventAttributes"                     .= _heWorkflowExecutionTerminatedEventAttributes
        , "workflowExecutionCancelRequestedEventAttributes"                .= _heWorkflowExecutionCancelRequestedEventAttributes
        , "decisionTaskScheduledEventAttributes"                           .= _heDecisionTaskScheduledEventAttributes
        , "decisionTaskStartedEventAttributes"                             .= _heDecisionTaskStartedEventAttributes
        , "decisionTaskCompletedEventAttributes"                           .= _heDecisionTaskCompletedEventAttributes
        , "decisionTaskTimedOutEventAttributes"                            .= _heDecisionTaskTimedOutEventAttributes
        , "activityTaskScheduledEventAttributes"                           .= _heActivityTaskScheduledEventAttributes
        , "activityTaskStartedEventAttributes"                             .= _heActivityTaskStartedEventAttributes
        , "activityTaskCompletedEventAttributes"                           .= _heActivityTaskCompletedEventAttributes
        , "activityTaskFailedEventAttributes"                              .= _heActivityTaskFailedEventAttributes
        , "activityTaskTimedOutEventAttributes"                            .= _heActivityTaskTimedOutEventAttributes
        , "activityTaskCanceledEventAttributes"                            .= _heActivityTaskCanceledEventAttributes
        , "activityTaskCancelRequestedEventAttributes"                     .= _heActivityTaskCancelRequestedEventAttributes
        , "workflowExecutionSignaledEventAttributes"                       .= _heWorkflowExecutionSignaledEventAttributes
        , "markerRecordedEventAttributes"                                  .= _heMarkerRecordedEventAttributes
        , "recordMarkerFailedEventAttributes"                              .= _heRecordMarkerFailedEventAttributes
        , "timerStartedEventAttributes"                                    .= _heTimerStartedEventAttributes
        , "timerFiredEventAttributes"                                      .= _heTimerFiredEventAttributes
        , "timerCanceledEventAttributes"                                   .= _heTimerCanceledEventAttributes
        , "startChildWorkflowExecutionInitiatedEventAttributes"            .= _heStartChildWorkflowExecutionInitiatedEventAttributes
        , "childWorkflowExecutionStartedEventAttributes"                   .= _heChildWorkflowExecutionStartedEventAttributes
        , "childWorkflowExecutionCompletedEventAttributes"                 .= _heChildWorkflowExecutionCompletedEventAttributes
        , "childWorkflowExecutionFailedEventAttributes"                    .= _heChildWorkflowExecutionFailedEventAttributes
        , "childWorkflowExecutionTimedOutEventAttributes"                  .= _heChildWorkflowExecutionTimedOutEventAttributes
        , "childWorkflowExecutionCanceledEventAttributes"                  .= _heChildWorkflowExecutionCanceledEventAttributes
        , "childWorkflowExecutionTerminatedEventAttributes"                .= _heChildWorkflowExecutionTerminatedEventAttributes
        , "signalExternalWorkflowExecutionInitiatedEventAttributes"        .= _heSignalExternalWorkflowExecutionInitiatedEventAttributes
        , "externalWorkflowExecutionSignaledEventAttributes"               .= _heExternalWorkflowExecutionSignaledEventAttributes
        , "signalExternalWorkflowExecutionFailedEventAttributes"           .= _heSignalExternalWorkflowExecutionFailedEventAttributes
        , "externalWorkflowExecutionCancelRequestedEventAttributes"        .= _heExternalWorkflowExecutionCancelRequestedEventAttributes
        , "requestCancelExternalWorkflowExecutionInitiatedEventAttributes" .= _heRequestCancelExternalWorkflowExecutionInitiatedEventAttributes
        , "requestCancelExternalWorkflowExecutionFailedEventAttributes"    .= _heRequestCancelExternalWorkflowExecutionFailedEventAttributes
        , "scheduleActivityTaskFailedEventAttributes"                      .= _heScheduleActivityTaskFailedEventAttributes
        , "requestCancelActivityTaskFailedEventAttributes"                 .= _heRequestCancelActivityTaskFailedEventAttributes
        , "startTimerFailedEventAttributes"                                .= _heStartTimerFailedEventAttributes
        , "cancelTimerFailedEventAttributes"                               .= _heCancelTimerFailedEventAttributes
        , "startChildWorkflowExecutionFailedEventAttributes"               .= _heStartChildWorkflowExecutionFailedEventAttributes
        ]

data ContinueAsNewWorkflowExecutionFailedEventAttributes = ContinueAsNewWorkflowExecutionFailedEventAttributes
    { _canwefeaCause                        :: ContinueAsNewWorkflowExecutionFailedCause
    , _canwefeaDecisionTaskCompletedEventId :: Integer
    } deriving (Eq, Show)

-- | 'ContinueAsNewWorkflowExecutionFailedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'canwefeaCause' @::@ 'ContinueAsNewWorkflowExecutionFailedCause'
--
-- * 'canwefeaDecisionTaskCompletedEventId' @::@ 'Integer'
--
continueAsNewWorkflowExecutionFailedEventAttributes :: ContinueAsNewWorkflowExecutionFailedCause -- ^ 'canwefeaCause'
                                                    -> Integer -- ^ 'canwefeaDecisionTaskCompletedEventId'
                                                    -> ContinueAsNewWorkflowExecutionFailedEventAttributes
continueAsNewWorkflowExecutionFailedEventAttributes p1 p2 = ContinueAsNewWorkflowExecutionFailedEventAttributes
    { _canwefeaCause                        = p1
    , _canwefeaDecisionTaskCompletedEventId = p2
    }

-- | The cause of the failure. This information is generated by the system and can
-- be useful for diagnostic purposes.
canwefeaCause :: Lens' ContinueAsNewWorkflowExecutionFailedEventAttributes ContinueAsNewWorkflowExecutionFailedCause
canwefeaCause = lens _canwefeaCause (\s a -> s { _canwefeaCause = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'ContinueAsNewWorkflowExecution' decision that started
-- this execution. This information can be useful for diagnosing problems by
-- tracing back the cause of events.
canwefeaDecisionTaskCompletedEventId :: Lens' ContinueAsNewWorkflowExecutionFailedEventAttributes Integer
canwefeaDecisionTaskCompletedEventId =
    lens _canwefeaDecisionTaskCompletedEventId
        (\s a -> s { _canwefeaDecisionTaskCompletedEventId = a })

instance FromJSON ContinueAsNewWorkflowExecutionFailedEventAttributes where
    parseJSON = withObject "ContinueAsNewWorkflowExecutionFailedEventAttributes" $ \o -> ContinueAsNewWorkflowExecutionFailedEventAttributes
        <$> o .:  "cause"
        <*> o .:  "decisionTaskCompletedEventId"

instance ToJSON ContinueAsNewWorkflowExecutionFailedEventAttributes where
    toJSON ContinueAsNewWorkflowExecutionFailedEventAttributes{..} = object
        [ "cause"                        .= _canwefeaCause
        , "decisionTaskCompletedEventId" .= _canwefeaDecisionTaskCompletedEventId
        ]

data SignalExternalWorkflowExecutionInitiatedEventAttributes = SignalExternalWorkflowExecutionInitiatedEventAttributes
    { _seweieaControl                      :: Maybe Text
    , _seweieaDecisionTaskCompletedEventId :: Integer
    , _seweieaInput                        :: Maybe Text
    , _seweieaRunId                        :: Maybe Text
    , _seweieaSignalName                   :: Text
    , _seweieaWorkflowId                   :: Text
    } deriving (Eq, Ord, Show)

-- | 'SignalExternalWorkflowExecutionInitiatedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'seweieaControl' @::@ 'Maybe' 'Text'
--
-- * 'seweieaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'seweieaInput' @::@ 'Maybe' 'Text'
--
-- * 'seweieaRunId' @::@ 'Maybe' 'Text'
--
-- * 'seweieaSignalName' @::@ 'Text'
--
-- * 'seweieaWorkflowId' @::@ 'Text'
--
signalExternalWorkflowExecutionInitiatedEventAttributes :: Text -- ^ 'seweieaWorkflowId'
                                                        -> Text -- ^ 'seweieaSignalName'
                                                        -> Integer -- ^ 'seweieaDecisionTaskCompletedEventId'
                                                        -> SignalExternalWorkflowExecutionInitiatedEventAttributes
signalExternalWorkflowExecutionInitiatedEventAttributes p1 p2 p3 = SignalExternalWorkflowExecutionInitiatedEventAttributes
    { _seweieaWorkflowId                   = p1
    , _seweieaSignalName                   = p2
    , _seweieaDecisionTaskCompletedEventId = p3
    , _seweieaRunId                        = Nothing
    , _seweieaInput                        = Nothing
    , _seweieaControl                      = Nothing
    }

-- | Optional data attached to the event that can be used by the decider in
-- subsequent decision tasks.
seweieaControl :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
seweieaControl = lens _seweieaControl (\s a -> s { _seweieaControl = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'SignalExternalWorkflowExecution' decision for this
-- signal. This information can be useful for diagnosing problems by tracing
-- back the cause of events leading up to this event.
seweieaDecisionTaskCompletedEventId :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Integer
seweieaDecisionTaskCompletedEventId =
    lens _seweieaDecisionTaskCompletedEventId
        (\s a -> s { _seweieaDecisionTaskCompletedEventId = a })

-- | Input provided to the signal (if any).
seweieaInput :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
seweieaInput = lens _seweieaInput (\s a -> s { _seweieaInput = a })

-- | The 'runId' of the external workflow execution to send the signal to.
seweieaRunId :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Maybe Text)
seweieaRunId = lens _seweieaRunId (\s a -> s { _seweieaRunId = a })

-- | The name of the signal.
seweieaSignalName :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Text
seweieaSignalName =
    lens _seweieaSignalName (\s a -> s { _seweieaSignalName = a })

-- | The 'workflowId' of the external workflow execution.
seweieaWorkflowId :: Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Text
seweieaWorkflowId =
    lens _seweieaWorkflowId (\s a -> s { _seweieaWorkflowId = a })

instance FromJSON SignalExternalWorkflowExecutionInitiatedEventAttributes where
    parseJSON = withObject "SignalExternalWorkflowExecutionInitiatedEventAttributes" $ \o -> SignalExternalWorkflowExecutionInitiatedEventAttributes
        <$> o .:? "control"
        <*> o .:  "decisionTaskCompletedEventId"
        <*> o .:? "input"
        <*> o .:? "runId"
        <*> o .:  "signalName"
        <*> o .:  "workflowId"

instance ToJSON SignalExternalWorkflowExecutionInitiatedEventAttributes where
    toJSON SignalExternalWorkflowExecutionInitiatedEventAttributes{..} = object
        [ "workflowId"                   .= _seweieaWorkflowId
        , "runId"                        .= _seweieaRunId
        , "signalName"                   .= _seweieaSignalName
        , "input"                        .= _seweieaInput
        , "decisionTaskCompletedEventId" .= _seweieaDecisionTaskCompletedEventId
        , "control"                      .= _seweieaControl
        ]

newtype CancelTimerDecisionAttributes = CancelTimerDecisionAttributes
    { _ctdaTimerId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'CancelTimerDecisionAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctdaTimerId' @::@ 'Text'
--
cancelTimerDecisionAttributes :: Text -- ^ 'ctdaTimerId'
                              -> CancelTimerDecisionAttributes
cancelTimerDecisionAttributes p1 = CancelTimerDecisionAttributes
    { _ctdaTimerId = p1
    }

-- | The unique Id of the timer to cancel. This field is required.
ctdaTimerId :: Lens' CancelTimerDecisionAttributes Text
ctdaTimerId = lens _ctdaTimerId (\s a -> s { _ctdaTimerId = a })

instance FromJSON CancelTimerDecisionAttributes where
    parseJSON = withObject "CancelTimerDecisionAttributes" $ \o -> CancelTimerDecisionAttributes
        <$> o .:  "timerId"

instance ToJSON CancelTimerDecisionAttributes where
    toJSON CancelTimerDecisionAttributes{..} = object
        [ "timerId" .= _ctdaTimerId
        ]

data WorkflowExecutionFailedEventAttributes = WorkflowExecutionFailedEventAttributes
    { _wefeaDecisionTaskCompletedEventId :: Integer
    , _wefeaDetails                      :: Maybe Text
    , _wefeaReason                       :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'WorkflowExecutionFailedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wefeaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'wefeaDetails' @::@ 'Maybe' 'Text'
--
-- * 'wefeaReason' @::@ 'Maybe' 'Text'
--
workflowExecutionFailedEventAttributes :: Integer -- ^ 'wefeaDecisionTaskCompletedEventId'
                                       -> WorkflowExecutionFailedEventAttributes
workflowExecutionFailedEventAttributes p1 = WorkflowExecutionFailedEventAttributes
    { _wefeaDecisionTaskCompletedEventId = p1
    , _wefeaReason                       = Nothing
    , _wefeaDetails                      = Nothing
    }

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'FailWorkflowExecution' decision to fail this execution.
-- This information can be useful for diagnosing problems by tracing back the
-- cause of events.
wefeaDecisionTaskCompletedEventId :: Lens' WorkflowExecutionFailedEventAttributes Integer
wefeaDecisionTaskCompletedEventId =
    lens _wefeaDecisionTaskCompletedEventId
        (\s a -> s { _wefeaDecisionTaskCompletedEventId = a })

-- | The details of the failure (if any).
wefeaDetails :: Lens' WorkflowExecutionFailedEventAttributes (Maybe Text)
wefeaDetails = lens _wefeaDetails (\s a -> s { _wefeaDetails = a })

-- | The descriptive reason provided for the failure (if any).
wefeaReason :: Lens' WorkflowExecutionFailedEventAttributes (Maybe Text)
wefeaReason = lens _wefeaReason (\s a -> s { _wefeaReason = a })

instance FromJSON WorkflowExecutionFailedEventAttributes where
    parseJSON = withObject "WorkflowExecutionFailedEventAttributes" $ \o -> WorkflowExecutionFailedEventAttributes
        <$> o .:  "decisionTaskCompletedEventId"
        <*> o .:? "details"
        <*> o .:? "reason"

instance ToJSON WorkflowExecutionFailedEventAttributes where
    toJSON WorkflowExecutionFailedEventAttributes{..} = object
        [ "reason"                       .= _wefeaReason
        , "details"                      .= _wefeaDetails
        , "decisionTaskCompletedEventId" .= _wefeaDecisionTaskCompletedEventId
        ]

data WorkflowExecutionConfiguration = WorkflowExecutionConfiguration
    { _wecChildPolicy                  :: ChildPolicy
    , _wecExecutionStartToCloseTimeout :: Text
    , _wecTaskList                     :: TaskList
    , _wecTaskStartToCloseTimeout      :: Text
    } deriving (Eq, Show)

-- | 'WorkflowExecutionConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wecChildPolicy' @::@ 'ChildPolicy'
--
-- * 'wecExecutionStartToCloseTimeout' @::@ 'Text'
--
-- * 'wecTaskList' @::@ 'TaskList'
--
-- * 'wecTaskStartToCloseTimeout' @::@ 'Text'
--
workflowExecutionConfiguration :: Text -- ^ 'wecTaskStartToCloseTimeout'
                               -> Text -- ^ 'wecExecutionStartToCloseTimeout'
                               -> TaskList -- ^ 'wecTaskList'
                               -> ChildPolicy -- ^ 'wecChildPolicy'
                               -> WorkflowExecutionConfiguration
workflowExecutionConfiguration p1 p2 p3 p4 = WorkflowExecutionConfiguration
    { _wecTaskStartToCloseTimeout      = p1
    , _wecExecutionStartToCloseTimeout = p2
    , _wecTaskList                     = p3
    , _wecChildPolicy                  = p4
    }

-- | The policy to use for the child workflow executions if this workflow
-- execution is terminated, by calling the 'TerminateWorkflowExecution' action
-- explicitly or due to an expired timeout. The supported child policies are:
--
-- TERMINATE: the child executions will be terminated.  REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a 'WorkflowExecutionCancelRequested' event in its history. It is up to the decider to take appropriate actions
-- when it receives an execution history with this event.   ABANDON: no action
-- will be taken. The child executions will continue to run.
wecChildPolicy :: Lens' WorkflowExecutionConfiguration ChildPolicy
wecChildPolicy = lens _wecChildPolicy (\s a -> s { _wecChildPolicy = a })

-- | The total duration for this workflow execution.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
wecExecutionStartToCloseTimeout :: Lens' WorkflowExecutionConfiguration Text
wecExecutionStartToCloseTimeout =
    lens _wecExecutionStartToCloseTimeout
        (\s a -> s { _wecExecutionStartToCloseTimeout = a })

-- | The task list used for the decision tasks generated for this workflow
-- execution.
wecTaskList :: Lens' WorkflowExecutionConfiguration TaskList
wecTaskList = lens _wecTaskList (\s a -> s { _wecTaskList = a })

-- | The maximum duration allowed for decision tasks for this workflow execution.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
wecTaskStartToCloseTimeout :: Lens' WorkflowExecutionConfiguration Text
wecTaskStartToCloseTimeout =
    lens _wecTaskStartToCloseTimeout
        (\s a -> s { _wecTaskStartToCloseTimeout = a })

instance FromJSON WorkflowExecutionConfiguration where
    parseJSON = withObject "WorkflowExecutionConfiguration" $ \o -> WorkflowExecutionConfiguration
        <$> o .:  "childPolicy"
        <*> o .:  "executionStartToCloseTimeout"
        <*> o .:  "taskList"
        <*> o .:  "taskStartToCloseTimeout"

instance ToJSON WorkflowExecutionConfiguration where
    toJSON WorkflowExecutionConfiguration{..} = object
        [ "taskStartToCloseTimeout"      .= _wecTaskStartToCloseTimeout
        , "executionStartToCloseTimeout" .= _wecExecutionStartToCloseTimeout
        , "taskList"                     .= _wecTaskList
        , "childPolicy"                  .= _wecChildPolicy
        ]

data WorkflowExecution = WorkflowExecution
    { _weRunId      :: Text
    , _weWorkflowId :: Text
    } deriving (Eq, Ord, Show)

-- | 'WorkflowExecution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'weRunId' @::@ 'Text'
--
-- * 'weWorkflowId' @::@ 'Text'
--
workflowExecution :: Text -- ^ 'weWorkflowId'
                  -> Text -- ^ 'weRunId'
                  -> WorkflowExecution
workflowExecution p1 p2 = WorkflowExecution
    { _weWorkflowId = p1
    , _weRunId      = p2
    }

-- | A system generated unique identifier for the workflow execution.
weRunId :: Lens' WorkflowExecution Text
weRunId = lens _weRunId (\s a -> s { _weRunId = a })

-- | The user defined identifier associated with the workflow execution.
weWorkflowId :: Lens' WorkflowExecution Text
weWorkflowId = lens _weWorkflowId (\s a -> s { _weWorkflowId = a })

instance FromJSON WorkflowExecution where
    parseJSON = withObject "WorkflowExecution" $ \o -> WorkflowExecution
        <$> o .:  "runId"
        <*> o .:  "workflowId"

instance ToJSON WorkflowExecution where
    toJSON WorkflowExecution{..} = object
        [ "workflowId" .= _weWorkflowId
        , "runId"      .= _weRunId
        ]

data RequestCancelExternalWorkflowExecutionFailedCause
    = RCEWEFCOperationNotPermitted                              -- ^ OPERATION_NOT_PERMITTED
    | RCEWEFCRequestCancelExternalWorkflowExecutionRateExceeded -- ^ REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED
    | RCEWEFCUnknownExternalWorkflowExecution                   -- ^ UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable RequestCancelExternalWorkflowExecutionFailedCause

instance FromText RequestCancelExternalWorkflowExecutionFailedCause where
    parser = takeText >>= \case
        "OPERATION_NOT_PERMITTED"                                  -> pure RCEWEFCOperationNotPermitted
        "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED" -> pure RCEWEFCRequestCancelExternalWorkflowExecutionRateExceeded
        "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"                      -> pure RCEWEFCUnknownExternalWorkflowExecution
        e                                                          -> fail $
            "Failure parsing RequestCancelExternalWorkflowExecutionFailedCause from " ++ show e

instance ToText RequestCancelExternalWorkflowExecutionFailedCause where
    toText = \case
        RCEWEFCOperationNotPermitted                              -> "OPERATION_NOT_PERMITTED"
        RCEWEFCRequestCancelExternalWorkflowExecutionRateExceeded -> "REQUEST_CANCEL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"
        RCEWEFCUnknownExternalWorkflowExecution                   -> "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

instance ToByteString RequestCancelExternalWorkflowExecutionFailedCause
instance ToHeader     RequestCancelExternalWorkflowExecutionFailedCause
instance ToQuery      RequestCancelExternalWorkflowExecutionFailedCause

instance FromJSON RequestCancelExternalWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "RequestCancelExternalWorkflowExecutionFailedCause"

instance ToJSON RequestCancelExternalWorkflowExecutionFailedCause where
    toJSON = toJSONText

data ContinueAsNewWorkflowExecutionDecisionAttributes = ContinueAsNewWorkflowExecutionDecisionAttributes
    { _canwedaChildPolicy                  :: Maybe ChildPolicy
    , _canwedaExecutionStartToCloseTimeout :: Maybe Text
    , _canwedaInput                        :: Maybe Text
    , _canwedaTagList                      :: List "tagList" Text
    , _canwedaTaskList                     :: Maybe TaskList
    , _canwedaTaskStartToCloseTimeout      :: Maybe Text
    , _canwedaWorkflowTypeVersion          :: Maybe Text
    } deriving (Eq, Show)

-- | 'ContinueAsNewWorkflowExecutionDecisionAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'canwedaChildPolicy' @::@ 'Maybe' 'ChildPolicy'
--
-- * 'canwedaExecutionStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'canwedaInput' @::@ 'Maybe' 'Text'
--
-- * 'canwedaTagList' @::@ ['Text']
--
-- * 'canwedaTaskList' @::@ 'Maybe' 'TaskList'
--
-- * 'canwedaTaskStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'canwedaWorkflowTypeVersion' @::@ 'Maybe' 'Text'
--
continueAsNewWorkflowExecutionDecisionAttributes :: ContinueAsNewWorkflowExecutionDecisionAttributes
continueAsNewWorkflowExecutionDecisionAttributes = ContinueAsNewWorkflowExecutionDecisionAttributes
    { _canwedaInput                        = Nothing
    , _canwedaExecutionStartToCloseTimeout = Nothing
    , _canwedaTaskList                     = Nothing
    , _canwedaTaskStartToCloseTimeout      = Nothing
    , _canwedaChildPolicy                  = Nothing
    , _canwedaTagList                      = mempty
    , _canwedaWorkflowTypeVersion          = Nothing
    }

-- | If set, specifies the policy to use for the child workflow executions of the
-- new execution if it is terminated by calling the 'TerminateWorkflowExecution'
-- action explicitly or due to an expired timeout. This policy overrides the
-- default child policy specified when registering the workflow type using 'RegisterWorkflowType'. The supported child policies are:
--
-- TERMINATE: the child executions will be terminated.  REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a 'WorkflowExecutionCancelRequested' event in its history. It is up to the decider to take appropriate actions
-- when it receives an execution history with this event.   ABANDON: no action
-- will be taken. The child executions will continue to run.
canwedaChildPolicy :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe ChildPolicy)
canwedaChildPolicy =
    lens _canwedaChildPolicy (\s a -> s { _canwedaChildPolicy = a })

-- | If set, specifies the total duration for this workflow execution. This
-- overrides the 'defaultExecutionStartToCloseTimeout' specified when registering
-- the workflow type.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
canwedaExecutionStartToCloseTimeout :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaExecutionStartToCloseTimeout =
    lens _canwedaExecutionStartToCloseTimeout
        (\s a -> s { _canwedaExecutionStartToCloseTimeout = a })

-- | The input provided to the new workflow execution.
canwedaInput :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaInput = lens _canwedaInput (\s a -> s { _canwedaInput = a })

-- | The list of tags to associate with the new workflow execution. A maximum of
-- 5 tags can be specified. You can list workflow executions with a specific tag
-- by calling 'ListOpenWorkflowExecutions' or 'ListClosedWorkflowExecutions' and
-- specifying a 'TagFilter'.
canwedaTagList :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes [Text]
canwedaTagList = lens _canwedaTagList (\s a -> s { _canwedaTagList = a }) . _List

canwedaTaskList :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe TaskList)
canwedaTaskList = lens _canwedaTaskList (\s a -> s { _canwedaTaskList = a })

-- | Specifies the maximum duration of decision tasks for the new workflow
-- execution. This parameter overrides the 'defaultTaskStartToCloseTimout'
-- specified when registering the workflow type using 'RegisterWorkflowType'.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
canwedaTaskStartToCloseTimeout :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaTaskStartToCloseTimeout =
    lens _canwedaTaskStartToCloseTimeout
        (\s a -> s { _canwedaTaskStartToCloseTimeout = a })

canwedaWorkflowTypeVersion :: Lens' ContinueAsNewWorkflowExecutionDecisionAttributes (Maybe Text)
canwedaWorkflowTypeVersion =
    lens _canwedaWorkflowTypeVersion
        (\s a -> s { _canwedaWorkflowTypeVersion = a })

instance FromJSON ContinueAsNewWorkflowExecutionDecisionAttributes where
    parseJSON = withObject "ContinueAsNewWorkflowExecutionDecisionAttributes" $ \o -> ContinueAsNewWorkflowExecutionDecisionAttributes
        <$> o .:? "childPolicy"
        <*> o .:? "executionStartToCloseTimeout"
        <*> o .:? "input"
        <*> o .:? "tagList" .!= mempty
        <*> o .:? "taskList"
        <*> o .:? "taskStartToCloseTimeout"
        <*> o .:? "workflowTypeVersion"

instance ToJSON ContinueAsNewWorkflowExecutionDecisionAttributes where
    toJSON ContinueAsNewWorkflowExecutionDecisionAttributes{..} = object
        [ "input"                        .= _canwedaInput
        , "executionStartToCloseTimeout" .= _canwedaExecutionStartToCloseTimeout
        , "taskList"                     .= _canwedaTaskList
        , "taskStartToCloseTimeout"      .= _canwedaTaskStartToCloseTimeout
        , "childPolicy"                  .= _canwedaChildPolicy
        , "tagList"                      .= _canwedaTagList
        , "workflowTypeVersion"          .= _canwedaWorkflowTypeVersion
        ]

data ExternalWorkflowExecutionCancelRequestedEventAttributes = ExternalWorkflowExecutionCancelRequestedEventAttributes
    { _ewecreaInitiatedEventId  :: Integer
    , _ewecreaWorkflowExecution :: WorkflowExecution
    } deriving (Eq, Show)

-- | 'ExternalWorkflowExecutionCancelRequestedEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ewecreaInitiatedEventId' @::@ 'Integer'
--
-- * 'ewecreaWorkflowExecution' @::@ 'WorkflowExecution'
--
externalWorkflowExecutionCancelRequestedEventAttributes :: WorkflowExecution -- ^ 'ewecreaWorkflowExecution'
                                                        -> Integer -- ^ 'ewecreaInitiatedEventId'
                                                        -> ExternalWorkflowExecutionCancelRequestedEventAttributes
externalWorkflowExecutionCancelRequestedEventAttributes p1 p2 = ExternalWorkflowExecutionCancelRequestedEventAttributes
    { _ewecreaWorkflowExecution = p1
    , _ewecreaInitiatedEventId  = p2
    }

-- | The id of the 'RequestCancelExternalWorkflowExecutionInitiated' event
-- corresponding to the 'RequestCancelExternalWorkflowExecution' decision to
-- cancel this external workflow execution. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to this
-- event.
ewecreaInitiatedEventId :: Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes Integer
ewecreaInitiatedEventId =
    lens _ewecreaInitiatedEventId (\s a -> s { _ewecreaInitiatedEventId = a })

-- | The external workflow execution to which the cancellation request was
-- delivered.
ewecreaWorkflowExecution :: Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes WorkflowExecution
ewecreaWorkflowExecution =
    lens _ewecreaWorkflowExecution
        (\s a -> s { _ewecreaWorkflowExecution = a })

instance FromJSON ExternalWorkflowExecutionCancelRequestedEventAttributes where
    parseJSON = withObject "ExternalWorkflowExecutionCancelRequestedEventAttributes" $ \o -> ExternalWorkflowExecutionCancelRequestedEventAttributes
        <$> o .:  "initiatedEventId"
        <*> o .:  "workflowExecution"

instance ToJSON ExternalWorkflowExecutionCancelRequestedEventAttributes where
    toJSON ExternalWorkflowExecutionCancelRequestedEventAttributes{..} = object
        [ "workflowExecution" .= _ewecreaWorkflowExecution
        , "initiatedEventId"  .= _ewecreaInitiatedEventId
        ]

data PendingTaskCount = PendingTaskCount
    { _ptcCount     :: Nat
    , _ptcTruncated :: Maybe Bool
    } deriving (Eq, Ord, Show)

-- | 'PendingTaskCount' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptcCount' @::@ 'Natural'
--
-- * 'ptcTruncated' @::@ 'Maybe' 'Bool'
--
pendingTaskCount :: Natural -- ^ 'ptcCount'
                 -> PendingTaskCount
pendingTaskCount p1 = PendingTaskCount
    { _ptcCount     = withIso _Nat (const id) p1
    , _ptcTruncated = Nothing
    }

-- | The number of tasks in the task list.
ptcCount :: Lens' PendingTaskCount Natural
ptcCount = lens _ptcCount (\s a -> s { _ptcCount = a }) . _Nat

-- | If set to true, indicates that the actual count was more than the maximum
-- supported by this API and the count returned is the truncated value.
ptcTruncated :: Lens' PendingTaskCount (Maybe Bool)
ptcTruncated = lens _ptcTruncated (\s a -> s { _ptcTruncated = a })

instance FromJSON PendingTaskCount where
    parseJSON = withObject "PendingTaskCount" $ \o -> PendingTaskCount
        <$> o .:  "count"
        <*> o .:? "truncated"

instance ToJSON PendingTaskCount where
    toJSON PendingTaskCount{..} = object
        [ "count"     .= _ptcCount
        , "truncated" .= _ptcTruncated
        ]

data WorkflowExecutionContinuedAsNewEventAttributes = WorkflowExecutionContinuedAsNewEventAttributes
    { _wecaneaChildPolicy                  :: ChildPolicy
    , _wecaneaDecisionTaskCompletedEventId :: Integer
    , _wecaneaExecutionStartToCloseTimeout :: Maybe Text
    , _wecaneaInput                        :: Maybe Text
    , _wecaneaNewExecutionRunId            :: Text
    , _wecaneaTagList                      :: List "tagList" Text
    , _wecaneaTaskList                     :: TaskList
    , _wecaneaTaskStartToCloseTimeout      :: Maybe Text
    , _wecaneaWorkflowType                 :: WorkflowType
    } deriving (Eq, Show)

-- | 'WorkflowExecutionContinuedAsNewEventAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wecaneaChildPolicy' @::@ 'ChildPolicy'
--
-- * 'wecaneaDecisionTaskCompletedEventId' @::@ 'Integer'
--
-- * 'wecaneaExecutionStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'wecaneaInput' @::@ 'Maybe' 'Text'
--
-- * 'wecaneaNewExecutionRunId' @::@ 'Text'
--
-- * 'wecaneaTagList' @::@ ['Text']
--
-- * 'wecaneaTaskList' @::@ 'TaskList'
--
-- * 'wecaneaTaskStartToCloseTimeout' @::@ 'Maybe' 'Text'
--
-- * 'wecaneaWorkflowType' @::@ 'WorkflowType'
--
workflowExecutionContinuedAsNewEventAttributes :: Integer -- ^ 'wecaneaDecisionTaskCompletedEventId'
                                               -> Text -- ^ 'wecaneaNewExecutionRunId'
                                               -> TaskList -- ^ 'wecaneaTaskList'
                                               -> ChildPolicy -- ^ 'wecaneaChildPolicy'
                                               -> WorkflowType -- ^ 'wecaneaWorkflowType'
                                               -> WorkflowExecutionContinuedAsNewEventAttributes
workflowExecutionContinuedAsNewEventAttributes p1 p2 p3 p4 p5 = WorkflowExecutionContinuedAsNewEventAttributes
    { _wecaneaDecisionTaskCompletedEventId = p1
    , _wecaneaNewExecutionRunId            = p2
    , _wecaneaTaskList                     = p3
    , _wecaneaChildPolicy                  = p4
    , _wecaneaWorkflowType                 = p5
    , _wecaneaInput                        = Nothing
    , _wecaneaExecutionStartToCloseTimeout = Nothing
    , _wecaneaTaskStartToCloseTimeout      = Nothing
    , _wecaneaTagList                      = mempty
    }

-- | The policy to use for the child workflow executions of the new execution if
-- it is terminated by calling the 'TerminateWorkflowExecution' action explicitly
-- or due to an expired timeout.
--
-- The supported child policies are:
--
-- TERMINATE: the child executions will be terminated.  REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a 'WorkflowExecutionCancelRequested' event in its history. It is up to the decider to take appropriate actions
-- when it receives an execution history with this event.   ABANDON: no action
-- will be taken. The child executions will continue to run.
wecaneaChildPolicy :: Lens' WorkflowExecutionContinuedAsNewEventAttributes ChildPolicy
wecaneaChildPolicy =
    lens _wecaneaChildPolicy (\s a -> s { _wecaneaChildPolicy = a })

-- | The id of the 'DecisionTaskCompleted' event corresponding to the decision task
-- that resulted in the 'ContinueAsNewWorkflowExecution' decision that started
-- this execution. This information can be useful for diagnosing problems by
-- tracing back the cause of events.
wecaneaDecisionTaskCompletedEventId :: Lens' WorkflowExecutionContinuedAsNewEventAttributes Integer
wecaneaDecisionTaskCompletedEventId =
    lens _wecaneaDecisionTaskCompletedEventId
        (\s a -> s { _wecaneaDecisionTaskCompletedEventId = a })

-- | The total duration allowed for the new workflow execution.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
wecaneaExecutionStartToCloseTimeout :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaExecutionStartToCloseTimeout =
    lens _wecaneaExecutionStartToCloseTimeout
        (\s a -> s { _wecaneaExecutionStartToCloseTimeout = a })

-- | The input provided to the new workflow execution.
wecaneaInput :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaInput = lens _wecaneaInput (\s a -> s { _wecaneaInput = a })

-- | The 'runId' of the new workflow execution.
wecaneaNewExecutionRunId :: Lens' WorkflowExecutionContinuedAsNewEventAttributes Text
wecaneaNewExecutionRunId =
    lens _wecaneaNewExecutionRunId
        (\s a -> s { _wecaneaNewExecutionRunId = a })

-- | The list of tags associated with the new workflow execution.
wecaneaTagList :: Lens' WorkflowExecutionContinuedAsNewEventAttributes [Text]
wecaneaTagList = lens _wecaneaTagList (\s a -> s { _wecaneaTagList = a }) . _List

wecaneaTaskList :: Lens' WorkflowExecutionContinuedAsNewEventAttributes TaskList
wecaneaTaskList = lens _wecaneaTaskList (\s a -> s { _wecaneaTaskList = a })

-- | The maximum duration of decision tasks for the new workflow execution.
--
-- The valid values are integers greater than or equal to '0'. An integer value
-- can be used to specify the duration in seconds while 'NONE' can be used to
-- specify unlimited duration.
wecaneaTaskStartToCloseTimeout :: Lens' WorkflowExecutionContinuedAsNewEventAttributes (Maybe Text)
wecaneaTaskStartToCloseTimeout =
    lens _wecaneaTaskStartToCloseTimeout
        (\s a -> s { _wecaneaTaskStartToCloseTimeout = a })

wecaneaWorkflowType :: Lens' WorkflowExecutionContinuedAsNewEventAttributes WorkflowType
wecaneaWorkflowType =
    lens _wecaneaWorkflowType (\s a -> s { _wecaneaWorkflowType = a })

instance FromJSON WorkflowExecutionContinuedAsNewEventAttributes where
    parseJSON = withObject "WorkflowExecutionContinuedAsNewEventAttributes" $ \o -> WorkflowExecutionContinuedAsNewEventAttributes
        <$> o .:  "childPolicy"
        <*> o .:  "decisionTaskCompletedEventId"
        <*> o .:? "executionStartToCloseTimeout"
        <*> o .:? "input"
        <*> o .:  "newExecutionRunId"
        <*> o .:? "tagList" .!= mempty
        <*> o .:  "taskList"
        <*> o .:? "taskStartToCloseTimeout"
        <*> o .:  "workflowType"

instance ToJSON WorkflowExecutionContinuedAsNewEventAttributes where
    toJSON WorkflowExecutionContinuedAsNewEventAttributes{..} = object
        [ "input"                        .= _wecaneaInput
        , "decisionTaskCompletedEventId" .= _wecaneaDecisionTaskCompletedEventId
        , "newExecutionRunId"            .= _wecaneaNewExecutionRunId
        , "executionStartToCloseTimeout" .= _wecaneaExecutionStartToCloseTimeout
        , "taskList"                     .= _wecaneaTaskList
        , "taskStartToCloseTimeout"      .= _wecaneaTaskStartToCloseTimeout
        , "childPolicy"                  .= _wecaneaChildPolicy
        , "tagList"                      .= _wecaneaTagList
        , "workflowType"                 .= _wecaneaWorkflowType
        ]
