{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTEventsData.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTEventsData.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidRequestException,
    _ThrottlingException,
    _InternalFailureException,
    _ServiceUnavailableException,
    _ResourceNotFoundException,

    -- * AlarmStateName
    AlarmStateName (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * CustomerActionName
    CustomerActionName (..),

    -- * ErrorCode
    ErrorCode (..),

    -- * EventType
    EventType (..),

    -- * TriggerType
    TriggerType (..),

    -- * AcknowledgeActionConfiguration
    AcknowledgeActionConfiguration (..),
    newAcknowledgeActionConfiguration,
    acknowledgeActionConfiguration_note,

    -- * AcknowledgeAlarmActionRequest
    AcknowledgeAlarmActionRequest (..),
    newAcknowledgeAlarmActionRequest,
    acknowledgeAlarmActionRequest_keyValue,
    acknowledgeAlarmActionRequest_note,
    acknowledgeAlarmActionRequest_requestId,
    acknowledgeAlarmActionRequest_alarmModelName,

    -- * Alarm
    Alarm (..),
    newAlarm,
    alarm_keyValue,
    alarm_creationTime,
    alarm_alarmState,
    alarm_alarmModelName,
    alarm_severity,
    alarm_lastUpdateTime,
    alarm_alarmModelVersion,

    -- * AlarmState
    AlarmState (..),
    newAlarmState,
    alarmState_customerAction,
    alarmState_stateName,
    alarmState_ruleEvaluation,
    alarmState_systemEvent,

    -- * AlarmSummary
    AlarmSummary (..),
    newAlarmSummary,
    alarmSummary_keyValue,
    alarmSummary_creationTime,
    alarmSummary_alarmModelName,
    alarmSummary_stateName,
    alarmSummary_lastUpdateTime,
    alarmSummary_alarmModelVersion,

    -- * BatchAlarmActionErrorEntry
    BatchAlarmActionErrorEntry (..),
    newBatchAlarmActionErrorEntry,
    batchAlarmActionErrorEntry_requestId,
    batchAlarmActionErrorEntry_errorCode,
    batchAlarmActionErrorEntry_errorMessage,

    -- * BatchPutMessageErrorEntry
    BatchPutMessageErrorEntry (..),
    newBatchPutMessageErrorEntry,
    batchPutMessageErrorEntry_errorCode,
    batchPutMessageErrorEntry_errorMessage,
    batchPutMessageErrorEntry_messageId,

    -- * BatchUpdateDetectorErrorEntry
    BatchUpdateDetectorErrorEntry (..),
    newBatchUpdateDetectorErrorEntry,
    batchUpdateDetectorErrorEntry_errorCode,
    batchUpdateDetectorErrorEntry_errorMessage,
    batchUpdateDetectorErrorEntry_messageId,

    -- * CustomerAction
    CustomerAction (..),
    newCustomerAction,
    customerAction_resetActionConfiguration,
    customerAction_actionName,
    customerAction_snoozeActionConfiguration,
    customerAction_enableActionConfiguration,
    customerAction_disableActionConfiguration,
    customerAction_acknowledgeActionConfiguration,

    -- * Detector
    Detector (..),
    newDetector,
    detector_keyValue,
    detector_creationTime,
    detector_state,
    detector_detectorModelName,
    detector_detectorModelVersion,
    detector_lastUpdateTime,

    -- * DetectorState
    DetectorState (..),
    newDetectorState,
    detectorState_stateName,
    detectorState_variables,
    detectorState_timers,

    -- * DetectorStateDefinition
    DetectorStateDefinition (..),
    newDetectorStateDefinition,
    detectorStateDefinition_stateName,
    detectorStateDefinition_variables,
    detectorStateDefinition_timers,

    -- * DetectorStateSummary
    DetectorStateSummary (..),
    newDetectorStateSummary,
    detectorStateSummary_stateName,

    -- * DetectorSummary
    DetectorSummary (..),
    newDetectorSummary,
    detectorSummary_keyValue,
    detectorSummary_creationTime,
    detectorSummary_state,
    detectorSummary_detectorModelName,
    detectorSummary_detectorModelVersion,
    detectorSummary_lastUpdateTime,

    -- * DisableActionConfiguration
    DisableActionConfiguration (..),
    newDisableActionConfiguration,
    disableActionConfiguration_note,

    -- * DisableAlarmActionRequest
    DisableAlarmActionRequest (..),
    newDisableAlarmActionRequest,
    disableAlarmActionRequest_keyValue,
    disableAlarmActionRequest_note,
    disableAlarmActionRequest_requestId,
    disableAlarmActionRequest_alarmModelName,

    -- * EnableActionConfiguration
    EnableActionConfiguration (..),
    newEnableActionConfiguration,
    enableActionConfiguration_note,

    -- * EnableAlarmActionRequest
    EnableAlarmActionRequest (..),
    newEnableAlarmActionRequest,
    enableAlarmActionRequest_keyValue,
    enableAlarmActionRequest_note,
    enableAlarmActionRequest_requestId,
    enableAlarmActionRequest_alarmModelName,

    -- * Message
    Message (..),
    newMessage,
    message_timestamp,
    message_messageId,
    message_inputName,
    message_payload,

    -- * ResetActionConfiguration
    ResetActionConfiguration (..),
    newResetActionConfiguration,
    resetActionConfiguration_note,

    -- * ResetAlarmActionRequest
    ResetAlarmActionRequest (..),
    newResetAlarmActionRequest,
    resetAlarmActionRequest_keyValue,
    resetAlarmActionRequest_note,
    resetAlarmActionRequest_requestId,
    resetAlarmActionRequest_alarmModelName,

    -- * RuleEvaluation
    RuleEvaluation (..),
    newRuleEvaluation,
    ruleEvaluation_simpleRuleEvaluation,

    -- * SimpleRuleEvaluation
    SimpleRuleEvaluation (..),
    newSimpleRuleEvaluation,
    simpleRuleEvaluation_thresholdValue,
    simpleRuleEvaluation_inputPropertyValue,
    simpleRuleEvaluation_operator,

    -- * SnoozeActionConfiguration
    SnoozeActionConfiguration (..),
    newSnoozeActionConfiguration,
    snoozeActionConfiguration_note,
    snoozeActionConfiguration_snoozeDuration,

    -- * SnoozeAlarmActionRequest
    SnoozeAlarmActionRequest (..),
    newSnoozeAlarmActionRequest,
    snoozeAlarmActionRequest_keyValue,
    snoozeAlarmActionRequest_note,
    snoozeAlarmActionRequest_requestId,
    snoozeAlarmActionRequest_alarmModelName,
    snoozeAlarmActionRequest_snoozeDuration,

    -- * StateChangeConfiguration
    StateChangeConfiguration (..),
    newStateChangeConfiguration,
    stateChangeConfiguration_triggerType,

    -- * SystemEvent
    SystemEvent (..),
    newSystemEvent,
    systemEvent_eventType,
    systemEvent_stateChangeConfiguration,

    -- * Timer
    Timer (..),
    newTimer,
    timer_name,
    timer_timestamp,

    -- * TimerDefinition
    TimerDefinition (..),
    newTimerDefinition,
    timerDefinition_name,
    timerDefinition_seconds,

    -- * TimestampValue
    TimestampValue (..),
    newTimestampValue,
    timestampValue_timeInMillis,

    -- * UpdateDetectorRequest
    UpdateDetectorRequest (..),
    newUpdateDetectorRequest,
    updateDetectorRequest_keyValue,
    updateDetectorRequest_messageId,
    updateDetectorRequest_detectorModelName,
    updateDetectorRequest_state,

    -- * Variable
    Variable (..),
    newVariable,
    variable_name,
    variable_value,

    -- * VariableDefinition
    VariableDefinition (..),
    newVariableDefinition,
    variableDefinition_name,
    variableDefinition_value,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTEventsData.Types.AcknowledgeActionConfiguration
import Network.AWS.IoTEventsData.Types.AcknowledgeAlarmActionRequest
import Network.AWS.IoTEventsData.Types.Alarm
import Network.AWS.IoTEventsData.Types.AlarmState
import Network.AWS.IoTEventsData.Types.AlarmStateName
import Network.AWS.IoTEventsData.Types.AlarmSummary
import Network.AWS.IoTEventsData.Types.BatchAlarmActionErrorEntry
import Network.AWS.IoTEventsData.Types.BatchPutMessageErrorEntry
import Network.AWS.IoTEventsData.Types.BatchUpdateDetectorErrorEntry
import Network.AWS.IoTEventsData.Types.ComparisonOperator
import Network.AWS.IoTEventsData.Types.CustomerAction
import Network.AWS.IoTEventsData.Types.CustomerActionName
import Network.AWS.IoTEventsData.Types.Detector
import Network.AWS.IoTEventsData.Types.DetectorState
import Network.AWS.IoTEventsData.Types.DetectorStateDefinition
import Network.AWS.IoTEventsData.Types.DetectorStateSummary
import Network.AWS.IoTEventsData.Types.DetectorSummary
import Network.AWS.IoTEventsData.Types.DisableActionConfiguration
import Network.AWS.IoTEventsData.Types.DisableAlarmActionRequest
import Network.AWS.IoTEventsData.Types.EnableActionConfiguration
import Network.AWS.IoTEventsData.Types.EnableAlarmActionRequest
import Network.AWS.IoTEventsData.Types.ErrorCode
import Network.AWS.IoTEventsData.Types.EventType
import Network.AWS.IoTEventsData.Types.Message
import Network.AWS.IoTEventsData.Types.ResetActionConfiguration
import Network.AWS.IoTEventsData.Types.ResetAlarmActionRequest
import Network.AWS.IoTEventsData.Types.RuleEvaluation
import Network.AWS.IoTEventsData.Types.SimpleRuleEvaluation
import Network.AWS.IoTEventsData.Types.SnoozeActionConfiguration
import Network.AWS.IoTEventsData.Types.SnoozeAlarmActionRequest
import Network.AWS.IoTEventsData.Types.StateChangeConfiguration
import Network.AWS.IoTEventsData.Types.SystemEvent
import Network.AWS.IoTEventsData.Types.Timer
import Network.AWS.IoTEventsData.Types.TimerDefinition
import Network.AWS.IoTEventsData.Types.TimestampValue
import Network.AWS.IoTEventsData.Types.TriggerType
import Network.AWS.IoTEventsData.Types.UpdateDetectorRequest
import Network.AWS.IoTEventsData.Types.Variable
import Network.AWS.IoTEventsData.Types.VariableDefinition
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-10-23@ of the Amazon IoT Events Data SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "IoTEventsData",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "data.iotevents",
      Core._serviceSigningName = "ioteventsdata",
      Core._serviceVersion = "2018-10-23",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "IoTEventsData",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The request was invalid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | The request could not be completed due to throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | An internal failure occurred.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500

-- | The service is currently unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
