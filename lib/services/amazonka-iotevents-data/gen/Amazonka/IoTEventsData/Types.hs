{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTEventsData.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ServiceUnavailableException,
    _ResourceNotFoundException,
    _ThrottlingException,
    _InvalidRequestException,
    _InternalFailureException,

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
    alarm_alarmModelName,
    alarm_alarmState,
    alarm_severity,
    alarm_alarmModelVersion,
    alarm_keyValue,
    alarm_creationTime,
    alarm_lastUpdateTime,

    -- * AlarmState
    AlarmState (..),
    newAlarmState,
    alarmState_ruleEvaluation,
    alarmState_stateName,
    alarmState_systemEvent,
    alarmState_customerAction,

    -- * AlarmSummary
    AlarmSummary (..),
    newAlarmSummary,
    alarmSummary_alarmModelName,
    alarmSummary_alarmModelVersion,
    alarmSummary_stateName,
    alarmSummary_keyValue,
    alarmSummary_creationTime,
    alarmSummary_lastUpdateTime,

    -- * BatchAlarmActionErrorEntry
    BatchAlarmActionErrorEntry (..),
    newBatchAlarmActionErrorEntry,
    batchAlarmActionErrorEntry_errorMessage,
    batchAlarmActionErrorEntry_requestId,
    batchAlarmActionErrorEntry_errorCode,

    -- * BatchDeleteDetectorErrorEntry
    BatchDeleteDetectorErrorEntry (..),
    newBatchDeleteDetectorErrorEntry,
    batchDeleteDetectorErrorEntry_errorMessage,
    batchDeleteDetectorErrorEntry_messageId,
    batchDeleteDetectorErrorEntry_errorCode,

    -- * BatchPutMessageErrorEntry
    BatchPutMessageErrorEntry (..),
    newBatchPutMessageErrorEntry,
    batchPutMessageErrorEntry_errorMessage,
    batchPutMessageErrorEntry_messageId,
    batchPutMessageErrorEntry_errorCode,

    -- * BatchUpdateDetectorErrorEntry
    BatchUpdateDetectorErrorEntry (..),
    newBatchUpdateDetectorErrorEntry,
    batchUpdateDetectorErrorEntry_errorMessage,
    batchUpdateDetectorErrorEntry_messageId,
    batchUpdateDetectorErrorEntry_errorCode,

    -- * CustomerAction
    CustomerAction (..),
    newCustomerAction,
    customerAction_resetActionConfiguration,
    customerAction_actionName,
    customerAction_enableActionConfiguration,
    customerAction_disableActionConfiguration,
    customerAction_snoozeActionConfiguration,
    customerAction_acknowledgeActionConfiguration,

    -- * DeleteDetectorRequest
    DeleteDetectorRequest (..),
    newDeleteDetectorRequest,
    deleteDetectorRequest_keyValue,
    deleteDetectorRequest_messageId,
    deleteDetectorRequest_detectorModelName,

    -- * Detector
    Detector (..),
    newDetector,
    detector_state,
    detector_detectorModelName,
    detector_detectorModelVersion,
    detector_keyValue,
    detector_creationTime,
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
    detectorSummary_state,
    detectorSummary_detectorModelName,
    detectorSummary_detectorModelVersion,
    detectorSummary_keyValue,
    detectorSummary_creationTime,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEventsData.Types.AcknowledgeActionConfiguration
import Amazonka.IoTEventsData.Types.AcknowledgeAlarmActionRequest
import Amazonka.IoTEventsData.Types.Alarm
import Amazonka.IoTEventsData.Types.AlarmState
import Amazonka.IoTEventsData.Types.AlarmStateName
import Amazonka.IoTEventsData.Types.AlarmSummary
import Amazonka.IoTEventsData.Types.BatchAlarmActionErrorEntry
import Amazonka.IoTEventsData.Types.BatchDeleteDetectorErrorEntry
import Amazonka.IoTEventsData.Types.BatchPutMessageErrorEntry
import Amazonka.IoTEventsData.Types.BatchUpdateDetectorErrorEntry
import Amazonka.IoTEventsData.Types.ComparisonOperator
import Amazonka.IoTEventsData.Types.CustomerAction
import Amazonka.IoTEventsData.Types.CustomerActionName
import Amazonka.IoTEventsData.Types.DeleteDetectorRequest
import Amazonka.IoTEventsData.Types.Detector
import Amazonka.IoTEventsData.Types.DetectorState
import Amazonka.IoTEventsData.Types.DetectorStateDefinition
import Amazonka.IoTEventsData.Types.DetectorStateSummary
import Amazonka.IoTEventsData.Types.DetectorSummary
import Amazonka.IoTEventsData.Types.DisableActionConfiguration
import Amazonka.IoTEventsData.Types.DisableAlarmActionRequest
import Amazonka.IoTEventsData.Types.EnableActionConfiguration
import Amazonka.IoTEventsData.Types.EnableAlarmActionRequest
import Amazonka.IoTEventsData.Types.ErrorCode
import Amazonka.IoTEventsData.Types.EventType
import Amazonka.IoTEventsData.Types.Message
import Amazonka.IoTEventsData.Types.ResetActionConfiguration
import Amazonka.IoTEventsData.Types.ResetAlarmActionRequest
import Amazonka.IoTEventsData.Types.RuleEvaluation
import Amazonka.IoTEventsData.Types.SimpleRuleEvaluation
import Amazonka.IoTEventsData.Types.SnoozeActionConfiguration
import Amazonka.IoTEventsData.Types.SnoozeAlarmActionRequest
import Amazonka.IoTEventsData.Types.StateChangeConfiguration
import Amazonka.IoTEventsData.Types.SystemEvent
import Amazonka.IoTEventsData.Types.Timer
import Amazonka.IoTEventsData.Types.TimerDefinition
import Amazonka.IoTEventsData.Types.TimestampValue
import Amazonka.IoTEventsData.Types.TriggerType
import Amazonka.IoTEventsData.Types.UpdateDetectorRequest
import Amazonka.IoTEventsData.Types.Variable
import Amazonka.IoTEventsData.Types.VariableDefinition
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-10-23@ of the Amazon IoT Events Data SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "IoTEventsData",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "data.iotevents",
      Core.signingName = "ioteventsdata",
      Core.version = "2018-10-23",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "IoTEventsData",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

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

-- | The request could not be completed due to throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The request was invalid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | An internal failure occurred.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500
