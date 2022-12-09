{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTEventsData.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Lens
  ( -- * Operations

    -- ** BatchAcknowledgeAlarm
    batchAcknowledgeAlarm_acknowledgeActionRequests,
    batchAcknowledgeAlarmResponse_errorEntries,
    batchAcknowledgeAlarmResponse_httpStatus,

    -- ** BatchDeleteDetector
    batchDeleteDetector_detectors,
    batchDeleteDetectorResponse_batchDeleteDetectorErrorEntries,
    batchDeleteDetectorResponse_httpStatus,

    -- ** BatchDisableAlarm
    batchDisableAlarm_disableActionRequests,
    batchDisableAlarmResponse_errorEntries,
    batchDisableAlarmResponse_httpStatus,

    -- ** BatchEnableAlarm
    batchEnableAlarm_enableActionRequests,
    batchEnableAlarmResponse_errorEntries,
    batchEnableAlarmResponse_httpStatus,

    -- ** BatchPutMessage
    batchPutMessage_messages,
    batchPutMessageResponse_batchPutMessageErrorEntries,
    batchPutMessageResponse_httpStatus,

    -- ** BatchResetAlarm
    batchResetAlarm_resetActionRequests,
    batchResetAlarmResponse_errorEntries,
    batchResetAlarmResponse_httpStatus,

    -- ** BatchSnoozeAlarm
    batchSnoozeAlarm_snoozeActionRequests,
    batchSnoozeAlarmResponse_errorEntries,
    batchSnoozeAlarmResponse_httpStatus,

    -- ** BatchUpdateDetector
    batchUpdateDetector_detectors,
    batchUpdateDetectorResponse_batchUpdateDetectorErrorEntries,
    batchUpdateDetectorResponse_httpStatus,

    -- ** DescribeAlarm
    describeAlarm_keyValue,
    describeAlarm_alarmModelName,
    describeAlarmResponse_alarm,
    describeAlarmResponse_httpStatus,

    -- ** DescribeDetector
    describeDetector_keyValue,
    describeDetector_detectorModelName,
    describeDetectorResponse_detector,
    describeDetectorResponse_httpStatus,

    -- ** ListAlarms
    listAlarms_maxResults,
    listAlarms_nextToken,
    listAlarms_alarmModelName,
    listAlarmsResponse_alarmSummaries,
    listAlarmsResponse_nextToken,
    listAlarmsResponse_httpStatus,

    -- ** ListDetectors
    listDetectors_maxResults,
    listDetectors_nextToken,
    listDetectors_stateName,
    listDetectors_detectorModelName,
    listDetectorsResponse_detectorSummaries,
    listDetectorsResponse_nextToken,
    listDetectorsResponse_httpStatus,

    -- * Types

    -- ** AcknowledgeActionConfiguration
    acknowledgeActionConfiguration_note,

    -- ** AcknowledgeAlarmActionRequest
    acknowledgeAlarmActionRequest_keyValue,
    acknowledgeAlarmActionRequest_note,
    acknowledgeAlarmActionRequest_requestId,
    acknowledgeAlarmActionRequest_alarmModelName,

    -- ** Alarm
    alarm_alarmModelName,
    alarm_alarmModelVersion,
    alarm_alarmState,
    alarm_creationTime,
    alarm_keyValue,
    alarm_lastUpdateTime,
    alarm_severity,

    -- ** AlarmState
    alarmState_customerAction,
    alarmState_ruleEvaluation,
    alarmState_stateName,
    alarmState_systemEvent,

    -- ** AlarmSummary
    alarmSummary_alarmModelName,
    alarmSummary_alarmModelVersion,
    alarmSummary_creationTime,
    alarmSummary_keyValue,
    alarmSummary_lastUpdateTime,
    alarmSummary_stateName,

    -- ** BatchAlarmActionErrorEntry
    batchAlarmActionErrorEntry_errorCode,
    batchAlarmActionErrorEntry_errorMessage,
    batchAlarmActionErrorEntry_requestId,

    -- ** BatchDeleteDetectorErrorEntry
    batchDeleteDetectorErrorEntry_errorCode,
    batchDeleteDetectorErrorEntry_errorMessage,
    batchDeleteDetectorErrorEntry_messageId,

    -- ** BatchPutMessageErrorEntry
    batchPutMessageErrorEntry_errorCode,
    batchPutMessageErrorEntry_errorMessage,
    batchPutMessageErrorEntry_messageId,

    -- ** BatchUpdateDetectorErrorEntry
    batchUpdateDetectorErrorEntry_errorCode,
    batchUpdateDetectorErrorEntry_errorMessage,
    batchUpdateDetectorErrorEntry_messageId,

    -- ** CustomerAction
    customerAction_acknowledgeActionConfiguration,
    customerAction_actionName,
    customerAction_disableActionConfiguration,
    customerAction_enableActionConfiguration,
    customerAction_resetActionConfiguration,
    customerAction_snoozeActionConfiguration,

    -- ** DeleteDetectorRequest
    deleteDetectorRequest_keyValue,
    deleteDetectorRequest_messageId,
    deleteDetectorRequest_detectorModelName,

    -- ** Detector
    detector_creationTime,
    detector_detectorModelName,
    detector_detectorModelVersion,
    detector_keyValue,
    detector_lastUpdateTime,
    detector_state,

    -- ** DetectorState
    detectorState_stateName,
    detectorState_variables,
    detectorState_timers,

    -- ** DetectorStateDefinition
    detectorStateDefinition_stateName,
    detectorStateDefinition_variables,
    detectorStateDefinition_timers,

    -- ** DetectorStateSummary
    detectorStateSummary_stateName,

    -- ** DetectorSummary
    detectorSummary_creationTime,
    detectorSummary_detectorModelName,
    detectorSummary_detectorModelVersion,
    detectorSummary_keyValue,
    detectorSummary_lastUpdateTime,
    detectorSummary_state,

    -- ** DisableActionConfiguration
    disableActionConfiguration_note,

    -- ** DisableAlarmActionRequest
    disableAlarmActionRequest_keyValue,
    disableAlarmActionRequest_note,
    disableAlarmActionRequest_requestId,
    disableAlarmActionRequest_alarmModelName,

    -- ** EnableActionConfiguration
    enableActionConfiguration_note,

    -- ** EnableAlarmActionRequest
    enableAlarmActionRequest_keyValue,
    enableAlarmActionRequest_note,
    enableAlarmActionRequest_requestId,
    enableAlarmActionRequest_alarmModelName,

    -- ** Message
    message_timestamp,
    message_messageId,
    message_inputName,
    message_payload,

    -- ** ResetActionConfiguration
    resetActionConfiguration_note,

    -- ** ResetAlarmActionRequest
    resetAlarmActionRequest_keyValue,
    resetAlarmActionRequest_note,
    resetAlarmActionRequest_requestId,
    resetAlarmActionRequest_alarmModelName,

    -- ** RuleEvaluation
    ruleEvaluation_simpleRuleEvaluation,

    -- ** SimpleRuleEvaluation
    simpleRuleEvaluation_inputPropertyValue,
    simpleRuleEvaluation_operator,
    simpleRuleEvaluation_thresholdValue,

    -- ** SnoozeActionConfiguration
    snoozeActionConfiguration_note,
    snoozeActionConfiguration_snoozeDuration,

    -- ** SnoozeAlarmActionRequest
    snoozeAlarmActionRequest_keyValue,
    snoozeAlarmActionRequest_note,
    snoozeAlarmActionRequest_requestId,
    snoozeAlarmActionRequest_alarmModelName,
    snoozeAlarmActionRequest_snoozeDuration,

    -- ** StateChangeConfiguration
    stateChangeConfiguration_triggerType,

    -- ** SystemEvent
    systemEvent_eventType,
    systemEvent_stateChangeConfiguration,

    -- ** Timer
    timer_name,
    timer_timestamp,

    -- ** TimerDefinition
    timerDefinition_name,
    timerDefinition_seconds,

    -- ** TimestampValue
    timestampValue_timeInMillis,

    -- ** UpdateDetectorRequest
    updateDetectorRequest_keyValue,
    updateDetectorRequest_messageId,
    updateDetectorRequest_detectorModelName,
    updateDetectorRequest_state,

    -- ** Variable
    variable_name,
    variable_value,

    -- ** VariableDefinition
    variableDefinition_name,
    variableDefinition_value,
  )
where

import Amazonka.IoTEventsData.BatchAcknowledgeAlarm
import Amazonka.IoTEventsData.BatchDeleteDetector
import Amazonka.IoTEventsData.BatchDisableAlarm
import Amazonka.IoTEventsData.BatchEnableAlarm
import Amazonka.IoTEventsData.BatchPutMessage
import Amazonka.IoTEventsData.BatchResetAlarm
import Amazonka.IoTEventsData.BatchSnoozeAlarm
import Amazonka.IoTEventsData.BatchUpdateDetector
import Amazonka.IoTEventsData.DescribeAlarm
import Amazonka.IoTEventsData.DescribeDetector
import Amazonka.IoTEventsData.ListAlarms
import Amazonka.IoTEventsData.ListDetectors
import Amazonka.IoTEventsData.Types.AcknowledgeActionConfiguration
import Amazonka.IoTEventsData.Types.AcknowledgeAlarmActionRequest
import Amazonka.IoTEventsData.Types.Alarm
import Amazonka.IoTEventsData.Types.AlarmState
import Amazonka.IoTEventsData.Types.AlarmSummary
import Amazonka.IoTEventsData.Types.BatchAlarmActionErrorEntry
import Amazonka.IoTEventsData.Types.BatchDeleteDetectorErrorEntry
import Amazonka.IoTEventsData.Types.BatchPutMessageErrorEntry
import Amazonka.IoTEventsData.Types.BatchUpdateDetectorErrorEntry
import Amazonka.IoTEventsData.Types.CustomerAction
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
import Amazonka.IoTEventsData.Types.UpdateDetectorRequest
import Amazonka.IoTEventsData.Types.Variable
import Amazonka.IoTEventsData.Types.VariableDefinition
