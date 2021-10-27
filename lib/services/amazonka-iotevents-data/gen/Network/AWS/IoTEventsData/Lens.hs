{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTEventsData.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTEventsData.Lens
  ( -- * Operations

    -- ** BatchSnoozeAlarm
    batchSnoozeAlarm_snoozeActionRequests,
    batchSnoozeAlarmResponse_errorEntries,
    batchSnoozeAlarmResponse_httpStatus,

    -- ** BatchDisableAlarm
    batchDisableAlarm_disableActionRequests,
    batchDisableAlarmResponse_errorEntries,
    batchDisableAlarmResponse_httpStatus,

    -- ** DescribeAlarm
    describeAlarm_keyValue,
    describeAlarm_alarmModelName,
    describeAlarmResponse_alarm,
    describeAlarmResponse_httpStatus,

    -- ** BatchPutMessage
    batchPutMessage_messages,
    batchPutMessageResponse_batchPutMessageErrorEntries,
    batchPutMessageResponse_httpStatus,

    -- ** DescribeDetector
    describeDetector_keyValue,
    describeDetector_detectorModelName,
    describeDetectorResponse_detector,
    describeDetectorResponse_httpStatus,

    -- ** BatchUpdateDetector
    batchUpdateDetector_detectors,
    batchUpdateDetectorResponse_batchUpdateDetectorErrorEntries,
    batchUpdateDetectorResponse_httpStatus,

    -- ** BatchAcknowledgeAlarm
    batchAcknowledgeAlarm_acknowledgeActionRequests,
    batchAcknowledgeAlarmResponse_errorEntries,
    batchAcknowledgeAlarmResponse_httpStatus,

    -- ** ListAlarms
    listAlarms_nextToken,
    listAlarms_maxResults,
    listAlarms_alarmModelName,
    listAlarmsResponse_nextToken,
    listAlarmsResponse_alarmSummaries,
    listAlarmsResponse_httpStatus,

    -- ** BatchResetAlarm
    batchResetAlarm_resetActionRequests,
    batchResetAlarmResponse_errorEntries,
    batchResetAlarmResponse_httpStatus,

    -- ** ListDetectors
    listDetectors_nextToken,
    listDetectors_stateName,
    listDetectors_maxResults,
    listDetectors_detectorModelName,
    listDetectorsResponse_nextToken,
    listDetectorsResponse_detectorSummaries,
    listDetectorsResponse_httpStatus,

    -- ** BatchEnableAlarm
    batchEnableAlarm_enableActionRequests,
    batchEnableAlarmResponse_errorEntries,
    batchEnableAlarmResponse_httpStatus,

    -- * Types

    -- ** AcknowledgeActionConfiguration
    acknowledgeActionConfiguration_note,

    -- ** AcknowledgeAlarmActionRequest
    acknowledgeAlarmActionRequest_keyValue,
    acknowledgeAlarmActionRequest_note,
    acknowledgeAlarmActionRequest_requestId,
    acknowledgeAlarmActionRequest_alarmModelName,

    -- ** Alarm
    alarm_keyValue,
    alarm_creationTime,
    alarm_alarmState,
    alarm_alarmModelName,
    alarm_severity,
    alarm_lastUpdateTime,
    alarm_alarmModelVersion,

    -- ** AlarmState
    alarmState_customerAction,
    alarmState_stateName,
    alarmState_ruleEvaluation,
    alarmState_systemEvent,

    -- ** AlarmSummary
    alarmSummary_keyValue,
    alarmSummary_creationTime,
    alarmSummary_alarmModelName,
    alarmSummary_stateName,
    alarmSummary_lastUpdateTime,
    alarmSummary_alarmModelVersion,

    -- ** BatchAlarmActionErrorEntry
    batchAlarmActionErrorEntry_requestId,
    batchAlarmActionErrorEntry_errorCode,
    batchAlarmActionErrorEntry_errorMessage,

    -- ** BatchPutMessageErrorEntry
    batchPutMessageErrorEntry_errorCode,
    batchPutMessageErrorEntry_errorMessage,
    batchPutMessageErrorEntry_messageId,

    -- ** BatchUpdateDetectorErrorEntry
    batchUpdateDetectorErrorEntry_errorCode,
    batchUpdateDetectorErrorEntry_errorMessage,
    batchUpdateDetectorErrorEntry_messageId,

    -- ** CustomerAction
    customerAction_resetActionConfiguration,
    customerAction_actionName,
    customerAction_snoozeActionConfiguration,
    customerAction_enableActionConfiguration,
    customerAction_disableActionConfiguration,
    customerAction_acknowledgeActionConfiguration,

    -- ** Detector
    detector_keyValue,
    detector_creationTime,
    detector_state,
    detector_detectorModelName,
    detector_detectorModelVersion,
    detector_lastUpdateTime,

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
    detectorSummary_keyValue,
    detectorSummary_creationTime,
    detectorSummary_state,
    detectorSummary_detectorModelName,
    detectorSummary_detectorModelVersion,
    detectorSummary_lastUpdateTime,

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
    simpleRuleEvaluation_thresholdValue,
    simpleRuleEvaluation_inputPropertyValue,
    simpleRuleEvaluation_operator,

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

import Network.AWS.IoTEventsData.BatchAcknowledgeAlarm
import Network.AWS.IoTEventsData.BatchDisableAlarm
import Network.AWS.IoTEventsData.BatchEnableAlarm
import Network.AWS.IoTEventsData.BatchPutMessage
import Network.AWS.IoTEventsData.BatchResetAlarm
import Network.AWS.IoTEventsData.BatchSnoozeAlarm
import Network.AWS.IoTEventsData.BatchUpdateDetector
import Network.AWS.IoTEventsData.DescribeAlarm
import Network.AWS.IoTEventsData.DescribeDetector
import Network.AWS.IoTEventsData.ListAlarms
import Network.AWS.IoTEventsData.ListDetectors
import Network.AWS.IoTEventsData.Types.AcknowledgeActionConfiguration
import Network.AWS.IoTEventsData.Types.AcknowledgeAlarmActionRequest
import Network.AWS.IoTEventsData.Types.Alarm
import Network.AWS.IoTEventsData.Types.AlarmState
import Network.AWS.IoTEventsData.Types.AlarmSummary
import Network.AWS.IoTEventsData.Types.BatchAlarmActionErrorEntry
import Network.AWS.IoTEventsData.Types.BatchPutMessageErrorEntry
import Network.AWS.IoTEventsData.Types.BatchUpdateDetectorErrorEntry
import Network.AWS.IoTEventsData.Types.CustomerAction
import Network.AWS.IoTEventsData.Types.Detector
import Network.AWS.IoTEventsData.Types.DetectorState
import Network.AWS.IoTEventsData.Types.DetectorStateDefinition
import Network.AWS.IoTEventsData.Types.DetectorStateSummary
import Network.AWS.IoTEventsData.Types.DetectorSummary
import Network.AWS.IoTEventsData.Types.DisableActionConfiguration
import Network.AWS.IoTEventsData.Types.DisableAlarmActionRequest
import Network.AWS.IoTEventsData.Types.EnableActionConfiguration
import Network.AWS.IoTEventsData.Types.EnableAlarmActionRequest
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
import Network.AWS.IoTEventsData.Types.UpdateDetectorRequest
import Network.AWS.IoTEventsData.Types.Variable
import Network.AWS.IoTEventsData.Types.VariableDefinition
