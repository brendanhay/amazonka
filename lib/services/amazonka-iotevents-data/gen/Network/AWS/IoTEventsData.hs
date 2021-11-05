{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.IoTEventsData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-10-23@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS IoT Events monitors your equipment or device fleets for failures or
-- changes in operation, and triggers actions when such events occur. You
-- can use AWS IoT Events Data API commands to send inputs to detectors,
-- list detectors, and view or update a detector\'s status.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/what-is-iotevents.html What is AWS IoT Events?>
-- in the /AWS IoT Events Developer Guide/.
module Network.AWS.IoTEventsData
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchSnoozeAlarm
    BatchSnoozeAlarm (BatchSnoozeAlarm'),
    newBatchSnoozeAlarm,
    BatchSnoozeAlarmResponse (BatchSnoozeAlarmResponse'),
    newBatchSnoozeAlarmResponse,

    -- ** BatchDisableAlarm
    BatchDisableAlarm (BatchDisableAlarm'),
    newBatchDisableAlarm,
    BatchDisableAlarmResponse (BatchDisableAlarmResponse'),
    newBatchDisableAlarmResponse,

    -- ** DescribeAlarm
    DescribeAlarm (DescribeAlarm'),
    newDescribeAlarm,
    DescribeAlarmResponse (DescribeAlarmResponse'),
    newDescribeAlarmResponse,

    -- ** BatchPutMessage
    BatchPutMessage (BatchPutMessage'),
    newBatchPutMessage,
    BatchPutMessageResponse (BatchPutMessageResponse'),
    newBatchPutMessageResponse,

    -- ** DescribeDetector
    DescribeDetector (DescribeDetector'),
    newDescribeDetector,
    DescribeDetectorResponse (DescribeDetectorResponse'),
    newDescribeDetectorResponse,

    -- ** BatchUpdateDetector
    BatchUpdateDetector (BatchUpdateDetector'),
    newBatchUpdateDetector,
    BatchUpdateDetectorResponse (BatchUpdateDetectorResponse'),
    newBatchUpdateDetectorResponse,

    -- ** BatchAcknowledgeAlarm
    BatchAcknowledgeAlarm (BatchAcknowledgeAlarm'),
    newBatchAcknowledgeAlarm,
    BatchAcknowledgeAlarmResponse (BatchAcknowledgeAlarmResponse'),
    newBatchAcknowledgeAlarmResponse,

    -- ** ListAlarms
    ListAlarms (ListAlarms'),
    newListAlarms,
    ListAlarmsResponse (ListAlarmsResponse'),
    newListAlarmsResponse,

    -- ** BatchResetAlarm
    BatchResetAlarm (BatchResetAlarm'),
    newBatchResetAlarm,
    BatchResetAlarmResponse (BatchResetAlarmResponse'),
    newBatchResetAlarmResponse,

    -- ** ListDetectors
    ListDetectors (ListDetectors'),
    newListDetectors,
    ListDetectorsResponse (ListDetectorsResponse'),
    newListDetectorsResponse,

    -- ** BatchEnableAlarm
    BatchEnableAlarm (BatchEnableAlarm'),
    newBatchEnableAlarm,
    BatchEnableAlarmResponse (BatchEnableAlarmResponse'),
    newBatchEnableAlarmResponse,

    -- * Types

    -- ** AlarmStateName
    AlarmStateName (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** CustomerActionName
    CustomerActionName (..),

    -- ** ErrorCode
    ErrorCode (..),

    -- ** EventType
    EventType (..),

    -- ** TriggerType
    TriggerType (..),

    -- ** AcknowledgeActionConfiguration
    AcknowledgeActionConfiguration (AcknowledgeActionConfiguration'),
    newAcknowledgeActionConfiguration,

    -- ** AcknowledgeAlarmActionRequest
    AcknowledgeAlarmActionRequest (AcknowledgeAlarmActionRequest'),
    newAcknowledgeAlarmActionRequest,

    -- ** Alarm
    Alarm (Alarm'),
    newAlarm,

    -- ** AlarmState
    AlarmState (AlarmState'),
    newAlarmState,

    -- ** AlarmSummary
    AlarmSummary (AlarmSummary'),
    newAlarmSummary,

    -- ** BatchAlarmActionErrorEntry
    BatchAlarmActionErrorEntry (BatchAlarmActionErrorEntry'),
    newBatchAlarmActionErrorEntry,

    -- ** BatchPutMessageErrorEntry
    BatchPutMessageErrorEntry (BatchPutMessageErrorEntry'),
    newBatchPutMessageErrorEntry,

    -- ** BatchUpdateDetectorErrorEntry
    BatchUpdateDetectorErrorEntry (BatchUpdateDetectorErrorEntry'),
    newBatchUpdateDetectorErrorEntry,

    -- ** CustomerAction
    CustomerAction (CustomerAction'),
    newCustomerAction,

    -- ** Detector
    Detector (Detector'),
    newDetector,

    -- ** DetectorState
    DetectorState (DetectorState'),
    newDetectorState,

    -- ** DetectorStateDefinition
    DetectorStateDefinition (DetectorStateDefinition'),
    newDetectorStateDefinition,

    -- ** DetectorStateSummary
    DetectorStateSummary (DetectorStateSummary'),
    newDetectorStateSummary,

    -- ** DetectorSummary
    DetectorSummary (DetectorSummary'),
    newDetectorSummary,

    -- ** DisableActionConfiguration
    DisableActionConfiguration (DisableActionConfiguration'),
    newDisableActionConfiguration,

    -- ** DisableAlarmActionRequest
    DisableAlarmActionRequest (DisableAlarmActionRequest'),
    newDisableAlarmActionRequest,

    -- ** EnableActionConfiguration
    EnableActionConfiguration (EnableActionConfiguration'),
    newEnableActionConfiguration,

    -- ** EnableAlarmActionRequest
    EnableAlarmActionRequest (EnableAlarmActionRequest'),
    newEnableAlarmActionRequest,

    -- ** Message
    Message (Message'),
    newMessage,

    -- ** ResetActionConfiguration
    ResetActionConfiguration (ResetActionConfiguration'),
    newResetActionConfiguration,

    -- ** ResetAlarmActionRequest
    ResetAlarmActionRequest (ResetAlarmActionRequest'),
    newResetAlarmActionRequest,

    -- ** RuleEvaluation
    RuleEvaluation (RuleEvaluation'),
    newRuleEvaluation,

    -- ** SimpleRuleEvaluation
    SimpleRuleEvaluation (SimpleRuleEvaluation'),
    newSimpleRuleEvaluation,

    -- ** SnoozeActionConfiguration
    SnoozeActionConfiguration (SnoozeActionConfiguration'),
    newSnoozeActionConfiguration,

    -- ** SnoozeAlarmActionRequest
    SnoozeAlarmActionRequest (SnoozeAlarmActionRequest'),
    newSnoozeAlarmActionRequest,

    -- ** StateChangeConfiguration
    StateChangeConfiguration (StateChangeConfiguration'),
    newStateChangeConfiguration,

    -- ** SystemEvent
    SystemEvent (SystemEvent'),
    newSystemEvent,

    -- ** Timer
    Timer (Timer'),
    newTimer,

    -- ** TimerDefinition
    TimerDefinition (TimerDefinition'),
    newTimerDefinition,

    -- ** TimestampValue
    TimestampValue (TimestampValue'),
    newTimestampValue,

    -- ** UpdateDetectorRequest
    UpdateDetectorRequest (UpdateDetectorRequest'),
    newUpdateDetectorRequest,

    -- ** Variable
    Variable (Variable'),
    newVariable,

    -- ** VariableDefinition
    VariableDefinition (VariableDefinition'),
    newVariableDefinition,
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
import Network.AWS.IoTEventsData.Lens
import Network.AWS.IoTEventsData.ListAlarms
import Network.AWS.IoTEventsData.ListDetectors
import Network.AWS.IoTEventsData.Types
import Network.AWS.IoTEventsData.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoTEventsData'.

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
