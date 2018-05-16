{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.Sum where

import Network.AWS.Prelude

data AgentHealth
  = AHHealthy
  | AHUnhealthy
  | AHUnknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AgentHealth where
    parser = takeLowerText >>= \case
        "healthy" -> pure AHHealthy
        "unhealthy" -> pure AHUnhealthy
        "unknown" -> pure AHUnknown
        e -> fromTextError $ "Failure parsing AgentHealth from value: '" <> e
           <> "'. Accepted values: healthy, unhealthy, unknown"

instance ToText AgentHealth where
    toText = \case
        AHHealthy -> "HEALTHY"
        AHUnhealthy -> "UNHEALTHY"
        AHUnknown -> "UNKNOWN"

instance Hashable     AgentHealth
instance NFData       AgentHealth
instance ToByteString AgentHealth
instance ToQuery      AgentHealth
instance ToHeader     AgentHealth

instance ToJSON AgentHealth where
    toJSON = toJSONText

instance FromJSON AgentHealth where
    parseJSON = parseJSONText "AgentHealth"

data AgentHealthCode
  = Idle
  | Running
  | Shutdown
  | Throttled
  | Unhealthy
  | Unknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AgentHealthCode where
    parser = takeLowerText >>= \case
        "idle" -> pure Idle
        "running" -> pure Running
        "shutdown" -> pure Shutdown
        "throttled" -> pure Throttled
        "unhealthy" -> pure Unhealthy
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing AgentHealthCode from value: '" <> e
           <> "'. Accepted values: idle, running, shutdown, throttled, unhealthy, unknown"

instance ToText AgentHealthCode where
    toText = \case
        Idle -> "IDLE"
        Running -> "RUNNING"
        Shutdown -> "SHUTDOWN"
        Throttled -> "THROTTLED"
        Unhealthy -> "UNHEALTHY"
        Unknown -> "UNKNOWN"

instance Hashable     AgentHealthCode
instance NFData       AgentHealthCode
instance ToByteString AgentHealthCode
instance ToQuery      AgentHealthCode
instance ToHeader     AgentHealthCode

instance ToJSON AgentHealthCode where
    toJSON = toJSONText

instance FromJSON AgentHealthCode where
    parseJSON = parseJSONText "AgentHealthCode"

data AssessmentRunNotificationSNSStatusCode
  = AccessDenied
  | InternalError
  | Success
  | TopicDoesNotExist
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AssessmentRunNotificationSNSStatusCode where
    parser = takeLowerText >>= \case
        "access_denied" -> pure AccessDenied
        "internal_error" -> pure InternalError
        "success" -> pure Success
        "topic_does_not_exist" -> pure TopicDoesNotExist
        e -> fromTextError $ "Failure parsing AssessmentRunNotificationSNSStatusCode from value: '" <> e
           <> "'. Accepted values: access_denied, internal_error, success, topic_does_not_exist"

instance ToText AssessmentRunNotificationSNSStatusCode where
    toText = \case
        AccessDenied -> "ACCESS_DENIED"
        InternalError -> "INTERNAL_ERROR"
        Success -> "SUCCESS"
        TopicDoesNotExist -> "TOPIC_DOES_NOT_EXIST"

instance Hashable     AssessmentRunNotificationSNSStatusCode
instance NFData       AssessmentRunNotificationSNSStatusCode
instance ToByteString AssessmentRunNotificationSNSStatusCode
instance ToQuery      AssessmentRunNotificationSNSStatusCode
instance ToHeader     AssessmentRunNotificationSNSStatusCode

instance FromJSON AssessmentRunNotificationSNSStatusCode where
    parseJSON = parseJSONText "AssessmentRunNotificationSNSStatusCode"

data AssessmentRunState
  = Canceled
  | CollectingData
  | Completed
  | CompletedWithErrors
  | Created
  | DataCollected
  | Error'
  | EvaluatingRules
  | Failed
  | StartDataCollectionInProgress
  | StartDataCollectionPending
  | StartEvaluatingRulesPending
  | StopDataCollectionPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AssessmentRunState where
    parser = takeLowerText >>= \case
        "canceled" -> pure Canceled
        "collecting_data" -> pure CollectingData
        "completed" -> pure Completed
        "completed_with_errors" -> pure CompletedWithErrors
        "created" -> pure Created
        "data_collected" -> pure DataCollected
        "error" -> pure Error'
        "evaluating_rules" -> pure EvaluatingRules
        "failed" -> pure Failed
        "start_data_collection_in_progress" -> pure StartDataCollectionInProgress
        "start_data_collection_pending" -> pure StartDataCollectionPending
        "start_evaluating_rules_pending" -> pure StartEvaluatingRulesPending
        "stop_data_collection_pending" -> pure StopDataCollectionPending
        e -> fromTextError $ "Failure parsing AssessmentRunState from value: '" <> e
           <> "'. Accepted values: canceled, collecting_data, completed, completed_with_errors, created, data_collected, error, evaluating_rules, failed, start_data_collection_in_progress, start_data_collection_pending, start_evaluating_rules_pending, stop_data_collection_pending"

instance ToText AssessmentRunState where
    toText = \case
        Canceled -> "CANCELED"
        CollectingData -> "COLLECTING_DATA"
        Completed -> "COMPLETED"
        CompletedWithErrors -> "COMPLETED_WITH_ERRORS"
        Created -> "CREATED"
        DataCollected -> "DATA_COLLECTED"
        Error' -> "ERROR"
        EvaluatingRules -> "EVALUATING_RULES"
        Failed -> "FAILED"
        StartDataCollectionInProgress -> "START_DATA_COLLECTION_IN_PROGRESS"
        StartDataCollectionPending -> "START_DATA_COLLECTION_PENDING"
        StartEvaluatingRulesPending -> "START_EVALUATING_RULES_PENDING"
        StopDataCollectionPending -> "STOP_DATA_COLLECTION_PENDING"

instance Hashable     AssessmentRunState
instance NFData       AssessmentRunState
instance ToByteString AssessmentRunState
instance ToQuery      AssessmentRunState
instance ToHeader     AssessmentRunState

instance ToJSON AssessmentRunState where
    toJSON = toJSONText

instance FromJSON AssessmentRunState where
    parseJSON = parseJSONText "AssessmentRunState"

data AssetType =
  EC2Instance
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AssetType where
    parser = takeLowerText >>= \case
        "ec2-instance" -> pure EC2Instance
        e -> fromTextError $ "Failure parsing AssetType from value: '" <> e
           <> "'. Accepted values: ec2-instance"

instance ToText AssetType where
    toText = \case
        EC2Instance -> "ec2-instance"

instance Hashable     AssetType
instance NFData       AssetType
instance ToByteString AssetType
instance ToQuery      AssetType
instance ToHeader     AssetType

instance FromJSON AssetType where
    parseJSON = parseJSONText "AssetType"

data FailedItemErrorCode
  = FIECAccessDenied
  | FIECDuplicateARN
  | FIECInternalError
  | FIECInvalidARN
  | FIECItemDoesNotExist
  | FIECLimitExceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FailedItemErrorCode where
    parser = takeLowerText >>= \case
        "access_denied" -> pure FIECAccessDenied
        "duplicate_arn" -> pure FIECDuplicateARN
        "internal_error" -> pure FIECInternalError
        "invalid_arn" -> pure FIECInvalidARN
        "item_does_not_exist" -> pure FIECItemDoesNotExist
        "limit_exceeded" -> pure FIECLimitExceeded
        e -> fromTextError $ "Failure parsing FailedItemErrorCode from value: '" <> e
           <> "'. Accepted values: access_denied, duplicate_arn, internal_error, invalid_arn, item_does_not_exist, limit_exceeded"

instance ToText FailedItemErrorCode where
    toText = \case
        FIECAccessDenied -> "ACCESS_DENIED"
        FIECDuplicateARN -> "DUPLICATE_ARN"
        FIECInternalError -> "INTERNAL_ERROR"
        FIECInvalidARN -> "INVALID_ARN"
        FIECItemDoesNotExist -> "ITEM_DOES_NOT_EXIST"
        FIECLimitExceeded -> "LIMIT_EXCEEDED"

instance Hashable     FailedItemErrorCode
instance NFData       FailedItemErrorCode
instance ToByteString FailedItemErrorCode
instance ToQuery      FailedItemErrorCode
instance ToHeader     FailedItemErrorCode

instance FromJSON FailedItemErrorCode where
    parseJSON = parseJSONText "FailedItemErrorCode"

data InspectorEvent
  = AssessmentRunCompleted
  | AssessmentRunStarted
  | AssessmentRunStateChanged
  | FindingReported
  | Other
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InspectorEvent where
    parser = takeLowerText >>= \case
        "assessment_run_completed" -> pure AssessmentRunCompleted
        "assessment_run_started" -> pure AssessmentRunStarted
        "assessment_run_state_changed" -> pure AssessmentRunStateChanged
        "finding_reported" -> pure FindingReported
        "other" -> pure Other
        e -> fromTextError $ "Failure parsing InspectorEvent from value: '" <> e
           <> "'. Accepted values: assessment_run_completed, assessment_run_started, assessment_run_state_changed, finding_reported, other"

instance ToText InspectorEvent where
    toText = \case
        AssessmentRunCompleted -> "ASSESSMENT_RUN_COMPLETED"
        AssessmentRunStarted -> "ASSESSMENT_RUN_STARTED"
        AssessmentRunStateChanged -> "ASSESSMENT_RUN_STATE_CHANGED"
        FindingReported -> "FINDING_REPORTED"
        Other -> "OTHER"

instance Hashable     InspectorEvent
instance NFData       InspectorEvent
instance ToByteString InspectorEvent
instance ToQuery      InspectorEvent
instance ToHeader     InspectorEvent

instance ToJSON InspectorEvent where
    toJSON = toJSONText

instance FromJSON InspectorEvent where
    parseJSON = parseJSONText "InspectorEvent"

data Locale =
  EnUs
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Locale where
    parser = takeLowerText >>= \case
        "en_us" -> pure EnUs
        e -> fromTextError $ "Failure parsing Locale from value: '" <> e
           <> "'. Accepted values: en_us"

instance ToText Locale where
    toText = \case
        EnUs -> "EN_US"

instance Hashable     Locale
instance NFData       Locale
instance ToByteString Locale
instance ToQuery      Locale
instance ToHeader     Locale

instance ToJSON Locale where
    toJSON = toJSONText

data ReportFileFormat
  = HTML
  | Pdf
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReportFileFormat where
    parser = takeLowerText >>= \case
        "html" -> pure HTML
        "pdf" -> pure Pdf
        e -> fromTextError $ "Failure parsing ReportFileFormat from value: '" <> e
           <> "'. Accepted values: html, pdf"

instance ToText ReportFileFormat where
    toText = \case
        HTML -> "HTML"
        Pdf -> "PDF"

instance Hashable     ReportFileFormat
instance NFData       ReportFileFormat
instance ToByteString ReportFileFormat
instance ToQuery      ReportFileFormat
instance ToHeader     ReportFileFormat

instance ToJSON ReportFileFormat where
    toJSON = toJSONText

data ReportStatus
  = RSCompleted
  | RSFailed
  | RSWorkInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReportStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure RSCompleted
        "failed" -> pure RSFailed
        "work_in_progress" -> pure RSWorkInProgress
        e -> fromTextError $ "Failure parsing ReportStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, work_in_progress"

instance ToText ReportStatus where
    toText = \case
        RSCompleted -> "COMPLETED"
        RSFailed -> "FAILED"
        RSWorkInProgress -> "WORK_IN_PROGRESS"

instance Hashable     ReportStatus
instance NFData       ReportStatus
instance ToByteString ReportStatus
instance ToQuery      ReportStatus
instance ToHeader     ReportStatus

instance FromJSON ReportStatus where
    parseJSON = parseJSONText "ReportStatus"

data ReportType
  = Finding
  | Full
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReportType where
    parser = takeLowerText >>= \case
        "finding" -> pure Finding
        "full" -> pure Full
        e -> fromTextError $ "Failure parsing ReportType from value: '" <> e
           <> "'. Accepted values: finding, full"

instance ToText ReportType where
    toText = \case
        Finding -> "FINDING"
        Full -> "FULL"

instance Hashable     ReportType
instance NFData       ReportType
instance ToByteString ReportType
instance ToQuery      ReportType
instance ToHeader     ReportType

instance ToJSON ReportType where
    toJSON = toJSONText

data Severity
  = High
  | Informational
  | Low
  | Medium
  | Undefined
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Severity where
    parser = takeLowerText >>= \case
        "high" -> pure High
        "informational" -> pure Informational
        "low" -> pure Low
        "medium" -> pure Medium
        "undefined" -> pure Undefined
        e -> fromTextError $ "Failure parsing Severity from value: '" <> e
           <> "'. Accepted values: high, informational, low, medium, undefined"

instance ToText Severity where
    toText = \case
        High -> "High"
        Informational -> "Informational"
        Low -> "Low"
        Medium -> "Medium"
        Undefined -> "Undefined"

instance Hashable     Severity
instance NFData       Severity
instance ToByteString Severity
instance ToQuery      Severity
instance ToHeader     Severity

instance ToJSON Severity where
    toJSON = toJSONText

instance FromJSON Severity where
    parseJSON = parseJSONText "Severity"

data StopAction
  = SkipEvaluation
  | StartEvaluation
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StopAction where
    parser = takeLowerText >>= \case
        "skip_evaluation" -> pure SkipEvaluation
        "start_evaluation" -> pure StartEvaluation
        e -> fromTextError $ "Failure parsing StopAction from value: '" <> e
           <> "'. Accepted values: skip_evaluation, start_evaluation"

instance ToText StopAction where
    toText = \case
        SkipEvaluation -> "SKIP_EVALUATION"
        StartEvaluation -> "START_EVALUATION"

instance Hashable     StopAction
instance NFData       StopAction
instance ToByteString StopAction
instance ToQuery      StopAction
instance ToHeader     StopAction

instance ToJSON StopAction where
    toJSON = toJSONText
