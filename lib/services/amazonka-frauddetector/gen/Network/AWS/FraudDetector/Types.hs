{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FraudDetector.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FraudDetector.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ResourceUnavailableException,
    _ConflictException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * AsyncJobStatus
    AsyncJobStatus (..),

    -- * DataSource
    DataSource (..),

    -- * DataType
    DataType (..),

    -- * DetectorVersionStatus
    DetectorVersionStatus (..),

    -- * EventIngestion
    EventIngestion (..),

    -- * Language
    Language (..),

    -- * ModelEndpointStatus
    ModelEndpointStatus (..),

    -- * ModelInputDataFormat
    ModelInputDataFormat (..),

    -- * ModelOutputDataFormat
    ModelOutputDataFormat (..),

    -- * ModelSource
    ModelSource (..),

    -- * ModelTypeEnum
    ModelTypeEnum (..),

    -- * ModelVersionStatus
    ModelVersionStatus (..),

    -- * RuleExecutionMode
    RuleExecutionMode (..),

    -- * TrainingDataSourceEnum
    TrainingDataSourceEnum (..),

    -- * UnlabeledEventsTreatment
    UnlabeledEventsTreatment (..),

    -- * BatchCreateVariableError
    BatchCreateVariableError (..),
    newBatchCreateVariableError,
    batchCreateVariableError_name,
    batchCreateVariableError_code,
    batchCreateVariableError_message,

    -- * BatchGetVariableError
    BatchGetVariableError (..),
    newBatchGetVariableError,
    batchGetVariableError_name,
    batchGetVariableError_code,
    batchGetVariableError_message,

    -- * BatchImport
    BatchImport (..),
    newBatchImport,
    batchImport_failureReason,
    batchImport_iamRoleArn,
    batchImport_status,
    batchImport_processedRecordsCount,
    batchImport_totalRecordsCount,
    batchImport_jobId,
    batchImport_arn,
    batchImport_startTime,
    batchImport_eventTypeName,
    batchImport_completionTime,
    batchImport_outputPath,
    batchImport_inputPath,
    batchImport_failedRecordsCount,

    -- * BatchPrediction
    BatchPrediction (..),
    newBatchPrediction,
    batchPrediction_failureReason,
    batchPrediction_iamRoleArn,
    batchPrediction_status,
    batchPrediction_processedRecordsCount,
    batchPrediction_totalRecordsCount,
    batchPrediction_lastHeartbeatTime,
    batchPrediction_jobId,
    batchPrediction_arn,
    batchPrediction_startTime,
    batchPrediction_eventTypeName,
    batchPrediction_completionTime,
    batchPrediction_outputPath,
    batchPrediction_detectorName,
    batchPrediction_detectorVersion,
    batchPrediction_inputPath,

    -- * DataValidationMetrics
    DataValidationMetrics (..),
    newDataValidationMetrics,
    dataValidationMetrics_fieldLevelMessages,
    dataValidationMetrics_fileLevelMessages,

    -- * Detector
    Detector (..),
    newDetector,
    detector_lastUpdatedTime,
    detector_arn,
    detector_createdTime,
    detector_eventTypeName,
    detector_detectorId,
    detector_description,

    -- * DetectorVersionSummary
    DetectorVersionSummary (..),
    newDetectorVersionSummary,
    detectorVersionSummary_status,
    detectorVersionSummary_lastUpdatedTime,
    detectorVersionSummary_detectorVersionId,
    detectorVersionSummary_description,

    -- * Entity
    Entity (..),
    newEntity,
    entity_entityType,
    entity_entityId,

    -- * EntityType
    EntityType (..),
    newEntityType,
    entityType_lastUpdatedTime,
    entityType_arn,
    entityType_createdTime,
    entityType_name,
    entityType_description,

    -- * Event
    Event (..),
    newEvent,
    event_eventTimestamp,
    event_entities,
    event_labelTimestamp,
    event_eventTypeName,
    event_eventVariables,
    event_currentLabel,
    event_eventId,

    -- * EventType
    EventType (..),
    newEventType,
    eventType_lastUpdatedTime,
    eventType_arn,
    eventType_createdTime,
    eventType_entityTypes,
    eventType_eventVariables,
    eventType_name,
    eventType_ingestedEventStatistics,
    eventType_labels,
    eventType_eventIngestion,
    eventType_description,

    -- * ExternalEventsDetail
    ExternalEventsDetail (..),
    newExternalEventsDetail,
    externalEventsDetail_dataLocation,
    externalEventsDetail_dataAccessRoleArn,

    -- * ExternalModel
    ExternalModel (..),
    newExternalModel,
    externalModel_modelEndpoint,
    externalModel_modelSource,
    externalModel_lastUpdatedTime,
    externalModel_arn,
    externalModel_createdTime,
    externalModel_modelEndpointStatus,
    externalModel_outputConfiguration,
    externalModel_invokeModelEndpointRoleArn,
    externalModel_inputConfiguration,

    -- * ExternalModelOutputs
    ExternalModelOutputs (..),
    newExternalModelOutputs,
    externalModelOutputs_externalModel,
    externalModelOutputs_outputs,

    -- * ExternalModelSummary
    ExternalModelSummary (..),
    newExternalModelSummary,
    externalModelSummary_modelEndpoint,
    externalModelSummary_modelSource,

    -- * FieldValidationMessage
    FieldValidationMessage (..),
    newFieldValidationMessage,
    fieldValidationMessage_identifier,
    fieldValidationMessage_content,
    fieldValidationMessage_fieldName,
    fieldValidationMessage_title,
    fieldValidationMessage_type,

    -- * FileValidationMessage
    FileValidationMessage (..),
    newFileValidationMessage,
    fileValidationMessage_content,
    fileValidationMessage_title,
    fileValidationMessage_type,

    -- * IngestedEventStatistics
    IngestedEventStatistics (..),
    newIngestedEventStatistics,
    ingestedEventStatistics_eventDataSizeInBytes,
    ingestedEventStatistics_mostRecentEvent,
    ingestedEventStatistics_lastUpdatedTime,
    ingestedEventStatistics_numberOfEvents,
    ingestedEventStatistics_leastRecentEvent,

    -- * IngestedEventsDetail
    IngestedEventsDetail (..),
    newIngestedEventsDetail,
    ingestedEventsDetail_ingestedEventsTimeWindow,

    -- * IngestedEventsTimeWindow
    IngestedEventsTimeWindow (..),
    newIngestedEventsTimeWindow,
    ingestedEventsTimeWindow_startTime,
    ingestedEventsTimeWindow_endTime,

    -- * KMSKey
    KMSKey (..),
    newKMSKey,
    kmsKey_kmsEncryptionKeyArn,

    -- * Label
    Label (..),
    newLabel,
    label_lastUpdatedTime,
    label_arn,
    label_createdTime,
    label_name,
    label_description,

    -- * LabelSchema
    LabelSchema (..),
    newLabelSchema,
    labelSchema_unlabeledEventsTreatment,
    labelSchema_labelMapper,

    -- * LogOddsMetric
    LogOddsMetric (..),
    newLogOddsMetric,
    logOddsMetric_variableName,
    logOddsMetric_variableType,
    logOddsMetric_variableImportance,

    -- * MetricDataPoint
    MetricDataPoint (..),
    newMetricDataPoint,
    metricDataPoint_precision,
    metricDataPoint_fpr,
    metricDataPoint_threshold,
    metricDataPoint_tpr,

    -- * Model
    Model (..),
    newModel,
    model_modelType,
    model_lastUpdatedTime,
    model_modelId,
    model_arn,
    model_createdTime,
    model_eventTypeName,
    model_description,

    -- * ModelEndpointDataBlob
    ModelEndpointDataBlob (..),
    newModelEndpointDataBlob,
    modelEndpointDataBlob_byteBuffer,
    modelEndpointDataBlob_contentType,

    -- * ModelInputConfiguration
    ModelInputConfiguration (..),
    newModelInputConfiguration,
    modelInputConfiguration_format,
    modelInputConfiguration_eventTypeName,
    modelInputConfiguration_csvInputTemplate,
    modelInputConfiguration_jsonInputTemplate,
    modelInputConfiguration_useEventVariables,

    -- * ModelOutputConfiguration
    ModelOutputConfiguration (..),
    newModelOutputConfiguration,
    modelOutputConfiguration_jsonKeyToVariableMap,
    modelOutputConfiguration_csvIndexToVariableMap,
    modelOutputConfiguration_format,

    -- * ModelScores
    ModelScores (..),
    newModelScores,
    modelScores_modelVersion,
    modelScores_scores,

    -- * ModelVersion
    ModelVersion (..),
    newModelVersion,
    modelVersion_arn,
    modelVersion_modelId,
    modelVersion_modelType,
    modelVersion_modelVersionNumber,

    -- * ModelVersionDetail
    ModelVersionDetail (..),
    newModelVersionDetail,
    modelVersionDetail_status,
    modelVersionDetail_modelType,
    modelVersionDetail_lastUpdatedTime,
    modelVersionDetail_modelId,
    modelVersionDetail_arn,
    modelVersionDetail_trainingDataSource,
    modelVersionDetail_createdTime,
    modelVersionDetail_externalEventsDetail,
    modelVersionDetail_ingestedEventsDetail,
    modelVersionDetail_modelVersionNumber,
    modelVersionDetail_trainingResult,
    modelVersionDetail_trainingDataSchema,

    -- * Outcome
    Outcome (..),
    newOutcome,
    outcome_lastUpdatedTime,
    outcome_arn,
    outcome_createdTime,
    outcome_name,
    outcome_description,

    -- * Rule
    Rule (..),
    newRule,
    rule_detectorId,
    rule_ruleId,
    rule_ruleVersion,

    -- * RuleDetail
    RuleDetail (..),
    newRuleDetail,
    ruleDetail_ruleVersion,
    ruleDetail_lastUpdatedTime,
    ruleDetail_arn,
    ruleDetail_createdTime,
    ruleDetail_ruleId,
    ruleDetail_outcomes,
    ruleDetail_detectorId,
    ruleDetail_expression,
    ruleDetail_language,
    ruleDetail_description,

    -- * RuleResult
    RuleResult (..),
    newRuleResult,
    ruleResult_ruleId,
    ruleResult_outcomes,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TrainingDataSchema
    TrainingDataSchema (..),
    newTrainingDataSchema,
    trainingDataSchema_modelVariables,
    trainingDataSchema_labelSchema,

    -- * TrainingMetrics
    TrainingMetrics (..),
    newTrainingMetrics,
    trainingMetrics_auc,
    trainingMetrics_metricDataPoints,

    -- * TrainingResult
    TrainingResult (..),
    newTrainingResult,
    trainingResult_dataValidationMetrics,
    trainingResult_trainingMetrics,
    trainingResult_variableImportanceMetrics,

    -- * Variable
    Variable (..),
    newVariable,
    variable_lastUpdatedTime,
    variable_arn,
    variable_createdTime,
    variable_name,
    variable_dataSource,
    variable_dataType,
    variable_defaultValue,
    variable_variableType,
    variable_description,

    -- * VariableEntry
    VariableEntry (..),
    newVariableEntry,
    variableEntry_name,
    variableEntry_dataSource,
    variableEntry_dataType,
    variableEntry_defaultValue,
    variableEntry_variableType,
    variableEntry_description,

    -- * VariableImportanceMetrics
    VariableImportanceMetrics (..),
    newVariableImportanceMetrics,
    variableImportanceMetrics_logOddsMetrics,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FraudDetector.Types.AsyncJobStatus
import Network.AWS.FraudDetector.Types.BatchCreateVariableError
import Network.AWS.FraudDetector.Types.BatchGetVariableError
import Network.AWS.FraudDetector.Types.BatchImport
import Network.AWS.FraudDetector.Types.BatchPrediction
import Network.AWS.FraudDetector.Types.DataSource
import Network.AWS.FraudDetector.Types.DataType
import Network.AWS.FraudDetector.Types.DataValidationMetrics
import Network.AWS.FraudDetector.Types.Detector
import Network.AWS.FraudDetector.Types.DetectorVersionStatus
import Network.AWS.FraudDetector.Types.DetectorVersionSummary
import Network.AWS.FraudDetector.Types.Entity
import Network.AWS.FraudDetector.Types.EntityType
import Network.AWS.FraudDetector.Types.Event
import Network.AWS.FraudDetector.Types.EventIngestion
import Network.AWS.FraudDetector.Types.EventType
import Network.AWS.FraudDetector.Types.ExternalEventsDetail
import Network.AWS.FraudDetector.Types.ExternalModel
import Network.AWS.FraudDetector.Types.ExternalModelOutputs
import Network.AWS.FraudDetector.Types.ExternalModelSummary
import Network.AWS.FraudDetector.Types.FieldValidationMessage
import Network.AWS.FraudDetector.Types.FileValidationMessage
import Network.AWS.FraudDetector.Types.IngestedEventStatistics
import Network.AWS.FraudDetector.Types.IngestedEventsDetail
import Network.AWS.FraudDetector.Types.IngestedEventsTimeWindow
import Network.AWS.FraudDetector.Types.KMSKey
import Network.AWS.FraudDetector.Types.Label
import Network.AWS.FraudDetector.Types.LabelSchema
import Network.AWS.FraudDetector.Types.Language
import Network.AWS.FraudDetector.Types.LogOddsMetric
import Network.AWS.FraudDetector.Types.MetricDataPoint
import Network.AWS.FraudDetector.Types.Model
import Network.AWS.FraudDetector.Types.ModelEndpointDataBlob
import Network.AWS.FraudDetector.Types.ModelEndpointStatus
import Network.AWS.FraudDetector.Types.ModelInputConfiguration
import Network.AWS.FraudDetector.Types.ModelInputDataFormat
import Network.AWS.FraudDetector.Types.ModelOutputConfiguration
import Network.AWS.FraudDetector.Types.ModelOutputDataFormat
import Network.AWS.FraudDetector.Types.ModelScores
import Network.AWS.FraudDetector.Types.ModelSource
import Network.AWS.FraudDetector.Types.ModelTypeEnum
import Network.AWS.FraudDetector.Types.ModelVersion
import Network.AWS.FraudDetector.Types.ModelVersionDetail
import Network.AWS.FraudDetector.Types.ModelVersionStatus
import Network.AWS.FraudDetector.Types.Outcome
import Network.AWS.FraudDetector.Types.Rule
import Network.AWS.FraudDetector.Types.RuleDetail
import Network.AWS.FraudDetector.Types.RuleExecutionMode
import Network.AWS.FraudDetector.Types.RuleResult
import Network.AWS.FraudDetector.Types.Tag
import Network.AWS.FraudDetector.Types.TrainingDataSchema
import Network.AWS.FraudDetector.Types.TrainingDataSourceEnum
import Network.AWS.FraudDetector.Types.TrainingMetrics
import Network.AWS.FraudDetector.Types.TrainingResult
import Network.AWS.FraudDetector.Types.UnlabeledEventsTreatment
import Network.AWS.FraudDetector.Types.Variable
import Network.AWS.FraudDetector.Types.VariableEntry
import Network.AWS.FraudDetector.Types.VariableImportanceMetrics
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2019-11-15@ of the Amazon Fraud Detector SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "FraudDetector",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "frauddetector",
      Core._serviceSigningName = "frauddetector",
      Core._serviceVersion = "2019-11-15",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "FraudDetector",
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

-- | An exception indicating a specified value is not allowed.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | An exception indicating Amazon Fraud Detector does not have the needed
-- permissions. This can occur if you submit a request, such as
-- @PutExternalModel@, that specifies a role that is not in your account.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | An exception indicating that the attached customer-owned (external)
-- model threw an exception when Amazon Fraud Detector invoked the model.
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- | An exception indicating there was a conflict during a delete operation.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | An exception indicating a throttling error.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | An exception indicating an internal server error.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | An exception indicating the specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
