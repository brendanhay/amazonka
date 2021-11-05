{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FraudDetector.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types
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

import qualified Amazonka.Core as Core
import Amazonka.FraudDetector.Types.AsyncJobStatus
import Amazonka.FraudDetector.Types.BatchCreateVariableError
import Amazonka.FraudDetector.Types.BatchGetVariableError
import Amazonka.FraudDetector.Types.BatchImport
import Amazonka.FraudDetector.Types.BatchPrediction
import Amazonka.FraudDetector.Types.DataSource
import Amazonka.FraudDetector.Types.DataType
import Amazonka.FraudDetector.Types.DataValidationMetrics
import Amazonka.FraudDetector.Types.Detector
import Amazonka.FraudDetector.Types.DetectorVersionStatus
import Amazonka.FraudDetector.Types.DetectorVersionSummary
import Amazonka.FraudDetector.Types.Entity
import Amazonka.FraudDetector.Types.EntityType
import Amazonka.FraudDetector.Types.Event
import Amazonka.FraudDetector.Types.EventIngestion
import Amazonka.FraudDetector.Types.EventType
import Amazonka.FraudDetector.Types.ExternalEventsDetail
import Amazonka.FraudDetector.Types.ExternalModel
import Amazonka.FraudDetector.Types.ExternalModelOutputs
import Amazonka.FraudDetector.Types.ExternalModelSummary
import Amazonka.FraudDetector.Types.FieldValidationMessage
import Amazonka.FraudDetector.Types.FileValidationMessage
import Amazonka.FraudDetector.Types.IngestedEventStatistics
import Amazonka.FraudDetector.Types.IngestedEventsDetail
import Amazonka.FraudDetector.Types.IngestedEventsTimeWindow
import Amazonka.FraudDetector.Types.KMSKey
import Amazonka.FraudDetector.Types.Label
import Amazonka.FraudDetector.Types.LabelSchema
import Amazonka.FraudDetector.Types.Language
import Amazonka.FraudDetector.Types.LogOddsMetric
import Amazonka.FraudDetector.Types.MetricDataPoint
import Amazonka.FraudDetector.Types.Model
import Amazonka.FraudDetector.Types.ModelEndpointDataBlob
import Amazonka.FraudDetector.Types.ModelEndpointStatus
import Amazonka.FraudDetector.Types.ModelInputConfiguration
import Amazonka.FraudDetector.Types.ModelInputDataFormat
import Amazonka.FraudDetector.Types.ModelOutputConfiguration
import Amazonka.FraudDetector.Types.ModelOutputDataFormat
import Amazonka.FraudDetector.Types.ModelScores
import Amazonka.FraudDetector.Types.ModelSource
import Amazonka.FraudDetector.Types.ModelTypeEnum
import Amazonka.FraudDetector.Types.ModelVersion
import Amazonka.FraudDetector.Types.ModelVersionDetail
import Amazonka.FraudDetector.Types.ModelVersionStatus
import Amazonka.FraudDetector.Types.Outcome
import Amazonka.FraudDetector.Types.Rule
import Amazonka.FraudDetector.Types.RuleDetail
import Amazonka.FraudDetector.Types.RuleExecutionMode
import Amazonka.FraudDetector.Types.RuleResult
import Amazonka.FraudDetector.Types.Tag
import Amazonka.FraudDetector.Types.TrainingDataSchema
import Amazonka.FraudDetector.Types.TrainingDataSourceEnum
import Amazonka.FraudDetector.Types.TrainingMetrics
import Amazonka.FraudDetector.Types.TrainingResult
import Amazonka.FraudDetector.Types.UnlabeledEventsTreatment
import Amazonka.FraudDetector.Types.Variable
import Amazonka.FraudDetector.Types.VariableEntry
import Amazonka.FraudDetector.Types.VariableImportanceMetrics
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

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
