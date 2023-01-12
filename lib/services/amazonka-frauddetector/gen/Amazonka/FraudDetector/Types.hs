{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FraudDetector.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ResourceUnavailableException,
    _ThrottlingException,
    _ValidationException,

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

    -- * ATIMetricDataPoint
    ATIMetricDataPoint (..),
    newATIMetricDataPoint,
    aTIMetricDataPoint_adr,
    aTIMetricDataPoint_atodr,
    aTIMetricDataPoint_cr,
    aTIMetricDataPoint_threshold,

    -- * ATIModelPerformance
    ATIModelPerformance (..),
    newATIModelPerformance,
    aTIModelPerformance_asi,

    -- * ATITrainingMetricsValue
    ATITrainingMetricsValue (..),
    newATITrainingMetricsValue,
    aTITrainingMetricsValue_metricDataPoints,
    aTITrainingMetricsValue_modelPerformance,

    -- * AggregatedLogOddsMetric
    AggregatedLogOddsMetric (..),
    newAggregatedLogOddsMetric,
    aggregatedLogOddsMetric_variableNames,
    aggregatedLogOddsMetric_aggregatedVariablesImportance,

    -- * AggregatedVariablesImpactExplanation
    AggregatedVariablesImpactExplanation (..),
    newAggregatedVariablesImpactExplanation,
    aggregatedVariablesImpactExplanation_eventVariableNames,
    aggregatedVariablesImpactExplanation_logOddsImpact,
    aggregatedVariablesImpactExplanation_relativeImpact,

    -- * AggregatedVariablesImportanceMetrics
    AggregatedVariablesImportanceMetrics (..),
    newAggregatedVariablesImportanceMetrics,
    aggregatedVariablesImportanceMetrics_logOddsMetrics,

    -- * BatchCreateVariableError
    BatchCreateVariableError (..),
    newBatchCreateVariableError,
    batchCreateVariableError_code,
    batchCreateVariableError_message,
    batchCreateVariableError_name,

    -- * BatchGetVariableError
    BatchGetVariableError (..),
    newBatchGetVariableError,
    batchGetVariableError_code,
    batchGetVariableError_message,
    batchGetVariableError_name,

    -- * BatchImport
    BatchImport (..),
    newBatchImport,
    batchImport_arn,
    batchImport_completionTime,
    batchImport_eventTypeName,
    batchImport_failedRecordsCount,
    batchImport_failureReason,
    batchImport_iamRoleArn,
    batchImport_inputPath,
    batchImport_jobId,
    batchImport_outputPath,
    batchImport_processedRecordsCount,
    batchImport_startTime,
    batchImport_status,
    batchImport_totalRecordsCount,

    -- * BatchPrediction
    BatchPrediction (..),
    newBatchPrediction,
    batchPrediction_arn,
    batchPrediction_completionTime,
    batchPrediction_detectorName,
    batchPrediction_detectorVersion,
    batchPrediction_eventTypeName,
    batchPrediction_failureReason,
    batchPrediction_iamRoleArn,
    batchPrediction_inputPath,
    batchPrediction_jobId,
    batchPrediction_lastHeartbeatTime,
    batchPrediction_outputPath,
    batchPrediction_processedRecordsCount,
    batchPrediction_startTime,
    batchPrediction_status,
    batchPrediction_totalRecordsCount,

    -- * DataValidationMetrics
    DataValidationMetrics (..),
    newDataValidationMetrics,
    dataValidationMetrics_fieldLevelMessages,
    dataValidationMetrics_fileLevelMessages,

    -- * Detector
    Detector (..),
    newDetector,
    detector_arn,
    detector_createdTime,
    detector_description,
    detector_detectorId,
    detector_eventTypeName,
    detector_lastUpdatedTime,

    -- * DetectorVersionSummary
    DetectorVersionSummary (..),
    newDetectorVersionSummary,
    detectorVersionSummary_description,
    detectorVersionSummary_detectorVersionId,
    detectorVersionSummary_lastUpdatedTime,
    detectorVersionSummary_status,

    -- * Entity
    Entity (..),
    newEntity,
    entity_entityType,
    entity_entityId,

    -- * EntityType
    EntityType (..),
    newEntityType,
    entityType_arn,
    entityType_createdTime,
    entityType_description,
    entityType_lastUpdatedTime,
    entityType_name,

    -- * EvaluatedExternalModel
    EvaluatedExternalModel (..),
    newEvaluatedExternalModel,
    evaluatedExternalModel_inputVariables,
    evaluatedExternalModel_modelEndpoint,
    evaluatedExternalModel_outputVariables,
    evaluatedExternalModel_useEventVariables,

    -- * EvaluatedModelVersion
    EvaluatedModelVersion (..),
    newEvaluatedModelVersion,
    evaluatedModelVersion_evaluations,
    evaluatedModelVersion_modelId,
    evaluatedModelVersion_modelType,
    evaluatedModelVersion_modelVersion,

    -- * EvaluatedRule
    EvaluatedRule (..),
    newEvaluatedRule,
    evaluatedRule_evaluated,
    evaluatedRule_expression,
    evaluatedRule_expressionWithValues,
    evaluatedRule_matched,
    evaluatedRule_outcomes,
    evaluatedRule_ruleId,
    evaluatedRule_ruleVersion,

    -- * Event
    Event (..),
    newEvent,
    event_currentLabel,
    event_entities,
    event_eventId,
    event_eventTimestamp,
    event_eventTypeName,
    event_eventVariables,
    event_labelTimestamp,

    -- * EventPredictionSummary
    EventPredictionSummary (..),
    newEventPredictionSummary,
    eventPredictionSummary_detectorId,
    eventPredictionSummary_detectorVersionId,
    eventPredictionSummary_eventId,
    eventPredictionSummary_eventTimestamp,
    eventPredictionSummary_eventTypeName,
    eventPredictionSummary_predictionTimestamp,

    -- * EventType
    EventType (..),
    newEventType,
    eventType_arn,
    eventType_createdTime,
    eventType_description,
    eventType_entityTypes,
    eventType_eventIngestion,
    eventType_eventVariables,
    eventType_ingestedEventStatistics,
    eventType_labels,
    eventType_lastUpdatedTime,
    eventType_name,

    -- * EventVariableSummary
    EventVariableSummary (..),
    newEventVariableSummary,
    eventVariableSummary_name,
    eventVariableSummary_source,
    eventVariableSummary_value,

    -- * ExternalEventsDetail
    ExternalEventsDetail (..),
    newExternalEventsDetail,
    externalEventsDetail_dataLocation,
    externalEventsDetail_dataAccessRoleArn,

    -- * ExternalModel
    ExternalModel (..),
    newExternalModel,
    externalModel_arn,
    externalModel_createdTime,
    externalModel_inputConfiguration,
    externalModel_invokeModelEndpointRoleArn,
    externalModel_lastUpdatedTime,
    externalModel_modelEndpoint,
    externalModel_modelEndpointStatus,
    externalModel_modelSource,
    externalModel_outputConfiguration,

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
    fieldValidationMessage_content,
    fieldValidationMessage_fieldName,
    fieldValidationMessage_identifier,
    fieldValidationMessage_title,
    fieldValidationMessage_type,

    -- * FileValidationMessage
    FileValidationMessage (..),
    newFileValidationMessage,
    fileValidationMessage_content,
    fileValidationMessage_title,
    fileValidationMessage_type,

    -- * FilterCondition
    FilterCondition (..),
    newFilterCondition,
    filterCondition_value,

    -- * IngestedEventStatistics
    IngestedEventStatistics (..),
    newIngestedEventStatistics,
    ingestedEventStatistics_eventDataSizeInBytes,
    ingestedEventStatistics_lastUpdatedTime,
    ingestedEventStatistics_leastRecentEvent,
    ingestedEventStatistics_mostRecentEvent,
    ingestedEventStatistics_numberOfEvents,

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
    label_arn,
    label_createdTime,
    label_description,
    label_lastUpdatedTime,
    label_name,

    -- * LabelSchema
    LabelSchema (..),
    newLabelSchema,
    labelSchema_labelMapper,
    labelSchema_unlabeledEventsTreatment,

    -- * LogOddsMetric
    LogOddsMetric (..),
    newLogOddsMetric,
    logOddsMetric_variableName,
    logOddsMetric_variableType,
    logOddsMetric_variableImportance,

    -- * MetricDataPoint
    MetricDataPoint (..),
    newMetricDataPoint,
    metricDataPoint_fpr,
    metricDataPoint_precision,
    metricDataPoint_threshold,
    metricDataPoint_tpr,

    -- * Model
    Model (..),
    newModel,
    model_arn,
    model_createdTime,
    model_description,
    model_eventTypeName,
    model_lastUpdatedTime,
    model_modelId,
    model_modelType,

    -- * ModelEndpointDataBlob
    ModelEndpointDataBlob (..),
    newModelEndpointDataBlob,
    modelEndpointDataBlob_byteBuffer,
    modelEndpointDataBlob_contentType,

    -- * ModelInputConfiguration
    ModelInputConfiguration (..),
    newModelInputConfiguration,
    modelInputConfiguration_csvInputTemplate,
    modelInputConfiguration_eventTypeName,
    modelInputConfiguration_format,
    modelInputConfiguration_jsonInputTemplate,
    modelInputConfiguration_useEventVariables,

    -- * ModelOutputConfiguration
    ModelOutputConfiguration (..),
    newModelOutputConfiguration,
    modelOutputConfiguration_csvIndexToVariableMap,
    modelOutputConfiguration_jsonKeyToVariableMap,
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
    modelVersionDetail_arn,
    modelVersionDetail_createdTime,
    modelVersionDetail_externalEventsDetail,
    modelVersionDetail_ingestedEventsDetail,
    modelVersionDetail_lastUpdatedTime,
    modelVersionDetail_modelId,
    modelVersionDetail_modelType,
    modelVersionDetail_modelVersionNumber,
    modelVersionDetail_status,
    modelVersionDetail_trainingDataSchema,
    modelVersionDetail_trainingDataSource,
    modelVersionDetail_trainingResult,
    modelVersionDetail_trainingResultV2,

    -- * ModelVersionEvaluation
    ModelVersionEvaluation (..),
    newModelVersionEvaluation,
    modelVersionEvaluation_evaluationScore,
    modelVersionEvaluation_outputVariableName,
    modelVersionEvaluation_predictionExplanations,

    -- * OFIMetricDataPoint
    OFIMetricDataPoint (..),
    newOFIMetricDataPoint,
    oFIMetricDataPoint_fpr,
    oFIMetricDataPoint_precision,
    oFIMetricDataPoint_threshold,
    oFIMetricDataPoint_tpr,

    -- * OFIModelPerformance
    OFIModelPerformance (..),
    newOFIModelPerformance,
    oFIModelPerformance_auc,

    -- * OFITrainingMetricsValue
    OFITrainingMetricsValue (..),
    newOFITrainingMetricsValue,
    oFITrainingMetricsValue_metricDataPoints,
    oFITrainingMetricsValue_modelPerformance,

    -- * Outcome
    Outcome (..),
    newOutcome,
    outcome_arn,
    outcome_createdTime,
    outcome_description,
    outcome_lastUpdatedTime,
    outcome_name,

    -- * PredictionExplanations
    PredictionExplanations (..),
    newPredictionExplanations,
    predictionExplanations_aggregatedVariablesImpactExplanations,
    predictionExplanations_variableImpactExplanations,

    -- * PredictionTimeRange
    PredictionTimeRange (..),
    newPredictionTimeRange,
    predictionTimeRange_startTime,
    predictionTimeRange_endTime,

    -- * Rule
    Rule (..),
    newRule,
    rule_detectorId,
    rule_ruleId,
    rule_ruleVersion,

    -- * RuleDetail
    RuleDetail (..),
    newRuleDetail,
    ruleDetail_arn,
    ruleDetail_createdTime,
    ruleDetail_description,
    ruleDetail_detectorId,
    ruleDetail_expression,
    ruleDetail_language,
    ruleDetail_lastUpdatedTime,
    ruleDetail_outcomes,
    ruleDetail_ruleId,
    ruleDetail_ruleVersion,

    -- * RuleResult
    RuleResult (..),
    newRuleResult,
    ruleResult_outcomes,
    ruleResult_ruleId,

    -- * TFIMetricDataPoint
    TFIMetricDataPoint (..),
    newTFIMetricDataPoint,
    tFIMetricDataPoint_fpr,
    tFIMetricDataPoint_precision,
    tFIMetricDataPoint_threshold,
    tFIMetricDataPoint_tpr,

    -- * TFIModelPerformance
    TFIModelPerformance (..),
    newTFIModelPerformance,
    tFIModelPerformance_auc,

    -- * TFITrainingMetricsValue
    TFITrainingMetricsValue (..),
    newTFITrainingMetricsValue,
    tFITrainingMetricsValue_metricDataPoints,
    tFITrainingMetricsValue_modelPerformance,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TrainingDataSchema
    TrainingDataSchema (..),
    newTrainingDataSchema,
    trainingDataSchema_labelSchema,
    trainingDataSchema_modelVariables,

    -- * TrainingMetrics
    TrainingMetrics (..),
    newTrainingMetrics,
    trainingMetrics_auc,
    trainingMetrics_metricDataPoints,

    -- * TrainingMetricsV2
    TrainingMetricsV2 (..),
    newTrainingMetricsV2,
    trainingMetricsV2_ati,
    trainingMetricsV2_ofi,
    trainingMetricsV2_tfi,

    -- * TrainingResult
    TrainingResult (..),
    newTrainingResult,
    trainingResult_dataValidationMetrics,
    trainingResult_trainingMetrics,
    trainingResult_variableImportanceMetrics,

    -- * TrainingResultV2
    TrainingResultV2 (..),
    newTrainingResultV2,
    trainingResultV2_aggregatedVariablesImportanceMetrics,
    trainingResultV2_dataValidationMetrics,
    trainingResultV2_trainingMetricsV2,
    trainingResultV2_variableImportanceMetrics,

    -- * Variable
    Variable (..),
    newVariable,
    variable_arn,
    variable_createdTime,
    variable_dataSource,
    variable_dataType,
    variable_defaultValue,
    variable_description,
    variable_lastUpdatedTime,
    variable_name,
    variable_variableType,

    -- * VariableEntry
    VariableEntry (..),
    newVariableEntry,
    variableEntry_dataSource,
    variableEntry_dataType,
    variableEntry_defaultValue,
    variableEntry_description,
    variableEntry_name,
    variableEntry_variableType,

    -- * VariableImpactExplanation
    VariableImpactExplanation (..),
    newVariableImpactExplanation,
    variableImpactExplanation_eventVariableName,
    variableImpactExplanation_logOddsImpact,
    variableImpactExplanation_relativeImpact,

    -- * VariableImportanceMetrics
    VariableImportanceMetrics (..),
    newVariableImportanceMetrics,
    variableImportanceMetrics_logOddsMetrics,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FraudDetector.Types.ATIMetricDataPoint
import Amazonka.FraudDetector.Types.ATIModelPerformance
import Amazonka.FraudDetector.Types.ATITrainingMetricsValue
import Amazonka.FraudDetector.Types.AggregatedLogOddsMetric
import Amazonka.FraudDetector.Types.AggregatedVariablesImpactExplanation
import Amazonka.FraudDetector.Types.AggregatedVariablesImportanceMetrics
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
import Amazonka.FraudDetector.Types.EvaluatedExternalModel
import Amazonka.FraudDetector.Types.EvaluatedModelVersion
import Amazonka.FraudDetector.Types.EvaluatedRule
import Amazonka.FraudDetector.Types.Event
import Amazonka.FraudDetector.Types.EventIngestion
import Amazonka.FraudDetector.Types.EventPredictionSummary
import Amazonka.FraudDetector.Types.EventType
import Amazonka.FraudDetector.Types.EventVariableSummary
import Amazonka.FraudDetector.Types.ExternalEventsDetail
import Amazonka.FraudDetector.Types.ExternalModel
import Amazonka.FraudDetector.Types.ExternalModelOutputs
import Amazonka.FraudDetector.Types.ExternalModelSummary
import Amazonka.FraudDetector.Types.FieldValidationMessage
import Amazonka.FraudDetector.Types.FileValidationMessage
import Amazonka.FraudDetector.Types.FilterCondition
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
import Amazonka.FraudDetector.Types.ModelVersionEvaluation
import Amazonka.FraudDetector.Types.ModelVersionStatus
import Amazonka.FraudDetector.Types.OFIMetricDataPoint
import Amazonka.FraudDetector.Types.OFIModelPerformance
import Amazonka.FraudDetector.Types.OFITrainingMetricsValue
import Amazonka.FraudDetector.Types.Outcome
import Amazonka.FraudDetector.Types.PredictionExplanations
import Amazonka.FraudDetector.Types.PredictionTimeRange
import Amazonka.FraudDetector.Types.Rule
import Amazonka.FraudDetector.Types.RuleDetail
import Amazonka.FraudDetector.Types.RuleExecutionMode
import Amazonka.FraudDetector.Types.RuleResult
import Amazonka.FraudDetector.Types.TFIMetricDataPoint
import Amazonka.FraudDetector.Types.TFIModelPerformance
import Amazonka.FraudDetector.Types.TFITrainingMetricsValue
import Amazonka.FraudDetector.Types.Tag
import Amazonka.FraudDetector.Types.TrainingDataSchema
import Amazonka.FraudDetector.Types.TrainingDataSourceEnum
import Amazonka.FraudDetector.Types.TrainingMetrics
import Amazonka.FraudDetector.Types.TrainingMetricsV2
import Amazonka.FraudDetector.Types.TrainingResult
import Amazonka.FraudDetector.Types.TrainingResultV2
import Amazonka.FraudDetector.Types.UnlabeledEventsTreatment
import Amazonka.FraudDetector.Types.Variable
import Amazonka.FraudDetector.Types.VariableEntry
import Amazonka.FraudDetector.Types.VariableImpactExplanation
import Amazonka.FraudDetector.Types.VariableImportanceMetrics
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-11-15@ of the Amazon Fraud Detector SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "FraudDetector",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "frauddetector",
      Core.signingName = "frauddetector",
      Core.version = "2019-11-15",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "FraudDetector",
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
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | An exception indicating Amazon Fraud Detector does not have the needed
-- permissions. This can occur if you submit a request, such as
-- @PutExternalModel@, that specifies a role that is not in your account.
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | An exception indicating there was a conflict during a delete operation.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | An exception indicating an internal server error.
_InternalServerException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | An exception indicating the specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | An exception indicating that the attached customer-owned (external)
-- model threw an exception when Amazon Fraud Detector invoked the model.
_ResourceUnavailableException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- | An exception indicating a throttling error.
_ThrottlingException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | An exception indicating a specified value is not allowed.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
