{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FraudDetector.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceUnavailableException,
    _AccessDeniedException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ConflictException,
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
    aTIMetricDataPoint_cr,
    aTIMetricDataPoint_adr,
    aTIMetricDataPoint_atodr,
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
    aggregatedVariablesImpactExplanation_logOddsImpact,
    aggregatedVariablesImpactExplanation_relativeImpact,
    aggregatedVariablesImpactExplanation_eventVariableNames,

    -- * AggregatedVariablesImportanceMetrics
    AggregatedVariablesImportanceMetrics (..),
    newAggregatedVariablesImportanceMetrics,
    aggregatedVariablesImportanceMetrics_logOddsMetrics,

    -- * BatchCreateVariableError
    BatchCreateVariableError (..),
    newBatchCreateVariableError,
    batchCreateVariableError_message,
    batchCreateVariableError_name,
    batchCreateVariableError_code,

    -- * BatchGetVariableError
    BatchGetVariableError (..),
    newBatchGetVariableError,
    batchGetVariableError_message,
    batchGetVariableError_name,
    batchGetVariableError_code,

    -- * BatchImport
    BatchImport (..),
    newBatchImport,
    batchImport_inputPath,
    batchImport_arn,
    batchImport_jobId,
    batchImport_status,
    batchImport_completionTime,
    batchImport_outputPath,
    batchImport_iamRoleArn,
    batchImport_totalRecordsCount,
    batchImport_eventTypeName,
    batchImport_failedRecordsCount,
    batchImport_processedRecordsCount,
    batchImport_startTime,
    batchImport_failureReason,

    -- * BatchPrediction
    BatchPrediction (..),
    newBatchPrediction,
    batchPrediction_inputPath,
    batchPrediction_detectorVersion,
    batchPrediction_arn,
    batchPrediction_jobId,
    batchPrediction_detectorName,
    batchPrediction_status,
    batchPrediction_completionTime,
    batchPrediction_outputPath,
    batchPrediction_iamRoleArn,
    batchPrediction_lastHeartbeatTime,
    batchPrediction_totalRecordsCount,
    batchPrediction_eventTypeName,
    batchPrediction_processedRecordsCount,
    batchPrediction_startTime,
    batchPrediction_failureReason,

    -- * DataValidationMetrics
    DataValidationMetrics (..),
    newDataValidationMetrics,
    dataValidationMetrics_fieldLevelMessages,
    dataValidationMetrics_fileLevelMessages,

    -- * Detector
    Detector (..),
    newDetector,
    detector_createdTime,
    detector_arn,
    detector_description,
    detector_lastUpdatedTime,
    detector_eventTypeName,
    detector_detectorId,

    -- * DetectorVersionSummary
    DetectorVersionSummary (..),
    newDetectorVersionSummary,
    detectorVersionSummary_detectorVersionId,
    detectorVersionSummary_status,
    detectorVersionSummary_description,
    detectorVersionSummary_lastUpdatedTime,

    -- * Entity
    Entity (..),
    newEntity,
    entity_entityType,
    entity_entityId,

    -- * EntityType
    EntityType (..),
    newEntityType,
    entityType_name,
    entityType_createdTime,
    entityType_arn,
    entityType_description,
    entityType_lastUpdatedTime,

    -- * EvaluatedExternalModel
    EvaluatedExternalModel (..),
    newEvaluatedExternalModel,
    evaluatedExternalModel_inputVariables,
    evaluatedExternalModel_modelEndpoint,
    evaluatedExternalModel_useEventVariables,
    evaluatedExternalModel_outputVariables,

    -- * EvaluatedModelVersion
    EvaluatedModelVersion (..),
    newEvaluatedModelVersion,
    evaluatedModelVersion_evaluations,
    evaluatedModelVersion_modelVersion,
    evaluatedModelVersion_modelType,
    evaluatedModelVersion_modelId,

    -- * EvaluatedRule
    EvaluatedRule (..),
    newEvaluatedRule,
    evaluatedRule_ruleVersion,
    evaluatedRule_expressionWithValues,
    evaluatedRule_matched,
    evaluatedRule_ruleId,
    evaluatedRule_expression,
    evaluatedRule_outcomes,
    evaluatedRule_evaluated,

    -- * Event
    Event (..),
    newEvent,
    event_entities,
    event_labelTimestamp,
    event_eventTimestamp,
    event_eventId,
    event_eventTypeName,
    event_currentLabel,
    event_eventVariables,

    -- * EventPredictionSummary
    EventPredictionSummary (..),
    newEventPredictionSummary,
    eventPredictionSummary_eventTimestamp,
    eventPredictionSummary_detectorVersionId,
    eventPredictionSummary_eventId,
    eventPredictionSummary_predictionTimestamp,
    eventPredictionSummary_eventTypeName,
    eventPredictionSummary_detectorId,

    -- * EventType
    EventType (..),
    newEventType,
    eventType_name,
    eventType_createdTime,
    eventType_entityTypes,
    eventType_arn,
    eventType_description,
    eventType_lastUpdatedTime,
    eventType_ingestedEventStatistics,
    eventType_labels,
    eventType_eventIngestion,
    eventType_eventVariables,

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
    externalModel_outputConfiguration,
    externalModel_createdTime,
    externalModel_arn,
    externalModel_inputConfiguration,
    externalModel_modelEndpointStatus,
    externalModel_lastUpdatedTime,
    externalModel_modelEndpoint,
    externalModel_modelSource,
    externalModel_invokeModelEndpointRoleArn,

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
    fieldValidationMessage_type,
    fieldValidationMessage_fieldName,
    fieldValidationMessage_title,
    fieldValidationMessage_identifier,
    fieldValidationMessage_content,

    -- * FileValidationMessage
    FileValidationMessage (..),
    newFileValidationMessage,
    fileValidationMessage_type,
    fileValidationMessage_title,
    fileValidationMessage_content,

    -- * FilterCondition
    FilterCondition (..),
    newFilterCondition,
    filterCondition_value,

    -- * IngestedEventStatistics
    IngestedEventStatistics (..),
    newIngestedEventStatistics,
    ingestedEventStatistics_eventDataSizeInBytes,
    ingestedEventStatistics_lastUpdatedTime,
    ingestedEventStatistics_numberOfEvents,
    ingestedEventStatistics_mostRecentEvent,
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
    label_name,
    label_createdTime,
    label_arn,
    label_description,
    label_lastUpdatedTime,

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
    metricDataPoint_tpr,
    metricDataPoint_fpr,
    metricDataPoint_precision,
    metricDataPoint_threshold,

    -- * Model
    Model (..),
    newModel,
    model_createdTime,
    model_arn,
    model_description,
    model_lastUpdatedTime,
    model_modelType,
    model_modelId,
    model_eventTypeName,

    -- * ModelEndpointDataBlob
    ModelEndpointDataBlob (..),
    newModelEndpointDataBlob,
    modelEndpointDataBlob_byteBuffer,
    modelEndpointDataBlob_contentType,

    -- * ModelInputConfiguration
    ModelInputConfiguration (..),
    newModelInputConfiguration,
    modelInputConfiguration_jsonInputTemplate,
    modelInputConfiguration_format,
    modelInputConfiguration_csvInputTemplate,
    modelInputConfiguration_eventTypeName,
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
    modelVersionDetail_createdTime,
    modelVersionDetail_ingestedEventsDetail,
    modelVersionDetail_trainingResultV2,
    modelVersionDetail_modelVersionNumber,
    modelVersionDetail_arn,
    modelVersionDetail_status,
    modelVersionDetail_lastUpdatedTime,
    modelVersionDetail_modelType,
    modelVersionDetail_trainingDataSchema,
    modelVersionDetail_trainingResult,
    modelVersionDetail_externalEventsDetail,
    modelVersionDetail_trainingDataSource,
    modelVersionDetail_modelId,

    -- * ModelVersionEvaluation
    ModelVersionEvaluation (..),
    newModelVersionEvaluation,
    modelVersionEvaluation_outputVariableName,
    modelVersionEvaluation_predictionExplanations,
    modelVersionEvaluation_evaluationScore,

    -- * OFIMetricDataPoint
    OFIMetricDataPoint (..),
    newOFIMetricDataPoint,
    oFIMetricDataPoint_tpr,
    oFIMetricDataPoint_fpr,
    oFIMetricDataPoint_precision,
    oFIMetricDataPoint_threshold,

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
    outcome_name,
    outcome_createdTime,
    outcome_arn,
    outcome_description,
    outcome_lastUpdatedTime,

    -- * PredictionExplanations
    PredictionExplanations (..),
    newPredictionExplanations,
    predictionExplanations_variableImpactExplanations,
    predictionExplanations_aggregatedVariablesImpactExplanations,

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
    ruleDetail_ruleVersion,
    ruleDetail_createdTime,
    ruleDetail_ruleId,
    ruleDetail_arn,
    ruleDetail_description,
    ruleDetail_lastUpdatedTime,
    ruleDetail_expression,
    ruleDetail_outcomes,
    ruleDetail_detectorId,
    ruleDetail_language,

    -- * RuleResult
    RuleResult (..),
    newRuleResult,
    ruleResult_ruleId,
    ruleResult_outcomes,

    -- * TFIMetricDataPoint
    TFIMetricDataPoint (..),
    newTFIMetricDataPoint,
    tFIMetricDataPoint_tpr,
    tFIMetricDataPoint_fpr,
    tFIMetricDataPoint_precision,
    tFIMetricDataPoint_threshold,

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
    trainingMetrics_metricDataPoints,
    trainingMetrics_auc,

    -- * TrainingMetricsV2
    TrainingMetricsV2 (..),
    newTrainingMetricsV2,
    trainingMetricsV2_ati,
    trainingMetricsV2_tfi,
    trainingMetricsV2_ofi,

    -- * TrainingResult
    TrainingResult (..),
    newTrainingResult,
    trainingResult_variableImportanceMetrics,
    trainingResult_dataValidationMetrics,
    trainingResult_trainingMetrics,

    -- * TrainingResultV2
    TrainingResultV2 (..),
    newTrainingResultV2,
    trainingResultV2_trainingMetricsV2,
    trainingResultV2_variableImportanceMetrics,
    trainingResultV2_dataValidationMetrics,
    trainingResultV2_aggregatedVariablesImportanceMetrics,

    -- * Variable
    Variable (..),
    newVariable,
    variable_name,
    variable_createdTime,
    variable_variableType,
    variable_arn,
    variable_defaultValue,
    variable_description,
    variable_lastUpdatedTime,
    variable_dataSource,
    variable_dataType,

    -- * VariableEntry
    VariableEntry (..),
    newVariableEntry,
    variableEntry_name,
    variableEntry_variableType,
    variableEntry_defaultValue,
    variableEntry_description,
    variableEntry_dataSource,
    variableEntry_dataType,

    -- * VariableImpactExplanation
    VariableImpactExplanation (..),
    newVariableImpactExplanation,
    variableImpactExplanation_logOddsImpact,
    variableImpactExplanation_eventVariableName,
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

-- | An exception indicating that the attached customer-owned (external)
-- model threw an exception when Amazon Fraud Detector invoked the model.
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- | An exception indicating Amazon Fraud Detector does not have the needed
-- permissions. This can occur if you submit a request, such as
-- @PutExternalModel@, that specifies a role that is not in your account.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

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

-- | An exception indicating a specified value is not allowed.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
