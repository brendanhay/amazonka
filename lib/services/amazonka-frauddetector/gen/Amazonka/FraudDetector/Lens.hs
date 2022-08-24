{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FraudDetector.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Lens
  ( -- * Operations

    -- ** BatchCreateVariable
    batchCreateVariable_tags,
    batchCreateVariable_variableEntries,
    batchCreateVariableResponse_errors,
    batchCreateVariableResponse_httpStatus,

    -- ** BatchGetVariable
    batchGetVariable_names,
    batchGetVariableResponse_errors,
    batchGetVariableResponse_variables,
    batchGetVariableResponse_httpStatus,

    -- ** CancelBatchImportJob
    cancelBatchImportJob_jobId,
    cancelBatchImportJobResponse_httpStatus,

    -- ** CancelBatchPredictionJob
    cancelBatchPredictionJob_jobId,
    cancelBatchPredictionJobResponse_httpStatus,

    -- ** CreateBatchImportJob
    createBatchImportJob_tags,
    createBatchImportJob_jobId,
    createBatchImportJob_inputPath,
    createBatchImportJob_outputPath,
    createBatchImportJob_eventTypeName,
    createBatchImportJob_iamRoleArn,
    createBatchImportJobResponse_httpStatus,

    -- ** CreateBatchPredictionJob
    createBatchPredictionJob_tags,
    createBatchPredictionJob_detectorVersion,
    createBatchPredictionJob_jobId,
    createBatchPredictionJob_inputPath,
    createBatchPredictionJob_outputPath,
    createBatchPredictionJob_eventTypeName,
    createBatchPredictionJob_detectorName,
    createBatchPredictionJob_iamRoleArn,
    createBatchPredictionJobResponse_httpStatus,

    -- ** CreateDetectorVersion
    createDetectorVersion_tags,
    createDetectorVersion_modelVersions,
    createDetectorVersion_description,
    createDetectorVersion_externalModelEndpoints,
    createDetectorVersion_ruleExecutionMode,
    createDetectorVersion_detectorId,
    createDetectorVersion_rules,
    createDetectorVersionResponse_detectorVersionId,
    createDetectorVersionResponse_status,
    createDetectorVersionResponse_detectorId,
    createDetectorVersionResponse_httpStatus,

    -- ** CreateModel
    createModel_tags,
    createModel_description,
    createModel_modelId,
    createModel_modelType,
    createModel_eventTypeName,
    createModelResponse_httpStatus,

    -- ** CreateModelVersion
    createModelVersion_tags,
    createModelVersion_ingestedEventsDetail,
    createModelVersion_externalEventsDetail,
    createModelVersion_modelId,
    createModelVersion_modelType,
    createModelVersion_trainingDataSource,
    createModelVersion_trainingDataSchema,
    createModelVersionResponse_modelVersionNumber,
    createModelVersionResponse_status,
    createModelVersionResponse_modelType,
    createModelVersionResponse_modelId,
    createModelVersionResponse_httpStatus,

    -- ** CreateRule
    createRule_tags,
    createRule_description,
    createRule_ruleId,
    createRule_detectorId,
    createRule_expression,
    createRule_language,
    createRule_outcomes,
    createRuleResponse_rule,
    createRuleResponse_httpStatus,

    -- ** CreateVariable
    createVariable_tags,
    createVariable_variableType,
    createVariable_description,
    createVariable_name,
    createVariable_dataType,
    createVariable_dataSource,
    createVariable_defaultValue,
    createVariableResponse_httpStatus,

    -- ** DeleteBatchImportJob
    deleteBatchImportJob_jobId,
    deleteBatchImportJobResponse_httpStatus,

    -- ** DeleteBatchPredictionJob
    deleteBatchPredictionJob_jobId,
    deleteBatchPredictionJobResponse_httpStatus,

    -- ** DeleteDetector
    deleteDetector_detectorId,
    deleteDetectorResponse_httpStatus,

    -- ** DeleteDetectorVersion
    deleteDetectorVersion_detectorId,
    deleteDetectorVersion_detectorVersionId,
    deleteDetectorVersionResponse_httpStatus,

    -- ** DeleteEntityType
    deleteEntityType_name,
    deleteEntityTypeResponse_httpStatus,

    -- ** DeleteEvent
    deleteEvent_deleteAuditHistory,
    deleteEvent_eventId,
    deleteEvent_eventTypeName,
    deleteEventResponse_httpStatus,

    -- ** DeleteEventType
    deleteEventType_name,
    deleteEventTypeResponse_httpStatus,

    -- ** DeleteEventsByEventType
    deleteEventsByEventType_eventTypeName,
    deleteEventsByEventTypeResponse_eventsDeletionStatus,
    deleteEventsByEventTypeResponse_eventTypeName,
    deleteEventsByEventTypeResponse_httpStatus,

    -- ** DeleteExternalModel
    deleteExternalModel_modelEndpoint,
    deleteExternalModelResponse_httpStatus,

    -- ** DeleteLabel
    deleteLabel_name,
    deleteLabelResponse_httpStatus,

    -- ** DeleteModel
    deleteModel_modelId,
    deleteModel_modelType,
    deleteModelResponse_httpStatus,

    -- ** DeleteModelVersion
    deleteModelVersion_modelId,
    deleteModelVersion_modelType,
    deleteModelVersion_modelVersionNumber,
    deleteModelVersionResponse_httpStatus,

    -- ** DeleteOutcome
    deleteOutcome_name,
    deleteOutcomeResponse_httpStatus,

    -- ** DeleteRule
    deleteRule_rule,
    deleteRuleResponse_httpStatus,

    -- ** DeleteVariable
    deleteVariable_name,
    deleteVariableResponse_httpStatus,

    -- ** DescribeDetector
    describeDetector_nextToken,
    describeDetector_maxResults,
    describeDetector_detectorId,
    describeDetectorResponse_nextToken,
    describeDetectorResponse_arn,
    describeDetectorResponse_detectorVersionSummaries,
    describeDetectorResponse_detectorId,
    describeDetectorResponse_httpStatus,

    -- ** DescribeModelVersions
    describeModelVersions_nextToken,
    describeModelVersions_modelVersionNumber,
    describeModelVersions_modelType,
    describeModelVersions_maxResults,
    describeModelVersions_modelId,
    describeModelVersionsResponse_nextToken,
    describeModelVersionsResponse_modelVersionDetails,
    describeModelVersionsResponse_httpStatus,

    -- ** GetBatchImportJobs
    getBatchImportJobs_nextToken,
    getBatchImportJobs_jobId,
    getBatchImportJobs_maxResults,
    getBatchImportJobsResponse_nextToken,
    getBatchImportJobsResponse_batchImports,
    getBatchImportJobsResponse_httpStatus,

    -- ** GetBatchPredictionJobs
    getBatchPredictionJobs_nextToken,
    getBatchPredictionJobs_jobId,
    getBatchPredictionJobs_maxResults,
    getBatchPredictionJobsResponse_nextToken,
    getBatchPredictionJobsResponse_batchPredictions,
    getBatchPredictionJobsResponse_httpStatus,

    -- ** GetDeleteEventsByEventTypeStatus
    getDeleteEventsByEventTypeStatus_eventTypeName,
    getDeleteEventsByEventTypeStatusResponse_eventsDeletionStatus,
    getDeleteEventsByEventTypeStatusResponse_eventTypeName,
    getDeleteEventsByEventTypeStatusResponse_httpStatus,

    -- ** GetDetectorVersion
    getDetectorVersion_detectorId,
    getDetectorVersion_detectorVersionId,
    getDetectorVersionResponse_createdTime,
    getDetectorVersionResponse_detectorVersionId,
    getDetectorVersionResponse_modelVersions,
    getDetectorVersionResponse_rules,
    getDetectorVersionResponse_arn,
    getDetectorVersionResponse_status,
    getDetectorVersionResponse_description,
    getDetectorVersionResponse_lastUpdatedTime,
    getDetectorVersionResponse_externalModelEndpoints,
    getDetectorVersionResponse_ruleExecutionMode,
    getDetectorVersionResponse_detectorId,
    getDetectorVersionResponse_httpStatus,

    -- ** GetDetectors
    getDetectors_nextToken,
    getDetectors_maxResults,
    getDetectors_detectorId,
    getDetectorsResponse_detectors,
    getDetectorsResponse_nextToken,
    getDetectorsResponse_httpStatus,

    -- ** GetEntityTypes
    getEntityTypes_name,
    getEntityTypes_nextToken,
    getEntityTypes_maxResults,
    getEntityTypesResponse_nextToken,
    getEntityTypesResponse_entityTypes,
    getEntityTypesResponse_httpStatus,

    -- ** GetEvent
    getEvent_eventId,
    getEvent_eventTypeName,
    getEventResponse_event,
    getEventResponse_httpStatus,

    -- ** GetEventPrediction
    getEventPrediction_detectorVersionId,
    getEventPrediction_externalModelEndpointDataBlobs,
    getEventPrediction_detectorId,
    getEventPrediction_eventId,
    getEventPrediction_eventTypeName,
    getEventPrediction_entities,
    getEventPrediction_eventTimestamp,
    getEventPrediction_eventVariables,
    getEventPredictionResponse_modelScores,
    getEventPredictionResponse_externalModelOutputs,
    getEventPredictionResponse_ruleResults,
    getEventPredictionResponse_httpStatus,

    -- ** GetEventPredictionMetadata
    getEventPredictionMetadata_eventId,
    getEventPredictionMetadata_eventTypeName,
    getEventPredictionMetadata_detectorId,
    getEventPredictionMetadata_detectorVersionId,
    getEventPredictionMetadata_predictionTimestamp,
    getEventPredictionMetadataResponse_entityId,
    getEventPredictionMetadataResponse_eventTimestamp,
    getEventPredictionMetadataResponse_detectorVersionId,
    getEventPredictionMetadataResponse_rules,
    getEventPredictionMetadataResponse_evaluatedExternalModels,
    getEventPredictionMetadataResponse_detectorVersionStatus,
    getEventPredictionMetadataResponse_evaluatedModelVersions,
    getEventPredictionMetadataResponse_eventId,
    getEventPredictionMetadataResponse_outcomes,
    getEventPredictionMetadataResponse_entityType,
    getEventPredictionMetadataResponse_predictionTimestamp,
    getEventPredictionMetadataResponse_ruleExecutionMode,
    getEventPredictionMetadataResponse_eventTypeName,
    getEventPredictionMetadataResponse_detectorId,
    getEventPredictionMetadataResponse_eventVariables,
    getEventPredictionMetadataResponse_httpStatus,

    -- ** GetEventTypes
    getEventTypes_name,
    getEventTypes_nextToken,
    getEventTypes_maxResults,
    getEventTypesResponse_nextToken,
    getEventTypesResponse_eventTypes,
    getEventTypesResponse_httpStatus,

    -- ** GetExternalModels
    getExternalModels_nextToken,
    getExternalModels_modelEndpoint,
    getExternalModels_maxResults,
    getExternalModelsResponse_nextToken,
    getExternalModelsResponse_externalModels,
    getExternalModelsResponse_httpStatus,

    -- ** GetKMSEncryptionKey
    getKMSEncryptionKeyResponse_kmsKey,
    getKMSEncryptionKeyResponse_httpStatus,

    -- ** GetLabels
    getLabels_name,
    getLabels_nextToken,
    getLabels_maxResults,
    getLabelsResponse_nextToken,
    getLabelsResponse_labels,
    getLabelsResponse_httpStatus,

    -- ** GetModelVersion
    getModelVersion_modelId,
    getModelVersion_modelType,
    getModelVersion_modelVersionNumber,
    getModelVersionResponse_ingestedEventsDetail,
    getModelVersionResponse_modelVersionNumber,
    getModelVersionResponse_arn,
    getModelVersionResponse_status,
    getModelVersionResponse_modelType,
    getModelVersionResponse_trainingDataSchema,
    getModelVersionResponse_externalEventsDetail,
    getModelVersionResponse_trainingDataSource,
    getModelVersionResponse_modelId,
    getModelVersionResponse_httpStatus,

    -- ** GetModels
    getModels_nextToken,
    getModels_modelType,
    getModels_maxResults,
    getModels_modelId,
    getModelsResponse_nextToken,
    getModelsResponse_models,
    getModelsResponse_httpStatus,

    -- ** GetOutcomes
    getOutcomes_name,
    getOutcomes_nextToken,
    getOutcomes_maxResults,
    getOutcomesResponse_nextToken,
    getOutcomesResponse_outcomes,
    getOutcomesResponse_httpStatus,

    -- ** GetRules
    getRules_ruleVersion,
    getRules_nextToken,
    getRules_ruleId,
    getRules_maxResults,
    getRules_detectorId,
    getRulesResponse_ruleDetails,
    getRulesResponse_nextToken,
    getRulesResponse_httpStatus,

    -- ** GetVariables
    getVariables_name,
    getVariables_nextToken,
    getVariables_maxResults,
    getVariablesResponse_nextToken,
    getVariablesResponse_variables,
    getVariablesResponse_httpStatus,

    -- ** ListEventPredictions
    listEventPredictions_eventType,
    listEventPredictions_nextToken,
    listEventPredictions_detectorVersionId,
    listEventPredictions_predictionTimeRange,
    listEventPredictions_maxResults,
    listEventPredictions_eventId,
    listEventPredictions_detectorId,
    listEventPredictionsResponse_eventPredictionSummaries,
    listEventPredictionsResponse_nextToken,
    listEventPredictionsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,

    -- ** PutDetector
    putDetector_tags,
    putDetector_description,
    putDetector_detectorId,
    putDetector_eventTypeName,
    putDetectorResponse_httpStatus,

    -- ** PutEntityType
    putEntityType_tags,
    putEntityType_description,
    putEntityType_name,
    putEntityTypeResponse_httpStatus,

    -- ** PutEventType
    putEventType_tags,
    putEventType_description,
    putEventType_labels,
    putEventType_eventIngestion,
    putEventType_name,
    putEventType_eventVariables,
    putEventType_entityTypes,
    putEventTypeResponse_httpStatus,

    -- ** PutExternalModel
    putExternalModel_tags,
    putExternalModel_modelEndpoint,
    putExternalModel_modelSource,
    putExternalModel_invokeModelEndpointRoleArn,
    putExternalModel_inputConfiguration,
    putExternalModel_outputConfiguration,
    putExternalModel_modelEndpointStatus,
    putExternalModelResponse_httpStatus,

    -- ** PutKMSEncryptionKey
    putKMSEncryptionKey_kmsEncryptionKeyArn,
    putKMSEncryptionKeyResponse_httpStatus,

    -- ** PutLabel
    putLabel_tags,
    putLabel_description,
    putLabel_name,
    putLabelResponse_httpStatus,

    -- ** PutOutcome
    putOutcome_tags,
    putOutcome_description,
    putOutcome_name,
    putOutcomeResponse_httpStatus,

    -- ** SendEvent
    sendEvent_labelTimestamp,
    sendEvent_assignedLabel,
    sendEvent_eventId,
    sendEvent_eventTypeName,
    sendEvent_eventTimestamp,
    sendEvent_eventVariables,
    sendEvent_entities,
    sendEventResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateDetectorVersion
    updateDetectorVersion_modelVersions,
    updateDetectorVersion_description,
    updateDetectorVersion_ruleExecutionMode,
    updateDetectorVersion_detectorId,
    updateDetectorVersion_detectorVersionId,
    updateDetectorVersion_externalModelEndpoints,
    updateDetectorVersion_rules,
    updateDetectorVersionResponse_httpStatus,

    -- ** UpdateDetectorVersionMetadata
    updateDetectorVersionMetadata_detectorId,
    updateDetectorVersionMetadata_detectorVersionId,
    updateDetectorVersionMetadata_description,
    updateDetectorVersionMetadataResponse_httpStatus,

    -- ** UpdateDetectorVersionStatus
    updateDetectorVersionStatus_detectorId,
    updateDetectorVersionStatus_detectorVersionId,
    updateDetectorVersionStatus_status,
    updateDetectorVersionStatusResponse_httpStatus,

    -- ** UpdateEventLabel
    updateEventLabel_eventId,
    updateEventLabel_eventTypeName,
    updateEventLabel_assignedLabel,
    updateEventLabel_labelTimestamp,
    updateEventLabelResponse_httpStatus,

    -- ** UpdateModel
    updateModel_description,
    updateModel_modelId,
    updateModel_modelType,
    updateModelResponse_httpStatus,

    -- ** UpdateModelVersion
    updateModelVersion_tags,
    updateModelVersion_ingestedEventsDetail,
    updateModelVersion_externalEventsDetail,
    updateModelVersion_modelId,
    updateModelVersion_modelType,
    updateModelVersion_majorVersionNumber,
    updateModelVersionResponse_modelVersionNumber,
    updateModelVersionResponse_status,
    updateModelVersionResponse_modelType,
    updateModelVersionResponse_modelId,
    updateModelVersionResponse_httpStatus,

    -- ** UpdateModelVersionStatus
    updateModelVersionStatus_modelId,
    updateModelVersionStatus_modelType,
    updateModelVersionStatus_modelVersionNumber,
    updateModelVersionStatus_status,
    updateModelVersionStatusResponse_httpStatus,

    -- ** UpdateRuleMetadata
    updateRuleMetadata_rule,
    updateRuleMetadata_description,
    updateRuleMetadataResponse_httpStatus,

    -- ** UpdateRuleVersion
    updateRuleVersion_tags,
    updateRuleVersion_description,
    updateRuleVersion_rule,
    updateRuleVersion_expression,
    updateRuleVersion_language,
    updateRuleVersion_outcomes,
    updateRuleVersionResponse_rule,
    updateRuleVersionResponse_httpStatus,

    -- ** UpdateVariable
    updateVariable_variableType,
    updateVariable_defaultValue,
    updateVariable_description,
    updateVariable_name,
    updateVariableResponse_httpStatus,

    -- * Types

    -- ** ATIMetricDataPoint
    aTIMetricDataPoint_cr,
    aTIMetricDataPoint_adr,
    aTIMetricDataPoint_atodr,
    aTIMetricDataPoint_threshold,

    -- ** ATIModelPerformance
    aTIModelPerformance_asi,

    -- ** ATITrainingMetricsValue
    aTITrainingMetricsValue_metricDataPoints,
    aTITrainingMetricsValue_modelPerformance,

    -- ** AggregatedLogOddsMetric
    aggregatedLogOddsMetric_variableNames,
    aggregatedLogOddsMetric_aggregatedVariablesImportance,

    -- ** AggregatedVariablesImpactExplanation
    aggregatedVariablesImpactExplanation_logOddsImpact,
    aggregatedVariablesImpactExplanation_relativeImpact,
    aggregatedVariablesImpactExplanation_eventVariableNames,

    -- ** AggregatedVariablesImportanceMetrics
    aggregatedVariablesImportanceMetrics_logOddsMetrics,

    -- ** BatchCreateVariableError
    batchCreateVariableError_message,
    batchCreateVariableError_name,
    batchCreateVariableError_code,

    -- ** BatchGetVariableError
    batchGetVariableError_message,
    batchGetVariableError_name,
    batchGetVariableError_code,

    -- ** BatchImport
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

    -- ** BatchPrediction
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

    -- ** DataValidationMetrics
    dataValidationMetrics_fieldLevelMessages,
    dataValidationMetrics_fileLevelMessages,

    -- ** Detector
    detector_createdTime,
    detector_arn,
    detector_description,
    detector_lastUpdatedTime,
    detector_eventTypeName,
    detector_detectorId,

    -- ** DetectorVersionSummary
    detectorVersionSummary_detectorVersionId,
    detectorVersionSummary_status,
    detectorVersionSummary_description,
    detectorVersionSummary_lastUpdatedTime,

    -- ** Entity
    entity_entityType,
    entity_entityId,

    -- ** EntityType
    entityType_name,
    entityType_createdTime,
    entityType_arn,
    entityType_description,
    entityType_lastUpdatedTime,

    -- ** EvaluatedExternalModel
    evaluatedExternalModel_inputVariables,
    evaluatedExternalModel_modelEndpoint,
    evaluatedExternalModel_useEventVariables,
    evaluatedExternalModel_outputVariables,

    -- ** EvaluatedModelVersion
    evaluatedModelVersion_evaluations,
    evaluatedModelVersion_modelVersion,
    evaluatedModelVersion_modelType,
    evaluatedModelVersion_modelId,

    -- ** EvaluatedRule
    evaluatedRule_ruleVersion,
    evaluatedRule_expressionWithValues,
    evaluatedRule_matched,
    evaluatedRule_ruleId,
    evaluatedRule_expression,
    evaluatedRule_outcomes,
    evaluatedRule_evaluated,

    -- ** Event
    event_entities,
    event_labelTimestamp,
    event_eventTimestamp,
    event_eventId,
    event_eventTypeName,
    event_currentLabel,
    event_eventVariables,

    -- ** EventPredictionSummary
    eventPredictionSummary_eventTimestamp,
    eventPredictionSummary_detectorVersionId,
    eventPredictionSummary_eventId,
    eventPredictionSummary_predictionTimestamp,
    eventPredictionSummary_eventTypeName,
    eventPredictionSummary_detectorId,

    -- ** EventType
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

    -- ** EventVariableSummary
    eventVariableSummary_name,
    eventVariableSummary_source,
    eventVariableSummary_value,

    -- ** ExternalEventsDetail
    externalEventsDetail_dataLocation,
    externalEventsDetail_dataAccessRoleArn,

    -- ** ExternalModel
    externalModel_outputConfiguration,
    externalModel_createdTime,
    externalModel_arn,
    externalModel_inputConfiguration,
    externalModel_modelEndpointStatus,
    externalModel_lastUpdatedTime,
    externalModel_modelEndpoint,
    externalModel_modelSource,
    externalModel_invokeModelEndpointRoleArn,

    -- ** ExternalModelOutputs
    externalModelOutputs_externalModel,
    externalModelOutputs_outputs,

    -- ** ExternalModelSummary
    externalModelSummary_modelEndpoint,
    externalModelSummary_modelSource,

    -- ** FieldValidationMessage
    fieldValidationMessage_type,
    fieldValidationMessage_fieldName,
    fieldValidationMessage_title,
    fieldValidationMessage_identifier,
    fieldValidationMessage_content,

    -- ** FileValidationMessage
    fileValidationMessage_type,
    fileValidationMessage_title,
    fileValidationMessage_content,

    -- ** FilterCondition
    filterCondition_value,

    -- ** IngestedEventStatistics
    ingestedEventStatistics_eventDataSizeInBytes,
    ingestedEventStatistics_lastUpdatedTime,
    ingestedEventStatistics_numberOfEvents,
    ingestedEventStatistics_mostRecentEvent,
    ingestedEventStatistics_leastRecentEvent,

    -- ** IngestedEventsDetail
    ingestedEventsDetail_ingestedEventsTimeWindow,

    -- ** IngestedEventsTimeWindow
    ingestedEventsTimeWindow_startTime,
    ingestedEventsTimeWindow_endTime,

    -- ** KMSKey
    kmsKey_kmsEncryptionKeyArn,

    -- ** Label
    label_name,
    label_createdTime,
    label_arn,
    label_description,
    label_lastUpdatedTime,

    -- ** LabelSchema
    labelSchema_labelMapper,
    labelSchema_unlabeledEventsTreatment,

    -- ** LogOddsMetric
    logOddsMetric_variableName,
    logOddsMetric_variableType,
    logOddsMetric_variableImportance,

    -- ** MetricDataPoint
    metricDataPoint_tpr,
    metricDataPoint_fpr,
    metricDataPoint_precision,
    metricDataPoint_threshold,

    -- ** Model
    model_createdTime,
    model_arn,
    model_description,
    model_lastUpdatedTime,
    model_modelType,
    model_modelId,
    model_eventTypeName,

    -- ** ModelEndpointDataBlob
    modelEndpointDataBlob_byteBuffer,
    modelEndpointDataBlob_contentType,

    -- ** ModelInputConfiguration
    modelInputConfiguration_jsonInputTemplate,
    modelInputConfiguration_format,
    modelInputConfiguration_csvInputTemplate,
    modelInputConfiguration_eventTypeName,
    modelInputConfiguration_useEventVariables,

    -- ** ModelOutputConfiguration
    modelOutputConfiguration_jsonKeyToVariableMap,
    modelOutputConfiguration_csvIndexToVariableMap,
    modelOutputConfiguration_format,

    -- ** ModelScores
    modelScores_modelVersion,
    modelScores_scores,

    -- ** ModelVersion
    modelVersion_arn,
    modelVersion_modelId,
    modelVersion_modelType,
    modelVersion_modelVersionNumber,

    -- ** ModelVersionDetail
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

    -- ** ModelVersionEvaluation
    modelVersionEvaluation_outputVariableName,
    modelVersionEvaluation_predictionExplanations,
    modelVersionEvaluation_evaluationScore,

    -- ** OFIMetricDataPoint
    oFIMetricDataPoint_tpr,
    oFIMetricDataPoint_fpr,
    oFIMetricDataPoint_precision,
    oFIMetricDataPoint_threshold,

    -- ** OFIModelPerformance
    oFIModelPerformance_auc,

    -- ** OFITrainingMetricsValue
    oFITrainingMetricsValue_metricDataPoints,
    oFITrainingMetricsValue_modelPerformance,

    -- ** Outcome
    outcome_name,
    outcome_createdTime,
    outcome_arn,
    outcome_description,
    outcome_lastUpdatedTime,

    -- ** PredictionExplanations
    predictionExplanations_variableImpactExplanations,
    predictionExplanations_aggregatedVariablesImpactExplanations,

    -- ** PredictionTimeRange
    predictionTimeRange_startTime,
    predictionTimeRange_endTime,

    -- ** Rule
    rule_detectorId,
    rule_ruleId,
    rule_ruleVersion,

    -- ** RuleDetail
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

    -- ** RuleResult
    ruleResult_ruleId,
    ruleResult_outcomes,

    -- ** TFIMetricDataPoint
    tFIMetricDataPoint_tpr,
    tFIMetricDataPoint_fpr,
    tFIMetricDataPoint_precision,
    tFIMetricDataPoint_threshold,

    -- ** TFIModelPerformance
    tFIModelPerformance_auc,

    -- ** TFITrainingMetricsValue
    tFITrainingMetricsValue_metricDataPoints,
    tFITrainingMetricsValue_modelPerformance,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TrainingDataSchema
    trainingDataSchema_labelSchema,
    trainingDataSchema_modelVariables,

    -- ** TrainingMetrics
    trainingMetrics_metricDataPoints,
    trainingMetrics_auc,

    -- ** TrainingMetricsV2
    trainingMetricsV2_ati,
    trainingMetricsV2_tfi,
    trainingMetricsV2_ofi,

    -- ** TrainingResult
    trainingResult_variableImportanceMetrics,
    trainingResult_dataValidationMetrics,
    trainingResult_trainingMetrics,

    -- ** TrainingResultV2
    trainingResultV2_trainingMetricsV2,
    trainingResultV2_variableImportanceMetrics,
    trainingResultV2_dataValidationMetrics,
    trainingResultV2_aggregatedVariablesImportanceMetrics,

    -- ** Variable
    variable_name,
    variable_createdTime,
    variable_variableType,
    variable_arn,
    variable_defaultValue,
    variable_description,
    variable_lastUpdatedTime,
    variable_dataSource,
    variable_dataType,

    -- ** VariableEntry
    variableEntry_name,
    variableEntry_variableType,
    variableEntry_defaultValue,
    variableEntry_description,
    variableEntry_dataSource,
    variableEntry_dataType,

    -- ** VariableImpactExplanation
    variableImpactExplanation_logOddsImpact,
    variableImpactExplanation_eventVariableName,
    variableImpactExplanation_relativeImpact,

    -- ** VariableImportanceMetrics
    variableImportanceMetrics_logOddsMetrics,
  )
where

import Amazonka.FraudDetector.BatchCreateVariable
import Amazonka.FraudDetector.BatchGetVariable
import Amazonka.FraudDetector.CancelBatchImportJob
import Amazonka.FraudDetector.CancelBatchPredictionJob
import Amazonka.FraudDetector.CreateBatchImportJob
import Amazonka.FraudDetector.CreateBatchPredictionJob
import Amazonka.FraudDetector.CreateDetectorVersion
import Amazonka.FraudDetector.CreateModel
import Amazonka.FraudDetector.CreateModelVersion
import Amazonka.FraudDetector.CreateRule
import Amazonka.FraudDetector.CreateVariable
import Amazonka.FraudDetector.DeleteBatchImportJob
import Amazonka.FraudDetector.DeleteBatchPredictionJob
import Amazonka.FraudDetector.DeleteDetector
import Amazonka.FraudDetector.DeleteDetectorVersion
import Amazonka.FraudDetector.DeleteEntityType
import Amazonka.FraudDetector.DeleteEvent
import Amazonka.FraudDetector.DeleteEventType
import Amazonka.FraudDetector.DeleteEventsByEventType
import Amazonka.FraudDetector.DeleteExternalModel
import Amazonka.FraudDetector.DeleteLabel
import Amazonka.FraudDetector.DeleteModel
import Amazonka.FraudDetector.DeleteModelVersion
import Amazonka.FraudDetector.DeleteOutcome
import Amazonka.FraudDetector.DeleteRule
import Amazonka.FraudDetector.DeleteVariable
import Amazonka.FraudDetector.DescribeDetector
import Amazonka.FraudDetector.DescribeModelVersions
import Amazonka.FraudDetector.GetBatchImportJobs
import Amazonka.FraudDetector.GetBatchPredictionJobs
import Amazonka.FraudDetector.GetDeleteEventsByEventTypeStatus
import Amazonka.FraudDetector.GetDetectorVersion
import Amazonka.FraudDetector.GetDetectors
import Amazonka.FraudDetector.GetEntityTypes
import Amazonka.FraudDetector.GetEvent
import Amazonka.FraudDetector.GetEventPrediction
import Amazonka.FraudDetector.GetEventPredictionMetadata
import Amazonka.FraudDetector.GetEventTypes
import Amazonka.FraudDetector.GetExternalModels
import Amazonka.FraudDetector.GetKMSEncryptionKey
import Amazonka.FraudDetector.GetLabels
import Amazonka.FraudDetector.GetModelVersion
import Amazonka.FraudDetector.GetModels
import Amazonka.FraudDetector.GetOutcomes
import Amazonka.FraudDetector.GetRules
import Amazonka.FraudDetector.GetVariables
import Amazonka.FraudDetector.ListEventPredictions
import Amazonka.FraudDetector.ListTagsForResource
import Amazonka.FraudDetector.PutDetector
import Amazonka.FraudDetector.PutEntityType
import Amazonka.FraudDetector.PutEventType
import Amazonka.FraudDetector.PutExternalModel
import Amazonka.FraudDetector.PutKMSEncryptionKey
import Amazonka.FraudDetector.PutLabel
import Amazonka.FraudDetector.PutOutcome
import Amazonka.FraudDetector.SendEvent
import Amazonka.FraudDetector.TagResource
import Amazonka.FraudDetector.Types.ATIMetricDataPoint
import Amazonka.FraudDetector.Types.ATIModelPerformance
import Amazonka.FraudDetector.Types.ATITrainingMetricsValue
import Amazonka.FraudDetector.Types.AggregatedLogOddsMetric
import Amazonka.FraudDetector.Types.AggregatedVariablesImpactExplanation
import Amazonka.FraudDetector.Types.AggregatedVariablesImportanceMetrics
import Amazonka.FraudDetector.Types.BatchCreateVariableError
import Amazonka.FraudDetector.Types.BatchGetVariableError
import Amazonka.FraudDetector.Types.BatchImport
import Amazonka.FraudDetector.Types.BatchPrediction
import Amazonka.FraudDetector.Types.DataValidationMetrics
import Amazonka.FraudDetector.Types.Detector
import Amazonka.FraudDetector.Types.DetectorVersionSummary
import Amazonka.FraudDetector.Types.Entity
import Amazonka.FraudDetector.Types.EntityType
import Amazonka.FraudDetector.Types.EvaluatedExternalModel
import Amazonka.FraudDetector.Types.EvaluatedModelVersion
import Amazonka.FraudDetector.Types.EvaluatedRule
import Amazonka.FraudDetector.Types.Event
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
import Amazonka.FraudDetector.Types.LogOddsMetric
import Amazonka.FraudDetector.Types.MetricDataPoint
import Amazonka.FraudDetector.Types.Model
import Amazonka.FraudDetector.Types.ModelEndpointDataBlob
import Amazonka.FraudDetector.Types.ModelInputConfiguration
import Amazonka.FraudDetector.Types.ModelOutputConfiguration
import Amazonka.FraudDetector.Types.ModelScores
import Amazonka.FraudDetector.Types.ModelVersion
import Amazonka.FraudDetector.Types.ModelVersionDetail
import Amazonka.FraudDetector.Types.ModelVersionEvaluation
import Amazonka.FraudDetector.Types.OFIMetricDataPoint
import Amazonka.FraudDetector.Types.OFIModelPerformance
import Amazonka.FraudDetector.Types.OFITrainingMetricsValue
import Amazonka.FraudDetector.Types.Outcome
import Amazonka.FraudDetector.Types.PredictionExplanations
import Amazonka.FraudDetector.Types.PredictionTimeRange
import Amazonka.FraudDetector.Types.Rule
import Amazonka.FraudDetector.Types.RuleDetail
import Amazonka.FraudDetector.Types.RuleResult
import Amazonka.FraudDetector.Types.TFIMetricDataPoint
import Amazonka.FraudDetector.Types.TFIModelPerformance
import Amazonka.FraudDetector.Types.TFITrainingMetricsValue
import Amazonka.FraudDetector.Types.Tag
import Amazonka.FraudDetector.Types.TrainingDataSchema
import Amazonka.FraudDetector.Types.TrainingMetrics
import Amazonka.FraudDetector.Types.TrainingMetricsV2
import Amazonka.FraudDetector.Types.TrainingResult
import Amazonka.FraudDetector.Types.TrainingResultV2
import Amazonka.FraudDetector.Types.Variable
import Amazonka.FraudDetector.Types.VariableEntry
import Amazonka.FraudDetector.Types.VariableImpactExplanation
import Amazonka.FraudDetector.Types.VariableImportanceMetrics
import Amazonka.FraudDetector.UntagResource
import Amazonka.FraudDetector.UpdateDetectorVersion
import Amazonka.FraudDetector.UpdateDetectorVersionMetadata
import Amazonka.FraudDetector.UpdateDetectorVersionStatus
import Amazonka.FraudDetector.UpdateEventLabel
import Amazonka.FraudDetector.UpdateModel
import Amazonka.FraudDetector.UpdateModelVersion
import Amazonka.FraudDetector.UpdateModelVersionStatus
import Amazonka.FraudDetector.UpdateRuleMetadata
import Amazonka.FraudDetector.UpdateRuleVersion
import Amazonka.FraudDetector.UpdateVariable
