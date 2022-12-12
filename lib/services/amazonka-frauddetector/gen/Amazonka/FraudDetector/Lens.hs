{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FraudDetector.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createBatchPredictionJob_detectorVersion,
    createBatchPredictionJob_tags,
    createBatchPredictionJob_jobId,
    createBatchPredictionJob_inputPath,
    createBatchPredictionJob_outputPath,
    createBatchPredictionJob_eventTypeName,
    createBatchPredictionJob_detectorName,
    createBatchPredictionJob_iamRoleArn,
    createBatchPredictionJobResponse_httpStatus,

    -- ** CreateDetectorVersion
    createDetectorVersion_description,
    createDetectorVersion_externalModelEndpoints,
    createDetectorVersion_modelVersions,
    createDetectorVersion_ruleExecutionMode,
    createDetectorVersion_tags,
    createDetectorVersion_detectorId,
    createDetectorVersion_rules,
    createDetectorVersionResponse_detectorId,
    createDetectorVersionResponse_detectorVersionId,
    createDetectorVersionResponse_status,
    createDetectorVersionResponse_httpStatus,

    -- ** CreateModel
    createModel_description,
    createModel_tags,
    createModel_modelId,
    createModel_modelType,
    createModel_eventTypeName,
    createModelResponse_httpStatus,

    -- ** CreateModelVersion
    createModelVersion_externalEventsDetail,
    createModelVersion_ingestedEventsDetail,
    createModelVersion_tags,
    createModelVersion_modelId,
    createModelVersion_modelType,
    createModelVersion_trainingDataSource,
    createModelVersion_trainingDataSchema,
    createModelVersionResponse_modelId,
    createModelVersionResponse_modelType,
    createModelVersionResponse_modelVersionNumber,
    createModelVersionResponse_status,
    createModelVersionResponse_httpStatus,

    -- ** CreateRule
    createRule_description,
    createRule_tags,
    createRule_ruleId,
    createRule_detectorId,
    createRule_expression,
    createRule_language,
    createRule_outcomes,
    createRuleResponse_rule,
    createRuleResponse_httpStatus,

    -- ** CreateVariable
    createVariable_description,
    createVariable_tags,
    createVariable_variableType,
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
    deleteEventsByEventTypeResponse_eventTypeName,
    deleteEventsByEventTypeResponse_eventsDeletionStatus,
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
    describeDetector_maxResults,
    describeDetector_nextToken,
    describeDetector_detectorId,
    describeDetectorResponse_arn,
    describeDetectorResponse_detectorId,
    describeDetectorResponse_detectorVersionSummaries,
    describeDetectorResponse_nextToken,
    describeDetectorResponse_httpStatus,

    -- ** DescribeModelVersions
    describeModelVersions_maxResults,
    describeModelVersions_modelId,
    describeModelVersions_modelType,
    describeModelVersions_modelVersionNumber,
    describeModelVersions_nextToken,
    describeModelVersionsResponse_modelVersionDetails,
    describeModelVersionsResponse_nextToken,
    describeModelVersionsResponse_httpStatus,

    -- ** GetBatchImportJobs
    getBatchImportJobs_jobId,
    getBatchImportJobs_maxResults,
    getBatchImportJobs_nextToken,
    getBatchImportJobsResponse_batchImports,
    getBatchImportJobsResponse_nextToken,
    getBatchImportJobsResponse_httpStatus,

    -- ** GetBatchPredictionJobs
    getBatchPredictionJobs_jobId,
    getBatchPredictionJobs_maxResults,
    getBatchPredictionJobs_nextToken,
    getBatchPredictionJobsResponse_batchPredictions,
    getBatchPredictionJobsResponse_nextToken,
    getBatchPredictionJobsResponse_httpStatus,

    -- ** GetDeleteEventsByEventTypeStatus
    getDeleteEventsByEventTypeStatus_eventTypeName,
    getDeleteEventsByEventTypeStatusResponse_eventTypeName,
    getDeleteEventsByEventTypeStatusResponse_eventsDeletionStatus,
    getDeleteEventsByEventTypeStatusResponse_httpStatus,

    -- ** GetDetectorVersion
    getDetectorVersion_detectorId,
    getDetectorVersion_detectorVersionId,
    getDetectorVersionResponse_arn,
    getDetectorVersionResponse_createdTime,
    getDetectorVersionResponse_description,
    getDetectorVersionResponse_detectorId,
    getDetectorVersionResponse_detectorVersionId,
    getDetectorVersionResponse_externalModelEndpoints,
    getDetectorVersionResponse_lastUpdatedTime,
    getDetectorVersionResponse_modelVersions,
    getDetectorVersionResponse_ruleExecutionMode,
    getDetectorVersionResponse_rules,
    getDetectorVersionResponse_status,
    getDetectorVersionResponse_httpStatus,

    -- ** GetDetectors
    getDetectors_detectorId,
    getDetectors_maxResults,
    getDetectors_nextToken,
    getDetectorsResponse_detectors,
    getDetectorsResponse_nextToken,
    getDetectorsResponse_httpStatus,

    -- ** GetEntityTypes
    getEntityTypes_maxResults,
    getEntityTypes_name,
    getEntityTypes_nextToken,
    getEntityTypesResponse_entityTypes,
    getEntityTypesResponse_nextToken,
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
    getEventPredictionResponse_externalModelOutputs,
    getEventPredictionResponse_modelScores,
    getEventPredictionResponse_ruleResults,
    getEventPredictionResponse_httpStatus,

    -- ** GetEventPredictionMetadata
    getEventPredictionMetadata_eventId,
    getEventPredictionMetadata_eventTypeName,
    getEventPredictionMetadata_detectorId,
    getEventPredictionMetadata_detectorVersionId,
    getEventPredictionMetadata_predictionTimestamp,
    getEventPredictionMetadataResponse_detectorId,
    getEventPredictionMetadataResponse_detectorVersionId,
    getEventPredictionMetadataResponse_detectorVersionStatus,
    getEventPredictionMetadataResponse_entityId,
    getEventPredictionMetadataResponse_entityType,
    getEventPredictionMetadataResponse_evaluatedExternalModels,
    getEventPredictionMetadataResponse_evaluatedModelVersions,
    getEventPredictionMetadataResponse_eventId,
    getEventPredictionMetadataResponse_eventTimestamp,
    getEventPredictionMetadataResponse_eventTypeName,
    getEventPredictionMetadataResponse_eventVariables,
    getEventPredictionMetadataResponse_outcomes,
    getEventPredictionMetadataResponse_predictionTimestamp,
    getEventPredictionMetadataResponse_ruleExecutionMode,
    getEventPredictionMetadataResponse_rules,
    getEventPredictionMetadataResponse_httpStatus,

    -- ** GetEventTypes
    getEventTypes_maxResults,
    getEventTypes_name,
    getEventTypes_nextToken,
    getEventTypesResponse_eventTypes,
    getEventTypesResponse_nextToken,
    getEventTypesResponse_httpStatus,

    -- ** GetExternalModels
    getExternalModels_maxResults,
    getExternalModels_modelEndpoint,
    getExternalModels_nextToken,
    getExternalModelsResponse_externalModels,
    getExternalModelsResponse_nextToken,
    getExternalModelsResponse_httpStatus,

    -- ** GetKMSEncryptionKey
    getKMSEncryptionKeyResponse_kmsKey,
    getKMSEncryptionKeyResponse_httpStatus,

    -- ** GetLabels
    getLabels_maxResults,
    getLabels_name,
    getLabels_nextToken,
    getLabelsResponse_labels,
    getLabelsResponse_nextToken,
    getLabelsResponse_httpStatus,

    -- ** GetModelVersion
    getModelVersion_modelId,
    getModelVersion_modelType,
    getModelVersion_modelVersionNumber,
    getModelVersionResponse_arn,
    getModelVersionResponse_externalEventsDetail,
    getModelVersionResponse_ingestedEventsDetail,
    getModelVersionResponse_modelId,
    getModelVersionResponse_modelType,
    getModelVersionResponse_modelVersionNumber,
    getModelVersionResponse_status,
    getModelVersionResponse_trainingDataSchema,
    getModelVersionResponse_trainingDataSource,
    getModelVersionResponse_httpStatus,

    -- ** GetModels
    getModels_maxResults,
    getModels_modelId,
    getModels_modelType,
    getModels_nextToken,
    getModelsResponse_models,
    getModelsResponse_nextToken,
    getModelsResponse_httpStatus,

    -- ** GetOutcomes
    getOutcomes_maxResults,
    getOutcomes_name,
    getOutcomes_nextToken,
    getOutcomesResponse_nextToken,
    getOutcomesResponse_outcomes,
    getOutcomesResponse_httpStatus,

    -- ** GetRules
    getRules_maxResults,
    getRules_nextToken,
    getRules_ruleId,
    getRules_ruleVersion,
    getRules_detectorId,
    getRulesResponse_nextToken,
    getRulesResponse_ruleDetails,
    getRulesResponse_httpStatus,

    -- ** GetVariables
    getVariables_maxResults,
    getVariables_name,
    getVariables_nextToken,
    getVariablesResponse_nextToken,
    getVariablesResponse_variables,
    getVariablesResponse_httpStatus,

    -- ** ListEventPredictions
    listEventPredictions_detectorId,
    listEventPredictions_detectorVersionId,
    listEventPredictions_eventId,
    listEventPredictions_eventType,
    listEventPredictions_maxResults,
    listEventPredictions_nextToken,
    listEventPredictions_predictionTimeRange,
    listEventPredictionsResponse_eventPredictionSummaries,
    listEventPredictionsResponse_nextToken,
    listEventPredictionsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_maxResults,
    listTagsForResource_nextToken,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutDetector
    putDetector_description,
    putDetector_tags,
    putDetector_detectorId,
    putDetector_eventTypeName,
    putDetectorResponse_httpStatus,

    -- ** PutEntityType
    putEntityType_description,
    putEntityType_tags,
    putEntityType_name,
    putEntityTypeResponse_httpStatus,

    -- ** PutEventType
    putEventType_description,
    putEventType_eventIngestion,
    putEventType_labels,
    putEventType_tags,
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
    putLabel_description,
    putLabel_tags,
    putLabel_name,
    putLabelResponse_httpStatus,

    -- ** PutOutcome
    putOutcome_description,
    putOutcome_tags,
    putOutcome_name,
    putOutcomeResponse_httpStatus,

    -- ** SendEvent
    sendEvent_assignedLabel,
    sendEvent_labelTimestamp,
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
    updateDetectorVersion_description,
    updateDetectorVersion_modelVersions,
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
    updateModelVersion_externalEventsDetail,
    updateModelVersion_ingestedEventsDetail,
    updateModelVersion_tags,
    updateModelVersion_modelId,
    updateModelVersion_modelType,
    updateModelVersion_majorVersionNumber,
    updateModelVersionResponse_modelId,
    updateModelVersionResponse_modelType,
    updateModelVersionResponse_modelVersionNumber,
    updateModelVersionResponse_status,
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
    updateRuleVersion_description,
    updateRuleVersion_tags,
    updateRuleVersion_rule,
    updateRuleVersion_expression,
    updateRuleVersion_language,
    updateRuleVersion_outcomes,
    updateRuleVersionResponse_rule,
    updateRuleVersionResponse_httpStatus,

    -- ** UpdateVariable
    updateVariable_defaultValue,
    updateVariable_description,
    updateVariable_variableType,
    updateVariable_name,
    updateVariableResponse_httpStatus,

    -- * Types

    -- ** ATIMetricDataPoint
    aTIMetricDataPoint_adr,
    aTIMetricDataPoint_atodr,
    aTIMetricDataPoint_cr,
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
    aggregatedVariablesImpactExplanation_eventVariableNames,
    aggregatedVariablesImpactExplanation_logOddsImpact,
    aggregatedVariablesImpactExplanation_relativeImpact,

    -- ** AggregatedVariablesImportanceMetrics
    aggregatedVariablesImportanceMetrics_logOddsMetrics,

    -- ** BatchCreateVariableError
    batchCreateVariableError_code,
    batchCreateVariableError_message,
    batchCreateVariableError_name,

    -- ** BatchGetVariableError
    batchGetVariableError_code,
    batchGetVariableError_message,
    batchGetVariableError_name,

    -- ** BatchImport
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

    -- ** BatchPrediction
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

    -- ** DataValidationMetrics
    dataValidationMetrics_fieldLevelMessages,
    dataValidationMetrics_fileLevelMessages,

    -- ** Detector
    detector_arn,
    detector_createdTime,
    detector_description,
    detector_detectorId,
    detector_eventTypeName,
    detector_lastUpdatedTime,

    -- ** DetectorVersionSummary
    detectorVersionSummary_description,
    detectorVersionSummary_detectorVersionId,
    detectorVersionSummary_lastUpdatedTime,
    detectorVersionSummary_status,

    -- ** Entity
    entity_entityType,
    entity_entityId,

    -- ** EntityType
    entityType_arn,
    entityType_createdTime,
    entityType_description,
    entityType_lastUpdatedTime,
    entityType_name,

    -- ** EvaluatedExternalModel
    evaluatedExternalModel_inputVariables,
    evaluatedExternalModel_modelEndpoint,
    evaluatedExternalModel_outputVariables,
    evaluatedExternalModel_useEventVariables,

    -- ** EvaluatedModelVersion
    evaluatedModelVersion_evaluations,
    evaluatedModelVersion_modelId,
    evaluatedModelVersion_modelType,
    evaluatedModelVersion_modelVersion,

    -- ** EvaluatedRule
    evaluatedRule_evaluated,
    evaluatedRule_expression,
    evaluatedRule_expressionWithValues,
    evaluatedRule_matched,
    evaluatedRule_outcomes,
    evaluatedRule_ruleId,
    evaluatedRule_ruleVersion,

    -- ** Event
    event_currentLabel,
    event_entities,
    event_eventId,
    event_eventTimestamp,
    event_eventTypeName,
    event_eventVariables,
    event_labelTimestamp,

    -- ** EventPredictionSummary
    eventPredictionSummary_detectorId,
    eventPredictionSummary_detectorVersionId,
    eventPredictionSummary_eventId,
    eventPredictionSummary_eventTimestamp,
    eventPredictionSummary_eventTypeName,
    eventPredictionSummary_predictionTimestamp,

    -- ** EventType
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

    -- ** EventVariableSummary
    eventVariableSummary_name,
    eventVariableSummary_source,
    eventVariableSummary_value,

    -- ** ExternalEventsDetail
    externalEventsDetail_dataLocation,
    externalEventsDetail_dataAccessRoleArn,

    -- ** ExternalModel
    externalModel_arn,
    externalModel_createdTime,
    externalModel_inputConfiguration,
    externalModel_invokeModelEndpointRoleArn,
    externalModel_lastUpdatedTime,
    externalModel_modelEndpoint,
    externalModel_modelEndpointStatus,
    externalModel_modelSource,
    externalModel_outputConfiguration,

    -- ** ExternalModelOutputs
    externalModelOutputs_externalModel,
    externalModelOutputs_outputs,

    -- ** ExternalModelSummary
    externalModelSummary_modelEndpoint,
    externalModelSummary_modelSource,

    -- ** FieldValidationMessage
    fieldValidationMessage_content,
    fieldValidationMessage_fieldName,
    fieldValidationMessage_identifier,
    fieldValidationMessage_title,
    fieldValidationMessage_type,

    -- ** FileValidationMessage
    fileValidationMessage_content,
    fileValidationMessage_title,
    fileValidationMessage_type,

    -- ** FilterCondition
    filterCondition_value,

    -- ** IngestedEventStatistics
    ingestedEventStatistics_eventDataSizeInBytes,
    ingestedEventStatistics_lastUpdatedTime,
    ingestedEventStatistics_leastRecentEvent,
    ingestedEventStatistics_mostRecentEvent,
    ingestedEventStatistics_numberOfEvents,

    -- ** IngestedEventsDetail
    ingestedEventsDetail_ingestedEventsTimeWindow,

    -- ** IngestedEventsTimeWindow
    ingestedEventsTimeWindow_startTime,
    ingestedEventsTimeWindow_endTime,

    -- ** KMSKey
    kmsKey_kmsEncryptionKeyArn,

    -- ** Label
    label_arn,
    label_createdTime,
    label_description,
    label_lastUpdatedTime,
    label_name,

    -- ** LabelSchema
    labelSchema_labelMapper,
    labelSchema_unlabeledEventsTreatment,

    -- ** LogOddsMetric
    logOddsMetric_variableName,
    logOddsMetric_variableType,
    logOddsMetric_variableImportance,

    -- ** MetricDataPoint
    metricDataPoint_fpr,
    metricDataPoint_precision,
    metricDataPoint_threshold,
    metricDataPoint_tpr,

    -- ** Model
    model_arn,
    model_createdTime,
    model_description,
    model_eventTypeName,
    model_lastUpdatedTime,
    model_modelId,
    model_modelType,

    -- ** ModelEndpointDataBlob
    modelEndpointDataBlob_byteBuffer,
    modelEndpointDataBlob_contentType,

    -- ** ModelInputConfiguration
    modelInputConfiguration_csvInputTemplate,
    modelInputConfiguration_eventTypeName,
    modelInputConfiguration_format,
    modelInputConfiguration_jsonInputTemplate,
    modelInputConfiguration_useEventVariables,

    -- ** ModelOutputConfiguration
    modelOutputConfiguration_csvIndexToVariableMap,
    modelOutputConfiguration_jsonKeyToVariableMap,
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

    -- ** ModelVersionEvaluation
    modelVersionEvaluation_evaluationScore,
    modelVersionEvaluation_outputVariableName,
    modelVersionEvaluation_predictionExplanations,

    -- ** OFIMetricDataPoint
    oFIMetricDataPoint_fpr,
    oFIMetricDataPoint_precision,
    oFIMetricDataPoint_threshold,
    oFIMetricDataPoint_tpr,

    -- ** OFIModelPerformance
    oFIModelPerformance_auc,

    -- ** OFITrainingMetricsValue
    oFITrainingMetricsValue_metricDataPoints,
    oFITrainingMetricsValue_modelPerformance,

    -- ** Outcome
    outcome_arn,
    outcome_createdTime,
    outcome_description,
    outcome_lastUpdatedTime,
    outcome_name,

    -- ** PredictionExplanations
    predictionExplanations_aggregatedVariablesImpactExplanations,
    predictionExplanations_variableImpactExplanations,

    -- ** PredictionTimeRange
    predictionTimeRange_startTime,
    predictionTimeRange_endTime,

    -- ** Rule
    rule_detectorId,
    rule_ruleId,
    rule_ruleVersion,

    -- ** RuleDetail
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

    -- ** RuleResult
    ruleResult_outcomes,
    ruleResult_ruleId,

    -- ** TFIMetricDataPoint
    tFIMetricDataPoint_fpr,
    tFIMetricDataPoint_precision,
    tFIMetricDataPoint_threshold,
    tFIMetricDataPoint_tpr,

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
    trainingMetrics_auc,
    trainingMetrics_metricDataPoints,

    -- ** TrainingMetricsV2
    trainingMetricsV2_ati,
    trainingMetricsV2_ofi,
    trainingMetricsV2_tfi,

    -- ** TrainingResult
    trainingResult_dataValidationMetrics,
    trainingResult_trainingMetrics,
    trainingResult_variableImportanceMetrics,

    -- ** TrainingResultV2
    trainingResultV2_aggregatedVariablesImportanceMetrics,
    trainingResultV2_dataValidationMetrics,
    trainingResultV2_trainingMetricsV2,
    trainingResultV2_variableImportanceMetrics,

    -- ** Variable
    variable_arn,
    variable_createdTime,
    variable_dataSource,
    variable_dataType,
    variable_defaultValue,
    variable_description,
    variable_lastUpdatedTime,
    variable_name,
    variable_variableType,

    -- ** VariableEntry
    variableEntry_dataSource,
    variableEntry_dataType,
    variableEntry_defaultValue,
    variableEntry_description,
    variableEntry_name,
    variableEntry_variableType,

    -- ** VariableImpactExplanation
    variableImpactExplanation_eventVariableName,
    variableImpactExplanation_logOddsImpact,
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
