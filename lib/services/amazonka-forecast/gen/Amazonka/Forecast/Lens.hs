{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Forecast.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Lens
  ( -- * Operations

    -- ** CreateAutoPredictor
    createAutoPredictor_tags,
    createAutoPredictor_encryptionConfig,
    createAutoPredictor_forecastDimensions,
    createAutoPredictor_optimizationMetric,
    createAutoPredictor_explainPredictor,
    createAutoPredictor_monitorConfig,
    createAutoPredictor_forecastTypes,
    createAutoPredictor_forecastHorizon,
    createAutoPredictor_dataConfig,
    createAutoPredictor_referencePredictorArn,
    createAutoPredictor_timeAlignmentBoundary,
    createAutoPredictor_forecastFrequency,
    createAutoPredictor_predictorName,
    createAutoPredictorResponse_predictorArn,
    createAutoPredictorResponse_httpStatus,

    -- ** CreateDataset
    createDataset_tags,
    createDataset_encryptionConfig,
    createDataset_dataFrequency,
    createDataset_datasetName,
    createDataset_domain,
    createDataset_datasetType,
    createDataset_schema,
    createDatasetResponse_datasetArn,
    createDatasetResponse_httpStatus,

    -- ** CreateDatasetGroup
    createDatasetGroup_tags,
    createDatasetGroup_datasetArns,
    createDatasetGroup_datasetGroupName,
    createDatasetGroup_domain,
    createDatasetGroupResponse_datasetGroupArn,
    createDatasetGroupResponse_httpStatus,

    -- ** CreateDatasetImportJob
    createDatasetImportJob_tags,
    createDatasetImportJob_format,
    createDatasetImportJob_timeZone,
    createDatasetImportJob_useGeolocationForTimeZone,
    createDatasetImportJob_timestampFormat,
    createDatasetImportJob_geolocationFormat,
    createDatasetImportJob_datasetImportJobName,
    createDatasetImportJob_datasetArn,
    createDatasetImportJob_dataSource,
    createDatasetImportJobResponse_datasetImportJobArn,
    createDatasetImportJobResponse_httpStatus,

    -- ** CreateExplainability
    createExplainability_tags,
    createExplainability_enableVisualization,
    createExplainability_startDateTime,
    createExplainability_schema,
    createExplainability_dataSource,
    createExplainability_endDateTime,
    createExplainability_explainabilityName,
    createExplainability_resourceArn,
    createExplainability_explainabilityConfig,
    createExplainabilityResponse_explainabilityArn,
    createExplainabilityResponse_httpStatus,

    -- ** CreateExplainabilityExport
    createExplainabilityExport_tags,
    createExplainabilityExport_format,
    createExplainabilityExport_explainabilityExportName,
    createExplainabilityExport_explainabilityArn,
    createExplainabilityExport_destination,
    createExplainabilityExportResponse_explainabilityExportArn,
    createExplainabilityExportResponse_httpStatus,

    -- ** CreateForecast
    createForecast_tags,
    createForecast_forecastTypes,
    createForecast_timeSeriesSelector,
    createForecast_forecastName,
    createForecast_predictorArn,
    createForecastResponse_forecastArn,
    createForecastResponse_httpStatus,

    -- ** CreateForecastExportJob
    createForecastExportJob_tags,
    createForecastExportJob_format,
    createForecastExportJob_forecastExportJobName,
    createForecastExportJob_forecastArn,
    createForecastExportJob_destination,
    createForecastExportJobResponse_forecastExportJobArn,
    createForecastExportJobResponse_httpStatus,

    -- ** CreateMonitor
    createMonitor_tags,
    createMonitor_monitorName,
    createMonitor_resourceArn,
    createMonitorResponse_monitorArn,
    createMonitorResponse_httpStatus,

    -- ** CreatePredictor
    createPredictor_tags,
    createPredictor_encryptionConfig,
    createPredictor_performAutoML,
    createPredictor_performHPO,
    createPredictor_optimizationMetric,
    createPredictor_evaluationParameters,
    createPredictor_forecastTypes,
    createPredictor_algorithmArn,
    createPredictor_autoMLOverrideStrategy,
    createPredictor_hPOConfig,
    createPredictor_trainingParameters,
    createPredictor_predictorName,
    createPredictor_forecastHorizon,
    createPredictor_inputDataConfig,
    createPredictor_featurizationConfig,
    createPredictorResponse_predictorArn,
    createPredictorResponse_httpStatus,

    -- ** CreatePredictorBacktestExportJob
    createPredictorBacktestExportJob_tags,
    createPredictorBacktestExportJob_format,
    createPredictorBacktestExportJob_predictorBacktestExportJobName,
    createPredictorBacktestExportJob_predictorArn,
    createPredictorBacktestExportJob_destination,
    createPredictorBacktestExportJobResponse_predictorBacktestExportJobArn,
    createPredictorBacktestExportJobResponse_httpStatus,

    -- ** CreateWhatIfAnalysis
    createWhatIfAnalysis_tags,
    createWhatIfAnalysis_timeSeriesSelector,
    createWhatIfAnalysis_whatIfAnalysisName,
    createWhatIfAnalysis_forecastArn,
    createWhatIfAnalysisResponse_whatIfAnalysisArn,
    createWhatIfAnalysisResponse_httpStatus,

    -- ** CreateWhatIfForecast
    createWhatIfForecast_tags,
    createWhatIfForecast_timeSeriesTransformations,
    createWhatIfForecast_timeSeriesReplacementsDataSource,
    createWhatIfForecast_whatIfForecastName,
    createWhatIfForecast_whatIfAnalysisArn,
    createWhatIfForecastResponse_whatIfForecastArn,
    createWhatIfForecastResponse_httpStatus,

    -- ** CreateWhatIfForecastExport
    createWhatIfForecastExport_tags,
    createWhatIfForecastExport_format,
    createWhatIfForecastExport_whatIfForecastExportName,
    createWhatIfForecastExport_whatIfForecastArns,
    createWhatIfForecastExport_destination,
    createWhatIfForecastExportResponse_whatIfForecastExportArn,
    createWhatIfForecastExportResponse_httpStatus,

    -- ** DeleteDataset
    deleteDataset_datasetArn,

    -- ** DeleteDatasetGroup
    deleteDatasetGroup_datasetGroupArn,

    -- ** DeleteDatasetImportJob
    deleteDatasetImportJob_datasetImportJobArn,

    -- ** DeleteExplainability
    deleteExplainability_explainabilityArn,

    -- ** DeleteExplainabilityExport
    deleteExplainabilityExport_explainabilityExportArn,

    -- ** DeleteForecast
    deleteForecast_forecastArn,

    -- ** DeleteForecastExportJob
    deleteForecastExportJob_forecastExportJobArn,

    -- ** DeleteMonitor
    deleteMonitor_monitorArn,

    -- ** DeletePredictor
    deletePredictor_predictorArn,

    -- ** DeletePredictorBacktestExportJob
    deletePredictorBacktestExportJob_predictorBacktestExportJobArn,

    -- ** DeleteResourceTree
    deleteResourceTree_resourceArn,

    -- ** DeleteWhatIfAnalysis
    deleteWhatIfAnalysis_whatIfAnalysisArn,

    -- ** DeleteWhatIfForecast
    deleteWhatIfForecast_whatIfForecastArn,

    -- ** DeleteWhatIfForecastExport
    deleteWhatIfForecastExport_whatIfForecastExportArn,

    -- ** DescribeAutoPredictor
    describeAutoPredictor_predictorArn,
    describeAutoPredictorResponse_lastModificationTime,
    describeAutoPredictorResponse_encryptionConfig,
    describeAutoPredictorResponse_message,
    describeAutoPredictorResponse_explainabilityInfo,
    describeAutoPredictorResponse_forecastDimensions,
    describeAutoPredictorResponse_optimizationMetric,
    describeAutoPredictorResponse_monitorInfo,
    describeAutoPredictorResponse_forecastTypes,
    describeAutoPredictorResponse_predictorName,
    describeAutoPredictorResponse_status,
    describeAutoPredictorResponse_estimatedTimeRemainingInMinutes,
    describeAutoPredictorResponse_forecastHorizon,
    describeAutoPredictorResponse_predictorArn,
    describeAutoPredictorResponse_datasetImportJobArns,
    describeAutoPredictorResponse_dataConfig,
    describeAutoPredictorResponse_creationTime,
    describeAutoPredictorResponse_timeAlignmentBoundary,
    describeAutoPredictorResponse_referencePredictorSummary,
    describeAutoPredictorResponse_forecastFrequency,
    describeAutoPredictorResponse_httpStatus,

    -- ** DescribeDataset
    describeDataset_datasetArn,
    describeDatasetResponse_lastModificationTime,
    describeDatasetResponse_encryptionConfig,
    describeDatasetResponse_domain,
    describeDatasetResponse_datasetType,
    describeDatasetResponse_datasetName,
    describeDatasetResponse_status,
    describeDatasetResponse_dataFrequency,
    describeDatasetResponse_datasetArn,
    describeDatasetResponse_schema,
    describeDatasetResponse_creationTime,
    describeDatasetResponse_httpStatus,

    -- ** DescribeDatasetGroup
    describeDatasetGroup_datasetGroupArn,
    describeDatasetGroupResponse_lastModificationTime,
    describeDatasetGroupResponse_datasetGroupName,
    describeDatasetGroupResponse_domain,
    describeDatasetGroupResponse_status,
    describeDatasetGroupResponse_datasetArns,
    describeDatasetGroupResponse_creationTime,
    describeDatasetGroupResponse_datasetGroupArn,
    describeDatasetGroupResponse_httpStatus,

    -- ** DescribeDatasetImportJob
    describeDatasetImportJob_datasetImportJobArn,
    describeDatasetImportJobResponse_lastModificationTime,
    describeDatasetImportJobResponse_message,
    describeDatasetImportJobResponse_format,
    describeDatasetImportJobResponse_timeZone,
    describeDatasetImportJobResponse_fieldStatistics,
    describeDatasetImportJobResponse_status,
    describeDatasetImportJobResponse_datasetArn,
    describeDatasetImportJobResponse_dataSize,
    describeDatasetImportJobResponse_datasetImportJobArn,
    describeDatasetImportJobResponse_useGeolocationForTimeZone,
    describeDatasetImportJobResponse_estimatedTimeRemainingInMinutes,
    describeDatasetImportJobResponse_dataSource,
    describeDatasetImportJobResponse_timestampFormat,
    describeDatasetImportJobResponse_creationTime,
    describeDatasetImportJobResponse_datasetImportJobName,
    describeDatasetImportJobResponse_geolocationFormat,
    describeDatasetImportJobResponse_httpStatus,

    -- ** DescribeExplainability
    describeExplainability_explainabilityArn,
    describeExplainabilityResponse_lastModificationTime,
    describeExplainabilityResponse_message,
    describeExplainabilityResponse_explainabilityConfig,
    describeExplainabilityResponse_enableVisualization,
    describeExplainabilityResponse_startDateTime,
    describeExplainabilityResponse_status,
    describeExplainabilityResponse_explainabilityArn,
    describeExplainabilityResponse_explainabilityName,
    describeExplainabilityResponse_estimatedTimeRemainingInMinutes,
    describeExplainabilityResponse_schema,
    describeExplainabilityResponse_dataSource,
    describeExplainabilityResponse_creationTime,
    describeExplainabilityResponse_resourceArn,
    describeExplainabilityResponse_endDateTime,
    describeExplainabilityResponse_httpStatus,

    -- ** DescribeExplainabilityExport
    describeExplainabilityExport_explainabilityExportArn,
    describeExplainabilityExportResponse_lastModificationTime,
    describeExplainabilityExportResponse_destination,
    describeExplainabilityExportResponse_explainabilityExportName,
    describeExplainabilityExportResponse_message,
    describeExplainabilityExportResponse_format,
    describeExplainabilityExportResponse_explainabilityExportArn,
    describeExplainabilityExportResponse_status,
    describeExplainabilityExportResponse_explainabilityArn,
    describeExplainabilityExportResponse_creationTime,
    describeExplainabilityExportResponse_httpStatus,

    -- ** DescribeForecast
    describeForecast_forecastArn,
    describeForecastResponse_lastModificationTime,
    describeForecastResponse_message,
    describeForecastResponse_forecastTypes,
    describeForecastResponse_status,
    describeForecastResponse_estimatedTimeRemainingInMinutes,
    describeForecastResponse_predictorArn,
    describeForecastResponse_forecastArn,
    describeForecastResponse_creationTime,
    describeForecastResponse_datasetGroupArn,
    describeForecastResponse_timeSeriesSelector,
    describeForecastResponse_forecastName,
    describeForecastResponse_httpStatus,

    -- ** DescribeForecastExportJob
    describeForecastExportJob_forecastExportJobArn,
    describeForecastExportJobResponse_lastModificationTime,
    describeForecastExportJobResponse_destination,
    describeForecastExportJobResponse_message,
    describeForecastExportJobResponse_forecastExportJobName,
    describeForecastExportJobResponse_format,
    describeForecastExportJobResponse_forecastExportJobArn,
    describeForecastExportJobResponse_status,
    describeForecastExportJobResponse_forecastArn,
    describeForecastExportJobResponse_creationTime,
    describeForecastExportJobResponse_httpStatus,

    -- ** DescribeMonitor
    describeMonitor_monitorArn,
    describeMonitorResponse_lastModificationTime,
    describeMonitorResponse_message,
    describeMonitorResponse_monitorArn,
    describeMonitorResponse_baseline,
    describeMonitorResponse_status,
    describeMonitorResponse_estimatedEvaluationTimeRemainingInMinutes,
    describeMonitorResponse_lastEvaluationTime,
    describeMonitorResponse_lastEvaluationState,
    describeMonitorResponse_monitorName,
    describeMonitorResponse_creationTime,
    describeMonitorResponse_resourceArn,
    describeMonitorResponse_httpStatus,

    -- ** DescribePredictor
    describePredictor_predictorArn,
    describePredictorResponse_lastModificationTime,
    describePredictorResponse_encryptionConfig,
    describePredictorResponse_message,
    describePredictorResponse_performAutoML,
    describePredictorResponse_performHPO,
    describePredictorResponse_optimizationMetric,
    describePredictorResponse_evaluationParameters,
    describePredictorResponse_isAutoPredictor,
    describePredictorResponse_forecastTypes,
    describePredictorResponse_predictorName,
    describePredictorResponse_status,
    describePredictorResponse_algorithmArn,
    describePredictorResponse_featurizationConfig,
    describePredictorResponse_estimatedTimeRemainingInMinutes,
    describePredictorResponse_forecastHorizon,
    describePredictorResponse_predictorArn,
    describePredictorResponse_datasetImportJobArns,
    describePredictorResponse_predictorExecutionDetails,
    describePredictorResponse_autoMLOverrideStrategy,
    describePredictorResponse_creationTime,
    describePredictorResponse_autoMLAlgorithmArns,
    describePredictorResponse_inputDataConfig,
    describePredictorResponse_hPOConfig,
    describePredictorResponse_trainingParameters,
    describePredictorResponse_httpStatus,

    -- ** DescribePredictorBacktestExportJob
    describePredictorBacktestExportJob_predictorBacktestExportJobArn,
    describePredictorBacktestExportJobResponse_lastModificationTime,
    describePredictorBacktestExportJobResponse_destination,
    describePredictorBacktestExportJobResponse_message,
    describePredictorBacktestExportJobResponse_format,
    describePredictorBacktestExportJobResponse_status,
    describePredictorBacktestExportJobResponse_predictorArn,
    describePredictorBacktestExportJobResponse_predictorBacktestExportJobName,
    describePredictorBacktestExportJobResponse_predictorBacktestExportJobArn,
    describePredictorBacktestExportJobResponse_creationTime,
    describePredictorBacktestExportJobResponse_httpStatus,

    -- ** DescribeWhatIfAnalysis
    describeWhatIfAnalysis_whatIfAnalysisArn,
    describeWhatIfAnalysisResponse_lastModificationTime,
    describeWhatIfAnalysisResponse_whatIfAnalysisArn,
    describeWhatIfAnalysisResponse_message,
    describeWhatIfAnalysisResponse_whatIfAnalysisName,
    describeWhatIfAnalysisResponse_status,
    describeWhatIfAnalysisResponse_estimatedTimeRemainingInMinutes,
    describeWhatIfAnalysisResponse_forecastArn,
    describeWhatIfAnalysisResponse_creationTime,
    describeWhatIfAnalysisResponse_timeSeriesSelector,
    describeWhatIfAnalysisResponse_httpStatus,

    -- ** DescribeWhatIfForecast
    describeWhatIfForecast_whatIfForecastArn,
    describeWhatIfForecastResponse_lastModificationTime,
    describeWhatIfForecastResponse_whatIfAnalysisArn,
    describeWhatIfForecastResponse_message,
    describeWhatIfForecastResponse_timeSeriesTransformations,
    describeWhatIfForecastResponse_whatIfForecastName,
    describeWhatIfForecastResponse_whatIfForecastArn,
    describeWhatIfForecastResponse_timeSeriesReplacementsDataSource,
    describeWhatIfForecastResponse_forecastTypes,
    describeWhatIfForecastResponse_status,
    describeWhatIfForecastResponse_estimatedTimeRemainingInMinutes,
    describeWhatIfForecastResponse_creationTime,
    describeWhatIfForecastResponse_httpStatus,

    -- ** DescribeWhatIfForecastExport
    describeWhatIfForecastExport_whatIfForecastExportArn,
    describeWhatIfForecastExportResponse_lastModificationTime,
    describeWhatIfForecastExportResponse_destination,
    describeWhatIfForecastExportResponse_message,
    describeWhatIfForecastExportResponse_whatIfForecastExportName,
    describeWhatIfForecastExportResponse_format,
    describeWhatIfForecastExportResponse_whatIfForecastArns,
    describeWhatIfForecastExportResponse_whatIfForecastExportArn,
    describeWhatIfForecastExportResponse_status,
    describeWhatIfForecastExportResponse_estimatedTimeRemainingInMinutes,
    describeWhatIfForecastExportResponse_creationTime,
    describeWhatIfForecastExportResponse_httpStatus,

    -- ** GetAccuracyMetrics
    getAccuracyMetrics_predictorArn,
    getAccuracyMetricsResponse_predictorEvaluationResults,
    getAccuracyMetricsResponse_optimizationMetric,
    getAccuracyMetricsResponse_isAutoPredictor,
    getAccuracyMetricsResponse_autoMLOverrideStrategy,
    getAccuracyMetricsResponse_httpStatus,

    -- ** ListDatasetGroups
    listDatasetGroups_nextToken,
    listDatasetGroups_maxResults,
    listDatasetGroupsResponse_nextToken,
    listDatasetGroupsResponse_datasetGroups,
    listDatasetGroupsResponse_httpStatus,

    -- ** ListDatasetImportJobs
    listDatasetImportJobs_nextToken,
    listDatasetImportJobs_filters,
    listDatasetImportJobs_maxResults,
    listDatasetImportJobsResponse_nextToken,
    listDatasetImportJobsResponse_datasetImportJobs,
    listDatasetImportJobsResponse_httpStatus,

    -- ** ListDatasets
    listDatasets_nextToken,
    listDatasets_maxResults,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_datasets,
    listDatasetsResponse_httpStatus,

    -- ** ListExplainabilities
    listExplainabilities_nextToken,
    listExplainabilities_filters,
    listExplainabilities_maxResults,
    listExplainabilitiesResponse_nextToken,
    listExplainabilitiesResponse_explainabilities,
    listExplainabilitiesResponse_httpStatus,

    -- ** ListExplainabilityExports
    listExplainabilityExports_nextToken,
    listExplainabilityExports_filters,
    listExplainabilityExports_maxResults,
    listExplainabilityExportsResponse_nextToken,
    listExplainabilityExportsResponse_explainabilityExports,
    listExplainabilityExportsResponse_httpStatus,

    -- ** ListForecastExportJobs
    listForecastExportJobs_nextToken,
    listForecastExportJobs_filters,
    listForecastExportJobs_maxResults,
    listForecastExportJobsResponse_nextToken,
    listForecastExportJobsResponse_forecastExportJobs,
    listForecastExportJobsResponse_httpStatus,

    -- ** ListForecasts
    listForecasts_nextToken,
    listForecasts_filters,
    listForecasts_maxResults,
    listForecastsResponse_nextToken,
    listForecastsResponse_forecasts,
    listForecastsResponse_httpStatus,

    -- ** ListMonitorEvaluations
    listMonitorEvaluations_nextToken,
    listMonitorEvaluations_filters,
    listMonitorEvaluations_maxResults,
    listMonitorEvaluations_monitorArn,
    listMonitorEvaluationsResponse_nextToken,
    listMonitorEvaluationsResponse_predictorMonitorEvaluations,
    listMonitorEvaluationsResponse_httpStatus,

    -- ** ListMonitors
    listMonitors_nextToken,
    listMonitors_filters,
    listMonitors_maxResults,
    listMonitorsResponse_nextToken,
    listMonitorsResponse_monitors,
    listMonitorsResponse_httpStatus,

    -- ** ListPredictorBacktestExportJobs
    listPredictorBacktestExportJobs_nextToken,
    listPredictorBacktestExportJobs_filters,
    listPredictorBacktestExportJobs_maxResults,
    listPredictorBacktestExportJobsResponse_nextToken,
    listPredictorBacktestExportJobsResponse_predictorBacktestExportJobs,
    listPredictorBacktestExportJobsResponse_httpStatus,

    -- ** ListPredictors
    listPredictors_nextToken,
    listPredictors_filters,
    listPredictors_maxResults,
    listPredictorsResponse_nextToken,
    listPredictorsResponse_predictors,
    listPredictorsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWhatIfAnalyses
    listWhatIfAnalyses_nextToken,
    listWhatIfAnalyses_filters,
    listWhatIfAnalyses_maxResults,
    listWhatIfAnalysesResponse_nextToken,
    listWhatIfAnalysesResponse_whatIfAnalyses,
    listWhatIfAnalysesResponse_httpStatus,

    -- ** ListWhatIfForecastExports
    listWhatIfForecastExports_nextToken,
    listWhatIfForecastExports_filters,
    listWhatIfForecastExports_maxResults,
    listWhatIfForecastExportsResponse_nextToken,
    listWhatIfForecastExportsResponse_whatIfForecastExports,
    listWhatIfForecastExportsResponse_httpStatus,

    -- ** ListWhatIfForecasts
    listWhatIfForecasts_nextToken,
    listWhatIfForecasts_filters,
    listWhatIfForecasts_maxResults,
    listWhatIfForecastsResponse_nextToken,
    listWhatIfForecastsResponse_whatIfForecasts,
    listWhatIfForecastsResponse_httpStatus,

    -- ** ResumeResource
    resumeResource_resourceArn,

    -- ** StopResource
    stopResource_resourceArn,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateDatasetGroup
    updateDatasetGroup_datasetGroupArn,
    updateDatasetGroup_datasetArns,
    updateDatasetGroupResponse_httpStatus,

    -- * Types

    -- ** Action
    action_attributeName,
    action_operation,
    action_value,

    -- ** AdditionalDataset
    additionalDataset_configuration,
    additionalDataset_name,

    -- ** AttributeConfig
    attributeConfig_attributeName,
    attributeConfig_transformations,

    -- ** Baseline
    baseline_predictorBaseline,

    -- ** BaselineMetric
    baselineMetric_name,
    baselineMetric_value,

    -- ** CategoricalParameterRange
    categoricalParameterRange_name,
    categoricalParameterRange_values,

    -- ** ContinuousParameterRange
    continuousParameterRange_scalingType,
    continuousParameterRange_name,
    continuousParameterRange_maxValue,
    continuousParameterRange_minValue,

    -- ** DataConfig
    dataConfig_additionalDatasets,
    dataConfig_attributeConfigs,
    dataConfig_datasetGroupArn,

    -- ** DataDestination
    dataDestination_s3Config,

    -- ** DataSource
    dataSource_s3Config,

    -- ** DatasetGroupSummary
    datasetGroupSummary_lastModificationTime,
    datasetGroupSummary_datasetGroupName,
    datasetGroupSummary_creationTime,
    datasetGroupSummary_datasetGroupArn,

    -- ** DatasetImportJobSummary
    datasetImportJobSummary_lastModificationTime,
    datasetImportJobSummary_message,
    datasetImportJobSummary_status,
    datasetImportJobSummary_datasetImportJobArn,
    datasetImportJobSummary_dataSource,
    datasetImportJobSummary_creationTime,
    datasetImportJobSummary_datasetImportJobName,

    -- ** DatasetSummary
    datasetSummary_lastModificationTime,
    datasetSummary_domain,
    datasetSummary_datasetType,
    datasetSummary_datasetName,
    datasetSummary_datasetArn,
    datasetSummary_creationTime,

    -- ** EncryptionConfig
    encryptionConfig_roleArn,
    encryptionConfig_kmsKeyArn,

    -- ** ErrorMetric
    errorMetric_wape,
    errorMetric_mase,
    errorMetric_forecastType,
    errorMetric_rmse,
    errorMetric_mape,

    -- ** EvaluationParameters
    evaluationParameters_numberOfBacktestWindows,
    evaluationParameters_backTestWindowOffset,

    -- ** EvaluationResult
    evaluationResult_testWindows,
    evaluationResult_algorithmArn,

    -- ** ExplainabilityConfig
    explainabilityConfig_timeSeriesGranularity,
    explainabilityConfig_timePointGranularity,

    -- ** ExplainabilityExportSummary
    explainabilityExportSummary_lastModificationTime,
    explainabilityExportSummary_destination,
    explainabilityExportSummary_explainabilityExportName,
    explainabilityExportSummary_message,
    explainabilityExportSummary_explainabilityExportArn,
    explainabilityExportSummary_status,
    explainabilityExportSummary_creationTime,

    -- ** ExplainabilityInfo
    explainabilityInfo_status,
    explainabilityInfo_explainabilityArn,

    -- ** ExplainabilitySummary
    explainabilitySummary_lastModificationTime,
    explainabilitySummary_message,
    explainabilitySummary_explainabilityConfig,
    explainabilitySummary_status,
    explainabilitySummary_explainabilityArn,
    explainabilitySummary_explainabilityName,
    explainabilitySummary_creationTime,
    explainabilitySummary_resourceArn,

    -- ** Featurization
    featurization_featurizationPipeline,
    featurization_attributeName,

    -- ** FeaturizationConfig
    featurizationConfig_forecastDimensions,
    featurizationConfig_featurizations,
    featurizationConfig_forecastFrequency,

    -- ** FeaturizationMethod
    featurizationMethod_featurizationMethodParameters,
    featurizationMethod_featurizationMethodName,

    -- ** Filter
    filter_key,
    filter_value,
    filter_condition,

    -- ** ForecastExportJobSummary
    forecastExportJobSummary_lastModificationTime,
    forecastExportJobSummary_destination,
    forecastExportJobSummary_message,
    forecastExportJobSummary_forecastExportJobName,
    forecastExportJobSummary_forecastExportJobArn,
    forecastExportJobSummary_status,
    forecastExportJobSummary_creationTime,

    -- ** ForecastSummary
    forecastSummary_lastModificationTime,
    forecastSummary_message,
    forecastSummary_status,
    forecastSummary_predictorArn,
    forecastSummary_forecastArn,
    forecastSummary_creationTime,
    forecastSummary_datasetGroupArn,
    forecastSummary_createdUsingAutoPredictor,
    forecastSummary_forecastName,

    -- ** HyperParameterTuningJobConfig
    hyperParameterTuningJobConfig_parameterRanges,

    -- ** InputDataConfig
    inputDataConfig_supplementaryFeatures,
    inputDataConfig_datasetGroupArn,

    -- ** IntegerParameterRange
    integerParameterRange_scalingType,
    integerParameterRange_name,
    integerParameterRange_maxValue,
    integerParameterRange_minValue,

    -- ** MetricResult
    metricResult_metricValue,
    metricResult_metricName,

    -- ** Metrics
    metrics_averageWeightedQuantileLoss,
    metrics_weightedQuantileLosses,
    metrics_errorMetrics,
    metrics_rmse,

    -- ** MonitorConfig
    monitorConfig_monitorName,

    -- ** MonitorDataSource
    monitorDataSource_datasetImportJobArn,
    monitorDataSource_predictorArn,
    monitorDataSource_forecastArn,

    -- ** MonitorInfo
    monitorInfo_monitorArn,
    monitorInfo_status,

    -- ** MonitorSummary
    monitorSummary_lastModificationTime,
    monitorSummary_monitorArn,
    monitorSummary_status,
    monitorSummary_monitorName,
    monitorSummary_creationTime,
    monitorSummary_resourceArn,

    -- ** ParameterRanges
    parameterRanges_categoricalParameterRanges,
    parameterRanges_integerParameterRanges,
    parameterRanges_continuousParameterRanges,

    -- ** PredictorBacktestExportJobSummary
    predictorBacktestExportJobSummary_lastModificationTime,
    predictorBacktestExportJobSummary_destination,
    predictorBacktestExportJobSummary_message,
    predictorBacktestExportJobSummary_status,
    predictorBacktestExportJobSummary_predictorBacktestExportJobName,
    predictorBacktestExportJobSummary_predictorBacktestExportJobArn,
    predictorBacktestExportJobSummary_creationTime,

    -- ** PredictorBaseline
    predictorBaseline_baselineMetrics,

    -- ** PredictorEvent
    predictorEvent_datetime,
    predictorEvent_detail,

    -- ** PredictorExecution
    predictorExecution_testWindows,
    predictorExecution_algorithmArn,

    -- ** PredictorExecutionDetails
    predictorExecutionDetails_predictorExecutions,

    -- ** PredictorMonitorEvaluation
    predictorMonitorEvaluation_evaluationTime,
    predictorMonitorEvaluation_message,
    predictorMonitorEvaluation_evaluationState,
    predictorMonitorEvaluation_monitorArn,
    predictorMonitorEvaluation_numItemsEvaluated,
    predictorMonitorEvaluation_windowEndDatetime,
    predictorMonitorEvaluation_monitorDataSource,
    predictorMonitorEvaluation_windowStartDatetime,
    predictorMonitorEvaluation_predictorEvent,
    predictorMonitorEvaluation_metricResults,
    predictorMonitorEvaluation_resourceArn,

    -- ** PredictorSummary
    predictorSummary_lastModificationTime,
    predictorSummary_message,
    predictorSummary_isAutoPredictor,
    predictorSummary_predictorName,
    predictorSummary_status,
    predictorSummary_predictorArn,
    predictorSummary_creationTime,
    predictorSummary_datasetGroupArn,
    predictorSummary_referencePredictorSummary,

    -- ** ReferencePredictorSummary
    referencePredictorSummary_arn,
    referencePredictorSummary_state,

    -- ** S3Config
    s3Config_kmsKeyArn,
    s3Config_path,
    s3Config_roleArn,

    -- ** Schema
    schema_attributes,

    -- ** SchemaAttribute
    schemaAttribute_attributeType,
    schemaAttribute_attributeName,

    -- ** Statistics
    statistics_countNanLong,
    statistics_countNullLong,
    statistics_countNull,
    statistics_max,
    statistics_countLong,
    statistics_countDistinctLong,
    statistics_avg,
    statistics_count,
    statistics_min,
    statistics_countNan,
    statistics_stddev,
    statistics_countDistinct,

    -- ** SupplementaryFeature
    supplementaryFeature_name,
    supplementaryFeature_value,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TestWindowSummary
    testWindowSummary_message,
    testWindowSummary_status,
    testWindowSummary_testWindowEnd,
    testWindowSummary_testWindowStart,

    -- ** TimeAlignmentBoundary
    timeAlignmentBoundary_dayOfWeek,
    timeAlignmentBoundary_month,
    timeAlignmentBoundary_hour,
    timeAlignmentBoundary_dayOfMonth,

    -- ** TimeSeriesCondition
    timeSeriesCondition_attributeName,
    timeSeriesCondition_attributeValue,
    timeSeriesCondition_condition,

    -- ** TimeSeriesIdentifiers
    timeSeriesIdentifiers_format,
    timeSeriesIdentifiers_schema,
    timeSeriesIdentifiers_dataSource,

    -- ** TimeSeriesReplacementsDataSource
    timeSeriesReplacementsDataSource_format,
    timeSeriesReplacementsDataSource_timestampFormat,
    timeSeriesReplacementsDataSource_s3Config,
    timeSeriesReplacementsDataSource_schema,

    -- ** TimeSeriesSelector
    timeSeriesSelector_timeSeriesIdentifiers,

    -- ** TimeSeriesTransformation
    timeSeriesTransformation_timeSeriesConditions,
    timeSeriesTransformation_action,

    -- ** WeightedQuantileLoss
    weightedQuantileLoss_quantile,
    weightedQuantileLoss_lossValue,

    -- ** WhatIfAnalysisSummary
    whatIfAnalysisSummary_lastModificationTime,
    whatIfAnalysisSummary_whatIfAnalysisArn,
    whatIfAnalysisSummary_message,
    whatIfAnalysisSummary_whatIfAnalysisName,
    whatIfAnalysisSummary_status,
    whatIfAnalysisSummary_forecastArn,
    whatIfAnalysisSummary_creationTime,

    -- ** WhatIfForecastExportSummary
    whatIfForecastExportSummary_lastModificationTime,
    whatIfForecastExportSummary_destination,
    whatIfForecastExportSummary_message,
    whatIfForecastExportSummary_whatIfForecastExportName,
    whatIfForecastExportSummary_whatIfForecastArns,
    whatIfForecastExportSummary_whatIfForecastExportArn,
    whatIfForecastExportSummary_status,
    whatIfForecastExportSummary_creationTime,

    -- ** WhatIfForecastSummary
    whatIfForecastSummary_lastModificationTime,
    whatIfForecastSummary_whatIfAnalysisArn,
    whatIfForecastSummary_message,
    whatIfForecastSummary_whatIfForecastName,
    whatIfForecastSummary_whatIfForecastArn,
    whatIfForecastSummary_status,
    whatIfForecastSummary_creationTime,

    -- ** WindowSummary
    windowSummary_itemCount,
    windowSummary_metrics,
    windowSummary_evaluationType,
    windowSummary_testWindowEnd,
    windowSummary_testWindowStart,
  )
where

import Amazonka.Forecast.CreateAutoPredictor
import Amazonka.Forecast.CreateDataset
import Amazonka.Forecast.CreateDatasetGroup
import Amazonka.Forecast.CreateDatasetImportJob
import Amazonka.Forecast.CreateExplainability
import Amazonka.Forecast.CreateExplainabilityExport
import Amazonka.Forecast.CreateForecast
import Amazonka.Forecast.CreateForecastExportJob
import Amazonka.Forecast.CreateMonitor
import Amazonka.Forecast.CreatePredictor
import Amazonka.Forecast.CreatePredictorBacktestExportJob
import Amazonka.Forecast.CreateWhatIfAnalysis
import Amazonka.Forecast.CreateWhatIfForecast
import Amazonka.Forecast.CreateWhatIfForecastExport
import Amazonka.Forecast.DeleteDataset
import Amazonka.Forecast.DeleteDatasetGroup
import Amazonka.Forecast.DeleteDatasetImportJob
import Amazonka.Forecast.DeleteExplainability
import Amazonka.Forecast.DeleteExplainabilityExport
import Amazonka.Forecast.DeleteForecast
import Amazonka.Forecast.DeleteForecastExportJob
import Amazonka.Forecast.DeleteMonitor
import Amazonka.Forecast.DeletePredictor
import Amazonka.Forecast.DeletePredictorBacktestExportJob
import Amazonka.Forecast.DeleteResourceTree
import Amazonka.Forecast.DeleteWhatIfAnalysis
import Amazonka.Forecast.DeleteWhatIfForecast
import Amazonka.Forecast.DeleteWhatIfForecastExport
import Amazonka.Forecast.DescribeAutoPredictor
import Amazonka.Forecast.DescribeDataset
import Amazonka.Forecast.DescribeDatasetGroup
import Amazonka.Forecast.DescribeDatasetImportJob
import Amazonka.Forecast.DescribeExplainability
import Amazonka.Forecast.DescribeExplainabilityExport
import Amazonka.Forecast.DescribeForecast
import Amazonka.Forecast.DescribeForecastExportJob
import Amazonka.Forecast.DescribeMonitor
import Amazonka.Forecast.DescribePredictor
import Amazonka.Forecast.DescribePredictorBacktestExportJob
import Amazonka.Forecast.DescribeWhatIfAnalysis
import Amazonka.Forecast.DescribeWhatIfForecast
import Amazonka.Forecast.DescribeWhatIfForecastExport
import Amazonka.Forecast.GetAccuracyMetrics
import Amazonka.Forecast.ListDatasetGroups
import Amazonka.Forecast.ListDatasetImportJobs
import Amazonka.Forecast.ListDatasets
import Amazonka.Forecast.ListExplainabilities
import Amazonka.Forecast.ListExplainabilityExports
import Amazonka.Forecast.ListForecastExportJobs
import Amazonka.Forecast.ListForecasts
import Amazonka.Forecast.ListMonitorEvaluations
import Amazonka.Forecast.ListMonitors
import Amazonka.Forecast.ListPredictorBacktestExportJobs
import Amazonka.Forecast.ListPredictors
import Amazonka.Forecast.ListTagsForResource
import Amazonka.Forecast.ListWhatIfAnalyses
import Amazonka.Forecast.ListWhatIfForecastExports
import Amazonka.Forecast.ListWhatIfForecasts
import Amazonka.Forecast.ResumeResource
import Amazonka.Forecast.StopResource
import Amazonka.Forecast.TagResource
import Amazonka.Forecast.Types.Action
import Amazonka.Forecast.Types.AdditionalDataset
import Amazonka.Forecast.Types.AttributeConfig
import Amazonka.Forecast.Types.Baseline
import Amazonka.Forecast.Types.BaselineMetric
import Amazonka.Forecast.Types.CategoricalParameterRange
import Amazonka.Forecast.Types.ContinuousParameterRange
import Amazonka.Forecast.Types.DataConfig
import Amazonka.Forecast.Types.DataDestination
import Amazonka.Forecast.Types.DataSource
import Amazonka.Forecast.Types.DatasetGroupSummary
import Amazonka.Forecast.Types.DatasetImportJobSummary
import Amazonka.Forecast.Types.DatasetSummary
import Amazonka.Forecast.Types.EncryptionConfig
import Amazonka.Forecast.Types.ErrorMetric
import Amazonka.Forecast.Types.EvaluationParameters
import Amazonka.Forecast.Types.EvaluationResult
import Amazonka.Forecast.Types.ExplainabilityConfig
import Amazonka.Forecast.Types.ExplainabilityExportSummary
import Amazonka.Forecast.Types.ExplainabilityInfo
import Amazonka.Forecast.Types.ExplainabilitySummary
import Amazonka.Forecast.Types.Featurization
import Amazonka.Forecast.Types.FeaturizationConfig
import Amazonka.Forecast.Types.FeaturizationMethod
import Amazonka.Forecast.Types.Filter
import Amazonka.Forecast.Types.ForecastExportJobSummary
import Amazonka.Forecast.Types.ForecastSummary
import Amazonka.Forecast.Types.HyperParameterTuningJobConfig
import Amazonka.Forecast.Types.InputDataConfig
import Amazonka.Forecast.Types.IntegerParameterRange
import Amazonka.Forecast.Types.MetricResult
import Amazonka.Forecast.Types.Metrics
import Amazonka.Forecast.Types.MonitorConfig
import Amazonka.Forecast.Types.MonitorDataSource
import Amazonka.Forecast.Types.MonitorInfo
import Amazonka.Forecast.Types.MonitorSummary
import Amazonka.Forecast.Types.ParameterRanges
import Amazonka.Forecast.Types.PredictorBacktestExportJobSummary
import Amazonka.Forecast.Types.PredictorBaseline
import Amazonka.Forecast.Types.PredictorEvent
import Amazonka.Forecast.Types.PredictorExecution
import Amazonka.Forecast.Types.PredictorExecutionDetails
import Amazonka.Forecast.Types.PredictorMonitorEvaluation
import Amazonka.Forecast.Types.PredictorSummary
import Amazonka.Forecast.Types.ReferencePredictorSummary
import Amazonka.Forecast.Types.S3Config
import Amazonka.Forecast.Types.Schema
import Amazonka.Forecast.Types.SchemaAttribute
import Amazonka.Forecast.Types.Statistics
import Amazonka.Forecast.Types.SupplementaryFeature
import Amazonka.Forecast.Types.Tag
import Amazonka.Forecast.Types.TestWindowSummary
import Amazonka.Forecast.Types.TimeAlignmentBoundary
import Amazonka.Forecast.Types.TimeSeriesCondition
import Amazonka.Forecast.Types.TimeSeriesIdentifiers
import Amazonka.Forecast.Types.TimeSeriesReplacementsDataSource
import Amazonka.Forecast.Types.TimeSeriesSelector
import Amazonka.Forecast.Types.TimeSeriesTransformation
import Amazonka.Forecast.Types.WeightedQuantileLoss
import Amazonka.Forecast.Types.WhatIfAnalysisSummary
import Amazonka.Forecast.Types.WhatIfForecastExportSummary
import Amazonka.Forecast.Types.WhatIfForecastSummary
import Amazonka.Forecast.Types.WindowSummary
import Amazonka.Forecast.UntagResource
import Amazonka.Forecast.UpdateDatasetGroup
