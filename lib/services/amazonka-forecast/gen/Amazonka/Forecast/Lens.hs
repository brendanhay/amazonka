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
    createAutoPredictor_dataConfig,
    createAutoPredictor_encryptionConfig,
    createAutoPredictor_explainPredictor,
    createAutoPredictor_forecastDimensions,
    createAutoPredictor_forecastFrequency,
    createAutoPredictor_forecastHorizon,
    createAutoPredictor_forecastTypes,
    createAutoPredictor_monitorConfig,
    createAutoPredictor_optimizationMetric,
    createAutoPredictor_referencePredictorArn,
    createAutoPredictor_tags,
    createAutoPredictor_timeAlignmentBoundary,
    createAutoPredictor_predictorName,
    createAutoPredictorResponse_predictorArn,
    createAutoPredictorResponse_httpStatus,

    -- ** CreateDataset
    createDataset_dataFrequency,
    createDataset_encryptionConfig,
    createDataset_tags,
    createDataset_datasetName,
    createDataset_domain,
    createDataset_datasetType,
    createDataset_schema,
    createDatasetResponse_datasetArn,
    createDatasetResponse_httpStatus,

    -- ** CreateDatasetGroup
    createDatasetGroup_datasetArns,
    createDatasetGroup_tags,
    createDatasetGroup_datasetGroupName,
    createDatasetGroup_domain,
    createDatasetGroupResponse_datasetGroupArn,
    createDatasetGroupResponse_httpStatus,

    -- ** CreateDatasetImportJob
    createDatasetImportJob_format,
    createDatasetImportJob_geolocationFormat,
    createDatasetImportJob_tags,
    createDatasetImportJob_timeZone,
    createDatasetImportJob_timestampFormat,
    createDatasetImportJob_useGeolocationForTimeZone,
    createDatasetImportJob_datasetImportJobName,
    createDatasetImportJob_datasetArn,
    createDatasetImportJob_dataSource,
    createDatasetImportJobResponse_datasetImportJobArn,
    createDatasetImportJobResponse_httpStatus,

    -- ** CreateExplainability
    createExplainability_dataSource,
    createExplainability_enableVisualization,
    createExplainability_endDateTime,
    createExplainability_schema,
    createExplainability_startDateTime,
    createExplainability_tags,
    createExplainability_explainabilityName,
    createExplainability_resourceArn,
    createExplainability_explainabilityConfig,
    createExplainabilityResponse_explainabilityArn,
    createExplainabilityResponse_httpStatus,

    -- ** CreateExplainabilityExport
    createExplainabilityExport_format,
    createExplainabilityExport_tags,
    createExplainabilityExport_explainabilityExportName,
    createExplainabilityExport_explainabilityArn,
    createExplainabilityExport_destination,
    createExplainabilityExportResponse_explainabilityExportArn,
    createExplainabilityExportResponse_httpStatus,

    -- ** CreateForecast
    createForecast_forecastTypes,
    createForecast_tags,
    createForecast_timeSeriesSelector,
    createForecast_forecastName,
    createForecast_predictorArn,
    createForecastResponse_forecastArn,
    createForecastResponse_httpStatus,

    -- ** CreateForecastExportJob
    createForecastExportJob_format,
    createForecastExportJob_tags,
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
    createPredictor_algorithmArn,
    createPredictor_autoMLOverrideStrategy,
    createPredictor_encryptionConfig,
    createPredictor_evaluationParameters,
    createPredictor_forecastTypes,
    createPredictor_hPOConfig,
    createPredictor_optimizationMetric,
    createPredictor_performAutoML,
    createPredictor_performHPO,
    createPredictor_tags,
    createPredictor_trainingParameters,
    createPredictor_predictorName,
    createPredictor_forecastHorizon,
    createPredictor_inputDataConfig,
    createPredictor_featurizationConfig,
    createPredictorResponse_predictorArn,
    createPredictorResponse_httpStatus,

    -- ** CreatePredictorBacktestExportJob
    createPredictorBacktestExportJob_format,
    createPredictorBacktestExportJob_tags,
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
    createWhatIfForecast_timeSeriesReplacementsDataSource,
    createWhatIfForecast_timeSeriesTransformations,
    createWhatIfForecast_whatIfForecastName,
    createWhatIfForecast_whatIfAnalysisArn,
    createWhatIfForecastResponse_whatIfForecastArn,
    createWhatIfForecastResponse_httpStatus,

    -- ** CreateWhatIfForecastExport
    createWhatIfForecastExport_format,
    createWhatIfForecastExport_tags,
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
    describeAutoPredictorResponse_creationTime,
    describeAutoPredictorResponse_dataConfig,
    describeAutoPredictorResponse_datasetImportJobArns,
    describeAutoPredictorResponse_encryptionConfig,
    describeAutoPredictorResponse_estimatedTimeRemainingInMinutes,
    describeAutoPredictorResponse_explainabilityInfo,
    describeAutoPredictorResponse_forecastDimensions,
    describeAutoPredictorResponse_forecastFrequency,
    describeAutoPredictorResponse_forecastHorizon,
    describeAutoPredictorResponse_forecastTypes,
    describeAutoPredictorResponse_lastModificationTime,
    describeAutoPredictorResponse_message,
    describeAutoPredictorResponse_monitorInfo,
    describeAutoPredictorResponse_optimizationMetric,
    describeAutoPredictorResponse_predictorArn,
    describeAutoPredictorResponse_predictorName,
    describeAutoPredictorResponse_referencePredictorSummary,
    describeAutoPredictorResponse_status,
    describeAutoPredictorResponse_timeAlignmentBoundary,
    describeAutoPredictorResponse_httpStatus,

    -- ** DescribeDataset
    describeDataset_datasetArn,
    describeDatasetResponse_creationTime,
    describeDatasetResponse_dataFrequency,
    describeDatasetResponse_datasetArn,
    describeDatasetResponse_datasetName,
    describeDatasetResponse_datasetType,
    describeDatasetResponse_domain,
    describeDatasetResponse_encryptionConfig,
    describeDatasetResponse_lastModificationTime,
    describeDatasetResponse_schema,
    describeDatasetResponse_status,
    describeDatasetResponse_httpStatus,

    -- ** DescribeDatasetGroup
    describeDatasetGroup_datasetGroupArn,
    describeDatasetGroupResponse_creationTime,
    describeDatasetGroupResponse_datasetArns,
    describeDatasetGroupResponse_datasetGroupArn,
    describeDatasetGroupResponse_datasetGroupName,
    describeDatasetGroupResponse_domain,
    describeDatasetGroupResponse_lastModificationTime,
    describeDatasetGroupResponse_status,
    describeDatasetGroupResponse_httpStatus,

    -- ** DescribeDatasetImportJob
    describeDatasetImportJob_datasetImportJobArn,
    describeDatasetImportJobResponse_creationTime,
    describeDatasetImportJobResponse_dataSize,
    describeDatasetImportJobResponse_dataSource,
    describeDatasetImportJobResponse_datasetArn,
    describeDatasetImportJobResponse_datasetImportJobArn,
    describeDatasetImportJobResponse_datasetImportJobName,
    describeDatasetImportJobResponse_estimatedTimeRemainingInMinutes,
    describeDatasetImportJobResponse_fieldStatistics,
    describeDatasetImportJobResponse_format,
    describeDatasetImportJobResponse_geolocationFormat,
    describeDatasetImportJobResponse_lastModificationTime,
    describeDatasetImportJobResponse_message,
    describeDatasetImportJobResponse_status,
    describeDatasetImportJobResponse_timeZone,
    describeDatasetImportJobResponse_timestampFormat,
    describeDatasetImportJobResponse_useGeolocationForTimeZone,
    describeDatasetImportJobResponse_httpStatus,

    -- ** DescribeExplainability
    describeExplainability_explainabilityArn,
    describeExplainabilityResponse_creationTime,
    describeExplainabilityResponse_dataSource,
    describeExplainabilityResponse_enableVisualization,
    describeExplainabilityResponse_endDateTime,
    describeExplainabilityResponse_estimatedTimeRemainingInMinutes,
    describeExplainabilityResponse_explainabilityArn,
    describeExplainabilityResponse_explainabilityConfig,
    describeExplainabilityResponse_explainabilityName,
    describeExplainabilityResponse_lastModificationTime,
    describeExplainabilityResponse_message,
    describeExplainabilityResponse_resourceArn,
    describeExplainabilityResponse_schema,
    describeExplainabilityResponse_startDateTime,
    describeExplainabilityResponse_status,
    describeExplainabilityResponse_httpStatus,

    -- ** DescribeExplainabilityExport
    describeExplainabilityExport_explainabilityExportArn,
    describeExplainabilityExportResponse_creationTime,
    describeExplainabilityExportResponse_destination,
    describeExplainabilityExportResponse_explainabilityArn,
    describeExplainabilityExportResponse_explainabilityExportArn,
    describeExplainabilityExportResponse_explainabilityExportName,
    describeExplainabilityExportResponse_format,
    describeExplainabilityExportResponse_lastModificationTime,
    describeExplainabilityExportResponse_message,
    describeExplainabilityExportResponse_status,
    describeExplainabilityExportResponse_httpStatus,

    -- ** DescribeForecast
    describeForecast_forecastArn,
    describeForecastResponse_creationTime,
    describeForecastResponse_datasetGroupArn,
    describeForecastResponse_estimatedTimeRemainingInMinutes,
    describeForecastResponse_forecastArn,
    describeForecastResponse_forecastName,
    describeForecastResponse_forecastTypes,
    describeForecastResponse_lastModificationTime,
    describeForecastResponse_message,
    describeForecastResponse_predictorArn,
    describeForecastResponse_status,
    describeForecastResponse_timeSeriesSelector,
    describeForecastResponse_httpStatus,

    -- ** DescribeForecastExportJob
    describeForecastExportJob_forecastExportJobArn,
    describeForecastExportJobResponse_creationTime,
    describeForecastExportJobResponse_destination,
    describeForecastExportJobResponse_forecastArn,
    describeForecastExportJobResponse_forecastExportJobArn,
    describeForecastExportJobResponse_forecastExportJobName,
    describeForecastExportJobResponse_format,
    describeForecastExportJobResponse_lastModificationTime,
    describeForecastExportJobResponse_message,
    describeForecastExportJobResponse_status,
    describeForecastExportJobResponse_httpStatus,

    -- ** DescribeMonitor
    describeMonitor_monitorArn,
    describeMonitorResponse_baseline,
    describeMonitorResponse_creationTime,
    describeMonitorResponse_estimatedEvaluationTimeRemainingInMinutes,
    describeMonitorResponse_lastEvaluationState,
    describeMonitorResponse_lastEvaluationTime,
    describeMonitorResponse_lastModificationTime,
    describeMonitorResponse_message,
    describeMonitorResponse_monitorArn,
    describeMonitorResponse_monitorName,
    describeMonitorResponse_resourceArn,
    describeMonitorResponse_status,
    describeMonitorResponse_httpStatus,

    -- ** DescribePredictor
    describePredictor_predictorArn,
    describePredictorResponse_algorithmArn,
    describePredictorResponse_autoMLAlgorithmArns,
    describePredictorResponse_autoMLOverrideStrategy,
    describePredictorResponse_creationTime,
    describePredictorResponse_datasetImportJobArns,
    describePredictorResponse_encryptionConfig,
    describePredictorResponse_estimatedTimeRemainingInMinutes,
    describePredictorResponse_evaluationParameters,
    describePredictorResponse_featurizationConfig,
    describePredictorResponse_forecastHorizon,
    describePredictorResponse_forecastTypes,
    describePredictorResponse_hPOConfig,
    describePredictorResponse_inputDataConfig,
    describePredictorResponse_isAutoPredictor,
    describePredictorResponse_lastModificationTime,
    describePredictorResponse_message,
    describePredictorResponse_optimizationMetric,
    describePredictorResponse_performAutoML,
    describePredictorResponse_performHPO,
    describePredictorResponse_predictorArn,
    describePredictorResponse_predictorExecutionDetails,
    describePredictorResponse_predictorName,
    describePredictorResponse_status,
    describePredictorResponse_trainingParameters,
    describePredictorResponse_httpStatus,

    -- ** DescribePredictorBacktestExportJob
    describePredictorBacktestExportJob_predictorBacktestExportJobArn,
    describePredictorBacktestExportJobResponse_creationTime,
    describePredictorBacktestExportJobResponse_destination,
    describePredictorBacktestExportJobResponse_format,
    describePredictorBacktestExportJobResponse_lastModificationTime,
    describePredictorBacktestExportJobResponse_message,
    describePredictorBacktestExportJobResponse_predictorArn,
    describePredictorBacktestExportJobResponse_predictorBacktestExportJobArn,
    describePredictorBacktestExportJobResponse_predictorBacktestExportJobName,
    describePredictorBacktestExportJobResponse_status,
    describePredictorBacktestExportJobResponse_httpStatus,

    -- ** DescribeWhatIfAnalysis
    describeWhatIfAnalysis_whatIfAnalysisArn,
    describeWhatIfAnalysisResponse_creationTime,
    describeWhatIfAnalysisResponse_estimatedTimeRemainingInMinutes,
    describeWhatIfAnalysisResponse_forecastArn,
    describeWhatIfAnalysisResponse_lastModificationTime,
    describeWhatIfAnalysisResponse_message,
    describeWhatIfAnalysisResponse_status,
    describeWhatIfAnalysisResponse_timeSeriesSelector,
    describeWhatIfAnalysisResponse_whatIfAnalysisArn,
    describeWhatIfAnalysisResponse_whatIfAnalysisName,
    describeWhatIfAnalysisResponse_httpStatus,

    -- ** DescribeWhatIfForecast
    describeWhatIfForecast_whatIfForecastArn,
    describeWhatIfForecastResponse_creationTime,
    describeWhatIfForecastResponse_estimatedTimeRemainingInMinutes,
    describeWhatIfForecastResponse_forecastTypes,
    describeWhatIfForecastResponse_lastModificationTime,
    describeWhatIfForecastResponse_message,
    describeWhatIfForecastResponse_status,
    describeWhatIfForecastResponse_timeSeriesReplacementsDataSource,
    describeWhatIfForecastResponse_timeSeriesTransformations,
    describeWhatIfForecastResponse_whatIfAnalysisArn,
    describeWhatIfForecastResponse_whatIfForecastArn,
    describeWhatIfForecastResponse_whatIfForecastName,
    describeWhatIfForecastResponse_httpStatus,

    -- ** DescribeWhatIfForecastExport
    describeWhatIfForecastExport_whatIfForecastExportArn,
    describeWhatIfForecastExportResponse_creationTime,
    describeWhatIfForecastExportResponse_destination,
    describeWhatIfForecastExportResponse_estimatedTimeRemainingInMinutes,
    describeWhatIfForecastExportResponse_format,
    describeWhatIfForecastExportResponse_lastModificationTime,
    describeWhatIfForecastExportResponse_message,
    describeWhatIfForecastExportResponse_status,
    describeWhatIfForecastExportResponse_whatIfForecastArns,
    describeWhatIfForecastExportResponse_whatIfForecastExportArn,
    describeWhatIfForecastExportResponse_whatIfForecastExportName,
    describeWhatIfForecastExportResponse_httpStatus,

    -- ** GetAccuracyMetrics
    getAccuracyMetrics_predictorArn,
    getAccuracyMetricsResponse_autoMLOverrideStrategy,
    getAccuracyMetricsResponse_isAutoPredictor,
    getAccuracyMetricsResponse_optimizationMetric,
    getAccuracyMetricsResponse_predictorEvaluationResults,
    getAccuracyMetricsResponse_httpStatus,

    -- ** ListDatasetGroups
    listDatasetGroups_maxResults,
    listDatasetGroups_nextToken,
    listDatasetGroupsResponse_datasetGroups,
    listDatasetGroupsResponse_nextToken,
    listDatasetGroupsResponse_httpStatus,

    -- ** ListDatasetImportJobs
    listDatasetImportJobs_filters,
    listDatasetImportJobs_maxResults,
    listDatasetImportJobs_nextToken,
    listDatasetImportJobsResponse_datasetImportJobs,
    listDatasetImportJobsResponse_nextToken,
    listDatasetImportJobsResponse_httpStatus,

    -- ** ListDatasets
    listDatasets_maxResults,
    listDatasets_nextToken,
    listDatasetsResponse_datasets,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_httpStatus,

    -- ** ListExplainabilities
    listExplainabilities_filters,
    listExplainabilities_maxResults,
    listExplainabilities_nextToken,
    listExplainabilitiesResponse_explainabilities,
    listExplainabilitiesResponse_nextToken,
    listExplainabilitiesResponse_httpStatus,

    -- ** ListExplainabilityExports
    listExplainabilityExports_filters,
    listExplainabilityExports_maxResults,
    listExplainabilityExports_nextToken,
    listExplainabilityExportsResponse_explainabilityExports,
    listExplainabilityExportsResponse_nextToken,
    listExplainabilityExportsResponse_httpStatus,

    -- ** ListForecastExportJobs
    listForecastExportJobs_filters,
    listForecastExportJobs_maxResults,
    listForecastExportJobs_nextToken,
    listForecastExportJobsResponse_forecastExportJobs,
    listForecastExportJobsResponse_nextToken,
    listForecastExportJobsResponse_httpStatus,

    -- ** ListForecasts
    listForecasts_filters,
    listForecasts_maxResults,
    listForecasts_nextToken,
    listForecastsResponse_forecasts,
    listForecastsResponse_nextToken,
    listForecastsResponse_httpStatus,

    -- ** ListMonitorEvaluations
    listMonitorEvaluations_filters,
    listMonitorEvaluations_maxResults,
    listMonitorEvaluations_nextToken,
    listMonitorEvaluations_monitorArn,
    listMonitorEvaluationsResponse_nextToken,
    listMonitorEvaluationsResponse_predictorMonitorEvaluations,
    listMonitorEvaluationsResponse_httpStatus,

    -- ** ListMonitors
    listMonitors_filters,
    listMonitors_maxResults,
    listMonitors_nextToken,
    listMonitorsResponse_monitors,
    listMonitorsResponse_nextToken,
    listMonitorsResponse_httpStatus,

    -- ** ListPredictorBacktestExportJobs
    listPredictorBacktestExportJobs_filters,
    listPredictorBacktestExportJobs_maxResults,
    listPredictorBacktestExportJobs_nextToken,
    listPredictorBacktestExportJobsResponse_nextToken,
    listPredictorBacktestExportJobsResponse_predictorBacktestExportJobs,
    listPredictorBacktestExportJobsResponse_httpStatus,

    -- ** ListPredictors
    listPredictors_filters,
    listPredictors_maxResults,
    listPredictors_nextToken,
    listPredictorsResponse_nextToken,
    listPredictorsResponse_predictors,
    listPredictorsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWhatIfAnalyses
    listWhatIfAnalyses_filters,
    listWhatIfAnalyses_maxResults,
    listWhatIfAnalyses_nextToken,
    listWhatIfAnalysesResponse_nextToken,
    listWhatIfAnalysesResponse_whatIfAnalyses,
    listWhatIfAnalysesResponse_httpStatus,

    -- ** ListWhatIfForecastExports
    listWhatIfForecastExports_filters,
    listWhatIfForecastExports_maxResults,
    listWhatIfForecastExports_nextToken,
    listWhatIfForecastExportsResponse_nextToken,
    listWhatIfForecastExportsResponse_whatIfForecastExports,
    listWhatIfForecastExportsResponse_httpStatus,

    -- ** ListWhatIfForecasts
    listWhatIfForecasts_filters,
    listWhatIfForecasts_maxResults,
    listWhatIfForecasts_nextToken,
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
    datasetGroupSummary_creationTime,
    datasetGroupSummary_datasetGroupArn,
    datasetGroupSummary_datasetGroupName,
    datasetGroupSummary_lastModificationTime,

    -- ** DatasetImportJobSummary
    datasetImportJobSummary_creationTime,
    datasetImportJobSummary_dataSource,
    datasetImportJobSummary_datasetImportJobArn,
    datasetImportJobSummary_datasetImportJobName,
    datasetImportJobSummary_lastModificationTime,
    datasetImportJobSummary_message,
    datasetImportJobSummary_status,

    -- ** DatasetSummary
    datasetSummary_creationTime,
    datasetSummary_datasetArn,
    datasetSummary_datasetName,
    datasetSummary_datasetType,
    datasetSummary_domain,
    datasetSummary_lastModificationTime,

    -- ** EncryptionConfig
    encryptionConfig_roleArn,
    encryptionConfig_kmsKeyArn,

    -- ** ErrorMetric
    errorMetric_forecastType,
    errorMetric_mape,
    errorMetric_mase,
    errorMetric_rmse,
    errorMetric_wape,

    -- ** EvaluationParameters
    evaluationParameters_backTestWindowOffset,
    evaluationParameters_numberOfBacktestWindows,

    -- ** EvaluationResult
    evaluationResult_algorithmArn,
    evaluationResult_testWindows,

    -- ** ExplainabilityConfig
    explainabilityConfig_timeSeriesGranularity,
    explainabilityConfig_timePointGranularity,

    -- ** ExplainabilityExportSummary
    explainabilityExportSummary_creationTime,
    explainabilityExportSummary_destination,
    explainabilityExportSummary_explainabilityExportArn,
    explainabilityExportSummary_explainabilityExportName,
    explainabilityExportSummary_lastModificationTime,
    explainabilityExportSummary_message,
    explainabilityExportSummary_status,

    -- ** ExplainabilityInfo
    explainabilityInfo_explainabilityArn,
    explainabilityInfo_status,

    -- ** ExplainabilitySummary
    explainabilitySummary_creationTime,
    explainabilitySummary_explainabilityArn,
    explainabilitySummary_explainabilityConfig,
    explainabilitySummary_explainabilityName,
    explainabilitySummary_lastModificationTime,
    explainabilitySummary_message,
    explainabilitySummary_resourceArn,
    explainabilitySummary_status,

    -- ** Featurization
    featurization_featurizationPipeline,
    featurization_attributeName,

    -- ** FeaturizationConfig
    featurizationConfig_featurizations,
    featurizationConfig_forecastDimensions,
    featurizationConfig_forecastFrequency,

    -- ** FeaturizationMethod
    featurizationMethod_featurizationMethodParameters,
    featurizationMethod_featurizationMethodName,

    -- ** Filter
    filter_key,
    filter_value,
    filter_condition,

    -- ** ForecastExportJobSummary
    forecastExportJobSummary_creationTime,
    forecastExportJobSummary_destination,
    forecastExportJobSummary_forecastExportJobArn,
    forecastExportJobSummary_forecastExportJobName,
    forecastExportJobSummary_lastModificationTime,
    forecastExportJobSummary_message,
    forecastExportJobSummary_status,

    -- ** ForecastSummary
    forecastSummary_createdUsingAutoPredictor,
    forecastSummary_creationTime,
    forecastSummary_datasetGroupArn,
    forecastSummary_forecastArn,
    forecastSummary_forecastName,
    forecastSummary_lastModificationTime,
    forecastSummary_message,
    forecastSummary_predictorArn,
    forecastSummary_status,

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
    metricResult_metricName,
    metricResult_metricValue,

    -- ** Metrics
    metrics_averageWeightedQuantileLoss,
    metrics_errorMetrics,
    metrics_rmse,
    metrics_weightedQuantileLosses,

    -- ** MonitorConfig
    monitorConfig_monitorName,

    -- ** MonitorDataSource
    monitorDataSource_datasetImportJobArn,
    monitorDataSource_forecastArn,
    monitorDataSource_predictorArn,

    -- ** MonitorInfo
    monitorInfo_monitorArn,
    monitorInfo_status,

    -- ** MonitorSummary
    monitorSummary_creationTime,
    monitorSummary_lastModificationTime,
    monitorSummary_monitorArn,
    monitorSummary_monitorName,
    monitorSummary_resourceArn,
    monitorSummary_status,

    -- ** ParameterRanges
    parameterRanges_categoricalParameterRanges,
    parameterRanges_continuousParameterRanges,
    parameterRanges_integerParameterRanges,

    -- ** PredictorBacktestExportJobSummary
    predictorBacktestExportJobSummary_creationTime,
    predictorBacktestExportJobSummary_destination,
    predictorBacktestExportJobSummary_lastModificationTime,
    predictorBacktestExportJobSummary_message,
    predictorBacktestExportJobSummary_predictorBacktestExportJobArn,
    predictorBacktestExportJobSummary_predictorBacktestExportJobName,
    predictorBacktestExportJobSummary_status,

    -- ** PredictorBaseline
    predictorBaseline_baselineMetrics,

    -- ** PredictorEvent
    predictorEvent_datetime,
    predictorEvent_detail,

    -- ** PredictorExecution
    predictorExecution_algorithmArn,
    predictorExecution_testWindows,

    -- ** PredictorExecutionDetails
    predictorExecutionDetails_predictorExecutions,

    -- ** PredictorMonitorEvaluation
    predictorMonitorEvaluation_evaluationState,
    predictorMonitorEvaluation_evaluationTime,
    predictorMonitorEvaluation_message,
    predictorMonitorEvaluation_metricResults,
    predictorMonitorEvaluation_monitorArn,
    predictorMonitorEvaluation_monitorDataSource,
    predictorMonitorEvaluation_numItemsEvaluated,
    predictorMonitorEvaluation_predictorEvent,
    predictorMonitorEvaluation_resourceArn,
    predictorMonitorEvaluation_windowEndDatetime,
    predictorMonitorEvaluation_windowStartDatetime,

    -- ** PredictorSummary
    predictorSummary_creationTime,
    predictorSummary_datasetGroupArn,
    predictorSummary_isAutoPredictor,
    predictorSummary_lastModificationTime,
    predictorSummary_message,
    predictorSummary_predictorArn,
    predictorSummary_predictorName,
    predictorSummary_referencePredictorSummary,
    predictorSummary_status,

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
    schemaAttribute_attributeName,
    schemaAttribute_attributeType,

    -- ** Statistics
    statistics_avg,
    statistics_count,
    statistics_countDistinct,
    statistics_countDistinctLong,
    statistics_countLong,
    statistics_countNan,
    statistics_countNanLong,
    statistics_countNull,
    statistics_countNullLong,
    statistics_max,
    statistics_min,
    statistics_stddev,

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
    timeAlignmentBoundary_dayOfMonth,
    timeAlignmentBoundary_dayOfWeek,
    timeAlignmentBoundary_hour,
    timeAlignmentBoundary_month,

    -- ** TimeSeriesCondition
    timeSeriesCondition_attributeName,
    timeSeriesCondition_attributeValue,
    timeSeriesCondition_condition,

    -- ** TimeSeriesIdentifiers
    timeSeriesIdentifiers_dataSource,
    timeSeriesIdentifiers_format,
    timeSeriesIdentifiers_schema,

    -- ** TimeSeriesReplacementsDataSource
    timeSeriesReplacementsDataSource_format,
    timeSeriesReplacementsDataSource_timestampFormat,
    timeSeriesReplacementsDataSource_s3Config,
    timeSeriesReplacementsDataSource_schema,

    -- ** TimeSeriesSelector
    timeSeriesSelector_timeSeriesIdentifiers,

    -- ** TimeSeriesTransformation
    timeSeriesTransformation_action,
    timeSeriesTransformation_timeSeriesConditions,

    -- ** WeightedQuantileLoss
    weightedQuantileLoss_lossValue,
    weightedQuantileLoss_quantile,

    -- ** WhatIfAnalysisSummary
    whatIfAnalysisSummary_creationTime,
    whatIfAnalysisSummary_forecastArn,
    whatIfAnalysisSummary_lastModificationTime,
    whatIfAnalysisSummary_message,
    whatIfAnalysisSummary_status,
    whatIfAnalysisSummary_whatIfAnalysisArn,
    whatIfAnalysisSummary_whatIfAnalysisName,

    -- ** WhatIfForecastExportSummary
    whatIfForecastExportSummary_creationTime,
    whatIfForecastExportSummary_destination,
    whatIfForecastExportSummary_lastModificationTime,
    whatIfForecastExportSummary_message,
    whatIfForecastExportSummary_status,
    whatIfForecastExportSummary_whatIfForecastArns,
    whatIfForecastExportSummary_whatIfForecastExportArn,
    whatIfForecastExportSummary_whatIfForecastExportName,

    -- ** WhatIfForecastSummary
    whatIfForecastSummary_creationTime,
    whatIfForecastSummary_lastModificationTime,
    whatIfForecastSummary_message,
    whatIfForecastSummary_status,
    whatIfForecastSummary_whatIfAnalysisArn,
    whatIfForecastSummary_whatIfForecastArn,
    whatIfForecastSummary_whatIfForecastName,

    -- ** WindowSummary
    windowSummary_evaluationType,
    windowSummary_itemCount,
    windowSummary_metrics,
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
