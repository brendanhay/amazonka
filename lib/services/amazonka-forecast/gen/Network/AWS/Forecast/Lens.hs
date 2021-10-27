{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Forecast.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Forecast.Lens
  ( -- * Operations

    -- ** ListDatasetGroups
    listDatasetGroups_nextToken,
    listDatasetGroups_maxResults,
    listDatasetGroupsResponse_nextToken,
    listDatasetGroupsResponse_datasetGroups,
    listDatasetGroupsResponse_httpStatus,

    -- ** CreateDatasetImportJob
    createDatasetImportJob_timestampFormat,
    createDatasetImportJob_useGeolocationForTimeZone,
    createDatasetImportJob_geolocationFormat,
    createDatasetImportJob_timeZone,
    createDatasetImportJob_tags,
    createDatasetImportJob_datasetImportJobName,
    createDatasetImportJob_datasetArn,
    createDatasetImportJob_dataSource,
    createDatasetImportJobResponse_datasetImportJobArn,
    createDatasetImportJobResponse_httpStatus,

    -- ** DescribeDataset
    describeDataset_datasetArn,
    describeDatasetResponse_creationTime,
    describeDatasetResponse_status,
    describeDatasetResponse_dataFrequency,
    describeDatasetResponse_datasetArn,
    describeDatasetResponse_domain,
    describeDatasetResponse_schema,
    describeDatasetResponse_datasetType,
    describeDatasetResponse_datasetName,
    describeDatasetResponse_encryptionConfig,
    describeDatasetResponse_lastModificationTime,
    describeDatasetResponse_httpStatus,

    -- ** ListForecasts
    listForecasts_filters,
    listForecasts_nextToken,
    listForecasts_maxResults,
    listForecastsResponse_forecasts,
    listForecastsResponse_nextToken,
    listForecastsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StopResource
    stopResource_resourceArn,

    -- ** DescribeDatasetImportJob
    describeDatasetImportJob_datasetImportJobArn,
    describeDatasetImportJobResponse_creationTime,
    describeDatasetImportJobResponse_status,
    describeDatasetImportJobResponse_datasetImportJobName,
    describeDatasetImportJobResponse_datasetArn,
    describeDatasetImportJobResponse_timestampFormat,
    describeDatasetImportJobResponse_dataSize,
    describeDatasetImportJobResponse_estimatedTimeRemainingInMinutes,
    describeDatasetImportJobResponse_fieldStatistics,
    describeDatasetImportJobResponse_dataSource,
    describeDatasetImportJobResponse_datasetImportJobArn,
    describeDatasetImportJobResponse_useGeolocationForTimeZone,
    describeDatasetImportJobResponse_message,
    describeDatasetImportJobResponse_geolocationFormat,
    describeDatasetImportJobResponse_timeZone,
    describeDatasetImportJobResponse_lastModificationTime,
    describeDatasetImportJobResponse_httpStatus,

    -- ** DescribeForecastExportJob
    describeForecastExportJob_forecastExportJobArn,
    describeForecastExportJobResponse_creationTime,
    describeForecastExportJobResponse_status,
    describeForecastExportJobResponse_destination,
    describeForecastExportJobResponse_forecastExportJobArn,
    describeForecastExportJobResponse_forecastArn,
    describeForecastExportJobResponse_forecastExportJobName,
    describeForecastExportJobResponse_message,
    describeForecastExportJobResponse_lastModificationTime,
    describeForecastExportJobResponse_httpStatus,

    -- ** DescribePredictor
    describePredictor_predictorArn,
    describePredictorResponse_creationTime,
    describePredictorResponse_forecastHorizon,
    describePredictorResponse_status,
    describePredictorResponse_performAutoML,
    describePredictorResponse_autoMLAlgorithmArns,
    describePredictorResponse_trainingParameters,
    describePredictorResponse_algorithmArn,
    describePredictorResponse_hPOConfig,
    describePredictorResponse_predictorArn,
    describePredictorResponse_optimizationMetric,
    describePredictorResponse_predictorExecutionDetails,
    describePredictorResponse_datasetImportJobArns,
    describePredictorResponse_estimatedTimeRemainingInMinutes,
    describePredictorResponse_autoMLOverrideStrategy,
    describePredictorResponse_evaluationParameters,
    describePredictorResponse_inputDataConfig,
    describePredictorResponse_predictorName,
    describePredictorResponse_featurizationConfig,
    describePredictorResponse_encryptionConfig,
    describePredictorResponse_forecastTypes,
    describePredictorResponse_message,
    describePredictorResponse_performHPO,
    describePredictorResponse_lastModificationTime,
    describePredictorResponse_httpStatus,

    -- ** DeleteDataset
    deleteDataset_datasetArn,

    -- ** DescribeForecast
    describeForecast_forecastArn,
    describeForecastResponse_creationTime,
    describeForecastResponse_status,
    describeForecastResponse_predictorArn,
    describeForecastResponse_forecastArn,
    describeForecastResponse_estimatedTimeRemainingInMinutes,
    describeForecastResponse_forecastName,
    describeForecastResponse_forecastTypes,
    describeForecastResponse_datasetGroupArn,
    describeForecastResponse_message,
    describeForecastResponse_lastModificationTime,
    describeForecastResponse_httpStatus,

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

    -- ** DeleteForecastExportJob
    deleteForecastExportJob_forecastExportJobArn,

    -- ** DeletePredictor
    deletePredictor_predictorArn,

    -- ** ListDatasetImportJobs
    listDatasetImportJobs_filters,
    listDatasetImportJobs_nextToken,
    listDatasetImportJobs_maxResults,
    listDatasetImportJobsResponse_datasetImportJobs,
    listDatasetImportJobsResponse_nextToken,
    listDatasetImportJobsResponse_httpStatus,

    -- ** DeleteDatasetImportJob
    deleteDatasetImportJob_datasetImportJobArn,

    -- ** GetAccuracyMetrics
    getAccuracyMetrics_predictorArn,
    getAccuracyMetricsResponse_predictorEvaluationResults,
    getAccuracyMetricsResponse_optimizationMetric,
    getAccuracyMetricsResponse_autoMLOverrideStrategy,
    getAccuracyMetricsResponse_httpStatus,

    -- ** DeleteDatasetGroup
    deleteDatasetGroup_datasetGroupArn,

    -- ** UpdateDatasetGroup
    updateDatasetGroup_datasetGroupArn,
    updateDatasetGroup_datasetArns,
    updateDatasetGroupResponse_httpStatus,

    -- ** CreateForecastExportJob
    createForecastExportJob_tags,
    createForecastExportJob_forecastExportJobName,
    createForecastExportJob_forecastArn,
    createForecastExportJob_destination,
    createForecastExportJobResponse_forecastExportJobArn,
    createForecastExportJobResponse_httpStatus,

    -- ** CreatePredictor
    createPredictor_performAutoML,
    createPredictor_trainingParameters,
    createPredictor_algorithmArn,
    createPredictor_hPOConfig,
    createPredictor_optimizationMetric,
    createPredictor_autoMLOverrideStrategy,
    createPredictor_evaluationParameters,
    createPredictor_encryptionConfig,
    createPredictor_forecastTypes,
    createPredictor_performHPO,
    createPredictor_tags,
    createPredictor_predictorName,
    createPredictor_forecastHorizon,
    createPredictor_inputDataConfig,
    createPredictor_featurizationConfig,
    createPredictorResponse_predictorArn,
    createPredictorResponse_httpStatus,

    -- ** ListPredictorBacktestExportJobs
    listPredictorBacktestExportJobs_filters,
    listPredictorBacktestExportJobs_nextToken,
    listPredictorBacktestExportJobs_maxResults,
    listPredictorBacktestExportJobsResponse_nextToken,
    listPredictorBacktestExportJobsResponse_predictorBacktestExportJobs,
    listPredictorBacktestExportJobsResponse_httpStatus,

    -- ** DeletePredictorBacktestExportJob
    deletePredictorBacktestExportJob_predictorBacktestExportJobArn,

    -- ** CreateForecast
    createForecast_forecastTypes,
    createForecast_tags,
    createForecast_forecastName,
    createForecast_predictorArn,
    createForecastResponse_forecastArn,
    createForecastResponse_httpStatus,

    -- ** CreatePredictorBacktestExportJob
    createPredictorBacktestExportJob_tags,
    createPredictorBacktestExportJob_predictorBacktestExportJobName,
    createPredictorBacktestExportJob_predictorArn,
    createPredictorBacktestExportJob_destination,
    createPredictorBacktestExportJobResponse_predictorBacktestExportJobArn,
    createPredictorBacktestExportJobResponse_httpStatus,

    -- ** DeleteForecast
    deleteForecast_forecastArn,

    -- ** DeleteResourceTree
    deleteResourceTree_resourceArn,

    -- ** DescribeDatasetGroup
    describeDatasetGroup_datasetGroupArn,
    describeDatasetGroupResponse_creationTime,
    describeDatasetGroupResponse_datasetArns,
    describeDatasetGroupResponse_status,
    describeDatasetGroupResponse_domain,
    describeDatasetGroupResponse_datasetGroupName,
    describeDatasetGroupResponse_datasetGroupArn,
    describeDatasetGroupResponse_lastModificationTime,
    describeDatasetGroupResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListDatasets
    listDatasets_nextToken,
    listDatasets_maxResults,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_datasets,
    listDatasetsResponse_httpStatus,

    -- ** DescribePredictorBacktestExportJob
    describePredictorBacktestExportJob_predictorBacktestExportJobArn,
    describePredictorBacktestExportJobResponse_creationTime,
    describePredictorBacktestExportJobResponse_status,
    describePredictorBacktestExportJobResponse_destination,
    describePredictorBacktestExportJobResponse_predictorArn,
    describePredictorBacktestExportJobResponse_predictorBacktestExportJobArn,
    describePredictorBacktestExportJobResponse_message,
    describePredictorBacktestExportJobResponse_predictorBacktestExportJobName,
    describePredictorBacktestExportJobResponse_lastModificationTime,
    describePredictorBacktestExportJobResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** CreateDatasetGroup
    createDatasetGroup_datasetArns,
    createDatasetGroup_tags,
    createDatasetGroup_datasetGroupName,
    createDatasetGroup_domain,
    createDatasetGroupResponse_datasetGroupArn,
    createDatasetGroupResponse_httpStatus,

    -- ** ListForecastExportJobs
    listForecastExportJobs_filters,
    listForecastExportJobs_nextToken,
    listForecastExportJobs_maxResults,
    listForecastExportJobsResponse_nextToken,
    listForecastExportJobsResponse_forecastExportJobs,
    listForecastExportJobsResponse_httpStatus,

    -- ** ListPredictors
    listPredictors_filters,
    listPredictors_nextToken,
    listPredictors_maxResults,
    listPredictorsResponse_nextToken,
    listPredictorsResponse_predictors,
    listPredictorsResponse_httpStatus,

    -- * Types

    -- ** CategoricalParameterRange
    categoricalParameterRange_name,
    categoricalParameterRange_values,

    -- ** ContinuousParameterRange
    continuousParameterRange_scalingType,
    continuousParameterRange_name,
    continuousParameterRange_maxValue,
    continuousParameterRange_minValue,

    -- ** DataDestination
    dataDestination_s3Config,

    -- ** DataSource
    dataSource_s3Config,

    -- ** DatasetGroupSummary
    datasetGroupSummary_creationTime,
    datasetGroupSummary_datasetGroupName,
    datasetGroupSummary_datasetGroupArn,
    datasetGroupSummary_lastModificationTime,

    -- ** DatasetImportJobSummary
    datasetImportJobSummary_creationTime,
    datasetImportJobSummary_status,
    datasetImportJobSummary_datasetImportJobName,
    datasetImportJobSummary_dataSource,
    datasetImportJobSummary_datasetImportJobArn,
    datasetImportJobSummary_message,
    datasetImportJobSummary_lastModificationTime,

    -- ** DatasetSummary
    datasetSummary_creationTime,
    datasetSummary_datasetArn,
    datasetSummary_domain,
    datasetSummary_datasetType,
    datasetSummary_datasetName,
    datasetSummary_lastModificationTime,

    -- ** EncryptionConfig
    encryptionConfig_roleArn,
    encryptionConfig_kmsKeyArn,

    -- ** ErrorMetric
    errorMetric_mase,
    errorMetric_wape,
    errorMetric_mape,
    errorMetric_rmse,
    errorMetric_forecastType,

    -- ** EvaluationParameters
    evaluationParameters_backTestWindowOffset,
    evaluationParameters_numberOfBacktestWindows,

    -- ** EvaluationResult
    evaluationResult_algorithmArn,
    evaluationResult_testWindows,

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
    forecastExportJobSummary_status,
    forecastExportJobSummary_destination,
    forecastExportJobSummary_forecastExportJobArn,
    forecastExportJobSummary_forecastExportJobName,
    forecastExportJobSummary_message,
    forecastExportJobSummary_lastModificationTime,

    -- ** ForecastSummary
    forecastSummary_creationTime,
    forecastSummary_status,
    forecastSummary_predictorArn,
    forecastSummary_forecastArn,
    forecastSummary_forecastName,
    forecastSummary_datasetGroupArn,
    forecastSummary_message,
    forecastSummary_lastModificationTime,

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

    -- ** Metrics
    metrics_errorMetrics,
    metrics_rmse,
    metrics_weightedQuantileLosses,
    metrics_averageWeightedQuantileLoss,

    -- ** ParameterRanges
    parameterRanges_categoricalParameterRanges,
    parameterRanges_integerParameterRanges,
    parameterRanges_continuousParameterRanges,

    -- ** PredictorBacktestExportJobSummary
    predictorBacktestExportJobSummary_creationTime,
    predictorBacktestExportJobSummary_status,
    predictorBacktestExportJobSummary_destination,
    predictorBacktestExportJobSummary_predictorBacktestExportJobArn,
    predictorBacktestExportJobSummary_message,
    predictorBacktestExportJobSummary_predictorBacktestExportJobName,
    predictorBacktestExportJobSummary_lastModificationTime,

    -- ** PredictorExecution
    predictorExecution_algorithmArn,
    predictorExecution_testWindows,

    -- ** PredictorExecutionDetails
    predictorExecutionDetails_predictorExecutions,

    -- ** PredictorSummary
    predictorSummary_creationTime,
    predictorSummary_status,
    predictorSummary_predictorArn,
    predictorSummary_predictorName,
    predictorSummary_datasetGroupArn,
    predictorSummary_message,
    predictorSummary_lastModificationTime,

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
    statistics_max,
    statistics_countNullLong,
    statistics_countNan,
    statistics_countNanLong,
    statistics_avg,
    statistics_countNull,
    statistics_count,
    statistics_countLong,
    statistics_stddev,
    statistics_min,
    statistics_countDistinctLong,
    statistics_countDistinct,

    -- ** SupplementaryFeature
    supplementaryFeature_name,
    supplementaryFeature_value,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TestWindowSummary
    testWindowSummary_status,
    testWindowSummary_testWindowEnd,
    testWindowSummary_testWindowStart,
    testWindowSummary_message,

    -- ** WeightedQuantileLoss
    weightedQuantileLoss_quantile,
    weightedQuantileLoss_lossValue,

    -- ** WindowSummary
    windowSummary_metrics,
    windowSummary_testWindowEnd,
    windowSummary_evaluationType,
    windowSummary_testWindowStart,
    windowSummary_itemCount,
  )
where

import Network.AWS.Forecast.CreateDataset
import Network.AWS.Forecast.CreateDatasetGroup
import Network.AWS.Forecast.CreateDatasetImportJob
import Network.AWS.Forecast.CreateForecast
import Network.AWS.Forecast.CreateForecastExportJob
import Network.AWS.Forecast.CreatePredictor
import Network.AWS.Forecast.CreatePredictorBacktestExportJob
import Network.AWS.Forecast.DeleteDataset
import Network.AWS.Forecast.DeleteDatasetGroup
import Network.AWS.Forecast.DeleteDatasetImportJob
import Network.AWS.Forecast.DeleteForecast
import Network.AWS.Forecast.DeleteForecastExportJob
import Network.AWS.Forecast.DeletePredictor
import Network.AWS.Forecast.DeletePredictorBacktestExportJob
import Network.AWS.Forecast.DeleteResourceTree
import Network.AWS.Forecast.DescribeDataset
import Network.AWS.Forecast.DescribeDatasetGroup
import Network.AWS.Forecast.DescribeDatasetImportJob
import Network.AWS.Forecast.DescribeForecast
import Network.AWS.Forecast.DescribeForecastExportJob
import Network.AWS.Forecast.DescribePredictor
import Network.AWS.Forecast.DescribePredictorBacktestExportJob
import Network.AWS.Forecast.GetAccuracyMetrics
import Network.AWS.Forecast.ListDatasetGroups
import Network.AWS.Forecast.ListDatasetImportJobs
import Network.AWS.Forecast.ListDatasets
import Network.AWS.Forecast.ListForecastExportJobs
import Network.AWS.Forecast.ListForecasts
import Network.AWS.Forecast.ListPredictorBacktestExportJobs
import Network.AWS.Forecast.ListPredictors
import Network.AWS.Forecast.ListTagsForResource
import Network.AWS.Forecast.StopResource
import Network.AWS.Forecast.TagResource
import Network.AWS.Forecast.Types.CategoricalParameterRange
import Network.AWS.Forecast.Types.ContinuousParameterRange
import Network.AWS.Forecast.Types.DataDestination
import Network.AWS.Forecast.Types.DataSource
import Network.AWS.Forecast.Types.DatasetGroupSummary
import Network.AWS.Forecast.Types.DatasetImportJobSummary
import Network.AWS.Forecast.Types.DatasetSummary
import Network.AWS.Forecast.Types.EncryptionConfig
import Network.AWS.Forecast.Types.ErrorMetric
import Network.AWS.Forecast.Types.EvaluationParameters
import Network.AWS.Forecast.Types.EvaluationResult
import Network.AWS.Forecast.Types.Featurization
import Network.AWS.Forecast.Types.FeaturizationConfig
import Network.AWS.Forecast.Types.FeaturizationMethod
import Network.AWS.Forecast.Types.Filter
import Network.AWS.Forecast.Types.ForecastExportJobSummary
import Network.AWS.Forecast.Types.ForecastSummary
import Network.AWS.Forecast.Types.HyperParameterTuningJobConfig
import Network.AWS.Forecast.Types.InputDataConfig
import Network.AWS.Forecast.Types.IntegerParameterRange
import Network.AWS.Forecast.Types.Metrics
import Network.AWS.Forecast.Types.ParameterRanges
import Network.AWS.Forecast.Types.PredictorBacktestExportJobSummary
import Network.AWS.Forecast.Types.PredictorExecution
import Network.AWS.Forecast.Types.PredictorExecutionDetails
import Network.AWS.Forecast.Types.PredictorSummary
import Network.AWS.Forecast.Types.S3Config
import Network.AWS.Forecast.Types.Schema
import Network.AWS.Forecast.Types.SchemaAttribute
import Network.AWS.Forecast.Types.Statistics
import Network.AWS.Forecast.Types.SupplementaryFeature
import Network.AWS.Forecast.Types.Tag
import Network.AWS.Forecast.Types.TestWindowSummary
import Network.AWS.Forecast.Types.WeightedQuantileLoss
import Network.AWS.Forecast.Types.WindowSummary
import Network.AWS.Forecast.UntagResource
import Network.AWS.Forecast.UpdateDatasetGroup
