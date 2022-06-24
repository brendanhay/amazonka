{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Forecast.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Lens
  ( -- * Operations

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
    createDatasetImportJob_timeZone,
    createDatasetImportJob_useGeolocationForTimeZone,
    createDatasetImportJob_timestampFormat,
    createDatasetImportJob_geolocationFormat,
    createDatasetImportJob_datasetImportJobName,
    createDatasetImportJob_datasetArn,
    createDatasetImportJob_dataSource,
    createDatasetImportJobResponse_datasetImportJobArn,
    createDatasetImportJobResponse_httpStatus,

    -- ** CreateForecast
    createForecast_tags,
    createForecast_forecastTypes,
    createForecast_forecastName,
    createForecast_predictorArn,
    createForecastResponse_forecastArn,
    createForecastResponse_httpStatus,

    -- ** CreateForecastExportJob
    createForecastExportJob_tags,
    createForecastExportJob_forecastExportJobName,
    createForecastExportJob_forecastArn,
    createForecastExportJob_destination,
    createForecastExportJobResponse_forecastExportJobArn,
    createForecastExportJobResponse_httpStatus,

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
    createPredictorBacktestExportJob_predictorBacktestExportJobName,
    createPredictorBacktestExportJob_predictorArn,
    createPredictorBacktestExportJob_destination,
    createPredictorBacktestExportJobResponse_predictorBacktestExportJobArn,
    createPredictorBacktestExportJobResponse_httpStatus,

    -- ** DeleteDataset
    deleteDataset_datasetArn,

    -- ** DeleteDatasetGroup
    deleteDatasetGroup_datasetGroupArn,

    -- ** DeleteDatasetImportJob
    deleteDatasetImportJob_datasetImportJobArn,

    -- ** DeleteForecast
    deleteForecast_forecastArn,

    -- ** DeleteForecastExportJob
    deleteForecastExportJob_forecastExportJobArn,

    -- ** DeletePredictor
    deletePredictor_predictorArn,

    -- ** DeletePredictorBacktestExportJob
    deletePredictorBacktestExportJob_predictorBacktestExportJobArn,

    -- ** DeleteResourceTree
    deleteResourceTree_resourceArn,

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
    describeForecastResponse_forecastName,
    describeForecastResponse_httpStatus,

    -- ** DescribeForecastExportJob
    describeForecastExportJob_forecastExportJobArn,
    describeForecastExportJobResponse_lastModificationTime,
    describeForecastExportJobResponse_destination,
    describeForecastExportJobResponse_message,
    describeForecastExportJobResponse_forecastExportJobName,
    describeForecastExportJobResponse_forecastExportJobArn,
    describeForecastExportJobResponse_status,
    describeForecastExportJobResponse_forecastArn,
    describeForecastExportJobResponse_creationTime,
    describeForecastExportJobResponse_httpStatus,

    -- ** DescribePredictor
    describePredictor_predictorArn,
    describePredictorResponse_lastModificationTime,
    describePredictorResponse_encryptionConfig,
    describePredictorResponse_message,
    describePredictorResponse_performAutoML,
    describePredictorResponse_performHPO,
    describePredictorResponse_optimizationMetric,
    describePredictorResponse_evaluationParameters,
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
    describePredictorBacktestExportJobResponse_status,
    describePredictorBacktestExportJobResponse_predictorArn,
    describePredictorBacktestExportJobResponse_predictorBacktestExportJobName,
    describePredictorBacktestExportJobResponse_predictorBacktestExportJobArn,
    describePredictorBacktestExportJobResponse_creationTime,
    describePredictorBacktestExportJobResponse_httpStatus,

    -- ** GetAccuracyMetrics
    getAccuracyMetrics_predictorArn,
    getAccuracyMetricsResponse_predictorEvaluationResults,
    getAccuracyMetricsResponse_optimizationMetric,
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

    -- ** Metrics
    metrics_averageWeightedQuantileLoss,
    metrics_weightedQuantileLosses,
    metrics_errorMetrics,
    metrics_rmse,

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

    -- ** PredictorExecution
    predictorExecution_testWindows,
    predictorExecution_algorithmArn,

    -- ** PredictorExecutionDetails
    predictorExecutionDetails_predictorExecutions,

    -- ** PredictorSummary
    predictorSummary_lastModificationTime,
    predictorSummary_message,
    predictorSummary_predictorName,
    predictorSummary_status,
    predictorSummary_predictorArn,
    predictorSummary_creationTime,
    predictorSummary_datasetGroupArn,

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

    -- ** WeightedQuantileLoss
    weightedQuantileLoss_quantile,
    weightedQuantileLoss_lossValue,

    -- ** WindowSummary
    windowSummary_itemCount,
    windowSummary_metrics,
    windowSummary_evaluationType,
    windowSummary_testWindowEnd,
    windowSummary_testWindowStart,
  )
where

import Amazonka.Forecast.CreateDataset
import Amazonka.Forecast.CreateDatasetGroup
import Amazonka.Forecast.CreateDatasetImportJob
import Amazonka.Forecast.CreateForecast
import Amazonka.Forecast.CreateForecastExportJob
import Amazonka.Forecast.CreatePredictor
import Amazonka.Forecast.CreatePredictorBacktestExportJob
import Amazonka.Forecast.DeleteDataset
import Amazonka.Forecast.DeleteDatasetGroup
import Amazonka.Forecast.DeleteDatasetImportJob
import Amazonka.Forecast.DeleteForecast
import Amazonka.Forecast.DeleteForecastExportJob
import Amazonka.Forecast.DeletePredictor
import Amazonka.Forecast.DeletePredictorBacktestExportJob
import Amazonka.Forecast.DeleteResourceTree
import Amazonka.Forecast.DescribeDataset
import Amazonka.Forecast.DescribeDatasetGroup
import Amazonka.Forecast.DescribeDatasetImportJob
import Amazonka.Forecast.DescribeForecast
import Amazonka.Forecast.DescribeForecastExportJob
import Amazonka.Forecast.DescribePredictor
import Amazonka.Forecast.DescribePredictorBacktestExportJob
import Amazonka.Forecast.GetAccuracyMetrics
import Amazonka.Forecast.ListDatasetGroups
import Amazonka.Forecast.ListDatasetImportJobs
import Amazonka.Forecast.ListDatasets
import Amazonka.Forecast.ListForecastExportJobs
import Amazonka.Forecast.ListForecasts
import Amazonka.Forecast.ListPredictorBacktestExportJobs
import Amazonka.Forecast.ListPredictors
import Amazonka.Forecast.ListTagsForResource
import Amazonka.Forecast.StopResource
import Amazonka.Forecast.TagResource
import Amazonka.Forecast.Types.CategoricalParameterRange
import Amazonka.Forecast.Types.ContinuousParameterRange
import Amazonka.Forecast.Types.DataDestination
import Amazonka.Forecast.Types.DataSource
import Amazonka.Forecast.Types.DatasetGroupSummary
import Amazonka.Forecast.Types.DatasetImportJobSummary
import Amazonka.Forecast.Types.DatasetSummary
import Amazonka.Forecast.Types.EncryptionConfig
import Amazonka.Forecast.Types.ErrorMetric
import Amazonka.Forecast.Types.EvaluationParameters
import Amazonka.Forecast.Types.EvaluationResult
import Amazonka.Forecast.Types.Featurization
import Amazonka.Forecast.Types.FeaturizationConfig
import Amazonka.Forecast.Types.FeaturizationMethod
import Amazonka.Forecast.Types.Filter
import Amazonka.Forecast.Types.ForecastExportJobSummary
import Amazonka.Forecast.Types.ForecastSummary
import Amazonka.Forecast.Types.HyperParameterTuningJobConfig
import Amazonka.Forecast.Types.InputDataConfig
import Amazonka.Forecast.Types.IntegerParameterRange
import Amazonka.Forecast.Types.Metrics
import Amazonka.Forecast.Types.ParameterRanges
import Amazonka.Forecast.Types.PredictorBacktestExportJobSummary
import Amazonka.Forecast.Types.PredictorExecution
import Amazonka.Forecast.Types.PredictorExecutionDetails
import Amazonka.Forecast.Types.PredictorSummary
import Amazonka.Forecast.Types.S3Config
import Amazonka.Forecast.Types.Schema
import Amazonka.Forecast.Types.SchemaAttribute
import Amazonka.Forecast.Types.Statistics
import Amazonka.Forecast.Types.SupplementaryFeature
import Amazonka.Forecast.Types.Tag
import Amazonka.Forecast.Types.TestWindowSummary
import Amazonka.Forecast.Types.WeightedQuantileLoss
import Amazonka.Forecast.Types.WindowSummary
import Amazonka.Forecast.UntagResource
import Amazonka.Forecast.UpdateDatasetGroup
