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
