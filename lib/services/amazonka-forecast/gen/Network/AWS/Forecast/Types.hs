{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Forecast.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Forecast.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceAlreadyExistsException,
    _InvalidNextTokenException,
    _InvalidInputException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _ResourceInUseException,

    -- * AttributeType
    AttributeType (..),

    -- * AutoMLOverrideStrategy
    AutoMLOverrideStrategy (..),

    -- * DatasetType
    DatasetType (..),

    -- * Domain
    Domain (..),

    -- * EvaluationType
    EvaluationType (..),

    -- * FeaturizationMethodName
    FeaturizationMethodName (..),

    -- * FilterConditionString
    FilterConditionString (..),

    -- * OptimizationMetric
    OptimizationMetric (..),

    -- * ScalingType
    ScalingType (..),

    -- * CategoricalParameterRange
    CategoricalParameterRange (..),
    newCategoricalParameterRange,
    categoricalParameterRange_name,
    categoricalParameterRange_values,

    -- * ContinuousParameterRange
    ContinuousParameterRange (..),
    newContinuousParameterRange,
    continuousParameterRange_scalingType,
    continuousParameterRange_name,
    continuousParameterRange_maxValue,
    continuousParameterRange_minValue,

    -- * DataDestination
    DataDestination (..),
    newDataDestination,
    dataDestination_s3Config,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_s3Config,

    -- * DatasetGroupSummary
    DatasetGroupSummary (..),
    newDatasetGroupSummary,
    datasetGroupSummary_creationTime,
    datasetGroupSummary_datasetGroupName,
    datasetGroupSummary_datasetGroupArn,
    datasetGroupSummary_lastModificationTime,

    -- * DatasetImportJobSummary
    DatasetImportJobSummary (..),
    newDatasetImportJobSummary,
    datasetImportJobSummary_creationTime,
    datasetImportJobSummary_status,
    datasetImportJobSummary_datasetImportJobName,
    datasetImportJobSummary_dataSource,
    datasetImportJobSummary_datasetImportJobArn,
    datasetImportJobSummary_message,
    datasetImportJobSummary_lastModificationTime,

    -- * DatasetSummary
    DatasetSummary (..),
    newDatasetSummary,
    datasetSummary_creationTime,
    datasetSummary_datasetArn,
    datasetSummary_domain,
    datasetSummary_datasetType,
    datasetSummary_datasetName,
    datasetSummary_lastModificationTime,

    -- * EncryptionConfig
    EncryptionConfig (..),
    newEncryptionConfig,
    encryptionConfig_roleArn,
    encryptionConfig_kmsKeyArn,

    -- * ErrorMetric
    ErrorMetric (..),
    newErrorMetric,
    errorMetric_mase,
    errorMetric_wape,
    errorMetric_mape,
    errorMetric_rmse,
    errorMetric_forecastType,

    -- * EvaluationParameters
    EvaluationParameters (..),
    newEvaluationParameters,
    evaluationParameters_backTestWindowOffset,
    evaluationParameters_numberOfBacktestWindows,

    -- * EvaluationResult
    EvaluationResult (..),
    newEvaluationResult,
    evaluationResult_algorithmArn,
    evaluationResult_testWindows,

    -- * Featurization
    Featurization (..),
    newFeaturization,
    featurization_featurizationPipeline,
    featurization_attributeName,

    -- * FeaturizationConfig
    FeaturizationConfig (..),
    newFeaturizationConfig,
    featurizationConfig_featurizations,
    featurizationConfig_forecastDimensions,
    featurizationConfig_forecastFrequency,

    -- * FeaturizationMethod
    FeaturizationMethod (..),
    newFeaturizationMethod,
    featurizationMethod_featurizationMethodParameters,
    featurizationMethod_featurizationMethodName,

    -- * Filter
    Filter (..),
    newFilter,
    filter_key,
    filter_value,
    filter_condition,

    -- * ForecastExportJobSummary
    ForecastExportJobSummary (..),
    newForecastExportJobSummary,
    forecastExportJobSummary_creationTime,
    forecastExportJobSummary_status,
    forecastExportJobSummary_destination,
    forecastExportJobSummary_forecastExportJobArn,
    forecastExportJobSummary_forecastExportJobName,
    forecastExportJobSummary_message,
    forecastExportJobSummary_lastModificationTime,

    -- * ForecastSummary
    ForecastSummary (..),
    newForecastSummary,
    forecastSummary_creationTime,
    forecastSummary_status,
    forecastSummary_predictorArn,
    forecastSummary_forecastArn,
    forecastSummary_forecastName,
    forecastSummary_datasetGroupArn,
    forecastSummary_message,
    forecastSummary_lastModificationTime,

    -- * HyperParameterTuningJobConfig
    HyperParameterTuningJobConfig (..),
    newHyperParameterTuningJobConfig,
    hyperParameterTuningJobConfig_parameterRanges,

    -- * InputDataConfig
    InputDataConfig (..),
    newInputDataConfig,
    inputDataConfig_supplementaryFeatures,
    inputDataConfig_datasetGroupArn,

    -- * IntegerParameterRange
    IntegerParameterRange (..),
    newIntegerParameterRange,
    integerParameterRange_scalingType,
    integerParameterRange_name,
    integerParameterRange_maxValue,
    integerParameterRange_minValue,

    -- * Metrics
    Metrics (..),
    newMetrics,
    metrics_errorMetrics,
    metrics_rmse,
    metrics_weightedQuantileLosses,
    metrics_averageWeightedQuantileLoss,

    -- * ParameterRanges
    ParameterRanges (..),
    newParameterRanges,
    parameterRanges_categoricalParameterRanges,
    parameterRanges_integerParameterRanges,
    parameterRanges_continuousParameterRanges,

    -- * PredictorBacktestExportJobSummary
    PredictorBacktestExportJobSummary (..),
    newPredictorBacktestExportJobSummary,
    predictorBacktestExportJobSummary_creationTime,
    predictorBacktestExportJobSummary_status,
    predictorBacktestExportJobSummary_destination,
    predictorBacktestExportJobSummary_predictorBacktestExportJobArn,
    predictorBacktestExportJobSummary_message,
    predictorBacktestExportJobSummary_predictorBacktestExportJobName,
    predictorBacktestExportJobSummary_lastModificationTime,

    -- * PredictorExecution
    PredictorExecution (..),
    newPredictorExecution,
    predictorExecution_algorithmArn,
    predictorExecution_testWindows,

    -- * PredictorExecutionDetails
    PredictorExecutionDetails (..),
    newPredictorExecutionDetails,
    predictorExecutionDetails_predictorExecutions,

    -- * PredictorSummary
    PredictorSummary (..),
    newPredictorSummary,
    predictorSummary_creationTime,
    predictorSummary_status,
    predictorSummary_predictorArn,
    predictorSummary_predictorName,
    predictorSummary_datasetGroupArn,
    predictorSummary_message,
    predictorSummary_lastModificationTime,

    -- * S3Config
    S3Config (..),
    newS3Config,
    s3Config_kmsKeyArn,
    s3Config_path,
    s3Config_roleArn,

    -- * Schema
    Schema (..),
    newSchema,
    schema_attributes,

    -- * SchemaAttribute
    SchemaAttribute (..),
    newSchemaAttribute,
    schemaAttribute_attributeType,
    schemaAttribute_attributeName,

    -- * Statistics
    Statistics (..),
    newStatistics,
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

    -- * SupplementaryFeature
    SupplementaryFeature (..),
    newSupplementaryFeature,
    supplementaryFeature_name,
    supplementaryFeature_value,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TestWindowSummary
    TestWindowSummary (..),
    newTestWindowSummary,
    testWindowSummary_status,
    testWindowSummary_testWindowEnd,
    testWindowSummary_testWindowStart,
    testWindowSummary_message,

    -- * WeightedQuantileLoss
    WeightedQuantileLoss (..),
    newWeightedQuantileLoss,
    weightedQuantileLoss_quantile,
    weightedQuantileLoss_lossValue,

    -- * WindowSummary
    WindowSummary (..),
    newWindowSummary,
    windowSummary_metrics,
    windowSummary_testWindowEnd,
    windowSummary_evaluationType,
    windowSummary_testWindowStart,
    windowSummary_itemCount,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Forecast.Types.AttributeType
import Network.AWS.Forecast.Types.AutoMLOverrideStrategy
import Network.AWS.Forecast.Types.CategoricalParameterRange
import Network.AWS.Forecast.Types.ContinuousParameterRange
import Network.AWS.Forecast.Types.DataDestination
import Network.AWS.Forecast.Types.DataSource
import Network.AWS.Forecast.Types.DatasetGroupSummary
import Network.AWS.Forecast.Types.DatasetImportJobSummary
import Network.AWS.Forecast.Types.DatasetSummary
import Network.AWS.Forecast.Types.DatasetType
import Network.AWS.Forecast.Types.Domain
import Network.AWS.Forecast.Types.EncryptionConfig
import Network.AWS.Forecast.Types.ErrorMetric
import Network.AWS.Forecast.Types.EvaluationParameters
import Network.AWS.Forecast.Types.EvaluationResult
import Network.AWS.Forecast.Types.EvaluationType
import Network.AWS.Forecast.Types.Featurization
import Network.AWS.Forecast.Types.FeaturizationConfig
import Network.AWS.Forecast.Types.FeaturizationMethod
import Network.AWS.Forecast.Types.FeaturizationMethodName
import Network.AWS.Forecast.Types.Filter
import Network.AWS.Forecast.Types.FilterConditionString
import Network.AWS.Forecast.Types.ForecastExportJobSummary
import Network.AWS.Forecast.Types.ForecastSummary
import Network.AWS.Forecast.Types.HyperParameterTuningJobConfig
import Network.AWS.Forecast.Types.InputDataConfig
import Network.AWS.Forecast.Types.IntegerParameterRange
import Network.AWS.Forecast.Types.Metrics
import Network.AWS.Forecast.Types.OptimizationMetric
import Network.AWS.Forecast.Types.ParameterRanges
import Network.AWS.Forecast.Types.PredictorBacktestExportJobSummary
import Network.AWS.Forecast.Types.PredictorExecution
import Network.AWS.Forecast.Types.PredictorExecutionDetails
import Network.AWS.Forecast.Types.PredictorSummary
import Network.AWS.Forecast.Types.S3Config
import Network.AWS.Forecast.Types.ScalingType
import Network.AWS.Forecast.Types.Schema
import Network.AWS.Forecast.Types.SchemaAttribute
import Network.AWS.Forecast.Types.Statistics
import Network.AWS.Forecast.Types.SupplementaryFeature
import Network.AWS.Forecast.Types.Tag
import Network.AWS.Forecast.Types.TestWindowSummary
import Network.AWS.Forecast.Types.WeightedQuantileLoss
import Network.AWS.Forecast.Types.WindowSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-06-26@ of the Amazon Forecast Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Forecast",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "forecast",
      Core._serviceSigningName = "forecast",
      Core._serviceVersion = "2018-06-26",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Forecast",
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

-- | There is already a resource with this name. Try again with a different
-- name.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The token is not valid. Tokens expire after 24 hours.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | We can\'t process the request because it includes an invalid value or a
-- value that exceeds the valid range.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | We can\'t find a resource with that Amazon Resource Name (ARN). Check
-- the ARN and try again.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The limit on the number of resources per account has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The specified resource is in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
