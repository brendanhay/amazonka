{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Forecast.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceAlreadyExistsException,
    _InvalidInputException,
    _ResourceNotFoundException,
    _ResourceInUseException,
    _LimitExceededException,
    _InvalidNextTokenException,

    -- * AttributeType
    AttributeType (..),

    -- * AutoMLOverrideStrategy
    AutoMLOverrideStrategy (..),

    -- * Condition
    Condition (..),

    -- * DatasetType
    DatasetType (..),

    -- * DayOfWeek
    DayOfWeek (..),

    -- * Domain
    Domain (..),

    -- * EvaluationType
    EvaluationType (..),

    -- * FeaturizationMethodName
    FeaturizationMethodName (..),

    -- * FilterConditionString
    FilterConditionString (..),

    -- * Month
    Month (..),

    -- * Operation
    Operation (..),

    -- * OptimizationMetric
    OptimizationMetric (..),

    -- * ScalingType
    ScalingType (..),

    -- * State
    State (..),

    -- * TimePointGranularity
    TimePointGranularity (..),

    -- * TimeSeriesGranularity
    TimeSeriesGranularity (..),

    -- * Action
    Action (..),
    newAction,
    action_attributeName,
    action_operation,
    action_value,

    -- * AdditionalDataset
    AdditionalDataset (..),
    newAdditionalDataset,
    additionalDataset_configuration,
    additionalDataset_name,

    -- * AttributeConfig
    AttributeConfig (..),
    newAttributeConfig,
    attributeConfig_attributeName,
    attributeConfig_transformations,

    -- * Baseline
    Baseline (..),
    newBaseline,
    baseline_predictorBaseline,

    -- * BaselineMetric
    BaselineMetric (..),
    newBaselineMetric,
    baselineMetric_name,
    baselineMetric_value,

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

    -- * DataConfig
    DataConfig (..),
    newDataConfig,
    dataConfig_additionalDatasets,
    dataConfig_attributeConfigs,
    dataConfig_datasetGroupArn,

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
    datasetGroupSummary_lastModificationTime,
    datasetGroupSummary_datasetGroupName,
    datasetGroupSummary_creationTime,
    datasetGroupSummary_datasetGroupArn,

    -- * DatasetImportJobSummary
    DatasetImportJobSummary (..),
    newDatasetImportJobSummary,
    datasetImportJobSummary_lastModificationTime,
    datasetImportJobSummary_message,
    datasetImportJobSummary_status,
    datasetImportJobSummary_datasetImportJobArn,
    datasetImportJobSummary_dataSource,
    datasetImportJobSummary_creationTime,
    datasetImportJobSummary_datasetImportJobName,

    -- * DatasetSummary
    DatasetSummary (..),
    newDatasetSummary,
    datasetSummary_lastModificationTime,
    datasetSummary_domain,
    datasetSummary_datasetType,
    datasetSummary_datasetName,
    datasetSummary_datasetArn,
    datasetSummary_creationTime,

    -- * EncryptionConfig
    EncryptionConfig (..),
    newEncryptionConfig,
    encryptionConfig_roleArn,
    encryptionConfig_kmsKeyArn,

    -- * ErrorMetric
    ErrorMetric (..),
    newErrorMetric,
    errorMetric_wape,
    errorMetric_mase,
    errorMetric_forecastType,
    errorMetric_rmse,
    errorMetric_mape,

    -- * EvaluationParameters
    EvaluationParameters (..),
    newEvaluationParameters,
    evaluationParameters_numberOfBacktestWindows,
    evaluationParameters_backTestWindowOffset,

    -- * EvaluationResult
    EvaluationResult (..),
    newEvaluationResult,
    evaluationResult_testWindows,
    evaluationResult_algorithmArn,

    -- * ExplainabilityConfig
    ExplainabilityConfig (..),
    newExplainabilityConfig,
    explainabilityConfig_timeSeriesGranularity,
    explainabilityConfig_timePointGranularity,

    -- * ExplainabilityExportSummary
    ExplainabilityExportSummary (..),
    newExplainabilityExportSummary,
    explainabilityExportSummary_lastModificationTime,
    explainabilityExportSummary_destination,
    explainabilityExportSummary_explainabilityExportName,
    explainabilityExportSummary_message,
    explainabilityExportSummary_explainabilityExportArn,
    explainabilityExportSummary_status,
    explainabilityExportSummary_creationTime,

    -- * ExplainabilityInfo
    ExplainabilityInfo (..),
    newExplainabilityInfo,
    explainabilityInfo_status,
    explainabilityInfo_explainabilityArn,

    -- * ExplainabilitySummary
    ExplainabilitySummary (..),
    newExplainabilitySummary,
    explainabilitySummary_lastModificationTime,
    explainabilitySummary_message,
    explainabilitySummary_explainabilityConfig,
    explainabilitySummary_status,
    explainabilitySummary_explainabilityArn,
    explainabilitySummary_explainabilityName,
    explainabilitySummary_creationTime,
    explainabilitySummary_resourceArn,

    -- * Featurization
    Featurization (..),
    newFeaturization,
    featurization_featurizationPipeline,
    featurization_attributeName,

    -- * FeaturizationConfig
    FeaturizationConfig (..),
    newFeaturizationConfig,
    featurizationConfig_forecastDimensions,
    featurizationConfig_featurizations,
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
    forecastExportJobSummary_lastModificationTime,
    forecastExportJobSummary_destination,
    forecastExportJobSummary_message,
    forecastExportJobSummary_forecastExportJobName,
    forecastExportJobSummary_forecastExportJobArn,
    forecastExportJobSummary_status,
    forecastExportJobSummary_creationTime,

    -- * ForecastSummary
    ForecastSummary (..),
    newForecastSummary,
    forecastSummary_lastModificationTime,
    forecastSummary_message,
    forecastSummary_status,
    forecastSummary_predictorArn,
    forecastSummary_forecastArn,
    forecastSummary_creationTime,
    forecastSummary_datasetGroupArn,
    forecastSummary_createdUsingAutoPredictor,
    forecastSummary_forecastName,

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

    -- * MetricResult
    MetricResult (..),
    newMetricResult,
    metricResult_metricValue,
    metricResult_metricName,

    -- * Metrics
    Metrics (..),
    newMetrics,
    metrics_averageWeightedQuantileLoss,
    metrics_weightedQuantileLosses,
    metrics_errorMetrics,
    metrics_rmse,

    -- * MonitorConfig
    MonitorConfig (..),
    newMonitorConfig,
    monitorConfig_monitorName,

    -- * MonitorDataSource
    MonitorDataSource (..),
    newMonitorDataSource,
    monitorDataSource_datasetImportJobArn,
    monitorDataSource_predictorArn,
    monitorDataSource_forecastArn,

    -- * MonitorInfo
    MonitorInfo (..),
    newMonitorInfo,
    monitorInfo_monitorArn,
    monitorInfo_status,

    -- * MonitorSummary
    MonitorSummary (..),
    newMonitorSummary,
    monitorSummary_lastModificationTime,
    monitorSummary_monitorArn,
    monitorSummary_status,
    monitorSummary_monitorName,
    monitorSummary_creationTime,
    monitorSummary_resourceArn,

    -- * ParameterRanges
    ParameterRanges (..),
    newParameterRanges,
    parameterRanges_categoricalParameterRanges,
    parameterRanges_integerParameterRanges,
    parameterRanges_continuousParameterRanges,

    -- * PredictorBacktestExportJobSummary
    PredictorBacktestExportJobSummary (..),
    newPredictorBacktestExportJobSummary,
    predictorBacktestExportJobSummary_lastModificationTime,
    predictorBacktestExportJobSummary_destination,
    predictorBacktestExportJobSummary_message,
    predictorBacktestExportJobSummary_status,
    predictorBacktestExportJobSummary_predictorBacktestExportJobName,
    predictorBacktestExportJobSummary_predictorBacktestExportJobArn,
    predictorBacktestExportJobSummary_creationTime,

    -- * PredictorBaseline
    PredictorBaseline (..),
    newPredictorBaseline,
    predictorBaseline_baselineMetrics,

    -- * PredictorEvent
    PredictorEvent (..),
    newPredictorEvent,
    predictorEvent_datetime,
    predictorEvent_detail,

    -- * PredictorExecution
    PredictorExecution (..),
    newPredictorExecution,
    predictorExecution_testWindows,
    predictorExecution_algorithmArn,

    -- * PredictorExecutionDetails
    PredictorExecutionDetails (..),
    newPredictorExecutionDetails,
    predictorExecutionDetails_predictorExecutions,

    -- * PredictorMonitorEvaluation
    PredictorMonitorEvaluation (..),
    newPredictorMonitorEvaluation,
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

    -- * PredictorSummary
    PredictorSummary (..),
    newPredictorSummary,
    predictorSummary_lastModificationTime,
    predictorSummary_message,
    predictorSummary_isAutoPredictor,
    predictorSummary_predictorName,
    predictorSummary_status,
    predictorSummary_predictorArn,
    predictorSummary_creationTime,
    predictorSummary_datasetGroupArn,
    predictorSummary_referencePredictorSummary,

    -- * ReferencePredictorSummary
    ReferencePredictorSummary (..),
    newReferencePredictorSummary,
    referencePredictorSummary_arn,
    referencePredictorSummary_state,

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
    testWindowSummary_message,
    testWindowSummary_status,
    testWindowSummary_testWindowEnd,
    testWindowSummary_testWindowStart,

    -- * TimeAlignmentBoundary
    TimeAlignmentBoundary (..),
    newTimeAlignmentBoundary,
    timeAlignmentBoundary_dayOfWeek,
    timeAlignmentBoundary_month,
    timeAlignmentBoundary_hour,
    timeAlignmentBoundary_dayOfMonth,

    -- * TimeSeriesCondition
    TimeSeriesCondition (..),
    newTimeSeriesCondition,
    timeSeriesCondition_attributeName,
    timeSeriesCondition_attributeValue,
    timeSeriesCondition_condition,

    -- * TimeSeriesIdentifiers
    TimeSeriesIdentifiers (..),
    newTimeSeriesIdentifiers,
    timeSeriesIdentifiers_format,
    timeSeriesIdentifiers_schema,
    timeSeriesIdentifiers_dataSource,

    -- * TimeSeriesReplacementsDataSource
    TimeSeriesReplacementsDataSource (..),
    newTimeSeriesReplacementsDataSource,
    timeSeriesReplacementsDataSource_format,
    timeSeriesReplacementsDataSource_timestampFormat,
    timeSeriesReplacementsDataSource_s3Config,
    timeSeriesReplacementsDataSource_schema,

    -- * TimeSeriesSelector
    TimeSeriesSelector (..),
    newTimeSeriesSelector,
    timeSeriesSelector_timeSeriesIdentifiers,

    -- * TimeSeriesTransformation
    TimeSeriesTransformation (..),
    newTimeSeriesTransformation,
    timeSeriesTransformation_timeSeriesConditions,
    timeSeriesTransformation_action,

    -- * WeightedQuantileLoss
    WeightedQuantileLoss (..),
    newWeightedQuantileLoss,
    weightedQuantileLoss_quantile,
    weightedQuantileLoss_lossValue,

    -- * WhatIfAnalysisSummary
    WhatIfAnalysisSummary (..),
    newWhatIfAnalysisSummary,
    whatIfAnalysisSummary_lastModificationTime,
    whatIfAnalysisSummary_whatIfAnalysisArn,
    whatIfAnalysisSummary_message,
    whatIfAnalysisSummary_whatIfAnalysisName,
    whatIfAnalysisSummary_status,
    whatIfAnalysisSummary_forecastArn,
    whatIfAnalysisSummary_creationTime,

    -- * WhatIfForecastExportSummary
    WhatIfForecastExportSummary (..),
    newWhatIfForecastExportSummary,
    whatIfForecastExportSummary_lastModificationTime,
    whatIfForecastExportSummary_destination,
    whatIfForecastExportSummary_message,
    whatIfForecastExportSummary_whatIfForecastExportName,
    whatIfForecastExportSummary_whatIfForecastArns,
    whatIfForecastExportSummary_whatIfForecastExportArn,
    whatIfForecastExportSummary_status,
    whatIfForecastExportSummary_creationTime,

    -- * WhatIfForecastSummary
    WhatIfForecastSummary (..),
    newWhatIfForecastSummary,
    whatIfForecastSummary_lastModificationTime,
    whatIfForecastSummary_whatIfAnalysisArn,
    whatIfForecastSummary_message,
    whatIfForecastSummary_whatIfForecastName,
    whatIfForecastSummary_whatIfForecastArn,
    whatIfForecastSummary_status,
    whatIfForecastSummary_creationTime,

    -- * WindowSummary
    WindowSummary (..),
    newWindowSummary,
    windowSummary_itemCount,
    windowSummary_metrics,
    windowSummary_evaluationType,
    windowSummary_testWindowEnd,
    windowSummary_testWindowStart,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types.Action
import Amazonka.Forecast.Types.AdditionalDataset
import Amazonka.Forecast.Types.AttributeConfig
import Amazonka.Forecast.Types.AttributeType
import Amazonka.Forecast.Types.AutoMLOverrideStrategy
import Amazonka.Forecast.Types.Baseline
import Amazonka.Forecast.Types.BaselineMetric
import Amazonka.Forecast.Types.CategoricalParameterRange
import Amazonka.Forecast.Types.Condition
import Amazonka.Forecast.Types.ContinuousParameterRange
import Amazonka.Forecast.Types.DataConfig
import Amazonka.Forecast.Types.DataDestination
import Amazonka.Forecast.Types.DataSource
import Amazonka.Forecast.Types.DatasetGroupSummary
import Amazonka.Forecast.Types.DatasetImportJobSummary
import Amazonka.Forecast.Types.DatasetSummary
import Amazonka.Forecast.Types.DatasetType
import Amazonka.Forecast.Types.DayOfWeek
import Amazonka.Forecast.Types.Domain
import Amazonka.Forecast.Types.EncryptionConfig
import Amazonka.Forecast.Types.ErrorMetric
import Amazonka.Forecast.Types.EvaluationParameters
import Amazonka.Forecast.Types.EvaluationResult
import Amazonka.Forecast.Types.EvaluationType
import Amazonka.Forecast.Types.ExplainabilityConfig
import Amazonka.Forecast.Types.ExplainabilityExportSummary
import Amazonka.Forecast.Types.ExplainabilityInfo
import Amazonka.Forecast.Types.ExplainabilitySummary
import Amazonka.Forecast.Types.Featurization
import Amazonka.Forecast.Types.FeaturizationConfig
import Amazonka.Forecast.Types.FeaturizationMethod
import Amazonka.Forecast.Types.FeaturizationMethodName
import Amazonka.Forecast.Types.Filter
import Amazonka.Forecast.Types.FilterConditionString
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
import Amazonka.Forecast.Types.Month
import Amazonka.Forecast.Types.Operation
import Amazonka.Forecast.Types.OptimizationMetric
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
import Amazonka.Forecast.Types.ScalingType
import Amazonka.Forecast.Types.Schema
import Amazonka.Forecast.Types.SchemaAttribute
import Amazonka.Forecast.Types.State
import Amazonka.Forecast.Types.Statistics
import Amazonka.Forecast.Types.SupplementaryFeature
import Amazonka.Forecast.Types.Tag
import Amazonka.Forecast.Types.TestWindowSummary
import Amazonka.Forecast.Types.TimeAlignmentBoundary
import Amazonka.Forecast.Types.TimePointGranularity
import Amazonka.Forecast.Types.TimeSeriesCondition
import Amazonka.Forecast.Types.TimeSeriesGranularity
import Amazonka.Forecast.Types.TimeSeriesIdentifiers
import Amazonka.Forecast.Types.TimeSeriesReplacementsDataSource
import Amazonka.Forecast.Types.TimeSeriesSelector
import Amazonka.Forecast.Types.TimeSeriesTransformation
import Amazonka.Forecast.Types.WeightedQuantileLoss
import Amazonka.Forecast.Types.WhatIfAnalysisSummary
import Amazonka.Forecast.Types.WhatIfForecastExportSummary
import Amazonka.Forecast.Types.WhatIfForecastSummary
import Amazonka.Forecast.Types.WindowSummary
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-06-26@ of the Amazon Forecast Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Forecast",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "forecast",
      Core.signingName = "forecast",
      Core.version = "2018-06-26",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Forecast",
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

-- | There is already a resource with this name. Try again with a different
-- name.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

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

-- | The specified resource is in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The limit on the number of resources per account has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The token is not valid. Tokens expire after 24 hours.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"
