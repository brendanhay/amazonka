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
    _InvalidInputException,
    _InvalidNextTokenException,
    _LimitExceededException,
    _ResourceAlreadyExistsException,
    _ResourceInUseException,
    _ResourceNotFoundException,

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
    datasetGroupSummary_creationTime,
    datasetGroupSummary_datasetGroupArn,
    datasetGroupSummary_datasetGroupName,
    datasetGroupSummary_lastModificationTime,

    -- * DatasetImportJobSummary
    DatasetImportJobSummary (..),
    newDatasetImportJobSummary,
    datasetImportJobSummary_creationTime,
    datasetImportJobSummary_dataSource,
    datasetImportJobSummary_datasetImportJobArn,
    datasetImportJobSummary_datasetImportJobName,
    datasetImportJobSummary_lastModificationTime,
    datasetImportJobSummary_message,
    datasetImportJobSummary_status,

    -- * DatasetSummary
    DatasetSummary (..),
    newDatasetSummary,
    datasetSummary_creationTime,
    datasetSummary_datasetArn,
    datasetSummary_datasetName,
    datasetSummary_datasetType,
    datasetSummary_domain,
    datasetSummary_lastModificationTime,

    -- * EncryptionConfig
    EncryptionConfig (..),
    newEncryptionConfig,
    encryptionConfig_roleArn,
    encryptionConfig_kmsKeyArn,

    -- * ErrorMetric
    ErrorMetric (..),
    newErrorMetric,
    errorMetric_forecastType,
    errorMetric_mape,
    errorMetric_mase,
    errorMetric_rmse,
    errorMetric_wape,

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

    -- * ExplainabilityConfig
    ExplainabilityConfig (..),
    newExplainabilityConfig,
    explainabilityConfig_timeSeriesGranularity,
    explainabilityConfig_timePointGranularity,

    -- * ExplainabilityExportSummary
    ExplainabilityExportSummary (..),
    newExplainabilityExportSummary,
    explainabilityExportSummary_creationTime,
    explainabilityExportSummary_destination,
    explainabilityExportSummary_explainabilityExportArn,
    explainabilityExportSummary_explainabilityExportName,
    explainabilityExportSummary_lastModificationTime,
    explainabilityExportSummary_message,
    explainabilityExportSummary_status,

    -- * ExplainabilityInfo
    ExplainabilityInfo (..),
    newExplainabilityInfo,
    explainabilityInfo_explainabilityArn,
    explainabilityInfo_status,

    -- * ExplainabilitySummary
    ExplainabilitySummary (..),
    newExplainabilitySummary,
    explainabilitySummary_creationTime,
    explainabilitySummary_explainabilityArn,
    explainabilitySummary_explainabilityConfig,
    explainabilitySummary_explainabilityName,
    explainabilitySummary_lastModificationTime,
    explainabilitySummary_message,
    explainabilitySummary_resourceArn,
    explainabilitySummary_status,

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
    forecastExportJobSummary_destination,
    forecastExportJobSummary_forecastExportJobArn,
    forecastExportJobSummary_forecastExportJobName,
    forecastExportJobSummary_lastModificationTime,
    forecastExportJobSummary_message,
    forecastExportJobSummary_status,

    -- * ForecastSummary
    ForecastSummary (..),
    newForecastSummary,
    forecastSummary_createdUsingAutoPredictor,
    forecastSummary_creationTime,
    forecastSummary_datasetGroupArn,
    forecastSummary_forecastArn,
    forecastSummary_forecastName,
    forecastSummary_lastModificationTime,
    forecastSummary_message,
    forecastSummary_predictorArn,
    forecastSummary_status,

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
    metricResult_metricName,
    metricResult_metricValue,

    -- * Metrics
    Metrics (..),
    newMetrics,
    metrics_averageWeightedQuantileLoss,
    metrics_errorMetrics,
    metrics_rmse,
    metrics_weightedQuantileLosses,

    -- * MonitorConfig
    MonitorConfig (..),
    newMonitorConfig,
    monitorConfig_monitorName,

    -- * MonitorDataSource
    MonitorDataSource (..),
    newMonitorDataSource,
    monitorDataSource_datasetImportJobArn,
    monitorDataSource_forecastArn,
    monitorDataSource_predictorArn,

    -- * MonitorInfo
    MonitorInfo (..),
    newMonitorInfo,
    monitorInfo_monitorArn,
    monitorInfo_status,

    -- * MonitorSummary
    MonitorSummary (..),
    newMonitorSummary,
    monitorSummary_creationTime,
    monitorSummary_lastModificationTime,
    monitorSummary_monitorArn,
    monitorSummary_monitorName,
    monitorSummary_resourceArn,
    monitorSummary_status,

    -- * ParameterRanges
    ParameterRanges (..),
    newParameterRanges,
    parameterRanges_categoricalParameterRanges,
    parameterRanges_continuousParameterRanges,
    parameterRanges_integerParameterRanges,

    -- * PredictorBacktestExportJobSummary
    PredictorBacktestExportJobSummary (..),
    newPredictorBacktestExportJobSummary,
    predictorBacktestExportJobSummary_creationTime,
    predictorBacktestExportJobSummary_destination,
    predictorBacktestExportJobSummary_lastModificationTime,
    predictorBacktestExportJobSummary_message,
    predictorBacktestExportJobSummary_predictorBacktestExportJobArn,
    predictorBacktestExportJobSummary_predictorBacktestExportJobName,
    predictorBacktestExportJobSummary_status,

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
    predictorExecution_algorithmArn,
    predictorExecution_testWindows,

    -- * PredictorExecutionDetails
    PredictorExecutionDetails (..),
    newPredictorExecutionDetails,
    predictorExecutionDetails_predictorExecutions,

    -- * PredictorMonitorEvaluation
    PredictorMonitorEvaluation (..),
    newPredictorMonitorEvaluation,
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

    -- * PredictorSummary
    PredictorSummary (..),
    newPredictorSummary,
    predictorSummary_creationTime,
    predictorSummary_datasetGroupArn,
    predictorSummary_isAutoPredictor,
    predictorSummary_lastModificationTime,
    predictorSummary_message,
    predictorSummary_predictorArn,
    predictorSummary_predictorName,
    predictorSummary_referencePredictorSummary,
    predictorSummary_status,

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
    schemaAttribute_attributeName,
    schemaAttribute_attributeType,

    -- * Statistics
    Statistics (..),
    newStatistics,
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
    timeAlignmentBoundary_dayOfMonth,
    timeAlignmentBoundary_dayOfWeek,
    timeAlignmentBoundary_hour,
    timeAlignmentBoundary_month,

    -- * TimeSeriesCondition
    TimeSeriesCondition (..),
    newTimeSeriesCondition,
    timeSeriesCondition_attributeName,
    timeSeriesCondition_attributeValue,
    timeSeriesCondition_condition,

    -- * TimeSeriesIdentifiers
    TimeSeriesIdentifiers (..),
    newTimeSeriesIdentifiers,
    timeSeriesIdentifiers_dataSource,
    timeSeriesIdentifiers_format,
    timeSeriesIdentifiers_schema,

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
    timeSeriesTransformation_action,
    timeSeriesTransformation_timeSeriesConditions,

    -- * WeightedQuantileLoss
    WeightedQuantileLoss (..),
    newWeightedQuantileLoss,
    weightedQuantileLoss_lossValue,
    weightedQuantileLoss_quantile,

    -- * WhatIfAnalysisSummary
    WhatIfAnalysisSummary (..),
    newWhatIfAnalysisSummary,
    whatIfAnalysisSummary_creationTime,
    whatIfAnalysisSummary_forecastArn,
    whatIfAnalysisSummary_lastModificationTime,
    whatIfAnalysisSummary_message,
    whatIfAnalysisSummary_status,
    whatIfAnalysisSummary_whatIfAnalysisArn,
    whatIfAnalysisSummary_whatIfAnalysisName,

    -- * WhatIfForecastExportSummary
    WhatIfForecastExportSummary (..),
    newWhatIfForecastExportSummary,
    whatIfForecastExportSummary_creationTime,
    whatIfForecastExportSummary_destination,
    whatIfForecastExportSummary_lastModificationTime,
    whatIfForecastExportSummary_message,
    whatIfForecastExportSummary_status,
    whatIfForecastExportSummary_whatIfForecastArns,
    whatIfForecastExportSummary_whatIfForecastExportArn,
    whatIfForecastExportSummary_whatIfForecastExportName,

    -- * WhatIfForecastSummary
    WhatIfForecastSummary (..),
    newWhatIfForecastSummary,
    whatIfForecastSummary_creationTime,
    whatIfForecastSummary_lastModificationTime,
    whatIfForecastSummary_message,
    whatIfForecastSummary_status,
    whatIfForecastSummary_whatIfAnalysisArn,
    whatIfForecastSummary_whatIfForecastArn,
    whatIfForecastSummary_whatIfForecastName,

    -- * WindowSummary
    WindowSummary (..),
    newWindowSummary,
    windowSummary_evaluationType,
    windowSummary_itemCount,
    windowSummary_metrics,
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
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | We can\'t process the request because it includes an invalid value or a
-- value that exceeds the valid range.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | The token is not valid. Tokens expire after 24 hours.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The limit on the number of resources per account has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | There is already a resource with this name. Try again with a different
-- name.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The specified resource is in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | We can\'t find a resource with that Amazon Resource Name (ARN). Check
-- the ARN and try again.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
