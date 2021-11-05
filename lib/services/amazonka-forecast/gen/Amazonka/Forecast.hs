{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Forecast
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-06-26@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Provides APIs for creating and managing Amazon Forecast resources.
module Amazonka.Forecast
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListDatasetGroups (Paginated)
    ListDatasetGroups (ListDatasetGroups'),
    newListDatasetGroups,
    ListDatasetGroupsResponse (ListDatasetGroupsResponse'),
    newListDatasetGroupsResponse,

    -- ** CreateDatasetImportJob
    CreateDatasetImportJob (CreateDatasetImportJob'),
    newCreateDatasetImportJob,
    CreateDatasetImportJobResponse (CreateDatasetImportJobResponse'),
    newCreateDatasetImportJobResponse,

    -- ** DescribeDataset
    DescribeDataset (DescribeDataset'),
    newDescribeDataset,
    DescribeDatasetResponse (DescribeDatasetResponse'),
    newDescribeDatasetResponse,

    -- ** ListForecasts (Paginated)
    ListForecasts (ListForecasts'),
    newListForecasts,
    ListForecastsResponse (ListForecastsResponse'),
    newListForecastsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StopResource
    StopResource (StopResource'),
    newStopResource,
    StopResourceResponse (StopResourceResponse'),
    newStopResourceResponse,

    -- ** DescribeDatasetImportJob
    DescribeDatasetImportJob (DescribeDatasetImportJob'),
    newDescribeDatasetImportJob,
    DescribeDatasetImportJobResponse (DescribeDatasetImportJobResponse'),
    newDescribeDatasetImportJobResponse,

    -- ** DescribeForecastExportJob
    DescribeForecastExportJob (DescribeForecastExportJob'),
    newDescribeForecastExportJob,
    DescribeForecastExportJobResponse (DescribeForecastExportJobResponse'),
    newDescribeForecastExportJobResponse,

    -- ** DescribePredictor
    DescribePredictor (DescribePredictor'),
    newDescribePredictor,
    DescribePredictorResponse (DescribePredictorResponse'),
    newDescribePredictorResponse,

    -- ** DeleteDataset
    DeleteDataset (DeleteDataset'),
    newDeleteDataset,
    DeleteDatasetResponse (DeleteDatasetResponse'),
    newDeleteDatasetResponse,

    -- ** DescribeForecast
    DescribeForecast (DescribeForecast'),
    newDescribeForecast,
    DescribeForecastResponse (DescribeForecastResponse'),
    newDescribeForecastResponse,

    -- ** CreateDataset
    CreateDataset (CreateDataset'),
    newCreateDataset,
    CreateDatasetResponse (CreateDatasetResponse'),
    newCreateDatasetResponse,

    -- ** DeleteForecastExportJob
    DeleteForecastExportJob (DeleteForecastExportJob'),
    newDeleteForecastExportJob,
    DeleteForecastExportJobResponse (DeleteForecastExportJobResponse'),
    newDeleteForecastExportJobResponse,

    -- ** DeletePredictor
    DeletePredictor (DeletePredictor'),
    newDeletePredictor,
    DeletePredictorResponse (DeletePredictorResponse'),
    newDeletePredictorResponse,

    -- ** ListDatasetImportJobs (Paginated)
    ListDatasetImportJobs (ListDatasetImportJobs'),
    newListDatasetImportJobs,
    ListDatasetImportJobsResponse (ListDatasetImportJobsResponse'),
    newListDatasetImportJobsResponse,

    -- ** DeleteDatasetImportJob
    DeleteDatasetImportJob (DeleteDatasetImportJob'),
    newDeleteDatasetImportJob,
    DeleteDatasetImportJobResponse (DeleteDatasetImportJobResponse'),
    newDeleteDatasetImportJobResponse,

    -- ** GetAccuracyMetrics
    GetAccuracyMetrics (GetAccuracyMetrics'),
    newGetAccuracyMetrics,
    GetAccuracyMetricsResponse (GetAccuracyMetricsResponse'),
    newGetAccuracyMetricsResponse,

    -- ** DeleteDatasetGroup
    DeleteDatasetGroup (DeleteDatasetGroup'),
    newDeleteDatasetGroup,
    DeleteDatasetGroupResponse (DeleteDatasetGroupResponse'),
    newDeleteDatasetGroupResponse,

    -- ** UpdateDatasetGroup
    UpdateDatasetGroup (UpdateDatasetGroup'),
    newUpdateDatasetGroup,
    UpdateDatasetGroupResponse (UpdateDatasetGroupResponse'),
    newUpdateDatasetGroupResponse,

    -- ** CreateForecastExportJob
    CreateForecastExportJob (CreateForecastExportJob'),
    newCreateForecastExportJob,
    CreateForecastExportJobResponse (CreateForecastExportJobResponse'),
    newCreateForecastExportJobResponse,

    -- ** CreatePredictor
    CreatePredictor (CreatePredictor'),
    newCreatePredictor,
    CreatePredictorResponse (CreatePredictorResponse'),
    newCreatePredictorResponse,

    -- ** ListPredictorBacktestExportJobs (Paginated)
    ListPredictorBacktestExportJobs (ListPredictorBacktestExportJobs'),
    newListPredictorBacktestExportJobs,
    ListPredictorBacktestExportJobsResponse (ListPredictorBacktestExportJobsResponse'),
    newListPredictorBacktestExportJobsResponse,

    -- ** DeletePredictorBacktestExportJob
    DeletePredictorBacktestExportJob (DeletePredictorBacktestExportJob'),
    newDeletePredictorBacktestExportJob,
    DeletePredictorBacktestExportJobResponse (DeletePredictorBacktestExportJobResponse'),
    newDeletePredictorBacktestExportJobResponse,

    -- ** CreateForecast
    CreateForecast (CreateForecast'),
    newCreateForecast,
    CreateForecastResponse (CreateForecastResponse'),
    newCreateForecastResponse,

    -- ** CreatePredictorBacktestExportJob
    CreatePredictorBacktestExportJob (CreatePredictorBacktestExportJob'),
    newCreatePredictorBacktestExportJob,
    CreatePredictorBacktestExportJobResponse (CreatePredictorBacktestExportJobResponse'),
    newCreatePredictorBacktestExportJobResponse,

    -- ** DeleteForecast
    DeleteForecast (DeleteForecast'),
    newDeleteForecast,
    DeleteForecastResponse (DeleteForecastResponse'),
    newDeleteForecastResponse,

    -- ** DeleteResourceTree
    DeleteResourceTree (DeleteResourceTree'),
    newDeleteResourceTree,
    DeleteResourceTreeResponse (DeleteResourceTreeResponse'),
    newDeleteResourceTreeResponse,

    -- ** DescribeDatasetGroup
    DescribeDatasetGroup (DescribeDatasetGroup'),
    newDescribeDatasetGroup,
    DescribeDatasetGroupResponse (DescribeDatasetGroupResponse'),
    newDescribeDatasetGroupResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListDatasets (Paginated)
    ListDatasets (ListDatasets'),
    newListDatasets,
    ListDatasetsResponse (ListDatasetsResponse'),
    newListDatasetsResponse,

    -- ** DescribePredictorBacktestExportJob
    DescribePredictorBacktestExportJob (DescribePredictorBacktestExportJob'),
    newDescribePredictorBacktestExportJob,
    DescribePredictorBacktestExportJobResponse (DescribePredictorBacktestExportJobResponse'),
    newDescribePredictorBacktestExportJobResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CreateDatasetGroup
    CreateDatasetGroup (CreateDatasetGroup'),
    newCreateDatasetGroup,
    CreateDatasetGroupResponse (CreateDatasetGroupResponse'),
    newCreateDatasetGroupResponse,

    -- ** ListForecastExportJobs (Paginated)
    ListForecastExportJobs (ListForecastExportJobs'),
    newListForecastExportJobs,
    ListForecastExportJobsResponse (ListForecastExportJobsResponse'),
    newListForecastExportJobsResponse,

    -- ** ListPredictors (Paginated)
    ListPredictors (ListPredictors'),
    newListPredictors,
    ListPredictorsResponse (ListPredictorsResponse'),
    newListPredictorsResponse,

    -- * Types

    -- ** AttributeType
    AttributeType (..),

    -- ** AutoMLOverrideStrategy
    AutoMLOverrideStrategy (..),

    -- ** DatasetType
    DatasetType (..),

    -- ** Domain
    Domain (..),

    -- ** EvaluationType
    EvaluationType (..),

    -- ** FeaturizationMethodName
    FeaturizationMethodName (..),

    -- ** FilterConditionString
    FilterConditionString (..),

    -- ** OptimizationMetric
    OptimizationMetric (..),

    -- ** ScalingType
    ScalingType (..),

    -- ** CategoricalParameterRange
    CategoricalParameterRange (CategoricalParameterRange'),
    newCategoricalParameterRange,

    -- ** ContinuousParameterRange
    ContinuousParameterRange (ContinuousParameterRange'),
    newContinuousParameterRange,

    -- ** DataDestination
    DataDestination (DataDestination'),
    newDataDestination,

    -- ** DataSource
    DataSource (DataSource'),
    newDataSource,

    -- ** DatasetGroupSummary
    DatasetGroupSummary (DatasetGroupSummary'),
    newDatasetGroupSummary,

    -- ** DatasetImportJobSummary
    DatasetImportJobSummary (DatasetImportJobSummary'),
    newDatasetImportJobSummary,

    -- ** DatasetSummary
    DatasetSummary (DatasetSummary'),
    newDatasetSummary,

    -- ** EncryptionConfig
    EncryptionConfig (EncryptionConfig'),
    newEncryptionConfig,

    -- ** ErrorMetric
    ErrorMetric (ErrorMetric'),
    newErrorMetric,

    -- ** EvaluationParameters
    EvaluationParameters (EvaluationParameters'),
    newEvaluationParameters,

    -- ** EvaluationResult
    EvaluationResult (EvaluationResult'),
    newEvaluationResult,

    -- ** Featurization
    Featurization (Featurization'),
    newFeaturization,

    -- ** FeaturizationConfig
    FeaturizationConfig (FeaturizationConfig'),
    newFeaturizationConfig,

    -- ** FeaturizationMethod
    FeaturizationMethod (FeaturizationMethod'),
    newFeaturizationMethod,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** ForecastExportJobSummary
    ForecastExportJobSummary (ForecastExportJobSummary'),
    newForecastExportJobSummary,

    -- ** ForecastSummary
    ForecastSummary (ForecastSummary'),
    newForecastSummary,

    -- ** HyperParameterTuningJobConfig
    HyperParameterTuningJobConfig (HyperParameterTuningJobConfig'),
    newHyperParameterTuningJobConfig,

    -- ** InputDataConfig
    InputDataConfig (InputDataConfig'),
    newInputDataConfig,

    -- ** IntegerParameterRange
    IntegerParameterRange (IntegerParameterRange'),
    newIntegerParameterRange,

    -- ** Metrics
    Metrics (Metrics'),
    newMetrics,

    -- ** ParameterRanges
    ParameterRanges (ParameterRanges'),
    newParameterRanges,

    -- ** PredictorBacktestExportJobSummary
    PredictorBacktestExportJobSummary (PredictorBacktestExportJobSummary'),
    newPredictorBacktestExportJobSummary,

    -- ** PredictorExecution
    PredictorExecution (PredictorExecution'),
    newPredictorExecution,

    -- ** PredictorExecutionDetails
    PredictorExecutionDetails (PredictorExecutionDetails'),
    newPredictorExecutionDetails,

    -- ** PredictorSummary
    PredictorSummary (PredictorSummary'),
    newPredictorSummary,

    -- ** S3Config
    S3Config (S3Config'),
    newS3Config,

    -- ** Schema
    Schema (Schema'),
    newSchema,

    -- ** SchemaAttribute
    SchemaAttribute (SchemaAttribute'),
    newSchemaAttribute,

    -- ** Statistics
    Statistics (Statistics'),
    newStatistics,

    -- ** SupplementaryFeature
    SupplementaryFeature (SupplementaryFeature'),
    newSupplementaryFeature,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TestWindowSummary
    TestWindowSummary (TestWindowSummary'),
    newTestWindowSummary,

    -- ** WeightedQuantileLoss
    WeightedQuantileLoss (WeightedQuantileLoss'),
    newWeightedQuantileLoss,

    -- ** WindowSummary
    WindowSummary (WindowSummary'),
    newWindowSummary,
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
import Amazonka.Forecast.Lens
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
import Amazonka.Forecast.Types
import Amazonka.Forecast.UntagResource
import Amazonka.Forecast.UpdateDatasetGroup
import Amazonka.Forecast.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Forecast'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
