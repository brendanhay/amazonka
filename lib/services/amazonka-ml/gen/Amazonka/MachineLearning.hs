{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MachineLearning
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2014-12-12@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Definition of the public APIs exposed by Amazon Machine Learning
module Amazonka.MachineLearning
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** InvalidTagException
    _InvalidTagException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** PredictorNotMountedException
    _PredictorNotMountedException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** TagLimitExceededException
    _TagLimitExceededException,

    -- * Waiters
    -- $waiters

    -- ** BatchPredictionAvailable
    newBatchPredictionAvailable,

    -- ** DataSourceAvailable
    newDataSourceAvailable,

    -- ** EvaluationAvailable
    newEvaluationAvailable,

    -- ** MLModelAvailable
    newMLModelAvailable,

    -- * Operations
    -- $operations

    -- ** AddTags
    AddTags (AddTags'),
    newAddTags,
    AddTagsResponse (AddTagsResponse'),
    newAddTagsResponse,

    -- ** CreateBatchPrediction
    CreateBatchPrediction (CreateBatchPrediction'),
    newCreateBatchPrediction,
    CreateBatchPredictionResponse (CreateBatchPredictionResponse'),
    newCreateBatchPredictionResponse,

    -- ** CreateDataSourceFromRDS
    CreateDataSourceFromRDS (CreateDataSourceFromRDS'),
    newCreateDataSourceFromRDS,
    CreateDataSourceFromRDSResponse (CreateDataSourceFromRDSResponse'),
    newCreateDataSourceFromRDSResponse,

    -- ** CreateDataSourceFromRedshift
    CreateDataSourceFromRedshift (CreateDataSourceFromRedshift'),
    newCreateDataSourceFromRedshift,
    CreateDataSourceFromRedshiftResponse (CreateDataSourceFromRedshiftResponse'),
    newCreateDataSourceFromRedshiftResponse,

    -- ** CreateDataSourceFromS3
    CreateDataSourceFromS3 (CreateDataSourceFromS3'),
    newCreateDataSourceFromS3,
    CreateDataSourceFromS3Response (CreateDataSourceFromS3Response'),
    newCreateDataSourceFromS3Response,

    -- ** CreateEvaluation
    CreateEvaluation (CreateEvaluation'),
    newCreateEvaluation,
    CreateEvaluationResponse (CreateEvaluationResponse'),
    newCreateEvaluationResponse,

    -- ** CreateMLModel
    CreateMLModel (CreateMLModel'),
    newCreateMLModel,
    CreateMLModelResponse (CreateMLModelResponse'),
    newCreateMLModelResponse,

    -- ** CreateRealtimeEndpoint
    CreateRealtimeEndpoint (CreateRealtimeEndpoint'),
    newCreateRealtimeEndpoint,
    CreateRealtimeEndpointResponse (CreateRealtimeEndpointResponse'),
    newCreateRealtimeEndpointResponse,

    -- ** DeleteBatchPrediction
    DeleteBatchPrediction (DeleteBatchPrediction'),
    newDeleteBatchPrediction,
    DeleteBatchPredictionResponse (DeleteBatchPredictionResponse'),
    newDeleteBatchPredictionResponse,

    -- ** DeleteDataSource
    DeleteDataSource (DeleteDataSource'),
    newDeleteDataSource,
    DeleteDataSourceResponse (DeleteDataSourceResponse'),
    newDeleteDataSourceResponse,

    -- ** DeleteEvaluation
    DeleteEvaluation (DeleteEvaluation'),
    newDeleteEvaluation,
    DeleteEvaluationResponse (DeleteEvaluationResponse'),
    newDeleteEvaluationResponse,

    -- ** DeleteMLModel
    DeleteMLModel (DeleteMLModel'),
    newDeleteMLModel,
    DeleteMLModelResponse (DeleteMLModelResponse'),
    newDeleteMLModelResponse,

    -- ** DeleteRealtimeEndpoint
    DeleteRealtimeEndpoint (DeleteRealtimeEndpoint'),
    newDeleteRealtimeEndpoint,
    DeleteRealtimeEndpointResponse (DeleteRealtimeEndpointResponse'),
    newDeleteRealtimeEndpointResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** DescribeBatchPredictions (Paginated)
    DescribeBatchPredictions (DescribeBatchPredictions'),
    newDescribeBatchPredictions,
    DescribeBatchPredictionsResponse (DescribeBatchPredictionsResponse'),
    newDescribeBatchPredictionsResponse,

    -- ** DescribeDataSources (Paginated)
    DescribeDataSources (DescribeDataSources'),
    newDescribeDataSources,
    DescribeDataSourcesResponse (DescribeDataSourcesResponse'),
    newDescribeDataSourcesResponse,

    -- ** DescribeEvaluations (Paginated)
    DescribeEvaluations (DescribeEvaluations'),
    newDescribeEvaluations,
    DescribeEvaluationsResponse (DescribeEvaluationsResponse'),
    newDescribeEvaluationsResponse,

    -- ** DescribeMLModels (Paginated)
    DescribeMLModels (DescribeMLModels'),
    newDescribeMLModels,
    DescribeMLModelsResponse (DescribeMLModelsResponse'),
    newDescribeMLModelsResponse,

    -- ** DescribeTags
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** GetBatchPrediction
    GetBatchPrediction (GetBatchPrediction'),
    newGetBatchPrediction,
    GetBatchPredictionResponse (GetBatchPredictionResponse'),
    newGetBatchPredictionResponse,

    -- ** GetDataSource
    GetDataSource (GetDataSource'),
    newGetDataSource,
    GetDataSourceResponse (GetDataSourceResponse'),
    newGetDataSourceResponse,

    -- ** GetEvaluation
    GetEvaluation (GetEvaluation'),
    newGetEvaluation,
    GetEvaluationResponse (GetEvaluationResponse'),
    newGetEvaluationResponse,

    -- ** GetMLModel
    GetMLModel (GetMLModel'),
    newGetMLModel,
    GetMLModelResponse (GetMLModelResponse'),
    newGetMLModelResponse,

    -- ** Predict
    Predict (Predict'),
    newPredict,
    PredictResponse (PredictResponse'),
    newPredictResponse,

    -- ** UpdateBatchPrediction
    UpdateBatchPrediction (UpdateBatchPrediction'),
    newUpdateBatchPrediction,
    UpdateBatchPredictionResponse (UpdateBatchPredictionResponse'),
    newUpdateBatchPredictionResponse,

    -- ** UpdateDataSource
    UpdateDataSource (UpdateDataSource'),
    newUpdateDataSource,
    UpdateDataSourceResponse (UpdateDataSourceResponse'),
    newUpdateDataSourceResponse,

    -- ** UpdateEvaluation
    UpdateEvaluation (UpdateEvaluation'),
    newUpdateEvaluation,
    UpdateEvaluationResponse (UpdateEvaluationResponse'),
    newUpdateEvaluationResponse,

    -- ** UpdateMLModel
    UpdateMLModel (UpdateMLModel'),
    newUpdateMLModel,
    UpdateMLModelResponse (UpdateMLModelResponse'),
    newUpdateMLModelResponse,

    -- * Types

    -- ** Algorithm
    Algorithm (..),

    -- ** BatchPredictionFilterVariable
    BatchPredictionFilterVariable (..),

    -- ** DataSourceFilterVariable
    DataSourceFilterVariable (..),

    -- ** DetailsAttributes
    DetailsAttributes (..),

    -- ** EntityStatus
    EntityStatus (..),

    -- ** EvaluationFilterVariable
    EvaluationFilterVariable (..),

    -- ** MLModelFilterVariable
    MLModelFilterVariable (..),

    -- ** MLModelType
    MLModelType (..),

    -- ** RealtimeEndpointStatus
    RealtimeEndpointStatus (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** TaggableResourceType
    TaggableResourceType (..),

    -- ** BatchPrediction
    BatchPrediction (BatchPrediction'),
    newBatchPrediction,

    -- ** DataSource
    DataSource (DataSource'),
    newDataSource,

    -- ** Evaluation
    Evaluation (Evaluation'),
    newEvaluation,

    -- ** MLModel
    MLModel (MLModel'),
    newMLModel,

    -- ** PerformanceMetrics
    PerformanceMetrics (PerformanceMetrics'),
    newPerformanceMetrics,

    -- ** Prediction
    Prediction (Prediction'),
    newPrediction,

    -- ** RDSDataSpec
    RDSDataSpec (RDSDataSpec'),
    newRDSDataSpec,

    -- ** RDSDatabase
    RDSDatabase (RDSDatabase'),
    newRDSDatabase,

    -- ** RDSDatabaseCredentials
    RDSDatabaseCredentials (RDSDatabaseCredentials'),
    newRDSDatabaseCredentials,

    -- ** RDSMetadata
    RDSMetadata (RDSMetadata'),
    newRDSMetadata,

    -- ** RealtimeEndpointInfo
    RealtimeEndpointInfo (RealtimeEndpointInfo'),
    newRealtimeEndpointInfo,

    -- ** RedshiftDataSpec
    RedshiftDataSpec (RedshiftDataSpec'),
    newRedshiftDataSpec,

    -- ** RedshiftDatabase
    RedshiftDatabase (RedshiftDatabase'),
    newRedshiftDatabase,

    -- ** RedshiftDatabaseCredentials
    RedshiftDatabaseCredentials (RedshiftDatabaseCredentials'),
    newRedshiftDatabaseCredentials,

    -- ** RedshiftMetadata
    RedshiftMetadata (RedshiftMetadata'),
    newRedshiftMetadata,

    -- ** S3DataSpec
    S3DataSpec (S3DataSpec'),
    newS3DataSpec,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.MachineLearning.AddTags
import Amazonka.MachineLearning.CreateBatchPrediction
import Amazonka.MachineLearning.CreateDataSourceFromRDS
import Amazonka.MachineLearning.CreateDataSourceFromRedshift
import Amazonka.MachineLearning.CreateDataSourceFromS3
import Amazonka.MachineLearning.CreateEvaluation
import Amazonka.MachineLearning.CreateMLModel
import Amazonka.MachineLearning.CreateRealtimeEndpoint
import Amazonka.MachineLearning.DeleteBatchPrediction
import Amazonka.MachineLearning.DeleteDataSource
import Amazonka.MachineLearning.DeleteEvaluation
import Amazonka.MachineLearning.DeleteMLModel
import Amazonka.MachineLearning.DeleteRealtimeEndpoint
import Amazonka.MachineLearning.DeleteTags
import Amazonka.MachineLearning.DescribeBatchPredictions
import Amazonka.MachineLearning.DescribeDataSources
import Amazonka.MachineLearning.DescribeEvaluations
import Amazonka.MachineLearning.DescribeMLModels
import Amazonka.MachineLearning.DescribeTags
import Amazonka.MachineLearning.GetBatchPrediction
import Amazonka.MachineLearning.GetDataSource
import Amazonka.MachineLearning.GetEvaluation
import Amazonka.MachineLearning.GetMLModel
import Amazonka.MachineLearning.Lens
import Amazonka.MachineLearning.Predict
import Amazonka.MachineLearning.Types
import Amazonka.MachineLearning.UpdateBatchPrediction
import Amazonka.MachineLearning.UpdateDataSource
import Amazonka.MachineLearning.UpdateEvaluation
import Amazonka.MachineLearning.UpdateMLModel
import Amazonka.MachineLearning.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MachineLearning'.

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
