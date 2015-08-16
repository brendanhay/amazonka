{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Definition of the public APIs exposed by Amazon Machine Learning
--
-- /See:/ <http://docs.aws.amazon.com/machine-learning/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.MachineLearning
    (
    -- * Service
      MachineLearning

    -- * Errors
    -- $errors

    -- ** InternalServerException
    , _InternalServerException

    -- ** InvalidInputException
    , _InvalidInputException

    -- ** IdempotentParameterMismatchException
    , _IdempotentParameterMismatchException

    -- ** PredictorNotMountedException
    , _PredictorNotMountedException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- ** MLModelAvailable
    , mLModelAvailable

    -- ** BatchPredictionAvailable
    , batchPredictionAvailable

    -- ** DataSourceAvailable
    , dataSourceAvailable

    -- ** EvaluationAvailable
    , evaluationAvailable

    -- * Operations
    -- $operations

    -- ** DeleteDataSource
    , module Network.AWS.MachineLearning.DeleteDataSource

    -- ** UpdateDataSource
    , module Network.AWS.MachineLearning.UpdateDataSource

    -- ** CreateDataSourceFromRedshift
    , module Network.AWS.MachineLearning.CreateDataSourceFromRedshift

    -- ** CreateDataSourceFromS
    , module Network.AWS.MachineLearning.CreateDataSourceFromS

    -- ** CreateMLModel
    , module Network.AWS.MachineLearning.CreateMLModel

    -- ** DeleteBatchPrediction
    , module Network.AWS.MachineLearning.DeleteBatchPrediction

    -- ** UpdateBatchPrediction
    , module Network.AWS.MachineLearning.UpdateBatchPrediction

    -- ** GetMLModel
    , module Network.AWS.MachineLearning.GetMLModel

    -- ** GetDataSource
    , module Network.AWS.MachineLearning.GetDataSource

    -- ** DeleteMLModel
    , module Network.AWS.MachineLearning.DeleteMLModel

    -- ** UpdateMLModel
    , module Network.AWS.MachineLearning.UpdateMLModel

    -- ** DescribeBatchPredictions (Paginated)
    , module Network.AWS.MachineLearning.DescribeBatchPredictions

    -- ** UpdateEvaluation
    , module Network.AWS.MachineLearning.UpdateEvaluation

    -- ** DeleteEvaluation
    , module Network.AWS.MachineLearning.DeleteEvaluation

    -- ** GetBatchPrediction
    , module Network.AWS.MachineLearning.GetBatchPrediction

    -- ** CreateEvaluation
    , module Network.AWS.MachineLearning.CreateEvaluation

    -- ** CreateDataSourceFromRDS
    , module Network.AWS.MachineLearning.CreateDataSourceFromRDS

    -- ** CreateBatchPrediction
    , module Network.AWS.MachineLearning.CreateBatchPrediction

    -- ** Predict
    , module Network.AWS.MachineLearning.Predict

    -- ** DeleteRealtimeEndpoint
    , module Network.AWS.MachineLearning.DeleteRealtimeEndpoint

    -- ** DescribeEvaluations (Paginated)
    , module Network.AWS.MachineLearning.DescribeEvaluations

    -- ** GetEvaluation
    , module Network.AWS.MachineLearning.GetEvaluation

    -- ** CreateRealtimeEndpoint
    , module Network.AWS.MachineLearning.CreateRealtimeEndpoint

    -- ** DescribeMLModels (Paginated)
    , module Network.AWS.MachineLearning.DescribeMLModels

    -- ** DescribeDataSources (Paginated)
    , module Network.AWS.MachineLearning.DescribeDataSources

    -- * Types

    -- ** Algorithm
    , Algorithm (..)

    -- ** BatchPredictionFilterVariable
    , BatchPredictionFilterVariable (..)

    -- ** DataSourceFilterVariable
    , DataSourceFilterVariable (..)

    -- ** DetailsAttributes
    , DetailsAttributes (..)

    -- ** EntityStatus
    , EntityStatus (..)

    -- ** EvaluationFilterVariable
    , EvaluationFilterVariable (..)

    -- ** MLModelFilterVariable
    , MLModelFilterVariable (..)

    -- ** MLModelType
    , MLModelType (..)

    -- ** RealtimeEndpointStatus
    , RealtimeEndpointStatus (..)

    -- ** SortOrder
    , SortOrder (..)

    -- ** BatchPrediction
    , BatchPrediction
    , batchPrediction
    , bpStatus
    , bpLastUpdatedAt
    , bpCreatedAt
    , bpInputDataLocationS3
    , bpMLModelId
    , bpBatchPredictionDataSourceId
    , bpBatchPredictionId
    , bpName
    , bpCreatedByIAMUser
    , bpMessage
    , bpOutputURI

    -- ** DataSource
    , DataSource
    , dataSource
    , dsStatus
    , dsNumberOfFiles
    , dsLastUpdatedAt
    , dsCreatedAt
    , dsRDSMetadata
    , dsDataSourceId
    , dsDataSizeInBytes
    , dsName
    , dsCreatedByIAMUser
    , dsDataLocationS3
    , dsComputeStatistics
    , dsMessage
    , dsRedshiftMetadata
    , dsRoleARN
    , dsDataRearrangement

    -- ** Evaluation
    , Evaluation
    , evaluation
    , eStatus
    , ePerformanceMetrics
    , eLastUpdatedAt
    , eCreatedAt
    , eInputDataLocationS3
    , eMLModelId
    , eName
    , eCreatedByIAMUser
    , eMessage
    , eEvaluationId
    , eEvaluationDataSourceId

    -- ** MLModel
    , MLModel
    , mLModel
    , mlmStatus
    , mlmTrainingParameters
    , mlmLastUpdatedAt
    , mlmCreatedAt
    , mlmScoreThresholdLastUpdatedAt
    , mlmInputDataLocationS3
    , mlmSizeInBytes
    , mlmMLModelId
    , mlmScoreThreshold
    , mlmName
    , mlmAlgorithm
    , mlmCreatedByIAMUser
    , mlmEndpointInfo
    , mlmTrainingDataSourceId
    , mlmMessage
    , mlmMLModelType

    -- ** PerformanceMetrics
    , PerformanceMetrics
    , performanceMetrics
    , pmProperties

    -- ** Prediction
    , Prediction
    , prediction
    , pPredictedValue
    , pPredictedLabel
    , pPredictedScores
    , pDetails

    -- ** RDSDataSpec
    , RDSDataSpec
    , rdsDataSpec
    , rdsdsDataSchemaURI
    , rdsdsDataSchema
    , rdsdsDataRearrangement
    , rdsdsDatabaseInformation
    , rdsdsSelectSqlQuery
    , rdsdsDatabaseCredentials
    , rdsdsS3StagingLocation
    , rdsdsResourceRole
    , rdsdsServiceRole
    , rdsdsSubnetId
    , rdsdsSecurityGroupIds

    -- ** RDSDatabase
    , RDSDatabase
    , rdsDatabase
    , rdsdInstanceIdentifier
    , rdsdDatabaseName

    -- ** RDSDatabaseCredentials
    , RDSDatabaseCredentials
    , rdsDatabaseCredentials
    , rdsdcUsername
    , rdsdcPassword

    -- ** RDSMetadata
    , RDSMetadata
    , rdsMetadata
    , rmSelectSqlQuery
    , rmDataPipelineId
    , rmDatabase
    , rmDatabaseUserName
    , rmResourceRole
    , rmServiceRole

    -- ** RealtimeEndpointInfo
    , RealtimeEndpointInfo
    , realtimeEndpointInfo
    , reiCreatedAt
    , reiEndpointURL
    , reiEndpointStatus
    , reiPeakRequestsPerSecond

    -- ** RedshiftDataSpec
    , RedshiftDataSpec
    , redshiftDataSpec
    , rDataSchemaURI
    , rDataSchema
    , rDataRearrangement
    , rDatabaseInformation
    , rSelectSqlQuery
    , rDatabaseCredentials
    , rS3StagingLocation

    -- ** RedshiftDatabase
    , RedshiftDatabase
    , redshiftDatabase
    , rdDatabaseName
    , rdClusterIdentifier

    -- ** RedshiftDatabaseCredentials
    , RedshiftDatabaseCredentials
    , redshiftDatabaseCredentials
    , rdcUsername
    , rdcPassword

    -- ** RedshiftMetadata
    , RedshiftMetadata
    , redshiftMetadata
    , redSelectSqlQuery
    , redRedshiftDatabase
    , redDatabaseUserName

    -- ** S3DataSpec
    , S3DataSpec
    , s3DataSpec
    , sdsDataSchema
    , sdsDataSchemaLocationS3
    , sdsDataRearrangement
    , sdsDataLocationS3
    ) where

import           Network.AWS.MachineLearning.CreateBatchPrediction
import           Network.AWS.MachineLearning.CreateDataSourceFromRDS
import           Network.AWS.MachineLearning.CreateDataSourceFromRedshift
import           Network.AWS.MachineLearning.CreateDataSourceFromS
import           Network.AWS.MachineLearning.CreateEvaluation
import           Network.AWS.MachineLearning.CreateMLModel
import           Network.AWS.MachineLearning.CreateRealtimeEndpoint
import           Network.AWS.MachineLearning.DeleteBatchPrediction
import           Network.AWS.MachineLearning.DeleteDataSource
import           Network.AWS.MachineLearning.DeleteEvaluation
import           Network.AWS.MachineLearning.DeleteMLModel
import           Network.AWS.MachineLearning.DeleteRealtimeEndpoint
import           Network.AWS.MachineLearning.DescribeBatchPredictions
import           Network.AWS.MachineLearning.DescribeDataSources
import           Network.AWS.MachineLearning.DescribeEvaluations
import           Network.AWS.MachineLearning.DescribeMLModels
import           Network.AWS.MachineLearning.GetBatchPrediction
import           Network.AWS.MachineLearning.GetDataSource
import           Network.AWS.MachineLearning.GetEvaluation
import           Network.AWS.MachineLearning.GetMLModel
import           Network.AWS.MachineLearning.Predict
import           Network.AWS.MachineLearning.Types
import           Network.AWS.MachineLearning.UpdateBatchPrediction
import           Network.AWS.MachineLearning.UpdateDataSource
import           Network.AWS.MachineLearning.UpdateEvaluation
import           Network.AWS.MachineLearning.UpdateMLModel
import           Network.AWS.MachineLearning.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'MachineLearning'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
