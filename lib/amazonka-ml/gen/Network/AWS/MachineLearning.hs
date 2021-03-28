{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Definition of the public APIs exposed by Amazon Machine Learning
module Network.AWS.MachineLearning
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** InvalidTagException
    , _InvalidTagException

    -- ** InternalServerException
    , _InternalServerException

    -- ** InvalidInputException
    , _InvalidInputException

    -- ** IdempotentParameterMismatchException
    , _IdempotentParameterMismatchException

    -- ** TagLimitExceededException
    , _TagLimitExceededException

    -- ** PredictorNotMountedException
    , _PredictorNotMountedException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- ** MLModelAvailable
    , mkMLModelAvailable

    -- ** BatchPredictionAvailable
    , mkBatchPredictionAvailable

    -- ** DataSourceAvailable
    , mkDataSourceAvailable

    -- ** EvaluationAvailable
    , mkEvaluationAvailable

    -- * Operations
    -- $operations

    -- ** UpdateDataSource 
    , module Network.AWS.MachineLearning.UpdateDataSource

    -- ** DeleteDataSource 
    , module Network.AWS.MachineLearning.DeleteDataSource

    -- ** DescribeTags 
    , module Network.AWS.MachineLearning.DescribeTags

    -- ** CreateDataSourceFromRedshift 
    , module Network.AWS.MachineLearning.CreateDataSourceFromRedshift

    -- ** CreateDataSourceFromS3 
    , module Network.AWS.MachineLearning.CreateDataSourceFromS3

    -- ** CreateMLModel 
    , module Network.AWS.MachineLearning.CreateMLModel

    -- ** DeleteTags 
    , module Network.AWS.MachineLearning.DeleteTags

    -- ** DeleteBatchPrediction 
    , module Network.AWS.MachineLearning.DeleteBatchPrediction

    -- ** UpdateBatchPrediction 
    , module Network.AWS.MachineLearning.UpdateBatchPrediction

    -- ** GetMLModel 
    , module Network.AWS.MachineLearning.GetMLModel

    -- ** GetDataSource 
    , module Network.AWS.MachineLearning.GetDataSource

    -- ** UpdateEvaluation 
    , module Network.AWS.MachineLearning.UpdateEvaluation

    -- ** DeleteEvaluation 
    , module Network.AWS.MachineLearning.DeleteEvaluation

    -- ** DeleteMLModel 
    , module Network.AWS.MachineLearning.DeleteMLModel

    -- ** UpdateMLModel 
    , module Network.AWS.MachineLearning.UpdateMLModel

    -- ** GetBatchPrediction 
    , module Network.AWS.MachineLearning.GetBatchPrediction

    -- ** DescribeBatchPredictions (Paginated)
    , module Network.AWS.MachineLearning.DescribeBatchPredictions

    -- ** CreateDataSourceFromRDS 
    , module Network.AWS.MachineLearning.CreateDataSourceFromRDS

    -- ** CreateEvaluation 
    , module Network.AWS.MachineLearning.CreateEvaluation

    -- ** Predict 
    , module Network.AWS.MachineLearning.Predict

    -- ** DeleteRealtimeEndpoint 
    , module Network.AWS.MachineLearning.DeleteRealtimeEndpoint

    -- ** CreateBatchPrediction 
    , module Network.AWS.MachineLearning.CreateBatchPrediction

    -- ** GetEvaluation 
    , module Network.AWS.MachineLearning.GetEvaluation

    -- ** DescribeEvaluations (Paginated)
    , module Network.AWS.MachineLearning.DescribeEvaluations

    -- ** CreateRealtimeEndpoint 
    , module Network.AWS.MachineLearning.CreateRealtimeEndpoint

    -- ** AddTags 
    , module Network.AWS.MachineLearning.AddTags

    -- ** DescribeMLModels (Paginated)
    , module Network.AWS.MachineLearning.DescribeMLModels

    -- ** DescribeDataSources (Paginated)
    , module Network.AWS.MachineLearning.DescribeDataSources

    -- * Types

    -- ** EntityName
    , EntityName (..)

    -- ** RDSDatabaseName
    , RDSDatabaseName (..)

    -- ** BatchPredictionFilterVariable
    , BatchPredictionFilterVariable (..)

    -- ** RDSInstanceIdentifier
    , RDSInstanceIdentifier (..)

    -- ** RDSDatabasePassword
    , RDSDatabasePassword (..)

    -- ** PerformanceMetrics
    , PerformanceMetrics (..)
    , mkPerformanceMetrics
    , pmProperties

    -- ** EDPResourceRole
    , EDPResourceRole (..)

    -- ** RedshiftClusterIdentifier
    , RedshiftClusterIdentifier (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** RealtimeEndpointInfo
    , RealtimeEndpointInfo (..)
    , mkRealtimeEndpointInfo
    , reiCreatedAt
    , reiEndpointStatus
    , reiEndpointUrl
    , reiPeakRequestsPerSecond

    -- ** Prediction
    , Prediction (..)
    , mkPrediction
    , pDetails
    , pPredictedLabel
    , pPredictedScores
    , pPredictedValue

    -- ** EDPSecurityGroupId
    , EDPSecurityGroupId (..)

    -- ** S3DataSpec
    , S3DataSpec (..)
    , mkS3DataSpec
    , sdsDataLocationS3
    , sdsDataRearrangement
    , sdsDataSchema
    , sdsDataSchemaLocationS3

    -- ** DetailsAttributes
    , DetailsAttributes (..)

    -- ** EvaluationFilterVariable
    , EvaluationFilterVariable (..)

    -- ** RealtimeEndpointStatus
    , RealtimeEndpointStatus (..)

    -- ** RDSDataSpec
    , RDSDataSpec (..)
    , mkRDSDataSpec
    , rdsdsDatabaseInformation
    , rdsdsSelectSqlQuery
    , rdsdsDatabaseCredentials
    , rdsdsS3StagingLocation
    , rdsdsResourceRole
    , rdsdsServiceRole
    , rdsdsSubnetId
    , rdsdsSecurityGroupIds
    , rdsdsDataRearrangement
    , rdsdsDataSchema
    , rdsdsDataSchemaUri

    -- ** Recipe
    , Recipe (..)

    -- ** EDPServiceRole
    , EDPServiceRole (..)

    -- ** RDSMetadata
    , RDSMetadata (..)
    , mkRDSMetadata
    , rdsmDataPipelineId
    , rdsmDatabase
    , rdsmDatabaseUserName
    , rdsmResourceRole
    , rdsmSelectSqlQuery
    , rdsmServiceRole

    -- ** RedshiftDatabaseUsername
    , RedshiftDatabaseUsername (..)

    -- ** PerformanceMetricsPropertyValue
    , PerformanceMetricsPropertyValue (..)

    -- ** RedshiftDatabase
    , RedshiftDatabase (..)
    , mkRedshiftDatabase
    , rdDatabaseName
    , rdClusterIdentifier

    -- ** RedshiftDatabaseCredentials
    , RedshiftDatabaseCredentials (..)
    , mkRedshiftDatabaseCredentials
    , rdcUsername
    , rdcPassword

    -- ** MLModel
    , MLModel (..)
    , mkMLModel
    , mlmAlgorithm
    , mlmComputeTime
    , mlmCreatedAt
    , mlmCreatedByIamUser
    , mlmEndpointInfo
    , mlmFinishedAt
    , mlmInputDataLocationS3
    , mlmLastUpdatedAt
    , mlmMLModelId
    , mlmMLModelType
    , mlmMessage
    , mlmName
    , mlmScoreThreshold
    , mlmScoreThresholdLastUpdatedAt
    , mlmSizeInBytes
    , mlmStartedAt
    , mlmStatus
    , mlmTrainingDataSourceId
    , mlmTrainingParameters

    -- ** DataSchema
    , DataSchema (..)

    -- ** AwsUserArn
    , AwsUserArn (..)

    -- ** VariableName
    , VariableName (..)

    -- ** MLModelName
    , MLModelName (..)

    -- ** BatchPrediction
    , BatchPrediction (..)
    , mkBatchPrediction
    , bpBatchPredictionDataSourceId
    , bpBatchPredictionId
    , bpComputeTime
    , bpCreatedAt
    , bpCreatedByIamUser
    , bpFinishedAt
    , bpInputDataLocationS3
    , bpInvalidRecordCount
    , bpLastUpdatedAt
    , bpMLModelId
    , bpMessage
    , bpName
    , bpOutputUri
    , bpStartedAt
    , bpStatus
    , bpTotalRecordCount

    -- ** VipURL
    , VipURL (..)

    -- ** SortOrder
    , SortOrder (..)

    -- ** RedshiftSelectSqlQuery
    , RedshiftSelectSqlQuery (..)

    -- ** Algorithm
    , Algorithm (..)

    -- ** PerformanceMetricsPropertyKey
    , PerformanceMetricsPropertyKey (..)

    -- ** EntityStatus
    , EntityStatus (..)

    -- ** DataSource
    , DataSource (..)
    , mkDataSource
    , dsComputeStatistics
    , dsComputeTime
    , dsCreatedAt
    , dsCreatedByIamUser
    , dsDataLocationS3
    , dsDataRearrangement
    , dsDataSizeInBytes
    , dsDataSourceId
    , dsFinishedAt
    , dsLastUpdatedAt
    , dsMessage
    , dsName
    , dsNumberOfFiles
    , dsRDSMetadata
    , dsRedshiftMetadata
    , dsRoleARN
    , dsStartedAt
    , dsStatus

    -- ** VariableValue
    , VariableValue (..)

    -- ** StringType
    , StringType (..)

    -- ** TaggableResourceType
    , TaggableResourceType (..)

    -- ** TagKey
    , TagKey (..)

    -- ** RDSDatabase
    , RDSDatabase (..)
    , mkRDSDatabase
    , rdsdInstanceIdentifier
    , rdsdDatabaseName

    -- ** RDSDatabaseCredentials
    , RDSDatabaseCredentials (..)
    , mkRDSDatabaseCredentials
    , rdsdcUsername
    , rdsdcPassword

    -- ** MLModelFilterVariable
    , MLModelFilterVariable (..)

    -- ** S3Url
    , S3Url (..)

    -- ** Message
    , Message (..)

    -- ** DataSourceFilterVariable
    , DataSourceFilterVariable (..)

    -- ** RedshiftDataSpec
    , RedshiftDataSpec (..)
    , mkRedshiftDataSpec
    , rdsDatabaseInformation
    , rdsSelectSqlQuery
    , rdsDatabaseCredentials
    , rdsS3StagingLocation
    , rdsDataRearrangement
    , rdsDataSchema
    , rdsDataSchemaUri

    -- ** EntityId
    , EntityId (..)

    -- ** Label
    , Label (..)

    -- ** Evaluation
    , Evaluation (..)
    , mkEvaluation
    , eComputeTime
    , eCreatedAt
    , eCreatedByIamUser
    , eEvaluationDataSourceId
    , eEvaluationId
    , eFinishedAt
    , eInputDataLocationS3
    , eLastUpdatedAt
    , eMLModelId
    , eMessage
    , eName
    , ePerformanceMetrics
    , eStartedAt
    , eStatus

    -- ** RedshiftMetadata
    , RedshiftMetadata (..)
    , mkRedshiftMetadata
    , rmDatabaseUserName
    , rmRedshiftDatabase
    , rmSelectSqlQuery

    -- ** MLModelType
    , MLModelType (..)

    -- ** DetailsValue
    , DetailsValue (..)

    -- ** ComparatorValue
    , ComparatorValue (..)

    -- ** DataRearrangement
    , DataRearrangement (..)

    -- ** RoleARN
    , RoleARN (..)

    -- ** DataSourceId
    , DataSourceId (..)

    -- ** DataSourceName
    , DataSourceName (..)

    -- ** EvaluationId
    , EvaluationId (..)

    -- ** MLModelId
    , MLModelId (..)

    -- ** EvaluationDataSourceId
    , EvaluationDataSourceId (..)

    -- ** EvaluationName
    , EvaluationName (..)

    -- ** NextToken
    , NextToken (..)

    -- ** ResourceId
    , ResourceId (..)

    -- ** CreatedByIamUser
    , CreatedByIamUser (..)

    -- ** InputDataLocationS3
    , InputDataLocationS3 (..)

    -- ** LogUri
    , LogUri (..)

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- ** BatchPredictionId
    , BatchPredictionId (..)

    -- ** EndpointUrl
    , EndpointUrl (..)

    -- ** PredictedLabel
    , PredictedLabel (..)

    -- ** DataLocationS3
    , DataLocationS3 (..)

    -- ** DataSchemaLocationS3
    , DataSchemaLocationS3 (..)

    -- ** SelectSqlQuery
    , SelectSqlQuery (..)

    -- ** S3StagingLocation
    , S3StagingLocation (..)

    -- ** ServiceRole
    , ServiceRole (..)

    -- ** SubnetId
    , SubnetId (..)

    -- ** DataSchemaUri
    , DataSchemaUri (..)

    -- ** DataPipelineId
    , DataPipelineId (..)

    -- ** DatabaseUserName
    , DatabaseUserName (..)

    -- ** BatchPredictionDataSourceId
    , BatchPredictionDataSourceId (..)

    -- ** OutputUri
    , OutputUri (..)

    -- ** DatabaseName
    , DatabaseName (..)

    -- ** Password
    , Password (..)

    -- ** Name
    , Name (..)

    -- ** TrainingDataSourceId
    , TrainingDataSourceId (..)

    -- ** RecipeUri
    , RecipeUri (..)

    -- ** EQ
    , EQ (..)

    -- ** GE
    , GE (..)

    -- ** GT
    , GT (..)

    -- ** LE
    , LE (..)

    -- ** LT
    , LT (..)

    -- ** NE
    , NE (..)

    -- ** Prefix
    , Prefix (..)

    -- ** Username
    , Username (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Waiters
import Network.AWS.MachineLearning.UpdateDataSource
import Network.AWS.MachineLearning.DeleteDataSource
import Network.AWS.MachineLearning.DescribeTags
import Network.AWS.MachineLearning.CreateDataSourceFromRedshift
import Network.AWS.MachineLearning.CreateDataSourceFromS3
import Network.AWS.MachineLearning.CreateMLModel
import Network.AWS.MachineLearning.DeleteTags
import Network.AWS.MachineLearning.DeleteBatchPrediction
import Network.AWS.MachineLearning.UpdateBatchPrediction
import Network.AWS.MachineLearning.GetMLModel
import Network.AWS.MachineLearning.GetDataSource
import Network.AWS.MachineLearning.UpdateEvaluation
import Network.AWS.MachineLearning.DeleteEvaluation
import Network.AWS.MachineLearning.DeleteMLModel
import Network.AWS.MachineLearning.UpdateMLModel
import Network.AWS.MachineLearning.GetBatchPrediction
import Network.AWS.MachineLearning.DescribeBatchPredictions
import Network.AWS.MachineLearning.CreateDataSourceFromRDS
import Network.AWS.MachineLearning.CreateEvaluation
import Network.AWS.MachineLearning.Predict
import Network.AWS.MachineLearning.DeleteRealtimeEndpoint
import Network.AWS.MachineLearning.CreateBatchPrediction
import Network.AWS.MachineLearning.GetEvaluation
import Network.AWS.MachineLearning.DescribeEvaluations
import Network.AWS.MachineLearning.CreateRealtimeEndpoint
import Network.AWS.MachineLearning.AddTags
import Network.AWS.MachineLearning.DescribeMLModels
import Network.AWS.MachineLearning.DescribeDataSources
import qualified Network.AWS.Prelude as Lude

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
