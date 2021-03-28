-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _InvalidTagException
    , _InternalServerException
    , _InvalidInputException
    , _IdempotentParameterMismatchException
    , _TagLimitExceededException
    , _PredictorNotMountedException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * EntityName
    , EntityName (..)

    -- * RDSDatabaseName
    , RDSDatabaseName (..)

    -- * BatchPredictionFilterVariable
    , BatchPredictionFilterVariable (..)

    -- * RDSInstanceIdentifier
    , RDSInstanceIdentifier (..)

    -- * RDSDatabasePassword
    , RDSDatabasePassword (..)

    -- * PerformanceMetrics
    , PerformanceMetrics (..)
    , mkPerformanceMetrics
    , pmProperties

    -- * EDPResourceRole
    , EDPResourceRole (..)

    -- * RedshiftClusterIdentifier
    , RedshiftClusterIdentifier (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * RealtimeEndpointInfo
    , RealtimeEndpointInfo (..)
    , mkRealtimeEndpointInfo
    , reiCreatedAt
    , reiEndpointStatus
    , reiEndpointUrl
    , reiPeakRequestsPerSecond

    -- * Prediction
    , Prediction (..)
    , mkPrediction
    , pDetails
    , pPredictedLabel
    , pPredictedScores
    , pPredictedValue

    -- * EDPSecurityGroupId
    , EDPSecurityGroupId (..)

    -- * S3DataSpec
    , S3DataSpec (..)
    , mkS3DataSpec
    , sdsDataLocationS3
    , sdsDataRearrangement
    , sdsDataSchema
    , sdsDataSchemaLocationS3

    -- * DetailsAttributes
    , DetailsAttributes (..)

    -- * EvaluationFilterVariable
    , EvaluationFilterVariable (..)

    -- * RealtimeEndpointStatus
    , RealtimeEndpointStatus (..)

    -- * RDSDataSpec
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

    -- * Recipe
    , Recipe (..)

    -- * EDPServiceRole
    , EDPServiceRole (..)

    -- * RDSMetadata
    , RDSMetadata (..)
    , mkRDSMetadata
    , rdsmDataPipelineId
    , rdsmDatabase
    , rdsmDatabaseUserName
    , rdsmResourceRole
    , rdsmSelectSqlQuery
    , rdsmServiceRole

    -- * RedshiftDatabaseUsername
    , RedshiftDatabaseUsername (..)

    -- * PerformanceMetricsPropertyValue
    , PerformanceMetricsPropertyValue (..)

    -- * RedshiftDatabase
    , RedshiftDatabase (..)
    , mkRedshiftDatabase
    , rdDatabaseName
    , rdClusterIdentifier

    -- * RedshiftDatabaseCredentials
    , RedshiftDatabaseCredentials (..)
    , mkRedshiftDatabaseCredentials
    , rdcUsername
    , rdcPassword

    -- * MLModel
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

    -- * DataSchema
    , DataSchema (..)

    -- * AwsUserArn
    , AwsUserArn (..)

    -- * VariableName
    , VariableName (..)

    -- * MLModelName
    , MLModelName (..)

    -- * BatchPrediction
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

    -- * VipURL
    , VipURL (..)

    -- * SortOrder
    , SortOrder (..)

    -- * RedshiftSelectSqlQuery
    , RedshiftSelectSqlQuery (..)

    -- * Algorithm
    , Algorithm (..)

    -- * PerformanceMetricsPropertyKey
    , PerformanceMetricsPropertyKey (..)

    -- * EntityStatus
    , EntityStatus (..)

    -- * DataSource
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

    -- * VariableValue
    , VariableValue (..)

    -- * StringType
    , StringType (..)

    -- * TaggableResourceType
    , TaggableResourceType (..)

    -- * TagKey
    , TagKey (..)

    -- * RDSDatabase
    , RDSDatabase (..)
    , mkRDSDatabase
    , rdsdInstanceIdentifier
    , rdsdDatabaseName

    -- * RDSDatabaseCredentials
    , RDSDatabaseCredentials (..)
    , mkRDSDatabaseCredentials
    , rdsdcUsername
    , rdsdcPassword

    -- * MLModelFilterVariable
    , MLModelFilterVariable (..)

    -- * S3Url
    , S3Url (..)

    -- * Message
    , Message (..)

    -- * DataSourceFilterVariable
    , DataSourceFilterVariable (..)

    -- * RedshiftDataSpec
    , RedshiftDataSpec (..)
    , mkRedshiftDataSpec
    , rdsDatabaseInformation
    , rdsSelectSqlQuery
    , rdsDatabaseCredentials
    , rdsS3StagingLocation
    , rdsDataRearrangement
    , rdsDataSchema
    , rdsDataSchemaUri

    -- * EntityId
    , EntityId (..)

    -- * Label
    , Label (..)

    -- * Evaluation
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

    -- * RedshiftMetadata
    , RedshiftMetadata (..)
    , mkRedshiftMetadata
    , rmDatabaseUserName
    , rmRedshiftDatabase
    , rmSelectSqlQuery

    -- * MLModelType
    , MLModelType (..)

    -- * DetailsValue
    , DetailsValue (..)

    -- * ComparatorValue
    , ComparatorValue (..)

    -- * DataRearrangement
    , DataRearrangement (..)

    -- * RoleARN
    , RoleARN (..)

    -- * DataSourceId
    , DataSourceId (..)

    -- * DataSourceName
    , DataSourceName (..)

    -- * EvaluationId
    , EvaluationId (..)

    -- * MLModelId
    , MLModelId (..)

    -- * EvaluationDataSourceId
    , EvaluationDataSourceId (..)

    -- * EvaluationName
    , EvaluationName (..)

    -- * NextToken
    , NextToken (..)

    -- * ResourceId
    , ResourceId (..)

    -- * CreatedByIamUser
    , CreatedByIamUser (..)

    -- * InputDataLocationS3
    , InputDataLocationS3 (..)

    -- * LogUri
    , LogUri (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * BatchPredictionId
    , BatchPredictionId (..)

    -- * EndpointUrl
    , EndpointUrl (..)

    -- * PredictedLabel
    , PredictedLabel (..)

    -- * DataLocationS3
    , DataLocationS3 (..)

    -- * DataSchemaLocationS3
    , DataSchemaLocationS3 (..)

    -- * SelectSqlQuery
    , SelectSqlQuery (..)

    -- * S3StagingLocation
    , S3StagingLocation (..)

    -- * ServiceRole
    , ServiceRole (..)

    -- * SubnetId
    , SubnetId (..)

    -- * DataSchemaUri
    , DataSchemaUri (..)

    -- * DataPipelineId
    , DataPipelineId (..)

    -- * DatabaseUserName
    , DatabaseUserName (..)

    -- * BatchPredictionDataSourceId
    , BatchPredictionDataSourceId (..)

    -- * OutputUri
    , OutputUri (..)

    -- * DatabaseName
    , DatabaseName (..)

    -- * Password
    , Password (..)

    -- * Name
    , Name (..)

    -- * TrainingDataSourceId
    , TrainingDataSourceId (..)

    -- * RecipeUri
    , RecipeUri (..)

    -- * EQ
    , EQ (..)

    -- * GE
    , GE (..)

    -- * GT
    , GT (..)

    -- * LE
    , LE (..)

    -- * LT
    , LT (..)

    -- * NE
    , NE (..)

    -- * Prefix
    , Prefix (..)

    -- * Username
    , Username (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
  
import Network.AWS.MachineLearning.Types.EntityName
  
import Network.AWS.MachineLearning.Types.RDSDatabaseName
  
import Network.AWS.MachineLearning.Types.BatchPredictionFilterVariable
  
import Network.AWS.MachineLearning.Types.RDSInstanceIdentifier
  
import Network.AWS.MachineLearning.Types.RDSDatabasePassword
  
import Network.AWS.MachineLearning.Types.PerformanceMetrics
  
import Network.AWS.MachineLearning.Types.EDPResourceRole
  
import Network.AWS.MachineLearning.Types.RedshiftClusterIdentifier
  
import Network.AWS.MachineLearning.Types.Tag
  
import Network.AWS.MachineLearning.Types.RealtimeEndpointInfo
  
import Network.AWS.MachineLearning.Types.Prediction
  
import Network.AWS.MachineLearning.Types.EDPSecurityGroupId
  
import Network.AWS.MachineLearning.Types.S3DataSpec
  
import Network.AWS.MachineLearning.Types.DetailsAttributes
  
import Network.AWS.MachineLearning.Types.EvaluationFilterVariable
  
import Network.AWS.MachineLearning.Types.RealtimeEndpointStatus
  
import Network.AWS.MachineLearning.Types.RDSDataSpec
  
import Network.AWS.MachineLearning.Types.Recipe
  
import Network.AWS.MachineLearning.Types.EDPServiceRole
  
import Network.AWS.MachineLearning.Types.RDSMetadata
  
import Network.AWS.MachineLearning.Types.RedshiftDatabaseUsername
  
import Network.AWS.MachineLearning.Types.PerformanceMetricsPropertyValue
  
import Network.AWS.MachineLearning.Types.RedshiftDatabase
  
import Network.AWS.MachineLearning.Types.RedshiftDatabaseCredentials
  
import Network.AWS.MachineLearning.Types.MLModel
  
import Network.AWS.MachineLearning.Types.DataSchema
  
import Network.AWS.MachineLearning.Types.AwsUserArn
  
import Network.AWS.MachineLearning.Types.VariableName
  
import Network.AWS.MachineLearning.Types.MLModelName
  
import Network.AWS.MachineLearning.Types.BatchPrediction
  
import Network.AWS.MachineLearning.Types.VipURL
  
import Network.AWS.MachineLearning.Types.SortOrder
  
import Network.AWS.MachineLearning.Types.RedshiftSelectSqlQuery
  
import Network.AWS.MachineLearning.Types.Algorithm
  
import Network.AWS.MachineLearning.Types.PerformanceMetricsPropertyKey
  
import Network.AWS.MachineLearning.Types.EntityStatus
  
import Network.AWS.MachineLearning.Types.DataSource
  
import Network.AWS.MachineLearning.Types.VariableValue
  
import Network.AWS.MachineLearning.Types.StringType
  
  
import Network.AWS.MachineLearning.Types.TaggableResourceType
  
import Network.AWS.MachineLearning.Types.TagKey
  
import Network.AWS.MachineLearning.Types.RDSDatabase
  
  
import Network.AWS.MachineLearning.Types.RDSDatabaseCredentials
  
import Network.AWS.MachineLearning.Types.MLModelFilterVariable
  
import Network.AWS.MachineLearning.Types.S3Url
  
  
import Network.AWS.MachineLearning.Types.Message
  
import Network.AWS.MachineLearning.Types.DataSourceFilterVariable
  
  
import Network.AWS.MachineLearning.Types.RedshiftDataSpec
  
import Network.AWS.MachineLearning.Types.EntityId
  
import Network.AWS.MachineLearning.Types.Label
  
import Network.AWS.MachineLearning.Types.Evaluation
  
  
import Network.AWS.MachineLearning.Types.RedshiftMetadata
  
  
import Network.AWS.MachineLearning.Types.MLModelType
  
import Network.AWS.MachineLearning.Types.DetailsValue
  
import Network.AWS.MachineLearning.Types.ComparatorValue
  
import Network.AWS.MachineLearning.Types.DataRearrangement
  
import Network.AWS.MachineLearning.Types.RoleARN
  
  
import Network.AWS.MachineLearning.Types.DataSourceId
  
import Network.AWS.MachineLearning.Types.DataSourceName
  
import Network.AWS.MachineLearning.Types.EvaluationId
  
import Network.AWS.MachineLearning.Types.MLModelId
  
import Network.AWS.MachineLearning.Types.EvaluationDataSourceId
  
import Network.AWS.MachineLearning.Types.EvaluationName
  
import Network.AWS.MachineLearning.Types.NextToken
  
import Network.AWS.MachineLearning.Types.ResourceId
  
import Network.AWS.MachineLearning.Types.CreatedByIamUser
  
import Network.AWS.MachineLearning.Types.InputDataLocationS3
  
import Network.AWS.MachineLearning.Types.LogUri
  
import Network.AWS.MachineLearning.Types.Key
  
import Network.AWS.MachineLearning.Types.Value
  
import Network.AWS.MachineLearning.Types.BatchPredictionId
  
import Network.AWS.MachineLearning.Types.EndpointUrl
  
import Network.AWS.MachineLearning.Types.PredictedLabel
  
import Network.AWS.MachineLearning.Types.DataLocationS3
  
import Network.AWS.MachineLearning.Types.DataSchemaLocationS3
  
import Network.AWS.MachineLearning.Types.SelectSqlQuery
  
import Network.AWS.MachineLearning.Types.S3StagingLocation
  
import Network.AWS.MachineLearning.Types.ServiceRole
  
import Network.AWS.MachineLearning.Types.SubnetId
  
import Network.AWS.MachineLearning.Types.DataSchemaUri
  
import Network.AWS.MachineLearning.Types.DataPipelineId
  
import Network.AWS.MachineLearning.Types.DatabaseUserName
  
import Network.AWS.MachineLearning.Types.BatchPredictionDataSourceId
  
import Network.AWS.MachineLearning.Types.OutputUri
  
import Network.AWS.MachineLearning.Types.DatabaseName
  
import Network.AWS.MachineLearning.Types.Password
  
import Network.AWS.MachineLearning.Types.Name
  
import Network.AWS.MachineLearning.Types.TrainingDataSourceId
  
import Network.AWS.MachineLearning.Types.RecipeUri
  
import Network.AWS.MachineLearning.Types.EQ
  
import Network.AWS.MachineLearning.Types.GE
  
import Network.AWS.MachineLearning.Types.GT
  
import Network.AWS.MachineLearning.Types.LE
  
import Network.AWS.MachineLearning.Types.LT
  
import Network.AWS.MachineLearning.Types.NE
  
import Network.AWS.MachineLearning.Types.Prefix
  
import Network.AWS.MachineLearning.Types.Username
  

-- | API version @2014-12-12@ of the Amazon Machine Learning SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "MachineLearning",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "machinelearning",
                 Core._svcVersion = "2014-12-12", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "MachineLearning",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | Prism for 'InvalidTagException' errors.
_InvalidTagException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagException
  = Core._MatchServiceError mkServiceConfig "InvalidTagException"
{-# INLINEABLE _InvalidTagException #-}
{-# DEPRECATED _InvalidTagException "Use generic-lens or generic-optics instead"  #-}

-- | An error on the server occurred when trying to process a request.
_InternalServerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerException
  = Core._MatchServiceError mkServiceConfig "InternalServerException"
      Core.. Core.hasStatues 500
{-# INLINEABLE _InternalServerException #-}
{-# DEPRECATED _InternalServerException "Use generic-lens or generic-optics instead"  #-}

-- | An error on the client occurred. Typically, the cause is an invalid input value.
_InvalidInputException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInputException
  = Core._MatchServiceError mkServiceConfig "InvalidInputException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidInputException #-}
{-# DEPRECATED _InvalidInputException "Use generic-lens or generic-optics instead"  #-}

-- | A second request to use or change an object was not allowed. This can result from retrying a request using a parameter that was not present in the original request.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException
  = Core._MatchServiceError mkServiceConfig
      "IdempotentParameterMismatchException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _IdempotentParameterMismatchException #-}
{-# DEPRECATED _IdempotentParameterMismatchException "Use generic-lens or generic-optics instead"  #-}

-- | Prism for 'TagLimitExceededException' errors.
_TagLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "TagLimitExceededException"
{-# INLINEABLE _TagLimitExceededException #-}
{-# DEPRECATED _TagLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The exception is thrown when a predict request is made to an unmounted @MLModel@ .
_PredictorNotMountedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PredictorNotMountedException
  = Core._MatchServiceError mkServiceConfig
      "PredictorNotMountedException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _PredictorNotMountedException #-}
{-# DEPRECATED _PredictorNotMountedException "Use generic-lens or generic-optics instead"  #-}

-- | A specified resource cannot be located.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The subscriber exceeded the maximum number of operations. This exception can occur when listing objects such as @DataSource@ .
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
      Core.. Core.hasStatues 417
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}
