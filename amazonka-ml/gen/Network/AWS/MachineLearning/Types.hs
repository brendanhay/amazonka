{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types
    (
    -- * Service
      MachineLearning

    -- * Errors
    , _InternalServerException
    , _InvalidInputException
    , _IdempotentParameterMismatchException
    , _PredictorNotMountedException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * Algorithm
    , Algorithm (..)

    -- * BatchPredictionFilterVariable
    , BatchPredictionFilterVariable (..)

    -- * DataSourceFilterVariable
    , DataSourceFilterVariable (..)

    -- * DetailsAttributes
    , DetailsAttributes (..)

    -- * EntityStatus
    , EntityStatus (..)

    -- * EvaluationFilterVariable
    , EvaluationFilterVariable (..)

    -- * MLModelFilterVariable
    , MLModelFilterVariable (..)

    -- * MLModelType
    , MLModelType (..)

    -- * RealtimeEndpointStatus
    , RealtimeEndpointStatus (..)

    -- * SortOrder
    , SortOrder (..)

    -- * BatchPrediction
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

    -- * DataSource
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

    -- * Evaluation
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

    -- * MLModel
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

    -- * PerformanceMetrics
    , PerformanceMetrics
    , performanceMetrics
    , pmProperties

    -- * Prediction
    , Prediction
    , prediction
    , pPredictedValue
    , pPredictedLabel
    , pPredictedScores
    , pDetails

    -- * RDSDataSpec
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

    -- * RDSDatabase
    , RDSDatabase
    , rdsDatabase
    , rdsdInstanceIdentifier
    , rdsdDatabaseName

    -- * RDSDatabaseCredentials
    , RDSDatabaseCredentials
    , rdsDatabaseCredentials
    , rdsdcUsername
    , rdsdcPassword

    -- * RDSMetadata
    , RDSMetadata
    , rdsMetadata
    , rmSelectSqlQuery
    , rmDataPipelineId
    , rmDatabase
    , rmDatabaseUserName
    , rmResourceRole
    , rmServiceRole

    -- * RealtimeEndpointInfo
    , RealtimeEndpointInfo
    , realtimeEndpointInfo
    , reiCreatedAt
    , reiEndpointURL
    , reiEndpointStatus
    , reiPeakRequestsPerSecond

    -- * RedshiftDataSpec
    , RedshiftDataSpec
    , redshiftDataSpec
    , rDataSchemaURI
    , rDataSchema
    , rDataRearrangement
    , rDatabaseInformation
    , rSelectSqlQuery
    , rDatabaseCredentials
    , rS3StagingLocation

    -- * RedshiftDatabase
    , RedshiftDatabase
    , redshiftDatabase
    , rdDatabaseName
    , rdClusterIdentifier

    -- * RedshiftDatabaseCredentials
    , RedshiftDatabaseCredentials
    , redshiftDatabaseCredentials
    , rdcUsername
    , rdcPassword

    -- * RedshiftMetadata
    , RedshiftMetadata
    , redshiftMetadata
    , redSelectSqlQuery
    , redRedshiftDatabase
    , redDatabaseUserName

    -- * S3DataSpec
    , S3DataSpec
    , s3DataSpec
    , sdsDataSchema
    , sdsDataSchemaLocationS3
    , sdsDataRearrangement
    , sdsDataLocationS3
    ) where

import           Network.AWS.MachineLearning.Types.Product
import           Network.AWS.MachineLearning.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2014-12-12@ of the Amazon Machine Learning SDK.
data MachineLearning

instance AWSService MachineLearning where
    type Sg MachineLearning = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "MachineLearning"
            , _svcPrefix = "machinelearning"
            , _svcVersion = "2014-12-12"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | An error on the server occurred when trying to process a request.
_InternalServerException :: AWSError a => Getting (First ServiceError) a ServiceError
_InternalServerException =
    _ServiceError . hasStatus 500 . hasCode "InternalServerException"

-- | An error on the client occurred. Typically, the cause is an invalid
-- input value.
_InvalidInputException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidInputException =
    _ServiceError . hasStatus 400 . hasCode "InvalidInputException"

-- | A second request to use or change an object was not allowed. This can
-- result from retrying a request using a parameter that was not present in
-- the original request.
_IdempotentParameterMismatchException :: AWSError a => Getting (First ServiceError) a ServiceError
_IdempotentParameterMismatchException =
    _ServiceError .
    hasStatus 400 . hasCode "IdempotentParameterMismatchException"

-- | The exception is thrown when a predict request is made to an unmounted
-- @MLModel@.
_PredictorNotMountedException :: AWSError a => Getting (First ServiceError) a ServiceError
_PredictorNotMountedException =
    _ServiceError . hasStatus 400 . hasCode "PredictorNotMountedException"

-- | A specified resource cannot be located.
_ResourceNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasStatus 404 . hasCode "ResourceNotFoundException"

-- | The subscriber exceeded the maximum number of operations. This exception
-- can occur when listing objects such as @DataSource@.
_LimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _ServiceError . hasStatus 417 . hasCode "LimitExceededException"
