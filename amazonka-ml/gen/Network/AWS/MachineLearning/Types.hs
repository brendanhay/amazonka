{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _PredictorNotMountedException,
    _TagLimitExceededException,
    _InvalidInputException,
    _InvalidTagException,
    _LimitExceededException,
    _ResourceNotFoundException,
    _IdempotentParameterMismatchException,
    _InternalServerException,

    -- * Algorithm
    Algorithm (..),

    -- * BatchPredictionFilterVariable
    BatchPredictionFilterVariable (..),

    -- * DataSourceFilterVariable
    DataSourceFilterVariable (..),

    -- * DetailsAttributes
    DetailsAttributes (..),

    -- * EntityStatus
    EntityStatus (..),

    -- * EvaluationFilterVariable
    EvaluationFilterVariable (..),

    -- * MLModelFilterVariable
    MLModelFilterVariable (..),

    -- * MLModelType
    MLModelType (..),

    -- * RealtimeEndpointStatus
    RealtimeEndpointStatus (..),

    -- * SortOrder
    SortOrder (..),

    -- * TaggableResourceType
    TaggableResourceType (..),

    -- * BatchPrediction
    BatchPrediction (..),
    newBatchPrediction,
    batchPrediction_batchPredictionId,
    batchPrediction_status,
    batchPrediction_startedAt,
    batchPrediction_outputUri,
    batchPrediction_message,
    batchPrediction_createdAt,
    batchPrediction_finishedAt,
    batchPrediction_createdByIamUser,
    batchPrediction_name,
    batchPrediction_invalidRecordCount,
    batchPrediction_totalRecordCount,
    batchPrediction_batchPredictionDataSourceId,
    batchPrediction_mLModelId,
    batchPrediction_inputDataLocationS3,
    batchPrediction_computeTime,
    batchPrediction_lastUpdatedAt,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_status,
    dataSource_startedAt,
    dataSource_dataRearrangement,
    dataSource_roleARN,
    dataSource_redshiftMetadata,
    dataSource_message,
    dataSource_dataSourceId,
    dataSource_computeStatistics,
    dataSource_dataLocationS3,
    dataSource_createdAt,
    dataSource_numberOfFiles,
    dataSource_finishedAt,
    dataSource_createdByIamUser,
    dataSource_name,
    dataSource_dataSizeInBytes,
    dataSource_computeTime,
    dataSource_rDSMetadata,
    dataSource_lastUpdatedAt,

    -- * Evaluation
    Evaluation (..),
    newEvaluation,
    evaluation_performanceMetrics,
    evaluation_status,
    evaluation_startedAt,
    evaluation_evaluationDataSourceId,
    evaluation_message,
    evaluation_createdAt,
    evaluation_finishedAt,
    evaluation_createdByIamUser,
    evaluation_name,
    evaluation_evaluationId,
    evaluation_mLModelId,
    evaluation_inputDataLocationS3,
    evaluation_computeTime,
    evaluation_lastUpdatedAt,

    -- * MLModel
    MLModel (..),
    newMLModel,
    mLModel_algorithm,
    mLModel_status,
    mLModel_startedAt,
    mLModel_message,
    mLModel_endpointInfo,
    mLModel_scoreThresholdLastUpdatedAt,
    mLModel_createdAt,
    mLModel_trainingParameters,
    mLModel_finishedAt,
    mLModel_scoreThreshold,
    mLModel_createdByIamUser,
    mLModel_name,
    mLModel_mLModelType,
    mLModel_mLModelId,
    mLModel_sizeInBytes,
    mLModel_inputDataLocationS3,
    mLModel_computeTime,
    mLModel_trainingDataSourceId,
    mLModel_lastUpdatedAt,

    -- * PerformanceMetrics
    PerformanceMetrics (..),
    newPerformanceMetrics,
    performanceMetrics_properties,

    -- * Prediction
    Prediction (..),
    newPrediction,
    prediction_predictedValue,
    prediction_predictedScores,
    prediction_predictedLabel,
    prediction_details,

    -- * RDSDataSpec
    RDSDataSpec (..),
    newRDSDataSpec,
    rDSDataSpec_dataRearrangement,
    rDSDataSpec_dataSchema,
    rDSDataSpec_dataSchemaUri,
    rDSDataSpec_databaseInformation,
    rDSDataSpec_selectSqlQuery,
    rDSDataSpec_databaseCredentials,
    rDSDataSpec_s3StagingLocation,
    rDSDataSpec_resourceRole,
    rDSDataSpec_serviceRole,
    rDSDataSpec_subnetId,
    rDSDataSpec_securityGroupIds,

    -- * RDSDatabase
    RDSDatabase (..),
    newRDSDatabase,
    rDSDatabase_instanceIdentifier,
    rDSDatabase_databaseName,

    -- * RDSDatabaseCredentials
    RDSDatabaseCredentials (..),
    newRDSDatabaseCredentials,
    rDSDatabaseCredentials_username,
    rDSDatabaseCredentials_password,

    -- * RDSMetadata
    RDSMetadata (..),
    newRDSMetadata,
    rDSMetadata_dataPipelineId,
    rDSMetadata_selectSqlQuery,
    rDSMetadata_serviceRole,
    rDSMetadata_resourceRole,
    rDSMetadata_databaseUserName,
    rDSMetadata_database,

    -- * RealtimeEndpointInfo
    RealtimeEndpointInfo (..),
    newRealtimeEndpointInfo,
    realtimeEndpointInfo_createdAt,
    realtimeEndpointInfo_peakRequestsPerSecond,
    realtimeEndpointInfo_endpointStatus,
    realtimeEndpointInfo_endpointUrl,

    -- * RedshiftDataSpec
    RedshiftDataSpec (..),
    newRedshiftDataSpec,
    redshiftDataSpec_dataRearrangement,
    redshiftDataSpec_dataSchema,
    redshiftDataSpec_dataSchemaUri,
    redshiftDataSpec_databaseInformation,
    redshiftDataSpec_selectSqlQuery,
    redshiftDataSpec_databaseCredentials,
    redshiftDataSpec_s3StagingLocation,

    -- * RedshiftDatabase
    RedshiftDatabase (..),
    newRedshiftDatabase,
    redshiftDatabase_databaseName,
    redshiftDatabase_clusterIdentifier,

    -- * RedshiftDatabaseCredentials
    RedshiftDatabaseCredentials (..),
    newRedshiftDatabaseCredentials,
    redshiftDatabaseCredentials_username,
    redshiftDatabaseCredentials_password,

    -- * RedshiftMetadata
    RedshiftMetadata (..),
    newRedshiftMetadata,
    redshiftMetadata_selectSqlQuery,
    redshiftMetadata_redshiftDatabase,
    redshiftMetadata_databaseUserName,

    -- * S3DataSpec
    S3DataSpec (..),
    newS3DataSpec,
    s3DataSpec_dataRearrangement,
    s3DataSpec_dataSchema,
    s3DataSpec_dataSchemaLocationS3,
    s3DataSpec_dataLocationS3,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.Algorithm
import Network.AWS.MachineLearning.Types.BatchPrediction
import Network.AWS.MachineLearning.Types.BatchPredictionFilterVariable
import Network.AWS.MachineLearning.Types.DataSource
import Network.AWS.MachineLearning.Types.DataSourceFilterVariable
import Network.AWS.MachineLearning.Types.DetailsAttributes
import Network.AWS.MachineLearning.Types.EntityStatus
import Network.AWS.MachineLearning.Types.Evaluation
import Network.AWS.MachineLearning.Types.EvaluationFilterVariable
import Network.AWS.MachineLearning.Types.MLModel
import Network.AWS.MachineLearning.Types.MLModelFilterVariable
import Network.AWS.MachineLearning.Types.MLModelType
import Network.AWS.MachineLearning.Types.PerformanceMetrics
import Network.AWS.MachineLearning.Types.Prediction
import Network.AWS.MachineLearning.Types.RDSDataSpec
import Network.AWS.MachineLearning.Types.RDSDatabase
import Network.AWS.MachineLearning.Types.RDSDatabaseCredentials
import Network.AWS.MachineLearning.Types.RDSMetadata
import Network.AWS.MachineLearning.Types.RealtimeEndpointInfo
import Network.AWS.MachineLearning.Types.RealtimeEndpointStatus
import Network.AWS.MachineLearning.Types.RedshiftDataSpec
import Network.AWS.MachineLearning.Types.RedshiftDatabase
import Network.AWS.MachineLearning.Types.RedshiftDatabaseCredentials
import Network.AWS.MachineLearning.Types.RedshiftMetadata
import Network.AWS.MachineLearning.Types.S3DataSpec
import Network.AWS.MachineLearning.Types.SortOrder
import Network.AWS.MachineLearning.Types.Tag
import Network.AWS.MachineLearning.Types.TaggableResourceType
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-12-12@ of the Amazon Machine Learning SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "MachineLearning",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "machinelearning",
      Core._serviceSigningName = "machinelearning",
      Core._serviceVersion = "2014-12-12",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "MachineLearning",
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
      | Prelude.otherwise = Prelude.Nothing

-- | The exception is thrown when a predict request is made to an unmounted
-- @MLModel@.
_PredictorNotMountedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PredictorNotMountedException =
  Core._MatchServiceError
    defaultService
    "PredictorNotMountedException"
    Prelude.. Core.hasStatus 400

-- | Prism for TagLimitExceededException' errors.
_TagLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TagLimitExceededException"

-- | An error on the client occurred. Typically, the cause is an invalid
-- input value.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"
    Prelude.. Core.hasStatus 400

-- | Prism for InvalidTagException' errors.
_InvalidTagException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagException =
  Core._MatchServiceError
    defaultService
    "InvalidTagException"

-- | The subscriber exceeded the maximum number of operations. This exception
-- can occur when listing objects such as @DataSource@.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 417

-- | A specified resource cannot be located.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | A second request to use or change an object was not allowed. This can
-- result from retrying a request using a parameter that was not present in
-- the original request.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"
    Prelude.. Core.hasStatus 400

-- | An error on the server occurred when trying to process a request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500
