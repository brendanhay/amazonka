{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MachineLearning.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidTagException,
    _InternalServerException,
    _InvalidInputException,
    _IdempotentParameterMismatchException,
    _TagLimitExceededException,
    _PredictorNotMountedException,
    _ResourceNotFoundException,
    _LimitExceededException,

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
    batchPrediction_status,
    batchPrediction_lastUpdatedAt,
    batchPrediction_createdAt,
    batchPrediction_computeTime,
    batchPrediction_inputDataLocationS3,
    batchPrediction_mLModelId,
    batchPrediction_batchPredictionDataSourceId,
    batchPrediction_totalRecordCount,
    batchPrediction_startedAt,
    batchPrediction_batchPredictionId,
    batchPrediction_finishedAt,
    batchPrediction_invalidRecordCount,
    batchPrediction_createdByIamUser,
    batchPrediction_name,
    batchPrediction_message,
    batchPrediction_outputUri,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_status,
    dataSource_numberOfFiles,
    dataSource_lastUpdatedAt,
    dataSource_createdAt,
    dataSource_computeTime,
    dataSource_dataSourceId,
    dataSource_rDSMetadata,
    dataSource_dataSizeInBytes,
    dataSource_startedAt,
    dataSource_finishedAt,
    dataSource_createdByIamUser,
    dataSource_name,
    dataSource_dataLocationS3,
    dataSource_computeStatistics,
    dataSource_message,
    dataSource_redshiftMetadata,
    dataSource_dataRearrangement,
    dataSource_roleARN,

    -- * Evaluation
    Evaluation (..),
    newEvaluation,
    evaluation_status,
    evaluation_performanceMetrics,
    evaluation_lastUpdatedAt,
    evaluation_createdAt,
    evaluation_computeTime,
    evaluation_inputDataLocationS3,
    evaluation_mLModelId,
    evaluation_startedAt,
    evaluation_finishedAt,
    evaluation_createdByIamUser,
    evaluation_name,
    evaluation_evaluationId,
    evaluation_message,
    evaluation_evaluationDataSourceId,

    -- * MLModel
    MLModel (..),
    newMLModel,
    mLModel_status,
    mLModel_lastUpdatedAt,
    mLModel_trainingParameters,
    mLModel_scoreThresholdLastUpdatedAt,
    mLModel_createdAt,
    mLModel_computeTime,
    mLModel_inputDataLocationS3,
    mLModel_mLModelId,
    mLModel_sizeInBytes,
    mLModel_startedAt,
    mLModel_scoreThreshold,
    mLModel_finishedAt,
    mLModel_algorithm,
    mLModel_createdByIamUser,
    mLModel_name,
    mLModel_endpointInfo,
    mLModel_trainingDataSourceId,
    mLModel_message,
    mLModel_mLModelType,

    -- * PerformanceMetrics
    PerformanceMetrics (..),
    newPerformanceMetrics,
    performanceMetrics_properties,

    -- * Prediction
    Prediction (..),
    newPrediction,
    prediction_predictedValue,
    prediction_predictedLabel,
    prediction_predictedScores,
    prediction_details,

    -- * RDSDataSpec
    RDSDataSpec (..),
    newRDSDataSpec,
    rDSDataSpec_dataSchemaUri,
    rDSDataSpec_dataSchema,
    rDSDataSpec_dataRearrangement,
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
    rDSMetadata_selectSqlQuery,
    rDSMetadata_dataPipelineId,
    rDSMetadata_database,
    rDSMetadata_databaseUserName,
    rDSMetadata_resourceRole,
    rDSMetadata_serviceRole,

    -- * RealtimeEndpointInfo
    RealtimeEndpointInfo (..),
    newRealtimeEndpointInfo,
    realtimeEndpointInfo_createdAt,
    realtimeEndpointInfo_endpointUrl,
    realtimeEndpointInfo_endpointStatus,
    realtimeEndpointInfo_peakRequestsPerSecond,

    -- * RedshiftDataSpec
    RedshiftDataSpec (..),
    newRedshiftDataSpec,
    redshiftDataSpec_dataSchemaUri,
    redshiftDataSpec_dataSchema,
    redshiftDataSpec_dataRearrangement,
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
    s3DataSpec_dataSchema,
    s3DataSpec_dataSchemaLocationS3,
    s3DataSpec_dataRearrangement,
    s3DataSpec_dataLocationS3,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MachineLearning.Types.Algorithm
import Amazonka.MachineLearning.Types.BatchPrediction
import Amazonka.MachineLearning.Types.BatchPredictionFilterVariable
import Amazonka.MachineLearning.Types.DataSource
import Amazonka.MachineLearning.Types.DataSourceFilterVariable
import Amazonka.MachineLearning.Types.DetailsAttributes
import Amazonka.MachineLearning.Types.EntityStatus
import Amazonka.MachineLearning.Types.Evaluation
import Amazonka.MachineLearning.Types.EvaluationFilterVariable
import Amazonka.MachineLearning.Types.MLModel
import Amazonka.MachineLearning.Types.MLModelFilterVariable
import Amazonka.MachineLearning.Types.MLModelType
import Amazonka.MachineLearning.Types.PerformanceMetrics
import Amazonka.MachineLearning.Types.Prediction
import Amazonka.MachineLearning.Types.RDSDataSpec
import Amazonka.MachineLearning.Types.RDSDatabase
import Amazonka.MachineLearning.Types.RDSDatabaseCredentials
import Amazonka.MachineLearning.Types.RDSMetadata
import Amazonka.MachineLearning.Types.RealtimeEndpointInfo
import Amazonka.MachineLearning.Types.RealtimeEndpointStatus
import Amazonka.MachineLearning.Types.RedshiftDataSpec
import Amazonka.MachineLearning.Types.RedshiftDatabase
import Amazonka.MachineLearning.Types.RedshiftDatabaseCredentials
import Amazonka.MachineLearning.Types.RedshiftMetadata
import Amazonka.MachineLearning.Types.S3DataSpec
import Amazonka.MachineLearning.Types.SortOrder
import Amazonka.MachineLearning.Types.Tag
import Amazonka.MachineLearning.Types.TaggableResourceType
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

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

-- | Prism for InvalidTagException' errors.
_InvalidTagException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagException =
  Core._MatchServiceError
    defaultService
    "InvalidTagException"

-- | An error on the server occurred when trying to process a request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | An error on the client occurred. Typically, the cause is an invalid
-- input value.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | A second request to use or change an object was not allowed. This can
-- result from retrying a request using a parameter that was not present in
-- the original request.
_IdempotentParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"

-- | Prism for TagLimitExceededException' errors.
_TagLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TagLimitExceededException"

-- | The exception is thrown when a predict request is made to an unmounted
-- @MLModel@.
_PredictorNotMountedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PredictorNotMountedException =
  Core._MatchServiceError
    defaultService
    "PredictorNotMountedException"

-- | A specified resource cannot be located.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The subscriber exceeded the maximum number of operations. This exception
-- can occur when listing objects such as @DataSource@.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
