{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MachineLearning.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _IdempotentParameterMismatchException,
    _InternalServerException,
    _InvalidInputException,
    _InvalidTagException,
    _LimitExceededException,
    _PredictorNotMountedException,
    _ResourceNotFoundException,
    _TagLimitExceededException,

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
    batchPrediction_batchPredictionDataSourceId,
    batchPrediction_batchPredictionId,
    batchPrediction_computeTime,
    batchPrediction_createdAt,
    batchPrediction_createdByIamUser,
    batchPrediction_finishedAt,
    batchPrediction_inputDataLocationS3,
    batchPrediction_invalidRecordCount,
    batchPrediction_lastUpdatedAt,
    batchPrediction_mLModelId,
    batchPrediction_message,
    batchPrediction_name,
    batchPrediction_outputUri,
    batchPrediction_startedAt,
    batchPrediction_status,
    batchPrediction_totalRecordCount,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_computeStatistics,
    dataSource_computeTime,
    dataSource_createdAt,
    dataSource_createdByIamUser,
    dataSource_dataLocationS3,
    dataSource_dataRearrangement,
    dataSource_dataSizeInBytes,
    dataSource_dataSourceId,
    dataSource_finishedAt,
    dataSource_lastUpdatedAt,
    dataSource_message,
    dataSource_name,
    dataSource_numberOfFiles,
    dataSource_rDSMetadata,
    dataSource_redshiftMetadata,
    dataSource_roleARN,
    dataSource_startedAt,
    dataSource_status,

    -- * Evaluation
    Evaluation (..),
    newEvaluation,
    evaluation_computeTime,
    evaluation_createdAt,
    evaluation_createdByIamUser,
    evaluation_evaluationDataSourceId,
    evaluation_evaluationId,
    evaluation_finishedAt,
    evaluation_inputDataLocationS3,
    evaluation_lastUpdatedAt,
    evaluation_mLModelId,
    evaluation_message,
    evaluation_name,
    evaluation_performanceMetrics,
    evaluation_startedAt,
    evaluation_status,

    -- * MLModel
    MLModel (..),
    newMLModel,
    mLModel_algorithm,
    mLModel_computeTime,
    mLModel_createdAt,
    mLModel_createdByIamUser,
    mLModel_endpointInfo,
    mLModel_finishedAt,
    mLModel_inputDataLocationS3,
    mLModel_lastUpdatedAt,
    mLModel_mLModelId,
    mLModel_mLModelType,
    mLModel_message,
    mLModel_name,
    mLModel_scoreThreshold,
    mLModel_scoreThresholdLastUpdatedAt,
    mLModel_sizeInBytes,
    mLModel_startedAt,
    mLModel_status,
    mLModel_trainingDataSourceId,
    mLModel_trainingParameters,

    -- * PerformanceMetrics
    PerformanceMetrics (..),
    newPerformanceMetrics,
    performanceMetrics_properties,

    -- * Prediction
    Prediction (..),
    newPrediction,
    prediction_details,
    prediction_predictedLabel,
    prediction_predictedScores,
    prediction_predictedValue,

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
    rDSMetadata_database,
    rDSMetadata_databaseUserName,
    rDSMetadata_resourceRole,
    rDSMetadata_selectSqlQuery,
    rDSMetadata_serviceRole,

    -- * RealtimeEndpointInfo
    RealtimeEndpointInfo (..),
    newRealtimeEndpointInfo,
    realtimeEndpointInfo_createdAt,
    realtimeEndpointInfo_endpointStatus,
    realtimeEndpointInfo_endpointUrl,
    realtimeEndpointInfo_peakRequestsPerSecond,

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
    redshiftMetadata_databaseUserName,
    redshiftMetadata_redshiftDatabase,
    redshiftMetadata_selectSqlQuery,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
    { Core.abbrev = "MachineLearning",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "machinelearning",
      Core.signingName = "machinelearning",
      Core.version = "2014-12-12",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "MachineLearning",
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

-- | A second request to use or change an object was not allowed. This can
-- result from retrying a request using a parameter that was not present in
-- the original request.
_IdempotentParameterMismatchException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IdempotentParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotentParameterMismatchException"

-- | An error on the server occurred when trying to process a request.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | An error on the client occurred. Typically, the cause is an invalid
-- input value.
_InvalidInputException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | Prism for InvalidTagException' errors.
_InvalidTagException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTagException =
  Core._MatchServiceError
    defaultService
    "InvalidTagException"

-- | The subscriber exceeded the maximum number of operations. This exception
-- can occur when listing objects such as @DataSource@.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The exception is thrown when a predict request is made to an unmounted
-- @MLModel@.
_PredictorNotMountedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PredictorNotMountedException =
  Core._MatchServiceError
    defaultService
    "PredictorNotMountedException"

-- | A specified resource cannot be located.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Prism for TagLimitExceededException' errors.
_TagLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TagLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TagLimitExceededException"
