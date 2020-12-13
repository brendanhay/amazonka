-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types
  ( -- * Service configuration
    machineLearningService,

    -- * Errors

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
    mkBatchPrediction,
    bpStatus,
    bpLastUpdatedAt,
    bpCreatedAt,
    bpComputeTime,
    bpInputDataLocationS3,
    bpMLModelId,
    bpBatchPredictionDataSourceId,
    bpTotalRecordCount,
    bpStartedAt,
    bpBatchPredictionId,
    bpFinishedAt,
    bpInvalidRecordCount,
    bpCreatedByIAMUser,
    bpName,
    bpMessage,
    bpOutputURI,

    -- * DataSource
    DataSource (..),
    mkDataSource,
    dsStatus,
    dsNumberOfFiles,
    dsLastUpdatedAt,
    dsCreatedAt,
    dsComputeTime,
    dsDataSourceId,
    dsRDSMetadata,
    dsDataSizeInBytes,
    dsStartedAt,
    dsFinishedAt,
    dsCreatedByIAMUser,
    dsName,
    dsDataLocationS3,
    dsComputeStatistics,
    dsMessage,
    dsRedshiftMetadata,
    dsDataRearrangement,
    dsRoleARN,

    -- * Evaluation
    Evaluation (..),
    mkEvaluation,
    eStatus,
    ePerformanceMetrics,
    eLastUpdatedAt,
    eCreatedAt,
    eComputeTime,
    eInputDataLocationS3,
    eMLModelId,
    eStartedAt,
    eFinishedAt,
    eCreatedByIAMUser,
    eName,
    eEvaluationId,
    eMessage,
    eEvaluationDataSourceId,

    -- * MLModel
    MLModel (..),
    mkMLModel,
    mlmStatus,
    mlmLastUpdatedAt,
    mlmTrainingParameters,
    mlmScoreThresholdLastUpdatedAt,
    mlmCreatedAt,
    mlmComputeTime,
    mlmInputDataLocationS3,
    mlmMLModelId,
    mlmSizeInBytes,
    mlmStartedAt,
    mlmScoreThreshold,
    mlmFinishedAt,
    mlmAlgorithm,
    mlmCreatedByIAMUser,
    mlmName,
    mlmEndpointInfo,
    mlmTrainingDataSourceId,
    mlmMessage,
    mlmMLModelType,

    -- * PerformanceMetrics
    PerformanceMetrics (..),
    mkPerformanceMetrics,
    pmProperties,

    -- * Prediction
    Prediction (..),
    mkPrediction,
    pPredictedValue,
    pPredictedLabel,
    pPredictedScores,
    pDetails,

    -- * RDSDataSpec
    RDSDataSpec (..),
    mkRDSDataSpec,
    rdsS3StagingLocation,
    rdsSelectSqlQuery,
    rdsDataSchemaURI,
    rdsSecurityGroupIds,
    rdsSubnetId,
    rdsDataSchema,
    rdsDatabaseInformation,
    rdsDatabaseCredentials,
    rdsResourceRole,
    rdsDataRearrangement,
    rdsServiceRole,

    -- * RDSDatabase
    RDSDatabase (..),
    mkRDSDatabase,
    rdInstanceIdentifier,
    rdDatabaseName,

    -- * RDSDatabaseCredentials
    RDSDatabaseCredentials (..),
    mkRDSDatabaseCredentials,
    rdcUsername,
    rdcPassword,

    -- * RDSMetadata
    RDSMetadata (..),
    mkRDSMetadata,
    rmSelectSqlQuery,
    rmDataPipelineId,
    rmDatabase,
    rmDatabaseUserName,
    rmResourceRole,
    rmServiceRole,

    -- * RealtimeEndpointInfo
    RealtimeEndpointInfo (..),
    mkRealtimeEndpointInfo,
    reiCreatedAt,
    reiEndpointURL,
    reiEndpointStatus,
    reiPeakRequestsPerSecond,

    -- * RedshiftDataSpec
    RedshiftDataSpec (..),
    mkRedshiftDataSpec,
    rdsfS3StagingLocation,
    rdsfSelectSqlQuery,
    rdsfDataSchemaURI,
    rdsfDataSchema,
    rdsfDatabaseInformation,
    rdsfDatabaseCredentials,
    rdsfDataRearrangement,

    -- * RedshiftDatabase
    RedshiftDatabase (..),
    mkRedshiftDatabase,
    rClusterIdentifier,
    rDatabaseName,

    -- * RedshiftDatabaseCredentials
    RedshiftDatabaseCredentials (..),
    mkRedshiftDatabaseCredentials,
    rUsername,
    rPassword,

    -- * RedshiftMetadata
    RedshiftMetadata (..),
    mkRedshiftMetadata,
    rSelectSqlQuery,
    rRedshiftDatabase,
    rDatabaseUserName,

    -- * S3DataSpec
    S3DataSpec (..),
    mkS3DataSpec,
    sdsDataSchema,
    sdsDataSchemaLocationS3,
    sdsDataLocationS3,
    sdsDataRearrangement,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,
  )
where

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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-12-12@ of the Amazon Machine Learning SDK configuration.
machineLearningService :: Lude.Service
machineLearningService =
  Lude.Service
    { Lude._svcAbbrev = "MachineLearning",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "machinelearning",
      Lude._svcVersion = "2014-12-12",
      Lude._svcEndpoint = Lude.defaultEndpoint machineLearningService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "MachineLearning",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
