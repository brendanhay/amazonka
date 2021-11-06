{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LookoutVision.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * DatasetStatus
    DatasetStatus (..),

    -- * ModelHostingStatus
    ModelHostingStatus (..),

    -- * ModelStatus
    ModelStatus (..),

    -- * DatasetDescription
    DatasetDescription (..),
    newDatasetDescription,
    datasetDescription_status,
    datasetDescription_imageStats,
    datasetDescription_statusMessage,
    datasetDescription_creationTimestamp,
    datasetDescription_datasetType,
    datasetDescription_projectName,
    datasetDescription_lastUpdatedTimestamp,

    -- * DatasetGroundTruthManifest
    DatasetGroundTruthManifest (..),
    newDatasetGroundTruthManifest,
    datasetGroundTruthManifest_s3Object,

    -- * DatasetImageStats
    DatasetImageStats (..),
    newDatasetImageStats,
    datasetImageStats_normal,
    datasetImageStats_anomaly,
    datasetImageStats_labeled,
    datasetImageStats_total,

    -- * DatasetMetadata
    DatasetMetadata (..),
    newDatasetMetadata,
    datasetMetadata_status,
    datasetMetadata_statusMessage,
    datasetMetadata_creationTimestamp,
    datasetMetadata_datasetType,

    -- * DatasetSource
    DatasetSource (..),
    newDatasetSource,
    datasetSource_groundTruthManifest,

    -- * DetectAnomalyResult
    DetectAnomalyResult (..),
    newDetectAnomalyResult,
    detectAnomalyResult_isAnomalous,
    detectAnomalyResult_confidence,
    detectAnomalyResult_source,

    -- * ImageSource
    ImageSource (..),
    newImageSource,
    imageSource_type,

    -- * InputS3Object
    InputS3Object (..),
    newInputS3Object,
    inputS3Object_versionId,
    inputS3Object_bucket,
    inputS3Object_key,

    -- * ModelDescription
    ModelDescription (..),
    newModelDescription,
    modelDescription_status,
    modelDescription_evaluationResult,
    modelDescription_evaluationEndTimestamp,
    modelDescription_modelArn,
    modelDescription_performance,
    modelDescription_kmsKeyId,
    modelDescription_statusMessage,
    modelDescription_creationTimestamp,
    modelDescription_outputConfig,
    modelDescription_modelVersion,
    modelDescription_description,
    modelDescription_evaluationManifest,

    -- * ModelMetadata
    ModelMetadata (..),
    newModelMetadata,
    modelMetadata_status,
    modelMetadata_modelArn,
    modelMetadata_performance,
    modelMetadata_statusMessage,
    modelMetadata_creationTimestamp,
    modelMetadata_modelVersion,
    modelMetadata_description,

    -- * ModelPerformance
    ModelPerformance (..),
    newModelPerformance,
    modelPerformance_recall,
    modelPerformance_precision,
    modelPerformance_f1Score,

    -- * OutputConfig
    OutputConfig (..),
    newOutputConfig,
    outputConfig_s3Location,

    -- * OutputS3Object
    OutputS3Object (..),
    newOutputS3Object,
    outputS3Object_bucket,
    outputS3Object_key,

    -- * ProjectDescription
    ProjectDescription (..),
    newProjectDescription,
    projectDescription_creationTimestamp,
    projectDescription_projectName,
    projectDescription_projectArn,
    projectDescription_datasets,

    -- * ProjectMetadata
    ProjectMetadata (..),
    newProjectMetadata,
    projectMetadata_creationTimestamp,
    projectMetadata_projectName,
    projectMetadata_projectArn,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_prefix,
    s3Location_bucket,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutVision.Types.DatasetDescription
import Amazonka.LookoutVision.Types.DatasetGroundTruthManifest
import Amazonka.LookoutVision.Types.DatasetImageStats
import Amazonka.LookoutVision.Types.DatasetMetadata
import Amazonka.LookoutVision.Types.DatasetSource
import Amazonka.LookoutVision.Types.DatasetStatus
import Amazonka.LookoutVision.Types.DetectAnomalyResult
import Amazonka.LookoutVision.Types.ImageSource
import Amazonka.LookoutVision.Types.InputS3Object
import Amazonka.LookoutVision.Types.ModelDescription
import Amazonka.LookoutVision.Types.ModelHostingStatus
import Amazonka.LookoutVision.Types.ModelMetadata
import Amazonka.LookoutVision.Types.ModelPerformance
import Amazonka.LookoutVision.Types.ModelStatus
import Amazonka.LookoutVision.Types.OutputConfig
import Amazonka.LookoutVision.Types.OutputS3Object
import Amazonka.LookoutVision.Types.ProjectDescription
import Amazonka.LookoutVision.Types.ProjectMetadata
import Amazonka.LookoutVision.Types.S3Location
import Amazonka.LookoutVision.Types.Tag
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-11-20@ of the Amazon Lookout for Vision SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "LookoutVision",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "lookoutvision",
      Core._serviceSigningName = "lookoutvision",
      Core._serviceVersion = "2020-11-20",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "LookoutVision",
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

-- | An input validation error occured. For example, invalid characters in a
-- project name, or if a pagination token is invalid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | You are not authorized to perform the action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The update or deletion of a resource caused an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | A service quota was exceeded the allowed limit. For more information,
-- see Limits in Amazon Lookout for Vision in the Amazon Lookout for Vision
-- Developer Guide.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Amazon Lookout for Vision is temporarily unable to process the request.
-- Try your call again.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Amazon Lookout for Vision experienced a service issue. Try your call
-- again.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
