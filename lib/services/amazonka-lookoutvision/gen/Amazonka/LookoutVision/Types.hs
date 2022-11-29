{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LookoutVision.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ConflictException,
    _ThrottlingException,
    _ValidationException,

    -- * DatasetStatus
    DatasetStatus (..),

    -- * ModelHostingStatus
    ModelHostingStatus (..),

    -- * ModelPackagingJobStatus
    ModelPackagingJobStatus (..),

    -- * ModelStatus
    ModelStatus (..),

    -- * TargetDevice
    TargetDevice (..),

    -- * TargetPlatformAccelerator
    TargetPlatformAccelerator (..),

    -- * TargetPlatformArch
    TargetPlatformArch (..),

    -- * TargetPlatformOs
    TargetPlatformOs (..),

    -- * Anomaly
    Anomaly (..),
    newAnomaly,
    anomaly_name,
    anomaly_pixelAnomaly,

    -- * DatasetDescription
    DatasetDescription (..),
    newDatasetDescription,
    datasetDescription_lastUpdatedTimestamp,
    datasetDescription_datasetType,
    datasetDescription_status,
    datasetDescription_creationTimestamp,
    datasetDescription_projectName,
    datasetDescription_statusMessage,
    datasetDescription_imageStats,

    -- * DatasetGroundTruthManifest
    DatasetGroundTruthManifest (..),
    newDatasetGroundTruthManifest,
    datasetGroundTruthManifest_s3Object,

    -- * DatasetImageStats
    DatasetImageStats (..),
    newDatasetImageStats,
    datasetImageStats_total,
    datasetImageStats_labeled,
    datasetImageStats_anomaly,
    datasetImageStats_normal,

    -- * DatasetMetadata
    DatasetMetadata (..),
    newDatasetMetadata,
    datasetMetadata_datasetType,
    datasetMetadata_status,
    datasetMetadata_creationTimestamp,
    datasetMetadata_statusMessage,

    -- * DatasetSource
    DatasetSource (..),
    newDatasetSource,
    datasetSource_groundTruthManifest,

    -- * DetectAnomalyResult
    DetectAnomalyResult (..),
    newDetectAnomalyResult,
    detectAnomalyResult_anomalies,
    detectAnomalyResult_confidence,
    detectAnomalyResult_source,
    detectAnomalyResult_isAnomalous,
    detectAnomalyResult_anomalyMask,

    -- * GreengrassConfiguration
    GreengrassConfiguration (..),
    newGreengrassConfiguration,
    greengrassConfiguration_componentDescription,
    greengrassConfiguration_tags,
    greengrassConfiguration_componentVersion,
    greengrassConfiguration_targetDevice,
    greengrassConfiguration_targetPlatform,
    greengrassConfiguration_compilerOptions,
    greengrassConfiguration_s3OutputLocation,
    greengrassConfiguration_componentName,

    -- * GreengrassOutputDetails
    GreengrassOutputDetails (..),
    newGreengrassOutputDetails,
    greengrassOutputDetails_componentVersion,
    greengrassOutputDetails_componentName,
    greengrassOutputDetails_componentVersionArn,

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
    modelDescription_evaluationManifest,
    modelDescription_minInferenceUnits,
    modelDescription_modelVersion,
    modelDescription_evaluationEndTimestamp,
    modelDescription_status,
    modelDescription_description,
    modelDescription_creationTimestamp,
    modelDescription_performance,
    modelDescription_modelArn,
    modelDescription_evaluationResult,
    modelDescription_kmsKeyId,
    modelDescription_maxInferenceUnits,
    modelDescription_statusMessage,
    modelDescription_outputConfig,

    -- * ModelMetadata
    ModelMetadata (..),
    newModelMetadata,
    modelMetadata_modelVersion,
    modelMetadata_status,
    modelMetadata_description,
    modelMetadata_creationTimestamp,
    modelMetadata_performance,
    modelMetadata_modelArn,
    modelMetadata_statusMessage,

    -- * ModelPackagingConfiguration
    ModelPackagingConfiguration (..),
    newModelPackagingConfiguration,
    modelPackagingConfiguration_greengrass,

    -- * ModelPackagingDescription
    ModelPackagingDescription (..),
    newModelPackagingDescription,
    modelPackagingDescription_lastUpdatedTimestamp,
    modelPackagingDescription_jobName,
    modelPackagingDescription_modelPackagingOutputDetails,
    modelPackagingDescription_modelVersion,
    modelPackagingDescription_status,
    modelPackagingDescription_creationTimestamp,
    modelPackagingDescription_modelPackagingJobDescription,
    modelPackagingDescription_modelPackagingMethod,
    modelPackagingDescription_projectName,
    modelPackagingDescription_statusMessage,
    modelPackagingDescription_modelPackagingConfiguration,

    -- * ModelPackagingJobMetadata
    ModelPackagingJobMetadata (..),
    newModelPackagingJobMetadata,
    modelPackagingJobMetadata_lastUpdatedTimestamp,
    modelPackagingJobMetadata_jobName,
    modelPackagingJobMetadata_modelVersion,
    modelPackagingJobMetadata_status,
    modelPackagingJobMetadata_creationTimestamp,
    modelPackagingJobMetadata_modelPackagingJobDescription,
    modelPackagingJobMetadata_modelPackagingMethod,
    modelPackagingJobMetadata_projectName,
    modelPackagingJobMetadata_statusMessage,

    -- * ModelPackagingOutputDetails
    ModelPackagingOutputDetails (..),
    newModelPackagingOutputDetails,
    modelPackagingOutputDetails_greengrass,

    -- * ModelPerformance
    ModelPerformance (..),
    newModelPerformance,
    modelPerformance_f1Score,
    modelPerformance_recall,
    modelPerformance_precision,

    -- * OutputConfig
    OutputConfig (..),
    newOutputConfig,
    outputConfig_s3Location,

    -- * OutputS3Object
    OutputS3Object (..),
    newOutputS3Object,
    outputS3Object_bucket,
    outputS3Object_key,

    -- * PixelAnomaly
    PixelAnomaly (..),
    newPixelAnomaly,
    pixelAnomaly_color,
    pixelAnomaly_totalPercentageArea,

    -- * ProjectDescription
    ProjectDescription (..),
    newProjectDescription,
    projectDescription_datasets,
    projectDescription_creationTimestamp,
    projectDescription_projectName,
    projectDescription_projectArn,

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

    -- * TargetPlatform
    TargetPlatform (..),
    newTargetPlatform,
    targetPlatform_accelerator,
    targetPlatform_os,
    targetPlatform_arch,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutVision.Types.Anomaly
import Amazonka.LookoutVision.Types.DatasetDescription
import Amazonka.LookoutVision.Types.DatasetGroundTruthManifest
import Amazonka.LookoutVision.Types.DatasetImageStats
import Amazonka.LookoutVision.Types.DatasetMetadata
import Amazonka.LookoutVision.Types.DatasetSource
import Amazonka.LookoutVision.Types.DatasetStatus
import Amazonka.LookoutVision.Types.DetectAnomalyResult
import Amazonka.LookoutVision.Types.GreengrassConfiguration
import Amazonka.LookoutVision.Types.GreengrassOutputDetails
import Amazonka.LookoutVision.Types.ImageSource
import Amazonka.LookoutVision.Types.InputS3Object
import Amazonka.LookoutVision.Types.ModelDescription
import Amazonka.LookoutVision.Types.ModelHostingStatus
import Amazonka.LookoutVision.Types.ModelMetadata
import Amazonka.LookoutVision.Types.ModelPackagingConfiguration
import Amazonka.LookoutVision.Types.ModelPackagingDescription
import Amazonka.LookoutVision.Types.ModelPackagingJobMetadata
import Amazonka.LookoutVision.Types.ModelPackagingJobStatus
import Amazonka.LookoutVision.Types.ModelPackagingOutputDetails
import Amazonka.LookoutVision.Types.ModelPerformance
import Amazonka.LookoutVision.Types.ModelStatus
import Amazonka.LookoutVision.Types.OutputConfig
import Amazonka.LookoutVision.Types.OutputS3Object
import Amazonka.LookoutVision.Types.PixelAnomaly
import Amazonka.LookoutVision.Types.ProjectDescription
import Amazonka.LookoutVision.Types.ProjectMetadata
import Amazonka.LookoutVision.Types.S3Location
import Amazonka.LookoutVision.Types.Tag
import Amazonka.LookoutVision.Types.TargetDevice
import Amazonka.LookoutVision.Types.TargetPlatform
import Amazonka.LookoutVision.Types.TargetPlatformAccelerator
import Amazonka.LookoutVision.Types.TargetPlatformArch
import Amazonka.LookoutVision.Types.TargetPlatformOs
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-11-20@ of the Amazon Lookout for Vision SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "LookoutVision",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "lookoutvision",
      Core.signingName = "lookoutvision",
      Core.version = "2020-11-20",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "LookoutVision",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | You are not authorized to perform the action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Amazon Lookout for Vision experienced a service issue. Try your call
-- again.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | A service quota was exceeded the allowed limit. For more information,
-- see Limits in Amazon Lookout for Vision in the Amazon Lookout for Vision
-- Developer Guide.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The update or deletion of a resource caused an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Amazon Lookout for Vision is temporarily unable to process the request.
-- Try your call again.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | An input validation error occured. For example, invalid characters in a
-- project name, or if a pagination token is invalid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
