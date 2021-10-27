{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LookoutEquipment.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LookoutEquipment.Types
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

    -- * DataUploadFrequency
    DataUploadFrequency (..),

    -- * DatasetStatus
    DatasetStatus (..),

    -- * InferenceExecutionStatus
    InferenceExecutionStatus (..),

    -- * InferenceSchedulerStatus
    InferenceSchedulerStatus (..),

    -- * IngestionJobStatus
    IngestionJobStatus (..),

    -- * ModelStatus
    ModelStatus (..),

    -- * TargetSamplingRate
    TargetSamplingRate (..),

    -- * DataIngestionJobSummary
    DataIngestionJobSummary (..),
    newDataIngestionJobSummary,
    dataIngestionJobSummary_ingestionInputConfiguration,
    dataIngestionJobSummary_status,
    dataIngestionJobSummary_datasetArn,
    dataIngestionJobSummary_jobId,
    dataIngestionJobSummary_datasetName,

    -- * DataPreProcessingConfiguration
    DataPreProcessingConfiguration (..),
    newDataPreProcessingConfiguration,
    dataPreProcessingConfiguration_targetSamplingRate,

    -- * DatasetSchema
    DatasetSchema (..),
    newDatasetSchema,
    datasetSchema_inlineDataSchema,

    -- * DatasetSummary
    DatasetSummary (..),
    newDatasetSummary,
    datasetSummary_status,
    datasetSummary_datasetArn,
    datasetSummary_createdAt,
    datasetSummary_datasetName,

    -- * InferenceExecutionSummary
    InferenceExecutionSummary (..),
    newInferenceExecutionSummary,
    inferenceExecutionSummary_status,
    inferenceExecutionSummary_failedReason,
    inferenceExecutionSummary_modelArn,
    inferenceExecutionSummary_dataStartTime,
    inferenceExecutionSummary_modelName,
    inferenceExecutionSummary_customerResultObject,
    inferenceExecutionSummary_inferenceSchedulerArn,
    inferenceExecutionSummary_scheduledStartTime,
    inferenceExecutionSummary_dataOutputConfiguration,
    inferenceExecutionSummary_dataEndTime,
    inferenceExecutionSummary_inferenceSchedulerName,
    inferenceExecutionSummary_dataInputConfiguration,

    -- * InferenceInputConfiguration
    InferenceInputConfiguration (..),
    newInferenceInputConfiguration,
    inferenceInputConfiguration_inputTimeZoneOffset,
    inferenceInputConfiguration_s3InputConfiguration,
    inferenceInputConfiguration_inferenceInputNameConfiguration,

    -- * InferenceInputNameConfiguration
    InferenceInputNameConfiguration (..),
    newInferenceInputNameConfiguration,
    inferenceInputNameConfiguration_timestampFormat,
    inferenceInputNameConfiguration_componentTimestampDelimiter,

    -- * InferenceOutputConfiguration
    InferenceOutputConfiguration (..),
    newInferenceOutputConfiguration,
    inferenceOutputConfiguration_kmsKeyId,
    inferenceOutputConfiguration_s3OutputConfiguration,

    -- * InferenceS3InputConfiguration
    InferenceS3InputConfiguration (..),
    newInferenceS3InputConfiguration,
    inferenceS3InputConfiguration_prefix,
    inferenceS3InputConfiguration_bucket,

    -- * InferenceS3OutputConfiguration
    InferenceS3OutputConfiguration (..),
    newInferenceS3OutputConfiguration,
    inferenceS3OutputConfiguration_prefix,
    inferenceS3OutputConfiguration_bucket,

    -- * InferenceSchedulerSummary
    InferenceSchedulerSummary (..),
    newInferenceSchedulerSummary,
    inferenceSchedulerSummary_status,
    inferenceSchedulerSummary_dataUploadFrequency,
    inferenceSchedulerSummary_dataDelayOffsetInMinutes,
    inferenceSchedulerSummary_modelArn,
    inferenceSchedulerSummary_modelName,
    inferenceSchedulerSummary_inferenceSchedulerArn,
    inferenceSchedulerSummary_inferenceSchedulerName,

    -- * IngestionInputConfiguration
    IngestionInputConfiguration (..),
    newIngestionInputConfiguration,
    ingestionInputConfiguration_s3InputConfiguration,

    -- * IngestionS3InputConfiguration
    IngestionS3InputConfiguration (..),
    newIngestionS3InputConfiguration,
    ingestionS3InputConfiguration_prefix,
    ingestionS3InputConfiguration_bucket,

    -- * LabelsInputConfiguration
    LabelsInputConfiguration (..),
    newLabelsInputConfiguration,
    labelsInputConfiguration_s3InputConfiguration,

    -- * LabelsS3InputConfiguration
    LabelsS3InputConfiguration (..),
    newLabelsS3InputConfiguration,
    labelsS3InputConfiguration_prefix,
    labelsS3InputConfiguration_bucket,

    -- * ModelSummary
    ModelSummary (..),
    newModelSummary,
    modelSummary_status,
    modelSummary_datasetArn,
    modelSummary_modelArn,
    modelSummary_createdAt,
    modelSummary_modelName,
    modelSummary_datasetName,

    -- * S3Object
    S3Object (..),
    newS3Object,
    s3Object_bucket,
    s3Object_key,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LookoutEquipment.Types.DataIngestionJobSummary
import Network.AWS.LookoutEquipment.Types.DataPreProcessingConfiguration
import Network.AWS.LookoutEquipment.Types.DataUploadFrequency
import Network.AWS.LookoutEquipment.Types.DatasetSchema
import Network.AWS.LookoutEquipment.Types.DatasetStatus
import Network.AWS.LookoutEquipment.Types.DatasetSummary
import Network.AWS.LookoutEquipment.Types.InferenceExecutionStatus
import Network.AWS.LookoutEquipment.Types.InferenceExecutionSummary
import Network.AWS.LookoutEquipment.Types.InferenceInputConfiguration
import Network.AWS.LookoutEquipment.Types.InferenceInputNameConfiguration
import Network.AWS.LookoutEquipment.Types.InferenceOutputConfiguration
import Network.AWS.LookoutEquipment.Types.InferenceS3InputConfiguration
import Network.AWS.LookoutEquipment.Types.InferenceS3OutputConfiguration
import Network.AWS.LookoutEquipment.Types.InferenceSchedulerStatus
import Network.AWS.LookoutEquipment.Types.InferenceSchedulerSummary
import Network.AWS.LookoutEquipment.Types.IngestionInputConfiguration
import Network.AWS.LookoutEquipment.Types.IngestionJobStatus
import Network.AWS.LookoutEquipment.Types.IngestionS3InputConfiguration
import Network.AWS.LookoutEquipment.Types.LabelsInputConfiguration
import Network.AWS.LookoutEquipment.Types.LabelsS3InputConfiguration
import Network.AWS.LookoutEquipment.Types.ModelStatus
import Network.AWS.LookoutEquipment.Types.ModelSummary
import Network.AWS.LookoutEquipment.Types.S3Object
import Network.AWS.LookoutEquipment.Types.Tag
import Network.AWS.LookoutEquipment.Types.TargetSamplingRate
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-12-15@ of the Amazon Lookout for Equipment SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "LookoutEquipment",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "lookoutequipment",
      Core._serviceSigningName = "lookoutequipment",
      Core._serviceVersion = "2020-12-15",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "LookoutEquipment",
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

-- | The input fails to satisfy constraints specified by Amazon Lookout for
-- Equipment or a related AWS service that\'s being utilized.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | The request could not be completed because you do not have access to the
-- resource.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The request could not be completed due to a conflict with the current
-- state of the target resource.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Resource limitations have been exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | Processing of the request has failed because of an unknown error,
-- exception or failure.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The resource requested could not be found. Verify the resource ID and
-- retry your request.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
