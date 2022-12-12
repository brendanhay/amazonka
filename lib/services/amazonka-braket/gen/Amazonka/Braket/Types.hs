{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Braket.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _DeviceOfflineException,
    _DeviceRetiredException,
    _InternalServiceException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * CancellationStatus
    CancellationStatus (..),

    -- * CompressionType
    CompressionType (..),

    -- * DeviceStatus
    DeviceStatus (..),

    -- * DeviceType
    DeviceType (..),

    -- * InstanceType
    InstanceType (..),

    -- * JobEventType
    JobEventType (..),

    -- * JobPrimaryStatus
    JobPrimaryStatus (..),

    -- * QuantumTaskStatus
    QuantumTaskStatus (..),

    -- * SearchJobsFilterOperator
    SearchJobsFilterOperator (..),

    -- * SearchQuantumTasksFilterOperator
    SearchQuantumTasksFilterOperator (..),

    -- * AlgorithmSpecification
    AlgorithmSpecification (..),
    newAlgorithmSpecification,
    algorithmSpecification_containerImage,
    algorithmSpecification_scriptModeConfig,

    -- * ContainerImage
    ContainerImage (..),
    newContainerImage,
    containerImage_uri,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_s3DataSource,

    -- * DeviceConfig
    DeviceConfig (..),
    newDeviceConfig,
    deviceConfig_device,

    -- * DeviceSummary
    DeviceSummary (..),
    newDeviceSummary,
    deviceSummary_deviceArn,
    deviceSummary_deviceName,
    deviceSummary_deviceStatus,
    deviceSummary_deviceType,
    deviceSummary_providerName,

    -- * InputFileConfig
    InputFileConfig (..),
    newInputFileConfig,
    inputFileConfig_contentType,
    inputFileConfig_channelName,
    inputFileConfig_dataSource,

    -- * InstanceConfig
    InstanceConfig (..),
    newInstanceConfig,
    instanceConfig_instanceCount,
    instanceConfig_instanceType,
    instanceConfig_volumeSizeInGb,

    -- * JobCheckpointConfig
    JobCheckpointConfig (..),
    newJobCheckpointConfig,
    jobCheckpointConfig_localPath,
    jobCheckpointConfig_s3Uri,

    -- * JobEventDetails
    JobEventDetails (..),
    newJobEventDetails,
    jobEventDetails_eventType,
    jobEventDetails_message,
    jobEventDetails_timeOfEvent,

    -- * JobOutputDataConfig
    JobOutputDataConfig (..),
    newJobOutputDataConfig,
    jobOutputDataConfig_kmsKeyId,
    jobOutputDataConfig_s3Path,

    -- * JobStoppingCondition
    JobStoppingCondition (..),
    newJobStoppingCondition,
    jobStoppingCondition_maxRuntimeInSeconds,

    -- * JobSummary
    JobSummary (..),
    newJobSummary,
    jobSummary_endedAt,
    jobSummary_startedAt,
    jobSummary_tags,
    jobSummary_createdAt,
    jobSummary_device,
    jobSummary_jobArn,
    jobSummary_jobName,
    jobSummary_status,

    -- * QuantumTaskSummary
    QuantumTaskSummary (..),
    newQuantumTaskSummary,
    quantumTaskSummary_endedAt,
    quantumTaskSummary_tags,
    quantumTaskSummary_createdAt,
    quantumTaskSummary_deviceArn,
    quantumTaskSummary_outputS3Bucket,
    quantumTaskSummary_outputS3Directory,
    quantumTaskSummary_quantumTaskArn,
    quantumTaskSummary_shots,
    quantumTaskSummary_status,

    -- * S3DataSource
    S3DataSource (..),
    newS3DataSource,
    s3DataSource_s3Uri,

    -- * ScriptModeConfig
    ScriptModeConfig (..),
    newScriptModeConfig,
    scriptModeConfig_compressionType,
    scriptModeConfig_entryPoint,
    scriptModeConfig_s3Uri,

    -- * SearchDevicesFilter
    SearchDevicesFilter (..),
    newSearchDevicesFilter,
    searchDevicesFilter_name,
    searchDevicesFilter_values,

    -- * SearchJobsFilter
    SearchJobsFilter (..),
    newSearchJobsFilter,
    searchJobsFilter_name,
    searchJobsFilter_operator,
    searchJobsFilter_values,

    -- * SearchQuantumTasksFilter
    SearchQuantumTasksFilter (..),
    newSearchQuantumTasksFilter,
    searchQuantumTasksFilter_name,
    searchQuantumTasksFilter_operator,
    searchQuantumTasksFilter_values,
  )
where

import Amazonka.Braket.Types.AlgorithmSpecification
import Amazonka.Braket.Types.CancellationStatus
import Amazonka.Braket.Types.CompressionType
import Amazonka.Braket.Types.ContainerImage
import Amazonka.Braket.Types.DataSource
import Amazonka.Braket.Types.DeviceConfig
import Amazonka.Braket.Types.DeviceStatus
import Amazonka.Braket.Types.DeviceSummary
import Amazonka.Braket.Types.DeviceType
import Amazonka.Braket.Types.InputFileConfig
import Amazonka.Braket.Types.InstanceConfig
import Amazonka.Braket.Types.InstanceType
import Amazonka.Braket.Types.JobCheckpointConfig
import Amazonka.Braket.Types.JobEventDetails
import Amazonka.Braket.Types.JobEventType
import Amazonka.Braket.Types.JobOutputDataConfig
import Amazonka.Braket.Types.JobPrimaryStatus
import Amazonka.Braket.Types.JobStoppingCondition
import Amazonka.Braket.Types.JobSummary
import Amazonka.Braket.Types.QuantumTaskStatus
import Amazonka.Braket.Types.QuantumTaskSummary
import Amazonka.Braket.Types.S3DataSource
import Amazonka.Braket.Types.ScriptModeConfig
import Amazonka.Braket.Types.SearchDevicesFilter
import Amazonka.Braket.Types.SearchJobsFilter
import Amazonka.Braket.Types.SearchJobsFilterOperator
import Amazonka.Braket.Types.SearchQuantumTasksFilter
import Amazonka.Braket.Types.SearchQuantumTasksFilterOperator
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-09-01@ of the Amazon Braket SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Braket",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "braket",
      Core.signingName = "braket",
      Core.version = "2019-09-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Braket",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An error occurred due to a conflict.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The specified device is currently offline.
_DeviceOfflineException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeviceOfflineException =
  Core._MatchServiceError
    defaultService
    "DeviceOfflineException"
    Prelude.. Core.hasStatus 424

-- | The specified device has been retired.
_DeviceRetiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeviceRetiredException =
  Core._MatchServiceError
    defaultService
    "DeviceRetiredException"
    Prelude.. Core.hasStatus 410

-- | The request processing has failed because of an unknown error,
-- exception, or failure.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"
    Prelude.. Core.hasStatus 500

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request failed because a service quota is exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The throttling rate limit is met.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the constraints specified by an AWS service.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
