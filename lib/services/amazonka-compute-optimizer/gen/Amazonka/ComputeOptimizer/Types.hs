{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ComputeOptimizer.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _OptInRequiredException,
    _ServiceUnavailableException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _ThrottlingException,
    _MissingAuthenticationToken,
    _InvalidParameterValueException,

    -- * CpuVendorArchitecture
    CpuVendorArchitecture (..),

    -- * EBSFilterName
    EBSFilterName (..),

    -- * EBSFinding
    EBSFinding (..),

    -- * EBSMetricName
    EBSMetricName (..),

    -- * EnrollmentFilterName
    EnrollmentFilterName (..),

    -- * ExportableAutoScalingGroupField
    ExportableAutoScalingGroupField (..),

    -- * ExportableInstanceField
    ExportableInstanceField (..),

    -- * ExportableLambdaFunctionField
    ExportableLambdaFunctionField (..),

    -- * ExportableVolumeField
    ExportableVolumeField (..),

    -- * FileFormat
    FileFormat (..),

    -- * FilterName
    FilterName (..),

    -- * Finding
    Finding (..),

    -- * FindingReasonCode
    FindingReasonCode (..),

    -- * InstanceRecommendationFindingReasonCode
    InstanceRecommendationFindingReasonCode (..),

    -- * JobFilterName
    JobFilterName (..),

    -- * JobStatus
    JobStatus (..),

    -- * LambdaFunctionMemoryMetricName
    LambdaFunctionMemoryMetricName (..),

    -- * LambdaFunctionMemoryMetricStatistic
    LambdaFunctionMemoryMetricStatistic (..),

    -- * LambdaFunctionMetricName
    LambdaFunctionMetricName (..),

    -- * LambdaFunctionMetricStatistic
    LambdaFunctionMetricStatistic (..),

    -- * LambdaFunctionRecommendationFilterName
    LambdaFunctionRecommendationFilterName (..),

    -- * LambdaFunctionRecommendationFinding
    LambdaFunctionRecommendationFinding (..),

    -- * LambdaFunctionRecommendationFindingReasonCode
    LambdaFunctionRecommendationFindingReasonCode (..),

    -- * MetricName
    MetricName (..),

    -- * MetricStatistic
    MetricStatistic (..),

    -- * PlatformDifference
    PlatformDifference (..),

    -- * RecommendationSourceType
    RecommendationSourceType (..),

    -- * ResourceType
    ResourceType (..),

    -- * Status
    Status (..),

    -- * AccountEnrollmentStatus
    AccountEnrollmentStatus (..),
    newAccountEnrollmentStatus,
    accountEnrollmentStatus_lastUpdatedTimestamp,
    accountEnrollmentStatus_statusReason,
    accountEnrollmentStatus_status,
    accountEnrollmentStatus_accountId,

    -- * AutoScalingGroupConfiguration
    AutoScalingGroupConfiguration (..),
    newAutoScalingGroupConfiguration,
    autoScalingGroupConfiguration_instanceType,
    autoScalingGroupConfiguration_minSize,
    autoScalingGroupConfiguration_maxSize,
    autoScalingGroupConfiguration_desiredCapacity,

    -- * AutoScalingGroupRecommendation
    AutoScalingGroupRecommendation (..),
    newAutoScalingGroupRecommendation,
    autoScalingGroupRecommendation_autoScalingGroupArn,
    autoScalingGroupRecommendation_recommendationOptions,
    autoScalingGroupRecommendation_lastRefreshTimestamp,
    autoScalingGroupRecommendation_currentConfiguration,
    autoScalingGroupRecommendation_lookBackPeriodInDays,
    autoScalingGroupRecommendation_autoScalingGroupName,
    autoScalingGroupRecommendation_accountId,
    autoScalingGroupRecommendation_utilizationMetrics,
    autoScalingGroupRecommendation_finding,

    -- * AutoScalingGroupRecommendationOption
    AutoScalingGroupRecommendationOption (..),
    newAutoScalingGroupRecommendationOption,
    autoScalingGroupRecommendationOption_performanceRisk,
    autoScalingGroupRecommendationOption_configuration,
    autoScalingGroupRecommendationOption_rank,
    autoScalingGroupRecommendationOption_projectedUtilizationMetrics,

    -- * EBSFilter
    EBSFilter (..),
    newEBSFilter,
    eBSFilter_name,
    eBSFilter_values,

    -- * EBSUtilizationMetric
    EBSUtilizationMetric (..),
    newEBSUtilizationMetric,
    eBSUtilizationMetric_name,
    eBSUtilizationMetric_statistic,
    eBSUtilizationMetric_value,

    -- * EnrollmentFilter
    EnrollmentFilter (..),
    newEnrollmentFilter,
    enrollmentFilter_name,
    enrollmentFilter_values,

    -- * ExportDestination
    ExportDestination (..),
    newExportDestination,
    exportDestination_s3,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * GetRecommendationError
    GetRecommendationError (..),
    newGetRecommendationError,
    getRecommendationError_message,
    getRecommendationError_code,
    getRecommendationError_identifier,

    -- * InstanceRecommendation
    InstanceRecommendation (..),
    newInstanceRecommendation,
    instanceRecommendation_findingReasonCodes,
    instanceRecommendation_instanceName,
    instanceRecommendation_recommendationOptions,
    instanceRecommendation_lastRefreshTimestamp,
    instanceRecommendation_instanceArn,
    instanceRecommendation_lookBackPeriodInDays,
    instanceRecommendation_accountId,
    instanceRecommendation_recommendationSources,
    instanceRecommendation_currentInstanceType,
    instanceRecommendation_utilizationMetrics,
    instanceRecommendation_finding,

    -- * InstanceRecommendationOption
    InstanceRecommendationOption (..),
    newInstanceRecommendationOption,
    instanceRecommendationOption_performanceRisk,
    instanceRecommendationOption_platformDifferences,
    instanceRecommendationOption_rank,
    instanceRecommendationOption_instanceType,
    instanceRecommendationOption_projectedUtilizationMetrics,

    -- * JobFilter
    JobFilter (..),
    newJobFilter,
    jobFilter_name,
    jobFilter_values,

    -- * LambdaFunctionMemoryProjectedMetric
    LambdaFunctionMemoryProjectedMetric (..),
    newLambdaFunctionMemoryProjectedMetric,
    lambdaFunctionMemoryProjectedMetric_name,
    lambdaFunctionMemoryProjectedMetric_statistic,
    lambdaFunctionMemoryProjectedMetric_value,

    -- * LambdaFunctionMemoryRecommendationOption
    LambdaFunctionMemoryRecommendationOption (..),
    newLambdaFunctionMemoryRecommendationOption,
    lambdaFunctionMemoryRecommendationOption_memorySize,
    lambdaFunctionMemoryRecommendationOption_rank,
    lambdaFunctionMemoryRecommendationOption_projectedUtilizationMetrics,

    -- * LambdaFunctionRecommendation
    LambdaFunctionRecommendation (..),
    newLambdaFunctionRecommendation,
    lambdaFunctionRecommendation_findingReasonCodes,
    lambdaFunctionRecommendation_functionArn,
    lambdaFunctionRecommendation_currentMemorySize,
    lambdaFunctionRecommendation_numberOfInvocations,
    lambdaFunctionRecommendation_lastRefreshTimestamp,
    lambdaFunctionRecommendation_functionVersion,
    lambdaFunctionRecommendation_lookbackPeriodInDays,
    lambdaFunctionRecommendation_accountId,
    lambdaFunctionRecommendation_utilizationMetrics,
    lambdaFunctionRecommendation_memorySizeRecommendationOptions,
    lambdaFunctionRecommendation_finding,

    -- * LambdaFunctionRecommendationFilter
    LambdaFunctionRecommendationFilter (..),
    newLambdaFunctionRecommendationFilter,
    lambdaFunctionRecommendationFilter_name,
    lambdaFunctionRecommendationFilter_values,

    -- * LambdaFunctionUtilizationMetric
    LambdaFunctionUtilizationMetric (..),
    newLambdaFunctionUtilizationMetric,
    lambdaFunctionUtilizationMetric_name,
    lambdaFunctionUtilizationMetric_statistic,
    lambdaFunctionUtilizationMetric_value,

    -- * ProjectedMetric
    ProjectedMetric (..),
    newProjectedMetric,
    projectedMetric_name,
    projectedMetric_timestamps,
    projectedMetric_values,

    -- * ReasonCodeSummary
    ReasonCodeSummary (..),
    newReasonCodeSummary,
    reasonCodeSummary_name,
    reasonCodeSummary_value,

    -- * RecommendationExportJob
    RecommendationExportJob (..),
    newRecommendationExportJob,
    recommendationExportJob_destination,
    recommendationExportJob_lastUpdatedTimestamp,
    recommendationExportJob_resourceType,
    recommendationExportJob_jobId,
    recommendationExportJob_status,
    recommendationExportJob_creationTimestamp,
    recommendationExportJob_failureReason,

    -- * RecommendationPreferences
    RecommendationPreferences (..),
    newRecommendationPreferences,
    recommendationPreferences_cpuVendorArchitectures,

    -- * RecommendationSource
    RecommendationSource (..),
    newRecommendationSource,
    recommendationSource_recommendationSourceType,
    recommendationSource_recommendationSourceArn,

    -- * RecommendationSummary
    RecommendationSummary (..),
    newRecommendationSummary,
    recommendationSummary_recommendationResourceType,
    recommendationSummary_summaries,
    recommendationSummary_accountId,

    -- * RecommendedOptionProjectedMetric
    RecommendedOptionProjectedMetric (..),
    newRecommendedOptionProjectedMetric,
    recommendedOptionProjectedMetric_rank,
    recommendedOptionProjectedMetric_projectedMetrics,
    recommendedOptionProjectedMetric_recommendedInstanceType,

    -- * S3Destination
    S3Destination (..),
    newS3Destination,
    s3Destination_key,
    s3Destination_bucket,
    s3Destination_metadataKey,

    -- * S3DestinationConfig
    S3DestinationConfig (..),
    newS3DestinationConfig,
    s3DestinationConfig_bucket,
    s3DestinationConfig_keyPrefix,

    -- * Summary
    Summary (..),
    newSummary,
    summary_name,
    summary_reasonCodeSummaries,
    summary_value,

    -- * UtilizationMetric
    UtilizationMetric (..),
    newUtilizationMetric,
    utilizationMetric_name,
    utilizationMetric_statistic,
    utilizationMetric_value,

    -- * VolumeConfiguration
    VolumeConfiguration (..),
    newVolumeConfiguration,
    volumeConfiguration_volumeBurstThroughput,
    volumeConfiguration_volumeType,
    volumeConfiguration_volumeSize,
    volumeConfiguration_volumeBaselineIOPS,
    volumeConfiguration_volumeBaselineThroughput,
    volumeConfiguration_volumeBurstIOPS,

    -- * VolumeRecommendation
    VolumeRecommendation (..),
    newVolumeRecommendation,
    volumeRecommendation_lastRefreshTimestamp,
    volumeRecommendation_currentConfiguration,
    volumeRecommendation_volumeArn,
    volumeRecommendation_lookBackPeriodInDays,
    volumeRecommendation_accountId,
    volumeRecommendation_utilizationMetrics,
    volumeRecommendation_volumeRecommendationOptions,
    volumeRecommendation_finding,

    -- * VolumeRecommendationOption
    VolumeRecommendationOption (..),
    newVolumeRecommendationOption,
    volumeRecommendationOption_performanceRisk,
    volumeRecommendationOption_configuration,
    volumeRecommendationOption_rank,
  )
where

import Amazonka.ComputeOptimizer.Types.AccountEnrollmentStatus
import Amazonka.ComputeOptimizer.Types.AutoScalingGroupConfiguration
import Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendation
import Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendationOption
import Amazonka.ComputeOptimizer.Types.CpuVendorArchitecture
import Amazonka.ComputeOptimizer.Types.EBSFilter
import Amazonka.ComputeOptimizer.Types.EBSFilterName
import Amazonka.ComputeOptimizer.Types.EBSFinding
import Amazonka.ComputeOptimizer.Types.EBSMetricName
import Amazonka.ComputeOptimizer.Types.EBSUtilizationMetric
import Amazonka.ComputeOptimizer.Types.EnrollmentFilter
import Amazonka.ComputeOptimizer.Types.EnrollmentFilterName
import Amazonka.ComputeOptimizer.Types.ExportDestination
import Amazonka.ComputeOptimizer.Types.ExportableAutoScalingGroupField
import Amazonka.ComputeOptimizer.Types.ExportableInstanceField
import Amazonka.ComputeOptimizer.Types.ExportableLambdaFunctionField
import Amazonka.ComputeOptimizer.Types.ExportableVolumeField
import Amazonka.ComputeOptimizer.Types.FileFormat
import Amazonka.ComputeOptimizer.Types.Filter
import Amazonka.ComputeOptimizer.Types.FilterName
import Amazonka.ComputeOptimizer.Types.Finding
import Amazonka.ComputeOptimizer.Types.FindingReasonCode
import Amazonka.ComputeOptimizer.Types.GetRecommendationError
import Amazonka.ComputeOptimizer.Types.InstanceRecommendation
import Amazonka.ComputeOptimizer.Types.InstanceRecommendationFindingReasonCode
import Amazonka.ComputeOptimizer.Types.InstanceRecommendationOption
import Amazonka.ComputeOptimizer.Types.JobFilter
import Amazonka.ComputeOptimizer.Types.JobFilterName
import Amazonka.ComputeOptimizer.Types.JobStatus
import Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryMetricName
import Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryMetricStatistic
import Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryProjectedMetric
import Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryRecommendationOption
import Amazonka.ComputeOptimizer.Types.LambdaFunctionMetricName
import Amazonka.ComputeOptimizer.Types.LambdaFunctionMetricStatistic
import Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendation
import Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFilter
import Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFilterName
import Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFinding
import Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFindingReasonCode
import Amazonka.ComputeOptimizer.Types.LambdaFunctionUtilizationMetric
import Amazonka.ComputeOptimizer.Types.MetricName
import Amazonka.ComputeOptimizer.Types.MetricStatistic
import Amazonka.ComputeOptimizer.Types.PlatformDifference
import Amazonka.ComputeOptimizer.Types.ProjectedMetric
import Amazonka.ComputeOptimizer.Types.ReasonCodeSummary
import Amazonka.ComputeOptimizer.Types.RecommendationExportJob
import Amazonka.ComputeOptimizer.Types.RecommendationPreferences
import Amazonka.ComputeOptimizer.Types.RecommendationSource
import Amazonka.ComputeOptimizer.Types.RecommendationSourceType
import Amazonka.ComputeOptimizer.Types.RecommendationSummary
import Amazonka.ComputeOptimizer.Types.RecommendedOptionProjectedMetric
import Amazonka.ComputeOptimizer.Types.ResourceType
import Amazonka.ComputeOptimizer.Types.S3Destination
import Amazonka.ComputeOptimizer.Types.S3DestinationConfig
import Amazonka.ComputeOptimizer.Types.Status
import Amazonka.ComputeOptimizer.Types.Summary
import Amazonka.ComputeOptimizer.Types.UtilizationMetric
import Amazonka.ComputeOptimizer.Types.VolumeConfiguration
import Amazonka.ComputeOptimizer.Types.VolumeRecommendation
import Amazonka.ComputeOptimizer.Types.VolumeRecommendationOption
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-11-01@ of the Amazon Compute Optimizer SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "ComputeOptimizer",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "compute-optimizer",
      Core._serviceSigningName = "compute-optimizer",
      Core._serviceVersion = "2019-11-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "ComputeOptimizer",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | An internal error has occurred. Try your call again.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The account is not opted in to Compute Optimizer.
_OptInRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OptInRequiredException =
  Core._MatchServiceError
    defaultService
    "OptInRequiredException"

-- | The request has failed due to a temporary failure of the server.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | A resource that is required for the action doesn\'t exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The request exceeds a limit of the service.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The request must contain either a valid (registered) Amazon Web Services
-- access key ID or X.509 certificate.
_MissingAuthenticationToken :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingAuthenticationToken =
  Core._MatchServiceError
    defaultService
    "MissingAuthenticationToken"

-- | The value supplied for the input parameter is out of range or not valid.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"
