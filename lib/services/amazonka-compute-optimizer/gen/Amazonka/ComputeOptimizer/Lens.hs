{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ComputeOptimizer.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Lens
  ( -- * Operations

    -- ** DescribeRecommendationExportJobs
    describeRecommendationExportJobs_nextToken,
    describeRecommendationExportJobs_filters,
    describeRecommendationExportJobs_maxResults,
    describeRecommendationExportJobs_jobIds,
    describeRecommendationExportJobsResponse_nextToken,
    describeRecommendationExportJobsResponse_recommendationExportJobs,
    describeRecommendationExportJobsResponse_httpStatus,

    -- ** ExportAutoScalingGroupRecommendations
    exportAutoScalingGroupRecommendations_accountIds,
    exportAutoScalingGroupRecommendations_recommendationPreferences,
    exportAutoScalingGroupRecommendations_filters,
    exportAutoScalingGroupRecommendations_includeMemberAccounts,
    exportAutoScalingGroupRecommendations_fileFormat,
    exportAutoScalingGroupRecommendations_fieldsToExport,
    exportAutoScalingGroupRecommendations_s3DestinationConfig,
    exportAutoScalingGroupRecommendationsResponse_jobId,
    exportAutoScalingGroupRecommendationsResponse_s3Destination,
    exportAutoScalingGroupRecommendationsResponse_httpStatus,

    -- ** ExportEBSVolumeRecommendations
    exportEBSVolumeRecommendations_accountIds,
    exportEBSVolumeRecommendations_filters,
    exportEBSVolumeRecommendations_includeMemberAccounts,
    exportEBSVolumeRecommendations_fileFormat,
    exportEBSVolumeRecommendations_fieldsToExport,
    exportEBSVolumeRecommendations_s3DestinationConfig,
    exportEBSVolumeRecommendationsResponse_jobId,
    exportEBSVolumeRecommendationsResponse_s3Destination,
    exportEBSVolumeRecommendationsResponse_httpStatus,

    -- ** ExportEC2InstanceRecommendations
    exportEC2InstanceRecommendations_accountIds,
    exportEC2InstanceRecommendations_recommendationPreferences,
    exportEC2InstanceRecommendations_filters,
    exportEC2InstanceRecommendations_includeMemberAccounts,
    exportEC2InstanceRecommendations_fileFormat,
    exportEC2InstanceRecommendations_fieldsToExport,
    exportEC2InstanceRecommendations_s3DestinationConfig,
    exportEC2InstanceRecommendationsResponse_jobId,
    exportEC2InstanceRecommendationsResponse_s3Destination,
    exportEC2InstanceRecommendationsResponse_httpStatus,

    -- ** ExportLambdaFunctionRecommendations
    exportLambdaFunctionRecommendations_accountIds,
    exportLambdaFunctionRecommendations_filters,
    exportLambdaFunctionRecommendations_includeMemberAccounts,
    exportLambdaFunctionRecommendations_fileFormat,
    exportLambdaFunctionRecommendations_fieldsToExport,
    exportLambdaFunctionRecommendations_s3DestinationConfig,
    exportLambdaFunctionRecommendationsResponse_jobId,
    exportLambdaFunctionRecommendationsResponse_s3Destination,
    exportLambdaFunctionRecommendationsResponse_httpStatus,

    -- ** GetAutoScalingGroupRecommendations
    getAutoScalingGroupRecommendations_accountIds,
    getAutoScalingGroupRecommendations_nextToken,
    getAutoScalingGroupRecommendations_recommendationPreferences,
    getAutoScalingGroupRecommendations_filters,
    getAutoScalingGroupRecommendations_maxResults,
    getAutoScalingGroupRecommendations_autoScalingGroupArns,
    getAutoScalingGroupRecommendationsResponse_nextToken,
    getAutoScalingGroupRecommendationsResponse_errors,
    getAutoScalingGroupRecommendationsResponse_autoScalingGroupRecommendations,
    getAutoScalingGroupRecommendationsResponse_httpStatus,

    -- ** GetEBSVolumeRecommendations
    getEBSVolumeRecommendations_accountIds,
    getEBSVolumeRecommendations_nextToken,
    getEBSVolumeRecommendations_filters,
    getEBSVolumeRecommendations_maxResults,
    getEBSVolumeRecommendations_volumeArns,
    getEBSVolumeRecommendationsResponse_nextToken,
    getEBSVolumeRecommendationsResponse_errors,
    getEBSVolumeRecommendationsResponse_volumeRecommendations,
    getEBSVolumeRecommendationsResponse_httpStatus,

    -- ** GetEC2InstanceRecommendations
    getEC2InstanceRecommendations_accountIds,
    getEC2InstanceRecommendations_nextToken,
    getEC2InstanceRecommendations_recommendationPreferences,
    getEC2InstanceRecommendations_filters,
    getEC2InstanceRecommendations_maxResults,
    getEC2InstanceRecommendations_instanceArns,
    getEC2InstanceRecommendationsResponse_instanceRecommendations,
    getEC2InstanceRecommendationsResponse_nextToken,
    getEC2InstanceRecommendationsResponse_errors,
    getEC2InstanceRecommendationsResponse_httpStatus,

    -- ** GetEC2RecommendationProjectedMetrics
    getEC2RecommendationProjectedMetrics_recommendationPreferences,
    getEC2RecommendationProjectedMetrics_instanceArn,
    getEC2RecommendationProjectedMetrics_stat,
    getEC2RecommendationProjectedMetrics_period,
    getEC2RecommendationProjectedMetrics_startTime,
    getEC2RecommendationProjectedMetrics_endTime,
    getEC2RecommendationProjectedMetricsResponse_recommendedOptionProjectedMetrics,
    getEC2RecommendationProjectedMetricsResponse_httpStatus,

    -- ** GetEnrollmentStatus
    getEnrollmentStatusResponse_lastUpdatedTimestamp,
    getEnrollmentStatusResponse_statusReason,
    getEnrollmentStatusResponse_status,
    getEnrollmentStatusResponse_numberOfMemberAccountsOptedIn,
    getEnrollmentStatusResponse_memberAccountsEnrolled,
    getEnrollmentStatusResponse_httpStatus,

    -- ** GetEnrollmentStatusesForOrganization
    getEnrollmentStatusesForOrganization_nextToken,
    getEnrollmentStatusesForOrganization_filters,
    getEnrollmentStatusesForOrganization_maxResults,
    getEnrollmentStatusesForOrganizationResponse_nextToken,
    getEnrollmentStatusesForOrganizationResponse_accountEnrollmentStatuses,
    getEnrollmentStatusesForOrganizationResponse_httpStatus,

    -- ** GetLambdaFunctionRecommendations
    getLambdaFunctionRecommendations_accountIds,
    getLambdaFunctionRecommendations_nextToken,
    getLambdaFunctionRecommendations_filters,
    getLambdaFunctionRecommendations_maxResults,
    getLambdaFunctionRecommendations_functionArns,
    getLambdaFunctionRecommendationsResponse_nextToken,
    getLambdaFunctionRecommendationsResponse_lambdaFunctionRecommendations,
    getLambdaFunctionRecommendationsResponse_httpStatus,

    -- ** GetRecommendationSummaries
    getRecommendationSummaries_accountIds,
    getRecommendationSummaries_nextToken,
    getRecommendationSummaries_maxResults,
    getRecommendationSummariesResponse_recommendationSummaries,
    getRecommendationSummariesResponse_nextToken,
    getRecommendationSummariesResponse_httpStatus,

    -- ** UpdateEnrollmentStatus
    updateEnrollmentStatus_includeMemberAccounts,
    updateEnrollmentStatus_status,
    updateEnrollmentStatusResponse_statusReason,
    updateEnrollmentStatusResponse_status,
    updateEnrollmentStatusResponse_httpStatus,

    -- * Types

    -- ** AccountEnrollmentStatus
    accountEnrollmentStatus_lastUpdatedTimestamp,
    accountEnrollmentStatus_statusReason,
    accountEnrollmentStatus_status,
    accountEnrollmentStatus_accountId,

    -- ** AutoScalingGroupConfiguration
    autoScalingGroupConfiguration_instanceType,
    autoScalingGroupConfiguration_minSize,
    autoScalingGroupConfiguration_maxSize,
    autoScalingGroupConfiguration_desiredCapacity,

    -- ** AutoScalingGroupRecommendation
    autoScalingGroupRecommendation_autoScalingGroupArn,
    autoScalingGroupRecommendation_recommendationOptions,
    autoScalingGroupRecommendation_lastRefreshTimestamp,
    autoScalingGroupRecommendation_currentConfiguration,
    autoScalingGroupRecommendation_lookBackPeriodInDays,
    autoScalingGroupRecommendation_autoScalingGroupName,
    autoScalingGroupRecommendation_accountId,
    autoScalingGroupRecommendation_utilizationMetrics,
    autoScalingGroupRecommendation_finding,

    -- ** AutoScalingGroupRecommendationOption
    autoScalingGroupRecommendationOption_performanceRisk,
    autoScalingGroupRecommendationOption_configuration,
    autoScalingGroupRecommendationOption_rank,
    autoScalingGroupRecommendationOption_projectedUtilizationMetrics,

    -- ** EBSFilter
    eBSFilter_name,
    eBSFilter_values,

    -- ** EBSUtilizationMetric
    eBSUtilizationMetric_name,
    eBSUtilizationMetric_statistic,
    eBSUtilizationMetric_value,

    -- ** EnrollmentFilter
    enrollmentFilter_name,
    enrollmentFilter_values,

    -- ** ExportDestination
    exportDestination_s3,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** GetRecommendationError
    getRecommendationError_message,
    getRecommendationError_code,
    getRecommendationError_identifier,

    -- ** InstanceRecommendation
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

    -- ** InstanceRecommendationOption
    instanceRecommendationOption_performanceRisk,
    instanceRecommendationOption_platformDifferences,
    instanceRecommendationOption_rank,
    instanceRecommendationOption_instanceType,
    instanceRecommendationOption_projectedUtilizationMetrics,

    -- ** JobFilter
    jobFilter_name,
    jobFilter_values,

    -- ** LambdaFunctionMemoryProjectedMetric
    lambdaFunctionMemoryProjectedMetric_name,
    lambdaFunctionMemoryProjectedMetric_statistic,
    lambdaFunctionMemoryProjectedMetric_value,

    -- ** LambdaFunctionMemoryRecommendationOption
    lambdaFunctionMemoryRecommendationOption_memorySize,
    lambdaFunctionMemoryRecommendationOption_rank,
    lambdaFunctionMemoryRecommendationOption_projectedUtilizationMetrics,

    -- ** LambdaFunctionRecommendation
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

    -- ** LambdaFunctionRecommendationFilter
    lambdaFunctionRecommendationFilter_name,
    lambdaFunctionRecommendationFilter_values,

    -- ** LambdaFunctionUtilizationMetric
    lambdaFunctionUtilizationMetric_name,
    lambdaFunctionUtilizationMetric_statistic,
    lambdaFunctionUtilizationMetric_value,

    -- ** ProjectedMetric
    projectedMetric_name,
    projectedMetric_timestamps,
    projectedMetric_values,

    -- ** ReasonCodeSummary
    reasonCodeSummary_name,
    reasonCodeSummary_value,

    -- ** RecommendationExportJob
    recommendationExportJob_destination,
    recommendationExportJob_lastUpdatedTimestamp,
    recommendationExportJob_resourceType,
    recommendationExportJob_jobId,
    recommendationExportJob_status,
    recommendationExportJob_creationTimestamp,
    recommendationExportJob_failureReason,

    -- ** RecommendationPreferences
    recommendationPreferences_cpuVendorArchitectures,

    -- ** RecommendationSource
    recommendationSource_recommendationSourceType,
    recommendationSource_recommendationSourceArn,

    -- ** RecommendationSummary
    recommendationSummary_recommendationResourceType,
    recommendationSummary_summaries,
    recommendationSummary_accountId,

    -- ** RecommendedOptionProjectedMetric
    recommendedOptionProjectedMetric_rank,
    recommendedOptionProjectedMetric_projectedMetrics,
    recommendedOptionProjectedMetric_recommendedInstanceType,

    -- ** S3Destination
    s3Destination_key,
    s3Destination_bucket,
    s3Destination_metadataKey,

    -- ** S3DestinationConfig
    s3DestinationConfig_bucket,
    s3DestinationConfig_keyPrefix,

    -- ** Summary
    summary_name,
    summary_reasonCodeSummaries,
    summary_value,

    -- ** UtilizationMetric
    utilizationMetric_name,
    utilizationMetric_statistic,
    utilizationMetric_value,

    -- ** VolumeConfiguration
    volumeConfiguration_volumeBurstThroughput,
    volumeConfiguration_volumeType,
    volumeConfiguration_volumeSize,
    volumeConfiguration_volumeBaselineIOPS,
    volumeConfiguration_volumeBaselineThroughput,
    volumeConfiguration_volumeBurstIOPS,

    -- ** VolumeRecommendation
    volumeRecommendation_lastRefreshTimestamp,
    volumeRecommendation_currentConfiguration,
    volumeRecommendation_volumeArn,
    volumeRecommendation_lookBackPeriodInDays,
    volumeRecommendation_accountId,
    volumeRecommendation_utilizationMetrics,
    volumeRecommendation_volumeRecommendationOptions,
    volumeRecommendation_finding,

    -- ** VolumeRecommendationOption
    volumeRecommendationOption_performanceRisk,
    volumeRecommendationOption_configuration,
    volumeRecommendationOption_rank,
  )
where

import Amazonka.ComputeOptimizer.DescribeRecommendationExportJobs
import Amazonka.ComputeOptimizer.ExportAutoScalingGroupRecommendations
import Amazonka.ComputeOptimizer.ExportEBSVolumeRecommendations
import Amazonka.ComputeOptimizer.ExportEC2InstanceRecommendations
import Amazonka.ComputeOptimizer.ExportLambdaFunctionRecommendations
import Amazonka.ComputeOptimizer.GetAutoScalingGroupRecommendations
import Amazonka.ComputeOptimizer.GetEBSVolumeRecommendations
import Amazonka.ComputeOptimizer.GetEC2InstanceRecommendations
import Amazonka.ComputeOptimizer.GetEC2RecommendationProjectedMetrics
import Amazonka.ComputeOptimizer.GetEnrollmentStatus
import Amazonka.ComputeOptimizer.GetEnrollmentStatusesForOrganization
import Amazonka.ComputeOptimizer.GetLambdaFunctionRecommendations
import Amazonka.ComputeOptimizer.GetRecommendationSummaries
import Amazonka.ComputeOptimizer.Types.AccountEnrollmentStatus
import Amazonka.ComputeOptimizer.Types.AutoScalingGroupConfiguration
import Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendation
import Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendationOption
import Amazonka.ComputeOptimizer.Types.EBSFilter
import Amazonka.ComputeOptimizer.Types.EBSUtilizationMetric
import Amazonka.ComputeOptimizer.Types.EnrollmentFilter
import Amazonka.ComputeOptimizer.Types.ExportDestination
import Amazonka.ComputeOptimizer.Types.Filter
import Amazonka.ComputeOptimizer.Types.GetRecommendationError
import Amazonka.ComputeOptimizer.Types.InstanceRecommendation
import Amazonka.ComputeOptimizer.Types.InstanceRecommendationOption
import Amazonka.ComputeOptimizer.Types.JobFilter
import Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryProjectedMetric
import Amazonka.ComputeOptimizer.Types.LambdaFunctionMemoryRecommendationOption
import Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendation
import Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFilter
import Amazonka.ComputeOptimizer.Types.LambdaFunctionUtilizationMetric
import Amazonka.ComputeOptimizer.Types.ProjectedMetric
import Amazonka.ComputeOptimizer.Types.ReasonCodeSummary
import Amazonka.ComputeOptimizer.Types.RecommendationExportJob
import Amazonka.ComputeOptimizer.Types.RecommendationPreferences
import Amazonka.ComputeOptimizer.Types.RecommendationSource
import Amazonka.ComputeOptimizer.Types.RecommendationSummary
import Amazonka.ComputeOptimizer.Types.RecommendedOptionProjectedMetric
import Amazonka.ComputeOptimizer.Types.S3Destination
import Amazonka.ComputeOptimizer.Types.S3DestinationConfig
import Amazonka.ComputeOptimizer.Types.Summary
import Amazonka.ComputeOptimizer.Types.UtilizationMetric
import Amazonka.ComputeOptimizer.Types.VolumeConfiguration
import Amazonka.ComputeOptimizer.Types.VolumeRecommendation
import Amazonka.ComputeOptimizer.Types.VolumeRecommendationOption
import Amazonka.ComputeOptimizer.UpdateEnrollmentStatus
