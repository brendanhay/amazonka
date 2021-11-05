{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ComputeOptimizer.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ComputeOptimizer.Lens
  ( -- * Operations

    -- ** ExportEBSVolumeRecommendations
    exportEBSVolumeRecommendations_accountIds,
    exportEBSVolumeRecommendations_fileFormat,
    exportEBSVolumeRecommendations_filters,
    exportEBSVolumeRecommendations_fieldsToExport,
    exportEBSVolumeRecommendations_includeMemberAccounts,
    exportEBSVolumeRecommendations_s3DestinationConfig,
    exportEBSVolumeRecommendationsResponse_jobId,
    exportEBSVolumeRecommendationsResponse_s3Destination,
    exportEBSVolumeRecommendationsResponse_httpStatus,

    -- ** GetRecommendationSummaries
    getRecommendationSummaries_accountIds,
    getRecommendationSummaries_nextToken,
    getRecommendationSummaries_maxResults,
    getRecommendationSummariesResponse_nextToken,
    getRecommendationSummariesResponse_recommendationSummaries,
    getRecommendationSummariesResponse_httpStatus,

    -- ** ExportAutoScalingGroupRecommendations
    exportAutoScalingGroupRecommendations_accountIds,
    exportAutoScalingGroupRecommendations_fileFormat,
    exportAutoScalingGroupRecommendations_filters,
    exportAutoScalingGroupRecommendations_fieldsToExport,
    exportAutoScalingGroupRecommendations_includeMemberAccounts,
    exportAutoScalingGroupRecommendations_recommendationPreferences,
    exportAutoScalingGroupRecommendations_s3DestinationConfig,
    exportAutoScalingGroupRecommendationsResponse_jobId,
    exportAutoScalingGroupRecommendationsResponse_s3Destination,
    exportAutoScalingGroupRecommendationsResponse_httpStatus,

    -- ** GetEC2InstanceRecommendations
    getEC2InstanceRecommendations_accountIds,
    getEC2InstanceRecommendations_filters,
    getEC2InstanceRecommendations_recommendationPreferences,
    getEC2InstanceRecommendations_nextToken,
    getEC2InstanceRecommendations_instanceArns,
    getEC2InstanceRecommendations_maxResults,
    getEC2InstanceRecommendationsResponse_nextToken,
    getEC2InstanceRecommendationsResponse_errors,
    getEC2InstanceRecommendationsResponse_instanceRecommendations,
    getEC2InstanceRecommendationsResponse_httpStatus,

    -- ** GetLambdaFunctionRecommendations
    getLambdaFunctionRecommendations_functionArns,
    getLambdaFunctionRecommendations_accountIds,
    getLambdaFunctionRecommendations_filters,
    getLambdaFunctionRecommendations_nextToken,
    getLambdaFunctionRecommendations_maxResults,
    getLambdaFunctionRecommendationsResponse_nextToken,
    getLambdaFunctionRecommendationsResponse_lambdaFunctionRecommendations,
    getLambdaFunctionRecommendationsResponse_httpStatus,

    -- ** UpdateEnrollmentStatus
    updateEnrollmentStatus_includeMemberAccounts,
    updateEnrollmentStatus_status,
    updateEnrollmentStatusResponse_status,
    updateEnrollmentStatusResponse_statusReason,
    updateEnrollmentStatusResponse_httpStatus,

    -- ** DescribeRecommendationExportJobs
    describeRecommendationExportJobs_filters,
    describeRecommendationExportJobs_nextToken,
    describeRecommendationExportJobs_maxResults,
    describeRecommendationExportJobs_jobIds,
    describeRecommendationExportJobsResponse_recommendationExportJobs,
    describeRecommendationExportJobsResponse_nextToken,
    describeRecommendationExportJobsResponse_httpStatus,

    -- ** GetEC2RecommendationProjectedMetrics
    getEC2RecommendationProjectedMetrics_recommendationPreferences,
    getEC2RecommendationProjectedMetrics_instanceArn,
    getEC2RecommendationProjectedMetrics_stat,
    getEC2RecommendationProjectedMetrics_period,
    getEC2RecommendationProjectedMetrics_startTime,
    getEC2RecommendationProjectedMetrics_endTime,
    getEC2RecommendationProjectedMetricsResponse_recommendedOptionProjectedMetrics,
    getEC2RecommendationProjectedMetricsResponse_httpStatus,

    -- ** GetEnrollmentStatusesForOrganization
    getEnrollmentStatusesForOrganization_filters,
    getEnrollmentStatusesForOrganization_nextToken,
    getEnrollmentStatusesForOrganization_maxResults,
    getEnrollmentStatusesForOrganizationResponse_accountEnrollmentStatuses,
    getEnrollmentStatusesForOrganizationResponse_nextToken,
    getEnrollmentStatusesForOrganizationResponse_httpStatus,

    -- ** GetEBSVolumeRecommendations
    getEBSVolumeRecommendations_accountIds,
    getEBSVolumeRecommendations_filters,
    getEBSVolumeRecommendations_nextToken,
    getEBSVolumeRecommendations_volumeArns,
    getEBSVolumeRecommendations_maxResults,
    getEBSVolumeRecommendationsResponse_nextToken,
    getEBSVolumeRecommendationsResponse_volumeRecommendations,
    getEBSVolumeRecommendationsResponse_errors,
    getEBSVolumeRecommendationsResponse_httpStatus,

    -- ** ExportLambdaFunctionRecommendations
    exportLambdaFunctionRecommendations_accountIds,
    exportLambdaFunctionRecommendations_fileFormat,
    exportLambdaFunctionRecommendations_filters,
    exportLambdaFunctionRecommendations_fieldsToExport,
    exportLambdaFunctionRecommendations_includeMemberAccounts,
    exportLambdaFunctionRecommendations_s3DestinationConfig,
    exportLambdaFunctionRecommendationsResponse_jobId,
    exportLambdaFunctionRecommendationsResponse_s3Destination,
    exportLambdaFunctionRecommendationsResponse_httpStatus,

    -- ** ExportEC2InstanceRecommendations
    exportEC2InstanceRecommendations_accountIds,
    exportEC2InstanceRecommendations_fileFormat,
    exportEC2InstanceRecommendations_filters,
    exportEC2InstanceRecommendations_fieldsToExport,
    exportEC2InstanceRecommendations_includeMemberAccounts,
    exportEC2InstanceRecommendations_recommendationPreferences,
    exportEC2InstanceRecommendations_s3DestinationConfig,
    exportEC2InstanceRecommendationsResponse_jobId,
    exportEC2InstanceRecommendationsResponse_s3Destination,
    exportEC2InstanceRecommendationsResponse_httpStatus,

    -- ** GetEnrollmentStatus
    getEnrollmentStatusResponse_status,
    getEnrollmentStatusResponse_numberOfMemberAccountsOptedIn,
    getEnrollmentStatusResponse_memberAccountsEnrolled,
    getEnrollmentStatusResponse_statusReason,
    getEnrollmentStatusResponse_lastUpdatedTimestamp,
    getEnrollmentStatusResponse_httpStatus,

    -- ** GetAutoScalingGroupRecommendations
    getAutoScalingGroupRecommendations_accountIds,
    getAutoScalingGroupRecommendations_filters,
    getAutoScalingGroupRecommendations_autoScalingGroupArns,
    getAutoScalingGroupRecommendations_recommendationPreferences,
    getAutoScalingGroupRecommendations_nextToken,
    getAutoScalingGroupRecommendations_maxResults,
    getAutoScalingGroupRecommendationsResponse_autoScalingGroupRecommendations,
    getAutoScalingGroupRecommendationsResponse_nextToken,
    getAutoScalingGroupRecommendationsResponse_errors,
    getAutoScalingGroupRecommendationsResponse_httpStatus,

    -- * Types

    -- ** AccountEnrollmentStatus
    accountEnrollmentStatus_status,
    accountEnrollmentStatus_accountId,
    accountEnrollmentStatus_statusReason,
    accountEnrollmentStatus_lastUpdatedTimestamp,

    -- ** AutoScalingGroupConfiguration
    autoScalingGroupConfiguration_maxSize,
    autoScalingGroupConfiguration_instanceType,
    autoScalingGroupConfiguration_desiredCapacity,
    autoScalingGroupConfiguration_minSize,

    -- ** AutoScalingGroupRecommendation
    autoScalingGroupRecommendation_finding,
    autoScalingGroupRecommendation_lastRefreshTimestamp,
    autoScalingGroupRecommendation_currentConfiguration,
    autoScalingGroupRecommendation_accountId,
    autoScalingGroupRecommendation_autoScalingGroupName,
    autoScalingGroupRecommendation_utilizationMetrics,
    autoScalingGroupRecommendation_autoScalingGroupArn,
    autoScalingGroupRecommendation_recommendationOptions,
    autoScalingGroupRecommendation_lookBackPeriodInDays,

    -- ** AutoScalingGroupRecommendationOption
    autoScalingGroupRecommendationOption_performanceRisk,
    autoScalingGroupRecommendationOption_projectedUtilizationMetrics,
    autoScalingGroupRecommendationOption_configuration,
    autoScalingGroupRecommendationOption_rank,

    -- ** EBSFilter
    eBSFilter_values,
    eBSFilter_name,

    -- ** EBSUtilizationMetric
    eBSUtilizationMetric_value,
    eBSUtilizationMetric_name,
    eBSUtilizationMetric_statistic,

    -- ** EnrollmentFilter
    enrollmentFilter_values,
    enrollmentFilter_name,

    -- ** ExportDestination
    exportDestination_s3,

    -- ** Filter
    filter_values,
    filter_name,

    -- ** GetRecommendationError
    getRecommendationError_identifier,
    getRecommendationError_code,
    getRecommendationError_message,

    -- ** InstanceRecommendation
    instanceRecommendation_instanceArn,
    instanceRecommendation_finding,
    instanceRecommendation_currentInstanceType,
    instanceRecommendation_lastRefreshTimestamp,
    instanceRecommendation_accountId,
    instanceRecommendation_findingReasonCodes,
    instanceRecommendation_recommendationSources,
    instanceRecommendation_utilizationMetrics,
    instanceRecommendation_instanceName,
    instanceRecommendation_recommendationOptions,
    instanceRecommendation_lookBackPeriodInDays,

    -- ** InstanceRecommendationOption
    instanceRecommendationOption_platformDifferences,
    instanceRecommendationOption_performanceRisk,
    instanceRecommendationOption_projectedUtilizationMetrics,
    instanceRecommendationOption_instanceType,
    instanceRecommendationOption_rank,

    -- ** JobFilter
    jobFilter_values,
    jobFilter_name,

    -- ** LambdaFunctionMemoryProjectedMetric
    lambdaFunctionMemoryProjectedMetric_value,
    lambdaFunctionMemoryProjectedMetric_name,
    lambdaFunctionMemoryProjectedMetric_statistic,

    -- ** LambdaFunctionMemoryRecommendationOption
    lambdaFunctionMemoryRecommendationOption_memorySize,
    lambdaFunctionMemoryRecommendationOption_projectedUtilizationMetrics,
    lambdaFunctionMemoryRecommendationOption_rank,

    -- ** LambdaFunctionRecommendation
    lambdaFunctionRecommendation_functionArn,
    lambdaFunctionRecommendation_finding,
    lambdaFunctionRecommendation_currentMemorySize,
    lambdaFunctionRecommendation_lastRefreshTimestamp,
    lambdaFunctionRecommendation_accountId,
    lambdaFunctionRecommendation_findingReasonCodes,
    lambdaFunctionRecommendation_utilizationMetrics,
    lambdaFunctionRecommendation_memorySizeRecommendationOptions,
    lambdaFunctionRecommendation_functionVersion,
    lambdaFunctionRecommendation_numberOfInvocations,
    lambdaFunctionRecommendation_lookbackPeriodInDays,

    -- ** LambdaFunctionRecommendationFilter
    lambdaFunctionRecommendationFilter_values,
    lambdaFunctionRecommendationFilter_name,

    -- ** LambdaFunctionUtilizationMetric
    lambdaFunctionUtilizationMetric_value,
    lambdaFunctionUtilizationMetric_name,
    lambdaFunctionUtilizationMetric_statistic,

    -- ** ProjectedMetric
    projectedMetric_values,
    projectedMetric_name,
    projectedMetric_timestamps,

    -- ** ReasonCodeSummary
    reasonCodeSummary_value,
    reasonCodeSummary_name,

    -- ** RecommendationExportJob
    recommendationExportJob_failureReason,
    recommendationExportJob_destination,
    recommendationExportJob_status,
    recommendationExportJob_jobId,
    recommendationExportJob_resourceType,
    recommendationExportJob_creationTimestamp,
    recommendationExportJob_lastUpdatedTimestamp,

    -- ** RecommendationPreferences
    recommendationPreferences_cpuVendorArchitectures,

    -- ** RecommendationSource
    recommendationSource_recommendationSourceArn,
    recommendationSource_recommendationSourceType,

    -- ** RecommendationSummary
    recommendationSummary_accountId,
    recommendationSummary_summaries,
    recommendationSummary_recommendationResourceType,

    -- ** RecommendedOptionProjectedMetric
    recommendedOptionProjectedMetric_recommendedInstanceType,
    recommendedOptionProjectedMetric_projectedMetrics,
    recommendedOptionProjectedMetric_rank,

    -- ** S3Destination
    s3Destination_bucket,
    s3Destination_key,
    s3Destination_metadataKey,

    -- ** S3DestinationConfig
    s3DestinationConfig_bucket,
    s3DestinationConfig_keyPrefix,

    -- ** Summary
    summary_reasonCodeSummaries,
    summary_value,
    summary_name,

    -- ** UtilizationMetric
    utilizationMetric_value,
    utilizationMetric_name,
    utilizationMetric_statistic,

    -- ** VolumeConfiguration
    volumeConfiguration_volumeSize,
    volumeConfiguration_volumeBaselineIOPS,
    volumeConfiguration_volumeBurstIOPS,
    volumeConfiguration_volumeType,
    volumeConfiguration_volumeBurstThroughput,
    volumeConfiguration_volumeBaselineThroughput,

    -- ** VolumeRecommendation
    volumeRecommendation_finding,
    volumeRecommendation_volumeArn,
    volumeRecommendation_lastRefreshTimestamp,
    volumeRecommendation_currentConfiguration,
    volumeRecommendation_accountId,
    volumeRecommendation_utilizationMetrics,
    volumeRecommendation_volumeRecommendationOptions,
    volumeRecommendation_lookBackPeriodInDays,

    -- ** VolumeRecommendationOption
    volumeRecommendationOption_performanceRisk,
    volumeRecommendationOption_configuration,
    volumeRecommendationOption_rank,
  )
where

import Network.AWS.ComputeOptimizer.DescribeRecommendationExportJobs
import Network.AWS.ComputeOptimizer.ExportAutoScalingGroupRecommendations
import Network.AWS.ComputeOptimizer.ExportEBSVolumeRecommendations
import Network.AWS.ComputeOptimizer.ExportEC2InstanceRecommendations
import Network.AWS.ComputeOptimizer.ExportLambdaFunctionRecommendations
import Network.AWS.ComputeOptimizer.GetAutoScalingGroupRecommendations
import Network.AWS.ComputeOptimizer.GetEBSVolumeRecommendations
import Network.AWS.ComputeOptimizer.GetEC2InstanceRecommendations
import Network.AWS.ComputeOptimizer.GetEC2RecommendationProjectedMetrics
import Network.AWS.ComputeOptimizer.GetEnrollmentStatus
import Network.AWS.ComputeOptimizer.GetEnrollmentStatusesForOrganization
import Network.AWS.ComputeOptimizer.GetLambdaFunctionRecommendations
import Network.AWS.ComputeOptimizer.GetRecommendationSummaries
import Network.AWS.ComputeOptimizer.Types.AccountEnrollmentStatus
import Network.AWS.ComputeOptimizer.Types.AutoScalingGroupConfiguration
import Network.AWS.ComputeOptimizer.Types.AutoScalingGroupRecommendation
import Network.AWS.ComputeOptimizer.Types.AutoScalingGroupRecommendationOption
import Network.AWS.ComputeOptimizer.Types.EBSFilter
import Network.AWS.ComputeOptimizer.Types.EBSUtilizationMetric
import Network.AWS.ComputeOptimizer.Types.EnrollmentFilter
import Network.AWS.ComputeOptimizer.Types.ExportDestination
import Network.AWS.ComputeOptimizer.Types.Filter
import Network.AWS.ComputeOptimizer.Types.GetRecommendationError
import Network.AWS.ComputeOptimizer.Types.InstanceRecommendation
import Network.AWS.ComputeOptimizer.Types.InstanceRecommendationOption
import Network.AWS.ComputeOptimizer.Types.JobFilter
import Network.AWS.ComputeOptimizer.Types.LambdaFunctionMemoryProjectedMetric
import Network.AWS.ComputeOptimizer.Types.LambdaFunctionMemoryRecommendationOption
import Network.AWS.ComputeOptimizer.Types.LambdaFunctionRecommendation
import Network.AWS.ComputeOptimizer.Types.LambdaFunctionRecommendationFilter
import Network.AWS.ComputeOptimizer.Types.LambdaFunctionUtilizationMetric
import Network.AWS.ComputeOptimizer.Types.ProjectedMetric
import Network.AWS.ComputeOptimizer.Types.ReasonCodeSummary
import Network.AWS.ComputeOptimizer.Types.RecommendationExportJob
import Network.AWS.ComputeOptimizer.Types.RecommendationPreferences
import Network.AWS.ComputeOptimizer.Types.RecommendationSource
import Network.AWS.ComputeOptimizer.Types.RecommendationSummary
import Network.AWS.ComputeOptimizer.Types.RecommendedOptionProjectedMetric
import Network.AWS.ComputeOptimizer.Types.S3Destination
import Network.AWS.ComputeOptimizer.Types.S3DestinationConfig
import Network.AWS.ComputeOptimizer.Types.Summary
import Network.AWS.ComputeOptimizer.Types.UtilizationMetric
import Network.AWS.ComputeOptimizer.Types.VolumeConfiguration
import Network.AWS.ComputeOptimizer.Types.VolumeRecommendation
import Network.AWS.ComputeOptimizer.Types.VolumeRecommendationOption
import Network.AWS.ComputeOptimizer.UpdateEnrollmentStatus
