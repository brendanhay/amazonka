{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ComputeOptimizer.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Lens
  ( -- * Operations

    -- ** DeleteRecommendationPreferences
    deleteRecommendationPreferences_scope,
    deleteRecommendationPreferences_resourceType,
    deleteRecommendationPreferences_recommendationPreferenceNames,
    deleteRecommendationPreferencesResponse_httpStatus,

    -- ** DescribeRecommendationExportJobs
    describeRecommendationExportJobs_filters,
    describeRecommendationExportJobs_jobIds,
    describeRecommendationExportJobs_maxResults,
    describeRecommendationExportJobs_nextToken,
    describeRecommendationExportJobsResponse_nextToken,
    describeRecommendationExportJobsResponse_recommendationExportJobs,
    describeRecommendationExportJobsResponse_httpStatus,

    -- ** ExportAutoScalingGroupRecommendations
    exportAutoScalingGroupRecommendations_accountIds,
    exportAutoScalingGroupRecommendations_fieldsToExport,
    exportAutoScalingGroupRecommendations_fileFormat,
    exportAutoScalingGroupRecommendations_filters,
    exportAutoScalingGroupRecommendations_includeMemberAccounts,
    exportAutoScalingGroupRecommendations_recommendationPreferences,
    exportAutoScalingGroupRecommendations_s3DestinationConfig,
    exportAutoScalingGroupRecommendationsResponse_jobId,
    exportAutoScalingGroupRecommendationsResponse_s3Destination,
    exportAutoScalingGroupRecommendationsResponse_httpStatus,

    -- ** ExportEBSVolumeRecommendations
    exportEBSVolumeRecommendations_accountIds,
    exportEBSVolumeRecommendations_fieldsToExport,
    exportEBSVolumeRecommendations_fileFormat,
    exportEBSVolumeRecommendations_filters,
    exportEBSVolumeRecommendations_includeMemberAccounts,
    exportEBSVolumeRecommendations_s3DestinationConfig,
    exportEBSVolumeRecommendationsResponse_jobId,
    exportEBSVolumeRecommendationsResponse_s3Destination,
    exportEBSVolumeRecommendationsResponse_httpStatus,

    -- ** ExportEC2InstanceRecommendations
    exportEC2InstanceRecommendations_accountIds,
    exportEC2InstanceRecommendations_fieldsToExport,
    exportEC2InstanceRecommendations_fileFormat,
    exportEC2InstanceRecommendations_filters,
    exportEC2InstanceRecommendations_includeMemberAccounts,
    exportEC2InstanceRecommendations_recommendationPreferences,
    exportEC2InstanceRecommendations_s3DestinationConfig,
    exportEC2InstanceRecommendationsResponse_jobId,
    exportEC2InstanceRecommendationsResponse_s3Destination,
    exportEC2InstanceRecommendationsResponse_httpStatus,

    -- ** ExportECSServiceRecommendations
    exportECSServiceRecommendations_accountIds,
    exportECSServiceRecommendations_fieldsToExport,
    exportECSServiceRecommendations_fileFormat,
    exportECSServiceRecommendations_filters,
    exportECSServiceRecommendations_includeMemberAccounts,
    exportECSServiceRecommendations_s3DestinationConfig,
    exportECSServiceRecommendationsResponse_jobId,
    exportECSServiceRecommendationsResponse_s3Destination,
    exportECSServiceRecommendationsResponse_httpStatus,

    -- ** ExportLambdaFunctionRecommendations
    exportLambdaFunctionRecommendations_accountIds,
    exportLambdaFunctionRecommendations_fieldsToExport,
    exportLambdaFunctionRecommendations_fileFormat,
    exportLambdaFunctionRecommendations_filters,
    exportLambdaFunctionRecommendations_includeMemberAccounts,
    exportLambdaFunctionRecommendations_s3DestinationConfig,
    exportLambdaFunctionRecommendationsResponse_jobId,
    exportLambdaFunctionRecommendationsResponse_s3Destination,
    exportLambdaFunctionRecommendationsResponse_httpStatus,

    -- ** GetAutoScalingGroupRecommendations
    getAutoScalingGroupRecommendations_accountIds,
    getAutoScalingGroupRecommendations_autoScalingGroupArns,
    getAutoScalingGroupRecommendations_filters,
    getAutoScalingGroupRecommendations_maxResults,
    getAutoScalingGroupRecommendations_nextToken,
    getAutoScalingGroupRecommendations_recommendationPreferences,
    getAutoScalingGroupRecommendationsResponse_autoScalingGroupRecommendations,
    getAutoScalingGroupRecommendationsResponse_errors,
    getAutoScalingGroupRecommendationsResponse_nextToken,
    getAutoScalingGroupRecommendationsResponse_httpStatus,

    -- ** GetEBSVolumeRecommendations
    getEBSVolumeRecommendations_accountIds,
    getEBSVolumeRecommendations_filters,
    getEBSVolumeRecommendations_maxResults,
    getEBSVolumeRecommendations_nextToken,
    getEBSVolumeRecommendations_volumeArns,
    getEBSVolumeRecommendationsResponse_errors,
    getEBSVolumeRecommendationsResponse_nextToken,
    getEBSVolumeRecommendationsResponse_volumeRecommendations,
    getEBSVolumeRecommendationsResponse_httpStatus,

    -- ** GetEC2InstanceRecommendations
    getEC2InstanceRecommendations_accountIds,
    getEC2InstanceRecommendations_filters,
    getEC2InstanceRecommendations_instanceArns,
    getEC2InstanceRecommendations_maxResults,
    getEC2InstanceRecommendations_nextToken,
    getEC2InstanceRecommendations_recommendationPreferences,
    getEC2InstanceRecommendationsResponse_errors,
    getEC2InstanceRecommendationsResponse_instanceRecommendations,
    getEC2InstanceRecommendationsResponse_nextToken,
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

    -- ** GetECSServiceRecommendationProjectedMetrics
    getECSServiceRecommendationProjectedMetrics_serviceArn,
    getECSServiceRecommendationProjectedMetrics_stat,
    getECSServiceRecommendationProjectedMetrics_period,
    getECSServiceRecommendationProjectedMetrics_startTime,
    getECSServiceRecommendationProjectedMetrics_endTime,
    getECSServiceRecommendationProjectedMetricsResponse_recommendedOptionProjectedMetrics,
    getECSServiceRecommendationProjectedMetricsResponse_httpStatus,

    -- ** GetECSServiceRecommendations
    getECSServiceRecommendations_accountIds,
    getECSServiceRecommendations_filters,
    getECSServiceRecommendations_maxResults,
    getECSServiceRecommendations_nextToken,
    getECSServiceRecommendations_serviceArns,
    getECSServiceRecommendationsResponse_ecsServiceRecommendations,
    getECSServiceRecommendationsResponse_errors,
    getECSServiceRecommendationsResponse_nextToken,
    getECSServiceRecommendationsResponse_httpStatus,

    -- ** GetEffectiveRecommendationPreferences
    getEffectiveRecommendationPreferences_resourceArn,
    getEffectiveRecommendationPreferencesResponse_enhancedInfrastructureMetrics,
    getEffectiveRecommendationPreferencesResponse_externalMetricsPreference,
    getEffectiveRecommendationPreferencesResponse_httpStatus,

    -- ** GetEnrollmentStatus
    getEnrollmentStatusResponse_lastUpdatedTimestamp,
    getEnrollmentStatusResponse_memberAccountsEnrolled,
    getEnrollmentStatusResponse_numberOfMemberAccountsOptedIn,
    getEnrollmentStatusResponse_status,
    getEnrollmentStatusResponse_statusReason,
    getEnrollmentStatusResponse_httpStatus,

    -- ** GetEnrollmentStatusesForOrganization
    getEnrollmentStatusesForOrganization_filters,
    getEnrollmentStatusesForOrganization_maxResults,
    getEnrollmentStatusesForOrganization_nextToken,
    getEnrollmentStatusesForOrganizationResponse_accountEnrollmentStatuses,
    getEnrollmentStatusesForOrganizationResponse_nextToken,
    getEnrollmentStatusesForOrganizationResponse_httpStatus,

    -- ** GetLambdaFunctionRecommendations
    getLambdaFunctionRecommendations_accountIds,
    getLambdaFunctionRecommendations_filters,
    getLambdaFunctionRecommendations_functionArns,
    getLambdaFunctionRecommendations_maxResults,
    getLambdaFunctionRecommendations_nextToken,
    getLambdaFunctionRecommendationsResponse_lambdaFunctionRecommendations,
    getLambdaFunctionRecommendationsResponse_nextToken,
    getLambdaFunctionRecommendationsResponse_httpStatus,

    -- ** GetRecommendationPreferences
    getRecommendationPreferences_maxResults,
    getRecommendationPreferences_nextToken,
    getRecommendationPreferences_scope,
    getRecommendationPreferences_resourceType,
    getRecommendationPreferencesResponse_nextToken,
    getRecommendationPreferencesResponse_recommendationPreferencesDetails,
    getRecommendationPreferencesResponse_httpStatus,

    -- ** GetRecommendationSummaries
    getRecommendationSummaries_accountIds,
    getRecommendationSummaries_maxResults,
    getRecommendationSummaries_nextToken,
    getRecommendationSummariesResponse_nextToken,
    getRecommendationSummariesResponse_recommendationSummaries,
    getRecommendationSummariesResponse_httpStatus,

    -- ** PutRecommendationPreferences
    putRecommendationPreferences_enhancedInfrastructureMetrics,
    putRecommendationPreferences_externalMetricsPreference,
    putRecommendationPreferences_inferredWorkloadTypes,
    putRecommendationPreferences_scope,
    putRecommendationPreferences_resourceType,
    putRecommendationPreferencesResponse_httpStatus,

    -- ** UpdateEnrollmentStatus
    updateEnrollmentStatus_includeMemberAccounts,
    updateEnrollmentStatus_status,
    updateEnrollmentStatusResponse_status,
    updateEnrollmentStatusResponse_statusReason,
    updateEnrollmentStatusResponse_httpStatus,

    -- * Types

    -- ** AccountEnrollmentStatus
    accountEnrollmentStatus_accountId,
    accountEnrollmentStatus_lastUpdatedTimestamp,
    accountEnrollmentStatus_status,
    accountEnrollmentStatus_statusReason,

    -- ** AutoScalingGroupConfiguration
    autoScalingGroupConfiguration_desiredCapacity,
    autoScalingGroupConfiguration_instanceType,
    autoScalingGroupConfiguration_maxSize,
    autoScalingGroupConfiguration_minSize,

    -- ** AutoScalingGroupRecommendation
    autoScalingGroupRecommendation_accountId,
    autoScalingGroupRecommendation_autoScalingGroupArn,
    autoScalingGroupRecommendation_autoScalingGroupName,
    autoScalingGroupRecommendation_currentConfiguration,
    autoScalingGroupRecommendation_currentPerformanceRisk,
    autoScalingGroupRecommendation_effectiveRecommendationPreferences,
    autoScalingGroupRecommendation_finding,
    autoScalingGroupRecommendation_inferredWorkloadTypes,
    autoScalingGroupRecommendation_lastRefreshTimestamp,
    autoScalingGroupRecommendation_lookBackPeriodInDays,
    autoScalingGroupRecommendation_recommendationOptions,
    autoScalingGroupRecommendation_utilizationMetrics,

    -- ** AutoScalingGroupRecommendationOption
    autoScalingGroupRecommendationOption_configuration,
    autoScalingGroupRecommendationOption_migrationEffort,
    autoScalingGroupRecommendationOption_performanceRisk,
    autoScalingGroupRecommendationOption_projectedUtilizationMetrics,
    autoScalingGroupRecommendationOption_rank,
    autoScalingGroupRecommendationOption_savingsOpportunity,

    -- ** ContainerConfiguration
    containerConfiguration_containerName,
    containerConfiguration_cpu,
    containerConfiguration_memorySizeConfiguration,

    -- ** ContainerRecommendation
    containerRecommendation_containerName,
    containerRecommendation_cpu,
    containerRecommendation_memorySizeConfiguration,

    -- ** CurrentPerformanceRiskRatings
    currentPerformanceRiskRatings_high,
    currentPerformanceRiskRatings_low,
    currentPerformanceRiskRatings_medium,
    currentPerformanceRiskRatings_veryLow,

    -- ** EBSFilter
    eBSFilter_name,
    eBSFilter_values,

    -- ** EBSUtilizationMetric
    eBSUtilizationMetric_name,
    eBSUtilizationMetric_statistic,
    eBSUtilizationMetric_value,

    -- ** ECSServiceProjectedMetric
    eCSServiceProjectedMetric_lowerBoundValues,
    eCSServiceProjectedMetric_name,
    eCSServiceProjectedMetric_timestamps,
    eCSServiceProjectedMetric_upperBoundValues,

    -- ** ECSServiceProjectedUtilizationMetric
    eCSServiceProjectedUtilizationMetric_lowerBoundValue,
    eCSServiceProjectedUtilizationMetric_name,
    eCSServiceProjectedUtilizationMetric_statistic,
    eCSServiceProjectedUtilizationMetric_upperBoundValue,

    -- ** ECSServiceRecommendation
    eCSServiceRecommendation_accountId,
    eCSServiceRecommendation_currentPerformanceRisk,
    eCSServiceRecommendation_currentServiceConfiguration,
    eCSServiceRecommendation_finding,
    eCSServiceRecommendation_findingReasonCodes,
    eCSServiceRecommendation_lastRefreshTimestamp,
    eCSServiceRecommendation_launchType,
    eCSServiceRecommendation_lookbackPeriodInDays,
    eCSServiceRecommendation_serviceArn,
    eCSServiceRecommendation_serviceRecommendationOptions,
    eCSServiceRecommendation_utilizationMetrics,

    -- ** ECSServiceRecommendationFilter
    eCSServiceRecommendationFilter_name,
    eCSServiceRecommendationFilter_values,

    -- ** ECSServiceRecommendationOption
    eCSServiceRecommendationOption_containerRecommendations,
    eCSServiceRecommendationOption_cpu,
    eCSServiceRecommendationOption_memory,
    eCSServiceRecommendationOption_projectedUtilizationMetrics,
    eCSServiceRecommendationOption_savingsOpportunity,

    -- ** ECSServiceRecommendedOptionProjectedMetric
    eCSServiceRecommendedOptionProjectedMetric_projectedMetrics,
    eCSServiceRecommendedOptionProjectedMetric_recommendedCpuUnits,
    eCSServiceRecommendedOptionProjectedMetric_recommendedMemorySize,

    -- ** ECSServiceUtilizationMetric
    eCSServiceUtilizationMetric_name,
    eCSServiceUtilizationMetric_statistic,
    eCSServiceUtilizationMetric_value,

    -- ** EffectiveRecommendationPreferences
    effectiveRecommendationPreferences_cpuVendorArchitectures,
    effectiveRecommendationPreferences_enhancedInfrastructureMetrics,
    effectiveRecommendationPreferences_externalMetricsPreference,
    effectiveRecommendationPreferences_inferredWorkloadTypes,

    -- ** EnrollmentFilter
    enrollmentFilter_name,
    enrollmentFilter_values,

    -- ** EstimatedMonthlySavings
    estimatedMonthlySavings_currency,
    estimatedMonthlySavings_value,

    -- ** ExportDestination
    exportDestination_s3,

    -- ** ExternalMetricsPreference
    externalMetricsPreference_source,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** GetRecommendationError
    getRecommendationError_code,
    getRecommendationError_identifier,
    getRecommendationError_message,

    -- ** InstanceRecommendation
    instanceRecommendation_accountId,
    instanceRecommendation_currentInstanceType,
    instanceRecommendation_currentPerformanceRisk,
    instanceRecommendation_effectiveRecommendationPreferences,
    instanceRecommendation_finding,
    instanceRecommendation_findingReasonCodes,
    instanceRecommendation_inferredWorkloadTypes,
    instanceRecommendation_instanceArn,
    instanceRecommendation_instanceName,
    instanceRecommendation_lastRefreshTimestamp,
    instanceRecommendation_lookBackPeriodInDays,
    instanceRecommendation_recommendationOptions,
    instanceRecommendation_recommendationSources,
    instanceRecommendation_utilizationMetrics,

    -- ** InstanceRecommendationOption
    instanceRecommendationOption_instanceType,
    instanceRecommendationOption_migrationEffort,
    instanceRecommendationOption_performanceRisk,
    instanceRecommendationOption_platformDifferences,
    instanceRecommendationOption_projectedUtilizationMetrics,
    instanceRecommendationOption_rank,
    instanceRecommendationOption_savingsOpportunity,

    -- ** JobFilter
    jobFilter_name,
    jobFilter_values,

    -- ** LambdaFunctionMemoryProjectedMetric
    lambdaFunctionMemoryProjectedMetric_name,
    lambdaFunctionMemoryProjectedMetric_statistic,
    lambdaFunctionMemoryProjectedMetric_value,

    -- ** LambdaFunctionMemoryRecommendationOption
    lambdaFunctionMemoryRecommendationOption_memorySize,
    lambdaFunctionMemoryRecommendationOption_projectedUtilizationMetrics,
    lambdaFunctionMemoryRecommendationOption_rank,
    lambdaFunctionMemoryRecommendationOption_savingsOpportunity,

    -- ** LambdaFunctionRecommendation
    lambdaFunctionRecommendation_accountId,
    lambdaFunctionRecommendation_currentMemorySize,
    lambdaFunctionRecommendation_currentPerformanceRisk,
    lambdaFunctionRecommendation_finding,
    lambdaFunctionRecommendation_findingReasonCodes,
    lambdaFunctionRecommendation_functionArn,
    lambdaFunctionRecommendation_functionVersion,
    lambdaFunctionRecommendation_lastRefreshTimestamp,
    lambdaFunctionRecommendation_lookbackPeriodInDays,
    lambdaFunctionRecommendation_memorySizeRecommendationOptions,
    lambdaFunctionRecommendation_numberOfInvocations,
    lambdaFunctionRecommendation_utilizationMetrics,

    -- ** LambdaFunctionRecommendationFilter
    lambdaFunctionRecommendationFilter_name,
    lambdaFunctionRecommendationFilter_values,

    -- ** LambdaFunctionUtilizationMetric
    lambdaFunctionUtilizationMetric_name,
    lambdaFunctionUtilizationMetric_statistic,
    lambdaFunctionUtilizationMetric_value,

    -- ** MemorySizeConfiguration
    memorySizeConfiguration_memory,
    memorySizeConfiguration_memoryReservation,

    -- ** ProjectedMetric
    projectedMetric_name,
    projectedMetric_timestamps,
    projectedMetric_values,

    -- ** ReasonCodeSummary
    reasonCodeSummary_name,
    reasonCodeSummary_value,

    -- ** RecommendationExportJob
    recommendationExportJob_creationTimestamp,
    recommendationExportJob_destination,
    recommendationExportJob_failureReason,
    recommendationExportJob_jobId,
    recommendationExportJob_lastUpdatedTimestamp,
    recommendationExportJob_resourceType,
    recommendationExportJob_status,

    -- ** RecommendationPreferences
    recommendationPreferences_cpuVendorArchitectures,

    -- ** RecommendationPreferencesDetail
    recommendationPreferencesDetail_enhancedInfrastructureMetrics,
    recommendationPreferencesDetail_externalMetricsPreference,
    recommendationPreferencesDetail_inferredWorkloadTypes,
    recommendationPreferencesDetail_resourceType,
    recommendationPreferencesDetail_scope,

    -- ** RecommendationSource
    recommendationSource_recommendationSourceArn,
    recommendationSource_recommendationSourceType,

    -- ** RecommendationSummary
    recommendationSummary_accountId,
    recommendationSummary_currentPerformanceRiskRatings,
    recommendationSummary_recommendationResourceType,
    recommendationSummary_savingsOpportunity,
    recommendationSummary_summaries,

    -- ** RecommendedOptionProjectedMetric
    recommendedOptionProjectedMetric_projectedMetrics,
    recommendedOptionProjectedMetric_rank,
    recommendedOptionProjectedMetric_recommendedInstanceType,

    -- ** S3Destination
    s3Destination_bucket,
    s3Destination_key,
    s3Destination_metadataKey,

    -- ** S3DestinationConfig
    s3DestinationConfig_bucket,
    s3DestinationConfig_keyPrefix,

    -- ** SavingsOpportunity
    savingsOpportunity_estimatedMonthlySavings,
    savingsOpportunity_savingsOpportunityPercentage,

    -- ** Scope
    scope_name,
    scope_value,

    -- ** ServiceConfiguration
    serviceConfiguration_autoScalingConfiguration,
    serviceConfiguration_containerConfigurations,
    serviceConfiguration_cpu,
    serviceConfiguration_memory,
    serviceConfiguration_taskDefinitionArn,

    -- ** Summary
    summary_name,
    summary_reasonCodeSummaries,
    summary_value,

    -- ** UtilizationMetric
    utilizationMetric_name,
    utilizationMetric_statistic,
    utilizationMetric_value,

    -- ** VolumeConfiguration
    volumeConfiguration_volumeBaselineIOPS,
    volumeConfiguration_volumeBaselineThroughput,
    volumeConfiguration_volumeBurstIOPS,
    volumeConfiguration_volumeBurstThroughput,
    volumeConfiguration_volumeSize,
    volumeConfiguration_volumeType,

    -- ** VolumeRecommendation
    volumeRecommendation_accountId,
    volumeRecommendation_currentConfiguration,
    volumeRecommendation_currentPerformanceRisk,
    volumeRecommendation_finding,
    volumeRecommendation_lastRefreshTimestamp,
    volumeRecommendation_lookBackPeriodInDays,
    volumeRecommendation_utilizationMetrics,
    volumeRecommendation_volumeArn,
    volumeRecommendation_volumeRecommendationOptions,

    -- ** VolumeRecommendationOption
    volumeRecommendationOption_configuration,
    volumeRecommendationOption_performanceRisk,
    volumeRecommendationOption_rank,
    volumeRecommendationOption_savingsOpportunity,
  )
where

import Amazonka.ComputeOptimizer.DeleteRecommendationPreferences
import Amazonka.ComputeOptimizer.DescribeRecommendationExportJobs
import Amazonka.ComputeOptimizer.ExportAutoScalingGroupRecommendations
import Amazonka.ComputeOptimizer.ExportEBSVolumeRecommendations
import Amazonka.ComputeOptimizer.ExportEC2InstanceRecommendations
import Amazonka.ComputeOptimizer.ExportECSServiceRecommendations
import Amazonka.ComputeOptimizer.ExportLambdaFunctionRecommendations
import Amazonka.ComputeOptimizer.GetAutoScalingGroupRecommendations
import Amazonka.ComputeOptimizer.GetEBSVolumeRecommendations
import Amazonka.ComputeOptimizer.GetEC2InstanceRecommendations
import Amazonka.ComputeOptimizer.GetEC2RecommendationProjectedMetrics
import Amazonka.ComputeOptimizer.GetECSServiceRecommendationProjectedMetrics
import Amazonka.ComputeOptimizer.GetECSServiceRecommendations
import Amazonka.ComputeOptimizer.GetEffectiveRecommendationPreferences
import Amazonka.ComputeOptimizer.GetEnrollmentStatus
import Amazonka.ComputeOptimizer.GetEnrollmentStatusesForOrganization
import Amazonka.ComputeOptimizer.GetLambdaFunctionRecommendations
import Amazonka.ComputeOptimizer.GetRecommendationPreferences
import Amazonka.ComputeOptimizer.GetRecommendationSummaries
import Amazonka.ComputeOptimizer.PutRecommendationPreferences
import Amazonka.ComputeOptimizer.Types.AccountEnrollmentStatus
import Amazonka.ComputeOptimizer.Types.AutoScalingGroupConfiguration
import Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendation
import Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendationOption
import Amazonka.ComputeOptimizer.Types.ContainerConfiguration
import Amazonka.ComputeOptimizer.Types.ContainerRecommendation
import Amazonka.ComputeOptimizer.Types.CurrentPerformanceRiskRatings
import Amazonka.ComputeOptimizer.Types.EBSFilter
import Amazonka.ComputeOptimizer.Types.EBSUtilizationMetric
import Amazonka.ComputeOptimizer.Types.ECSServiceProjectedMetric
import Amazonka.ComputeOptimizer.Types.ECSServiceProjectedUtilizationMetric
import Amazonka.ComputeOptimizer.Types.ECSServiceRecommendation
import Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFilter
import Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationOption
import Amazonka.ComputeOptimizer.Types.ECSServiceRecommendedOptionProjectedMetric
import Amazonka.ComputeOptimizer.Types.ECSServiceUtilizationMetric
import Amazonka.ComputeOptimizer.Types.EffectiveRecommendationPreferences
import Amazonka.ComputeOptimizer.Types.EnrollmentFilter
import Amazonka.ComputeOptimizer.Types.EstimatedMonthlySavings
import Amazonka.ComputeOptimizer.Types.ExportDestination
import Amazonka.ComputeOptimizer.Types.ExternalMetricsPreference
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
import Amazonka.ComputeOptimizer.Types.MemorySizeConfiguration
import Amazonka.ComputeOptimizer.Types.ProjectedMetric
import Amazonka.ComputeOptimizer.Types.ReasonCodeSummary
import Amazonka.ComputeOptimizer.Types.RecommendationExportJob
import Amazonka.ComputeOptimizer.Types.RecommendationPreferences
import Amazonka.ComputeOptimizer.Types.RecommendationPreferencesDetail
import Amazonka.ComputeOptimizer.Types.RecommendationSource
import Amazonka.ComputeOptimizer.Types.RecommendationSummary
import Amazonka.ComputeOptimizer.Types.RecommendedOptionProjectedMetric
import Amazonka.ComputeOptimizer.Types.S3Destination
import Amazonka.ComputeOptimizer.Types.S3DestinationConfig
import Amazonka.ComputeOptimizer.Types.SavingsOpportunity
import Amazonka.ComputeOptimizer.Types.Scope
import Amazonka.ComputeOptimizer.Types.ServiceConfiguration
import Amazonka.ComputeOptimizer.Types.Summary
import Amazonka.ComputeOptimizer.Types.UtilizationMetric
import Amazonka.ComputeOptimizer.Types.VolumeConfiguration
import Amazonka.ComputeOptimizer.Types.VolumeRecommendation
import Amazonka.ComputeOptimizer.Types.VolumeRecommendationOption
import Amazonka.ComputeOptimizer.UpdateEnrollmentStatus
