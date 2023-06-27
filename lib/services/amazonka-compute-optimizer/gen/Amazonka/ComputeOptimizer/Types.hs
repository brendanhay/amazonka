{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ComputeOptimizer.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    _InvalidParameterValueException,
    _LimitExceededException,
    _MissingAuthenticationToken,
    _OptInRequiredException,
    _ResourceNotFoundException,
    _ServiceUnavailableException,
    _ThrottlingException,

    -- * AutoScalingConfiguration
    AutoScalingConfiguration (..),

    -- * CpuVendorArchitecture
    CpuVendorArchitecture (..),

    -- * Currency
    Currency (..),

    -- * CurrentPerformanceRisk
    CurrentPerformanceRisk (..),

    -- * EBSFilterName
    EBSFilterName (..),

    -- * EBSFinding
    EBSFinding (..),

    -- * EBSMetricName
    EBSMetricName (..),

    -- * ECSServiceLaunchType
    ECSServiceLaunchType (..),

    -- * ECSServiceMetricName
    ECSServiceMetricName (..),

    -- * ECSServiceMetricStatistic
    ECSServiceMetricStatistic (..),

    -- * ECSServiceRecommendationFilterName
    ECSServiceRecommendationFilterName (..),

    -- * ECSServiceRecommendationFinding
    ECSServiceRecommendationFinding (..),

    -- * ECSServiceRecommendationFindingReasonCode
    ECSServiceRecommendationFindingReasonCode (..),

    -- * EnhancedInfrastructureMetrics
    EnhancedInfrastructureMetrics (..),

    -- * EnrollmentFilterName
    EnrollmentFilterName (..),

    -- * ExportableAutoScalingGroupField
    ExportableAutoScalingGroupField (..),

    -- * ExportableECSServiceField
    ExportableECSServiceField (..),

    -- * ExportableInstanceField
    ExportableInstanceField (..),

    -- * ExportableLambdaFunctionField
    ExportableLambdaFunctionField (..),

    -- * ExportableVolumeField
    ExportableVolumeField (..),

    -- * ExternalMetricStatusCode
    ExternalMetricStatusCode (..),

    -- * ExternalMetricsSource
    ExternalMetricsSource (..),

    -- * FileFormat
    FileFormat (..),

    -- * FilterName
    FilterName (..),

    -- * Finding
    Finding (..),

    -- * FindingReasonCode
    FindingReasonCode (..),

    -- * InferredWorkloadType
    InferredWorkloadType (..),

    -- * InferredWorkloadTypesPreference
    InferredWorkloadTypesPreference (..),

    -- * InstanceRecommendationFindingReasonCode
    InstanceRecommendationFindingReasonCode (..),

    -- * InstanceState
    InstanceState (..),

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

    -- * MigrationEffort
    MigrationEffort (..),

    -- * PlatformDifference
    PlatformDifference (..),

    -- * RecommendationPreferenceName
    RecommendationPreferenceName (..),

    -- * RecommendationSourceType
    RecommendationSourceType (..),

    -- * ResourceType
    ResourceType (..),

    -- * ScopeName
    ScopeName (..),

    -- * Status
    Status (..),

    -- * AccountEnrollmentStatus
    AccountEnrollmentStatus (..),
    newAccountEnrollmentStatus,
    accountEnrollmentStatus_accountId,
    accountEnrollmentStatus_lastUpdatedTimestamp,
    accountEnrollmentStatus_status,
    accountEnrollmentStatus_statusReason,

    -- * AutoScalingGroupConfiguration
    AutoScalingGroupConfiguration (..),
    newAutoScalingGroupConfiguration,
    autoScalingGroupConfiguration_desiredCapacity,
    autoScalingGroupConfiguration_instanceType,
    autoScalingGroupConfiguration_maxSize,
    autoScalingGroupConfiguration_minSize,

    -- * AutoScalingGroupRecommendation
    AutoScalingGroupRecommendation (..),
    newAutoScalingGroupRecommendation,
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

    -- * AutoScalingGroupRecommendationOption
    AutoScalingGroupRecommendationOption (..),
    newAutoScalingGroupRecommendationOption,
    autoScalingGroupRecommendationOption_configuration,
    autoScalingGroupRecommendationOption_migrationEffort,
    autoScalingGroupRecommendationOption_performanceRisk,
    autoScalingGroupRecommendationOption_projectedUtilizationMetrics,
    autoScalingGroupRecommendationOption_rank,
    autoScalingGroupRecommendationOption_savingsOpportunity,

    -- * ContainerConfiguration
    ContainerConfiguration (..),
    newContainerConfiguration,
    containerConfiguration_containerName,
    containerConfiguration_cpu,
    containerConfiguration_memorySizeConfiguration,

    -- * ContainerRecommendation
    ContainerRecommendation (..),
    newContainerRecommendation,
    containerRecommendation_containerName,
    containerRecommendation_cpu,
    containerRecommendation_memorySizeConfiguration,

    -- * CurrentPerformanceRiskRatings
    CurrentPerformanceRiskRatings (..),
    newCurrentPerformanceRiskRatings,
    currentPerformanceRiskRatings_high,
    currentPerformanceRiskRatings_low,
    currentPerformanceRiskRatings_medium,
    currentPerformanceRiskRatings_veryLow,

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

    -- * ECSServiceProjectedMetric
    ECSServiceProjectedMetric (..),
    newECSServiceProjectedMetric,
    eCSServiceProjectedMetric_lowerBoundValues,
    eCSServiceProjectedMetric_name,
    eCSServiceProjectedMetric_timestamps,
    eCSServiceProjectedMetric_upperBoundValues,

    -- * ECSServiceProjectedUtilizationMetric
    ECSServiceProjectedUtilizationMetric (..),
    newECSServiceProjectedUtilizationMetric,
    eCSServiceProjectedUtilizationMetric_lowerBoundValue,
    eCSServiceProjectedUtilizationMetric_name,
    eCSServiceProjectedUtilizationMetric_statistic,
    eCSServiceProjectedUtilizationMetric_upperBoundValue,

    -- * ECSServiceRecommendation
    ECSServiceRecommendation (..),
    newECSServiceRecommendation,
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
    eCSServiceRecommendation_tags,
    eCSServiceRecommendation_utilizationMetrics,

    -- * ECSServiceRecommendationFilter
    ECSServiceRecommendationFilter (..),
    newECSServiceRecommendationFilter,
    eCSServiceRecommendationFilter_name,
    eCSServiceRecommendationFilter_values,

    -- * ECSServiceRecommendationOption
    ECSServiceRecommendationOption (..),
    newECSServiceRecommendationOption,
    eCSServiceRecommendationOption_containerRecommendations,
    eCSServiceRecommendationOption_cpu,
    eCSServiceRecommendationOption_memory,
    eCSServiceRecommendationOption_projectedUtilizationMetrics,
    eCSServiceRecommendationOption_savingsOpportunity,

    -- * ECSServiceRecommendedOptionProjectedMetric
    ECSServiceRecommendedOptionProjectedMetric (..),
    newECSServiceRecommendedOptionProjectedMetric,
    eCSServiceRecommendedOptionProjectedMetric_projectedMetrics,
    eCSServiceRecommendedOptionProjectedMetric_recommendedCpuUnits,
    eCSServiceRecommendedOptionProjectedMetric_recommendedMemorySize,

    -- * ECSServiceUtilizationMetric
    ECSServiceUtilizationMetric (..),
    newECSServiceUtilizationMetric,
    eCSServiceUtilizationMetric_name,
    eCSServiceUtilizationMetric_statistic,
    eCSServiceUtilizationMetric_value,

    -- * EffectiveRecommendationPreferences
    EffectiveRecommendationPreferences (..),
    newEffectiveRecommendationPreferences,
    effectiveRecommendationPreferences_cpuVendorArchitectures,
    effectiveRecommendationPreferences_enhancedInfrastructureMetrics,
    effectiveRecommendationPreferences_externalMetricsPreference,
    effectiveRecommendationPreferences_inferredWorkloadTypes,

    -- * EnrollmentFilter
    EnrollmentFilter (..),
    newEnrollmentFilter,
    enrollmentFilter_name,
    enrollmentFilter_values,

    -- * EstimatedMonthlySavings
    EstimatedMonthlySavings (..),
    newEstimatedMonthlySavings,
    estimatedMonthlySavings_currency,
    estimatedMonthlySavings_value,

    -- * ExportDestination
    ExportDestination (..),
    newExportDestination,
    exportDestination_s3,

    -- * ExternalMetricStatus
    ExternalMetricStatus (..),
    newExternalMetricStatus,
    externalMetricStatus_statusCode,
    externalMetricStatus_statusReason,

    -- * ExternalMetricsPreference
    ExternalMetricsPreference (..),
    newExternalMetricsPreference,
    externalMetricsPreference_source,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * GetRecommendationError
    GetRecommendationError (..),
    newGetRecommendationError,
    getRecommendationError_code,
    getRecommendationError_identifier,
    getRecommendationError_message,

    -- * InferredWorkloadSaving
    InferredWorkloadSaving (..),
    newInferredWorkloadSaving,
    inferredWorkloadSaving_estimatedMonthlySavings,
    inferredWorkloadSaving_inferredWorkloadTypes,

    -- * InstanceRecommendation
    InstanceRecommendation (..),
    newInstanceRecommendation,
    instanceRecommendation_accountId,
    instanceRecommendation_currentInstanceType,
    instanceRecommendation_currentPerformanceRisk,
    instanceRecommendation_effectiveRecommendationPreferences,
    instanceRecommendation_externalMetricStatus,
    instanceRecommendation_finding,
    instanceRecommendation_findingReasonCodes,
    instanceRecommendation_inferredWorkloadTypes,
    instanceRecommendation_instanceArn,
    instanceRecommendation_instanceName,
    instanceRecommendation_instanceState,
    instanceRecommendation_lastRefreshTimestamp,
    instanceRecommendation_lookBackPeriodInDays,
    instanceRecommendation_recommendationOptions,
    instanceRecommendation_recommendationSources,
    instanceRecommendation_tags,
    instanceRecommendation_utilizationMetrics,

    -- * InstanceRecommendationOption
    InstanceRecommendationOption (..),
    newInstanceRecommendationOption,
    instanceRecommendationOption_instanceType,
    instanceRecommendationOption_migrationEffort,
    instanceRecommendationOption_performanceRisk,
    instanceRecommendationOption_platformDifferences,
    instanceRecommendationOption_projectedUtilizationMetrics,
    instanceRecommendationOption_rank,
    instanceRecommendationOption_savingsOpportunity,

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
    lambdaFunctionMemoryRecommendationOption_projectedUtilizationMetrics,
    lambdaFunctionMemoryRecommendationOption_rank,
    lambdaFunctionMemoryRecommendationOption_savingsOpportunity,

    -- * LambdaFunctionRecommendation
    LambdaFunctionRecommendation (..),
    newLambdaFunctionRecommendation,
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
    lambdaFunctionRecommendation_tags,
    lambdaFunctionRecommendation_utilizationMetrics,

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

    -- * MemorySizeConfiguration
    MemorySizeConfiguration (..),
    newMemorySizeConfiguration,
    memorySizeConfiguration_memory,
    memorySizeConfiguration_memoryReservation,

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
    recommendationExportJob_creationTimestamp,
    recommendationExportJob_destination,
    recommendationExportJob_failureReason,
    recommendationExportJob_jobId,
    recommendationExportJob_lastUpdatedTimestamp,
    recommendationExportJob_resourceType,
    recommendationExportJob_status,

    -- * RecommendationPreferences
    RecommendationPreferences (..),
    newRecommendationPreferences,
    recommendationPreferences_cpuVendorArchitectures,

    -- * RecommendationPreferencesDetail
    RecommendationPreferencesDetail (..),
    newRecommendationPreferencesDetail,
    recommendationPreferencesDetail_enhancedInfrastructureMetrics,
    recommendationPreferencesDetail_externalMetricsPreference,
    recommendationPreferencesDetail_inferredWorkloadTypes,
    recommendationPreferencesDetail_resourceType,
    recommendationPreferencesDetail_scope,

    -- * RecommendationSource
    RecommendationSource (..),
    newRecommendationSource,
    recommendationSource_recommendationSourceArn,
    recommendationSource_recommendationSourceType,

    -- * RecommendationSummary
    RecommendationSummary (..),
    newRecommendationSummary,
    recommendationSummary_accountId,
    recommendationSummary_currentPerformanceRiskRatings,
    recommendationSummary_inferredWorkloadSavings,
    recommendationSummary_recommendationResourceType,
    recommendationSummary_savingsOpportunity,
    recommendationSummary_summaries,

    -- * RecommendedOptionProjectedMetric
    RecommendedOptionProjectedMetric (..),
    newRecommendedOptionProjectedMetric,
    recommendedOptionProjectedMetric_projectedMetrics,
    recommendedOptionProjectedMetric_rank,
    recommendedOptionProjectedMetric_recommendedInstanceType,

    -- * S3Destination
    S3Destination (..),
    newS3Destination,
    s3Destination_bucket,
    s3Destination_key,
    s3Destination_metadataKey,

    -- * S3DestinationConfig
    S3DestinationConfig (..),
    newS3DestinationConfig,
    s3DestinationConfig_bucket,
    s3DestinationConfig_keyPrefix,

    -- * SavingsOpportunity
    SavingsOpportunity (..),
    newSavingsOpportunity,
    savingsOpportunity_estimatedMonthlySavings,
    savingsOpportunity_savingsOpportunityPercentage,

    -- * Scope
    Scope (..),
    newScope,
    scope_name,
    scope_value,

    -- * ServiceConfiguration
    ServiceConfiguration (..),
    newServiceConfiguration,
    serviceConfiguration_autoScalingConfiguration,
    serviceConfiguration_containerConfigurations,
    serviceConfiguration_cpu,
    serviceConfiguration_memory,
    serviceConfiguration_taskDefinitionArn,

    -- * Summary
    Summary (..),
    newSummary,
    summary_name,
    summary_reasonCodeSummaries,
    summary_value,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UtilizationMetric
    UtilizationMetric (..),
    newUtilizationMetric,
    utilizationMetric_name,
    utilizationMetric_statistic,
    utilizationMetric_value,

    -- * VolumeConfiguration
    VolumeConfiguration (..),
    newVolumeConfiguration,
    volumeConfiguration_rootVolume,
    volumeConfiguration_volumeBaselineIOPS,
    volumeConfiguration_volumeBaselineThroughput,
    volumeConfiguration_volumeBurstIOPS,
    volumeConfiguration_volumeBurstThroughput,
    volumeConfiguration_volumeSize,
    volumeConfiguration_volumeType,

    -- * VolumeRecommendation
    VolumeRecommendation (..),
    newVolumeRecommendation,
    volumeRecommendation_accountId,
    volumeRecommendation_currentConfiguration,
    volumeRecommendation_currentPerformanceRisk,
    volumeRecommendation_finding,
    volumeRecommendation_lastRefreshTimestamp,
    volumeRecommendation_lookBackPeriodInDays,
    volumeRecommendation_tags,
    volumeRecommendation_utilizationMetrics,
    volumeRecommendation_volumeArn,
    volumeRecommendation_volumeRecommendationOptions,

    -- * VolumeRecommendationOption
    VolumeRecommendationOption (..),
    newVolumeRecommendationOption,
    volumeRecommendationOption_configuration,
    volumeRecommendationOption_performanceRisk,
    volumeRecommendationOption_rank,
    volumeRecommendationOption_savingsOpportunity,
  )
where

import Amazonka.ComputeOptimizer.Types.AccountEnrollmentStatus
import Amazonka.ComputeOptimizer.Types.AutoScalingConfiguration
import Amazonka.ComputeOptimizer.Types.AutoScalingGroupConfiguration
import Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendation
import Amazonka.ComputeOptimizer.Types.AutoScalingGroupRecommendationOption
import Amazonka.ComputeOptimizer.Types.ContainerConfiguration
import Amazonka.ComputeOptimizer.Types.ContainerRecommendation
import Amazonka.ComputeOptimizer.Types.CpuVendorArchitecture
import Amazonka.ComputeOptimizer.Types.Currency
import Amazonka.ComputeOptimizer.Types.CurrentPerformanceRisk
import Amazonka.ComputeOptimizer.Types.CurrentPerformanceRiskRatings
import Amazonka.ComputeOptimizer.Types.EBSFilter
import Amazonka.ComputeOptimizer.Types.EBSFilterName
import Amazonka.ComputeOptimizer.Types.EBSFinding
import Amazonka.ComputeOptimizer.Types.EBSMetricName
import Amazonka.ComputeOptimizer.Types.EBSUtilizationMetric
import Amazonka.ComputeOptimizer.Types.ECSServiceLaunchType
import Amazonka.ComputeOptimizer.Types.ECSServiceMetricName
import Amazonka.ComputeOptimizer.Types.ECSServiceMetricStatistic
import Amazonka.ComputeOptimizer.Types.ECSServiceProjectedMetric
import Amazonka.ComputeOptimizer.Types.ECSServiceProjectedUtilizationMetric
import Amazonka.ComputeOptimizer.Types.ECSServiceRecommendation
import Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFilter
import Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFilterName
import Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFinding
import Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFindingReasonCode
import Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationOption
import Amazonka.ComputeOptimizer.Types.ECSServiceRecommendedOptionProjectedMetric
import Amazonka.ComputeOptimizer.Types.ECSServiceUtilizationMetric
import Amazonka.ComputeOptimizer.Types.EffectiveRecommendationPreferences
import Amazonka.ComputeOptimizer.Types.EnhancedInfrastructureMetrics
import Amazonka.ComputeOptimizer.Types.EnrollmentFilter
import Amazonka.ComputeOptimizer.Types.EnrollmentFilterName
import Amazonka.ComputeOptimizer.Types.EstimatedMonthlySavings
import Amazonka.ComputeOptimizer.Types.ExportDestination
import Amazonka.ComputeOptimizer.Types.ExportableAutoScalingGroupField
import Amazonka.ComputeOptimizer.Types.ExportableECSServiceField
import Amazonka.ComputeOptimizer.Types.ExportableInstanceField
import Amazonka.ComputeOptimizer.Types.ExportableLambdaFunctionField
import Amazonka.ComputeOptimizer.Types.ExportableVolumeField
import Amazonka.ComputeOptimizer.Types.ExternalMetricStatus
import Amazonka.ComputeOptimizer.Types.ExternalMetricStatusCode
import Amazonka.ComputeOptimizer.Types.ExternalMetricsPreference
import Amazonka.ComputeOptimizer.Types.ExternalMetricsSource
import Amazonka.ComputeOptimizer.Types.FileFormat
import Amazonka.ComputeOptimizer.Types.Filter
import Amazonka.ComputeOptimizer.Types.FilterName
import Amazonka.ComputeOptimizer.Types.Finding
import Amazonka.ComputeOptimizer.Types.FindingReasonCode
import Amazonka.ComputeOptimizer.Types.GetRecommendationError
import Amazonka.ComputeOptimizer.Types.InferredWorkloadSaving
import Amazonka.ComputeOptimizer.Types.InferredWorkloadType
import Amazonka.ComputeOptimizer.Types.InferredWorkloadTypesPreference
import Amazonka.ComputeOptimizer.Types.InstanceRecommendation
import Amazonka.ComputeOptimizer.Types.InstanceRecommendationFindingReasonCode
import Amazonka.ComputeOptimizer.Types.InstanceRecommendationOption
import Amazonka.ComputeOptimizer.Types.InstanceState
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
import Amazonka.ComputeOptimizer.Types.MemorySizeConfiguration
import Amazonka.ComputeOptimizer.Types.MetricName
import Amazonka.ComputeOptimizer.Types.MetricStatistic
import Amazonka.ComputeOptimizer.Types.MigrationEffort
import Amazonka.ComputeOptimizer.Types.PlatformDifference
import Amazonka.ComputeOptimizer.Types.ProjectedMetric
import Amazonka.ComputeOptimizer.Types.ReasonCodeSummary
import Amazonka.ComputeOptimizer.Types.RecommendationExportJob
import Amazonka.ComputeOptimizer.Types.RecommendationPreferenceName
import Amazonka.ComputeOptimizer.Types.RecommendationPreferences
import Amazonka.ComputeOptimizer.Types.RecommendationPreferencesDetail
import Amazonka.ComputeOptimizer.Types.RecommendationSource
import Amazonka.ComputeOptimizer.Types.RecommendationSourceType
import Amazonka.ComputeOptimizer.Types.RecommendationSummary
import Amazonka.ComputeOptimizer.Types.RecommendedOptionProjectedMetric
import Amazonka.ComputeOptimizer.Types.ResourceType
import Amazonka.ComputeOptimizer.Types.S3Destination
import Amazonka.ComputeOptimizer.Types.S3DestinationConfig
import Amazonka.ComputeOptimizer.Types.SavingsOpportunity
import Amazonka.ComputeOptimizer.Types.Scope
import Amazonka.ComputeOptimizer.Types.ScopeName
import Amazonka.ComputeOptimizer.Types.ServiceConfiguration
import Amazonka.ComputeOptimizer.Types.Status
import Amazonka.ComputeOptimizer.Types.Summary
import Amazonka.ComputeOptimizer.Types.Tag
import Amazonka.ComputeOptimizer.Types.UtilizationMetric
import Amazonka.ComputeOptimizer.Types.VolumeConfiguration
import Amazonka.ComputeOptimizer.Types.VolumeRecommendation
import Amazonka.ComputeOptimizer.Types.VolumeRecommendationOption
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-11-01@ of the Amazon Compute Optimizer SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ComputeOptimizer",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "compute-optimizer",
      Core.signingName = "compute-optimizer",
      Core.version = "2019-11-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ComputeOptimizer",
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
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | An internal error has occurred. Try your call again.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The value supplied for the input parameter is out of range or not valid.
_InvalidParameterValueException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"

-- | The request exceeds a limit of the service.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The request must contain either a valid (registered) Amazon Web Services
-- access key ID or X.509 certificate.
_MissingAuthenticationToken :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MissingAuthenticationToken =
  Core._MatchServiceError
    defaultService
    "MissingAuthenticationToken"

-- | The account is not opted in to Compute Optimizer.
_OptInRequiredException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OptInRequiredException =
  Core._MatchServiceError
    defaultService
    "OptInRequiredException"

-- | A resource that is required for the action doesn\'t exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The request has failed due to a temporary failure of the server.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
