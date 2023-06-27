{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ComputeOptimizer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-11-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Compute Optimizer is a service that analyzes the configuration and
-- utilization metrics of your Amazon Web Services compute resources, such
-- as Amazon EC2 instances, Amazon EC2 Auto Scaling groups, Lambda
-- functions, Amazon EBS volumes, and Amazon ECS services on Fargate. It
-- reports whether your resources are optimal, and generates optimization
-- recommendations to reduce the cost and improve the performance of your
-- workloads. Compute Optimizer also provides recent utilization metric
-- data, in addition to projected utilization metric data for the
-- recommendations, which you can use to evaluate which recommendation
-- provides the best price-performance trade-off. The analysis of your
-- usage patterns can help you decide when to move or resize your running
-- resources, and still meet your performance and capacity requirements.
-- For more information about Compute Optimizer, including the required
-- permissions to use the service, see the
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/ Compute Optimizer User Guide>.
module Amazonka.ComputeOptimizer
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** MissingAuthenticationToken
    _MissingAuthenticationToken,

    -- ** OptInRequiredException
    _OptInRequiredException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteRecommendationPreferences
    DeleteRecommendationPreferences (DeleteRecommendationPreferences'),
    newDeleteRecommendationPreferences,
    DeleteRecommendationPreferencesResponse (DeleteRecommendationPreferencesResponse'),
    newDeleteRecommendationPreferencesResponse,

    -- ** DescribeRecommendationExportJobs (Paginated)
    DescribeRecommendationExportJobs (DescribeRecommendationExportJobs'),
    newDescribeRecommendationExportJobs,
    DescribeRecommendationExportJobsResponse (DescribeRecommendationExportJobsResponse'),
    newDescribeRecommendationExportJobsResponse,

    -- ** ExportAutoScalingGroupRecommendations
    ExportAutoScalingGroupRecommendations (ExportAutoScalingGroupRecommendations'),
    newExportAutoScalingGroupRecommendations,
    ExportAutoScalingGroupRecommendationsResponse (ExportAutoScalingGroupRecommendationsResponse'),
    newExportAutoScalingGroupRecommendationsResponse,

    -- ** ExportEBSVolumeRecommendations
    ExportEBSVolumeRecommendations (ExportEBSVolumeRecommendations'),
    newExportEBSVolumeRecommendations,
    ExportEBSVolumeRecommendationsResponse (ExportEBSVolumeRecommendationsResponse'),
    newExportEBSVolumeRecommendationsResponse,

    -- ** ExportEC2InstanceRecommendations
    ExportEC2InstanceRecommendations (ExportEC2InstanceRecommendations'),
    newExportEC2InstanceRecommendations,
    ExportEC2InstanceRecommendationsResponse (ExportEC2InstanceRecommendationsResponse'),
    newExportEC2InstanceRecommendationsResponse,

    -- ** ExportECSServiceRecommendations
    ExportECSServiceRecommendations (ExportECSServiceRecommendations'),
    newExportECSServiceRecommendations,
    ExportECSServiceRecommendationsResponse (ExportECSServiceRecommendationsResponse'),
    newExportECSServiceRecommendationsResponse,

    -- ** ExportLambdaFunctionRecommendations
    ExportLambdaFunctionRecommendations (ExportLambdaFunctionRecommendations'),
    newExportLambdaFunctionRecommendations,
    ExportLambdaFunctionRecommendationsResponse (ExportLambdaFunctionRecommendationsResponse'),
    newExportLambdaFunctionRecommendationsResponse,

    -- ** GetAutoScalingGroupRecommendations
    GetAutoScalingGroupRecommendations (GetAutoScalingGroupRecommendations'),
    newGetAutoScalingGroupRecommendations,
    GetAutoScalingGroupRecommendationsResponse (GetAutoScalingGroupRecommendationsResponse'),
    newGetAutoScalingGroupRecommendationsResponse,

    -- ** GetEBSVolumeRecommendations
    GetEBSVolumeRecommendations (GetEBSVolumeRecommendations'),
    newGetEBSVolumeRecommendations,
    GetEBSVolumeRecommendationsResponse (GetEBSVolumeRecommendationsResponse'),
    newGetEBSVolumeRecommendationsResponse,

    -- ** GetEC2InstanceRecommendations
    GetEC2InstanceRecommendations (GetEC2InstanceRecommendations'),
    newGetEC2InstanceRecommendations,
    GetEC2InstanceRecommendationsResponse (GetEC2InstanceRecommendationsResponse'),
    newGetEC2InstanceRecommendationsResponse,

    -- ** GetEC2RecommendationProjectedMetrics
    GetEC2RecommendationProjectedMetrics (GetEC2RecommendationProjectedMetrics'),
    newGetEC2RecommendationProjectedMetrics,
    GetEC2RecommendationProjectedMetricsResponse (GetEC2RecommendationProjectedMetricsResponse'),
    newGetEC2RecommendationProjectedMetricsResponse,

    -- ** GetECSServiceRecommendationProjectedMetrics
    GetECSServiceRecommendationProjectedMetrics (GetECSServiceRecommendationProjectedMetrics'),
    newGetECSServiceRecommendationProjectedMetrics,
    GetECSServiceRecommendationProjectedMetricsResponse (GetECSServiceRecommendationProjectedMetricsResponse'),
    newGetECSServiceRecommendationProjectedMetricsResponse,

    -- ** GetECSServiceRecommendations
    GetECSServiceRecommendations (GetECSServiceRecommendations'),
    newGetECSServiceRecommendations,
    GetECSServiceRecommendationsResponse (GetECSServiceRecommendationsResponse'),
    newGetECSServiceRecommendationsResponse,

    -- ** GetEffectiveRecommendationPreferences
    GetEffectiveRecommendationPreferences (GetEffectiveRecommendationPreferences'),
    newGetEffectiveRecommendationPreferences,
    GetEffectiveRecommendationPreferencesResponse (GetEffectiveRecommendationPreferencesResponse'),
    newGetEffectiveRecommendationPreferencesResponse,

    -- ** GetEnrollmentStatus
    GetEnrollmentStatus (GetEnrollmentStatus'),
    newGetEnrollmentStatus,
    GetEnrollmentStatusResponse (GetEnrollmentStatusResponse'),
    newGetEnrollmentStatusResponse,

    -- ** GetEnrollmentStatusesForOrganization (Paginated)
    GetEnrollmentStatusesForOrganization (GetEnrollmentStatusesForOrganization'),
    newGetEnrollmentStatusesForOrganization,
    GetEnrollmentStatusesForOrganizationResponse (GetEnrollmentStatusesForOrganizationResponse'),
    newGetEnrollmentStatusesForOrganizationResponse,

    -- ** GetLambdaFunctionRecommendations (Paginated)
    GetLambdaFunctionRecommendations (GetLambdaFunctionRecommendations'),
    newGetLambdaFunctionRecommendations,
    GetLambdaFunctionRecommendationsResponse (GetLambdaFunctionRecommendationsResponse'),
    newGetLambdaFunctionRecommendationsResponse,

    -- ** GetRecommendationPreferences (Paginated)
    GetRecommendationPreferences (GetRecommendationPreferences'),
    newGetRecommendationPreferences,
    GetRecommendationPreferencesResponse (GetRecommendationPreferencesResponse'),
    newGetRecommendationPreferencesResponse,

    -- ** GetRecommendationSummaries (Paginated)
    GetRecommendationSummaries (GetRecommendationSummaries'),
    newGetRecommendationSummaries,
    GetRecommendationSummariesResponse (GetRecommendationSummariesResponse'),
    newGetRecommendationSummariesResponse,

    -- ** PutRecommendationPreferences
    PutRecommendationPreferences (PutRecommendationPreferences'),
    newPutRecommendationPreferences,
    PutRecommendationPreferencesResponse (PutRecommendationPreferencesResponse'),
    newPutRecommendationPreferencesResponse,

    -- ** UpdateEnrollmentStatus
    UpdateEnrollmentStatus (UpdateEnrollmentStatus'),
    newUpdateEnrollmentStatus,
    UpdateEnrollmentStatusResponse (UpdateEnrollmentStatusResponse'),
    newUpdateEnrollmentStatusResponse,

    -- * Types

    -- ** AutoScalingConfiguration
    AutoScalingConfiguration (..),

    -- ** CpuVendorArchitecture
    CpuVendorArchitecture (..),

    -- ** Currency
    Currency (..),

    -- ** CurrentPerformanceRisk
    CurrentPerformanceRisk (..),

    -- ** EBSFilterName
    EBSFilterName (..),

    -- ** EBSFinding
    EBSFinding (..),

    -- ** EBSMetricName
    EBSMetricName (..),

    -- ** ECSServiceLaunchType
    ECSServiceLaunchType (..),

    -- ** ECSServiceMetricName
    ECSServiceMetricName (..),

    -- ** ECSServiceMetricStatistic
    ECSServiceMetricStatistic (..),

    -- ** ECSServiceRecommendationFilterName
    ECSServiceRecommendationFilterName (..),

    -- ** ECSServiceRecommendationFinding
    ECSServiceRecommendationFinding (..),

    -- ** ECSServiceRecommendationFindingReasonCode
    ECSServiceRecommendationFindingReasonCode (..),

    -- ** EnhancedInfrastructureMetrics
    EnhancedInfrastructureMetrics (..),

    -- ** EnrollmentFilterName
    EnrollmentFilterName (..),

    -- ** ExportableAutoScalingGroupField
    ExportableAutoScalingGroupField (..),

    -- ** ExportableECSServiceField
    ExportableECSServiceField (..),

    -- ** ExportableInstanceField
    ExportableInstanceField (..),

    -- ** ExportableLambdaFunctionField
    ExportableLambdaFunctionField (..),

    -- ** ExportableVolumeField
    ExportableVolumeField (..),

    -- ** ExternalMetricStatusCode
    ExternalMetricStatusCode (..),

    -- ** ExternalMetricsSource
    ExternalMetricsSource (..),

    -- ** FileFormat
    FileFormat (..),

    -- ** FilterName
    FilterName (..),

    -- ** Finding
    Finding (..),

    -- ** FindingReasonCode
    FindingReasonCode (..),

    -- ** InferredWorkloadType
    InferredWorkloadType (..),

    -- ** InferredWorkloadTypesPreference
    InferredWorkloadTypesPreference (..),

    -- ** InstanceRecommendationFindingReasonCode
    InstanceRecommendationFindingReasonCode (..),

    -- ** InstanceState
    InstanceState (..),

    -- ** JobFilterName
    JobFilterName (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** LambdaFunctionMemoryMetricName
    LambdaFunctionMemoryMetricName (..),

    -- ** LambdaFunctionMemoryMetricStatistic
    LambdaFunctionMemoryMetricStatistic (..),

    -- ** LambdaFunctionMetricName
    LambdaFunctionMetricName (..),

    -- ** LambdaFunctionMetricStatistic
    LambdaFunctionMetricStatistic (..),

    -- ** LambdaFunctionRecommendationFilterName
    LambdaFunctionRecommendationFilterName (..),

    -- ** LambdaFunctionRecommendationFinding
    LambdaFunctionRecommendationFinding (..),

    -- ** LambdaFunctionRecommendationFindingReasonCode
    LambdaFunctionRecommendationFindingReasonCode (..),

    -- ** MetricName
    MetricName (..),

    -- ** MetricStatistic
    MetricStatistic (..),

    -- ** MigrationEffort
    MigrationEffort (..),

    -- ** PlatformDifference
    PlatformDifference (..),

    -- ** RecommendationPreferenceName
    RecommendationPreferenceName (..),

    -- ** RecommendationSourceType
    RecommendationSourceType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** ScopeName
    ScopeName (..),

    -- ** Status
    Status (..),

    -- ** AccountEnrollmentStatus
    AccountEnrollmentStatus (AccountEnrollmentStatus'),
    newAccountEnrollmentStatus,

    -- ** AutoScalingGroupConfiguration
    AutoScalingGroupConfiguration (AutoScalingGroupConfiguration'),
    newAutoScalingGroupConfiguration,

    -- ** AutoScalingGroupRecommendation
    AutoScalingGroupRecommendation (AutoScalingGroupRecommendation'),
    newAutoScalingGroupRecommendation,

    -- ** AutoScalingGroupRecommendationOption
    AutoScalingGroupRecommendationOption (AutoScalingGroupRecommendationOption'),
    newAutoScalingGroupRecommendationOption,

    -- ** ContainerConfiguration
    ContainerConfiguration (ContainerConfiguration'),
    newContainerConfiguration,

    -- ** ContainerRecommendation
    ContainerRecommendation (ContainerRecommendation'),
    newContainerRecommendation,

    -- ** CurrentPerformanceRiskRatings
    CurrentPerformanceRiskRatings (CurrentPerformanceRiskRatings'),
    newCurrentPerformanceRiskRatings,

    -- ** EBSFilter
    EBSFilter (EBSFilter'),
    newEBSFilter,

    -- ** EBSUtilizationMetric
    EBSUtilizationMetric (EBSUtilizationMetric'),
    newEBSUtilizationMetric,

    -- ** ECSServiceProjectedMetric
    ECSServiceProjectedMetric (ECSServiceProjectedMetric'),
    newECSServiceProjectedMetric,

    -- ** ECSServiceProjectedUtilizationMetric
    ECSServiceProjectedUtilizationMetric (ECSServiceProjectedUtilizationMetric'),
    newECSServiceProjectedUtilizationMetric,

    -- ** ECSServiceRecommendation
    ECSServiceRecommendation (ECSServiceRecommendation'),
    newECSServiceRecommendation,

    -- ** ECSServiceRecommendationFilter
    ECSServiceRecommendationFilter (ECSServiceRecommendationFilter'),
    newECSServiceRecommendationFilter,

    -- ** ECSServiceRecommendationOption
    ECSServiceRecommendationOption (ECSServiceRecommendationOption'),
    newECSServiceRecommendationOption,

    -- ** ECSServiceRecommendedOptionProjectedMetric
    ECSServiceRecommendedOptionProjectedMetric (ECSServiceRecommendedOptionProjectedMetric'),
    newECSServiceRecommendedOptionProjectedMetric,

    -- ** ECSServiceUtilizationMetric
    ECSServiceUtilizationMetric (ECSServiceUtilizationMetric'),
    newECSServiceUtilizationMetric,

    -- ** EffectiveRecommendationPreferences
    EffectiveRecommendationPreferences (EffectiveRecommendationPreferences'),
    newEffectiveRecommendationPreferences,

    -- ** EnrollmentFilter
    EnrollmentFilter (EnrollmentFilter'),
    newEnrollmentFilter,

    -- ** EstimatedMonthlySavings
    EstimatedMonthlySavings (EstimatedMonthlySavings'),
    newEstimatedMonthlySavings,

    -- ** ExportDestination
    ExportDestination (ExportDestination'),
    newExportDestination,

    -- ** ExternalMetricStatus
    ExternalMetricStatus (ExternalMetricStatus'),
    newExternalMetricStatus,

    -- ** ExternalMetricsPreference
    ExternalMetricsPreference (ExternalMetricsPreference'),
    newExternalMetricsPreference,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** GetRecommendationError
    GetRecommendationError (GetRecommendationError'),
    newGetRecommendationError,

    -- ** InferredWorkloadSaving
    InferredWorkloadSaving (InferredWorkloadSaving'),
    newInferredWorkloadSaving,

    -- ** InstanceRecommendation
    InstanceRecommendation (InstanceRecommendation'),
    newInstanceRecommendation,

    -- ** InstanceRecommendationOption
    InstanceRecommendationOption (InstanceRecommendationOption'),
    newInstanceRecommendationOption,

    -- ** JobFilter
    JobFilter (JobFilter'),
    newJobFilter,

    -- ** LambdaFunctionMemoryProjectedMetric
    LambdaFunctionMemoryProjectedMetric (LambdaFunctionMemoryProjectedMetric'),
    newLambdaFunctionMemoryProjectedMetric,

    -- ** LambdaFunctionMemoryRecommendationOption
    LambdaFunctionMemoryRecommendationOption (LambdaFunctionMemoryRecommendationOption'),
    newLambdaFunctionMemoryRecommendationOption,

    -- ** LambdaFunctionRecommendation
    LambdaFunctionRecommendation (LambdaFunctionRecommendation'),
    newLambdaFunctionRecommendation,

    -- ** LambdaFunctionRecommendationFilter
    LambdaFunctionRecommendationFilter (LambdaFunctionRecommendationFilter'),
    newLambdaFunctionRecommendationFilter,

    -- ** LambdaFunctionUtilizationMetric
    LambdaFunctionUtilizationMetric (LambdaFunctionUtilizationMetric'),
    newLambdaFunctionUtilizationMetric,

    -- ** MemorySizeConfiguration
    MemorySizeConfiguration (MemorySizeConfiguration'),
    newMemorySizeConfiguration,

    -- ** ProjectedMetric
    ProjectedMetric (ProjectedMetric'),
    newProjectedMetric,

    -- ** ReasonCodeSummary
    ReasonCodeSummary (ReasonCodeSummary'),
    newReasonCodeSummary,

    -- ** RecommendationExportJob
    RecommendationExportJob (RecommendationExportJob'),
    newRecommendationExportJob,

    -- ** RecommendationPreferences
    RecommendationPreferences (RecommendationPreferences'),
    newRecommendationPreferences,

    -- ** RecommendationPreferencesDetail
    RecommendationPreferencesDetail (RecommendationPreferencesDetail'),
    newRecommendationPreferencesDetail,

    -- ** RecommendationSource
    RecommendationSource (RecommendationSource'),
    newRecommendationSource,

    -- ** RecommendationSummary
    RecommendationSummary (RecommendationSummary'),
    newRecommendationSummary,

    -- ** RecommendedOptionProjectedMetric
    RecommendedOptionProjectedMetric (RecommendedOptionProjectedMetric'),
    newRecommendedOptionProjectedMetric,

    -- ** S3Destination
    S3Destination (S3Destination'),
    newS3Destination,

    -- ** S3DestinationConfig
    S3DestinationConfig (S3DestinationConfig'),
    newS3DestinationConfig,

    -- ** SavingsOpportunity
    SavingsOpportunity (SavingsOpportunity'),
    newSavingsOpportunity,

    -- ** Scope
    Scope (Scope'),
    newScope,

    -- ** ServiceConfiguration
    ServiceConfiguration (ServiceConfiguration'),
    newServiceConfiguration,

    -- ** Summary
    Summary (Summary'),
    newSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** UtilizationMetric
    UtilizationMetric (UtilizationMetric'),
    newUtilizationMetric,

    -- ** VolumeConfiguration
    VolumeConfiguration (VolumeConfiguration'),
    newVolumeConfiguration,

    -- ** VolumeRecommendation
    VolumeRecommendation (VolumeRecommendation'),
    newVolumeRecommendation,

    -- ** VolumeRecommendationOption
    VolumeRecommendationOption (VolumeRecommendationOption'),
    newVolumeRecommendationOption,
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
import Amazonka.ComputeOptimizer.Lens
import Amazonka.ComputeOptimizer.PutRecommendationPreferences
import Amazonka.ComputeOptimizer.Types
import Amazonka.ComputeOptimizer.UpdateEnrollmentStatus
import Amazonka.ComputeOptimizer.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ComputeOptimizer'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
