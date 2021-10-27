{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.ComputeOptimizer
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- functions, and Amazon EBS volumes. It reports whether your resources are
-- optimal, and generates optimization recommendations to reduce the cost
-- and improve the performance of your workloads. Compute Optimizer also
-- provides recent utilization metric data, in addition to projected
-- utilization metric data for the recommendations, which you can use to
-- evaluate which recommendation provides the best price-performance
-- trade-off. The analysis of your usage patterns can help you decide when
-- to move or resize your running resources, and still meet your
-- performance and capacity requirements. For more information about
-- Compute Optimizer, including the required permissions to use the
-- service, see the
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/ Compute Optimizer User Guide>.
module Network.AWS.ComputeOptimizer
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** OptInRequiredException
    _OptInRequiredException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** MissingAuthenticationToken
    _MissingAuthenticationToken,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ExportEBSVolumeRecommendations
    ExportEBSVolumeRecommendations (ExportEBSVolumeRecommendations'),
    newExportEBSVolumeRecommendations,
    ExportEBSVolumeRecommendationsResponse (ExportEBSVolumeRecommendationsResponse'),
    newExportEBSVolumeRecommendationsResponse,

    -- ** GetRecommendationSummaries
    GetRecommendationSummaries (GetRecommendationSummaries'),
    newGetRecommendationSummaries,
    GetRecommendationSummariesResponse (GetRecommendationSummariesResponse'),
    newGetRecommendationSummariesResponse,

    -- ** ExportAutoScalingGroupRecommendations
    ExportAutoScalingGroupRecommendations (ExportAutoScalingGroupRecommendations'),
    newExportAutoScalingGroupRecommendations,
    ExportAutoScalingGroupRecommendationsResponse (ExportAutoScalingGroupRecommendationsResponse'),
    newExportAutoScalingGroupRecommendationsResponse,

    -- ** GetEC2InstanceRecommendations
    GetEC2InstanceRecommendations (GetEC2InstanceRecommendations'),
    newGetEC2InstanceRecommendations,
    GetEC2InstanceRecommendationsResponse (GetEC2InstanceRecommendationsResponse'),
    newGetEC2InstanceRecommendationsResponse,

    -- ** GetLambdaFunctionRecommendations
    GetLambdaFunctionRecommendations (GetLambdaFunctionRecommendations'),
    newGetLambdaFunctionRecommendations,
    GetLambdaFunctionRecommendationsResponse (GetLambdaFunctionRecommendationsResponse'),
    newGetLambdaFunctionRecommendationsResponse,

    -- ** UpdateEnrollmentStatus
    UpdateEnrollmentStatus (UpdateEnrollmentStatus'),
    newUpdateEnrollmentStatus,
    UpdateEnrollmentStatusResponse (UpdateEnrollmentStatusResponse'),
    newUpdateEnrollmentStatusResponse,

    -- ** DescribeRecommendationExportJobs
    DescribeRecommendationExportJobs (DescribeRecommendationExportJobs'),
    newDescribeRecommendationExportJobs,
    DescribeRecommendationExportJobsResponse (DescribeRecommendationExportJobsResponse'),
    newDescribeRecommendationExportJobsResponse,

    -- ** GetEC2RecommendationProjectedMetrics
    GetEC2RecommendationProjectedMetrics (GetEC2RecommendationProjectedMetrics'),
    newGetEC2RecommendationProjectedMetrics,
    GetEC2RecommendationProjectedMetricsResponse (GetEC2RecommendationProjectedMetricsResponse'),
    newGetEC2RecommendationProjectedMetricsResponse,

    -- ** GetEnrollmentStatusesForOrganization
    GetEnrollmentStatusesForOrganization (GetEnrollmentStatusesForOrganization'),
    newGetEnrollmentStatusesForOrganization,
    GetEnrollmentStatusesForOrganizationResponse (GetEnrollmentStatusesForOrganizationResponse'),
    newGetEnrollmentStatusesForOrganizationResponse,

    -- ** GetEBSVolumeRecommendations
    GetEBSVolumeRecommendations (GetEBSVolumeRecommendations'),
    newGetEBSVolumeRecommendations,
    GetEBSVolumeRecommendationsResponse (GetEBSVolumeRecommendationsResponse'),
    newGetEBSVolumeRecommendationsResponse,

    -- ** ExportLambdaFunctionRecommendations
    ExportLambdaFunctionRecommendations (ExportLambdaFunctionRecommendations'),
    newExportLambdaFunctionRecommendations,
    ExportLambdaFunctionRecommendationsResponse (ExportLambdaFunctionRecommendationsResponse'),
    newExportLambdaFunctionRecommendationsResponse,

    -- ** ExportEC2InstanceRecommendations
    ExportEC2InstanceRecommendations (ExportEC2InstanceRecommendations'),
    newExportEC2InstanceRecommendations,
    ExportEC2InstanceRecommendationsResponse (ExportEC2InstanceRecommendationsResponse'),
    newExportEC2InstanceRecommendationsResponse,

    -- ** GetEnrollmentStatus
    GetEnrollmentStatus (GetEnrollmentStatus'),
    newGetEnrollmentStatus,
    GetEnrollmentStatusResponse (GetEnrollmentStatusResponse'),
    newGetEnrollmentStatusResponse,

    -- ** GetAutoScalingGroupRecommendations
    GetAutoScalingGroupRecommendations (GetAutoScalingGroupRecommendations'),
    newGetAutoScalingGroupRecommendations,
    GetAutoScalingGroupRecommendationsResponse (GetAutoScalingGroupRecommendationsResponse'),
    newGetAutoScalingGroupRecommendationsResponse,

    -- * Types

    -- ** CpuVendorArchitecture
    CpuVendorArchitecture (..),

    -- ** EBSFilterName
    EBSFilterName (..),

    -- ** EBSFinding
    EBSFinding (..),

    -- ** EBSMetricName
    EBSMetricName (..),

    -- ** EnrollmentFilterName
    EnrollmentFilterName (..),

    -- ** ExportableAutoScalingGroupField
    ExportableAutoScalingGroupField (..),

    -- ** ExportableInstanceField
    ExportableInstanceField (..),

    -- ** ExportableLambdaFunctionField
    ExportableLambdaFunctionField (..),

    -- ** ExportableVolumeField
    ExportableVolumeField (..),

    -- ** FileFormat
    FileFormat (..),

    -- ** FilterName
    FilterName (..),

    -- ** Finding
    Finding (..),

    -- ** FindingReasonCode
    FindingReasonCode (..),

    -- ** InstanceRecommendationFindingReasonCode
    InstanceRecommendationFindingReasonCode (..),

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

    -- ** PlatformDifference
    PlatformDifference (..),

    -- ** RecommendationSourceType
    RecommendationSourceType (..),

    -- ** ResourceType
    ResourceType (..),

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

    -- ** EBSFilter
    EBSFilter (EBSFilter'),
    newEBSFilter,

    -- ** EBSUtilizationMetric
    EBSUtilizationMetric (EBSUtilizationMetric'),
    newEBSUtilizationMetric,

    -- ** EnrollmentFilter
    EnrollmentFilter (EnrollmentFilter'),
    newEnrollmentFilter,

    -- ** ExportDestination
    ExportDestination (ExportDestination'),
    newExportDestination,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** GetRecommendationError
    GetRecommendationError (GetRecommendationError'),
    newGetRecommendationError,

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

    -- ** Summary
    Summary (Summary'),
    newSummary,

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
import Network.AWS.ComputeOptimizer.Lens
import Network.AWS.ComputeOptimizer.Types
import Network.AWS.ComputeOptimizer.UpdateEnrollmentStatus
import Network.AWS.ComputeOptimizer.Waiters

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
