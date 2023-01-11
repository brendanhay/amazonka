{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.EMRContainers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-10-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon EMR on EKS provides a deployment option for Amazon EMR that
-- allows you to run open-source big data frameworks on Amazon Elastic
-- Kubernetes Service (Amazon EKS). With this deployment option, you can
-- focus on running analytics workloads while Amazon EMR on EKS builds,
-- configures, and manages containers for open-source applications. For
-- more information about Amazon EMR on EKS concepts and tasks, see
-- <https://docs.aws.amazon.com/emr/latest/EMR-on-EKS-DevelopmentGuide/emr-eks.html What is Amazon EMR on EKS>.
--
-- /Amazon EMR containers/ is the API name for Amazon EMR on EKS. The
-- @emr-containers@ prefix is used in the following scenarios:
--
-- -   It is the prefix in the CLI commands for Amazon EMR on EKS. For
--     example, @aws emr-containers start-job-run@.
--
-- -   It is the prefix before IAM policy actions for Amazon EMR on EKS.
--     For example, @\"Action\": [ \"emr-containers:StartJobRun\"]@. For
--     more information, see
--     <https://docs.aws.amazon.com/emr/latest/EMR-on-EKS-DevelopmentGuide/security_iam_service-with-iam.html#security_iam_service-with-iam-id-based-policies-actions Policy actions for Amazon EMR on EKS>.
--
-- -   It is the prefix used in Amazon EMR on EKS service endpoints. For
--     example, @emr-containers.us-east-2.amazonaws.com@. For more
--     information, see
--     <https://docs.aws.amazon.com/emr/latest/EMR-on-EKS-DevelopmentGuide/service-quotas.html#service-endpoints Amazon EMR on EKS Service Endpoints>.
module Amazonka.EMRContainers
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelJobRun
    CancelJobRun (CancelJobRun'),
    newCancelJobRun,
    CancelJobRunResponse (CancelJobRunResponse'),
    newCancelJobRunResponse,

    -- ** CreateJobTemplate
    CreateJobTemplate (CreateJobTemplate'),
    newCreateJobTemplate,
    CreateJobTemplateResponse (CreateJobTemplateResponse'),
    newCreateJobTemplateResponse,

    -- ** CreateManagedEndpoint
    CreateManagedEndpoint (CreateManagedEndpoint'),
    newCreateManagedEndpoint,
    CreateManagedEndpointResponse (CreateManagedEndpointResponse'),
    newCreateManagedEndpointResponse,

    -- ** CreateVirtualCluster
    CreateVirtualCluster (CreateVirtualCluster'),
    newCreateVirtualCluster,
    CreateVirtualClusterResponse (CreateVirtualClusterResponse'),
    newCreateVirtualClusterResponse,

    -- ** DeleteJobTemplate
    DeleteJobTemplate (DeleteJobTemplate'),
    newDeleteJobTemplate,
    DeleteJobTemplateResponse (DeleteJobTemplateResponse'),
    newDeleteJobTemplateResponse,

    -- ** DeleteManagedEndpoint
    DeleteManagedEndpoint (DeleteManagedEndpoint'),
    newDeleteManagedEndpoint,
    DeleteManagedEndpointResponse (DeleteManagedEndpointResponse'),
    newDeleteManagedEndpointResponse,

    -- ** DeleteVirtualCluster
    DeleteVirtualCluster (DeleteVirtualCluster'),
    newDeleteVirtualCluster,
    DeleteVirtualClusterResponse (DeleteVirtualClusterResponse'),
    newDeleteVirtualClusterResponse,

    -- ** DescribeJobRun
    DescribeJobRun (DescribeJobRun'),
    newDescribeJobRun,
    DescribeJobRunResponse (DescribeJobRunResponse'),
    newDescribeJobRunResponse,

    -- ** DescribeJobTemplate
    DescribeJobTemplate (DescribeJobTemplate'),
    newDescribeJobTemplate,
    DescribeJobTemplateResponse (DescribeJobTemplateResponse'),
    newDescribeJobTemplateResponse,

    -- ** DescribeManagedEndpoint
    DescribeManagedEndpoint (DescribeManagedEndpoint'),
    newDescribeManagedEndpoint,
    DescribeManagedEndpointResponse (DescribeManagedEndpointResponse'),
    newDescribeManagedEndpointResponse,

    -- ** DescribeVirtualCluster
    DescribeVirtualCluster (DescribeVirtualCluster'),
    newDescribeVirtualCluster,
    DescribeVirtualClusterResponse (DescribeVirtualClusterResponse'),
    newDescribeVirtualClusterResponse,

    -- ** ListJobRuns (Paginated)
    ListJobRuns (ListJobRuns'),
    newListJobRuns,
    ListJobRunsResponse (ListJobRunsResponse'),
    newListJobRunsResponse,

    -- ** ListJobTemplates (Paginated)
    ListJobTemplates (ListJobTemplates'),
    newListJobTemplates,
    ListJobTemplatesResponse (ListJobTemplatesResponse'),
    newListJobTemplatesResponse,

    -- ** ListManagedEndpoints (Paginated)
    ListManagedEndpoints (ListManagedEndpoints'),
    newListManagedEndpoints,
    ListManagedEndpointsResponse (ListManagedEndpointsResponse'),
    newListManagedEndpointsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListVirtualClusters (Paginated)
    ListVirtualClusters (ListVirtualClusters'),
    newListVirtualClusters,
    ListVirtualClustersResponse (ListVirtualClustersResponse'),
    newListVirtualClustersResponse,

    -- ** StartJobRun
    StartJobRun (StartJobRun'),
    newStartJobRun,
    StartJobRunResponse (StartJobRunResponse'),
    newStartJobRunResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- * Types

    -- ** ContainerProviderType
    ContainerProviderType (..),

    -- ** EndpointState
    EndpointState (..),

    -- ** FailureReason
    FailureReason (..),

    -- ** JobRunState
    JobRunState (..),

    -- ** PersistentAppUI
    PersistentAppUI (..),

    -- ** TemplateParameterDataType
    TemplateParameterDataType (..),

    -- ** VirtualClusterState
    VirtualClusterState (..),

    -- ** Certificate
    Certificate (Certificate'),
    newCertificate,

    -- ** CloudWatchMonitoringConfiguration
    CloudWatchMonitoringConfiguration (CloudWatchMonitoringConfiguration'),
    newCloudWatchMonitoringConfiguration,

    -- ** Configuration
    Configuration (Configuration'),
    newConfiguration,

    -- ** ConfigurationOverrides
    ConfigurationOverrides (ConfigurationOverrides'),
    newConfigurationOverrides,

    -- ** ContainerInfo
    ContainerInfo (ContainerInfo'),
    newContainerInfo,

    -- ** ContainerProvider
    ContainerProvider (ContainerProvider'),
    newContainerProvider,

    -- ** EksInfo
    EksInfo (EksInfo'),
    newEksInfo,

    -- ** Endpoint
    Endpoint (Endpoint'),
    newEndpoint,

    -- ** JobDriver
    JobDriver (JobDriver'),
    newJobDriver,

    -- ** JobRun
    JobRun (JobRun'),
    newJobRun,

    -- ** JobTemplate
    JobTemplate (JobTemplate'),
    newJobTemplate,

    -- ** JobTemplateData
    JobTemplateData (JobTemplateData'),
    newJobTemplateData,

    -- ** MonitoringConfiguration
    MonitoringConfiguration (MonitoringConfiguration'),
    newMonitoringConfiguration,

    -- ** ParametricCloudWatchMonitoringConfiguration
    ParametricCloudWatchMonitoringConfiguration (ParametricCloudWatchMonitoringConfiguration'),
    newParametricCloudWatchMonitoringConfiguration,

    -- ** ParametricConfigurationOverrides
    ParametricConfigurationOverrides (ParametricConfigurationOverrides'),
    newParametricConfigurationOverrides,

    -- ** ParametricMonitoringConfiguration
    ParametricMonitoringConfiguration (ParametricMonitoringConfiguration'),
    newParametricMonitoringConfiguration,

    -- ** ParametricS3MonitoringConfiguration
    ParametricS3MonitoringConfiguration (ParametricS3MonitoringConfiguration'),
    newParametricS3MonitoringConfiguration,

    -- ** S3MonitoringConfiguration
    S3MonitoringConfiguration (S3MonitoringConfiguration'),
    newS3MonitoringConfiguration,

    -- ** SparkSqlJobDriver
    SparkSqlJobDriver (SparkSqlJobDriver'),
    newSparkSqlJobDriver,

    -- ** SparkSubmitJobDriver
    SparkSubmitJobDriver (SparkSubmitJobDriver'),
    newSparkSubmitJobDriver,

    -- ** TemplateParameterConfiguration
    TemplateParameterConfiguration (TemplateParameterConfiguration'),
    newTemplateParameterConfiguration,

    -- ** VirtualCluster
    VirtualCluster (VirtualCluster'),
    newVirtualCluster,
  )
where

import Amazonka.EMRContainers.CancelJobRun
import Amazonka.EMRContainers.CreateJobTemplate
import Amazonka.EMRContainers.CreateManagedEndpoint
import Amazonka.EMRContainers.CreateVirtualCluster
import Amazonka.EMRContainers.DeleteJobTemplate
import Amazonka.EMRContainers.DeleteManagedEndpoint
import Amazonka.EMRContainers.DeleteVirtualCluster
import Amazonka.EMRContainers.DescribeJobRun
import Amazonka.EMRContainers.DescribeJobTemplate
import Amazonka.EMRContainers.DescribeManagedEndpoint
import Amazonka.EMRContainers.DescribeVirtualCluster
import Amazonka.EMRContainers.Lens
import Amazonka.EMRContainers.ListJobRuns
import Amazonka.EMRContainers.ListJobTemplates
import Amazonka.EMRContainers.ListManagedEndpoints
import Amazonka.EMRContainers.ListTagsForResource
import Amazonka.EMRContainers.ListVirtualClusters
import Amazonka.EMRContainers.StartJobRun
import Amazonka.EMRContainers.TagResource
import Amazonka.EMRContainers.Types
import Amazonka.EMRContainers.UntagResource
import Amazonka.EMRContainers.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'EMRContainers'.

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
