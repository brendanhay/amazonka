{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMRContainers.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMRContainers.Lens
  ( -- * Operations

    -- ** ListManagedEndpoints
    listManagedEndpoints_states,
    listManagedEndpoints_createdAfter,
    listManagedEndpoints_types,
    listManagedEndpoints_nextToken,
    listManagedEndpoints_maxResults,
    listManagedEndpoints_createdBefore,
    listManagedEndpoints_virtualClusterId,
    listManagedEndpointsResponse_nextToken,
    listManagedEndpointsResponse_endpoints,
    listManagedEndpointsResponse_httpStatus,

    -- ** CreateVirtualCluster
    createVirtualCluster_tags,
    createVirtualCluster_name,
    createVirtualCluster_containerProvider,
    createVirtualCluster_clientToken,
    createVirtualClusterResponse_arn,
    createVirtualClusterResponse_name,
    createVirtualClusterResponse_id,
    createVirtualClusterResponse_httpStatus,

    -- ** DeleteVirtualCluster
    deleteVirtualCluster_id,
    deleteVirtualClusterResponse_id,
    deleteVirtualClusterResponse_httpStatus,

    -- ** CreateManagedEndpoint
    createManagedEndpoint_configurationOverrides,
    createManagedEndpoint_tags,
    createManagedEndpoint_name,
    createManagedEndpoint_virtualClusterId,
    createManagedEndpoint_type,
    createManagedEndpoint_releaseLabel,
    createManagedEndpoint_executionRoleArn,
    createManagedEndpoint_certificateArn,
    createManagedEndpoint_clientToken,
    createManagedEndpointResponse_arn,
    createManagedEndpointResponse_name,
    createManagedEndpointResponse_id,
    createManagedEndpointResponse_virtualClusterId,
    createManagedEndpointResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CancelJobRun
    cancelJobRun_id,
    cancelJobRun_virtualClusterId,
    cancelJobRunResponse_id,
    cancelJobRunResponse_virtualClusterId,
    cancelJobRunResponse_httpStatus,

    -- ** DeleteManagedEndpoint
    deleteManagedEndpoint_id,
    deleteManagedEndpoint_virtualClusterId,
    deleteManagedEndpointResponse_id,
    deleteManagedEndpointResponse_virtualClusterId,
    deleteManagedEndpointResponse_httpStatus,

    -- ** ListJobRuns
    listJobRuns_states,
    listJobRuns_createdAfter,
    listJobRuns_nextToken,
    listJobRuns_name,
    listJobRuns_maxResults,
    listJobRuns_createdBefore,
    listJobRuns_virtualClusterId,
    listJobRunsResponse_nextToken,
    listJobRunsResponse_jobRuns,
    listJobRunsResponse_httpStatus,

    -- ** ListVirtualClusters
    listVirtualClusters_states,
    listVirtualClusters_createdAfter,
    listVirtualClusters_containerProviderType,
    listVirtualClusters_nextToken,
    listVirtualClusters_containerProviderId,
    listVirtualClusters_maxResults,
    listVirtualClusters_createdBefore,
    listVirtualClustersResponse_nextToken,
    listVirtualClustersResponse_virtualClusters,
    listVirtualClustersResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DescribeManagedEndpoint
    describeManagedEndpoint_id,
    describeManagedEndpoint_virtualClusterId,
    describeManagedEndpointResponse_endpoint,
    describeManagedEndpointResponse_httpStatus,

    -- ** DescribeJobRun
    describeJobRun_id,
    describeJobRun_virtualClusterId,
    describeJobRunResponse_jobRun,
    describeJobRunResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeVirtualCluster
    describeVirtualCluster_id,
    describeVirtualClusterResponse_virtualCluster,
    describeVirtualClusterResponse_httpStatus,

    -- ** StartJobRun
    startJobRun_configurationOverrides,
    startJobRun_name,
    startJobRun_tags,
    startJobRun_virtualClusterId,
    startJobRun_clientToken,
    startJobRun_executionRoleArn,
    startJobRun_releaseLabel,
    startJobRun_jobDriver,
    startJobRunResponse_arn,
    startJobRunResponse_name,
    startJobRunResponse_id,
    startJobRunResponse_virtualClusterId,
    startJobRunResponse_httpStatus,

    -- * Types

    -- ** CloudWatchMonitoringConfiguration
    cloudWatchMonitoringConfiguration_logStreamNamePrefix,
    cloudWatchMonitoringConfiguration_logGroupName,

    -- ** Configuration
    configuration_configurations,
    configuration_properties,
    configuration_classification,

    -- ** ConfigurationOverrides
    configurationOverrides_monitoringConfiguration,
    configurationOverrides_applicationConfiguration,

    -- ** ContainerInfo
    containerInfo_eksInfo,

    -- ** ContainerProvider
    containerProvider_info,
    containerProvider_type,
    containerProvider_id,

    -- ** EksInfo
    eksInfo_namespace,

    -- ** Endpoint
    endpoint_failureReason,
    endpoint_state,
    endpoint_arn,
    endpoint_createdAt,
    endpoint_subnetIds,
    endpoint_stateDetails,
    endpoint_certificateArn,
    endpoint_executionRoleArn,
    endpoint_securityGroup,
    endpoint_configurationOverrides,
    endpoint_name,
    endpoint_releaseLabel,
    endpoint_id,
    endpoint_type,
    endpoint_serverUrl,
    endpoint_virtualClusterId,
    endpoint_tags,

    -- ** JobDriver
    jobDriver_sparkSubmitJobDriver,

    -- ** JobRun
    jobRun_failureReason,
    jobRun_state,
    jobRun_clientToken,
    jobRun_arn,
    jobRun_createdAt,
    jobRun_stateDetails,
    jobRun_createdBy,
    jobRun_executionRoleArn,
    jobRun_jobDriver,
    jobRun_configurationOverrides,
    jobRun_finishedAt,
    jobRun_name,
    jobRun_releaseLabel,
    jobRun_id,
    jobRun_virtualClusterId,
    jobRun_tags,

    -- ** MonitoringConfiguration
    monitoringConfiguration_persistentAppUI,
    monitoringConfiguration_s3MonitoringConfiguration,
    monitoringConfiguration_cloudWatchMonitoringConfiguration,

    -- ** S3MonitoringConfiguration
    s3MonitoringConfiguration_logUri,

    -- ** SparkSubmitJobDriver
    sparkSubmitJobDriver_sparkSubmitParameters,
    sparkSubmitJobDriver_entryPointArguments,
    sparkSubmitJobDriver_entryPoint,

    -- ** VirtualCluster
    virtualCluster_state,
    virtualCluster_arn,
    virtualCluster_createdAt,
    virtualCluster_name,
    virtualCluster_id,
    virtualCluster_containerProvider,
    virtualCluster_tags,
  )
where

import Network.AWS.EMRContainers.CancelJobRun
import Network.AWS.EMRContainers.CreateManagedEndpoint
import Network.AWS.EMRContainers.CreateVirtualCluster
import Network.AWS.EMRContainers.DeleteManagedEndpoint
import Network.AWS.EMRContainers.DeleteVirtualCluster
import Network.AWS.EMRContainers.DescribeJobRun
import Network.AWS.EMRContainers.DescribeManagedEndpoint
import Network.AWS.EMRContainers.DescribeVirtualCluster
import Network.AWS.EMRContainers.ListJobRuns
import Network.AWS.EMRContainers.ListManagedEndpoints
import Network.AWS.EMRContainers.ListTagsForResource
import Network.AWS.EMRContainers.ListVirtualClusters
import Network.AWS.EMRContainers.StartJobRun
import Network.AWS.EMRContainers.TagResource
import Network.AWS.EMRContainers.Types.CloudWatchMonitoringConfiguration
import Network.AWS.EMRContainers.Types.Configuration
import Network.AWS.EMRContainers.Types.ConfigurationOverrides
import Network.AWS.EMRContainers.Types.ContainerInfo
import Network.AWS.EMRContainers.Types.ContainerProvider
import Network.AWS.EMRContainers.Types.EksInfo
import Network.AWS.EMRContainers.Types.Endpoint
import Network.AWS.EMRContainers.Types.JobDriver
import Network.AWS.EMRContainers.Types.JobRun
import Network.AWS.EMRContainers.Types.MonitoringConfiguration
import Network.AWS.EMRContainers.Types.S3MonitoringConfiguration
import Network.AWS.EMRContainers.Types.SparkSubmitJobDriver
import Network.AWS.EMRContainers.Types.VirtualCluster
import Network.AWS.EMRContainers.UntagResource
