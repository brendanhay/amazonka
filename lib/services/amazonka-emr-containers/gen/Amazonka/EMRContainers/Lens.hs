{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EMRContainers.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Lens
  ( -- * Operations

    -- ** CancelJobRun
    cancelJobRun_id,
    cancelJobRun_virtualClusterId,
    cancelJobRunResponse_id,
    cancelJobRunResponse_virtualClusterId,
    cancelJobRunResponse_httpStatus,

    -- ** CreateManagedEndpoint
    createManagedEndpoint_tags,
    createManagedEndpoint_configurationOverrides,
    createManagedEndpoint_name,
    createManagedEndpoint_virtualClusterId,
    createManagedEndpoint_type,
    createManagedEndpoint_releaseLabel,
    createManagedEndpoint_executionRoleArn,
    createManagedEndpoint_certificateArn,
    createManagedEndpoint_clientToken,
    createManagedEndpointResponse_name,
    createManagedEndpointResponse_arn,
    createManagedEndpointResponse_id,
    createManagedEndpointResponse_virtualClusterId,
    createManagedEndpointResponse_httpStatus,

    -- ** CreateVirtualCluster
    createVirtualCluster_tags,
    createVirtualCluster_name,
    createVirtualCluster_containerProvider,
    createVirtualCluster_clientToken,
    createVirtualClusterResponse_name,
    createVirtualClusterResponse_arn,
    createVirtualClusterResponse_id,
    createVirtualClusterResponse_httpStatus,

    -- ** DeleteManagedEndpoint
    deleteManagedEndpoint_id,
    deleteManagedEndpoint_virtualClusterId,
    deleteManagedEndpointResponse_id,
    deleteManagedEndpointResponse_virtualClusterId,
    deleteManagedEndpointResponse_httpStatus,

    -- ** DeleteVirtualCluster
    deleteVirtualCluster_id,
    deleteVirtualClusterResponse_id,
    deleteVirtualClusterResponse_httpStatus,

    -- ** DescribeJobRun
    describeJobRun_id,
    describeJobRun_virtualClusterId,
    describeJobRunResponse_jobRun,
    describeJobRunResponse_httpStatus,

    -- ** DescribeManagedEndpoint
    describeManagedEndpoint_id,
    describeManagedEndpoint_virtualClusterId,
    describeManagedEndpointResponse_endpoint,
    describeManagedEndpointResponse_httpStatus,

    -- ** DescribeVirtualCluster
    describeVirtualCluster_id,
    describeVirtualClusterResponse_virtualCluster,
    describeVirtualClusterResponse_httpStatus,

    -- ** ListJobRuns
    listJobRuns_name,
    listJobRuns_nextToken,
    listJobRuns_createdBefore,
    listJobRuns_maxResults,
    listJobRuns_createdAfter,
    listJobRuns_states,
    listJobRuns_virtualClusterId,
    listJobRunsResponse_nextToken,
    listJobRunsResponse_jobRuns,
    listJobRunsResponse_httpStatus,

    -- ** ListManagedEndpoints
    listManagedEndpoints_nextToken,
    listManagedEndpoints_createdBefore,
    listManagedEndpoints_types,
    listManagedEndpoints_maxResults,
    listManagedEndpoints_createdAfter,
    listManagedEndpoints_states,
    listManagedEndpoints_virtualClusterId,
    listManagedEndpointsResponse_nextToken,
    listManagedEndpointsResponse_endpoints,
    listManagedEndpointsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListVirtualClusters
    listVirtualClusters_nextToken,
    listVirtualClusters_createdBefore,
    listVirtualClusters_containerProviderType,
    listVirtualClusters_maxResults,
    listVirtualClusters_containerProviderId,
    listVirtualClusters_createdAfter,
    listVirtualClusters_states,
    listVirtualClustersResponse_nextToken,
    listVirtualClustersResponse_virtualClusters,
    listVirtualClustersResponse_httpStatus,

    -- ** StartJobRun
    startJobRun_tags,
    startJobRun_name,
    startJobRun_configurationOverrides,
    startJobRun_virtualClusterId,
    startJobRun_clientToken,
    startJobRun_executionRoleArn,
    startJobRun_releaseLabel,
    startJobRun_jobDriver,
    startJobRunResponse_name,
    startJobRunResponse_arn,
    startJobRunResponse_id,
    startJobRunResponse_virtualClusterId,
    startJobRunResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** CloudWatchMonitoringConfiguration
    cloudWatchMonitoringConfiguration_logStreamNamePrefix,
    cloudWatchMonitoringConfiguration_logGroupName,

    -- ** Configuration
    configuration_properties,
    configuration_configurations,
    configuration_classification,

    -- ** ConfigurationOverrides
    configurationOverrides_applicationConfiguration,
    configurationOverrides_monitoringConfiguration,

    -- ** ContainerInfo
    containerInfo_eksInfo,

    -- ** ContainerProvider
    containerProvider_info,
    containerProvider_type,
    containerProvider_id,

    -- ** EksInfo
    eksInfo_namespace,

    -- ** Endpoint
    endpoint_tags,
    endpoint_name,
    endpoint_type,
    endpoint_stateDetails,
    endpoint_securityGroup,
    endpoint_releaseLabel,
    endpoint_arn,
    endpoint_state,
    endpoint_serverUrl,
    endpoint_id,
    endpoint_certificateArn,
    endpoint_configurationOverrides,
    endpoint_virtualClusterId,
    endpoint_executionRoleArn,
    endpoint_subnetIds,
    endpoint_createdAt,
    endpoint_failureReason,

    -- ** JobDriver
    jobDriver_sparkSubmitJobDriver,

    -- ** JobRun
    jobRun_tags,
    jobRun_name,
    jobRun_clientToken,
    jobRun_stateDetails,
    jobRun_finishedAt,
    jobRun_jobDriver,
    jobRun_releaseLabel,
    jobRun_arn,
    jobRun_state,
    jobRun_id,
    jobRun_configurationOverrides,
    jobRun_virtualClusterId,
    jobRun_executionRoleArn,
    jobRun_createdBy,
    jobRun_createdAt,
    jobRun_failureReason,

    -- ** MonitoringConfiguration
    monitoringConfiguration_persistentAppUI,
    monitoringConfiguration_s3MonitoringConfiguration,
    monitoringConfiguration_cloudWatchMonitoringConfiguration,

    -- ** S3MonitoringConfiguration
    s3MonitoringConfiguration_logUri,

    -- ** SparkSubmitJobDriver
    sparkSubmitJobDriver_entryPointArguments,
    sparkSubmitJobDriver_sparkSubmitParameters,
    sparkSubmitJobDriver_entryPoint,

    -- ** VirtualCluster
    virtualCluster_tags,
    virtualCluster_name,
    virtualCluster_containerProvider,
    virtualCluster_arn,
    virtualCluster_state,
    virtualCluster_id,
    virtualCluster_createdAt,
  )
where

import Amazonka.EMRContainers.CancelJobRun
import Amazonka.EMRContainers.CreateManagedEndpoint
import Amazonka.EMRContainers.CreateVirtualCluster
import Amazonka.EMRContainers.DeleteManagedEndpoint
import Amazonka.EMRContainers.DeleteVirtualCluster
import Amazonka.EMRContainers.DescribeJobRun
import Amazonka.EMRContainers.DescribeManagedEndpoint
import Amazonka.EMRContainers.DescribeVirtualCluster
import Amazonka.EMRContainers.ListJobRuns
import Amazonka.EMRContainers.ListManagedEndpoints
import Amazonka.EMRContainers.ListTagsForResource
import Amazonka.EMRContainers.ListVirtualClusters
import Amazonka.EMRContainers.StartJobRun
import Amazonka.EMRContainers.TagResource
import Amazonka.EMRContainers.Types.CloudWatchMonitoringConfiguration
import Amazonka.EMRContainers.Types.Configuration
import Amazonka.EMRContainers.Types.ConfigurationOverrides
import Amazonka.EMRContainers.Types.ContainerInfo
import Amazonka.EMRContainers.Types.ContainerProvider
import Amazonka.EMRContainers.Types.EksInfo
import Amazonka.EMRContainers.Types.Endpoint
import Amazonka.EMRContainers.Types.JobDriver
import Amazonka.EMRContainers.Types.JobRun
import Amazonka.EMRContainers.Types.MonitoringConfiguration
import Amazonka.EMRContainers.Types.S3MonitoringConfiguration
import Amazonka.EMRContainers.Types.SparkSubmitJobDriver
import Amazonka.EMRContainers.Types.VirtualCluster
import Amazonka.EMRContainers.UntagResource
