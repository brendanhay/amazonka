{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EMRContainers.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
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

    -- ** CreateJobTemplate
    createJobTemplate_kmsKeyArn,
    createJobTemplate_tags,
    createJobTemplate_name,
    createJobTemplate_clientToken,
    createJobTemplate_jobTemplateData,
    createJobTemplateResponse_arn,
    createJobTemplateResponse_createdAt,
    createJobTemplateResponse_id,
    createJobTemplateResponse_name,
    createJobTemplateResponse_httpStatus,

    -- ** CreateManagedEndpoint
    createManagedEndpoint_certificateArn,
    createManagedEndpoint_configurationOverrides,
    createManagedEndpoint_tags,
    createManagedEndpoint_name,
    createManagedEndpoint_virtualClusterId,
    createManagedEndpoint_type,
    createManagedEndpoint_releaseLabel,
    createManagedEndpoint_executionRoleArn,
    createManagedEndpoint_clientToken,
    createManagedEndpointResponse_arn,
    createManagedEndpointResponse_id,
    createManagedEndpointResponse_name,
    createManagedEndpointResponse_virtualClusterId,
    createManagedEndpointResponse_httpStatus,

    -- ** CreateVirtualCluster
    createVirtualCluster_tags,
    createVirtualCluster_name,
    createVirtualCluster_containerProvider,
    createVirtualCluster_clientToken,
    createVirtualClusterResponse_arn,
    createVirtualClusterResponse_id,
    createVirtualClusterResponse_name,
    createVirtualClusterResponse_httpStatus,

    -- ** DeleteJobTemplate
    deleteJobTemplate_id,
    deleteJobTemplateResponse_id,
    deleteJobTemplateResponse_httpStatus,

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

    -- ** DescribeJobTemplate
    describeJobTemplate_id,
    describeJobTemplateResponse_jobTemplate,
    describeJobTemplateResponse_httpStatus,

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
    listJobRuns_createdAfter,
    listJobRuns_createdBefore,
    listJobRuns_maxResults,
    listJobRuns_name,
    listJobRuns_nextToken,
    listJobRuns_states,
    listJobRuns_virtualClusterId,
    listJobRunsResponse_jobRuns,
    listJobRunsResponse_nextToken,
    listJobRunsResponse_httpStatus,

    -- ** ListJobTemplates
    listJobTemplates_createdAfter,
    listJobTemplates_createdBefore,
    listJobTemplates_maxResults,
    listJobTemplates_nextToken,
    listJobTemplatesResponse_nextToken,
    listJobTemplatesResponse_templates,
    listJobTemplatesResponse_httpStatus,

    -- ** ListManagedEndpoints
    listManagedEndpoints_createdAfter,
    listManagedEndpoints_createdBefore,
    listManagedEndpoints_maxResults,
    listManagedEndpoints_nextToken,
    listManagedEndpoints_states,
    listManagedEndpoints_types,
    listManagedEndpoints_virtualClusterId,
    listManagedEndpointsResponse_endpoints,
    listManagedEndpointsResponse_nextToken,
    listManagedEndpointsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListVirtualClusters
    listVirtualClusters_containerProviderId,
    listVirtualClusters_containerProviderType,
    listVirtualClusters_createdAfter,
    listVirtualClusters_createdBefore,
    listVirtualClusters_maxResults,
    listVirtualClusters_nextToken,
    listVirtualClusters_states,
    listVirtualClustersResponse_nextToken,
    listVirtualClustersResponse_virtualClusters,
    listVirtualClustersResponse_httpStatus,

    -- ** StartJobRun
    startJobRun_configurationOverrides,
    startJobRun_executionRoleArn,
    startJobRun_jobDriver,
    startJobRun_jobTemplateId,
    startJobRun_jobTemplateParameters,
    startJobRun_name,
    startJobRun_releaseLabel,
    startJobRun_tags,
    startJobRun_virtualClusterId,
    startJobRun_clientToken,
    startJobRunResponse_arn,
    startJobRunResponse_id,
    startJobRunResponse_name,
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

    -- ** Certificate
    certificate_certificateArn,
    certificate_certificateData,

    -- ** CloudWatchMonitoringConfiguration
    cloudWatchMonitoringConfiguration_logStreamNamePrefix,
    cloudWatchMonitoringConfiguration_logGroupName,

    -- ** Configuration
    configuration_configurations,
    configuration_properties,
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
    endpoint_arn,
    endpoint_certificateArn,
    endpoint_certificateAuthority,
    endpoint_configurationOverrides,
    endpoint_createdAt,
    endpoint_executionRoleArn,
    endpoint_failureReason,
    endpoint_id,
    endpoint_name,
    endpoint_releaseLabel,
    endpoint_securityGroup,
    endpoint_serverUrl,
    endpoint_state,
    endpoint_stateDetails,
    endpoint_subnetIds,
    endpoint_tags,
    endpoint_type,
    endpoint_virtualClusterId,

    -- ** JobDriver
    jobDriver_sparkSqlJobDriver,
    jobDriver_sparkSubmitJobDriver,

    -- ** JobRun
    jobRun_arn,
    jobRun_clientToken,
    jobRun_configurationOverrides,
    jobRun_createdAt,
    jobRun_createdBy,
    jobRun_executionRoleArn,
    jobRun_failureReason,
    jobRun_finishedAt,
    jobRun_id,
    jobRun_jobDriver,
    jobRun_name,
    jobRun_releaseLabel,
    jobRun_state,
    jobRun_stateDetails,
    jobRun_tags,
    jobRun_virtualClusterId,

    -- ** JobTemplate
    jobTemplate_arn,
    jobTemplate_createdAt,
    jobTemplate_createdBy,
    jobTemplate_decryptionError,
    jobTemplate_id,
    jobTemplate_kmsKeyArn,
    jobTemplate_name,
    jobTemplate_tags,
    jobTemplate_jobTemplateData,

    -- ** JobTemplateData
    jobTemplateData_configurationOverrides,
    jobTemplateData_jobTags,
    jobTemplateData_parameterConfiguration,
    jobTemplateData_executionRoleArn,
    jobTemplateData_releaseLabel,
    jobTemplateData_jobDriver,

    -- ** MonitoringConfiguration
    monitoringConfiguration_cloudWatchMonitoringConfiguration,
    monitoringConfiguration_persistentAppUI,
    monitoringConfiguration_s3MonitoringConfiguration,

    -- ** ParametricCloudWatchMonitoringConfiguration
    parametricCloudWatchMonitoringConfiguration_logGroupName,
    parametricCloudWatchMonitoringConfiguration_logStreamNamePrefix,

    -- ** ParametricConfigurationOverrides
    parametricConfigurationOverrides_applicationConfiguration,
    parametricConfigurationOverrides_monitoringConfiguration,

    -- ** ParametricMonitoringConfiguration
    parametricMonitoringConfiguration_cloudWatchMonitoringConfiguration,
    parametricMonitoringConfiguration_persistentAppUI,
    parametricMonitoringConfiguration_s3MonitoringConfiguration,

    -- ** ParametricS3MonitoringConfiguration
    parametricS3MonitoringConfiguration_logUri,

    -- ** S3MonitoringConfiguration
    s3MonitoringConfiguration_logUri,

    -- ** SparkSqlJobDriver
    sparkSqlJobDriver_entryPoint,
    sparkSqlJobDriver_sparkSqlParameters,

    -- ** SparkSubmitJobDriver
    sparkSubmitJobDriver_entryPointArguments,
    sparkSubmitJobDriver_sparkSubmitParameters,
    sparkSubmitJobDriver_entryPoint,

    -- ** TemplateParameterConfiguration
    templateParameterConfiguration_defaultValue,
    templateParameterConfiguration_type,

    -- ** VirtualCluster
    virtualCluster_arn,
    virtualCluster_containerProvider,
    virtualCluster_createdAt,
    virtualCluster_id,
    virtualCluster_name,
    virtualCluster_state,
    virtualCluster_tags,
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
import Amazonka.EMRContainers.ListJobRuns
import Amazonka.EMRContainers.ListJobTemplates
import Amazonka.EMRContainers.ListManagedEndpoints
import Amazonka.EMRContainers.ListTagsForResource
import Amazonka.EMRContainers.ListVirtualClusters
import Amazonka.EMRContainers.StartJobRun
import Amazonka.EMRContainers.TagResource
import Amazonka.EMRContainers.Types.Certificate
import Amazonka.EMRContainers.Types.CloudWatchMonitoringConfiguration
import Amazonka.EMRContainers.Types.Configuration
import Amazonka.EMRContainers.Types.ConfigurationOverrides
import Amazonka.EMRContainers.Types.ContainerInfo
import Amazonka.EMRContainers.Types.ContainerProvider
import Amazonka.EMRContainers.Types.EksInfo
import Amazonka.EMRContainers.Types.Endpoint
import Amazonka.EMRContainers.Types.JobDriver
import Amazonka.EMRContainers.Types.JobRun
import Amazonka.EMRContainers.Types.JobTemplate
import Amazonka.EMRContainers.Types.JobTemplateData
import Amazonka.EMRContainers.Types.MonitoringConfiguration
import Amazonka.EMRContainers.Types.ParametricCloudWatchMonitoringConfiguration
import Amazonka.EMRContainers.Types.ParametricConfigurationOverrides
import Amazonka.EMRContainers.Types.ParametricMonitoringConfiguration
import Amazonka.EMRContainers.Types.ParametricS3MonitoringConfiguration
import Amazonka.EMRContainers.Types.S3MonitoringConfiguration
import Amazonka.EMRContainers.Types.SparkSqlJobDriver
import Amazonka.EMRContainers.Types.SparkSubmitJobDriver
import Amazonka.EMRContainers.Types.TemplateParameterConfiguration
import Amazonka.EMRContainers.Types.VirtualCluster
import Amazonka.EMRContainers.UntagResource
