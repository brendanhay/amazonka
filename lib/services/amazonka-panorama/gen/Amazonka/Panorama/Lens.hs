{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Panorama.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Lens
  ( -- * Operations

    -- ** CreateApplicationInstance
    createApplicationInstance_tags,
    createApplicationInstance_name,
    createApplicationInstance_applicationInstanceIdToReplace,
    createApplicationInstance_manifestOverridesPayload,
    createApplicationInstance_description,
    createApplicationInstance_runtimeRoleArn,
    createApplicationInstance_manifestPayload,
    createApplicationInstance_defaultRuntimeContextDevice,
    createApplicationInstanceResponse_httpStatus,
    createApplicationInstanceResponse_applicationInstanceId,

    -- ** CreateJobForDevices
    createJobForDevices_deviceIds,
    createJobForDevices_deviceJobConfig,
    createJobForDevices_jobType,
    createJobForDevicesResponse_httpStatus,
    createJobForDevicesResponse_jobs,

    -- ** CreateNodeFromTemplateJob
    createNodeFromTemplateJob_nodeDescription,
    createNodeFromTemplateJob_jobTags,
    createNodeFromTemplateJob_templateType,
    createNodeFromTemplateJob_outputPackageName,
    createNodeFromTemplateJob_outputPackageVersion,
    createNodeFromTemplateJob_nodeName,
    createNodeFromTemplateJob_templateParameters,
    createNodeFromTemplateJobResponse_httpStatus,
    createNodeFromTemplateJobResponse_jobId,

    -- ** CreatePackage
    createPackage_tags,
    createPackage_packageName,
    createPackageResponse_arn,
    createPackageResponse_packageId,
    createPackageResponse_httpStatus,
    createPackageResponse_storageLocation,

    -- ** CreatePackageImportJob
    createPackageImportJob_jobTags,
    createPackageImportJob_jobType,
    createPackageImportJob_inputConfig,
    createPackageImportJob_outputConfig,
    createPackageImportJob_clientToken,
    createPackageImportJobResponse_httpStatus,
    createPackageImportJobResponse_jobId,

    -- ** DeleteDevice
    deleteDevice_deviceId,
    deleteDeviceResponse_deviceId,
    deleteDeviceResponse_httpStatus,

    -- ** DeletePackage
    deletePackage_forceDelete,
    deletePackage_packageId,
    deletePackageResponse_httpStatus,

    -- ** DeregisterPackageVersion
    deregisterPackageVersion_updatedLatestPatchVersion,
    deregisterPackageVersion_ownerAccount,
    deregisterPackageVersion_packageId,
    deregisterPackageVersion_packageVersion,
    deregisterPackageVersion_patchVersion,
    deregisterPackageVersionResponse_httpStatus,

    -- ** DescribeApplicationInstance
    describeApplicationInstance_applicationInstanceId,
    describeApplicationInstanceResponse_tags,
    describeApplicationInstanceResponse_statusDescription,
    describeApplicationInstanceResponse_name,
    describeApplicationInstanceResponse_createdTime,
    describeApplicationInstanceResponse_defaultRuntimeContextDevice,
    describeApplicationInstanceResponse_applicationInstanceIdToReplace,
    describeApplicationInstanceResponse_healthStatus,
    describeApplicationInstanceResponse_arn,
    describeApplicationInstanceResponse_status,
    describeApplicationInstanceResponse_lastUpdatedTime,
    describeApplicationInstanceResponse_description,
    describeApplicationInstanceResponse_applicationInstanceId,
    describeApplicationInstanceResponse_runtimeRoleArn,
    describeApplicationInstanceResponse_defaultRuntimeContextDeviceName,
    describeApplicationInstanceResponse_httpStatus,

    -- ** DescribeApplicationInstanceDetails
    describeApplicationInstanceDetails_applicationInstanceId,
    describeApplicationInstanceDetailsResponse_name,
    describeApplicationInstanceDetailsResponse_createdTime,
    describeApplicationInstanceDetailsResponse_defaultRuntimeContextDevice,
    describeApplicationInstanceDetailsResponse_applicationInstanceIdToReplace,
    describeApplicationInstanceDetailsResponse_manifestOverridesPayload,
    describeApplicationInstanceDetailsResponse_manifestPayload,
    describeApplicationInstanceDetailsResponse_description,
    describeApplicationInstanceDetailsResponse_applicationInstanceId,
    describeApplicationInstanceDetailsResponse_httpStatus,

    -- ** DescribeDevice
    describeDevice_deviceId,
    describeDeviceResponse_tags,
    describeDeviceResponse_currentNetworkingStatus,
    describeDeviceResponse_name,
    describeDeviceResponse_type,
    describeDeviceResponse_createdTime,
    describeDeviceResponse_leaseExpirationTime,
    describeDeviceResponse_latestSoftware,
    describeDeviceResponse_provisioningStatus,
    describeDeviceResponse_deviceId,
    describeDeviceResponse_networkingConfiguration,
    describeDeviceResponse_arn,
    describeDeviceResponse_description,
    describeDeviceResponse_deviceConnectionStatus,
    describeDeviceResponse_serialNumber,
    describeDeviceResponse_currentSoftware,
    describeDeviceResponse_httpStatus,

    -- ** DescribeDeviceJob
    describeDeviceJob_jobId,
    describeDeviceJobResponse_createdTime,
    describeDeviceJobResponse_deviceId,
    describeDeviceJobResponse_deviceName,
    describeDeviceJobResponse_jobId,
    describeDeviceJobResponse_status,
    describeDeviceJobResponse_imageVersion,
    describeDeviceJobResponse_deviceArn,
    describeDeviceJobResponse_deviceType,
    describeDeviceJobResponse_httpStatus,

    -- ** DescribeNode
    describeNode_ownerAccount,
    describeNode_nodeId,
    describeNodeResponse_assetName,
    describeNodeResponse_packageArn,
    describeNodeResponse_httpStatus,
    describeNodeResponse_nodeId,
    describeNodeResponse_name,
    describeNodeResponse_category,
    describeNodeResponse_ownerAccount,
    describeNodeResponse_packageName,
    describeNodeResponse_packageId,
    describeNodeResponse_packageVersion,
    describeNodeResponse_patchVersion,
    describeNodeResponse_nodeInterface,
    describeNodeResponse_description,
    describeNodeResponse_createdTime,
    describeNodeResponse_lastUpdatedTime,

    -- ** DescribeNodeFromTemplateJob
    describeNodeFromTemplateJob_jobId,
    describeNodeFromTemplateJobResponse_nodeDescription,
    describeNodeFromTemplateJobResponse_jobTags,
    describeNodeFromTemplateJobResponse_httpStatus,
    describeNodeFromTemplateJobResponse_jobId,
    describeNodeFromTemplateJobResponse_status,
    describeNodeFromTemplateJobResponse_statusMessage,
    describeNodeFromTemplateJobResponse_createdTime,
    describeNodeFromTemplateJobResponse_lastUpdatedTime,
    describeNodeFromTemplateJobResponse_outputPackageName,
    describeNodeFromTemplateJobResponse_outputPackageVersion,
    describeNodeFromTemplateJobResponse_nodeName,
    describeNodeFromTemplateJobResponse_templateType,
    describeNodeFromTemplateJobResponse_templateParameters,

    -- ** DescribePackage
    describePackage_packageId,
    describePackageResponse_writeAccessPrincipalArns,
    describePackageResponse_readAccessPrincipalArns,
    describePackageResponse_httpStatus,
    describePackageResponse_packageId,
    describePackageResponse_packageName,
    describePackageResponse_arn,
    describePackageResponse_storageLocation,
    describePackageResponse_createdTime,
    describePackageResponse_tags,

    -- ** DescribePackageImportJob
    describePackageImportJob_jobId,
    describePackageImportJobResponse_clientToken,
    describePackageImportJobResponse_jobTags,
    describePackageImportJobResponse_httpStatus,
    describePackageImportJobResponse_jobId,
    describePackageImportJobResponse_jobType,
    describePackageImportJobResponse_inputConfig,
    describePackageImportJobResponse_outputConfig,
    describePackageImportJobResponse_output,
    describePackageImportJobResponse_createdTime,
    describePackageImportJobResponse_lastUpdatedTime,
    describePackageImportJobResponse_status,
    describePackageImportJobResponse_statusMessage,

    -- ** DescribePackageVersion
    describePackageVersion_patchVersion,
    describePackageVersion_ownerAccount,
    describePackageVersion_packageId,
    describePackageVersion_packageVersion,
    describePackageVersionResponse_statusDescription,
    describePackageVersionResponse_packageArn,
    describePackageVersionResponse_registeredTime,
    describePackageVersionResponse_ownerAccount,
    describePackageVersionResponse_httpStatus,
    describePackageVersionResponse_packageId,
    describePackageVersionResponse_packageName,
    describePackageVersionResponse_packageVersion,
    describePackageVersionResponse_patchVersion,
    describePackageVersionResponse_isLatestPatch,
    describePackageVersionResponse_status,

    -- ** ListApplicationInstanceDependencies
    listApplicationInstanceDependencies_nextToken,
    listApplicationInstanceDependencies_maxResults,
    listApplicationInstanceDependencies_applicationInstanceId,
    listApplicationInstanceDependenciesResponse_nextToken,
    listApplicationInstanceDependenciesResponse_packageObjects,
    listApplicationInstanceDependenciesResponse_httpStatus,

    -- ** ListApplicationInstanceNodeInstances
    listApplicationInstanceNodeInstances_nextToken,
    listApplicationInstanceNodeInstances_maxResults,
    listApplicationInstanceNodeInstances_applicationInstanceId,
    listApplicationInstanceNodeInstancesResponse_nextToken,
    listApplicationInstanceNodeInstancesResponse_nodeInstances,
    listApplicationInstanceNodeInstancesResponse_httpStatus,

    -- ** ListApplicationInstances
    listApplicationInstances_nextToken,
    listApplicationInstances_deviceId,
    listApplicationInstances_maxResults,
    listApplicationInstances_statusFilter,
    listApplicationInstancesResponse_nextToken,
    listApplicationInstancesResponse_applicationInstances,
    listApplicationInstancesResponse_httpStatus,

    -- ** ListDevices
    listDevices_nextToken,
    listDevices_maxResults,
    listDevicesResponse_nextToken,
    listDevicesResponse_httpStatus,
    listDevicesResponse_devices,

    -- ** ListDevicesJobs
    listDevicesJobs_nextToken,
    listDevicesJobs_deviceId,
    listDevicesJobs_maxResults,
    listDevicesJobsResponse_nextToken,
    listDevicesJobsResponse_deviceJobs,
    listDevicesJobsResponse_httpStatus,

    -- ** ListNodeFromTemplateJobs
    listNodeFromTemplateJobs_nextToken,
    listNodeFromTemplateJobs_maxResults,
    listNodeFromTemplateJobsResponse_nextToken,
    listNodeFromTemplateJobsResponse_httpStatus,
    listNodeFromTemplateJobsResponse_nodeFromTemplateJobs,

    -- ** ListNodes
    listNodes_nextToken,
    listNodes_packageName,
    listNodes_packageVersion,
    listNodes_maxResults,
    listNodes_category,
    listNodes_patchVersion,
    listNodes_ownerAccount,
    listNodesResponse_nextToken,
    listNodesResponse_nodes,
    listNodesResponse_httpStatus,

    -- ** ListPackageImportJobs
    listPackageImportJobs_nextToken,
    listPackageImportJobs_maxResults,
    listPackageImportJobsResponse_nextToken,
    listPackageImportJobsResponse_httpStatus,
    listPackageImportJobsResponse_packageImportJobs,

    -- ** ListPackages
    listPackages_nextToken,
    listPackages_maxResults,
    listPackagesResponse_nextToken,
    listPackagesResponse_packages,
    listPackagesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ProvisionDevice
    provisionDevice_tags,
    provisionDevice_networkingConfiguration,
    provisionDevice_description,
    provisionDevice_name,
    provisionDeviceResponse_deviceId,
    provisionDeviceResponse_iotThingName,
    provisionDeviceResponse_certificates,
    provisionDeviceResponse_httpStatus,
    provisionDeviceResponse_arn,
    provisionDeviceResponse_status,

    -- ** RegisterPackageVersion
    registerPackageVersion_ownerAccount,
    registerPackageVersion_markLatest,
    registerPackageVersion_packageId,
    registerPackageVersion_packageVersion,
    registerPackageVersion_patchVersion,
    registerPackageVersionResponse_httpStatus,

    -- ** RemoveApplicationInstance
    removeApplicationInstance_applicationInstanceId,
    removeApplicationInstanceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateDeviceMetadata
    updateDeviceMetadata_description,
    updateDeviceMetadata_deviceId,
    updateDeviceMetadataResponse_deviceId,
    updateDeviceMetadataResponse_httpStatus,

    -- * Types

    -- ** ApplicationInstance
    applicationInstance_tags,
    applicationInstance_statusDescription,
    applicationInstance_name,
    applicationInstance_createdTime,
    applicationInstance_defaultRuntimeContextDevice,
    applicationInstance_healthStatus,
    applicationInstance_arn,
    applicationInstance_status,
    applicationInstance_description,
    applicationInstance_applicationInstanceId,
    applicationInstance_defaultRuntimeContextDeviceName,

    -- ** Device
    device_name,
    device_createdTime,
    device_leaseExpirationTime,
    device_provisioningStatus,
    device_deviceId,
    device_lastUpdatedTime,

    -- ** DeviceJob
    deviceJob_createdTime,
    deviceJob_deviceId,
    deviceJob_deviceName,
    deviceJob_jobId,

    -- ** DeviceJobConfig
    deviceJobConfig_oTAJobConfig,

    -- ** EthernetPayload
    ethernetPayload_staticIpConnectionInfo,
    ethernetPayload_connectionType,

    -- ** EthernetStatus
    ethernetStatus_hwAddress,
    ethernetStatus_connectionStatus,
    ethernetStatus_ipAddress,

    -- ** Job
    job_deviceId,
    job_jobId,

    -- ** JobResourceTags
    jobResourceTags_resourceType,
    jobResourceTags_tags,

    -- ** ManifestOverridesPayload
    manifestOverridesPayload_payloadData,

    -- ** ManifestPayload
    manifestPayload_payloadData,

    -- ** NetworkPayload
    networkPayload_ethernet0,
    networkPayload_ethernet1,

    -- ** NetworkStatus
    networkStatus_ethernet0Status,
    networkStatus_ethernet1Status,

    -- ** Node
    node_packageArn,
    node_description,
    node_ownerAccount,
    node_nodeId,
    node_name,
    node_category,
    node_packageName,
    node_packageId,
    node_packageVersion,
    node_patchVersion,
    node_createdTime,

    -- ** NodeFromTemplateJob
    nodeFromTemplateJob_templateType,
    nodeFromTemplateJob_createdTime,
    nodeFromTemplateJob_jobId,
    nodeFromTemplateJob_status,
    nodeFromTemplateJob_nodeName,
    nodeFromTemplateJob_statusMessage,

    -- ** NodeInputPort
    nodeInputPort_name,
    nodeInputPort_type,
    nodeInputPort_defaultValue,
    nodeInputPort_description,
    nodeInputPort_maxConnections,

    -- ** NodeInstance
    nodeInstance_nodeId,
    nodeInstance_packageName,
    nodeInstance_packagePatchVersion,
    nodeInstance_packageVersion,
    nodeInstance_nodeName,
    nodeInstance_nodeInstanceId,
    nodeInstance_currentStatus,

    -- ** NodeInterface
    nodeInterface_inputs,
    nodeInterface_outputs,

    -- ** NodeOutputPort
    nodeOutputPort_name,
    nodeOutputPort_type,
    nodeOutputPort_description,

    -- ** OTAJobConfig
    oTAJobConfig_imageVersion,

    -- ** OutPutS3Location
    outPutS3Location_bucketName,
    outPutS3Location_objectKey,

    -- ** PackageImportJob
    packageImportJob_createdTime,
    packageImportJob_jobId,
    packageImportJob_status,
    packageImportJob_lastUpdatedTime,
    packageImportJob_statusMessage,
    packageImportJob_jobType,

    -- ** PackageImportJobInputConfig
    packageImportJobInputConfig_packageVersionInputConfig,

    -- ** PackageImportJobOutput
    packageImportJobOutput_packageId,
    packageImportJobOutput_packageVersion,
    packageImportJobOutput_patchVersion,
    packageImportJobOutput_outputS3Location,

    -- ** PackageImportJobOutputConfig
    packageImportJobOutputConfig_packageVersionOutputConfig,

    -- ** PackageListItem
    packageListItem_tags,
    packageListItem_packageName,
    packageListItem_createdTime,
    packageListItem_arn,
    packageListItem_packageId,

    -- ** PackageObject
    packageObject_name,
    packageObject_packageVersion,
    packageObject_patchVersion,

    -- ** PackageVersionInputConfig
    packageVersionInputConfig_s3Location,

    -- ** PackageVersionOutputConfig
    packageVersionOutputConfig_markLatest,
    packageVersionOutputConfig_packageName,
    packageVersionOutputConfig_packageVersion,

    -- ** S3Location
    s3Location_region,
    s3Location_bucketName,
    s3Location_objectKey,

    -- ** StaticIpConnectionInfo
    staticIpConnectionInfo_ipAddress,
    staticIpConnectionInfo_mask,
    staticIpConnectionInfo_dns,
    staticIpConnectionInfo_defaultGateway,

    -- ** StorageLocation
    storageLocation_bucket,
    storageLocation_repoPrefixLocation,
    storageLocation_generatedPrefixLocation,
    storageLocation_binaryPrefixLocation,
    storageLocation_manifestPrefixLocation,
  )
where

import Amazonka.Panorama.CreateApplicationInstance
import Amazonka.Panorama.CreateJobForDevices
import Amazonka.Panorama.CreateNodeFromTemplateJob
import Amazonka.Panorama.CreatePackage
import Amazonka.Panorama.CreatePackageImportJob
import Amazonka.Panorama.DeleteDevice
import Amazonka.Panorama.DeletePackage
import Amazonka.Panorama.DeregisterPackageVersion
import Amazonka.Panorama.DescribeApplicationInstance
import Amazonka.Panorama.DescribeApplicationInstanceDetails
import Amazonka.Panorama.DescribeDevice
import Amazonka.Panorama.DescribeDeviceJob
import Amazonka.Panorama.DescribeNode
import Amazonka.Panorama.DescribeNodeFromTemplateJob
import Amazonka.Panorama.DescribePackage
import Amazonka.Panorama.DescribePackageImportJob
import Amazonka.Panorama.DescribePackageVersion
import Amazonka.Panorama.ListApplicationInstanceDependencies
import Amazonka.Panorama.ListApplicationInstanceNodeInstances
import Amazonka.Panorama.ListApplicationInstances
import Amazonka.Panorama.ListDevices
import Amazonka.Panorama.ListDevicesJobs
import Amazonka.Panorama.ListNodeFromTemplateJobs
import Amazonka.Panorama.ListNodes
import Amazonka.Panorama.ListPackageImportJobs
import Amazonka.Panorama.ListPackages
import Amazonka.Panorama.ListTagsForResource
import Amazonka.Panorama.ProvisionDevice
import Amazonka.Panorama.RegisterPackageVersion
import Amazonka.Panorama.RemoveApplicationInstance
import Amazonka.Panorama.TagResource
import Amazonka.Panorama.Types.ApplicationInstance
import Amazonka.Panorama.Types.Device
import Amazonka.Panorama.Types.DeviceJob
import Amazonka.Panorama.Types.DeviceJobConfig
import Amazonka.Panorama.Types.EthernetPayload
import Amazonka.Panorama.Types.EthernetStatus
import Amazonka.Panorama.Types.Job
import Amazonka.Panorama.Types.JobResourceTags
import Amazonka.Panorama.Types.ManifestOverridesPayload
import Amazonka.Panorama.Types.ManifestPayload
import Amazonka.Panorama.Types.NetworkPayload
import Amazonka.Panorama.Types.NetworkStatus
import Amazonka.Panorama.Types.Node
import Amazonka.Panorama.Types.NodeFromTemplateJob
import Amazonka.Panorama.Types.NodeInputPort
import Amazonka.Panorama.Types.NodeInstance
import Amazonka.Panorama.Types.NodeInterface
import Amazonka.Panorama.Types.NodeOutputPort
import Amazonka.Panorama.Types.OTAJobConfig
import Amazonka.Panorama.Types.OutPutS3Location
import Amazonka.Panorama.Types.PackageImportJob
import Amazonka.Panorama.Types.PackageImportJobInputConfig
import Amazonka.Panorama.Types.PackageImportJobOutput
import Amazonka.Panorama.Types.PackageImportJobOutputConfig
import Amazonka.Panorama.Types.PackageListItem
import Amazonka.Panorama.Types.PackageObject
import Amazonka.Panorama.Types.PackageVersionInputConfig
import Amazonka.Panorama.Types.PackageVersionOutputConfig
import Amazonka.Panorama.Types.S3Location
import Amazonka.Panorama.Types.StaticIpConnectionInfo
import Amazonka.Panorama.Types.StorageLocation
import Amazonka.Panorama.UntagResource
import Amazonka.Panorama.UpdateDeviceMetadata
