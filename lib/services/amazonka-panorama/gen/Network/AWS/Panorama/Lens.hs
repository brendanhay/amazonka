{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Panorama.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Panorama.Lens
  ( -- * Operations

    -- ** UpdateDeviceMetadata
    updateDeviceMetadata_description,
    updateDeviceMetadata_deviceId,
    updateDeviceMetadataResponse_deviceId,
    updateDeviceMetadataResponse_httpStatus,

    -- ** DescribeApplicationInstanceDetails
    describeApplicationInstanceDetails_applicationInstanceId,
    describeApplicationInstanceDetailsResponse_createdTime,
    describeApplicationInstanceDetailsResponse_defaultRuntimeContextDevice,
    describeApplicationInstanceDetailsResponse_manifestOverridesPayload,
    describeApplicationInstanceDetailsResponse_name,
    describeApplicationInstanceDetailsResponse_applicationInstanceId,
    describeApplicationInstanceDetailsResponse_description,
    describeApplicationInstanceDetailsResponse_manifestPayload,
    describeApplicationInstanceDetailsResponse_applicationInstanceIdToReplace,
    describeApplicationInstanceDetailsResponse_httpStatus,

    -- ** CreateApplicationInstance
    createApplicationInstance_manifestOverridesPayload,
    createApplicationInstance_name,
    createApplicationInstance_runtimeRoleArn,
    createApplicationInstance_description,
    createApplicationInstance_tags,
    createApplicationInstance_applicationInstanceIdToReplace,
    createApplicationInstance_manifestPayload,
    createApplicationInstance_defaultRuntimeContextDevice,
    createApplicationInstanceResponse_httpStatus,
    createApplicationInstanceResponse_applicationInstanceId,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RemoveApplicationInstance
    removeApplicationInstance_applicationInstanceId,
    removeApplicationInstanceResponse_httpStatus,

    -- ** ListDevicesJobs
    listDevicesJobs_nextToken,
    listDevicesJobs_deviceId,
    listDevicesJobs_maxResults,
    listDevicesJobsResponse_nextToken,
    listDevicesJobsResponse_deviceJobs,
    listDevicesJobsResponse_httpStatus,

    -- ** CreateNodeFromTemplateJob
    createNodeFromTemplateJob_jobTags,
    createNodeFromTemplateJob_nodeDescription,
    createNodeFromTemplateJob_templateType,
    createNodeFromTemplateJob_outputPackageName,
    createNodeFromTemplateJob_outputPackageVersion,
    createNodeFromTemplateJob_nodeName,
    createNodeFromTemplateJob_templateParameters,
    createNodeFromTemplateJobResponse_httpStatus,
    createNodeFromTemplateJobResponse_jobId,

    -- ** DeregisterPackageVersion
    deregisterPackageVersion_updatedLatestPatchVersion,
    deregisterPackageVersion_ownerAccount,
    deregisterPackageVersion_packageId,
    deregisterPackageVersion_packageVersion,
    deregisterPackageVersion_patchVersion,
    deregisterPackageVersionResponse_httpStatus,

    -- ** ListPackages
    listPackages_nextToken,
    listPackages_maxResults,
    listPackagesResponse_packages,
    listPackagesResponse_nextToken,
    listPackagesResponse_httpStatus,

    -- ** DescribeApplicationInstance
    describeApplicationInstance_applicationInstanceId,
    describeApplicationInstanceResponse_status,
    describeApplicationInstanceResponse_statusDescription,
    describeApplicationInstanceResponse_lastUpdatedTime,
    describeApplicationInstanceResponse_arn,
    describeApplicationInstanceResponse_createdTime,
    describeApplicationInstanceResponse_defaultRuntimeContextDevice,
    describeApplicationInstanceResponse_defaultRuntimeContextDeviceName,
    describeApplicationInstanceResponse_name,
    describeApplicationInstanceResponse_runtimeRoleArn,
    describeApplicationInstanceResponse_healthStatus,
    describeApplicationInstanceResponse_applicationInstanceId,
    describeApplicationInstanceResponse_description,
    describeApplicationInstanceResponse_tags,
    describeApplicationInstanceResponse_applicationInstanceIdToReplace,
    describeApplicationInstanceResponse_httpStatus,

    -- ** RegisterPackageVersion
    registerPackageVersion_markLatest,
    registerPackageVersion_ownerAccount,
    registerPackageVersion_packageId,
    registerPackageVersion_packageVersion,
    registerPackageVersion_patchVersion,
    registerPackageVersionResponse_httpStatus,

    -- ** DescribeNodeFromTemplateJob
    describeNodeFromTemplateJob_jobId,
    describeNodeFromTemplateJobResponse_jobTags,
    describeNodeFromTemplateJobResponse_nodeDescription,
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

    -- ** CreatePackageImportJob
    createPackageImportJob_jobTags,
    createPackageImportJob_jobType,
    createPackageImportJob_inputConfig,
    createPackageImportJob_outputConfig,
    createPackageImportJob_clientToken,
    createPackageImportJobResponse_httpStatus,
    createPackageImportJobResponse_jobId,

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

    -- ** ListApplicationInstances
    listApplicationInstances_nextToken,
    listApplicationInstances_statusFilter,
    listApplicationInstances_deviceId,
    listApplicationInstances_maxResults,
    listApplicationInstancesResponse_nextToken,
    listApplicationInstancesResponse_applicationInstances,
    listApplicationInstancesResponse_httpStatus,

    -- ** DescribeDeviceJob
    describeDeviceJob_jobId,
    describeDeviceJobResponse_status,
    describeDeviceJobResponse_jobId,
    describeDeviceJobResponse_createdTime,
    describeDeviceJobResponse_deviceArn,
    describeDeviceJobResponse_imageVersion,
    describeDeviceJobResponse_deviceName,
    describeDeviceJobResponse_deviceId,
    describeDeviceJobResponse_deviceType,
    describeDeviceJobResponse_httpStatus,

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

    -- ** DescribeDevice
    describeDevice_deviceId,
    describeDeviceResponse_latestSoftware,
    describeDeviceResponse_provisioningStatus,
    describeDeviceResponse_arn,
    describeDeviceResponse_createdTime,
    describeDeviceResponse_currentSoftware,
    describeDeviceResponse_name,
    describeDeviceResponse_deviceConnectionStatus,
    describeDeviceResponse_deviceId,
    describeDeviceResponse_type,
    describeDeviceResponse_leaseExpirationTime,
    describeDeviceResponse_serialNumber,
    describeDeviceResponse_currentNetworkingStatus,
    describeDeviceResponse_description,
    describeDeviceResponse_networkingConfiguration,
    describeDeviceResponse_tags,
    describeDeviceResponse_httpStatus,

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

    -- ** ListNodeFromTemplateJobs
    listNodeFromTemplateJobs_nextToken,
    listNodeFromTemplateJobs_maxResults,
    listNodeFromTemplateJobsResponse_nextToken,
    listNodeFromTemplateJobsResponse_httpStatus,
    listNodeFromTemplateJobsResponse_nodeFromTemplateJobs,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListApplicationInstanceDependencies
    listApplicationInstanceDependencies_nextToken,
    listApplicationInstanceDependencies_maxResults,
    listApplicationInstanceDependencies_applicationInstanceId,
    listApplicationInstanceDependenciesResponse_packageObjects,
    listApplicationInstanceDependenciesResponse_nextToken,
    listApplicationInstanceDependenciesResponse_httpStatus,

    -- ** ListApplicationInstanceNodeInstances
    listApplicationInstanceNodeInstances_nextToken,
    listApplicationInstanceNodeInstances_maxResults,
    listApplicationInstanceNodeInstances_applicationInstanceId,
    listApplicationInstanceNodeInstancesResponse_nextToken,
    listApplicationInstanceNodeInstancesResponse_nodeInstances,
    listApplicationInstanceNodeInstancesResponse_httpStatus,

    -- ** CreateJobForDevices
    createJobForDevices_deviceIds,
    createJobForDevices_deviceJobConfig,
    createJobForDevices_jobType,
    createJobForDevicesResponse_httpStatus,
    createJobForDevicesResponse_jobs,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeletePackage
    deletePackage_forceDelete,
    deletePackage_packageId,
    deletePackageResponse_httpStatus,

    -- ** CreatePackage
    createPackage_tags,
    createPackage_packageName,
    createPackageResponse_packageId,
    createPackageResponse_arn,
    createPackageResponse_httpStatus,
    createPackageResponse_storageLocation,

    -- ** ProvisionDevice
    provisionDevice_description,
    provisionDevice_networkingConfiguration,
    provisionDevice_tags,
    provisionDevice_name,
    provisionDeviceResponse_certificates,
    provisionDeviceResponse_deviceId,
    provisionDeviceResponse_iotThingName,
    provisionDeviceResponse_httpStatus,
    provisionDeviceResponse_arn,
    provisionDeviceResponse_status,

    -- ** DeleteDevice
    deleteDevice_deviceId,
    deleteDeviceResponse_deviceId,
    deleteDeviceResponse_httpStatus,

    -- ** ListNodes
    listNodes_patchVersion,
    listNodes_packageName,
    listNodes_packageVersion,
    listNodes_category,
    listNodes_nextToken,
    listNodes_ownerAccount,
    listNodes_maxResults,
    listNodesResponse_nextToken,
    listNodesResponse_nodes,
    listNodesResponse_httpStatus,

    -- ** ListDevices
    listDevices_nextToken,
    listDevices_maxResults,
    listDevicesResponse_nextToken,
    listDevicesResponse_httpStatus,
    listDevicesResponse_devices,

    -- ** ListPackageImportJobs
    listPackageImportJobs_nextToken,
    listPackageImportJobs_maxResults,
    listPackageImportJobsResponse_nextToken,
    listPackageImportJobsResponse_httpStatus,
    listPackageImportJobsResponse_packageImportJobs,

    -- * Types

    -- ** ApplicationInstance
    applicationInstance_status,
    applicationInstance_statusDescription,
    applicationInstance_arn,
    applicationInstance_createdTime,
    applicationInstance_defaultRuntimeContextDevice,
    applicationInstance_defaultRuntimeContextDeviceName,
    applicationInstance_name,
    applicationInstance_healthStatus,
    applicationInstance_applicationInstanceId,
    applicationInstance_description,
    applicationInstance_tags,

    -- ** Device
    device_lastUpdatedTime,
    device_provisioningStatus,
    device_createdTime,
    device_name,
    device_deviceId,
    device_leaseExpirationTime,

    -- ** DeviceJob
    deviceJob_jobId,
    deviceJob_createdTime,
    deviceJob_deviceName,
    deviceJob_deviceId,

    -- ** DeviceJobConfig
    deviceJobConfig_oTAJobConfig,

    -- ** EthernetPayload
    ethernetPayload_staticIpConnectionInfo,
    ethernetPayload_connectionType,

    -- ** EthernetStatus
    ethernetStatus_ipAddress,
    ethernetStatus_connectionStatus,
    ethernetStatus_hwAddress,

    -- ** Job
    job_jobId,
    job_deviceId,

    -- ** JobResourceTags
    jobResourceTags_resourceType,
    jobResourceTags_tags,

    -- ** ManifestOverridesPayload
    manifestOverridesPayload_payloadData,

    -- ** ManifestPayload
    manifestPayload_payloadData,

    -- ** NetworkPayload
    networkPayload_ethernet1,
    networkPayload_ethernet0,

    -- ** NetworkStatus
    networkStatus_ethernet1Status,
    networkStatus_ethernet0Status,

    -- ** Node
    node_packageArn,
    node_ownerAccount,
    node_description,
    node_nodeId,
    node_name,
    node_category,
    node_packageName,
    node_packageId,
    node_packageVersion,
    node_patchVersion,
    node_createdTime,

    -- ** NodeFromTemplateJob
    nodeFromTemplateJob_status,
    nodeFromTemplateJob_jobId,
    nodeFromTemplateJob_createdTime,
    nodeFromTemplateJob_templateType,
    nodeFromTemplateJob_nodeName,
    nodeFromTemplateJob_statusMessage,

    -- ** NodeInputPort
    nodeInputPort_maxConnections,
    nodeInputPort_name,
    nodeInputPort_defaultValue,
    nodeInputPort_type,
    nodeInputPort_description,

    -- ** NodeInstance
    nodeInstance_packageName,
    nodeInstance_packageVersion,
    nodeInstance_packagePatchVersion,
    nodeInstance_nodeName,
    nodeInstance_nodeId,
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
    packageImportJob_status,
    packageImportJob_jobType,
    packageImportJob_lastUpdatedTime,
    packageImportJob_jobId,
    packageImportJob_createdTime,
    packageImportJob_statusMessage,

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
    packageListItem_packageId,
    packageListItem_arn,
    packageListItem_createdTime,
    packageListItem_packageName,
    packageListItem_tags,

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

import Network.AWS.Panorama.CreateApplicationInstance
import Network.AWS.Panorama.CreateJobForDevices
import Network.AWS.Panorama.CreateNodeFromTemplateJob
import Network.AWS.Panorama.CreatePackage
import Network.AWS.Panorama.CreatePackageImportJob
import Network.AWS.Panorama.DeleteDevice
import Network.AWS.Panorama.DeletePackage
import Network.AWS.Panorama.DeregisterPackageVersion
import Network.AWS.Panorama.DescribeApplicationInstance
import Network.AWS.Panorama.DescribeApplicationInstanceDetails
import Network.AWS.Panorama.DescribeDevice
import Network.AWS.Panorama.DescribeDeviceJob
import Network.AWS.Panorama.DescribeNode
import Network.AWS.Panorama.DescribeNodeFromTemplateJob
import Network.AWS.Panorama.DescribePackage
import Network.AWS.Panorama.DescribePackageImportJob
import Network.AWS.Panorama.DescribePackageVersion
import Network.AWS.Panorama.ListApplicationInstanceDependencies
import Network.AWS.Panorama.ListApplicationInstanceNodeInstances
import Network.AWS.Panorama.ListApplicationInstances
import Network.AWS.Panorama.ListDevices
import Network.AWS.Panorama.ListDevicesJobs
import Network.AWS.Panorama.ListNodeFromTemplateJobs
import Network.AWS.Panorama.ListNodes
import Network.AWS.Panorama.ListPackageImportJobs
import Network.AWS.Panorama.ListPackages
import Network.AWS.Panorama.ListTagsForResource
import Network.AWS.Panorama.ProvisionDevice
import Network.AWS.Panorama.RegisterPackageVersion
import Network.AWS.Panorama.RemoveApplicationInstance
import Network.AWS.Panorama.TagResource
import Network.AWS.Panorama.Types.ApplicationInstance
import Network.AWS.Panorama.Types.Device
import Network.AWS.Panorama.Types.DeviceJob
import Network.AWS.Panorama.Types.DeviceJobConfig
import Network.AWS.Panorama.Types.EthernetPayload
import Network.AWS.Panorama.Types.EthernetStatus
import Network.AWS.Panorama.Types.Job
import Network.AWS.Panorama.Types.JobResourceTags
import Network.AWS.Panorama.Types.ManifestOverridesPayload
import Network.AWS.Panorama.Types.ManifestPayload
import Network.AWS.Panorama.Types.NetworkPayload
import Network.AWS.Panorama.Types.NetworkStatus
import Network.AWS.Panorama.Types.Node
import Network.AWS.Panorama.Types.NodeFromTemplateJob
import Network.AWS.Panorama.Types.NodeInputPort
import Network.AWS.Panorama.Types.NodeInstance
import Network.AWS.Panorama.Types.NodeInterface
import Network.AWS.Panorama.Types.NodeOutputPort
import Network.AWS.Panorama.Types.OTAJobConfig
import Network.AWS.Panorama.Types.OutPutS3Location
import Network.AWS.Panorama.Types.PackageImportJob
import Network.AWS.Panorama.Types.PackageImportJobInputConfig
import Network.AWS.Panorama.Types.PackageImportJobOutput
import Network.AWS.Panorama.Types.PackageImportJobOutputConfig
import Network.AWS.Panorama.Types.PackageListItem
import Network.AWS.Panorama.Types.PackageObject
import Network.AWS.Panorama.Types.PackageVersionInputConfig
import Network.AWS.Panorama.Types.PackageVersionOutputConfig
import Network.AWS.Panorama.Types.S3Location
import Network.AWS.Panorama.Types.StaticIpConnectionInfo
import Network.AWS.Panorama.Types.StorageLocation
import Network.AWS.Panorama.UntagResource
import Network.AWS.Panorama.UpdateDeviceMetadata
