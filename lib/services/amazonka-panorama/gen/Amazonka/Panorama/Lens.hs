{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Panorama.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Lens
  ( -- * Operations

    -- ** CreateApplicationInstance
    createApplicationInstance_applicationInstanceIdToReplace,
    createApplicationInstance_description,
    createApplicationInstance_manifestOverridesPayload,
    createApplicationInstance_name,
    createApplicationInstance_runtimeRoleArn,
    createApplicationInstance_tags,
    createApplicationInstance_defaultRuntimeContextDevice,
    createApplicationInstance_manifestPayload,
    createApplicationInstanceResponse_httpStatus,
    createApplicationInstanceResponse_applicationInstanceId,

    -- ** CreateJobForDevices
    createJobForDevices_deviceJobConfig,
    createJobForDevices_deviceIds,
    createJobForDevices_jobType,
    createJobForDevicesResponse_httpStatus,
    createJobForDevicesResponse_jobs,

    -- ** CreateNodeFromTemplateJob
    createNodeFromTemplateJob_jobTags,
    createNodeFromTemplateJob_nodeDescription,
    createNodeFromTemplateJob_nodeName,
    createNodeFromTemplateJob_outputPackageName,
    createNodeFromTemplateJob_outputPackageVersion,
    createNodeFromTemplateJob_templateParameters,
    createNodeFromTemplateJob_templateType,
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
    createPackageImportJob_clientToken,
    createPackageImportJob_inputConfig,
    createPackageImportJob_jobType,
    createPackageImportJob_outputConfig,
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
    deregisterPackageVersion_ownerAccount,
    deregisterPackageVersion_updatedLatestPatchVersion,
    deregisterPackageVersion_packageId,
    deregisterPackageVersion_packageVersion,
    deregisterPackageVersion_patchVersion,
    deregisterPackageVersionResponse_httpStatus,

    -- ** DescribeApplicationInstance
    describeApplicationInstance_applicationInstanceId,
    describeApplicationInstanceResponse_applicationInstanceId,
    describeApplicationInstanceResponse_applicationInstanceIdToReplace,
    describeApplicationInstanceResponse_arn,
    describeApplicationInstanceResponse_createdTime,
    describeApplicationInstanceResponse_defaultRuntimeContextDevice,
    describeApplicationInstanceResponse_defaultRuntimeContextDeviceName,
    describeApplicationInstanceResponse_description,
    describeApplicationInstanceResponse_healthStatus,
    describeApplicationInstanceResponse_lastUpdatedTime,
    describeApplicationInstanceResponse_name,
    describeApplicationInstanceResponse_runtimeContextStates,
    describeApplicationInstanceResponse_runtimeRoleArn,
    describeApplicationInstanceResponse_status,
    describeApplicationInstanceResponse_statusDescription,
    describeApplicationInstanceResponse_tags,
    describeApplicationInstanceResponse_httpStatus,

    -- ** DescribeApplicationInstanceDetails
    describeApplicationInstanceDetails_applicationInstanceId,
    describeApplicationInstanceDetailsResponse_applicationInstanceId,
    describeApplicationInstanceDetailsResponse_applicationInstanceIdToReplace,
    describeApplicationInstanceDetailsResponse_createdTime,
    describeApplicationInstanceDetailsResponse_defaultRuntimeContextDevice,
    describeApplicationInstanceDetailsResponse_description,
    describeApplicationInstanceDetailsResponse_manifestOverridesPayload,
    describeApplicationInstanceDetailsResponse_manifestPayload,
    describeApplicationInstanceDetailsResponse_name,
    describeApplicationInstanceDetailsResponse_httpStatus,

    -- ** DescribeDevice
    describeDevice_deviceId,
    describeDeviceResponse_alternateSoftwares,
    describeDeviceResponse_arn,
    describeDeviceResponse_brand,
    describeDeviceResponse_createdTime,
    describeDeviceResponse_currentNetworkingStatus,
    describeDeviceResponse_currentSoftware,
    describeDeviceResponse_description,
    describeDeviceResponse_deviceAggregatedStatus,
    describeDeviceResponse_deviceConnectionStatus,
    describeDeviceResponse_deviceId,
    describeDeviceResponse_latestAlternateSoftware,
    describeDeviceResponse_latestDeviceJob,
    describeDeviceResponse_latestSoftware,
    describeDeviceResponse_leaseExpirationTime,
    describeDeviceResponse_name,
    describeDeviceResponse_networkingConfiguration,
    describeDeviceResponse_provisioningStatus,
    describeDeviceResponse_serialNumber,
    describeDeviceResponse_tags,
    describeDeviceResponse_type,
    describeDeviceResponse_httpStatus,

    -- ** DescribeDeviceJob
    describeDeviceJob_jobId,
    describeDeviceJobResponse_createdTime,
    describeDeviceJobResponse_deviceArn,
    describeDeviceJobResponse_deviceId,
    describeDeviceJobResponse_deviceName,
    describeDeviceJobResponse_deviceType,
    describeDeviceJobResponse_imageVersion,
    describeDeviceJobResponse_jobId,
    describeDeviceJobResponse_jobType,
    describeDeviceJobResponse_status,
    describeDeviceJobResponse_httpStatus,

    -- ** DescribeNode
    describeNode_ownerAccount,
    describeNode_nodeId,
    describeNodeResponse_assetName,
    describeNodeResponse_packageArn,
    describeNodeResponse_httpStatus,
    describeNodeResponse_category,
    describeNodeResponse_createdTime,
    describeNodeResponse_description,
    describeNodeResponse_lastUpdatedTime,
    describeNodeResponse_name,
    describeNodeResponse_nodeId,
    describeNodeResponse_nodeInterface,
    describeNodeResponse_ownerAccount,
    describeNodeResponse_packageId,
    describeNodeResponse_packageName,
    describeNodeResponse_packageVersion,
    describeNodeResponse_patchVersion,

    -- ** DescribeNodeFromTemplateJob
    describeNodeFromTemplateJob_jobId,
    describeNodeFromTemplateJobResponse_jobTags,
    describeNodeFromTemplateJobResponse_nodeDescription,
    describeNodeFromTemplateJobResponse_httpStatus,
    describeNodeFromTemplateJobResponse_createdTime,
    describeNodeFromTemplateJobResponse_jobId,
    describeNodeFromTemplateJobResponse_lastUpdatedTime,
    describeNodeFromTemplateJobResponse_nodeName,
    describeNodeFromTemplateJobResponse_outputPackageName,
    describeNodeFromTemplateJobResponse_outputPackageVersion,
    describeNodeFromTemplateJobResponse_status,
    describeNodeFromTemplateJobResponse_statusMessage,
    describeNodeFromTemplateJobResponse_templateParameters,
    describeNodeFromTemplateJobResponse_templateType,

    -- ** DescribePackage
    describePackage_packageId,
    describePackageResponse_readAccessPrincipalArns,
    describePackageResponse_writeAccessPrincipalArns,
    describePackageResponse_httpStatus,
    describePackageResponse_arn,
    describePackageResponse_createdTime,
    describePackageResponse_packageId,
    describePackageResponse_packageName,
    describePackageResponse_storageLocation,
    describePackageResponse_tags,

    -- ** DescribePackageImportJob
    describePackageImportJob_jobId,
    describePackageImportJobResponse_clientToken,
    describePackageImportJobResponse_jobTags,
    describePackageImportJobResponse_httpStatus,
    describePackageImportJobResponse_createdTime,
    describePackageImportJobResponse_inputConfig,
    describePackageImportJobResponse_jobId,
    describePackageImportJobResponse_jobType,
    describePackageImportJobResponse_lastUpdatedTime,
    describePackageImportJobResponse_output,
    describePackageImportJobResponse_outputConfig,
    describePackageImportJobResponse_status,
    describePackageImportJobResponse_statusMessage,

    -- ** DescribePackageVersion
    describePackageVersion_ownerAccount,
    describePackageVersion_patchVersion,
    describePackageVersion_packageId,
    describePackageVersion_packageVersion,
    describePackageVersionResponse_ownerAccount,
    describePackageVersionResponse_packageArn,
    describePackageVersionResponse_registeredTime,
    describePackageVersionResponse_statusDescription,
    describePackageVersionResponse_httpStatus,
    describePackageVersionResponse_isLatestPatch,
    describePackageVersionResponse_packageId,
    describePackageVersionResponse_packageName,
    describePackageVersionResponse_packageVersion,
    describePackageVersionResponse_patchVersion,
    describePackageVersionResponse_status,

    -- ** ListApplicationInstanceDependencies
    listApplicationInstanceDependencies_maxResults,
    listApplicationInstanceDependencies_nextToken,
    listApplicationInstanceDependencies_applicationInstanceId,
    listApplicationInstanceDependenciesResponse_nextToken,
    listApplicationInstanceDependenciesResponse_packageObjects,
    listApplicationInstanceDependenciesResponse_httpStatus,

    -- ** ListApplicationInstanceNodeInstances
    listApplicationInstanceNodeInstances_maxResults,
    listApplicationInstanceNodeInstances_nextToken,
    listApplicationInstanceNodeInstances_applicationInstanceId,
    listApplicationInstanceNodeInstancesResponse_nextToken,
    listApplicationInstanceNodeInstancesResponse_nodeInstances,
    listApplicationInstanceNodeInstancesResponse_httpStatus,

    -- ** ListApplicationInstances
    listApplicationInstances_deviceId,
    listApplicationInstances_maxResults,
    listApplicationInstances_nextToken,
    listApplicationInstances_statusFilter,
    listApplicationInstancesResponse_applicationInstances,
    listApplicationInstancesResponse_nextToken,
    listApplicationInstancesResponse_httpStatus,

    -- ** ListDevices
    listDevices_deviceAggregatedStatusFilter,
    listDevices_maxResults,
    listDevices_nameFilter,
    listDevices_nextToken,
    listDevices_sortBy,
    listDevices_sortOrder,
    listDevicesResponse_nextToken,
    listDevicesResponse_httpStatus,
    listDevicesResponse_devices,

    -- ** ListDevicesJobs
    listDevicesJobs_deviceId,
    listDevicesJobs_maxResults,
    listDevicesJobs_nextToken,
    listDevicesJobsResponse_deviceJobs,
    listDevicesJobsResponse_nextToken,
    listDevicesJobsResponse_httpStatus,

    -- ** ListNodeFromTemplateJobs
    listNodeFromTemplateJobs_maxResults,
    listNodeFromTemplateJobs_nextToken,
    listNodeFromTemplateJobsResponse_nextToken,
    listNodeFromTemplateJobsResponse_httpStatus,
    listNodeFromTemplateJobsResponse_nodeFromTemplateJobs,

    -- ** ListNodes
    listNodes_category,
    listNodes_maxResults,
    listNodes_nextToken,
    listNodes_ownerAccount,
    listNodes_packageName,
    listNodes_packageVersion,
    listNodes_patchVersion,
    listNodesResponse_nextToken,
    listNodesResponse_nodes,
    listNodesResponse_httpStatus,

    -- ** ListPackageImportJobs
    listPackageImportJobs_maxResults,
    listPackageImportJobs_nextToken,
    listPackageImportJobsResponse_nextToken,
    listPackageImportJobsResponse_httpStatus,
    listPackageImportJobsResponse_packageImportJobs,

    -- ** ListPackages
    listPackages_maxResults,
    listPackages_nextToken,
    listPackagesResponse_nextToken,
    listPackagesResponse_packages,
    listPackagesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

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

    -- ** RegisterPackageVersion
    registerPackageVersion_markLatest,
    registerPackageVersion_ownerAccount,
    registerPackageVersion_packageId,
    registerPackageVersion_packageVersion,
    registerPackageVersion_patchVersion,
    registerPackageVersionResponse_httpStatus,

    -- ** RemoveApplicationInstance
    removeApplicationInstance_applicationInstanceId,
    removeApplicationInstanceResponse_httpStatus,

    -- ** SignalApplicationInstanceNodeInstances
    signalApplicationInstanceNodeInstances_applicationInstanceId,
    signalApplicationInstanceNodeInstances_nodeSignals,
    signalApplicationInstanceNodeInstancesResponse_httpStatus,
    signalApplicationInstanceNodeInstancesResponse_applicationInstanceId,

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

    -- ** AlternateSoftwareMetadata
    alternateSoftwareMetadata_version,

    -- ** ApplicationInstance
    applicationInstance_applicationInstanceId,
    applicationInstance_arn,
    applicationInstance_createdTime,
    applicationInstance_defaultRuntimeContextDevice,
    applicationInstance_defaultRuntimeContextDeviceName,
    applicationInstance_description,
    applicationInstance_healthStatus,
    applicationInstance_name,
    applicationInstance_runtimeContextStates,
    applicationInstance_status,
    applicationInstance_statusDescription,
    applicationInstance_tags,

    -- ** Device
    device_brand,
    device_createdTime,
    device_currentSoftware,
    device_description,
    device_deviceAggregatedStatus,
    device_deviceId,
    device_lastUpdatedTime,
    device_latestDeviceJob,
    device_leaseExpirationTime,
    device_name,
    device_provisioningStatus,
    device_tags,
    device_type,

    -- ** DeviceJob
    deviceJob_createdTime,
    deviceJob_deviceId,
    deviceJob_deviceName,
    deviceJob_jobId,
    deviceJob_jobType,

    -- ** DeviceJobConfig
    deviceJobConfig_oTAJobConfig,

    -- ** EthernetPayload
    ethernetPayload_staticIpConnectionInfo,
    ethernetPayload_connectionType,

    -- ** EthernetStatus
    ethernetStatus_connectionStatus,
    ethernetStatus_hwAddress,
    ethernetStatus_ipAddress,

    -- ** Job
    job_deviceId,
    job_jobId,

    -- ** JobResourceTags
    jobResourceTags_resourceType,
    jobResourceTags_tags,

    -- ** LatestDeviceJob
    latestDeviceJob_imageVersion,
    latestDeviceJob_jobType,
    latestDeviceJob_status,

    -- ** ManifestOverridesPayload
    manifestOverridesPayload_payloadData,

    -- ** ManifestPayload
    manifestPayload_payloadData,

    -- ** NetworkPayload
    networkPayload_ethernet0,
    networkPayload_ethernet1,
    networkPayload_ntp,

    -- ** NetworkStatus
    networkStatus_ethernet0Status,
    networkStatus_ethernet1Status,
    networkStatus_lastUpdatedTime,
    networkStatus_ntpStatus,

    -- ** Node
    node_description,
    node_ownerAccount,
    node_packageArn,
    node_category,
    node_createdTime,
    node_name,
    node_nodeId,
    node_packageId,
    node_packageName,
    node_packageVersion,
    node_patchVersion,

    -- ** NodeFromTemplateJob
    nodeFromTemplateJob_createdTime,
    nodeFromTemplateJob_jobId,
    nodeFromTemplateJob_nodeName,
    nodeFromTemplateJob_status,
    nodeFromTemplateJob_statusMessage,
    nodeFromTemplateJob_templateType,

    -- ** NodeInputPort
    nodeInputPort_defaultValue,
    nodeInputPort_description,
    nodeInputPort_maxConnections,
    nodeInputPort_name,
    nodeInputPort_type,

    -- ** NodeInstance
    nodeInstance_nodeId,
    nodeInstance_nodeName,
    nodeInstance_packageName,
    nodeInstance_packagePatchVersion,
    nodeInstance_packageVersion,
    nodeInstance_currentStatus,
    nodeInstance_nodeInstanceId,

    -- ** NodeInterface
    nodeInterface_inputs,
    nodeInterface_outputs,

    -- ** NodeOutputPort
    nodeOutputPort_description,
    nodeOutputPort_name,
    nodeOutputPort_type,

    -- ** NodeSignal
    nodeSignal_nodeInstanceId,
    nodeSignal_signal,

    -- ** NtpPayload
    ntpPayload_ntpServers,

    -- ** NtpStatus
    ntpStatus_connectionStatus,
    ntpStatus_ipAddress,
    ntpStatus_ntpServerName,

    -- ** OTAJobConfig
    oTAJobConfig_imageVersion,

    -- ** OutPutS3Location
    outPutS3Location_bucketName,
    outPutS3Location_objectKey,

    -- ** PackageImportJob
    packageImportJob_createdTime,
    packageImportJob_jobId,
    packageImportJob_jobType,
    packageImportJob_lastUpdatedTime,
    packageImportJob_status,
    packageImportJob_statusMessage,

    -- ** PackageImportJobInputConfig
    packageImportJobInputConfig_packageVersionInputConfig,

    -- ** PackageImportJobOutput
    packageImportJobOutput_outputS3Location,
    packageImportJobOutput_packageId,
    packageImportJobOutput_packageVersion,
    packageImportJobOutput_patchVersion,

    -- ** PackageImportJobOutputConfig
    packageImportJobOutputConfig_packageVersionOutputConfig,

    -- ** PackageListItem
    packageListItem_arn,
    packageListItem_createdTime,
    packageListItem_packageId,
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

    -- ** ReportedRuntimeContextState
    reportedRuntimeContextState_desiredState,
    reportedRuntimeContextState_deviceReportedStatus,
    reportedRuntimeContextState_deviceReportedTime,
    reportedRuntimeContextState_runtimeContextName,

    -- ** S3Location
    s3Location_region,
    s3Location_bucketName,
    s3Location_objectKey,

    -- ** StaticIpConnectionInfo
    staticIpConnectionInfo_defaultGateway,
    staticIpConnectionInfo_dns,
    staticIpConnectionInfo_ipAddress,
    staticIpConnectionInfo_mask,

    -- ** StorageLocation
    storageLocation_binaryPrefixLocation,
    storageLocation_bucket,
    storageLocation_generatedPrefixLocation,
    storageLocation_manifestPrefixLocation,
    storageLocation_repoPrefixLocation,
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
import Amazonka.Panorama.SignalApplicationInstanceNodeInstances
import Amazonka.Panorama.TagResource
import Amazonka.Panorama.Types.AlternateSoftwareMetadata
import Amazonka.Panorama.Types.ApplicationInstance
import Amazonka.Panorama.Types.Device
import Amazonka.Panorama.Types.DeviceJob
import Amazonka.Panorama.Types.DeviceJobConfig
import Amazonka.Panorama.Types.EthernetPayload
import Amazonka.Panorama.Types.EthernetStatus
import Amazonka.Panorama.Types.Job
import Amazonka.Panorama.Types.JobResourceTags
import Amazonka.Panorama.Types.LatestDeviceJob
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
import Amazonka.Panorama.Types.NodeSignal
import Amazonka.Panorama.Types.NtpPayload
import Amazonka.Panorama.Types.NtpStatus
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
import Amazonka.Panorama.Types.ReportedRuntimeContextState
import Amazonka.Panorama.Types.S3Location
import Amazonka.Panorama.Types.StaticIpConnectionInfo
import Amazonka.Panorama.Types.StorageLocation
import Amazonka.Panorama.UntagResource
import Amazonka.Panorama.UpdateDeviceMetadata
