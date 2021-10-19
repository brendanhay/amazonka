{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Lens
  ( -- * Operations

    -- ** CancelCluster
    cancelCluster_clusterId,
    cancelClusterResponse_httpStatus,

    -- ** DescribeCluster
    describeCluster_clusterId,
    describeClusterResponse_clusterMetadata,
    describeClusterResponse_httpStatus,

    -- ** CreateAddress
    createAddress_address,
    createAddressResponse_addressId,
    createAddressResponse_httpStatus,

    -- ** CreateReturnShippingLabel
    createReturnShippingLabel_shippingOption,
    createReturnShippingLabel_jobId,
    createReturnShippingLabelResponse_status,
    createReturnShippingLabelResponse_httpStatus,

    -- ** GetSnowballUsage
    getSnowballUsageResponse_snowballsInUse,
    getSnowballUsageResponse_snowballLimit,
    getSnowballUsageResponse_httpStatus,

    -- ** DescribeAddresses
    describeAddresses_nextToken,
    describeAddresses_maxResults,
    describeAddressesResponse_addresses,
    describeAddressesResponse_nextToken,
    describeAddressesResponse_httpStatus,

    -- ** ListCompatibleImages
    listCompatibleImages_nextToken,
    listCompatibleImages_maxResults,
    listCompatibleImagesResponse_compatibleImages,
    listCompatibleImagesResponse_nextToken,
    listCompatibleImagesResponse_httpStatus,

    -- ** CreateLongTermPricing
    createLongTermPricing_snowballType,
    createLongTermPricing_isLongTermPricingAutoRenew,
    createLongTermPricing_longTermPricingType,
    createLongTermPricingResponse_longTermPricingId,
    createLongTermPricingResponse_httpStatus,

    -- ** UpdateCluster
    updateCluster_notification,
    updateCluster_forwardingAddressId,
    updateCluster_addressId,
    updateCluster_shippingOption,
    updateCluster_resources,
    updateCluster_onDeviceServiceConfiguration,
    updateCluster_description,
    updateCluster_roleARN,
    updateCluster_clusterId,
    updateClusterResponse_httpStatus,

    -- ** GetSoftwareUpdates
    getSoftwareUpdates_jobId,
    getSoftwareUpdatesResponse_updatesURI,
    getSoftwareUpdatesResponse_httpStatus,

    -- ** CreateJob
    createJob_jobType,
    createJob_kmsKeyARN,
    createJob_remoteManagement,
    createJob_notification,
    createJob_forwardingAddressId,
    createJob_addressId,
    createJob_snowballType,
    createJob_longTermPricingId,
    createJob_shippingOption,
    createJob_resources,
    createJob_onDeviceServiceConfiguration,
    createJob_clusterId,
    createJob_deviceConfiguration,
    createJob_description,
    createJob_taxDocuments,
    createJob_roleARN,
    createJob_snowballCapacityPreference,
    createJobResponse_jobId,
    createJobResponse_httpStatus,

    -- ** ListLongTermPricing
    listLongTermPricing_nextToken,
    listLongTermPricing_maxResults,
    listLongTermPricingResponse_longTermPricingEntries,
    listLongTermPricingResponse_nextToken,
    listLongTermPricingResponse_httpStatus,

    -- ** GetJobManifest
    getJobManifest_jobId,
    getJobManifestResponse_manifestURI,
    getJobManifestResponse_httpStatus,

    -- ** CreateCluster
    createCluster_kmsKeyARN,
    createCluster_remoteManagement,
    createCluster_notification,
    createCluster_forwardingAddressId,
    createCluster_onDeviceServiceConfiguration,
    createCluster_description,
    createCluster_taxDocuments,
    createCluster_jobType,
    createCluster_resources,
    createCluster_addressId,
    createCluster_roleARN,
    createCluster_snowballType,
    createCluster_shippingOption,
    createClusterResponse_clusterId,
    createClusterResponse_httpStatus,

    -- ** ListJobs
    listJobs_nextToken,
    listJobs_maxResults,
    listJobsResponse_jobListEntries,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,

    -- ** UpdateJob
    updateJob_notification,
    updateJob_forwardingAddressId,
    updateJob_addressId,
    updateJob_shippingOption,
    updateJob_resources,
    updateJob_onDeviceServiceConfiguration,
    updateJob_description,
    updateJob_roleARN,
    updateJob_snowballCapacityPreference,
    updateJob_jobId,
    updateJobResponse_httpStatus,

    -- ** UpdateJobShipmentState
    updateJobShipmentState_jobId,
    updateJobShipmentState_shipmentState,
    updateJobShipmentStateResponse_httpStatus,

    -- ** GetJobUnlockCode
    getJobUnlockCode_jobId,
    getJobUnlockCodeResponse_unlockCode,
    getJobUnlockCodeResponse_httpStatus,

    -- ** ListClusterJobs
    listClusterJobs_nextToken,
    listClusterJobs_maxResults,
    listClusterJobs_clusterId,
    listClusterJobsResponse_jobListEntries,
    listClusterJobsResponse_nextToken,
    listClusterJobsResponse_httpStatus,

    -- ** DescribeJob
    describeJob_jobId,
    describeJobResponse_jobMetadata,
    describeJobResponse_subJobMetadata,
    describeJobResponse_httpStatus,

    -- ** UpdateLongTermPricing
    updateLongTermPricing_isLongTermPricingAutoRenew,
    updateLongTermPricing_replacementJob,
    updateLongTermPricing_longTermPricingId,
    updateLongTermPricingResponse_httpStatus,

    -- ** ListClusters
    listClusters_nextToken,
    listClusters_maxResults,
    listClustersResponse_clusterListEntries,
    listClustersResponse_nextToken,
    listClustersResponse_httpStatus,

    -- ** DescribeAddress
    describeAddress_addressId,
    describeAddressResponse_address,
    describeAddressResponse_httpStatus,

    -- ** DescribeReturnShippingLabel
    describeReturnShippingLabel_jobId,
    describeReturnShippingLabelResponse_status,
    describeReturnShippingLabelResponse_expirationDate,
    describeReturnShippingLabelResponse_httpStatus,

    -- ** CancelJob
    cancelJob_jobId,
    cancelJobResponse_httpStatus,

    -- * Types

    -- ** Address
    address_isRestricted,
    address_street3,
    address_landmark,
    address_postalCode,
    address_country,
    address_stateOrProvince,
    address_street2,
    address_addressId,
    address_city,
    address_phoneNumber,
    address_company,
    address_name,
    address_prefectureOrDistrict,
    address_street1,

    -- ** ClusterListEntry
    clusterListEntry_clusterState,
    clusterListEntry_clusterId,
    clusterListEntry_creationDate,
    clusterListEntry_description,

    -- ** ClusterMetadata
    clusterMetadata_jobType,
    clusterMetadata_kmsKeyARN,
    clusterMetadata_clusterState,
    clusterMetadata_notification,
    clusterMetadata_forwardingAddressId,
    clusterMetadata_addressId,
    clusterMetadata_snowballType,
    clusterMetadata_shippingOption,
    clusterMetadata_resources,
    clusterMetadata_onDeviceServiceConfiguration,
    clusterMetadata_clusterId,
    clusterMetadata_creationDate,
    clusterMetadata_description,
    clusterMetadata_taxDocuments,
    clusterMetadata_roleARN,

    -- ** CompatibleImage
    compatibleImage_name,
    compatibleImage_amiId,

    -- ** DataTransfer
    dataTransfer_totalObjects,
    dataTransfer_totalBytes,
    dataTransfer_objectsTransferred,
    dataTransfer_bytesTransferred,

    -- ** DeviceConfiguration
    deviceConfiguration_snowconeDeviceConfiguration,

    -- ** Ec2AmiResource
    ec2AmiResource_snowballAmiId,
    ec2AmiResource_amiId,

    -- ** EventTriggerDefinition
    eventTriggerDefinition_eventResourceARN,

    -- ** INDTaxDocuments
    iNDTaxDocuments_gstin,

    -- ** JobListEntry
    jobListEntry_jobType,
    jobListEntry_jobId,
    jobListEntry_jobState,
    jobListEntry_snowballType,
    jobListEntry_creationDate,
    jobListEntry_description,
    jobListEntry_isMaster,

    -- ** JobLogs
    jobLogs_jobFailureLogURI,
    jobLogs_jobCompletionReportURI,
    jobLogs_jobSuccessLogURI,

    -- ** JobMetadata
    jobMetadata_jobType,
    jobMetadata_kmsKeyARN,
    jobMetadata_remoteManagement,
    jobMetadata_jobId,
    jobMetadata_jobLogInfo,
    jobMetadata_notification,
    jobMetadata_jobState,
    jobMetadata_forwardingAddressId,
    jobMetadata_shippingDetails,
    jobMetadata_addressId,
    jobMetadata_snowballType,
    jobMetadata_dataTransferProgress,
    jobMetadata_longTermPricingId,
    jobMetadata_resources,
    jobMetadata_onDeviceServiceConfiguration,
    jobMetadata_clusterId,
    jobMetadata_creationDate,
    jobMetadata_deviceConfiguration,
    jobMetadata_description,
    jobMetadata_taxDocuments,
    jobMetadata_roleARN,
    jobMetadata_snowballCapacityPreference,

    -- ** JobResource
    jobResource_ec2AmiResources,
    jobResource_lambdaResources,
    jobResource_s3Resources,

    -- ** KeyRange
    keyRange_endMarker,
    keyRange_beginMarker,

    -- ** LambdaResource
    lambdaResource_eventTriggers,
    lambdaResource_lambdaArn,

    -- ** LongTermPricingListEntry
    longTermPricingListEntry_longTermPricingType,
    longTermPricingListEntry_longTermPricingStartDate,
    longTermPricingListEntry_snowballType,
    longTermPricingListEntry_longTermPricingId,
    longTermPricingListEntry_longTermPricingEndDate,
    longTermPricingListEntry_currentActiveJob,
    longTermPricingListEntry_isLongTermPricingAutoRenew,
    longTermPricingListEntry_longTermPricingStatus,
    longTermPricingListEntry_jobIds,
    longTermPricingListEntry_replacementJob,

    -- ** NFSOnDeviceServiceConfiguration
    nFSOnDeviceServiceConfiguration_storageLimit,
    nFSOnDeviceServiceConfiguration_storageUnit,

    -- ** Notification
    notification_notifyAll,
    notification_snsTopicARN,
    notification_jobStatesToNotify,

    -- ** OnDeviceServiceConfiguration
    onDeviceServiceConfiguration_nFSOnDeviceService,

    -- ** S3Resource
    s3Resource_keyRange,
    s3Resource_bucketArn,
    s3Resource_targetOnDeviceServices,

    -- ** Shipment
    shipment_status,
    shipment_trackingNumber,

    -- ** ShippingDetails
    shippingDetails_shippingOption,
    shippingDetails_outboundShipment,
    shippingDetails_inboundShipment,

    -- ** SnowconeDeviceConfiguration
    snowconeDeviceConfiguration_wirelessConnection,

    -- ** TargetOnDeviceService
    targetOnDeviceService_transferOption,
    targetOnDeviceService_serviceName,

    -- ** TaxDocuments
    taxDocuments_ind,

    -- ** WirelessConnection
    wirelessConnection_isWifiEnabled,
  )
where

import Network.AWS.Snowball.CancelCluster
import Network.AWS.Snowball.CancelJob
import Network.AWS.Snowball.CreateAddress
import Network.AWS.Snowball.CreateCluster
import Network.AWS.Snowball.CreateJob
import Network.AWS.Snowball.CreateLongTermPricing
import Network.AWS.Snowball.CreateReturnShippingLabel
import Network.AWS.Snowball.DescribeAddress
import Network.AWS.Snowball.DescribeAddresses
import Network.AWS.Snowball.DescribeCluster
import Network.AWS.Snowball.DescribeJob
import Network.AWS.Snowball.DescribeReturnShippingLabel
import Network.AWS.Snowball.GetJobManifest
import Network.AWS.Snowball.GetJobUnlockCode
import Network.AWS.Snowball.GetSnowballUsage
import Network.AWS.Snowball.GetSoftwareUpdates
import Network.AWS.Snowball.ListClusterJobs
import Network.AWS.Snowball.ListClusters
import Network.AWS.Snowball.ListCompatibleImages
import Network.AWS.Snowball.ListJobs
import Network.AWS.Snowball.ListLongTermPricing
import Network.AWS.Snowball.Types.Address
import Network.AWS.Snowball.Types.ClusterListEntry
import Network.AWS.Snowball.Types.ClusterMetadata
import Network.AWS.Snowball.Types.CompatibleImage
import Network.AWS.Snowball.Types.DataTransfer
import Network.AWS.Snowball.Types.DeviceConfiguration
import Network.AWS.Snowball.Types.Ec2AmiResource
import Network.AWS.Snowball.Types.EventTriggerDefinition
import Network.AWS.Snowball.Types.INDTaxDocuments
import Network.AWS.Snowball.Types.JobListEntry
import Network.AWS.Snowball.Types.JobLogs
import Network.AWS.Snowball.Types.JobMetadata
import Network.AWS.Snowball.Types.JobResource
import Network.AWS.Snowball.Types.KeyRange
import Network.AWS.Snowball.Types.LambdaResource
import Network.AWS.Snowball.Types.LongTermPricingListEntry
import Network.AWS.Snowball.Types.NFSOnDeviceServiceConfiguration
import Network.AWS.Snowball.Types.Notification
import Network.AWS.Snowball.Types.OnDeviceServiceConfiguration
import Network.AWS.Snowball.Types.S3Resource
import Network.AWS.Snowball.Types.Shipment
import Network.AWS.Snowball.Types.ShippingDetails
import Network.AWS.Snowball.Types.SnowconeDeviceConfiguration
import Network.AWS.Snowball.Types.TargetOnDeviceService
import Network.AWS.Snowball.Types.TaxDocuments
import Network.AWS.Snowball.Types.WirelessConnection
import Network.AWS.Snowball.UpdateCluster
import Network.AWS.Snowball.UpdateJob
import Network.AWS.Snowball.UpdateJobShipmentState
import Network.AWS.Snowball.UpdateLongTermPricing
