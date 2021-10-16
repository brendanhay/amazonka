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

    -- ** ListClusterJobs
    listClusterJobs_nextToken,
    listClusterJobs_maxResults,
    listClusterJobs_clusterId,
    listClusterJobsResponse_jobListEntries,
    listClusterJobsResponse_nextToken,
    listClusterJobsResponse_httpStatus,

    -- ** UpdateJobShipmentState
    updateJobShipmentState_jobId,
    updateJobShipmentState_shipmentState,
    updateJobShipmentStateResponse_httpStatus,

    -- ** CancelJob
    cancelJob_jobId,
    cancelJobResponse_httpStatus,

    -- ** UpdateJob
    updateJob_roleARN,
    updateJob_shippingOption,
    updateJob_onDeviceServiceConfiguration,
    updateJob_snowballCapacityPreference,
    updateJob_resources,
    updateJob_description,
    updateJob_addressId,
    updateJob_forwardingAddressId,
    updateJob_notification,
    updateJob_jobId,
    updateJobResponse_httpStatus,

    -- ** CreateCluster
    createCluster_remoteManagement,
    createCluster_kmsKeyARN,
    createCluster_onDeviceServiceConfiguration,
    createCluster_taxDocuments,
    createCluster_description,
    createCluster_forwardingAddressId,
    createCluster_notification,
    createCluster_jobType,
    createCluster_resources,
    createCluster_addressId,
    createCluster_roleARN,
    createCluster_snowballType,
    createCluster_shippingOption,
    createClusterResponse_clusterId,
    createClusterResponse_httpStatus,

    -- ** ListLongTermPricing
    listLongTermPricing_nextToken,
    listLongTermPricing_maxResults,
    listLongTermPricingResponse_nextToken,
    listLongTermPricingResponse_longTermPricingEntries,
    listLongTermPricingResponse_httpStatus,

    -- ** GetSoftwareUpdates
    getSoftwareUpdates_jobId,
    getSoftwareUpdatesResponse_updatesURI,
    getSoftwareUpdatesResponse_httpStatus,

    -- ** DescribeReturnShippingLabel
    describeReturnShippingLabel_jobId,
    describeReturnShippingLabelResponse_status,
    describeReturnShippingLabelResponse_expirationDate,
    describeReturnShippingLabelResponse_httpStatus,

    -- ** DescribeAddress
    describeAddress_addressId,
    describeAddressResponse_address,
    describeAddressResponse_httpStatus,

    -- ** ListCompatibleImages
    listCompatibleImages_nextToken,
    listCompatibleImages_maxResults,
    listCompatibleImagesResponse_nextToken,
    listCompatibleImagesResponse_compatibleImages,
    listCompatibleImagesResponse_httpStatus,

    -- ** DescribeAddresses
    describeAddresses_nextToken,
    describeAddresses_maxResults,
    describeAddressesResponse_nextToken,
    describeAddressesResponse_addresses,
    describeAddressesResponse_httpStatus,

    -- ** DescribeJob
    describeJob_jobId,
    describeJobResponse_subJobMetadata,
    describeJobResponse_jobMetadata,
    describeJobResponse_httpStatus,

    -- ** DescribeCluster
    describeCluster_clusterId,
    describeClusterResponse_clusterMetadata,
    describeClusterResponse_httpStatus,

    -- ** GetJobUnlockCode
    getJobUnlockCode_jobId,
    getJobUnlockCodeResponse_unlockCode,
    getJobUnlockCodeResponse_httpStatus,

    -- ** CancelCluster
    cancelCluster_clusterId,
    cancelClusterResponse_httpStatus,

    -- ** GetJobManifest
    getJobManifest_jobId,
    getJobManifestResponse_manifestURI,
    getJobManifestResponse_httpStatus,

    -- ** ListJobs
    listJobs_nextToken,
    listJobs_maxResults,
    listJobsResponse_jobListEntries,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,

    -- ** CreateJob
    createJob_clusterId,
    createJob_remoteManagement,
    createJob_roleARN,
    createJob_shippingOption,
    createJob_deviceConfiguration,
    createJob_jobType,
    createJob_kmsKeyARN,
    createJob_onDeviceServiceConfiguration,
    createJob_taxDocuments,
    createJob_snowballCapacityPreference,
    createJob_longTermPricingId,
    createJob_resources,
    createJob_snowballType,
    createJob_description,
    createJob_addressId,
    createJob_forwardingAddressId,
    createJob_notification,
    createJobResponse_jobId,
    createJobResponse_httpStatus,

    -- ** ListClusters
    listClusters_nextToken,
    listClusters_maxResults,
    listClustersResponse_nextToken,
    listClustersResponse_clusterListEntries,
    listClustersResponse_httpStatus,

    -- ** UpdateCluster
    updateCluster_roleARN,
    updateCluster_shippingOption,
    updateCluster_onDeviceServiceConfiguration,
    updateCluster_resources,
    updateCluster_description,
    updateCluster_addressId,
    updateCluster_forwardingAddressId,
    updateCluster_notification,
    updateCluster_clusterId,
    updateClusterResponse_httpStatus,

    -- ** CreateLongTermPricing
    createLongTermPricing_snowballType,
    createLongTermPricing_isLongTermPricingAutoRenew,
    createLongTermPricing_longTermPricingType,
    createLongTermPricingResponse_longTermPricingId,
    createLongTermPricingResponse_httpStatus,

    -- ** UpdateLongTermPricing
    updateLongTermPricing_replacementJob,
    updateLongTermPricing_isLongTermPricingAutoRenew,
    updateLongTermPricing_longTermPricingId,
    updateLongTermPricingResponse_httpStatus,

    -- ** GetSnowballUsage
    getSnowballUsageResponse_snowballsInUse,
    getSnowballUsageResponse_snowballLimit,
    getSnowballUsageResponse_httpStatus,

    -- ** CreateAddress
    createAddress_address,
    createAddressResponse_addressId,
    createAddressResponse_httpStatus,

    -- ** CreateReturnShippingLabel
    createReturnShippingLabel_shippingOption,
    createReturnShippingLabel_jobId,
    createReturnShippingLabelResponse_status,
    createReturnShippingLabelResponse_httpStatus,

    -- * Types

    -- ** Address
    address_company,
    address_isRestricted,
    address_phoneNumber,
    address_postalCode,
    address_street1,
    address_landmark,
    address_name,
    address_city,
    address_street2,
    address_addressId,
    address_country,
    address_stateOrProvince,
    address_prefectureOrDistrict,
    address_street3,

    -- ** ClusterListEntry
    clusterListEntry_clusterId,
    clusterListEntry_creationDate,
    clusterListEntry_description,
    clusterListEntry_clusterState,

    -- ** ClusterMetadata
    clusterMetadata_clusterId,
    clusterMetadata_roleARN,
    clusterMetadata_shippingOption,
    clusterMetadata_creationDate,
    clusterMetadata_jobType,
    clusterMetadata_kmsKeyARN,
    clusterMetadata_onDeviceServiceConfiguration,
    clusterMetadata_taxDocuments,
    clusterMetadata_resources,
    clusterMetadata_snowballType,
    clusterMetadata_description,
    clusterMetadata_addressId,
    clusterMetadata_forwardingAddressId,
    clusterMetadata_clusterState,
    clusterMetadata_notification,

    -- ** CompatibleImage
    compatibleImage_amiId,
    compatibleImage_name,

    -- ** DataTransfer
    dataTransfer_totalObjects,
    dataTransfer_bytesTransferred,
    dataTransfer_totalBytes,
    dataTransfer_objectsTransferred,

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
    jobListEntry_isMaster,
    jobListEntry_jobState,
    jobListEntry_creationDate,
    jobListEntry_jobType,
    jobListEntry_snowballType,
    jobListEntry_description,
    jobListEntry_jobId,

    -- ** JobLogs
    jobLogs_jobCompletionReportURI,
    jobLogs_jobSuccessLogURI,
    jobLogs_jobFailureLogURI,

    -- ** JobMetadata
    jobMetadata_clusterId,
    jobMetadata_remoteManagement,
    jobMetadata_roleARN,
    jobMetadata_jobState,
    jobMetadata_deviceConfiguration,
    jobMetadata_creationDate,
    jobMetadata_jobType,
    jobMetadata_kmsKeyARN,
    jobMetadata_onDeviceServiceConfiguration,
    jobMetadata_taxDocuments,
    jobMetadata_snowballCapacityPreference,
    jobMetadata_longTermPricingId,
    jobMetadata_resources,
    jobMetadata_dataTransferProgress,
    jobMetadata_snowballType,
    jobMetadata_description,
    jobMetadata_addressId,
    jobMetadata_forwardingAddressId,
    jobMetadata_shippingDetails,
    jobMetadata_notification,
    jobMetadata_jobLogInfo,
    jobMetadata_jobId,

    -- ** JobResource
    jobResource_s3Resources,
    jobResource_ec2AmiResources,
    jobResource_lambdaResources,

    -- ** KeyRange
    keyRange_endMarker,
    keyRange_beginMarker,

    -- ** LambdaResource
    lambdaResource_eventTriggers,
    lambdaResource_lambdaArn,

    -- ** LongTermPricingListEntry
    longTermPricingListEntry_longTermPricingStartDate,
    longTermPricingListEntry_longTermPricingStatus,
    longTermPricingListEntry_longTermPricingEndDate,
    longTermPricingListEntry_longTermPricingType,
    longTermPricingListEntry_jobIds,
    longTermPricingListEntry_replacementJob,
    longTermPricingListEntry_longTermPricingId,
    longTermPricingListEntry_snowballType,
    longTermPricingListEntry_currentActiveJob,
    longTermPricingListEntry_isLongTermPricingAutoRenew,

    -- ** NFSOnDeviceServiceConfiguration
    nFSOnDeviceServiceConfiguration_storageLimit,
    nFSOnDeviceServiceConfiguration_storageUnit,

    -- ** Notification
    notification_jobStatesToNotify,
    notification_snsTopicARN,
    notification_notifyAll,

    -- ** OnDeviceServiceConfiguration
    onDeviceServiceConfiguration_nFSOnDeviceService,

    -- ** S3Resource
    s3Resource_targetOnDeviceServices,
    s3Resource_bucketArn,
    s3Resource_keyRange,

    -- ** Shipment
    shipment_trackingNumber,
    shipment_status,

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
