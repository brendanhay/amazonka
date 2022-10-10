{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Snowball.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Lens
  ( -- * Operations

    -- ** CancelCluster
    cancelCluster_clusterId,
    cancelClusterResponse_httpStatus,

    -- ** CancelJob
    cancelJob_jobId,
    cancelJobResponse_httpStatus,

    -- ** CreateAddress
    createAddress_address,
    createAddressResponse_addressId,
    createAddressResponse_httpStatus,

    -- ** CreateCluster
    createCluster_forwardingAddressId,
    createCluster_description,
    createCluster_kmsKeyARN,
    createCluster_notification,
    createCluster_remoteManagement,
    createCluster_taxDocuments,
    createCluster_onDeviceServiceConfiguration,
    createCluster_jobType,
    createCluster_resources,
    createCluster_addressId,
    createCluster_roleARN,
    createCluster_snowballType,
    createCluster_shippingOption,
    createClusterResponse_clusterId,
    createClusterResponse_httpStatus,

    -- ** CreateJob
    createJob_roleARN,
    createJob_deviceConfiguration,
    createJob_forwardingAddressId,
    createJob_description,
    createJob_kmsKeyARN,
    createJob_longTermPricingId,
    createJob_notification,
    createJob_remoteManagement,
    createJob_clusterId,
    createJob_taxDocuments,
    createJob_resources,
    createJob_snowballCapacityPreference,
    createJob_addressId,
    createJob_snowballType,
    createJob_jobType,
    createJob_shippingOption,
    createJob_onDeviceServiceConfiguration,
    createJobResponse_jobId,
    createJobResponse_httpStatus,

    -- ** CreateLongTermPricing
    createLongTermPricing_isLongTermPricingAutoRenew,
    createLongTermPricing_snowballType,
    createLongTermPricing_longTermPricingType,
    createLongTermPricingResponse_longTermPricingId,
    createLongTermPricingResponse_httpStatus,

    -- ** CreateReturnShippingLabel
    createReturnShippingLabel_shippingOption,
    createReturnShippingLabel_jobId,
    createReturnShippingLabelResponse_status,
    createReturnShippingLabelResponse_httpStatus,

    -- ** DescribeAddress
    describeAddress_addressId,
    describeAddressResponse_address,
    describeAddressResponse_httpStatus,

    -- ** DescribeAddresses
    describeAddresses_nextToken,
    describeAddresses_maxResults,
    describeAddressesResponse_nextToken,
    describeAddressesResponse_addresses,
    describeAddressesResponse_httpStatus,

    -- ** DescribeCluster
    describeCluster_clusterId,
    describeClusterResponse_clusterMetadata,
    describeClusterResponse_httpStatus,

    -- ** DescribeJob
    describeJob_jobId,
    describeJobResponse_jobMetadata,
    describeJobResponse_subJobMetadata,
    describeJobResponse_httpStatus,

    -- ** DescribeReturnShippingLabel
    describeReturnShippingLabel_jobId,
    describeReturnShippingLabelResponse_status,
    describeReturnShippingLabelResponse_returnShippingLabelURI,
    describeReturnShippingLabelResponse_expirationDate,
    describeReturnShippingLabelResponse_httpStatus,

    -- ** GetJobManifest
    getJobManifest_jobId,
    getJobManifestResponse_manifestURI,
    getJobManifestResponse_httpStatus,

    -- ** GetJobUnlockCode
    getJobUnlockCode_jobId,
    getJobUnlockCodeResponse_unlockCode,
    getJobUnlockCodeResponse_httpStatus,

    -- ** GetSnowballUsage
    getSnowballUsageResponse_snowballsInUse,
    getSnowballUsageResponse_snowballLimit,
    getSnowballUsageResponse_httpStatus,

    -- ** GetSoftwareUpdates
    getSoftwareUpdates_jobId,
    getSoftwareUpdatesResponse_updatesURI,
    getSoftwareUpdatesResponse_httpStatus,

    -- ** ListClusterJobs
    listClusterJobs_nextToken,
    listClusterJobs_maxResults,
    listClusterJobs_clusterId,
    listClusterJobsResponse_nextToken,
    listClusterJobsResponse_jobListEntries,
    listClusterJobsResponse_httpStatus,

    -- ** ListClusters
    listClusters_nextToken,
    listClusters_maxResults,
    listClustersResponse_nextToken,
    listClustersResponse_clusterListEntries,
    listClustersResponse_httpStatus,

    -- ** ListCompatibleImages
    listCompatibleImages_nextToken,
    listCompatibleImages_maxResults,
    listCompatibleImagesResponse_nextToken,
    listCompatibleImagesResponse_compatibleImages,
    listCompatibleImagesResponse_httpStatus,

    -- ** ListJobs
    listJobs_nextToken,
    listJobs_maxResults,
    listJobsResponse_nextToken,
    listJobsResponse_jobListEntries,
    listJobsResponse_httpStatus,

    -- ** ListLongTermPricing
    listLongTermPricing_nextToken,
    listLongTermPricing_maxResults,
    listLongTermPricingResponse_nextToken,
    listLongTermPricingResponse_longTermPricingEntries,
    listLongTermPricingResponse_httpStatus,

    -- ** UpdateCluster
    updateCluster_roleARN,
    updateCluster_forwardingAddressId,
    updateCluster_description,
    updateCluster_notification,
    updateCluster_resources,
    updateCluster_addressId,
    updateCluster_shippingOption,
    updateCluster_onDeviceServiceConfiguration,
    updateCluster_clusterId,
    updateClusterResponse_httpStatus,

    -- ** UpdateJob
    updateJob_roleARN,
    updateJob_forwardingAddressId,
    updateJob_description,
    updateJob_notification,
    updateJob_resources,
    updateJob_snowballCapacityPreference,
    updateJob_addressId,
    updateJob_shippingOption,
    updateJob_onDeviceServiceConfiguration,
    updateJob_jobId,
    updateJobResponse_httpStatus,

    -- ** UpdateJobShipmentState
    updateJobShipmentState_jobId,
    updateJobShipmentState_shipmentState,
    updateJobShipmentStateResponse_httpStatus,

    -- ** UpdateLongTermPricing
    updateLongTermPricing_isLongTermPricingAutoRenew,
    updateLongTermPricing_replacementJob,
    updateLongTermPricing_longTermPricingId,
    updateLongTermPricingResponse_httpStatus,

    -- * Types

    -- ** Address
    address_stateOrProvince,
    address_street1,
    address_isRestricted,
    address_name,
    address_postalCode,
    address_company,
    address_street3,
    address_country,
    address_prefectureOrDistrict,
    address_city,
    address_landmark,
    address_phoneNumber,
    address_street2,
    address_addressId,

    -- ** ClusterListEntry
    clusterListEntry_creationDate,
    clusterListEntry_description,
    clusterListEntry_clusterState,
    clusterListEntry_clusterId,

    -- ** ClusterMetadata
    clusterMetadata_roleARN,
    clusterMetadata_creationDate,
    clusterMetadata_forwardingAddressId,
    clusterMetadata_description,
    clusterMetadata_kmsKeyARN,
    clusterMetadata_clusterState,
    clusterMetadata_notification,
    clusterMetadata_clusterId,
    clusterMetadata_taxDocuments,
    clusterMetadata_resources,
    clusterMetadata_addressId,
    clusterMetadata_snowballType,
    clusterMetadata_jobType,
    clusterMetadata_shippingOption,
    clusterMetadata_onDeviceServiceConfiguration,

    -- ** CompatibleImage
    compatibleImage_amiId,
    compatibleImage_name,

    -- ** DataTransfer
    dataTransfer_objectsTransferred,
    dataTransfer_totalBytes,
    dataTransfer_totalObjects,
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
    jobListEntry_creationDate,
    jobListEntry_jobId,
    jobListEntry_description,
    jobListEntry_isMaster,
    jobListEntry_jobState,
    jobListEntry_snowballType,
    jobListEntry_jobType,

    -- ** JobLogs
    jobLogs_jobSuccessLogURI,
    jobLogs_jobFailureLogURI,
    jobLogs_jobCompletionReportURI,

    -- ** JobMetadata
    jobMetadata_shippingDetails,
    jobMetadata_roleARN,
    jobMetadata_deviceConfiguration,
    jobMetadata_creationDate,
    jobMetadata_jobId,
    jobMetadata_forwardingAddressId,
    jobMetadata_description,
    jobMetadata_kmsKeyARN,
    jobMetadata_longTermPricingId,
    jobMetadata_notification,
    jobMetadata_dataTransferProgress,
    jobMetadata_remoteManagement,
    jobMetadata_clusterId,
    jobMetadata_jobState,
    jobMetadata_taxDocuments,
    jobMetadata_resources,
    jobMetadata_snowballCapacityPreference,
    jobMetadata_addressId,
    jobMetadata_snowballType,
    jobMetadata_jobType,
    jobMetadata_jobLogInfo,
    jobMetadata_onDeviceServiceConfiguration,

    -- ** JobResource
    jobResource_ec2AmiResources,
    jobResource_s3Resources,
    jobResource_lambdaResources,

    -- ** KeyRange
    keyRange_beginMarker,
    keyRange_endMarker,

    -- ** LambdaResource
    lambdaResource_eventTriggers,
    lambdaResource_lambdaArn,

    -- ** LongTermPricingListEntry
    longTermPricingListEntry_longTermPricingStatus,
    longTermPricingListEntry_longTermPricingEndDate,
    longTermPricingListEntry_currentActiveJob,
    longTermPricingListEntry_longTermPricingType,
    longTermPricingListEntry_isLongTermPricingAutoRenew,
    longTermPricingListEntry_longTermPricingId,
    longTermPricingListEntry_replacementJob,
    longTermPricingListEntry_longTermPricingStartDate,
    longTermPricingListEntry_snowballType,
    longTermPricingListEntry_jobIds,

    -- ** NFSOnDeviceServiceConfiguration
    nFSOnDeviceServiceConfiguration_storageUnit,
    nFSOnDeviceServiceConfiguration_storageLimit,

    -- ** Notification
    notification_jobStatesToNotify,
    notification_snsTopicARN,
    notification_notifyAll,

    -- ** OnDeviceServiceConfiguration
    onDeviceServiceConfiguration_nFSOnDeviceService,
    onDeviceServiceConfiguration_tGWOnDeviceService,

    -- ** S3Resource
    s3Resource_targetOnDeviceServices,
    s3Resource_bucketArn,
    s3Resource_keyRange,

    -- ** Shipment
    shipment_status,
    shipment_trackingNumber,

    -- ** ShippingDetails
    shippingDetails_outboundShipment,
    shippingDetails_shippingOption,
    shippingDetails_inboundShipment,

    -- ** SnowconeDeviceConfiguration
    snowconeDeviceConfiguration_wirelessConnection,

    -- ** TGWOnDeviceServiceConfiguration
    tGWOnDeviceServiceConfiguration_storageUnit,
    tGWOnDeviceServiceConfiguration_storageLimit,

    -- ** TargetOnDeviceService
    targetOnDeviceService_transferOption,
    targetOnDeviceService_serviceName,

    -- ** TaxDocuments
    taxDocuments_ind,

    -- ** WirelessConnection
    wirelessConnection_isWifiEnabled,
  )
where

import Amazonka.Snowball.CancelCluster
import Amazonka.Snowball.CancelJob
import Amazonka.Snowball.CreateAddress
import Amazonka.Snowball.CreateCluster
import Amazonka.Snowball.CreateJob
import Amazonka.Snowball.CreateLongTermPricing
import Amazonka.Snowball.CreateReturnShippingLabel
import Amazonka.Snowball.DescribeAddress
import Amazonka.Snowball.DescribeAddresses
import Amazonka.Snowball.DescribeCluster
import Amazonka.Snowball.DescribeJob
import Amazonka.Snowball.DescribeReturnShippingLabel
import Amazonka.Snowball.GetJobManifest
import Amazonka.Snowball.GetJobUnlockCode
import Amazonka.Snowball.GetSnowballUsage
import Amazonka.Snowball.GetSoftwareUpdates
import Amazonka.Snowball.ListClusterJobs
import Amazonka.Snowball.ListClusters
import Amazonka.Snowball.ListCompatibleImages
import Amazonka.Snowball.ListJobs
import Amazonka.Snowball.ListLongTermPricing
import Amazonka.Snowball.Types.Address
import Amazonka.Snowball.Types.ClusterListEntry
import Amazonka.Snowball.Types.ClusterMetadata
import Amazonka.Snowball.Types.CompatibleImage
import Amazonka.Snowball.Types.DataTransfer
import Amazonka.Snowball.Types.DeviceConfiguration
import Amazonka.Snowball.Types.Ec2AmiResource
import Amazonka.Snowball.Types.EventTriggerDefinition
import Amazonka.Snowball.Types.INDTaxDocuments
import Amazonka.Snowball.Types.JobListEntry
import Amazonka.Snowball.Types.JobLogs
import Amazonka.Snowball.Types.JobMetadata
import Amazonka.Snowball.Types.JobResource
import Amazonka.Snowball.Types.KeyRange
import Amazonka.Snowball.Types.LambdaResource
import Amazonka.Snowball.Types.LongTermPricingListEntry
import Amazonka.Snowball.Types.NFSOnDeviceServiceConfiguration
import Amazonka.Snowball.Types.Notification
import Amazonka.Snowball.Types.OnDeviceServiceConfiguration
import Amazonka.Snowball.Types.S3Resource
import Amazonka.Snowball.Types.Shipment
import Amazonka.Snowball.Types.ShippingDetails
import Amazonka.Snowball.Types.SnowconeDeviceConfiguration
import Amazonka.Snowball.Types.TGWOnDeviceServiceConfiguration
import Amazonka.Snowball.Types.TargetOnDeviceService
import Amazonka.Snowball.Types.TaxDocuments
import Amazonka.Snowball.Types.WirelessConnection
import Amazonka.Snowball.UpdateCluster
import Amazonka.Snowball.UpdateJob
import Amazonka.Snowball.UpdateJobShipmentState
import Amazonka.Snowball.UpdateLongTermPricing
