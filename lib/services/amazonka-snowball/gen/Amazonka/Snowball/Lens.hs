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
    createCluster_description,
    createCluster_forwardingAddressId,
    createCluster_kmsKeyARN,
    createCluster_notification,
    createCluster_onDeviceServiceConfiguration,
    createCluster_remoteManagement,
    createCluster_taxDocuments,
    createCluster_jobType,
    createCluster_resources,
    createCluster_addressId,
    createCluster_roleARN,
    createCluster_snowballType,
    createCluster_shippingOption,
    createClusterResponse_clusterId,
    createClusterResponse_httpStatus,

    -- ** CreateJob
    createJob_addressId,
    createJob_clusterId,
    createJob_description,
    createJob_deviceConfiguration,
    createJob_forwardingAddressId,
    createJob_jobType,
    createJob_kmsKeyARN,
    createJob_longTermPricingId,
    createJob_notification,
    createJob_onDeviceServiceConfiguration,
    createJob_remoteManagement,
    createJob_resources,
    createJob_roleARN,
    createJob_shippingOption,
    createJob_snowballCapacityPreference,
    createJob_snowballType,
    createJob_taxDocuments,
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
    describeAddresses_maxResults,
    describeAddresses_nextToken,
    describeAddressesResponse_addresses,
    describeAddressesResponse_nextToken,
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
    describeReturnShippingLabelResponse_expirationDate,
    describeReturnShippingLabelResponse_returnShippingLabelURI,
    describeReturnShippingLabelResponse_status,
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
    getSnowballUsageResponse_snowballLimit,
    getSnowballUsageResponse_snowballsInUse,
    getSnowballUsageResponse_httpStatus,

    -- ** GetSoftwareUpdates
    getSoftwareUpdates_jobId,
    getSoftwareUpdatesResponse_updatesURI,
    getSoftwareUpdatesResponse_httpStatus,

    -- ** ListClusterJobs
    listClusterJobs_maxResults,
    listClusterJobs_nextToken,
    listClusterJobs_clusterId,
    listClusterJobsResponse_jobListEntries,
    listClusterJobsResponse_nextToken,
    listClusterJobsResponse_httpStatus,

    -- ** ListClusters
    listClusters_maxResults,
    listClusters_nextToken,
    listClustersResponse_clusterListEntries,
    listClustersResponse_nextToken,
    listClustersResponse_httpStatus,

    -- ** ListCompatibleImages
    listCompatibleImages_maxResults,
    listCompatibleImages_nextToken,
    listCompatibleImagesResponse_compatibleImages,
    listCompatibleImagesResponse_nextToken,
    listCompatibleImagesResponse_httpStatus,

    -- ** ListJobs
    listJobs_maxResults,
    listJobs_nextToken,
    listJobsResponse_jobListEntries,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,

    -- ** ListLongTermPricing
    listLongTermPricing_maxResults,
    listLongTermPricing_nextToken,
    listLongTermPricingResponse_longTermPricingEntries,
    listLongTermPricingResponse_nextToken,
    listLongTermPricingResponse_httpStatus,

    -- ** UpdateCluster
    updateCluster_addressId,
    updateCluster_description,
    updateCluster_forwardingAddressId,
    updateCluster_notification,
    updateCluster_onDeviceServiceConfiguration,
    updateCluster_resources,
    updateCluster_roleARN,
    updateCluster_shippingOption,
    updateCluster_clusterId,
    updateClusterResponse_httpStatus,

    -- ** UpdateJob
    updateJob_addressId,
    updateJob_description,
    updateJob_forwardingAddressId,
    updateJob_notification,
    updateJob_onDeviceServiceConfiguration,
    updateJob_resources,
    updateJob_roleARN,
    updateJob_shippingOption,
    updateJob_snowballCapacityPreference,
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
    address_addressId,
    address_city,
    address_company,
    address_country,
    address_isRestricted,
    address_landmark,
    address_name,
    address_phoneNumber,
    address_postalCode,
    address_prefectureOrDistrict,
    address_stateOrProvince,
    address_street1,
    address_street2,
    address_street3,

    -- ** ClusterListEntry
    clusterListEntry_clusterId,
    clusterListEntry_clusterState,
    clusterListEntry_creationDate,
    clusterListEntry_description,

    -- ** ClusterMetadata
    clusterMetadata_addressId,
    clusterMetadata_clusterId,
    clusterMetadata_clusterState,
    clusterMetadata_creationDate,
    clusterMetadata_description,
    clusterMetadata_forwardingAddressId,
    clusterMetadata_jobType,
    clusterMetadata_kmsKeyARN,
    clusterMetadata_notification,
    clusterMetadata_onDeviceServiceConfiguration,
    clusterMetadata_resources,
    clusterMetadata_roleARN,
    clusterMetadata_shippingOption,
    clusterMetadata_snowballType,
    clusterMetadata_taxDocuments,

    -- ** CompatibleImage
    compatibleImage_amiId,
    compatibleImage_name,

    -- ** DataTransfer
    dataTransfer_bytesTransferred,
    dataTransfer_objectsTransferred,
    dataTransfer_totalBytes,
    dataTransfer_totalObjects,

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
    jobListEntry_description,
    jobListEntry_isMaster,
    jobListEntry_jobId,
    jobListEntry_jobState,
    jobListEntry_jobType,
    jobListEntry_snowballType,

    -- ** JobLogs
    jobLogs_jobCompletionReportURI,
    jobLogs_jobFailureLogURI,
    jobLogs_jobSuccessLogURI,

    -- ** JobMetadata
    jobMetadata_addressId,
    jobMetadata_clusterId,
    jobMetadata_creationDate,
    jobMetadata_dataTransferProgress,
    jobMetadata_description,
    jobMetadata_deviceConfiguration,
    jobMetadata_forwardingAddressId,
    jobMetadata_jobId,
    jobMetadata_jobLogInfo,
    jobMetadata_jobState,
    jobMetadata_jobType,
    jobMetadata_kmsKeyARN,
    jobMetadata_longTermPricingId,
    jobMetadata_notification,
    jobMetadata_onDeviceServiceConfiguration,
    jobMetadata_remoteManagement,
    jobMetadata_resources,
    jobMetadata_roleARN,
    jobMetadata_shippingDetails,
    jobMetadata_snowballCapacityPreference,
    jobMetadata_snowballType,
    jobMetadata_taxDocuments,

    -- ** JobResource
    jobResource_ec2AmiResources,
    jobResource_lambdaResources,
    jobResource_s3Resources,

    -- ** KeyRange
    keyRange_beginMarker,
    keyRange_endMarker,

    -- ** LambdaResource
    lambdaResource_eventTriggers,
    lambdaResource_lambdaArn,

    -- ** LongTermPricingListEntry
    longTermPricingListEntry_currentActiveJob,
    longTermPricingListEntry_isLongTermPricingAutoRenew,
    longTermPricingListEntry_jobIds,
    longTermPricingListEntry_longTermPricingEndDate,
    longTermPricingListEntry_longTermPricingId,
    longTermPricingListEntry_longTermPricingStartDate,
    longTermPricingListEntry_longTermPricingStatus,
    longTermPricingListEntry_longTermPricingType,
    longTermPricingListEntry_replacementJob,
    longTermPricingListEntry_snowballType,

    -- ** NFSOnDeviceServiceConfiguration
    nFSOnDeviceServiceConfiguration_storageLimit,
    nFSOnDeviceServiceConfiguration_storageUnit,

    -- ** Notification
    notification_jobStatesToNotify,
    notification_notifyAll,
    notification_snsTopicARN,

    -- ** OnDeviceServiceConfiguration
    onDeviceServiceConfiguration_nFSOnDeviceService,
    onDeviceServiceConfiguration_tGWOnDeviceService,

    -- ** S3Resource
    s3Resource_bucketArn,
    s3Resource_keyRange,
    s3Resource_targetOnDeviceServices,

    -- ** Shipment
    shipment_status,
    shipment_trackingNumber,

    -- ** ShippingDetails
    shippingDetails_inboundShipment,
    shippingDetails_outboundShipment,
    shippingDetails_shippingOption,

    -- ** SnowconeDeviceConfiguration
    snowconeDeviceConfiguration_wirelessConnection,

    -- ** TGWOnDeviceServiceConfiguration
    tGWOnDeviceServiceConfiguration_storageLimit,
    tGWOnDeviceServiceConfiguration_storageUnit,

    -- ** TargetOnDeviceService
    targetOnDeviceService_serviceName,
    targetOnDeviceService_transferOption,

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
