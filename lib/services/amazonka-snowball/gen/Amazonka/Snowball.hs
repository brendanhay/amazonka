{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Snowball
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-06-30@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Snow Family is a petabyte-scale data transport solution that uses
-- secure devices to transfer large amounts of data between your
-- on-premises data centers and Amazon Simple Storage Service (Amazon S3).
-- The Snow commands described here provide access to the same
-- functionality that is available in the AWS Snow Family Management
-- Console, which enables you to create and manage jobs for a Snow device.
-- To transfer data locally with a Snow device, you\'ll need to use the
-- Snowball Edge client or the Amazon S3 API Interface for Snowball or AWS
-- OpsHub for Snow Family. For more information, see the
-- <https://docs.aws.amazon.com/AWSImportExport/latest/ug/api-reference.html User Guide>.
module Amazonka.Snowball
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidResourceException
    _InvalidResourceException,

    -- ** UnsupportedAddressException
    _UnsupportedAddressException,

    -- ** ReturnShippingLabelAlreadyExistsException
    _ReturnShippingLabelAlreadyExistsException,

    -- ** KMSRequestFailedException
    _KMSRequestFailedException,

    -- ** InvalidJobStateException
    _InvalidJobStateException,

    -- ** InvalidInputCombinationException
    _InvalidInputCombinationException,

    -- ** ConflictException
    _ConflictException,

    -- ** Ec2RequestFailedException
    _Ec2RequestFailedException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidAddressException
    _InvalidAddressException,

    -- ** ClusterLimitExceededException
    _ClusterLimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelCluster
    CancelCluster (CancelCluster'),
    newCancelCluster,
    CancelClusterResponse (CancelClusterResponse'),
    newCancelClusterResponse,

    -- ** DescribeCluster
    DescribeCluster (DescribeCluster'),
    newDescribeCluster,
    DescribeClusterResponse (DescribeClusterResponse'),
    newDescribeClusterResponse,

    -- ** CreateAddress
    CreateAddress (CreateAddress'),
    newCreateAddress,
    CreateAddressResponse (CreateAddressResponse'),
    newCreateAddressResponse,

    -- ** CreateReturnShippingLabel
    CreateReturnShippingLabel (CreateReturnShippingLabel'),
    newCreateReturnShippingLabel,
    CreateReturnShippingLabelResponse (CreateReturnShippingLabelResponse'),
    newCreateReturnShippingLabelResponse,

    -- ** GetSnowballUsage
    GetSnowballUsage (GetSnowballUsage'),
    newGetSnowballUsage,
    GetSnowballUsageResponse (GetSnowballUsageResponse'),
    newGetSnowballUsageResponse,

    -- ** DescribeAddresses (Paginated)
    DescribeAddresses (DescribeAddresses'),
    newDescribeAddresses,
    DescribeAddressesResponse (DescribeAddressesResponse'),
    newDescribeAddressesResponse,

    -- ** ListCompatibleImages (Paginated)
    ListCompatibleImages (ListCompatibleImages'),
    newListCompatibleImages,
    ListCompatibleImagesResponse (ListCompatibleImagesResponse'),
    newListCompatibleImagesResponse,

    -- ** CreateLongTermPricing
    CreateLongTermPricing (CreateLongTermPricing'),
    newCreateLongTermPricing,
    CreateLongTermPricingResponse (CreateLongTermPricingResponse'),
    newCreateLongTermPricingResponse,

    -- ** UpdateCluster
    UpdateCluster (UpdateCluster'),
    newUpdateCluster,
    UpdateClusterResponse (UpdateClusterResponse'),
    newUpdateClusterResponse,

    -- ** GetSoftwareUpdates
    GetSoftwareUpdates (GetSoftwareUpdates'),
    newGetSoftwareUpdates,
    GetSoftwareUpdatesResponse (GetSoftwareUpdatesResponse'),
    newGetSoftwareUpdatesResponse,

    -- ** CreateJob
    CreateJob (CreateJob'),
    newCreateJob,
    CreateJobResponse (CreateJobResponse'),
    newCreateJobResponse,

    -- ** ListLongTermPricing
    ListLongTermPricing (ListLongTermPricing'),
    newListLongTermPricing,
    ListLongTermPricingResponse (ListLongTermPricingResponse'),
    newListLongTermPricingResponse,

    -- ** GetJobManifest
    GetJobManifest (GetJobManifest'),
    newGetJobManifest,
    GetJobManifestResponse (GetJobManifestResponse'),
    newGetJobManifestResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** UpdateJob
    UpdateJob (UpdateJob'),
    newUpdateJob,
    UpdateJobResponse (UpdateJobResponse'),
    newUpdateJobResponse,

    -- ** UpdateJobShipmentState
    UpdateJobShipmentState (UpdateJobShipmentState'),
    newUpdateJobShipmentState,
    UpdateJobShipmentStateResponse (UpdateJobShipmentStateResponse'),
    newUpdateJobShipmentStateResponse,

    -- ** GetJobUnlockCode
    GetJobUnlockCode (GetJobUnlockCode'),
    newGetJobUnlockCode,
    GetJobUnlockCodeResponse (GetJobUnlockCodeResponse'),
    newGetJobUnlockCodeResponse,

    -- ** ListClusterJobs (Paginated)
    ListClusterJobs (ListClusterJobs'),
    newListClusterJobs,
    ListClusterJobsResponse (ListClusterJobsResponse'),
    newListClusterJobsResponse,

    -- ** DescribeJob
    DescribeJob (DescribeJob'),
    newDescribeJob,
    DescribeJobResponse (DescribeJobResponse'),
    newDescribeJobResponse,

    -- ** UpdateLongTermPricing
    UpdateLongTermPricing (UpdateLongTermPricing'),
    newUpdateLongTermPricing,
    UpdateLongTermPricingResponse (UpdateLongTermPricingResponse'),
    newUpdateLongTermPricingResponse,

    -- ** ListClusters (Paginated)
    ListClusters (ListClusters'),
    newListClusters,
    ListClustersResponse (ListClustersResponse'),
    newListClustersResponse,

    -- ** DescribeAddress
    DescribeAddress (DescribeAddress'),
    newDescribeAddress,
    DescribeAddressResponse (DescribeAddressResponse'),
    newDescribeAddressResponse,

    -- ** DescribeReturnShippingLabel
    DescribeReturnShippingLabel (DescribeReturnShippingLabel'),
    newDescribeReturnShippingLabel,
    DescribeReturnShippingLabelResponse (DescribeReturnShippingLabelResponse'),
    newDescribeReturnShippingLabelResponse,

    -- ** CancelJob
    CancelJob (CancelJob'),
    newCancelJob,
    CancelJobResponse (CancelJobResponse'),
    newCancelJobResponse,

    -- * Types

    -- ** ClusterState
    ClusterState (..),

    -- ** DeviceServiceName
    DeviceServiceName (..),

    -- ** JobState
    JobState (..),

    -- ** JobType
    JobType (..),

    -- ** LongTermPricingType
    LongTermPricingType (..),

    -- ** RemoteManagement
    RemoteManagement (..),

    -- ** ShipmentState
    ShipmentState (..),

    -- ** ShippingLabelStatus
    ShippingLabelStatus (..),

    -- ** ShippingOption
    ShippingOption (..),

    -- ** SnowballCapacity
    SnowballCapacity (..),

    -- ** SnowballType
    SnowballType (..),

    -- ** StorageUnit
    StorageUnit (..),

    -- ** TransferOption
    TransferOption (..),

    -- ** Address
    Address (Address'),
    newAddress,

    -- ** ClusterListEntry
    ClusterListEntry (ClusterListEntry'),
    newClusterListEntry,

    -- ** ClusterMetadata
    ClusterMetadata (ClusterMetadata'),
    newClusterMetadata,

    -- ** CompatibleImage
    CompatibleImage (CompatibleImage'),
    newCompatibleImage,

    -- ** DataTransfer
    DataTransfer (DataTransfer'),
    newDataTransfer,

    -- ** DeviceConfiguration
    DeviceConfiguration (DeviceConfiguration'),
    newDeviceConfiguration,

    -- ** Ec2AmiResource
    Ec2AmiResource (Ec2AmiResource'),
    newEc2AmiResource,

    -- ** EventTriggerDefinition
    EventTriggerDefinition (EventTriggerDefinition'),
    newEventTriggerDefinition,

    -- ** INDTaxDocuments
    INDTaxDocuments (INDTaxDocuments'),
    newINDTaxDocuments,

    -- ** JobListEntry
    JobListEntry (JobListEntry'),
    newJobListEntry,

    -- ** JobLogs
    JobLogs (JobLogs'),
    newJobLogs,

    -- ** JobMetadata
    JobMetadata (JobMetadata'),
    newJobMetadata,

    -- ** JobResource
    JobResource (JobResource'),
    newJobResource,

    -- ** KeyRange
    KeyRange (KeyRange'),
    newKeyRange,

    -- ** LambdaResource
    LambdaResource (LambdaResource'),
    newLambdaResource,

    -- ** LongTermPricingListEntry
    LongTermPricingListEntry (LongTermPricingListEntry'),
    newLongTermPricingListEntry,

    -- ** NFSOnDeviceServiceConfiguration
    NFSOnDeviceServiceConfiguration (NFSOnDeviceServiceConfiguration'),
    newNFSOnDeviceServiceConfiguration,

    -- ** Notification
    Notification (Notification'),
    newNotification,

    -- ** OnDeviceServiceConfiguration
    OnDeviceServiceConfiguration (OnDeviceServiceConfiguration'),
    newOnDeviceServiceConfiguration,

    -- ** S3Resource
    S3Resource (S3Resource'),
    newS3Resource,

    -- ** Shipment
    Shipment (Shipment'),
    newShipment,

    -- ** ShippingDetails
    ShippingDetails (ShippingDetails'),
    newShippingDetails,

    -- ** SnowconeDeviceConfiguration
    SnowconeDeviceConfiguration (SnowconeDeviceConfiguration'),
    newSnowconeDeviceConfiguration,

    -- ** TargetOnDeviceService
    TargetOnDeviceService (TargetOnDeviceService'),
    newTargetOnDeviceService,

    -- ** TaxDocuments
    TaxDocuments (TaxDocuments'),
    newTaxDocuments,

    -- ** WirelessConnection
    WirelessConnection (WirelessConnection'),
    newWirelessConnection,
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
import Amazonka.Snowball.Lens
import Amazonka.Snowball.ListClusterJobs
import Amazonka.Snowball.ListClusters
import Amazonka.Snowball.ListCompatibleImages
import Amazonka.Snowball.ListJobs
import Amazonka.Snowball.ListLongTermPricing
import Amazonka.Snowball.Types
import Amazonka.Snowball.UpdateCluster
import Amazonka.Snowball.UpdateJob
import Amazonka.Snowball.UpdateJobShipmentState
import Amazonka.Snowball.UpdateLongTermPricing
import Amazonka.Snowball.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Snowball'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
