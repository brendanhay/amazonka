{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
module Network.AWS.Snowball
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidResourceException
    _InvalidResourceException,

    -- ** InvalidInputCombinationException
    _InvalidInputCombinationException,

    -- ** ClusterLimitExceededException
    _ClusterLimitExceededException,

    -- ** InvalidAddressException
    _InvalidAddressException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** ReturnShippingLabelAlreadyExistsException
    _ReturnShippingLabelAlreadyExistsException,

    -- ** UnsupportedAddressException
    _UnsupportedAddressException,

    -- ** ConflictException
    _ConflictException,

    -- ** Ec2RequestFailedException
    _Ec2RequestFailedException,

    -- ** InvalidJobStateException
    _InvalidJobStateException,

    -- ** KMSRequestFailedException
    _KMSRequestFailedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListClusterJobs (Paginated)
    ListClusterJobs (ListClusterJobs'),
    newListClusterJobs,
    ListClusterJobsResponse (ListClusterJobsResponse'),
    newListClusterJobsResponse,

    -- ** CancelJob
    CancelJob (CancelJob'),
    newCancelJob,
    CancelJobResponse (CancelJobResponse'),
    newCancelJobResponse,

    -- ** UpdateJobShipmentState
    UpdateJobShipmentState (UpdateJobShipmentState'),
    newUpdateJobShipmentState,
    UpdateJobShipmentStateResponse (UpdateJobShipmentStateResponse'),
    newUpdateJobShipmentStateResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** UpdateJob
    UpdateJob (UpdateJob'),
    newUpdateJob,
    UpdateJobResponse (UpdateJobResponse'),
    newUpdateJobResponse,

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

    -- ** GetSoftwareUpdates
    GetSoftwareUpdates (GetSoftwareUpdates'),
    newGetSoftwareUpdates,
    GetSoftwareUpdatesResponse (GetSoftwareUpdatesResponse'),
    newGetSoftwareUpdatesResponse,

    -- ** ListCompatibleImages (Paginated)
    ListCompatibleImages (ListCompatibleImages'),
    newListCompatibleImages,
    ListCompatibleImagesResponse (ListCompatibleImagesResponse'),
    newListCompatibleImagesResponse,

    -- ** DescribeAddresses (Paginated)
    DescribeAddresses (DescribeAddresses'),
    newDescribeAddresses,
    DescribeAddressesResponse (DescribeAddressesResponse'),
    newDescribeAddressesResponse,

    -- ** DescribeJob
    DescribeJob (DescribeJob'),
    newDescribeJob,
    DescribeJobResponse (DescribeJobResponse'),
    newDescribeJobResponse,

    -- ** DescribeCluster
    DescribeCluster (DescribeCluster'),
    newDescribeCluster,
    DescribeClusterResponse (DescribeClusterResponse'),
    newDescribeClusterResponse,

    -- ** CancelCluster
    CancelCluster (CancelCluster'),
    newCancelCluster,
    CancelClusterResponse (CancelClusterResponse'),
    newCancelClusterResponse,

    -- ** GetJobUnlockCode
    GetJobUnlockCode (GetJobUnlockCode'),
    newGetJobUnlockCode,
    GetJobUnlockCodeResponse (GetJobUnlockCodeResponse'),
    newGetJobUnlockCodeResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** GetJobManifest
    GetJobManifest (GetJobManifest'),
    newGetJobManifest,
    GetJobManifestResponse (GetJobManifestResponse'),
    newGetJobManifestResponse,

    -- ** CreateJob
    CreateJob (CreateJob'),
    newCreateJob,
    CreateJobResponse (CreateJobResponse'),
    newCreateJobResponse,

    -- ** UpdateCluster
    UpdateCluster (UpdateCluster'),
    newUpdateCluster,
    UpdateClusterResponse (UpdateClusterResponse'),
    newUpdateClusterResponse,

    -- ** ListClusters (Paginated)
    ListClusters (ListClusters'),
    newListClusters,
    ListClustersResponse (ListClustersResponse'),
    newListClustersResponse,

    -- ** GetSnowballUsage
    GetSnowballUsage (GetSnowballUsage'),
    newGetSnowballUsage,
    GetSnowballUsageResponse (GetSnowballUsageResponse'),
    newGetSnowballUsageResponse,

    -- ** CreateReturnShippingLabel
    CreateReturnShippingLabel (CreateReturnShippingLabel'),
    newCreateReturnShippingLabel,
    CreateReturnShippingLabelResponse (CreateReturnShippingLabelResponse'),
    newCreateReturnShippingLabelResponse,

    -- ** CreateAddress
    CreateAddress (CreateAddress'),
    newCreateAddress,
    CreateAddressResponse (CreateAddressResponse'),
    newCreateAddressResponse,

    -- * Types

    -- ** ClusterState
    ClusterState (..),

    -- ** JobState
    JobState (..),

    -- ** JobType
    JobType (..),

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

    -- ** Notification
    Notification (Notification'),
    newNotification,

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

    -- ** TaxDocuments
    TaxDocuments (TaxDocuments'),
    newTaxDocuments,

    -- ** WirelessConnection
    WirelessConnection (WirelessConnection'),
    newWirelessConnection,
  )
where

import Network.AWS.Snowball.CancelCluster
import Network.AWS.Snowball.CancelJob
import Network.AWS.Snowball.CreateAddress
import Network.AWS.Snowball.CreateCluster
import Network.AWS.Snowball.CreateJob
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
import Network.AWS.Snowball.Lens
import Network.AWS.Snowball.ListClusterJobs
import Network.AWS.Snowball.ListClusters
import Network.AWS.Snowball.ListCompatibleImages
import Network.AWS.Snowball.ListJobs
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.UpdateCluster
import Network.AWS.Snowball.UpdateJob
import Network.AWS.Snowball.UpdateJobShipmentState
import Network.AWS.Snowball.Waiters

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
