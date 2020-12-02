{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Snow Family is a petabyte-scale data transport solution that uses secure devices to transfer large amounts of data between your on-premises data centers and Amazon Simple Storage Service (Amazon S3). The Snow commands described here provide access to the same functionality that is available in the AWS Snow Family Management Console, which enables you to create and manage jobs for a Snow device. To transfer data locally with a Snow device, you'll need to use the Snowball Edge client or the Amazon S3 API Interface for Snowball or AWS OpsHub for Snow Family. For more information, see the <https://docs.aws.amazon.com/AWSImportExport/latest/ug/api-reference.html User Guide> .
module Network.AWS.Snowball
  ( -- * Service Configuration
    snowball,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelCluster
    module Network.AWS.Snowball.CancelCluster,

    -- ** DescribeCluster
    module Network.AWS.Snowball.DescribeCluster,

    -- ** CreateAddress
    module Network.AWS.Snowball.CreateAddress,

    -- ** CreateReturnShippingLabel
    module Network.AWS.Snowball.CreateReturnShippingLabel,

    -- ** GetSnowballUsage
    module Network.AWS.Snowball.GetSnowballUsage,

    -- ** DescribeAddresses (Paginated)
    module Network.AWS.Snowball.DescribeAddresses,

    -- ** ListCompatibleImages (Paginated)
    module Network.AWS.Snowball.ListCompatibleImages,

    -- ** UpdateCluster
    module Network.AWS.Snowball.UpdateCluster,

    -- ** GetSoftwareUpdates
    module Network.AWS.Snowball.GetSoftwareUpdates,

    -- ** CreateJob
    module Network.AWS.Snowball.CreateJob,

    -- ** GetJobManifest
    module Network.AWS.Snowball.GetJobManifest,

    -- ** CreateCluster
    module Network.AWS.Snowball.CreateCluster,

    -- ** ListJobs (Paginated)
    module Network.AWS.Snowball.ListJobs,

    -- ** UpdateJob
    module Network.AWS.Snowball.UpdateJob,

    -- ** UpdateJobShipmentState
    module Network.AWS.Snowball.UpdateJobShipmentState,

    -- ** GetJobUnlockCode
    module Network.AWS.Snowball.GetJobUnlockCode,

    -- ** ListClusterJobs (Paginated)
    module Network.AWS.Snowball.ListClusterJobs,

    -- ** DescribeJob
    module Network.AWS.Snowball.DescribeJob,

    -- ** ListClusters (Paginated)
    module Network.AWS.Snowball.ListClusters,

    -- ** DescribeAddress
    module Network.AWS.Snowball.DescribeAddress,

    -- ** DescribeReturnShippingLabel
    module Network.AWS.Snowball.DescribeReturnShippingLabel,

    -- ** CancelJob
    module Network.AWS.Snowball.CancelJob,

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
    Address,
    address,
    aIsRestricted,
    aStreet3,
    aLandmark,
    aPostalCode,
    aCountry,
    aStateOrProvince,
    aStreet2,
    aAddressId,
    aCity,
    aPhoneNumber,
    aCompany,
    aName,
    aPrefectureOrDistrict,
    aStreet1,

    -- ** ClusterListEntry
    ClusterListEntry,
    clusterListEntry,
    cleClusterState,
    cleClusterId,
    cleCreationDate,
    cleDescription,

    -- ** ClusterMetadata
    ClusterMetadata,
    clusterMetadata,
    cmJobType,
    cmKMSKeyARN,
    cmClusterState,
    cmNotification,
    cmForwardingAddressId,
    cmAddressId,
    cmSnowballType,
    cmShippingOption,
    cmResources,
    cmClusterId,
    cmCreationDate,
    cmDescription,
    cmTaxDocuments,
    cmRoleARN,

    -- ** CompatibleImage
    CompatibleImage,
    compatibleImage,
    ciName,
    ciAMIId,

    -- ** DataTransfer
    DataTransfer,
    dataTransfer,
    dtTotalObjects,
    dtTotalBytes,
    dtObjectsTransferred,
    dtBytesTransferred,

    -- ** DeviceConfiguration
    DeviceConfiguration,
    deviceConfiguration,
    dcSnowconeDeviceConfiguration,

    -- ** EC2AMIResource
    EC2AMIResource,
    ec2AMIResource,
    earSnowballAMIId,
    earAMIId,

    -- ** EventTriggerDefinition
    EventTriggerDefinition,
    eventTriggerDefinition,
    etdEventResourceARN,

    -- ** INDTaxDocuments
    INDTaxDocuments,
    iNDTaxDocuments,
    indtdGSTIN,

    -- ** JobListEntry
    JobListEntry,
    jobListEntry,
    jleJobType,
    jleJobId,
    jleJobState,
    jleSnowballType,
    jleCreationDate,
    jleDescription,
    jleIsMaster,

    -- ** JobLogs
    JobLogs,
    jobLogs,
    jlJobFailureLogURI,
    jlJobCompletionReportURI,
    jlJobSuccessLogURI,

    -- ** JobMetadata
    JobMetadata,
    jobMetadata,
    jmJobType,
    jmKMSKeyARN,
    jmJobId,
    jmJobLogInfo,
    jmNotification,
    jmJobState,
    jmForwardingAddressId,
    jmShippingDetails,
    jmAddressId,
    jmSnowballType,
    jmDataTransferProgress,
    jmResources,
    jmClusterId,
    jmCreationDate,
    jmDeviceConfiguration,
    jmDescription,
    jmTaxDocuments,
    jmRoleARN,
    jmSnowballCapacityPreference,

    -- ** JobResource
    JobResource,
    jobResource,
    jrEC2AMIResources,
    jrLambdaResources,
    jrS3Resources,

    -- ** KeyRange
    KeyRange,
    keyRange,
    krEndMarker,
    krBeginMarker,

    -- ** LambdaResource
    LambdaResource,
    lambdaResource,
    lrEventTriggers,
    lrLambdaARN,

    -- ** Notification
    Notification,
    notification,
    nNotifyAll,
    nSNSTopicARN,
    nJobStatesToNotify,

    -- ** S3Resource
    S3Resource,
    s3Resource,
    srKeyRange,
    srBucketARN,

    -- ** Shipment
    Shipment,
    shipment,
    sStatus,
    sTrackingNumber,

    -- ** ShippingDetails
    ShippingDetails,
    shippingDetails,
    sdShippingOption,
    sdOutboundShipment,
    sdInboundShipment,

    -- ** SnowconeDeviceConfiguration
    SnowconeDeviceConfiguration,
    snowconeDeviceConfiguration,
    sdcWirelessConnection,

    -- ** TaxDocuments
    TaxDocuments,
    taxDocuments,
    tdIND,

    -- ** WirelessConnection
    WirelessConnection,
    wirelessConnection,
    wcIsWifiEnabled,
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
