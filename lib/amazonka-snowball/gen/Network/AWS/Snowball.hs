{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Snowball is a petabyte-scale data transport solution that uses secure appliances to transfer large amounts of data between your on-premises data centers and Amazon Simple Storage Service (Amazon S3). The Snowball commands described here provide access to the same functionality that is available in the AWS Snowball Management Console, which enables you to create and manage jobs for Snowball. To transfer data locally with a Snowball appliance, you'll need to use the Snowball client or the Amazon S3 API adapter for Snowball. For more information, see the <http://docs.aws.amazon.com/AWSImportExport/latest/ug/api-reference.html User Guide> .
--
--
module Network.AWS.Snowball
    (
    -- * Service Configuration
      snowball

    -- * Errors
    -- $errors

    -- ** InvalidResourceException
    , _InvalidResourceException

    -- ** UnsupportedAddressException
    , _UnsupportedAddressException

    -- ** KMSRequestFailedException
    , _KMSRequestFailedException

    -- ** InvalidJobStateException
    , _InvalidJobStateException

    -- ** InvalidInputCombinationException
    , _InvalidInputCombinationException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** InvalidAddressException
    , _InvalidAddressException

    -- ** ClusterLimitExceededException
    , _ClusterLimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelCluster
    , module Network.AWS.Snowball.CancelCluster

    -- ** DescribeCluster
    , module Network.AWS.Snowball.DescribeCluster

    -- ** CreateAddress
    , module Network.AWS.Snowball.CreateAddress

    -- ** GetSnowballUsage
    , module Network.AWS.Snowball.GetSnowballUsage

    -- ** DescribeAddresses (Paginated)
    , module Network.AWS.Snowball.DescribeAddresses

    -- ** UpdateCluster
    , module Network.AWS.Snowball.UpdateCluster

    -- ** CreateJob
    , module Network.AWS.Snowball.CreateJob

    -- ** GetJobManifest
    , module Network.AWS.Snowball.GetJobManifest

    -- ** CreateCluster
    , module Network.AWS.Snowball.CreateCluster

    -- ** ListJobs (Paginated)
    , module Network.AWS.Snowball.ListJobs

    -- ** UpdateJob
    , module Network.AWS.Snowball.UpdateJob

    -- ** GetJobUnlockCode
    , module Network.AWS.Snowball.GetJobUnlockCode

    -- ** ListClusterJobs
    , module Network.AWS.Snowball.ListClusterJobs

    -- ** DescribeJob
    , module Network.AWS.Snowball.DescribeJob

    -- ** ListClusters
    , module Network.AWS.Snowball.ListClusters

    -- ** DescribeAddress
    , module Network.AWS.Snowball.DescribeAddress

    -- ** CancelJob
    , module Network.AWS.Snowball.CancelJob

    -- * Types

    -- ** ClusterState
    , ClusterState (..)

    -- ** JobState
    , JobState (..)

    -- ** JobType
    , JobType (..)

    -- ** ShippingOption
    , ShippingOption (..)

    -- ** SnowballCapacity
    , SnowballCapacity (..)

    -- ** SnowballType
    , SnowballType (..)

    -- ** Address
    , Address
    , address
    , aIsRestricted
    , aStreet3
    , aLandmark
    , aPostalCode
    , aCountry
    , aStateOrProvince
    , aStreet2
    , aAddressId
    , aCity
    , aPhoneNumber
    , aCompany
    , aName
    , aPrefectureOrDistrict
    , aStreet1

    -- ** ClusterListEntry
    , ClusterListEntry
    , clusterListEntry
    , cleClusterState
    , cleClusterId
    , cleCreationDate
    , cleDescription

    -- ** ClusterMetadata
    , ClusterMetadata
    , clusterMetadata
    , cmJobType
    , cmKMSKeyARN
    , cmClusterState
    , cmNotification
    , cmForwardingAddressId
    , cmAddressId
    , cmSnowballType
    , cmShippingOption
    , cmResources
    , cmClusterId
    , cmCreationDate
    , cmDescription
    , cmRoleARN

    -- ** DataTransfer
    , DataTransfer
    , dataTransfer
    , dtTotalObjects
    , dtTotalBytes
    , dtObjectsTransferred
    , dtBytesTransferred

    -- ** EventTriggerDefinition
    , EventTriggerDefinition
    , eventTriggerDefinition
    , etdEventResourceARN

    -- ** JobListEntry
    , JobListEntry
    , jobListEntry
    , jleJobType
    , jleJobId
    , jleJobState
    , jleSnowballType
    , jleCreationDate
    , jleDescription
    , jleIsMaster

    -- ** JobLogs
    , JobLogs
    , jobLogs
    , jlJobFailureLogURI
    , jlJobCompletionReportURI
    , jlJobSuccessLogURI

    -- ** JobMetadata
    , JobMetadata
    , jobMetadata
    , jmJobType
    , jmKMSKeyARN
    , jmJobId
    , jmJobLogInfo
    , jmNotification
    , jmJobState
    , jmForwardingAddressId
    , jmShippingDetails
    , jmAddressId
    , jmSnowballType
    , jmDataTransferProgress
    , jmResources
    , jmClusterId
    , jmCreationDate
    , jmDescription
    , jmRoleARN
    , jmSnowballCapacityPreference

    -- ** JobResource
    , JobResource
    , jobResource
    , jrLambdaResources
    , jrS3Resources

    -- ** KeyRange
    , KeyRange
    , keyRange
    , krEndMarker
    , krBeginMarker

    -- ** LambdaResource
    , LambdaResource
    , lambdaResource
    , lrEventTriggers
    , lrLambdaARN

    -- ** Notification
    , Notification
    , notification
    , nNotifyAll
    , nSNSTopicARN
    , nJobStatesToNotify

    -- ** S3Resource
    , S3Resource
    , s3Resource
    , srKeyRange
    , srBucketARN

    -- ** Shipment
    , Shipment
    , shipment
    , sStatus
    , sTrackingNumber

    -- ** ShippingDetails
    , ShippingDetails
    , shippingDetails
    , sdShippingOption
    , sdOutboundShipment
    , sdInboundShipment
    ) where

import Network.AWS.Snowball.CancelCluster
import Network.AWS.Snowball.CancelJob
import Network.AWS.Snowball.CreateAddress
import Network.AWS.Snowball.CreateCluster
import Network.AWS.Snowball.CreateJob
import Network.AWS.Snowball.DescribeAddress
import Network.AWS.Snowball.DescribeAddresses
import Network.AWS.Snowball.DescribeCluster
import Network.AWS.Snowball.DescribeJob
import Network.AWS.Snowball.GetJobManifest
import Network.AWS.Snowball.GetJobUnlockCode
import Network.AWS.Snowball.GetSnowballUsage
import Network.AWS.Snowball.ListClusterJobs
import Network.AWS.Snowball.ListClusters
import Network.AWS.Snowball.ListJobs
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.UpdateCluster
import Network.AWS.Snowball.UpdateJob
import Network.AWS.Snowball.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Snowball'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
