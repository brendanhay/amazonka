{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is a test of the welcome page front matter.
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

    -- ** InvalidAddressException
    , _InvalidAddressException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateAddress
    , module Network.AWS.Snowball.CreateAddress

    -- ** GetSnowballUsage
    , module Network.AWS.Snowball.GetSnowballUsage

    -- ** DescribeAddresses
    , module Network.AWS.Snowball.DescribeAddresses

    -- ** CreateJob
    , module Network.AWS.Snowball.CreateJob

    -- ** GetJobManifest
    , module Network.AWS.Snowball.GetJobManifest

    -- ** ListJobs
    , module Network.AWS.Snowball.ListJobs

    -- ** UpdateJob
    , module Network.AWS.Snowball.UpdateJob

    -- ** GetJobUnlockCode
    , module Network.AWS.Snowball.GetJobUnlockCode

    -- ** DescribeJob
    , module Network.AWS.Snowball.DescribeJob

    -- ** DescribeAddress
    , module Network.AWS.Snowball.DescribeAddress

    -- ** CancelJob
    , module Network.AWS.Snowball.CancelJob

    -- * Types

    -- ** JobState
    , JobState (..)

    -- ** JobType
    , JobType (..)

    -- ** ShippingOption
    , ShippingOption (..)

    -- ** SnowballCapacity
    , SnowballCapacity (..)

    -- ** Address
    , Address
    , address
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

    -- ** DataTransfer
    , DataTransfer
    , dataTransfer
    , dtTotalObjects
    , dtTotalBytes
    , dtObjectsTransferred
    , dtBytesTransferred

    -- ** JobListEntry
    , JobListEntry
    , jobListEntry
    , jleJobId
    , jleJobState
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
    , jmShippingDetails
    , jmAddressId
    , jmDataTransferProgress
    , jmResources
    , jmCreationDate
    , jmDescription
    , jmRoleARN
    , jmSnowballCapacityPreference

    -- ** JobResource
    , JobResource
    , jobResource
    , jrS3Resources

    -- ** KeyRange
    , KeyRange
    , keyRange
    , krEndMarker
    , krBeginMarker

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

import           Network.AWS.Snowball.CancelJob
import           Network.AWS.Snowball.CreateAddress
import           Network.AWS.Snowball.CreateJob
import           Network.AWS.Snowball.DescribeAddress
import           Network.AWS.Snowball.DescribeAddresses
import           Network.AWS.Snowball.DescribeJob
import           Network.AWS.Snowball.GetJobManifest
import           Network.AWS.Snowball.GetJobUnlockCode
import           Network.AWS.Snowball.GetSnowballUsage
import           Network.AWS.Snowball.ListJobs
import           Network.AWS.Snowball.Types
import           Network.AWS.Snowball.UpdateJob
import           Network.AWS.Snowball.Waiters

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
