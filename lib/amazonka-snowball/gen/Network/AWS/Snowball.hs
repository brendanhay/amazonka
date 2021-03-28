{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

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
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** InvalidResourceException
    , _InvalidResourceException

    -- ** UnsupportedAddressException
    , _UnsupportedAddressException

    -- ** ReturnShippingLabelAlreadyExistsException
    , _ReturnShippingLabelAlreadyExistsException

    -- ** KMSRequestFailedException
    , _KMSRequestFailedException

    -- ** InvalidJobStateException
    , _InvalidJobStateException

    -- ** InvalidInputCombinationException
    , _InvalidInputCombinationException

    -- ** ConflictException
    , _ConflictException

    -- ** Ec2RequestFailedException
    , _Ec2RequestFailedException

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

    -- ** CreateReturnShippingLabel 
    , module Network.AWS.Snowball.CreateReturnShippingLabel

    -- ** GetSnowballUsage 
    , module Network.AWS.Snowball.GetSnowballUsage

    -- ** DescribeAddresses (Paginated)
    , module Network.AWS.Snowball.DescribeAddresses

    -- ** ListCompatibleImages (Paginated)
    , module Network.AWS.Snowball.ListCompatibleImages

    -- ** UpdateCluster 
    , module Network.AWS.Snowball.UpdateCluster

    -- ** GetSoftwareUpdates 
    , module Network.AWS.Snowball.GetSoftwareUpdates

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

    -- ** UpdateJobShipmentState 
    , module Network.AWS.Snowball.UpdateJobShipmentState

    -- ** GetJobUnlockCode 
    , module Network.AWS.Snowball.GetJobUnlockCode

    -- ** ListClusterJobs (Paginated)
    , module Network.AWS.Snowball.ListClusterJobs

    -- ** DescribeJob 
    , module Network.AWS.Snowball.DescribeJob

    -- ** ListClusters (Paginated)
    , module Network.AWS.Snowball.ListClusters

    -- ** DescribeAddress 
    , module Network.AWS.Snowball.DescribeAddress

    -- ** DescribeReturnShippingLabel 
    , module Network.AWS.Snowball.DescribeReturnShippingLabel

    -- ** CancelJob 
    , module Network.AWS.Snowball.CancelJob

    -- * Types

    -- ** JobType
    , JobType (..)

    -- ** KmsKeyARN
    , KmsKeyARN (..)

    -- ** GSTIN
    , GSTIN (..)

    -- ** INDTaxDocuments
    , INDTaxDocuments (..)
    , mkINDTaxDocuments
    , indtdGSTIN

    -- ** S3Resource
    , S3Resource (..)
    , mkS3Resource
    , srBucketArn
    , srKeyRange

    -- ** SnsTopicARN
    , SnsTopicARN (..)

    -- ** JobId
    , JobId (..)

    -- ** JobMetadata
    , JobMetadata (..)
    , mkJobMetadata
    , jmAddressId
    , jmClusterId
    , jmCreationDate
    , jmDataTransferProgress
    , jmDescription
    , jmDeviceConfiguration
    , jmForwardingAddressId
    , jmJobId
    , jmJobLogInfo
    , jmJobState
    , jmJobType
    , jmKmsKeyARN
    , jmNotification
    , jmResources
    , jmRoleARN
    , jmShippingDetails
    , jmSnowballCapacityPreference
    , jmSnowballType
    , jmTaxDocuments

    -- ** ClusterState
    , ClusterState (..)

    -- ** LambdaResource
    , LambdaResource (..)
    , mkLambdaResource
    , lrEventTriggers
    , lrLambdaArn

    -- ** Notification
    , Notification (..)
    , mkNotification
    , nJobStatesToNotify
    , nNotifyAll
    , nSnsTopicARN

    -- ** JobResource
    , JobResource (..)
    , mkJobResource
    , jrEc2AmiResources
    , jrLambdaResources
    , jrS3Resources

    -- ** JobLogs
    , JobLogs (..)
    , mkJobLogs
    , jlJobCompletionReportURI
    , jlJobFailureLogURI
    , jlJobSuccessLogURI

    -- ** JobState
    , JobState (..)

    -- ** ShippingDetails
    , ShippingDetails (..)
    , mkShippingDetails
    , sdInboundShipment
    , sdOutboundShipment
    , sdShippingOption

    -- ** AddressId
    , AddressId (..)

    -- ** Address
    , Address (..)
    , mkAddress
    , aAddressId
    , aCity
    , aCompany
    , aCountry
    , aIsRestricted
    , aLandmark
    , aName
    , aPhoneNumber
    , aPostalCode
    , aPrefectureOrDistrict
    , aStateOrProvince
    , aStreet1
    , aStreet2
    , aStreet3

    -- ** SnowballType
    , SnowballType (..)

    -- ** WirelessConnection
    , WirelessConnection (..)
    , mkWirelessConnection
    , wcIsWifiEnabled

    -- ** CompatibleImage
    , CompatibleImage (..)
    , mkCompatibleImage
    , ciAmiId
    , ciName

    -- ** ShippingOption
    , ShippingOption (..)

    -- ** ClusterListEntry
    , ClusterListEntry (..)
    , mkClusterListEntry
    , cleClusterId
    , cleClusterState
    , cleCreationDate
    , cleDescription

    -- ** ResourceARN
    , ResourceARN (..)

    -- ** ClusterMetadata
    , ClusterMetadata (..)
    , mkClusterMetadata
    , cmAddressId
    , cmClusterId
    , cmClusterState
    , cmCreationDate
    , cmDescription
    , cmForwardingAddressId
    , cmJobType
    , cmKmsKeyARN
    , cmNotification
    , cmResources
    , cmRoleARN
    , cmShippingOption
    , cmSnowballType
    , cmTaxDocuments

    -- ** ShipmentState
    , ShipmentState (..)

    -- ** ClusterId
    , ClusterId (..)

    -- ** SnowconeDeviceConfiguration
    , SnowconeDeviceConfiguration (..)
    , mkSnowconeDeviceConfiguration
    , sdcWirelessConnection

    -- ** SnowballCapacity
    , SnowballCapacity (..)

    -- ** ShippingLabelStatus
    , ShippingLabelStatus (..)

    -- ** KeyRange
    , KeyRange (..)
    , mkKeyRange
    , krBeginMarker
    , krEndMarker

    -- ** AmiId
    , AmiId (..)

    -- ** DeviceConfiguration
    , DeviceConfiguration (..)
    , mkDeviceConfiguration
    , dcSnowconeDeviceConfiguration

    -- ** Shipment
    , Shipment (..)
    , mkShipment
    , sStatus
    , sTrackingNumber

    -- ** Ec2AmiResource
    , Ec2AmiResource (..)
    , mkEc2AmiResource
    , earAmiId
    , earSnowballAmiId

    -- ** DataTransfer
    , DataTransfer (..)
    , mkDataTransfer
    , dtBytesTransferred
    , dtObjectsTransferred
    , dtTotalBytes
    , dtTotalObjects

    -- ** TaxDocuments
    , TaxDocuments (..)
    , mkTaxDocuments
    , tdIND

    -- ** EventTriggerDefinition
    , EventTriggerDefinition (..)
    , mkEventTriggerDefinition
    , etdEventResourceARN

    -- ** JobListEntry
    , JobListEntry (..)
    , mkJobListEntry
    , jleCreationDate
    , jleDescription
    , jleIsMaster
    , jleJobId
    , jleJobState
    , jleJobType
    , jleSnowballType

    -- ** RoleARN
    , RoleARN (..)

    -- ** ForwardingAddressId
    , ForwardingAddressId (..)

    -- ** BucketArn
    , BucketArn (..)

    -- ** LambdaArn
    , LambdaArn (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.Snowball.Types
import Network.AWS.Snowball.Waiters
import Network.AWS.Snowball.CancelCluster
import Network.AWS.Snowball.DescribeCluster
import Network.AWS.Snowball.CreateAddress
import Network.AWS.Snowball.CreateReturnShippingLabel
import Network.AWS.Snowball.GetSnowballUsage
import Network.AWS.Snowball.DescribeAddresses
import Network.AWS.Snowball.ListCompatibleImages
import Network.AWS.Snowball.UpdateCluster
import Network.AWS.Snowball.GetSoftwareUpdates
import Network.AWS.Snowball.CreateJob
import Network.AWS.Snowball.GetJobManifest
import Network.AWS.Snowball.CreateCluster
import Network.AWS.Snowball.ListJobs
import Network.AWS.Snowball.UpdateJob
import Network.AWS.Snowball.UpdateJobShipmentState
import Network.AWS.Snowball.GetJobUnlockCode
import Network.AWS.Snowball.ListClusterJobs
import Network.AWS.Snowball.DescribeJob
import Network.AWS.Snowball.ListClusters
import Network.AWS.Snowball.DescribeAddress
import Network.AWS.Snowball.DescribeReturnShippingLabel
import Network.AWS.Snowball.CancelJob
import qualified Network.AWS.Prelude as Lude

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
