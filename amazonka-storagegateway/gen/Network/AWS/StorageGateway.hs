{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Storage Gateway Service__
--
-- AWS Storage Gateway is the service that connects an on-premises software appliance with cloud-based storage to provide seamless and secure integration between an organization's on-premises IT environment and AWS's storage infrastructure. The service enables you to securely upload data to the AWS cloud for cost effective backup and rapid disaster recovery.
--
-- Use the following links to get started using the /AWS Storage Gateway Service API Reference/ :
--
--     * <http://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#AWSStorageGatewayHTTPRequestsHeaders AWS Storage Gateway Required Request Headers> : Describes the required headers that you must send with every POST request to AWS Storage Gateway.
--
--     * <http://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#AWSStorageGatewaySigningRequests Signing Requests> : AWS Storage Gateway requires that you authenticate every request you send; this topic describes how sign such a request.
--
--     * <http://docs.aws.amazon.com/storagegateway/latest/userguide/AWSStorageGatewayAPI.html#APIErrorResponses Error Responses> : Provides reference information about AWS Storage Gateway errors.
--
--     * <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_Operations.html Operations in AWS Storage Gateway> : Contains detailed descriptions of all AWS Storage Gateway operations, their request parameters, response elements, possible errors, and examples of requests and responses.
--
--     * <http://docs.aws.amazon.com/general/latest/gr/rande.html#sg_region AWS Storage Gateway Regions and Endpoints:> Provides a list of each region and endpoints available for use with AWS Storage Gateway.
--
--
--
-- /Important:/ IDs for Storage Gateway volumes and Amazon EBS snapshots created from gateway volumes are changing to a longer format. Starting in December 2016, all new volumes and snapshots will be created with a 17-character string. Starting in April 2016, you will be able to use these longer IDs so you can test your systems with the new format. For more information, see <https://aws.amazon.com/ec2/faqs/#longer-ids Longer EC2 and EBS Resource IDs> .
--
-- For example, a volume Amazon Resource Name (ARN) with the longer volume ID format looks like the following:
--
-- @arn:aws:storagegateway:us-west-2:111122223333:gateway/sgw-12A3456B/volume/vol-1122AABBCCDDEEFFG@ .
--
-- A snapshot ID with the longer ID format looks like the following: @snap-78e226633445566ee@ .
--
-- For more information, see <https://forums.aws.amazon.com/ann.jspa?annID=3557 Announcement: Heads-up – Longer AWS Storage Gateway volume and snapshot IDs coming in 2016> .
--
module Network.AWS.StorageGateway
    (
    -- * Service Configuration
      storageGateway

    -- * Errors
    -- $errors

    -- ** InvalidGatewayRequestException
    , _InvalidGatewayRequestException

    -- ** ServiceUnavailableError
    , _ServiceUnavailableError

    -- ** InternalServerError
    , _InternalServerError

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelArchival
    , module Network.AWS.StorageGateway.CancelArchival

    -- ** CreateStorediSCSIVolume
    , module Network.AWS.StorageGateway.CreateStorediSCSIVolume

    -- ** CreateNFSFileShare
    , module Network.AWS.StorageGateway.CreateNFSFileShare

    -- ** DescribeChapCredentials
    , module Network.AWS.StorageGateway.DescribeChapCredentials

    -- ** SetLocalConsolePassword
    , module Network.AWS.StorageGateway.SetLocalConsolePassword

    -- ** CreateTapes
    , module Network.AWS.StorageGateway.CreateTapes

    -- ** UpdateVTLDeviceType
    , module Network.AWS.StorageGateway.UpdateVTLDeviceType

    -- ** CreateCachediSCSIVolume
    , module Network.AWS.StorageGateway.CreateCachediSCSIVolume

    -- ** ListFileShares
    , module Network.AWS.StorageGateway.ListFileShares

    -- ** DeleteFileShare
    , module Network.AWS.StorageGateway.DeleteFileShare

    -- ** ListVolumeInitiators
    , module Network.AWS.StorageGateway.ListVolumeInitiators

    -- ** AddUploadBuffer
    , module Network.AWS.StorageGateway.AddUploadBuffer

    -- ** ListTagsForResource
    , module Network.AWS.StorageGateway.ListTagsForResource

    -- ** NotifyWhenUploaded
    , module Network.AWS.StorageGateway.NotifyWhenUploaded

    -- ** UpdateGatewayInformation
    , module Network.AWS.StorageGateway.UpdateGatewayInformation

    -- ** DescribeMaintenanceStartTime
    , module Network.AWS.StorageGateway.DescribeMaintenanceStartTime

    -- ** DescribeWorkingStorage
    , module Network.AWS.StorageGateway.DescribeWorkingStorage

    -- ** DescribeCachediSCSIVolumes
    , module Network.AWS.StorageGateway.DescribeCachediSCSIVolumes

    -- ** AddCache
    , module Network.AWS.StorageGateway.AddCache

    -- ** StartGateway
    , module Network.AWS.StorageGateway.StartGateway

    -- ** ShutdownGateway
    , module Network.AWS.StorageGateway.ShutdownGateway

    -- ** UpdateGatewaySoftwareNow
    , module Network.AWS.StorageGateway.UpdateGatewaySoftwareNow

    -- ** RemoveTagsFromResource
    , module Network.AWS.StorageGateway.RemoveTagsFromResource

    -- ** DeleteChapCredentials
    , module Network.AWS.StorageGateway.DeleteChapCredentials

    -- ** UpdateChapCredentials
    , module Network.AWS.StorageGateway.UpdateChapCredentials

    -- ** DescribeUploadBuffer
    , module Network.AWS.StorageGateway.DescribeUploadBuffer

    -- ** DescribeTapes (Paginated)
    , module Network.AWS.StorageGateway.DescribeTapes

    -- ** DescribeStorediSCSIVolumes
    , module Network.AWS.StorageGateway.DescribeStorediSCSIVolumes

    -- ** CreateSnapshotFromVolumeRecoveryPoint
    , module Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint

    -- ** RetrieveTapeRecoveryPoint
    , module Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint

    -- ** AddTagsToResource
    , module Network.AWS.StorageGateway.AddTagsToResource

    -- ** DeleteGateway
    , module Network.AWS.StorageGateway.DeleteGateway

    -- ** UpdateMaintenanceStartTime
    , module Network.AWS.StorageGateway.UpdateMaintenanceStartTime

    -- ** DescribeGatewayInformation
    , module Network.AWS.StorageGateway.DescribeGatewayInformation

    -- ** RefreshCache
    , module Network.AWS.StorageGateway.RefreshCache

    -- ** UpdateNFSFileShare
    , module Network.AWS.StorageGateway.UpdateNFSFileShare

    -- ** RetrieveTapeArchive
    , module Network.AWS.StorageGateway.RetrieveTapeArchive

    -- ** DescribeTapeArchives (Paginated)
    , module Network.AWS.StorageGateway.DescribeTapeArchives

    -- ** DisableGateway
    , module Network.AWS.StorageGateway.DisableGateway

    -- ** DescribeSnapshotSchedule
    , module Network.AWS.StorageGateway.DescribeSnapshotSchedule

    -- ** CreateTapeWithBarcode
    , module Network.AWS.StorageGateway.CreateTapeWithBarcode

    -- ** DescribeBandwidthRateLimit
    , module Network.AWS.StorageGateway.DescribeBandwidthRateLimit

    -- ** DeleteSnapshotSchedule
    , module Network.AWS.StorageGateway.DeleteSnapshotSchedule

    -- ** UpdateSnapshotSchedule
    , module Network.AWS.StorageGateway.UpdateSnapshotSchedule

    -- ** CreateSnapshot
    , module Network.AWS.StorageGateway.CreateSnapshot

    -- ** CancelRetrieval
    , module Network.AWS.StorageGateway.CancelRetrieval

    -- ** DescribeVTLDevices (Paginated)
    , module Network.AWS.StorageGateway.DescribeVTLDevices

    -- ** DeleteTapeArchive
    , module Network.AWS.StorageGateway.DeleteTapeArchive

    -- ** DescribeNFSFileShares
    , module Network.AWS.StorageGateway.DescribeNFSFileShares

    -- ** ListVolumeRecoveryPoints
    , module Network.AWS.StorageGateway.ListVolumeRecoveryPoints

    -- ** ListTapes
    , module Network.AWS.StorageGateway.ListTapes

    -- ** ResetCache
    , module Network.AWS.StorageGateway.ResetCache

    -- ** ListGateways (Paginated)
    , module Network.AWS.StorageGateway.ListGateways

    -- ** DeleteTape
    , module Network.AWS.StorageGateway.DeleteTape

    -- ** ListLocalDisks
    , module Network.AWS.StorageGateway.ListLocalDisks

    -- ** ListVolumes (Paginated)
    , module Network.AWS.StorageGateway.ListVolumes

    -- ** UpdateBandwidthRateLimit
    , module Network.AWS.StorageGateway.UpdateBandwidthRateLimit

    -- ** AddWorkingStorage
    , module Network.AWS.StorageGateway.AddWorkingStorage

    -- ** DescribeTapeRecoveryPoints (Paginated)
    , module Network.AWS.StorageGateway.DescribeTapeRecoveryPoints

    -- ** DeleteBandwidthRateLimit
    , module Network.AWS.StorageGateway.DeleteBandwidthRateLimit

    -- ** ActivateGateway
    , module Network.AWS.StorageGateway.ActivateGateway

    -- ** DescribeCache
    , module Network.AWS.StorageGateway.DescribeCache

    -- ** DeleteVolume
    , module Network.AWS.StorageGateway.DeleteVolume

    -- * Types

    -- ** ObjectACL
    , ObjectACL (..)

    -- ** CachediSCSIVolume
    , CachediSCSIVolume
    , cachediSCSIVolume
    , cscsivVolumeiSCSIAttributes
    , cscsivVolumeStatus
    , cscsivSourceSnapshotId
    , cscsivVolumeARN
    , cscsivVolumeProgress
    , cscsivVolumeSizeInBytes
    , cscsivVolumeUsedInBytes
    , cscsivCreatedDate
    , cscsivVolumeId
    , cscsivVolumeType

    -- ** ChapInfo
    , ChapInfo
    , chapInfo
    , ciTargetARN
    , ciSecretToAuthenticateInitiator
    , ciInitiatorName
    , ciSecretToAuthenticateTarget

    -- ** DeviceiSCSIAttributes
    , DeviceiSCSIAttributes
    , deviceiSCSIAttributes
    , dscsiaTargetARN
    , dscsiaChapEnabled
    , dscsiaNetworkInterfaceId
    , dscsiaNetworkInterfacePort

    -- ** Disk
    , Disk
    , disk
    , dDiskAllocationResource
    , dDiskAllocationType
    , dDiskNode
    , dDiskPath
    , dDiskSizeInBytes
    , dDiskStatus
    , dDiskId

    -- ** FileShareInfo
    , FileShareInfo
    , fileShareInfo
    , fsiFileShareStatus
    , fsiGatewayARN
    , fsiFileShareId
    , fsiFileShareARN

    -- ** GatewayInfo
    , GatewayInfo
    , gatewayInfo
    , giGatewayARN
    , giGatewayOperationalState
    , giGatewayName
    , giGatewayId
    , giGatewayType

    -- ** NFSFileShareDefaults
    , NFSFileShareDefaults
    , nFSFileShareDefaults
    , nfsfsdFileMode
    , nfsfsdOwnerId
    , nfsfsdDirectoryMode
    , nfsfsdGroupId

    -- ** NFSFileShareInfo
    , NFSFileShareInfo
    , nFSFileShareInfo
    , nfsfsiFileShareStatus
    , nfsfsiKMSKey
    , nfsfsiGatewayARN
    , nfsfsiPath
    , nfsfsiObjectACL
    , nfsfsiKMSEncrypted
    , nfsfsiFileShareId
    , nfsfsiFileShareARN
    , nfsfsiDefaultStorageClass
    , nfsfsiRole
    , nfsfsiSquash
    , nfsfsiRequesterPays
    , nfsfsiNFSFileShareDefaults
    , nfsfsiLocationARN
    , nfsfsiClientList
    , nfsfsiGuessMIMETypeEnabled
    , nfsfsiReadOnly

    -- ** NetworkInterface
    , NetworkInterface
    , networkInterface
    , niIPv6Address
    , niMACAddress
    , niIPv4Address

    -- ** StorediSCSIVolume
    , StorediSCSIVolume
    , storediSCSIVolume
    , sscsivVolumeiSCSIAttributes
    , sscsivVolumeStatus
    , sscsivSourceSnapshotId
    , sscsivPreservedExistingData
    , sscsivVolumeARN
    , sscsivVolumeProgress
    , sscsivVolumeSizeInBytes
    , sscsivVolumeUsedInBytes
    , sscsivCreatedDate
    , sscsivVolumeId
    , sscsivVolumeDiskId
    , sscsivVolumeType

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** Tape
    , Tape
    , tape
    , tTapeBarcode
    , tTapeStatus
    , tTapeARN
    , tProgress
    , tTapeSizeInBytes
    , tVTLDevice
    , tTapeUsedInBytes
    , tTapeCreatedDate

    -- ** TapeArchive
    , TapeArchive
    , tapeArchive
    , taTapeBarcode
    , taTapeStatus
    , taTapeARN
    , taTapeSizeInBytes
    , taCompletionTime
    , taTapeUsedInBytes
    , taTapeCreatedDate
    , taRetrievedTo

    -- ** TapeInfo
    , TapeInfo
    , tapeInfo
    , tiTapeBarcode
    , tiTapeStatus
    , tiTapeARN
    , tiGatewayARN
    , tiTapeSizeInBytes

    -- ** TapeRecoveryPointInfo
    , TapeRecoveryPointInfo
    , tapeRecoveryPointInfo
    , trpiTapeStatus
    , trpiTapeRecoveryPointTime
    , trpiTapeARN
    , trpiTapeSizeInBytes

    -- ** VTLDevice
    , VTLDevice
    , vTLDevice
    , vtldDeviceiSCSIAttributes
    , vtldVTLDeviceVendor
    , vtldVTLDeviceARN
    , vtldVTLDeviceType
    , vtldVTLDeviceProductIdentifier

    -- ** VolumeInfo
    , VolumeInfo
    , volumeInfo
    , viGatewayARN
    , viVolumeARN
    , viVolumeSizeInBytes
    , viVolumeId
    , viGatewayId
    , viVolumeType

    -- ** VolumeRecoveryPointInfo
    , VolumeRecoveryPointInfo
    , volumeRecoveryPointInfo
    , vrpiVolumeRecoveryPointTime
    , vrpiVolumeARN
    , vrpiVolumeSizeInBytes
    , vrpiVolumeUsageInBytes

    -- ** VolumeiSCSIAttributes
    , VolumeiSCSIAttributes
    , volumeiSCSIAttributes
    , vscsiaLunNumber
    , vscsiaTargetARN
    , vscsiaChapEnabled
    , vscsiaNetworkInterfaceId
    , vscsiaNetworkInterfacePort
    ) where

import Network.AWS.StorageGateway.ActivateGateway
import Network.AWS.StorageGateway.AddCache
import Network.AWS.StorageGateway.AddTagsToResource
import Network.AWS.StorageGateway.AddUploadBuffer
import Network.AWS.StorageGateway.AddWorkingStorage
import Network.AWS.StorageGateway.CancelArchival
import Network.AWS.StorageGateway.CancelRetrieval
import Network.AWS.StorageGateway.CreateCachediSCSIVolume
import Network.AWS.StorageGateway.CreateNFSFileShare
import Network.AWS.StorageGateway.CreateSnapshot
import Network.AWS.StorageGateway.CreateSnapshotFromVolumeRecoveryPoint
import Network.AWS.StorageGateway.CreateStorediSCSIVolume
import Network.AWS.StorageGateway.CreateTapes
import Network.AWS.StorageGateway.CreateTapeWithBarcode
import Network.AWS.StorageGateway.DeleteBandwidthRateLimit
import Network.AWS.StorageGateway.DeleteChapCredentials
import Network.AWS.StorageGateway.DeleteFileShare
import Network.AWS.StorageGateway.DeleteGateway
import Network.AWS.StorageGateway.DeleteSnapshotSchedule
import Network.AWS.StorageGateway.DeleteTape
import Network.AWS.StorageGateway.DeleteTapeArchive
import Network.AWS.StorageGateway.DeleteVolume
import Network.AWS.StorageGateway.DescribeBandwidthRateLimit
import Network.AWS.StorageGateway.DescribeCache
import Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
import Network.AWS.StorageGateway.DescribeChapCredentials
import Network.AWS.StorageGateway.DescribeGatewayInformation
import Network.AWS.StorageGateway.DescribeMaintenanceStartTime
import Network.AWS.StorageGateway.DescribeNFSFileShares
import Network.AWS.StorageGateway.DescribeSnapshotSchedule
import Network.AWS.StorageGateway.DescribeStorediSCSIVolumes
import Network.AWS.StorageGateway.DescribeTapeArchives
import Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
import Network.AWS.StorageGateway.DescribeTapes
import Network.AWS.StorageGateway.DescribeUploadBuffer
import Network.AWS.StorageGateway.DescribeVTLDevices
import Network.AWS.StorageGateway.DescribeWorkingStorage
import Network.AWS.StorageGateway.DisableGateway
import Network.AWS.StorageGateway.ListFileShares
import Network.AWS.StorageGateway.ListGateways
import Network.AWS.StorageGateway.ListLocalDisks
import Network.AWS.StorageGateway.ListTagsForResource
import Network.AWS.StorageGateway.ListTapes
import Network.AWS.StorageGateway.ListVolumeInitiators
import Network.AWS.StorageGateway.ListVolumeRecoveryPoints
import Network.AWS.StorageGateway.ListVolumes
import Network.AWS.StorageGateway.NotifyWhenUploaded
import Network.AWS.StorageGateway.RefreshCache
import Network.AWS.StorageGateway.RemoveTagsFromResource
import Network.AWS.StorageGateway.ResetCache
import Network.AWS.StorageGateway.RetrieveTapeArchive
import Network.AWS.StorageGateway.RetrieveTapeRecoveryPoint
import Network.AWS.StorageGateway.SetLocalConsolePassword
import Network.AWS.StorageGateway.ShutdownGateway
import Network.AWS.StorageGateway.StartGateway
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.UpdateBandwidthRateLimit
import Network.AWS.StorageGateway.UpdateChapCredentials
import Network.AWS.StorageGateway.UpdateGatewayInformation
import Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
import Network.AWS.StorageGateway.UpdateMaintenanceStartTime
import Network.AWS.StorageGateway.UpdateNFSFileShare
import Network.AWS.StorageGateway.UpdateSnapshotSchedule
import Network.AWS.StorageGateway.UpdateVTLDeviceType
import Network.AWS.StorageGateway.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'StorageGateway'.
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
