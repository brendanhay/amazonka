{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types
    (
    -- * Service Configuration
      storageGateway

    -- * Errors
    , _InvalidGatewayRequestException
    , _ServiceUnavailableError
    , _InternalServerError

    -- * ObjectACL
    , ObjectACL (..)

    -- * CachediSCSIVolume
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

    -- * ChapInfo
    , ChapInfo
    , chapInfo
    , ciTargetARN
    , ciSecretToAuthenticateInitiator
    , ciInitiatorName
    , ciSecretToAuthenticateTarget

    -- * DeviceiSCSIAttributes
    , DeviceiSCSIAttributes
    , deviceiSCSIAttributes
    , dscsiaTargetARN
    , dscsiaChapEnabled
    , dscsiaNetworkInterfaceId
    , dscsiaNetworkInterfacePort

    -- * Disk
    , Disk
    , disk
    , dDiskAllocationResource
    , dDiskAllocationType
    , dDiskNode
    , dDiskPath
    , dDiskSizeInBytes
    , dDiskStatus
    , dDiskId

    -- * FileShareInfo
    , FileShareInfo
    , fileShareInfo
    , fsiFileShareStatus
    , fsiGatewayARN
    , fsiFileShareId
    , fsiFileShareARN

    -- * GatewayInfo
    , GatewayInfo
    , gatewayInfo
    , giGatewayARN
    , giGatewayOperationalState
    , giGatewayName
    , giGatewayId
    , giGatewayType

    -- * NFSFileShareDefaults
    , NFSFileShareDefaults
    , nFSFileShareDefaults
    , nfsfsdFileMode
    , nfsfsdOwnerId
    , nfsfsdDirectoryMode
    , nfsfsdGroupId

    -- * NFSFileShareInfo
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

    -- * NetworkInterface
    , NetworkInterface
    , networkInterface
    , niIPv6Address
    , niMACAddress
    , niIPv4Address

    -- * StorediSCSIVolume
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

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * Tape
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

    -- * TapeArchive
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

    -- * TapeInfo
    , TapeInfo
    , tapeInfo
    , tiTapeBarcode
    , tiTapeStatus
    , tiTapeARN
    , tiGatewayARN
    , tiTapeSizeInBytes

    -- * TapeRecoveryPointInfo
    , TapeRecoveryPointInfo
    , tapeRecoveryPointInfo
    , trpiTapeStatus
    , trpiTapeRecoveryPointTime
    , trpiTapeARN
    , trpiTapeSizeInBytes

    -- * VTLDevice
    , VTLDevice
    , vTLDevice
    , vtldDeviceiSCSIAttributes
    , vtldVTLDeviceVendor
    , vtldVTLDeviceARN
    , vtldVTLDeviceType
    , vtldVTLDeviceProductIdentifier

    -- * VolumeInfo
    , VolumeInfo
    , volumeInfo
    , viGatewayARN
    , viVolumeARN
    , viVolumeSizeInBytes
    , viVolumeId
    , viGatewayId
    , viVolumeType

    -- * VolumeRecoveryPointInfo
    , VolumeRecoveryPointInfo
    , volumeRecoveryPointInfo
    , vrpiVolumeRecoveryPointTime
    , vrpiVolumeARN
    , vrpiVolumeSizeInBytes
    , vrpiVolumeUsageInBytes

    -- * VolumeiSCSIAttributes
    , VolumeiSCSIAttributes
    , volumeiSCSIAttributes
    , vscsiaLunNumber
    , vscsiaTargetARN
    , vscsiaChapEnabled
    , vscsiaNetworkInterfaceId
    , vscsiaNetworkInterfacePort
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.StorageGateway.Types.Product
import Network.AWS.StorageGateway.Types.Sum

-- | API version @2013-06-30@ of the Amazon Storage Gateway SDK configuration.
storageGateway :: Service
storageGateway =
  Service
    { _svcAbbrev = "StorageGateway"
    , _svcSigner = v4
    , _svcPrefix = "storagegateway"
    , _svcVersion = "2013-06-30"
    , _svcEndpoint = defaultEndpoint storageGateway
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "StorageGateway"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | An exception occurred because an invalid gateway request was issued to the service. For more information, see the error and message fields.
--
--
_InvalidGatewayRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidGatewayRequestException =
  _MatchServiceError storageGateway "InvalidGatewayRequestException"


-- | An internal server error has occurred because the service is unavailable. For more information, see the error and message fields.
--
--
_ServiceUnavailableError :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableError =
  _MatchServiceError storageGateway "ServiceUnavailableError"


-- | An internal server error has occurred during the request. For more information, see the error and message fields.
--
--
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _MatchServiceError storageGateway "InternalServerError"

