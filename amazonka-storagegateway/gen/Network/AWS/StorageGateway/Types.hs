{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types
    (
    -- * Service
      StorageGateway

    -- * Errors
    , _InvalidGatewayRequestException
    , _InternalServerError

    -- * CachediSCSIVolume
    , CachediSCSIVolume
    , cachediSCSIVolume
    , cscsivVolumeStatus
    , cscsivVolumeiSCSIAttributes
    , cscsivSourceSnapshotId
    , cscsivVolumeARN
    , cscsivVolumeProgress
    , cscsivVolumeSizeInBytes
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

    -- * GatewayInfo
    , GatewayInfo
    , gatewayInfo
    , giGatewayARN
    , giGatewayOperationalState
    , giGatewayType

    -- * NetworkInterface
    , NetworkInterface
    , networkInterface
    , niIPv6Address
    , niMACAddress
    , niIPv4Address

    -- * StorediSCSIVolume
    , StorediSCSIVolume
    , storediSCSIVolume
    , sscsivVolumeStatus
    , sscsivVolumeiSCSIAttributes
    , sscsivSourceSnapshotId
    , sscsivPreservedExistingData
    , sscsivVolumeARN
    , sscsivVolumeProgress
    , sscsivVolumeSizeInBytes
    , sscsivVolumeId
    , sscsivVolumeType
    , sscsivVolumeDiskId

    -- * Tape
    , Tape
    , tape
    , tTapeBarcode
    , tTapeStatus
    , tProgress
    , tTapeARN
    , tTapeSizeInBytes
    , tVTLDevice

    -- * TapeArchive
    , TapeArchive
    , tapeArchive
    , taTapeBarcode
    , taTapeStatus
    , taTapeARN
    , taTapeSizeInBytes
    , taCompletionTime
    , taRetrievedTo

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
    , viVolumeARN
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

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Network.AWS.StorageGateway.Types.Product
import           Network.AWS.StorageGateway.Types.Sum

-- | Version @2013-06-30@ of the Amazon Storage Gateway SDK.
data StorageGateway

instance AWSService StorageGateway where
    type Sg StorageGateway = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "StorageGateway"
            , _svcPrefix = "storagegateway"
            , _svcVersion = "2013-06-30"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | An exception occurred because an invalid gateway request was issued to
-- the service. See the error and message fields for more information.
_InvalidGatewayRequestException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidGatewayRequestException =
    _ServiceError . hasStatus 400 . hasCode "InvalidGatewayRequestException"

-- | An internal server error has occurred during the request. See the error
-- and message fields for more information.
_InternalServerError :: AWSError a => Getting (First ServiceError) a ServiceError
_InternalServerError =
    _ServiceError . hasStatus 500 . hasCode "InternalServerError"
