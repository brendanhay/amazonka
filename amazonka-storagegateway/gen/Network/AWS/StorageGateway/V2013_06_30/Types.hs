{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Storage Gateway is a service that connects an on-premises software
-- appliance with cloud-based storage to provide seamless and secure
-- integration between your on-premises IT environment and AWS's storage
-- infrastructure.
module Network.AWS.StorageGateway.V2013_06_30.Types
    (
    -- * Service
      StorageGateway
    -- ** Errors
    , Er (..)
    -- * ErrorCode
    , ErrorCode (..)

    -- * CachediSCSIVolumeInformation
    , CachediSCSIVolumeInformation (..)
    , cscsiviVolumeARN
    , cscsiviVolumeId
    , cscsiviVolumeType
    , cscsiviVolumeStatus
    , cscsiviVolumeSizeInBytes
    , cscsiviVolumeProgress
    , cscsiviSourceSnapshotId
    , cscsiviVolumeiSCSIAttributes

    -- * ChapInfo
    , ChapInfo (..)
    , ciTargetARN
    , ciSecretToAuthenticateInitiator
    , ciInitiatorName
    , ciSecretToAuthenticateTarget

    -- * DeviceiSCSIAttributes
    , DeviceiSCSIAttributes (..)
    , dscsiaTargetARN
    , dscsiaNetworkInterfaceId
    , dscsiaNetworkInterfacePort
    , dscsiaChapEnabled

    -- * DiskInformation
    , DiskInformation (..)
    , ddnDiskId
    , ddnDiskPath
    , ddnDiskNode
    , ddnDiskSizeInBytes
    , ddnDiskAllocationType
    , ddnDiskAllocationResource

    -- * GatewayInformation
    , GatewayInformation (..)
    , gjGatewayARN
    , gjGatewayType

    -- * NetworkInterface
    , NetworkInterface (..)
    , niIpv4Address
    , niMacAddress
    , niIpv6Address

    -- * StorageGatewayError
    , StorageGatewayError (..)
    , sgeErrorCode
    , sgeErrorDetails

    -- * StorediSCSIVolumeInformation
    , StorediSCSIVolumeInformation (..)
    , sscsiviVolumeARN
    , sscsiviVolumeId
    , sscsiviVolumeType
    , sscsiviVolumeStatus
    , sscsiviVolumeSizeInBytes
    , sscsiviVolumeProgress
    , sscsiviVolumeDiskId
    , sscsiviSourceSnapshotId
    , sscsiviPreservedExistingData
    , sscsiviVolumeiSCSIAttributes

    -- * Tape
    , Tape (..)
    , teTapeARN
    , teTapeBarcode
    , teTapeSizeInBytes
    , teTapeStatus
    , teVTLDevice
    , teProgress

    -- * TapeArchive
    , TapeArchive (..)
    , tbTapeARN
    , tbTapeBarcode
    , tbTapeSizeInBytes
    , tbCompletionTime
    , tbRetrievedTo
    , tbTapeStatus

    -- * TapeRecoveryPointInfo
    , TapeRecoveryPointInfo (..)
    , trpjTapeARN
    , trpjTapeRecoveryPointTime
    , trpjTapeSizeInBytes

    -- * VTLDevice
    , VTLDevice (..)
    , vtleVTLDeviceARN
    , vtleDeviceiSCSIAttributes

    -- * VolumeInformation
    , VolumeInformation (..)
    , vlVolumeARN
    , vlVolumeType

    -- * VolumeRecoveryPointInfo
    , VolumeRecoveryPointInfo (..)
    , vrpjVolumeARN
    , vrpjVolumeSizeInBytes
    , vrpjVolumeUsageInBytes
    , vrpjVolumeRecoveryPointTime

    -- * VolumeiSCSIAttributes
    , VolumeiSCSIAttributes (..)
    , vscsiaTargetARN
    , vscsiaNetworkInterfaceId
    , vscsiaNetworkInterfacePort
    , vscsiaLunNumber
    , vscsiaChapEnabled

    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2013-06-30@) of the
-- @AWS Storage Gateway@ service.
data StorageGateway deriving (Typeable)

instance AWSService StorageGateway where
    type Sg StorageGateway = V4
    data Er StorageGateway
        = InternalServerError
            { _iseMessage :: Maybe Text
            , _iseError :: Maybe StorageGatewayError
            }
        | InvalidGatewayRequestException
            { _igreMessage :: Maybe Text
            , _igreError :: Maybe StorageGatewayError
            }
        | StorageGatewayClient HttpException
        | StorageGatewaySerializer String
        | StorageGatewayService String

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "storagegateway"
        , _svcVersion  = "2013-06-30"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er StorageGateway)
deriving instance Generic (Er StorageGateway)

instance AWSError (Er StorageGateway) where
    awsError = const "StorageGatewayError"

instance AWSServiceError (Er StorageGateway) where
    serviceError    = StorageGatewayService
    clientError     = StorageGatewayClient
    serializerError = StorageGatewaySerializer

instance Exception (Er StorageGateway)

-- | Additional information about the error.
data ErrorCode
    = ErrorCodeActivationKeyExpired -- ^ ActivationKeyExpired
    | ErrorCodeActivationKeyInvalid -- ^ ActivationKeyInvalid
    | ErrorCodeActivationKeyNotFound -- ^ ActivationKeyNotFound
    | ErrorCodeAuthenticationFailure -- ^ AuthenticationFailure
    | ErrorCodeBandwidthThrottleScheduleNotFound -- ^ BandwidthThrottleScheduleNotFound
    | ErrorCodeBlocked -- ^ Blocked
    | ErrorCodeCannotExportSnapshot -- ^ CannotExportSnapshot
    | ErrorCodeChapCredentialNotFound -- ^ ChapCredentialNotFound
    | ErrorCodeDiskAlreadyAllocated -- ^ DiskAlreadyAllocated
    | ErrorCodeDiskDoesNotExist -- ^ DiskDoesNotExist
    | ErrorCodeDiskSizeGreaterThanVolumeMaxSize -- ^ DiskSizeGreaterThanVolumeMaxSize
    | ErrorCodeDiskSizeLessThanVolumeSize -- ^ DiskSizeLessThanVolumeSize
    | ErrorCodeDiskSizeNotGigAligned -- ^ DiskSizeNotGigAligned
    | ErrorCodeDuplicateCertificateInfo -- ^ DuplicateCertificateInfo
    | ErrorCodeDuplicateSchedule -- ^ DuplicateSchedule
    | ErrorCodeEndpointNotFound -- ^ EndpointNotFound
    | ErrorCodeGatewayInternalError -- ^ GatewayInternalError
    | ErrorCodeGatewayNotConnected -- ^ GatewayNotConnected
    | ErrorCodeGatewayNotFound -- ^ GatewayNotFound
    | ErrorCodeGatewayProxyNetworkConnectionBusy -- ^ GatewayProxyNetworkConnectionBusy
    | ErrorCodeIAMNotSupported -- ^ IAMNotSupported
    | ErrorCodeInitiatorInvalid -- ^ InitiatorInvalid
    | ErrorCodeInitiatorNotFound -- ^ InitiatorNotFound
    | ErrorCodeInternalError -- ^ InternalError
    | ErrorCodeInvalidEndpoint -- ^ InvalidEndpoint
    | ErrorCodeInvalidGateway -- ^ InvalidGateway
    | ErrorCodeInvalidParameters -- ^ InvalidParameters
    | ErrorCodeInvalidSchedule -- ^ InvalidSchedule
    | ErrorCodeLocalStorageLimitExceeded -- ^ LocalStorageLimitExceeded
    | ErrorCodeLunAlreadyAllocated  -- ^ LunAlreadyAllocated 
    | ErrorCodeLunInvalid -- ^ LunInvalid
    | ErrorCodeMaximumContentLengthExceeded -- ^ MaximumContentLengthExceeded
    | ErrorCodeMaximumTapeCartridgeCountExceeded -- ^ MaximumTapeCartridgeCountExceeded
    | ErrorCodeMaximumVolumeCountExceeded -- ^ MaximumVolumeCountExceeded
    | ErrorCodeNetworkConfigurationChanged -- ^ NetworkConfigurationChanged
    | ErrorCodeNoDisksAvailable -- ^ NoDisksAvailable
    | ErrorCodeNotImplemented -- ^ NotImplemented
    | ErrorCodeNotSupported -- ^ NotSupported
    | ErrorCodeOperationAborted -- ^ OperationAborted
    | ErrorCodeOutdatedGateway -- ^ OutdatedGateway
    | ErrorCodeParametersNotImplemented -- ^ ParametersNotImplemented
    | ErrorCodeRegionInvalid -- ^ RegionInvalid
    | ErrorCodeRequestTimeout -- ^ RequestTimeout
    | ErrorCodeServiceUnavailable -- ^ ServiceUnavailable
    | ErrorCodeSnapshotDeleted -- ^ SnapshotDeleted
    | ErrorCodeSnapshotIdInvalid -- ^ SnapshotIdInvalid
    | ErrorCodeSnapshotInProgress -- ^ SnapshotInProgress
    | ErrorCodeSnapshotNotFound -- ^ SnapshotNotFound
    | ErrorCodeSnapshotScheduleNotFound -- ^ SnapshotScheduleNotFound
    | ErrorCodeStagingAreaFull -- ^ StagingAreaFull
    | ErrorCodeStorageFailure -- ^ StorageFailure
    | ErrorCodeTapeCartridgeNotFound -- ^ TapeCartridgeNotFound
    | ErrorCodeTargetAlreadyExists -- ^ TargetAlreadyExists
    | ErrorCodeTargetInvalid -- ^ TargetInvalid
    | ErrorCodeTargetNotFound -- ^ TargetNotFound
    | ErrorCodeUnauthorizedOperation -- ^ UnauthorizedOperation
    | ErrorCodeVolumeAlreadyExists -- ^ VolumeAlreadyExists
    | ErrorCodeVolumeIdInvalid -- ^ VolumeIdInvalid
    | ErrorCodeVolumeInUse -- ^ VolumeInUse
    | ErrorCodeVolumeNotFound -- ^ VolumeNotFound
    | ErrorCodeVolumeNotReady -- ^ VolumeNotReady
      deriving (Eq, Show, Generic)

instance Hashable ErrorCode

instance FromText ErrorCode where
    parser = match "ActivationKeyExpired" ErrorCodeActivationKeyExpired
         <|> match "ActivationKeyInvalid" ErrorCodeActivationKeyInvalid
         <|> match "ActivationKeyNotFound" ErrorCodeActivationKeyNotFound
         <|> match "AuthenticationFailure" ErrorCodeAuthenticationFailure
         <|> match "BandwidthThrottleScheduleNotFound" ErrorCodeBandwidthThrottleScheduleNotFound
         <|> match "Blocked" ErrorCodeBlocked
         <|> match "CannotExportSnapshot" ErrorCodeCannotExportSnapshot
         <|> match "ChapCredentialNotFound" ErrorCodeChapCredentialNotFound
         <|> match "DiskAlreadyAllocated" ErrorCodeDiskAlreadyAllocated
         <|> match "DiskDoesNotExist" ErrorCodeDiskDoesNotExist
         <|> match "DiskSizeGreaterThanVolumeMaxSize" ErrorCodeDiskSizeGreaterThanVolumeMaxSize
         <|> match "DiskSizeLessThanVolumeSize" ErrorCodeDiskSizeLessThanVolumeSize
         <|> match "DiskSizeNotGigAligned" ErrorCodeDiskSizeNotGigAligned
         <|> match "DuplicateCertificateInfo" ErrorCodeDuplicateCertificateInfo
         <|> match "DuplicateSchedule" ErrorCodeDuplicateSchedule
         <|> match "EndpointNotFound" ErrorCodeEndpointNotFound
         <|> match "GatewayInternalError" ErrorCodeGatewayInternalError
         <|> match "GatewayNotConnected" ErrorCodeGatewayNotConnected
         <|> match "GatewayNotFound" ErrorCodeGatewayNotFound
         <|> match "GatewayProxyNetworkConnectionBusy" ErrorCodeGatewayProxyNetworkConnectionBusy
         <|> match "IAMNotSupported" ErrorCodeIAMNotSupported
         <|> match "InitiatorInvalid" ErrorCodeInitiatorInvalid
         <|> match "InitiatorNotFound" ErrorCodeInitiatorNotFound
         <|> match "InternalError" ErrorCodeInternalError
         <|> match "InvalidEndpoint" ErrorCodeInvalidEndpoint
         <|> match "InvalidGateway" ErrorCodeInvalidGateway
         <|> match "InvalidParameters" ErrorCodeInvalidParameters
         <|> match "InvalidSchedule" ErrorCodeInvalidSchedule
         <|> match "LocalStorageLimitExceeded" ErrorCodeLocalStorageLimitExceeded
         <|> match "LunAlreadyAllocated " ErrorCodeLunAlreadyAllocated 
         <|> match "LunInvalid" ErrorCodeLunInvalid
         <|> match "MaximumContentLengthExceeded" ErrorCodeMaximumContentLengthExceeded
         <|> match "MaximumTapeCartridgeCountExceeded" ErrorCodeMaximumTapeCartridgeCountExceeded
         <|> match "MaximumVolumeCountExceeded" ErrorCodeMaximumVolumeCountExceeded
         <|> match "NetworkConfigurationChanged" ErrorCodeNetworkConfigurationChanged
         <|> match "NoDisksAvailable" ErrorCodeNoDisksAvailable
         <|> match "NotImplemented" ErrorCodeNotImplemented
         <|> match "NotSupported" ErrorCodeNotSupported
         <|> match "OperationAborted" ErrorCodeOperationAborted
         <|> match "OutdatedGateway" ErrorCodeOutdatedGateway
         <|> match "ParametersNotImplemented" ErrorCodeParametersNotImplemented
         <|> match "RegionInvalid" ErrorCodeRegionInvalid
         <|> match "RequestTimeout" ErrorCodeRequestTimeout
         <|> match "ServiceUnavailable" ErrorCodeServiceUnavailable
         <|> match "SnapshotDeleted" ErrorCodeSnapshotDeleted
         <|> match "SnapshotIdInvalid" ErrorCodeSnapshotIdInvalid
         <|> match "SnapshotInProgress" ErrorCodeSnapshotInProgress
         <|> match "SnapshotNotFound" ErrorCodeSnapshotNotFound
         <|> match "SnapshotScheduleNotFound" ErrorCodeSnapshotScheduleNotFound
         <|> match "StagingAreaFull" ErrorCodeStagingAreaFull
         <|> match "StorageFailure" ErrorCodeStorageFailure
         <|> match "TapeCartridgeNotFound" ErrorCodeTapeCartridgeNotFound
         <|> match "TargetAlreadyExists" ErrorCodeTargetAlreadyExists
         <|> match "TargetInvalid" ErrorCodeTargetInvalid
         <|> match "TargetNotFound" ErrorCodeTargetNotFound
         <|> match "UnauthorizedOperation" ErrorCodeUnauthorizedOperation
         <|> match "VolumeAlreadyExists" ErrorCodeVolumeAlreadyExists
         <|> match "VolumeIdInvalid" ErrorCodeVolumeIdInvalid
         <|> match "VolumeInUse" ErrorCodeVolumeInUse
         <|> match "VolumeNotFound" ErrorCodeVolumeNotFound
         <|> match "VolumeNotReady" ErrorCodeVolumeNotReady

instance ToText ErrorCode where
    toText ErrorCodeActivationKeyExpired = "ActivationKeyExpired"
    toText ErrorCodeActivationKeyInvalid = "ActivationKeyInvalid"
    toText ErrorCodeActivationKeyNotFound = "ActivationKeyNotFound"
    toText ErrorCodeAuthenticationFailure = "AuthenticationFailure"
    toText ErrorCodeBandwidthThrottleScheduleNotFound = "BandwidthThrottleScheduleNotFound"
    toText ErrorCodeBlocked = "Blocked"
    toText ErrorCodeCannotExportSnapshot = "CannotExportSnapshot"
    toText ErrorCodeChapCredentialNotFound = "ChapCredentialNotFound"
    toText ErrorCodeDiskAlreadyAllocated = "DiskAlreadyAllocated"
    toText ErrorCodeDiskDoesNotExist = "DiskDoesNotExist"
    toText ErrorCodeDiskSizeGreaterThanVolumeMaxSize = "DiskSizeGreaterThanVolumeMaxSize"
    toText ErrorCodeDiskSizeLessThanVolumeSize = "DiskSizeLessThanVolumeSize"
    toText ErrorCodeDiskSizeNotGigAligned = "DiskSizeNotGigAligned"
    toText ErrorCodeDuplicateCertificateInfo = "DuplicateCertificateInfo"
    toText ErrorCodeDuplicateSchedule = "DuplicateSchedule"
    toText ErrorCodeEndpointNotFound = "EndpointNotFound"
    toText ErrorCodeGatewayInternalError = "GatewayInternalError"
    toText ErrorCodeGatewayNotConnected = "GatewayNotConnected"
    toText ErrorCodeGatewayNotFound = "GatewayNotFound"
    toText ErrorCodeGatewayProxyNetworkConnectionBusy = "GatewayProxyNetworkConnectionBusy"
    toText ErrorCodeIAMNotSupported = "IAMNotSupported"
    toText ErrorCodeInitiatorInvalid = "InitiatorInvalid"
    toText ErrorCodeInitiatorNotFound = "InitiatorNotFound"
    toText ErrorCodeInternalError = "InternalError"
    toText ErrorCodeInvalidEndpoint = "InvalidEndpoint"
    toText ErrorCodeInvalidGateway = "InvalidGateway"
    toText ErrorCodeInvalidParameters = "InvalidParameters"
    toText ErrorCodeInvalidSchedule = "InvalidSchedule"
    toText ErrorCodeLocalStorageLimitExceeded = "LocalStorageLimitExceeded"
    toText ErrorCodeLunAlreadyAllocated  = "LunAlreadyAllocated "
    toText ErrorCodeLunInvalid = "LunInvalid"
    toText ErrorCodeMaximumContentLengthExceeded = "MaximumContentLengthExceeded"
    toText ErrorCodeMaximumTapeCartridgeCountExceeded = "MaximumTapeCartridgeCountExceeded"
    toText ErrorCodeMaximumVolumeCountExceeded = "MaximumVolumeCountExceeded"
    toText ErrorCodeNetworkConfigurationChanged = "NetworkConfigurationChanged"
    toText ErrorCodeNoDisksAvailable = "NoDisksAvailable"
    toText ErrorCodeNotImplemented = "NotImplemented"
    toText ErrorCodeNotSupported = "NotSupported"
    toText ErrorCodeOperationAborted = "OperationAborted"
    toText ErrorCodeOutdatedGateway = "OutdatedGateway"
    toText ErrorCodeParametersNotImplemented = "ParametersNotImplemented"
    toText ErrorCodeRegionInvalid = "RegionInvalid"
    toText ErrorCodeRequestTimeout = "RequestTimeout"
    toText ErrorCodeServiceUnavailable = "ServiceUnavailable"
    toText ErrorCodeSnapshotDeleted = "SnapshotDeleted"
    toText ErrorCodeSnapshotIdInvalid = "SnapshotIdInvalid"
    toText ErrorCodeSnapshotInProgress = "SnapshotInProgress"
    toText ErrorCodeSnapshotNotFound = "SnapshotNotFound"
    toText ErrorCodeSnapshotScheduleNotFound = "SnapshotScheduleNotFound"
    toText ErrorCodeStagingAreaFull = "StagingAreaFull"
    toText ErrorCodeStorageFailure = "StorageFailure"
    toText ErrorCodeTapeCartridgeNotFound = "TapeCartridgeNotFound"
    toText ErrorCodeTargetAlreadyExists = "TargetAlreadyExists"
    toText ErrorCodeTargetInvalid = "TargetInvalid"
    toText ErrorCodeTargetNotFound = "TargetNotFound"
    toText ErrorCodeUnauthorizedOperation = "UnauthorizedOperation"
    toText ErrorCodeVolumeAlreadyExists = "VolumeAlreadyExists"
    toText ErrorCodeVolumeIdInvalid = "VolumeIdInvalid"
    toText ErrorCodeVolumeInUse = "VolumeInUse"
    toText ErrorCodeVolumeNotFound = "VolumeNotFound"
    toText ErrorCodeVolumeNotReady = "VolumeNotReady"

instance ToByteString ErrorCode where
    toBS ErrorCodeActivationKeyExpired = "ActivationKeyExpired"
    toBS ErrorCodeActivationKeyInvalid = "ActivationKeyInvalid"
    toBS ErrorCodeActivationKeyNotFound = "ActivationKeyNotFound"
    toBS ErrorCodeAuthenticationFailure = "AuthenticationFailure"
    toBS ErrorCodeBandwidthThrottleScheduleNotFound = "BandwidthThrottleScheduleNotFound"
    toBS ErrorCodeBlocked = "Blocked"
    toBS ErrorCodeCannotExportSnapshot = "CannotExportSnapshot"
    toBS ErrorCodeChapCredentialNotFound = "ChapCredentialNotFound"
    toBS ErrorCodeDiskAlreadyAllocated = "DiskAlreadyAllocated"
    toBS ErrorCodeDiskDoesNotExist = "DiskDoesNotExist"
    toBS ErrorCodeDiskSizeGreaterThanVolumeMaxSize = "DiskSizeGreaterThanVolumeMaxSize"
    toBS ErrorCodeDiskSizeLessThanVolumeSize = "DiskSizeLessThanVolumeSize"
    toBS ErrorCodeDiskSizeNotGigAligned = "DiskSizeNotGigAligned"
    toBS ErrorCodeDuplicateCertificateInfo = "DuplicateCertificateInfo"
    toBS ErrorCodeDuplicateSchedule = "DuplicateSchedule"
    toBS ErrorCodeEndpointNotFound = "EndpointNotFound"
    toBS ErrorCodeGatewayInternalError = "GatewayInternalError"
    toBS ErrorCodeGatewayNotConnected = "GatewayNotConnected"
    toBS ErrorCodeGatewayNotFound = "GatewayNotFound"
    toBS ErrorCodeGatewayProxyNetworkConnectionBusy = "GatewayProxyNetworkConnectionBusy"
    toBS ErrorCodeIAMNotSupported = "IAMNotSupported"
    toBS ErrorCodeInitiatorInvalid = "InitiatorInvalid"
    toBS ErrorCodeInitiatorNotFound = "InitiatorNotFound"
    toBS ErrorCodeInternalError = "InternalError"
    toBS ErrorCodeInvalidEndpoint = "InvalidEndpoint"
    toBS ErrorCodeInvalidGateway = "InvalidGateway"
    toBS ErrorCodeInvalidParameters = "InvalidParameters"
    toBS ErrorCodeInvalidSchedule = "InvalidSchedule"
    toBS ErrorCodeLocalStorageLimitExceeded = "LocalStorageLimitExceeded"
    toBS ErrorCodeLunAlreadyAllocated  = "LunAlreadyAllocated "
    toBS ErrorCodeLunInvalid = "LunInvalid"
    toBS ErrorCodeMaximumContentLengthExceeded = "MaximumContentLengthExceeded"
    toBS ErrorCodeMaximumTapeCartridgeCountExceeded = "MaximumTapeCartridgeCountExceeded"
    toBS ErrorCodeMaximumVolumeCountExceeded = "MaximumVolumeCountExceeded"
    toBS ErrorCodeNetworkConfigurationChanged = "NetworkConfigurationChanged"
    toBS ErrorCodeNoDisksAvailable = "NoDisksAvailable"
    toBS ErrorCodeNotImplemented = "NotImplemented"
    toBS ErrorCodeNotSupported = "NotSupported"
    toBS ErrorCodeOperationAborted = "OperationAborted"
    toBS ErrorCodeOutdatedGateway = "OutdatedGateway"
    toBS ErrorCodeParametersNotImplemented = "ParametersNotImplemented"
    toBS ErrorCodeRegionInvalid = "RegionInvalid"
    toBS ErrorCodeRequestTimeout = "RequestTimeout"
    toBS ErrorCodeServiceUnavailable = "ServiceUnavailable"
    toBS ErrorCodeSnapshotDeleted = "SnapshotDeleted"
    toBS ErrorCodeSnapshotIdInvalid = "SnapshotIdInvalid"
    toBS ErrorCodeSnapshotInProgress = "SnapshotInProgress"
    toBS ErrorCodeSnapshotNotFound = "SnapshotNotFound"
    toBS ErrorCodeSnapshotScheduleNotFound = "SnapshotScheduleNotFound"
    toBS ErrorCodeStagingAreaFull = "StagingAreaFull"
    toBS ErrorCodeStorageFailure = "StorageFailure"
    toBS ErrorCodeTapeCartridgeNotFound = "TapeCartridgeNotFound"
    toBS ErrorCodeTargetAlreadyExists = "TargetAlreadyExists"
    toBS ErrorCodeTargetInvalid = "TargetInvalid"
    toBS ErrorCodeTargetNotFound = "TargetNotFound"
    toBS ErrorCodeUnauthorizedOperation = "UnauthorizedOperation"
    toBS ErrorCodeVolumeAlreadyExists = "VolumeAlreadyExists"
    toBS ErrorCodeVolumeIdInvalid = "VolumeIdInvalid"
    toBS ErrorCodeVolumeInUse = "VolumeInUse"
    toBS ErrorCodeVolumeNotFound = "VolumeNotFound"
    toBS ErrorCodeVolumeNotReady = "VolumeNotReady"

instance ToHeader ErrorCode where
    toHeader k = toHeader k . toBS

instance ToQuery ErrorCode where
    toQuery = toQuery . toBS

instance FromJSON ErrorCode

instance ToJSON ErrorCode

data CachediSCSIVolumeInformation = CachediSCSIVolumeInformation
    { _cscsiviVolumeARN :: Maybe Text
    , _cscsiviVolumeId :: Maybe Text
    , _cscsiviVolumeType :: Maybe Text
    , _cscsiviVolumeStatus :: Maybe Text
    , _cscsiviVolumeSizeInBytes :: Maybe Integer
    , _cscsiviVolumeProgress :: Maybe Double
    , _cscsiviSourceSnapshotId :: Maybe Text
    , _cscsiviVolumeiSCSIAttributes :: Maybe VolumeiSCSIAttributes
      -- ^ Lists iSCSI information about a volume.
    } deriving (Show, Generic)

cscsiviVolumeARN :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviVolumeARN f x =
    f (_cscsiviVolumeARN x)
        <&> \y -> x { _cscsiviVolumeARN = y }
{-# INLINE cscsiviVolumeARN #-}

cscsiviVolumeId :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviVolumeId f x =
    f (_cscsiviVolumeId x)
        <&> \y -> x { _cscsiviVolumeId = y }
{-# INLINE cscsiviVolumeId #-}

cscsiviVolumeType :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviVolumeType f x =
    f (_cscsiviVolumeType x)
        <&> \y -> x { _cscsiviVolumeType = y }
{-# INLINE cscsiviVolumeType #-}

cscsiviVolumeStatus :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviVolumeStatus f x =
    f (_cscsiviVolumeStatus x)
        <&> \y -> x { _cscsiviVolumeStatus = y }
{-# INLINE cscsiviVolumeStatus #-}

cscsiviVolumeSizeInBytes :: Lens' CachediSCSIVolumeInformation (Maybe Integer)
cscsiviVolumeSizeInBytes f x =
    f (_cscsiviVolumeSizeInBytes x)
        <&> \y -> x { _cscsiviVolumeSizeInBytes = y }
{-# INLINE cscsiviVolumeSizeInBytes #-}

cscsiviVolumeProgress :: Lens' CachediSCSIVolumeInformation (Maybe Double)
cscsiviVolumeProgress f x =
    f (_cscsiviVolumeProgress x)
        <&> \y -> x { _cscsiviVolumeProgress = y }
{-# INLINE cscsiviVolumeProgress #-}

cscsiviSourceSnapshotId :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviSourceSnapshotId f x =
    f (_cscsiviSourceSnapshotId x)
        <&> \y -> x { _cscsiviSourceSnapshotId = y }
{-# INLINE cscsiviSourceSnapshotId #-}

-- | Lists iSCSI information about a volume.
cscsiviVolumeiSCSIAttributes :: Lens' CachediSCSIVolumeInformation (Maybe VolumeiSCSIAttributes)
cscsiviVolumeiSCSIAttributes f x =
    f (_cscsiviVolumeiSCSIAttributes x)
        <&> \y -> x { _cscsiviVolumeiSCSIAttributes = y }
{-# INLINE cscsiviVolumeiSCSIAttributes #-}

instance FromJSON CachediSCSIVolumeInformation

-- | Describes Challenge-Handshake Authentication Protocol (CHAP) information
-- that supports authentication between your gateway and iSCSI initiators.
data ChapInfo = ChapInfo
    { _ciTargetARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the volume. Valid Values: 50 to
      -- 500 lowercase letters, numbers, periods (.), and hyphens (-).
    , _ciSecretToAuthenticateInitiator :: Maybe Text
      -- ^ The secret key that the initiator (e.g. Windows client) must
      -- provide to participate in mutual CHAP with the target.
    , _ciInitiatorName :: Maybe Text
      -- ^ The iSCSI initiator that connects to the target.
    , _ciSecretToAuthenticateTarget :: Maybe Text
      -- ^ The secret key that the target must provide to participate in
      -- mutual CHAP with the initiator (e.g. Windows client).
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the volume. Valid Values: 50 to 500
-- lowercase letters, numbers, periods (.), and hyphens (-).
ciTargetARN :: Lens' ChapInfo (Maybe Text)
ciTargetARN f x =
    f (_ciTargetARN x)
        <&> \y -> x { _ciTargetARN = y }
{-# INLINE ciTargetARN #-}

-- | The secret key that the initiator (e.g. Windows client) must provide to
-- participate in mutual CHAP with the target.
ciSecretToAuthenticateInitiator :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateInitiator f x =
    f (_ciSecretToAuthenticateInitiator x)
        <&> \y -> x { _ciSecretToAuthenticateInitiator = y }
{-# INLINE ciSecretToAuthenticateInitiator #-}

-- | The iSCSI initiator that connects to the target.
ciInitiatorName :: Lens' ChapInfo (Maybe Text)
ciInitiatorName f x =
    f (_ciInitiatorName x)
        <&> \y -> x { _ciInitiatorName = y }
{-# INLINE ciInitiatorName #-}

-- | The secret key that the target must provide to participate in mutual CHAP
-- with the initiator (e.g. Windows client).
ciSecretToAuthenticateTarget :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateTarget f x =
    f (_ciSecretToAuthenticateTarget x)
        <&> \y -> x { _ciSecretToAuthenticateTarget = y }
{-# INLINE ciSecretToAuthenticateTarget #-}

instance FromJSON ChapInfo

data DeviceiSCSIAttributes = DeviceiSCSIAttributes
    { _dscsiaTargetARN :: Maybe Text
    , _dscsiaNetworkInterfaceId :: Maybe Text
    , _dscsiaNetworkInterfacePort :: Maybe Integer
    , _dscsiaChapEnabled :: Maybe Bool
    } deriving (Show, Generic)

dscsiaTargetARN :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaTargetARN f x =
    f (_dscsiaTargetARN x)
        <&> \y -> x { _dscsiaTargetARN = y }
{-# INLINE dscsiaTargetARN #-}

dscsiaNetworkInterfaceId :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaNetworkInterfaceId f x =
    f (_dscsiaNetworkInterfaceId x)
        <&> \y -> x { _dscsiaNetworkInterfaceId = y }
{-# INLINE dscsiaNetworkInterfaceId #-}

dscsiaNetworkInterfacePort :: Lens' DeviceiSCSIAttributes (Maybe Integer)
dscsiaNetworkInterfacePort f x =
    f (_dscsiaNetworkInterfacePort x)
        <&> \y -> x { _dscsiaNetworkInterfacePort = y }
{-# INLINE dscsiaNetworkInterfacePort #-}

dscsiaChapEnabled :: Lens' DeviceiSCSIAttributes (Maybe Bool)
dscsiaChapEnabled f x =
    f (_dscsiaChapEnabled x)
        <&> \y -> x { _dscsiaChapEnabled = y }
{-# INLINE dscsiaChapEnabled #-}

instance FromJSON DeviceiSCSIAttributes

instance ToJSON DeviceiSCSIAttributes

data DiskInformation = DiskInformation
    { _ddnDiskId :: Maybe Text
    , _ddnDiskPath :: Maybe Text
    , _ddnDiskNode :: Maybe Text
    , _ddnDiskSizeInBytes :: Maybe Integer
    , _ddnDiskAllocationType :: Maybe Text
    , _ddnDiskAllocationResource :: Maybe Text
    } deriving (Show, Generic)

ddnDiskId :: Lens' DiskInformation (Maybe Text)
ddnDiskId f x =
    f (_ddnDiskId x)
        <&> \y -> x { _ddnDiskId = y }
{-# INLINE ddnDiskId #-}

ddnDiskPath :: Lens' DiskInformation (Maybe Text)
ddnDiskPath f x =
    f (_ddnDiskPath x)
        <&> \y -> x { _ddnDiskPath = y }
{-# INLINE ddnDiskPath #-}

ddnDiskNode :: Lens' DiskInformation (Maybe Text)
ddnDiskNode f x =
    f (_ddnDiskNode x)
        <&> \y -> x { _ddnDiskNode = y }
{-# INLINE ddnDiskNode #-}

ddnDiskSizeInBytes :: Lens' DiskInformation (Maybe Integer)
ddnDiskSizeInBytes f x =
    f (_ddnDiskSizeInBytes x)
        <&> \y -> x { _ddnDiskSizeInBytes = y }
{-# INLINE ddnDiskSizeInBytes #-}

ddnDiskAllocationType :: Lens' DiskInformation (Maybe Text)
ddnDiskAllocationType f x =
    f (_ddnDiskAllocationType x)
        <&> \y -> x { _ddnDiskAllocationType = y }
{-# INLINE ddnDiskAllocationType #-}

ddnDiskAllocationResource :: Lens' DiskInformation (Maybe Text)
ddnDiskAllocationResource f x =
    f (_ddnDiskAllocationResource x)
        <&> \y -> x { _ddnDiskAllocationResource = y }
{-# INLINE ddnDiskAllocationResource #-}

instance FromJSON DiskInformation

data GatewayInformation = GatewayInformation
    { _gjGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _gjGatewayType :: Maybe Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
gjGatewayARN :: Lens' GatewayInformation (Maybe Text)
gjGatewayARN f x =
    f (_gjGatewayARN x)
        <&> \y -> x { _gjGatewayARN = y }
{-# INLINE gjGatewayARN #-}

gjGatewayType :: Lens' GatewayInformation (Maybe Text)
gjGatewayType f x =
    f (_gjGatewayType x)
        <&> \y -> x { _gjGatewayType = y }
{-# INLINE gjGatewayType #-}

instance FromJSON GatewayInformation

-- | Describes a gateway's network interface.
data NetworkInterface = NetworkInterface
    { _niIpv4Address :: Maybe Text
      -- ^ The Internet Protocol version 4 (IPv4) address of the interface.
    , _niMacAddress :: Maybe Text
      -- ^ The Media Access Control (MAC) address of the interface. This is
      -- currently unsupported and will not be returned in output.
    , _niIpv6Address :: Maybe Text
      -- ^ The Internet Protocol version 6 (IPv6) address of the interface.
      -- Currently not supported.
    } deriving (Show, Generic)

-- | The Internet Protocol version 4 (IPv4) address of the interface.
niIpv4Address :: Lens' NetworkInterface (Maybe Text)
niIpv4Address f x =
    f (_niIpv4Address x)
        <&> \y -> x { _niIpv4Address = y }
{-# INLINE niIpv4Address #-}

-- | The Media Access Control (MAC) address of the interface. This is currently
-- unsupported and will not be returned in output.
niMacAddress :: Lens' NetworkInterface (Maybe Text)
niMacAddress f x =
    f (_niMacAddress x)
        <&> \y -> x { _niMacAddress = y }
{-# INLINE niMacAddress #-}

-- | The Internet Protocol version 6 (IPv6) address of the interface. Currently
-- not supported.
niIpv6Address :: Lens' NetworkInterface (Maybe Text)
niIpv6Address f x =
    f (_niIpv6Address x)
        <&> \y -> x { _niIpv6Address = y }
{-# INLINE niIpv6Address #-}

instance FromJSON NetworkInterface

-- | A StorageGatewayError that provides more detail about the cause of the
-- error.
data StorageGatewayError = StorageGatewayError
    { _sgeErrorCode :: Maybe ErrorCode
      -- ^ Additional information about the error.
    , _sgeErrorDetails :: Map Text Text
      -- ^ Human-readable text that provides detail about the error that
      -- occured.
    } deriving (Show, Generic)

-- | Additional information about the error.
sgeErrorCode :: Lens' StorageGatewayError (Maybe ErrorCode)
sgeErrorCode f x =
    f (_sgeErrorCode x)
        <&> \y -> x { _sgeErrorCode = y }
{-# INLINE sgeErrorCode #-}

-- | Human-readable text that provides detail about the error that occured.
sgeErrorDetails :: Lens' StorageGatewayError (Map Text Text)
sgeErrorDetails f x =
    f (_sgeErrorDetails x)
        <&> \y -> x { _sgeErrorDetails = y }
{-# INLINE sgeErrorDetails #-}

instance FromJSON StorageGatewayError

instance ToJSON StorageGatewayError

data StorediSCSIVolumeInformation = StorediSCSIVolumeInformation
    { _sscsiviVolumeARN :: Maybe Text
    , _sscsiviVolumeId :: Maybe Text
    , _sscsiviVolumeType :: Maybe Text
    , _sscsiviVolumeStatus :: Maybe Text
    , _sscsiviVolumeSizeInBytes :: Maybe Integer
    , _sscsiviVolumeProgress :: Maybe Double
    , _sscsiviVolumeDiskId :: Maybe Text
    , _sscsiviSourceSnapshotId :: Maybe Text
    , _sscsiviPreservedExistingData :: Maybe Bool
    , _sscsiviVolumeiSCSIAttributes :: Maybe VolumeiSCSIAttributes
      -- ^ Lists iSCSI information about a volume.
    } deriving (Show, Generic)

sscsiviVolumeARN :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeARN f x =
    f (_sscsiviVolumeARN x)
        <&> \y -> x { _sscsiviVolumeARN = y }
{-# INLINE sscsiviVolumeARN #-}

sscsiviVolumeId :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeId f x =
    f (_sscsiviVolumeId x)
        <&> \y -> x { _sscsiviVolumeId = y }
{-# INLINE sscsiviVolumeId #-}

sscsiviVolumeType :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeType f x =
    f (_sscsiviVolumeType x)
        <&> \y -> x { _sscsiviVolumeType = y }
{-# INLINE sscsiviVolumeType #-}

sscsiviVolumeStatus :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeStatus f x =
    f (_sscsiviVolumeStatus x)
        <&> \y -> x { _sscsiviVolumeStatus = y }
{-# INLINE sscsiviVolumeStatus #-}

sscsiviVolumeSizeInBytes :: Lens' StorediSCSIVolumeInformation (Maybe Integer)
sscsiviVolumeSizeInBytes f x =
    f (_sscsiviVolumeSizeInBytes x)
        <&> \y -> x { _sscsiviVolumeSizeInBytes = y }
{-# INLINE sscsiviVolumeSizeInBytes #-}

sscsiviVolumeProgress :: Lens' StorediSCSIVolumeInformation (Maybe Double)
sscsiviVolumeProgress f x =
    f (_sscsiviVolumeProgress x)
        <&> \y -> x { _sscsiviVolumeProgress = y }
{-# INLINE sscsiviVolumeProgress #-}

sscsiviVolumeDiskId :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeDiskId f x =
    f (_sscsiviVolumeDiskId x)
        <&> \y -> x { _sscsiviVolumeDiskId = y }
{-# INLINE sscsiviVolumeDiskId #-}

sscsiviSourceSnapshotId :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviSourceSnapshotId f x =
    f (_sscsiviSourceSnapshotId x)
        <&> \y -> x { _sscsiviSourceSnapshotId = y }
{-# INLINE sscsiviSourceSnapshotId #-}

sscsiviPreservedExistingData :: Lens' StorediSCSIVolumeInformation (Maybe Bool)
sscsiviPreservedExistingData f x =
    f (_sscsiviPreservedExistingData x)
        <&> \y -> x { _sscsiviPreservedExistingData = y }
{-# INLINE sscsiviPreservedExistingData #-}

-- | Lists iSCSI information about a volume.
sscsiviVolumeiSCSIAttributes :: Lens' StorediSCSIVolumeInformation (Maybe VolumeiSCSIAttributes)
sscsiviVolumeiSCSIAttributes f x =
    f (_sscsiviVolumeiSCSIAttributes x)
        <&> \y -> x { _sscsiviVolumeiSCSIAttributes = y }
{-# INLINE sscsiviVolumeiSCSIAttributes #-}

instance FromJSON StorediSCSIVolumeInformation

data Tape = Tape
    { _teTapeARN :: Maybe Text
    , _teTapeBarcode :: Maybe Text
    , _teTapeSizeInBytes :: Maybe Integer
    , _teTapeStatus :: Maybe Text
    , _teVTLDevice :: Maybe Text
    , _teProgress :: Maybe Double
    } deriving (Show, Generic)

teTapeARN :: Lens' Tape (Maybe Text)
teTapeARN f x =
    f (_teTapeARN x)
        <&> \y -> x { _teTapeARN = y }
{-# INLINE teTapeARN #-}

teTapeBarcode :: Lens' Tape (Maybe Text)
teTapeBarcode f x =
    f (_teTapeBarcode x)
        <&> \y -> x { _teTapeBarcode = y }
{-# INLINE teTapeBarcode #-}

teTapeSizeInBytes :: Lens' Tape (Maybe Integer)
teTapeSizeInBytes f x =
    f (_teTapeSizeInBytes x)
        <&> \y -> x { _teTapeSizeInBytes = y }
{-# INLINE teTapeSizeInBytes #-}

teTapeStatus :: Lens' Tape (Maybe Text)
teTapeStatus f x =
    f (_teTapeStatus x)
        <&> \y -> x { _teTapeStatus = y }
{-# INLINE teTapeStatus #-}

teVTLDevice :: Lens' Tape (Maybe Text)
teVTLDevice f x =
    f (_teVTLDevice x)
        <&> \y -> x { _teVTLDevice = y }
{-# INLINE teVTLDevice #-}

teProgress :: Lens' Tape (Maybe Double)
teProgress f x =
    f (_teProgress x)
        <&> \y -> x { _teProgress = y }
{-# INLINE teProgress #-}

instance FromJSON Tape

data TapeArchive = TapeArchive
    { _tbTapeARN :: Maybe Text
    , _tbTapeBarcode :: Maybe Text
    , _tbTapeSizeInBytes :: Maybe Integer
    , _tbCompletionTime :: Maybe ISO8601
    , _tbRetrievedTo :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _tbTapeStatus :: Maybe Text
    } deriving (Show, Generic)

tbTapeARN :: Lens' TapeArchive (Maybe Text)
tbTapeARN f x =
    f (_tbTapeARN x)
        <&> \y -> x { _tbTapeARN = y }
{-# INLINE tbTapeARN #-}

tbTapeBarcode :: Lens' TapeArchive (Maybe Text)
tbTapeBarcode f x =
    f (_tbTapeBarcode x)
        <&> \y -> x { _tbTapeBarcode = y }
{-# INLINE tbTapeBarcode #-}

tbTapeSizeInBytes :: Lens' TapeArchive (Maybe Integer)
tbTapeSizeInBytes f x =
    f (_tbTapeSizeInBytes x)
        <&> \y -> x { _tbTapeSizeInBytes = y }
{-# INLINE tbTapeSizeInBytes #-}

tbCompletionTime :: Lens' TapeArchive (Maybe ISO8601)
tbCompletionTime f x =
    f (_tbCompletionTime x)
        <&> \y -> x { _tbCompletionTime = y }
{-# INLINE tbCompletionTime #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
tbRetrievedTo :: Lens' TapeArchive (Maybe Text)
tbRetrievedTo f x =
    f (_tbRetrievedTo x)
        <&> \y -> x { _tbRetrievedTo = y }
{-# INLINE tbRetrievedTo #-}

tbTapeStatus :: Lens' TapeArchive (Maybe Text)
tbTapeStatus f x =
    f (_tbTapeStatus x)
        <&> \y -> x { _tbTapeStatus = y }
{-# INLINE tbTapeStatus #-}

instance FromJSON TapeArchive

data TapeRecoveryPointInfo = TapeRecoveryPointInfo
    { _trpjTapeARN :: Maybe Text
    , _trpjTapeRecoveryPointTime :: Maybe ISO8601
    , _trpjTapeSizeInBytes :: Maybe Integer
    } deriving (Show, Generic)

trpjTapeARN :: Lens' TapeRecoveryPointInfo (Maybe Text)
trpjTapeARN f x =
    f (_trpjTapeARN x)
        <&> \y -> x { _trpjTapeARN = y }
{-# INLINE trpjTapeARN #-}

trpjTapeRecoveryPointTime :: Lens' TapeRecoveryPointInfo (Maybe ISO8601)
trpjTapeRecoveryPointTime f x =
    f (_trpjTapeRecoveryPointTime x)
        <&> \y -> x { _trpjTapeRecoveryPointTime = y }
{-# INLINE trpjTapeRecoveryPointTime #-}

trpjTapeSizeInBytes :: Lens' TapeRecoveryPointInfo (Maybe Integer)
trpjTapeSizeInBytes f x =
    f (_trpjTapeSizeInBytes x)
        <&> \y -> x { _trpjTapeSizeInBytes = y }
{-# INLINE trpjTapeSizeInBytes #-}

instance FromJSON TapeRecoveryPointInfo

data VTLDevice = VTLDevice
    { _vtleVTLDeviceARN :: Maybe Text
    , _vtleDeviceiSCSIAttributes :: Maybe DeviceiSCSIAttributes
    } deriving (Show, Generic)

vtleVTLDeviceARN :: Lens' VTLDevice (Maybe Text)
vtleVTLDeviceARN f x =
    f (_vtleVTLDeviceARN x)
        <&> \y -> x { _vtleVTLDeviceARN = y }
{-# INLINE vtleVTLDeviceARN #-}

vtleDeviceiSCSIAttributes :: Lens' VTLDevice (Maybe DeviceiSCSIAttributes)
vtleDeviceiSCSIAttributes f x =
    f (_vtleDeviceiSCSIAttributes x)
        <&> \y -> x { _vtleDeviceiSCSIAttributes = y }
{-# INLINE vtleDeviceiSCSIAttributes #-}

instance FromJSON VTLDevice

data VolumeInformation = VolumeInformation
    { _vlVolumeARN :: Maybe Text
    , _vlVolumeType :: Maybe Text
    } deriving (Show, Generic)

vlVolumeARN :: Lens' VolumeInformation (Maybe Text)
vlVolumeARN f x =
    f (_vlVolumeARN x)
        <&> \y -> x { _vlVolumeARN = y }
{-# INLINE vlVolumeARN #-}

vlVolumeType :: Lens' VolumeInformation (Maybe Text)
vlVolumeType f x =
    f (_vlVolumeType x)
        <&> \y -> x { _vlVolumeType = y }
{-# INLINE vlVolumeType #-}

instance FromJSON VolumeInformation

data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo
    { _vrpjVolumeARN :: Maybe Text
    , _vrpjVolumeSizeInBytes :: Maybe Integer
    , _vrpjVolumeUsageInBytes :: Maybe Integer
    , _vrpjVolumeRecoveryPointTime :: Maybe Text
    } deriving (Show, Generic)

vrpjVolumeARN :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpjVolumeARN f x =
    f (_vrpjVolumeARN x)
        <&> \y -> x { _vrpjVolumeARN = y }
{-# INLINE vrpjVolumeARN #-}

vrpjVolumeSizeInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpjVolumeSizeInBytes f x =
    f (_vrpjVolumeSizeInBytes x)
        <&> \y -> x { _vrpjVolumeSizeInBytes = y }
{-# INLINE vrpjVolumeSizeInBytes #-}

vrpjVolumeUsageInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpjVolumeUsageInBytes f x =
    f (_vrpjVolumeUsageInBytes x)
        <&> \y -> x { _vrpjVolumeUsageInBytes = y }
{-# INLINE vrpjVolumeUsageInBytes #-}

vrpjVolumeRecoveryPointTime :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpjVolumeRecoveryPointTime f x =
    f (_vrpjVolumeRecoveryPointTime x)
        <&> \y -> x { _vrpjVolumeRecoveryPointTime = y }
{-# INLINE vrpjVolumeRecoveryPointTime #-}

instance FromJSON VolumeRecoveryPointInfo

-- | Lists iSCSI information about a volume.
data VolumeiSCSIAttributes = VolumeiSCSIAttributes
    { _vscsiaTargetARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the volume target.
    , _vscsiaNetworkInterfaceId :: Maybe Text
      -- ^ The network interface identifier.
    , _vscsiaNetworkInterfacePort :: Maybe Integer
      -- ^ The port used to communicate with iSCSI targets.
    , _vscsiaLunNumber :: Maybe Integer
      -- ^ The logical disk number.
    , _vscsiaChapEnabled :: Maybe Bool
      -- ^ Indicates whether mutual CHAP is enabled for the iSCSI target.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the volume target.
vscsiaTargetARN :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaTargetARN f x =
    f (_vscsiaTargetARN x)
        <&> \y -> x { _vscsiaTargetARN = y }
{-# INLINE vscsiaTargetARN #-}

-- | The network interface identifier.
vscsiaNetworkInterfaceId :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaNetworkInterfaceId f x =
    f (_vscsiaNetworkInterfaceId x)
        <&> \y -> x { _vscsiaNetworkInterfaceId = y }
{-# INLINE vscsiaNetworkInterfaceId #-}

-- | The port used to communicate with iSCSI targets.
vscsiaNetworkInterfacePort :: Lens' VolumeiSCSIAttributes (Maybe Integer)
vscsiaNetworkInterfacePort f x =
    f (_vscsiaNetworkInterfacePort x)
        <&> \y -> x { _vscsiaNetworkInterfacePort = y }
{-# INLINE vscsiaNetworkInterfacePort #-}

-- | The logical disk number.
vscsiaLunNumber :: Lens' VolumeiSCSIAttributes (Maybe Integer)
vscsiaLunNumber f x =
    f (_vscsiaLunNumber x)
        <&> \y -> x { _vscsiaLunNumber = y }
{-# INLINE vscsiaLunNumber #-}

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
vscsiaChapEnabled :: Lens' VolumeiSCSIAttributes (Maybe Bool)
vscsiaChapEnabled f x =
    f (_vscsiaChapEnabled x)
        <&> \y -> x { _vscsiaChapEnabled = y }
{-# INLINE vscsiaChapEnabled #-}

instance FromJSON VolumeiSCSIAttributes

instance ToJSON VolumeiSCSIAttributes
