{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
    , CachediSCSIVolumeInformation
    , mkCachediSCSIVolumeInformation
    , cscsiviVolumeARN
    , cscsiviVolumeId
    , cscsiviVolumeType
    , cscsiviVolumeStatus
    , cscsiviVolumeSizeInBytes
    , cscsiviVolumeProgress
    , cscsiviSourceSnapshotId
    , cscsiviVolumeiSCSIAttributes

    -- * ChapInfo
    , ChapInfo
    , mkChapInfo
    , ciTargetARN
    , ciSecretToAuthenticateInitiator
    , ciInitiatorName
    , ciSecretToAuthenticateTarget

    -- * DeviceiSCSIAttributes
    , DeviceiSCSIAttributes
    , mkDeviceiSCSIAttributes
    , dscsiaTargetARN
    , dscsiaNetworkInterfaceId
    , dscsiaNetworkInterfacePort
    , dscsiaChapEnabled

    -- * DiskInformation
    , DiskInformation
    , mkDiskInformation
    , diDiskId
    , diDiskPath
    , diDiskNode
    , diDiskSizeInBytes
    , diDiskAllocationType
    , diDiskAllocationResource

    -- * GatewayInformation
    , GatewayInformation
    , mkGatewayInformation
    , giGatewayARN
    , giGatewayType

    -- * NetworkInterface
    , NetworkInterface
    , mkNetworkInterface
    , niIpv4Address
    , niMacAddress
    , niIpv6Address

    -- * StorageGatewayError
    , StorageGatewayError
    , mkStorageGatewayError
    , sgeErrorCode
    , sgeErrorDetails

    -- * StorediSCSIVolumeInformation
    , StorediSCSIVolumeInformation
    , mkStorediSCSIVolumeInformation
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
    , Tape
    , mkTape
    , tTapeARN
    , tTapeBarcode
    , tTapeSizeInBytes
    , tTapeStatus
    , tVTLDevice
    , tProgress

    -- * TapeArchive
    , TapeArchive
    , mkTapeArchive
    , taTapeARN
    , taTapeBarcode
    , taTapeSizeInBytes
    , taCompletionTime
    , taRetrievedTo
    , taTapeStatus

    -- * TapeRecoveryPointInfo
    , TapeRecoveryPointInfo
    , mkTapeRecoveryPointInfo
    , trpiTapeARN
    , trpiTapeRecoveryPointTime
    , trpiTapeSizeInBytes

    -- * VTLDevice
    , VTLDevice
    , mkVTLDevice
    , vtldVTLDeviceARN
    , vtldDeviceiSCSIAttributes

    -- * VolumeInformation
    , VolumeInformation
    , mkVolumeInformation
    , viVolumeARN
    , viVolumeType

    -- * VolumeRecoveryPointInfo
    , VolumeRecoveryPointInfo
    , mkVolumeRecoveryPointInfo
    , vrpiVolumeARN
    , vrpiVolumeSizeInBytes
    , vrpiVolumeUsageInBytes
    , vrpiVolumeRecoveryPointTime

    -- * VolumeiSCSIAttributes
    , VolumeiSCSIAttributes
    , mkVolumeiSCSIAttributes
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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CachediSCSIVolumeInformation' data type to populate a request.
mkCachediSCSIVolumeInformation :: CachediSCSIVolumeInformation
mkCachediSCSIVolumeInformation = CachediSCSIVolumeInformation
    { _cscsiviVolumeARN = Nothing
    , _cscsiviVolumeId = Nothing
    , _cscsiviVolumeType = Nothing
    , _cscsiviVolumeStatus = Nothing
    , _cscsiviVolumeSizeInBytes = Nothing
    , _cscsiviVolumeProgress = Nothing
    , _cscsiviSourceSnapshotId = Nothing
    , _cscsiviVolumeiSCSIAttributes = Nothing
    }
{-# INLINE mkCachediSCSIVolumeInformation #-}

cscsiviVolumeARN :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviVolumeARN =
    lens _cscsiviVolumeARN (\s a -> s { _cscsiviVolumeARN = a })
{-# INLINE cscsiviVolumeARN #-}

cscsiviVolumeId :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviVolumeId = lens _cscsiviVolumeId (\s a -> s { _cscsiviVolumeId = a })
{-# INLINE cscsiviVolumeId #-}

cscsiviVolumeType :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviVolumeType =
    lens _cscsiviVolumeType (\s a -> s { _cscsiviVolumeType = a })
{-# INLINE cscsiviVolumeType #-}

cscsiviVolumeStatus :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviVolumeStatus =
    lens _cscsiviVolumeStatus (\s a -> s { _cscsiviVolumeStatus = a })
{-# INLINE cscsiviVolumeStatus #-}

cscsiviVolumeSizeInBytes :: Lens' CachediSCSIVolumeInformation (Maybe Integer)
cscsiviVolumeSizeInBytes =
    lens _cscsiviVolumeSizeInBytes
         (\s a -> s { _cscsiviVolumeSizeInBytes = a })
{-# INLINE cscsiviVolumeSizeInBytes #-}

cscsiviVolumeProgress :: Lens' CachediSCSIVolumeInformation (Maybe Double)
cscsiviVolumeProgress =
    lens _cscsiviVolumeProgress (\s a -> s { _cscsiviVolumeProgress = a })
{-# INLINE cscsiviVolumeProgress #-}

cscsiviSourceSnapshotId :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviSourceSnapshotId =
    lens _cscsiviSourceSnapshotId
         (\s a -> s { _cscsiviSourceSnapshotId = a })
{-# INLINE cscsiviSourceSnapshotId #-}

-- | Lists iSCSI information about a volume.
cscsiviVolumeiSCSIAttributes :: Lens' CachediSCSIVolumeInformation (Maybe VolumeiSCSIAttributes)
cscsiviVolumeiSCSIAttributes =
    lens _cscsiviVolumeiSCSIAttributes
         (\s a -> s { _cscsiviVolumeiSCSIAttributes = a })
{-# INLINE cscsiviVolumeiSCSIAttributes #-}

instance FromJSON CachediSCSIVolumeInformation

-- | Describes Challenge-Handshake Authentication Protocol (CHAP) information
-- that supports authentication between your gateway and iSCSI initiators.
data ChapInfo = ChapInfo
    { _ciTargetARN :: Maybe Text
    , _ciSecretToAuthenticateInitiator :: Maybe Text
    , _ciInitiatorName :: Maybe Text
    , _ciSecretToAuthenticateTarget :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ChapInfo' data type to populate a request.
mkChapInfo :: ChapInfo
mkChapInfo = ChapInfo
    { _ciTargetARN = Nothing
    , _ciSecretToAuthenticateInitiator = Nothing
    , _ciInitiatorName = Nothing
    , _ciSecretToAuthenticateTarget = Nothing
    }
{-# INLINE mkChapInfo #-}

-- | The Amazon Resource Name (ARN) of the volume. Valid Values: 50 to 500
-- lowercase letters, numbers, periods (.), and hyphens (-).
ciTargetARN :: Lens' ChapInfo (Maybe Text)
ciTargetARN = lens _ciTargetARN (\s a -> s { _ciTargetARN = a })
{-# INLINE ciTargetARN #-}

-- | The secret key that the initiator (e.g. Windows client) must provide to
-- participate in mutual CHAP with the target.
ciSecretToAuthenticateInitiator :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateInitiator =
    lens _ciSecretToAuthenticateInitiator
         (\s a -> s { _ciSecretToAuthenticateInitiator = a })
{-# INLINE ciSecretToAuthenticateInitiator #-}

-- | The iSCSI initiator that connects to the target.
ciInitiatorName :: Lens' ChapInfo (Maybe Text)
ciInitiatorName = lens _ciInitiatorName (\s a -> s { _ciInitiatorName = a })
{-# INLINE ciInitiatorName #-}

-- | The secret key that the target must provide to participate in mutual CHAP
-- with the initiator (e.g. Windows client).
ciSecretToAuthenticateTarget :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateTarget =
    lens _ciSecretToAuthenticateTarget
         (\s a -> s { _ciSecretToAuthenticateTarget = a })
{-# INLINE ciSecretToAuthenticateTarget #-}

instance FromJSON ChapInfo

data DeviceiSCSIAttributes = DeviceiSCSIAttributes
    { _dscsiaTargetARN :: Maybe Text
    , _dscsiaNetworkInterfaceId :: Maybe Text
    , _dscsiaNetworkInterfacePort :: Maybe Integer
    , _dscsiaChapEnabled :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DeviceiSCSIAttributes' data type to populate a request.
mkDeviceiSCSIAttributes :: DeviceiSCSIAttributes
mkDeviceiSCSIAttributes = DeviceiSCSIAttributes
    { _dscsiaTargetARN = Nothing
    , _dscsiaNetworkInterfaceId = Nothing
    , _dscsiaNetworkInterfacePort = Nothing
    , _dscsiaChapEnabled = Nothing
    }
{-# INLINE mkDeviceiSCSIAttributes #-}

dscsiaTargetARN :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaTargetARN = lens _dscsiaTargetARN (\s a -> s { _dscsiaTargetARN = a })
{-# INLINE dscsiaTargetARN #-}

dscsiaNetworkInterfaceId :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaNetworkInterfaceId =
    lens _dscsiaNetworkInterfaceId
         (\s a -> s { _dscsiaNetworkInterfaceId = a })
{-# INLINE dscsiaNetworkInterfaceId #-}

dscsiaNetworkInterfacePort :: Lens' DeviceiSCSIAttributes (Maybe Integer)
dscsiaNetworkInterfacePort =
    lens _dscsiaNetworkInterfacePort
         (\s a -> s { _dscsiaNetworkInterfacePort = a })
{-# INLINE dscsiaNetworkInterfacePort #-}

dscsiaChapEnabled :: Lens' DeviceiSCSIAttributes (Maybe Bool)
dscsiaChapEnabled =
    lens _dscsiaChapEnabled (\s a -> s { _dscsiaChapEnabled = a })
{-# INLINE dscsiaChapEnabled #-}

instance FromJSON DeviceiSCSIAttributes

instance ToJSON DeviceiSCSIAttributes

data DiskInformation = DiskInformation
    { _diDiskId :: Maybe Text
    , _diDiskPath :: Maybe Text
    , _diDiskNode :: Maybe Text
    , _diDiskSizeInBytes :: Maybe Integer
    , _diDiskAllocationType :: Maybe Text
    , _diDiskAllocationResource :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DiskInformation' data type to populate a request.
mkDiskInformation :: DiskInformation
mkDiskInformation = DiskInformation
    { _diDiskId = Nothing
    , _diDiskPath = Nothing
    , _diDiskNode = Nothing
    , _diDiskSizeInBytes = Nothing
    , _diDiskAllocationType = Nothing
    , _diDiskAllocationResource = Nothing
    }
{-# INLINE mkDiskInformation #-}

diDiskId :: Lens' DiskInformation (Maybe Text)
diDiskId = lens _diDiskId (\s a -> s { _diDiskId = a })
{-# INLINE diDiskId #-}

diDiskPath :: Lens' DiskInformation (Maybe Text)
diDiskPath = lens _diDiskPath (\s a -> s { _diDiskPath = a })
{-# INLINE diDiskPath #-}

diDiskNode :: Lens' DiskInformation (Maybe Text)
diDiskNode = lens _diDiskNode (\s a -> s { _diDiskNode = a })
{-# INLINE diDiskNode #-}

diDiskSizeInBytes :: Lens' DiskInformation (Maybe Integer)
diDiskSizeInBytes =
    lens _diDiskSizeInBytes (\s a -> s { _diDiskSizeInBytes = a })
{-# INLINE diDiskSizeInBytes #-}

diDiskAllocationType :: Lens' DiskInformation (Maybe Text)
diDiskAllocationType =
    lens _diDiskAllocationType (\s a -> s { _diDiskAllocationType = a })
{-# INLINE diDiskAllocationType #-}

diDiskAllocationResource :: Lens' DiskInformation (Maybe Text)
diDiskAllocationResource =
    lens _diDiskAllocationResource
         (\s a -> s { _diDiskAllocationResource = a })
{-# INLINE diDiskAllocationResource #-}

instance FromJSON DiskInformation

data GatewayInformation = GatewayInformation
    { _giGatewayARN :: Maybe Text
    , _giGatewayType :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'GatewayInformation' data type to populate a request.
mkGatewayInformation :: GatewayInformation
mkGatewayInformation = GatewayInformation
    { _giGatewayARN = Nothing
    , _giGatewayType = Nothing
    }
{-# INLINE mkGatewayInformation #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
giGatewayARN :: Lens' GatewayInformation (Maybe Text)
giGatewayARN = lens _giGatewayARN (\s a -> s { _giGatewayARN = a })
{-# INLINE giGatewayARN #-}

giGatewayType :: Lens' GatewayInformation (Maybe Text)
giGatewayType = lens _giGatewayType (\s a -> s { _giGatewayType = a })
{-# INLINE giGatewayType #-}

instance FromJSON GatewayInformation

-- | Describes a gateway's network interface.
data NetworkInterface = NetworkInterface
    { _niIpv4Address :: Maybe Text
    , _niMacAddress :: Maybe Text
    , _niIpv6Address :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkInterface' data type to populate a request.
mkNetworkInterface :: NetworkInterface
mkNetworkInterface = NetworkInterface
    { _niIpv4Address = Nothing
    , _niMacAddress = Nothing
    , _niIpv6Address = Nothing
    }
{-# INLINE mkNetworkInterface #-}

-- | The Internet Protocol version 4 (IPv4) address of the interface.
niIpv4Address :: Lens' NetworkInterface (Maybe Text)
niIpv4Address = lens _niIpv4Address (\s a -> s { _niIpv4Address = a })
{-# INLINE niIpv4Address #-}

-- | The Media Access Control (MAC) address of the interface. This is currently
-- unsupported and will not be returned in output.
niMacAddress :: Lens' NetworkInterface (Maybe Text)
niMacAddress = lens _niMacAddress (\s a -> s { _niMacAddress = a })
{-# INLINE niMacAddress #-}

-- | The Internet Protocol version 6 (IPv6) address of the interface. Currently
-- not supported.
niIpv6Address :: Lens' NetworkInterface (Maybe Text)
niIpv6Address = lens _niIpv6Address (\s a -> s { _niIpv6Address = a })
{-# INLINE niIpv6Address #-}

instance FromJSON NetworkInterface

-- | A StorageGatewayError that provides more detail about the cause of the
-- error.
data StorageGatewayError = StorageGatewayError
    { _sgeErrorCode :: Maybe ErrorCode
    , _sgeErrorDetails :: Map Text Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StorageGatewayError' data type to populate a request.
mkStorageGatewayError :: StorageGatewayError
mkStorageGatewayError = StorageGatewayError
    { _sgeErrorCode = Nothing
    , _sgeErrorDetails = mempty
    }
{-# INLINE mkStorageGatewayError #-}

-- | Additional information about the error.
sgeErrorCode :: Lens' StorageGatewayError (Maybe ErrorCode)
sgeErrorCode = lens _sgeErrorCode (\s a -> s { _sgeErrorCode = a })
{-# INLINE sgeErrorCode #-}

-- | Human-readable text that provides detail about the error that occured.
sgeErrorDetails :: Lens' StorageGatewayError (Map Text Text)
sgeErrorDetails = lens _sgeErrorDetails (\s a -> s { _sgeErrorDetails = a })
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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StorediSCSIVolumeInformation' data type to populate a request.
mkStorediSCSIVolumeInformation :: StorediSCSIVolumeInformation
mkStorediSCSIVolumeInformation = StorediSCSIVolumeInformation
    { _sscsiviVolumeARN = Nothing
    , _sscsiviVolumeId = Nothing
    , _sscsiviVolumeType = Nothing
    , _sscsiviVolumeStatus = Nothing
    , _sscsiviVolumeSizeInBytes = Nothing
    , _sscsiviVolumeProgress = Nothing
    , _sscsiviVolumeDiskId = Nothing
    , _sscsiviSourceSnapshotId = Nothing
    , _sscsiviPreservedExistingData = Nothing
    , _sscsiviVolumeiSCSIAttributes = Nothing
    }
{-# INLINE mkStorediSCSIVolumeInformation #-}

sscsiviVolumeARN :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeARN =
    lens _sscsiviVolumeARN (\s a -> s { _sscsiviVolumeARN = a })
{-# INLINE sscsiviVolumeARN #-}

sscsiviVolumeId :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeId = lens _sscsiviVolumeId (\s a -> s { _sscsiviVolumeId = a })
{-# INLINE sscsiviVolumeId #-}

sscsiviVolumeType :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeType =
    lens _sscsiviVolumeType (\s a -> s { _sscsiviVolumeType = a })
{-# INLINE sscsiviVolumeType #-}

sscsiviVolumeStatus :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeStatus =
    lens _sscsiviVolumeStatus (\s a -> s { _sscsiviVolumeStatus = a })
{-# INLINE sscsiviVolumeStatus #-}

sscsiviVolumeSizeInBytes :: Lens' StorediSCSIVolumeInformation (Maybe Integer)
sscsiviVolumeSizeInBytes =
    lens _sscsiviVolumeSizeInBytes
         (\s a -> s { _sscsiviVolumeSizeInBytes = a })
{-# INLINE sscsiviVolumeSizeInBytes #-}

sscsiviVolumeProgress :: Lens' StorediSCSIVolumeInformation (Maybe Double)
sscsiviVolumeProgress =
    lens _sscsiviVolumeProgress (\s a -> s { _sscsiviVolumeProgress = a })
{-# INLINE sscsiviVolumeProgress #-}

sscsiviVolumeDiskId :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeDiskId =
    lens _sscsiviVolumeDiskId (\s a -> s { _sscsiviVolumeDiskId = a })
{-# INLINE sscsiviVolumeDiskId #-}

sscsiviSourceSnapshotId :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviSourceSnapshotId =
    lens _sscsiviSourceSnapshotId
         (\s a -> s { _sscsiviSourceSnapshotId = a })
{-# INLINE sscsiviSourceSnapshotId #-}

sscsiviPreservedExistingData :: Lens' StorediSCSIVolumeInformation (Maybe Bool)
sscsiviPreservedExistingData =
    lens _sscsiviPreservedExistingData
         (\s a -> s { _sscsiviPreservedExistingData = a })
{-# INLINE sscsiviPreservedExistingData #-}

-- | Lists iSCSI information about a volume.
sscsiviVolumeiSCSIAttributes :: Lens' StorediSCSIVolumeInformation (Maybe VolumeiSCSIAttributes)
sscsiviVolumeiSCSIAttributes =
    lens _sscsiviVolumeiSCSIAttributes
         (\s a -> s { _sscsiviVolumeiSCSIAttributes = a })
{-# INLINE sscsiviVolumeiSCSIAttributes #-}

instance FromJSON StorediSCSIVolumeInformation

data Tape = Tape
    { _tTapeARN :: Maybe Text
    , _tTapeBarcode :: Maybe Text
    , _tTapeSizeInBytes :: Maybe Integer
    , _tTapeStatus :: Maybe Text
    , _tVTLDevice :: Maybe Text
    , _tProgress :: Maybe Double
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tape' data type to populate a request.
mkTape :: Tape
mkTape = Tape
    { _tTapeARN = Nothing
    , _tTapeBarcode = Nothing
    , _tTapeSizeInBytes = Nothing
    , _tTapeStatus = Nothing
    , _tVTLDevice = Nothing
    , _tProgress = Nothing
    }
{-# INLINE mkTape #-}

tTapeARN :: Lens' Tape (Maybe Text)
tTapeARN = lens _tTapeARN (\s a -> s { _tTapeARN = a })
{-# INLINE tTapeARN #-}

tTapeBarcode :: Lens' Tape (Maybe Text)
tTapeBarcode = lens _tTapeBarcode (\s a -> s { _tTapeBarcode = a })
{-# INLINE tTapeBarcode #-}

tTapeSizeInBytes :: Lens' Tape (Maybe Integer)
tTapeSizeInBytes =
    lens _tTapeSizeInBytes (\s a -> s { _tTapeSizeInBytes = a })
{-# INLINE tTapeSizeInBytes #-}

tTapeStatus :: Lens' Tape (Maybe Text)
tTapeStatus = lens _tTapeStatus (\s a -> s { _tTapeStatus = a })
{-# INLINE tTapeStatus #-}

tVTLDevice :: Lens' Tape (Maybe Text)
tVTLDevice = lens _tVTLDevice (\s a -> s { _tVTLDevice = a })
{-# INLINE tVTLDevice #-}

tProgress :: Lens' Tape (Maybe Double)
tProgress = lens _tProgress (\s a -> s { _tProgress = a })
{-# INLINE tProgress #-}

instance FromJSON Tape

data TapeArchive = TapeArchive
    { _taTapeARN :: Maybe Text
    , _taTapeBarcode :: Maybe Text
    , _taTapeSizeInBytes :: Maybe Integer
    , _taCompletionTime :: Maybe ISO8601
    , _taRetrievedTo :: Maybe Text
    , _taTapeStatus :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TapeArchive' data type to populate a request.
mkTapeArchive :: TapeArchive
mkTapeArchive = TapeArchive
    { _taTapeARN = Nothing
    , _taTapeBarcode = Nothing
    , _taTapeSizeInBytes = Nothing
    , _taCompletionTime = Nothing
    , _taRetrievedTo = Nothing
    , _taTapeStatus = Nothing
    }
{-# INLINE mkTapeArchive #-}

taTapeARN :: Lens' TapeArchive (Maybe Text)
taTapeARN = lens _taTapeARN (\s a -> s { _taTapeARN = a })
{-# INLINE taTapeARN #-}

taTapeBarcode :: Lens' TapeArchive (Maybe Text)
taTapeBarcode = lens _taTapeBarcode (\s a -> s { _taTapeBarcode = a })
{-# INLINE taTapeBarcode #-}

taTapeSizeInBytes :: Lens' TapeArchive (Maybe Integer)
taTapeSizeInBytes =
    lens _taTapeSizeInBytes (\s a -> s { _taTapeSizeInBytes = a })
{-# INLINE taTapeSizeInBytes #-}

taCompletionTime :: Lens' TapeArchive (Maybe ISO8601)
taCompletionTime =
    lens _taCompletionTime (\s a -> s { _taCompletionTime = a })
{-# INLINE taCompletionTime #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
taRetrievedTo :: Lens' TapeArchive (Maybe Text)
taRetrievedTo = lens _taRetrievedTo (\s a -> s { _taRetrievedTo = a })
{-# INLINE taRetrievedTo #-}

taTapeStatus :: Lens' TapeArchive (Maybe Text)
taTapeStatus = lens _taTapeStatus (\s a -> s { _taTapeStatus = a })
{-# INLINE taTapeStatus #-}

instance FromJSON TapeArchive

data TapeRecoveryPointInfo = TapeRecoveryPointInfo
    { _trpiTapeARN :: Maybe Text
    , _trpiTapeRecoveryPointTime :: Maybe ISO8601
    , _trpiTapeSizeInBytes :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TapeRecoveryPointInfo' data type to populate a request.
mkTapeRecoveryPointInfo :: TapeRecoveryPointInfo
mkTapeRecoveryPointInfo = TapeRecoveryPointInfo
    { _trpiTapeARN = Nothing
    , _trpiTapeRecoveryPointTime = Nothing
    , _trpiTapeSizeInBytes = Nothing
    }
{-# INLINE mkTapeRecoveryPointInfo #-}

trpiTapeARN :: Lens' TapeRecoveryPointInfo (Maybe Text)
trpiTapeARN = lens _trpiTapeARN (\s a -> s { _trpiTapeARN = a })
{-# INLINE trpiTapeARN #-}

trpiTapeRecoveryPointTime :: Lens' TapeRecoveryPointInfo (Maybe ISO8601)
trpiTapeRecoveryPointTime =
    lens _trpiTapeRecoveryPointTime
         (\s a -> s { _trpiTapeRecoveryPointTime = a })
{-# INLINE trpiTapeRecoveryPointTime #-}

trpiTapeSizeInBytes :: Lens' TapeRecoveryPointInfo (Maybe Integer)
trpiTapeSizeInBytes =
    lens _trpiTapeSizeInBytes (\s a -> s { _trpiTapeSizeInBytes = a })
{-# INLINE trpiTapeSizeInBytes #-}

instance FromJSON TapeRecoveryPointInfo

data VTLDevice = VTLDevice
    { _vtldVTLDeviceARN :: Maybe Text
    , _vtldDeviceiSCSIAttributes :: Maybe DeviceiSCSIAttributes
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VTLDevice' data type to populate a request.
mkVTLDevice :: VTLDevice
mkVTLDevice = VTLDevice
    { _vtldVTLDeviceARN = Nothing
    , _vtldDeviceiSCSIAttributes = Nothing
    }
{-# INLINE mkVTLDevice #-}

vtldVTLDeviceARN :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceARN =
    lens _vtldVTLDeviceARN (\s a -> s { _vtldVTLDeviceARN = a })
{-# INLINE vtldVTLDeviceARN #-}

vtldDeviceiSCSIAttributes :: Lens' VTLDevice (Maybe DeviceiSCSIAttributes)
vtldDeviceiSCSIAttributes =
    lens _vtldDeviceiSCSIAttributes
         (\s a -> s { _vtldDeviceiSCSIAttributes = a })
{-# INLINE vtldDeviceiSCSIAttributes #-}

instance FromJSON VTLDevice

data VolumeInformation = VolumeInformation
    { _viVolumeARN :: Maybe Text
    , _viVolumeType :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeInformation' data type to populate a request.
mkVolumeInformation :: VolumeInformation
mkVolumeInformation = VolumeInformation
    { _viVolumeARN = Nothing
    , _viVolumeType = Nothing
    }
{-# INLINE mkVolumeInformation #-}

viVolumeARN :: Lens' VolumeInformation (Maybe Text)
viVolumeARN = lens _viVolumeARN (\s a -> s { _viVolumeARN = a })
{-# INLINE viVolumeARN #-}

viVolumeType :: Lens' VolumeInformation (Maybe Text)
viVolumeType = lens _viVolumeType (\s a -> s { _viVolumeType = a })
{-# INLINE viVolumeType #-}

instance FromJSON VolumeInformation

data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo
    { _vrpiVolumeARN :: Maybe Text
    , _vrpiVolumeSizeInBytes :: Maybe Integer
    , _vrpiVolumeUsageInBytes :: Maybe Integer
    , _vrpiVolumeRecoveryPointTime :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeRecoveryPointInfo' data type to populate a request.
mkVolumeRecoveryPointInfo :: VolumeRecoveryPointInfo
mkVolumeRecoveryPointInfo = VolumeRecoveryPointInfo
    { _vrpiVolumeARN = Nothing
    , _vrpiVolumeSizeInBytes = Nothing
    , _vrpiVolumeUsageInBytes = Nothing
    , _vrpiVolumeRecoveryPointTime = Nothing
    }
{-# INLINE mkVolumeRecoveryPointInfo #-}

vrpiVolumeARN :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpiVolumeARN = lens _vrpiVolumeARN (\s a -> s { _vrpiVolumeARN = a })
{-# INLINE vrpiVolumeARN #-}

vrpiVolumeSizeInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpiVolumeSizeInBytes =
    lens _vrpiVolumeSizeInBytes (\s a -> s { _vrpiVolumeSizeInBytes = a })
{-# INLINE vrpiVolumeSizeInBytes #-}

vrpiVolumeUsageInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpiVolumeUsageInBytes =
    lens _vrpiVolumeUsageInBytes (\s a -> s { _vrpiVolumeUsageInBytes = a })
{-# INLINE vrpiVolumeUsageInBytes #-}

vrpiVolumeRecoveryPointTime :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpiVolumeRecoveryPointTime =
    lens _vrpiVolumeRecoveryPointTime
         (\s a -> s { _vrpiVolumeRecoveryPointTime = a })
{-# INLINE vrpiVolumeRecoveryPointTime #-}

instance FromJSON VolumeRecoveryPointInfo

-- | Lists iSCSI information about a volume.
data VolumeiSCSIAttributes = VolumeiSCSIAttributes
    { _vscsiaTargetARN :: Maybe Text
    , _vscsiaNetworkInterfaceId :: Maybe Text
    , _vscsiaNetworkInterfacePort :: Maybe Integer
    , _vscsiaLunNumber :: Maybe Integer
    , _vscsiaChapEnabled :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeiSCSIAttributes' data type to populate a request.
mkVolumeiSCSIAttributes :: VolumeiSCSIAttributes
mkVolumeiSCSIAttributes = VolumeiSCSIAttributes
    { _vscsiaTargetARN = Nothing
    , _vscsiaNetworkInterfaceId = Nothing
    , _vscsiaNetworkInterfacePort = Nothing
    , _vscsiaLunNumber = Nothing
    , _vscsiaChapEnabled = Nothing
    }
{-# INLINE mkVolumeiSCSIAttributes #-}

-- | The Amazon Resource Name (ARN) of the volume target.
vscsiaTargetARN :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaTargetARN = lens _vscsiaTargetARN (\s a -> s { _vscsiaTargetARN = a })
{-# INLINE vscsiaTargetARN #-}

-- | The network interface identifier.
vscsiaNetworkInterfaceId :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaNetworkInterfaceId =
    lens _vscsiaNetworkInterfaceId
         (\s a -> s { _vscsiaNetworkInterfaceId = a })
{-# INLINE vscsiaNetworkInterfaceId #-}

-- | The port used to communicate with iSCSI targets.
vscsiaNetworkInterfacePort :: Lens' VolumeiSCSIAttributes (Maybe Integer)
vscsiaNetworkInterfacePort =
    lens _vscsiaNetworkInterfacePort
         (\s a -> s { _vscsiaNetworkInterfacePort = a })
{-# INLINE vscsiaNetworkInterfacePort #-}

-- | The logical disk number.
vscsiaLunNumber :: Lens' VolumeiSCSIAttributes (Maybe Integer)
vscsiaLunNumber = lens _vscsiaLunNumber (\s a -> s { _vscsiaLunNumber = a })
{-# INLINE vscsiaLunNumber #-}

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
vscsiaChapEnabled :: Lens' VolumeiSCSIAttributes (Maybe Bool)
vscsiaChapEnabled =
    lens _vscsiaChapEnabled (\s a -> s { _vscsiaChapEnabled = a })
{-# INLINE vscsiaChapEnabled #-}

instance FromJSON VolumeiSCSIAttributes

instance ToJSON VolumeiSCSIAttributes
