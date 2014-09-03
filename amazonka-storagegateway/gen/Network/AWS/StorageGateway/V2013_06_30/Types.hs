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

cscsiviVolumeARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CachediSCSIVolumeInformation
    -> f CachediSCSIVolumeInformation
cscsiviVolumeARN f x =
    (\y -> x { _cscsiviVolumeARN = y })
       <$> f (_cscsiviVolumeARN x)
{-# INLINE cscsiviVolumeARN #-}

cscsiviVolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CachediSCSIVolumeInformation
    -> f CachediSCSIVolumeInformation
cscsiviVolumeId f x =
    (\y -> x { _cscsiviVolumeId = y })
       <$> f (_cscsiviVolumeId x)
{-# INLINE cscsiviVolumeId #-}

cscsiviVolumeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CachediSCSIVolumeInformation
    -> f CachediSCSIVolumeInformation
cscsiviVolumeType f x =
    (\y -> x { _cscsiviVolumeType = y })
       <$> f (_cscsiviVolumeType x)
{-# INLINE cscsiviVolumeType #-}

cscsiviVolumeStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CachediSCSIVolumeInformation
    -> f CachediSCSIVolumeInformation
cscsiviVolumeStatus f x =
    (\y -> x { _cscsiviVolumeStatus = y })
       <$> f (_cscsiviVolumeStatus x)
{-# INLINE cscsiviVolumeStatus #-}

cscsiviVolumeSizeInBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> CachediSCSIVolumeInformation
    -> f CachediSCSIVolumeInformation
cscsiviVolumeSizeInBytes f x =
    (\y -> x { _cscsiviVolumeSizeInBytes = y })
       <$> f (_cscsiviVolumeSizeInBytes x)
{-# INLINE cscsiviVolumeSizeInBytes #-}

cscsiviVolumeProgress
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> CachediSCSIVolumeInformation
    -> f CachediSCSIVolumeInformation
cscsiviVolumeProgress f x =
    (\y -> x { _cscsiviVolumeProgress = y })
       <$> f (_cscsiviVolumeProgress x)
{-# INLINE cscsiviVolumeProgress #-}

cscsiviSourceSnapshotId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CachediSCSIVolumeInformation
    -> f CachediSCSIVolumeInformation
cscsiviSourceSnapshotId f x =
    (\y -> x { _cscsiviSourceSnapshotId = y })
       <$> f (_cscsiviSourceSnapshotId x)
{-# INLINE cscsiviSourceSnapshotId #-}

-- | Lists iSCSI information about a volume.
cscsiviVolumeiSCSIAttributes
    :: Functor f
    => (Maybe VolumeiSCSIAttributes
    -> f (Maybe VolumeiSCSIAttributes))
    -> CachediSCSIVolumeInformation
    -> f CachediSCSIVolumeInformation
cscsiviVolumeiSCSIAttributes f x =
    (\y -> x { _cscsiviVolumeiSCSIAttributes = y })
       <$> f (_cscsiviVolumeiSCSIAttributes x)
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
ciTargetARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ChapInfo
    -> f ChapInfo
ciTargetARN f x =
    (\y -> x { _ciTargetARN = y })
       <$> f (_ciTargetARN x)
{-# INLINE ciTargetARN #-}

-- | The secret key that the initiator (e.g. Windows client) must provide to
-- participate in mutual CHAP with the target.
ciSecretToAuthenticateInitiator
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ChapInfo
    -> f ChapInfo
ciSecretToAuthenticateInitiator f x =
    (\y -> x { _ciSecretToAuthenticateInitiator = y })
       <$> f (_ciSecretToAuthenticateInitiator x)
{-# INLINE ciSecretToAuthenticateInitiator #-}

-- | The iSCSI initiator that connects to the target.
ciInitiatorName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ChapInfo
    -> f ChapInfo
ciInitiatorName f x =
    (\y -> x { _ciInitiatorName = y })
       <$> f (_ciInitiatorName x)
{-# INLINE ciInitiatorName #-}

-- | The secret key that the target must provide to participate in mutual CHAP
-- with the initiator (e.g. Windows client).
ciSecretToAuthenticateTarget
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ChapInfo
    -> f ChapInfo
ciSecretToAuthenticateTarget f x =
    (\y -> x { _ciSecretToAuthenticateTarget = y })
       <$> f (_ciSecretToAuthenticateTarget x)
{-# INLINE ciSecretToAuthenticateTarget #-}

instance FromJSON ChapInfo

data DeviceiSCSIAttributes = DeviceiSCSIAttributes
    { _dscsiaTargetARN :: Maybe Text
    , _dscsiaNetworkInterfaceId :: Maybe Text
    , _dscsiaNetworkInterfacePort :: Maybe Integer
    , _dscsiaChapEnabled :: Maybe Bool
    } deriving (Show, Generic)

dscsiaTargetARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DeviceiSCSIAttributes
    -> f DeviceiSCSIAttributes
dscsiaTargetARN f x =
    (\y -> x { _dscsiaTargetARN = y })
       <$> f (_dscsiaTargetARN x)
{-# INLINE dscsiaTargetARN #-}

dscsiaNetworkInterfaceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DeviceiSCSIAttributes
    -> f DeviceiSCSIAttributes
dscsiaNetworkInterfaceId f x =
    (\y -> x { _dscsiaNetworkInterfaceId = y })
       <$> f (_dscsiaNetworkInterfaceId x)
{-# INLINE dscsiaNetworkInterfaceId #-}

dscsiaNetworkInterfacePort
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DeviceiSCSIAttributes
    -> f DeviceiSCSIAttributes
dscsiaNetworkInterfacePort f x =
    (\y -> x { _dscsiaNetworkInterfacePort = y })
       <$> f (_dscsiaNetworkInterfacePort x)
{-# INLINE dscsiaNetworkInterfacePort #-}

dscsiaChapEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DeviceiSCSIAttributes
    -> f DeviceiSCSIAttributes
dscsiaChapEnabled f x =
    (\y -> x { _dscsiaChapEnabled = y })
       <$> f (_dscsiaChapEnabled x)
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

ddnDiskId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DiskInformation
    -> f DiskInformation
ddnDiskId f x =
    (\y -> x { _ddnDiskId = y })
       <$> f (_ddnDiskId x)
{-# INLINE ddnDiskId #-}

ddnDiskPath
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DiskInformation
    -> f DiskInformation
ddnDiskPath f x =
    (\y -> x { _ddnDiskPath = y })
       <$> f (_ddnDiskPath x)
{-# INLINE ddnDiskPath #-}

ddnDiskNode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DiskInformation
    -> f DiskInformation
ddnDiskNode f x =
    (\y -> x { _ddnDiskNode = y })
       <$> f (_ddnDiskNode x)
{-# INLINE ddnDiskNode #-}

ddnDiskSizeInBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DiskInformation
    -> f DiskInformation
ddnDiskSizeInBytes f x =
    (\y -> x { _ddnDiskSizeInBytes = y })
       <$> f (_ddnDiskSizeInBytes x)
{-# INLINE ddnDiskSizeInBytes #-}

ddnDiskAllocationType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DiskInformation
    -> f DiskInformation
ddnDiskAllocationType f x =
    (\y -> x { _ddnDiskAllocationType = y })
       <$> f (_ddnDiskAllocationType x)
{-# INLINE ddnDiskAllocationType #-}

ddnDiskAllocationResource
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DiskInformation
    -> f DiskInformation
ddnDiskAllocationResource f x =
    (\y -> x { _ddnDiskAllocationResource = y })
       <$> f (_ddnDiskAllocationResource x)
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
gjGatewayARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GatewayInformation
    -> f GatewayInformation
gjGatewayARN f x =
    (\y -> x { _gjGatewayARN = y })
       <$> f (_gjGatewayARN x)
{-# INLINE gjGatewayARN #-}

gjGatewayType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GatewayInformation
    -> f GatewayInformation
gjGatewayType f x =
    (\y -> x { _gjGatewayType = y })
       <$> f (_gjGatewayType x)
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
niIpv4Address
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterface
    -> f NetworkInterface
niIpv4Address f x =
    (\y -> x { _niIpv4Address = y })
       <$> f (_niIpv4Address x)
{-# INLINE niIpv4Address #-}

-- | The Media Access Control (MAC) address of the interface. This is currently
-- unsupported and will not be returned in output.
niMacAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterface
    -> f NetworkInterface
niMacAddress f x =
    (\y -> x { _niMacAddress = y })
       <$> f (_niMacAddress x)
{-# INLINE niMacAddress #-}

-- | The Internet Protocol version 6 (IPv6) address of the interface. Currently
-- not supported.
niIpv6Address
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NetworkInterface
    -> f NetworkInterface
niIpv6Address f x =
    (\y -> x { _niIpv6Address = y })
       <$> f (_niIpv6Address x)
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
sgeErrorCode
    :: Functor f
    => (Maybe ErrorCode
    -> f (Maybe ErrorCode))
    -> StorageGatewayError
    -> f StorageGatewayError
sgeErrorCode f x =
    (\y -> x { _sgeErrorCode = y })
       <$> f (_sgeErrorCode x)
{-# INLINE sgeErrorCode #-}

-- | Human-readable text that provides detail about the error that occured.
sgeErrorDetails
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> StorageGatewayError
    -> f StorageGatewayError
sgeErrorDetails f x =
    (\y -> x { _sgeErrorDetails = y })
       <$> f (_sgeErrorDetails x)
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

sscsiviVolumeARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StorediSCSIVolumeInformation
    -> f StorediSCSIVolumeInformation
sscsiviVolumeARN f x =
    (\y -> x { _sscsiviVolumeARN = y })
       <$> f (_sscsiviVolumeARN x)
{-# INLINE sscsiviVolumeARN #-}

sscsiviVolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StorediSCSIVolumeInformation
    -> f StorediSCSIVolumeInformation
sscsiviVolumeId f x =
    (\y -> x { _sscsiviVolumeId = y })
       <$> f (_sscsiviVolumeId x)
{-# INLINE sscsiviVolumeId #-}

sscsiviVolumeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StorediSCSIVolumeInformation
    -> f StorediSCSIVolumeInformation
sscsiviVolumeType f x =
    (\y -> x { _sscsiviVolumeType = y })
       <$> f (_sscsiviVolumeType x)
{-# INLINE sscsiviVolumeType #-}

sscsiviVolumeStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StorediSCSIVolumeInformation
    -> f StorediSCSIVolumeInformation
sscsiviVolumeStatus f x =
    (\y -> x { _sscsiviVolumeStatus = y })
       <$> f (_sscsiviVolumeStatus x)
{-# INLINE sscsiviVolumeStatus #-}

sscsiviVolumeSizeInBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> StorediSCSIVolumeInformation
    -> f StorediSCSIVolumeInformation
sscsiviVolumeSizeInBytes f x =
    (\y -> x { _sscsiviVolumeSizeInBytes = y })
       <$> f (_sscsiviVolumeSizeInBytes x)
{-# INLINE sscsiviVolumeSizeInBytes #-}

sscsiviVolumeProgress
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> StorediSCSIVolumeInformation
    -> f StorediSCSIVolumeInformation
sscsiviVolumeProgress f x =
    (\y -> x { _sscsiviVolumeProgress = y })
       <$> f (_sscsiviVolumeProgress x)
{-# INLINE sscsiviVolumeProgress #-}

sscsiviVolumeDiskId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StorediSCSIVolumeInformation
    -> f StorediSCSIVolumeInformation
sscsiviVolumeDiskId f x =
    (\y -> x { _sscsiviVolumeDiskId = y })
       <$> f (_sscsiviVolumeDiskId x)
{-# INLINE sscsiviVolumeDiskId #-}

sscsiviSourceSnapshotId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> StorediSCSIVolumeInformation
    -> f StorediSCSIVolumeInformation
sscsiviSourceSnapshotId f x =
    (\y -> x { _sscsiviSourceSnapshotId = y })
       <$> f (_sscsiviSourceSnapshotId x)
{-# INLINE sscsiviSourceSnapshotId #-}

sscsiviPreservedExistingData
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> StorediSCSIVolumeInformation
    -> f StorediSCSIVolumeInformation
sscsiviPreservedExistingData f x =
    (\y -> x { _sscsiviPreservedExistingData = y })
       <$> f (_sscsiviPreservedExistingData x)
{-# INLINE sscsiviPreservedExistingData #-}

-- | Lists iSCSI information about a volume.
sscsiviVolumeiSCSIAttributes
    :: Functor f
    => (Maybe VolumeiSCSIAttributes
    -> f (Maybe VolumeiSCSIAttributes))
    -> StorediSCSIVolumeInformation
    -> f StorediSCSIVolumeInformation
sscsiviVolumeiSCSIAttributes f x =
    (\y -> x { _sscsiviVolumeiSCSIAttributes = y })
       <$> f (_sscsiviVolumeiSCSIAttributes x)
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

teTapeARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Tape
    -> f Tape
teTapeARN f x =
    (\y -> x { _teTapeARN = y })
       <$> f (_teTapeARN x)
{-# INLINE teTapeARN #-}

teTapeBarcode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Tape
    -> f Tape
teTapeBarcode f x =
    (\y -> x { _teTapeBarcode = y })
       <$> f (_teTapeBarcode x)
{-# INLINE teTapeBarcode #-}

teTapeSizeInBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Tape
    -> f Tape
teTapeSizeInBytes f x =
    (\y -> x { _teTapeSizeInBytes = y })
       <$> f (_teTapeSizeInBytes x)
{-# INLINE teTapeSizeInBytes #-}

teTapeStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Tape
    -> f Tape
teTapeStatus f x =
    (\y -> x { _teTapeStatus = y })
       <$> f (_teTapeStatus x)
{-# INLINE teTapeStatus #-}

teVTLDevice
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Tape
    -> f Tape
teVTLDevice f x =
    (\y -> x { _teVTLDevice = y })
       <$> f (_teVTLDevice x)
{-# INLINE teVTLDevice #-}

teProgress
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> Tape
    -> f Tape
teProgress f x =
    (\y -> x { _teProgress = y })
       <$> f (_teProgress x)
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

tbTapeARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TapeArchive
    -> f TapeArchive
tbTapeARN f x =
    (\y -> x { _tbTapeARN = y })
       <$> f (_tbTapeARN x)
{-# INLINE tbTapeARN #-}

tbTapeBarcode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TapeArchive
    -> f TapeArchive
tbTapeBarcode f x =
    (\y -> x { _tbTapeBarcode = y })
       <$> f (_tbTapeBarcode x)
{-# INLINE tbTapeBarcode #-}

tbTapeSizeInBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> TapeArchive
    -> f TapeArchive
tbTapeSizeInBytes f x =
    (\y -> x { _tbTapeSizeInBytes = y })
       <$> f (_tbTapeSizeInBytes x)
{-# INLINE tbTapeSizeInBytes #-}

tbCompletionTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> TapeArchive
    -> f TapeArchive
tbCompletionTime f x =
    (\y -> x { _tbCompletionTime = y })
       <$> f (_tbCompletionTime x)
{-# INLINE tbCompletionTime #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
tbRetrievedTo
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TapeArchive
    -> f TapeArchive
tbRetrievedTo f x =
    (\y -> x { _tbRetrievedTo = y })
       <$> f (_tbRetrievedTo x)
{-# INLINE tbRetrievedTo #-}

tbTapeStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TapeArchive
    -> f TapeArchive
tbTapeStatus f x =
    (\y -> x { _tbTapeStatus = y })
       <$> f (_tbTapeStatus x)
{-# INLINE tbTapeStatus #-}

instance FromJSON TapeArchive

data TapeRecoveryPointInfo = TapeRecoveryPointInfo
    { _trpjTapeARN :: Maybe Text
    , _trpjTapeRecoveryPointTime :: Maybe ISO8601
    , _trpjTapeSizeInBytes :: Maybe Integer
    } deriving (Show, Generic)

trpjTapeARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TapeRecoveryPointInfo
    -> f TapeRecoveryPointInfo
trpjTapeARN f x =
    (\y -> x { _trpjTapeARN = y })
       <$> f (_trpjTapeARN x)
{-# INLINE trpjTapeARN #-}

trpjTapeRecoveryPointTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> TapeRecoveryPointInfo
    -> f TapeRecoveryPointInfo
trpjTapeRecoveryPointTime f x =
    (\y -> x { _trpjTapeRecoveryPointTime = y })
       <$> f (_trpjTapeRecoveryPointTime x)
{-# INLINE trpjTapeRecoveryPointTime #-}

trpjTapeSizeInBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> TapeRecoveryPointInfo
    -> f TapeRecoveryPointInfo
trpjTapeSizeInBytes f x =
    (\y -> x { _trpjTapeSizeInBytes = y })
       <$> f (_trpjTapeSizeInBytes x)
{-# INLINE trpjTapeSizeInBytes #-}

instance FromJSON TapeRecoveryPointInfo

data VTLDevice = VTLDevice
    { _vtleVTLDeviceARN :: Maybe Text
    , _vtleDeviceiSCSIAttributes :: Maybe DeviceiSCSIAttributes
    } deriving (Show, Generic)

vtleVTLDeviceARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VTLDevice
    -> f VTLDevice
vtleVTLDeviceARN f x =
    (\y -> x { _vtleVTLDeviceARN = y })
       <$> f (_vtleVTLDeviceARN x)
{-# INLINE vtleVTLDeviceARN #-}

vtleDeviceiSCSIAttributes
    :: Functor f
    => (Maybe DeviceiSCSIAttributes
    -> f (Maybe DeviceiSCSIAttributes))
    -> VTLDevice
    -> f VTLDevice
vtleDeviceiSCSIAttributes f x =
    (\y -> x { _vtleDeviceiSCSIAttributes = y })
       <$> f (_vtleDeviceiSCSIAttributes x)
{-# INLINE vtleDeviceiSCSIAttributes #-}

instance FromJSON VTLDevice

data VolumeInformation = VolumeInformation
    { _vlVolumeARN :: Maybe Text
    , _vlVolumeType :: Maybe Text
    } deriving (Show, Generic)

vlVolumeARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeInformation
    -> f VolumeInformation
vlVolumeARN f x =
    (\y -> x { _vlVolumeARN = y })
       <$> f (_vlVolumeARN x)
{-# INLINE vlVolumeARN #-}

vlVolumeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeInformation
    -> f VolumeInformation
vlVolumeType f x =
    (\y -> x { _vlVolumeType = y })
       <$> f (_vlVolumeType x)
{-# INLINE vlVolumeType #-}

instance FromJSON VolumeInformation

data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo
    { _vrpjVolumeARN :: Maybe Text
    , _vrpjVolumeSizeInBytes :: Maybe Integer
    , _vrpjVolumeUsageInBytes :: Maybe Integer
    , _vrpjVolumeRecoveryPointTime :: Maybe Text
    } deriving (Show, Generic)

vrpjVolumeARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeRecoveryPointInfo
    -> f VolumeRecoveryPointInfo
vrpjVolumeARN f x =
    (\y -> x { _vrpjVolumeARN = y })
       <$> f (_vrpjVolumeARN x)
{-# INLINE vrpjVolumeARN #-}

vrpjVolumeSizeInBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> VolumeRecoveryPointInfo
    -> f VolumeRecoveryPointInfo
vrpjVolumeSizeInBytes f x =
    (\y -> x { _vrpjVolumeSizeInBytes = y })
       <$> f (_vrpjVolumeSizeInBytes x)
{-# INLINE vrpjVolumeSizeInBytes #-}

vrpjVolumeUsageInBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> VolumeRecoveryPointInfo
    -> f VolumeRecoveryPointInfo
vrpjVolumeUsageInBytes f x =
    (\y -> x { _vrpjVolumeUsageInBytes = y })
       <$> f (_vrpjVolumeUsageInBytes x)
{-# INLINE vrpjVolumeUsageInBytes #-}

vrpjVolumeRecoveryPointTime
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeRecoveryPointInfo
    -> f VolumeRecoveryPointInfo
vrpjVolumeRecoveryPointTime f x =
    (\y -> x { _vrpjVolumeRecoveryPointTime = y })
       <$> f (_vrpjVolumeRecoveryPointTime x)
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
vscsiaTargetARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeiSCSIAttributes
    -> f VolumeiSCSIAttributes
vscsiaTargetARN f x =
    (\y -> x { _vscsiaTargetARN = y })
       <$> f (_vscsiaTargetARN x)
{-# INLINE vscsiaTargetARN #-}

-- | The network interface identifier.
vscsiaNetworkInterfaceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> VolumeiSCSIAttributes
    -> f VolumeiSCSIAttributes
vscsiaNetworkInterfaceId f x =
    (\y -> x { _vscsiaNetworkInterfaceId = y })
       <$> f (_vscsiaNetworkInterfaceId x)
{-# INLINE vscsiaNetworkInterfaceId #-}

-- | The port used to communicate with iSCSI targets.
vscsiaNetworkInterfacePort
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> VolumeiSCSIAttributes
    -> f VolumeiSCSIAttributes
vscsiaNetworkInterfacePort f x =
    (\y -> x { _vscsiaNetworkInterfacePort = y })
       <$> f (_vscsiaNetworkInterfacePort x)
{-# INLINE vscsiaNetworkInterfacePort #-}

-- | The logical disk number.
vscsiaLunNumber
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> VolumeiSCSIAttributes
    -> f VolumeiSCSIAttributes
vscsiaLunNumber f x =
    (\y -> x { _vscsiaLunNumber = y })
       <$> f (_vscsiaLunNumber x)
{-# INLINE vscsiaLunNumber #-}

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
vscsiaChapEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> VolumeiSCSIAttributes
    -> f VolumeiSCSIAttributes
vscsiaChapEnabled f x =
    (\y -> x { _vscsiaChapEnabled = y })
       <$> f (_vscsiaChapEnabled x)
{-# INLINE vscsiaChapEnabled #-}

instance FromJSON VolumeiSCSIAttributes

instance ToJSON VolumeiSCSIAttributes
