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
    , ddnDiskId
    , ddnDiskPath
    , ddnDiskNode
    , ddnDiskSizeInBytes
    , ddnDiskAllocationType
    , ddnDiskAllocationResource

    -- * GatewayInformation
    , GatewayInformation
    , gjGatewayARN
    , gjGatewayType

    -- * NetworkInterface
    , NetworkInterface
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
    , teTapeARN
    , teTapeBarcode
    , teTapeSizeInBytes
    , teTapeStatus
    , teVTLDevice
    , teProgress

    -- * TapeArchive
    , TapeArchive
    , tbTapeARN
    , tbTapeBarcode
    , tbTapeSizeInBytes
    , tbCompletionTime
    , tbRetrievedTo
    , tbTapeStatus

    -- * TapeRecoveryPointInfo
    , TapeRecoveryPointInfo
    , trpjTapeARN
    , trpjTapeRecoveryPointTime
    , trpjTapeSizeInBytes

    -- * VTLDevice
    , VTLDevice
    , vtleVTLDeviceARN
    , vtleDeviceiSCSIAttributes

    -- * VolumeInformation
    , VolumeInformation
    , vlVolumeARN
    , vlVolumeType

    -- * VolumeRecoveryPointInfo
    , VolumeRecoveryPointInfo
    , vrpjVolumeARN
    , vrpjVolumeSizeInBytes
    , vrpjVolumeUsageInBytes
    , vrpjVolumeRecoveryPointTime

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
cscsiviVolumeARN = lens _cscsiviVolumeARN (\s a -> s { _cscsiviVolumeARN = a })
{-# INLINE cscsiviVolumeARN #-}

cscsiviVolumeId :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviVolumeId = lens _cscsiviVolumeId (\s a -> s { _cscsiviVolumeId = a })
{-# INLINE cscsiviVolumeId #-}

cscsiviVolumeType :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviVolumeType = lens _cscsiviVolumeType (\s a -> s { _cscsiviVolumeType = a })
{-# INLINE cscsiviVolumeType #-}

cscsiviVolumeStatus :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviVolumeStatus = lens _cscsiviVolumeStatus (\s a -> s { _cscsiviVolumeStatus = a })
{-# INLINE cscsiviVolumeStatus #-}

cscsiviVolumeSizeInBytes :: Lens' CachediSCSIVolumeInformation (Maybe Integer)
cscsiviVolumeSizeInBytes = lens _cscsiviVolumeSizeInBytes (\s a -> s { _cscsiviVolumeSizeInBytes = a })
{-# INLINE cscsiviVolumeSizeInBytes #-}

cscsiviVolumeProgress :: Lens' CachediSCSIVolumeInformation (Maybe Double)
cscsiviVolumeProgress = lens _cscsiviVolumeProgress (\s a -> s { _cscsiviVolumeProgress = a })
{-# INLINE cscsiviVolumeProgress #-}

cscsiviSourceSnapshotId :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviSourceSnapshotId = lens _cscsiviSourceSnapshotId (\s a -> s { _cscsiviSourceSnapshotId = a })
{-# INLINE cscsiviSourceSnapshotId #-}

-- | Lists iSCSI information about a volume.
cscsiviVolumeiSCSIAttributes :: Lens' CachediSCSIVolumeInformation (Maybe VolumeiSCSIAttributes)
cscsiviVolumeiSCSIAttributes = lens _cscsiviVolumeiSCSIAttributes (\s a -> s { _cscsiviVolumeiSCSIAttributes = a })
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
ciTargetARN = lens _ciTargetARN (\s a -> s { _ciTargetARN = a })
{-# INLINE ciTargetARN #-}

-- | The secret key that the initiator (e.g. Windows client) must provide to
-- participate in mutual CHAP with the target.
ciSecretToAuthenticateInitiator :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateInitiator = lens _ciSecretToAuthenticateInitiator (\s a -> s { _ciSecretToAuthenticateInitiator = a })
{-# INLINE ciSecretToAuthenticateInitiator #-}

-- | The iSCSI initiator that connects to the target.
ciInitiatorName :: Lens' ChapInfo (Maybe Text)
ciInitiatorName = lens _ciInitiatorName (\s a -> s { _ciInitiatorName = a })
{-# INLINE ciInitiatorName #-}

-- | The secret key that the target must provide to participate in mutual CHAP
-- with the initiator (e.g. Windows client).
ciSecretToAuthenticateTarget :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateTarget = lens _ciSecretToAuthenticateTarget (\s a -> s { _ciSecretToAuthenticateTarget = a })
{-# INLINE ciSecretToAuthenticateTarget #-}

instance FromJSON ChapInfo

data DeviceiSCSIAttributes = DeviceiSCSIAttributes
    { _dscsiaTargetARN :: Maybe Text
    , _dscsiaNetworkInterfaceId :: Maybe Text
    , _dscsiaNetworkInterfacePort :: Maybe Integer
    , _dscsiaChapEnabled :: Maybe Bool
    } deriving (Show, Generic)

dscsiaTargetARN :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaTargetARN = lens _dscsiaTargetARN (\s a -> s { _dscsiaTargetARN = a })
{-# INLINE dscsiaTargetARN #-}

dscsiaNetworkInterfaceId :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaNetworkInterfaceId = lens _dscsiaNetworkInterfaceId (\s a -> s { _dscsiaNetworkInterfaceId = a })
{-# INLINE dscsiaNetworkInterfaceId #-}

dscsiaNetworkInterfacePort :: Lens' DeviceiSCSIAttributes (Maybe Integer)
dscsiaNetworkInterfacePort = lens _dscsiaNetworkInterfacePort (\s a -> s { _dscsiaNetworkInterfacePort = a })
{-# INLINE dscsiaNetworkInterfacePort #-}

dscsiaChapEnabled :: Lens' DeviceiSCSIAttributes (Maybe Bool)
dscsiaChapEnabled = lens _dscsiaChapEnabled (\s a -> s { _dscsiaChapEnabled = a })
{-# INLINE dscsiaChapEnabled #-}

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
ddnDiskId = lens _ddnDiskId (\s a -> s { _ddnDiskId = a })
{-# INLINE ddnDiskId #-}

ddnDiskPath :: Lens' DiskInformation (Maybe Text)
ddnDiskPath = lens _ddnDiskPath (\s a -> s { _ddnDiskPath = a })
{-# INLINE ddnDiskPath #-}

ddnDiskNode :: Lens' DiskInformation (Maybe Text)
ddnDiskNode = lens _ddnDiskNode (\s a -> s { _ddnDiskNode = a })
{-# INLINE ddnDiskNode #-}

ddnDiskSizeInBytes :: Lens' DiskInformation (Maybe Integer)
ddnDiskSizeInBytes = lens _ddnDiskSizeInBytes (\s a -> s { _ddnDiskSizeInBytes = a })
{-# INLINE ddnDiskSizeInBytes #-}

ddnDiskAllocationType :: Lens' DiskInformation (Maybe Text)
ddnDiskAllocationType = lens _ddnDiskAllocationType (\s a -> s { _ddnDiskAllocationType = a })
{-# INLINE ddnDiskAllocationType #-}

ddnDiskAllocationResource :: Lens' DiskInformation (Maybe Text)
ddnDiskAllocationResource = lens _ddnDiskAllocationResource (\s a -> s { _ddnDiskAllocationResource = a })
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
gjGatewayARN = lens _gjGatewayARN (\s a -> s { _gjGatewayARN = a })
{-# INLINE gjGatewayARN #-}

gjGatewayType :: Lens' GatewayInformation (Maybe Text)
gjGatewayType = lens _gjGatewayType (\s a -> s { _gjGatewayType = a })
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
      -- ^ Additional information about the error.
    , _sgeErrorDetails :: Map Text Text
      -- ^ Human-readable text that provides detail about the error that
      -- occured.
    } deriving (Show, Generic)

-- | Additional information about the error.
sgeErrorCode :: Lens' StorageGatewayError (Maybe ErrorCode)
sgeErrorCode = lens _sgeErrorCode (\s a -> s { _sgeErrorCode = a })
{-# INLINE sgeErrorCode #-}

-- | Human-readable text that provides detail about the error that occured.
sgeErrorDetails :: Lens' StorageGatewayError (Map Text Text)
sgeErrorDetails = lens _sgeErrorDetails (\s a -> s { _sgeErrorDetails = a })
{-# INLINE sgeErrorDetails #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StorageGatewayError' data type to populate a request.
mkStorageGatewayError :: StorageGatewayError
mkStorageGatewayError = StorageGatewayError
    { _sgeErrorCode = Nothing
    , _sgeErrorDetails = mempty
    }
{-# INLINE mkStorageGatewayError #-}

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
sscsiviVolumeARN = lens _sscsiviVolumeARN (\s a -> s { _sscsiviVolumeARN = a })
{-# INLINE sscsiviVolumeARN #-}

sscsiviVolumeId :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeId = lens _sscsiviVolumeId (\s a -> s { _sscsiviVolumeId = a })
{-# INLINE sscsiviVolumeId #-}

sscsiviVolumeType :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeType = lens _sscsiviVolumeType (\s a -> s { _sscsiviVolumeType = a })
{-# INLINE sscsiviVolumeType #-}

sscsiviVolumeStatus :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeStatus = lens _sscsiviVolumeStatus (\s a -> s { _sscsiviVolumeStatus = a })
{-# INLINE sscsiviVolumeStatus #-}

sscsiviVolumeSizeInBytes :: Lens' StorediSCSIVolumeInformation (Maybe Integer)
sscsiviVolumeSizeInBytes = lens _sscsiviVolumeSizeInBytes (\s a -> s { _sscsiviVolumeSizeInBytes = a })
{-# INLINE sscsiviVolumeSizeInBytes #-}

sscsiviVolumeProgress :: Lens' StorediSCSIVolumeInformation (Maybe Double)
sscsiviVolumeProgress = lens _sscsiviVolumeProgress (\s a -> s { _sscsiviVolumeProgress = a })
{-# INLINE sscsiviVolumeProgress #-}

sscsiviVolumeDiskId :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeDiskId = lens _sscsiviVolumeDiskId (\s a -> s { _sscsiviVolumeDiskId = a })
{-# INLINE sscsiviVolumeDiskId #-}

sscsiviSourceSnapshotId :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviSourceSnapshotId = lens _sscsiviSourceSnapshotId (\s a -> s { _sscsiviSourceSnapshotId = a })
{-# INLINE sscsiviSourceSnapshotId #-}

sscsiviPreservedExistingData :: Lens' StorediSCSIVolumeInformation (Maybe Bool)
sscsiviPreservedExistingData = lens _sscsiviPreservedExistingData (\s a -> s { _sscsiviPreservedExistingData = a })
{-# INLINE sscsiviPreservedExistingData #-}

-- | Lists iSCSI information about a volume.
sscsiviVolumeiSCSIAttributes :: Lens' StorediSCSIVolumeInformation (Maybe VolumeiSCSIAttributes)
sscsiviVolumeiSCSIAttributes = lens _sscsiviVolumeiSCSIAttributes (\s a -> s { _sscsiviVolumeiSCSIAttributes = a })
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
teTapeARN = lens _teTapeARN (\s a -> s { _teTapeARN = a })
{-# INLINE teTapeARN #-}

teTapeBarcode :: Lens' Tape (Maybe Text)
teTapeBarcode = lens _teTapeBarcode (\s a -> s { _teTapeBarcode = a })
{-# INLINE teTapeBarcode #-}

teTapeSizeInBytes :: Lens' Tape (Maybe Integer)
teTapeSizeInBytes = lens _teTapeSizeInBytes (\s a -> s { _teTapeSizeInBytes = a })
{-# INLINE teTapeSizeInBytes #-}

teTapeStatus :: Lens' Tape (Maybe Text)
teTapeStatus = lens _teTapeStatus (\s a -> s { _teTapeStatus = a })
{-# INLINE teTapeStatus #-}

teVTLDevice :: Lens' Tape (Maybe Text)
teVTLDevice = lens _teVTLDevice (\s a -> s { _teVTLDevice = a })
{-# INLINE teVTLDevice #-}

teProgress :: Lens' Tape (Maybe Double)
teProgress = lens _teProgress (\s a -> s { _teProgress = a })
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
tbTapeARN = lens _tbTapeARN (\s a -> s { _tbTapeARN = a })
{-# INLINE tbTapeARN #-}

tbTapeBarcode :: Lens' TapeArchive (Maybe Text)
tbTapeBarcode = lens _tbTapeBarcode (\s a -> s { _tbTapeBarcode = a })
{-# INLINE tbTapeBarcode #-}

tbTapeSizeInBytes :: Lens' TapeArchive (Maybe Integer)
tbTapeSizeInBytes = lens _tbTapeSizeInBytes (\s a -> s { _tbTapeSizeInBytes = a })
{-# INLINE tbTapeSizeInBytes #-}

tbCompletionTime :: Lens' TapeArchive (Maybe ISO8601)
tbCompletionTime = lens _tbCompletionTime (\s a -> s { _tbCompletionTime = a })
{-# INLINE tbCompletionTime #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
tbRetrievedTo :: Lens' TapeArchive (Maybe Text)
tbRetrievedTo = lens _tbRetrievedTo (\s a -> s { _tbRetrievedTo = a })
{-# INLINE tbRetrievedTo #-}

tbTapeStatus :: Lens' TapeArchive (Maybe Text)
tbTapeStatus = lens _tbTapeStatus (\s a -> s { _tbTapeStatus = a })
{-# INLINE tbTapeStatus #-}

instance FromJSON TapeArchive

data TapeRecoveryPointInfo = TapeRecoveryPointInfo
    { _trpjTapeARN :: Maybe Text
    , _trpjTapeRecoveryPointTime :: Maybe ISO8601
    , _trpjTapeSizeInBytes :: Maybe Integer
    } deriving (Show, Generic)

trpjTapeARN :: Lens' TapeRecoveryPointInfo (Maybe Text)
trpjTapeARN = lens _trpjTapeARN (\s a -> s { _trpjTapeARN = a })
{-# INLINE trpjTapeARN #-}

trpjTapeRecoveryPointTime :: Lens' TapeRecoveryPointInfo (Maybe ISO8601)
trpjTapeRecoveryPointTime = lens _trpjTapeRecoveryPointTime (\s a -> s { _trpjTapeRecoveryPointTime = a })
{-# INLINE trpjTapeRecoveryPointTime #-}

trpjTapeSizeInBytes :: Lens' TapeRecoveryPointInfo (Maybe Integer)
trpjTapeSizeInBytes = lens _trpjTapeSizeInBytes (\s a -> s { _trpjTapeSizeInBytes = a })
{-# INLINE trpjTapeSizeInBytes #-}

instance FromJSON TapeRecoveryPointInfo

data VTLDevice = VTLDevice
    { _vtleVTLDeviceARN :: Maybe Text
    , _vtleDeviceiSCSIAttributes :: Maybe DeviceiSCSIAttributes
    } deriving (Show, Generic)

vtleVTLDeviceARN :: Lens' VTLDevice (Maybe Text)
vtleVTLDeviceARN = lens _vtleVTLDeviceARN (\s a -> s { _vtleVTLDeviceARN = a })
{-# INLINE vtleVTLDeviceARN #-}

vtleDeviceiSCSIAttributes :: Lens' VTLDevice (Maybe DeviceiSCSIAttributes)
vtleDeviceiSCSIAttributes = lens _vtleDeviceiSCSIAttributes (\s a -> s { _vtleDeviceiSCSIAttributes = a })
{-# INLINE vtleDeviceiSCSIAttributes #-}

instance FromJSON VTLDevice

data VolumeInformation = VolumeInformation
    { _vlVolumeARN :: Maybe Text
    , _vlVolumeType :: Maybe Text
    } deriving (Show, Generic)

vlVolumeARN :: Lens' VolumeInformation (Maybe Text)
vlVolumeARN = lens _vlVolumeARN (\s a -> s { _vlVolumeARN = a })
{-# INLINE vlVolumeARN #-}

vlVolumeType :: Lens' VolumeInformation (Maybe Text)
vlVolumeType = lens _vlVolumeType (\s a -> s { _vlVolumeType = a })
{-# INLINE vlVolumeType #-}

instance FromJSON VolumeInformation

data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo
    { _vrpjVolumeARN :: Maybe Text
    , _vrpjVolumeSizeInBytes :: Maybe Integer
    , _vrpjVolumeUsageInBytes :: Maybe Integer
    , _vrpjVolumeRecoveryPointTime :: Maybe Text
    } deriving (Show, Generic)

vrpjVolumeARN :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpjVolumeARN = lens _vrpjVolumeARN (\s a -> s { _vrpjVolumeARN = a })
{-# INLINE vrpjVolumeARN #-}

vrpjVolumeSizeInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpjVolumeSizeInBytes = lens _vrpjVolumeSizeInBytes (\s a -> s { _vrpjVolumeSizeInBytes = a })
{-# INLINE vrpjVolumeSizeInBytes #-}

vrpjVolumeUsageInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpjVolumeUsageInBytes = lens _vrpjVolumeUsageInBytes (\s a -> s { _vrpjVolumeUsageInBytes = a })
{-# INLINE vrpjVolumeUsageInBytes #-}

vrpjVolumeRecoveryPointTime :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpjVolumeRecoveryPointTime = lens _vrpjVolumeRecoveryPointTime (\s a -> s { _vrpjVolumeRecoveryPointTime = a })
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
vscsiaTargetARN = lens _vscsiaTargetARN (\s a -> s { _vscsiaTargetARN = a })
{-# INLINE vscsiaTargetARN #-}

-- | The network interface identifier.
vscsiaNetworkInterfaceId :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaNetworkInterfaceId = lens _vscsiaNetworkInterfaceId (\s a -> s { _vscsiaNetworkInterfaceId = a })
{-# INLINE vscsiaNetworkInterfaceId #-}

-- | The port used to communicate with iSCSI targets.
vscsiaNetworkInterfacePort :: Lens' VolumeiSCSIAttributes (Maybe Integer)
vscsiaNetworkInterfacePort = lens _vscsiaNetworkInterfacePort (\s a -> s { _vscsiaNetworkInterfacePort = a })
{-# INLINE vscsiaNetworkInterfacePort #-}

-- | The logical disk number.
vscsiaLunNumber :: Lens' VolumeiSCSIAttributes (Maybe Integer)
vscsiaLunNumber = lens _vscsiaLunNumber (\s a -> s { _vscsiaLunNumber = a })
{-# INLINE vscsiaLunNumber #-}

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
vscsiaChapEnabled :: Lens' VolumeiSCSIAttributes (Maybe Bool)
vscsiaChapEnabled = lens _vscsiaChapEnabled (\s a -> s { _vscsiaChapEnabled = a })
{-# INLINE vscsiaChapEnabled #-}

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

instance FromJSON VolumeiSCSIAttributes

instance ToJSON VolumeiSCSIAttributes
