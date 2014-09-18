{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.Types
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
module Network.AWS.StorageGateway.Types
    (
    -- * Service
      StorageGateway
    -- ** Errors
    , StorageGatewayError (..)
    , _InternalServerError
    , _InvalidGatewayRequestException
    , _StorageGatewayClient
    , _StorageGatewaySerializer
    , _StorageGatewayService
    -- * ErrorCode
    , ErrorCode (..)

    -- * CachediSCSIVolumeInformation
    , CachediSCSIVolumeInformation
    , cachediSCSIVolumeInformation
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
    , chapInfo
    , ciTargetARN
    , ciSecretToAuthenticateInitiator
    , ciInitiatorName
    , ciSecretToAuthenticateTarget

    -- * DeviceiSCSIAttributes
    , DeviceiSCSIAttributes
    , deviceiSCSIAttributes
    , dscsiaTargetARN
    , dscsiaNetworkInterfaceId
    , dscsiaNetworkInterfacePort
    , dscsiaChapEnabled

    -- * DiskInformation
    , DiskInformation
    , diskInformation
    , diDiskId
    , diDiskPath
    , diDiskNode
    , diDiskSizeInBytes
    , diDiskAllocationType
    , diDiskAllocationResource

    -- * GatewayInformation
    , GatewayInformation
    , gatewayInformation
    , giGatewayARN
    , giGatewayType

    -- * NetworkInterface
    , NetworkInterface
    , networkInterface
    , niIpv4Address
    , niMacAddress
    , niIpv6Address

    -- * StorageGatewayError
    , StorageGatewayError
    , storageGatewayError
    , sgeErrorCode
    , sgeErrorDetails

    -- * StorediSCSIVolumeInformation
    , StorediSCSIVolumeInformation
    , storediSCSIVolumeInformation
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
    , tape
    , tTapeARN
    , tTapeBarcode
    , tTapeSizeInBytes
    , tTapeStatus
    , tVTLDevice
    , tProgress

    -- * TapeArchive
    , TapeArchive
    , tapeArchive
    , taTapeARN
    , taTapeBarcode
    , taTapeSizeInBytes
    , taCompletionTime
    , taRetrievedTo
    , taTapeStatus

    -- * TapeRecoveryPointInfo
    , TapeRecoveryPointInfo
    , tapeRecoveryPointInfo
    , trpiTapeARN
    , trpiTapeRecoveryPointTime
    , trpiTapeSizeInBytes

    -- * VTLDevice
    , VTLDevice
    , vTLDevice
    , vtldVTLDeviceARN
    , vtldDeviceiSCSIAttributes

    -- * VolumeInformation
    , VolumeInformation
    , volumeInformation
    , viVolumeARN
    , viVolumeType

    -- * VolumeRecoveryPointInfo
    , VolumeRecoveryPointInfo
    , volumeRecoveryPointInfo
    , vrpiVolumeARN
    , vrpiVolumeSizeInBytes
    , vrpiVolumeUsageInBytes
    , vrpiVolumeRecoveryPointTime

    -- * VolumeiSCSIAttributes
    , VolumeiSCSIAttributes
    , volumeiSCSIAttributes
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
    type Er StorageGateway = StorageGatewayError

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "storagegateway"
        , _svcVersion  = "2013-06-30"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'StorageGateway' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data StorageGatewayError
      -- | An internal server error has occured during the request. See the
      -- error and message fields for more information.
    = InternalServerError
        { _iseMessage :: Maybe Text
        , _iseError :: Maybe StorageGatewayError
        }
      -- | An exception occured because an invalid gateway request was
      -- issued to the service. See the error and message fields for more
      -- information.
    | InvalidGatewayRequestException
        { _igreMessage :: Maybe Text
        , _igreError :: Maybe StorageGatewayError
        }
    | StorageGatewayClient HttpException
    | StorageGatewaySerializer String
    | StorageGatewayService String
      deriving (Show, Typeable, Generic)

instance AWSError StorageGatewayError where
    awsError = const "StorageGatewayError"

instance AWSServiceError StorageGatewayError where
    serviceError    = StorageGatewayService
    clientError     = StorageGatewayClient
    serializerError = StorageGatewaySerializer

instance Exception StorageGatewayError

-- | An internal server error has occured during the request. See the error and
-- message fields for more information.
--
-- See: 'InternalServerError'
_InternalServerError :: Prism' StorageGatewayError (Maybe Text, Maybe StorageGatewayError)
    (\(p1, p2) -> InternalServerError p1 p2)
    (\case
        InternalServerError p1 p2 -> Right (p1, p2)
        x -> Left x)

-- | An exception occured because an invalid gateway request was issued to the
-- service. See the error and message fields for more information.
--
-- See: 'InvalidGatewayRequestException'
_InvalidGatewayRequestException :: Prism' StorageGatewayError (Maybe Text, Maybe StorageGatewayError)
    (\(p1, p2) -> InvalidGatewayRequestException p1 p2)
    (\case
        InvalidGatewayRequestException p1 p2 -> Right (p1, p2)
        x -> Left x)

-- | See: 'StorageGatewayClient'
_StorageGatewayClient :: Prism' StorageGatewayError HttpException
    StorageGatewayClient
    (\case
        StorageGatewayClient p1 -> Right p1
        x -> Left x)

-- | See: 'StorageGatewaySerializer'
_StorageGatewaySerializer :: Prism' StorageGatewayError String
    StorageGatewaySerializer
    (\case
        StorageGatewaySerializer p1 -> Right p1
        x -> Left x)

-- | See: 'StorageGatewayService'
_StorageGatewayService :: Prism' StorageGatewayError String
    StorageGatewayService
    (\case
        StorageGatewayService p1 -> Right p1
        x -> Left x)

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
-- a valid 'CachediSCSIVolumeInformation' data type.
--
-- 'CachediSCSIVolumeInformation' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeARN ::@ @Maybe Text@
--
-- * @VolumeId ::@ @Maybe Text@
--
-- * @VolumeType ::@ @Maybe Text@
--
-- * @VolumeStatus ::@ @Maybe Text@
--
-- * @VolumeSizeInBytes ::@ @Maybe Integer@
--
-- * @VolumeProgress ::@ @Maybe Double@
--
-- * @SourceSnapshotId ::@ @Maybe Text@
--
-- * @VolumeiSCSIAttributes ::@ @Maybe VolumeiSCSIAttributes@
--
cachediSCSIVolumeInformation :: CachediSCSIVolumeInformation
cachediSCSIVolumeInformation = CachediSCSIVolumeInformation
    { _cscsiviVolumeARN = Nothing
    , _cscsiviVolumeId = Nothing
    , _cscsiviVolumeType = Nothing
    , _cscsiviVolumeStatus = Nothing
    , _cscsiviVolumeSizeInBytes = Nothing
    , _cscsiviVolumeProgress = Nothing
    , _cscsiviSourceSnapshotId = Nothing
    , _cscsiviVolumeiSCSIAttributes = Nothing
    }

cscsiviVolumeARN :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviVolumeARN =
    lens _cscsiviVolumeARN (\s a -> s { _cscsiviVolumeARN = a })

cscsiviVolumeId :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviVolumeId = lens _cscsiviVolumeId (\s a -> s { _cscsiviVolumeId = a })

cscsiviVolumeType :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviVolumeType =
    lens _cscsiviVolumeType (\s a -> s { _cscsiviVolumeType = a })

cscsiviVolumeStatus :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviVolumeStatus =
    lens _cscsiviVolumeStatus (\s a -> s { _cscsiviVolumeStatus = a })

cscsiviVolumeSizeInBytes :: Lens' CachediSCSIVolumeInformation (Maybe Integer)
cscsiviVolumeSizeInBytes =
    lens _cscsiviVolumeSizeInBytes
         (\s a -> s { _cscsiviVolumeSizeInBytes = a })

cscsiviVolumeProgress :: Lens' CachediSCSIVolumeInformation (Maybe Double)
cscsiviVolumeProgress =
    lens _cscsiviVolumeProgress (\s a -> s { _cscsiviVolumeProgress = a })

cscsiviSourceSnapshotId :: Lens' CachediSCSIVolumeInformation (Maybe Text)
cscsiviSourceSnapshotId =
    lens _cscsiviSourceSnapshotId
         (\s a -> s { _cscsiviSourceSnapshotId = a })

-- | Lists iSCSI information about a volume.
cscsiviVolumeiSCSIAttributes :: Lens' CachediSCSIVolumeInformation (Maybe VolumeiSCSIAttributes)
cscsiviVolumeiSCSIAttributes =
    lens _cscsiviVolumeiSCSIAttributes
         (\s a -> s { _cscsiviVolumeiSCSIAttributes = a })

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
-- a valid 'ChapInfo' data type.
--
-- 'ChapInfo' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TargetARN ::@ @Maybe Text@
--
-- * @SecretToAuthenticateInitiator ::@ @Maybe Text@
--
-- * @InitiatorName ::@ @Maybe Text@
--
-- * @SecretToAuthenticateTarget ::@ @Maybe Text@
--
chapInfo :: ChapInfo
chapInfo = ChapInfo
    { _ciTargetARN = Nothing
    , _ciSecretToAuthenticateInitiator = Nothing
    , _ciInitiatorName = Nothing
    , _ciSecretToAuthenticateTarget = Nothing
    }

-- | The Amazon Resource Name (ARN) of the volume. Valid Values: 50 to 500
-- lowercase letters, numbers, periods (.), and hyphens (-).
ciTargetARN :: Lens' ChapInfo (Maybe Text)
ciTargetARN = lens _ciTargetARN (\s a -> s { _ciTargetARN = a })

-- | The secret key that the initiator (e.g. Windows client) must provide to
-- participate in mutual CHAP with the target.
ciSecretToAuthenticateInitiator :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateInitiator =
    lens _ciSecretToAuthenticateInitiator
         (\s a -> s { _ciSecretToAuthenticateInitiator = a })

-- | The iSCSI initiator that connects to the target.
ciInitiatorName :: Lens' ChapInfo (Maybe Text)
ciInitiatorName = lens _ciInitiatorName (\s a -> s { _ciInitiatorName = a })

-- | The secret key that the target must provide to participate in mutual CHAP
-- with the initiator (e.g. Windows client).
ciSecretToAuthenticateTarget :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateTarget =
    lens _ciSecretToAuthenticateTarget
         (\s a -> s { _ciSecretToAuthenticateTarget = a })

instance FromJSON ChapInfo

data DeviceiSCSIAttributes = DeviceiSCSIAttributes
    { _dscsiaTargetARN :: Maybe Text
    , _dscsiaNetworkInterfaceId :: Maybe Text
    , _dscsiaNetworkInterfacePort :: Maybe Integer
    , _dscsiaChapEnabled :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DeviceiSCSIAttributes' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TargetARN ::@ @Maybe Text@
--
-- * @NetworkInterfaceId ::@ @Maybe Text@
--
-- * @NetworkInterfacePort ::@ @Maybe Integer@
--
-- * @ChapEnabled ::@ @Maybe Bool@
--
deviceiSCSIAttributes :: DeviceiSCSIAttributes
deviceiSCSIAttributes = DeviceiSCSIAttributes
    { _dscsiaTargetARN = Nothing
    , _dscsiaNetworkInterfaceId = Nothing
    , _dscsiaNetworkInterfacePort = Nothing
    , _dscsiaChapEnabled = Nothing
    }

dscsiaTargetARN :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaTargetARN = lens _dscsiaTargetARN (\s a -> s { _dscsiaTargetARN = a })

dscsiaNetworkInterfaceId :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaNetworkInterfaceId =
    lens _dscsiaNetworkInterfaceId
         (\s a -> s { _dscsiaNetworkInterfaceId = a })

dscsiaNetworkInterfacePort :: Lens' DeviceiSCSIAttributes (Maybe Integer)
dscsiaNetworkInterfacePort =
    lens _dscsiaNetworkInterfacePort
         (\s a -> s { _dscsiaNetworkInterfacePort = a })

dscsiaChapEnabled :: Lens' DeviceiSCSIAttributes (Maybe Bool)
dscsiaChapEnabled =
    lens _dscsiaChapEnabled (\s a -> s { _dscsiaChapEnabled = a })

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
-- a valid 'DiskInformation' data type.
--
-- 'DiskInformation' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DiskId ::@ @Maybe Text@
--
-- * @DiskPath ::@ @Maybe Text@
--
-- * @DiskNode ::@ @Maybe Text@
--
-- * @DiskSizeInBytes ::@ @Maybe Integer@
--
-- * @DiskAllocationType ::@ @Maybe Text@
--
-- * @DiskAllocationResource ::@ @Maybe Text@
--
diskInformation :: DiskInformation
diskInformation = DiskInformation
    { _diDiskId = Nothing
    , _diDiskPath = Nothing
    , _diDiskNode = Nothing
    , _diDiskSizeInBytes = Nothing
    , _diDiskAllocationType = Nothing
    , _diDiskAllocationResource = Nothing
    }

diDiskId :: Lens' DiskInformation (Maybe Text)
diDiskId = lens _diDiskId (\s a -> s { _diDiskId = a })

diDiskPath :: Lens' DiskInformation (Maybe Text)
diDiskPath = lens _diDiskPath (\s a -> s { _diDiskPath = a })

diDiskNode :: Lens' DiskInformation (Maybe Text)
diDiskNode = lens _diDiskNode (\s a -> s { _diDiskNode = a })

diDiskSizeInBytes :: Lens' DiskInformation (Maybe Integer)
diDiskSizeInBytes =
    lens _diDiskSizeInBytes (\s a -> s { _diDiskSizeInBytes = a })

diDiskAllocationType :: Lens' DiskInformation (Maybe Text)
diDiskAllocationType =
    lens _diDiskAllocationType (\s a -> s { _diDiskAllocationType = a })

diDiskAllocationResource :: Lens' DiskInformation (Maybe Text)
diDiskAllocationResource =
    lens _diDiskAllocationResource
         (\s a -> s { _diDiskAllocationResource = a })

instance FromJSON DiskInformation

data GatewayInformation = GatewayInformation
    { _giGatewayARN :: Maybe Text
    , _giGatewayType :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'GatewayInformation' data type.
--
-- 'GatewayInformation' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Maybe Text@
--
-- * @GatewayType ::@ @Maybe Text@
--
gatewayInformation :: GatewayInformation
gatewayInformation = GatewayInformation
    { _giGatewayARN = Nothing
    , _giGatewayType = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
giGatewayARN :: Lens' GatewayInformation (Maybe Text)
giGatewayARN = lens _giGatewayARN (\s a -> s { _giGatewayARN = a })

giGatewayType :: Lens' GatewayInformation (Maybe Text)
giGatewayType = lens _giGatewayType (\s a -> s { _giGatewayType = a })

instance FromJSON GatewayInformation

-- | Describes a gateway's network interface.
data NetworkInterface = NetworkInterface
    { _niIpv4Address :: Maybe Text
    , _niMacAddress :: Maybe Text
    , _niIpv6Address :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NetworkInterface' data type.
--
-- 'NetworkInterface' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Ipv4Address ::@ @Maybe Text@
--
-- * @MacAddress ::@ @Maybe Text@
--
-- * @Ipv6Address ::@ @Maybe Text@
--
networkInterface :: NetworkInterface
networkInterface = NetworkInterface
    { _niIpv4Address = Nothing
    , _niMacAddress = Nothing
    , _niIpv6Address = Nothing
    }

-- | The Internet Protocol version 4 (IPv4) address of the interface.
niIpv4Address :: Lens' NetworkInterface (Maybe Text)
niIpv4Address = lens _niIpv4Address (\s a -> s { _niIpv4Address = a })

-- | The Media Access Control (MAC) address of the interface. This is currently
-- unsupported and will not be returned in output.
niMacAddress :: Lens' NetworkInterface (Maybe Text)
niMacAddress = lens _niMacAddress (\s a -> s { _niMacAddress = a })

-- | The Internet Protocol version 6 (IPv6) address of the interface. Currently
-- not supported.
niIpv6Address :: Lens' NetworkInterface (Maybe Text)
niIpv6Address = lens _niIpv6Address (\s a -> s { _niIpv6Address = a })

instance FromJSON NetworkInterface

-- | A StorageGatewayError that provides more detail about the cause of the
-- error.
data StorageGatewayError = StorageGatewayError
    { _sgeErrorCode :: Maybe ErrorCode
    , _sgeErrorDetails :: Map Text Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StorageGatewayError' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ErrorCode ::@ @Maybe ErrorCode@
--
-- * @ErrorDetails ::@ @Map Text Text@
--
storageGatewayError :: StorageGatewayError
storageGatewayError = StorageGatewayError
    { _sgeErrorCode = Nothing
    , _sgeErrorDetails = mempty
    }

-- | Additional information about the error.
sgeErrorCode :: Lens' StorageGatewayError (Maybe ErrorCode)
sgeErrorCode = lens _sgeErrorCode (\s a -> s { _sgeErrorCode = a })

-- | Human-readable text that provides detail about the error that occured.
sgeErrorDetails :: Lens' StorageGatewayError (Map Text Text)
sgeErrorDetails = lens _sgeErrorDetails (\s a -> s { _sgeErrorDetails = a })

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
-- a valid 'StorediSCSIVolumeInformation' data type.
--
-- 'StorediSCSIVolumeInformation' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeARN ::@ @Maybe Text@
--
-- * @VolumeId ::@ @Maybe Text@
--
-- * @VolumeType ::@ @Maybe Text@
--
-- * @VolumeStatus ::@ @Maybe Text@
--
-- * @VolumeSizeInBytes ::@ @Maybe Integer@
--
-- * @VolumeProgress ::@ @Maybe Double@
--
-- * @VolumeDiskId ::@ @Maybe Text@
--
-- * @SourceSnapshotId ::@ @Maybe Text@
--
-- * @PreservedExistingData ::@ @Maybe Bool@
--
-- * @VolumeiSCSIAttributes ::@ @Maybe VolumeiSCSIAttributes@
--
storediSCSIVolumeInformation :: StorediSCSIVolumeInformation
storediSCSIVolumeInformation = StorediSCSIVolumeInformation
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

sscsiviVolumeARN :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeARN =
    lens _sscsiviVolumeARN (\s a -> s { _sscsiviVolumeARN = a })

sscsiviVolumeId :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeId = lens _sscsiviVolumeId (\s a -> s { _sscsiviVolumeId = a })

sscsiviVolumeType :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeType =
    lens _sscsiviVolumeType (\s a -> s { _sscsiviVolumeType = a })

sscsiviVolumeStatus :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeStatus =
    lens _sscsiviVolumeStatus (\s a -> s { _sscsiviVolumeStatus = a })

sscsiviVolumeSizeInBytes :: Lens' StorediSCSIVolumeInformation (Maybe Integer)
sscsiviVolumeSizeInBytes =
    lens _sscsiviVolumeSizeInBytes
         (\s a -> s { _sscsiviVolumeSizeInBytes = a })

sscsiviVolumeProgress :: Lens' StorediSCSIVolumeInformation (Maybe Double)
sscsiviVolumeProgress =
    lens _sscsiviVolumeProgress (\s a -> s { _sscsiviVolumeProgress = a })

sscsiviVolumeDiskId :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviVolumeDiskId =
    lens _sscsiviVolumeDiskId (\s a -> s { _sscsiviVolumeDiskId = a })

sscsiviSourceSnapshotId :: Lens' StorediSCSIVolumeInformation (Maybe Text)
sscsiviSourceSnapshotId =
    lens _sscsiviSourceSnapshotId
         (\s a -> s { _sscsiviSourceSnapshotId = a })

sscsiviPreservedExistingData :: Lens' StorediSCSIVolumeInformation (Maybe Bool)
sscsiviPreservedExistingData =
    lens _sscsiviPreservedExistingData
         (\s a -> s { _sscsiviPreservedExistingData = a })

-- | Lists iSCSI information about a volume.
sscsiviVolumeiSCSIAttributes :: Lens' StorediSCSIVolumeInformation (Maybe VolumeiSCSIAttributes)
sscsiviVolumeiSCSIAttributes =
    lens _sscsiviVolumeiSCSIAttributes
         (\s a -> s { _sscsiviVolumeiSCSIAttributes = a })

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
-- a valid 'Tape' data type.
--
-- 'Tape' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TapeARN ::@ @Maybe Text@
--
-- * @TapeBarcode ::@ @Maybe Text@
--
-- * @TapeSizeInBytes ::@ @Maybe Integer@
--
-- * @TapeStatus ::@ @Maybe Text@
--
-- * @VTLDevice ::@ @Maybe Text@
--
-- * @Progress ::@ @Maybe Double@
--
tape :: Tape
tape = Tape
    { _tTapeARN = Nothing
    , _tTapeBarcode = Nothing
    , _tTapeSizeInBytes = Nothing
    , _tTapeStatus = Nothing
    , _tVTLDevice = Nothing
    , _tProgress = Nothing
    }

tTapeARN :: Lens' Tape (Maybe Text)
tTapeARN = lens _tTapeARN (\s a -> s { _tTapeARN = a })

tTapeBarcode :: Lens' Tape (Maybe Text)
tTapeBarcode = lens _tTapeBarcode (\s a -> s { _tTapeBarcode = a })

tTapeSizeInBytes :: Lens' Tape (Maybe Integer)
tTapeSizeInBytes =
    lens _tTapeSizeInBytes (\s a -> s { _tTapeSizeInBytes = a })

tTapeStatus :: Lens' Tape (Maybe Text)
tTapeStatus = lens _tTapeStatus (\s a -> s { _tTapeStatus = a })

tVTLDevice :: Lens' Tape (Maybe Text)
tVTLDevice = lens _tVTLDevice (\s a -> s { _tVTLDevice = a })

tProgress :: Lens' Tape (Maybe Double)
tProgress = lens _tProgress (\s a -> s { _tProgress = a })

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
-- a valid 'TapeArchive' data type.
--
-- 'TapeArchive' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TapeARN ::@ @Maybe Text@
--
-- * @TapeBarcode ::@ @Maybe Text@
--
-- * @TapeSizeInBytes ::@ @Maybe Integer@
--
-- * @CompletionTime ::@ @Maybe ISO8601@
--
-- * @RetrievedTo ::@ @Maybe Text@
--
-- * @TapeStatus ::@ @Maybe Text@
--
tapeArchive :: TapeArchive
tapeArchive = TapeArchive
    { _taTapeARN = Nothing
    , _taTapeBarcode = Nothing
    , _taTapeSizeInBytes = Nothing
    , _taCompletionTime = Nothing
    , _taRetrievedTo = Nothing
    , _taTapeStatus = Nothing
    }

taTapeARN :: Lens' TapeArchive (Maybe Text)
taTapeARN = lens _taTapeARN (\s a -> s { _taTapeARN = a })

taTapeBarcode :: Lens' TapeArchive (Maybe Text)
taTapeBarcode = lens _taTapeBarcode (\s a -> s { _taTapeBarcode = a })

taTapeSizeInBytes :: Lens' TapeArchive (Maybe Integer)
taTapeSizeInBytes =
    lens _taTapeSizeInBytes (\s a -> s { _taTapeSizeInBytes = a })

taCompletionTime :: Lens' TapeArchive (Maybe ISO8601)
taCompletionTime =
    lens _taCompletionTime (\s a -> s { _taCompletionTime = a })

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
taRetrievedTo :: Lens' TapeArchive (Maybe Text)
taRetrievedTo = lens _taRetrievedTo (\s a -> s { _taRetrievedTo = a })

taTapeStatus :: Lens' TapeArchive (Maybe Text)
taTapeStatus = lens _taTapeStatus (\s a -> s { _taTapeStatus = a })

instance FromJSON TapeArchive

data TapeRecoveryPointInfo = TapeRecoveryPointInfo
    { _trpiTapeARN :: Maybe Text
    , _trpiTapeRecoveryPointTime :: Maybe ISO8601
    , _trpiTapeSizeInBytes :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TapeRecoveryPointInfo' data type.
--
-- 'TapeRecoveryPointInfo' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TapeARN ::@ @Maybe Text@
--
-- * @TapeRecoveryPointTime ::@ @Maybe ISO8601@
--
-- * @TapeSizeInBytes ::@ @Maybe Integer@
--
tapeRecoveryPointInfo :: TapeRecoveryPointInfo
tapeRecoveryPointInfo = TapeRecoveryPointInfo
    { _trpiTapeARN = Nothing
    , _trpiTapeRecoveryPointTime = Nothing
    , _trpiTapeSizeInBytes = Nothing
    }

trpiTapeARN :: Lens' TapeRecoveryPointInfo (Maybe Text)
trpiTapeARN = lens _trpiTapeARN (\s a -> s { _trpiTapeARN = a })

trpiTapeRecoveryPointTime :: Lens' TapeRecoveryPointInfo (Maybe ISO8601)
trpiTapeRecoveryPointTime =
    lens _trpiTapeRecoveryPointTime
         (\s a -> s { _trpiTapeRecoveryPointTime = a })

trpiTapeSizeInBytes :: Lens' TapeRecoveryPointInfo (Maybe Integer)
trpiTapeSizeInBytes =
    lens _trpiTapeSizeInBytes (\s a -> s { _trpiTapeSizeInBytes = a })

instance FromJSON TapeRecoveryPointInfo

data VTLDevice = VTLDevice
    { _vtldVTLDeviceARN :: Maybe Text
    , _vtldDeviceiSCSIAttributes :: Maybe DeviceiSCSIAttributes
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VTLDevice' data type.
--
-- 'VTLDevice' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VTLDeviceARN ::@ @Maybe Text@
--
-- * @DeviceiSCSIAttributes ::@ @Maybe DeviceiSCSIAttributes@
--
vTLDevice :: VTLDevice
vTLDevice = VTLDevice
    { _vtldVTLDeviceARN = Nothing
    , _vtldDeviceiSCSIAttributes = Nothing
    }

vtldVTLDeviceARN :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceARN =
    lens _vtldVTLDeviceARN (\s a -> s { _vtldVTLDeviceARN = a })

vtldDeviceiSCSIAttributes :: Lens' VTLDevice (Maybe DeviceiSCSIAttributes)
vtldDeviceiSCSIAttributes =
    lens _vtldDeviceiSCSIAttributes
         (\s a -> s { _vtldDeviceiSCSIAttributes = a })

instance FromJSON VTLDevice

data VolumeInformation = VolumeInformation
    { _viVolumeARN :: Maybe Text
    , _viVolumeType :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeInformation' data type.
--
-- 'VolumeInformation' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeARN ::@ @Maybe Text@
--
-- * @VolumeType ::@ @Maybe Text@
--
volumeInformation :: VolumeInformation
volumeInformation = VolumeInformation
    { _viVolumeARN = Nothing
    , _viVolumeType = Nothing
    }

viVolumeARN :: Lens' VolumeInformation (Maybe Text)
viVolumeARN = lens _viVolumeARN (\s a -> s { _viVolumeARN = a })

viVolumeType :: Lens' VolumeInformation (Maybe Text)
viVolumeType = lens _viVolumeType (\s a -> s { _viVolumeType = a })

instance FromJSON VolumeInformation

data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo
    { _vrpiVolumeARN :: Maybe Text
    , _vrpiVolumeSizeInBytes :: Maybe Integer
    , _vrpiVolumeUsageInBytes :: Maybe Integer
    , _vrpiVolumeRecoveryPointTime :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'VolumeRecoveryPointInfo' data type.
--
-- 'VolumeRecoveryPointInfo' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeARN ::@ @Maybe Text@
--
-- * @VolumeSizeInBytes ::@ @Maybe Integer@
--
-- * @VolumeUsageInBytes ::@ @Maybe Integer@
--
-- * @VolumeRecoveryPointTime ::@ @Maybe Text@
--
volumeRecoveryPointInfo :: VolumeRecoveryPointInfo
volumeRecoveryPointInfo = VolumeRecoveryPointInfo
    { _vrpiVolumeARN = Nothing
    , _vrpiVolumeSizeInBytes = Nothing
    , _vrpiVolumeUsageInBytes = Nothing
    , _vrpiVolumeRecoveryPointTime = Nothing
    }

vrpiVolumeARN :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpiVolumeARN = lens _vrpiVolumeARN (\s a -> s { _vrpiVolumeARN = a })

vrpiVolumeSizeInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpiVolumeSizeInBytes =
    lens _vrpiVolumeSizeInBytes (\s a -> s { _vrpiVolumeSizeInBytes = a })

vrpiVolumeUsageInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpiVolumeUsageInBytes =
    lens _vrpiVolumeUsageInBytes (\s a -> s { _vrpiVolumeUsageInBytes = a })

vrpiVolumeRecoveryPointTime :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpiVolumeRecoveryPointTime =
    lens _vrpiVolumeRecoveryPointTime
         (\s a -> s { _vrpiVolumeRecoveryPointTime = a })

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
--
-- The fields accessible through corresponding lenses are:
--
-- * @TargetARN ::@ @Maybe Text@
--
-- * @NetworkInterfaceId ::@ @Maybe Text@
--
-- * @NetworkInterfacePort ::@ @Maybe Integer@
--
-- * @LunNumber ::@ @Maybe Integer@
--
-- * @ChapEnabled ::@ @Maybe Bool@
--
volumeiSCSIAttributes :: VolumeiSCSIAttributes
volumeiSCSIAttributes = VolumeiSCSIAttributes
    { _vscsiaTargetARN = Nothing
    , _vscsiaNetworkInterfaceId = Nothing
    , _vscsiaNetworkInterfacePort = Nothing
    , _vscsiaLunNumber = Nothing
    , _vscsiaChapEnabled = Nothing
    }

-- | The Amazon Resource Name (ARN) of the volume target.
vscsiaTargetARN :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaTargetARN = lens _vscsiaTargetARN (\s a -> s { _vscsiaTargetARN = a })

-- | The network interface identifier.
vscsiaNetworkInterfaceId :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaNetworkInterfaceId =
    lens _vscsiaNetworkInterfaceId
         (\s a -> s { _vscsiaNetworkInterfaceId = a })

-- | The port used to communicate with iSCSI targets.
vscsiaNetworkInterfacePort :: Lens' VolumeiSCSIAttributes (Maybe Integer)
vscsiaNetworkInterfacePort =
    lens _vscsiaNetworkInterfacePort
         (\s a -> s { _vscsiaNetworkInterfacePort = a })

-- | The logical disk number.
vscsiaLunNumber :: Lens' VolumeiSCSIAttributes (Maybe Integer)
vscsiaLunNumber = lens _vscsiaLunNumber (\s a -> s { _vscsiaLunNumber = a })

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
vscsiaChapEnabled :: Lens' VolumeiSCSIAttributes (Maybe Bool)
vscsiaChapEnabled =
    lens _vscsiaChapEnabled (\s a -> s { _vscsiaChapEnabled = a })

instance FromJSON VolumeiSCSIAttributes

instance ToJSON VolumeiSCSIAttributes
