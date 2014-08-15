{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

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
module Network.AWS.StorageGateway.V2013_06_30.Types where

import Control.Lens.TH (makeIso, makeLenses)
import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2013-06-30@) of the
-- @AWS Storage Gateway@ service.
data StorageGateway deriving (Typeable)

instance AWSService StorageGateway where
    type Sg StorageGateway = V4
    data Er StorageGateway
        = InternalServerError
            { _iseError :: Maybe StorageGatewayError
            , _iseMessage :: Maybe Text
            }
        | InvalidGatewayRequestException
            { _igreError :: Maybe StorageGatewayError
            , _igreMessage :: Maybe Text
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

instance ToByteString ErrorCode

instance FromJSON ErrorCode

instance ToJSON ErrorCode

data CachediSCSIVolumeInformation = CachediSCSIVolumeInformation
    { _cscsiviVolumeiSCSIAttributes :: Maybe VolumeiSCSIAttributes
      -- ^ Lists iSCSI information about a volume.
    , _cscsiviVolumeStatus :: Maybe Text
    , _cscsiviSourceSnapshotId :: Maybe Text
    , _cscsiviVolumeARN :: Maybe Text
    , _cscsiviVolumeProgress :: Maybe Double
    , _cscsiviVolumeSizeInBytes :: Maybe Integer
    , _cscsiviVolumeId :: Maybe Text
    , _cscsiviVolumeType :: Maybe Text
    } deriving (Show, Generic)

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

instance FromJSON ChapInfo

data DeviceiSCSIAttributes = DeviceiSCSIAttributes
    { _dscsiaTargetARN :: Maybe Text
    , _dscsiaChapEnabled :: Maybe Bool
    , _dscsiaNetworkInterfaceId :: Maybe Text
    , _dscsiaNetworkInterfacePort :: Maybe Integer
    } deriving (Show, Generic)

instance FromJSON DeviceiSCSIAttributes

instance ToJSON DeviceiSCSIAttributes

data DiskInformation = DiskInformation
    { _dwDiskAllocationResource :: Maybe Text
    , _dwDiskAllocationType :: Maybe Text
    , _dwDiskNode :: Maybe Text
    , _dwDiskPath :: Maybe Text
    , _dwDiskSizeInBytes :: Maybe Integer
    , _dwDiskId :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON DiskInformation

data GatewayInformation = GatewayInformation
    { _gjGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _gjGatewayType :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON GatewayInformation

-- | Describes a gateway's network interface.
data NetworkInterface = NetworkInterface
    { _niIpv6Address :: Maybe Text
      -- ^ The Internet Protocol version 6 (IPv6) address of the interface.
      -- Currently not supported.
    , _niMacAddress :: Maybe Text
      -- ^ The Media Access Control (MAC) address of the interface. This is
      -- currently unsupported and will not be returned in output.
    , _niIpv4Address :: Maybe Text
      -- ^ The Internet Protocol version 4 (IPv4) address of the interface.
    } deriving (Show, Generic)

instance FromJSON NetworkInterface

-- | A StorageGatewayError that provides more detail about the cause of the
-- error.
data StorageGatewayError = StorageGatewayError
    { _sgeErrorDetails :: Map Text Text
      -- ^ Human-readable text that provides detail about the error that
      -- occured.
    , _sgeErrorCode :: Maybe ErrorCode
      -- ^ Additional information about the error.
    } deriving (Show, Generic)

instance FromJSON StorageGatewayError

instance ToJSON StorageGatewayError

data StorediSCSIVolumeInformation = StorediSCSIVolumeInformation
    { _sscsiviVolumeiSCSIAttributes :: Maybe VolumeiSCSIAttributes
      -- ^ Lists iSCSI information about a volume.
    , _sscsiviVolumeStatus :: Maybe Text
    , _sscsiviSourceSnapshotId :: Maybe Text
    , _sscsiviPreservedExistingData :: Maybe Bool
    , _sscsiviVolumeARN :: Maybe Text
    , _sscsiviVolumeProgress :: Maybe Double
    , _sscsiviVolumeSizeInBytes :: Maybe Integer
    , _sscsiviVolumeId :: Maybe Text
    , _sscsiviVolumeDiskId :: Maybe Text
    , _sscsiviVolumeType :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON StorediSCSIVolumeInformation

data Tape = Tape
    { _vTapeBarcode :: Maybe Text
    , _vTapeStatus :: Maybe Text
    , _vTapeARN :: Maybe Text
    , _vProgress :: Maybe Double
    , _vTapeSizeInBytes :: Maybe Integer
    , _vVTLDevice :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON Tape

data TapeArchive = TapeArchive
    { _tcTapeBarcode :: Maybe Text
    , _tcTapeStatus :: Maybe Text
    , _tcTapeARN :: Maybe Text
    , _tcTapeSizeInBytes :: Maybe Integer
    , _tcCompletionTime :: Maybe ISO8601
    , _tcRetrievedTo :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

instance FromJSON TapeArchive

data TapeRecoveryPointInfo = TapeRecoveryPointInfo
    { _trpjTapeRecoveryPointTime :: Maybe ISO8601
    , _trpjTapeARN :: Maybe Text
    , _trpjTapeSizeInBytes :: Maybe Integer
    } deriving (Show, Generic)

instance FromJSON TapeRecoveryPointInfo

data VTLDevice = VTLDevice
    { _vtleDeviceiSCSIAttributes :: Maybe DeviceiSCSIAttributes
    , _vtleVTLDeviceARN :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON VTLDevice

data VolumeInformation = VolumeInformation
    { _vlVolumeARN :: Maybe Text
    , _vlVolumeType :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON VolumeInformation

data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo
    { _vrpjVolumeRecoveryPointTime :: Maybe Text
    , _vrpjVolumeARN :: Maybe Text
    , _vrpjVolumeSizeInBytes :: Maybe Integer
    , _vrpjVolumeUsageInBytes :: Maybe Integer
    } deriving (Show, Generic)

instance FromJSON VolumeRecoveryPointInfo

-- | Lists iSCSI information about a volume.
data VolumeiSCSIAttributes = VolumeiSCSIAttributes
    { _vscsiaLunNumber :: Maybe Integer
      -- ^ The logical disk number.
    , _vscsiaTargetARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the volume target.
    , _vscsiaChapEnabled :: Maybe Bool
      -- ^ Indicates whether mutual CHAP is enabled for the iSCSI target.
    , _vscsiaNetworkInterfaceId :: Maybe Text
      -- ^ The network interface identifier.
    , _vscsiaNetworkInterfacePort :: Maybe Integer
      -- ^ The port used to communicate with iSCSI targets.
    } deriving (Show, Generic)

instance FromJSON VolumeiSCSIAttributes

instance ToJSON VolumeiSCSIAttributes

-- Newtypes

-- Products
makeLenses ''CachediSCSIVolumeInformation
makeLenses ''ChapInfo
makeLenses ''DeviceiSCSIAttributes
makeLenses ''DiskInformation
makeLenses ''GatewayInformation
makeLenses ''NetworkInterface
makeLenses ''StorageGatewayError
makeLenses ''StorediSCSIVolumeInformation
makeLenses ''Tape
makeLenses ''TapeArchive
makeLenses ''TapeRecoveryPointInfo
makeLenses ''VTLDevice
makeLenses ''VolumeInformation
makeLenses ''VolumeRecoveryPointInfo
makeLenses ''VolumeiSCSIAttributes
