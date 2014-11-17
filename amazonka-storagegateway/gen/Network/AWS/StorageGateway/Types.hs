{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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

module Network.AWS.StorageGateway.Types
    (
    -- * Service
      StorageGateway
    -- ** Error
    , JSONError
    -- ** JSON
    , jsonOptions

    -- * ChapInfo
    , ChapInfo
    , chapInfo
    , ciInitiatorName
    , ciSecretToAuthenticateInitiator
    , ciSecretToAuthenticateTarget
    , ciTargetARN

    -- * VolumeiSCSIAttributes
    , VolumeiSCSIAttributes
    , volumeiSCSIAttributes
    , vscsiaChapEnabled
    , vscsiaLunNumber
    , vscsiaNetworkInterfaceId
    , vscsiaNetworkInterfacePort
    , vscsiaTargetARN

    -- * DeviceiSCSIAttributes
    , DeviceiSCSIAttributes
    , deviceiSCSIAttributes
    , dscsiaChapEnabled
    , dscsiaNetworkInterfaceId
    , dscsiaNetworkInterfacePort
    , dscsiaTargetARN

    -- * Error'
    , Error'
    , error
    , eErrorCode
    , eErrorDetails

    -- * Disk
    , Disk
    , disk
    , dDiskAllocationResource
    , dDiskAllocationType
    , dDiskId
    , dDiskNode
    , dDiskPath
    , dDiskSizeInBytes

    -- * Tape
    , Tape
    , tape
    , tProgress
    , tTapeARN
    , tTapeBarcode
    , tTapeSizeInBytes
    , tTapeStatus
    , tVTLDevice

    -- * NetworkInterface
    , NetworkInterface
    , networkInterface
    , niIpv4Address
    , niIpv6Address
    , niMacAddress

    -- * VTLDevice
    , VTLDevice
    , vtldevice
    , vtldDeviceiSCSIAttributes
    , vtldVTLDeviceARN
    , vtldVTLDeviceProductIdentifier
    , vtldVTLDeviceType
    , vtldVTLDeviceVendor

    -- * TapeRecoveryPointInfo
    , TapeRecoveryPointInfo
    , tapeRecoveryPointInfo
    , trpiTapeARN
    , trpiTapeRecoveryPointTime
    , trpiTapeSizeInBytes
    , trpiTapeStatus

    -- * VolumeRecoveryPointInfo
    , VolumeRecoveryPointInfo
    , volumeRecoveryPointInfo
    , vrpiVolumeARN
    , vrpiVolumeRecoveryPointTime
    , vrpiVolumeSizeInBytes
    , vrpiVolumeUsageInBytes

    -- * TapeArchive
    , TapeArchive
    , tapeArchive
    , taCompletionTime
    , taRetrievedTo
    , taTapeARN
    , taTapeBarcode
    , taTapeSizeInBytes
    , taTapeStatus

    -- * ErrorCode
    , ErrorCode (..)

    -- * StorediSCSIVolume
    , StorediSCSIVolume
    , storediSCSIVolume
    , sscsivPreservedExistingData
    , sscsivSourceSnapshotId
    , sscsivVolumeARN
    , sscsivVolumeDiskId
    , sscsivVolumeId
    , sscsivVolumeProgress
    , sscsivVolumeSizeInBytes
    , sscsivVolumeStatus
    , sscsivVolumeType
    , sscsivVolumeiSCSIAttributes

    -- * CachediSCSIVolume
    , CachediSCSIVolume
    , cachediSCSIVolume
    , cscsivSourceSnapshotId
    , cscsivVolumeARN
    , cscsivVolumeId
    , cscsivVolumeProgress
    , cscsivVolumeSizeInBytes
    , cscsivVolumeStatus
    , cscsivVolumeType
    , cscsivVolumeiSCSIAttributes

    -- * VolumeInfo
    , VolumeInfo
    , volumeInfo
    , viVolumeARN
    , viVolumeType

    -- * GatewayInfo
    , GatewayInfo
    , gatewayInfo
    , giGatewayARN
    , giGatewayOperationalState
    , giGatewayType
    ) where

import Data.Char (isUpper)
import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4
import qualified GHC.Exts

-- | Supported version (@2013-06-30@) of the Amazon Storage Gateway.
data StorageGateway deriving (Typeable)

instance AWSService StorageGateway where
    type Sg StorageGateway = V4
    type Er StorageGateway = JSONError

    service = Service
        { _svcEndpoint = regional
        , _svcAbbrev   = "StorageGateway"
        , _svcPrefix   = "storagegateway"
        , _svcVersion  = "2013-06-30"
        , _svcTarget   = Nothing
        }

    handle = jsonError alwaysFail

jsonOptions :: AesonOptions
jsonOptions = defaultOptions
    { fieldLabelModifier = dropWhile (not . isUpper)
    }

data ChapInfo = ChapInfo
    { _ciInitiatorName                 :: Maybe Text
    , _ciSecretToAuthenticateInitiator :: Maybe Text
    , _ciSecretToAuthenticateTarget    :: Maybe Text
    , _ciTargetARN                     :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ChapInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciInitiatorName' @::@ 'Maybe' 'Text'
--
-- * 'ciSecretToAuthenticateInitiator' @::@ 'Maybe' 'Text'
--
-- * 'ciSecretToAuthenticateTarget' @::@ 'Maybe' 'Text'
--
-- * 'ciTargetARN' @::@ 'Maybe' 'Text'
--
chapInfo :: ChapInfo
chapInfo = ChapInfo
    { _ciTargetARN                     = Nothing
    , _ciSecretToAuthenticateInitiator = Nothing
    , _ciInitiatorName                 = Nothing
    , _ciSecretToAuthenticateTarget    = Nothing
    }

-- | The iSCSI initiator that connects to the target.
ciInitiatorName :: Lens' ChapInfo (Maybe Text)
ciInitiatorName = lens _ciInitiatorName (\s a -> s { _ciInitiatorName = a })

-- | The secret key that the initiator (e.g. Windows client) must provide to
-- participate in mutual CHAP with the target.
ciSecretToAuthenticateInitiator :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateInitiator =
    lens _ciSecretToAuthenticateInitiator
        (\s a -> s { _ciSecretToAuthenticateInitiator = a })

-- | The secret key that the target must provide to participate in mutual CHAP
-- with the initiator (e.g. Windows client).
ciSecretToAuthenticateTarget :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateTarget =
    lens _ciSecretToAuthenticateTarget
        (\s a -> s { _ciSecretToAuthenticateTarget = a })

-- | The Amazon Resource Name (ARN) of the volume. Valid Values: 50 to 500
-- lowercase letters, numbers, periods (.), and hyphens (-).
ciTargetARN :: Lens' ChapInfo (Maybe Text)
ciTargetARN = lens _ciTargetARN (\s a -> s { _ciTargetARN = a })

instance FromJSON ChapInfo where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON ChapInfo where
    toJSON = genericToJSON jsonOptions

data VolumeiSCSIAttributes = VolumeiSCSIAttributes
    { _vscsiaChapEnabled          :: Maybe Bool
    , _vscsiaLunNumber            :: Maybe Nat
    , _vscsiaNetworkInterfaceId   :: Maybe Text
    , _vscsiaNetworkInterfacePort :: Maybe Int
    , _vscsiaTargetARN            :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'VolumeiSCSIAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vscsiaChapEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'vscsiaLunNumber' @::@ 'Maybe' 'Natural'
--
-- * 'vscsiaNetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'vscsiaNetworkInterfacePort' @::@ 'Maybe' 'Int'
--
-- * 'vscsiaTargetARN' @::@ 'Maybe' 'Text'
--
volumeiSCSIAttributes :: VolumeiSCSIAttributes
volumeiSCSIAttributes = VolumeiSCSIAttributes
    { _vscsiaTargetARN            = Nothing
    , _vscsiaNetworkInterfaceId   = Nothing
    , _vscsiaNetworkInterfacePort = Nothing
    , _vscsiaLunNumber            = Nothing
    , _vscsiaChapEnabled          = Nothing
    }

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
vscsiaChapEnabled :: Lens' VolumeiSCSIAttributes (Maybe Bool)
vscsiaChapEnabled =
    lens _vscsiaChapEnabled (\s a -> s { _vscsiaChapEnabled = a })

-- | The logical disk number.
vscsiaLunNumber :: Lens' VolumeiSCSIAttributes (Maybe Natural)
vscsiaLunNumber = lens _vscsiaLunNumber (\s a -> s { _vscsiaLunNumber = a })
    . mapping _Nat

-- | The network interface identifier.
vscsiaNetworkInterfaceId :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaNetworkInterfaceId =
    lens _vscsiaNetworkInterfaceId
        (\s a -> s { _vscsiaNetworkInterfaceId = a })

-- | The port used to communicate with iSCSI targets.
vscsiaNetworkInterfacePort :: Lens' VolumeiSCSIAttributes (Maybe Int)
vscsiaNetworkInterfacePort =
    lens _vscsiaNetworkInterfacePort
        (\s a -> s { _vscsiaNetworkInterfacePort = a })

-- | The Amazon Resource Name (ARN) of the volume target.
vscsiaTargetARN :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaTargetARN = lens _vscsiaTargetARN (\s a -> s { _vscsiaTargetARN = a })

instance FromJSON VolumeiSCSIAttributes where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON VolumeiSCSIAttributes where
    toJSON = genericToJSON jsonOptions

data DeviceiSCSIAttributes = DeviceiSCSIAttributes
    { _dscsiaChapEnabled          :: Maybe Bool
    , _dscsiaNetworkInterfaceId   :: Maybe Text
    , _dscsiaNetworkInterfacePort :: Maybe Int
    , _dscsiaTargetARN            :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeviceiSCSIAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscsiaChapEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'dscsiaNetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'dscsiaNetworkInterfacePort' @::@ 'Maybe' 'Int'
--
-- * 'dscsiaTargetARN' @::@ 'Maybe' 'Text'
--
deviceiSCSIAttributes :: DeviceiSCSIAttributes
deviceiSCSIAttributes = DeviceiSCSIAttributes
    { _dscsiaTargetARN            = Nothing
    , _dscsiaNetworkInterfaceId   = Nothing
    , _dscsiaNetworkInterfacePort = Nothing
    , _dscsiaChapEnabled          = Nothing
    }

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
dscsiaChapEnabled :: Lens' DeviceiSCSIAttributes (Maybe Bool)
dscsiaChapEnabled =
    lens _dscsiaChapEnabled (\s a -> s { _dscsiaChapEnabled = a })

-- | The network interface identifier of the VTL device.
dscsiaNetworkInterfaceId :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaNetworkInterfaceId =
    lens _dscsiaNetworkInterfaceId
        (\s a -> s { _dscsiaNetworkInterfaceId = a })

-- | The port used to communicate with iSCSI VTL device targets.
dscsiaNetworkInterfacePort :: Lens' DeviceiSCSIAttributes (Maybe Int)
dscsiaNetworkInterfacePort =
    lens _dscsiaNetworkInterfacePort
        (\s a -> s { _dscsiaNetworkInterfacePort = a })

-- | Specifies the unique Amazon Resource Name(ARN) that encodes the iSCSI
-- qualified name(iqn) of a tape drive or media changer target.
dscsiaTargetARN :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaTargetARN = lens _dscsiaTargetARN (\s a -> s { _dscsiaTargetARN = a })

instance FromJSON DeviceiSCSIAttributes where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON DeviceiSCSIAttributes where
    toJSON = genericToJSON jsonOptions

data Error' = Error'
    { _eErrorCode    :: Maybe Text
    , _eErrorDetails :: Map Text Text
    } deriving (Eq, Show, Generic)

-- | 'Error'' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eErrorCode' @::@ 'Maybe' 'Text'
--
-- * 'eErrorDetails' @::@ 'HashMap' 'Text' 'Text'
--
error :: Error'
error = Error'
    { _eErrorCode    = Nothing
    , _eErrorDetails = mempty
    }

-- | Additional information about the error.
eErrorCode :: Lens' Error' (Maybe Text)
eErrorCode = lens _eErrorCode (\s a -> s { _eErrorCode = a })

-- | Human-readable text that provides detail about the error that occurred.
eErrorDetails :: Lens' Error' (HashMap Text Text)
eErrorDetails = lens _eErrorDetails (\s a -> s { _eErrorDetails = a })
    . _Map

instance FromJSON Error' where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON Error' where
    toJSON = genericToJSON jsonOptions

data Disk = Disk
    { _dDiskAllocationResource :: Maybe Text
    , _dDiskAllocationType     :: Maybe Text
    , _dDiskId                 :: Maybe Text
    , _dDiskNode               :: Maybe Text
    , _dDiskPath               :: Maybe Text
    , _dDiskSizeInBytes        :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | 'Disk' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dDiskAllocationResource' @::@ 'Maybe' 'Text'
--
-- * 'dDiskAllocationType' @::@ 'Maybe' 'Text'
--
-- * 'dDiskId' @::@ 'Maybe' 'Text'
--
-- * 'dDiskNode' @::@ 'Maybe' 'Text'
--
-- * 'dDiskPath' @::@ 'Maybe' 'Text'
--
-- * 'dDiskSizeInBytes' @::@ 'Maybe' 'Integer'
--
disk :: Disk
disk = Disk
    { _dDiskId                 = Nothing
    , _dDiskPath               = Nothing
    , _dDiskNode               = Nothing
    , _dDiskSizeInBytes        = Nothing
    , _dDiskAllocationType     = Nothing
    , _dDiskAllocationResource = Nothing
    }

dDiskAllocationResource :: Lens' Disk (Maybe Text)
dDiskAllocationResource =
    lens _dDiskAllocationResource (\s a -> s { _dDiskAllocationResource = a })

dDiskAllocationType :: Lens' Disk (Maybe Text)
dDiskAllocationType =
    lens _dDiskAllocationType (\s a -> s { _dDiskAllocationType = a })

dDiskId :: Lens' Disk (Maybe Text)
dDiskId = lens _dDiskId (\s a -> s { _dDiskId = a })

dDiskNode :: Lens' Disk (Maybe Text)
dDiskNode = lens _dDiskNode (\s a -> s { _dDiskNode = a })

dDiskPath :: Lens' Disk (Maybe Text)
dDiskPath = lens _dDiskPath (\s a -> s { _dDiskPath = a })

dDiskSizeInBytes :: Lens' Disk (Maybe Integer)
dDiskSizeInBytes = lens _dDiskSizeInBytes (\s a -> s { _dDiskSizeInBytes = a })

instance FromJSON Disk where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON Disk where
    toJSON = genericToJSON jsonOptions

data Tape = Tape
    { _tProgress        :: Maybe Double
    , _tTapeARN         :: Maybe Text
    , _tTapeBarcode     :: Maybe Text
    , _tTapeSizeInBytes :: Maybe Nat
    , _tTapeStatus      :: Maybe Text
    , _tVTLDevice       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Tape' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tProgress' @::@ 'Maybe' 'Double'
--
-- * 'tTapeARN' @::@ 'Maybe' 'Text'
--
-- * 'tTapeBarcode' @::@ 'Maybe' 'Text'
--
-- * 'tTapeSizeInBytes' @::@ 'Maybe' 'Natural'
--
-- * 'tTapeStatus' @::@ 'Maybe' 'Text'
--
-- * 'tVTLDevice' @::@ 'Maybe' 'Text'
--
tape :: Tape
tape = Tape
    { _tTapeARN         = Nothing
    , _tTapeBarcode     = Nothing
    , _tTapeSizeInBytes = Nothing
    , _tTapeStatus      = Nothing
    , _tVTLDevice       = Nothing
    , _tProgress        = Nothing
    }

-- | For archiving virtual tapes, indicates how much data remains to be
-- uploaded before archiving is complete. Range: 0 (not started) to 100
-- (complete).
tProgress :: Lens' Tape (Maybe Double)
tProgress = lens _tProgress (\s a -> s { _tProgress = a })

-- | The Amazon Resource Name (ARN) of the virtual tape.
tTapeARN :: Lens' Tape (Maybe Text)
tTapeARN = lens _tTapeARN (\s a -> s { _tTapeARN = a })

-- | The barcode that identifies a specific virtual tape.
tTapeBarcode :: Lens' Tape (Maybe Text)
tTapeBarcode = lens _tTapeBarcode (\s a -> s { _tTapeBarcode = a })

-- | The size, in bytes, of the virtual tape.
tTapeSizeInBytes :: Lens' Tape (Maybe Natural)
tTapeSizeInBytes = lens _tTapeSizeInBytes (\s a -> s { _tTapeSizeInBytes = a })
    . mapping _Nat

-- | The current state of the virtual tape.
tTapeStatus :: Lens' Tape (Maybe Text)
tTapeStatus = lens _tTapeStatus (\s a -> s { _tTapeStatus = a })

-- | The virtual tape library (VTL) device that the virtual tape is associated
-- with.
tVTLDevice :: Lens' Tape (Maybe Text)
tVTLDevice = lens _tVTLDevice (\s a -> s { _tVTLDevice = a })

instance FromJSON Tape where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON Tape where
    toJSON = genericToJSON jsonOptions

data NetworkInterface = NetworkInterface
    { _niIpv4Address :: Maybe Text
    , _niIpv6Address :: Maybe Text
    , _niMacAddress  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'NetworkInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'niIpv4Address' @::@ 'Maybe' 'Text'
--
-- * 'niIpv6Address' @::@ 'Maybe' 'Text'
--
-- * 'niMacAddress' @::@ 'Maybe' 'Text'
--
networkInterface :: NetworkInterface
networkInterface = NetworkInterface
    { _niIpv4Address = Nothing
    , _niMacAddress  = Nothing
    , _niIpv6Address = Nothing
    }

-- | The Internet Protocol version 4 (IPv4) address of the interface.
niIpv4Address :: Lens' NetworkInterface (Maybe Text)
niIpv4Address = lens _niIpv4Address (\s a -> s { _niIpv4Address = a })

-- | The Internet Protocol version 6 (IPv6) address of the interface.
-- Currently not supported.
niIpv6Address :: Lens' NetworkInterface (Maybe Text)
niIpv6Address = lens _niIpv6Address (\s a -> s { _niIpv6Address = a })

-- | The Media Access Control (MAC) address of the interface.
niMacAddress :: Lens' NetworkInterface (Maybe Text)
niMacAddress = lens _niMacAddress (\s a -> s { _niMacAddress = a })

instance FromJSON NetworkInterface where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON NetworkInterface where
    toJSON = genericToJSON jsonOptions

data VTLDevice = VTLDevice
    { _vtldDeviceiSCSIAttributes      :: Maybe DeviceiSCSIAttributes
    , _vtldVTLDeviceARN               :: Maybe Text
    , _vtldVTLDeviceProductIdentifier :: Maybe Text
    , _vtldVTLDeviceType              :: Maybe Text
    , _vtldVTLDeviceVendor            :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'VTLDevice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vtldDeviceiSCSIAttributes' @::@ 'Maybe' 'DeviceiSCSIAttributes'
--
-- * 'vtldVTLDeviceARN' @::@ 'Maybe' 'Text'
--
-- * 'vtldVTLDeviceProductIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'vtldVTLDeviceType' @::@ 'Maybe' 'Text'
--
-- * 'vtldVTLDeviceVendor' @::@ 'Maybe' 'Text'
--
vtldevice :: VTLDevice
vtldevice = VTLDevice
    { _vtldVTLDeviceARN               = Nothing
    , _vtldVTLDeviceType              = Nothing
    , _vtldVTLDeviceVendor            = Nothing
    , _vtldVTLDeviceProductIdentifier = Nothing
    , _vtldDeviceiSCSIAttributes      = Nothing
    }

-- | A list of iSCSI information about a VTL device.
vtldDeviceiSCSIAttributes :: Lens' VTLDevice (Maybe DeviceiSCSIAttributes)
vtldDeviceiSCSIAttributes =
    lens _vtldDeviceiSCSIAttributes
        (\s a -> s { _vtldDeviceiSCSIAttributes = a })

-- | Specifies the unique Amazon Resource Name (ARN) of the device (tape drive
-- or media changer).
vtldVTLDeviceARN :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceARN = lens _vtldVTLDeviceARN (\s a -> s { _vtldVTLDeviceARN = a })

vtldVTLDeviceProductIdentifier :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceProductIdentifier =
    lens _vtldVTLDeviceProductIdentifier
        (\s a -> s { _vtldVTLDeviceProductIdentifier = a })

vtldVTLDeviceType :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceType =
    lens _vtldVTLDeviceType (\s a -> s { _vtldVTLDeviceType = a })

vtldVTLDeviceVendor :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceVendor =
    lens _vtldVTLDeviceVendor (\s a -> s { _vtldVTLDeviceVendor = a })

instance FromJSON VTLDevice where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON VTLDevice where
    toJSON = genericToJSON jsonOptions

data TapeRecoveryPointInfo = TapeRecoveryPointInfo
    { _trpiTapeARN               :: Maybe Text
    , _trpiTapeRecoveryPointTime :: Maybe RFC822
    , _trpiTapeSizeInBytes       :: Maybe Nat
    , _trpiTapeStatus            :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'TapeRecoveryPointInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'trpiTapeARN' @::@ 'Maybe' 'Text'
--
-- * 'trpiTapeRecoveryPointTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'trpiTapeSizeInBytes' @::@ 'Maybe' 'Natural'
--
-- * 'trpiTapeStatus' @::@ 'Maybe' 'Text'
--
tapeRecoveryPointInfo :: TapeRecoveryPointInfo
tapeRecoveryPointInfo = TapeRecoveryPointInfo
    { _trpiTapeARN               = Nothing
    , _trpiTapeRecoveryPointTime = Nothing
    , _trpiTapeSizeInBytes       = Nothing
    , _trpiTapeStatus            = Nothing
    }

-- | The Amazon Resource Name (ARN) of the virtual tape.
trpiTapeARN :: Lens' TapeRecoveryPointInfo (Maybe Text)
trpiTapeARN = lens _trpiTapeARN (\s a -> s { _trpiTapeARN = a })

-- | The time when the point-in-time view of the virtual tape was replicated
-- for later recovery. The string format of the tape recovery point time is
-- in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
trpiTapeRecoveryPointTime :: Lens' TapeRecoveryPointInfo (Maybe UTCTime)
trpiTapeRecoveryPointTime =
    lens _trpiTapeRecoveryPointTime
        (\s a -> s { _trpiTapeRecoveryPointTime = a })
            . mapping _Time

-- | The size, in bytes, of the virtual tapes to recover.
trpiTapeSizeInBytes :: Lens' TapeRecoveryPointInfo (Maybe Natural)
trpiTapeSizeInBytes =
    lens _trpiTapeSizeInBytes (\s a -> s { _trpiTapeSizeInBytes = a })
        . mapping _Nat

trpiTapeStatus :: Lens' TapeRecoveryPointInfo (Maybe Text)
trpiTapeStatus = lens _trpiTapeStatus (\s a -> s { _trpiTapeStatus = a })

instance FromJSON TapeRecoveryPointInfo where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON TapeRecoveryPointInfo where
    toJSON = genericToJSON jsonOptions

data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo
    { _vrpiVolumeARN               :: Maybe Text
    , _vrpiVolumeRecoveryPointTime :: Maybe Text
    , _vrpiVolumeSizeInBytes       :: Maybe Integer
    , _vrpiVolumeUsageInBytes      :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | 'VolumeRecoveryPointInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vrpiVolumeARN' @::@ 'Maybe' 'Text'
--
-- * 'vrpiVolumeRecoveryPointTime' @::@ 'Maybe' 'Text'
--
-- * 'vrpiVolumeSizeInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'vrpiVolumeUsageInBytes' @::@ 'Maybe' 'Integer'
--
volumeRecoveryPointInfo :: VolumeRecoveryPointInfo
volumeRecoveryPointInfo = VolumeRecoveryPointInfo
    { _vrpiVolumeARN               = Nothing
    , _vrpiVolumeSizeInBytes       = Nothing
    , _vrpiVolumeUsageInBytes      = Nothing
    , _vrpiVolumeRecoveryPointTime = Nothing
    }

vrpiVolumeARN :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpiVolumeARN = lens _vrpiVolumeARN (\s a -> s { _vrpiVolumeARN = a })

vrpiVolumeRecoveryPointTime :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpiVolumeRecoveryPointTime =
    lens _vrpiVolumeRecoveryPointTime
        (\s a -> s { _vrpiVolumeRecoveryPointTime = a })

vrpiVolumeSizeInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpiVolumeSizeInBytes =
    lens _vrpiVolumeSizeInBytes (\s a -> s { _vrpiVolumeSizeInBytes = a })

vrpiVolumeUsageInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpiVolumeUsageInBytes =
    lens _vrpiVolumeUsageInBytes (\s a -> s { _vrpiVolumeUsageInBytes = a })

instance FromJSON VolumeRecoveryPointInfo where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON VolumeRecoveryPointInfo where
    toJSON = genericToJSON jsonOptions

data TapeArchive = TapeArchive
    { _taCompletionTime  :: Maybe RFC822
    , _taRetrievedTo     :: Maybe Text
    , _taTapeARN         :: Maybe Text
    , _taTapeBarcode     :: Maybe Text
    , _taTapeSizeInBytes :: Maybe Nat
    , _taTapeStatus      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'TapeArchive' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'taCompletionTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'taRetrievedTo' @::@ 'Maybe' 'Text'
--
-- * 'taTapeARN' @::@ 'Maybe' 'Text'
--
-- * 'taTapeBarcode' @::@ 'Maybe' 'Text'
--
-- * 'taTapeSizeInBytes' @::@ 'Maybe' 'Natural'
--
-- * 'taTapeStatus' @::@ 'Maybe' 'Text'
--
tapeArchive :: TapeArchive
tapeArchive = TapeArchive
    { _taTapeARN         = Nothing
    , _taTapeBarcode     = Nothing
    , _taTapeSizeInBytes = Nothing
    , _taCompletionTime  = Nothing
    , _taRetrievedTo     = Nothing
    , _taTapeStatus      = Nothing
    }

-- | The time that the archiving of the virtual tape was completed. The string
-- format of the completion time is in the ISO8601 extended
-- YYYY-MM-DD'T'HH:MM:SS'Z' format.
taCompletionTime :: Lens' TapeArchive (Maybe UTCTime)
taCompletionTime = lens _taCompletionTime (\s a -> s { _taCompletionTime = a })
    . mapping _Time

-- | The Amazon Resource Name (ARN) of the gateway-VTL that the virtual tape
-- is being retrieved to. The virtual tape is retrieved from the virtual
-- tape shelf (VTS).
taRetrievedTo :: Lens' TapeArchive (Maybe Text)
taRetrievedTo = lens _taRetrievedTo (\s a -> s { _taRetrievedTo = a })

-- | The Amazon Resource Name (ARN) of an archived virtual tape.
taTapeARN :: Lens' TapeArchive (Maybe Text)
taTapeARN = lens _taTapeARN (\s a -> s { _taTapeARN = a })

-- | The barcode that identifies the archived virtual tape.
taTapeBarcode :: Lens' TapeArchive (Maybe Text)
taTapeBarcode = lens _taTapeBarcode (\s a -> s { _taTapeBarcode = a })

-- | The size, in bytes, of the archived virtual tape.
taTapeSizeInBytes :: Lens' TapeArchive (Maybe Natural)
taTapeSizeInBytes =
    lens _taTapeSizeInBytes (\s a -> s { _taTapeSizeInBytes = a })
        . mapping _Nat

-- | The current state of the archived virtual tape.
taTapeStatus :: Lens' TapeArchive (Maybe Text)
taTapeStatus = lens _taTapeStatus (\s a -> s { _taTapeStatus = a })

instance FromJSON TapeArchive where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON TapeArchive where
    toJSON = genericToJSON jsonOptions

data ErrorCode
    = ActivationKeyExpired              -- ^ ActivationKeyExpired
    | ActivationKeyInvalid              -- ^ ActivationKeyInvalid
    | ActivationKeyNotFound             -- ^ ActivationKeyNotFound
    | AuthenticationFailure             -- ^ AuthenticationFailure
    | BandwidthThrottleScheduleNotFound -- ^ BandwidthThrottleScheduleNotFound
    | Blocked                           -- ^ Blocked
    | CannotExportSnapshot              -- ^ CannotExportSnapshot
    | ChapCredentialNotFound            -- ^ ChapCredentialNotFound
    | DiskAlreadyAllocated              -- ^ DiskAlreadyAllocated
    | DiskDoesNotExist                  -- ^ DiskDoesNotExist
    | DiskSizeGreaterThanVolumeMaxSize  -- ^ DiskSizeGreaterThanVolumeMaxSize
    | DiskSizeLessThanVolumeSize        -- ^ DiskSizeLessThanVolumeSize
    | DiskSizeNotGigAligned             -- ^ DiskSizeNotGigAligned
    | DuplicateCertificateInfo          -- ^ DuplicateCertificateInfo
    | DuplicateSchedule                 -- ^ DuplicateSchedule
    | EndpointNotFound                  -- ^ EndpointNotFound
    | GatewayInternalError              -- ^ GatewayInternalError
    | GatewayNotConnected               -- ^ GatewayNotConnected
    | GatewayNotFound                   -- ^ GatewayNotFound
    | GatewayProxyNetworkConnectionBusy -- ^ GatewayProxyNetworkConnectionBusy
    | IAMNotSupported                   -- ^ IAMNotSupported
    | InitiatorInvalid                  -- ^ InitiatorInvalid
    | InitiatorNotFound                 -- ^ InitiatorNotFound
    | InternalError                     -- ^ InternalError
    | InvalidEndpoint                   -- ^ InvalidEndpoint
    | InvalidGateway                    -- ^ InvalidGateway
    | InvalidParameters                 -- ^ InvalidParameters
    | InvalidSchedule                   -- ^ InvalidSchedule
    | LocalStorageLimitExceeded         -- ^ LocalStorageLimitExceeded
    | LunAlreadyAllocated               -- ^ LunAlreadyAllocated 
    | LunInvalid                        -- ^ LunInvalid
    | MaximumContentLengthExceeded      -- ^ MaximumContentLengthExceeded
    | MaximumTapeCartridgeCountExceeded -- ^ MaximumTapeCartridgeCountExceeded
    | MaximumVolumeCountExceeded        -- ^ MaximumVolumeCountExceeded
    | NetworkConfigurationChanged       -- ^ NetworkConfigurationChanged
    | NoDisksAvailable                  -- ^ NoDisksAvailable
    | NotImplemented                    -- ^ NotImplemented
    | NotSupported                      -- ^ NotSupported
    | OperationAborted                  -- ^ OperationAborted
    | OutdatedGateway                   -- ^ OutdatedGateway
    | ParametersNotImplemented          -- ^ ParametersNotImplemented
    | RegionInvalid                     -- ^ RegionInvalid
    | RequestTimeout                    -- ^ RequestTimeout
    | ServiceUnavailable                -- ^ ServiceUnavailable
    | SnapshotDeleted                   -- ^ SnapshotDeleted
    | SnapshotIdInvalid                 -- ^ SnapshotIdInvalid
    | SnapshotInProgress                -- ^ SnapshotInProgress
    | SnapshotNotFound                  -- ^ SnapshotNotFound
    | SnapshotScheduleNotFound          -- ^ SnapshotScheduleNotFound
    | StagingAreaFull                   -- ^ StagingAreaFull
    | StorageFailure                    -- ^ StorageFailure
    | TapeCartridgeNotFound             -- ^ TapeCartridgeNotFound
    | TargetAlreadyExists               -- ^ TargetAlreadyExists
    | TargetInvalid                     -- ^ TargetInvalid
    | TargetNotFound                    -- ^ TargetNotFound
    | UnauthorizedOperation             -- ^ UnauthorizedOperation
    | VolumeAlreadyExists               -- ^ VolumeAlreadyExists
    | VolumeIdInvalid                   -- ^ VolumeIdInvalid
    | VolumeInUse                       -- ^ VolumeInUse
    | VolumeNotFound                    -- ^ VolumeNotFound
    | VolumeNotReady                    -- ^ VolumeNotReady
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ErrorCode

instance FromText ErrorCode where
    parser = match "ActivationKeyExpired"              ActivationKeyExpired
         <|> match "ActivationKeyInvalid"              ActivationKeyInvalid
         <|> match "ActivationKeyNotFound"             ActivationKeyNotFound
         <|> match "AuthenticationFailure"             AuthenticationFailure
         <|> match "BandwidthThrottleScheduleNotFound" BandwidthThrottleScheduleNotFound
         <|> match "Blocked"                           Blocked
         <|> match "CannotExportSnapshot"              CannotExportSnapshot
         <|> match "ChapCredentialNotFound"            ChapCredentialNotFound
         <|> match "DiskAlreadyAllocated"              DiskAlreadyAllocated
         <|> match "DiskDoesNotExist"                  DiskDoesNotExist
         <|> match "DiskSizeGreaterThanVolumeMaxSize"  DiskSizeGreaterThanVolumeMaxSize
         <|> match "DiskSizeLessThanVolumeSize"        DiskSizeLessThanVolumeSize
         <|> match "DiskSizeNotGigAligned"             DiskSizeNotGigAligned
         <|> match "DuplicateCertificateInfo"          DuplicateCertificateInfo
         <|> match "DuplicateSchedule"                 DuplicateSchedule
         <|> match "EndpointNotFound"                  EndpointNotFound
         <|> match "GatewayInternalError"              GatewayInternalError
         <|> match "GatewayNotConnected"               GatewayNotConnected
         <|> match "GatewayNotFound"                   GatewayNotFound
         <|> match "GatewayProxyNetworkConnectionBusy" GatewayProxyNetworkConnectionBusy
         <|> match "IAMNotSupported"                   IAMNotSupported
         <|> match "InitiatorInvalid"                  InitiatorInvalid
         <|> match "InitiatorNotFound"                 InitiatorNotFound
         <|> match "InternalError"                     InternalError
         <|> match "InvalidEndpoint"                   InvalidEndpoint
         <|> match "InvalidGateway"                    InvalidGateway
         <|> match "InvalidParameters"                 InvalidParameters
         <|> match "InvalidSchedule"                   InvalidSchedule
         <|> match "LocalStorageLimitExceeded"         LocalStorageLimitExceeded
         <|> match "LunAlreadyAllocated "              LunAlreadyAllocated
         <|> match "LunInvalid"                        LunInvalid
         <|> match "MaximumContentLengthExceeded"      MaximumContentLengthExceeded
         <|> match "MaximumTapeCartridgeCountExceeded" MaximumTapeCartridgeCountExceeded
         <|> match "MaximumVolumeCountExceeded"        MaximumVolumeCountExceeded
         <|> match "NetworkConfigurationChanged"       NetworkConfigurationChanged
         <|> match "NoDisksAvailable"                  NoDisksAvailable
         <|> match "NotImplemented"                    NotImplemented
         <|> match "NotSupported"                      NotSupported
         <|> match "OperationAborted"                  OperationAborted
         <|> match "OutdatedGateway"                   OutdatedGateway
         <|> match "ParametersNotImplemented"          ParametersNotImplemented
         <|> match "RegionInvalid"                     RegionInvalid
         <|> match "RequestTimeout"                    RequestTimeout
         <|> match "ServiceUnavailable"                ServiceUnavailable
         <|> match "SnapshotDeleted"                   SnapshotDeleted
         <|> match "SnapshotIdInvalid"                 SnapshotIdInvalid
         <|> match "SnapshotInProgress"                SnapshotInProgress
         <|> match "SnapshotNotFound"                  SnapshotNotFound
         <|> match "SnapshotScheduleNotFound"          SnapshotScheduleNotFound
         <|> match "StagingAreaFull"                   StagingAreaFull
         <|> match "StorageFailure"                    StorageFailure
         <|> match "TapeCartridgeNotFound"             TapeCartridgeNotFound
         <|> match "TargetAlreadyExists"               TargetAlreadyExists
         <|> match "TargetInvalid"                     TargetInvalid
         <|> match "TargetNotFound"                    TargetNotFound
         <|> match "UnauthorizedOperation"             UnauthorizedOperation
         <|> match "VolumeAlreadyExists"               VolumeAlreadyExists
         <|> match "VolumeIdInvalid"                   VolumeIdInvalid
         <|> match "VolumeInUse"                       VolumeInUse
         <|> match "VolumeNotFound"                    VolumeNotFound
         <|> match "VolumeNotReady"                    VolumeNotReady

instance ToText ErrorCode where
    toText = \case
        ActivationKeyExpired              -> "ActivationKeyExpired"
        ActivationKeyInvalid              -> "ActivationKeyInvalid"
        ActivationKeyNotFound             -> "ActivationKeyNotFound"
        AuthenticationFailure             -> "AuthenticationFailure"
        BandwidthThrottleScheduleNotFound -> "BandwidthThrottleScheduleNotFound"
        Blocked                           -> "Blocked"
        CannotExportSnapshot              -> "CannotExportSnapshot"
        ChapCredentialNotFound            -> "ChapCredentialNotFound"
        DiskAlreadyAllocated              -> "DiskAlreadyAllocated"
        DiskDoesNotExist                  -> "DiskDoesNotExist"
        DiskSizeGreaterThanVolumeMaxSize  -> "DiskSizeGreaterThanVolumeMaxSize"
        DiskSizeLessThanVolumeSize        -> "DiskSizeLessThanVolumeSize"
        DiskSizeNotGigAligned             -> "DiskSizeNotGigAligned"
        DuplicateCertificateInfo          -> "DuplicateCertificateInfo"
        DuplicateSchedule                 -> "DuplicateSchedule"
        EndpointNotFound                  -> "EndpointNotFound"
        GatewayInternalError              -> "GatewayInternalError"
        GatewayNotConnected               -> "GatewayNotConnected"
        GatewayNotFound                   -> "GatewayNotFound"
        GatewayProxyNetworkConnectionBusy -> "GatewayProxyNetworkConnectionBusy"
        IAMNotSupported                   -> "IAMNotSupported"
        InitiatorInvalid                  -> "InitiatorInvalid"
        InitiatorNotFound                 -> "InitiatorNotFound"
        InternalError                     -> "InternalError"
        InvalidEndpoint                   -> "InvalidEndpoint"
        InvalidGateway                    -> "InvalidGateway"
        InvalidParameters                 -> "InvalidParameters"
        InvalidSchedule                   -> "InvalidSchedule"
        LocalStorageLimitExceeded         -> "LocalStorageLimitExceeded"
        LunAlreadyAllocated               -> "LunAlreadyAllocated "
        LunInvalid                        -> "LunInvalid"
        MaximumContentLengthExceeded      -> "MaximumContentLengthExceeded"
        MaximumTapeCartridgeCountExceeded -> "MaximumTapeCartridgeCountExceeded"
        MaximumVolumeCountExceeded        -> "MaximumVolumeCountExceeded"
        NetworkConfigurationChanged       -> "NetworkConfigurationChanged"
        NoDisksAvailable                  -> "NoDisksAvailable"
        NotImplemented                    -> "NotImplemented"
        NotSupported                      -> "NotSupported"
        OperationAborted                  -> "OperationAborted"
        OutdatedGateway                   -> "OutdatedGateway"
        ParametersNotImplemented          -> "ParametersNotImplemented"
        RegionInvalid                     -> "RegionInvalid"
        RequestTimeout                    -> "RequestTimeout"
        ServiceUnavailable                -> "ServiceUnavailable"
        SnapshotDeleted                   -> "SnapshotDeleted"
        SnapshotIdInvalid                 -> "SnapshotIdInvalid"
        SnapshotInProgress                -> "SnapshotInProgress"
        SnapshotNotFound                  -> "SnapshotNotFound"
        SnapshotScheduleNotFound          -> "SnapshotScheduleNotFound"
        StagingAreaFull                   -> "StagingAreaFull"
        StorageFailure                    -> "StorageFailure"
        TapeCartridgeNotFound             -> "TapeCartridgeNotFound"
        TargetAlreadyExists               -> "TargetAlreadyExists"
        TargetInvalid                     -> "TargetInvalid"
        TargetNotFound                    -> "TargetNotFound"
        UnauthorizedOperation             -> "UnauthorizedOperation"
        VolumeAlreadyExists               -> "VolumeAlreadyExists"
        VolumeIdInvalid                   -> "VolumeIdInvalid"
        VolumeInUse                       -> "VolumeInUse"
        VolumeNotFound                    -> "VolumeNotFound"
        VolumeNotReady                    -> "VolumeNotReady"

instance FromJSON ErrorCode where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON ErrorCode where
    toJSON = genericToJSON jsonOptions

data StorediSCSIVolume = StorediSCSIVolume
    { _sscsivPreservedExistingData :: Maybe Bool
    , _sscsivSourceSnapshotId      :: Maybe Text
    , _sscsivVolumeARN             :: Maybe Text
    , _sscsivVolumeDiskId          :: Maybe Text
    , _sscsivVolumeId              :: Maybe Text
    , _sscsivVolumeProgress        :: Maybe Double
    , _sscsivVolumeSizeInBytes     :: Maybe Integer
    , _sscsivVolumeStatus          :: Maybe Text
    , _sscsivVolumeType            :: Maybe Text
    , _sscsivVolumeiSCSIAttributes :: Maybe VolumeiSCSIAttributes
    } deriving (Eq, Show, Generic)

-- | 'StorediSCSIVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sscsivPreservedExistingData' @::@ 'Maybe' 'Bool'
--
-- * 'sscsivSourceSnapshotId' @::@ 'Maybe' 'Text'
--
-- * 'sscsivVolumeARN' @::@ 'Maybe' 'Text'
--
-- * 'sscsivVolumeDiskId' @::@ 'Maybe' 'Text'
--
-- * 'sscsivVolumeId' @::@ 'Maybe' 'Text'
--
-- * 'sscsivVolumeProgress' @::@ 'Maybe' 'Double'
--
-- * 'sscsivVolumeSizeInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'sscsivVolumeStatus' @::@ 'Maybe' 'Text'
--
-- * 'sscsivVolumeType' @::@ 'Maybe' 'Text'
--
-- * 'sscsivVolumeiSCSIAttributes' @::@ 'Maybe' 'VolumeiSCSIAttributes'
--
storediSCSIVolume :: StorediSCSIVolume
storediSCSIVolume = StorediSCSIVolume
    { _sscsivVolumeARN             = Nothing
    , _sscsivVolumeId              = Nothing
    , _sscsivVolumeType            = Nothing
    , _sscsivVolumeStatus          = Nothing
    , _sscsivVolumeSizeInBytes     = Nothing
    , _sscsivVolumeProgress        = Nothing
    , _sscsivVolumeDiskId          = Nothing
    , _sscsivSourceSnapshotId      = Nothing
    , _sscsivPreservedExistingData = Nothing
    , _sscsivVolumeiSCSIAttributes = Nothing
    }

sscsivPreservedExistingData :: Lens' StorediSCSIVolume (Maybe Bool)
sscsivPreservedExistingData =
    lens _sscsivPreservedExistingData
        (\s a -> s { _sscsivPreservedExistingData = a })

sscsivSourceSnapshotId :: Lens' StorediSCSIVolume (Maybe Text)
sscsivSourceSnapshotId =
    lens _sscsivSourceSnapshotId (\s a -> s { _sscsivSourceSnapshotId = a })

sscsivVolumeARN :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeARN = lens _sscsivVolumeARN (\s a -> s { _sscsivVolumeARN = a })

sscsivVolumeDiskId :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeDiskId =
    lens _sscsivVolumeDiskId (\s a -> s { _sscsivVolumeDiskId = a })

sscsivVolumeId :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeId = lens _sscsivVolumeId (\s a -> s { _sscsivVolumeId = a })

sscsivVolumeProgress :: Lens' StorediSCSIVolume (Maybe Double)
sscsivVolumeProgress =
    lens _sscsivVolumeProgress (\s a -> s { _sscsivVolumeProgress = a })

sscsivVolumeSizeInBytes :: Lens' StorediSCSIVolume (Maybe Integer)
sscsivVolumeSizeInBytes =
    lens _sscsivVolumeSizeInBytes (\s a -> s { _sscsivVolumeSizeInBytes = a })

sscsivVolumeStatus :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeStatus =
    lens _sscsivVolumeStatus (\s a -> s { _sscsivVolumeStatus = a })

sscsivVolumeType :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeType = lens _sscsivVolumeType (\s a -> s { _sscsivVolumeType = a })

sscsivVolumeiSCSIAttributes :: Lens' StorediSCSIVolume (Maybe VolumeiSCSIAttributes)
sscsivVolumeiSCSIAttributes =
    lens _sscsivVolumeiSCSIAttributes
        (\s a -> s { _sscsivVolumeiSCSIAttributes = a })

instance FromJSON StorediSCSIVolume where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON StorediSCSIVolume where
    toJSON = genericToJSON jsonOptions

data CachediSCSIVolume = CachediSCSIVolume
    { _cscsivSourceSnapshotId      :: Maybe Text
    , _cscsivVolumeARN             :: Maybe Text
    , _cscsivVolumeId              :: Maybe Text
    , _cscsivVolumeProgress        :: Maybe Double
    , _cscsivVolumeSizeInBytes     :: Maybe Integer
    , _cscsivVolumeStatus          :: Maybe Text
    , _cscsivVolumeType            :: Maybe Text
    , _cscsivVolumeiSCSIAttributes :: Maybe VolumeiSCSIAttributes
    } deriving (Eq, Show, Generic)

-- | 'CachediSCSIVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cscsivSourceSnapshotId' @::@ 'Maybe' 'Text'
--
-- * 'cscsivVolumeARN' @::@ 'Maybe' 'Text'
--
-- * 'cscsivVolumeId' @::@ 'Maybe' 'Text'
--
-- * 'cscsivVolumeProgress' @::@ 'Maybe' 'Double'
--
-- * 'cscsivVolumeSizeInBytes' @::@ 'Maybe' 'Integer'
--
-- * 'cscsivVolumeStatus' @::@ 'Maybe' 'Text'
--
-- * 'cscsivVolumeType' @::@ 'Maybe' 'Text'
--
-- * 'cscsivVolumeiSCSIAttributes' @::@ 'Maybe' 'VolumeiSCSIAttributes'
--
cachediSCSIVolume :: CachediSCSIVolume
cachediSCSIVolume = CachediSCSIVolume
    { _cscsivVolumeARN             = Nothing
    , _cscsivVolumeId              = Nothing
    , _cscsivVolumeType            = Nothing
    , _cscsivVolumeStatus          = Nothing
    , _cscsivVolumeSizeInBytes     = Nothing
    , _cscsivVolumeProgress        = Nothing
    , _cscsivSourceSnapshotId      = Nothing
    , _cscsivVolumeiSCSIAttributes = Nothing
    }

cscsivSourceSnapshotId :: Lens' CachediSCSIVolume (Maybe Text)
cscsivSourceSnapshotId =
    lens _cscsivSourceSnapshotId (\s a -> s { _cscsivSourceSnapshotId = a })

cscsivVolumeARN :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeARN = lens _cscsivVolumeARN (\s a -> s { _cscsivVolumeARN = a })

cscsivVolumeId :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeId = lens _cscsivVolumeId (\s a -> s { _cscsivVolumeId = a })

cscsivVolumeProgress :: Lens' CachediSCSIVolume (Maybe Double)
cscsivVolumeProgress =
    lens _cscsivVolumeProgress (\s a -> s { _cscsivVolumeProgress = a })

cscsivVolumeSizeInBytes :: Lens' CachediSCSIVolume (Maybe Integer)
cscsivVolumeSizeInBytes =
    lens _cscsivVolumeSizeInBytes (\s a -> s { _cscsivVolumeSizeInBytes = a })

cscsivVolumeStatus :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeStatus =
    lens _cscsivVolumeStatus (\s a -> s { _cscsivVolumeStatus = a })

cscsivVolumeType :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeType = lens _cscsivVolumeType (\s a -> s { _cscsivVolumeType = a })

cscsivVolumeiSCSIAttributes :: Lens' CachediSCSIVolume (Maybe VolumeiSCSIAttributes)
cscsivVolumeiSCSIAttributes =
    lens _cscsivVolumeiSCSIAttributes
        (\s a -> s { _cscsivVolumeiSCSIAttributes = a })

instance FromJSON CachediSCSIVolume where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON CachediSCSIVolume where
    toJSON = genericToJSON jsonOptions

data VolumeInfo = VolumeInfo
    { _viVolumeARN  :: Maybe Text
    , _viVolumeType :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'VolumeInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'viVolumeARN' @::@ 'Maybe' 'Text'
--
-- * 'viVolumeType' @::@ 'Maybe' 'Text'
--
volumeInfo :: VolumeInfo
volumeInfo = VolumeInfo
    { _viVolumeARN  = Nothing
    , _viVolumeType = Nothing
    }

viVolumeARN :: Lens' VolumeInfo (Maybe Text)
viVolumeARN = lens _viVolumeARN (\s a -> s { _viVolumeARN = a })

viVolumeType :: Lens' VolumeInfo (Maybe Text)
viVolumeType = lens _viVolumeType (\s a -> s { _viVolumeType = a })

instance FromJSON VolumeInfo where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON VolumeInfo where
    toJSON = genericToJSON jsonOptions

data GatewayInfo = GatewayInfo
    { _giGatewayARN              :: Maybe Text
    , _giGatewayOperationalState :: Maybe Text
    , _giGatewayType             :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GatewayInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giGatewayARN' @::@ 'Maybe' 'Text'
--
-- * 'giGatewayOperationalState' @::@ 'Maybe' 'Text'
--
-- * 'giGatewayType' @::@ 'Maybe' 'Text'
--
gatewayInfo :: GatewayInfo
gatewayInfo = GatewayInfo
    { _giGatewayARN              = Nothing
    , _giGatewayType             = Nothing
    , _giGatewayOperationalState = Nothing
    }

giGatewayARN :: Lens' GatewayInfo (Maybe Text)
giGatewayARN = lens _giGatewayARN (\s a -> s { _giGatewayARN = a })

giGatewayOperationalState :: Lens' GatewayInfo (Maybe Text)
giGatewayOperationalState =
    lens _giGatewayOperationalState
        (\s a -> s { _giGatewayOperationalState = a })

giGatewayType :: Lens' GatewayInfo (Maybe Text)
giGatewayType = lens _giGatewayType (\s a -> s { _giGatewayType = a })

instance FromJSON GatewayInfo where
    parseJSON = genericParseJSON jsonOptions

instance ToJSON GatewayInfo where
    toJSON = genericToJSON jsonOptions
