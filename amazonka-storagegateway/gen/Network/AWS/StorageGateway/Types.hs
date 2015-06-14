{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.StorageGateway.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.StorageGateway.Types
    (
    -- * Service
      StorageGateway
    -- ** Errors
    , JSONError

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
    , disDiskAllocationResource
    , disDiskAllocationType
    , disDiskNode
    , disDiskPath
    , disDiskSizeInBytes
    , disDiskStatus
    , disDiskId

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
    , tapTapeBarcode
    , tapTapeStatus
    , tapProgress
    , tapTapeARN
    , tapTapeSizeInBytes
    , tapVTLDevice

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

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2013-06-30@ of the Amazon Storage Gateway SDK.
data StorageGateway

instance AWSService StorageGateway where
    type Sg StorageGateway = V4
    type Er StorageGateway = JSONError

    service = service'
      where
        service' :: Service StorageGateway
        service' = Service
            { _svcAbbrev  = "StorageGateway"
            , _svcPrefix  = "storagegateway"
            , _svcVersion = "2013-06-30"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry StorageGateway
        retry = undefined

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'cachediSCSIVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cscsivVolumeStatus'
--
-- * 'cscsivVolumeiSCSIAttributes'
--
-- * 'cscsivSourceSnapshotId'
--
-- * 'cscsivVolumeARN'
--
-- * 'cscsivVolumeProgress'
--
-- * 'cscsivVolumeSizeInBytes'
--
-- * 'cscsivVolumeId'
--
-- * 'cscsivVolumeType'
data CachediSCSIVolume = CachediSCSIVolume'{_cscsivVolumeStatus :: Maybe Text, _cscsivVolumeiSCSIAttributes :: Maybe VolumeiSCSIAttributes, _cscsivSourceSnapshotId :: Maybe Text, _cscsivVolumeARN :: Maybe Text, _cscsivVolumeProgress :: Maybe Double, _cscsivVolumeSizeInBytes :: Maybe Integer, _cscsivVolumeId :: Maybe Text, _cscsivVolumeType :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CachediSCSIVolume' smart constructor.
cachediSCSIVolume :: CachediSCSIVolume
cachediSCSIVolume = CachediSCSIVolume'{_cscsivVolumeStatus = Nothing, _cscsivVolumeiSCSIAttributes = Nothing, _cscsivSourceSnapshotId = Nothing, _cscsivVolumeARN = Nothing, _cscsivVolumeProgress = Nothing, _cscsivVolumeSizeInBytes = Nothing, _cscsivVolumeId = Nothing, _cscsivVolumeType = Nothing};

-- | FIXME: Undocumented member.
cscsivVolumeStatus :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeStatus = lens _cscsivVolumeStatus (\ s a -> s{_cscsivVolumeStatus = a});

-- | FIXME: Undocumented member.
cscsivVolumeiSCSIAttributes :: Lens' CachediSCSIVolume (Maybe VolumeiSCSIAttributes)
cscsivVolumeiSCSIAttributes = lens _cscsivVolumeiSCSIAttributes (\ s a -> s{_cscsivVolumeiSCSIAttributes = a});

-- | FIXME: Undocumented member.
cscsivSourceSnapshotId :: Lens' CachediSCSIVolume (Maybe Text)
cscsivSourceSnapshotId = lens _cscsivSourceSnapshotId (\ s a -> s{_cscsivSourceSnapshotId = a});

-- | FIXME: Undocumented member.
cscsivVolumeARN :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeARN = lens _cscsivVolumeARN (\ s a -> s{_cscsivVolumeARN = a});

-- | FIXME: Undocumented member.
cscsivVolumeProgress :: Lens' CachediSCSIVolume (Maybe Double)
cscsivVolumeProgress = lens _cscsivVolumeProgress (\ s a -> s{_cscsivVolumeProgress = a});

-- | FIXME: Undocumented member.
cscsivVolumeSizeInBytes :: Lens' CachediSCSIVolume (Maybe Integer)
cscsivVolumeSizeInBytes = lens _cscsivVolumeSizeInBytes (\ s a -> s{_cscsivVolumeSizeInBytes = a});

-- | FIXME: Undocumented member.
cscsivVolumeId :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeId = lens _cscsivVolumeId (\ s a -> s{_cscsivVolumeId = a});

-- | FIXME: Undocumented member.
cscsivVolumeType :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeType = lens _cscsivVolumeType (\ s a -> s{_cscsivVolumeType = a});

instance FromJSON CachediSCSIVolume where
        parseJSON
          = withObject "CachediSCSIVolume"
              (\ x ->
                 CachediSCSIVolume' <$>
                   x .:? "VolumeStatus" <*>
                     x .:? "VolumeiSCSIAttributes"
                     <*> x .:? "SourceSnapshotId"
                     <*> x .:? "VolumeARN"
                     <*> x .:? "VolumeProgress"
                     <*> x .:? "VolumeSizeInBytes"
                     <*> x .:? "VolumeId"
                     <*> x .:? "VolumeType")

-- | /See:/ 'chapInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciTargetARN'
--
-- * 'ciSecretToAuthenticateInitiator'
--
-- * 'ciInitiatorName'
--
-- * 'ciSecretToAuthenticateTarget'
data ChapInfo = ChapInfo'{_ciTargetARN :: Maybe Text, _ciSecretToAuthenticateInitiator :: Maybe Text, _ciInitiatorName :: Maybe Text, _ciSecretToAuthenticateTarget :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ChapInfo' smart constructor.
chapInfo :: ChapInfo
chapInfo = ChapInfo'{_ciTargetARN = Nothing, _ciSecretToAuthenticateInitiator = Nothing, _ciInitiatorName = Nothing, _ciSecretToAuthenticateTarget = Nothing};

-- | The Amazon Resource Name (ARN) of the volume.
--
-- /Valid Values/: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
ciTargetARN :: Lens' ChapInfo (Maybe Text)
ciTargetARN = lens _ciTargetARN (\ s a -> s{_ciTargetARN = a});

-- | The secret key that the initiator (for example, the Windows client) must
-- provide to participate in mutual CHAP with the target.
ciSecretToAuthenticateInitiator :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateInitiator = lens _ciSecretToAuthenticateInitiator (\ s a -> s{_ciSecretToAuthenticateInitiator = a});

-- | The iSCSI initiator that connects to the target.
ciInitiatorName :: Lens' ChapInfo (Maybe Text)
ciInitiatorName = lens _ciInitiatorName (\ s a -> s{_ciInitiatorName = a});

-- | The secret key that the target must provide to participate in mutual
-- CHAP with the initiator (e.g. Windows client).
ciSecretToAuthenticateTarget :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateTarget = lens _ciSecretToAuthenticateTarget (\ s a -> s{_ciSecretToAuthenticateTarget = a});

instance FromJSON ChapInfo where
        parseJSON
          = withObject "ChapInfo"
              (\ x ->
                 ChapInfo' <$>
                   x .:? "TargetARN" <*>
                     x .:? "SecretToAuthenticateInitiator"
                     <*> x .:? "InitiatorName"
                     <*> x .:? "SecretToAuthenticateTarget")

-- | /See:/ 'deviceiSCSIAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscsiaTargetARN'
--
-- * 'dscsiaChapEnabled'
--
-- * 'dscsiaNetworkInterfaceId'
--
-- * 'dscsiaNetworkInterfacePort'
data DeviceiSCSIAttributes = DeviceiSCSIAttributes'{_dscsiaTargetARN :: Maybe Text, _dscsiaChapEnabled :: Maybe Bool, _dscsiaNetworkInterfaceId :: Maybe Text, _dscsiaNetworkInterfacePort :: Maybe Int} deriving (Eq, Read, Show)

-- | 'DeviceiSCSIAttributes' smart constructor.
deviceiSCSIAttributes :: DeviceiSCSIAttributes
deviceiSCSIAttributes = DeviceiSCSIAttributes'{_dscsiaTargetARN = Nothing, _dscsiaChapEnabled = Nothing, _dscsiaNetworkInterfaceId = Nothing, _dscsiaNetworkInterfacePort = Nothing};

-- | Specifies the unique Amazon Resource Name(ARN) that encodes the iSCSI
-- qualified name(iqn) of a tape drive or media changer target.
dscsiaTargetARN :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaTargetARN = lens _dscsiaTargetARN (\ s a -> s{_dscsiaTargetARN = a});

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
dscsiaChapEnabled :: Lens' DeviceiSCSIAttributes (Maybe Bool)
dscsiaChapEnabled = lens _dscsiaChapEnabled (\ s a -> s{_dscsiaChapEnabled = a});

-- | The network interface identifier of the VTL device.
dscsiaNetworkInterfaceId :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaNetworkInterfaceId = lens _dscsiaNetworkInterfaceId (\ s a -> s{_dscsiaNetworkInterfaceId = a});

-- | The port used to communicate with iSCSI VTL device targets.
dscsiaNetworkInterfacePort :: Lens' DeviceiSCSIAttributes (Maybe Int)
dscsiaNetworkInterfacePort = lens _dscsiaNetworkInterfacePort (\ s a -> s{_dscsiaNetworkInterfacePort = a});

instance FromJSON DeviceiSCSIAttributes where
        parseJSON
          = withObject "DeviceiSCSIAttributes"
              (\ x ->
                 DeviceiSCSIAttributes' <$>
                   x .:? "TargetARN" <*> x .:? "ChapEnabled" <*>
                     x .:? "NetworkInterfaceId"
                     <*> x .:? "NetworkInterfacePort")

-- | /See:/ 'disk' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'disDiskAllocationResource'
--
-- * 'disDiskAllocationType'
--
-- * 'disDiskNode'
--
-- * 'disDiskPath'
--
-- * 'disDiskSizeInBytes'
--
-- * 'disDiskStatus'
--
-- * 'disDiskId'
data Disk = Disk'{_disDiskAllocationResource :: Maybe Text, _disDiskAllocationType :: Maybe Text, _disDiskNode :: Maybe Text, _disDiskPath :: Maybe Text, _disDiskSizeInBytes :: Maybe Integer, _disDiskStatus :: Maybe Text, _disDiskId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Disk' smart constructor.
disk :: Disk
disk = Disk'{_disDiskAllocationResource = Nothing, _disDiskAllocationType = Nothing, _disDiskNode = Nothing, _disDiskPath = Nothing, _disDiskSizeInBytes = Nothing, _disDiskStatus = Nothing, _disDiskId = Nothing};

-- | FIXME: Undocumented member.
disDiskAllocationResource :: Lens' Disk (Maybe Text)
disDiskAllocationResource = lens _disDiskAllocationResource (\ s a -> s{_disDiskAllocationResource = a});

-- | FIXME: Undocumented member.
disDiskAllocationType :: Lens' Disk (Maybe Text)
disDiskAllocationType = lens _disDiskAllocationType (\ s a -> s{_disDiskAllocationType = a});

-- | FIXME: Undocumented member.
disDiskNode :: Lens' Disk (Maybe Text)
disDiskNode = lens _disDiskNode (\ s a -> s{_disDiskNode = a});

-- | FIXME: Undocumented member.
disDiskPath :: Lens' Disk (Maybe Text)
disDiskPath = lens _disDiskPath (\ s a -> s{_disDiskPath = a});

-- | FIXME: Undocumented member.
disDiskSizeInBytes :: Lens' Disk (Maybe Integer)
disDiskSizeInBytes = lens _disDiskSizeInBytes (\ s a -> s{_disDiskSizeInBytes = a});

-- | FIXME: Undocumented member.
disDiskStatus :: Lens' Disk (Maybe Text)
disDiskStatus = lens _disDiskStatus (\ s a -> s{_disDiskStatus = a});

-- | FIXME: Undocumented member.
disDiskId :: Lens' Disk (Maybe Text)
disDiskId = lens _disDiskId (\ s a -> s{_disDiskId = a});

instance FromJSON Disk where
        parseJSON
          = withObject "Disk"
              (\ x ->
                 Disk' <$>
                   x .:? "DiskAllocationResource" <*>
                     x .:? "DiskAllocationType"
                     <*> x .:? "DiskNode"
                     <*> x .:? "DiskPath"
                     <*> x .:? "DiskSizeInBytes"
                     <*> x .:? "DiskStatus"
                     <*> x .:? "DiskId")

-- | /See:/ 'gatewayInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giGatewayARN'
--
-- * 'giGatewayOperationalState'
--
-- * 'giGatewayType'
data GatewayInfo = GatewayInfo'{_giGatewayARN :: Maybe Text, _giGatewayOperationalState :: Maybe Text, _giGatewayType :: Maybe Text} deriving (Eq, Read, Show)

-- | 'GatewayInfo' smart constructor.
gatewayInfo :: GatewayInfo
gatewayInfo = GatewayInfo'{_giGatewayARN = Nothing, _giGatewayOperationalState = Nothing, _giGatewayType = Nothing};

-- | FIXME: Undocumented member.
giGatewayARN :: Lens' GatewayInfo (Maybe Text)
giGatewayARN = lens _giGatewayARN (\ s a -> s{_giGatewayARN = a});

-- | FIXME: Undocumented member.
giGatewayOperationalState :: Lens' GatewayInfo (Maybe Text)
giGatewayOperationalState = lens _giGatewayOperationalState (\ s a -> s{_giGatewayOperationalState = a});

-- | FIXME: Undocumented member.
giGatewayType :: Lens' GatewayInfo (Maybe Text)
giGatewayType = lens _giGatewayType (\ s a -> s{_giGatewayType = a});

instance FromJSON GatewayInfo where
        parseJSON
          = withObject "GatewayInfo"
              (\ x ->
                 GatewayInfo' <$>
                   x .:? "GatewayARN" <*>
                     x .:? "GatewayOperationalState"
                     <*> x .:? "GatewayType")

-- | /See:/ 'networkInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'niIPv6Address'
--
-- * 'niMACAddress'
--
-- * 'niIPv4Address'
data NetworkInterface = NetworkInterface'{_niIPv6Address :: Maybe Text, _niMACAddress :: Maybe Text, _niIPv4Address :: Maybe Text} deriving (Eq, Read, Show)

-- | 'NetworkInterface' smart constructor.
networkInterface :: NetworkInterface
networkInterface = NetworkInterface'{_niIPv6Address = Nothing, _niMACAddress = Nothing, _niIPv4Address = Nothing};

-- | The Internet Protocol version 6 (IPv6) address of the interface.
-- /Currently not supported/.
niIPv6Address :: Lens' NetworkInterface (Maybe Text)
niIPv6Address = lens _niIPv6Address (\ s a -> s{_niIPv6Address = a});

-- | The Media Access Control (MAC) address of the interface.
--
-- This is currently unsupported and will not be returned in output.
niMACAddress :: Lens' NetworkInterface (Maybe Text)
niMACAddress = lens _niMACAddress (\ s a -> s{_niMACAddress = a});

-- | The Internet Protocol version 4 (IPv4) address of the interface.
niIPv4Address :: Lens' NetworkInterface (Maybe Text)
niIPv4Address = lens _niIPv4Address (\ s a -> s{_niIPv4Address = a});

instance FromJSON NetworkInterface where
        parseJSON
          = withObject "NetworkInterface"
              (\ x ->
                 NetworkInterface' <$>
                   x .:? "Ipv6Address" <*> x .:? "MacAddress" <*>
                     x .:? "Ipv4Address")

-- | /See:/ 'storediSCSIVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sscsivVolumeStatus'
--
-- * 'sscsivVolumeiSCSIAttributes'
--
-- * 'sscsivSourceSnapshotId'
--
-- * 'sscsivPreservedExistingData'
--
-- * 'sscsivVolumeARN'
--
-- * 'sscsivVolumeProgress'
--
-- * 'sscsivVolumeSizeInBytes'
--
-- * 'sscsivVolumeId'
--
-- * 'sscsivVolumeType'
--
-- * 'sscsivVolumeDiskId'
data StorediSCSIVolume = StorediSCSIVolume'{_sscsivVolumeStatus :: Maybe Text, _sscsivVolumeiSCSIAttributes :: Maybe VolumeiSCSIAttributes, _sscsivSourceSnapshotId :: Maybe Text, _sscsivPreservedExistingData :: Maybe Bool, _sscsivVolumeARN :: Maybe Text, _sscsivVolumeProgress :: Maybe Double, _sscsivVolumeSizeInBytes :: Maybe Integer, _sscsivVolumeId :: Maybe Text, _sscsivVolumeType :: Maybe Text, _sscsivVolumeDiskId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'StorediSCSIVolume' smart constructor.
storediSCSIVolume :: StorediSCSIVolume
storediSCSIVolume = StorediSCSIVolume'{_sscsivVolumeStatus = Nothing, _sscsivVolumeiSCSIAttributes = Nothing, _sscsivSourceSnapshotId = Nothing, _sscsivPreservedExistingData = Nothing, _sscsivVolumeARN = Nothing, _sscsivVolumeProgress = Nothing, _sscsivVolumeSizeInBytes = Nothing, _sscsivVolumeId = Nothing, _sscsivVolumeType = Nothing, _sscsivVolumeDiskId = Nothing};

-- | FIXME: Undocumented member.
sscsivVolumeStatus :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeStatus = lens _sscsivVolumeStatus (\ s a -> s{_sscsivVolumeStatus = a});

-- | FIXME: Undocumented member.
sscsivVolumeiSCSIAttributes :: Lens' StorediSCSIVolume (Maybe VolumeiSCSIAttributes)
sscsivVolumeiSCSIAttributes = lens _sscsivVolumeiSCSIAttributes (\ s a -> s{_sscsivVolumeiSCSIAttributes = a});

-- | FIXME: Undocumented member.
sscsivSourceSnapshotId :: Lens' StorediSCSIVolume (Maybe Text)
sscsivSourceSnapshotId = lens _sscsivSourceSnapshotId (\ s a -> s{_sscsivSourceSnapshotId = a});

-- | FIXME: Undocumented member.
sscsivPreservedExistingData :: Lens' StorediSCSIVolume (Maybe Bool)
sscsivPreservedExistingData = lens _sscsivPreservedExistingData (\ s a -> s{_sscsivPreservedExistingData = a});

-- | FIXME: Undocumented member.
sscsivVolumeARN :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeARN = lens _sscsivVolumeARN (\ s a -> s{_sscsivVolumeARN = a});

-- | FIXME: Undocumented member.
sscsivVolumeProgress :: Lens' StorediSCSIVolume (Maybe Double)
sscsivVolumeProgress = lens _sscsivVolumeProgress (\ s a -> s{_sscsivVolumeProgress = a});

-- | FIXME: Undocumented member.
sscsivVolumeSizeInBytes :: Lens' StorediSCSIVolume (Maybe Integer)
sscsivVolumeSizeInBytes = lens _sscsivVolumeSizeInBytes (\ s a -> s{_sscsivVolumeSizeInBytes = a});

-- | FIXME: Undocumented member.
sscsivVolumeId :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeId = lens _sscsivVolumeId (\ s a -> s{_sscsivVolumeId = a});

-- | FIXME: Undocumented member.
sscsivVolumeType :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeType = lens _sscsivVolumeType (\ s a -> s{_sscsivVolumeType = a});

-- | FIXME: Undocumented member.
sscsivVolumeDiskId :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeDiskId = lens _sscsivVolumeDiskId (\ s a -> s{_sscsivVolumeDiskId = a});

instance FromJSON StorediSCSIVolume where
        parseJSON
          = withObject "StorediSCSIVolume"
              (\ x ->
                 StorediSCSIVolume' <$>
                   x .:? "VolumeStatus" <*>
                     x .:? "VolumeiSCSIAttributes"
                     <*> x .:? "SourceSnapshotId"
                     <*> x .:? "PreservedExistingData"
                     <*> x .:? "VolumeARN"
                     <*> x .:? "VolumeProgress"
                     <*> x .:? "VolumeSizeInBytes"
                     <*> x .:? "VolumeId"
                     <*> x .:? "VolumeType"
                     <*> x .:? "VolumeDiskId")

-- | /See:/ 'tape' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tapTapeBarcode'
--
-- * 'tapTapeStatus'
--
-- * 'tapProgress'
--
-- * 'tapTapeARN'
--
-- * 'tapTapeSizeInBytes'
--
-- * 'tapVTLDevice'
data Tape = Tape'{_tapTapeBarcode :: Maybe Text, _tapTapeStatus :: Maybe Text, _tapProgress :: Maybe Double, _tapTapeARN :: Maybe Text, _tapTapeSizeInBytes :: Maybe Integer, _tapVTLDevice :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Tape' smart constructor.
tape :: Tape
tape = Tape'{_tapTapeBarcode = Nothing, _tapTapeStatus = Nothing, _tapProgress = Nothing, _tapTapeARN = Nothing, _tapTapeSizeInBytes = Nothing, _tapVTLDevice = Nothing};

-- | The barcode that identifies a specific virtual tape.
tapTapeBarcode :: Lens' Tape (Maybe Text)
tapTapeBarcode = lens _tapTapeBarcode (\ s a -> s{_tapTapeBarcode = a});

-- | The current state of the virtual tape.
tapTapeStatus :: Lens' Tape (Maybe Text)
tapTapeStatus = lens _tapTapeStatus (\ s a -> s{_tapTapeStatus = a});

-- | For archiving virtual tapes, indicates how much data remains to be
-- uploaded before archiving is complete.
--
-- Range: 0 (not started) to 100 (complete).
tapProgress :: Lens' Tape (Maybe Double)
tapProgress = lens _tapProgress (\ s a -> s{_tapProgress = a});

-- | The Amazon Resource Name (ARN) of the virtual tape.
tapTapeARN :: Lens' Tape (Maybe Text)
tapTapeARN = lens _tapTapeARN (\ s a -> s{_tapTapeARN = a});

-- | The size, in bytes, of the virtual tape.
tapTapeSizeInBytes :: Lens' Tape (Maybe Integer)
tapTapeSizeInBytes = lens _tapTapeSizeInBytes (\ s a -> s{_tapTapeSizeInBytes = a});

-- | The virtual tape library (VTL) device that the virtual tape is
-- associated with.
tapVTLDevice :: Lens' Tape (Maybe Text)
tapVTLDevice = lens _tapVTLDevice (\ s a -> s{_tapVTLDevice = a});

instance FromJSON Tape where
        parseJSON
          = withObject "Tape"
              (\ x ->
                 Tape' <$>
                   x .:? "TapeBarcode" <*> x .:? "TapeStatus" <*>
                     x .:? "Progress"
                     <*> x .:? "TapeARN"
                     <*> x .:? "TapeSizeInBytes"
                     <*> x .:? "VTLDevice")

-- | /See:/ 'tapeArchive' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'taTapeBarcode'
--
-- * 'taTapeStatus'
--
-- * 'taTapeARN'
--
-- * 'taTapeSizeInBytes'
--
-- * 'taCompletionTime'
--
-- * 'taRetrievedTo'
data TapeArchive = TapeArchive'{_taTapeBarcode :: Maybe Text, _taTapeStatus :: Maybe Text, _taTapeARN :: Maybe Text, _taTapeSizeInBytes :: Maybe Integer, _taCompletionTime :: Maybe POSIX, _taRetrievedTo :: Maybe Text} deriving (Eq, Read, Show)

-- | 'TapeArchive' smart constructor.
tapeArchive :: TapeArchive
tapeArchive = TapeArchive'{_taTapeBarcode = Nothing, _taTapeStatus = Nothing, _taTapeARN = Nothing, _taTapeSizeInBytes = Nothing, _taCompletionTime = Nothing, _taRetrievedTo = Nothing};

-- | The barcode that identifies the archived virtual tape.
taTapeBarcode :: Lens' TapeArchive (Maybe Text)
taTapeBarcode = lens _taTapeBarcode (\ s a -> s{_taTapeBarcode = a});

-- | The current state of the archived virtual tape.
taTapeStatus :: Lens' TapeArchive (Maybe Text)
taTapeStatus = lens _taTapeStatus (\ s a -> s{_taTapeStatus = a});

-- | The Amazon Resource Name (ARN) of an archived virtual tape.
taTapeARN :: Lens' TapeArchive (Maybe Text)
taTapeARN = lens _taTapeARN (\ s a -> s{_taTapeARN = a});

-- | The size, in bytes, of the archived virtual tape.
taTapeSizeInBytes :: Lens' TapeArchive (Maybe Integer)
taTapeSizeInBytes = lens _taTapeSizeInBytes (\ s a -> s{_taTapeSizeInBytes = a});

-- | The time that the archiving of the virtual tape was completed.
--
-- The string format of the completion time is in the ISO8601 extended
-- YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
taCompletionTime :: Lens' TapeArchive (Maybe UTCTime)
taCompletionTime = lens _taCompletionTime (\ s a -> s{_taCompletionTime = a}) . mapping _Time;

-- | The Amazon Resource Name (ARN) of the gateway-VTL that the virtual tape
-- is being retrieved to.
--
-- The virtual tape is retrieved from the virtual tape shelf (VTS).
taRetrievedTo :: Lens' TapeArchive (Maybe Text)
taRetrievedTo = lens _taRetrievedTo (\ s a -> s{_taRetrievedTo = a});

instance FromJSON TapeArchive where
        parseJSON
          = withObject "TapeArchive"
              (\ x ->
                 TapeArchive' <$>
                   x .:? "TapeBarcode" <*> x .:? "TapeStatus" <*>
                     x .:? "TapeARN"
                     <*> x .:? "TapeSizeInBytes"
                     <*> x .:? "CompletionTime"
                     <*> x .:? "RetrievedTo")

-- | /See:/ 'tapeRecoveryPointInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'trpiTapeStatus'
--
-- * 'trpiTapeRecoveryPointTime'
--
-- * 'trpiTapeARN'
--
-- * 'trpiTapeSizeInBytes'
data TapeRecoveryPointInfo = TapeRecoveryPointInfo'{_trpiTapeStatus :: Maybe Text, _trpiTapeRecoveryPointTime :: Maybe POSIX, _trpiTapeARN :: Maybe Text, _trpiTapeSizeInBytes :: Maybe Integer} deriving (Eq, Read, Show)

-- | 'TapeRecoveryPointInfo' smart constructor.
tapeRecoveryPointInfo :: TapeRecoveryPointInfo
tapeRecoveryPointInfo = TapeRecoveryPointInfo'{_trpiTapeStatus = Nothing, _trpiTapeRecoveryPointTime = Nothing, _trpiTapeARN = Nothing, _trpiTapeSizeInBytes = Nothing};

-- | FIXME: Undocumented member.
trpiTapeStatus :: Lens' TapeRecoveryPointInfo (Maybe Text)
trpiTapeStatus = lens _trpiTapeStatus (\ s a -> s{_trpiTapeStatus = a});

-- | The time when the point-in-time view of the virtual tape was replicated
-- for later recovery.
--
-- The string format of the tape recovery point time is in the ISO8601
-- extended YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
trpiTapeRecoveryPointTime :: Lens' TapeRecoveryPointInfo (Maybe UTCTime)
trpiTapeRecoveryPointTime = lens _trpiTapeRecoveryPointTime (\ s a -> s{_trpiTapeRecoveryPointTime = a}) . mapping _Time;

-- | The Amazon Resource Name (ARN) of the virtual tape.
trpiTapeARN :: Lens' TapeRecoveryPointInfo (Maybe Text)
trpiTapeARN = lens _trpiTapeARN (\ s a -> s{_trpiTapeARN = a});

-- | The size, in bytes, of the virtual tapes to recover.
trpiTapeSizeInBytes :: Lens' TapeRecoveryPointInfo (Maybe Integer)
trpiTapeSizeInBytes = lens _trpiTapeSizeInBytes (\ s a -> s{_trpiTapeSizeInBytes = a});

instance FromJSON TapeRecoveryPointInfo where
        parseJSON
          = withObject "TapeRecoveryPointInfo"
              (\ x ->
                 TapeRecoveryPointInfo' <$>
                   x .:? "TapeStatus" <*> x .:? "TapeRecoveryPointTime"
                     <*> x .:? "TapeARN"
                     <*> x .:? "TapeSizeInBytes")

-- | /See:/ 'vTLDevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vtldDeviceiSCSIAttributes'
--
-- * 'vtldVTLDeviceVendor'
--
-- * 'vtldVTLDeviceARN'
--
-- * 'vtldVTLDeviceType'
--
-- * 'vtldVTLDeviceProductIdentifier'
data VTLDevice = VTLDevice'{_vtldDeviceiSCSIAttributes :: Maybe DeviceiSCSIAttributes, _vtldVTLDeviceVendor :: Maybe Text, _vtldVTLDeviceARN :: Maybe Text, _vtldVTLDeviceType :: Maybe Text, _vtldVTLDeviceProductIdentifier :: Maybe Text} deriving (Eq, Read, Show)

-- | 'VTLDevice' smart constructor.
vTLDevice :: VTLDevice
vTLDevice = VTLDevice'{_vtldDeviceiSCSIAttributes = Nothing, _vtldVTLDeviceVendor = Nothing, _vtldVTLDeviceARN = Nothing, _vtldVTLDeviceType = Nothing, _vtldVTLDeviceProductIdentifier = Nothing};

-- | A list of iSCSI information about a VTL device.
vtldDeviceiSCSIAttributes :: Lens' VTLDevice (Maybe DeviceiSCSIAttributes)
vtldDeviceiSCSIAttributes = lens _vtldDeviceiSCSIAttributes (\ s a -> s{_vtldDeviceiSCSIAttributes = a});

-- | FIXME: Undocumented member.
vtldVTLDeviceVendor :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceVendor = lens _vtldVTLDeviceVendor (\ s a -> s{_vtldVTLDeviceVendor = a});

-- | Specifies the unique Amazon Resource Name (ARN) of the device (tape
-- drive or media changer).
vtldVTLDeviceARN :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceARN = lens _vtldVTLDeviceARN (\ s a -> s{_vtldVTLDeviceARN = a});

-- | FIXME: Undocumented member.
vtldVTLDeviceType :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceType = lens _vtldVTLDeviceType (\ s a -> s{_vtldVTLDeviceType = a});

-- | FIXME: Undocumented member.
vtldVTLDeviceProductIdentifier :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceProductIdentifier = lens _vtldVTLDeviceProductIdentifier (\ s a -> s{_vtldVTLDeviceProductIdentifier = a});

instance FromJSON VTLDevice where
        parseJSON
          = withObject "VTLDevice"
              (\ x ->
                 VTLDevice' <$>
                   x .:? "DeviceiSCSIAttributes" <*>
                     x .:? "VTLDeviceVendor"
                     <*> x .:? "VTLDeviceARN"
                     <*> x .:? "VTLDeviceType"
                     <*> x .:? "VTLDeviceProductIdentifier")

-- | /See:/ 'volumeInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'viVolumeARN'
--
-- * 'viVolumeType'
data VolumeInfo = VolumeInfo'{_viVolumeARN :: Maybe Text, _viVolumeType :: Maybe Text} deriving (Eq, Read, Show)

-- | 'VolumeInfo' smart constructor.
volumeInfo :: VolumeInfo
volumeInfo = VolumeInfo'{_viVolumeARN = Nothing, _viVolumeType = Nothing};

-- | FIXME: Undocumented member.
viVolumeARN :: Lens' VolumeInfo (Maybe Text)
viVolumeARN = lens _viVolumeARN (\ s a -> s{_viVolumeARN = a});

-- | FIXME: Undocumented member.
viVolumeType :: Lens' VolumeInfo (Maybe Text)
viVolumeType = lens _viVolumeType (\ s a -> s{_viVolumeType = a});

instance FromJSON VolumeInfo where
        parseJSON
          = withObject "VolumeInfo"
              (\ x ->
                 VolumeInfo' <$>
                   x .:? "VolumeARN" <*> x .:? "VolumeType")

-- | /See:/ 'volumeRecoveryPointInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vrpiVolumeRecoveryPointTime'
--
-- * 'vrpiVolumeARN'
--
-- * 'vrpiVolumeSizeInBytes'
--
-- * 'vrpiVolumeUsageInBytes'
data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo'{_vrpiVolumeRecoveryPointTime :: Maybe Text, _vrpiVolumeARN :: Maybe Text, _vrpiVolumeSizeInBytes :: Maybe Integer, _vrpiVolumeUsageInBytes :: Maybe Integer} deriving (Eq, Read, Show)

-- | 'VolumeRecoveryPointInfo' smart constructor.
volumeRecoveryPointInfo :: VolumeRecoveryPointInfo
volumeRecoveryPointInfo = VolumeRecoveryPointInfo'{_vrpiVolumeRecoveryPointTime = Nothing, _vrpiVolumeARN = Nothing, _vrpiVolumeSizeInBytes = Nothing, _vrpiVolumeUsageInBytes = Nothing};

-- | FIXME: Undocumented member.
vrpiVolumeRecoveryPointTime :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpiVolumeRecoveryPointTime = lens _vrpiVolumeRecoveryPointTime (\ s a -> s{_vrpiVolumeRecoveryPointTime = a});

-- | FIXME: Undocumented member.
vrpiVolumeARN :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpiVolumeARN = lens _vrpiVolumeARN (\ s a -> s{_vrpiVolumeARN = a});

-- | FIXME: Undocumented member.
vrpiVolumeSizeInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpiVolumeSizeInBytes = lens _vrpiVolumeSizeInBytes (\ s a -> s{_vrpiVolumeSizeInBytes = a});

-- | FIXME: Undocumented member.
vrpiVolumeUsageInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpiVolumeUsageInBytes = lens _vrpiVolumeUsageInBytes (\ s a -> s{_vrpiVolumeUsageInBytes = a});

instance FromJSON VolumeRecoveryPointInfo where
        parseJSON
          = withObject "VolumeRecoveryPointInfo"
              (\ x ->
                 VolumeRecoveryPointInfo' <$>
                   x .:? "VolumeRecoveryPointTime" <*> x .:? "VolumeARN"
                     <*> x .:? "VolumeSizeInBytes"
                     <*> x .:? "VolumeUsageInBytes")

-- | /See:/ 'volumeiSCSIAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vscsiaLunNumber'
--
-- * 'vscsiaTargetARN'
--
-- * 'vscsiaChapEnabled'
--
-- * 'vscsiaNetworkInterfaceId'
--
-- * 'vscsiaNetworkInterfacePort'
data VolumeiSCSIAttributes = VolumeiSCSIAttributes'{_vscsiaLunNumber :: Maybe Nat, _vscsiaTargetARN :: Maybe Text, _vscsiaChapEnabled :: Maybe Bool, _vscsiaNetworkInterfaceId :: Maybe Text, _vscsiaNetworkInterfacePort :: Maybe Int} deriving (Eq, Read, Show)

-- | 'VolumeiSCSIAttributes' smart constructor.
volumeiSCSIAttributes :: VolumeiSCSIAttributes
volumeiSCSIAttributes = VolumeiSCSIAttributes'{_vscsiaLunNumber = Nothing, _vscsiaTargetARN = Nothing, _vscsiaChapEnabled = Nothing, _vscsiaNetworkInterfaceId = Nothing, _vscsiaNetworkInterfacePort = Nothing};

-- | The logical disk number.
vscsiaLunNumber :: Lens' VolumeiSCSIAttributes (Maybe Natural)
vscsiaLunNumber = lens _vscsiaLunNumber (\ s a -> s{_vscsiaLunNumber = a}) . mapping _Nat;

-- | The Amazon Resource Name (ARN) of the volume target.
vscsiaTargetARN :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaTargetARN = lens _vscsiaTargetARN (\ s a -> s{_vscsiaTargetARN = a});

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
vscsiaChapEnabled :: Lens' VolumeiSCSIAttributes (Maybe Bool)
vscsiaChapEnabled = lens _vscsiaChapEnabled (\ s a -> s{_vscsiaChapEnabled = a});

-- | The network interface identifier.
vscsiaNetworkInterfaceId :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaNetworkInterfaceId = lens _vscsiaNetworkInterfaceId (\ s a -> s{_vscsiaNetworkInterfaceId = a});

-- | The port used to communicate with iSCSI targets.
vscsiaNetworkInterfacePort :: Lens' VolumeiSCSIAttributes (Maybe Int)
vscsiaNetworkInterfacePort = lens _vscsiaNetworkInterfacePort (\ s a -> s{_vscsiaNetworkInterfacePort = a});

instance FromJSON VolumeiSCSIAttributes where
        parseJSON
          = withObject "VolumeiSCSIAttributes"
              (\ x ->
                 VolumeiSCSIAttributes' <$>
                   x .:? "LunNumber" <*> x .:? "TargetARN" <*>
                     x .:? "ChapEnabled"
                     <*> x .:? "NetworkInterfaceId"
                     <*> x .:? "NetworkInterfacePort")
