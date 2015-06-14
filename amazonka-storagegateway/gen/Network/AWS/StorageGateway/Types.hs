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
    , cscsivVolumeiSCSIAttributes
    , cscsivSourceSnapshotId
    , cscsivVolumeProgress
    , cscsivVolumeSizeInBytes
    , cscsivVolumeStatus
    , cscsivVolumeARN
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
    , dscsiaChapEnabled
    , dscsiaNetworkInterfaceId
    , dscsiaNetworkInterfacePort
    , dscsiaTargetARN

    -- * Disk
    , Disk
    , disk
    , disDiskAllocationResource
    , disDiskNode
    , disDiskPath
    , disDiskSizeInBytes
    , disDiskStatus
    , disDiskAllocationType
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
    , sscsivVolumeiSCSIAttributes
    , sscsivSourceSnapshotId
    , sscsivPreservedExistingData
    , sscsivVolumeProgress
    , sscsivVolumeSizeInBytes
    , sscsivVolumeStatus
    , sscsivVolumeARN
    , sscsivVolumeId
    , sscsivVolumeType
    , sscsivVolumeDiskId

    -- * Tape
    , Tape
    , tape
    , tapTapeStatus
    , tapProgress
    , tapTapeSizeInBytes
    , tapTapeBarcode
    , tapTapeARN
    , tapVTLDevice

    -- * TapeArchive
    , TapeArchive
    , tapeArchive
    , taTapeStatus
    , taTapeSizeInBytes
    , taCompletionTime
    , taTapeBarcode
    , taTapeARN
    , taRetrievedTo

    -- * TapeRecoveryPointInfo
    , TapeRecoveryPointInfo
    , tapeRecoveryPointInfo
    , trpiTapeStatus
    , trpiTapeRecoveryPointTime
    , trpiTapeSizeInBytes
    , trpiTapeARN

    -- * VTLDevice
    , VTLDevice
    , vTLDevice
    , vtldDeviceiSCSIAttributes
    , vtldVTLDeviceVendor
    , vtldVTLDeviceType
    , vtldVTLDeviceProductIdentifier
    , vtldVTLDeviceARN

    -- * VolumeInfo
    , VolumeInfo
    , volumeInfo
    , viVolumeARN
    , viVolumeType

    -- * VolumeRecoveryPointInfo
    , VolumeRecoveryPointInfo
    , volumeRecoveryPointInfo
    , vrpiVolumeRecoveryPointTime
    , vrpiVolumeSizeInBytes
    , vrpiVolumeUsageInBytes
    , vrpiVolumeARN

    -- * VolumeiSCSIAttributes
    , VolumeiSCSIAttributes
    , volumeiSCSIAttributes
    , vscsiaChapEnabled
    , vscsiaNetworkInterfaceId
    , vscsiaNetworkInterfacePort
    , vscsiaLunNumber
    , vscsiaTargetARN
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
-- * 'cscsivVolumeiSCSIAttributes'
--
-- * 'cscsivSourceSnapshotId'
--
-- * 'cscsivVolumeProgress'
--
-- * 'cscsivVolumeSizeInBytes'
--
-- * 'cscsivVolumeStatus'
--
-- * 'cscsivVolumeARN'
--
-- * 'cscsivVolumeId'
--
-- * 'cscsivVolumeType'
data CachediSCSIVolume = CachediSCSIVolume'{_cscsivVolumeiSCSIAttributes :: Maybe VolumeiSCSIAttributes, _cscsivSourceSnapshotId :: Maybe Text, _cscsivVolumeProgress :: Maybe Double, _cscsivVolumeSizeInBytes :: Maybe Integer, _cscsivVolumeStatus :: Text, _cscsivVolumeARN :: Text, _cscsivVolumeId :: Text, _cscsivVolumeType :: Text} deriving (Eq, Read, Show)

-- | 'CachediSCSIVolume' smart constructor.
cachediSCSIVolume :: Text -> Text -> Text -> Text -> CachediSCSIVolume
cachediSCSIVolume pVolumeStatus pVolumeARN pVolumeId pVolumeType = CachediSCSIVolume'{_cscsivVolumeiSCSIAttributes = Nothing, _cscsivSourceSnapshotId = Nothing, _cscsivVolumeProgress = Nothing, _cscsivVolumeSizeInBytes = Nothing, _cscsivVolumeStatus = pVolumeStatus, _cscsivVolumeARN = pVolumeARN, _cscsivVolumeId = pVolumeId, _cscsivVolumeType = pVolumeType};

-- | FIXME: Undocumented member.
cscsivVolumeiSCSIAttributes :: Lens' CachediSCSIVolume (Maybe VolumeiSCSIAttributes)
cscsivVolumeiSCSIAttributes = lens _cscsivVolumeiSCSIAttributes (\ s a -> s{_cscsivVolumeiSCSIAttributes = a});

-- | FIXME: Undocumented member.
cscsivSourceSnapshotId :: Lens' CachediSCSIVolume (Maybe Text)
cscsivSourceSnapshotId = lens _cscsivSourceSnapshotId (\ s a -> s{_cscsivSourceSnapshotId = a});

-- | FIXME: Undocumented member.
cscsivVolumeProgress :: Lens' CachediSCSIVolume (Maybe Double)
cscsivVolumeProgress = lens _cscsivVolumeProgress (\ s a -> s{_cscsivVolumeProgress = a});

-- | FIXME: Undocumented member.
cscsivVolumeSizeInBytes :: Lens' CachediSCSIVolume (Maybe Integer)
cscsivVolumeSizeInBytes = lens _cscsivVolumeSizeInBytes (\ s a -> s{_cscsivVolumeSizeInBytes = a});

-- | FIXME: Undocumented member.
cscsivVolumeStatus :: Lens' CachediSCSIVolume Text
cscsivVolumeStatus = lens _cscsivVolumeStatus (\ s a -> s{_cscsivVolumeStatus = a});

-- | FIXME: Undocumented member.
cscsivVolumeARN :: Lens' CachediSCSIVolume Text
cscsivVolumeARN = lens _cscsivVolumeARN (\ s a -> s{_cscsivVolumeARN = a});

-- | FIXME: Undocumented member.
cscsivVolumeId :: Lens' CachediSCSIVolume Text
cscsivVolumeId = lens _cscsivVolumeId (\ s a -> s{_cscsivVolumeId = a});

-- | FIXME: Undocumented member.
cscsivVolumeType :: Lens' CachediSCSIVolume Text
cscsivVolumeType = lens _cscsivVolumeType (\ s a -> s{_cscsivVolumeType = a});

instance FromJSON CachediSCSIVolume where
        parseJSON
          = withObject "CachediSCSIVolume"
              (\ x ->
                 CachediSCSIVolume' <$>
                   x .:? "VolumeiSCSIAttributes" <*>
                     x .:? "SourceSnapshotId"
                     <*> x .:? "VolumeProgress"
                     <*> x .:? "VolumeSizeInBytes"
                     <*> x .: "VolumeStatus"
                     <*> x .: "VolumeARN"
                     <*> x .: "VolumeId"
                     <*> x .: "VolumeType")

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
data ChapInfo = ChapInfo'{_ciTargetARN :: Text, _ciSecretToAuthenticateInitiator :: Text, _ciInitiatorName :: Text, _ciSecretToAuthenticateTarget :: Text} deriving (Eq, Read, Show)

-- | 'ChapInfo' smart constructor.
chapInfo :: Text -> Text -> Text -> Text -> ChapInfo
chapInfo pTargetARN pSecretToAuthenticateInitiator pInitiatorName pSecretToAuthenticateTarget = ChapInfo'{_ciTargetARN = pTargetARN, _ciSecretToAuthenticateInitiator = pSecretToAuthenticateInitiator, _ciInitiatorName = pInitiatorName, _ciSecretToAuthenticateTarget = pSecretToAuthenticateTarget};

-- | The Amazon Resource Name (ARN) of the volume.
--
-- /Valid Values/: 50 to 500 lowercase letters, numbers, periods (.), and
-- hyphens (-).
ciTargetARN :: Lens' ChapInfo Text
ciTargetARN = lens _ciTargetARN (\ s a -> s{_ciTargetARN = a});

-- | The secret key that the initiator (for example, the Windows client) must
-- provide to participate in mutual CHAP with the target.
ciSecretToAuthenticateInitiator :: Lens' ChapInfo Text
ciSecretToAuthenticateInitiator = lens _ciSecretToAuthenticateInitiator (\ s a -> s{_ciSecretToAuthenticateInitiator = a});

-- | The iSCSI initiator that connects to the target.
ciInitiatorName :: Lens' ChapInfo Text
ciInitiatorName = lens _ciInitiatorName (\ s a -> s{_ciInitiatorName = a});

-- | The secret key that the target must provide to participate in mutual
-- CHAP with the initiator (e.g. Windows client).
ciSecretToAuthenticateTarget :: Lens' ChapInfo Text
ciSecretToAuthenticateTarget = lens _ciSecretToAuthenticateTarget (\ s a -> s{_ciSecretToAuthenticateTarget = a});

instance FromJSON ChapInfo where
        parseJSON
          = withObject "ChapInfo"
              (\ x ->
                 ChapInfo' <$>
                   x .: "TargetARN" <*>
                     x .: "SecretToAuthenticateInitiator"
                     <*> x .: "InitiatorName"
                     <*> x .: "SecretToAuthenticateTarget")

-- | /See:/ 'deviceiSCSIAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscsiaChapEnabled'
--
-- * 'dscsiaNetworkInterfaceId'
--
-- * 'dscsiaNetworkInterfacePort'
--
-- * 'dscsiaTargetARN'
data DeviceiSCSIAttributes = DeviceiSCSIAttributes'{_dscsiaChapEnabled :: Maybe Bool, _dscsiaNetworkInterfaceId :: Maybe Text, _dscsiaNetworkInterfacePort :: Maybe Int, _dscsiaTargetARN :: Text} deriving (Eq, Read, Show)

-- | 'DeviceiSCSIAttributes' smart constructor.
deviceiSCSIAttributes :: Text -> DeviceiSCSIAttributes
deviceiSCSIAttributes pTargetARN = DeviceiSCSIAttributes'{_dscsiaChapEnabled = Nothing, _dscsiaNetworkInterfaceId = Nothing, _dscsiaNetworkInterfacePort = Nothing, _dscsiaTargetARN = pTargetARN};

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
dscsiaChapEnabled :: Lens' DeviceiSCSIAttributes (Maybe Bool)
dscsiaChapEnabled = lens _dscsiaChapEnabled (\ s a -> s{_dscsiaChapEnabled = a});

-- | The network interface identifier of the VTL device.
dscsiaNetworkInterfaceId :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaNetworkInterfaceId = lens _dscsiaNetworkInterfaceId (\ s a -> s{_dscsiaNetworkInterfaceId = a});

-- | The port used to communicate with iSCSI VTL device targets.
dscsiaNetworkInterfacePort :: Lens' DeviceiSCSIAttributes (Maybe Int)
dscsiaNetworkInterfacePort = lens _dscsiaNetworkInterfacePort (\ s a -> s{_dscsiaNetworkInterfacePort = a});

-- | Specifies the unique Amazon Resource Name(ARN) that encodes the iSCSI
-- qualified name(iqn) of a tape drive or media changer target.
dscsiaTargetARN :: Lens' DeviceiSCSIAttributes Text
dscsiaTargetARN = lens _dscsiaTargetARN (\ s a -> s{_dscsiaTargetARN = a});

instance FromJSON DeviceiSCSIAttributes where
        parseJSON
          = withObject "DeviceiSCSIAttributes"
              (\ x ->
                 DeviceiSCSIAttributes' <$>
                   x .:? "ChapEnabled" <*> x .:? "NetworkInterfaceId"
                     <*> x .:? "NetworkInterfacePort"
                     <*> x .: "TargetARN")

-- | /See:/ 'disk' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'disDiskAllocationResource'
--
-- * 'disDiskNode'
--
-- * 'disDiskPath'
--
-- * 'disDiskSizeInBytes'
--
-- * 'disDiskStatus'
--
-- * 'disDiskAllocationType'
--
-- * 'disDiskId'
data Disk = Disk'{_disDiskAllocationResource :: Maybe Text, _disDiskNode :: Maybe Text, _disDiskPath :: Maybe Text, _disDiskSizeInBytes :: Maybe Integer, _disDiskStatus :: Maybe Text, _disDiskAllocationType :: Text, _disDiskId :: Text} deriving (Eq, Read, Show)

-- | 'Disk' smart constructor.
disk :: Text -> Text -> Disk
disk pDiskAllocationType pDiskId = Disk'{_disDiskAllocationResource = Nothing, _disDiskNode = Nothing, _disDiskPath = Nothing, _disDiskSizeInBytes = Nothing, _disDiskStatus = Nothing, _disDiskAllocationType = pDiskAllocationType, _disDiskId = pDiskId};

-- | FIXME: Undocumented member.
disDiskAllocationResource :: Lens' Disk (Maybe Text)
disDiskAllocationResource = lens _disDiskAllocationResource (\ s a -> s{_disDiskAllocationResource = a});

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
disDiskAllocationType :: Lens' Disk Text
disDiskAllocationType = lens _disDiskAllocationType (\ s a -> s{_disDiskAllocationType = a});

-- | FIXME: Undocumented member.
disDiskId :: Lens' Disk Text
disDiskId = lens _disDiskId (\ s a -> s{_disDiskId = a});

instance FromJSON Disk where
        parseJSON
          = withObject "Disk"
              (\ x ->
                 Disk' <$>
                   x .:? "DiskAllocationResource" <*> x .:? "DiskNode"
                     <*> x .:? "DiskPath"
                     <*> x .:? "DiskSizeInBytes"
                     <*> x .:? "DiskStatus"
                     <*> x .: "DiskAllocationType"
                     <*> x .: "DiskId")

-- | /See:/ 'gatewayInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giGatewayARN'
--
-- * 'giGatewayOperationalState'
--
-- * 'giGatewayType'
data GatewayInfo = GatewayInfo'{_giGatewayARN :: Text, _giGatewayOperationalState :: Text, _giGatewayType :: Text} deriving (Eq, Read, Show)

-- | 'GatewayInfo' smart constructor.
gatewayInfo :: Text -> Text -> Text -> GatewayInfo
gatewayInfo pGatewayARN pGatewayOperationalState pGatewayType = GatewayInfo'{_giGatewayARN = pGatewayARN, _giGatewayOperationalState = pGatewayOperationalState, _giGatewayType = pGatewayType};

-- | FIXME: Undocumented member.
giGatewayARN :: Lens' GatewayInfo Text
giGatewayARN = lens _giGatewayARN (\ s a -> s{_giGatewayARN = a});

-- | FIXME: Undocumented member.
giGatewayOperationalState :: Lens' GatewayInfo Text
giGatewayOperationalState = lens _giGatewayOperationalState (\ s a -> s{_giGatewayOperationalState = a});

-- | FIXME: Undocumented member.
giGatewayType :: Lens' GatewayInfo Text
giGatewayType = lens _giGatewayType (\ s a -> s{_giGatewayType = a});

instance FromJSON GatewayInfo where
        parseJSON
          = withObject "GatewayInfo"
              (\ x ->
                 GatewayInfo' <$>
                   x .: "GatewayARN" <*> x .: "GatewayOperationalState"
                     <*> x .: "GatewayType")

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
-- * 'sscsivVolumeiSCSIAttributes'
--
-- * 'sscsivSourceSnapshotId'
--
-- * 'sscsivPreservedExistingData'
--
-- * 'sscsivVolumeProgress'
--
-- * 'sscsivVolumeSizeInBytes'
--
-- * 'sscsivVolumeStatus'
--
-- * 'sscsivVolumeARN'
--
-- * 'sscsivVolumeId'
--
-- * 'sscsivVolumeType'
--
-- * 'sscsivVolumeDiskId'
data StorediSCSIVolume = StorediSCSIVolume'{_sscsivVolumeiSCSIAttributes :: Maybe VolumeiSCSIAttributes, _sscsivSourceSnapshotId :: Maybe Text, _sscsivPreservedExistingData :: Maybe Bool, _sscsivVolumeProgress :: Maybe Double, _sscsivVolumeSizeInBytes :: Maybe Integer, _sscsivVolumeStatus :: Text, _sscsivVolumeARN :: Text, _sscsivVolumeId :: Text, _sscsivVolumeType :: Text, _sscsivVolumeDiskId :: Text} deriving (Eq, Read, Show)

-- | 'StorediSCSIVolume' smart constructor.
storediSCSIVolume :: Text -> Text -> Text -> Text -> Text -> StorediSCSIVolume
storediSCSIVolume pVolumeStatus pVolumeARN pVolumeId pVolumeType pVolumeDiskId = StorediSCSIVolume'{_sscsivVolumeiSCSIAttributes = Nothing, _sscsivSourceSnapshotId = Nothing, _sscsivPreservedExistingData = Nothing, _sscsivVolumeProgress = Nothing, _sscsivVolumeSizeInBytes = Nothing, _sscsivVolumeStatus = pVolumeStatus, _sscsivVolumeARN = pVolumeARN, _sscsivVolumeId = pVolumeId, _sscsivVolumeType = pVolumeType, _sscsivVolumeDiskId = pVolumeDiskId};

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
sscsivVolumeProgress :: Lens' StorediSCSIVolume (Maybe Double)
sscsivVolumeProgress = lens _sscsivVolumeProgress (\ s a -> s{_sscsivVolumeProgress = a});

-- | FIXME: Undocumented member.
sscsivVolumeSizeInBytes :: Lens' StorediSCSIVolume (Maybe Integer)
sscsivVolumeSizeInBytes = lens _sscsivVolumeSizeInBytes (\ s a -> s{_sscsivVolumeSizeInBytes = a});

-- | FIXME: Undocumented member.
sscsivVolumeStatus :: Lens' StorediSCSIVolume Text
sscsivVolumeStatus = lens _sscsivVolumeStatus (\ s a -> s{_sscsivVolumeStatus = a});

-- | FIXME: Undocumented member.
sscsivVolumeARN :: Lens' StorediSCSIVolume Text
sscsivVolumeARN = lens _sscsivVolumeARN (\ s a -> s{_sscsivVolumeARN = a});

-- | FIXME: Undocumented member.
sscsivVolumeId :: Lens' StorediSCSIVolume Text
sscsivVolumeId = lens _sscsivVolumeId (\ s a -> s{_sscsivVolumeId = a});

-- | FIXME: Undocumented member.
sscsivVolumeType :: Lens' StorediSCSIVolume Text
sscsivVolumeType = lens _sscsivVolumeType (\ s a -> s{_sscsivVolumeType = a});

-- | FIXME: Undocumented member.
sscsivVolumeDiskId :: Lens' StorediSCSIVolume Text
sscsivVolumeDiskId = lens _sscsivVolumeDiskId (\ s a -> s{_sscsivVolumeDiskId = a});

instance FromJSON StorediSCSIVolume where
        parseJSON
          = withObject "StorediSCSIVolume"
              (\ x ->
                 StorediSCSIVolume' <$>
                   x .:? "VolumeiSCSIAttributes" <*>
                     x .:? "SourceSnapshotId"
                     <*> x .:? "PreservedExistingData"
                     <*> x .:? "VolumeProgress"
                     <*> x .:? "VolumeSizeInBytes"
                     <*> x .: "VolumeStatus"
                     <*> x .: "VolumeARN"
                     <*> x .: "VolumeId"
                     <*> x .: "VolumeType"
                     <*> x .: "VolumeDiskId")

-- | /See:/ 'tape' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tapTapeStatus'
--
-- * 'tapProgress'
--
-- * 'tapTapeSizeInBytes'
--
-- * 'tapTapeBarcode'
--
-- * 'tapTapeARN'
--
-- * 'tapVTLDevice'
data Tape = Tape'{_tapTapeStatus :: Maybe Text, _tapProgress :: Maybe Double, _tapTapeSizeInBytes :: Maybe Integer, _tapTapeBarcode :: Text, _tapTapeARN :: Text, _tapVTLDevice :: Text} deriving (Eq, Read, Show)

-- | 'Tape' smart constructor.
tape :: Text -> Text -> Text -> Tape
tape pTapeBarcode pTapeARN pVTLDevice = Tape'{_tapTapeStatus = Nothing, _tapProgress = Nothing, _tapTapeSizeInBytes = Nothing, _tapTapeBarcode = pTapeBarcode, _tapTapeARN = pTapeARN, _tapVTLDevice = pVTLDevice};

-- | The current state of the virtual tape.
tapTapeStatus :: Lens' Tape (Maybe Text)
tapTapeStatus = lens _tapTapeStatus (\ s a -> s{_tapTapeStatus = a});

-- | For archiving virtual tapes, indicates how much data remains to be
-- uploaded before archiving is complete.
--
-- Range: 0 (not started) to 100 (complete).
tapProgress :: Lens' Tape (Maybe Double)
tapProgress = lens _tapProgress (\ s a -> s{_tapProgress = a});

-- | The size, in bytes, of the virtual tape.
tapTapeSizeInBytes :: Lens' Tape (Maybe Integer)
tapTapeSizeInBytes = lens _tapTapeSizeInBytes (\ s a -> s{_tapTapeSizeInBytes = a});

-- | The barcode that identifies a specific virtual tape.
tapTapeBarcode :: Lens' Tape Text
tapTapeBarcode = lens _tapTapeBarcode (\ s a -> s{_tapTapeBarcode = a});

-- | The Amazon Resource Name (ARN) of the virtual tape.
tapTapeARN :: Lens' Tape Text
tapTapeARN = lens _tapTapeARN (\ s a -> s{_tapTapeARN = a});

-- | The virtual tape library (VTL) device that the virtual tape is
-- associated with.
tapVTLDevice :: Lens' Tape Text
tapVTLDevice = lens _tapVTLDevice (\ s a -> s{_tapVTLDevice = a});

instance FromJSON Tape where
        parseJSON
          = withObject "Tape"
              (\ x ->
                 Tape' <$>
                   x .:? "TapeStatus" <*> x .:? "Progress" <*>
                     x .:? "TapeSizeInBytes"
                     <*> x .: "TapeBarcode"
                     <*> x .: "TapeARN"
                     <*> x .: "VTLDevice")

-- | /See:/ 'tapeArchive' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'taTapeStatus'
--
-- * 'taTapeSizeInBytes'
--
-- * 'taCompletionTime'
--
-- * 'taTapeBarcode'
--
-- * 'taTapeARN'
--
-- * 'taRetrievedTo'
data TapeArchive = TapeArchive'{_taTapeStatus :: Maybe Text, _taTapeSizeInBytes :: Maybe Integer, _taCompletionTime :: Maybe POSIX, _taTapeBarcode :: Text, _taTapeARN :: Text, _taRetrievedTo :: Text} deriving (Eq, Read, Show)

-- | 'TapeArchive' smart constructor.
tapeArchive :: Text -> Text -> Text -> TapeArchive
tapeArchive pTapeBarcode pTapeARN pRetrievedTo = TapeArchive'{_taTapeStatus = Nothing, _taTapeSizeInBytes = Nothing, _taCompletionTime = Nothing, _taTapeBarcode = pTapeBarcode, _taTapeARN = pTapeARN, _taRetrievedTo = pRetrievedTo};

-- | The current state of the archived virtual tape.
taTapeStatus :: Lens' TapeArchive (Maybe Text)
taTapeStatus = lens _taTapeStatus (\ s a -> s{_taTapeStatus = a});

-- | The size, in bytes, of the archived virtual tape.
taTapeSizeInBytes :: Lens' TapeArchive (Maybe Integer)
taTapeSizeInBytes = lens _taTapeSizeInBytes (\ s a -> s{_taTapeSizeInBytes = a});

-- | The time that the archiving of the virtual tape was completed.
--
-- The string format of the completion time is in the ISO8601 extended
-- YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
taCompletionTime :: Lens' TapeArchive (Maybe UTCTime)
taCompletionTime = lens _taCompletionTime (\ s a -> s{_taCompletionTime = a}) . mapping _Time;

-- | The barcode that identifies the archived virtual tape.
taTapeBarcode :: Lens' TapeArchive Text
taTapeBarcode = lens _taTapeBarcode (\ s a -> s{_taTapeBarcode = a});

-- | The Amazon Resource Name (ARN) of an archived virtual tape.
taTapeARN :: Lens' TapeArchive Text
taTapeARN = lens _taTapeARN (\ s a -> s{_taTapeARN = a});

-- | The Amazon Resource Name (ARN) of the gateway-VTL that the virtual tape
-- is being retrieved to.
--
-- The virtual tape is retrieved from the virtual tape shelf (VTS).
taRetrievedTo :: Lens' TapeArchive Text
taRetrievedTo = lens _taRetrievedTo (\ s a -> s{_taRetrievedTo = a});

instance FromJSON TapeArchive where
        parseJSON
          = withObject "TapeArchive"
              (\ x ->
                 TapeArchive' <$>
                   x .:? "TapeStatus" <*> x .:? "TapeSizeInBytes" <*>
                     x .:? "CompletionTime"
                     <*> x .: "TapeBarcode"
                     <*> x .: "TapeARN"
                     <*> x .: "RetrievedTo")

-- | /See:/ 'tapeRecoveryPointInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'trpiTapeStatus'
--
-- * 'trpiTapeRecoveryPointTime'
--
-- * 'trpiTapeSizeInBytes'
--
-- * 'trpiTapeARN'
data TapeRecoveryPointInfo = TapeRecoveryPointInfo'{_trpiTapeStatus :: Maybe Text, _trpiTapeRecoveryPointTime :: Maybe POSIX, _trpiTapeSizeInBytes :: Maybe Integer, _trpiTapeARN :: Text} deriving (Eq, Read, Show)

-- | 'TapeRecoveryPointInfo' smart constructor.
tapeRecoveryPointInfo :: Text -> TapeRecoveryPointInfo
tapeRecoveryPointInfo pTapeARN = TapeRecoveryPointInfo'{_trpiTapeStatus = Nothing, _trpiTapeRecoveryPointTime = Nothing, _trpiTapeSizeInBytes = Nothing, _trpiTapeARN = pTapeARN};

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

-- | The size, in bytes, of the virtual tapes to recover.
trpiTapeSizeInBytes :: Lens' TapeRecoveryPointInfo (Maybe Integer)
trpiTapeSizeInBytes = lens _trpiTapeSizeInBytes (\ s a -> s{_trpiTapeSizeInBytes = a});

-- | The Amazon Resource Name (ARN) of the virtual tape.
trpiTapeARN :: Lens' TapeRecoveryPointInfo Text
trpiTapeARN = lens _trpiTapeARN (\ s a -> s{_trpiTapeARN = a});

instance FromJSON TapeRecoveryPointInfo where
        parseJSON
          = withObject "TapeRecoveryPointInfo"
              (\ x ->
                 TapeRecoveryPointInfo' <$>
                   x .:? "TapeStatus" <*> x .:? "TapeRecoveryPointTime"
                     <*> x .:? "TapeSizeInBytes"
                     <*> x .: "TapeARN")

-- | /See:/ 'vTLDevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vtldDeviceiSCSIAttributes'
--
-- * 'vtldVTLDeviceVendor'
--
-- * 'vtldVTLDeviceType'
--
-- * 'vtldVTLDeviceProductIdentifier'
--
-- * 'vtldVTLDeviceARN'
data VTLDevice = VTLDevice'{_vtldDeviceiSCSIAttributes :: Maybe DeviceiSCSIAttributes, _vtldVTLDeviceVendor :: Maybe Text, _vtldVTLDeviceType :: Maybe Text, _vtldVTLDeviceProductIdentifier :: Maybe Text, _vtldVTLDeviceARN :: Text} deriving (Eq, Read, Show)

-- | 'VTLDevice' smart constructor.
vTLDevice :: Text -> VTLDevice
vTLDevice pVTLDeviceARN = VTLDevice'{_vtldDeviceiSCSIAttributes = Nothing, _vtldVTLDeviceVendor = Nothing, _vtldVTLDeviceType = Nothing, _vtldVTLDeviceProductIdentifier = Nothing, _vtldVTLDeviceARN = pVTLDeviceARN};

-- | A list of iSCSI information about a VTL device.
vtldDeviceiSCSIAttributes :: Lens' VTLDevice (Maybe DeviceiSCSIAttributes)
vtldDeviceiSCSIAttributes = lens _vtldDeviceiSCSIAttributes (\ s a -> s{_vtldDeviceiSCSIAttributes = a});

-- | FIXME: Undocumented member.
vtldVTLDeviceVendor :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceVendor = lens _vtldVTLDeviceVendor (\ s a -> s{_vtldVTLDeviceVendor = a});

-- | FIXME: Undocumented member.
vtldVTLDeviceType :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceType = lens _vtldVTLDeviceType (\ s a -> s{_vtldVTLDeviceType = a});

-- | FIXME: Undocumented member.
vtldVTLDeviceProductIdentifier :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceProductIdentifier = lens _vtldVTLDeviceProductIdentifier (\ s a -> s{_vtldVTLDeviceProductIdentifier = a});

-- | Specifies the unique Amazon Resource Name (ARN) of the device (tape
-- drive or media changer).
vtldVTLDeviceARN :: Lens' VTLDevice Text
vtldVTLDeviceARN = lens _vtldVTLDeviceARN (\ s a -> s{_vtldVTLDeviceARN = a});

instance FromJSON VTLDevice where
        parseJSON
          = withObject "VTLDevice"
              (\ x ->
                 VTLDevice' <$>
                   x .:? "DeviceiSCSIAttributes" <*>
                     x .:? "VTLDeviceVendor"
                     <*> x .:? "VTLDeviceType"
                     <*> x .:? "VTLDeviceProductIdentifier"
                     <*> x .: "VTLDeviceARN")

-- | /See:/ 'volumeInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'viVolumeARN'
--
-- * 'viVolumeType'
data VolumeInfo = VolumeInfo'{_viVolumeARN :: Text, _viVolumeType :: Text} deriving (Eq, Read, Show)

-- | 'VolumeInfo' smart constructor.
volumeInfo :: Text -> Text -> VolumeInfo
volumeInfo pVolumeARN pVolumeType = VolumeInfo'{_viVolumeARN = pVolumeARN, _viVolumeType = pVolumeType};

-- | FIXME: Undocumented member.
viVolumeARN :: Lens' VolumeInfo Text
viVolumeARN = lens _viVolumeARN (\ s a -> s{_viVolumeARN = a});

-- | FIXME: Undocumented member.
viVolumeType :: Lens' VolumeInfo Text
viVolumeType = lens _viVolumeType (\ s a -> s{_viVolumeType = a});

instance FromJSON VolumeInfo where
        parseJSON
          = withObject "VolumeInfo"
              (\ x ->
                 VolumeInfo' <$>
                   x .: "VolumeARN" <*> x .: "VolumeType")

-- | /See:/ 'volumeRecoveryPointInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vrpiVolumeRecoveryPointTime'
--
-- * 'vrpiVolumeSizeInBytes'
--
-- * 'vrpiVolumeUsageInBytes'
--
-- * 'vrpiVolumeARN'
data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo'{_vrpiVolumeRecoveryPointTime :: Maybe Text, _vrpiVolumeSizeInBytes :: Maybe Integer, _vrpiVolumeUsageInBytes :: Maybe Integer, _vrpiVolumeARN :: Text} deriving (Eq, Read, Show)

-- | 'VolumeRecoveryPointInfo' smart constructor.
volumeRecoveryPointInfo :: Text -> VolumeRecoveryPointInfo
volumeRecoveryPointInfo pVolumeARN = VolumeRecoveryPointInfo'{_vrpiVolumeRecoveryPointTime = Nothing, _vrpiVolumeSizeInBytes = Nothing, _vrpiVolumeUsageInBytes = Nothing, _vrpiVolumeARN = pVolumeARN};

-- | FIXME: Undocumented member.
vrpiVolumeRecoveryPointTime :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpiVolumeRecoveryPointTime = lens _vrpiVolumeRecoveryPointTime (\ s a -> s{_vrpiVolumeRecoveryPointTime = a});

-- | FIXME: Undocumented member.
vrpiVolumeSizeInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpiVolumeSizeInBytes = lens _vrpiVolumeSizeInBytes (\ s a -> s{_vrpiVolumeSizeInBytes = a});

-- | FIXME: Undocumented member.
vrpiVolumeUsageInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpiVolumeUsageInBytes = lens _vrpiVolumeUsageInBytes (\ s a -> s{_vrpiVolumeUsageInBytes = a});

-- | FIXME: Undocumented member.
vrpiVolumeARN :: Lens' VolumeRecoveryPointInfo Text
vrpiVolumeARN = lens _vrpiVolumeARN (\ s a -> s{_vrpiVolumeARN = a});

instance FromJSON VolumeRecoveryPointInfo where
        parseJSON
          = withObject "VolumeRecoveryPointInfo"
              (\ x ->
                 VolumeRecoveryPointInfo' <$>
                   x .:? "VolumeRecoveryPointTime" <*>
                     x .:? "VolumeSizeInBytes"
                     <*> x .:? "VolumeUsageInBytes"
                     <*> x .: "VolumeARN")

-- | /See:/ 'volumeiSCSIAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vscsiaChapEnabled'
--
-- * 'vscsiaNetworkInterfaceId'
--
-- * 'vscsiaNetworkInterfacePort'
--
-- * 'vscsiaLunNumber'
--
-- * 'vscsiaTargetARN'
data VolumeiSCSIAttributes = VolumeiSCSIAttributes'{_vscsiaChapEnabled :: Maybe Bool, _vscsiaNetworkInterfaceId :: Maybe Text, _vscsiaNetworkInterfacePort :: Maybe Int, _vscsiaLunNumber :: Nat, _vscsiaTargetARN :: Text} deriving (Eq, Read, Show)

-- | 'VolumeiSCSIAttributes' smart constructor.
volumeiSCSIAttributes :: Natural -> Text -> VolumeiSCSIAttributes
volumeiSCSIAttributes pLunNumber pTargetARN = VolumeiSCSIAttributes'{_vscsiaChapEnabled = Nothing, _vscsiaNetworkInterfaceId = Nothing, _vscsiaNetworkInterfacePort = Nothing, _vscsiaLunNumber = _Nat # pLunNumber, _vscsiaTargetARN = pTargetARN};

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
vscsiaChapEnabled :: Lens' VolumeiSCSIAttributes (Maybe Bool)
vscsiaChapEnabled = lens _vscsiaChapEnabled (\ s a -> s{_vscsiaChapEnabled = a});

-- | The network interface identifier.
vscsiaNetworkInterfaceId :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaNetworkInterfaceId = lens _vscsiaNetworkInterfaceId (\ s a -> s{_vscsiaNetworkInterfaceId = a});

-- | The port used to communicate with iSCSI targets.
vscsiaNetworkInterfacePort :: Lens' VolumeiSCSIAttributes (Maybe Int)
vscsiaNetworkInterfacePort = lens _vscsiaNetworkInterfacePort (\ s a -> s{_vscsiaNetworkInterfacePort = a});

-- | The logical disk number.
vscsiaLunNumber :: Lens' VolumeiSCSIAttributes Natural
vscsiaLunNumber = lens _vscsiaLunNumber (\ s a -> s{_vscsiaLunNumber = a}) . _Nat;

-- | The Amazon Resource Name (ARN) of the volume target.
vscsiaTargetARN :: Lens' VolumeiSCSIAttributes Text
vscsiaTargetARN = lens _vscsiaTargetARN (\ s a -> s{_vscsiaTargetARN = a});

instance FromJSON VolumeiSCSIAttributes where
        parseJSON
          = withObject "VolumeiSCSIAttributes"
              (\ x ->
                 VolumeiSCSIAttributes' <$>
                   x .:? "ChapEnabled" <*> x .:? "NetworkInterfaceId"
                     <*> x .:? "NetworkInterfacePort"
                     <*> x .: "LunNumber"
                     <*> x .: "TargetARN")
