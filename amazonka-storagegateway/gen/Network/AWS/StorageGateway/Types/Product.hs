{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.Product where

import           Network.AWS.Prelude
import           Network.AWS.StorageGateway.Types.Sum

-- | /See:/ 'cachediSCSIVolume' smart constructor.
data CachediSCSIVolume = CachediSCSIVolume'
    { _cscsivVolumeiSCSIAttributes :: !(Maybe VolumeiSCSIAttributes)
    , _cscsivVolumeStatus          :: !(Maybe Text)
    , _cscsivSourceSnapshotId      :: !(Maybe Text)
    , _cscsivVolumeARN             :: !(Maybe Text)
    , _cscsivVolumeProgress        :: !(Maybe Double)
    , _cscsivVolumeSizeInBytes     :: !(Maybe Integer)
    , _cscsivVolumeId              :: !(Maybe Text)
    , _cscsivVolumeType            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CachediSCSIVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscsivVolumeiSCSIAttributes'
--
-- * 'cscsivVolumeStatus'
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
cachediSCSIVolume
    :: CachediSCSIVolume
cachediSCSIVolume =
    CachediSCSIVolume'
    { _cscsivVolumeiSCSIAttributes = Nothing
    , _cscsivVolumeStatus = Nothing
    , _cscsivSourceSnapshotId = Nothing
    , _cscsivVolumeARN = Nothing
    , _cscsivVolumeProgress = Nothing
    , _cscsivVolumeSizeInBytes = Nothing
    , _cscsivVolumeId = Nothing
    , _cscsivVolumeType = Nothing
    }

-- | Undocumented member.
cscsivVolumeiSCSIAttributes :: Lens' CachediSCSIVolume (Maybe VolumeiSCSIAttributes)
cscsivVolumeiSCSIAttributes = lens _cscsivVolumeiSCSIAttributes (\ s a -> s{_cscsivVolumeiSCSIAttributes = a});

-- | Undocumented member.
cscsivVolumeStatus :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeStatus = lens _cscsivVolumeStatus (\ s a -> s{_cscsivVolumeStatus = a});

-- | Undocumented member.
cscsivSourceSnapshotId :: Lens' CachediSCSIVolume (Maybe Text)
cscsivSourceSnapshotId = lens _cscsivSourceSnapshotId (\ s a -> s{_cscsivSourceSnapshotId = a});

-- | Undocumented member.
cscsivVolumeARN :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeARN = lens _cscsivVolumeARN (\ s a -> s{_cscsivVolumeARN = a});

-- | Undocumented member.
cscsivVolumeProgress :: Lens' CachediSCSIVolume (Maybe Double)
cscsivVolumeProgress = lens _cscsivVolumeProgress (\ s a -> s{_cscsivVolumeProgress = a});

-- | Undocumented member.
cscsivVolumeSizeInBytes :: Lens' CachediSCSIVolume (Maybe Integer)
cscsivVolumeSizeInBytes = lens _cscsivVolumeSizeInBytes (\ s a -> s{_cscsivVolumeSizeInBytes = a});

-- | Undocumented member.
cscsivVolumeId :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeId = lens _cscsivVolumeId (\ s a -> s{_cscsivVolumeId = a});

-- | Undocumented member.
cscsivVolumeType :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeType = lens _cscsivVolumeType (\ s a -> s{_cscsivVolumeType = a});

instance FromJSON CachediSCSIVolume where
        parseJSON
          = withObject "CachediSCSIVolume"
              (\ x ->
                 CachediSCSIVolume' <$>
                   (x .:? "VolumeiSCSIAttributes") <*>
                     (x .:? "VolumeStatus")
                     <*> (x .:? "SourceSnapshotId")
                     <*> (x .:? "VolumeARN")
                     <*> (x .:? "VolumeProgress")
                     <*> (x .:? "VolumeSizeInBytes")
                     <*> (x .:? "VolumeId")
                     <*> (x .:? "VolumeType"))

-- | Describes Challenge-Handshake Authentication Protocol (CHAP) information
-- that supports authentication between your gateway and iSCSI initiators.
--
-- /See:/ 'chapInfo' smart constructor.
data ChapInfo = ChapInfo'
    { _ciTargetARN                     :: !(Maybe Text)
    , _ciSecretToAuthenticateInitiator :: !(Maybe Text)
    , _ciInitiatorName                 :: !(Maybe Text)
    , _ciSecretToAuthenticateTarget    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChapInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciTargetARN'
--
-- * 'ciSecretToAuthenticateInitiator'
--
-- * 'ciInitiatorName'
--
-- * 'ciSecretToAuthenticateTarget'
chapInfo
    :: ChapInfo
chapInfo =
    ChapInfo'
    { _ciTargetARN = Nothing
    , _ciSecretToAuthenticateInitiator = Nothing
    , _ciInitiatorName = Nothing
    , _ciSecretToAuthenticateTarget = Nothing
    }

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
                   (x .:? "TargetARN") <*>
                     (x .:? "SecretToAuthenticateInitiator")
                     <*> (x .:? "InitiatorName")
                     <*> (x .:? "SecretToAuthenticateTarget"))

-- | Lists iSCSI information about a VTL device.
--
-- /See:/ 'deviceiSCSIAttributes' smart constructor.
data DeviceiSCSIAttributes = DeviceiSCSIAttributes'
    { _dscsiaTargetARN            :: !(Maybe Text)
    , _dscsiaChapEnabled          :: !(Maybe Bool)
    , _dscsiaNetworkInterfaceId   :: !(Maybe Text)
    , _dscsiaNetworkInterfacePort :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeviceiSCSIAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscsiaTargetARN'
--
-- * 'dscsiaChapEnabled'
--
-- * 'dscsiaNetworkInterfaceId'
--
-- * 'dscsiaNetworkInterfacePort'
deviceiSCSIAttributes
    :: DeviceiSCSIAttributes
deviceiSCSIAttributes =
    DeviceiSCSIAttributes'
    { _dscsiaTargetARN = Nothing
    , _dscsiaChapEnabled = Nothing
    , _dscsiaNetworkInterfaceId = Nothing
    , _dscsiaNetworkInterfacePort = Nothing
    }

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
                   (x .:? "TargetARN") <*> (x .:? "ChapEnabled") <*>
                     (x .:? "NetworkInterfaceId")
                     <*> (x .:? "NetworkInterfacePort"))

-- | /See:/ 'disk' smart constructor.
data Disk = Disk'
    { _dDiskAllocationResource :: !(Maybe Text)
    , _dDiskAllocationType     :: !(Maybe Text)
    , _dDiskNode               :: !(Maybe Text)
    , _dDiskPath               :: !(Maybe Text)
    , _dDiskSizeInBytes        :: !(Maybe Integer)
    , _dDiskStatus             :: !(Maybe Text)
    , _dDiskId                 :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Disk' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDiskAllocationResource'
--
-- * 'dDiskAllocationType'
--
-- * 'dDiskNode'
--
-- * 'dDiskPath'
--
-- * 'dDiskSizeInBytes'
--
-- * 'dDiskStatus'
--
-- * 'dDiskId'
disk
    :: Disk
disk =
    Disk'
    { _dDiskAllocationResource = Nothing
    , _dDiskAllocationType = Nothing
    , _dDiskNode = Nothing
    , _dDiskPath = Nothing
    , _dDiskSizeInBytes = Nothing
    , _dDiskStatus = Nothing
    , _dDiskId = Nothing
    }

-- | Undocumented member.
dDiskAllocationResource :: Lens' Disk (Maybe Text)
dDiskAllocationResource = lens _dDiskAllocationResource (\ s a -> s{_dDiskAllocationResource = a});

-- | Undocumented member.
dDiskAllocationType :: Lens' Disk (Maybe Text)
dDiskAllocationType = lens _dDiskAllocationType (\ s a -> s{_dDiskAllocationType = a});

-- | Undocumented member.
dDiskNode :: Lens' Disk (Maybe Text)
dDiskNode = lens _dDiskNode (\ s a -> s{_dDiskNode = a});

-- | Undocumented member.
dDiskPath :: Lens' Disk (Maybe Text)
dDiskPath = lens _dDiskPath (\ s a -> s{_dDiskPath = a});

-- | Undocumented member.
dDiskSizeInBytes :: Lens' Disk (Maybe Integer)
dDiskSizeInBytes = lens _dDiskSizeInBytes (\ s a -> s{_dDiskSizeInBytes = a});

-- | Undocumented member.
dDiskStatus :: Lens' Disk (Maybe Text)
dDiskStatus = lens _dDiskStatus (\ s a -> s{_dDiskStatus = a});

-- | Undocumented member.
dDiskId :: Lens' Disk (Maybe Text)
dDiskId = lens _dDiskId (\ s a -> s{_dDiskId = a});

instance FromJSON Disk where
        parseJSON
          = withObject "Disk"
              (\ x ->
                 Disk' <$>
                   (x .:? "DiskAllocationResource") <*>
                     (x .:? "DiskAllocationType")
                     <*> (x .:? "DiskNode")
                     <*> (x .:? "DiskPath")
                     <*> (x .:? "DiskSizeInBytes")
                     <*> (x .:? "DiskStatus")
                     <*> (x .:? "DiskId"))

-- | /See:/ 'gatewayInfo' smart constructor.
data GatewayInfo = GatewayInfo'
    { _giGatewayARN              :: !(Maybe Text)
    , _giGatewayOperationalState :: !(Maybe Text)
    , _giGatewayName             :: !(Maybe Text)
    , _giGatewayType             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GatewayInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giGatewayARN'
--
-- * 'giGatewayOperationalState'
--
-- * 'giGatewayName'
--
-- * 'giGatewayType'
gatewayInfo
    :: GatewayInfo
gatewayInfo =
    GatewayInfo'
    { _giGatewayARN = Nothing
    , _giGatewayOperationalState = Nothing
    , _giGatewayName = Nothing
    , _giGatewayType = Nothing
    }

-- | Undocumented member.
giGatewayARN :: Lens' GatewayInfo (Maybe Text)
giGatewayARN = lens _giGatewayARN (\ s a -> s{_giGatewayARN = a});

-- | Undocumented member.
giGatewayOperationalState :: Lens' GatewayInfo (Maybe Text)
giGatewayOperationalState = lens _giGatewayOperationalState (\ s a -> s{_giGatewayOperationalState = a});

-- | Undocumented member.
giGatewayName :: Lens' GatewayInfo (Maybe Text)
giGatewayName = lens _giGatewayName (\ s a -> s{_giGatewayName = a});

-- | Undocumented member.
giGatewayType :: Lens' GatewayInfo (Maybe Text)
giGatewayType = lens _giGatewayType (\ s a -> s{_giGatewayType = a});

instance FromJSON GatewayInfo where
        parseJSON
          = withObject "GatewayInfo"
              (\ x ->
                 GatewayInfo' <$>
                   (x .:? "GatewayARN") <*>
                     (x .:? "GatewayOperationalState")
                     <*> (x .:? "GatewayName")
                     <*> (x .:? "GatewayType"))

-- | Describes a gateway\'s network interface.
--
-- /See:/ 'networkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
    { _niIPv6Address :: !(Maybe Text)
    , _niMACAddress  :: !(Maybe Text)
    , _niIPv4Address :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niIPv6Address'
--
-- * 'niMACAddress'
--
-- * 'niIPv4Address'
networkInterface
    :: NetworkInterface
networkInterface =
    NetworkInterface'
    { _niIPv6Address = Nothing
    , _niMACAddress = Nothing
    , _niIPv4Address = Nothing
    }

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
                   (x .:? "Ipv6Address") <*> (x .:? "MacAddress") <*>
                     (x .:? "Ipv4Address"))

-- | /See:/ 'storediSCSIVolume' smart constructor.
data StorediSCSIVolume = StorediSCSIVolume'
    { _sscsivVolumeiSCSIAttributes :: !(Maybe VolumeiSCSIAttributes)
    , _sscsivVolumeStatus          :: !(Maybe Text)
    , _sscsivSourceSnapshotId      :: !(Maybe Text)
    , _sscsivPreservedExistingData :: !(Maybe Bool)
    , _sscsivVolumeARN             :: !(Maybe Text)
    , _sscsivVolumeProgress        :: !(Maybe Double)
    , _sscsivVolumeSizeInBytes     :: !(Maybe Integer)
    , _sscsivVolumeId              :: !(Maybe Text)
    , _sscsivVolumeDiskId          :: !(Maybe Text)
    , _sscsivVolumeType            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StorediSCSIVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sscsivVolumeiSCSIAttributes'
--
-- * 'sscsivVolumeStatus'
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
-- * 'sscsivVolumeDiskId'
--
-- * 'sscsivVolumeType'
storediSCSIVolume
    :: StorediSCSIVolume
storediSCSIVolume =
    StorediSCSIVolume'
    { _sscsivVolumeiSCSIAttributes = Nothing
    , _sscsivVolumeStatus = Nothing
    , _sscsivSourceSnapshotId = Nothing
    , _sscsivPreservedExistingData = Nothing
    , _sscsivVolumeARN = Nothing
    , _sscsivVolumeProgress = Nothing
    , _sscsivVolumeSizeInBytes = Nothing
    , _sscsivVolumeId = Nothing
    , _sscsivVolumeDiskId = Nothing
    , _sscsivVolumeType = Nothing
    }

-- | Undocumented member.
sscsivVolumeiSCSIAttributes :: Lens' StorediSCSIVolume (Maybe VolumeiSCSIAttributes)
sscsivVolumeiSCSIAttributes = lens _sscsivVolumeiSCSIAttributes (\ s a -> s{_sscsivVolumeiSCSIAttributes = a});

-- | Undocumented member.
sscsivVolumeStatus :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeStatus = lens _sscsivVolumeStatus (\ s a -> s{_sscsivVolumeStatus = a});

-- | Undocumented member.
sscsivSourceSnapshotId :: Lens' StorediSCSIVolume (Maybe Text)
sscsivSourceSnapshotId = lens _sscsivSourceSnapshotId (\ s a -> s{_sscsivSourceSnapshotId = a});

-- | Undocumented member.
sscsivPreservedExistingData :: Lens' StorediSCSIVolume (Maybe Bool)
sscsivPreservedExistingData = lens _sscsivPreservedExistingData (\ s a -> s{_sscsivPreservedExistingData = a});

-- | Undocumented member.
sscsivVolumeARN :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeARN = lens _sscsivVolumeARN (\ s a -> s{_sscsivVolumeARN = a});

-- | Undocumented member.
sscsivVolumeProgress :: Lens' StorediSCSIVolume (Maybe Double)
sscsivVolumeProgress = lens _sscsivVolumeProgress (\ s a -> s{_sscsivVolumeProgress = a});

-- | Undocumented member.
sscsivVolumeSizeInBytes :: Lens' StorediSCSIVolume (Maybe Integer)
sscsivVolumeSizeInBytes = lens _sscsivVolumeSizeInBytes (\ s a -> s{_sscsivVolumeSizeInBytes = a});

-- | Undocumented member.
sscsivVolumeId :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeId = lens _sscsivVolumeId (\ s a -> s{_sscsivVolumeId = a});

-- | Undocumented member.
sscsivVolumeDiskId :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeDiskId = lens _sscsivVolumeDiskId (\ s a -> s{_sscsivVolumeDiskId = a});

-- | Undocumented member.
sscsivVolumeType :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeType = lens _sscsivVolumeType (\ s a -> s{_sscsivVolumeType = a});

instance FromJSON StorediSCSIVolume where
        parseJSON
          = withObject "StorediSCSIVolume"
              (\ x ->
                 StorediSCSIVolume' <$>
                   (x .:? "VolumeiSCSIAttributes") <*>
                     (x .:? "VolumeStatus")
                     <*> (x .:? "SourceSnapshotId")
                     <*> (x .:? "PreservedExistingData")
                     <*> (x .:? "VolumeARN")
                     <*> (x .:? "VolumeProgress")
                     <*> (x .:? "VolumeSizeInBytes")
                     <*> (x .:? "VolumeId")
                     <*> (x .:? "VolumeDiskId")
                     <*> (x .:? "VolumeType"))

-- | /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagKey   :: !Text
    , _tagValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey'
--
-- * 'tagValue'
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ =
    Tag'
    { _tagKey = pKey_
    , _tagValue = pValue_
    }

-- | Undocumented member.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

-- | Undocumented member.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _tagKey),
                  Just ("Value" .= _tagValue)])

-- | Describes a virtual tape object.
--
-- /See:/ 'tape' smart constructor.
data Tape = Tape'
    { _tTapeBarcode     :: !(Maybe Text)
    , _tTapeStatus      :: !(Maybe Text)
    , _tTapeARN         :: !(Maybe Text)
    , _tProgress        :: !(Maybe Double)
    , _tTapeSizeInBytes :: !(Maybe Integer)
    , _tVTLDevice       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tape' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTapeBarcode'
--
-- * 'tTapeStatus'
--
-- * 'tTapeARN'
--
-- * 'tProgress'
--
-- * 'tTapeSizeInBytes'
--
-- * 'tVTLDevice'
tape
    :: Tape
tape =
    Tape'
    { _tTapeBarcode = Nothing
    , _tTapeStatus = Nothing
    , _tTapeARN = Nothing
    , _tProgress = Nothing
    , _tTapeSizeInBytes = Nothing
    , _tVTLDevice = Nothing
    }

-- | The barcode that identifies a specific virtual tape.
tTapeBarcode :: Lens' Tape (Maybe Text)
tTapeBarcode = lens _tTapeBarcode (\ s a -> s{_tTapeBarcode = a});

-- | The current state of the virtual tape.
tTapeStatus :: Lens' Tape (Maybe Text)
tTapeStatus = lens _tTapeStatus (\ s a -> s{_tTapeStatus = a});

-- | The Amazon Resource Name (ARN) of the virtual tape.
tTapeARN :: Lens' Tape (Maybe Text)
tTapeARN = lens _tTapeARN (\ s a -> s{_tTapeARN = a});

-- | For archiving virtual tapes, indicates how much data remains to be
-- uploaded before archiving is complete.
--
-- Range: 0 (not started) to 100 (complete).
tProgress :: Lens' Tape (Maybe Double)
tProgress = lens _tProgress (\ s a -> s{_tProgress = a});

-- | The size, in bytes, of the virtual tape.
tTapeSizeInBytes :: Lens' Tape (Maybe Integer)
tTapeSizeInBytes = lens _tTapeSizeInBytes (\ s a -> s{_tTapeSizeInBytes = a});

-- | The virtual tape library (VTL) device that the virtual tape is
-- associated with.
tVTLDevice :: Lens' Tape (Maybe Text)
tVTLDevice = lens _tVTLDevice (\ s a -> s{_tVTLDevice = a});

instance FromJSON Tape where
        parseJSON
          = withObject "Tape"
              (\ x ->
                 Tape' <$>
                   (x .:? "TapeBarcode") <*> (x .:? "TapeStatus") <*>
                     (x .:? "TapeARN")
                     <*> (x .:? "Progress")
                     <*> (x .:? "TapeSizeInBytes")
                     <*> (x .:? "VTLDevice"))

-- | Represents a virtual tape that is archived in the virtual tape shelf
-- (VTS).
--
-- /See:/ 'tapeArchive' smart constructor.
data TapeArchive = TapeArchive'
    { _taTapeBarcode     :: !(Maybe Text)
    , _taTapeStatus      :: !(Maybe Text)
    , _taTapeARN         :: !(Maybe Text)
    , _taTapeSizeInBytes :: !(Maybe Integer)
    , _taCompletionTime  :: !(Maybe POSIX)
    , _taRetrievedTo     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TapeArchive' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
tapeArchive
    :: TapeArchive
tapeArchive =
    TapeArchive'
    { _taTapeBarcode = Nothing
    , _taTapeStatus = Nothing
    , _taTapeARN = Nothing
    , _taTapeSizeInBytes = Nothing
    , _taCompletionTime = Nothing
    , _taRetrievedTo = Nothing
    }

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
                   (x .:? "TapeBarcode") <*> (x .:? "TapeStatus") <*>
                     (x .:? "TapeARN")
                     <*> (x .:? "TapeSizeInBytes")
                     <*> (x .:? "CompletionTime")
                     <*> (x .:? "RetrievedTo"))

-- | Describes a recovery point.
--
-- /See:/ 'tapeRecoveryPointInfo' smart constructor.
data TapeRecoveryPointInfo = TapeRecoveryPointInfo'
    { _trpiTapeStatus            :: !(Maybe Text)
    , _trpiTapeRecoveryPointTime :: !(Maybe POSIX)
    , _trpiTapeARN               :: !(Maybe Text)
    , _trpiTapeSizeInBytes       :: !(Maybe Integer)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TapeRecoveryPointInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trpiTapeStatus'
--
-- * 'trpiTapeRecoveryPointTime'
--
-- * 'trpiTapeARN'
--
-- * 'trpiTapeSizeInBytes'
tapeRecoveryPointInfo
    :: TapeRecoveryPointInfo
tapeRecoveryPointInfo =
    TapeRecoveryPointInfo'
    { _trpiTapeStatus = Nothing
    , _trpiTapeRecoveryPointTime = Nothing
    , _trpiTapeARN = Nothing
    , _trpiTapeSizeInBytes = Nothing
    }

-- | Undocumented member.
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
                   (x .:? "TapeStatus") <*>
                     (x .:? "TapeRecoveryPointTime")
                     <*> (x .:? "TapeARN")
                     <*> (x .:? "TapeSizeInBytes"))

-- | Represents a device object associated with a gateway-VTL.
--
-- /See:/ 'vTLDevice' smart constructor.
data VTLDevice = VTLDevice'
    { _vtldDeviceiSCSIAttributes      :: !(Maybe DeviceiSCSIAttributes)
    , _vtldVTLDeviceVendor            :: !(Maybe Text)
    , _vtldVTLDeviceARN               :: !(Maybe Text)
    , _vtldVTLDeviceType              :: !(Maybe Text)
    , _vtldVTLDeviceProductIdentifier :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VTLDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
vTLDevice
    :: VTLDevice
vTLDevice =
    VTLDevice'
    { _vtldDeviceiSCSIAttributes = Nothing
    , _vtldVTLDeviceVendor = Nothing
    , _vtldVTLDeviceARN = Nothing
    , _vtldVTLDeviceType = Nothing
    , _vtldVTLDeviceProductIdentifier = Nothing
    }

-- | A list of iSCSI information about a VTL device.
vtldDeviceiSCSIAttributes :: Lens' VTLDevice (Maybe DeviceiSCSIAttributes)
vtldDeviceiSCSIAttributes = lens _vtldDeviceiSCSIAttributes (\ s a -> s{_vtldDeviceiSCSIAttributes = a});

-- | Undocumented member.
vtldVTLDeviceVendor :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceVendor = lens _vtldVTLDeviceVendor (\ s a -> s{_vtldVTLDeviceVendor = a});

-- | Specifies the unique Amazon Resource Name (ARN) of the device (tape
-- drive or media changer).
vtldVTLDeviceARN :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceARN = lens _vtldVTLDeviceARN (\ s a -> s{_vtldVTLDeviceARN = a});

-- | Undocumented member.
vtldVTLDeviceType :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceType = lens _vtldVTLDeviceType (\ s a -> s{_vtldVTLDeviceType = a});

-- | Undocumented member.
vtldVTLDeviceProductIdentifier :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceProductIdentifier = lens _vtldVTLDeviceProductIdentifier (\ s a -> s{_vtldVTLDeviceProductIdentifier = a});

instance FromJSON VTLDevice where
        parseJSON
          = withObject "VTLDevice"
              (\ x ->
                 VTLDevice' <$>
                   (x .:? "DeviceiSCSIAttributes") <*>
                     (x .:? "VTLDeviceVendor")
                     <*> (x .:? "VTLDeviceARN")
                     <*> (x .:? "VTLDeviceType")
                     <*> (x .:? "VTLDeviceProductIdentifier"))

-- | /See:/ 'volumeInfo' smart constructor.
data VolumeInfo = VolumeInfo'
    { _viVolumeARN  :: !(Maybe Text)
    , _viVolumeType :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VolumeInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'viVolumeARN'
--
-- * 'viVolumeType'
volumeInfo
    :: VolumeInfo
volumeInfo =
    VolumeInfo'
    { _viVolumeARN = Nothing
    , _viVolumeType = Nothing
    }

-- | Undocumented member.
viVolumeARN :: Lens' VolumeInfo (Maybe Text)
viVolumeARN = lens _viVolumeARN (\ s a -> s{_viVolumeARN = a});

-- | Undocumented member.
viVolumeType :: Lens' VolumeInfo (Maybe Text)
viVolumeType = lens _viVolumeType (\ s a -> s{_viVolumeType = a});

instance FromJSON VolumeInfo where
        parseJSON
          = withObject "VolumeInfo"
              (\ x ->
                 VolumeInfo' <$>
                   (x .:? "VolumeARN") <*> (x .:? "VolumeType"))

-- | /See:/ 'volumeRecoveryPointInfo' smart constructor.
data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo'
    { _vrpiVolumeRecoveryPointTime :: !(Maybe Text)
    , _vrpiVolumeARN               :: !(Maybe Text)
    , _vrpiVolumeSizeInBytes       :: !(Maybe Integer)
    , _vrpiVolumeUsageInBytes      :: !(Maybe Integer)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VolumeRecoveryPointInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vrpiVolumeRecoveryPointTime'
--
-- * 'vrpiVolumeARN'
--
-- * 'vrpiVolumeSizeInBytes'
--
-- * 'vrpiVolumeUsageInBytes'
volumeRecoveryPointInfo
    :: VolumeRecoveryPointInfo
volumeRecoveryPointInfo =
    VolumeRecoveryPointInfo'
    { _vrpiVolumeRecoveryPointTime = Nothing
    , _vrpiVolumeARN = Nothing
    , _vrpiVolumeSizeInBytes = Nothing
    , _vrpiVolumeUsageInBytes = Nothing
    }

-- | Undocumented member.
vrpiVolumeRecoveryPointTime :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpiVolumeRecoveryPointTime = lens _vrpiVolumeRecoveryPointTime (\ s a -> s{_vrpiVolumeRecoveryPointTime = a});

-- | Undocumented member.
vrpiVolumeARN :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpiVolumeARN = lens _vrpiVolumeARN (\ s a -> s{_vrpiVolumeARN = a});

-- | Undocumented member.
vrpiVolumeSizeInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpiVolumeSizeInBytes = lens _vrpiVolumeSizeInBytes (\ s a -> s{_vrpiVolumeSizeInBytes = a});

-- | Undocumented member.
vrpiVolumeUsageInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpiVolumeUsageInBytes = lens _vrpiVolumeUsageInBytes (\ s a -> s{_vrpiVolumeUsageInBytes = a});

instance FromJSON VolumeRecoveryPointInfo where
        parseJSON
          = withObject "VolumeRecoveryPointInfo"
              (\ x ->
                 VolumeRecoveryPointInfo' <$>
                   (x .:? "VolumeRecoveryPointTime") <*>
                     (x .:? "VolumeARN")
                     <*> (x .:? "VolumeSizeInBytes")
                     <*> (x .:? "VolumeUsageInBytes"))

-- | Lists iSCSI information about a volume.
--
-- /See:/ 'volumeiSCSIAttributes' smart constructor.
data VolumeiSCSIAttributes = VolumeiSCSIAttributes'
    { _vscsiaLunNumber            :: !(Maybe Nat)
    , _vscsiaTargetARN            :: !(Maybe Text)
    , _vscsiaChapEnabled          :: !(Maybe Bool)
    , _vscsiaNetworkInterfaceId   :: !(Maybe Text)
    , _vscsiaNetworkInterfacePort :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VolumeiSCSIAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
volumeiSCSIAttributes
    :: VolumeiSCSIAttributes
volumeiSCSIAttributes =
    VolumeiSCSIAttributes'
    { _vscsiaLunNumber = Nothing
    , _vscsiaTargetARN = Nothing
    , _vscsiaChapEnabled = Nothing
    , _vscsiaNetworkInterfaceId = Nothing
    , _vscsiaNetworkInterfacePort = Nothing
    }

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
                   (x .:? "LunNumber") <*> (x .:? "TargetARN") <*>
                     (x .:? "ChapEnabled")
                     <*> (x .:? "NetworkInterfaceId")
                     <*> (x .:? "NetworkInterfacePort"))
