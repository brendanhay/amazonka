{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types.Sum

-- | Describes an iSCSI cached volume.
--
--
--
-- /See:/ 'cachediSCSIVolume' smart constructor.
data CachediSCSIVolume = CachediSCSIVolume'
  { _cscsivVolumeiSCSIAttributes :: !(Maybe VolumeiSCSIAttributes)
  , _cscsivVolumeStatus          :: !(Maybe Text)
  , _cscsivSourceSnapshotId      :: !(Maybe Text)
  , _cscsivVolumeARN             :: !(Maybe Text)
  , _cscsivVolumeProgress        :: !(Maybe Double)
  , _cscsivVolumeSizeInBytes     :: !(Maybe Integer)
  , _cscsivVolumeUsedInBytes     :: !(Maybe Integer)
  , _cscsivCreatedDate           :: !(Maybe POSIX)
  , _cscsivVolumeId              :: !(Maybe Text)
  , _cscsivVolumeType            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CachediSCSIVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscsivVolumeiSCSIAttributes' - An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
--
-- * 'cscsivVolumeStatus' - One of the VolumeStatus values that indicates the state of the storage volume.
--
-- * 'cscsivSourceSnapshotId' - If the cached volume was created from a snapshot, this field contains the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not included.
--
-- * 'cscsivVolumeARN' - The Amazon Resource Name (ARN) of the storage volume.
--
-- * 'cscsivVolumeProgress' - Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the cached volume is not restoring or bootstrapping.
--
-- * 'cscsivVolumeSizeInBytes' - The size, in bytes, of the volume capacity.
--
-- * 'cscsivVolumeUsedInBytes' - The size of the data stored on the volume in bytes.
--
-- * 'cscsivCreatedDate' - The date the volume was created. Volumes created prior to March 28, 2017 don’t have this time stamp.
--
-- * 'cscsivVolumeId' - The unique identifier of the volume, e.g. vol-AE4B946D.
--
-- * 'cscsivVolumeType' - One of the VolumeType enumeration values that describes the type of the volume.
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
    , _cscsivVolumeUsedInBytes = Nothing
    , _cscsivCreatedDate = Nothing
    , _cscsivVolumeId = Nothing
    , _cscsivVolumeType = Nothing
    }


-- | An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
cscsivVolumeiSCSIAttributes :: Lens' CachediSCSIVolume (Maybe VolumeiSCSIAttributes)
cscsivVolumeiSCSIAttributes = lens _cscsivVolumeiSCSIAttributes (\ s a -> s{_cscsivVolumeiSCSIAttributes = a})

-- | One of the VolumeStatus values that indicates the state of the storage volume.
cscsivVolumeStatus :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeStatus = lens _cscsivVolumeStatus (\ s a -> s{_cscsivVolumeStatus = a})

-- | If the cached volume was created from a snapshot, this field contains the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not included.
cscsivSourceSnapshotId :: Lens' CachediSCSIVolume (Maybe Text)
cscsivSourceSnapshotId = lens _cscsivSourceSnapshotId (\ s a -> s{_cscsivSourceSnapshotId = a})

-- | The Amazon Resource Name (ARN) of the storage volume.
cscsivVolumeARN :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeARN = lens _cscsivVolumeARN (\ s a -> s{_cscsivVolumeARN = a})

-- | Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the cached volume is not restoring or bootstrapping.
cscsivVolumeProgress :: Lens' CachediSCSIVolume (Maybe Double)
cscsivVolumeProgress = lens _cscsivVolumeProgress (\ s a -> s{_cscsivVolumeProgress = a})

-- | The size, in bytes, of the volume capacity.
cscsivVolumeSizeInBytes :: Lens' CachediSCSIVolume (Maybe Integer)
cscsivVolumeSizeInBytes = lens _cscsivVolumeSizeInBytes (\ s a -> s{_cscsivVolumeSizeInBytes = a})

-- | The size of the data stored on the volume in bytes.
cscsivVolumeUsedInBytes :: Lens' CachediSCSIVolume (Maybe Integer)
cscsivVolumeUsedInBytes = lens _cscsivVolumeUsedInBytes (\ s a -> s{_cscsivVolumeUsedInBytes = a})

-- | The date the volume was created. Volumes created prior to March 28, 2017 don’t have this time stamp.
cscsivCreatedDate :: Lens' CachediSCSIVolume (Maybe UTCTime)
cscsivCreatedDate = lens _cscsivCreatedDate (\ s a -> s{_cscsivCreatedDate = a}) . mapping _Time

-- | The unique identifier of the volume, e.g. vol-AE4B946D.
cscsivVolumeId :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeId = lens _cscsivVolumeId (\ s a -> s{_cscsivVolumeId = a})

-- | One of the VolumeType enumeration values that describes the type of the volume.
cscsivVolumeType :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeType = lens _cscsivVolumeType (\ s a -> s{_cscsivVolumeType = a})

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
                     <*> (x .:? "VolumeUsedInBytes")
                     <*> (x .:? "CreatedDate")
                     <*> (x .:? "VolumeId")
                     <*> (x .:? "VolumeType"))

instance Hashable CachediSCSIVolume where

instance NFData CachediSCSIVolume where

-- | Describes Challenge-Handshake Authentication Protocol (CHAP) information that supports authentication between your gateway and iSCSI initiators.
--
--
--
-- /See:/ 'chapInfo' smart constructor.
data ChapInfo = ChapInfo'
  { _ciTargetARN                     :: !(Maybe Text)
  , _ciSecretToAuthenticateInitiator :: !(Maybe Text)
  , _ciInitiatorName                 :: !(Maybe Text)
  , _ciSecretToAuthenticateTarget    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChapInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciTargetARN' - The Amazon Resource Name (ARN) of the volume. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- * 'ciSecretToAuthenticateInitiator' - The secret key that the initiator (for example, the Windows client) must provide to participate in mutual CHAP with the target.
--
-- * 'ciInitiatorName' - The iSCSI initiator that connects to the target.
--
-- * 'ciSecretToAuthenticateTarget' - The secret key that the target must provide to participate in mutual CHAP with the initiator (e.g. Windows client).
chapInfo
    :: ChapInfo
chapInfo =
  ChapInfo'
    { _ciTargetARN = Nothing
    , _ciSecretToAuthenticateInitiator = Nothing
    , _ciInitiatorName = Nothing
    , _ciSecretToAuthenticateTarget = Nothing
    }


-- | The Amazon Resource Name (ARN) of the volume. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
ciTargetARN :: Lens' ChapInfo (Maybe Text)
ciTargetARN = lens _ciTargetARN (\ s a -> s{_ciTargetARN = a})

-- | The secret key that the initiator (for example, the Windows client) must provide to participate in mutual CHAP with the target.
ciSecretToAuthenticateInitiator :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateInitiator = lens _ciSecretToAuthenticateInitiator (\ s a -> s{_ciSecretToAuthenticateInitiator = a})

-- | The iSCSI initiator that connects to the target.
ciInitiatorName :: Lens' ChapInfo (Maybe Text)
ciInitiatorName = lens _ciInitiatorName (\ s a -> s{_ciInitiatorName = a})

-- | The secret key that the target must provide to participate in mutual CHAP with the initiator (e.g. Windows client).
ciSecretToAuthenticateTarget :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateTarget = lens _ciSecretToAuthenticateTarget (\ s a -> s{_ciSecretToAuthenticateTarget = a})

instance FromJSON ChapInfo where
        parseJSON
          = withObject "ChapInfo"
              (\ x ->
                 ChapInfo' <$>
                   (x .:? "TargetARN") <*>
                     (x .:? "SecretToAuthenticateInitiator")
                     <*> (x .:? "InitiatorName")
                     <*> (x .:? "SecretToAuthenticateTarget"))

instance Hashable ChapInfo where

instance NFData ChapInfo where

-- | Lists iSCSI information about a VTL device.
--
--
--
-- /See:/ 'deviceiSCSIAttributes' smart constructor.
data DeviceiSCSIAttributes = DeviceiSCSIAttributes'
  { _dscsiaTargetARN            :: !(Maybe Text)
  , _dscsiaChapEnabled          :: !(Maybe Bool)
  , _dscsiaNetworkInterfaceId   :: !(Maybe Text)
  , _dscsiaNetworkInterfacePort :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeviceiSCSIAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscsiaTargetARN' - Specifies the unique Amazon Resource Name(ARN) that encodes the iSCSI qualified name(iqn) of a tape drive or media changer target.
--
-- * 'dscsiaChapEnabled' - Indicates whether mutual CHAP is enabled for the iSCSI target.
--
-- * 'dscsiaNetworkInterfaceId' - The network interface identifier of the VTL device.
--
-- * 'dscsiaNetworkInterfacePort' - The port used to communicate with iSCSI VTL device targets.
deviceiSCSIAttributes
    :: DeviceiSCSIAttributes
deviceiSCSIAttributes =
  DeviceiSCSIAttributes'
    { _dscsiaTargetARN = Nothing
    , _dscsiaChapEnabled = Nothing
    , _dscsiaNetworkInterfaceId = Nothing
    , _dscsiaNetworkInterfacePort = Nothing
    }


-- | Specifies the unique Amazon Resource Name(ARN) that encodes the iSCSI qualified name(iqn) of a tape drive or media changer target.
dscsiaTargetARN :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaTargetARN = lens _dscsiaTargetARN (\ s a -> s{_dscsiaTargetARN = a})

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
dscsiaChapEnabled :: Lens' DeviceiSCSIAttributes (Maybe Bool)
dscsiaChapEnabled = lens _dscsiaChapEnabled (\ s a -> s{_dscsiaChapEnabled = a})

-- | The network interface identifier of the VTL device.
dscsiaNetworkInterfaceId :: Lens' DeviceiSCSIAttributes (Maybe Text)
dscsiaNetworkInterfaceId = lens _dscsiaNetworkInterfaceId (\ s a -> s{_dscsiaNetworkInterfaceId = a})

-- | The port used to communicate with iSCSI VTL device targets.
dscsiaNetworkInterfacePort :: Lens' DeviceiSCSIAttributes (Maybe Int)
dscsiaNetworkInterfacePort = lens _dscsiaNetworkInterfacePort (\ s a -> s{_dscsiaNetworkInterfacePort = a})

instance FromJSON DeviceiSCSIAttributes where
        parseJSON
          = withObject "DeviceiSCSIAttributes"
              (\ x ->
                 DeviceiSCSIAttributes' <$>
                   (x .:? "TargetARN") <*> (x .:? "ChapEnabled") <*>
                     (x .:? "NetworkInterfaceId")
                     <*> (x .:? "NetworkInterfacePort"))

instance Hashable DeviceiSCSIAttributes where

instance NFData DeviceiSCSIAttributes where

-- | /See:/ 'disk' smart constructor.
data Disk = Disk'
  { _dDiskAllocationResource :: !(Maybe Text)
  , _dDiskAllocationType     :: !(Maybe Text)
  , _dDiskNode               :: !(Maybe Text)
  , _dDiskPath               :: !(Maybe Text)
  , _dDiskSizeInBytes        :: !(Maybe Integer)
  , _dDiskStatus             :: !(Maybe Text)
  , _dDiskId                 :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Disk' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDiskAllocationResource' - Undocumented member.
--
-- * 'dDiskAllocationType' - Undocumented member.
--
-- * 'dDiskNode' - Undocumented member.
--
-- * 'dDiskPath' - Undocumented member.
--
-- * 'dDiskSizeInBytes' - Undocumented member.
--
-- * 'dDiskStatus' - Undocumented member.
--
-- * 'dDiskId' - Undocumented member.
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
dDiskAllocationResource = lens _dDiskAllocationResource (\ s a -> s{_dDiskAllocationResource = a})

-- | Undocumented member.
dDiskAllocationType :: Lens' Disk (Maybe Text)
dDiskAllocationType = lens _dDiskAllocationType (\ s a -> s{_dDiskAllocationType = a})

-- | Undocumented member.
dDiskNode :: Lens' Disk (Maybe Text)
dDiskNode = lens _dDiskNode (\ s a -> s{_dDiskNode = a})

-- | Undocumented member.
dDiskPath :: Lens' Disk (Maybe Text)
dDiskPath = lens _dDiskPath (\ s a -> s{_dDiskPath = a})

-- | Undocumented member.
dDiskSizeInBytes :: Lens' Disk (Maybe Integer)
dDiskSizeInBytes = lens _dDiskSizeInBytes (\ s a -> s{_dDiskSizeInBytes = a})

-- | Undocumented member.
dDiskStatus :: Lens' Disk (Maybe Text)
dDiskStatus = lens _dDiskStatus (\ s a -> s{_dDiskStatus = a})

-- | Undocumented member.
dDiskId :: Lens' Disk (Maybe Text)
dDiskId = lens _dDiskId (\ s a -> s{_dDiskId = a})

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

instance Hashable Disk where

instance NFData Disk where

-- | Describes a file share.
--
--
--
-- /See:/ 'fileShareInfo' smart constructor.
data FileShareInfo = FileShareInfo'
  { _fsiFileShareStatus :: !(Maybe Text)
  , _fsiGatewayARN      :: !(Maybe Text)
  , _fsiFileShareId     :: !(Maybe Text)
  , _fsiFileShareARN    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FileShareInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsiFileShareStatus' - Undocumented member.
--
-- * 'fsiGatewayARN' - Undocumented member.
--
-- * 'fsiFileShareId' - Undocumented member.
--
-- * 'fsiFileShareARN' - Undocumented member.
fileShareInfo
    :: FileShareInfo
fileShareInfo =
  FileShareInfo'
    { _fsiFileShareStatus = Nothing
    , _fsiGatewayARN = Nothing
    , _fsiFileShareId = Nothing
    , _fsiFileShareARN = Nothing
    }


-- | Undocumented member.
fsiFileShareStatus :: Lens' FileShareInfo (Maybe Text)
fsiFileShareStatus = lens _fsiFileShareStatus (\ s a -> s{_fsiFileShareStatus = a})

-- | Undocumented member.
fsiGatewayARN :: Lens' FileShareInfo (Maybe Text)
fsiGatewayARN = lens _fsiGatewayARN (\ s a -> s{_fsiGatewayARN = a})

-- | Undocumented member.
fsiFileShareId :: Lens' FileShareInfo (Maybe Text)
fsiFileShareId = lens _fsiFileShareId (\ s a -> s{_fsiFileShareId = a})

-- | Undocumented member.
fsiFileShareARN :: Lens' FileShareInfo (Maybe Text)
fsiFileShareARN = lens _fsiFileShareARN (\ s a -> s{_fsiFileShareARN = a})

instance FromJSON FileShareInfo where
        parseJSON
          = withObject "FileShareInfo"
              (\ x ->
                 FileShareInfo' <$>
                   (x .:? "FileShareStatus") <*> (x .:? "GatewayARN")
                     <*> (x .:? "FileShareId")
                     <*> (x .:? "FileShareARN"))

instance Hashable FileShareInfo where

instance NFData FileShareInfo where

-- | Describes a gateway object.
--
--
--
-- /See:/ 'gatewayInfo' smart constructor.
data GatewayInfo = GatewayInfo'
  { _giGatewayARN              :: !(Maybe Text)
  , _giGatewayOperationalState :: !(Maybe Text)
  , _giGatewayName             :: !(Maybe Text)
  , _giGatewayId               :: !(Maybe Text)
  , _giGatewayType             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GatewayInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giGatewayARN' - The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and region.
--
-- * 'giGatewayOperationalState' - The state of the gateway. Valid Values: DISABLED or ACTIVE
--
-- * 'giGatewayName' - The name of the gateway.
--
-- * 'giGatewayId' - The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
--
-- * 'giGatewayType' - The type of the gateway.
gatewayInfo
    :: GatewayInfo
gatewayInfo =
  GatewayInfo'
    { _giGatewayARN = Nothing
    , _giGatewayOperationalState = Nothing
    , _giGatewayName = Nothing
    , _giGatewayId = Nothing
    , _giGatewayType = Nothing
    }


-- | The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and region.
giGatewayARN :: Lens' GatewayInfo (Maybe Text)
giGatewayARN = lens _giGatewayARN (\ s a -> s{_giGatewayARN = a})

-- | The state of the gateway. Valid Values: DISABLED or ACTIVE
giGatewayOperationalState :: Lens' GatewayInfo (Maybe Text)
giGatewayOperationalState = lens _giGatewayOperationalState (\ s a -> s{_giGatewayOperationalState = a})

-- | The name of the gateway.
giGatewayName :: Lens' GatewayInfo (Maybe Text)
giGatewayName = lens _giGatewayName (\ s a -> s{_giGatewayName = a})

-- | The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
giGatewayId :: Lens' GatewayInfo (Maybe Text)
giGatewayId = lens _giGatewayId (\ s a -> s{_giGatewayId = a})

-- | The type of the gateway.
giGatewayType :: Lens' GatewayInfo (Maybe Text)
giGatewayType = lens _giGatewayType (\ s a -> s{_giGatewayType = a})

instance FromJSON GatewayInfo where
        parseJSON
          = withObject "GatewayInfo"
              (\ x ->
                 GatewayInfo' <$>
                   (x .:? "GatewayARN") <*>
                     (x .:? "GatewayOperationalState")
                     <*> (x .:? "GatewayName")
                     <*> (x .:? "GatewayId")
                     <*> (x .:? "GatewayType"))

instance Hashable GatewayInfo where

instance NFData GatewayInfo where

-- | Describes file share default values. Files and folders stored as Amazon S3 objects in S3 buckets don't, by default, have Unix file permissions assigned to them. Upon discovery in an S3 bucket by Storage Gateway, the S3 objects that represent files and folders are assigned these default Unix permissions. This operation is only supported in the file gateway type.
--
--
--
-- /See:/ 'nFSFileShareDefaults' smart constructor.
data NFSFileShareDefaults = NFSFileShareDefaults'
  { _nfsfsdFileMode      :: !(Maybe Text)
  , _nfsfsdOwnerId       :: !(Maybe Nat)
  , _nfsfsdDirectoryMode :: !(Maybe Text)
  , _nfsfsdGroupId       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NFSFileShareDefaults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nfsfsdFileMode' - The Unix file mode in the form "nnnn". For example, "0666" represents the default file mode inside the file share. The default value is 0666.
--
-- * 'nfsfsdOwnerId' - The default owner ID for files in the file share (unless the files have another owner ID specified). The default value is nfsnobody.
--
-- * 'nfsfsdDirectoryMode' - The Unix directory mode in the form "nnnn". For example, "0666" represents the default access mode for all directories inside the file share. The default value is 0777.
--
-- * 'nfsfsdGroupId' - The default group ID for the file share (unless the files have another group ID specified). The default value is nfsnobody.
nFSFileShareDefaults
    :: NFSFileShareDefaults
nFSFileShareDefaults =
  NFSFileShareDefaults'
    { _nfsfsdFileMode = Nothing
    , _nfsfsdOwnerId = Nothing
    , _nfsfsdDirectoryMode = Nothing
    , _nfsfsdGroupId = Nothing
    }


-- | The Unix file mode in the form "nnnn". For example, "0666" represents the default file mode inside the file share. The default value is 0666.
nfsfsdFileMode :: Lens' NFSFileShareDefaults (Maybe Text)
nfsfsdFileMode = lens _nfsfsdFileMode (\ s a -> s{_nfsfsdFileMode = a})

-- | The default owner ID for files in the file share (unless the files have another owner ID specified). The default value is nfsnobody.
nfsfsdOwnerId :: Lens' NFSFileShareDefaults (Maybe Natural)
nfsfsdOwnerId = lens _nfsfsdOwnerId (\ s a -> s{_nfsfsdOwnerId = a}) . mapping _Nat

-- | The Unix directory mode in the form "nnnn". For example, "0666" represents the default access mode for all directories inside the file share. The default value is 0777.
nfsfsdDirectoryMode :: Lens' NFSFileShareDefaults (Maybe Text)
nfsfsdDirectoryMode = lens _nfsfsdDirectoryMode (\ s a -> s{_nfsfsdDirectoryMode = a})

-- | The default group ID for the file share (unless the files have another group ID specified). The default value is nfsnobody.
nfsfsdGroupId :: Lens' NFSFileShareDefaults (Maybe Natural)
nfsfsdGroupId = lens _nfsfsdGroupId (\ s a -> s{_nfsfsdGroupId = a}) . mapping _Nat

instance FromJSON NFSFileShareDefaults where
        parseJSON
          = withObject "NFSFileShareDefaults"
              (\ x ->
                 NFSFileShareDefaults' <$>
                   (x .:? "FileMode") <*> (x .:? "OwnerId") <*>
                     (x .:? "DirectoryMode")
                     <*> (x .:? "GroupId"))

instance Hashable NFSFileShareDefaults where

instance NFData NFSFileShareDefaults where

instance ToJSON NFSFileShareDefaults where
        toJSON NFSFileShareDefaults'{..}
          = object
              (catMaybes
                 [("FileMode" .=) <$> _nfsfsdFileMode,
                  ("OwnerId" .=) <$> _nfsfsdOwnerId,
                  ("DirectoryMode" .=) <$> _nfsfsdDirectoryMode,
                  ("GroupId" .=) <$> _nfsfsdGroupId])

-- | The Unix file permissions and ownership information assigned, by default, to native S3 objects when file gateway discovers them in S3 buckets. This operation is only supported in file gateways.
--
--
--
-- /See:/ 'nFSFileShareInfo' smart constructor.
data NFSFileShareInfo = NFSFileShareInfo'
  { _nfsfsiFileShareStatus      :: !(Maybe Text)
  , _nfsfsiKMSKey               :: !(Maybe Text)
  , _nfsfsiGatewayARN           :: !(Maybe Text)
  , _nfsfsiPath                 :: !(Maybe Text)
  , _nfsfsiObjectACL            :: !(Maybe ObjectACL)
  , _nfsfsiKMSEncrypted         :: !(Maybe Bool)
  , _nfsfsiFileShareId          :: !(Maybe Text)
  , _nfsfsiFileShareARN         :: !(Maybe Text)
  , _nfsfsiDefaultStorageClass  :: !(Maybe Text)
  , _nfsfsiRole                 :: !(Maybe Text)
  , _nfsfsiSquash               :: !(Maybe Text)
  , _nfsfsiRequesterPays        :: !(Maybe Bool)
  , _nfsfsiNFSFileShareDefaults :: !(Maybe NFSFileShareDefaults)
  , _nfsfsiLocationARN          :: !(Maybe Text)
  , _nfsfsiClientList           :: !(Maybe (List1 Text))
  , _nfsfsiGuessMIMETypeEnabled :: !(Maybe Bool)
  , _nfsfsiReadOnly             :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NFSFileShareInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nfsfsiFileShareStatus' - Undocumented member.
--
-- * 'nfsfsiKMSKey' - Undocumented member.
--
-- * 'nfsfsiGatewayARN' - Undocumented member.
--
-- * 'nfsfsiPath' - Undocumented member.
--
-- * 'nfsfsiObjectACL' - Undocumented member.
--
-- * 'nfsfsiKMSEncrypted' - True to use Amazon S3 server side encryption with your own KMS key, or false to use a key managed by Amazon S3. Optional.
--
-- * 'nfsfsiFileShareId' - Undocumented member.
--
-- * 'nfsfsiFileShareARN' - Undocumented member.
--
-- * 'nfsfsiDefaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by file gateway. Possible values are S3_STANDARD or S3_STANDARD_IA. If this field is not populated, the default value S3_STANDARD is used. Optional.
--
-- * 'nfsfsiRole' - Undocumented member.
--
-- * 'nfsfsiSquash' - Undocumented member.
--
-- * 'nfsfsiRequesterPays' - Sets who pays the cost of the request and the data download from the Amazon S3 bucket. Set this value to true if you want the requester to pay instead of the bucket owner, and otherwise to false.
--
-- * 'nfsfsiNFSFileShareDefaults' - Undocumented member.
--
-- * 'nfsfsiLocationARN' - Undocumented member.
--
-- * 'nfsfsiClientList' - Undocumented member.
--
-- * 'nfsfsiGuessMIMETypeEnabled' - Enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to true to enable MIME type guessing, and otherwise to false. The default value is true.
--
-- * 'nfsfsiReadOnly' - Sets the write status of a file share. This value is true if the write status is read-only, and otherwise false.
nFSFileShareInfo
    :: NFSFileShareInfo
nFSFileShareInfo =
  NFSFileShareInfo'
    { _nfsfsiFileShareStatus = Nothing
    , _nfsfsiKMSKey = Nothing
    , _nfsfsiGatewayARN = Nothing
    , _nfsfsiPath = Nothing
    , _nfsfsiObjectACL = Nothing
    , _nfsfsiKMSEncrypted = Nothing
    , _nfsfsiFileShareId = Nothing
    , _nfsfsiFileShareARN = Nothing
    , _nfsfsiDefaultStorageClass = Nothing
    , _nfsfsiRole = Nothing
    , _nfsfsiSquash = Nothing
    , _nfsfsiRequesterPays = Nothing
    , _nfsfsiNFSFileShareDefaults = Nothing
    , _nfsfsiLocationARN = Nothing
    , _nfsfsiClientList = Nothing
    , _nfsfsiGuessMIMETypeEnabled = Nothing
    , _nfsfsiReadOnly = Nothing
    }


-- | Undocumented member.
nfsfsiFileShareStatus :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiFileShareStatus = lens _nfsfsiFileShareStatus (\ s a -> s{_nfsfsiFileShareStatus = a})

-- | Undocumented member.
nfsfsiKMSKey :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiKMSKey = lens _nfsfsiKMSKey (\ s a -> s{_nfsfsiKMSKey = a})

-- | Undocumented member.
nfsfsiGatewayARN :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiGatewayARN = lens _nfsfsiGatewayARN (\ s a -> s{_nfsfsiGatewayARN = a})

-- | Undocumented member.
nfsfsiPath :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiPath = lens _nfsfsiPath (\ s a -> s{_nfsfsiPath = a})

-- | Undocumented member.
nfsfsiObjectACL :: Lens' NFSFileShareInfo (Maybe ObjectACL)
nfsfsiObjectACL = lens _nfsfsiObjectACL (\ s a -> s{_nfsfsiObjectACL = a})

-- | True to use Amazon S3 server side encryption with your own KMS key, or false to use a key managed by Amazon S3. Optional.
nfsfsiKMSEncrypted :: Lens' NFSFileShareInfo (Maybe Bool)
nfsfsiKMSEncrypted = lens _nfsfsiKMSEncrypted (\ s a -> s{_nfsfsiKMSEncrypted = a})

-- | Undocumented member.
nfsfsiFileShareId :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiFileShareId = lens _nfsfsiFileShareId (\ s a -> s{_nfsfsiFileShareId = a})

-- | Undocumented member.
nfsfsiFileShareARN :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiFileShareARN = lens _nfsfsiFileShareARN (\ s a -> s{_nfsfsiFileShareARN = a})

-- | The default storage class for objects put into an Amazon S3 bucket by file gateway. Possible values are S3_STANDARD or S3_STANDARD_IA. If this field is not populated, the default value S3_STANDARD is used. Optional.
nfsfsiDefaultStorageClass :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiDefaultStorageClass = lens _nfsfsiDefaultStorageClass (\ s a -> s{_nfsfsiDefaultStorageClass = a})

-- | Undocumented member.
nfsfsiRole :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiRole = lens _nfsfsiRole (\ s a -> s{_nfsfsiRole = a})

-- | Undocumented member.
nfsfsiSquash :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiSquash = lens _nfsfsiSquash (\ s a -> s{_nfsfsiSquash = a})

-- | Sets who pays the cost of the request and the data download from the Amazon S3 bucket. Set this value to true if you want the requester to pay instead of the bucket owner, and otherwise to false.
nfsfsiRequesterPays :: Lens' NFSFileShareInfo (Maybe Bool)
nfsfsiRequesterPays = lens _nfsfsiRequesterPays (\ s a -> s{_nfsfsiRequesterPays = a})

-- | Undocumented member.
nfsfsiNFSFileShareDefaults :: Lens' NFSFileShareInfo (Maybe NFSFileShareDefaults)
nfsfsiNFSFileShareDefaults = lens _nfsfsiNFSFileShareDefaults (\ s a -> s{_nfsfsiNFSFileShareDefaults = a})

-- | Undocumented member.
nfsfsiLocationARN :: Lens' NFSFileShareInfo (Maybe Text)
nfsfsiLocationARN = lens _nfsfsiLocationARN (\ s a -> s{_nfsfsiLocationARN = a})

-- | Undocumented member.
nfsfsiClientList :: Lens' NFSFileShareInfo (Maybe (NonEmpty Text))
nfsfsiClientList = lens _nfsfsiClientList (\ s a -> s{_nfsfsiClientList = a}) . mapping _List1

-- | Enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to true to enable MIME type guessing, and otherwise to false. The default value is true.
nfsfsiGuessMIMETypeEnabled :: Lens' NFSFileShareInfo (Maybe Bool)
nfsfsiGuessMIMETypeEnabled = lens _nfsfsiGuessMIMETypeEnabled (\ s a -> s{_nfsfsiGuessMIMETypeEnabled = a})

-- | Sets the write status of a file share. This value is true if the write status is read-only, and otherwise false.
nfsfsiReadOnly :: Lens' NFSFileShareInfo (Maybe Bool)
nfsfsiReadOnly = lens _nfsfsiReadOnly (\ s a -> s{_nfsfsiReadOnly = a})

instance FromJSON NFSFileShareInfo where
        parseJSON
          = withObject "NFSFileShareInfo"
              (\ x ->
                 NFSFileShareInfo' <$>
                   (x .:? "FileShareStatus") <*> (x .:? "KMSKey") <*>
                     (x .:? "GatewayARN")
                     <*> (x .:? "Path")
                     <*> (x .:? "ObjectACL")
                     <*> (x .:? "KMSEncrypted")
                     <*> (x .:? "FileShareId")
                     <*> (x .:? "FileShareARN")
                     <*> (x .:? "DefaultStorageClass")
                     <*> (x .:? "Role")
                     <*> (x .:? "Squash")
                     <*> (x .:? "RequesterPays")
                     <*> (x .:? "NFSFileShareDefaults")
                     <*> (x .:? "LocationARN")
                     <*> (x .:? "ClientList")
                     <*> (x .:? "GuessMIMETypeEnabled")
                     <*> (x .:? "ReadOnly"))

instance Hashable NFSFileShareInfo where

instance NFData NFSFileShareInfo where

-- | Describes a gateway's network interface.
--
--
--
-- /See:/ 'networkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { _niIPv6Address :: !(Maybe Text)
  , _niMACAddress  :: !(Maybe Text)
  , _niIPv4Address :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'niIPv6Address' - The Internet Protocol version 6 (IPv6) address of the interface. /Currently not supported/ .
--
-- * 'niMACAddress' - The Media Access Control (MAC) address of the interface.
--
-- * 'niIPv4Address' - The Internet Protocol version 4 (IPv4) address of the interface.
networkInterface
    :: NetworkInterface
networkInterface =
  NetworkInterface'
    { _niIPv6Address = Nothing
    , _niMACAddress = Nothing
    , _niIPv4Address = Nothing
    }


-- | The Internet Protocol version 6 (IPv6) address of the interface. /Currently not supported/ .
niIPv6Address :: Lens' NetworkInterface (Maybe Text)
niIPv6Address = lens _niIPv6Address (\ s a -> s{_niIPv6Address = a})

-- | The Media Access Control (MAC) address of the interface.
niMACAddress :: Lens' NetworkInterface (Maybe Text)
niMACAddress = lens _niMACAddress (\ s a -> s{_niMACAddress = a})

-- | The Internet Protocol version 4 (IPv4) address of the interface.
niIPv4Address :: Lens' NetworkInterface (Maybe Text)
niIPv4Address = lens _niIPv4Address (\ s a -> s{_niIPv4Address = a})

instance FromJSON NetworkInterface where
        parseJSON
          = withObject "NetworkInterface"
              (\ x ->
                 NetworkInterface' <$>
                   (x .:? "Ipv6Address") <*> (x .:? "MacAddress") <*>
                     (x .:? "Ipv4Address"))

instance Hashable NetworkInterface where

instance NFData NetworkInterface where

-- | Describes an iSCSI stored volume.
--
--
--
-- /See:/ 'storediSCSIVolume' smart constructor.
data StorediSCSIVolume = StorediSCSIVolume'
  { _sscsivVolumeiSCSIAttributes :: !(Maybe VolumeiSCSIAttributes)
  , _sscsivVolumeStatus          :: !(Maybe Text)
  , _sscsivSourceSnapshotId      :: !(Maybe Text)
  , _sscsivPreservedExistingData :: !(Maybe Bool)
  , _sscsivVolumeARN             :: !(Maybe Text)
  , _sscsivVolumeProgress        :: !(Maybe Double)
  , _sscsivVolumeSizeInBytes     :: !(Maybe Integer)
  , _sscsivVolumeUsedInBytes     :: !(Maybe Integer)
  , _sscsivCreatedDate           :: !(Maybe POSIX)
  , _sscsivVolumeId              :: !(Maybe Text)
  , _sscsivVolumeDiskId          :: !(Maybe Text)
  , _sscsivVolumeType            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StorediSCSIVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sscsivVolumeiSCSIAttributes' - An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
--
-- * 'sscsivVolumeStatus' - One of the VolumeStatus values that indicates the state of the storage volume.
--
-- * 'sscsivSourceSnapshotId' - If the stored volume was created from a snapshot, this field contains the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not included.
--
-- * 'sscsivPreservedExistingData' - Indicates if when the stored volume was created, existing data on the underlying local disk was preserved. Valid Values: true, false
--
-- * 'sscsivVolumeARN' - The Amazon Resource Name (ARN) of the storage volume.
--
-- * 'sscsivVolumeProgress' - Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the stored volume is not restoring or bootstrapping.
--
-- * 'sscsivVolumeSizeInBytes' - The size of the volume in bytes.
--
-- * 'sscsivVolumeUsedInBytes' - The size of the data stored on the volume in bytes.
--
-- * 'sscsivCreatedDate' - The date the volume was created. Volumes created prior to March 28, 2017 don’t have this time stamp.
--
-- * 'sscsivVolumeId' - The unique identifier of the volume, e.g. vol-AE4B946D.
--
-- * 'sscsivVolumeDiskId' - The ID of the local disk that was specified in the 'CreateStorediSCSIVolume' operation.
--
-- * 'sscsivVolumeType' - One of the VolumeType enumeration values describing the type of the volume.
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
    , _sscsivVolumeUsedInBytes = Nothing
    , _sscsivCreatedDate = Nothing
    , _sscsivVolumeId = Nothing
    , _sscsivVolumeDiskId = Nothing
    , _sscsivVolumeType = Nothing
    }


-- | An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
sscsivVolumeiSCSIAttributes :: Lens' StorediSCSIVolume (Maybe VolumeiSCSIAttributes)
sscsivVolumeiSCSIAttributes = lens _sscsivVolumeiSCSIAttributes (\ s a -> s{_sscsivVolumeiSCSIAttributes = a})

-- | One of the VolumeStatus values that indicates the state of the storage volume.
sscsivVolumeStatus :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeStatus = lens _sscsivVolumeStatus (\ s a -> s{_sscsivVolumeStatus = a})

-- | If the stored volume was created from a snapshot, this field contains the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not included.
sscsivSourceSnapshotId :: Lens' StorediSCSIVolume (Maybe Text)
sscsivSourceSnapshotId = lens _sscsivSourceSnapshotId (\ s a -> s{_sscsivSourceSnapshotId = a})

-- | Indicates if when the stored volume was created, existing data on the underlying local disk was preserved. Valid Values: true, false
sscsivPreservedExistingData :: Lens' StorediSCSIVolume (Maybe Bool)
sscsivPreservedExistingData = lens _sscsivPreservedExistingData (\ s a -> s{_sscsivPreservedExistingData = a})

-- | The Amazon Resource Name (ARN) of the storage volume.
sscsivVolumeARN :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeARN = lens _sscsivVolumeARN (\ s a -> s{_sscsivVolumeARN = a})

-- | Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the stored volume is not restoring or bootstrapping.
sscsivVolumeProgress :: Lens' StorediSCSIVolume (Maybe Double)
sscsivVolumeProgress = lens _sscsivVolumeProgress (\ s a -> s{_sscsivVolumeProgress = a})

-- | The size of the volume in bytes.
sscsivVolumeSizeInBytes :: Lens' StorediSCSIVolume (Maybe Integer)
sscsivVolumeSizeInBytes = lens _sscsivVolumeSizeInBytes (\ s a -> s{_sscsivVolumeSizeInBytes = a})

-- | The size of the data stored on the volume in bytes.
sscsivVolumeUsedInBytes :: Lens' StorediSCSIVolume (Maybe Integer)
sscsivVolumeUsedInBytes = lens _sscsivVolumeUsedInBytes (\ s a -> s{_sscsivVolumeUsedInBytes = a})

-- | The date the volume was created. Volumes created prior to March 28, 2017 don’t have this time stamp.
sscsivCreatedDate :: Lens' StorediSCSIVolume (Maybe UTCTime)
sscsivCreatedDate = lens _sscsivCreatedDate (\ s a -> s{_sscsivCreatedDate = a}) . mapping _Time

-- | The unique identifier of the volume, e.g. vol-AE4B946D.
sscsivVolumeId :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeId = lens _sscsivVolumeId (\ s a -> s{_sscsivVolumeId = a})

-- | The ID of the local disk that was specified in the 'CreateStorediSCSIVolume' operation.
sscsivVolumeDiskId :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeDiskId = lens _sscsivVolumeDiskId (\ s a -> s{_sscsivVolumeDiskId = a})

-- | One of the VolumeType enumeration values describing the type of the volume.
sscsivVolumeType :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeType = lens _sscsivVolumeType (\ s a -> s{_sscsivVolumeType = a})

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
                     <*> (x .:? "VolumeUsedInBytes")
                     <*> (x .:? "CreatedDate")
                     <*> (x .:? "VolumeId")
                     <*> (x .:? "VolumeDiskId")
                     <*> (x .:? "VolumeType"))

instance Hashable StorediSCSIVolume where

instance NFData StorediSCSIVolume where

-- | /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - Undocumented member.
--
-- * 'tagValue' - Undocumented member.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | Undocumented member.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | Undocumented member.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _tagKey),
                  Just ("Value" .= _tagValue)])

-- | Describes a virtual tape object.
--
--
--
-- /See:/ 'tape' smart constructor.
data Tape = Tape'
  { _tTapeBarcode     :: !(Maybe Text)
  , _tTapeStatus      :: !(Maybe Text)
  , _tTapeARN         :: !(Maybe Text)
  , _tProgress        :: !(Maybe Double)
  , _tTapeSizeInBytes :: !(Maybe Integer)
  , _tVTLDevice       :: !(Maybe Text)
  , _tTapeUsedInBytes :: !(Maybe Integer)
  , _tTapeCreatedDate :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tape' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTapeBarcode' - The barcode that identifies a specific virtual tape.
--
-- * 'tTapeStatus' - The current state of the virtual tape.
--
-- * 'tTapeARN' - The Amazon Resource Name (ARN) of the virtual tape.
--
-- * 'tProgress' - For archiving virtual tapes, indicates how much data remains to be uploaded before archiving is complete. Range: 0 (not started) to 100 (complete).
--
-- * 'tTapeSizeInBytes' - The size, in bytes, of the virtual tape capacity.
--
-- * 'tVTLDevice' - The virtual tape library (VTL) device that the virtual tape is associated with.
--
-- * 'tTapeUsedInBytes' - The size, in bytes, of data stored on the virtual tape.
--
-- * 'tTapeCreatedDate' - The date the virtual tape was created.
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
    , _tTapeUsedInBytes = Nothing
    , _tTapeCreatedDate = Nothing
    }


-- | The barcode that identifies a specific virtual tape.
tTapeBarcode :: Lens' Tape (Maybe Text)
tTapeBarcode = lens _tTapeBarcode (\ s a -> s{_tTapeBarcode = a})

-- | The current state of the virtual tape.
tTapeStatus :: Lens' Tape (Maybe Text)
tTapeStatus = lens _tTapeStatus (\ s a -> s{_tTapeStatus = a})

-- | The Amazon Resource Name (ARN) of the virtual tape.
tTapeARN :: Lens' Tape (Maybe Text)
tTapeARN = lens _tTapeARN (\ s a -> s{_tTapeARN = a})

-- | For archiving virtual tapes, indicates how much data remains to be uploaded before archiving is complete. Range: 0 (not started) to 100 (complete).
tProgress :: Lens' Tape (Maybe Double)
tProgress = lens _tProgress (\ s a -> s{_tProgress = a})

-- | The size, in bytes, of the virtual tape capacity.
tTapeSizeInBytes :: Lens' Tape (Maybe Integer)
tTapeSizeInBytes = lens _tTapeSizeInBytes (\ s a -> s{_tTapeSizeInBytes = a})

-- | The virtual tape library (VTL) device that the virtual tape is associated with.
tVTLDevice :: Lens' Tape (Maybe Text)
tVTLDevice = lens _tVTLDevice (\ s a -> s{_tVTLDevice = a})

-- | The size, in bytes, of data stored on the virtual tape.
tTapeUsedInBytes :: Lens' Tape (Maybe Integer)
tTapeUsedInBytes = lens _tTapeUsedInBytes (\ s a -> s{_tTapeUsedInBytes = a})

-- | The date the virtual tape was created.
tTapeCreatedDate :: Lens' Tape (Maybe UTCTime)
tTapeCreatedDate = lens _tTapeCreatedDate (\ s a -> s{_tTapeCreatedDate = a}) . mapping _Time

instance FromJSON Tape where
        parseJSON
          = withObject "Tape"
              (\ x ->
                 Tape' <$>
                   (x .:? "TapeBarcode") <*> (x .:? "TapeStatus") <*>
                     (x .:? "TapeARN")
                     <*> (x .:? "Progress")
                     <*> (x .:? "TapeSizeInBytes")
                     <*> (x .:? "VTLDevice")
                     <*> (x .:? "TapeUsedInBytes")
                     <*> (x .:? "TapeCreatedDate"))

instance Hashable Tape where

instance NFData Tape where

-- | Represents a virtual tape that is archived in the virtual tape shelf (VTS).
--
--
--
-- /See:/ 'tapeArchive' smart constructor.
data TapeArchive = TapeArchive'
  { _taTapeBarcode     :: !(Maybe Text)
  , _taTapeStatus      :: !(Maybe Text)
  , _taTapeARN         :: !(Maybe Text)
  , _taTapeSizeInBytes :: !(Maybe Integer)
  , _taCompletionTime  :: !(Maybe POSIX)
  , _taTapeUsedInBytes :: !(Maybe Integer)
  , _taTapeCreatedDate :: !(Maybe POSIX)
  , _taRetrievedTo     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TapeArchive' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'taTapeBarcode' - The barcode that identifies the archived virtual tape.
--
-- * 'taTapeStatus' - The current state of the archived virtual tape.
--
-- * 'taTapeARN' - The Amazon Resource Name (ARN) of an archived virtual tape.
--
-- * 'taTapeSizeInBytes' - The size, in bytes, of the archived virtual tape.
--
-- * 'taCompletionTime' - The time that the archiving of the virtual tape was completed. The string format of the completion time is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
--
-- * 'taTapeUsedInBytes' - The size, in bytes, of data stored on the virtual tape.
--
-- * 'taTapeCreatedDate' - Undocumented member.
--
-- * 'taRetrievedTo' - The Amazon Resource Name (ARN) of the tape gateway that the virtual tape is being retrieved to. The virtual tape is retrieved from the virtual tape shelf (VTS).
tapeArchive
    :: TapeArchive
tapeArchive =
  TapeArchive'
    { _taTapeBarcode = Nothing
    , _taTapeStatus = Nothing
    , _taTapeARN = Nothing
    , _taTapeSizeInBytes = Nothing
    , _taCompletionTime = Nothing
    , _taTapeUsedInBytes = Nothing
    , _taTapeCreatedDate = Nothing
    , _taRetrievedTo = Nothing
    }


-- | The barcode that identifies the archived virtual tape.
taTapeBarcode :: Lens' TapeArchive (Maybe Text)
taTapeBarcode = lens _taTapeBarcode (\ s a -> s{_taTapeBarcode = a})

-- | The current state of the archived virtual tape.
taTapeStatus :: Lens' TapeArchive (Maybe Text)
taTapeStatus = lens _taTapeStatus (\ s a -> s{_taTapeStatus = a})

-- | The Amazon Resource Name (ARN) of an archived virtual tape.
taTapeARN :: Lens' TapeArchive (Maybe Text)
taTapeARN = lens _taTapeARN (\ s a -> s{_taTapeARN = a})

-- | The size, in bytes, of the archived virtual tape.
taTapeSizeInBytes :: Lens' TapeArchive (Maybe Integer)
taTapeSizeInBytes = lens _taTapeSizeInBytes (\ s a -> s{_taTapeSizeInBytes = a})

-- | The time that the archiving of the virtual tape was completed. The string format of the completion time is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
taCompletionTime :: Lens' TapeArchive (Maybe UTCTime)
taCompletionTime = lens _taCompletionTime (\ s a -> s{_taCompletionTime = a}) . mapping _Time

-- | The size, in bytes, of data stored on the virtual tape.
taTapeUsedInBytes :: Lens' TapeArchive (Maybe Integer)
taTapeUsedInBytes = lens _taTapeUsedInBytes (\ s a -> s{_taTapeUsedInBytes = a})

-- | Undocumented member.
taTapeCreatedDate :: Lens' TapeArchive (Maybe UTCTime)
taTapeCreatedDate = lens _taTapeCreatedDate (\ s a -> s{_taTapeCreatedDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the tape gateway that the virtual tape is being retrieved to. The virtual tape is retrieved from the virtual tape shelf (VTS).
taRetrievedTo :: Lens' TapeArchive (Maybe Text)
taRetrievedTo = lens _taRetrievedTo (\ s a -> s{_taRetrievedTo = a})

instance FromJSON TapeArchive where
        parseJSON
          = withObject "TapeArchive"
              (\ x ->
                 TapeArchive' <$>
                   (x .:? "TapeBarcode") <*> (x .:? "TapeStatus") <*>
                     (x .:? "TapeARN")
                     <*> (x .:? "TapeSizeInBytes")
                     <*> (x .:? "CompletionTime")
                     <*> (x .:? "TapeUsedInBytes")
                     <*> (x .:? "TapeCreatedDate")
                     <*> (x .:? "RetrievedTo"))

instance Hashable TapeArchive where

instance NFData TapeArchive where

-- | Describes a virtual tape.
--
--
--
-- /See:/ 'tapeInfo' smart constructor.
data TapeInfo = TapeInfo'
  { _tiTapeBarcode     :: !(Maybe Text)
  , _tiTapeStatus      :: !(Maybe Text)
  , _tiTapeARN         :: !(Maybe Text)
  , _tiGatewayARN      :: !(Maybe Text)
  , _tiTapeSizeInBytes :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TapeInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiTapeBarcode' - The barcode that identifies a specific virtual tape.
--
-- * 'tiTapeStatus' - The status of the tape.
--
-- * 'tiTapeARN' - The Amazon Resource Name (ARN) of a virtual tape.
--
-- * 'tiGatewayARN' - The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and region.
--
-- * 'tiTapeSizeInBytes' - The size, in bytes, of a virtual tape.
tapeInfo
    :: TapeInfo
tapeInfo =
  TapeInfo'
    { _tiTapeBarcode = Nothing
    , _tiTapeStatus = Nothing
    , _tiTapeARN = Nothing
    , _tiGatewayARN = Nothing
    , _tiTapeSizeInBytes = Nothing
    }


-- | The barcode that identifies a specific virtual tape.
tiTapeBarcode :: Lens' TapeInfo (Maybe Text)
tiTapeBarcode = lens _tiTapeBarcode (\ s a -> s{_tiTapeBarcode = a})

-- | The status of the tape.
tiTapeStatus :: Lens' TapeInfo (Maybe Text)
tiTapeStatus = lens _tiTapeStatus (\ s a -> s{_tiTapeStatus = a})

-- | The Amazon Resource Name (ARN) of a virtual tape.
tiTapeARN :: Lens' TapeInfo (Maybe Text)
tiTapeARN = lens _tiTapeARN (\ s a -> s{_tiTapeARN = a})

-- | The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and region.
tiGatewayARN :: Lens' TapeInfo (Maybe Text)
tiGatewayARN = lens _tiGatewayARN (\ s a -> s{_tiGatewayARN = a})

-- | The size, in bytes, of a virtual tape.
tiTapeSizeInBytes :: Lens' TapeInfo (Maybe Integer)
tiTapeSizeInBytes = lens _tiTapeSizeInBytes (\ s a -> s{_tiTapeSizeInBytes = a})

instance FromJSON TapeInfo where
        parseJSON
          = withObject "TapeInfo"
              (\ x ->
                 TapeInfo' <$>
                   (x .:? "TapeBarcode") <*> (x .:? "TapeStatus") <*>
                     (x .:? "TapeARN")
                     <*> (x .:? "GatewayARN")
                     <*> (x .:? "TapeSizeInBytes"))

instance Hashable TapeInfo where

instance NFData TapeInfo where

-- | Describes a recovery point.
--
--
--
-- /See:/ 'tapeRecoveryPointInfo' smart constructor.
data TapeRecoveryPointInfo = TapeRecoveryPointInfo'
  { _trpiTapeStatus            :: !(Maybe Text)
  , _trpiTapeRecoveryPointTime :: !(Maybe POSIX)
  , _trpiTapeARN               :: !(Maybe Text)
  , _trpiTapeSizeInBytes       :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TapeRecoveryPointInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trpiTapeStatus' - Undocumented member.
--
-- * 'trpiTapeRecoveryPointTime' - The time when the point-in-time view of the virtual tape was replicated for later recovery. The string format of the tape recovery point time is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
--
-- * 'trpiTapeARN' - The Amazon Resource Name (ARN) of the virtual tape.
--
-- * 'trpiTapeSizeInBytes' - The size, in bytes, of the virtual tapes to recover.
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
trpiTapeStatus = lens _trpiTapeStatus (\ s a -> s{_trpiTapeStatus = a})

-- | The time when the point-in-time view of the virtual tape was replicated for later recovery. The string format of the tape recovery point time is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
trpiTapeRecoveryPointTime :: Lens' TapeRecoveryPointInfo (Maybe UTCTime)
trpiTapeRecoveryPointTime = lens _trpiTapeRecoveryPointTime (\ s a -> s{_trpiTapeRecoveryPointTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the virtual tape.
trpiTapeARN :: Lens' TapeRecoveryPointInfo (Maybe Text)
trpiTapeARN = lens _trpiTapeARN (\ s a -> s{_trpiTapeARN = a})

-- | The size, in bytes, of the virtual tapes to recover.
trpiTapeSizeInBytes :: Lens' TapeRecoveryPointInfo (Maybe Integer)
trpiTapeSizeInBytes = lens _trpiTapeSizeInBytes (\ s a -> s{_trpiTapeSizeInBytes = a})

instance FromJSON TapeRecoveryPointInfo where
        parseJSON
          = withObject "TapeRecoveryPointInfo"
              (\ x ->
                 TapeRecoveryPointInfo' <$>
                   (x .:? "TapeStatus") <*>
                     (x .:? "TapeRecoveryPointTime")
                     <*> (x .:? "TapeARN")
                     <*> (x .:? "TapeSizeInBytes"))

instance Hashable TapeRecoveryPointInfo where

instance NFData TapeRecoveryPointInfo where

-- | Represents a device object associated with a tape gateway.
--
--
--
-- /See:/ 'vTLDevice' smart constructor.
data VTLDevice = VTLDevice'
  { _vtldDeviceiSCSIAttributes      :: !(Maybe DeviceiSCSIAttributes)
  , _vtldVTLDeviceVendor            :: !(Maybe Text)
  , _vtldVTLDeviceARN               :: !(Maybe Text)
  , _vtldVTLDeviceType              :: !(Maybe Text)
  , _vtldVTLDeviceProductIdentifier :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VTLDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vtldDeviceiSCSIAttributes' - A list of iSCSI information about a VTL device.
--
-- * 'vtldVTLDeviceVendor' - Undocumented member.
--
-- * 'vtldVTLDeviceARN' - Specifies the unique Amazon Resource Name (ARN) of the device (tape drive or media changer).
--
-- * 'vtldVTLDeviceType' - Undocumented member.
--
-- * 'vtldVTLDeviceProductIdentifier' - Undocumented member.
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
vtldDeviceiSCSIAttributes = lens _vtldDeviceiSCSIAttributes (\ s a -> s{_vtldDeviceiSCSIAttributes = a})

-- | Undocumented member.
vtldVTLDeviceVendor :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceVendor = lens _vtldVTLDeviceVendor (\ s a -> s{_vtldVTLDeviceVendor = a})

-- | Specifies the unique Amazon Resource Name (ARN) of the device (tape drive or media changer).
vtldVTLDeviceARN :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceARN = lens _vtldVTLDeviceARN (\ s a -> s{_vtldVTLDeviceARN = a})

-- | Undocumented member.
vtldVTLDeviceType :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceType = lens _vtldVTLDeviceType (\ s a -> s{_vtldVTLDeviceType = a})

-- | Undocumented member.
vtldVTLDeviceProductIdentifier :: Lens' VTLDevice (Maybe Text)
vtldVTLDeviceProductIdentifier = lens _vtldVTLDeviceProductIdentifier (\ s a -> s{_vtldVTLDeviceProductIdentifier = a})

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

instance Hashable VTLDevice where

instance NFData VTLDevice where

-- | Describes a storage volume object.
--
--
--
-- /See:/ 'volumeInfo' smart constructor.
data VolumeInfo = VolumeInfo'
  { _viGatewayARN        :: !(Maybe Text)
  , _viVolumeARN         :: !(Maybe Text)
  , _viVolumeSizeInBytes :: !(Maybe Integer)
  , _viVolumeId          :: !(Maybe Text)
  , _viGatewayId         :: !(Maybe Text)
  , _viVolumeType        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VolumeInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'viGatewayARN' - Undocumented member.
--
-- * 'viVolumeARN' - The Amazon Resource Name (ARN) for the storage volume. For example, the following is a valid ARN: @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/volume/vol-1122AABB@  Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- * 'viVolumeSizeInBytes' - The size of the volume in bytes. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- * 'viVolumeId' - The unique identifier assigned to the volume. This ID becomes part of the volume Amazon Resource Name (ARN), which you use as input for other operations. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- * 'viGatewayId' - The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- * 'viVolumeType' - Undocumented member.
volumeInfo
    :: VolumeInfo
volumeInfo =
  VolumeInfo'
    { _viGatewayARN = Nothing
    , _viVolumeARN = Nothing
    , _viVolumeSizeInBytes = Nothing
    , _viVolumeId = Nothing
    , _viGatewayId = Nothing
    , _viVolumeType = Nothing
    }


-- | Undocumented member.
viGatewayARN :: Lens' VolumeInfo (Maybe Text)
viGatewayARN = lens _viGatewayARN (\ s a -> s{_viGatewayARN = a})

-- | The Amazon Resource Name (ARN) for the storage volume. For example, the following is a valid ARN: @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/volume/vol-1122AABB@  Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
viVolumeARN :: Lens' VolumeInfo (Maybe Text)
viVolumeARN = lens _viVolumeARN (\ s a -> s{_viVolumeARN = a})

-- | The size of the volume in bytes. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
viVolumeSizeInBytes :: Lens' VolumeInfo (Maybe Integer)
viVolumeSizeInBytes = lens _viVolumeSizeInBytes (\ s a -> s{_viVolumeSizeInBytes = a})

-- | The unique identifier assigned to the volume. This ID becomes part of the volume Amazon Resource Name (ARN), which you use as input for other operations. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
viVolumeId :: Lens' VolumeInfo (Maybe Text)
viVolumeId = lens _viVolumeId (\ s a -> s{_viVolumeId = a})

-- | The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
viGatewayId :: Lens' VolumeInfo (Maybe Text)
viGatewayId = lens _viGatewayId (\ s a -> s{_viGatewayId = a})

-- | Undocumented member.
viVolumeType :: Lens' VolumeInfo (Maybe Text)
viVolumeType = lens _viVolumeType (\ s a -> s{_viVolumeType = a})

instance FromJSON VolumeInfo where
        parseJSON
          = withObject "VolumeInfo"
              (\ x ->
                 VolumeInfo' <$>
                   (x .:? "GatewayARN") <*> (x .:? "VolumeARN") <*>
                     (x .:? "VolumeSizeInBytes")
                     <*> (x .:? "VolumeId")
                     <*> (x .:? "GatewayId")
                     <*> (x .:? "VolumeType"))

instance Hashable VolumeInfo where

instance NFData VolumeInfo where

-- | /See:/ 'volumeRecoveryPointInfo' smart constructor.
data VolumeRecoveryPointInfo = VolumeRecoveryPointInfo'
  { _vrpiVolumeRecoveryPointTime :: !(Maybe Text)
  , _vrpiVolumeARN               :: !(Maybe Text)
  , _vrpiVolumeSizeInBytes       :: !(Maybe Integer)
  , _vrpiVolumeUsageInBytes      :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VolumeRecoveryPointInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vrpiVolumeRecoveryPointTime' - Undocumented member.
--
-- * 'vrpiVolumeARN' - Undocumented member.
--
-- * 'vrpiVolumeSizeInBytes' - Undocumented member.
--
-- * 'vrpiVolumeUsageInBytes' - Undocumented member.
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
vrpiVolumeRecoveryPointTime = lens _vrpiVolumeRecoveryPointTime (\ s a -> s{_vrpiVolumeRecoveryPointTime = a})

-- | Undocumented member.
vrpiVolumeARN :: Lens' VolumeRecoveryPointInfo (Maybe Text)
vrpiVolumeARN = lens _vrpiVolumeARN (\ s a -> s{_vrpiVolumeARN = a})

-- | Undocumented member.
vrpiVolumeSizeInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpiVolumeSizeInBytes = lens _vrpiVolumeSizeInBytes (\ s a -> s{_vrpiVolumeSizeInBytes = a})

-- | Undocumented member.
vrpiVolumeUsageInBytes :: Lens' VolumeRecoveryPointInfo (Maybe Integer)
vrpiVolumeUsageInBytes = lens _vrpiVolumeUsageInBytes (\ s a -> s{_vrpiVolumeUsageInBytes = a})

instance FromJSON VolumeRecoveryPointInfo where
        parseJSON
          = withObject "VolumeRecoveryPointInfo"
              (\ x ->
                 VolumeRecoveryPointInfo' <$>
                   (x .:? "VolumeRecoveryPointTime") <*>
                     (x .:? "VolumeARN")
                     <*> (x .:? "VolumeSizeInBytes")
                     <*> (x .:? "VolumeUsageInBytes"))

instance Hashable VolumeRecoveryPointInfo where

instance NFData VolumeRecoveryPointInfo where

-- | Lists iSCSI information about a volume.
--
--
--
-- /See:/ 'volumeiSCSIAttributes' smart constructor.
data VolumeiSCSIAttributes = VolumeiSCSIAttributes'
  { _vscsiaLunNumber            :: !(Maybe Nat)
  , _vscsiaTargetARN            :: !(Maybe Text)
  , _vscsiaChapEnabled          :: !(Maybe Bool)
  , _vscsiaNetworkInterfaceId   :: !(Maybe Text)
  , _vscsiaNetworkInterfacePort :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VolumeiSCSIAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vscsiaLunNumber' - The logical disk number.
--
-- * 'vscsiaTargetARN' - The Amazon Resource Name (ARN) of the volume target.
--
-- * 'vscsiaChapEnabled' - Indicates whether mutual CHAP is enabled for the iSCSI target.
--
-- * 'vscsiaNetworkInterfaceId' - The network interface identifier.
--
-- * 'vscsiaNetworkInterfacePort' - The port used to communicate with iSCSI targets.
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
vscsiaLunNumber = lens _vscsiaLunNumber (\ s a -> s{_vscsiaLunNumber = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the volume target.
vscsiaTargetARN :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaTargetARN = lens _vscsiaTargetARN (\ s a -> s{_vscsiaTargetARN = a})

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
vscsiaChapEnabled :: Lens' VolumeiSCSIAttributes (Maybe Bool)
vscsiaChapEnabled = lens _vscsiaChapEnabled (\ s a -> s{_vscsiaChapEnabled = a})

-- | The network interface identifier.
vscsiaNetworkInterfaceId :: Lens' VolumeiSCSIAttributes (Maybe Text)
vscsiaNetworkInterfaceId = lens _vscsiaNetworkInterfaceId (\ s a -> s{_vscsiaNetworkInterfaceId = a})

-- | The port used to communicate with iSCSI targets.
vscsiaNetworkInterfacePort :: Lens' VolumeiSCSIAttributes (Maybe Int)
vscsiaNetworkInterfacePort = lens _vscsiaNetworkInterfacePort (\ s a -> s{_vscsiaNetworkInterfacePort = a})

instance FromJSON VolumeiSCSIAttributes where
        parseJSON
          = withObject "VolumeiSCSIAttributes"
              (\ x ->
                 VolumeiSCSIAttributes' <$>
                   (x .:? "LunNumber") <*> (x .:? "TargetARN") <*>
                     (x .:? "ChapEnabled")
                     <*> (x .:? "NetworkInterfaceId")
                     <*> (x .:? "NetworkInterfacePort"))

instance Hashable VolumeiSCSIAttributes where

instance NFData VolumeiSCSIAttributes where
