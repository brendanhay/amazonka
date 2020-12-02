{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.CachediSCSIVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.CachediSCSIVolume where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes

-- | Describes an iSCSI cached volume.
--
--
--
-- /See:/ 'cachediSCSIVolume' smart constructor.
data CachediSCSIVolume = CachediSCSIVolume'
  { _cscsivVolumeiSCSIAttributes ::
      !(Maybe VolumeiSCSIAttributes),
    _cscsivVolumeStatus :: !(Maybe Text),
    _cscsivSourceSnapshotId :: !(Maybe Text),
    _cscsivKMSKey :: !(Maybe Text),
    _cscsivVolumeAttachmentStatus :: !(Maybe Text),
    _cscsivVolumeARN :: !(Maybe Text),
    _cscsivVolumeProgress :: !(Maybe Double),
    _cscsivVolumeSizeInBytes :: !(Maybe Integer),
    _cscsivVolumeUsedInBytes :: !(Maybe Integer),
    _cscsivCreatedDate :: !(Maybe POSIX),
    _cscsivVolumeId :: !(Maybe Text),
    _cscsivVolumeType :: !(Maybe Text),
    _cscsivTargetName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CachediSCSIVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscsivVolumeiSCSIAttributes' - An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
--
-- * 'cscsivVolumeStatus' - One of the VolumeStatus values that indicates the state of the storage volume.
--
-- * 'cscsivSourceSnapshotId' - If the cached volume was created from a snapshot, this field contains the snapshot ID used, e.g., snap-78e22663. Otherwise, this field is not included.
--
-- * 'cscsivKMSKey' - Undocumented member.
--
-- * 'cscsivVolumeAttachmentStatus' - A value that indicates whether a storage volume is attached to or detached from a gateway. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway> .
--
-- * 'cscsivVolumeARN' - The Amazon Resource Name (ARN) of the storage volume.
--
-- * 'cscsivVolumeProgress' - Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the cached volume is not restoring or bootstrapping.
--
-- * 'cscsivVolumeSizeInBytes' - The size, in bytes, of the volume capacity.
--
-- * 'cscsivVolumeUsedInBytes' - The size of the data stored on the volume in bytes. This value is calculated based on the number of blocks that are touched, instead of the actual amount of data written. This value can be useful for sequential write patterns but less accurate for random write patterns. @VolumeUsedInBytes@ is different from the compressed size of the volume, which is the value that is used to calculate your bill.
--
-- * 'cscsivCreatedDate' - The date the volume was created. Volumes created prior to March 28, 2017 don’t have this timestamp.
--
-- * 'cscsivVolumeId' - The unique identifier of the volume, e.g., vol-AE4B946D.
--
-- * 'cscsivVolumeType' - One of the VolumeType enumeration values that describes the type of the volume.
--
-- * 'cscsivTargetName' - The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway. If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
cachediSCSIVolume ::
  CachediSCSIVolume
cachediSCSIVolume =
  CachediSCSIVolume'
    { _cscsivVolumeiSCSIAttributes = Nothing,
      _cscsivVolumeStatus = Nothing,
      _cscsivSourceSnapshotId = Nothing,
      _cscsivKMSKey = Nothing,
      _cscsivVolumeAttachmentStatus = Nothing,
      _cscsivVolumeARN = Nothing,
      _cscsivVolumeProgress = Nothing,
      _cscsivVolumeSizeInBytes = Nothing,
      _cscsivVolumeUsedInBytes = Nothing,
      _cscsivCreatedDate = Nothing,
      _cscsivVolumeId = Nothing,
      _cscsivVolumeType = Nothing,
      _cscsivTargetName = Nothing
    }

-- | An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
cscsivVolumeiSCSIAttributes :: Lens' CachediSCSIVolume (Maybe VolumeiSCSIAttributes)
cscsivVolumeiSCSIAttributes = lens _cscsivVolumeiSCSIAttributes (\s a -> s {_cscsivVolumeiSCSIAttributes = a})

-- | One of the VolumeStatus values that indicates the state of the storage volume.
cscsivVolumeStatus :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeStatus = lens _cscsivVolumeStatus (\s a -> s {_cscsivVolumeStatus = a})

-- | If the cached volume was created from a snapshot, this field contains the snapshot ID used, e.g., snap-78e22663. Otherwise, this field is not included.
cscsivSourceSnapshotId :: Lens' CachediSCSIVolume (Maybe Text)
cscsivSourceSnapshotId = lens _cscsivSourceSnapshotId (\s a -> s {_cscsivSourceSnapshotId = a})

-- | Undocumented member.
cscsivKMSKey :: Lens' CachediSCSIVolume (Maybe Text)
cscsivKMSKey = lens _cscsivKMSKey (\s a -> s {_cscsivKMSKey = a})

-- | A value that indicates whether a storage volume is attached to or detached from a gateway. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway> .
cscsivVolumeAttachmentStatus :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeAttachmentStatus = lens _cscsivVolumeAttachmentStatus (\s a -> s {_cscsivVolumeAttachmentStatus = a})

-- | The Amazon Resource Name (ARN) of the storage volume.
cscsivVolumeARN :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeARN = lens _cscsivVolumeARN (\s a -> s {_cscsivVolumeARN = a})

-- | Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the cached volume is not restoring or bootstrapping.
cscsivVolumeProgress :: Lens' CachediSCSIVolume (Maybe Double)
cscsivVolumeProgress = lens _cscsivVolumeProgress (\s a -> s {_cscsivVolumeProgress = a})

-- | The size, in bytes, of the volume capacity.
cscsivVolumeSizeInBytes :: Lens' CachediSCSIVolume (Maybe Integer)
cscsivVolumeSizeInBytes = lens _cscsivVolumeSizeInBytes (\s a -> s {_cscsivVolumeSizeInBytes = a})

-- | The size of the data stored on the volume in bytes. This value is calculated based on the number of blocks that are touched, instead of the actual amount of data written. This value can be useful for sequential write patterns but less accurate for random write patterns. @VolumeUsedInBytes@ is different from the compressed size of the volume, which is the value that is used to calculate your bill.
cscsivVolumeUsedInBytes :: Lens' CachediSCSIVolume (Maybe Integer)
cscsivVolumeUsedInBytes = lens _cscsivVolumeUsedInBytes (\s a -> s {_cscsivVolumeUsedInBytes = a})

-- | The date the volume was created. Volumes created prior to March 28, 2017 don’t have this timestamp.
cscsivCreatedDate :: Lens' CachediSCSIVolume (Maybe UTCTime)
cscsivCreatedDate = lens _cscsivCreatedDate (\s a -> s {_cscsivCreatedDate = a}) . mapping _Time

-- | The unique identifier of the volume, e.g., vol-AE4B946D.
cscsivVolumeId :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeId = lens _cscsivVolumeId (\s a -> s {_cscsivVolumeId = a})

-- | One of the VolumeType enumeration values that describes the type of the volume.
cscsivVolumeType :: Lens' CachediSCSIVolume (Maybe Text)
cscsivVolumeType = lens _cscsivVolumeType (\s a -> s {_cscsivVolumeType = a})

-- | The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway. If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
cscsivTargetName :: Lens' CachediSCSIVolume (Maybe Text)
cscsivTargetName = lens _cscsivTargetName (\s a -> s {_cscsivTargetName = a})

instance FromJSON CachediSCSIVolume where
  parseJSON =
    withObject
      "CachediSCSIVolume"
      ( \x ->
          CachediSCSIVolume'
            <$> (x .:? "VolumeiSCSIAttributes")
            <*> (x .:? "VolumeStatus")
            <*> (x .:? "SourceSnapshotId")
            <*> (x .:? "KMSKey")
            <*> (x .:? "VolumeAttachmentStatus")
            <*> (x .:? "VolumeARN")
            <*> (x .:? "VolumeProgress")
            <*> (x .:? "VolumeSizeInBytes")
            <*> (x .:? "VolumeUsedInBytes")
            <*> (x .:? "CreatedDate")
            <*> (x .:? "VolumeId")
            <*> (x .:? "VolumeType")
            <*> (x .:? "TargetName")
      )

instance Hashable CachediSCSIVolume

instance NFData CachediSCSIVolume
