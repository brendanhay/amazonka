{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.StorediSCSIVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.StorediSCSIVolume where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes

-- | Describes an iSCSI stored volume.
--
--
--
-- /See:/ 'storediSCSIVolume' smart constructor.
data StorediSCSIVolume = StorediSCSIVolume'
  { _sscsivVolumeiSCSIAttributes ::
      !(Maybe VolumeiSCSIAttributes),
    _sscsivVolumeStatus :: !(Maybe Text),
    _sscsivSourceSnapshotId :: !(Maybe Text),
    _sscsivPreservedExistingData :: !(Maybe Bool),
    _sscsivKMSKey :: !(Maybe Text),
    _sscsivVolumeAttachmentStatus :: !(Maybe Text),
    _sscsivVolumeARN :: !(Maybe Text),
    _sscsivVolumeProgress :: !(Maybe Double),
    _sscsivVolumeSizeInBytes :: !(Maybe Integer),
    _sscsivVolumeUsedInBytes :: !(Maybe Integer),
    _sscsivCreatedDate :: !(Maybe POSIX),
    _sscsivVolumeId :: !(Maybe Text),
    _sscsivVolumeDiskId :: !(Maybe Text),
    _sscsivVolumeType :: !(Maybe Text),
    _sscsivTargetName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
-- * 'sscsivPreservedExistingData' - Indicates if when the stored volume was created, existing data on the underlying local disk was preserved. Valid Values: @true@ | @false@
--
-- * 'sscsivKMSKey' - Undocumented member.
--
-- * 'sscsivVolumeAttachmentStatus' - A value that indicates whether a storage volume is attached to, detached from, or is in the process of detaching from a gateway. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway> .
--
-- * 'sscsivVolumeARN' - The Amazon Resource Name (ARN) of the storage volume.
--
-- * 'sscsivVolumeProgress' - Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the stored volume is not restoring or bootstrapping.
--
-- * 'sscsivVolumeSizeInBytes' - The size of the volume in bytes.
--
-- * 'sscsivVolumeUsedInBytes' - The size of the data stored on the volume in bytes. This value is calculated based on the number of blocks that are touched, instead of the actual amount of data written. This value can be useful for sequential write patterns but less accurate for random write patterns. @VolumeUsedInBytes@ is different from the compressed size of the volume, which is the value that is used to calculate your bill.
--
-- * 'sscsivCreatedDate' - The date the volume was created. Volumes created prior to March 28, 2017 don’t have this timestamp.
--
-- * 'sscsivVolumeId' - The unique identifier of the volume, e.g., vol-AE4B946D.
--
-- * 'sscsivVolumeDiskId' - The ID of the local disk that was specified in the 'CreateStorediSCSIVolume' operation.
--
-- * 'sscsivVolumeType' - One of the VolumeType enumeration values describing the type of the volume.
--
-- * 'sscsivTargetName' - The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway. If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
storediSCSIVolume ::
  StorediSCSIVolume
storediSCSIVolume =
  StorediSCSIVolume'
    { _sscsivVolumeiSCSIAttributes = Nothing,
      _sscsivVolumeStatus = Nothing,
      _sscsivSourceSnapshotId = Nothing,
      _sscsivPreservedExistingData = Nothing,
      _sscsivKMSKey = Nothing,
      _sscsivVolumeAttachmentStatus = Nothing,
      _sscsivVolumeARN = Nothing,
      _sscsivVolumeProgress = Nothing,
      _sscsivVolumeSizeInBytes = Nothing,
      _sscsivVolumeUsedInBytes = Nothing,
      _sscsivCreatedDate = Nothing,
      _sscsivVolumeId = Nothing,
      _sscsivVolumeDiskId = Nothing,
      _sscsivVolumeType = Nothing,
      _sscsivTargetName = Nothing
    }

-- | An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
sscsivVolumeiSCSIAttributes :: Lens' StorediSCSIVolume (Maybe VolumeiSCSIAttributes)
sscsivVolumeiSCSIAttributes = lens _sscsivVolumeiSCSIAttributes (\s a -> s {_sscsivVolumeiSCSIAttributes = a})

-- | One of the VolumeStatus values that indicates the state of the storage volume.
sscsivVolumeStatus :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeStatus = lens _sscsivVolumeStatus (\s a -> s {_sscsivVolumeStatus = a})

-- | If the stored volume was created from a snapshot, this field contains the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not included.
sscsivSourceSnapshotId :: Lens' StorediSCSIVolume (Maybe Text)
sscsivSourceSnapshotId = lens _sscsivSourceSnapshotId (\s a -> s {_sscsivSourceSnapshotId = a})

-- | Indicates if when the stored volume was created, existing data on the underlying local disk was preserved. Valid Values: @true@ | @false@
sscsivPreservedExistingData :: Lens' StorediSCSIVolume (Maybe Bool)
sscsivPreservedExistingData = lens _sscsivPreservedExistingData (\s a -> s {_sscsivPreservedExistingData = a})

-- | Undocumented member.
sscsivKMSKey :: Lens' StorediSCSIVolume (Maybe Text)
sscsivKMSKey = lens _sscsivKMSKey (\s a -> s {_sscsivKMSKey = a})

-- | A value that indicates whether a storage volume is attached to, detached from, or is in the process of detaching from a gateway. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway> .
sscsivVolumeAttachmentStatus :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeAttachmentStatus = lens _sscsivVolumeAttachmentStatus (\s a -> s {_sscsivVolumeAttachmentStatus = a})

-- | The Amazon Resource Name (ARN) of the storage volume.
sscsivVolumeARN :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeARN = lens _sscsivVolumeARN (\s a -> s {_sscsivVolumeARN = a})

-- | Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the stored volume is not restoring or bootstrapping.
sscsivVolumeProgress :: Lens' StorediSCSIVolume (Maybe Double)
sscsivVolumeProgress = lens _sscsivVolumeProgress (\s a -> s {_sscsivVolumeProgress = a})

-- | The size of the volume in bytes.
sscsivVolumeSizeInBytes :: Lens' StorediSCSIVolume (Maybe Integer)
sscsivVolumeSizeInBytes = lens _sscsivVolumeSizeInBytes (\s a -> s {_sscsivVolumeSizeInBytes = a})

-- | The size of the data stored on the volume in bytes. This value is calculated based on the number of blocks that are touched, instead of the actual amount of data written. This value can be useful for sequential write patterns but less accurate for random write patterns. @VolumeUsedInBytes@ is different from the compressed size of the volume, which is the value that is used to calculate your bill.
sscsivVolumeUsedInBytes :: Lens' StorediSCSIVolume (Maybe Integer)
sscsivVolumeUsedInBytes = lens _sscsivVolumeUsedInBytes (\s a -> s {_sscsivVolumeUsedInBytes = a})

-- | The date the volume was created. Volumes created prior to March 28, 2017 don’t have this timestamp.
sscsivCreatedDate :: Lens' StorediSCSIVolume (Maybe UTCTime)
sscsivCreatedDate = lens _sscsivCreatedDate (\s a -> s {_sscsivCreatedDate = a}) . mapping _Time

-- | The unique identifier of the volume, e.g., vol-AE4B946D.
sscsivVolumeId :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeId = lens _sscsivVolumeId (\s a -> s {_sscsivVolumeId = a})

-- | The ID of the local disk that was specified in the 'CreateStorediSCSIVolume' operation.
sscsivVolumeDiskId :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeDiskId = lens _sscsivVolumeDiskId (\s a -> s {_sscsivVolumeDiskId = a})

-- | One of the VolumeType enumeration values describing the type of the volume.
sscsivVolumeType :: Lens' StorediSCSIVolume (Maybe Text)
sscsivVolumeType = lens _sscsivVolumeType (\s a -> s {_sscsivVolumeType = a})

-- | The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway. If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
sscsivTargetName :: Lens' StorediSCSIVolume (Maybe Text)
sscsivTargetName = lens _sscsivTargetName (\s a -> s {_sscsivTargetName = a})

instance FromJSON StorediSCSIVolume where
  parseJSON =
    withObject
      "StorediSCSIVolume"
      ( \x ->
          StorediSCSIVolume'
            <$> (x .:? "VolumeiSCSIAttributes")
            <*> (x .:? "VolumeStatus")
            <*> (x .:? "SourceSnapshotId")
            <*> (x .:? "PreservedExistingData")
            <*> (x .:? "KMSKey")
            <*> (x .:? "VolumeAttachmentStatus")
            <*> (x .:? "VolumeARN")
            <*> (x .:? "VolumeProgress")
            <*> (x .:? "VolumeSizeInBytes")
            <*> (x .:? "VolumeUsedInBytes")
            <*> (x .:? "CreatedDate")
            <*> (x .:? "VolumeId")
            <*> (x .:? "VolumeDiskId")
            <*> (x .:? "VolumeType")
            <*> (x .:? "TargetName")
      )

instance Hashable StorediSCSIVolume

instance NFData StorediSCSIVolume
