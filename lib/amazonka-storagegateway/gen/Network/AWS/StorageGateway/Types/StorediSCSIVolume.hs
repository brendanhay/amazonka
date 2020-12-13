{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.StorediSCSIVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.StorediSCSIVolume
  ( StorediSCSIVolume (..),

    -- * Smart constructor
    mkStorediSCSIVolume,

    -- * Lenses
    sscsivVolumeiSCSIAttributes,
    sscsivVolumeStatus,
    sscsivSourceSnapshotId,
    sscsivPreservedExistingData,
    sscsivKMSKey,
    sscsivVolumeAttachmentStatus,
    sscsivVolumeARN,
    sscsivVolumeProgress,
    sscsivVolumeSizeInBytes,
    sscsivVolumeUsedInBytes,
    sscsivCreatedDate,
    sscsivVolumeId,
    sscsivVolumeDiskId,
    sscsivVolumeType,
    sscsivTargetName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes

-- | Describes an iSCSI stored volume.
--
-- /See:/ 'mkStorediSCSIVolume' smart constructor.
data StorediSCSIVolume = StorediSCSIVolume'
  { -- | An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
    volumeiSCSIAttributes :: Lude.Maybe VolumeiSCSIAttributes,
    -- | One of the VolumeStatus values that indicates the state of the storage volume.
    volumeStatus :: Lude.Maybe Lude.Text,
    -- | If the stored volume was created from a snapshot, this field contains the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not included.
    sourceSnapshotId :: Lude.Maybe Lude.Text,
    -- | Indicates if when the stored volume was created, existing data on the underlying local disk was preserved.
    --
    -- Valid Values: @true@ | @false@
    preservedExistingData :: Lude.Maybe Lude.Bool,
    kmsKey :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether a storage volume is attached to, detached from, or is in the process of detaching from a gateway. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway> .
    volumeAttachmentStatus :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the storage volume.
    volumeARN :: Lude.Maybe Lude.Text,
    -- | Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the stored volume is not restoring or bootstrapping.
    volumeProgress :: Lude.Maybe Lude.Double,
    -- | The size of the volume in bytes.
    volumeSizeInBytes :: Lude.Maybe Lude.Integer,
    -- | The size of the data stored on the volume in bytes. This value is calculated based on the number of blocks that are touched, instead of the actual amount of data written. This value can be useful for sequential write patterns but less accurate for random write patterns. @VolumeUsedInBytes@ is different from the compressed size of the volume, which is the value that is used to calculate your bill.
    volumeUsedInBytes :: Lude.Maybe Lude.Integer,
    -- | The date the volume was created. Volumes created prior to March 28, 2017 don’t have this timestamp.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The unique identifier of the volume, e.g., vol-AE4B946D.
    volumeId :: Lude.Maybe Lude.Text,
    -- | The ID of the local disk that was specified in the 'CreateStorediSCSIVolume' operation.
    volumeDiskId :: Lude.Maybe Lude.Text,
    -- | One of the VolumeType enumeration values describing the type of the volume.
    volumeType :: Lude.Maybe Lude.Text,
    -- | The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
    --
    -- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
    targetName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StorediSCSIVolume' with the minimum fields required to make a request.
--
-- * 'volumeiSCSIAttributes' - An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
-- * 'volumeStatus' - One of the VolumeStatus values that indicates the state of the storage volume.
-- * 'sourceSnapshotId' - If the stored volume was created from a snapshot, this field contains the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not included.
-- * 'preservedExistingData' - Indicates if when the stored volume was created, existing data on the underlying local disk was preserved.
--
-- Valid Values: @true@ | @false@
-- * 'kmsKey' -
-- * 'volumeAttachmentStatus' - A value that indicates whether a storage volume is attached to, detached from, or is in the process of detaching from a gateway. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway> .
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the storage volume.
-- * 'volumeProgress' - Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the stored volume is not restoring or bootstrapping.
-- * 'volumeSizeInBytes' - The size of the volume in bytes.
-- * 'volumeUsedInBytes' - The size of the data stored on the volume in bytes. This value is calculated based on the number of blocks that are touched, instead of the actual amount of data written. This value can be useful for sequential write patterns but less accurate for random write patterns. @VolumeUsedInBytes@ is different from the compressed size of the volume, which is the value that is used to calculate your bill.
-- * 'createdDate' - The date the volume was created. Volumes created prior to March 28, 2017 don’t have this timestamp.
-- * 'volumeId' - The unique identifier of the volume, e.g., vol-AE4B946D.
-- * 'volumeDiskId' - The ID of the local disk that was specified in the 'CreateStorediSCSIVolume' operation.
-- * 'volumeType' - One of the VolumeType enumeration values describing the type of the volume.
-- * 'targetName' - The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
--
-- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
mkStorediSCSIVolume ::
  StorediSCSIVolume
mkStorediSCSIVolume =
  StorediSCSIVolume'
    { volumeiSCSIAttributes = Lude.Nothing,
      volumeStatus = Lude.Nothing,
      sourceSnapshotId = Lude.Nothing,
      preservedExistingData = Lude.Nothing,
      kmsKey = Lude.Nothing,
      volumeAttachmentStatus = Lude.Nothing,
      volumeARN = Lude.Nothing,
      volumeProgress = Lude.Nothing,
      volumeSizeInBytes = Lude.Nothing,
      volumeUsedInBytes = Lude.Nothing,
      createdDate = Lude.Nothing,
      volumeId = Lude.Nothing,
      volumeDiskId = Lude.Nothing,
      volumeType = Lude.Nothing,
      targetName = Lude.Nothing
    }

-- | An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
--
-- /Note:/ Consider using 'volumeiSCSIAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeiSCSIAttributes :: Lens.Lens' StorediSCSIVolume (Lude.Maybe VolumeiSCSIAttributes)
sscsivVolumeiSCSIAttributes = Lens.lens (volumeiSCSIAttributes :: StorediSCSIVolume -> Lude.Maybe VolumeiSCSIAttributes) (\s a -> s {volumeiSCSIAttributes = a} :: StorediSCSIVolume)
{-# DEPRECATED sscsivVolumeiSCSIAttributes "Use generic-lens or generic-optics with 'volumeiSCSIAttributes' instead." #-}

-- | One of the VolumeStatus values that indicates the state of the storage volume.
--
-- /Note:/ Consider using 'volumeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeStatus :: Lens.Lens' StorediSCSIVolume (Lude.Maybe Lude.Text)
sscsivVolumeStatus = Lens.lens (volumeStatus :: StorediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {volumeStatus = a} :: StorediSCSIVolume)
{-# DEPRECATED sscsivVolumeStatus "Use generic-lens or generic-optics with 'volumeStatus' instead." #-}

-- | If the stored volume was created from a snapshot, this field contains the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not included.
--
-- /Note:/ Consider using 'sourceSnapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivSourceSnapshotId :: Lens.Lens' StorediSCSIVolume (Lude.Maybe Lude.Text)
sscsivSourceSnapshotId = Lens.lens (sourceSnapshotId :: StorediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {sourceSnapshotId = a} :: StorediSCSIVolume)
{-# DEPRECATED sscsivSourceSnapshotId "Use generic-lens or generic-optics with 'sourceSnapshotId' instead." #-}

-- | Indicates if when the stored volume was created, existing data on the underlying local disk was preserved.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'preservedExistingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivPreservedExistingData :: Lens.Lens' StorediSCSIVolume (Lude.Maybe Lude.Bool)
sscsivPreservedExistingData = Lens.lens (preservedExistingData :: StorediSCSIVolume -> Lude.Maybe Lude.Bool) (\s a -> s {preservedExistingData = a} :: StorediSCSIVolume)
{-# DEPRECATED sscsivPreservedExistingData "Use generic-lens or generic-optics with 'preservedExistingData' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivKMSKey :: Lens.Lens' StorediSCSIVolume (Lude.Maybe Lude.Text)
sscsivKMSKey = Lens.lens (kmsKey :: StorediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {kmsKey = a} :: StorediSCSIVolume)
{-# DEPRECATED sscsivKMSKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

-- | A value that indicates whether a storage volume is attached to, detached from, or is in the process of detaching from a gateway. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway> .
--
-- /Note:/ Consider using 'volumeAttachmentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeAttachmentStatus :: Lens.Lens' StorediSCSIVolume (Lude.Maybe Lude.Text)
sscsivVolumeAttachmentStatus = Lens.lens (volumeAttachmentStatus :: StorediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {volumeAttachmentStatus = a} :: StorediSCSIVolume)
{-# DEPRECATED sscsivVolumeAttachmentStatus "Use generic-lens or generic-optics with 'volumeAttachmentStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the storage volume.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeARN :: Lens.Lens' StorediSCSIVolume (Lude.Maybe Lude.Text)
sscsivVolumeARN = Lens.lens (volumeARN :: StorediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {volumeARN = a} :: StorediSCSIVolume)
{-# DEPRECATED sscsivVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the stored volume is not restoring or bootstrapping.
--
-- /Note:/ Consider using 'volumeProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeProgress :: Lens.Lens' StorediSCSIVolume (Lude.Maybe Lude.Double)
sscsivVolumeProgress = Lens.lens (volumeProgress :: StorediSCSIVolume -> Lude.Maybe Lude.Double) (\s a -> s {volumeProgress = a} :: StorediSCSIVolume)
{-# DEPRECATED sscsivVolumeProgress "Use generic-lens or generic-optics with 'volumeProgress' instead." #-}

-- | The size of the volume in bytes.
--
-- /Note:/ Consider using 'volumeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeSizeInBytes :: Lens.Lens' StorediSCSIVolume (Lude.Maybe Lude.Integer)
sscsivVolumeSizeInBytes = Lens.lens (volumeSizeInBytes :: StorediSCSIVolume -> Lude.Maybe Lude.Integer) (\s a -> s {volumeSizeInBytes = a} :: StorediSCSIVolume)
{-# DEPRECATED sscsivVolumeSizeInBytes "Use generic-lens or generic-optics with 'volumeSizeInBytes' instead." #-}

-- | The size of the data stored on the volume in bytes. This value is calculated based on the number of blocks that are touched, instead of the actual amount of data written. This value can be useful for sequential write patterns but less accurate for random write patterns. @VolumeUsedInBytes@ is different from the compressed size of the volume, which is the value that is used to calculate your bill.
--
-- /Note:/ Consider using 'volumeUsedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeUsedInBytes :: Lens.Lens' StorediSCSIVolume (Lude.Maybe Lude.Integer)
sscsivVolumeUsedInBytes = Lens.lens (volumeUsedInBytes :: StorediSCSIVolume -> Lude.Maybe Lude.Integer) (\s a -> s {volumeUsedInBytes = a} :: StorediSCSIVolume)
{-# DEPRECATED sscsivVolumeUsedInBytes "Use generic-lens or generic-optics with 'volumeUsedInBytes' instead." #-}

-- | The date the volume was created. Volumes created prior to March 28, 2017 don’t have this timestamp.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivCreatedDate :: Lens.Lens' StorediSCSIVolume (Lude.Maybe Lude.Timestamp)
sscsivCreatedDate = Lens.lens (createdDate :: StorediSCSIVolume -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: StorediSCSIVolume)
{-# DEPRECATED sscsivCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The unique identifier of the volume, e.g., vol-AE4B946D.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeId :: Lens.Lens' StorediSCSIVolume (Lude.Maybe Lude.Text)
sscsivVolumeId = Lens.lens (volumeId :: StorediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {volumeId = a} :: StorediSCSIVolume)
{-# DEPRECATED sscsivVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The ID of the local disk that was specified in the 'CreateStorediSCSIVolume' operation.
--
-- /Note:/ Consider using 'volumeDiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeDiskId :: Lens.Lens' StorediSCSIVolume (Lude.Maybe Lude.Text)
sscsivVolumeDiskId = Lens.lens (volumeDiskId :: StorediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {volumeDiskId = a} :: StorediSCSIVolume)
{-# DEPRECATED sscsivVolumeDiskId "Use generic-lens or generic-optics with 'volumeDiskId' instead." #-}

-- | One of the VolumeType enumeration values describing the type of the volume.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeType :: Lens.Lens' StorediSCSIVolume (Lude.Maybe Lude.Text)
sscsivVolumeType = Lens.lens (volumeType :: StorediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {volumeType = a} :: StorediSCSIVolume)
{-# DEPRECATED sscsivVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
--
-- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
--
-- /Note:/ Consider using 'targetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivTargetName :: Lens.Lens' StorediSCSIVolume (Lude.Maybe Lude.Text)
sscsivTargetName = Lens.lens (targetName :: StorediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {targetName = a} :: StorediSCSIVolume)
{-# DEPRECATED sscsivTargetName "Use generic-lens or generic-optics with 'targetName' instead." #-}

instance Lude.FromJSON StorediSCSIVolume where
  parseJSON =
    Lude.withObject
      "StorediSCSIVolume"
      ( \x ->
          StorediSCSIVolume'
            Lude.<$> (x Lude..:? "VolumeiSCSIAttributes")
            Lude.<*> (x Lude..:? "VolumeStatus")
            Lude.<*> (x Lude..:? "SourceSnapshotId")
            Lude.<*> (x Lude..:? "PreservedExistingData")
            Lude.<*> (x Lude..:? "KMSKey")
            Lude.<*> (x Lude..:? "VolumeAttachmentStatus")
            Lude.<*> (x Lude..:? "VolumeARN")
            Lude.<*> (x Lude..:? "VolumeProgress")
            Lude.<*> (x Lude..:? "VolumeSizeInBytes")
            Lude.<*> (x Lude..:? "VolumeUsedInBytes")
            Lude.<*> (x Lude..:? "CreatedDate")
            Lude.<*> (x Lude..:? "VolumeId")
            Lude.<*> (x Lude..:? "VolumeDiskId")
            Lude.<*> (x Lude..:? "VolumeType")
            Lude.<*> (x Lude..:? "TargetName")
      )
