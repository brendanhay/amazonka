-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.CachediSCSIVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.CachediSCSIVolume
  ( CachediSCSIVolume (..),

    -- * Smart constructor
    mkCachediSCSIVolume,

    -- * Lenses
    cscsivVolumeiSCSIAttributes,
    cscsivVolumeStatus,
    cscsivSourceSnapshotId,
    cscsivKMSKey,
    cscsivVolumeAttachmentStatus,
    cscsivVolumeARN,
    cscsivVolumeProgress,
    cscsivVolumeSizeInBytes,
    cscsivVolumeUsedInBytes,
    cscsivCreatedDate,
    cscsivVolumeId,
    cscsivVolumeType,
    cscsivTargetName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes

-- | Describes an iSCSI cached volume.
--
-- /See:/ 'mkCachediSCSIVolume' smart constructor.
data CachediSCSIVolume = CachediSCSIVolume'
  { volumeiSCSIAttributes ::
      Lude.Maybe VolumeiSCSIAttributes,
    volumeStatus :: Lude.Maybe Lude.Text,
    sourceSnapshotId :: Lude.Maybe Lude.Text,
    kmsKey :: Lude.Maybe Lude.Text,
    volumeAttachmentStatus :: Lude.Maybe Lude.Text,
    volumeARN :: Lude.Maybe Lude.Text,
    volumeProgress :: Lude.Maybe Lude.Double,
    volumeSizeInBytes :: Lude.Maybe Lude.Integer,
    volumeUsedInBytes :: Lude.Maybe Lude.Integer,
    createdDate :: Lude.Maybe Lude.Timestamp,
    volumeId :: Lude.Maybe Lude.Text,
    volumeType :: Lude.Maybe Lude.Text,
    targetName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CachediSCSIVolume' with the minimum fields required to make a request.
--
-- * 'createdDate' - The date the volume was created. Volumes created prior to March 28, 2017 don’t have this timestamp.
-- * 'kmsKey' - Undocumented field.
-- * 'sourceSnapshotId' - If the cached volume was created from a snapshot, this field contains the snapshot ID used, e.g., snap-78e22663. Otherwise, this field is not included.
-- * 'targetName' - The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
--
-- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the storage volume.
-- * 'volumeAttachmentStatus' - A value that indicates whether a storage volume is attached to or detached from a gateway. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway> .
-- * 'volumeId' - The unique identifier of the volume, e.g., vol-AE4B946D.
-- * 'volumeProgress' - Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the cached volume is not restoring or bootstrapping.
-- * 'volumeSizeInBytes' - The size, in bytes, of the volume capacity.
-- * 'volumeStatus' - One of the VolumeStatus values that indicates the state of the storage volume.
-- * 'volumeType' - One of the VolumeType enumeration values that describes the type of the volume.
-- * 'volumeUsedInBytes' - The size of the data stored on the volume in bytes. This value is calculated based on the number of blocks that are touched, instead of the actual amount of data written. This value can be useful for sequential write patterns but less accurate for random write patterns. @VolumeUsedInBytes@ is different from the compressed size of the volume, which is the value that is used to calculate your bill.
-- * 'volumeiSCSIAttributes' - An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
mkCachediSCSIVolume ::
  CachediSCSIVolume
mkCachediSCSIVolume =
  CachediSCSIVolume'
    { volumeiSCSIAttributes = Lude.Nothing,
      volumeStatus = Lude.Nothing,
      sourceSnapshotId = Lude.Nothing,
      kmsKey = Lude.Nothing,
      volumeAttachmentStatus = Lude.Nothing,
      volumeARN = Lude.Nothing,
      volumeProgress = Lude.Nothing,
      volumeSizeInBytes = Lude.Nothing,
      volumeUsedInBytes = Lude.Nothing,
      createdDate = Lude.Nothing,
      volumeId = Lude.Nothing,
      volumeType = Lude.Nothing,
      targetName = Lude.Nothing
    }

-- | An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
--
-- /Note:/ Consider using 'volumeiSCSIAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsivVolumeiSCSIAttributes :: Lens.Lens' CachediSCSIVolume (Lude.Maybe VolumeiSCSIAttributes)
cscsivVolumeiSCSIAttributes = Lens.lens (volumeiSCSIAttributes :: CachediSCSIVolume -> Lude.Maybe VolumeiSCSIAttributes) (\s a -> s {volumeiSCSIAttributes = a} :: CachediSCSIVolume)
{-# DEPRECATED cscsivVolumeiSCSIAttributes "Use generic-lens or generic-optics with 'volumeiSCSIAttributes' instead." #-}

-- | One of the VolumeStatus values that indicates the state of the storage volume.
--
-- /Note:/ Consider using 'volumeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsivVolumeStatus :: Lens.Lens' CachediSCSIVolume (Lude.Maybe Lude.Text)
cscsivVolumeStatus = Lens.lens (volumeStatus :: CachediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {volumeStatus = a} :: CachediSCSIVolume)
{-# DEPRECATED cscsivVolumeStatus "Use generic-lens or generic-optics with 'volumeStatus' instead." #-}

-- | If the cached volume was created from a snapshot, this field contains the snapshot ID used, e.g., snap-78e22663. Otherwise, this field is not included.
--
-- /Note:/ Consider using 'sourceSnapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsivSourceSnapshotId :: Lens.Lens' CachediSCSIVolume (Lude.Maybe Lude.Text)
cscsivSourceSnapshotId = Lens.lens (sourceSnapshotId :: CachediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {sourceSnapshotId = a} :: CachediSCSIVolume)
{-# DEPRECATED cscsivSourceSnapshotId "Use generic-lens or generic-optics with 'sourceSnapshotId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsivKMSKey :: Lens.Lens' CachediSCSIVolume (Lude.Maybe Lude.Text)
cscsivKMSKey = Lens.lens (kmsKey :: CachediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {kmsKey = a} :: CachediSCSIVolume)
{-# DEPRECATED cscsivKMSKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

-- | A value that indicates whether a storage volume is attached to or detached from a gateway. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway> .
--
-- /Note:/ Consider using 'volumeAttachmentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsivVolumeAttachmentStatus :: Lens.Lens' CachediSCSIVolume (Lude.Maybe Lude.Text)
cscsivVolumeAttachmentStatus = Lens.lens (volumeAttachmentStatus :: CachediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {volumeAttachmentStatus = a} :: CachediSCSIVolume)
{-# DEPRECATED cscsivVolumeAttachmentStatus "Use generic-lens or generic-optics with 'volumeAttachmentStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the storage volume.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsivVolumeARN :: Lens.Lens' CachediSCSIVolume (Lude.Maybe Lude.Text)
cscsivVolumeARN = Lens.lens (volumeARN :: CachediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {volumeARN = a} :: CachediSCSIVolume)
{-# DEPRECATED cscsivVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the cached volume is not restoring or bootstrapping.
--
-- /Note:/ Consider using 'volumeProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsivVolumeProgress :: Lens.Lens' CachediSCSIVolume (Lude.Maybe Lude.Double)
cscsivVolumeProgress = Lens.lens (volumeProgress :: CachediSCSIVolume -> Lude.Maybe Lude.Double) (\s a -> s {volumeProgress = a} :: CachediSCSIVolume)
{-# DEPRECATED cscsivVolumeProgress "Use generic-lens or generic-optics with 'volumeProgress' instead." #-}

-- | The size, in bytes, of the volume capacity.
--
-- /Note:/ Consider using 'volumeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsivVolumeSizeInBytes :: Lens.Lens' CachediSCSIVolume (Lude.Maybe Lude.Integer)
cscsivVolumeSizeInBytes = Lens.lens (volumeSizeInBytes :: CachediSCSIVolume -> Lude.Maybe Lude.Integer) (\s a -> s {volumeSizeInBytes = a} :: CachediSCSIVolume)
{-# DEPRECATED cscsivVolumeSizeInBytes "Use generic-lens or generic-optics with 'volumeSizeInBytes' instead." #-}

-- | The size of the data stored on the volume in bytes. This value is calculated based on the number of blocks that are touched, instead of the actual amount of data written. This value can be useful for sequential write patterns but less accurate for random write patterns. @VolumeUsedInBytes@ is different from the compressed size of the volume, which is the value that is used to calculate your bill.
--
-- /Note:/ Consider using 'volumeUsedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsivVolumeUsedInBytes :: Lens.Lens' CachediSCSIVolume (Lude.Maybe Lude.Integer)
cscsivVolumeUsedInBytes = Lens.lens (volumeUsedInBytes :: CachediSCSIVolume -> Lude.Maybe Lude.Integer) (\s a -> s {volumeUsedInBytes = a} :: CachediSCSIVolume)
{-# DEPRECATED cscsivVolumeUsedInBytes "Use generic-lens or generic-optics with 'volumeUsedInBytes' instead." #-}

-- | The date the volume was created. Volumes created prior to March 28, 2017 don’t have this timestamp.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsivCreatedDate :: Lens.Lens' CachediSCSIVolume (Lude.Maybe Lude.Timestamp)
cscsivCreatedDate = Lens.lens (createdDate :: CachediSCSIVolume -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: CachediSCSIVolume)
{-# DEPRECATED cscsivCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The unique identifier of the volume, e.g., vol-AE4B946D.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsivVolumeId :: Lens.Lens' CachediSCSIVolume (Lude.Maybe Lude.Text)
cscsivVolumeId = Lens.lens (volumeId :: CachediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {volumeId = a} :: CachediSCSIVolume)
{-# DEPRECATED cscsivVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | One of the VolumeType enumeration values that describes the type of the volume.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsivVolumeType :: Lens.Lens' CachediSCSIVolume (Lude.Maybe Lude.Text)
cscsivVolumeType = Lens.lens (volumeType :: CachediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {volumeType = a} :: CachediSCSIVolume)
{-# DEPRECATED cscsivVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
--
-- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
--
-- /Note:/ Consider using 'targetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsivTargetName :: Lens.Lens' CachediSCSIVolume (Lude.Maybe Lude.Text)
cscsivTargetName = Lens.lens (targetName :: CachediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {targetName = a} :: CachediSCSIVolume)
{-# DEPRECATED cscsivTargetName "Use generic-lens or generic-optics with 'targetName' instead." #-}

instance Lude.FromJSON CachediSCSIVolume where
  parseJSON =
    Lude.withObject
      "CachediSCSIVolume"
      ( \x ->
          CachediSCSIVolume'
            Lude.<$> (x Lude..:? "VolumeiSCSIAttributes")
            Lude.<*> (x Lude..:? "VolumeStatus")
            Lude.<*> (x Lude..:? "SourceSnapshotId")
            Lude.<*> (x Lude..:? "KMSKey")
            Lude.<*> (x Lude..:? "VolumeAttachmentStatus")
            Lude.<*> (x Lude..:? "VolumeARN")
            Lude.<*> (x Lude..:? "VolumeProgress")
            Lude.<*> (x Lude..:? "VolumeSizeInBytes")
            Lude.<*> (x Lude..:? "VolumeUsedInBytes")
            Lude.<*> (x Lude..:? "CreatedDate")
            Lude.<*> (x Lude..:? "VolumeId")
            Lude.<*> (x Lude..:? "VolumeType")
            Lude.<*> (x Lude..:? "TargetName")
      )
