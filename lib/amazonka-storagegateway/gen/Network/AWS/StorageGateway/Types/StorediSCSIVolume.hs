{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.StorediSCSIVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.StorediSCSIVolume
  ( StorediSCSIVolume (..)
  -- * Smart constructor
  , mkStorediSCSIVolume
  -- * Lenses
  , sscsivCreatedDate
  , sscsivKMSKey
  , sscsivPreservedExistingData
  , sscsivSourceSnapshotId
  , sscsivTargetName
  , sscsivVolumeARN
  , sscsivVolumeAttachmentStatus
  , sscsivVolumeDiskId
  , sscsivVolumeId
  , sscsivVolumeProgress
  , sscsivVolumeSizeInBytes
  , sscsivVolumeStatus
  , sscsivVolumeType
  , sscsivVolumeUsedInBytes
  , sscsivVolumeiSCSIAttributes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.KMSKey as Types
import qualified Network.AWS.StorageGateway.Types.SourceSnapshotId as Types
import qualified Network.AWS.StorageGateway.Types.TargetName as Types
import qualified Network.AWS.StorageGateway.Types.VolumeARN as Types
import qualified Network.AWS.StorageGateway.Types.VolumeAttachmentStatus as Types
import qualified Network.AWS.StorageGateway.Types.VolumeDiskId as Types
import qualified Network.AWS.StorageGateway.Types.VolumeId as Types
import qualified Network.AWS.StorageGateway.Types.VolumeStatus as Types
import qualified Network.AWS.StorageGateway.Types.VolumeType as Types
import qualified Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes as Types

-- | Describes an iSCSI stored volume.
--
-- /See:/ 'mkStorediSCSIVolume' smart constructor.
data StorediSCSIVolume = StorediSCSIVolume'
  { createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the volume was created. Volumes created prior to March 28, 2017 don’t have this timestamp.
  , kMSKey :: Core.Maybe Types.KMSKey
  , preservedExistingData :: Core.Maybe Core.Bool
    -- ^ Indicates if when the stored volume was created, existing data on the underlying local disk was preserved.
--
-- Valid Values: @true@ | @false@ 
  , sourceSnapshotId :: Core.Maybe Types.SourceSnapshotId
    -- ^ If the stored volume was created from a snapshot, this field contains the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not included.
  , targetName :: Core.Maybe Types.TargetName
    -- ^ The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
--
-- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
  , volumeARN :: Core.Maybe Types.VolumeARN
    -- ^ The Amazon Resource Name (ARN) of the storage volume.
  , volumeAttachmentStatus :: Core.Maybe Types.VolumeAttachmentStatus
    -- ^ A value that indicates whether a storage volume is attached to, detached from, or is in the process of detaching from a gateway. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway> .
  , volumeDiskId :: Core.Maybe Types.VolumeDiskId
    -- ^ The ID of the local disk that was specified in the 'CreateStorediSCSIVolume' operation.
  , volumeId :: Core.Maybe Types.VolumeId
    -- ^ The unique identifier of the volume, e.g., vol-AE4B946D.
  , volumeProgress :: Core.Maybe Core.Double
    -- ^ Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the stored volume is not restoring or bootstrapping.
  , volumeSizeInBytes :: Core.Maybe Core.Integer
    -- ^ The size of the volume in bytes.
  , volumeStatus :: Core.Maybe Types.VolumeStatus
    -- ^ One of the VolumeStatus values that indicates the state of the storage volume.
  , volumeType :: Core.Maybe Types.VolumeType
    -- ^ One of the VolumeType enumeration values describing the type of the volume.
  , volumeUsedInBytes :: Core.Maybe Core.Integer
    -- ^ The size of the data stored on the volume in bytes. This value is calculated based on the number of blocks that are touched, instead of the actual amount of data written. This value can be useful for sequential write patterns but less accurate for random write patterns. @VolumeUsedInBytes@ is different from the compressed size of the volume, which is the value that is used to calculate your bill.
  , volumeiSCSIAttributes :: Core.Maybe Types.VolumeiSCSIAttributes
    -- ^ An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StorediSCSIVolume' value with any optional fields omitted.
mkStorediSCSIVolume
    :: StorediSCSIVolume
mkStorediSCSIVolume
  = StorediSCSIVolume'{createdDate = Core.Nothing,
                       kMSKey = Core.Nothing, preservedExistingData = Core.Nothing,
                       sourceSnapshotId = Core.Nothing, targetName = Core.Nothing,
                       volumeARN = Core.Nothing, volumeAttachmentStatus = Core.Nothing,
                       volumeDiskId = Core.Nothing, volumeId = Core.Nothing,
                       volumeProgress = Core.Nothing, volumeSizeInBytes = Core.Nothing,
                       volumeStatus = Core.Nothing, volumeType = Core.Nothing,
                       volumeUsedInBytes = Core.Nothing,
                       volumeiSCSIAttributes = Core.Nothing}

-- | The date the volume was created. Volumes created prior to March 28, 2017 don’t have this timestamp.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivCreatedDate :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.NominalDiffTime)
sscsivCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE sscsivCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kMSKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivKMSKey :: Lens.Lens' StorediSCSIVolume (Core.Maybe Types.KMSKey)
sscsivKMSKey = Lens.field @"kMSKey"
{-# INLINEABLE sscsivKMSKey #-}
{-# DEPRECATED kMSKey "Use generic-lens or generic-optics with 'kMSKey' instead"  #-}

-- | Indicates if when the stored volume was created, existing data on the underlying local disk was preserved.
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'preservedExistingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivPreservedExistingData :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Bool)
sscsivPreservedExistingData = Lens.field @"preservedExistingData"
{-# INLINEABLE sscsivPreservedExistingData #-}
{-# DEPRECATED preservedExistingData "Use generic-lens or generic-optics with 'preservedExistingData' instead"  #-}

-- | If the stored volume was created from a snapshot, this field contains the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not included.
--
-- /Note:/ Consider using 'sourceSnapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivSourceSnapshotId :: Lens.Lens' StorediSCSIVolume (Core.Maybe Types.SourceSnapshotId)
sscsivSourceSnapshotId = Lens.field @"sourceSnapshotId"
{-# INLINEABLE sscsivSourceSnapshotId #-}
{-# DEPRECATED sourceSnapshotId "Use generic-lens or generic-optics with 'sourceSnapshotId' instead"  #-}

-- | The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
--
-- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
--
-- /Note:/ Consider using 'targetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivTargetName :: Lens.Lens' StorediSCSIVolume (Core.Maybe Types.TargetName)
sscsivTargetName = Lens.field @"targetName"
{-# INLINEABLE sscsivTargetName #-}
{-# DEPRECATED targetName "Use generic-lens or generic-optics with 'targetName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the storage volume.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeARN :: Lens.Lens' StorediSCSIVolume (Core.Maybe Types.VolumeARN)
sscsivVolumeARN = Lens.field @"volumeARN"
{-# INLINEABLE sscsivVolumeARN #-}
{-# DEPRECATED volumeARN "Use generic-lens or generic-optics with 'volumeARN' instead"  #-}

-- | A value that indicates whether a storage volume is attached to, detached from, or is in the process of detaching from a gateway. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway> .
--
-- /Note:/ Consider using 'volumeAttachmentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeAttachmentStatus :: Lens.Lens' StorediSCSIVolume (Core.Maybe Types.VolumeAttachmentStatus)
sscsivVolumeAttachmentStatus = Lens.field @"volumeAttachmentStatus"
{-# INLINEABLE sscsivVolumeAttachmentStatus #-}
{-# DEPRECATED volumeAttachmentStatus "Use generic-lens or generic-optics with 'volumeAttachmentStatus' instead"  #-}

-- | The ID of the local disk that was specified in the 'CreateStorediSCSIVolume' operation.
--
-- /Note:/ Consider using 'volumeDiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeDiskId :: Lens.Lens' StorediSCSIVolume (Core.Maybe Types.VolumeDiskId)
sscsivVolumeDiskId = Lens.field @"volumeDiskId"
{-# INLINEABLE sscsivVolumeDiskId #-}
{-# DEPRECATED volumeDiskId "Use generic-lens or generic-optics with 'volumeDiskId' instead"  #-}

-- | The unique identifier of the volume, e.g., vol-AE4B946D.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeId :: Lens.Lens' StorediSCSIVolume (Core.Maybe Types.VolumeId)
sscsivVolumeId = Lens.field @"volumeId"
{-# INLINEABLE sscsivVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

-- | Represents the percentage complete if the volume is restoring or bootstrapping that represents the percent of data transferred. This field does not appear in the response if the stored volume is not restoring or bootstrapping.
--
-- /Note:/ Consider using 'volumeProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeProgress :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Double)
sscsivVolumeProgress = Lens.field @"volumeProgress"
{-# INLINEABLE sscsivVolumeProgress #-}
{-# DEPRECATED volumeProgress "Use generic-lens or generic-optics with 'volumeProgress' instead"  #-}

-- | The size of the volume in bytes.
--
-- /Note:/ Consider using 'volumeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeSizeInBytes :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Integer)
sscsivVolumeSizeInBytes = Lens.field @"volumeSizeInBytes"
{-# INLINEABLE sscsivVolumeSizeInBytes #-}
{-# DEPRECATED volumeSizeInBytes "Use generic-lens or generic-optics with 'volumeSizeInBytes' instead"  #-}

-- | One of the VolumeStatus values that indicates the state of the storage volume.
--
-- /Note:/ Consider using 'volumeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeStatus :: Lens.Lens' StorediSCSIVolume (Core.Maybe Types.VolumeStatus)
sscsivVolumeStatus = Lens.field @"volumeStatus"
{-# INLINEABLE sscsivVolumeStatus #-}
{-# DEPRECATED volumeStatus "Use generic-lens or generic-optics with 'volumeStatus' instead"  #-}

-- | One of the VolumeType enumeration values describing the type of the volume.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeType :: Lens.Lens' StorediSCSIVolume (Core.Maybe Types.VolumeType)
sscsivVolumeType = Lens.field @"volumeType"
{-# INLINEABLE sscsivVolumeType #-}
{-# DEPRECATED volumeType "Use generic-lens or generic-optics with 'volumeType' instead"  #-}

-- | The size of the data stored on the volume in bytes. This value is calculated based on the number of blocks that are touched, instead of the actual amount of data written. This value can be useful for sequential write patterns but less accurate for random write patterns. @VolumeUsedInBytes@ is different from the compressed size of the volume, which is the value that is used to calculate your bill.
--
-- /Note:/ Consider using 'volumeUsedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeUsedInBytes :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Integer)
sscsivVolumeUsedInBytes = Lens.field @"volumeUsedInBytes"
{-# INLINEABLE sscsivVolumeUsedInBytes #-}
{-# DEPRECATED volumeUsedInBytes "Use generic-lens or generic-optics with 'volumeUsedInBytes' instead"  #-}

-- | An 'VolumeiSCSIAttributes' object that represents a collection of iSCSI attributes for one stored volume.
--
-- /Note:/ Consider using 'volumeiSCSIAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscsivVolumeiSCSIAttributes :: Lens.Lens' StorediSCSIVolume (Core.Maybe Types.VolumeiSCSIAttributes)
sscsivVolumeiSCSIAttributes = Lens.field @"volumeiSCSIAttributes"
{-# INLINEABLE sscsivVolumeiSCSIAttributes #-}
{-# DEPRECATED volumeiSCSIAttributes "Use generic-lens or generic-optics with 'volumeiSCSIAttributes' instead"  #-}

instance Core.FromJSON StorediSCSIVolume where
        parseJSON
          = Core.withObject "StorediSCSIVolume" Core.$
              \ x ->
                StorediSCSIVolume' Core.<$>
                  (x Core..:? "CreatedDate") Core.<*> x Core..:? "KMSKey" Core.<*>
                    x Core..:? "PreservedExistingData"
                    Core.<*> x Core..:? "SourceSnapshotId"
                    Core.<*> x Core..:? "TargetName"
                    Core.<*> x Core..:? "VolumeARN"
                    Core.<*> x Core..:? "VolumeAttachmentStatus"
                    Core.<*> x Core..:? "VolumeDiskId"
                    Core.<*> x Core..:? "VolumeId"
                    Core.<*> x Core..:? "VolumeProgress"
                    Core.<*> x Core..:? "VolumeSizeInBytes"
                    Core.<*> x Core..:? "VolumeStatus"
                    Core.<*> x Core..:? "VolumeType"
                    Core.<*> x Core..:? "VolumeUsedInBytes"
                    Core.<*> x Core..:? "VolumeiSCSIAttributes"
