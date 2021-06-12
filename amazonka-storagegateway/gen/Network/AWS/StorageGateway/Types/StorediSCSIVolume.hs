{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.StorediSCSIVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.StorediSCSIVolume where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes

-- | Describes an iSCSI stored volume.
--
-- /See:/ 'newStorediSCSIVolume' smart constructor.
data StorediSCSIVolume = StorediSCSIVolume'
  { -- | If the stored volume was created from a snapshot, this field contains
    -- the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not
    -- included.
    sourceSnapshotId :: Core.Maybe Core.Text,
    -- | One of the VolumeStatus values that indicates the state of the storage
    -- volume.
    volumeStatus :: Core.Maybe Core.Text,
    -- | The date the volume was created. Volumes created prior to March 28, 2017
    -- don’t have this timestamp.
    createdDate :: Core.Maybe Core.POSIX,
    -- | The name of the iSCSI target used by an initiator to connect to a volume
    -- and used as a suffix for the target ARN. For example, specifying
    -- @TargetName@ as /myvolume/ results in the target ARN of
    -- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
    -- The target name must be unique across all volumes on a gateway.
    --
    -- If you don\'t specify a value, Storage Gateway uses the value that was
    -- previously used for this volume as the new target name.
    targetName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the storage volume.
    volumeARN :: Core.Maybe Core.Text,
    -- | The unique identifier of the volume, e.g., vol-AE4B946D.
    volumeId :: Core.Maybe Core.Text,
    kmsKey :: Core.Maybe Core.Text,
    -- | Indicates if when the stored volume was created, existing data on the
    -- underlying local disk was preserved.
    --
    -- Valid Values: @true@ | @false@
    preservedExistingData :: Core.Maybe Core.Bool,
    -- | An VolumeiSCSIAttributes object that represents a collection of iSCSI
    -- attributes for one stored volume.
    volumeiSCSIAttributes :: Core.Maybe VolumeiSCSIAttributes,
    -- | The size of the data stored on the volume in bytes. This value is
    -- calculated based on the number of blocks that are touched, instead of
    -- the actual amount of data written. This value can be useful for
    -- sequential write patterns but less accurate for random write patterns.
    -- @VolumeUsedInBytes@ is different from the compressed size of the volume,
    -- which is the value that is used to calculate your bill.
    --
    -- This value is not available for volumes created prior to May 13, 2015,
    -- until you store data on the volume.
    volumeUsedInBytes :: Core.Maybe Core.Integer,
    -- | The size of the volume in bytes.
    volumeSizeInBytes :: Core.Maybe Core.Integer,
    -- | The ID of the local disk that was specified in the
    -- CreateStorediSCSIVolume operation.
    volumeDiskId :: Core.Maybe Core.Text,
    -- | One of the VolumeType enumeration values describing the type of the
    -- volume.
    volumeType :: Core.Maybe Core.Text,
    -- | A value that indicates whether a storage volume is attached to, detached
    -- from, or is in the process of detaching from a gateway. For more
    -- information, see
    -- <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway>.
    volumeAttachmentStatus :: Core.Maybe Core.Text,
    -- | Represents the percentage complete if the volume is restoring or
    -- bootstrapping that represents the percent of data transferred. This
    -- field does not appear in the response if the stored volume is not
    -- restoring or bootstrapping.
    volumeProgress :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StorediSCSIVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceSnapshotId', 'storediSCSIVolume_sourceSnapshotId' - If the stored volume was created from a snapshot, this field contains
-- the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not
-- included.
--
-- 'volumeStatus', 'storediSCSIVolume_volumeStatus' - One of the VolumeStatus values that indicates the state of the storage
-- volume.
--
-- 'createdDate', 'storediSCSIVolume_createdDate' - The date the volume was created. Volumes created prior to March 28, 2017
-- don’t have this timestamp.
--
-- 'targetName', 'storediSCSIVolume_targetName' - The name of the iSCSI target used by an initiator to connect to a volume
-- and used as a suffix for the target ARN. For example, specifying
-- @TargetName@ as /myvolume/ results in the target ARN of
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
-- The target name must be unique across all volumes on a gateway.
--
-- If you don\'t specify a value, Storage Gateway uses the value that was
-- previously used for this volume as the new target name.
--
-- 'volumeARN', 'storediSCSIVolume_volumeARN' - The Amazon Resource Name (ARN) of the storage volume.
--
-- 'volumeId', 'storediSCSIVolume_volumeId' - The unique identifier of the volume, e.g., vol-AE4B946D.
--
-- 'kmsKey', 'storediSCSIVolume_kmsKey' - Undocumented member.
--
-- 'preservedExistingData', 'storediSCSIVolume_preservedExistingData' - Indicates if when the stored volume was created, existing data on the
-- underlying local disk was preserved.
--
-- Valid Values: @true@ | @false@
--
-- 'volumeiSCSIAttributes', 'storediSCSIVolume_volumeiSCSIAttributes' - An VolumeiSCSIAttributes object that represents a collection of iSCSI
-- attributes for one stored volume.
--
-- 'volumeUsedInBytes', 'storediSCSIVolume_volumeUsedInBytes' - The size of the data stored on the volume in bytes. This value is
-- calculated based on the number of blocks that are touched, instead of
-- the actual amount of data written. This value can be useful for
-- sequential write patterns but less accurate for random write patterns.
-- @VolumeUsedInBytes@ is different from the compressed size of the volume,
-- which is the value that is used to calculate your bill.
--
-- This value is not available for volumes created prior to May 13, 2015,
-- until you store data on the volume.
--
-- 'volumeSizeInBytes', 'storediSCSIVolume_volumeSizeInBytes' - The size of the volume in bytes.
--
-- 'volumeDiskId', 'storediSCSIVolume_volumeDiskId' - The ID of the local disk that was specified in the
-- CreateStorediSCSIVolume operation.
--
-- 'volumeType', 'storediSCSIVolume_volumeType' - One of the VolumeType enumeration values describing the type of the
-- volume.
--
-- 'volumeAttachmentStatus', 'storediSCSIVolume_volumeAttachmentStatus' - A value that indicates whether a storage volume is attached to, detached
-- from, or is in the process of detaching from a gateway. For more
-- information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway>.
--
-- 'volumeProgress', 'storediSCSIVolume_volumeProgress' - Represents the percentage complete if the volume is restoring or
-- bootstrapping that represents the percent of data transferred. This
-- field does not appear in the response if the stored volume is not
-- restoring or bootstrapping.
newStorediSCSIVolume ::
  StorediSCSIVolume
newStorediSCSIVolume =
  StorediSCSIVolume'
    { sourceSnapshotId = Core.Nothing,
      volumeStatus = Core.Nothing,
      createdDate = Core.Nothing,
      targetName = Core.Nothing,
      volumeARN = Core.Nothing,
      volumeId = Core.Nothing,
      kmsKey = Core.Nothing,
      preservedExistingData = Core.Nothing,
      volumeiSCSIAttributes = Core.Nothing,
      volumeUsedInBytes = Core.Nothing,
      volumeSizeInBytes = Core.Nothing,
      volumeDiskId = Core.Nothing,
      volumeType = Core.Nothing,
      volumeAttachmentStatus = Core.Nothing,
      volumeProgress = Core.Nothing
    }

-- | If the stored volume was created from a snapshot, this field contains
-- the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not
-- included.
storediSCSIVolume_sourceSnapshotId :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Text)
storediSCSIVolume_sourceSnapshotId = Lens.lens (\StorediSCSIVolume' {sourceSnapshotId} -> sourceSnapshotId) (\s@StorediSCSIVolume' {} a -> s {sourceSnapshotId = a} :: StorediSCSIVolume)

-- | One of the VolumeStatus values that indicates the state of the storage
-- volume.
storediSCSIVolume_volumeStatus :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Text)
storediSCSIVolume_volumeStatus = Lens.lens (\StorediSCSIVolume' {volumeStatus} -> volumeStatus) (\s@StorediSCSIVolume' {} a -> s {volumeStatus = a} :: StorediSCSIVolume)

-- | The date the volume was created. Volumes created prior to March 28, 2017
-- don’t have this timestamp.
storediSCSIVolume_createdDate :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.UTCTime)
storediSCSIVolume_createdDate = Lens.lens (\StorediSCSIVolume' {createdDate} -> createdDate) (\s@StorediSCSIVolume' {} a -> s {createdDate = a} :: StorediSCSIVolume) Core.. Lens.mapping Core._Time

-- | The name of the iSCSI target used by an initiator to connect to a volume
-- and used as a suffix for the target ARN. For example, specifying
-- @TargetName@ as /myvolume/ results in the target ARN of
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
-- The target name must be unique across all volumes on a gateway.
--
-- If you don\'t specify a value, Storage Gateway uses the value that was
-- previously used for this volume as the new target name.
storediSCSIVolume_targetName :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Text)
storediSCSIVolume_targetName = Lens.lens (\StorediSCSIVolume' {targetName} -> targetName) (\s@StorediSCSIVolume' {} a -> s {targetName = a} :: StorediSCSIVolume)

-- | The Amazon Resource Name (ARN) of the storage volume.
storediSCSIVolume_volumeARN :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Text)
storediSCSIVolume_volumeARN = Lens.lens (\StorediSCSIVolume' {volumeARN} -> volumeARN) (\s@StorediSCSIVolume' {} a -> s {volumeARN = a} :: StorediSCSIVolume)

-- | The unique identifier of the volume, e.g., vol-AE4B946D.
storediSCSIVolume_volumeId :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Text)
storediSCSIVolume_volumeId = Lens.lens (\StorediSCSIVolume' {volumeId} -> volumeId) (\s@StorediSCSIVolume' {} a -> s {volumeId = a} :: StorediSCSIVolume)

-- | Undocumented member.
storediSCSIVolume_kmsKey :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Text)
storediSCSIVolume_kmsKey = Lens.lens (\StorediSCSIVolume' {kmsKey} -> kmsKey) (\s@StorediSCSIVolume' {} a -> s {kmsKey = a} :: StorediSCSIVolume)

-- | Indicates if when the stored volume was created, existing data on the
-- underlying local disk was preserved.
--
-- Valid Values: @true@ | @false@
storediSCSIVolume_preservedExistingData :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Bool)
storediSCSIVolume_preservedExistingData = Lens.lens (\StorediSCSIVolume' {preservedExistingData} -> preservedExistingData) (\s@StorediSCSIVolume' {} a -> s {preservedExistingData = a} :: StorediSCSIVolume)

-- | An VolumeiSCSIAttributes object that represents a collection of iSCSI
-- attributes for one stored volume.
storediSCSIVolume_volumeiSCSIAttributes :: Lens.Lens' StorediSCSIVolume (Core.Maybe VolumeiSCSIAttributes)
storediSCSIVolume_volumeiSCSIAttributes = Lens.lens (\StorediSCSIVolume' {volumeiSCSIAttributes} -> volumeiSCSIAttributes) (\s@StorediSCSIVolume' {} a -> s {volumeiSCSIAttributes = a} :: StorediSCSIVolume)

-- | The size of the data stored on the volume in bytes. This value is
-- calculated based on the number of blocks that are touched, instead of
-- the actual amount of data written. This value can be useful for
-- sequential write patterns but less accurate for random write patterns.
-- @VolumeUsedInBytes@ is different from the compressed size of the volume,
-- which is the value that is used to calculate your bill.
--
-- This value is not available for volumes created prior to May 13, 2015,
-- until you store data on the volume.
storediSCSIVolume_volumeUsedInBytes :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Integer)
storediSCSIVolume_volumeUsedInBytes = Lens.lens (\StorediSCSIVolume' {volumeUsedInBytes} -> volumeUsedInBytes) (\s@StorediSCSIVolume' {} a -> s {volumeUsedInBytes = a} :: StorediSCSIVolume)

-- | The size of the volume in bytes.
storediSCSIVolume_volumeSizeInBytes :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Integer)
storediSCSIVolume_volumeSizeInBytes = Lens.lens (\StorediSCSIVolume' {volumeSizeInBytes} -> volumeSizeInBytes) (\s@StorediSCSIVolume' {} a -> s {volumeSizeInBytes = a} :: StorediSCSIVolume)

-- | The ID of the local disk that was specified in the
-- CreateStorediSCSIVolume operation.
storediSCSIVolume_volumeDiskId :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Text)
storediSCSIVolume_volumeDiskId = Lens.lens (\StorediSCSIVolume' {volumeDiskId} -> volumeDiskId) (\s@StorediSCSIVolume' {} a -> s {volumeDiskId = a} :: StorediSCSIVolume)

-- | One of the VolumeType enumeration values describing the type of the
-- volume.
storediSCSIVolume_volumeType :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Text)
storediSCSIVolume_volumeType = Lens.lens (\StorediSCSIVolume' {volumeType} -> volumeType) (\s@StorediSCSIVolume' {} a -> s {volumeType = a} :: StorediSCSIVolume)

-- | A value that indicates whether a storage volume is attached to, detached
-- from, or is in the process of detaching from a gateway. For more
-- information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway>.
storediSCSIVolume_volumeAttachmentStatus :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Text)
storediSCSIVolume_volumeAttachmentStatus = Lens.lens (\StorediSCSIVolume' {volumeAttachmentStatus} -> volumeAttachmentStatus) (\s@StorediSCSIVolume' {} a -> s {volumeAttachmentStatus = a} :: StorediSCSIVolume)

-- | Represents the percentage complete if the volume is restoring or
-- bootstrapping that represents the percent of data transferred. This
-- field does not appear in the response if the stored volume is not
-- restoring or bootstrapping.
storediSCSIVolume_volumeProgress :: Lens.Lens' StorediSCSIVolume (Core.Maybe Core.Double)
storediSCSIVolume_volumeProgress = Lens.lens (\StorediSCSIVolume' {volumeProgress} -> volumeProgress) (\s@StorediSCSIVolume' {} a -> s {volumeProgress = a} :: StorediSCSIVolume)

instance Core.FromJSON StorediSCSIVolume where
  parseJSON =
    Core.withObject
      "StorediSCSIVolume"
      ( \x ->
          StorediSCSIVolume'
            Core.<$> (x Core..:? "SourceSnapshotId")
            Core.<*> (x Core..:? "VolumeStatus")
            Core.<*> (x Core..:? "CreatedDate")
            Core.<*> (x Core..:? "TargetName")
            Core.<*> (x Core..:? "VolumeARN")
            Core.<*> (x Core..:? "VolumeId")
            Core.<*> (x Core..:? "KMSKey")
            Core.<*> (x Core..:? "PreservedExistingData")
            Core.<*> (x Core..:? "VolumeiSCSIAttributes")
            Core.<*> (x Core..:? "VolumeUsedInBytes")
            Core.<*> (x Core..:? "VolumeSizeInBytes")
            Core.<*> (x Core..:? "VolumeDiskId")
            Core.<*> (x Core..:? "VolumeType")
            Core.<*> (x Core..:? "VolumeAttachmentStatus")
            Core.<*> (x Core..:? "VolumeProgress")
      )

instance Core.Hashable StorediSCSIVolume

instance Core.NFData StorediSCSIVolume
