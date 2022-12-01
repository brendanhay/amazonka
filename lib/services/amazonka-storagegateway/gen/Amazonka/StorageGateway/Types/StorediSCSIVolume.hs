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
-- Module      : Amazonka.StorageGateway.Types.StorediSCSIVolume
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.StorediSCSIVolume where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.StorageGateway.Types.VolumeiSCSIAttributes

-- | Describes an iSCSI stored volume.
--
-- /See:/ 'newStorediSCSIVolume' smart constructor.
data StorediSCSIVolume = StorediSCSIVolume'
  { -- | The size of the data stored on the volume in bytes. This value is
    -- calculated based on the number of blocks that are touched, instead of
    -- the actual amount of data written. This value can be useful for
    -- sequential write patterns but less accurate for random write patterns.
    -- @VolumeUsedInBytes@ is different from the compressed size of the volume,
    -- which is the value that is used to calculate your bill.
    --
    -- This value is not available for volumes created prior to May 13, 2015,
    -- until you store data on the volume.
    volumeUsedInBytes :: Prelude.Maybe Prelude.Integer,
    -- | Represents the percentage complete if the volume is restoring or
    -- bootstrapping that represents the percent of data transferred. This
    -- field does not appear in the response if the stored volume is not
    -- restoring or bootstrapping.
    volumeProgress :: Prelude.Maybe Prelude.Double,
    -- | The size of the volume in bytes.
    volumeSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The name of the iSCSI target used by an initiator to connect to a volume
    -- and used as a suffix for the target ARN. For example, specifying
    -- @TargetName@ as /myvolume/ results in the target ARN of
    -- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
    -- The target name must be unique across all volumes on a gateway.
    --
    -- If you don\'t specify a value, Storage Gateway uses the value that was
    -- previously used for this volume as the new target name.
    targetName :: Prelude.Maybe Prelude.Text,
    -- | An VolumeiSCSIAttributes object that represents a collection of iSCSI
    -- attributes for one stored volume.
    volumeiSCSIAttributes :: Prelude.Maybe VolumeiSCSIAttributes,
    -- | One of the VolumeStatus values that indicates the state of the storage
    -- volume.
    volumeStatus :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether a storage volume is attached to, detached
    -- from, or is in the process of detaching from a gateway. For more
    -- information, see
    -- <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway>.
    volumeAttachmentStatus :: Prelude.Maybe Prelude.Text,
    -- | Indicates if when the stored volume was created, existing data on the
    -- underlying local disk was preserved.
    --
    -- Valid Values: @true@ | @false@
    preservedExistingData :: Prelude.Maybe Prelude.Bool,
    -- | If the stored volume was created from a snapshot, this field contains
    -- the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not
    -- included.
    sourceSnapshotId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the local disk that was specified in the
    -- CreateStorediSCSIVolume operation.
    volumeDiskId :: Prelude.Maybe Prelude.Text,
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | One of the VolumeType enumeration values describing the type of the
    -- volume.
    volumeType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the storage volume.
    volumeARN :: Prelude.Maybe Prelude.Text,
    -- | The date the volume was created. Volumes created prior to March 28, 2017
    -- don’t have this timestamp.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier of the volume, e.g., vol-AE4B946D.
    volumeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StorediSCSIVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'volumeProgress', 'storediSCSIVolume_volumeProgress' - Represents the percentage complete if the volume is restoring or
-- bootstrapping that represents the percent of data transferred. This
-- field does not appear in the response if the stored volume is not
-- restoring or bootstrapping.
--
-- 'volumeSizeInBytes', 'storediSCSIVolume_volumeSizeInBytes' - The size of the volume in bytes.
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
-- 'volumeiSCSIAttributes', 'storediSCSIVolume_volumeiSCSIAttributes' - An VolumeiSCSIAttributes object that represents a collection of iSCSI
-- attributes for one stored volume.
--
-- 'volumeStatus', 'storediSCSIVolume_volumeStatus' - One of the VolumeStatus values that indicates the state of the storage
-- volume.
--
-- 'volumeAttachmentStatus', 'storediSCSIVolume_volumeAttachmentStatus' - A value that indicates whether a storage volume is attached to, detached
-- from, or is in the process of detaching from a gateway. For more
-- information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway>.
--
-- 'preservedExistingData', 'storediSCSIVolume_preservedExistingData' - Indicates if when the stored volume was created, existing data on the
-- underlying local disk was preserved.
--
-- Valid Values: @true@ | @false@
--
-- 'sourceSnapshotId', 'storediSCSIVolume_sourceSnapshotId' - If the stored volume was created from a snapshot, this field contains
-- the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not
-- included.
--
-- 'volumeDiskId', 'storediSCSIVolume_volumeDiskId' - The ID of the local disk that was specified in the
-- CreateStorediSCSIVolume operation.
--
-- 'kmsKey', 'storediSCSIVolume_kmsKey' - Undocumented member.
--
-- 'volumeType', 'storediSCSIVolume_volumeType' - One of the VolumeType enumeration values describing the type of the
-- volume.
--
-- 'volumeARN', 'storediSCSIVolume_volumeARN' - The Amazon Resource Name (ARN) of the storage volume.
--
-- 'createdDate', 'storediSCSIVolume_createdDate' - The date the volume was created. Volumes created prior to March 28, 2017
-- don’t have this timestamp.
--
-- 'volumeId', 'storediSCSIVolume_volumeId' - The unique identifier of the volume, e.g., vol-AE4B946D.
newStorediSCSIVolume ::
  StorediSCSIVolume
newStorediSCSIVolume =
  StorediSCSIVolume'
    { volumeUsedInBytes =
        Prelude.Nothing,
      volumeProgress = Prelude.Nothing,
      volumeSizeInBytes = Prelude.Nothing,
      targetName = Prelude.Nothing,
      volumeiSCSIAttributes = Prelude.Nothing,
      volumeStatus = Prelude.Nothing,
      volumeAttachmentStatus = Prelude.Nothing,
      preservedExistingData = Prelude.Nothing,
      sourceSnapshotId = Prelude.Nothing,
      volumeDiskId = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      volumeType = Prelude.Nothing,
      volumeARN = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      volumeId = Prelude.Nothing
    }

-- | The size of the data stored on the volume in bytes. This value is
-- calculated based on the number of blocks that are touched, instead of
-- the actual amount of data written. This value can be useful for
-- sequential write patterns but less accurate for random write patterns.
-- @VolumeUsedInBytes@ is different from the compressed size of the volume,
-- which is the value that is used to calculate your bill.
--
-- This value is not available for volumes created prior to May 13, 2015,
-- until you store data on the volume.
storediSCSIVolume_volumeUsedInBytes :: Lens.Lens' StorediSCSIVolume (Prelude.Maybe Prelude.Integer)
storediSCSIVolume_volumeUsedInBytes = Lens.lens (\StorediSCSIVolume' {volumeUsedInBytes} -> volumeUsedInBytes) (\s@StorediSCSIVolume' {} a -> s {volumeUsedInBytes = a} :: StorediSCSIVolume)

-- | Represents the percentage complete if the volume is restoring or
-- bootstrapping that represents the percent of data transferred. This
-- field does not appear in the response if the stored volume is not
-- restoring or bootstrapping.
storediSCSIVolume_volumeProgress :: Lens.Lens' StorediSCSIVolume (Prelude.Maybe Prelude.Double)
storediSCSIVolume_volumeProgress = Lens.lens (\StorediSCSIVolume' {volumeProgress} -> volumeProgress) (\s@StorediSCSIVolume' {} a -> s {volumeProgress = a} :: StorediSCSIVolume)

-- | The size of the volume in bytes.
storediSCSIVolume_volumeSizeInBytes :: Lens.Lens' StorediSCSIVolume (Prelude.Maybe Prelude.Integer)
storediSCSIVolume_volumeSizeInBytes = Lens.lens (\StorediSCSIVolume' {volumeSizeInBytes} -> volumeSizeInBytes) (\s@StorediSCSIVolume' {} a -> s {volumeSizeInBytes = a} :: StorediSCSIVolume)

-- | The name of the iSCSI target used by an initiator to connect to a volume
-- and used as a suffix for the target ARN. For example, specifying
-- @TargetName@ as /myvolume/ results in the target ARN of
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
-- The target name must be unique across all volumes on a gateway.
--
-- If you don\'t specify a value, Storage Gateway uses the value that was
-- previously used for this volume as the new target name.
storediSCSIVolume_targetName :: Lens.Lens' StorediSCSIVolume (Prelude.Maybe Prelude.Text)
storediSCSIVolume_targetName = Lens.lens (\StorediSCSIVolume' {targetName} -> targetName) (\s@StorediSCSIVolume' {} a -> s {targetName = a} :: StorediSCSIVolume)

-- | An VolumeiSCSIAttributes object that represents a collection of iSCSI
-- attributes for one stored volume.
storediSCSIVolume_volumeiSCSIAttributes :: Lens.Lens' StorediSCSIVolume (Prelude.Maybe VolumeiSCSIAttributes)
storediSCSIVolume_volumeiSCSIAttributes = Lens.lens (\StorediSCSIVolume' {volumeiSCSIAttributes} -> volumeiSCSIAttributes) (\s@StorediSCSIVolume' {} a -> s {volumeiSCSIAttributes = a} :: StorediSCSIVolume)

-- | One of the VolumeStatus values that indicates the state of the storage
-- volume.
storediSCSIVolume_volumeStatus :: Lens.Lens' StorediSCSIVolume (Prelude.Maybe Prelude.Text)
storediSCSIVolume_volumeStatus = Lens.lens (\StorediSCSIVolume' {volumeStatus} -> volumeStatus) (\s@StorediSCSIVolume' {} a -> s {volumeStatus = a} :: StorediSCSIVolume)

-- | A value that indicates whether a storage volume is attached to, detached
-- from, or is in the process of detaching from a gateway. For more
-- information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway>.
storediSCSIVolume_volumeAttachmentStatus :: Lens.Lens' StorediSCSIVolume (Prelude.Maybe Prelude.Text)
storediSCSIVolume_volumeAttachmentStatus = Lens.lens (\StorediSCSIVolume' {volumeAttachmentStatus} -> volumeAttachmentStatus) (\s@StorediSCSIVolume' {} a -> s {volumeAttachmentStatus = a} :: StorediSCSIVolume)

-- | Indicates if when the stored volume was created, existing data on the
-- underlying local disk was preserved.
--
-- Valid Values: @true@ | @false@
storediSCSIVolume_preservedExistingData :: Lens.Lens' StorediSCSIVolume (Prelude.Maybe Prelude.Bool)
storediSCSIVolume_preservedExistingData = Lens.lens (\StorediSCSIVolume' {preservedExistingData} -> preservedExistingData) (\s@StorediSCSIVolume' {} a -> s {preservedExistingData = a} :: StorediSCSIVolume)

-- | If the stored volume was created from a snapshot, this field contains
-- the snapshot ID used, e.g. snap-78e22663. Otherwise, this field is not
-- included.
storediSCSIVolume_sourceSnapshotId :: Lens.Lens' StorediSCSIVolume (Prelude.Maybe Prelude.Text)
storediSCSIVolume_sourceSnapshotId = Lens.lens (\StorediSCSIVolume' {sourceSnapshotId} -> sourceSnapshotId) (\s@StorediSCSIVolume' {} a -> s {sourceSnapshotId = a} :: StorediSCSIVolume)

-- | The ID of the local disk that was specified in the
-- CreateStorediSCSIVolume operation.
storediSCSIVolume_volumeDiskId :: Lens.Lens' StorediSCSIVolume (Prelude.Maybe Prelude.Text)
storediSCSIVolume_volumeDiskId = Lens.lens (\StorediSCSIVolume' {volumeDiskId} -> volumeDiskId) (\s@StorediSCSIVolume' {} a -> s {volumeDiskId = a} :: StorediSCSIVolume)

-- | Undocumented member.
storediSCSIVolume_kmsKey :: Lens.Lens' StorediSCSIVolume (Prelude.Maybe Prelude.Text)
storediSCSIVolume_kmsKey = Lens.lens (\StorediSCSIVolume' {kmsKey} -> kmsKey) (\s@StorediSCSIVolume' {} a -> s {kmsKey = a} :: StorediSCSIVolume)

-- | One of the VolumeType enumeration values describing the type of the
-- volume.
storediSCSIVolume_volumeType :: Lens.Lens' StorediSCSIVolume (Prelude.Maybe Prelude.Text)
storediSCSIVolume_volumeType = Lens.lens (\StorediSCSIVolume' {volumeType} -> volumeType) (\s@StorediSCSIVolume' {} a -> s {volumeType = a} :: StorediSCSIVolume)

-- | The Amazon Resource Name (ARN) of the storage volume.
storediSCSIVolume_volumeARN :: Lens.Lens' StorediSCSIVolume (Prelude.Maybe Prelude.Text)
storediSCSIVolume_volumeARN = Lens.lens (\StorediSCSIVolume' {volumeARN} -> volumeARN) (\s@StorediSCSIVolume' {} a -> s {volumeARN = a} :: StorediSCSIVolume)

-- | The date the volume was created. Volumes created prior to March 28, 2017
-- don’t have this timestamp.
storediSCSIVolume_createdDate :: Lens.Lens' StorediSCSIVolume (Prelude.Maybe Prelude.UTCTime)
storediSCSIVolume_createdDate = Lens.lens (\StorediSCSIVolume' {createdDate} -> createdDate) (\s@StorediSCSIVolume' {} a -> s {createdDate = a} :: StorediSCSIVolume) Prelude.. Lens.mapping Core._Time

-- | The unique identifier of the volume, e.g., vol-AE4B946D.
storediSCSIVolume_volumeId :: Lens.Lens' StorediSCSIVolume (Prelude.Maybe Prelude.Text)
storediSCSIVolume_volumeId = Lens.lens (\StorediSCSIVolume' {volumeId} -> volumeId) (\s@StorediSCSIVolume' {} a -> s {volumeId = a} :: StorediSCSIVolume)

instance Core.FromJSON StorediSCSIVolume where
  parseJSON =
    Core.withObject
      "StorediSCSIVolume"
      ( \x ->
          StorediSCSIVolume'
            Prelude.<$> (x Core..:? "VolumeUsedInBytes")
            Prelude.<*> (x Core..:? "VolumeProgress")
            Prelude.<*> (x Core..:? "VolumeSizeInBytes")
            Prelude.<*> (x Core..:? "TargetName")
            Prelude.<*> (x Core..:? "VolumeiSCSIAttributes")
            Prelude.<*> (x Core..:? "VolumeStatus")
            Prelude.<*> (x Core..:? "VolumeAttachmentStatus")
            Prelude.<*> (x Core..:? "PreservedExistingData")
            Prelude.<*> (x Core..:? "SourceSnapshotId")
            Prelude.<*> (x Core..:? "VolumeDiskId")
            Prelude.<*> (x Core..:? "KMSKey")
            Prelude.<*> (x Core..:? "VolumeType")
            Prelude.<*> (x Core..:? "VolumeARN")
            Prelude.<*> (x Core..:? "CreatedDate")
            Prelude.<*> (x Core..:? "VolumeId")
      )

instance Prelude.Hashable StorediSCSIVolume where
  hashWithSalt _salt StorediSCSIVolume' {..} =
    _salt `Prelude.hashWithSalt` volumeUsedInBytes
      `Prelude.hashWithSalt` volumeProgress
      `Prelude.hashWithSalt` volumeSizeInBytes
      `Prelude.hashWithSalt` targetName
      `Prelude.hashWithSalt` volumeiSCSIAttributes
      `Prelude.hashWithSalt` volumeStatus
      `Prelude.hashWithSalt` volumeAttachmentStatus
      `Prelude.hashWithSalt` preservedExistingData
      `Prelude.hashWithSalt` sourceSnapshotId
      `Prelude.hashWithSalt` volumeDiskId
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` volumeType
      `Prelude.hashWithSalt` volumeARN
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData StorediSCSIVolume where
  rnf StorediSCSIVolume' {..} =
    Prelude.rnf volumeUsedInBytes
      `Prelude.seq` Prelude.rnf volumeProgress
      `Prelude.seq` Prelude.rnf volumeSizeInBytes
      `Prelude.seq` Prelude.rnf targetName
      `Prelude.seq` Prelude.rnf volumeiSCSIAttributes
      `Prelude.seq` Prelude.rnf volumeStatus
      `Prelude.seq` Prelude.rnf volumeAttachmentStatus
      `Prelude.seq` Prelude.rnf preservedExistingData
      `Prelude.seq` Prelude.rnf sourceSnapshotId
      `Prelude.seq` Prelude.rnf volumeDiskId
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf volumeType
      `Prelude.seq` Prelude.rnf volumeARN
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf volumeId
