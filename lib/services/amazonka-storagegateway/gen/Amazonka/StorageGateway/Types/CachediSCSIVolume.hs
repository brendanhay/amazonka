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
-- Module      : Amazonka.StorageGateway.Types.CachediSCSIVolume
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.CachediSCSIVolume where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.StorageGateway.Types.VolumeiSCSIAttributes

-- | Describes an iSCSI cached volume.
--
-- /See:/ 'newCachediSCSIVolume' smart constructor.
data CachediSCSIVolume = CachediSCSIVolume'
  { -- | The date the volume was created. Volumes created prior to March 28, 2017
    -- don’t have this timestamp.
    createdDate :: Prelude.Maybe Data.POSIX,
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | If the cached volume was created from a snapshot, this field contains
    -- the snapshot ID used, e.g., snap-78e22663. Otherwise, this field is not
    -- included.
    sourceSnapshotId :: Prelude.Maybe Prelude.Text,
    -- | The name of the iSCSI target used by an initiator to connect to a volume
    -- and used as a suffix for the target ARN. For example, specifying
    -- @TargetName@ as /myvolume/ results in the target ARN of
    -- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
    -- The target name must be unique across all volumes on a gateway.
    --
    -- If you don\'t specify a value, Storage Gateway uses the value that was
    -- previously used for this volume as the new target name.
    targetName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the storage volume.
    volumeARN :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether a storage volume is attached to or
    -- detached from a gateway. For more information, see
    -- <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway>.
    volumeAttachmentStatus :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the volume, e.g., vol-AE4B946D.
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | Represents the percentage complete if the volume is restoring or
    -- bootstrapping that represents the percent of data transferred. This
    -- field does not appear in the response if the cached volume is not
    -- restoring or bootstrapping.
    volumeProgress :: Prelude.Maybe Prelude.Double,
    -- | The size, in bytes, of the volume capacity.
    volumeSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | One of the VolumeStatus values that indicates the state of the storage
    -- volume.
    volumeStatus :: Prelude.Maybe Prelude.Text,
    -- | One of the VolumeType enumeration values that describes the type of the
    -- volume.
    volumeType :: Prelude.Maybe Prelude.Text,
    -- | The size of the data stored on the volume in bytes. This value is
    -- calculated based on the number of blocks that are touched, instead of
    -- the actual amount of data written. This value can be useful for
    -- sequential write patterns but less accurate for random write patterns.
    -- @VolumeUsedInBytes@ is different from the compressed size of the volume,
    -- which is the value that is used to calculate your bill.
    --
    -- This value is not available for volumes created prior to May 13, 2015,
    -- until you store data on the volume.
    --
    -- If you use a delete tool that overwrites the data on your volume with
    -- random data, your usage will not be reduced. This is because the random
    -- data is not compressible. If you want to reduce the amount of billed
    -- storage on your volume, we recommend overwriting your files with zeros
    -- to compress the data to a negligible amount of actual storage.
    volumeUsedInBytes :: Prelude.Maybe Prelude.Integer,
    -- | An VolumeiSCSIAttributes object that represents a collection of iSCSI
    -- attributes for one stored volume.
    volumeiSCSIAttributes :: Prelude.Maybe VolumeiSCSIAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CachediSCSIVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'cachediSCSIVolume_createdDate' - The date the volume was created. Volumes created prior to March 28, 2017
-- don’t have this timestamp.
--
-- 'kmsKey', 'cachediSCSIVolume_kmsKey' - Undocumented member.
--
-- 'sourceSnapshotId', 'cachediSCSIVolume_sourceSnapshotId' - If the cached volume was created from a snapshot, this field contains
-- the snapshot ID used, e.g., snap-78e22663. Otherwise, this field is not
-- included.
--
-- 'targetName', 'cachediSCSIVolume_targetName' - The name of the iSCSI target used by an initiator to connect to a volume
-- and used as a suffix for the target ARN. For example, specifying
-- @TargetName@ as /myvolume/ results in the target ARN of
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
-- The target name must be unique across all volumes on a gateway.
--
-- If you don\'t specify a value, Storage Gateway uses the value that was
-- previously used for this volume as the new target name.
--
-- 'volumeARN', 'cachediSCSIVolume_volumeARN' - The Amazon Resource Name (ARN) of the storage volume.
--
-- 'volumeAttachmentStatus', 'cachediSCSIVolume_volumeAttachmentStatus' - A value that indicates whether a storage volume is attached to or
-- detached from a gateway. For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway>.
--
-- 'volumeId', 'cachediSCSIVolume_volumeId' - The unique identifier of the volume, e.g., vol-AE4B946D.
--
-- 'volumeProgress', 'cachediSCSIVolume_volumeProgress' - Represents the percentage complete if the volume is restoring or
-- bootstrapping that represents the percent of data transferred. This
-- field does not appear in the response if the cached volume is not
-- restoring or bootstrapping.
--
-- 'volumeSizeInBytes', 'cachediSCSIVolume_volumeSizeInBytes' - The size, in bytes, of the volume capacity.
--
-- 'volumeStatus', 'cachediSCSIVolume_volumeStatus' - One of the VolumeStatus values that indicates the state of the storage
-- volume.
--
-- 'volumeType', 'cachediSCSIVolume_volumeType' - One of the VolumeType enumeration values that describes the type of the
-- volume.
--
-- 'volumeUsedInBytes', 'cachediSCSIVolume_volumeUsedInBytes' - The size of the data stored on the volume in bytes. This value is
-- calculated based on the number of blocks that are touched, instead of
-- the actual amount of data written. This value can be useful for
-- sequential write patterns but less accurate for random write patterns.
-- @VolumeUsedInBytes@ is different from the compressed size of the volume,
-- which is the value that is used to calculate your bill.
--
-- This value is not available for volumes created prior to May 13, 2015,
-- until you store data on the volume.
--
-- If you use a delete tool that overwrites the data on your volume with
-- random data, your usage will not be reduced. This is because the random
-- data is not compressible. If you want to reduce the amount of billed
-- storage on your volume, we recommend overwriting your files with zeros
-- to compress the data to a negligible amount of actual storage.
--
-- 'volumeiSCSIAttributes', 'cachediSCSIVolume_volumeiSCSIAttributes' - An VolumeiSCSIAttributes object that represents a collection of iSCSI
-- attributes for one stored volume.
newCachediSCSIVolume ::
  CachediSCSIVolume
newCachediSCSIVolume =
  CachediSCSIVolume'
    { createdDate = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      sourceSnapshotId = Prelude.Nothing,
      targetName = Prelude.Nothing,
      volumeARN = Prelude.Nothing,
      volumeAttachmentStatus = Prelude.Nothing,
      volumeId = Prelude.Nothing,
      volumeProgress = Prelude.Nothing,
      volumeSizeInBytes = Prelude.Nothing,
      volumeStatus = Prelude.Nothing,
      volumeType = Prelude.Nothing,
      volumeUsedInBytes = Prelude.Nothing,
      volumeiSCSIAttributes = Prelude.Nothing
    }

-- | The date the volume was created. Volumes created prior to March 28, 2017
-- don’t have this timestamp.
cachediSCSIVolume_createdDate :: Lens.Lens' CachediSCSIVolume (Prelude.Maybe Prelude.UTCTime)
cachediSCSIVolume_createdDate = Lens.lens (\CachediSCSIVolume' {createdDate} -> createdDate) (\s@CachediSCSIVolume' {} a -> s {createdDate = a} :: CachediSCSIVolume) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
cachediSCSIVolume_kmsKey :: Lens.Lens' CachediSCSIVolume (Prelude.Maybe Prelude.Text)
cachediSCSIVolume_kmsKey = Lens.lens (\CachediSCSIVolume' {kmsKey} -> kmsKey) (\s@CachediSCSIVolume' {} a -> s {kmsKey = a} :: CachediSCSIVolume)

-- | If the cached volume was created from a snapshot, this field contains
-- the snapshot ID used, e.g., snap-78e22663. Otherwise, this field is not
-- included.
cachediSCSIVolume_sourceSnapshotId :: Lens.Lens' CachediSCSIVolume (Prelude.Maybe Prelude.Text)
cachediSCSIVolume_sourceSnapshotId = Lens.lens (\CachediSCSIVolume' {sourceSnapshotId} -> sourceSnapshotId) (\s@CachediSCSIVolume' {} a -> s {sourceSnapshotId = a} :: CachediSCSIVolume)

-- | The name of the iSCSI target used by an initiator to connect to a volume
-- and used as a suffix for the target ARN. For example, specifying
-- @TargetName@ as /myvolume/ results in the target ARN of
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
-- The target name must be unique across all volumes on a gateway.
--
-- If you don\'t specify a value, Storage Gateway uses the value that was
-- previously used for this volume as the new target name.
cachediSCSIVolume_targetName :: Lens.Lens' CachediSCSIVolume (Prelude.Maybe Prelude.Text)
cachediSCSIVolume_targetName = Lens.lens (\CachediSCSIVolume' {targetName} -> targetName) (\s@CachediSCSIVolume' {} a -> s {targetName = a} :: CachediSCSIVolume)

-- | The Amazon Resource Name (ARN) of the storage volume.
cachediSCSIVolume_volumeARN :: Lens.Lens' CachediSCSIVolume (Prelude.Maybe Prelude.Text)
cachediSCSIVolume_volumeARN = Lens.lens (\CachediSCSIVolume' {volumeARN} -> volumeARN) (\s@CachediSCSIVolume' {} a -> s {volumeARN = a} :: CachediSCSIVolume)

-- | A value that indicates whether a storage volume is attached to or
-- detached from a gateway. For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/managing-volumes.html#attach-detach-volume Moving your volumes to a different gateway>.
cachediSCSIVolume_volumeAttachmentStatus :: Lens.Lens' CachediSCSIVolume (Prelude.Maybe Prelude.Text)
cachediSCSIVolume_volumeAttachmentStatus = Lens.lens (\CachediSCSIVolume' {volumeAttachmentStatus} -> volumeAttachmentStatus) (\s@CachediSCSIVolume' {} a -> s {volumeAttachmentStatus = a} :: CachediSCSIVolume)

-- | The unique identifier of the volume, e.g., vol-AE4B946D.
cachediSCSIVolume_volumeId :: Lens.Lens' CachediSCSIVolume (Prelude.Maybe Prelude.Text)
cachediSCSIVolume_volumeId = Lens.lens (\CachediSCSIVolume' {volumeId} -> volumeId) (\s@CachediSCSIVolume' {} a -> s {volumeId = a} :: CachediSCSIVolume)

-- | Represents the percentage complete if the volume is restoring or
-- bootstrapping that represents the percent of data transferred. This
-- field does not appear in the response if the cached volume is not
-- restoring or bootstrapping.
cachediSCSIVolume_volumeProgress :: Lens.Lens' CachediSCSIVolume (Prelude.Maybe Prelude.Double)
cachediSCSIVolume_volumeProgress = Lens.lens (\CachediSCSIVolume' {volumeProgress} -> volumeProgress) (\s@CachediSCSIVolume' {} a -> s {volumeProgress = a} :: CachediSCSIVolume)

-- | The size, in bytes, of the volume capacity.
cachediSCSIVolume_volumeSizeInBytes :: Lens.Lens' CachediSCSIVolume (Prelude.Maybe Prelude.Integer)
cachediSCSIVolume_volumeSizeInBytes = Lens.lens (\CachediSCSIVolume' {volumeSizeInBytes} -> volumeSizeInBytes) (\s@CachediSCSIVolume' {} a -> s {volumeSizeInBytes = a} :: CachediSCSIVolume)

-- | One of the VolumeStatus values that indicates the state of the storage
-- volume.
cachediSCSIVolume_volumeStatus :: Lens.Lens' CachediSCSIVolume (Prelude.Maybe Prelude.Text)
cachediSCSIVolume_volumeStatus = Lens.lens (\CachediSCSIVolume' {volumeStatus} -> volumeStatus) (\s@CachediSCSIVolume' {} a -> s {volumeStatus = a} :: CachediSCSIVolume)

-- | One of the VolumeType enumeration values that describes the type of the
-- volume.
cachediSCSIVolume_volumeType :: Lens.Lens' CachediSCSIVolume (Prelude.Maybe Prelude.Text)
cachediSCSIVolume_volumeType = Lens.lens (\CachediSCSIVolume' {volumeType} -> volumeType) (\s@CachediSCSIVolume' {} a -> s {volumeType = a} :: CachediSCSIVolume)

-- | The size of the data stored on the volume in bytes. This value is
-- calculated based on the number of blocks that are touched, instead of
-- the actual amount of data written. This value can be useful for
-- sequential write patterns but less accurate for random write patterns.
-- @VolumeUsedInBytes@ is different from the compressed size of the volume,
-- which is the value that is used to calculate your bill.
--
-- This value is not available for volumes created prior to May 13, 2015,
-- until you store data on the volume.
--
-- If you use a delete tool that overwrites the data on your volume with
-- random data, your usage will not be reduced. This is because the random
-- data is not compressible. If you want to reduce the amount of billed
-- storage on your volume, we recommend overwriting your files with zeros
-- to compress the data to a negligible amount of actual storage.
cachediSCSIVolume_volumeUsedInBytes :: Lens.Lens' CachediSCSIVolume (Prelude.Maybe Prelude.Integer)
cachediSCSIVolume_volumeUsedInBytes = Lens.lens (\CachediSCSIVolume' {volumeUsedInBytes} -> volumeUsedInBytes) (\s@CachediSCSIVolume' {} a -> s {volumeUsedInBytes = a} :: CachediSCSIVolume)

-- | An VolumeiSCSIAttributes object that represents a collection of iSCSI
-- attributes for one stored volume.
cachediSCSIVolume_volumeiSCSIAttributes :: Lens.Lens' CachediSCSIVolume (Prelude.Maybe VolumeiSCSIAttributes)
cachediSCSIVolume_volumeiSCSIAttributes = Lens.lens (\CachediSCSIVolume' {volumeiSCSIAttributes} -> volumeiSCSIAttributes) (\s@CachediSCSIVolume' {} a -> s {volumeiSCSIAttributes = a} :: CachediSCSIVolume)

instance Data.FromJSON CachediSCSIVolume where
  parseJSON =
    Data.withObject
      "CachediSCSIVolume"
      ( \x ->
          CachediSCSIVolume'
            Prelude.<$> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "KMSKey")
            Prelude.<*> (x Data..:? "SourceSnapshotId")
            Prelude.<*> (x Data..:? "TargetName")
            Prelude.<*> (x Data..:? "VolumeARN")
            Prelude.<*> (x Data..:? "VolumeAttachmentStatus")
            Prelude.<*> (x Data..:? "VolumeId")
            Prelude.<*> (x Data..:? "VolumeProgress")
            Prelude.<*> (x Data..:? "VolumeSizeInBytes")
            Prelude.<*> (x Data..:? "VolumeStatus")
            Prelude.<*> (x Data..:? "VolumeType")
            Prelude.<*> (x Data..:? "VolumeUsedInBytes")
            Prelude.<*> (x Data..:? "VolumeiSCSIAttributes")
      )

instance Prelude.Hashable CachediSCSIVolume where
  hashWithSalt _salt CachediSCSIVolume' {..} =
    _salt
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` sourceSnapshotId
      `Prelude.hashWithSalt` targetName
      `Prelude.hashWithSalt` volumeARN
      `Prelude.hashWithSalt` volumeAttachmentStatus
      `Prelude.hashWithSalt` volumeId
      `Prelude.hashWithSalt` volumeProgress
      `Prelude.hashWithSalt` volumeSizeInBytes
      `Prelude.hashWithSalt` volumeStatus
      `Prelude.hashWithSalt` volumeType
      `Prelude.hashWithSalt` volumeUsedInBytes
      `Prelude.hashWithSalt` volumeiSCSIAttributes

instance Prelude.NFData CachediSCSIVolume where
  rnf CachediSCSIVolume' {..} =
    Prelude.rnf createdDate `Prelude.seq`
      Prelude.rnf kmsKey `Prelude.seq`
        Prelude.rnf sourceSnapshotId `Prelude.seq`
          Prelude.rnf targetName `Prelude.seq`
            Prelude.rnf volumeARN `Prelude.seq`
              Prelude.rnf volumeAttachmentStatus `Prelude.seq`
                Prelude.rnf volumeId `Prelude.seq`
                  Prelude.rnf volumeProgress `Prelude.seq`
                    Prelude.rnf volumeSizeInBytes `Prelude.seq`
                      Prelude.rnf volumeStatus `Prelude.seq`
                        Prelude.rnf volumeType `Prelude.seq`
                          Prelude.rnf volumeUsedInBytes `Prelude.seq`
                            Prelude.rnf volumeiSCSIAttributes
