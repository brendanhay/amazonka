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
-- Module      : Network.AWS.EC2.Types.Volume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Volume where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VolumeAttachment
import Network.AWS.EC2.Types.VolumeState
import Network.AWS.EC2.Types.VolumeType
import qualified Network.AWS.Lens as Lens

-- | Describes a volume.
--
-- /See:/ 'newVolume' smart constructor.
data Volume = Volume'
  { -- | Indicates whether Amazon EBS Multi-Attach is enabled.
    multiAttachEnabled :: Core.Maybe Core.Bool,
    -- | Indicates whether the volume was created using fast snapshot restore.
    fastRestored :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Core.Maybe Core.Text,
    -- | The throughput that the volume supports, in MiB\/s.
    throughput :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS
    -- KMS) customer master key (CMK) that was used to protect the volume
    -- encryption key for the volume.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | Any tags assigned to the volume.
    tags :: Core.Maybe [Tag],
    -- | The number of I\/O operations per second (IOPS). For @gp3@, @io1@, and
    -- @io2@ volumes, this represents the number of IOPS that are provisioned
    -- for the volume. For @gp2@ volumes, this represents the baseline
    -- performance of the volume and the rate at which the volume accumulates
    -- I\/O credits for bursting.
    iops :: Core.Maybe Core.Int,
    -- | Information about the volume attachments.
    attachments :: Core.Maybe [VolumeAttachment],
    -- | The Availability Zone for the volume.
    availabilityZone :: Core.Text,
    -- | The time stamp when volume creation was initiated.
    createTime :: Core.ISO8601,
    -- | Indicates whether the volume is encrypted.
    encrypted :: Core.Bool,
    -- | The size of the volume, in GiBs.
    size :: Core.Int,
    -- | The snapshot from which the volume was created, if applicable.
    snapshotId :: Core.Text,
    -- | The volume state.
    state :: VolumeState,
    -- | The ID of the volume.
    volumeId :: Core.Text,
    -- | The volume type.
    volumeType :: VolumeType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Volume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiAttachEnabled', 'volume_multiAttachEnabled' - Indicates whether Amazon EBS Multi-Attach is enabled.
--
-- 'fastRestored', 'volume_fastRestored' - Indicates whether the volume was created using fast snapshot restore.
--
-- 'outpostArn', 'volume_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'throughput', 'volume_throughput' - The throughput that the volume supports, in MiB\/s.
--
-- 'kmsKeyId', 'volume_kmsKeyId' - The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS
-- KMS) customer master key (CMK) that was used to protect the volume
-- encryption key for the volume.
--
-- 'tags', 'volume_tags' - Any tags assigned to the volume.
--
-- 'iops', 'volume_iops' - The number of I\/O operations per second (IOPS). For @gp3@, @io1@, and
-- @io2@ volumes, this represents the number of IOPS that are provisioned
-- for the volume. For @gp2@ volumes, this represents the baseline
-- performance of the volume and the rate at which the volume accumulates
-- I\/O credits for bursting.
--
-- 'attachments', 'volume_attachments' - Information about the volume attachments.
--
-- 'availabilityZone', 'volume_availabilityZone' - The Availability Zone for the volume.
--
-- 'createTime', 'volume_createTime' - The time stamp when volume creation was initiated.
--
-- 'encrypted', 'volume_encrypted' - Indicates whether the volume is encrypted.
--
-- 'size', 'volume_size' - The size of the volume, in GiBs.
--
-- 'snapshotId', 'volume_snapshotId' - The snapshot from which the volume was created, if applicable.
--
-- 'state', 'volume_state' - The volume state.
--
-- 'volumeId', 'volume_volumeId' - The ID of the volume.
--
-- 'volumeType', 'volume_volumeType' - The volume type.
newVolume ::
  -- | 'availabilityZone'
  Core.Text ->
  -- | 'createTime'
  Core.UTCTime ->
  -- | 'encrypted'
  Core.Bool ->
  -- | 'size'
  Core.Int ->
  -- | 'snapshotId'
  Core.Text ->
  -- | 'state'
  VolumeState ->
  -- | 'volumeId'
  Core.Text ->
  -- | 'volumeType'
  VolumeType ->
  Volume
newVolume
  pAvailabilityZone_
  pCreateTime_
  pEncrypted_
  pSize_
  pSnapshotId_
  pState_
  pVolumeId_
  pVolumeType_ =
    Volume'
      { multiAttachEnabled = Core.Nothing,
        fastRestored = Core.Nothing,
        outpostArn = Core.Nothing,
        throughput = Core.Nothing,
        kmsKeyId = Core.Nothing,
        tags = Core.Nothing,
        iops = Core.Nothing,
        attachments = Core.Nothing,
        availabilityZone = pAvailabilityZone_,
        createTime = Core._Time Lens.# pCreateTime_,
        encrypted = pEncrypted_,
        size = pSize_,
        snapshotId = pSnapshotId_,
        state = pState_,
        volumeId = pVolumeId_,
        volumeType = pVolumeType_
      }

-- | Indicates whether Amazon EBS Multi-Attach is enabled.
volume_multiAttachEnabled :: Lens.Lens' Volume (Core.Maybe Core.Bool)
volume_multiAttachEnabled = Lens.lens (\Volume' {multiAttachEnabled} -> multiAttachEnabled) (\s@Volume' {} a -> s {multiAttachEnabled = a} :: Volume)

-- | Indicates whether the volume was created using fast snapshot restore.
volume_fastRestored :: Lens.Lens' Volume (Core.Maybe Core.Bool)
volume_fastRestored = Lens.lens (\Volume' {fastRestored} -> fastRestored) (\s@Volume' {} a -> s {fastRestored = a} :: Volume)

-- | The Amazon Resource Name (ARN) of the Outpost.
volume_outpostArn :: Lens.Lens' Volume (Core.Maybe Core.Text)
volume_outpostArn = Lens.lens (\Volume' {outpostArn} -> outpostArn) (\s@Volume' {} a -> s {outpostArn = a} :: Volume)

-- | The throughput that the volume supports, in MiB\/s.
volume_throughput :: Lens.Lens' Volume (Core.Maybe Core.Int)
volume_throughput = Lens.lens (\Volume' {throughput} -> throughput) (\s@Volume' {} a -> s {throughput = a} :: Volume)

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS
-- KMS) customer master key (CMK) that was used to protect the volume
-- encryption key for the volume.
volume_kmsKeyId :: Lens.Lens' Volume (Core.Maybe Core.Text)
volume_kmsKeyId = Lens.lens (\Volume' {kmsKeyId} -> kmsKeyId) (\s@Volume' {} a -> s {kmsKeyId = a} :: Volume)

-- | Any tags assigned to the volume.
volume_tags :: Lens.Lens' Volume (Core.Maybe [Tag])
volume_tags = Lens.lens (\Volume' {tags} -> tags) (\s@Volume' {} a -> s {tags = a} :: Volume) Core.. Lens.mapping Lens._Coerce

-- | The number of I\/O operations per second (IOPS). For @gp3@, @io1@, and
-- @io2@ volumes, this represents the number of IOPS that are provisioned
-- for the volume. For @gp2@ volumes, this represents the baseline
-- performance of the volume and the rate at which the volume accumulates
-- I\/O credits for bursting.
volume_iops :: Lens.Lens' Volume (Core.Maybe Core.Int)
volume_iops = Lens.lens (\Volume' {iops} -> iops) (\s@Volume' {} a -> s {iops = a} :: Volume)

-- | Information about the volume attachments.
volume_attachments :: Lens.Lens' Volume (Core.Maybe [VolumeAttachment])
volume_attachments = Lens.lens (\Volume' {attachments} -> attachments) (\s@Volume' {} a -> s {attachments = a} :: Volume) Core.. Lens.mapping Lens._Coerce

-- | The Availability Zone for the volume.
volume_availabilityZone :: Lens.Lens' Volume Core.Text
volume_availabilityZone = Lens.lens (\Volume' {availabilityZone} -> availabilityZone) (\s@Volume' {} a -> s {availabilityZone = a} :: Volume)

-- | The time stamp when volume creation was initiated.
volume_createTime :: Lens.Lens' Volume Core.UTCTime
volume_createTime = Lens.lens (\Volume' {createTime} -> createTime) (\s@Volume' {} a -> s {createTime = a} :: Volume) Core.. Core._Time

-- | Indicates whether the volume is encrypted.
volume_encrypted :: Lens.Lens' Volume Core.Bool
volume_encrypted = Lens.lens (\Volume' {encrypted} -> encrypted) (\s@Volume' {} a -> s {encrypted = a} :: Volume)

-- | The size of the volume, in GiBs.
volume_size :: Lens.Lens' Volume Core.Int
volume_size = Lens.lens (\Volume' {size} -> size) (\s@Volume' {} a -> s {size = a} :: Volume)

-- | The snapshot from which the volume was created, if applicable.
volume_snapshotId :: Lens.Lens' Volume Core.Text
volume_snapshotId = Lens.lens (\Volume' {snapshotId} -> snapshotId) (\s@Volume' {} a -> s {snapshotId = a} :: Volume)

-- | The volume state.
volume_state :: Lens.Lens' Volume VolumeState
volume_state = Lens.lens (\Volume' {state} -> state) (\s@Volume' {} a -> s {state = a} :: Volume)

-- | The ID of the volume.
volume_volumeId :: Lens.Lens' Volume Core.Text
volume_volumeId = Lens.lens (\Volume' {volumeId} -> volumeId) (\s@Volume' {} a -> s {volumeId = a} :: Volume)

-- | The volume type.
volume_volumeType :: Lens.Lens' Volume VolumeType
volume_volumeType = Lens.lens (\Volume' {volumeType} -> volumeType) (\s@Volume' {} a -> s {volumeType = a} :: Volume)

instance Core.FromXML Volume where
  parseXML x =
    Volume'
      Core.<$> (x Core..@? "multiAttachEnabled")
      Core.<*> (x Core..@? "fastRestored")
      Core.<*> (x Core..@? "outpostArn")
      Core.<*> (x Core..@? "throughput")
      Core.<*> (x Core..@? "kmsKeyId")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "iops")
      Core.<*> ( x Core..@? "attachmentSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@ "availabilityZone")
      Core.<*> (x Core..@ "createTime")
      Core.<*> (x Core..@ "encrypted")
      Core.<*> (x Core..@ "size")
      Core.<*> (x Core..@ "snapshotId")
      Core.<*> (x Core..@ "status")
      Core.<*> (x Core..@ "volumeId")
      Core.<*> (x Core..@ "volumeType")

instance Core.Hashable Volume

instance Core.NFData Volume
