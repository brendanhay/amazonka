{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VolumeAttachment
import Network.AWS.EC2.Types.VolumeState
import Network.AWS.EC2.Types.VolumeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a volume.
--
-- /See:/ 'newVolume' smart constructor.
data Volume = Volume'
  { -- | Indicates whether Amazon EBS Multi-Attach is enabled.
    multiAttachEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the volume was created using fast snapshot restore.
    fastRestored :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The throughput that the volume supports, in MiB\/s.
    throughput :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS
    -- KMS) customer master key (CMK) that was used to protect the volume
    -- encryption key for the volume.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the volume.
    tags :: Prelude.Maybe [Tag],
    -- | The number of I\/O operations per second (IOPS). For @gp3@, @io1@, and
    -- @io2@ volumes, this represents the number of IOPS that are provisioned
    -- for the volume. For @gp2@ volumes, this represents the baseline
    -- performance of the volume and the rate at which the volume accumulates
    -- I\/O credits for bursting.
    iops :: Prelude.Maybe Prelude.Int,
    -- | Information about the volume attachments.
    attachments :: Prelude.Maybe [VolumeAttachment],
    -- | The Availability Zone for the volume.
    availabilityZone :: Prelude.Text,
    -- | The time stamp when volume creation was initiated.
    createTime :: Prelude.ISO8601,
    -- | Indicates whether the volume is encrypted.
    encrypted :: Prelude.Bool,
    -- | The size of the volume, in GiBs.
    size :: Prelude.Int,
    -- | The snapshot from which the volume was created, if applicable.
    snapshotId :: Prelude.Text,
    -- | The volume state.
    state :: VolumeState,
    -- | The ID of the volume.
    volumeId :: Prelude.Text,
    -- | The volume type.
    volumeType :: VolumeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'encrypted'
  Prelude.Bool ->
  -- | 'size'
  Prelude.Int ->
  -- | 'snapshotId'
  Prelude.Text ->
  -- | 'state'
  VolumeState ->
  -- | 'volumeId'
  Prelude.Text ->
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
      { multiAttachEnabled = Prelude.Nothing,
        fastRestored = Prelude.Nothing,
        outpostArn = Prelude.Nothing,
        throughput = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        tags = Prelude.Nothing,
        iops = Prelude.Nothing,
        attachments = Prelude.Nothing,
        availabilityZone = pAvailabilityZone_,
        createTime = Prelude._Time Lens.# pCreateTime_,
        encrypted = pEncrypted_,
        size = pSize_,
        snapshotId = pSnapshotId_,
        state = pState_,
        volumeId = pVolumeId_,
        volumeType = pVolumeType_
      }

-- | Indicates whether Amazon EBS Multi-Attach is enabled.
volume_multiAttachEnabled :: Lens.Lens' Volume (Prelude.Maybe Prelude.Bool)
volume_multiAttachEnabled = Lens.lens (\Volume' {multiAttachEnabled} -> multiAttachEnabled) (\s@Volume' {} a -> s {multiAttachEnabled = a} :: Volume)

-- | Indicates whether the volume was created using fast snapshot restore.
volume_fastRestored :: Lens.Lens' Volume (Prelude.Maybe Prelude.Bool)
volume_fastRestored = Lens.lens (\Volume' {fastRestored} -> fastRestored) (\s@Volume' {} a -> s {fastRestored = a} :: Volume)

-- | The Amazon Resource Name (ARN) of the Outpost.
volume_outpostArn :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_outpostArn = Lens.lens (\Volume' {outpostArn} -> outpostArn) (\s@Volume' {} a -> s {outpostArn = a} :: Volume)

-- | The throughput that the volume supports, in MiB\/s.
volume_throughput :: Lens.Lens' Volume (Prelude.Maybe Prelude.Int)
volume_throughput = Lens.lens (\Volume' {throughput} -> throughput) (\s@Volume' {} a -> s {throughput = a} :: Volume)

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS
-- KMS) customer master key (CMK) that was used to protect the volume
-- encryption key for the volume.
volume_kmsKeyId :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_kmsKeyId = Lens.lens (\Volume' {kmsKeyId} -> kmsKeyId) (\s@Volume' {} a -> s {kmsKeyId = a} :: Volume)

-- | Any tags assigned to the volume.
volume_tags :: Lens.Lens' Volume (Prelude.Maybe [Tag])
volume_tags = Lens.lens (\Volume' {tags} -> tags) (\s@Volume' {} a -> s {tags = a} :: Volume) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of I\/O operations per second (IOPS). For @gp3@, @io1@, and
-- @io2@ volumes, this represents the number of IOPS that are provisioned
-- for the volume. For @gp2@ volumes, this represents the baseline
-- performance of the volume and the rate at which the volume accumulates
-- I\/O credits for bursting.
volume_iops :: Lens.Lens' Volume (Prelude.Maybe Prelude.Int)
volume_iops = Lens.lens (\Volume' {iops} -> iops) (\s@Volume' {} a -> s {iops = a} :: Volume)

-- | Information about the volume attachments.
volume_attachments :: Lens.Lens' Volume (Prelude.Maybe [VolumeAttachment])
volume_attachments = Lens.lens (\Volume' {attachments} -> attachments) (\s@Volume' {} a -> s {attachments = a} :: Volume) Prelude.. Lens.mapping Prelude._Coerce

-- | The Availability Zone for the volume.
volume_availabilityZone :: Lens.Lens' Volume Prelude.Text
volume_availabilityZone = Lens.lens (\Volume' {availabilityZone} -> availabilityZone) (\s@Volume' {} a -> s {availabilityZone = a} :: Volume)

-- | The time stamp when volume creation was initiated.
volume_createTime :: Lens.Lens' Volume Prelude.UTCTime
volume_createTime = Lens.lens (\Volume' {createTime} -> createTime) (\s@Volume' {} a -> s {createTime = a} :: Volume) Prelude.. Prelude._Time

-- | Indicates whether the volume is encrypted.
volume_encrypted :: Lens.Lens' Volume Prelude.Bool
volume_encrypted = Lens.lens (\Volume' {encrypted} -> encrypted) (\s@Volume' {} a -> s {encrypted = a} :: Volume)

-- | The size of the volume, in GiBs.
volume_size :: Lens.Lens' Volume Prelude.Int
volume_size = Lens.lens (\Volume' {size} -> size) (\s@Volume' {} a -> s {size = a} :: Volume)

-- | The snapshot from which the volume was created, if applicable.
volume_snapshotId :: Lens.Lens' Volume Prelude.Text
volume_snapshotId = Lens.lens (\Volume' {snapshotId} -> snapshotId) (\s@Volume' {} a -> s {snapshotId = a} :: Volume)

-- | The volume state.
volume_state :: Lens.Lens' Volume VolumeState
volume_state = Lens.lens (\Volume' {state} -> state) (\s@Volume' {} a -> s {state = a} :: Volume)

-- | The ID of the volume.
volume_volumeId :: Lens.Lens' Volume Prelude.Text
volume_volumeId = Lens.lens (\Volume' {volumeId} -> volumeId) (\s@Volume' {} a -> s {volumeId = a} :: Volume)

-- | The volume type.
volume_volumeType :: Lens.Lens' Volume VolumeType
volume_volumeType = Lens.lens (\Volume' {volumeType} -> volumeType) (\s@Volume' {} a -> s {volumeType = a} :: Volume)

instance Prelude.FromXML Volume where
  parseXML x =
    Volume'
      Prelude.<$> (x Prelude..@? "multiAttachEnabled")
      Prelude.<*> (x Prelude..@? "fastRestored")
      Prelude.<*> (x Prelude..@? "outpostArn")
      Prelude.<*> (x Prelude..@? "throughput")
      Prelude.<*> (x Prelude..@? "kmsKeyId")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "iops")
      Prelude.<*> ( x Prelude..@? "attachmentSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@ "availabilityZone")
      Prelude.<*> (x Prelude..@ "createTime")
      Prelude.<*> (x Prelude..@ "encrypted")
      Prelude.<*> (x Prelude..@ "size")
      Prelude.<*> (x Prelude..@ "snapshotId")
      Prelude.<*> (x Prelude..@ "status")
      Prelude.<*> (x Prelude..@ "volumeId")
      Prelude.<*> (x Prelude..@ "volumeType")

instance Prelude.Hashable Volume

instance Prelude.NFData Volume
