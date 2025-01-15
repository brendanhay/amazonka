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
-- Module      : Amazonka.EC2.Types.Volume
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Volume where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.VolumeAttachment
import Amazonka.EC2.Types.VolumeState
import Amazonka.EC2.Types.VolumeType
import qualified Amazonka.Prelude as Prelude

-- | Describes a volume.
--
-- /See:/ 'newVolume' smart constructor.
data Volume = Volume'
  { -- | Information about the volume attachments.
    attachments :: Prelude.Maybe [VolumeAttachment],
    -- | Indicates whether the volume was created using fast snapshot restore.
    fastRestored :: Prelude.Maybe Prelude.Bool,
    -- | The number of I\/O operations per second (IOPS). For @gp3@, @io1@, and
    -- @io2@ volumes, this represents the number of IOPS that are provisioned
    -- for the volume. For @gp2@ volumes, this represents the baseline
    -- performance of the volume and the rate at which the volume accumulates
    -- I\/O credits for bursting.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the Key Management Service (KMS) KMS
    -- key that was used to protect the volume encryption key for the volume.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether Amazon EBS Multi-Attach is enabled.
    multiAttachEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the volume.
    tags :: Prelude.Maybe [Tag],
    -- | The throughput that the volume supports, in MiB\/s.
    throughput :: Prelude.Maybe Prelude.Int,
    -- | The Availability Zone for the volume.
    availabilityZone :: Prelude.Text,
    -- | The time stamp when volume creation was initiated.
    createTime :: Data.ISO8601,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Volume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachments', 'volume_attachments' - Information about the volume attachments.
--
-- 'fastRestored', 'volume_fastRestored' - Indicates whether the volume was created using fast snapshot restore.
--
-- 'iops', 'volume_iops' - The number of I\/O operations per second (IOPS). For @gp3@, @io1@, and
-- @io2@ volumes, this represents the number of IOPS that are provisioned
-- for the volume. For @gp2@ volumes, this represents the baseline
-- performance of the volume and the rate at which the volume accumulates
-- I\/O credits for bursting.
--
-- 'kmsKeyId', 'volume_kmsKeyId' - The Amazon Resource Name (ARN) of the Key Management Service (KMS) KMS
-- key that was used to protect the volume encryption key for the volume.
--
-- 'multiAttachEnabled', 'volume_multiAttachEnabled' - Indicates whether Amazon EBS Multi-Attach is enabled.
--
-- 'outpostArn', 'volume_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'tags', 'volume_tags' - Any tags assigned to the volume.
--
-- 'throughput', 'volume_throughput' - The throughput that the volume supports, in MiB\/s.
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
      { attachments = Prelude.Nothing,
        fastRestored = Prelude.Nothing,
        iops = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        multiAttachEnabled = Prelude.Nothing,
        outpostArn = Prelude.Nothing,
        tags = Prelude.Nothing,
        throughput = Prelude.Nothing,
        availabilityZone = pAvailabilityZone_,
        createTime = Data._Time Lens.# pCreateTime_,
        encrypted = pEncrypted_,
        size = pSize_,
        snapshotId = pSnapshotId_,
        state = pState_,
        volumeId = pVolumeId_,
        volumeType = pVolumeType_
      }

-- | Information about the volume attachments.
volume_attachments :: Lens.Lens' Volume (Prelude.Maybe [VolumeAttachment])
volume_attachments = Lens.lens (\Volume' {attachments} -> attachments) (\s@Volume' {} a -> s {attachments = a} :: Volume) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the volume was created using fast snapshot restore.
volume_fastRestored :: Lens.Lens' Volume (Prelude.Maybe Prelude.Bool)
volume_fastRestored = Lens.lens (\Volume' {fastRestored} -> fastRestored) (\s@Volume' {} a -> s {fastRestored = a} :: Volume)

-- | The number of I\/O operations per second (IOPS). For @gp3@, @io1@, and
-- @io2@ volumes, this represents the number of IOPS that are provisioned
-- for the volume. For @gp2@ volumes, this represents the baseline
-- performance of the volume and the rate at which the volume accumulates
-- I\/O credits for bursting.
volume_iops :: Lens.Lens' Volume (Prelude.Maybe Prelude.Int)
volume_iops = Lens.lens (\Volume' {iops} -> iops) (\s@Volume' {} a -> s {iops = a} :: Volume)

-- | The Amazon Resource Name (ARN) of the Key Management Service (KMS) KMS
-- key that was used to protect the volume encryption key for the volume.
volume_kmsKeyId :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_kmsKeyId = Lens.lens (\Volume' {kmsKeyId} -> kmsKeyId) (\s@Volume' {} a -> s {kmsKeyId = a} :: Volume)

-- | Indicates whether Amazon EBS Multi-Attach is enabled.
volume_multiAttachEnabled :: Lens.Lens' Volume (Prelude.Maybe Prelude.Bool)
volume_multiAttachEnabled = Lens.lens (\Volume' {multiAttachEnabled} -> multiAttachEnabled) (\s@Volume' {} a -> s {multiAttachEnabled = a} :: Volume)

-- | The Amazon Resource Name (ARN) of the Outpost.
volume_outpostArn :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_outpostArn = Lens.lens (\Volume' {outpostArn} -> outpostArn) (\s@Volume' {} a -> s {outpostArn = a} :: Volume)

-- | Any tags assigned to the volume.
volume_tags :: Lens.Lens' Volume (Prelude.Maybe [Tag])
volume_tags = Lens.lens (\Volume' {tags} -> tags) (\s@Volume' {} a -> s {tags = a} :: Volume) Prelude.. Lens.mapping Lens.coerced

-- | The throughput that the volume supports, in MiB\/s.
volume_throughput :: Lens.Lens' Volume (Prelude.Maybe Prelude.Int)
volume_throughput = Lens.lens (\Volume' {throughput} -> throughput) (\s@Volume' {} a -> s {throughput = a} :: Volume)

-- | The Availability Zone for the volume.
volume_availabilityZone :: Lens.Lens' Volume Prelude.Text
volume_availabilityZone = Lens.lens (\Volume' {availabilityZone} -> availabilityZone) (\s@Volume' {} a -> s {availabilityZone = a} :: Volume)

-- | The time stamp when volume creation was initiated.
volume_createTime :: Lens.Lens' Volume Prelude.UTCTime
volume_createTime = Lens.lens (\Volume' {createTime} -> createTime) (\s@Volume' {} a -> s {createTime = a} :: Volume) Prelude.. Data._Time

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

instance Data.FromXML Volume where
  parseXML x =
    Volume'
      Prelude.<$> ( x Data..@? "attachmentSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "fastRestored")
      Prelude.<*> (x Data..@? "iops")
      Prelude.<*> (x Data..@? "kmsKeyId")
      Prelude.<*> (x Data..@? "multiAttachEnabled")
      Prelude.<*> (x Data..@? "outpostArn")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "throughput")
      Prelude.<*> (x Data..@ "availabilityZone")
      Prelude.<*> (x Data..@ "createTime")
      Prelude.<*> (x Data..@ "encrypted")
      Prelude.<*> (x Data..@ "size")
      Prelude.<*> (x Data..@ "snapshotId")
      Prelude.<*> (x Data..@ "status")
      Prelude.<*> (x Data..@ "volumeId")
      Prelude.<*> (x Data..@ "volumeType")

instance Prelude.Hashable Volume where
  hashWithSalt _salt Volume' {..} =
    _salt
      `Prelude.hashWithSalt` attachments
      `Prelude.hashWithSalt` fastRestored
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` multiAttachEnabled
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` throughput
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` volumeId
      `Prelude.hashWithSalt` volumeType

instance Prelude.NFData Volume where
  rnf Volume' {..} =
    Prelude.rnf attachments `Prelude.seq`
      Prelude.rnf fastRestored `Prelude.seq`
        Prelude.rnf iops `Prelude.seq`
          Prelude.rnf kmsKeyId `Prelude.seq`
            Prelude.rnf multiAttachEnabled `Prelude.seq`
              Prelude.rnf outpostArn `Prelude.seq`
                Prelude.rnf tags `Prelude.seq`
                  Prelude.rnf throughput `Prelude.seq`
                    Prelude.rnf availabilityZone `Prelude.seq`
                      Prelude.rnf createTime `Prelude.seq`
                        Prelude.rnf encrypted `Prelude.seq`
                          Prelude.rnf size `Prelude.seq`
                            Prelude.rnf snapshotId `Prelude.seq`
                              Prelude.rnf state `Prelude.seq`
                                Prelude.rnf volumeId `Prelude.seq`
                                  Prelude.rnf volumeType
