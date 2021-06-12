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
-- Module      : Network.AWS.EC2.Types.Snapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Snapshot where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SnapshotState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a snapshot.
--
-- /See:/ 'newSnapshot' smart constructor.
data Snapshot = Snapshot'
  { -- | The AWS owner alias, from an Amazon-maintained list (@amazon@). This is
    -- not the user-configured AWS account alias set using the IAM console.
    ownerAlias :: Core.Maybe Core.Text,
    -- | Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot
    -- copy operation fails (for example, if the proper AWS Key Management
    -- Service (AWS KMS) permissions are not obtained) this field displays
    -- error state details to help you diagnose why the error occurred. This
    -- parameter is only returned by DescribeSnapshots.
    stateMessage :: Core.Maybe Core.Text,
    -- | The ARN of the AWS Outpost on which the snapshot is stored. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html EBS Local Snapshot on Outposts>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    outpostArn :: Core.Maybe Core.Text,
    -- | The data encryption key identifier for the snapshot. This value is a
    -- unique identifier that corresponds to the data encryption key that was
    -- used to encrypt the original volume or snapshot copy. Because data
    -- encryption keys are inherited by volumes created from snapshots, and
    -- vice versa, if snapshots share the same data encryption key identifier,
    -- then they belong to the same volume\/snapshot lineage. This parameter is
    -- only returned by DescribeSnapshots.
    dataEncryptionKeyId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS
    -- KMS) customer master key (CMK) that was used to protect the volume
    -- encryption key for the parent volume.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | Any tags assigned to the snapshot.
    tags :: Core.Maybe [Tag],
    -- | The ID of the snapshot. Each snapshot receives a unique identifier when
    -- it is created.
    snapshotId :: Core.Text,
    -- | The AWS account ID of the EBS snapshot owner.
    ownerId :: Core.Text,
    -- | The ID of the volume that was used to create the snapshot. Snapshots
    -- created by the CopySnapshot action have an arbitrary volume ID that
    -- should not be used for any purpose.
    volumeId :: Core.Text,
    -- | The size of the volume, in GiB.
    volumeSize :: Core.Int,
    -- | The description for the snapshot.
    description :: Core.Text,
    -- | The time stamp when the snapshot was initiated.
    startTime :: Core.ISO8601,
    -- | The progress of the snapshot, as a percentage.
    progress :: Core.Text,
    -- | The snapshot state.
    state :: SnapshotState,
    -- | Indicates whether the snapshot is encrypted.
    encrypted :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Snapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerAlias', 'snapshot_ownerAlias' - The AWS owner alias, from an Amazon-maintained list (@amazon@). This is
-- not the user-configured AWS account alias set using the IAM console.
--
-- 'stateMessage', 'snapshot_stateMessage' - Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot
-- copy operation fails (for example, if the proper AWS Key Management
-- Service (AWS KMS) permissions are not obtained) this field displays
-- error state details to help you diagnose why the error occurred. This
-- parameter is only returned by DescribeSnapshots.
--
-- 'outpostArn', 'snapshot_outpostArn' - The ARN of the AWS Outpost on which the snapshot is stored. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html EBS Local Snapshot on Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'dataEncryptionKeyId', 'snapshot_dataEncryptionKeyId' - The data encryption key identifier for the snapshot. This value is a
-- unique identifier that corresponds to the data encryption key that was
-- used to encrypt the original volume or snapshot copy. Because data
-- encryption keys are inherited by volumes created from snapshots, and
-- vice versa, if snapshots share the same data encryption key identifier,
-- then they belong to the same volume\/snapshot lineage. This parameter is
-- only returned by DescribeSnapshots.
--
-- 'kmsKeyId', 'snapshot_kmsKeyId' - The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS
-- KMS) customer master key (CMK) that was used to protect the volume
-- encryption key for the parent volume.
--
-- 'tags', 'snapshot_tags' - Any tags assigned to the snapshot.
--
-- 'snapshotId', 'snapshot_snapshotId' - The ID of the snapshot. Each snapshot receives a unique identifier when
-- it is created.
--
-- 'ownerId', 'snapshot_ownerId' - The AWS account ID of the EBS snapshot owner.
--
-- 'volumeId', 'snapshot_volumeId' - The ID of the volume that was used to create the snapshot. Snapshots
-- created by the CopySnapshot action have an arbitrary volume ID that
-- should not be used for any purpose.
--
-- 'volumeSize', 'snapshot_volumeSize' - The size of the volume, in GiB.
--
-- 'description', 'snapshot_description' - The description for the snapshot.
--
-- 'startTime', 'snapshot_startTime' - The time stamp when the snapshot was initiated.
--
-- 'progress', 'snapshot_progress' - The progress of the snapshot, as a percentage.
--
-- 'state', 'snapshot_state' - The snapshot state.
--
-- 'encrypted', 'snapshot_encrypted' - Indicates whether the snapshot is encrypted.
newSnapshot ::
  -- | 'snapshotId'
  Core.Text ->
  -- | 'ownerId'
  Core.Text ->
  -- | 'volumeId'
  Core.Text ->
  -- | 'volumeSize'
  Core.Int ->
  -- | 'description'
  Core.Text ->
  -- | 'startTime'
  Core.UTCTime ->
  -- | 'progress'
  Core.Text ->
  -- | 'state'
  SnapshotState ->
  -- | 'encrypted'
  Core.Bool ->
  Snapshot
newSnapshot
  pSnapshotId_
  pOwnerId_
  pVolumeId_
  pVolumeSize_
  pDescription_
  pStartTime_
  pProgress_
  pState_
  pEncrypted_ =
    Snapshot'
      { ownerAlias = Core.Nothing,
        stateMessage = Core.Nothing,
        outpostArn = Core.Nothing,
        dataEncryptionKeyId = Core.Nothing,
        kmsKeyId = Core.Nothing,
        tags = Core.Nothing,
        snapshotId = pSnapshotId_,
        ownerId = pOwnerId_,
        volumeId = pVolumeId_,
        volumeSize = pVolumeSize_,
        description = pDescription_,
        startTime = Core._Time Lens.# pStartTime_,
        progress = pProgress_,
        state = pState_,
        encrypted = pEncrypted_
      }

-- | The AWS owner alias, from an Amazon-maintained list (@amazon@). This is
-- not the user-configured AWS account alias set using the IAM console.
snapshot_ownerAlias :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_ownerAlias = Lens.lens (\Snapshot' {ownerAlias} -> ownerAlias) (\s@Snapshot' {} a -> s {ownerAlias = a} :: Snapshot)

-- | Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot
-- copy operation fails (for example, if the proper AWS Key Management
-- Service (AWS KMS) permissions are not obtained) this field displays
-- error state details to help you diagnose why the error occurred. This
-- parameter is only returned by DescribeSnapshots.
snapshot_stateMessage :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_stateMessage = Lens.lens (\Snapshot' {stateMessage} -> stateMessage) (\s@Snapshot' {} a -> s {stateMessage = a} :: Snapshot)

-- | The ARN of the AWS Outpost on which the snapshot is stored. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html EBS Local Snapshot on Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
snapshot_outpostArn :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_outpostArn = Lens.lens (\Snapshot' {outpostArn} -> outpostArn) (\s@Snapshot' {} a -> s {outpostArn = a} :: Snapshot)

-- | The data encryption key identifier for the snapshot. This value is a
-- unique identifier that corresponds to the data encryption key that was
-- used to encrypt the original volume or snapshot copy. Because data
-- encryption keys are inherited by volumes created from snapshots, and
-- vice versa, if snapshots share the same data encryption key identifier,
-- then they belong to the same volume\/snapshot lineage. This parameter is
-- only returned by DescribeSnapshots.
snapshot_dataEncryptionKeyId :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_dataEncryptionKeyId = Lens.lens (\Snapshot' {dataEncryptionKeyId} -> dataEncryptionKeyId) (\s@Snapshot' {} a -> s {dataEncryptionKeyId = a} :: Snapshot)

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS
-- KMS) customer master key (CMK) that was used to protect the volume
-- encryption key for the parent volume.
snapshot_kmsKeyId :: Lens.Lens' Snapshot (Core.Maybe Core.Text)
snapshot_kmsKeyId = Lens.lens (\Snapshot' {kmsKeyId} -> kmsKeyId) (\s@Snapshot' {} a -> s {kmsKeyId = a} :: Snapshot)

-- | Any tags assigned to the snapshot.
snapshot_tags :: Lens.Lens' Snapshot (Core.Maybe [Tag])
snapshot_tags = Lens.lens (\Snapshot' {tags} -> tags) (\s@Snapshot' {} a -> s {tags = a} :: Snapshot) Core.. Lens.mapping Lens._Coerce

-- | The ID of the snapshot. Each snapshot receives a unique identifier when
-- it is created.
snapshot_snapshotId :: Lens.Lens' Snapshot Core.Text
snapshot_snapshotId = Lens.lens (\Snapshot' {snapshotId} -> snapshotId) (\s@Snapshot' {} a -> s {snapshotId = a} :: Snapshot)

-- | The AWS account ID of the EBS snapshot owner.
snapshot_ownerId :: Lens.Lens' Snapshot Core.Text
snapshot_ownerId = Lens.lens (\Snapshot' {ownerId} -> ownerId) (\s@Snapshot' {} a -> s {ownerId = a} :: Snapshot)

-- | The ID of the volume that was used to create the snapshot. Snapshots
-- created by the CopySnapshot action have an arbitrary volume ID that
-- should not be used for any purpose.
snapshot_volumeId :: Lens.Lens' Snapshot Core.Text
snapshot_volumeId = Lens.lens (\Snapshot' {volumeId} -> volumeId) (\s@Snapshot' {} a -> s {volumeId = a} :: Snapshot)

-- | The size of the volume, in GiB.
snapshot_volumeSize :: Lens.Lens' Snapshot Core.Int
snapshot_volumeSize = Lens.lens (\Snapshot' {volumeSize} -> volumeSize) (\s@Snapshot' {} a -> s {volumeSize = a} :: Snapshot)

-- | The description for the snapshot.
snapshot_description :: Lens.Lens' Snapshot Core.Text
snapshot_description = Lens.lens (\Snapshot' {description} -> description) (\s@Snapshot' {} a -> s {description = a} :: Snapshot)

-- | The time stamp when the snapshot was initiated.
snapshot_startTime :: Lens.Lens' Snapshot Core.UTCTime
snapshot_startTime = Lens.lens (\Snapshot' {startTime} -> startTime) (\s@Snapshot' {} a -> s {startTime = a} :: Snapshot) Core.. Core._Time

-- | The progress of the snapshot, as a percentage.
snapshot_progress :: Lens.Lens' Snapshot Core.Text
snapshot_progress = Lens.lens (\Snapshot' {progress} -> progress) (\s@Snapshot' {} a -> s {progress = a} :: Snapshot)

-- | The snapshot state.
snapshot_state :: Lens.Lens' Snapshot SnapshotState
snapshot_state = Lens.lens (\Snapshot' {state} -> state) (\s@Snapshot' {} a -> s {state = a} :: Snapshot)

-- | Indicates whether the snapshot is encrypted.
snapshot_encrypted :: Lens.Lens' Snapshot Core.Bool
snapshot_encrypted = Lens.lens (\Snapshot' {encrypted} -> encrypted) (\s@Snapshot' {} a -> s {encrypted = a} :: Snapshot)

instance Core.FromXML Snapshot where
  parseXML x =
    Snapshot'
      Core.<$> (x Core..@? "ownerAlias")
      Core.<*> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "outpostArn")
      Core.<*> (x Core..@? "dataEncryptionKeyId")
      Core.<*> (x Core..@? "kmsKeyId")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@ "snapshotId")
      Core.<*> (x Core..@ "ownerId")
      Core.<*> (x Core..@ "volumeId")
      Core.<*> (x Core..@ "volumeSize")
      Core.<*> (x Core..@ "description")
      Core.<*> (x Core..@ "startTime")
      Core.<*> (x Core..@ "progress")
      Core.<*> (x Core..@ "status")
      Core.<*> (x Core..@ "encrypted")

instance Core.Hashable Snapshot

instance Core.NFData Snapshot
