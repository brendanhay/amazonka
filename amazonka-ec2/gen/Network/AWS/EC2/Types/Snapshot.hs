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
-- Module      : Network.AWS.EC2.Types.Snapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Snapshot where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SnapshotState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a snapshot.
--
-- /See:/ 'newSnapshot' smart constructor.
data Snapshot = Snapshot'
  { -- | The AWS owner alias, from an Amazon-maintained list (@amazon@). This is
    -- not the user-configured AWS account alias set using the IAM console.
    ownerAlias :: Prelude.Maybe Prelude.Text,
    -- | Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot
    -- copy operation fails (for example, if the proper AWS Key Management
    -- Service (AWS KMS) permissions are not obtained) this field displays
    -- error state details to help you diagnose why the error occurred. This
    -- parameter is only returned by DescribeSnapshots.
    stateMessage :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the AWS Outpost on which the snapshot is stored. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html EBS Local Snapshot on Outposts>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The data encryption key identifier for the snapshot. This value is a
    -- unique identifier that corresponds to the data encryption key that was
    -- used to encrypt the original volume or snapshot copy. Because data
    -- encryption keys are inherited by volumes created from snapshots, and
    -- vice versa, if snapshots share the same data encryption key identifier,
    -- then they belong to the same volume\/snapshot lineage. This parameter is
    -- only returned by DescribeSnapshots.
    dataEncryptionKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS
    -- KMS) customer master key (CMK) that was used to protect the volume
    -- encryption key for the parent volume.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the snapshot.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the snapshot. Each snapshot receives a unique identifier when
    -- it is created.
    snapshotId :: Prelude.Text,
    -- | The AWS account ID of the EBS snapshot owner.
    ownerId :: Prelude.Text,
    -- | The ID of the volume that was used to create the snapshot. Snapshots
    -- created by the CopySnapshot action have an arbitrary volume ID that
    -- should not be used for any purpose.
    volumeId :: Prelude.Text,
    -- | The size of the volume, in GiB.
    volumeSize :: Prelude.Int,
    -- | The description for the snapshot.
    description :: Prelude.Text,
    -- | The time stamp when the snapshot was initiated.
    startTime :: Prelude.ISO8601,
    -- | The progress of the snapshot, as a percentage.
    progress :: Prelude.Text,
    -- | The snapshot state.
    state :: SnapshotState,
    -- | Indicates whether the snapshot is encrypted.
    encrypted :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'ownerId'
  Prelude.Text ->
  -- | 'volumeId'
  Prelude.Text ->
  -- | 'volumeSize'
  Prelude.Int ->
  -- | 'description'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'progress'
  Prelude.Text ->
  -- | 'state'
  SnapshotState ->
  -- | 'encrypted'
  Prelude.Bool ->
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
      { ownerAlias = Prelude.Nothing,
        stateMessage = Prelude.Nothing,
        outpostArn = Prelude.Nothing,
        dataEncryptionKeyId = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        tags = Prelude.Nothing,
        snapshotId = pSnapshotId_,
        ownerId = pOwnerId_,
        volumeId = pVolumeId_,
        volumeSize = pVolumeSize_,
        description = pDescription_,
        startTime = Prelude._Time Lens.# pStartTime_,
        progress = pProgress_,
        state = pState_,
        encrypted = pEncrypted_
      }

-- | The AWS owner alias, from an Amazon-maintained list (@amazon@). This is
-- not the user-configured AWS account alias set using the IAM console.
snapshot_ownerAlias :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_ownerAlias = Lens.lens (\Snapshot' {ownerAlias} -> ownerAlias) (\s@Snapshot' {} a -> s {ownerAlias = a} :: Snapshot)

-- | Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot
-- copy operation fails (for example, if the proper AWS Key Management
-- Service (AWS KMS) permissions are not obtained) this field displays
-- error state details to help you diagnose why the error occurred. This
-- parameter is only returned by DescribeSnapshots.
snapshot_stateMessage :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_stateMessage = Lens.lens (\Snapshot' {stateMessage} -> stateMessage) (\s@Snapshot' {} a -> s {stateMessage = a} :: Snapshot)

-- | The ARN of the AWS Outpost on which the snapshot is stored. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html EBS Local Snapshot on Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
snapshot_outpostArn :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_outpostArn = Lens.lens (\Snapshot' {outpostArn} -> outpostArn) (\s@Snapshot' {} a -> s {outpostArn = a} :: Snapshot)

-- | The data encryption key identifier for the snapshot. This value is a
-- unique identifier that corresponds to the data encryption key that was
-- used to encrypt the original volume or snapshot copy. Because data
-- encryption keys are inherited by volumes created from snapshots, and
-- vice versa, if snapshots share the same data encryption key identifier,
-- then they belong to the same volume\/snapshot lineage. This parameter is
-- only returned by DescribeSnapshots.
snapshot_dataEncryptionKeyId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_dataEncryptionKeyId = Lens.lens (\Snapshot' {dataEncryptionKeyId} -> dataEncryptionKeyId) (\s@Snapshot' {} a -> s {dataEncryptionKeyId = a} :: Snapshot)

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS
-- KMS) customer master key (CMK) that was used to protect the volume
-- encryption key for the parent volume.
snapshot_kmsKeyId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_kmsKeyId = Lens.lens (\Snapshot' {kmsKeyId} -> kmsKeyId) (\s@Snapshot' {} a -> s {kmsKeyId = a} :: Snapshot)

-- | Any tags assigned to the snapshot.
snapshot_tags :: Lens.Lens' Snapshot (Prelude.Maybe [Tag])
snapshot_tags = Lens.lens (\Snapshot' {tags} -> tags) (\s@Snapshot' {} a -> s {tags = a} :: Snapshot) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the snapshot. Each snapshot receives a unique identifier when
-- it is created.
snapshot_snapshotId :: Lens.Lens' Snapshot Prelude.Text
snapshot_snapshotId = Lens.lens (\Snapshot' {snapshotId} -> snapshotId) (\s@Snapshot' {} a -> s {snapshotId = a} :: Snapshot)

-- | The AWS account ID of the EBS snapshot owner.
snapshot_ownerId :: Lens.Lens' Snapshot Prelude.Text
snapshot_ownerId = Lens.lens (\Snapshot' {ownerId} -> ownerId) (\s@Snapshot' {} a -> s {ownerId = a} :: Snapshot)

-- | The ID of the volume that was used to create the snapshot. Snapshots
-- created by the CopySnapshot action have an arbitrary volume ID that
-- should not be used for any purpose.
snapshot_volumeId :: Lens.Lens' Snapshot Prelude.Text
snapshot_volumeId = Lens.lens (\Snapshot' {volumeId} -> volumeId) (\s@Snapshot' {} a -> s {volumeId = a} :: Snapshot)

-- | The size of the volume, in GiB.
snapshot_volumeSize :: Lens.Lens' Snapshot Prelude.Int
snapshot_volumeSize = Lens.lens (\Snapshot' {volumeSize} -> volumeSize) (\s@Snapshot' {} a -> s {volumeSize = a} :: Snapshot)

-- | The description for the snapshot.
snapshot_description :: Lens.Lens' Snapshot Prelude.Text
snapshot_description = Lens.lens (\Snapshot' {description} -> description) (\s@Snapshot' {} a -> s {description = a} :: Snapshot)

-- | The time stamp when the snapshot was initiated.
snapshot_startTime :: Lens.Lens' Snapshot Prelude.UTCTime
snapshot_startTime = Lens.lens (\Snapshot' {startTime} -> startTime) (\s@Snapshot' {} a -> s {startTime = a} :: Snapshot) Prelude.. Prelude._Time

-- | The progress of the snapshot, as a percentage.
snapshot_progress :: Lens.Lens' Snapshot Prelude.Text
snapshot_progress = Lens.lens (\Snapshot' {progress} -> progress) (\s@Snapshot' {} a -> s {progress = a} :: Snapshot)

-- | The snapshot state.
snapshot_state :: Lens.Lens' Snapshot SnapshotState
snapshot_state = Lens.lens (\Snapshot' {state} -> state) (\s@Snapshot' {} a -> s {state = a} :: Snapshot)

-- | Indicates whether the snapshot is encrypted.
snapshot_encrypted :: Lens.Lens' Snapshot Prelude.Bool
snapshot_encrypted = Lens.lens (\Snapshot' {encrypted} -> encrypted) (\s@Snapshot' {} a -> s {encrypted = a} :: Snapshot)

instance Prelude.FromXML Snapshot where
  parseXML x =
    Snapshot'
      Prelude.<$> (x Prelude..@? "ownerAlias")
      Prelude.<*> (x Prelude..@? "statusMessage")
      Prelude.<*> (x Prelude..@? "outpostArn")
      Prelude.<*> (x Prelude..@? "dataEncryptionKeyId")
      Prelude.<*> (x Prelude..@? "kmsKeyId")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@ "snapshotId")
      Prelude.<*> (x Prelude..@ "ownerId")
      Prelude.<*> (x Prelude..@ "volumeId")
      Prelude.<*> (x Prelude..@ "volumeSize")
      Prelude.<*> (x Prelude..@ "description")
      Prelude.<*> (x Prelude..@ "startTime")
      Prelude.<*> (x Prelude..@ "progress")
      Prelude.<*> (x Prelude..@ "status")
      Prelude.<*> (x Prelude..@ "encrypted")

instance Prelude.Hashable Snapshot

instance Prelude.NFData Snapshot
