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
-- Module      : Amazonka.EC2.Types.Snapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Snapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.SnapshotState
import Amazonka.EC2.Types.StorageTier
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a snapshot.
--
-- /See:/ 'newSnapshot' smart constructor.
data Snapshot = Snapshot'
  { -- | The data encryption key identifier for the snapshot. This value is a
    -- unique identifier that corresponds to the data encryption key that was
    -- used to encrypt the original volume or snapshot copy. Because data
    -- encryption keys are inherited by volumes created from snapshots, and
    -- vice versa, if snapshots share the same data encryption key identifier,
    -- then they belong to the same volume\/snapshot lineage. This parameter is
    -- only returned by DescribeSnapshots.
    dataEncryptionKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Key Management Service (KMS) KMS
    -- key that was used to protect the volume encryption key for the parent
    -- volume.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Outpost on which the snapshot is stored. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html Amazon EBS local snapshots on Outposts>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services owner alias, from an Amazon-maintained list
    -- (@amazon@). This is not the user-configured Amazon Web Services account
    -- alias set using the IAM console.
    ownerAlias :: Prelude.Maybe Prelude.Text,
    -- | Only for archived snapshots that are temporarily restored. Indicates the
    -- date and time when a temporarily restored snapshot will be automatically
    -- re-archived.
    restoreExpiryTime :: Prelude.Maybe Data.ISO8601,
    -- | Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot
    -- copy operation fails (for example, if the proper Key Management Service
    -- (KMS) permissions are not obtained) this field displays error state
    -- details to help you diagnose why the error occurred. This parameter is
    -- only returned by DescribeSnapshots.
    stateMessage :: Prelude.Maybe Prelude.Text,
    -- | The storage tier in which the snapshot is stored. @standard@ indicates
    -- that the snapshot is stored in the standard snapshot storage tier and
    -- that it is ready for use. @archive@ indicates that the snapshot is
    -- currently archived and that it must be restored before it can be used.
    storageTier :: Prelude.Maybe StorageTier,
    -- | Any tags assigned to the snapshot.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the snapshot. Each snapshot receives a unique identifier when
    -- it is created.
    snapshotId :: Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the EBS snapshot.
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
    startTime :: Data.ISO8601,
    -- | The progress of the snapshot, as a percentage.
    progress :: Prelude.Text,
    -- | The snapshot state.
    state :: SnapshotState,
    -- | Indicates whether the snapshot is encrypted.
    encrypted :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Snapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataEncryptionKeyId', 'snapshot_dataEncryptionKeyId' - The data encryption key identifier for the snapshot. This value is a
-- unique identifier that corresponds to the data encryption key that was
-- used to encrypt the original volume or snapshot copy. Because data
-- encryption keys are inherited by volumes created from snapshots, and
-- vice versa, if snapshots share the same data encryption key identifier,
-- then they belong to the same volume\/snapshot lineage. This parameter is
-- only returned by DescribeSnapshots.
--
-- 'kmsKeyId', 'snapshot_kmsKeyId' - The Amazon Resource Name (ARN) of the Key Management Service (KMS) KMS
-- key that was used to protect the volume encryption key for the parent
-- volume.
--
-- 'outpostArn', 'snapshot_outpostArn' - The ARN of the Outpost on which the snapshot is stored. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html Amazon EBS local snapshots on Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'ownerAlias', 'snapshot_ownerAlias' - The Amazon Web Services owner alias, from an Amazon-maintained list
-- (@amazon@). This is not the user-configured Amazon Web Services account
-- alias set using the IAM console.
--
-- 'restoreExpiryTime', 'snapshot_restoreExpiryTime' - Only for archived snapshots that are temporarily restored. Indicates the
-- date and time when a temporarily restored snapshot will be automatically
-- re-archived.
--
-- 'stateMessage', 'snapshot_stateMessage' - Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot
-- copy operation fails (for example, if the proper Key Management Service
-- (KMS) permissions are not obtained) this field displays error state
-- details to help you diagnose why the error occurred. This parameter is
-- only returned by DescribeSnapshots.
--
-- 'storageTier', 'snapshot_storageTier' - The storage tier in which the snapshot is stored. @standard@ indicates
-- that the snapshot is stored in the standard snapshot storage tier and
-- that it is ready for use. @archive@ indicates that the snapshot is
-- currently archived and that it must be restored before it can be used.
--
-- 'tags', 'snapshot_tags' - Any tags assigned to the snapshot.
--
-- 'snapshotId', 'snapshot_snapshotId' - The ID of the snapshot. Each snapshot receives a unique identifier when
-- it is created.
--
-- 'ownerId', 'snapshot_ownerId' - The ID of the Amazon Web Services account that owns the EBS snapshot.
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
      { dataEncryptionKeyId = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        outpostArn = Prelude.Nothing,
        ownerAlias = Prelude.Nothing,
        restoreExpiryTime = Prelude.Nothing,
        stateMessage = Prelude.Nothing,
        storageTier = Prelude.Nothing,
        tags = Prelude.Nothing,
        snapshotId = pSnapshotId_,
        ownerId = pOwnerId_,
        volumeId = pVolumeId_,
        volumeSize = pVolumeSize_,
        description = pDescription_,
        startTime = Data._Time Lens.# pStartTime_,
        progress = pProgress_,
        state = pState_,
        encrypted = pEncrypted_
      }

-- | The data encryption key identifier for the snapshot. This value is a
-- unique identifier that corresponds to the data encryption key that was
-- used to encrypt the original volume or snapshot copy. Because data
-- encryption keys are inherited by volumes created from snapshots, and
-- vice versa, if snapshots share the same data encryption key identifier,
-- then they belong to the same volume\/snapshot lineage. This parameter is
-- only returned by DescribeSnapshots.
snapshot_dataEncryptionKeyId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_dataEncryptionKeyId = Lens.lens (\Snapshot' {dataEncryptionKeyId} -> dataEncryptionKeyId) (\s@Snapshot' {} a -> s {dataEncryptionKeyId = a} :: Snapshot)

-- | The Amazon Resource Name (ARN) of the Key Management Service (KMS) KMS
-- key that was used to protect the volume encryption key for the parent
-- volume.
snapshot_kmsKeyId :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_kmsKeyId = Lens.lens (\Snapshot' {kmsKeyId} -> kmsKeyId) (\s@Snapshot' {} a -> s {kmsKeyId = a} :: Snapshot)

-- | The ARN of the Outpost on which the snapshot is stored. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html Amazon EBS local snapshots on Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
snapshot_outpostArn :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_outpostArn = Lens.lens (\Snapshot' {outpostArn} -> outpostArn) (\s@Snapshot' {} a -> s {outpostArn = a} :: Snapshot)

-- | The Amazon Web Services owner alias, from an Amazon-maintained list
-- (@amazon@). This is not the user-configured Amazon Web Services account
-- alias set using the IAM console.
snapshot_ownerAlias :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_ownerAlias = Lens.lens (\Snapshot' {ownerAlias} -> ownerAlias) (\s@Snapshot' {} a -> s {ownerAlias = a} :: Snapshot)

-- | Only for archived snapshots that are temporarily restored. Indicates the
-- date and time when a temporarily restored snapshot will be automatically
-- re-archived.
snapshot_restoreExpiryTime :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.UTCTime)
snapshot_restoreExpiryTime = Lens.lens (\Snapshot' {restoreExpiryTime} -> restoreExpiryTime) (\s@Snapshot' {} a -> s {restoreExpiryTime = a} :: Snapshot) Prelude.. Lens.mapping Data._Time

-- | Encrypted Amazon EBS snapshots are copied asynchronously. If a snapshot
-- copy operation fails (for example, if the proper Key Management Service
-- (KMS) permissions are not obtained) this field displays error state
-- details to help you diagnose why the error occurred. This parameter is
-- only returned by DescribeSnapshots.
snapshot_stateMessage :: Lens.Lens' Snapshot (Prelude.Maybe Prelude.Text)
snapshot_stateMessage = Lens.lens (\Snapshot' {stateMessage} -> stateMessage) (\s@Snapshot' {} a -> s {stateMessage = a} :: Snapshot)

-- | The storage tier in which the snapshot is stored. @standard@ indicates
-- that the snapshot is stored in the standard snapshot storage tier and
-- that it is ready for use. @archive@ indicates that the snapshot is
-- currently archived and that it must be restored before it can be used.
snapshot_storageTier :: Lens.Lens' Snapshot (Prelude.Maybe StorageTier)
snapshot_storageTier = Lens.lens (\Snapshot' {storageTier} -> storageTier) (\s@Snapshot' {} a -> s {storageTier = a} :: Snapshot)

-- | Any tags assigned to the snapshot.
snapshot_tags :: Lens.Lens' Snapshot (Prelude.Maybe [Tag])
snapshot_tags = Lens.lens (\Snapshot' {tags} -> tags) (\s@Snapshot' {} a -> s {tags = a} :: Snapshot) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the snapshot. Each snapshot receives a unique identifier when
-- it is created.
snapshot_snapshotId :: Lens.Lens' Snapshot Prelude.Text
snapshot_snapshotId = Lens.lens (\Snapshot' {snapshotId} -> snapshotId) (\s@Snapshot' {} a -> s {snapshotId = a} :: Snapshot)

-- | The ID of the Amazon Web Services account that owns the EBS snapshot.
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
snapshot_startTime = Lens.lens (\Snapshot' {startTime} -> startTime) (\s@Snapshot' {} a -> s {startTime = a} :: Snapshot) Prelude.. Data._Time

-- | The progress of the snapshot, as a percentage.
snapshot_progress :: Lens.Lens' Snapshot Prelude.Text
snapshot_progress = Lens.lens (\Snapshot' {progress} -> progress) (\s@Snapshot' {} a -> s {progress = a} :: Snapshot)

-- | The snapshot state.
snapshot_state :: Lens.Lens' Snapshot SnapshotState
snapshot_state = Lens.lens (\Snapshot' {state} -> state) (\s@Snapshot' {} a -> s {state = a} :: Snapshot)

-- | Indicates whether the snapshot is encrypted.
snapshot_encrypted :: Lens.Lens' Snapshot Prelude.Bool
snapshot_encrypted = Lens.lens (\Snapshot' {encrypted} -> encrypted) (\s@Snapshot' {} a -> s {encrypted = a} :: Snapshot)

instance Data.FromXML Snapshot where
  parseXML x =
    Snapshot'
      Prelude.<$> (x Data..@? "dataEncryptionKeyId")
      Prelude.<*> (x Data..@? "kmsKeyId")
      Prelude.<*> (x Data..@? "outpostArn")
      Prelude.<*> (x Data..@? "ownerAlias")
      Prelude.<*> (x Data..@? "restoreExpiryTime")
      Prelude.<*> (x Data..@? "statusMessage")
      Prelude.<*> (x Data..@? "storageTier")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@ "snapshotId")
      Prelude.<*> (x Data..@ "ownerId")
      Prelude.<*> (x Data..@ "volumeId")
      Prelude.<*> (x Data..@ "volumeSize")
      Prelude.<*> (x Data..@ "description")
      Prelude.<*> (x Data..@ "startTime")
      Prelude.<*> (x Data..@ "progress")
      Prelude.<*> (x Data..@ "status")
      Prelude.<*> (x Data..@ "encrypted")

instance Prelude.Hashable Snapshot where
  hashWithSalt _salt Snapshot' {..} =
    _salt
      `Prelude.hashWithSalt` dataEncryptionKeyId
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` ownerAlias
      `Prelude.hashWithSalt` restoreExpiryTime
      `Prelude.hashWithSalt` stateMessage
      `Prelude.hashWithSalt` storageTier
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` volumeId
      `Prelude.hashWithSalt` volumeSize
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` progress
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` encrypted

instance Prelude.NFData Snapshot where
  rnf Snapshot' {..} =
    Prelude.rnf dataEncryptionKeyId
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf ownerAlias
      `Prelude.seq` Prelude.rnf restoreExpiryTime
      `Prelude.seq` Prelude.rnf stateMessage
      `Prelude.seq` Prelude.rnf storageTier
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf volumeId
      `Prelude.seq` Prelude.rnf volumeSize
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf progress
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf encrypted
