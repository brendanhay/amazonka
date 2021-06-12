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
-- Module      : Network.AWS.EC2.Types.SnapshotInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SnapshotState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Information about a snapshot.
--
-- /See:/ 'newSnapshotInfo' smart constructor.
data SnapshotInfo = SnapshotInfo'
  { -- | Account id used when creating this snapshot.
    ownerId :: Core.Maybe Core.Text,
    -- | Indicates whether the snapshot is encrypted.
    encrypted :: Core.Maybe Core.Bool,
    -- | The ARN of the AWS Outpost on which the snapshot is stored. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html EBS Local Snapshot on Outposts>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    outpostArn :: Core.Maybe Core.Text,
    -- | Time this snapshot was started. This is the same for all snapshots
    -- initiated by the same request.
    startTime :: Core.Maybe Core.ISO8601,
    -- | Source volume from which this snapshot was created.
    volumeId :: Core.Maybe Core.Text,
    -- | Current state of the snapshot.
    state :: Core.Maybe SnapshotState,
    -- | Snapshot id that can be used to describe this snapshot.
    snapshotId :: Core.Maybe Core.Text,
    -- | Tags associated with this snapshot.
    tags :: Core.Maybe [Tag],
    -- | Description specified by the CreateSnapshotRequest that has been applied
    -- to all snapshots.
    description :: Core.Maybe Core.Text,
    -- | Progress this snapshot has made towards completing.
    progress :: Core.Maybe Core.Text,
    -- | Size of the volume from which this snapshot was created.
    volumeSize :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SnapshotInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'snapshotInfo_ownerId' - Account id used when creating this snapshot.
--
-- 'encrypted', 'snapshotInfo_encrypted' - Indicates whether the snapshot is encrypted.
--
-- 'outpostArn', 'snapshotInfo_outpostArn' - The ARN of the AWS Outpost on which the snapshot is stored. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html EBS Local Snapshot on Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'startTime', 'snapshotInfo_startTime' - Time this snapshot was started. This is the same for all snapshots
-- initiated by the same request.
--
-- 'volumeId', 'snapshotInfo_volumeId' - Source volume from which this snapshot was created.
--
-- 'state', 'snapshotInfo_state' - Current state of the snapshot.
--
-- 'snapshotId', 'snapshotInfo_snapshotId' - Snapshot id that can be used to describe this snapshot.
--
-- 'tags', 'snapshotInfo_tags' - Tags associated with this snapshot.
--
-- 'description', 'snapshotInfo_description' - Description specified by the CreateSnapshotRequest that has been applied
-- to all snapshots.
--
-- 'progress', 'snapshotInfo_progress' - Progress this snapshot has made towards completing.
--
-- 'volumeSize', 'snapshotInfo_volumeSize' - Size of the volume from which this snapshot was created.
newSnapshotInfo ::
  SnapshotInfo
newSnapshotInfo =
  SnapshotInfo'
    { ownerId = Core.Nothing,
      encrypted = Core.Nothing,
      outpostArn = Core.Nothing,
      startTime = Core.Nothing,
      volumeId = Core.Nothing,
      state = Core.Nothing,
      snapshotId = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      progress = Core.Nothing,
      volumeSize = Core.Nothing
    }

-- | Account id used when creating this snapshot.
snapshotInfo_ownerId :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Text)
snapshotInfo_ownerId = Lens.lens (\SnapshotInfo' {ownerId} -> ownerId) (\s@SnapshotInfo' {} a -> s {ownerId = a} :: SnapshotInfo)

-- | Indicates whether the snapshot is encrypted.
snapshotInfo_encrypted :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Bool)
snapshotInfo_encrypted = Lens.lens (\SnapshotInfo' {encrypted} -> encrypted) (\s@SnapshotInfo' {} a -> s {encrypted = a} :: SnapshotInfo)

-- | The ARN of the AWS Outpost on which the snapshot is stored. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html EBS Local Snapshot on Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
snapshotInfo_outpostArn :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Text)
snapshotInfo_outpostArn = Lens.lens (\SnapshotInfo' {outpostArn} -> outpostArn) (\s@SnapshotInfo' {} a -> s {outpostArn = a} :: SnapshotInfo)

-- | Time this snapshot was started. This is the same for all snapshots
-- initiated by the same request.
snapshotInfo_startTime :: Lens.Lens' SnapshotInfo (Core.Maybe Core.UTCTime)
snapshotInfo_startTime = Lens.lens (\SnapshotInfo' {startTime} -> startTime) (\s@SnapshotInfo' {} a -> s {startTime = a} :: SnapshotInfo) Core.. Lens.mapping Core._Time

-- | Source volume from which this snapshot was created.
snapshotInfo_volumeId :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Text)
snapshotInfo_volumeId = Lens.lens (\SnapshotInfo' {volumeId} -> volumeId) (\s@SnapshotInfo' {} a -> s {volumeId = a} :: SnapshotInfo)

-- | Current state of the snapshot.
snapshotInfo_state :: Lens.Lens' SnapshotInfo (Core.Maybe SnapshotState)
snapshotInfo_state = Lens.lens (\SnapshotInfo' {state} -> state) (\s@SnapshotInfo' {} a -> s {state = a} :: SnapshotInfo)

-- | Snapshot id that can be used to describe this snapshot.
snapshotInfo_snapshotId :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Text)
snapshotInfo_snapshotId = Lens.lens (\SnapshotInfo' {snapshotId} -> snapshotId) (\s@SnapshotInfo' {} a -> s {snapshotId = a} :: SnapshotInfo)

-- | Tags associated with this snapshot.
snapshotInfo_tags :: Lens.Lens' SnapshotInfo (Core.Maybe [Tag])
snapshotInfo_tags = Lens.lens (\SnapshotInfo' {tags} -> tags) (\s@SnapshotInfo' {} a -> s {tags = a} :: SnapshotInfo) Core.. Lens.mapping Lens._Coerce

-- | Description specified by the CreateSnapshotRequest that has been applied
-- to all snapshots.
snapshotInfo_description :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Text)
snapshotInfo_description = Lens.lens (\SnapshotInfo' {description} -> description) (\s@SnapshotInfo' {} a -> s {description = a} :: SnapshotInfo)

-- | Progress this snapshot has made towards completing.
snapshotInfo_progress :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Text)
snapshotInfo_progress = Lens.lens (\SnapshotInfo' {progress} -> progress) (\s@SnapshotInfo' {} a -> s {progress = a} :: SnapshotInfo)

-- | Size of the volume from which this snapshot was created.
snapshotInfo_volumeSize :: Lens.Lens' SnapshotInfo (Core.Maybe Core.Int)
snapshotInfo_volumeSize = Lens.lens (\SnapshotInfo' {volumeSize} -> volumeSize) (\s@SnapshotInfo' {} a -> s {volumeSize = a} :: SnapshotInfo)

instance Core.FromXML SnapshotInfo where
  parseXML x =
    SnapshotInfo'
      Core.<$> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "encrypted")
      Core.<*> (x Core..@? "outpostArn")
      Core.<*> (x Core..@? "startTime")
      Core.<*> (x Core..@? "volumeId")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "snapshotId")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "progress")
      Core.<*> (x Core..@? "volumeSize")

instance Core.Hashable SnapshotInfo

instance Core.NFData SnapshotInfo
