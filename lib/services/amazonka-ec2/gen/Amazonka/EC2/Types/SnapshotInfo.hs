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
-- Module      : Amazonka.EC2.Types.SnapshotInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SnapshotInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.SnapshotState
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Information about a snapshot.
--
-- /See:/ 'newSnapshotInfo' smart constructor.
data SnapshotInfo = SnapshotInfo'
  { -- | Tags associated with this snapshot.
    tags :: Prelude.Maybe [Tag],
    -- | Progress this snapshot has made towards completing.
    progress :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Outpost on which the snapshot is stored. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html Amazon EBS local snapshots on Outposts>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | Account id used when creating this snapshot.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Snapshot id that can be used to describe this snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | Current state of the snapshot.
    state :: Prelude.Maybe SnapshotState,
    -- | Size of the volume from which this snapshot was created.
    volumeSize :: Prelude.Maybe Prelude.Int,
    -- | Description specified by the CreateSnapshotRequest that has been applied
    -- to all snapshots.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the snapshot is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | Source volume from which this snapshot was created.
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | Time this snapshot was started. This is the same for all snapshots
    -- initiated by the same request.
    startTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapshotInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'snapshotInfo_tags' - Tags associated with this snapshot.
--
-- 'progress', 'snapshotInfo_progress' - Progress this snapshot has made towards completing.
--
-- 'outpostArn', 'snapshotInfo_outpostArn' - The ARN of the Outpost on which the snapshot is stored. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html Amazon EBS local snapshots on Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'ownerId', 'snapshotInfo_ownerId' - Account id used when creating this snapshot.
--
-- 'snapshotId', 'snapshotInfo_snapshotId' - Snapshot id that can be used to describe this snapshot.
--
-- 'state', 'snapshotInfo_state' - Current state of the snapshot.
--
-- 'volumeSize', 'snapshotInfo_volumeSize' - Size of the volume from which this snapshot was created.
--
-- 'description', 'snapshotInfo_description' - Description specified by the CreateSnapshotRequest that has been applied
-- to all snapshots.
--
-- 'encrypted', 'snapshotInfo_encrypted' - Indicates whether the snapshot is encrypted.
--
-- 'volumeId', 'snapshotInfo_volumeId' - Source volume from which this snapshot was created.
--
-- 'startTime', 'snapshotInfo_startTime' - Time this snapshot was started. This is the same for all snapshots
-- initiated by the same request.
newSnapshotInfo ::
  SnapshotInfo
newSnapshotInfo =
  SnapshotInfo'
    { tags = Prelude.Nothing,
      progress = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      state = Prelude.Nothing,
      volumeSize = Prelude.Nothing,
      description = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      volumeId = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | Tags associated with this snapshot.
snapshotInfo_tags :: Lens.Lens' SnapshotInfo (Prelude.Maybe [Tag])
snapshotInfo_tags = Lens.lens (\SnapshotInfo' {tags} -> tags) (\s@SnapshotInfo' {} a -> s {tags = a} :: SnapshotInfo) Prelude.. Lens.mapping Lens.coerced

-- | Progress this snapshot has made towards completing.
snapshotInfo_progress :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Text)
snapshotInfo_progress = Lens.lens (\SnapshotInfo' {progress} -> progress) (\s@SnapshotInfo' {} a -> s {progress = a} :: SnapshotInfo)

-- | The ARN of the Outpost on which the snapshot is stored. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html Amazon EBS local snapshots on Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
snapshotInfo_outpostArn :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Text)
snapshotInfo_outpostArn = Lens.lens (\SnapshotInfo' {outpostArn} -> outpostArn) (\s@SnapshotInfo' {} a -> s {outpostArn = a} :: SnapshotInfo)

-- | Account id used when creating this snapshot.
snapshotInfo_ownerId :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Text)
snapshotInfo_ownerId = Lens.lens (\SnapshotInfo' {ownerId} -> ownerId) (\s@SnapshotInfo' {} a -> s {ownerId = a} :: SnapshotInfo)

-- | Snapshot id that can be used to describe this snapshot.
snapshotInfo_snapshotId :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Text)
snapshotInfo_snapshotId = Lens.lens (\SnapshotInfo' {snapshotId} -> snapshotId) (\s@SnapshotInfo' {} a -> s {snapshotId = a} :: SnapshotInfo)

-- | Current state of the snapshot.
snapshotInfo_state :: Lens.Lens' SnapshotInfo (Prelude.Maybe SnapshotState)
snapshotInfo_state = Lens.lens (\SnapshotInfo' {state} -> state) (\s@SnapshotInfo' {} a -> s {state = a} :: SnapshotInfo)

-- | Size of the volume from which this snapshot was created.
snapshotInfo_volumeSize :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Int)
snapshotInfo_volumeSize = Lens.lens (\SnapshotInfo' {volumeSize} -> volumeSize) (\s@SnapshotInfo' {} a -> s {volumeSize = a} :: SnapshotInfo)

-- | Description specified by the CreateSnapshotRequest that has been applied
-- to all snapshots.
snapshotInfo_description :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Text)
snapshotInfo_description = Lens.lens (\SnapshotInfo' {description} -> description) (\s@SnapshotInfo' {} a -> s {description = a} :: SnapshotInfo)

-- | Indicates whether the snapshot is encrypted.
snapshotInfo_encrypted :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Bool)
snapshotInfo_encrypted = Lens.lens (\SnapshotInfo' {encrypted} -> encrypted) (\s@SnapshotInfo' {} a -> s {encrypted = a} :: SnapshotInfo)

-- | Source volume from which this snapshot was created.
snapshotInfo_volumeId :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Text)
snapshotInfo_volumeId = Lens.lens (\SnapshotInfo' {volumeId} -> volumeId) (\s@SnapshotInfo' {} a -> s {volumeId = a} :: SnapshotInfo)

-- | Time this snapshot was started. This is the same for all snapshots
-- initiated by the same request.
snapshotInfo_startTime :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.UTCTime)
snapshotInfo_startTime = Lens.lens (\SnapshotInfo' {startTime} -> startTime) (\s@SnapshotInfo' {} a -> s {startTime = a} :: SnapshotInfo) Prelude.. Lens.mapping Data._Time

instance Data.FromXML SnapshotInfo where
  parseXML x =
    SnapshotInfo'
      Prelude.<$> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "progress")
      Prelude.<*> (x Data..@? "outpostArn")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "snapshotId")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "volumeSize")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "encrypted")
      Prelude.<*> (x Data..@? "volumeId")
      Prelude.<*> (x Data..@? "startTime")

instance Prelude.Hashable SnapshotInfo where
  hashWithSalt _salt SnapshotInfo' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` progress
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` volumeSize
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` volumeId
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData SnapshotInfo where
  rnf SnapshotInfo' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf progress
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf volumeSize
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf volumeId
      `Prelude.seq` Prelude.rnf startTime
