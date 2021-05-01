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
-- Module      : Network.AWS.EC2.Types.SnapshotInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SnapshotState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a snapshot.
--
-- /See:/ 'newSnapshotInfo' smart constructor.
data SnapshotInfo = SnapshotInfo'
  { -- | Account id used when creating this snapshot.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the snapshot is encrypted.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the AWS Outpost on which the snapshot is stored. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html EBS Local Snapshot on Outposts>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | Time this snapshot was started. This is the same for all snapshots
    -- initiated by the same request.
    startTime :: Prelude.Maybe Prelude.ISO8601,
    -- | Source volume from which this snapshot was created.
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | Current state of the snapshot.
    state :: Prelude.Maybe SnapshotState,
    -- | Snapshot id that can be used to describe this snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | Tags associated with this snapshot.
    tags :: Prelude.Maybe [Tag],
    -- | Description specified by the CreateSnapshotRequest that has been applied
    -- to all snapshots.
    description :: Prelude.Maybe Prelude.Text,
    -- | Progress this snapshot has made towards completing.
    progress :: Prelude.Maybe Prelude.Text,
    -- | Size of the volume from which this snapshot was created.
    volumeSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { ownerId = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      startTime = Prelude.Nothing,
      volumeId = Prelude.Nothing,
      state = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      progress = Prelude.Nothing,
      volumeSize = Prelude.Nothing
    }

-- | Account id used when creating this snapshot.
snapshotInfo_ownerId :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Text)
snapshotInfo_ownerId = Lens.lens (\SnapshotInfo' {ownerId} -> ownerId) (\s@SnapshotInfo' {} a -> s {ownerId = a} :: SnapshotInfo)

-- | Indicates whether the snapshot is encrypted.
snapshotInfo_encrypted :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Bool)
snapshotInfo_encrypted = Lens.lens (\SnapshotInfo' {encrypted} -> encrypted) (\s@SnapshotInfo' {} a -> s {encrypted = a} :: SnapshotInfo)

-- | The ARN of the AWS Outpost on which the snapshot is stored. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html EBS Local Snapshot on Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
snapshotInfo_outpostArn :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Text)
snapshotInfo_outpostArn = Lens.lens (\SnapshotInfo' {outpostArn} -> outpostArn) (\s@SnapshotInfo' {} a -> s {outpostArn = a} :: SnapshotInfo)

-- | Time this snapshot was started. This is the same for all snapshots
-- initiated by the same request.
snapshotInfo_startTime :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.UTCTime)
snapshotInfo_startTime = Lens.lens (\SnapshotInfo' {startTime} -> startTime) (\s@SnapshotInfo' {} a -> s {startTime = a} :: SnapshotInfo) Prelude.. Lens.mapping Prelude._Time

-- | Source volume from which this snapshot was created.
snapshotInfo_volumeId :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Text)
snapshotInfo_volumeId = Lens.lens (\SnapshotInfo' {volumeId} -> volumeId) (\s@SnapshotInfo' {} a -> s {volumeId = a} :: SnapshotInfo)

-- | Current state of the snapshot.
snapshotInfo_state :: Lens.Lens' SnapshotInfo (Prelude.Maybe SnapshotState)
snapshotInfo_state = Lens.lens (\SnapshotInfo' {state} -> state) (\s@SnapshotInfo' {} a -> s {state = a} :: SnapshotInfo)

-- | Snapshot id that can be used to describe this snapshot.
snapshotInfo_snapshotId :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Text)
snapshotInfo_snapshotId = Lens.lens (\SnapshotInfo' {snapshotId} -> snapshotId) (\s@SnapshotInfo' {} a -> s {snapshotId = a} :: SnapshotInfo)

-- | Tags associated with this snapshot.
snapshotInfo_tags :: Lens.Lens' SnapshotInfo (Prelude.Maybe [Tag])
snapshotInfo_tags = Lens.lens (\SnapshotInfo' {tags} -> tags) (\s@SnapshotInfo' {} a -> s {tags = a} :: SnapshotInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | Description specified by the CreateSnapshotRequest that has been applied
-- to all snapshots.
snapshotInfo_description :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Text)
snapshotInfo_description = Lens.lens (\SnapshotInfo' {description} -> description) (\s@SnapshotInfo' {} a -> s {description = a} :: SnapshotInfo)

-- | Progress this snapshot has made towards completing.
snapshotInfo_progress :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Text)
snapshotInfo_progress = Lens.lens (\SnapshotInfo' {progress} -> progress) (\s@SnapshotInfo' {} a -> s {progress = a} :: SnapshotInfo)

-- | Size of the volume from which this snapshot was created.
snapshotInfo_volumeSize :: Lens.Lens' SnapshotInfo (Prelude.Maybe Prelude.Int)
snapshotInfo_volumeSize = Lens.lens (\SnapshotInfo' {volumeSize} -> volumeSize) (\s@SnapshotInfo' {} a -> s {volumeSize = a} :: SnapshotInfo)

instance Prelude.FromXML SnapshotInfo where
  parseXML x =
    SnapshotInfo'
      Prelude.<$> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "encrypted")
      Prelude.<*> (x Prelude..@? "outpostArn")
      Prelude.<*> (x Prelude..@? "startTime")
      Prelude.<*> (x Prelude..@? "volumeId")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "snapshotId")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "description")
      Prelude.<*> (x Prelude..@? "progress")
      Prelude.<*> (x Prelude..@? "volumeSize")

instance Prelude.Hashable SnapshotInfo

instance Prelude.NFData SnapshotInfo
