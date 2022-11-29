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
-- Module      : Amazonka.EC2.Types.SnapshotRecycleBinInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SnapshotRecycleBinInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about a snapshot that is currently in the Recycle Bin.
--
-- /See:/ 'newSnapshotRecycleBinInfo' smart constructor.
data SnapshotRecycleBinInfo = SnapshotRecycleBinInfo'
  { -- | The date and time when the snaphsot entered the Recycle Bin.
    recycleBinEnterTime :: Prelude.Maybe Core.ISO8601,
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The description for the snapshot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the snapshot is to be permanently deleted from
    -- the Recycle Bin.
    recycleBinExitTime :: Prelude.Maybe Core.ISO8601,
    -- | The ID of the volume from which the snapshot was created.
    volumeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapshotRecycleBinInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recycleBinEnterTime', 'snapshotRecycleBinInfo_recycleBinEnterTime' - The date and time when the snaphsot entered the Recycle Bin.
--
-- 'snapshotId', 'snapshotRecycleBinInfo_snapshotId' - The ID of the snapshot.
--
-- 'description', 'snapshotRecycleBinInfo_description' - The description for the snapshot.
--
-- 'recycleBinExitTime', 'snapshotRecycleBinInfo_recycleBinExitTime' - The date and time when the snapshot is to be permanently deleted from
-- the Recycle Bin.
--
-- 'volumeId', 'snapshotRecycleBinInfo_volumeId' - The ID of the volume from which the snapshot was created.
newSnapshotRecycleBinInfo ::
  SnapshotRecycleBinInfo
newSnapshotRecycleBinInfo =
  SnapshotRecycleBinInfo'
    { recycleBinEnterTime =
        Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      description = Prelude.Nothing,
      recycleBinExitTime = Prelude.Nothing,
      volumeId = Prelude.Nothing
    }

-- | The date and time when the snaphsot entered the Recycle Bin.
snapshotRecycleBinInfo_recycleBinEnterTime :: Lens.Lens' SnapshotRecycleBinInfo (Prelude.Maybe Prelude.UTCTime)
snapshotRecycleBinInfo_recycleBinEnterTime = Lens.lens (\SnapshotRecycleBinInfo' {recycleBinEnterTime} -> recycleBinEnterTime) (\s@SnapshotRecycleBinInfo' {} a -> s {recycleBinEnterTime = a} :: SnapshotRecycleBinInfo) Prelude.. Lens.mapping Core._Time

-- | The ID of the snapshot.
snapshotRecycleBinInfo_snapshotId :: Lens.Lens' SnapshotRecycleBinInfo (Prelude.Maybe Prelude.Text)
snapshotRecycleBinInfo_snapshotId = Lens.lens (\SnapshotRecycleBinInfo' {snapshotId} -> snapshotId) (\s@SnapshotRecycleBinInfo' {} a -> s {snapshotId = a} :: SnapshotRecycleBinInfo)

-- | The description for the snapshot.
snapshotRecycleBinInfo_description :: Lens.Lens' SnapshotRecycleBinInfo (Prelude.Maybe Prelude.Text)
snapshotRecycleBinInfo_description = Lens.lens (\SnapshotRecycleBinInfo' {description} -> description) (\s@SnapshotRecycleBinInfo' {} a -> s {description = a} :: SnapshotRecycleBinInfo)

-- | The date and time when the snapshot is to be permanently deleted from
-- the Recycle Bin.
snapshotRecycleBinInfo_recycleBinExitTime :: Lens.Lens' SnapshotRecycleBinInfo (Prelude.Maybe Prelude.UTCTime)
snapshotRecycleBinInfo_recycleBinExitTime = Lens.lens (\SnapshotRecycleBinInfo' {recycleBinExitTime} -> recycleBinExitTime) (\s@SnapshotRecycleBinInfo' {} a -> s {recycleBinExitTime = a} :: SnapshotRecycleBinInfo) Prelude.. Lens.mapping Core._Time

-- | The ID of the volume from which the snapshot was created.
snapshotRecycleBinInfo_volumeId :: Lens.Lens' SnapshotRecycleBinInfo (Prelude.Maybe Prelude.Text)
snapshotRecycleBinInfo_volumeId = Lens.lens (\SnapshotRecycleBinInfo' {volumeId} -> volumeId) (\s@SnapshotRecycleBinInfo' {} a -> s {volumeId = a} :: SnapshotRecycleBinInfo)

instance Core.FromXML SnapshotRecycleBinInfo where
  parseXML x =
    SnapshotRecycleBinInfo'
      Prelude.<$> (x Core..@? "recycleBinEnterTime")
      Prelude.<*> (x Core..@? "snapshotId")
      Prelude.<*> (x Core..@? "description")
      Prelude.<*> (x Core..@? "recycleBinExitTime")
      Prelude.<*> (x Core..@? "volumeId")

instance Prelude.Hashable SnapshotRecycleBinInfo where
  hashWithSalt _salt SnapshotRecycleBinInfo' {..} =
    _salt `Prelude.hashWithSalt` recycleBinEnterTime
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` recycleBinExitTime
      `Prelude.hashWithSalt` volumeId

instance Prelude.NFData SnapshotRecycleBinInfo where
  rnf SnapshotRecycleBinInfo' {..} =
    Prelude.rnf recycleBinEnterTime
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf recycleBinExitTime
      `Prelude.seq` Prelude.rnf volumeId
