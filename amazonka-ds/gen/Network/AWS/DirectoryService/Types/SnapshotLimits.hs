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
-- Module      : Network.AWS.DirectoryService.Types.SnapshotLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.SnapshotLimits where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains manual snapshot limit information for a directory.
--
-- /See:/ 'newSnapshotLimits' smart constructor.
data SnapshotLimits = SnapshotLimits'
  { -- | The current number of manual snapshots of the directory.
    manualSnapshotsCurrentCount :: Core.Maybe Core.Natural,
    -- | Indicates if the manual snapshot limit has been reached.
    manualSnapshotsLimitReached :: Core.Maybe Core.Bool,
    -- | The maximum number of manual snapshots allowed.
    manualSnapshotsLimit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SnapshotLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manualSnapshotsCurrentCount', 'snapshotLimits_manualSnapshotsCurrentCount' - The current number of manual snapshots of the directory.
--
-- 'manualSnapshotsLimitReached', 'snapshotLimits_manualSnapshotsLimitReached' - Indicates if the manual snapshot limit has been reached.
--
-- 'manualSnapshotsLimit', 'snapshotLimits_manualSnapshotsLimit' - The maximum number of manual snapshots allowed.
newSnapshotLimits ::
  SnapshotLimits
newSnapshotLimits =
  SnapshotLimits'
    { manualSnapshotsCurrentCount =
        Core.Nothing,
      manualSnapshotsLimitReached = Core.Nothing,
      manualSnapshotsLimit = Core.Nothing
    }

-- | The current number of manual snapshots of the directory.
snapshotLimits_manualSnapshotsCurrentCount :: Lens.Lens' SnapshotLimits (Core.Maybe Core.Natural)
snapshotLimits_manualSnapshotsCurrentCount = Lens.lens (\SnapshotLimits' {manualSnapshotsCurrentCount} -> manualSnapshotsCurrentCount) (\s@SnapshotLimits' {} a -> s {manualSnapshotsCurrentCount = a} :: SnapshotLimits)

-- | Indicates if the manual snapshot limit has been reached.
snapshotLimits_manualSnapshotsLimitReached :: Lens.Lens' SnapshotLimits (Core.Maybe Core.Bool)
snapshotLimits_manualSnapshotsLimitReached = Lens.lens (\SnapshotLimits' {manualSnapshotsLimitReached} -> manualSnapshotsLimitReached) (\s@SnapshotLimits' {} a -> s {manualSnapshotsLimitReached = a} :: SnapshotLimits)

-- | The maximum number of manual snapshots allowed.
snapshotLimits_manualSnapshotsLimit :: Lens.Lens' SnapshotLimits (Core.Maybe Core.Natural)
snapshotLimits_manualSnapshotsLimit = Lens.lens (\SnapshotLimits' {manualSnapshotsLimit} -> manualSnapshotsLimit) (\s@SnapshotLimits' {} a -> s {manualSnapshotsLimit = a} :: SnapshotLimits)

instance Core.FromJSON SnapshotLimits where
  parseJSON =
    Core.withObject
      "SnapshotLimits"
      ( \x ->
          SnapshotLimits'
            Core.<$> (x Core..:? "ManualSnapshotsCurrentCount")
            Core.<*> (x Core..:? "ManualSnapshotsLimitReached")
            Core.<*> (x Core..:? "ManualSnapshotsLimit")
      )

instance Core.Hashable SnapshotLimits

instance Core.NFData SnapshotLimits
