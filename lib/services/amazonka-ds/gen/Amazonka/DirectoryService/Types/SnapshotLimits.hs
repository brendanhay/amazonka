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
-- Module      : Amazonka.DirectoryService.Types.SnapshotLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.SnapshotLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains manual snapshot limit information for a directory.
--
-- /See:/ 'newSnapshotLimits' smart constructor.
data SnapshotLimits = SnapshotLimits'
  { -- | The current number of manual snapshots of the directory.
    manualSnapshotsCurrentCount :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of manual snapshots allowed.
    manualSnapshotsLimit :: Prelude.Maybe Prelude.Natural,
    -- | Indicates if the manual snapshot limit has been reached.
    manualSnapshotsLimitReached :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'manualSnapshotsLimit', 'snapshotLimits_manualSnapshotsLimit' - The maximum number of manual snapshots allowed.
--
-- 'manualSnapshotsLimitReached', 'snapshotLimits_manualSnapshotsLimitReached' - Indicates if the manual snapshot limit has been reached.
newSnapshotLimits ::
  SnapshotLimits
newSnapshotLimits =
  SnapshotLimits'
    { manualSnapshotsCurrentCount =
        Prelude.Nothing,
      manualSnapshotsLimit = Prelude.Nothing,
      manualSnapshotsLimitReached = Prelude.Nothing
    }

-- | The current number of manual snapshots of the directory.
snapshotLimits_manualSnapshotsCurrentCount :: Lens.Lens' SnapshotLimits (Prelude.Maybe Prelude.Natural)
snapshotLimits_manualSnapshotsCurrentCount = Lens.lens (\SnapshotLimits' {manualSnapshotsCurrentCount} -> manualSnapshotsCurrentCount) (\s@SnapshotLimits' {} a -> s {manualSnapshotsCurrentCount = a} :: SnapshotLimits)

-- | The maximum number of manual snapshots allowed.
snapshotLimits_manualSnapshotsLimit :: Lens.Lens' SnapshotLimits (Prelude.Maybe Prelude.Natural)
snapshotLimits_manualSnapshotsLimit = Lens.lens (\SnapshotLimits' {manualSnapshotsLimit} -> manualSnapshotsLimit) (\s@SnapshotLimits' {} a -> s {manualSnapshotsLimit = a} :: SnapshotLimits)

-- | Indicates if the manual snapshot limit has been reached.
snapshotLimits_manualSnapshotsLimitReached :: Lens.Lens' SnapshotLimits (Prelude.Maybe Prelude.Bool)
snapshotLimits_manualSnapshotsLimitReached = Lens.lens (\SnapshotLimits' {manualSnapshotsLimitReached} -> manualSnapshotsLimitReached) (\s@SnapshotLimits' {} a -> s {manualSnapshotsLimitReached = a} :: SnapshotLimits)

instance Data.FromJSON SnapshotLimits where
  parseJSON =
    Data.withObject
      "SnapshotLimits"
      ( \x ->
          SnapshotLimits'
            Prelude.<$> (x Data..:? "ManualSnapshotsCurrentCount")
            Prelude.<*> (x Data..:? "ManualSnapshotsLimit")
            Prelude.<*> (x Data..:? "ManualSnapshotsLimitReached")
      )

instance Prelude.Hashable SnapshotLimits where
  hashWithSalt _salt SnapshotLimits' {..} =
    _salt
      `Prelude.hashWithSalt` manualSnapshotsCurrentCount
      `Prelude.hashWithSalt` manualSnapshotsLimit
      `Prelude.hashWithSalt` manualSnapshotsLimitReached

instance Prelude.NFData SnapshotLimits where
  rnf SnapshotLimits' {..} =
    Prelude.rnf manualSnapshotsCurrentCount
      `Prelude.seq` Prelude.rnf manualSnapshotsLimit
      `Prelude.seq` Prelude.rnf manualSnapshotsLimitReached
