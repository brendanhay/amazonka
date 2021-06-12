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
-- Module      : Network.AWS.EC2.Types.DisableFastSnapshotRestoreErrorItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DisableFastSnapshotRestoreErrorItem where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DisableFastSnapshotRestoreStateErrorItem
import qualified Network.AWS.Lens as Lens

-- | Contains information about the errors that occurred when disabling fast
-- snapshot restores.
--
-- /See:/ 'newDisableFastSnapshotRestoreErrorItem' smart constructor.
data DisableFastSnapshotRestoreErrorItem = DisableFastSnapshotRestoreErrorItem'
  { -- | The errors.
    fastSnapshotRestoreStateErrors :: Core.Maybe [DisableFastSnapshotRestoreStateErrorItem],
    -- | The ID of the snapshot.
    snapshotId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableFastSnapshotRestoreErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fastSnapshotRestoreStateErrors', 'disableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors' - The errors.
--
-- 'snapshotId', 'disableFastSnapshotRestoreErrorItem_snapshotId' - The ID of the snapshot.
newDisableFastSnapshotRestoreErrorItem ::
  DisableFastSnapshotRestoreErrorItem
newDisableFastSnapshotRestoreErrorItem =
  DisableFastSnapshotRestoreErrorItem'
    { fastSnapshotRestoreStateErrors =
        Core.Nothing,
      snapshotId = Core.Nothing
    }

-- | The errors.
disableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors :: Lens.Lens' DisableFastSnapshotRestoreErrorItem (Core.Maybe [DisableFastSnapshotRestoreStateErrorItem])
disableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors = Lens.lens (\DisableFastSnapshotRestoreErrorItem' {fastSnapshotRestoreStateErrors} -> fastSnapshotRestoreStateErrors) (\s@DisableFastSnapshotRestoreErrorItem' {} a -> s {fastSnapshotRestoreStateErrors = a} :: DisableFastSnapshotRestoreErrorItem) Core.. Lens.mapping Lens._Coerce

-- | The ID of the snapshot.
disableFastSnapshotRestoreErrorItem_snapshotId :: Lens.Lens' DisableFastSnapshotRestoreErrorItem (Core.Maybe Core.Text)
disableFastSnapshotRestoreErrorItem_snapshotId = Lens.lens (\DisableFastSnapshotRestoreErrorItem' {snapshotId} -> snapshotId) (\s@DisableFastSnapshotRestoreErrorItem' {} a -> s {snapshotId = a} :: DisableFastSnapshotRestoreErrorItem)

instance
  Core.FromXML
    DisableFastSnapshotRestoreErrorItem
  where
  parseXML x =
    DisableFastSnapshotRestoreErrorItem'
      Core.<$> ( x Core..@? "fastSnapshotRestoreStateErrorSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "snapshotId")

instance
  Core.Hashable
    DisableFastSnapshotRestoreErrorItem

instance
  Core.NFData
    DisableFastSnapshotRestoreErrorItem
