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
-- Module      : Amazonka.EC2.Types.DisableFastSnapshotRestoreErrorItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DisableFastSnapshotRestoreErrorItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DisableFastSnapshotRestoreStateErrorItem
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the errors that occurred when disabling fast
-- snapshot restores.
--
-- /See:/ 'newDisableFastSnapshotRestoreErrorItem' smart constructor.
data DisableFastSnapshotRestoreErrorItem = DisableFastSnapshotRestoreErrorItem'
  { -- | The errors.
    fastSnapshotRestoreStateErrors :: Prelude.Maybe [DisableFastSnapshotRestoreStateErrorItem],
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      snapshotId = Prelude.Nothing
    }

-- | The errors.
disableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors :: Lens.Lens' DisableFastSnapshotRestoreErrorItem (Prelude.Maybe [DisableFastSnapshotRestoreStateErrorItem])
disableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors = Lens.lens (\DisableFastSnapshotRestoreErrorItem' {fastSnapshotRestoreStateErrors} -> fastSnapshotRestoreStateErrors) (\s@DisableFastSnapshotRestoreErrorItem' {} a -> s {fastSnapshotRestoreStateErrors = a} :: DisableFastSnapshotRestoreErrorItem) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the snapshot.
disableFastSnapshotRestoreErrorItem_snapshotId :: Lens.Lens' DisableFastSnapshotRestoreErrorItem (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreErrorItem_snapshotId = Lens.lens (\DisableFastSnapshotRestoreErrorItem' {snapshotId} -> snapshotId) (\s@DisableFastSnapshotRestoreErrorItem' {} a -> s {snapshotId = a} :: DisableFastSnapshotRestoreErrorItem)

instance
  Data.FromXML
    DisableFastSnapshotRestoreErrorItem
  where
  parseXML x =
    DisableFastSnapshotRestoreErrorItem'
      Prelude.<$> ( x
                      Data..@? "fastSnapshotRestoreStateErrorSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "snapshotId")

instance
  Prelude.Hashable
    DisableFastSnapshotRestoreErrorItem
  where
  hashWithSalt
    _salt
    DisableFastSnapshotRestoreErrorItem' {..} =
      _salt
        `Prelude.hashWithSalt` fastSnapshotRestoreStateErrors
        `Prelude.hashWithSalt` snapshotId

instance
  Prelude.NFData
    DisableFastSnapshotRestoreErrorItem
  where
  rnf DisableFastSnapshotRestoreErrorItem' {..} =
    Prelude.rnf fastSnapshotRestoreStateErrors
      `Prelude.seq` Prelude.rnf snapshotId
