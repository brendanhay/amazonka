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
-- Module      : Amazonka.EC2.Types.EnableFastSnapshotRestoreErrorItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.EnableFastSnapshotRestoreErrorItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.EnableFastSnapshotRestoreStateErrorItem
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the errors that occurred when enabling fast
-- snapshot restores.
--
-- /See:/ 'newEnableFastSnapshotRestoreErrorItem' smart constructor.
data EnableFastSnapshotRestoreErrorItem = EnableFastSnapshotRestoreErrorItem'
  { -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The errors.
    fastSnapshotRestoreStateErrors :: Prelude.Maybe [EnableFastSnapshotRestoreStateErrorItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableFastSnapshotRestoreErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotId', 'enableFastSnapshotRestoreErrorItem_snapshotId' - The ID of the snapshot.
--
-- 'fastSnapshotRestoreStateErrors', 'enableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors' - The errors.
newEnableFastSnapshotRestoreErrorItem ::
  EnableFastSnapshotRestoreErrorItem
newEnableFastSnapshotRestoreErrorItem =
  EnableFastSnapshotRestoreErrorItem'
    { snapshotId =
        Prelude.Nothing,
      fastSnapshotRestoreStateErrors =
        Prelude.Nothing
    }

-- | The ID of the snapshot.
enableFastSnapshotRestoreErrorItem_snapshotId :: Lens.Lens' EnableFastSnapshotRestoreErrorItem (Prelude.Maybe Prelude.Text)
enableFastSnapshotRestoreErrorItem_snapshotId = Lens.lens (\EnableFastSnapshotRestoreErrorItem' {snapshotId} -> snapshotId) (\s@EnableFastSnapshotRestoreErrorItem' {} a -> s {snapshotId = a} :: EnableFastSnapshotRestoreErrorItem)

-- | The errors.
enableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors :: Lens.Lens' EnableFastSnapshotRestoreErrorItem (Prelude.Maybe [EnableFastSnapshotRestoreStateErrorItem])
enableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors = Lens.lens (\EnableFastSnapshotRestoreErrorItem' {fastSnapshotRestoreStateErrors} -> fastSnapshotRestoreStateErrors) (\s@EnableFastSnapshotRestoreErrorItem' {} a -> s {fastSnapshotRestoreStateErrors = a} :: EnableFastSnapshotRestoreErrorItem) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromXML
    EnableFastSnapshotRestoreErrorItem
  where
  parseXML x =
    EnableFastSnapshotRestoreErrorItem'
      Prelude.<$> (x Data..@? "snapshotId")
      Prelude.<*> ( x Data..@? "fastSnapshotRestoreStateErrorSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance
  Prelude.Hashable
    EnableFastSnapshotRestoreErrorItem
  where
  hashWithSalt
    _salt
    EnableFastSnapshotRestoreErrorItem' {..} =
      _salt `Prelude.hashWithSalt` snapshotId
        `Prelude.hashWithSalt` fastSnapshotRestoreStateErrors

instance
  Prelude.NFData
    EnableFastSnapshotRestoreErrorItem
  where
  rnf EnableFastSnapshotRestoreErrorItem' {..} =
    Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf fastSnapshotRestoreStateErrors
