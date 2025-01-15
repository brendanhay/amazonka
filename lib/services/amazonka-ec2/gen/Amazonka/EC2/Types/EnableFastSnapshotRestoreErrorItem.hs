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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | The errors.
    fastSnapshotRestoreStateErrors :: Prelude.Maybe [EnableFastSnapshotRestoreStateErrorItem],
    -- | The ID of the snapshot.
    snapshotId :: Prelude.Maybe Prelude.Text
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
-- 'fastSnapshotRestoreStateErrors', 'enableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors' - The errors.
--
-- 'snapshotId', 'enableFastSnapshotRestoreErrorItem_snapshotId' - The ID of the snapshot.
newEnableFastSnapshotRestoreErrorItem ::
  EnableFastSnapshotRestoreErrorItem
newEnableFastSnapshotRestoreErrorItem =
  EnableFastSnapshotRestoreErrorItem'
    { fastSnapshotRestoreStateErrors =
        Prelude.Nothing,
      snapshotId = Prelude.Nothing
    }

-- | The errors.
enableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors :: Lens.Lens' EnableFastSnapshotRestoreErrorItem (Prelude.Maybe [EnableFastSnapshotRestoreStateErrorItem])
enableFastSnapshotRestoreErrorItem_fastSnapshotRestoreStateErrors = Lens.lens (\EnableFastSnapshotRestoreErrorItem' {fastSnapshotRestoreStateErrors} -> fastSnapshotRestoreStateErrors) (\s@EnableFastSnapshotRestoreErrorItem' {} a -> s {fastSnapshotRestoreStateErrors = a} :: EnableFastSnapshotRestoreErrorItem) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the snapshot.
enableFastSnapshotRestoreErrorItem_snapshotId :: Lens.Lens' EnableFastSnapshotRestoreErrorItem (Prelude.Maybe Prelude.Text)
enableFastSnapshotRestoreErrorItem_snapshotId = Lens.lens (\EnableFastSnapshotRestoreErrorItem' {snapshotId} -> snapshotId) (\s@EnableFastSnapshotRestoreErrorItem' {} a -> s {snapshotId = a} :: EnableFastSnapshotRestoreErrorItem)

instance
  Data.FromXML
    EnableFastSnapshotRestoreErrorItem
  where
  parseXML x =
    EnableFastSnapshotRestoreErrorItem'
      Prelude.<$> ( x
                      Data..@? "fastSnapshotRestoreStateErrorSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "snapshotId")

instance
  Prelude.Hashable
    EnableFastSnapshotRestoreErrorItem
  where
  hashWithSalt
    _salt
    EnableFastSnapshotRestoreErrorItem' {..} =
      _salt
        `Prelude.hashWithSalt` fastSnapshotRestoreStateErrors
        `Prelude.hashWithSalt` snapshotId

instance
  Prelude.NFData
    EnableFastSnapshotRestoreErrorItem
  where
  rnf EnableFastSnapshotRestoreErrorItem' {..} =
    Prelude.rnf fastSnapshotRestoreStateErrors `Prelude.seq`
      Prelude.rnf snapshotId
