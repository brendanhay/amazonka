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
-- Module      : Amazonka.EC2.Types.EnableFastSnapshotRestoreStateErrorItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.EnableFastSnapshotRestoreStateErrorItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.EnableFastSnapshotRestoreStateError
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an error that occurred when enabling fast
-- snapshot restores.
--
-- /See:/ 'newEnableFastSnapshotRestoreStateErrorItem' smart constructor.
data EnableFastSnapshotRestoreStateErrorItem = EnableFastSnapshotRestoreStateErrorItem'
  { -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The error.
    error :: Prelude.Maybe EnableFastSnapshotRestoreStateError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableFastSnapshotRestoreStateErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'enableFastSnapshotRestoreStateErrorItem_availabilityZone' - The Availability Zone.
--
-- 'error', 'enableFastSnapshotRestoreStateErrorItem_error' - The error.
newEnableFastSnapshotRestoreStateErrorItem ::
  EnableFastSnapshotRestoreStateErrorItem
newEnableFastSnapshotRestoreStateErrorItem =
  EnableFastSnapshotRestoreStateErrorItem'
    { availabilityZone =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The Availability Zone.
enableFastSnapshotRestoreStateErrorItem_availabilityZone :: Lens.Lens' EnableFastSnapshotRestoreStateErrorItem (Prelude.Maybe Prelude.Text)
enableFastSnapshotRestoreStateErrorItem_availabilityZone = Lens.lens (\EnableFastSnapshotRestoreStateErrorItem' {availabilityZone} -> availabilityZone) (\s@EnableFastSnapshotRestoreStateErrorItem' {} a -> s {availabilityZone = a} :: EnableFastSnapshotRestoreStateErrorItem)

-- | The error.
enableFastSnapshotRestoreStateErrorItem_error :: Lens.Lens' EnableFastSnapshotRestoreStateErrorItem (Prelude.Maybe EnableFastSnapshotRestoreStateError)
enableFastSnapshotRestoreStateErrorItem_error = Lens.lens (\EnableFastSnapshotRestoreStateErrorItem' {error} -> error) (\s@EnableFastSnapshotRestoreStateErrorItem' {} a -> s {error = a} :: EnableFastSnapshotRestoreStateErrorItem)

instance
  Data.FromXML
    EnableFastSnapshotRestoreStateErrorItem
  where
  parseXML x =
    EnableFastSnapshotRestoreStateErrorItem'
      Prelude.<$> (x Data..@? "availabilityZone")
        Prelude.<*> (x Data..@? "error")

instance
  Prelude.Hashable
    EnableFastSnapshotRestoreStateErrorItem
  where
  hashWithSalt
    _salt
    EnableFastSnapshotRestoreStateErrorItem' {..} =
      _salt `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` error

instance
  Prelude.NFData
    EnableFastSnapshotRestoreStateErrorItem
  where
  rnf EnableFastSnapshotRestoreStateErrorItem' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf error
