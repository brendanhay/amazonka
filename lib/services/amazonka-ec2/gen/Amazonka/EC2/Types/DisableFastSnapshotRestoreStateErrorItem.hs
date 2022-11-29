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
-- Module      : Amazonka.EC2.Types.DisableFastSnapshotRestoreStateErrorItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DisableFastSnapshotRestoreStateErrorItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DisableFastSnapshotRestoreStateError
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an error that occurred when disabling fast
-- snapshot restores.
--
-- /See:/ 'newDisableFastSnapshotRestoreStateErrorItem' smart constructor.
data DisableFastSnapshotRestoreStateErrorItem = DisableFastSnapshotRestoreStateErrorItem'
  { -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The error.
    error :: Prelude.Maybe DisableFastSnapshotRestoreStateError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableFastSnapshotRestoreStateErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'disableFastSnapshotRestoreStateErrorItem_availabilityZone' - The Availability Zone.
--
-- 'error', 'disableFastSnapshotRestoreStateErrorItem_error' - The error.
newDisableFastSnapshotRestoreStateErrorItem ::
  DisableFastSnapshotRestoreStateErrorItem
newDisableFastSnapshotRestoreStateErrorItem =
  DisableFastSnapshotRestoreStateErrorItem'
    { availabilityZone =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The Availability Zone.
disableFastSnapshotRestoreStateErrorItem_availabilityZone :: Lens.Lens' DisableFastSnapshotRestoreStateErrorItem (Prelude.Maybe Prelude.Text)
disableFastSnapshotRestoreStateErrorItem_availabilityZone = Lens.lens (\DisableFastSnapshotRestoreStateErrorItem' {availabilityZone} -> availabilityZone) (\s@DisableFastSnapshotRestoreStateErrorItem' {} a -> s {availabilityZone = a} :: DisableFastSnapshotRestoreStateErrorItem)

-- | The error.
disableFastSnapshotRestoreStateErrorItem_error :: Lens.Lens' DisableFastSnapshotRestoreStateErrorItem (Prelude.Maybe DisableFastSnapshotRestoreStateError)
disableFastSnapshotRestoreStateErrorItem_error = Lens.lens (\DisableFastSnapshotRestoreStateErrorItem' {error} -> error) (\s@DisableFastSnapshotRestoreStateErrorItem' {} a -> s {error = a} :: DisableFastSnapshotRestoreStateErrorItem)

instance
  Core.FromXML
    DisableFastSnapshotRestoreStateErrorItem
  where
  parseXML x =
    DisableFastSnapshotRestoreStateErrorItem'
      Prelude.<$> (x Core..@? "availabilityZone")
        Prelude.<*> (x Core..@? "error")

instance
  Prelude.Hashable
    DisableFastSnapshotRestoreStateErrorItem
  where
  hashWithSalt
    _salt
    DisableFastSnapshotRestoreStateErrorItem' {..} =
      _salt `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` error

instance
  Prelude.NFData
    DisableFastSnapshotRestoreStateErrorItem
  where
  rnf DisableFastSnapshotRestoreStateErrorItem' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf error
