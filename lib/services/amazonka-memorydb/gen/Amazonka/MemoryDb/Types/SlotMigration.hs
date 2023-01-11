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
-- Module      : Amazonka.MemoryDb.Types.SlotMigration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.SlotMigration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the progress of an online resharding operation.
--
-- /See:/ 'newSlotMigration' smart constructor.
data SlotMigration = SlotMigration'
  { -- | The percentage of the slot migration that is complete.
    progressPercentage :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotMigration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'progressPercentage', 'slotMigration_progressPercentage' - The percentage of the slot migration that is complete.
newSlotMigration ::
  SlotMigration
newSlotMigration =
  SlotMigration'
    { progressPercentage =
        Prelude.Nothing
    }

-- | The percentage of the slot migration that is complete.
slotMigration_progressPercentage :: Lens.Lens' SlotMigration (Prelude.Maybe Prelude.Double)
slotMigration_progressPercentage = Lens.lens (\SlotMigration' {progressPercentage} -> progressPercentage) (\s@SlotMigration' {} a -> s {progressPercentage = a} :: SlotMigration)

instance Data.FromJSON SlotMigration where
  parseJSON =
    Data.withObject
      "SlotMigration"
      ( \x ->
          SlotMigration'
            Prelude.<$> (x Data..:? "ProgressPercentage")
      )

instance Prelude.Hashable SlotMigration where
  hashWithSalt _salt SlotMigration' {..} =
    _salt `Prelude.hashWithSalt` progressPercentage

instance Prelude.NFData SlotMigration where
  rnf SlotMigration' {..} =
    Prelude.rnf progressPercentage
