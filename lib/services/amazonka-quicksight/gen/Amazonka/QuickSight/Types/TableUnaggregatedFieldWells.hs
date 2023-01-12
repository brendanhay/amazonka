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
-- Module      : Amazonka.QuickSight.Types.TableUnaggregatedFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableUnaggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.UnaggregatedField

-- | The unaggregated field well for the table.
--
-- /See:/ 'newTableUnaggregatedFieldWells' smart constructor.
data TableUnaggregatedFieldWells = TableUnaggregatedFieldWells'
  { -- | The values field well for a pivot table. Values are unaggregated for an
    -- unaggregated table.
    values :: Prelude.Maybe [UnaggregatedField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableUnaggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'tableUnaggregatedFieldWells_values' - The values field well for a pivot table. Values are unaggregated for an
-- unaggregated table.
newTableUnaggregatedFieldWells ::
  TableUnaggregatedFieldWells
newTableUnaggregatedFieldWells =
  TableUnaggregatedFieldWells'
    { values =
        Prelude.Nothing
    }

-- | The values field well for a pivot table. Values are unaggregated for an
-- unaggregated table.
tableUnaggregatedFieldWells_values :: Lens.Lens' TableUnaggregatedFieldWells (Prelude.Maybe [UnaggregatedField])
tableUnaggregatedFieldWells_values = Lens.lens (\TableUnaggregatedFieldWells' {values} -> values) (\s@TableUnaggregatedFieldWells' {} a -> s {values = a} :: TableUnaggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TableUnaggregatedFieldWells where
  parseJSON =
    Data.withObject
      "TableUnaggregatedFieldWells"
      ( \x ->
          TableUnaggregatedFieldWells'
            Prelude.<$> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TableUnaggregatedFieldWells where
  hashWithSalt _salt TableUnaggregatedFieldWells' {..} =
    _salt `Prelude.hashWithSalt` values

instance Prelude.NFData TableUnaggregatedFieldWells where
  rnf TableUnaggregatedFieldWells' {..} =
    Prelude.rnf values

instance Data.ToJSON TableUnaggregatedFieldWells where
  toJSON TableUnaggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Values" Data..=) Prelude.<$> values]
      )
