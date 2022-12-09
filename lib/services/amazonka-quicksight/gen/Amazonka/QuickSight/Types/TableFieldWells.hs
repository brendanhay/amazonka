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
-- Module      : Amazonka.QuickSight.Types.TableFieldWells
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableAggregatedFieldWells
import Amazonka.QuickSight.Types.TableUnaggregatedFieldWells

-- | The field wells for a table visual.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newTableFieldWells' smart constructor.
data TableFieldWells = TableFieldWells'
  { -- | The aggregated field well for the table.
    tableAggregatedFieldWells :: Prelude.Maybe TableAggregatedFieldWells,
    -- | The unaggregated field well for the table.
    tableUnaggregatedFieldWells :: Prelude.Maybe TableUnaggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableAggregatedFieldWells', 'tableFieldWells_tableAggregatedFieldWells' - The aggregated field well for the table.
--
-- 'tableUnaggregatedFieldWells', 'tableFieldWells_tableUnaggregatedFieldWells' - The unaggregated field well for the table.
newTableFieldWells ::
  TableFieldWells
newTableFieldWells =
  TableFieldWells'
    { tableAggregatedFieldWells =
        Prelude.Nothing,
      tableUnaggregatedFieldWells = Prelude.Nothing
    }

-- | The aggregated field well for the table.
tableFieldWells_tableAggregatedFieldWells :: Lens.Lens' TableFieldWells (Prelude.Maybe TableAggregatedFieldWells)
tableFieldWells_tableAggregatedFieldWells = Lens.lens (\TableFieldWells' {tableAggregatedFieldWells} -> tableAggregatedFieldWells) (\s@TableFieldWells' {} a -> s {tableAggregatedFieldWells = a} :: TableFieldWells)

-- | The unaggregated field well for the table.
tableFieldWells_tableUnaggregatedFieldWells :: Lens.Lens' TableFieldWells (Prelude.Maybe TableUnaggregatedFieldWells)
tableFieldWells_tableUnaggregatedFieldWells = Lens.lens (\TableFieldWells' {tableUnaggregatedFieldWells} -> tableUnaggregatedFieldWells) (\s@TableFieldWells' {} a -> s {tableUnaggregatedFieldWells = a} :: TableFieldWells)

instance Data.FromJSON TableFieldWells where
  parseJSON =
    Data.withObject
      "TableFieldWells"
      ( \x ->
          TableFieldWells'
            Prelude.<$> (x Data..:? "TableAggregatedFieldWells")
            Prelude.<*> (x Data..:? "TableUnaggregatedFieldWells")
      )

instance Prelude.Hashable TableFieldWells where
  hashWithSalt _salt TableFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` tableAggregatedFieldWells
      `Prelude.hashWithSalt` tableUnaggregatedFieldWells

instance Prelude.NFData TableFieldWells where
  rnf TableFieldWells' {..} =
    Prelude.rnf tableAggregatedFieldWells
      `Prelude.seq` Prelude.rnf tableUnaggregatedFieldWells

instance Data.ToJSON TableFieldWells where
  toJSON TableFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TableAggregatedFieldWells" Data..=)
              Prelude.<$> tableAggregatedFieldWells,
            ("TableUnaggregatedFieldWells" Data..=)
              Prelude.<$> tableUnaggregatedFieldWells
          ]
      )
