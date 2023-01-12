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
-- Module      : Amazonka.QuickSight.Types.SmallMultiplesOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SmallMultiplesOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PanelConfiguration

-- | Options that determine the layout and display options of a chart\'s
-- small multiples.
--
-- /See:/ 'newSmallMultiplesOptions' smart constructor.
data SmallMultiplesOptions = SmallMultiplesOptions'
  { -- | Sets the maximum number of visible columns to display in the grid of
    -- small multiples panels.
    --
    -- The default is @Auto@, which automatically adjusts the columns in the
    -- grid to fit the overall layout and size of the given chart.
    maxVisibleColumns :: Prelude.Maybe Prelude.Natural,
    -- | Sets the maximum number of visible rows to display in the grid of small
    -- multiples panels.
    --
    -- The default value is @Auto@, which automatically adjusts the rows in the
    -- grid to fit the overall layout and size of the given chart.
    maxVisibleRows :: Prelude.Maybe Prelude.Natural,
    -- | Configures the display options for each small multiples panel.
    panelConfiguration :: Prelude.Maybe PanelConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SmallMultiplesOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxVisibleColumns', 'smallMultiplesOptions_maxVisibleColumns' - Sets the maximum number of visible columns to display in the grid of
-- small multiples panels.
--
-- The default is @Auto@, which automatically adjusts the columns in the
-- grid to fit the overall layout and size of the given chart.
--
-- 'maxVisibleRows', 'smallMultiplesOptions_maxVisibleRows' - Sets the maximum number of visible rows to display in the grid of small
-- multiples panels.
--
-- The default value is @Auto@, which automatically adjusts the rows in the
-- grid to fit the overall layout and size of the given chart.
--
-- 'panelConfiguration', 'smallMultiplesOptions_panelConfiguration' - Configures the display options for each small multiples panel.
newSmallMultiplesOptions ::
  SmallMultiplesOptions
newSmallMultiplesOptions =
  SmallMultiplesOptions'
    { maxVisibleColumns =
        Prelude.Nothing,
      maxVisibleRows = Prelude.Nothing,
      panelConfiguration = Prelude.Nothing
    }

-- | Sets the maximum number of visible columns to display in the grid of
-- small multiples panels.
--
-- The default is @Auto@, which automatically adjusts the columns in the
-- grid to fit the overall layout and size of the given chart.
smallMultiplesOptions_maxVisibleColumns :: Lens.Lens' SmallMultiplesOptions (Prelude.Maybe Prelude.Natural)
smallMultiplesOptions_maxVisibleColumns = Lens.lens (\SmallMultiplesOptions' {maxVisibleColumns} -> maxVisibleColumns) (\s@SmallMultiplesOptions' {} a -> s {maxVisibleColumns = a} :: SmallMultiplesOptions)

-- | Sets the maximum number of visible rows to display in the grid of small
-- multiples panels.
--
-- The default value is @Auto@, which automatically adjusts the rows in the
-- grid to fit the overall layout and size of the given chart.
smallMultiplesOptions_maxVisibleRows :: Lens.Lens' SmallMultiplesOptions (Prelude.Maybe Prelude.Natural)
smallMultiplesOptions_maxVisibleRows = Lens.lens (\SmallMultiplesOptions' {maxVisibleRows} -> maxVisibleRows) (\s@SmallMultiplesOptions' {} a -> s {maxVisibleRows = a} :: SmallMultiplesOptions)

-- | Configures the display options for each small multiples panel.
smallMultiplesOptions_panelConfiguration :: Lens.Lens' SmallMultiplesOptions (Prelude.Maybe PanelConfiguration)
smallMultiplesOptions_panelConfiguration = Lens.lens (\SmallMultiplesOptions' {panelConfiguration} -> panelConfiguration) (\s@SmallMultiplesOptions' {} a -> s {panelConfiguration = a} :: SmallMultiplesOptions)

instance Data.FromJSON SmallMultiplesOptions where
  parseJSON =
    Data.withObject
      "SmallMultiplesOptions"
      ( \x ->
          SmallMultiplesOptions'
            Prelude.<$> (x Data..:? "MaxVisibleColumns")
            Prelude.<*> (x Data..:? "MaxVisibleRows")
            Prelude.<*> (x Data..:? "PanelConfiguration")
      )

instance Prelude.Hashable SmallMultiplesOptions where
  hashWithSalt _salt SmallMultiplesOptions' {..} =
    _salt `Prelude.hashWithSalt` maxVisibleColumns
      `Prelude.hashWithSalt` maxVisibleRows
      `Prelude.hashWithSalt` panelConfiguration

instance Prelude.NFData SmallMultiplesOptions where
  rnf SmallMultiplesOptions' {..} =
    Prelude.rnf maxVisibleColumns
      `Prelude.seq` Prelude.rnf maxVisibleRows
      `Prelude.seq` Prelude.rnf panelConfiguration

instance Data.ToJSON SmallMultiplesOptions where
  toJSON SmallMultiplesOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxVisibleColumns" Data..=)
              Prelude.<$> maxVisibleColumns,
            ("MaxVisibleRows" Data..=)
              Prelude.<$> maxVisibleRows,
            ("PanelConfiguration" Data..=)
              Prelude.<$> panelConfiguration
          ]
      )
