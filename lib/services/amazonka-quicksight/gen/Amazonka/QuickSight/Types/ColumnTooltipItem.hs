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
-- Module      : Amazonka.QuickSight.Types.ColumnTooltipItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColumnTooltipItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AggregationFunction
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.Visibility

-- | The tooltip item for the columns that are not part of a field well.
--
-- /See:/ 'newColumnTooltipItem' smart constructor.
data ColumnTooltipItem = ColumnTooltipItem'
  { -- | The aggregation function of the column tooltip item.
    aggregation :: Prelude.Maybe AggregationFunction,
    -- | The label of the tooltip item.
    label :: Prelude.Maybe Prelude.Text,
    -- | The visibility of the tooltip item.
    visibility :: Prelude.Maybe Visibility,
    -- | The target column of the tooltip item.
    column :: ColumnIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnTooltipItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregation', 'columnTooltipItem_aggregation' - The aggregation function of the column tooltip item.
--
-- 'label', 'columnTooltipItem_label' - The label of the tooltip item.
--
-- 'visibility', 'columnTooltipItem_visibility' - The visibility of the tooltip item.
--
-- 'column', 'columnTooltipItem_column' - The target column of the tooltip item.
newColumnTooltipItem ::
  -- | 'column'
  ColumnIdentifier ->
  ColumnTooltipItem
newColumnTooltipItem pColumn_ =
  ColumnTooltipItem'
    { aggregation = Prelude.Nothing,
      label = Prelude.Nothing,
      visibility = Prelude.Nothing,
      column = pColumn_
    }

-- | The aggregation function of the column tooltip item.
columnTooltipItem_aggregation :: Lens.Lens' ColumnTooltipItem (Prelude.Maybe AggregationFunction)
columnTooltipItem_aggregation = Lens.lens (\ColumnTooltipItem' {aggregation} -> aggregation) (\s@ColumnTooltipItem' {} a -> s {aggregation = a} :: ColumnTooltipItem)

-- | The label of the tooltip item.
columnTooltipItem_label :: Lens.Lens' ColumnTooltipItem (Prelude.Maybe Prelude.Text)
columnTooltipItem_label = Lens.lens (\ColumnTooltipItem' {label} -> label) (\s@ColumnTooltipItem' {} a -> s {label = a} :: ColumnTooltipItem)

-- | The visibility of the tooltip item.
columnTooltipItem_visibility :: Lens.Lens' ColumnTooltipItem (Prelude.Maybe Visibility)
columnTooltipItem_visibility = Lens.lens (\ColumnTooltipItem' {visibility} -> visibility) (\s@ColumnTooltipItem' {} a -> s {visibility = a} :: ColumnTooltipItem)

-- | The target column of the tooltip item.
columnTooltipItem_column :: Lens.Lens' ColumnTooltipItem ColumnIdentifier
columnTooltipItem_column = Lens.lens (\ColumnTooltipItem' {column} -> column) (\s@ColumnTooltipItem' {} a -> s {column = a} :: ColumnTooltipItem)

instance Data.FromJSON ColumnTooltipItem where
  parseJSON =
    Data.withObject
      "ColumnTooltipItem"
      ( \x ->
          ColumnTooltipItem'
            Prelude.<$> (x Data..:? "Aggregation")
            Prelude.<*> (x Data..:? "Label")
            Prelude.<*> (x Data..:? "Visibility")
            Prelude.<*> (x Data..: "Column")
      )

instance Prelude.Hashable ColumnTooltipItem where
  hashWithSalt _salt ColumnTooltipItem' {..} =
    _salt
      `Prelude.hashWithSalt` aggregation
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` visibility
      `Prelude.hashWithSalt` column

instance Prelude.NFData ColumnTooltipItem where
  rnf ColumnTooltipItem' {..} =
    Prelude.rnf aggregation
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf visibility
      `Prelude.seq` Prelude.rnf column

instance Data.ToJSON ColumnTooltipItem where
  toJSON ColumnTooltipItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Aggregation" Data..=) Prelude.<$> aggregation,
            ("Label" Data..=) Prelude.<$> label,
            ("Visibility" Data..=) Prelude.<$> visibility,
            Prelude.Just ("Column" Data..= column)
          ]
      )
