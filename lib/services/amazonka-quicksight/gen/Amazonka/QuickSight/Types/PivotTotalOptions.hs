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
-- Module      : Amazonka.QuickSight.Types.PivotTotalOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTotalOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableCellStyle
import Amazonka.QuickSight.Types.TableTotalsPlacement
import Amazonka.QuickSight.Types.TableTotalsScrollStatus
import Amazonka.QuickSight.Types.Visibility

-- | The optional configuration of totals cells in a @PivotTableVisual@.
--
-- /See:/ 'newPivotTotalOptions' smart constructor.
data PivotTotalOptions = PivotTotalOptions'
  { -- | The custom label string for the total cells.
    customLabel :: Prelude.Maybe Prelude.Text,
    -- | The cell styling options for the total of header cells.
    metricHeaderCellStyle :: Prelude.Maybe TableCellStyle,
    -- | The placement (start, end) for the total cells.
    placement :: Prelude.Maybe TableTotalsPlacement,
    -- | The scroll status (pinned, scrolled) for the total cells.
    scrollStatus :: Prelude.Maybe TableTotalsScrollStatus,
    -- | The cell styling options for the total cells.
    totalCellStyle :: Prelude.Maybe TableCellStyle,
    -- | The visibility configuration for the total cells.
    totalsVisibility :: Prelude.Maybe Visibility,
    -- | The cell styling options for the totals of value cells.
    valueCellStyle :: Prelude.Maybe TableCellStyle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTotalOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customLabel', 'pivotTotalOptions_customLabel' - The custom label string for the total cells.
--
-- 'metricHeaderCellStyle', 'pivotTotalOptions_metricHeaderCellStyle' - The cell styling options for the total of header cells.
--
-- 'placement', 'pivotTotalOptions_placement' - The placement (start, end) for the total cells.
--
-- 'scrollStatus', 'pivotTotalOptions_scrollStatus' - The scroll status (pinned, scrolled) for the total cells.
--
-- 'totalCellStyle', 'pivotTotalOptions_totalCellStyle' - The cell styling options for the total cells.
--
-- 'totalsVisibility', 'pivotTotalOptions_totalsVisibility' - The visibility configuration for the total cells.
--
-- 'valueCellStyle', 'pivotTotalOptions_valueCellStyle' - The cell styling options for the totals of value cells.
newPivotTotalOptions ::
  PivotTotalOptions
newPivotTotalOptions =
  PivotTotalOptions'
    { customLabel = Prelude.Nothing,
      metricHeaderCellStyle = Prelude.Nothing,
      placement = Prelude.Nothing,
      scrollStatus = Prelude.Nothing,
      totalCellStyle = Prelude.Nothing,
      totalsVisibility = Prelude.Nothing,
      valueCellStyle = Prelude.Nothing
    }

-- | The custom label string for the total cells.
pivotTotalOptions_customLabel :: Lens.Lens' PivotTotalOptions (Prelude.Maybe Prelude.Text)
pivotTotalOptions_customLabel = Lens.lens (\PivotTotalOptions' {customLabel} -> customLabel) (\s@PivotTotalOptions' {} a -> s {customLabel = a} :: PivotTotalOptions)

-- | The cell styling options for the total of header cells.
pivotTotalOptions_metricHeaderCellStyle :: Lens.Lens' PivotTotalOptions (Prelude.Maybe TableCellStyle)
pivotTotalOptions_metricHeaderCellStyle = Lens.lens (\PivotTotalOptions' {metricHeaderCellStyle} -> metricHeaderCellStyle) (\s@PivotTotalOptions' {} a -> s {metricHeaderCellStyle = a} :: PivotTotalOptions)

-- | The placement (start, end) for the total cells.
pivotTotalOptions_placement :: Lens.Lens' PivotTotalOptions (Prelude.Maybe TableTotalsPlacement)
pivotTotalOptions_placement = Lens.lens (\PivotTotalOptions' {placement} -> placement) (\s@PivotTotalOptions' {} a -> s {placement = a} :: PivotTotalOptions)

-- | The scroll status (pinned, scrolled) for the total cells.
pivotTotalOptions_scrollStatus :: Lens.Lens' PivotTotalOptions (Prelude.Maybe TableTotalsScrollStatus)
pivotTotalOptions_scrollStatus = Lens.lens (\PivotTotalOptions' {scrollStatus} -> scrollStatus) (\s@PivotTotalOptions' {} a -> s {scrollStatus = a} :: PivotTotalOptions)

-- | The cell styling options for the total cells.
pivotTotalOptions_totalCellStyle :: Lens.Lens' PivotTotalOptions (Prelude.Maybe TableCellStyle)
pivotTotalOptions_totalCellStyle = Lens.lens (\PivotTotalOptions' {totalCellStyle} -> totalCellStyle) (\s@PivotTotalOptions' {} a -> s {totalCellStyle = a} :: PivotTotalOptions)

-- | The visibility configuration for the total cells.
pivotTotalOptions_totalsVisibility :: Lens.Lens' PivotTotalOptions (Prelude.Maybe Visibility)
pivotTotalOptions_totalsVisibility = Lens.lens (\PivotTotalOptions' {totalsVisibility} -> totalsVisibility) (\s@PivotTotalOptions' {} a -> s {totalsVisibility = a} :: PivotTotalOptions)

-- | The cell styling options for the totals of value cells.
pivotTotalOptions_valueCellStyle :: Lens.Lens' PivotTotalOptions (Prelude.Maybe TableCellStyle)
pivotTotalOptions_valueCellStyle = Lens.lens (\PivotTotalOptions' {valueCellStyle} -> valueCellStyle) (\s@PivotTotalOptions' {} a -> s {valueCellStyle = a} :: PivotTotalOptions)

instance Data.FromJSON PivotTotalOptions where
  parseJSON =
    Data.withObject
      "PivotTotalOptions"
      ( \x ->
          PivotTotalOptions'
            Prelude.<$> (x Data..:? "CustomLabel")
            Prelude.<*> (x Data..:? "MetricHeaderCellStyle")
            Prelude.<*> (x Data..:? "Placement")
            Prelude.<*> (x Data..:? "ScrollStatus")
            Prelude.<*> (x Data..:? "TotalCellStyle")
            Prelude.<*> (x Data..:? "TotalsVisibility")
            Prelude.<*> (x Data..:? "ValueCellStyle")
      )

instance Prelude.Hashable PivotTotalOptions where
  hashWithSalt _salt PivotTotalOptions' {..} =
    _salt `Prelude.hashWithSalt` customLabel
      `Prelude.hashWithSalt` metricHeaderCellStyle
      `Prelude.hashWithSalt` placement
      `Prelude.hashWithSalt` scrollStatus
      `Prelude.hashWithSalt` totalCellStyle
      `Prelude.hashWithSalt` totalsVisibility
      `Prelude.hashWithSalt` valueCellStyle

instance Prelude.NFData PivotTotalOptions where
  rnf PivotTotalOptions' {..} =
    Prelude.rnf customLabel
      `Prelude.seq` Prelude.rnf metricHeaderCellStyle
      `Prelude.seq` Prelude.rnf placement
      `Prelude.seq` Prelude.rnf scrollStatus
      `Prelude.seq` Prelude.rnf totalCellStyle
      `Prelude.seq` Prelude.rnf totalsVisibility
      `Prelude.seq` Prelude.rnf valueCellStyle

instance Data.ToJSON PivotTotalOptions where
  toJSON PivotTotalOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomLabel" Data..=) Prelude.<$> customLabel,
            ("MetricHeaderCellStyle" Data..=)
              Prelude.<$> metricHeaderCellStyle,
            ("Placement" Data..=) Prelude.<$> placement,
            ("ScrollStatus" Data..=) Prelude.<$> scrollStatus,
            ("TotalCellStyle" Data..=)
              Prelude.<$> totalCellStyle,
            ("TotalsVisibility" Data..=)
              Prelude.<$> totalsVisibility,
            ("ValueCellStyle" Data..=)
              Prelude.<$> valueCellStyle
          ]
      )
