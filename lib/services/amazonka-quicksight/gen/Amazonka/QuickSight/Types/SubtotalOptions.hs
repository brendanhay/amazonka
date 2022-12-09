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
-- Module      : Amazonka.QuickSight.Types.SubtotalOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SubtotalOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PivotTableFieldSubtotalOptions
import Amazonka.QuickSight.Types.PivotTableSubtotalLevel
import Amazonka.QuickSight.Types.TableCellStyle
import Amazonka.QuickSight.Types.Visibility

-- | The subtotal options.
--
-- /See:/ 'newSubtotalOptions' smart constructor.
data SubtotalOptions = SubtotalOptions'
  { -- | The custom label string for the subtotal cells.
    customLabel :: Prelude.Maybe Prelude.Text,
    -- | The field level (all, custom, last) for the subtotal cells.
    fieldLevel :: Prelude.Maybe PivotTableSubtotalLevel,
    -- | The optional configuration of subtotal cells.
    fieldLevelOptions :: Prelude.Maybe [PivotTableFieldSubtotalOptions],
    -- | The cell styling options for the subtotals of header cells.
    metricHeaderCellStyle :: Prelude.Maybe TableCellStyle,
    -- | The cell styling options for the subtotal cells.
    totalCellStyle :: Prelude.Maybe TableCellStyle,
    -- | The visibility configuration for the subtotal cells.
    totalsVisibility :: Prelude.Maybe Visibility,
    -- | The cell styling options for the subtotals of value cells.
    valueCellStyle :: Prelude.Maybe TableCellStyle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubtotalOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customLabel', 'subtotalOptions_customLabel' - The custom label string for the subtotal cells.
--
-- 'fieldLevel', 'subtotalOptions_fieldLevel' - The field level (all, custom, last) for the subtotal cells.
--
-- 'fieldLevelOptions', 'subtotalOptions_fieldLevelOptions' - The optional configuration of subtotal cells.
--
-- 'metricHeaderCellStyle', 'subtotalOptions_metricHeaderCellStyle' - The cell styling options for the subtotals of header cells.
--
-- 'totalCellStyle', 'subtotalOptions_totalCellStyle' - The cell styling options for the subtotal cells.
--
-- 'totalsVisibility', 'subtotalOptions_totalsVisibility' - The visibility configuration for the subtotal cells.
--
-- 'valueCellStyle', 'subtotalOptions_valueCellStyle' - The cell styling options for the subtotals of value cells.
newSubtotalOptions ::
  SubtotalOptions
newSubtotalOptions =
  SubtotalOptions'
    { customLabel = Prelude.Nothing,
      fieldLevel = Prelude.Nothing,
      fieldLevelOptions = Prelude.Nothing,
      metricHeaderCellStyle = Prelude.Nothing,
      totalCellStyle = Prelude.Nothing,
      totalsVisibility = Prelude.Nothing,
      valueCellStyle = Prelude.Nothing
    }

-- | The custom label string for the subtotal cells.
subtotalOptions_customLabel :: Lens.Lens' SubtotalOptions (Prelude.Maybe Prelude.Text)
subtotalOptions_customLabel = Lens.lens (\SubtotalOptions' {customLabel} -> customLabel) (\s@SubtotalOptions' {} a -> s {customLabel = a} :: SubtotalOptions)

-- | The field level (all, custom, last) for the subtotal cells.
subtotalOptions_fieldLevel :: Lens.Lens' SubtotalOptions (Prelude.Maybe PivotTableSubtotalLevel)
subtotalOptions_fieldLevel = Lens.lens (\SubtotalOptions' {fieldLevel} -> fieldLevel) (\s@SubtotalOptions' {} a -> s {fieldLevel = a} :: SubtotalOptions)

-- | The optional configuration of subtotal cells.
subtotalOptions_fieldLevelOptions :: Lens.Lens' SubtotalOptions (Prelude.Maybe [PivotTableFieldSubtotalOptions])
subtotalOptions_fieldLevelOptions = Lens.lens (\SubtotalOptions' {fieldLevelOptions} -> fieldLevelOptions) (\s@SubtotalOptions' {} a -> s {fieldLevelOptions = a} :: SubtotalOptions) Prelude.. Lens.mapping Lens.coerced

-- | The cell styling options for the subtotals of header cells.
subtotalOptions_metricHeaderCellStyle :: Lens.Lens' SubtotalOptions (Prelude.Maybe TableCellStyle)
subtotalOptions_metricHeaderCellStyle = Lens.lens (\SubtotalOptions' {metricHeaderCellStyle} -> metricHeaderCellStyle) (\s@SubtotalOptions' {} a -> s {metricHeaderCellStyle = a} :: SubtotalOptions)

-- | The cell styling options for the subtotal cells.
subtotalOptions_totalCellStyle :: Lens.Lens' SubtotalOptions (Prelude.Maybe TableCellStyle)
subtotalOptions_totalCellStyle = Lens.lens (\SubtotalOptions' {totalCellStyle} -> totalCellStyle) (\s@SubtotalOptions' {} a -> s {totalCellStyle = a} :: SubtotalOptions)

-- | The visibility configuration for the subtotal cells.
subtotalOptions_totalsVisibility :: Lens.Lens' SubtotalOptions (Prelude.Maybe Visibility)
subtotalOptions_totalsVisibility = Lens.lens (\SubtotalOptions' {totalsVisibility} -> totalsVisibility) (\s@SubtotalOptions' {} a -> s {totalsVisibility = a} :: SubtotalOptions)

-- | The cell styling options for the subtotals of value cells.
subtotalOptions_valueCellStyle :: Lens.Lens' SubtotalOptions (Prelude.Maybe TableCellStyle)
subtotalOptions_valueCellStyle = Lens.lens (\SubtotalOptions' {valueCellStyle} -> valueCellStyle) (\s@SubtotalOptions' {} a -> s {valueCellStyle = a} :: SubtotalOptions)

instance Data.FromJSON SubtotalOptions where
  parseJSON =
    Data.withObject
      "SubtotalOptions"
      ( \x ->
          SubtotalOptions'
            Prelude.<$> (x Data..:? "CustomLabel")
            Prelude.<*> (x Data..:? "FieldLevel")
            Prelude.<*> ( x Data..:? "FieldLevelOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "MetricHeaderCellStyle")
            Prelude.<*> (x Data..:? "TotalCellStyle")
            Prelude.<*> (x Data..:? "TotalsVisibility")
            Prelude.<*> (x Data..:? "ValueCellStyle")
      )

instance Prelude.Hashable SubtotalOptions where
  hashWithSalt _salt SubtotalOptions' {..} =
    _salt `Prelude.hashWithSalt` customLabel
      `Prelude.hashWithSalt` fieldLevel
      `Prelude.hashWithSalt` fieldLevelOptions
      `Prelude.hashWithSalt` metricHeaderCellStyle
      `Prelude.hashWithSalt` totalCellStyle
      `Prelude.hashWithSalt` totalsVisibility
      `Prelude.hashWithSalt` valueCellStyle

instance Prelude.NFData SubtotalOptions where
  rnf SubtotalOptions' {..} =
    Prelude.rnf customLabel
      `Prelude.seq` Prelude.rnf fieldLevel
      `Prelude.seq` Prelude.rnf fieldLevelOptions
      `Prelude.seq` Prelude.rnf metricHeaderCellStyle
      `Prelude.seq` Prelude.rnf totalCellStyle
      `Prelude.seq` Prelude.rnf totalsVisibility
      `Prelude.seq` Prelude.rnf valueCellStyle

instance Data.ToJSON SubtotalOptions where
  toJSON SubtotalOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomLabel" Data..=) Prelude.<$> customLabel,
            ("FieldLevel" Data..=) Prelude.<$> fieldLevel,
            ("FieldLevelOptions" Data..=)
              Prelude.<$> fieldLevelOptions,
            ("MetricHeaderCellStyle" Data..=)
              Prelude.<$> metricHeaderCellStyle,
            ("TotalCellStyle" Data..=)
              Prelude.<$> totalCellStyle,
            ("TotalsVisibility" Data..=)
              Prelude.<$> totalsVisibility,
            ("ValueCellStyle" Data..=)
              Prelude.<$> valueCellStyle
          ]
      )
