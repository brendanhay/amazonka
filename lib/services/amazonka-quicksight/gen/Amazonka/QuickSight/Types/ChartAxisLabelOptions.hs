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
-- Module      : Amazonka.QuickSight.Types.ChartAxisLabelOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ChartAxisLabelOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisLabelOptions
import Amazonka.QuickSight.Types.Visibility

-- | The label options for an axis on a chart.
--
-- /See:/ 'newChartAxisLabelOptions' smart constructor.
data ChartAxisLabelOptions = ChartAxisLabelOptions'
  { -- | The label options for a chart axis.
    axisLabelOptions :: Prelude.Maybe [AxisLabelOptions],
    -- | The visibility configuration of the sort icon on a chart\'s axis label.
    sortIconVisibility :: Prelude.Maybe Visibility,
    -- | The visibility of an axis label on a chart. Choose one of the following
    -- options:
    --
    -- -   @VISIBLE@: Shows the axis.
    --
    -- -   @HIDDEN@: Hides the axis.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChartAxisLabelOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'axisLabelOptions', 'chartAxisLabelOptions_axisLabelOptions' - The label options for a chart axis.
--
-- 'sortIconVisibility', 'chartAxisLabelOptions_sortIconVisibility' - The visibility configuration of the sort icon on a chart\'s axis label.
--
-- 'visibility', 'chartAxisLabelOptions_visibility' - The visibility of an axis label on a chart. Choose one of the following
-- options:
--
-- -   @VISIBLE@: Shows the axis.
--
-- -   @HIDDEN@: Hides the axis.
newChartAxisLabelOptions ::
  ChartAxisLabelOptions
newChartAxisLabelOptions =
  ChartAxisLabelOptions'
    { axisLabelOptions =
        Prelude.Nothing,
      sortIconVisibility = Prelude.Nothing,
      visibility = Prelude.Nothing
    }

-- | The label options for a chart axis.
chartAxisLabelOptions_axisLabelOptions :: Lens.Lens' ChartAxisLabelOptions (Prelude.Maybe [AxisLabelOptions])
chartAxisLabelOptions_axisLabelOptions = Lens.lens (\ChartAxisLabelOptions' {axisLabelOptions} -> axisLabelOptions) (\s@ChartAxisLabelOptions' {} a -> s {axisLabelOptions = a} :: ChartAxisLabelOptions) Prelude.. Lens.mapping Lens.coerced

-- | The visibility configuration of the sort icon on a chart\'s axis label.
chartAxisLabelOptions_sortIconVisibility :: Lens.Lens' ChartAxisLabelOptions (Prelude.Maybe Visibility)
chartAxisLabelOptions_sortIconVisibility = Lens.lens (\ChartAxisLabelOptions' {sortIconVisibility} -> sortIconVisibility) (\s@ChartAxisLabelOptions' {} a -> s {sortIconVisibility = a} :: ChartAxisLabelOptions)

-- | The visibility of an axis label on a chart. Choose one of the following
-- options:
--
-- -   @VISIBLE@: Shows the axis.
--
-- -   @HIDDEN@: Hides the axis.
chartAxisLabelOptions_visibility :: Lens.Lens' ChartAxisLabelOptions (Prelude.Maybe Visibility)
chartAxisLabelOptions_visibility = Lens.lens (\ChartAxisLabelOptions' {visibility} -> visibility) (\s@ChartAxisLabelOptions' {} a -> s {visibility = a} :: ChartAxisLabelOptions)

instance Data.FromJSON ChartAxisLabelOptions where
  parseJSON =
    Data.withObject
      "ChartAxisLabelOptions"
      ( \x ->
          ChartAxisLabelOptions'
            Prelude.<$> ( x Data..:? "AxisLabelOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SortIconVisibility")
            Prelude.<*> (x Data..:? "Visibility")
      )

instance Prelude.Hashable ChartAxisLabelOptions where
  hashWithSalt _salt ChartAxisLabelOptions' {..} =
    _salt `Prelude.hashWithSalt` axisLabelOptions
      `Prelude.hashWithSalt` sortIconVisibility
      `Prelude.hashWithSalt` visibility

instance Prelude.NFData ChartAxisLabelOptions where
  rnf ChartAxisLabelOptions' {..} =
    Prelude.rnf axisLabelOptions
      `Prelude.seq` Prelude.rnf sortIconVisibility
      `Prelude.seq` Prelude.rnf visibility

instance Data.ToJSON ChartAxisLabelOptions where
  toJSON ChartAxisLabelOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AxisLabelOptions" Data..=)
              Prelude.<$> axisLabelOptions,
            ("SortIconVisibility" Data..=)
              Prelude.<$> sortIconVisibility,
            ("Visibility" Data..=) Prelude.<$> visibility
          ]
      )
