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
-- Module      : Amazonka.QuickSight.Types.AxisDisplayOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AxisDisplayOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisDataOptions
import Amazonka.QuickSight.Types.AxisTickLabelOptions
import Amazonka.QuickSight.Types.ScrollBarOptions
import Amazonka.QuickSight.Types.Visibility

-- | The display options for the axis label.
--
-- /See:/ 'newAxisDisplayOptions' smart constructor.
data AxisDisplayOptions = AxisDisplayOptions'
  { -- | Determines whether or not the axis line is visible.
    axisLineVisibility :: Prelude.Maybe Visibility,
    -- | The offset value that determines the starting placement of the axis
    -- within a visual\'s bounds.
    axisOffset :: Prelude.Maybe Prelude.Text,
    -- | The data options for an axis.
    dataOptions :: Prelude.Maybe AxisDataOptions,
    -- | Determines whether or not the grid line is visible.
    gridLineVisibility :: Prelude.Maybe Visibility,
    -- | The scroll bar options for an axis.
    scrollbarOptions :: Prelude.Maybe ScrollBarOptions,
    -- | The tick label options of an axis.
    tickLabelOptions :: Prelude.Maybe AxisTickLabelOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AxisDisplayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'axisLineVisibility', 'axisDisplayOptions_axisLineVisibility' - Determines whether or not the axis line is visible.
--
-- 'axisOffset', 'axisDisplayOptions_axisOffset' - The offset value that determines the starting placement of the axis
-- within a visual\'s bounds.
--
-- 'dataOptions', 'axisDisplayOptions_dataOptions' - The data options for an axis.
--
-- 'gridLineVisibility', 'axisDisplayOptions_gridLineVisibility' - Determines whether or not the grid line is visible.
--
-- 'scrollbarOptions', 'axisDisplayOptions_scrollbarOptions' - The scroll bar options for an axis.
--
-- 'tickLabelOptions', 'axisDisplayOptions_tickLabelOptions' - The tick label options of an axis.
newAxisDisplayOptions ::
  AxisDisplayOptions
newAxisDisplayOptions =
  AxisDisplayOptions'
    { axisLineVisibility =
        Prelude.Nothing,
      axisOffset = Prelude.Nothing,
      dataOptions = Prelude.Nothing,
      gridLineVisibility = Prelude.Nothing,
      scrollbarOptions = Prelude.Nothing,
      tickLabelOptions = Prelude.Nothing
    }

-- | Determines whether or not the axis line is visible.
axisDisplayOptions_axisLineVisibility :: Lens.Lens' AxisDisplayOptions (Prelude.Maybe Visibility)
axisDisplayOptions_axisLineVisibility = Lens.lens (\AxisDisplayOptions' {axisLineVisibility} -> axisLineVisibility) (\s@AxisDisplayOptions' {} a -> s {axisLineVisibility = a} :: AxisDisplayOptions)

-- | The offset value that determines the starting placement of the axis
-- within a visual\'s bounds.
axisDisplayOptions_axisOffset :: Lens.Lens' AxisDisplayOptions (Prelude.Maybe Prelude.Text)
axisDisplayOptions_axisOffset = Lens.lens (\AxisDisplayOptions' {axisOffset} -> axisOffset) (\s@AxisDisplayOptions' {} a -> s {axisOffset = a} :: AxisDisplayOptions)

-- | The data options for an axis.
axisDisplayOptions_dataOptions :: Lens.Lens' AxisDisplayOptions (Prelude.Maybe AxisDataOptions)
axisDisplayOptions_dataOptions = Lens.lens (\AxisDisplayOptions' {dataOptions} -> dataOptions) (\s@AxisDisplayOptions' {} a -> s {dataOptions = a} :: AxisDisplayOptions)

-- | Determines whether or not the grid line is visible.
axisDisplayOptions_gridLineVisibility :: Lens.Lens' AxisDisplayOptions (Prelude.Maybe Visibility)
axisDisplayOptions_gridLineVisibility = Lens.lens (\AxisDisplayOptions' {gridLineVisibility} -> gridLineVisibility) (\s@AxisDisplayOptions' {} a -> s {gridLineVisibility = a} :: AxisDisplayOptions)

-- | The scroll bar options for an axis.
axisDisplayOptions_scrollbarOptions :: Lens.Lens' AxisDisplayOptions (Prelude.Maybe ScrollBarOptions)
axisDisplayOptions_scrollbarOptions = Lens.lens (\AxisDisplayOptions' {scrollbarOptions} -> scrollbarOptions) (\s@AxisDisplayOptions' {} a -> s {scrollbarOptions = a} :: AxisDisplayOptions)

-- | The tick label options of an axis.
axisDisplayOptions_tickLabelOptions :: Lens.Lens' AxisDisplayOptions (Prelude.Maybe AxisTickLabelOptions)
axisDisplayOptions_tickLabelOptions = Lens.lens (\AxisDisplayOptions' {tickLabelOptions} -> tickLabelOptions) (\s@AxisDisplayOptions' {} a -> s {tickLabelOptions = a} :: AxisDisplayOptions)

instance Data.FromJSON AxisDisplayOptions where
  parseJSON =
    Data.withObject
      "AxisDisplayOptions"
      ( \x ->
          AxisDisplayOptions'
            Prelude.<$> (x Data..:? "AxisLineVisibility")
            Prelude.<*> (x Data..:? "AxisOffset")
            Prelude.<*> (x Data..:? "DataOptions")
            Prelude.<*> (x Data..:? "GridLineVisibility")
            Prelude.<*> (x Data..:? "ScrollbarOptions")
            Prelude.<*> (x Data..:? "TickLabelOptions")
      )

instance Prelude.Hashable AxisDisplayOptions where
  hashWithSalt _salt AxisDisplayOptions' {..} =
    _salt
      `Prelude.hashWithSalt` axisLineVisibility
      `Prelude.hashWithSalt` axisOffset
      `Prelude.hashWithSalt` dataOptions
      `Prelude.hashWithSalt` gridLineVisibility
      `Prelude.hashWithSalt` scrollbarOptions
      `Prelude.hashWithSalt` tickLabelOptions

instance Prelude.NFData AxisDisplayOptions where
  rnf AxisDisplayOptions' {..} =
    Prelude.rnf axisLineVisibility `Prelude.seq`
      Prelude.rnf axisOffset `Prelude.seq`
        Prelude.rnf dataOptions `Prelude.seq`
          Prelude.rnf gridLineVisibility `Prelude.seq`
            Prelude.rnf scrollbarOptions `Prelude.seq`
              Prelude.rnf tickLabelOptions

instance Data.ToJSON AxisDisplayOptions where
  toJSON AxisDisplayOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AxisLineVisibility" Data..=)
              Prelude.<$> axisLineVisibility,
            ("AxisOffset" Data..=) Prelude.<$> axisOffset,
            ("DataOptions" Data..=) Prelude.<$> dataOptions,
            ("GridLineVisibility" Data..=)
              Prelude.<$> gridLineVisibility,
            ("ScrollbarOptions" Data..=)
              Prelude.<$> scrollbarOptions,
            ("TickLabelOptions" Data..=)
              Prelude.<$> tickLabelOptions
          ]
      )
