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
-- Module      : Amazonka.QuickSight.Types.GaugeChartArcConditionalFormatting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GaugeChartArcConditionalFormatting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ConditionalFormattingColor

-- | The options that determine the presentation of the arc of a
-- @GaugeChartVisual@.
--
-- /See:/ 'newGaugeChartArcConditionalFormatting' smart constructor.
data GaugeChartArcConditionalFormatting = GaugeChartArcConditionalFormatting'
  { -- | The conditional formatting of the arc foreground color.
    foregroundColor :: Prelude.Maybe ConditionalFormattingColor
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GaugeChartArcConditionalFormatting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'foregroundColor', 'gaugeChartArcConditionalFormatting_foregroundColor' - The conditional formatting of the arc foreground color.
newGaugeChartArcConditionalFormatting ::
  GaugeChartArcConditionalFormatting
newGaugeChartArcConditionalFormatting =
  GaugeChartArcConditionalFormatting'
    { foregroundColor =
        Prelude.Nothing
    }

-- | The conditional formatting of the arc foreground color.
gaugeChartArcConditionalFormatting_foregroundColor :: Lens.Lens' GaugeChartArcConditionalFormatting (Prelude.Maybe ConditionalFormattingColor)
gaugeChartArcConditionalFormatting_foregroundColor = Lens.lens (\GaugeChartArcConditionalFormatting' {foregroundColor} -> foregroundColor) (\s@GaugeChartArcConditionalFormatting' {} a -> s {foregroundColor = a} :: GaugeChartArcConditionalFormatting)

instance
  Data.FromJSON
    GaugeChartArcConditionalFormatting
  where
  parseJSON =
    Data.withObject
      "GaugeChartArcConditionalFormatting"
      ( \x ->
          GaugeChartArcConditionalFormatting'
            Prelude.<$> (x Data..:? "ForegroundColor")
      )

instance
  Prelude.Hashable
    GaugeChartArcConditionalFormatting
  where
  hashWithSalt
    _salt
    GaugeChartArcConditionalFormatting' {..} =
      _salt `Prelude.hashWithSalt` foregroundColor

instance
  Prelude.NFData
    GaugeChartArcConditionalFormatting
  where
  rnf GaugeChartArcConditionalFormatting' {..} =
    Prelude.rnf foregroundColor

instance
  Data.ToJSON
    GaugeChartArcConditionalFormatting
  where
  toJSON GaugeChartArcConditionalFormatting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ForegroundColor" Data..=)
              Prelude.<$> foregroundColor
          ]
      )
