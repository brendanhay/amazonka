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
-- Module      : Amazonka.QuickSight.Types.GaugeChartConditionalFormattingOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GaugeChartConditionalFormattingOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GaugeChartArcConditionalFormatting
import Amazonka.QuickSight.Types.GaugeChartPrimaryValueConditionalFormatting

-- | Conditional formatting options of a @GaugeChartVisual@.
--
-- /See:/ 'newGaugeChartConditionalFormattingOption' smart constructor.
data GaugeChartConditionalFormattingOption = GaugeChartConditionalFormattingOption'
  { -- | The options that determine the presentation of the arc of a
    -- @GaugeChartVisual@.
    arc :: Prelude.Maybe GaugeChartArcConditionalFormatting,
    -- | The conditional formatting for the primary value of a
    -- @GaugeChartVisual@.
    primaryValue :: Prelude.Maybe GaugeChartPrimaryValueConditionalFormatting
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GaugeChartConditionalFormattingOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arc', 'gaugeChartConditionalFormattingOption_arc' - The options that determine the presentation of the arc of a
-- @GaugeChartVisual@.
--
-- 'primaryValue', 'gaugeChartConditionalFormattingOption_primaryValue' - The conditional formatting for the primary value of a
-- @GaugeChartVisual@.
newGaugeChartConditionalFormattingOption ::
  GaugeChartConditionalFormattingOption
newGaugeChartConditionalFormattingOption =
  GaugeChartConditionalFormattingOption'
    { arc =
        Prelude.Nothing,
      primaryValue = Prelude.Nothing
    }

-- | The options that determine the presentation of the arc of a
-- @GaugeChartVisual@.
gaugeChartConditionalFormattingOption_arc :: Lens.Lens' GaugeChartConditionalFormattingOption (Prelude.Maybe GaugeChartArcConditionalFormatting)
gaugeChartConditionalFormattingOption_arc = Lens.lens (\GaugeChartConditionalFormattingOption' {arc} -> arc) (\s@GaugeChartConditionalFormattingOption' {} a -> s {arc = a} :: GaugeChartConditionalFormattingOption)

-- | The conditional formatting for the primary value of a
-- @GaugeChartVisual@.
gaugeChartConditionalFormattingOption_primaryValue :: Lens.Lens' GaugeChartConditionalFormattingOption (Prelude.Maybe GaugeChartPrimaryValueConditionalFormatting)
gaugeChartConditionalFormattingOption_primaryValue = Lens.lens (\GaugeChartConditionalFormattingOption' {primaryValue} -> primaryValue) (\s@GaugeChartConditionalFormattingOption' {} a -> s {primaryValue = a} :: GaugeChartConditionalFormattingOption)

instance
  Data.FromJSON
    GaugeChartConditionalFormattingOption
  where
  parseJSON =
    Data.withObject
      "GaugeChartConditionalFormattingOption"
      ( \x ->
          GaugeChartConditionalFormattingOption'
            Prelude.<$> (x Data..:? "Arc")
            Prelude.<*> (x Data..:? "PrimaryValue")
      )

instance
  Prelude.Hashable
    GaugeChartConditionalFormattingOption
  where
  hashWithSalt
    _salt
    GaugeChartConditionalFormattingOption' {..} =
      _salt
        `Prelude.hashWithSalt` arc
        `Prelude.hashWithSalt` primaryValue

instance
  Prelude.NFData
    GaugeChartConditionalFormattingOption
  where
  rnf GaugeChartConditionalFormattingOption' {..} =
    Prelude.rnf arc `Prelude.seq`
      Prelude.rnf primaryValue

instance
  Data.ToJSON
    GaugeChartConditionalFormattingOption
  where
  toJSON GaugeChartConditionalFormattingOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Arc" Data..=) Prelude.<$> arc,
            ("PrimaryValue" Data..=) Prelude.<$> primaryValue
          ]
      )
