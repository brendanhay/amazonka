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
-- Module      : Amazonka.QuickSight.Types.GaugeChartPrimaryValueConditionalFormatting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GaugeChartPrimaryValueConditionalFormatting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ConditionalFormattingColor
import Amazonka.QuickSight.Types.ConditionalFormattingIcon

-- | The conditional formatting for the primary value of a
-- @GaugeChartVisual@.
--
-- /See:/ 'newGaugeChartPrimaryValueConditionalFormatting' smart constructor.
data GaugeChartPrimaryValueConditionalFormatting = GaugeChartPrimaryValueConditionalFormatting'
  { -- | The conditional formatting of the primary value icon.
    icon :: Prelude.Maybe ConditionalFormattingIcon,
    -- | The conditional formatting of the primary value text color.
    textColor :: Prelude.Maybe ConditionalFormattingColor
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GaugeChartPrimaryValueConditionalFormatting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'icon', 'gaugeChartPrimaryValueConditionalFormatting_icon' - The conditional formatting of the primary value icon.
--
-- 'textColor', 'gaugeChartPrimaryValueConditionalFormatting_textColor' - The conditional formatting of the primary value text color.
newGaugeChartPrimaryValueConditionalFormatting ::
  GaugeChartPrimaryValueConditionalFormatting
newGaugeChartPrimaryValueConditionalFormatting =
  GaugeChartPrimaryValueConditionalFormatting'
    { icon =
        Prelude.Nothing,
      textColor = Prelude.Nothing
    }

-- | The conditional formatting of the primary value icon.
gaugeChartPrimaryValueConditionalFormatting_icon :: Lens.Lens' GaugeChartPrimaryValueConditionalFormatting (Prelude.Maybe ConditionalFormattingIcon)
gaugeChartPrimaryValueConditionalFormatting_icon = Lens.lens (\GaugeChartPrimaryValueConditionalFormatting' {icon} -> icon) (\s@GaugeChartPrimaryValueConditionalFormatting' {} a -> s {icon = a} :: GaugeChartPrimaryValueConditionalFormatting)

-- | The conditional formatting of the primary value text color.
gaugeChartPrimaryValueConditionalFormatting_textColor :: Lens.Lens' GaugeChartPrimaryValueConditionalFormatting (Prelude.Maybe ConditionalFormattingColor)
gaugeChartPrimaryValueConditionalFormatting_textColor = Lens.lens (\GaugeChartPrimaryValueConditionalFormatting' {textColor} -> textColor) (\s@GaugeChartPrimaryValueConditionalFormatting' {} a -> s {textColor = a} :: GaugeChartPrimaryValueConditionalFormatting)

instance
  Data.FromJSON
    GaugeChartPrimaryValueConditionalFormatting
  where
  parseJSON =
    Data.withObject
      "GaugeChartPrimaryValueConditionalFormatting"
      ( \x ->
          GaugeChartPrimaryValueConditionalFormatting'
            Prelude.<$> (x Data..:? "Icon")
              Prelude.<*> (x Data..:? "TextColor")
      )

instance
  Prelude.Hashable
    GaugeChartPrimaryValueConditionalFormatting
  where
  hashWithSalt
    _salt
    GaugeChartPrimaryValueConditionalFormatting' {..} =
      _salt `Prelude.hashWithSalt` icon
        `Prelude.hashWithSalt` textColor

instance
  Prelude.NFData
    GaugeChartPrimaryValueConditionalFormatting
  where
  rnf GaugeChartPrimaryValueConditionalFormatting' {..} =
    Prelude.rnf icon
      `Prelude.seq` Prelude.rnf textColor

instance
  Data.ToJSON
    GaugeChartPrimaryValueConditionalFormatting
  where
  toJSON
    GaugeChartPrimaryValueConditionalFormatting' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Icon" Data..=) Prelude.<$> icon,
              ("TextColor" Data..=) Prelude.<$> textColor
            ]
        )
