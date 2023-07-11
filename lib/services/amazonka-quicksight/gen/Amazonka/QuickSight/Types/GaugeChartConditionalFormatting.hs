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
-- Module      : Amazonka.QuickSight.Types.GaugeChartConditionalFormatting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GaugeChartConditionalFormatting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GaugeChartConditionalFormattingOption

-- | The conditional formatting of a @GaugeChartVisual@.
--
-- /See:/ 'newGaugeChartConditionalFormatting' smart constructor.
data GaugeChartConditionalFormatting = GaugeChartConditionalFormatting'
  { -- | Conditional formatting options of a @GaugeChartVisual@.
    conditionalFormattingOptions :: Prelude.Maybe [GaugeChartConditionalFormattingOption]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GaugeChartConditionalFormatting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionalFormattingOptions', 'gaugeChartConditionalFormatting_conditionalFormattingOptions' - Conditional formatting options of a @GaugeChartVisual@.
newGaugeChartConditionalFormatting ::
  GaugeChartConditionalFormatting
newGaugeChartConditionalFormatting =
  GaugeChartConditionalFormatting'
    { conditionalFormattingOptions =
        Prelude.Nothing
    }

-- | Conditional formatting options of a @GaugeChartVisual@.
gaugeChartConditionalFormatting_conditionalFormattingOptions :: Lens.Lens' GaugeChartConditionalFormatting (Prelude.Maybe [GaugeChartConditionalFormattingOption])
gaugeChartConditionalFormatting_conditionalFormattingOptions = Lens.lens (\GaugeChartConditionalFormatting' {conditionalFormattingOptions} -> conditionalFormattingOptions) (\s@GaugeChartConditionalFormatting' {} a -> s {conditionalFormattingOptions = a} :: GaugeChartConditionalFormatting) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    GaugeChartConditionalFormatting
  where
  parseJSON =
    Data.withObject
      "GaugeChartConditionalFormatting"
      ( \x ->
          GaugeChartConditionalFormatting'
            Prelude.<$> ( x
                            Data..:? "ConditionalFormattingOptions"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    GaugeChartConditionalFormatting
  where
  hashWithSalt
    _salt
    GaugeChartConditionalFormatting' {..} =
      _salt
        `Prelude.hashWithSalt` conditionalFormattingOptions

instance
  Prelude.NFData
    GaugeChartConditionalFormatting
  where
  rnf GaugeChartConditionalFormatting' {..} =
    Prelude.rnf conditionalFormattingOptions

instance Data.ToJSON GaugeChartConditionalFormatting where
  toJSON GaugeChartConditionalFormatting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConditionalFormattingOptions" Data..=)
              Prelude.<$> conditionalFormattingOptions
          ]
      )
