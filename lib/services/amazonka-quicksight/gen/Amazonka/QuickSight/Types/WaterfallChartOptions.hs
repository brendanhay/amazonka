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
-- Module      : Amazonka.QuickSight.Types.WaterfallChartOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WaterfallChartOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The options that determine the presentation of a waterfall visual.
--
-- /See:/ 'newWaterfallChartOptions' smart constructor.
data WaterfallChartOptions = WaterfallChartOptions'
  { -- | This option determines the total bar label of a waterfall visual.
    totalBarLabel :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WaterfallChartOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalBarLabel', 'waterfallChartOptions_totalBarLabel' - This option determines the total bar label of a waterfall visual.
newWaterfallChartOptions ::
  WaterfallChartOptions
newWaterfallChartOptions =
  WaterfallChartOptions'
    { totalBarLabel =
        Prelude.Nothing
    }

-- | This option determines the total bar label of a waterfall visual.
waterfallChartOptions_totalBarLabel :: Lens.Lens' WaterfallChartOptions (Prelude.Maybe Prelude.Text)
waterfallChartOptions_totalBarLabel = Lens.lens (\WaterfallChartOptions' {totalBarLabel} -> totalBarLabel) (\s@WaterfallChartOptions' {} a -> s {totalBarLabel = a} :: WaterfallChartOptions)

instance Data.FromJSON WaterfallChartOptions where
  parseJSON =
    Data.withObject
      "WaterfallChartOptions"
      ( \x ->
          WaterfallChartOptions'
            Prelude.<$> (x Data..:? "TotalBarLabel")
      )

instance Prelude.Hashable WaterfallChartOptions where
  hashWithSalt _salt WaterfallChartOptions' {..} =
    _salt `Prelude.hashWithSalt` totalBarLabel

instance Prelude.NFData WaterfallChartOptions where
  rnf WaterfallChartOptions' {..} =
    Prelude.rnf totalBarLabel

instance Data.ToJSON WaterfallChartOptions where
  toJSON WaterfallChartOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TotalBarLabel" Data..=)
              Prelude.<$> totalBarLabel
          ]
      )
