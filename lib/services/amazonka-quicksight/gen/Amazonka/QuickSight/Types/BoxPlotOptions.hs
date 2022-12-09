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
-- Module      : Amazonka.QuickSight.Types.BoxPlotOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BoxPlotOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.BoxPlotStyleOptions
import Amazonka.QuickSight.Types.Visibility

-- | The options of a box plot visual.
--
-- /See:/ 'newBoxPlotOptions' smart constructor.
data BoxPlotOptions = BoxPlotOptions'
  { -- | Determines the visibility of all data points of the box plot.
    allDataPointsVisibility :: Prelude.Maybe Visibility,
    -- | Determines the visibility of the outlier in a box plot.
    outlierVisibility :: Prelude.Maybe Visibility,
    -- | The style options of the box plot.
    styleOptions :: Prelude.Maybe BoxPlotStyleOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BoxPlotOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allDataPointsVisibility', 'boxPlotOptions_allDataPointsVisibility' - Determines the visibility of all data points of the box plot.
--
-- 'outlierVisibility', 'boxPlotOptions_outlierVisibility' - Determines the visibility of the outlier in a box plot.
--
-- 'styleOptions', 'boxPlotOptions_styleOptions' - The style options of the box plot.
newBoxPlotOptions ::
  BoxPlotOptions
newBoxPlotOptions =
  BoxPlotOptions'
    { allDataPointsVisibility =
        Prelude.Nothing,
      outlierVisibility = Prelude.Nothing,
      styleOptions = Prelude.Nothing
    }

-- | Determines the visibility of all data points of the box plot.
boxPlotOptions_allDataPointsVisibility :: Lens.Lens' BoxPlotOptions (Prelude.Maybe Visibility)
boxPlotOptions_allDataPointsVisibility = Lens.lens (\BoxPlotOptions' {allDataPointsVisibility} -> allDataPointsVisibility) (\s@BoxPlotOptions' {} a -> s {allDataPointsVisibility = a} :: BoxPlotOptions)

-- | Determines the visibility of the outlier in a box plot.
boxPlotOptions_outlierVisibility :: Lens.Lens' BoxPlotOptions (Prelude.Maybe Visibility)
boxPlotOptions_outlierVisibility = Lens.lens (\BoxPlotOptions' {outlierVisibility} -> outlierVisibility) (\s@BoxPlotOptions' {} a -> s {outlierVisibility = a} :: BoxPlotOptions)

-- | The style options of the box plot.
boxPlotOptions_styleOptions :: Lens.Lens' BoxPlotOptions (Prelude.Maybe BoxPlotStyleOptions)
boxPlotOptions_styleOptions = Lens.lens (\BoxPlotOptions' {styleOptions} -> styleOptions) (\s@BoxPlotOptions' {} a -> s {styleOptions = a} :: BoxPlotOptions)

instance Data.FromJSON BoxPlotOptions where
  parseJSON =
    Data.withObject
      "BoxPlotOptions"
      ( \x ->
          BoxPlotOptions'
            Prelude.<$> (x Data..:? "AllDataPointsVisibility")
            Prelude.<*> (x Data..:? "OutlierVisibility")
            Prelude.<*> (x Data..:? "StyleOptions")
      )

instance Prelude.Hashable BoxPlotOptions where
  hashWithSalt _salt BoxPlotOptions' {..} =
    _salt
      `Prelude.hashWithSalt` allDataPointsVisibility
      `Prelude.hashWithSalt` outlierVisibility
      `Prelude.hashWithSalt` styleOptions

instance Prelude.NFData BoxPlotOptions where
  rnf BoxPlotOptions' {..} =
    Prelude.rnf allDataPointsVisibility
      `Prelude.seq` Prelude.rnf outlierVisibility
      `Prelude.seq` Prelude.rnf styleOptions

instance Data.ToJSON BoxPlotOptions where
  toJSON BoxPlotOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllDataPointsVisibility" Data..=)
              Prelude.<$> allDataPointsVisibility,
            ("OutlierVisibility" Data..=)
              Prelude.<$> outlierVisibility,
            ("StyleOptions" Data..=) Prelude.<$> styleOptions
          ]
      )
