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
-- Module      : Amazonka.QuickSight.Types.LineSeriesAxisDisplayOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LineSeriesAxisDisplayOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisDisplayOptions
import Amazonka.QuickSight.Types.MissingDataConfiguration

-- | The series axis configuration of a line chart.
--
-- /See:/ 'newLineSeriesAxisDisplayOptions' smart constructor.
data LineSeriesAxisDisplayOptions = LineSeriesAxisDisplayOptions'
  { -- | The options that determine the presentation of the line series axis.
    axisOptions :: Prelude.Maybe AxisDisplayOptions,
    -- | The configuration options that determine how missing data is treated
    -- during the rendering of a line chart.
    missingDataConfigurations :: Prelude.Maybe [MissingDataConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineSeriesAxisDisplayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'axisOptions', 'lineSeriesAxisDisplayOptions_axisOptions' - The options that determine the presentation of the line series axis.
--
-- 'missingDataConfigurations', 'lineSeriesAxisDisplayOptions_missingDataConfigurations' - The configuration options that determine how missing data is treated
-- during the rendering of a line chart.
newLineSeriesAxisDisplayOptions ::
  LineSeriesAxisDisplayOptions
newLineSeriesAxisDisplayOptions =
  LineSeriesAxisDisplayOptions'
    { axisOptions =
        Prelude.Nothing,
      missingDataConfigurations = Prelude.Nothing
    }

-- | The options that determine the presentation of the line series axis.
lineSeriesAxisDisplayOptions_axisOptions :: Lens.Lens' LineSeriesAxisDisplayOptions (Prelude.Maybe AxisDisplayOptions)
lineSeriesAxisDisplayOptions_axisOptions = Lens.lens (\LineSeriesAxisDisplayOptions' {axisOptions} -> axisOptions) (\s@LineSeriesAxisDisplayOptions' {} a -> s {axisOptions = a} :: LineSeriesAxisDisplayOptions)

-- | The configuration options that determine how missing data is treated
-- during the rendering of a line chart.
lineSeriesAxisDisplayOptions_missingDataConfigurations :: Lens.Lens' LineSeriesAxisDisplayOptions (Prelude.Maybe [MissingDataConfiguration])
lineSeriesAxisDisplayOptions_missingDataConfigurations = Lens.lens (\LineSeriesAxisDisplayOptions' {missingDataConfigurations} -> missingDataConfigurations) (\s@LineSeriesAxisDisplayOptions' {} a -> s {missingDataConfigurations = a} :: LineSeriesAxisDisplayOptions) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LineSeriesAxisDisplayOptions where
  parseJSON =
    Data.withObject
      "LineSeriesAxisDisplayOptions"
      ( \x ->
          LineSeriesAxisDisplayOptions'
            Prelude.<$> (x Data..:? "AxisOptions")
            Prelude.<*> ( x Data..:? "MissingDataConfigurations"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    LineSeriesAxisDisplayOptions
  where
  hashWithSalt _salt LineSeriesAxisDisplayOptions' {..} =
    _salt `Prelude.hashWithSalt` axisOptions
      `Prelude.hashWithSalt` missingDataConfigurations

instance Prelude.NFData LineSeriesAxisDisplayOptions where
  rnf LineSeriesAxisDisplayOptions' {..} =
    Prelude.rnf axisOptions
      `Prelude.seq` Prelude.rnf missingDataConfigurations

instance Data.ToJSON LineSeriesAxisDisplayOptions where
  toJSON LineSeriesAxisDisplayOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AxisOptions" Data..=) Prelude.<$> axisOptions,
            ("MissingDataConfigurations" Data..=)
              Prelude.<$> missingDataConfigurations
          ]
      )
