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
-- Module      : Amazonka.QuickSight.Types.DonutOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DonutOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ArcOptions
import Amazonka.QuickSight.Types.DonutCenterOptions

-- | The options for configuring a donut chart or pie chart.
--
-- /See:/ 'newDonutOptions' smart constructor.
data DonutOptions = DonutOptions'
  { -- | The option for define the arc of the chart shape. Valid values are as
    -- follows:
    --
    -- -   @WHOLE@ - A pie chart
    --
    -- -   @SMALL@- A small-sized donut chart
    --
    -- -   @MEDIUM@- A medium-sized donut chart
    --
    -- -   @LARGE@- A large-sized donut chart
    arcOptions :: Prelude.Maybe ArcOptions,
    -- | The label options of the label that is displayed in the center of a
    -- donut chart. This option isn\'t available for pie charts.
    donutCenterOptions :: Prelude.Maybe DonutCenterOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DonutOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arcOptions', 'donutOptions_arcOptions' - The option for define the arc of the chart shape. Valid values are as
-- follows:
--
-- -   @WHOLE@ - A pie chart
--
-- -   @SMALL@- A small-sized donut chart
--
-- -   @MEDIUM@- A medium-sized donut chart
--
-- -   @LARGE@- A large-sized donut chart
--
-- 'donutCenterOptions', 'donutOptions_donutCenterOptions' - The label options of the label that is displayed in the center of a
-- donut chart. This option isn\'t available for pie charts.
newDonutOptions ::
  DonutOptions
newDonutOptions =
  DonutOptions'
    { arcOptions = Prelude.Nothing,
      donutCenterOptions = Prelude.Nothing
    }

-- | The option for define the arc of the chart shape. Valid values are as
-- follows:
--
-- -   @WHOLE@ - A pie chart
--
-- -   @SMALL@- A small-sized donut chart
--
-- -   @MEDIUM@- A medium-sized donut chart
--
-- -   @LARGE@- A large-sized donut chart
donutOptions_arcOptions :: Lens.Lens' DonutOptions (Prelude.Maybe ArcOptions)
donutOptions_arcOptions = Lens.lens (\DonutOptions' {arcOptions} -> arcOptions) (\s@DonutOptions' {} a -> s {arcOptions = a} :: DonutOptions)

-- | The label options of the label that is displayed in the center of a
-- donut chart. This option isn\'t available for pie charts.
donutOptions_donutCenterOptions :: Lens.Lens' DonutOptions (Prelude.Maybe DonutCenterOptions)
donutOptions_donutCenterOptions = Lens.lens (\DonutOptions' {donutCenterOptions} -> donutCenterOptions) (\s@DonutOptions' {} a -> s {donutCenterOptions = a} :: DonutOptions)

instance Data.FromJSON DonutOptions where
  parseJSON =
    Data.withObject
      "DonutOptions"
      ( \x ->
          DonutOptions'
            Prelude.<$> (x Data..:? "ArcOptions")
            Prelude.<*> (x Data..:? "DonutCenterOptions")
      )

instance Prelude.Hashable DonutOptions where
  hashWithSalt _salt DonutOptions' {..} =
    _salt
      `Prelude.hashWithSalt` arcOptions
      `Prelude.hashWithSalt` donutCenterOptions

instance Prelude.NFData DonutOptions where
  rnf DonutOptions' {..} =
    Prelude.rnf arcOptions `Prelude.seq`
      Prelude.rnf donutCenterOptions

instance Data.ToJSON DonutOptions where
  toJSON DonutOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ArcOptions" Data..=) Prelude.<$> arcOptions,
            ("DonutCenterOptions" Data..=)
              Prelude.<$> donutCenterOptions
          ]
      )
