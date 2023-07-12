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
-- Module      : Amazonka.QuickSight.Types.ScatterPlotFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ScatterPlotFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ScatterPlotCategoricallyAggregatedFieldWells
import Amazonka.QuickSight.Types.ScatterPlotUnaggregatedFieldWells

-- | The field well configuration of a scatter plot.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newScatterPlotFieldWells' smart constructor.
data ScatterPlotFieldWells = ScatterPlotFieldWells'
  { -- | The aggregated field wells of a scatter plot. Scatter plots that have a
    -- field in the category (group\/color) field will have aggregated field
    -- wells. The x and y-axes of these scatter plots are aggregated by
    -- category.
    scatterPlotCategoricallyAggregatedFieldWells :: Prelude.Maybe ScatterPlotCategoricallyAggregatedFieldWells,
    -- | The unaggregated field wells of a scatter plot. Scatter plots without a
    -- category field well have unaggregated field wells. The x and y-axes of
    -- these scatter plots are unaggregated.
    scatterPlotUnaggregatedFieldWells :: Prelude.Maybe ScatterPlotUnaggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScatterPlotFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scatterPlotCategoricallyAggregatedFieldWells', 'scatterPlotFieldWells_scatterPlotCategoricallyAggregatedFieldWells' - The aggregated field wells of a scatter plot. Scatter plots that have a
-- field in the category (group\/color) field will have aggregated field
-- wells. The x and y-axes of these scatter plots are aggregated by
-- category.
--
-- 'scatterPlotUnaggregatedFieldWells', 'scatterPlotFieldWells_scatterPlotUnaggregatedFieldWells' - The unaggregated field wells of a scatter plot. Scatter plots without a
-- category field well have unaggregated field wells. The x and y-axes of
-- these scatter plots are unaggregated.
newScatterPlotFieldWells ::
  ScatterPlotFieldWells
newScatterPlotFieldWells =
  ScatterPlotFieldWells'
    { scatterPlotCategoricallyAggregatedFieldWells =
        Prelude.Nothing,
      scatterPlotUnaggregatedFieldWells = Prelude.Nothing
    }

-- | The aggregated field wells of a scatter plot. Scatter plots that have a
-- field in the category (group\/color) field will have aggregated field
-- wells. The x and y-axes of these scatter plots are aggregated by
-- category.
scatterPlotFieldWells_scatterPlotCategoricallyAggregatedFieldWells :: Lens.Lens' ScatterPlotFieldWells (Prelude.Maybe ScatterPlotCategoricallyAggregatedFieldWells)
scatterPlotFieldWells_scatterPlotCategoricallyAggregatedFieldWells = Lens.lens (\ScatterPlotFieldWells' {scatterPlotCategoricallyAggregatedFieldWells} -> scatterPlotCategoricallyAggregatedFieldWells) (\s@ScatterPlotFieldWells' {} a -> s {scatterPlotCategoricallyAggregatedFieldWells = a} :: ScatterPlotFieldWells)

-- | The unaggregated field wells of a scatter plot. Scatter plots without a
-- category field well have unaggregated field wells. The x and y-axes of
-- these scatter plots are unaggregated.
scatterPlotFieldWells_scatterPlotUnaggregatedFieldWells :: Lens.Lens' ScatterPlotFieldWells (Prelude.Maybe ScatterPlotUnaggregatedFieldWells)
scatterPlotFieldWells_scatterPlotUnaggregatedFieldWells = Lens.lens (\ScatterPlotFieldWells' {scatterPlotUnaggregatedFieldWells} -> scatterPlotUnaggregatedFieldWells) (\s@ScatterPlotFieldWells' {} a -> s {scatterPlotUnaggregatedFieldWells = a} :: ScatterPlotFieldWells)

instance Data.FromJSON ScatterPlotFieldWells where
  parseJSON =
    Data.withObject
      "ScatterPlotFieldWells"
      ( \x ->
          ScatterPlotFieldWells'
            Prelude.<$> ( x
                            Data..:? "ScatterPlotCategoricallyAggregatedFieldWells"
                        )
            Prelude.<*> (x Data..:? "ScatterPlotUnaggregatedFieldWells")
      )

instance Prelude.Hashable ScatterPlotFieldWells where
  hashWithSalt _salt ScatterPlotFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` scatterPlotCategoricallyAggregatedFieldWells
      `Prelude.hashWithSalt` scatterPlotUnaggregatedFieldWells

instance Prelude.NFData ScatterPlotFieldWells where
  rnf ScatterPlotFieldWells' {..} =
    Prelude.rnf
      scatterPlotCategoricallyAggregatedFieldWells
      `Prelude.seq` Prelude.rnf scatterPlotUnaggregatedFieldWells

instance Data.ToJSON ScatterPlotFieldWells where
  toJSON ScatterPlotFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ( "ScatterPlotCategoricallyAggregatedFieldWells"
                Data..=
            )
              Prelude.<$> scatterPlotCategoricallyAggregatedFieldWells,
            ("ScatterPlotUnaggregatedFieldWells" Data..=)
              Prelude.<$> scatterPlotUnaggregatedFieldWells
          ]
      )
