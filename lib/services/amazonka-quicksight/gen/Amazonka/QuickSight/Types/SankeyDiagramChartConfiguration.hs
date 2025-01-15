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
-- Module      : Amazonka.QuickSight.Types.SankeyDiagramChartConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SankeyDiagramChartConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataLabelOptions
import Amazonka.QuickSight.Types.SankeyDiagramFieldWells
import Amazonka.QuickSight.Types.SankeyDiagramSortConfiguration

-- | The configuration of a sankey diagram.
--
-- /See:/ 'newSankeyDiagramChartConfiguration' smart constructor.
data SankeyDiagramChartConfiguration = SankeyDiagramChartConfiguration'
  { -- | The data label configuration of a sankey diagram.
    dataLabels :: Prelude.Maybe DataLabelOptions,
    -- | The field well configuration of a sankey diagram.
    fieldWells :: Prelude.Maybe SankeyDiagramFieldWells,
    -- | The sort configuration of a sankey diagram.
    sortConfiguration :: Prelude.Maybe SankeyDiagramSortConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SankeyDiagramChartConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataLabels', 'sankeyDiagramChartConfiguration_dataLabels' - The data label configuration of a sankey diagram.
--
-- 'fieldWells', 'sankeyDiagramChartConfiguration_fieldWells' - The field well configuration of a sankey diagram.
--
-- 'sortConfiguration', 'sankeyDiagramChartConfiguration_sortConfiguration' - The sort configuration of a sankey diagram.
newSankeyDiagramChartConfiguration ::
  SankeyDiagramChartConfiguration
newSankeyDiagramChartConfiguration =
  SankeyDiagramChartConfiguration'
    { dataLabels =
        Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing
    }

-- | The data label configuration of a sankey diagram.
sankeyDiagramChartConfiguration_dataLabels :: Lens.Lens' SankeyDiagramChartConfiguration (Prelude.Maybe DataLabelOptions)
sankeyDiagramChartConfiguration_dataLabels = Lens.lens (\SankeyDiagramChartConfiguration' {dataLabels} -> dataLabels) (\s@SankeyDiagramChartConfiguration' {} a -> s {dataLabels = a} :: SankeyDiagramChartConfiguration)

-- | The field well configuration of a sankey diagram.
sankeyDiagramChartConfiguration_fieldWells :: Lens.Lens' SankeyDiagramChartConfiguration (Prelude.Maybe SankeyDiagramFieldWells)
sankeyDiagramChartConfiguration_fieldWells = Lens.lens (\SankeyDiagramChartConfiguration' {fieldWells} -> fieldWells) (\s@SankeyDiagramChartConfiguration' {} a -> s {fieldWells = a} :: SankeyDiagramChartConfiguration)

-- | The sort configuration of a sankey diagram.
sankeyDiagramChartConfiguration_sortConfiguration :: Lens.Lens' SankeyDiagramChartConfiguration (Prelude.Maybe SankeyDiagramSortConfiguration)
sankeyDiagramChartConfiguration_sortConfiguration = Lens.lens (\SankeyDiagramChartConfiguration' {sortConfiguration} -> sortConfiguration) (\s@SankeyDiagramChartConfiguration' {} a -> s {sortConfiguration = a} :: SankeyDiagramChartConfiguration)

instance
  Data.FromJSON
    SankeyDiagramChartConfiguration
  where
  parseJSON =
    Data.withObject
      "SankeyDiagramChartConfiguration"
      ( \x ->
          SankeyDiagramChartConfiguration'
            Prelude.<$> (x Data..:? "DataLabels")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "SortConfiguration")
      )

instance
  Prelude.Hashable
    SankeyDiagramChartConfiguration
  where
  hashWithSalt
    _salt
    SankeyDiagramChartConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` dataLabels
        `Prelude.hashWithSalt` fieldWells
        `Prelude.hashWithSalt` sortConfiguration

instance
  Prelude.NFData
    SankeyDiagramChartConfiguration
  where
  rnf SankeyDiagramChartConfiguration' {..} =
    Prelude.rnf dataLabels `Prelude.seq`
      Prelude.rnf fieldWells `Prelude.seq`
        Prelude.rnf sortConfiguration

instance Data.ToJSON SankeyDiagramChartConfiguration where
  toJSON SankeyDiagramChartConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataLabels" Data..=) Prelude.<$> dataLabels,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("SortConfiguration" Data..=)
              Prelude.<$> sortConfiguration
          ]
      )
