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
-- Module      : Amazonka.QuickSight.Types.SankeyDiagramAggregatedFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SankeyDiagramAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The field well configuration of a sankey diagram.
--
-- /See:/ 'newSankeyDiagramAggregatedFieldWells' smart constructor.
data SankeyDiagramAggregatedFieldWells = SankeyDiagramAggregatedFieldWells'
  { -- | The destination field wells of a sankey diagram.
    destination :: Prelude.Maybe [DimensionField],
    -- | The source field wells of a sankey diagram.
    source :: Prelude.Maybe [DimensionField],
    -- | The weight field wells of a sankey diagram.
    weight :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SankeyDiagramAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'sankeyDiagramAggregatedFieldWells_destination' - The destination field wells of a sankey diagram.
--
-- 'source', 'sankeyDiagramAggregatedFieldWells_source' - The source field wells of a sankey diagram.
--
-- 'weight', 'sankeyDiagramAggregatedFieldWells_weight' - The weight field wells of a sankey diagram.
newSankeyDiagramAggregatedFieldWells ::
  SankeyDiagramAggregatedFieldWells
newSankeyDiagramAggregatedFieldWells =
  SankeyDiagramAggregatedFieldWells'
    { destination =
        Prelude.Nothing,
      source = Prelude.Nothing,
      weight = Prelude.Nothing
    }

-- | The destination field wells of a sankey diagram.
sankeyDiagramAggregatedFieldWells_destination :: Lens.Lens' SankeyDiagramAggregatedFieldWells (Prelude.Maybe [DimensionField])
sankeyDiagramAggregatedFieldWells_destination = Lens.lens (\SankeyDiagramAggregatedFieldWells' {destination} -> destination) (\s@SankeyDiagramAggregatedFieldWells' {} a -> s {destination = a} :: SankeyDiagramAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The source field wells of a sankey diagram.
sankeyDiagramAggregatedFieldWells_source :: Lens.Lens' SankeyDiagramAggregatedFieldWells (Prelude.Maybe [DimensionField])
sankeyDiagramAggregatedFieldWells_source = Lens.lens (\SankeyDiagramAggregatedFieldWells' {source} -> source) (\s@SankeyDiagramAggregatedFieldWells' {} a -> s {source = a} :: SankeyDiagramAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The weight field wells of a sankey diagram.
sankeyDiagramAggregatedFieldWells_weight :: Lens.Lens' SankeyDiagramAggregatedFieldWells (Prelude.Maybe [MeasureField])
sankeyDiagramAggregatedFieldWells_weight = Lens.lens (\SankeyDiagramAggregatedFieldWells' {weight} -> weight) (\s@SankeyDiagramAggregatedFieldWells' {} a -> s {weight = a} :: SankeyDiagramAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    SankeyDiagramAggregatedFieldWells
  where
  parseJSON =
    Data.withObject
      "SankeyDiagramAggregatedFieldWells"
      ( \x ->
          SankeyDiagramAggregatedFieldWells'
            Prelude.<$> (x Data..:? "Destination" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Source" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Weight" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    SankeyDiagramAggregatedFieldWells
  where
  hashWithSalt
    _salt
    SankeyDiagramAggregatedFieldWells' {..} =
      _salt
        `Prelude.hashWithSalt` destination
        `Prelude.hashWithSalt` source
        `Prelude.hashWithSalt` weight

instance
  Prelude.NFData
    SankeyDiagramAggregatedFieldWells
  where
  rnf SankeyDiagramAggregatedFieldWells' {..} =
    Prelude.rnf destination `Prelude.seq`
      Prelude.rnf source `Prelude.seq`
        Prelude.rnf weight

instance
  Data.ToJSON
    SankeyDiagramAggregatedFieldWells
  where
  toJSON SankeyDiagramAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Destination" Data..=) Prelude.<$> destination,
            ("Source" Data..=) Prelude.<$> source,
            ("Weight" Data..=) Prelude.<$> weight
          ]
      )
