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
-- Module      : Amazonka.QuickSight.Types.SankeyDiagramSortConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SankeyDiagramSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldSortOptions
import Amazonka.QuickSight.Types.ItemsLimitConfiguration

-- | The sort configuration of a sankey diagram.
--
-- /See:/ 'newSankeyDiagramSortConfiguration' smart constructor.
data SankeyDiagramSortConfiguration = SankeyDiagramSortConfiguration'
  { -- | The limit on the number of destination nodes that are displayed in a
    -- sankey diagram.
    destinationItemsLimit :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The limit on the number of source nodes that are displayed in a sankey
    -- diagram.
    sourceItemsLimit :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The sort configuration of the weight fields.
    weightSort :: Prelude.Maybe [FieldSortOptions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SankeyDiagramSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationItemsLimit', 'sankeyDiagramSortConfiguration_destinationItemsLimit' - The limit on the number of destination nodes that are displayed in a
-- sankey diagram.
--
-- 'sourceItemsLimit', 'sankeyDiagramSortConfiguration_sourceItemsLimit' - The limit on the number of source nodes that are displayed in a sankey
-- diagram.
--
-- 'weightSort', 'sankeyDiagramSortConfiguration_weightSort' - The sort configuration of the weight fields.
newSankeyDiagramSortConfiguration ::
  SankeyDiagramSortConfiguration
newSankeyDiagramSortConfiguration =
  SankeyDiagramSortConfiguration'
    { destinationItemsLimit =
        Prelude.Nothing,
      sourceItemsLimit = Prelude.Nothing,
      weightSort = Prelude.Nothing
    }

-- | The limit on the number of destination nodes that are displayed in a
-- sankey diagram.
sankeyDiagramSortConfiguration_destinationItemsLimit :: Lens.Lens' SankeyDiagramSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
sankeyDiagramSortConfiguration_destinationItemsLimit = Lens.lens (\SankeyDiagramSortConfiguration' {destinationItemsLimit} -> destinationItemsLimit) (\s@SankeyDiagramSortConfiguration' {} a -> s {destinationItemsLimit = a} :: SankeyDiagramSortConfiguration)

-- | The limit on the number of source nodes that are displayed in a sankey
-- diagram.
sankeyDiagramSortConfiguration_sourceItemsLimit :: Lens.Lens' SankeyDiagramSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
sankeyDiagramSortConfiguration_sourceItemsLimit = Lens.lens (\SankeyDiagramSortConfiguration' {sourceItemsLimit} -> sourceItemsLimit) (\s@SankeyDiagramSortConfiguration' {} a -> s {sourceItemsLimit = a} :: SankeyDiagramSortConfiguration)

-- | The sort configuration of the weight fields.
sankeyDiagramSortConfiguration_weightSort :: Lens.Lens' SankeyDiagramSortConfiguration (Prelude.Maybe [FieldSortOptions])
sankeyDiagramSortConfiguration_weightSort = Lens.lens (\SankeyDiagramSortConfiguration' {weightSort} -> weightSort) (\s@SankeyDiagramSortConfiguration' {} a -> s {weightSort = a} :: SankeyDiagramSortConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SankeyDiagramSortConfiguration where
  parseJSON =
    Data.withObject
      "SankeyDiagramSortConfiguration"
      ( \x ->
          SankeyDiagramSortConfiguration'
            Prelude.<$> (x Data..:? "DestinationItemsLimit")
            Prelude.<*> (x Data..:? "SourceItemsLimit")
            Prelude.<*> (x Data..:? "WeightSort" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    SankeyDiagramSortConfiguration
  where
  hashWithSalt
    _salt
    SankeyDiagramSortConfiguration' {..} =
      _salt `Prelude.hashWithSalt` destinationItemsLimit
        `Prelude.hashWithSalt` sourceItemsLimit
        `Prelude.hashWithSalt` weightSort

instance
  Prelude.NFData
    SankeyDiagramSortConfiguration
  where
  rnf SankeyDiagramSortConfiguration' {..} =
    Prelude.rnf destinationItemsLimit
      `Prelude.seq` Prelude.rnf sourceItemsLimit
      `Prelude.seq` Prelude.rnf weightSort

instance Data.ToJSON SankeyDiagramSortConfiguration where
  toJSON SankeyDiagramSortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationItemsLimit" Data..=)
              Prelude.<$> destinationItemsLimit,
            ("SourceItemsLimit" Data..=)
              Prelude.<$> sourceItemsLimit,
            ("WeightSort" Data..=) Prelude.<$> weightSort
          ]
      )
