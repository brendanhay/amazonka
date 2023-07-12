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
-- Module      : Amazonka.QuickSight.Types.SankeyDiagramFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SankeyDiagramFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SankeyDiagramAggregatedFieldWells

-- | The field well configuration of a sankey diagram.
--
-- /See:/ 'newSankeyDiagramFieldWells' smart constructor.
data SankeyDiagramFieldWells = SankeyDiagramFieldWells'
  { -- | The field well configuration of a sankey diagram.
    sankeyDiagramAggregatedFieldWells :: Prelude.Maybe SankeyDiagramAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SankeyDiagramFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sankeyDiagramAggregatedFieldWells', 'sankeyDiagramFieldWells_sankeyDiagramAggregatedFieldWells' - The field well configuration of a sankey diagram.
newSankeyDiagramFieldWells ::
  SankeyDiagramFieldWells
newSankeyDiagramFieldWells =
  SankeyDiagramFieldWells'
    { sankeyDiagramAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The field well configuration of a sankey diagram.
sankeyDiagramFieldWells_sankeyDiagramAggregatedFieldWells :: Lens.Lens' SankeyDiagramFieldWells (Prelude.Maybe SankeyDiagramAggregatedFieldWells)
sankeyDiagramFieldWells_sankeyDiagramAggregatedFieldWells = Lens.lens (\SankeyDiagramFieldWells' {sankeyDiagramAggregatedFieldWells} -> sankeyDiagramAggregatedFieldWells) (\s@SankeyDiagramFieldWells' {} a -> s {sankeyDiagramAggregatedFieldWells = a} :: SankeyDiagramFieldWells)

instance Data.FromJSON SankeyDiagramFieldWells where
  parseJSON =
    Data.withObject
      "SankeyDiagramFieldWells"
      ( \x ->
          SankeyDiagramFieldWells'
            Prelude.<$> (x Data..:? "SankeyDiagramAggregatedFieldWells")
      )

instance Prelude.Hashable SankeyDiagramFieldWells where
  hashWithSalt _salt SankeyDiagramFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` sankeyDiagramAggregatedFieldWells

instance Prelude.NFData SankeyDiagramFieldWells where
  rnf SankeyDiagramFieldWells' {..} =
    Prelude.rnf sankeyDiagramAggregatedFieldWells

instance Data.ToJSON SankeyDiagramFieldWells where
  toJSON SankeyDiagramFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SankeyDiagramAggregatedFieldWells" Data..=)
              Prelude.<$> sankeyDiagramAggregatedFieldWells
          ]
      )
