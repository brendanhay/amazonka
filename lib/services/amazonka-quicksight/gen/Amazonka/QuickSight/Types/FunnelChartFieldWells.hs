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
-- Module      : Amazonka.QuickSight.Types.FunnelChartFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FunnelChartFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FunnelChartAggregatedFieldWells

-- | The field well configuration of a @FunnelChartVisual@.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newFunnelChartFieldWells' smart constructor.
data FunnelChartFieldWells = FunnelChartFieldWells'
  { -- | The field well configuration of a @FunnelChartVisual@.
    funnelChartAggregatedFieldWells :: Prelude.Maybe FunnelChartAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunnelChartFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'funnelChartAggregatedFieldWells', 'funnelChartFieldWells_funnelChartAggregatedFieldWells' - The field well configuration of a @FunnelChartVisual@.
newFunnelChartFieldWells ::
  FunnelChartFieldWells
newFunnelChartFieldWells =
  FunnelChartFieldWells'
    { funnelChartAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The field well configuration of a @FunnelChartVisual@.
funnelChartFieldWells_funnelChartAggregatedFieldWells :: Lens.Lens' FunnelChartFieldWells (Prelude.Maybe FunnelChartAggregatedFieldWells)
funnelChartFieldWells_funnelChartAggregatedFieldWells = Lens.lens (\FunnelChartFieldWells' {funnelChartAggregatedFieldWells} -> funnelChartAggregatedFieldWells) (\s@FunnelChartFieldWells' {} a -> s {funnelChartAggregatedFieldWells = a} :: FunnelChartFieldWells)

instance Data.FromJSON FunnelChartFieldWells where
  parseJSON =
    Data.withObject
      "FunnelChartFieldWells"
      ( \x ->
          FunnelChartFieldWells'
            Prelude.<$> (x Data..:? "FunnelChartAggregatedFieldWells")
      )

instance Prelude.Hashable FunnelChartFieldWells where
  hashWithSalt _salt FunnelChartFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` funnelChartAggregatedFieldWells

instance Prelude.NFData FunnelChartFieldWells where
  rnf FunnelChartFieldWells' {..} =
    Prelude.rnf funnelChartAggregatedFieldWells

instance Data.ToJSON FunnelChartFieldWells where
  toJSON FunnelChartFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FunnelChartAggregatedFieldWells" Data..=)
              Prelude.<$> funnelChartAggregatedFieldWells
          ]
      )
