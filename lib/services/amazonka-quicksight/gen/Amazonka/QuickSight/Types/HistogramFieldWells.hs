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
-- Module      : Amazonka.QuickSight.Types.HistogramFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.HistogramFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.HistogramAggregatedFieldWells

-- | The field well configuration of a histogram.
--
-- /See:/ 'newHistogramFieldWells' smart constructor.
data HistogramFieldWells = HistogramFieldWells'
  { -- | The field well configuration of a histogram.
    histogramAggregatedFieldWells :: Prelude.Maybe HistogramAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HistogramFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'histogramAggregatedFieldWells', 'histogramFieldWells_histogramAggregatedFieldWells' - The field well configuration of a histogram.
newHistogramFieldWells ::
  HistogramFieldWells
newHistogramFieldWells =
  HistogramFieldWells'
    { histogramAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The field well configuration of a histogram.
histogramFieldWells_histogramAggregatedFieldWells :: Lens.Lens' HistogramFieldWells (Prelude.Maybe HistogramAggregatedFieldWells)
histogramFieldWells_histogramAggregatedFieldWells = Lens.lens (\HistogramFieldWells' {histogramAggregatedFieldWells} -> histogramAggregatedFieldWells) (\s@HistogramFieldWells' {} a -> s {histogramAggregatedFieldWells = a} :: HistogramFieldWells)

instance Data.FromJSON HistogramFieldWells where
  parseJSON =
    Data.withObject
      "HistogramFieldWells"
      ( \x ->
          HistogramFieldWells'
            Prelude.<$> (x Data..:? "HistogramAggregatedFieldWells")
      )

instance Prelude.Hashable HistogramFieldWells where
  hashWithSalt _salt HistogramFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` histogramAggregatedFieldWells

instance Prelude.NFData HistogramFieldWells where
  rnf HistogramFieldWells' {..} =
    Prelude.rnf histogramAggregatedFieldWells

instance Data.ToJSON HistogramFieldWells where
  toJSON HistogramFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HistogramAggregatedFieldWells" Data..=)
              Prelude.<$> histogramAggregatedFieldWells
          ]
      )
