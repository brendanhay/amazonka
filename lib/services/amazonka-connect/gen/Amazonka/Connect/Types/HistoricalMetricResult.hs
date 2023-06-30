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
-- Module      : Amazonka.Connect.Types.HistoricalMetricResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HistoricalMetricResult where

import Amazonka.Connect.Types.Dimensions
import Amazonka.Connect.Types.HistoricalMetricData
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the historical metrics retrieved.
--
-- /See:/ 'newHistoricalMetricResult' smart constructor.
data HistoricalMetricResult = HistoricalMetricResult'
  { -- | The set of metrics.
    collections :: Prelude.Maybe [HistoricalMetricData],
    -- | The dimension for the metrics.
    dimensions :: Prelude.Maybe Dimensions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HistoricalMetricResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collections', 'historicalMetricResult_collections' - The set of metrics.
--
-- 'dimensions', 'historicalMetricResult_dimensions' - The dimension for the metrics.
newHistoricalMetricResult ::
  HistoricalMetricResult
newHistoricalMetricResult =
  HistoricalMetricResult'
    { collections =
        Prelude.Nothing,
      dimensions = Prelude.Nothing
    }

-- | The set of metrics.
historicalMetricResult_collections :: Lens.Lens' HistoricalMetricResult (Prelude.Maybe [HistoricalMetricData])
historicalMetricResult_collections = Lens.lens (\HistoricalMetricResult' {collections} -> collections) (\s@HistoricalMetricResult' {} a -> s {collections = a} :: HistoricalMetricResult) Prelude.. Lens.mapping Lens.coerced

-- | The dimension for the metrics.
historicalMetricResult_dimensions :: Lens.Lens' HistoricalMetricResult (Prelude.Maybe Dimensions)
historicalMetricResult_dimensions = Lens.lens (\HistoricalMetricResult' {dimensions} -> dimensions) (\s@HistoricalMetricResult' {} a -> s {dimensions = a} :: HistoricalMetricResult)

instance Data.FromJSON HistoricalMetricResult where
  parseJSON =
    Data.withObject
      "HistoricalMetricResult"
      ( \x ->
          HistoricalMetricResult'
            Prelude.<$> (x Data..:? "Collections" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Dimensions")
      )

instance Prelude.Hashable HistoricalMetricResult where
  hashWithSalt _salt HistoricalMetricResult' {..} =
    _salt
      `Prelude.hashWithSalt` collections
      `Prelude.hashWithSalt` dimensions

instance Prelude.NFData HistoricalMetricResult where
  rnf HistoricalMetricResult' {..} =
    Prelude.rnf collections
      `Prelude.seq` Prelude.rnf dimensions
