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
-- Module      : Amazonka.Connect.Types.MetricResultV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.MetricResultV2 where

import Amazonka.Connect.Types.MetricDataV2
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the metric results.
--
-- /See:/ 'newMetricResultV2' smart constructor.
data MetricResultV2 = MetricResultV2'
  { -- | The set of metrics.
    collections :: Prelude.Maybe [MetricDataV2],
    -- | The dimension for the metrics.
    dimensions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricResultV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collections', 'metricResultV2_collections' - The set of metrics.
--
-- 'dimensions', 'metricResultV2_dimensions' - The dimension for the metrics.
newMetricResultV2 ::
  MetricResultV2
newMetricResultV2 =
  MetricResultV2'
    { collections = Prelude.Nothing,
      dimensions = Prelude.Nothing
    }

-- | The set of metrics.
metricResultV2_collections :: Lens.Lens' MetricResultV2 (Prelude.Maybe [MetricDataV2])
metricResultV2_collections = Lens.lens (\MetricResultV2' {collections} -> collections) (\s@MetricResultV2' {} a -> s {collections = a} :: MetricResultV2) Prelude.. Lens.mapping Lens.coerced

-- | The dimension for the metrics.
metricResultV2_dimensions :: Lens.Lens' MetricResultV2 (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
metricResultV2_dimensions = Lens.lens (\MetricResultV2' {dimensions} -> dimensions) (\s@MetricResultV2' {} a -> s {dimensions = a} :: MetricResultV2) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MetricResultV2 where
  parseJSON =
    Data.withObject
      "MetricResultV2"
      ( \x ->
          MetricResultV2'
            Prelude.<$> (x Data..:? "Collections" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Dimensions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable MetricResultV2 where
  hashWithSalt _salt MetricResultV2' {..} =
    _salt
      `Prelude.hashWithSalt` collections
      `Prelude.hashWithSalt` dimensions

instance Prelude.NFData MetricResultV2 where
  rnf MetricResultV2' {..} =
    Prelude.rnf collections
      `Prelude.seq` Prelude.rnf dimensions
