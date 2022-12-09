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
-- Module      : Amazonka.LookoutMetrics.Types.ItemizedMetricStats
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.ItemizedMetricStats where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Aggregated statistics about a measure affected by an anomaly.
--
-- /See:/ 'newItemizedMetricStats' smart constructor.
data ItemizedMetricStats = ItemizedMetricStats'
  { -- | The name of the measure.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The number of times that the measure appears.
    occurrenceCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ItemizedMetricStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'itemizedMetricStats_metricName' - The name of the measure.
--
-- 'occurrenceCount', 'itemizedMetricStats_occurrenceCount' - The number of times that the measure appears.
newItemizedMetricStats ::
  ItemizedMetricStats
newItemizedMetricStats =
  ItemizedMetricStats'
    { metricName = Prelude.Nothing,
      occurrenceCount = Prelude.Nothing
    }

-- | The name of the measure.
itemizedMetricStats_metricName :: Lens.Lens' ItemizedMetricStats (Prelude.Maybe Prelude.Text)
itemizedMetricStats_metricName = Lens.lens (\ItemizedMetricStats' {metricName} -> metricName) (\s@ItemizedMetricStats' {} a -> s {metricName = a} :: ItemizedMetricStats)

-- | The number of times that the measure appears.
itemizedMetricStats_occurrenceCount :: Lens.Lens' ItemizedMetricStats (Prelude.Maybe Prelude.Int)
itemizedMetricStats_occurrenceCount = Lens.lens (\ItemizedMetricStats' {occurrenceCount} -> occurrenceCount) (\s@ItemizedMetricStats' {} a -> s {occurrenceCount = a} :: ItemizedMetricStats)

instance Data.FromJSON ItemizedMetricStats where
  parseJSON =
    Data.withObject
      "ItemizedMetricStats"
      ( \x ->
          ItemizedMetricStats'
            Prelude.<$> (x Data..:? "MetricName")
            Prelude.<*> (x Data..:? "OccurrenceCount")
      )

instance Prelude.Hashable ItemizedMetricStats where
  hashWithSalt _salt ItemizedMetricStats' {..} =
    _salt `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` occurrenceCount

instance Prelude.NFData ItemizedMetricStats where
  rnf ItemizedMetricStats' {..} =
    Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf occurrenceCount
