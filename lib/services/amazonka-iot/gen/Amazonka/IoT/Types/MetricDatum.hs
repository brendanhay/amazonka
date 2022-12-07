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
-- Module      : Amazonka.IoT.Types.MetricDatum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.MetricDatum where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.MetricValue
import qualified Amazonka.Prelude as Prelude

-- | A metric.
--
-- /See:/ 'newMetricDatum' smart constructor.
data MetricDatum = MetricDatum'
  { -- | The time the metric value was reported.
    timestamp :: Prelude.Maybe Data.POSIX,
    -- | The value reported for the metric.
    value :: Prelude.Maybe MetricValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDatum' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'metricDatum_timestamp' - The time the metric value was reported.
--
-- 'value', 'metricDatum_value' - The value reported for the metric.
newMetricDatum ::
  MetricDatum
newMetricDatum =
  MetricDatum'
    { timestamp = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The time the metric value was reported.
metricDatum_timestamp :: Lens.Lens' MetricDatum (Prelude.Maybe Prelude.UTCTime)
metricDatum_timestamp = Lens.lens (\MetricDatum' {timestamp} -> timestamp) (\s@MetricDatum' {} a -> s {timestamp = a} :: MetricDatum) Prelude.. Lens.mapping Data._Time

-- | The value reported for the metric.
metricDatum_value :: Lens.Lens' MetricDatum (Prelude.Maybe MetricValue)
metricDatum_value = Lens.lens (\MetricDatum' {value} -> value) (\s@MetricDatum' {} a -> s {value = a} :: MetricDatum)

instance Data.FromJSON MetricDatum where
  parseJSON =
    Data.withObject
      "MetricDatum"
      ( \x ->
          MetricDatum'
            Prelude.<$> (x Data..:? "timestamp")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable MetricDatum where
  hashWithSalt _salt MetricDatum' {..} =
    _salt `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` value

instance Prelude.NFData MetricDatum where
  rnf MetricDatum' {..} =
    Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf value
