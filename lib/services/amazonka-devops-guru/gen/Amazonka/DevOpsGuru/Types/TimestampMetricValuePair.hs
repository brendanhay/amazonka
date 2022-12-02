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
-- Module      : Amazonka.DevOpsGuru.Types.TimestampMetricValuePair
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.TimestampMetricValuePair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A pair that contains metric values at the respective timestamp.
--
-- /See:/ 'newTimestampMetricValuePair' smart constructor.
data TimestampMetricValuePair = TimestampMetricValuePair'
  { -- | Value of the anomalous metric data point at respective Timestamp.
    metricValue :: Prelude.Maybe Prelude.Double,
    -- | A @Timestamp@ that specifies the time the event occurred.
    timestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimestampMetricValuePair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricValue', 'timestampMetricValuePair_metricValue' - Value of the anomalous metric data point at respective Timestamp.
--
-- 'timestamp', 'timestampMetricValuePair_timestamp' - A @Timestamp@ that specifies the time the event occurred.
newTimestampMetricValuePair ::
  TimestampMetricValuePair
newTimestampMetricValuePair =
  TimestampMetricValuePair'
    { metricValue =
        Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | Value of the anomalous metric data point at respective Timestamp.
timestampMetricValuePair_metricValue :: Lens.Lens' TimestampMetricValuePair (Prelude.Maybe Prelude.Double)
timestampMetricValuePair_metricValue = Lens.lens (\TimestampMetricValuePair' {metricValue} -> metricValue) (\s@TimestampMetricValuePair' {} a -> s {metricValue = a} :: TimestampMetricValuePair)

-- | A @Timestamp@ that specifies the time the event occurred.
timestampMetricValuePair_timestamp :: Lens.Lens' TimestampMetricValuePair (Prelude.Maybe Prelude.UTCTime)
timestampMetricValuePair_timestamp = Lens.lens (\TimestampMetricValuePair' {timestamp} -> timestamp) (\s@TimestampMetricValuePair' {} a -> s {timestamp = a} :: TimestampMetricValuePair) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON TimestampMetricValuePair where
  parseJSON =
    Data.withObject
      "TimestampMetricValuePair"
      ( \x ->
          TimestampMetricValuePair'
            Prelude.<$> (x Data..:? "MetricValue")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable TimestampMetricValuePair where
  hashWithSalt _salt TimestampMetricValuePair' {..} =
    _salt `Prelude.hashWithSalt` metricValue
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData TimestampMetricValuePair where
  rnf TimestampMetricValuePair' {..} =
    Prelude.rnf metricValue
      `Prelude.seq` Prelude.rnf timestamp
