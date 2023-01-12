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
-- Module      : Amazonka.SESV2.Types.MetricDataResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.MetricDataResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The result of a single metric data query.
--
-- /See:/ 'newMetricDataResult' smart constructor.
data MetricDataResult = MetricDataResult'
  { -- | The query identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | A list of timestamps for the metric data results.
    timestamps :: Prelude.Maybe [Data.POSIX],
    -- | A list of values (cumulative \/ sum) for the metric data results.
    values :: Prelude.Maybe [Prelude.Integer]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDataResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'metricDataResult_id' - The query identifier.
--
-- 'timestamps', 'metricDataResult_timestamps' - A list of timestamps for the metric data results.
--
-- 'values', 'metricDataResult_values' - A list of values (cumulative \/ sum) for the metric data results.
newMetricDataResult ::
  MetricDataResult
newMetricDataResult =
  MetricDataResult'
    { id = Prelude.Nothing,
      timestamps = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The query identifier.
metricDataResult_id :: Lens.Lens' MetricDataResult (Prelude.Maybe Prelude.Text)
metricDataResult_id = Lens.lens (\MetricDataResult' {id} -> id) (\s@MetricDataResult' {} a -> s {id = a} :: MetricDataResult)

-- | A list of timestamps for the metric data results.
metricDataResult_timestamps :: Lens.Lens' MetricDataResult (Prelude.Maybe [Prelude.UTCTime])
metricDataResult_timestamps = Lens.lens (\MetricDataResult' {timestamps} -> timestamps) (\s@MetricDataResult' {} a -> s {timestamps = a} :: MetricDataResult) Prelude.. Lens.mapping Lens.coerced

-- | A list of values (cumulative \/ sum) for the metric data results.
metricDataResult_values :: Lens.Lens' MetricDataResult (Prelude.Maybe [Prelude.Integer])
metricDataResult_values = Lens.lens (\MetricDataResult' {values} -> values) (\s@MetricDataResult' {} a -> s {values = a} :: MetricDataResult) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MetricDataResult where
  parseJSON =
    Data.withObject
      "MetricDataResult"
      ( \x ->
          MetricDataResult'
            Prelude.<$> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Timestamps" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable MetricDataResult where
  hashWithSalt _salt MetricDataResult' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` timestamps
      `Prelude.hashWithSalt` values

instance Prelude.NFData MetricDataResult where
  rnf MetricDataResult' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf timestamps
      `Prelude.seq` Prelude.rnf values
