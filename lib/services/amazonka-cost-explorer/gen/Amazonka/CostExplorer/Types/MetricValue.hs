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
-- Module      : Amazonka.CostExplorer.Types.MetricValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.MetricValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The aggregated value for a metric.
--
-- /See:/ 'newMetricValue' smart constructor.
data MetricValue = MetricValue'
  { -- | The actual number that represents the metric.
    amount :: Prelude.Maybe Prelude.Text,
    -- | The unit that the metric is given in.
    unit :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amount', 'metricValue_amount' - The actual number that represents the metric.
--
-- 'unit', 'metricValue_unit' - The unit that the metric is given in.
newMetricValue ::
  MetricValue
newMetricValue =
  MetricValue'
    { amount = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The actual number that represents the metric.
metricValue_amount :: Lens.Lens' MetricValue (Prelude.Maybe Prelude.Text)
metricValue_amount = Lens.lens (\MetricValue' {amount} -> amount) (\s@MetricValue' {} a -> s {amount = a} :: MetricValue)

-- | The unit that the metric is given in.
metricValue_unit :: Lens.Lens' MetricValue (Prelude.Maybe Prelude.Text)
metricValue_unit = Lens.lens (\MetricValue' {unit} -> unit) (\s@MetricValue' {} a -> s {unit = a} :: MetricValue)

instance Data.FromJSON MetricValue where
  parseJSON =
    Data.withObject
      "MetricValue"
      ( \x ->
          MetricValue'
            Prelude.<$> (x Data..:? "Amount")
            Prelude.<*> (x Data..:? "Unit")
      )

instance Prelude.Hashable MetricValue where
  hashWithSalt _salt MetricValue' {..} =
    _salt
      `Prelude.hashWithSalt` amount
      `Prelude.hashWithSalt` unit

instance Prelude.NFData MetricValue where
  rnf MetricValue' {..} =
    Prelude.rnf amount `Prelude.seq` Prelude.rnf unit
