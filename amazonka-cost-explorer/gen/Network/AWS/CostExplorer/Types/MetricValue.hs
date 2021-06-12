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
-- Module      : Network.AWS.CostExplorer.Types.MetricValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.MetricValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The aggregated value for a metric.
--
-- /See:/ 'newMetricValue' smart constructor.
data MetricValue = MetricValue'
  { -- | The actual number that represents the metric.
    amount :: Core.Maybe Core.Text,
    -- | The unit that the metric is given in.
    unit :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { amount = Core.Nothing,
      unit = Core.Nothing
    }

-- | The actual number that represents the metric.
metricValue_amount :: Lens.Lens' MetricValue (Core.Maybe Core.Text)
metricValue_amount = Lens.lens (\MetricValue' {amount} -> amount) (\s@MetricValue' {} a -> s {amount = a} :: MetricValue)

-- | The unit that the metric is given in.
metricValue_unit :: Lens.Lens' MetricValue (Core.Maybe Core.Text)
metricValue_unit = Lens.lens (\MetricValue' {unit} -> unit) (\s@MetricValue' {} a -> s {unit = a} :: MetricValue)

instance Core.FromJSON MetricValue where
  parseJSON =
    Core.withObject
      "MetricValue"
      ( \x ->
          MetricValue'
            Core.<$> (x Core..:? "Amount") Core.<*> (x Core..:? "Unit")
      )

instance Core.Hashable MetricValue

instance Core.NFData MetricValue
