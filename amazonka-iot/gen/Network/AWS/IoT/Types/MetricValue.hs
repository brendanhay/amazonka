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
-- Module      : Network.AWS.IoT.Types.MetricValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MetricValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The value to be compared with the @metric@.
--
-- /See:/ 'newMetricValue' smart constructor.
data MetricValue = MetricValue'
  { -- | The numeral values of a metric.
    numbers :: Core.Maybe [Core.Double],
    -- | If the @comparisonOperator@ calls for a set of ports, use this to
    -- specify that set to be compared with the @metric@.
    ports :: Core.Maybe [Core.Natural],
    -- | If the @comparisonOperator@ calls for a set of CIDRs, use this to
    -- specify that set to be compared with the @metric@.
    cidrs :: Core.Maybe [Core.Text],
    -- | The string values of a metric.
    strings :: Core.Maybe [Core.Text],
    -- | If the @comparisonOperator@ calls for a numeric value, use this to
    -- specify that numeric value to be compared with the @metric@.
    count :: Core.Maybe Core.Natural,
    -- | The numeral value of a metric.
    number :: Core.Maybe Core.Double
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
-- 'numbers', 'metricValue_numbers' - The numeral values of a metric.
--
-- 'ports', 'metricValue_ports' - If the @comparisonOperator@ calls for a set of ports, use this to
-- specify that set to be compared with the @metric@.
--
-- 'cidrs', 'metricValue_cidrs' - If the @comparisonOperator@ calls for a set of CIDRs, use this to
-- specify that set to be compared with the @metric@.
--
-- 'strings', 'metricValue_strings' - The string values of a metric.
--
-- 'count', 'metricValue_count' - If the @comparisonOperator@ calls for a numeric value, use this to
-- specify that numeric value to be compared with the @metric@.
--
-- 'number', 'metricValue_number' - The numeral value of a metric.
newMetricValue ::
  MetricValue
newMetricValue =
  MetricValue'
    { numbers = Core.Nothing,
      ports = Core.Nothing,
      cidrs = Core.Nothing,
      strings = Core.Nothing,
      count = Core.Nothing,
      number = Core.Nothing
    }

-- | The numeral values of a metric.
metricValue_numbers :: Lens.Lens' MetricValue (Core.Maybe [Core.Double])
metricValue_numbers = Lens.lens (\MetricValue' {numbers} -> numbers) (\s@MetricValue' {} a -> s {numbers = a} :: MetricValue) Core.. Lens.mapping Lens._Coerce

-- | If the @comparisonOperator@ calls for a set of ports, use this to
-- specify that set to be compared with the @metric@.
metricValue_ports :: Lens.Lens' MetricValue (Core.Maybe [Core.Natural])
metricValue_ports = Lens.lens (\MetricValue' {ports} -> ports) (\s@MetricValue' {} a -> s {ports = a} :: MetricValue) Core.. Lens.mapping Lens._Coerce

-- | If the @comparisonOperator@ calls for a set of CIDRs, use this to
-- specify that set to be compared with the @metric@.
metricValue_cidrs :: Lens.Lens' MetricValue (Core.Maybe [Core.Text])
metricValue_cidrs = Lens.lens (\MetricValue' {cidrs} -> cidrs) (\s@MetricValue' {} a -> s {cidrs = a} :: MetricValue) Core.. Lens.mapping Lens._Coerce

-- | The string values of a metric.
metricValue_strings :: Lens.Lens' MetricValue (Core.Maybe [Core.Text])
metricValue_strings = Lens.lens (\MetricValue' {strings} -> strings) (\s@MetricValue' {} a -> s {strings = a} :: MetricValue) Core.. Lens.mapping Lens._Coerce

-- | If the @comparisonOperator@ calls for a numeric value, use this to
-- specify that numeric value to be compared with the @metric@.
metricValue_count :: Lens.Lens' MetricValue (Core.Maybe Core.Natural)
metricValue_count = Lens.lens (\MetricValue' {count} -> count) (\s@MetricValue' {} a -> s {count = a} :: MetricValue)

-- | The numeral value of a metric.
metricValue_number :: Lens.Lens' MetricValue (Core.Maybe Core.Double)
metricValue_number = Lens.lens (\MetricValue' {number} -> number) (\s@MetricValue' {} a -> s {number = a} :: MetricValue)

instance Core.FromJSON MetricValue where
  parseJSON =
    Core.withObject
      "MetricValue"
      ( \x ->
          MetricValue'
            Core.<$> (x Core..:? "numbers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ports" Core..!= Core.mempty)
            Core.<*> (x Core..:? "cidrs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "strings" Core..!= Core.mempty)
            Core.<*> (x Core..:? "count")
            Core.<*> (x Core..:? "number")
      )

instance Core.Hashable MetricValue

instance Core.NFData MetricValue

instance Core.ToJSON MetricValue where
  toJSON MetricValue' {..} =
    Core.object
      ( Core.catMaybes
          [ ("numbers" Core..=) Core.<$> numbers,
            ("ports" Core..=) Core.<$> ports,
            ("cidrs" Core..=) Core.<$> cidrs,
            ("strings" Core..=) Core.<$> strings,
            ("count" Core..=) Core.<$> count,
            ("number" Core..=) Core.<$> number
          ]
      )
