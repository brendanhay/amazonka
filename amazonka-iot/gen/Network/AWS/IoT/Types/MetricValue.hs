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
import qualified Network.AWS.Prelude as Prelude

-- | The value to be compared with the @metric@.
--
-- /See:/ 'newMetricValue' smart constructor.
data MetricValue = MetricValue'
  { -- | The numeral values of a metric.
    numbers :: Prelude.Maybe [Prelude.Double],
    -- | If the @comparisonOperator@ calls for a set of ports, use this to
    -- specify that set to be compared with the @metric@.
    ports :: Prelude.Maybe [Prelude.Natural],
    -- | If the @comparisonOperator@ calls for a set of CIDRs, use this to
    -- specify that set to be compared with the @metric@.
    cidrs :: Prelude.Maybe [Prelude.Text],
    -- | The string values of a metric.
    strings :: Prelude.Maybe [Prelude.Text],
    -- | The numeral value of a metric.
    number :: Prelude.Maybe Prelude.Double,
    -- | If the @comparisonOperator@ calls for a numeric value, use this to
    -- specify that numeric value to be compared with the @metric@.
    count :: Prelude.Maybe Prelude.Natural
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
-- 'number', 'metricValue_number' - The numeral value of a metric.
--
-- 'count', 'metricValue_count' - If the @comparisonOperator@ calls for a numeric value, use this to
-- specify that numeric value to be compared with the @metric@.
newMetricValue ::
  MetricValue
newMetricValue =
  MetricValue'
    { numbers = Prelude.Nothing,
      ports = Prelude.Nothing,
      cidrs = Prelude.Nothing,
      strings = Prelude.Nothing,
      number = Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | The numeral values of a metric.
metricValue_numbers :: Lens.Lens' MetricValue (Prelude.Maybe [Prelude.Double])
metricValue_numbers = Lens.lens (\MetricValue' {numbers} -> numbers) (\s@MetricValue' {} a -> s {numbers = a} :: MetricValue) Prelude.. Lens.mapping Lens._Coerce

-- | If the @comparisonOperator@ calls for a set of ports, use this to
-- specify that set to be compared with the @metric@.
metricValue_ports :: Lens.Lens' MetricValue (Prelude.Maybe [Prelude.Natural])
metricValue_ports = Lens.lens (\MetricValue' {ports} -> ports) (\s@MetricValue' {} a -> s {ports = a} :: MetricValue) Prelude.. Lens.mapping Lens._Coerce

-- | If the @comparisonOperator@ calls for a set of CIDRs, use this to
-- specify that set to be compared with the @metric@.
metricValue_cidrs :: Lens.Lens' MetricValue (Prelude.Maybe [Prelude.Text])
metricValue_cidrs = Lens.lens (\MetricValue' {cidrs} -> cidrs) (\s@MetricValue' {} a -> s {cidrs = a} :: MetricValue) Prelude.. Lens.mapping Lens._Coerce

-- | The string values of a metric.
metricValue_strings :: Lens.Lens' MetricValue (Prelude.Maybe [Prelude.Text])
metricValue_strings = Lens.lens (\MetricValue' {strings} -> strings) (\s@MetricValue' {} a -> s {strings = a} :: MetricValue) Prelude.. Lens.mapping Lens._Coerce

-- | The numeral value of a metric.
metricValue_number :: Lens.Lens' MetricValue (Prelude.Maybe Prelude.Double)
metricValue_number = Lens.lens (\MetricValue' {number} -> number) (\s@MetricValue' {} a -> s {number = a} :: MetricValue)

-- | If the @comparisonOperator@ calls for a numeric value, use this to
-- specify that numeric value to be compared with the @metric@.
metricValue_count :: Lens.Lens' MetricValue (Prelude.Maybe Prelude.Natural)
metricValue_count = Lens.lens (\MetricValue' {count} -> count) (\s@MetricValue' {} a -> s {count = a} :: MetricValue)

instance Core.FromJSON MetricValue where
  parseJSON =
    Core.withObject
      "MetricValue"
      ( \x ->
          MetricValue'
            Prelude.<$> (x Core..:? "numbers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ports" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "cidrs" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "strings" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "number")
            Prelude.<*> (x Core..:? "count")
      )

instance Prelude.Hashable MetricValue

instance Prelude.NFData MetricValue

instance Core.ToJSON MetricValue where
  toJSON MetricValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("numbers" Core..=) Prelude.<$> numbers,
            ("ports" Core..=) Prelude.<$> ports,
            ("cidrs" Core..=) Prelude.<$> cidrs,
            ("strings" Core..=) Prelude.<$> strings,
            ("number" Core..=) Prelude.<$> number,
            ("count" Core..=) Prelude.<$> count
          ]
      )
