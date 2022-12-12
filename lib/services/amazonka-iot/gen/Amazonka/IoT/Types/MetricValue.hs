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
-- Module      : Amazonka.IoT.Types.MetricValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.MetricValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The value to be compared with the @metric@.
--
-- /See:/ 'newMetricValue' smart constructor.
data MetricValue = MetricValue'
  { -- | If the @comparisonOperator@ calls for a set of CIDRs, use this to
    -- specify that set to be compared with the @metric@.
    cidrs :: Prelude.Maybe [Prelude.Text],
    -- | If the @comparisonOperator@ calls for a numeric value, use this to
    -- specify that numeric value to be compared with the @metric@.
    count :: Prelude.Maybe Prelude.Natural,
    -- | The numeral value of a metric.
    number :: Prelude.Maybe Prelude.Double,
    -- | The numeral values of a metric.
    numbers :: Prelude.Maybe [Prelude.Double],
    -- | If the @comparisonOperator@ calls for a set of ports, use this to
    -- specify that set to be compared with the @metric@.
    ports :: Prelude.Maybe [Prelude.Natural],
    -- | The string values of a metric.
    strings :: Prelude.Maybe [Prelude.Text]
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
-- 'cidrs', 'metricValue_cidrs' - If the @comparisonOperator@ calls for a set of CIDRs, use this to
-- specify that set to be compared with the @metric@.
--
-- 'count', 'metricValue_count' - If the @comparisonOperator@ calls for a numeric value, use this to
-- specify that numeric value to be compared with the @metric@.
--
-- 'number', 'metricValue_number' - The numeral value of a metric.
--
-- 'numbers', 'metricValue_numbers' - The numeral values of a metric.
--
-- 'ports', 'metricValue_ports' - If the @comparisonOperator@ calls for a set of ports, use this to
-- specify that set to be compared with the @metric@.
--
-- 'strings', 'metricValue_strings' - The string values of a metric.
newMetricValue ::
  MetricValue
newMetricValue =
  MetricValue'
    { cidrs = Prelude.Nothing,
      count = Prelude.Nothing,
      number = Prelude.Nothing,
      numbers = Prelude.Nothing,
      ports = Prelude.Nothing,
      strings = Prelude.Nothing
    }

-- | If the @comparisonOperator@ calls for a set of CIDRs, use this to
-- specify that set to be compared with the @metric@.
metricValue_cidrs :: Lens.Lens' MetricValue (Prelude.Maybe [Prelude.Text])
metricValue_cidrs = Lens.lens (\MetricValue' {cidrs} -> cidrs) (\s@MetricValue' {} a -> s {cidrs = a} :: MetricValue) Prelude.. Lens.mapping Lens.coerced

-- | If the @comparisonOperator@ calls for a numeric value, use this to
-- specify that numeric value to be compared with the @metric@.
metricValue_count :: Lens.Lens' MetricValue (Prelude.Maybe Prelude.Natural)
metricValue_count = Lens.lens (\MetricValue' {count} -> count) (\s@MetricValue' {} a -> s {count = a} :: MetricValue)

-- | The numeral value of a metric.
metricValue_number :: Lens.Lens' MetricValue (Prelude.Maybe Prelude.Double)
metricValue_number = Lens.lens (\MetricValue' {number} -> number) (\s@MetricValue' {} a -> s {number = a} :: MetricValue)

-- | The numeral values of a metric.
metricValue_numbers :: Lens.Lens' MetricValue (Prelude.Maybe [Prelude.Double])
metricValue_numbers = Lens.lens (\MetricValue' {numbers} -> numbers) (\s@MetricValue' {} a -> s {numbers = a} :: MetricValue) Prelude.. Lens.mapping Lens.coerced

-- | If the @comparisonOperator@ calls for a set of ports, use this to
-- specify that set to be compared with the @metric@.
metricValue_ports :: Lens.Lens' MetricValue (Prelude.Maybe [Prelude.Natural])
metricValue_ports = Lens.lens (\MetricValue' {ports} -> ports) (\s@MetricValue' {} a -> s {ports = a} :: MetricValue) Prelude.. Lens.mapping Lens.coerced

-- | The string values of a metric.
metricValue_strings :: Lens.Lens' MetricValue (Prelude.Maybe [Prelude.Text])
metricValue_strings = Lens.lens (\MetricValue' {strings} -> strings) (\s@MetricValue' {} a -> s {strings = a} :: MetricValue) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MetricValue where
  parseJSON =
    Data.withObject
      "MetricValue"
      ( \x ->
          MetricValue'
            Prelude.<$> (x Data..:? "cidrs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "count")
            Prelude.<*> (x Data..:? "number")
            Prelude.<*> (x Data..:? "numbers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ports" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "strings" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable MetricValue where
  hashWithSalt _salt MetricValue' {..} =
    _salt `Prelude.hashWithSalt` cidrs
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` number
      `Prelude.hashWithSalt` numbers
      `Prelude.hashWithSalt` ports
      `Prelude.hashWithSalt` strings

instance Prelude.NFData MetricValue where
  rnf MetricValue' {..} =
    Prelude.rnf cidrs
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf number
      `Prelude.seq` Prelude.rnf numbers
      `Prelude.seq` Prelude.rnf ports
      `Prelude.seq` Prelude.rnf strings

instance Data.ToJSON MetricValue where
  toJSON MetricValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cidrs" Data..=) Prelude.<$> cidrs,
            ("count" Data..=) Prelude.<$> count,
            ("number" Data..=) Prelude.<$> number,
            ("numbers" Data..=) Prelude.<$> numbers,
            ("ports" Data..=) Prelude.<$> ports,
            ("strings" Data..=) Prelude.<$> strings
          ]
      )
