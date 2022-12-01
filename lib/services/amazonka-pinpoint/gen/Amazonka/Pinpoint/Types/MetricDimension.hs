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
-- Module      : Amazonka.Pinpoint.Types.MetricDimension
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.MetricDimension where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies metric-based criteria for including or excluding endpoints
-- from a segment. These criteria derive from custom metrics that you
-- define for endpoints.
--
-- /See:/ 'newMetricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { -- | The operator to use when comparing metric values. Valid values are:
    -- GREATER_THAN, LESS_THAN, GREATER_THAN_OR_EQUAL, LESS_THAN_OR_EQUAL, and
    -- EQUAL.
    comparisonOperator :: Prelude.Text,
    -- | The value to compare.
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparisonOperator', 'metricDimension_comparisonOperator' - The operator to use when comparing metric values. Valid values are:
-- GREATER_THAN, LESS_THAN, GREATER_THAN_OR_EQUAL, LESS_THAN_OR_EQUAL, and
-- EQUAL.
--
-- 'value', 'metricDimension_value' - The value to compare.
newMetricDimension ::
  -- | 'comparisonOperator'
  Prelude.Text ->
  -- | 'value'
  Prelude.Double ->
  MetricDimension
newMetricDimension pComparisonOperator_ pValue_ =
  MetricDimension'
    { comparisonOperator =
        pComparisonOperator_,
      value = pValue_
    }

-- | The operator to use when comparing metric values. Valid values are:
-- GREATER_THAN, LESS_THAN, GREATER_THAN_OR_EQUAL, LESS_THAN_OR_EQUAL, and
-- EQUAL.
metricDimension_comparisonOperator :: Lens.Lens' MetricDimension Prelude.Text
metricDimension_comparisonOperator = Lens.lens (\MetricDimension' {comparisonOperator} -> comparisonOperator) (\s@MetricDimension' {} a -> s {comparisonOperator = a} :: MetricDimension)

-- | The value to compare.
metricDimension_value :: Lens.Lens' MetricDimension Prelude.Double
metricDimension_value = Lens.lens (\MetricDimension' {value} -> value) (\s@MetricDimension' {} a -> s {value = a} :: MetricDimension)

instance Core.FromJSON MetricDimension where
  parseJSON =
    Core.withObject
      "MetricDimension"
      ( \x ->
          MetricDimension'
            Prelude.<$> (x Core..: "ComparisonOperator")
            Prelude.<*> (x Core..: "Value")
      )

instance Prelude.Hashable MetricDimension where
  hashWithSalt _salt MetricDimension' {..} =
    _salt `Prelude.hashWithSalt` comparisonOperator
      `Prelude.hashWithSalt` value

instance Prelude.NFData MetricDimension where
  rnf MetricDimension' {..} =
    Prelude.rnf comparisonOperator
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON MetricDimension where
  toJSON MetricDimension' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ComparisonOperator" Core..= comparisonOperator),
            Prelude.Just ("Value" Core..= value)
          ]
      )
