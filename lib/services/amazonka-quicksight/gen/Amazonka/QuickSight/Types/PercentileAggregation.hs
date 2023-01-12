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
-- Module      : Amazonka.QuickSight.Types.PercentileAggregation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PercentileAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An aggregation based on the percentile of values in a dimension or
-- measure.
--
-- /See:/ 'newPercentileAggregation' smart constructor.
data PercentileAggregation = PercentileAggregation'
  { -- | The percentile value. This value can be any numeric constant 0–100. A
    -- percentile value of 50 computes the median value of the measure.
    percentileValue :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PercentileAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'percentileValue', 'percentileAggregation_percentileValue' - The percentile value. This value can be any numeric constant 0–100. A
-- percentile value of 50 computes the median value of the measure.
newPercentileAggregation ::
  PercentileAggregation
newPercentileAggregation =
  PercentileAggregation'
    { percentileValue =
        Prelude.Nothing
    }

-- | The percentile value. This value can be any numeric constant 0–100. A
-- percentile value of 50 computes the median value of the measure.
percentileAggregation_percentileValue :: Lens.Lens' PercentileAggregation (Prelude.Maybe Prelude.Double)
percentileAggregation_percentileValue = Lens.lens (\PercentileAggregation' {percentileValue} -> percentileValue) (\s@PercentileAggregation' {} a -> s {percentileValue = a} :: PercentileAggregation)

instance Data.FromJSON PercentileAggregation where
  parseJSON =
    Data.withObject
      "PercentileAggregation"
      ( \x ->
          PercentileAggregation'
            Prelude.<$> (x Data..:? "PercentileValue")
      )

instance Prelude.Hashable PercentileAggregation where
  hashWithSalt _salt PercentileAggregation' {..} =
    _salt `Prelude.hashWithSalt` percentileValue

instance Prelude.NFData PercentileAggregation where
  rnf PercentileAggregation' {..} =
    Prelude.rnf percentileValue

instance Data.ToJSON PercentileAggregation where
  toJSON PercentileAggregation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PercentileValue" Data..=)
              Prelude.<$> percentileValue
          ]
      )
