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
-- Module      : Amazonka.AutoScaling.Types.PredictiveScalingCustomizedCapacityMetric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.PredictiveScalingCustomizedCapacityMetric where

import Amazonka.AutoScaling.Types.MetricDataQuery
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a customized capacity metric for a predictive scaling policy.
--
-- /See:/ 'newPredictiveScalingCustomizedCapacityMetric' smart constructor.
data PredictiveScalingCustomizedCapacityMetric = PredictiveScalingCustomizedCapacityMetric'
  { -- | One or more metric data queries to provide the data points for a
    -- capacity metric. Use multiple metric data queries only if you are
    -- performing a math expression on returned data.
    metricDataQueries :: [MetricDataQuery]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictiveScalingCustomizedCapacityMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDataQueries', 'predictiveScalingCustomizedCapacityMetric_metricDataQueries' - One or more metric data queries to provide the data points for a
-- capacity metric. Use multiple metric data queries only if you are
-- performing a math expression on returned data.
newPredictiveScalingCustomizedCapacityMetric ::
  PredictiveScalingCustomizedCapacityMetric
newPredictiveScalingCustomizedCapacityMetric =
  PredictiveScalingCustomizedCapacityMetric'
    { metricDataQueries =
        Prelude.mempty
    }

-- | One or more metric data queries to provide the data points for a
-- capacity metric. Use multiple metric data queries only if you are
-- performing a math expression on returned data.
predictiveScalingCustomizedCapacityMetric_metricDataQueries :: Lens.Lens' PredictiveScalingCustomizedCapacityMetric [MetricDataQuery]
predictiveScalingCustomizedCapacityMetric_metricDataQueries = Lens.lens (\PredictiveScalingCustomizedCapacityMetric' {metricDataQueries} -> metricDataQueries) (\s@PredictiveScalingCustomizedCapacityMetric' {} a -> s {metricDataQueries = a} :: PredictiveScalingCustomizedCapacityMetric) Prelude.. Lens.coerced

instance
  Core.FromXML
    PredictiveScalingCustomizedCapacityMetric
  where
  parseXML x =
    PredictiveScalingCustomizedCapacityMetric'
      Prelude.<$> ( x Core..@? "MetricDataQueries"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.parseXMLList "member"
                  )

instance
  Prelude.Hashable
    PredictiveScalingCustomizedCapacityMetric
  where
  hashWithSalt
    _salt
    PredictiveScalingCustomizedCapacityMetric' {..} =
      _salt `Prelude.hashWithSalt` metricDataQueries

instance
  Prelude.NFData
    PredictiveScalingCustomizedCapacityMetric
  where
  rnf PredictiveScalingCustomizedCapacityMetric' {..} =
    Prelude.rnf metricDataQueries

instance
  Core.ToQuery
    PredictiveScalingCustomizedCapacityMetric
  where
  toQuery
    PredictiveScalingCustomizedCapacityMetric' {..} =
      Prelude.mconcat
        [ "MetricDataQueries"
            Core.=: Core.toQueryList "member" metricDataQueries
        ]
