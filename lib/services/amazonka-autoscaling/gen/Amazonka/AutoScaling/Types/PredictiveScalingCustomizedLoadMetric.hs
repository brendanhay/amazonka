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
-- Module      : Amazonka.AutoScaling.Types.PredictiveScalingCustomizedLoadMetric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.PredictiveScalingCustomizedLoadMetric where

import Amazonka.AutoScaling.Types.MetricDataQuery
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a custom load metric for a predictive scaling policy.
--
-- /See:/ 'newPredictiveScalingCustomizedLoadMetric' smart constructor.
data PredictiveScalingCustomizedLoadMetric = PredictiveScalingCustomizedLoadMetric'
  { -- | One or more metric data queries to provide the data points for a load
    -- metric. Use multiple metric data queries only if you are performing a
    -- math expression on returned data.
    metricDataQueries :: [MetricDataQuery]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictiveScalingCustomizedLoadMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDataQueries', 'predictiveScalingCustomizedLoadMetric_metricDataQueries' - One or more metric data queries to provide the data points for a load
-- metric. Use multiple metric data queries only if you are performing a
-- math expression on returned data.
newPredictiveScalingCustomizedLoadMetric ::
  PredictiveScalingCustomizedLoadMetric
newPredictiveScalingCustomizedLoadMetric =
  PredictiveScalingCustomizedLoadMetric'
    { metricDataQueries =
        Prelude.mempty
    }

-- | One or more metric data queries to provide the data points for a load
-- metric. Use multiple metric data queries only if you are performing a
-- math expression on returned data.
predictiveScalingCustomizedLoadMetric_metricDataQueries :: Lens.Lens' PredictiveScalingCustomizedLoadMetric [MetricDataQuery]
predictiveScalingCustomizedLoadMetric_metricDataQueries = Lens.lens (\PredictiveScalingCustomizedLoadMetric' {metricDataQueries} -> metricDataQueries) (\s@PredictiveScalingCustomizedLoadMetric' {} a -> s {metricDataQueries = a} :: PredictiveScalingCustomizedLoadMetric) Prelude.. Lens.coerced

instance
  Core.FromXML
    PredictiveScalingCustomizedLoadMetric
  where
  parseXML x =
    PredictiveScalingCustomizedLoadMetric'
      Prelude.<$> ( x Core..@? "MetricDataQueries"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.parseXMLList "member"
                  )

instance
  Prelude.Hashable
    PredictiveScalingCustomizedLoadMetric
  where
  hashWithSalt
    _salt
    PredictiveScalingCustomizedLoadMetric' {..} =
      _salt `Prelude.hashWithSalt` metricDataQueries

instance
  Prelude.NFData
    PredictiveScalingCustomizedLoadMetric
  where
  rnf PredictiveScalingCustomizedLoadMetric' {..} =
    Prelude.rnf metricDataQueries

instance
  Core.ToQuery
    PredictiveScalingCustomizedLoadMetric
  where
  toQuery PredictiveScalingCustomizedLoadMetric' {..} =
    Prelude.mconcat
      [ "MetricDataQueries"
          Core.=: Core.toQueryList "member" metricDataQueries
      ]
