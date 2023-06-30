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
-- Module      : Amazonka.AutoScaling.Types.PredictiveScalingCustomizedScalingMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.PredictiveScalingCustomizedScalingMetric where

import Amazonka.AutoScaling.Types.MetricDataQuery
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a custom scaling metric for a predictive scaling policy.
--
-- /See:/ 'newPredictiveScalingCustomizedScalingMetric' smart constructor.
data PredictiveScalingCustomizedScalingMetric = PredictiveScalingCustomizedScalingMetric'
  { -- | One or more metric data queries to provide the data points for a scaling
    -- metric. Use multiple metric data queries only if you are performing a
    -- math expression on returned data.
    metricDataQueries :: [MetricDataQuery]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictiveScalingCustomizedScalingMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDataQueries', 'predictiveScalingCustomizedScalingMetric_metricDataQueries' - One or more metric data queries to provide the data points for a scaling
-- metric. Use multiple metric data queries only if you are performing a
-- math expression on returned data.
newPredictiveScalingCustomizedScalingMetric ::
  PredictiveScalingCustomizedScalingMetric
newPredictiveScalingCustomizedScalingMetric =
  PredictiveScalingCustomizedScalingMetric'
    { metricDataQueries =
        Prelude.mempty
    }

-- | One or more metric data queries to provide the data points for a scaling
-- metric. Use multiple metric data queries only if you are performing a
-- math expression on returned data.
predictiveScalingCustomizedScalingMetric_metricDataQueries :: Lens.Lens' PredictiveScalingCustomizedScalingMetric [MetricDataQuery]
predictiveScalingCustomizedScalingMetric_metricDataQueries = Lens.lens (\PredictiveScalingCustomizedScalingMetric' {metricDataQueries} -> metricDataQueries) (\s@PredictiveScalingCustomizedScalingMetric' {} a -> s {metricDataQueries = a} :: PredictiveScalingCustomizedScalingMetric) Prelude.. Lens.coerced

instance
  Data.FromXML
    PredictiveScalingCustomizedScalingMetric
  where
  parseXML x =
    PredictiveScalingCustomizedScalingMetric'
      Prelude.<$> ( x
                      Data..@? "MetricDataQueries"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList "member"
                  )

instance
  Prelude.Hashable
    PredictiveScalingCustomizedScalingMetric
  where
  hashWithSalt
    _salt
    PredictiveScalingCustomizedScalingMetric' {..} =
      _salt `Prelude.hashWithSalt` metricDataQueries

instance
  Prelude.NFData
    PredictiveScalingCustomizedScalingMetric
  where
  rnf PredictiveScalingCustomizedScalingMetric' {..} =
    Prelude.rnf metricDataQueries

instance
  Data.ToQuery
    PredictiveScalingCustomizedScalingMetric
  where
  toQuery PredictiveScalingCustomizedScalingMetric' {..} =
    Prelude.mconcat
      [ "MetricDataQueries"
          Data.=: Data.toQueryList "member" metricDataQueries
      ]
