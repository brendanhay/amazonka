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
-- Module      : Amazonka.CloudWatch.Types.MetricMathAnomalyDetector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.MetricMathAnomalyDetector where

import Amazonka.CloudWatch.Types.MetricDataQuery
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates the CloudWatch math expression that provides the time series
-- the anomaly detector uses as input. The designated math expression must
-- return a single time series.
--
-- /See:/ 'newMetricMathAnomalyDetector' smart constructor.
data MetricMathAnomalyDetector = MetricMathAnomalyDetector'
  { -- | An array of metric data query structures that enables you to create an
    -- anomaly detector based on the result of a metric math expression. Each
    -- item in @MetricDataQueries@ gets a metric or performs a math expression.
    -- One item in @MetricDataQueries@ is the expression that provides the time
    -- series that the anomaly detector uses as input. Designate the expression
    -- by setting @ReturnData@ to @True@ for this object in the array. For all
    -- other expressions and metrics, set @ReturnData@ to @False@. The
    -- designated expression must return a single time series.
    metricDataQueries :: Prelude.Maybe [MetricDataQuery]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricMathAnomalyDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDataQueries', 'metricMathAnomalyDetector_metricDataQueries' - An array of metric data query structures that enables you to create an
-- anomaly detector based on the result of a metric math expression. Each
-- item in @MetricDataQueries@ gets a metric or performs a math expression.
-- One item in @MetricDataQueries@ is the expression that provides the time
-- series that the anomaly detector uses as input. Designate the expression
-- by setting @ReturnData@ to @True@ for this object in the array. For all
-- other expressions and metrics, set @ReturnData@ to @False@. The
-- designated expression must return a single time series.
newMetricMathAnomalyDetector ::
  MetricMathAnomalyDetector
newMetricMathAnomalyDetector =
  MetricMathAnomalyDetector'
    { metricDataQueries =
        Prelude.Nothing
    }

-- | An array of metric data query structures that enables you to create an
-- anomaly detector based on the result of a metric math expression. Each
-- item in @MetricDataQueries@ gets a metric or performs a math expression.
-- One item in @MetricDataQueries@ is the expression that provides the time
-- series that the anomaly detector uses as input. Designate the expression
-- by setting @ReturnData@ to @True@ for this object in the array. For all
-- other expressions and metrics, set @ReturnData@ to @False@. The
-- designated expression must return a single time series.
metricMathAnomalyDetector_metricDataQueries :: Lens.Lens' MetricMathAnomalyDetector (Prelude.Maybe [MetricDataQuery])
metricMathAnomalyDetector_metricDataQueries = Lens.lens (\MetricMathAnomalyDetector' {metricDataQueries} -> metricDataQueries) (\s@MetricMathAnomalyDetector' {} a -> s {metricDataQueries = a} :: MetricMathAnomalyDetector) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML MetricMathAnomalyDetector where
  parseXML x =
    MetricMathAnomalyDetector'
      Prelude.<$> ( x Data..@? "MetricDataQueries"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable MetricMathAnomalyDetector where
  hashWithSalt _salt MetricMathAnomalyDetector' {..} =
    _salt `Prelude.hashWithSalt` metricDataQueries

instance Prelude.NFData MetricMathAnomalyDetector where
  rnf MetricMathAnomalyDetector' {..} =
    Prelude.rnf metricDataQueries

instance Data.ToQuery MetricMathAnomalyDetector where
  toQuery MetricMathAnomalyDetector' {..} =
    Prelude.mconcat
      [ "MetricDataQueries"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> metricDataQueries
            )
      ]
