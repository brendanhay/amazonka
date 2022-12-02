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
-- Module      : Amazonka.LookoutMetrics.Types.Metric
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.Metric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.AggregationFunction
import qualified Amazonka.Prelude as Prelude

-- | A calculation made by contrasting a measure and a dimension from your
-- source data.
--
-- /See:/ 'newMetric' smart constructor.
data Metric = Metric'
  { -- | The namespace for the metric.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The name of the metric.
    metricName :: Prelude.Text,
    -- | The function with which the metric is calculated.
    aggregationFunction :: AggregationFunction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Metric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespace', 'metric_namespace' - The namespace for the metric.
--
-- 'metricName', 'metric_metricName' - The name of the metric.
--
-- 'aggregationFunction', 'metric_aggregationFunction' - The function with which the metric is calculated.
newMetric ::
  -- | 'metricName'
  Prelude.Text ->
  -- | 'aggregationFunction'
  AggregationFunction ->
  Metric
newMetric pMetricName_ pAggregationFunction_ =
  Metric'
    { namespace = Prelude.Nothing,
      metricName = pMetricName_,
      aggregationFunction = pAggregationFunction_
    }

-- | The namespace for the metric.
metric_namespace :: Lens.Lens' Metric (Prelude.Maybe Prelude.Text)
metric_namespace = Lens.lens (\Metric' {namespace} -> namespace) (\s@Metric' {} a -> s {namespace = a} :: Metric)

-- | The name of the metric.
metric_metricName :: Lens.Lens' Metric Prelude.Text
metric_metricName = Lens.lens (\Metric' {metricName} -> metricName) (\s@Metric' {} a -> s {metricName = a} :: Metric)

-- | The function with which the metric is calculated.
metric_aggregationFunction :: Lens.Lens' Metric AggregationFunction
metric_aggregationFunction = Lens.lens (\Metric' {aggregationFunction} -> aggregationFunction) (\s@Metric' {} a -> s {aggregationFunction = a} :: Metric)

instance Data.FromJSON Metric where
  parseJSON =
    Data.withObject
      "Metric"
      ( \x ->
          Metric'
            Prelude.<$> (x Data..:? "Namespace")
            Prelude.<*> (x Data..: "MetricName")
            Prelude.<*> (x Data..: "AggregationFunction")
      )

instance Prelude.Hashable Metric where
  hashWithSalt _salt Metric' {..} =
    _salt `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` aggregationFunction

instance Prelude.NFData Metric where
  rnf Metric' {..} =
    Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf aggregationFunction

instance Data.ToJSON Metric where
  toJSON Metric' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Namespace" Data..=) Prelude.<$> namespace,
            Prelude.Just ("MetricName" Data..= metricName),
            Prelude.Just
              ("AggregationFunction" Data..= aggregationFunction)
          ]
      )
