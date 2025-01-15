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
-- Module      : Amazonka.CloudWatch.Types.MetricStreamStatisticsMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.MetricStreamStatisticsMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This object contains the information for one metric that is to be
-- streamed with additional statistics.
--
-- /See:/ 'newMetricStreamStatisticsMetric' smart constructor.
data MetricStreamStatisticsMetric = MetricStreamStatisticsMetric'
  { -- | The namespace of the metric.
    namespace :: Prelude.Text,
    -- | The name of the metric.
    metricName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricStreamStatisticsMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespace', 'metricStreamStatisticsMetric_namespace' - The namespace of the metric.
--
-- 'metricName', 'metricStreamStatisticsMetric_metricName' - The name of the metric.
newMetricStreamStatisticsMetric ::
  -- | 'namespace'
  Prelude.Text ->
  -- | 'metricName'
  Prelude.Text ->
  MetricStreamStatisticsMetric
newMetricStreamStatisticsMetric
  pNamespace_
  pMetricName_ =
    MetricStreamStatisticsMetric'
      { namespace =
          pNamespace_,
        metricName = pMetricName_
      }

-- | The namespace of the metric.
metricStreamStatisticsMetric_namespace :: Lens.Lens' MetricStreamStatisticsMetric Prelude.Text
metricStreamStatisticsMetric_namespace = Lens.lens (\MetricStreamStatisticsMetric' {namespace} -> namespace) (\s@MetricStreamStatisticsMetric' {} a -> s {namespace = a} :: MetricStreamStatisticsMetric)

-- | The name of the metric.
metricStreamStatisticsMetric_metricName :: Lens.Lens' MetricStreamStatisticsMetric Prelude.Text
metricStreamStatisticsMetric_metricName = Lens.lens (\MetricStreamStatisticsMetric' {metricName} -> metricName) (\s@MetricStreamStatisticsMetric' {} a -> s {metricName = a} :: MetricStreamStatisticsMetric)

instance Data.FromXML MetricStreamStatisticsMetric where
  parseXML x =
    MetricStreamStatisticsMetric'
      Prelude.<$> (x Data..@ "Namespace")
      Prelude.<*> (x Data..@ "MetricName")

instance
  Prelude.Hashable
    MetricStreamStatisticsMetric
  where
  hashWithSalt _salt MetricStreamStatisticsMetric' {..} =
    _salt
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` metricName

instance Prelude.NFData MetricStreamStatisticsMetric where
  rnf MetricStreamStatisticsMetric' {..} =
    Prelude.rnf namespace `Prelude.seq`
      Prelude.rnf metricName

instance Data.ToQuery MetricStreamStatisticsMetric where
  toQuery MetricStreamStatisticsMetric' {..} =
    Prelude.mconcat
      [ "Namespace" Data.=: namespace,
        "MetricName" Data.=: metricName
      ]
