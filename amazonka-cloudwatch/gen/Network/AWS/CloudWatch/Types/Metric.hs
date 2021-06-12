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
-- Module      : Network.AWS.CloudWatch.Types.Metric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Metric where

import Network.AWS.CloudWatch.Types.Dimension
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a specific metric.
--
-- /See:/ 'newMetric' smart constructor.
data Metric = Metric'
  { -- | The name of the metric. This is a required field.
    metricName :: Core.Maybe Core.Text,
    -- | The dimensions for the metric.
    dimensions :: Core.Maybe [Dimension],
    -- | The namespace of the metric.
    namespace :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Metric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'metric_metricName' - The name of the metric. This is a required field.
--
-- 'dimensions', 'metric_dimensions' - The dimensions for the metric.
--
-- 'namespace', 'metric_namespace' - The namespace of the metric.
newMetric ::
  Metric
newMetric =
  Metric'
    { metricName = Core.Nothing,
      dimensions = Core.Nothing,
      namespace = Core.Nothing
    }

-- | The name of the metric. This is a required field.
metric_metricName :: Lens.Lens' Metric (Core.Maybe Core.Text)
metric_metricName = Lens.lens (\Metric' {metricName} -> metricName) (\s@Metric' {} a -> s {metricName = a} :: Metric)

-- | The dimensions for the metric.
metric_dimensions :: Lens.Lens' Metric (Core.Maybe [Dimension])
metric_dimensions = Lens.lens (\Metric' {dimensions} -> dimensions) (\s@Metric' {} a -> s {dimensions = a} :: Metric) Core.. Lens.mapping Lens._Coerce

-- | The namespace of the metric.
metric_namespace :: Lens.Lens' Metric (Core.Maybe Core.Text)
metric_namespace = Lens.lens (\Metric' {namespace} -> namespace) (\s@Metric' {} a -> s {namespace = a} :: Metric)

instance Core.FromXML Metric where
  parseXML x =
    Metric'
      Core.<$> (x Core..@? "MetricName")
      Core.<*> ( x Core..@? "Dimensions" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Namespace")

instance Core.Hashable Metric

instance Core.NFData Metric

instance Core.ToQuery Metric where
  toQuery Metric' {..} =
    Core.mconcat
      [ "MetricName" Core.=: metricName,
        "Dimensions"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> dimensions),
        "Namespace" Core.=: namespace
      ]
