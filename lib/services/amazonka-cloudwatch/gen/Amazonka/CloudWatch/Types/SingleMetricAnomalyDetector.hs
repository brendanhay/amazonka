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
-- Module      : Amazonka.CloudWatch.Types.SingleMetricAnomalyDetector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.SingleMetricAnomalyDetector where

import Amazonka.CloudWatch.Types.Dimension
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Designates the CloudWatch metric and statistic that provides the time
-- series the anomaly detector uses as input.
--
-- /See:/ 'newSingleMetricAnomalyDetector' smart constructor.
data SingleMetricAnomalyDetector = SingleMetricAnomalyDetector'
  { -- | The metric dimensions to create the anomaly detection model for.
    dimensions :: Prelude.Maybe [Dimension],
    -- | The statistic to use for the metric and anomaly detection model.
    stat :: Prelude.Maybe Prelude.Text,
    -- | The name of the metric to create the anomaly detection model for.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the metric to create the anomaly detection model for.
    namespace :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SingleMetricAnomalyDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'singleMetricAnomalyDetector_dimensions' - The metric dimensions to create the anomaly detection model for.
--
-- 'stat', 'singleMetricAnomalyDetector_stat' - The statistic to use for the metric and anomaly detection model.
--
-- 'metricName', 'singleMetricAnomalyDetector_metricName' - The name of the metric to create the anomaly detection model for.
--
-- 'namespace', 'singleMetricAnomalyDetector_namespace' - The namespace of the metric to create the anomaly detection model for.
newSingleMetricAnomalyDetector ::
  SingleMetricAnomalyDetector
newSingleMetricAnomalyDetector =
  SingleMetricAnomalyDetector'
    { dimensions =
        Prelude.Nothing,
      stat = Prelude.Nothing,
      metricName = Prelude.Nothing,
      namespace = Prelude.Nothing
    }

-- | The metric dimensions to create the anomaly detection model for.
singleMetricAnomalyDetector_dimensions :: Lens.Lens' SingleMetricAnomalyDetector (Prelude.Maybe [Dimension])
singleMetricAnomalyDetector_dimensions = Lens.lens (\SingleMetricAnomalyDetector' {dimensions} -> dimensions) (\s@SingleMetricAnomalyDetector' {} a -> s {dimensions = a} :: SingleMetricAnomalyDetector) Prelude.. Lens.mapping Lens.coerced

-- | The statistic to use for the metric and anomaly detection model.
singleMetricAnomalyDetector_stat :: Lens.Lens' SingleMetricAnomalyDetector (Prelude.Maybe Prelude.Text)
singleMetricAnomalyDetector_stat = Lens.lens (\SingleMetricAnomalyDetector' {stat} -> stat) (\s@SingleMetricAnomalyDetector' {} a -> s {stat = a} :: SingleMetricAnomalyDetector)

-- | The name of the metric to create the anomaly detection model for.
singleMetricAnomalyDetector_metricName :: Lens.Lens' SingleMetricAnomalyDetector (Prelude.Maybe Prelude.Text)
singleMetricAnomalyDetector_metricName = Lens.lens (\SingleMetricAnomalyDetector' {metricName} -> metricName) (\s@SingleMetricAnomalyDetector' {} a -> s {metricName = a} :: SingleMetricAnomalyDetector)

-- | The namespace of the metric to create the anomaly detection model for.
singleMetricAnomalyDetector_namespace :: Lens.Lens' SingleMetricAnomalyDetector (Prelude.Maybe Prelude.Text)
singleMetricAnomalyDetector_namespace = Lens.lens (\SingleMetricAnomalyDetector' {namespace} -> namespace) (\s@SingleMetricAnomalyDetector' {} a -> s {namespace = a} :: SingleMetricAnomalyDetector)

instance Core.FromXML SingleMetricAnomalyDetector where
  parseXML x =
    SingleMetricAnomalyDetector'
      Prelude.<$> ( x Core..@? "Dimensions" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "Stat")
      Prelude.<*> (x Core..@? "MetricName")
      Prelude.<*> (x Core..@? "Namespace")

instance Prelude.Hashable SingleMetricAnomalyDetector where
  hashWithSalt _salt SingleMetricAnomalyDetector' {..} =
    _salt `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` stat
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData SingleMetricAnomalyDetector where
  rnf SingleMetricAnomalyDetector' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf stat
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf namespace

instance Core.ToQuery SingleMetricAnomalyDetector where
  toQuery SingleMetricAnomalyDetector' {..} =
    Prelude.mconcat
      [ "Dimensions"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> dimensions),
        "Stat" Core.=: stat,
        "MetricName" Core.=: metricName,
        "Namespace" Core.=: namespace
      ]
