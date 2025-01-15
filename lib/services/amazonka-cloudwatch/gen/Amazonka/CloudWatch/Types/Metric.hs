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
-- Module      : Amazonka.CloudWatch.Types.Metric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.Metric where

import Amazonka.CloudWatch.Types.Dimension
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a specific metric.
--
-- /See:/ 'newMetric' smart constructor.
data Metric = Metric'
  { -- | The dimensions for the metric.
    dimensions :: Prelude.Maybe [Dimension],
    -- | The name of the metric. This is a required field.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the metric.
    namespace :: Prelude.Maybe Prelude.Text
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
-- 'dimensions', 'metric_dimensions' - The dimensions for the metric.
--
-- 'metricName', 'metric_metricName' - The name of the metric. This is a required field.
--
-- 'namespace', 'metric_namespace' - The namespace of the metric.
newMetric ::
  Metric
newMetric =
  Metric'
    { dimensions = Prelude.Nothing,
      metricName = Prelude.Nothing,
      namespace = Prelude.Nothing
    }

-- | The dimensions for the metric.
metric_dimensions :: Lens.Lens' Metric (Prelude.Maybe [Dimension])
metric_dimensions = Lens.lens (\Metric' {dimensions} -> dimensions) (\s@Metric' {} a -> s {dimensions = a} :: Metric) Prelude.. Lens.mapping Lens.coerced

-- | The name of the metric. This is a required field.
metric_metricName :: Lens.Lens' Metric (Prelude.Maybe Prelude.Text)
metric_metricName = Lens.lens (\Metric' {metricName} -> metricName) (\s@Metric' {} a -> s {metricName = a} :: Metric)

-- | The namespace of the metric.
metric_namespace :: Lens.Lens' Metric (Prelude.Maybe Prelude.Text)
metric_namespace = Lens.lens (\Metric' {namespace} -> namespace) (\s@Metric' {} a -> s {namespace = a} :: Metric)

instance Data.FromXML Metric where
  parseXML x =
    Metric'
      Prelude.<$> ( x Data..@? "Dimensions" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "MetricName")
      Prelude.<*> (x Data..@? "Namespace")

instance Prelude.Hashable Metric where
  hashWithSalt _salt Metric' {..} =
    _salt
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData Metric where
  rnf Metric' {..} =
    Prelude.rnf dimensions `Prelude.seq`
      Prelude.rnf metricName `Prelude.seq`
        Prelude.rnf namespace

instance Data.ToQuery Metric where
  toQuery Metric' {..} =
    Prelude.mconcat
      [ "Dimensions"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> dimensions),
        "MetricName" Data.=: metricName,
        "Namespace" Data.=: namespace
      ]
