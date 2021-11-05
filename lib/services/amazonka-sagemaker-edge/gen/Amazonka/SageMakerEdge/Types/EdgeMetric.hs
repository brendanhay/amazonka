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
-- Module      : Amazonka.SageMakerEdge.Types.EdgeMetric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerEdge.Types.EdgeMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information required for edge device metrics.
--
-- /See:/ 'newEdgeMetric' smart constructor.
data EdgeMetric = EdgeMetric'
  { -- | The dimension of metrics published.
    dimension :: Prelude.Maybe Prelude.Text,
    -- | Returns the name of the metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | Returns the value of the metric.
    value :: Prelude.Maybe Prelude.Double,
    -- | Timestamp of when the metric was requested.
    timestamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EdgeMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimension', 'edgeMetric_dimension' - The dimension of metrics published.
--
-- 'metricName', 'edgeMetric_metricName' - Returns the name of the metric.
--
-- 'value', 'edgeMetric_value' - Returns the value of the metric.
--
-- 'timestamp', 'edgeMetric_timestamp' - Timestamp of when the metric was requested.
newEdgeMetric ::
  EdgeMetric
newEdgeMetric =
  EdgeMetric'
    { dimension = Prelude.Nothing,
      metricName = Prelude.Nothing,
      value = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The dimension of metrics published.
edgeMetric_dimension :: Lens.Lens' EdgeMetric (Prelude.Maybe Prelude.Text)
edgeMetric_dimension = Lens.lens (\EdgeMetric' {dimension} -> dimension) (\s@EdgeMetric' {} a -> s {dimension = a} :: EdgeMetric)

-- | Returns the name of the metric.
edgeMetric_metricName :: Lens.Lens' EdgeMetric (Prelude.Maybe Prelude.Text)
edgeMetric_metricName = Lens.lens (\EdgeMetric' {metricName} -> metricName) (\s@EdgeMetric' {} a -> s {metricName = a} :: EdgeMetric)

-- | Returns the value of the metric.
edgeMetric_value :: Lens.Lens' EdgeMetric (Prelude.Maybe Prelude.Double)
edgeMetric_value = Lens.lens (\EdgeMetric' {value} -> value) (\s@EdgeMetric' {} a -> s {value = a} :: EdgeMetric)

-- | Timestamp of when the metric was requested.
edgeMetric_timestamp :: Lens.Lens' EdgeMetric (Prelude.Maybe Prelude.UTCTime)
edgeMetric_timestamp = Lens.lens (\EdgeMetric' {timestamp} -> timestamp) (\s@EdgeMetric' {} a -> s {timestamp = a} :: EdgeMetric) Prelude.. Lens.mapping Core._Time

instance Prelude.Hashable EdgeMetric

instance Prelude.NFData EdgeMetric

instance Core.ToJSON EdgeMetric where
  toJSON EdgeMetric' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Dimension" Core..=) Prelude.<$> dimension,
            ("MetricName" Core..=) Prelude.<$> metricName,
            ("Value" Core..=) Prelude.<$> value,
            ("Timestamp" Core..=) Prelude.<$> timestamp
          ]
      )
