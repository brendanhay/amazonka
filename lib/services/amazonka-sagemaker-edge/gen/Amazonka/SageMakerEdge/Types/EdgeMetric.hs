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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerEdge.Types.EdgeMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information required for edge device metrics.
--
-- /See:/ 'newEdgeMetric' smart constructor.
data EdgeMetric = EdgeMetric'
  { -- | Timestamp of when the metric was requested.
    timestamp :: Prelude.Maybe Data.POSIX,
    -- | Returns the name of the metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The dimension of metrics published.
    dimension :: Prelude.Maybe Prelude.Text,
    -- | Returns the value of the metric.
    value :: Prelude.Maybe Prelude.Double
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
-- 'timestamp', 'edgeMetric_timestamp' - Timestamp of when the metric was requested.
--
-- 'metricName', 'edgeMetric_metricName' - Returns the name of the metric.
--
-- 'dimension', 'edgeMetric_dimension' - The dimension of metrics published.
--
-- 'value', 'edgeMetric_value' - Returns the value of the metric.
newEdgeMetric ::
  EdgeMetric
newEdgeMetric =
  EdgeMetric'
    { timestamp = Prelude.Nothing,
      metricName = Prelude.Nothing,
      dimension = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Timestamp of when the metric was requested.
edgeMetric_timestamp :: Lens.Lens' EdgeMetric (Prelude.Maybe Prelude.UTCTime)
edgeMetric_timestamp = Lens.lens (\EdgeMetric' {timestamp} -> timestamp) (\s@EdgeMetric' {} a -> s {timestamp = a} :: EdgeMetric) Prelude.. Lens.mapping Data._Time

-- | Returns the name of the metric.
edgeMetric_metricName :: Lens.Lens' EdgeMetric (Prelude.Maybe Prelude.Text)
edgeMetric_metricName = Lens.lens (\EdgeMetric' {metricName} -> metricName) (\s@EdgeMetric' {} a -> s {metricName = a} :: EdgeMetric)

-- | The dimension of metrics published.
edgeMetric_dimension :: Lens.Lens' EdgeMetric (Prelude.Maybe Prelude.Text)
edgeMetric_dimension = Lens.lens (\EdgeMetric' {dimension} -> dimension) (\s@EdgeMetric' {} a -> s {dimension = a} :: EdgeMetric)

-- | Returns the value of the metric.
edgeMetric_value :: Lens.Lens' EdgeMetric (Prelude.Maybe Prelude.Double)
edgeMetric_value = Lens.lens (\EdgeMetric' {value} -> value) (\s@EdgeMetric' {} a -> s {value = a} :: EdgeMetric)

instance Prelude.Hashable EdgeMetric where
  hashWithSalt _salt EdgeMetric' {..} =
    _salt `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` dimension
      `Prelude.hashWithSalt` value

instance Prelude.NFData EdgeMetric where
  rnf EdgeMetric' {..} =
    Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf dimension
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON EdgeMetric where
  toJSON EdgeMetric' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Timestamp" Data..=) Prelude.<$> timestamp,
            ("MetricName" Data..=) Prelude.<$> metricName,
            ("Dimension" Data..=) Prelude.<$> dimension,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
