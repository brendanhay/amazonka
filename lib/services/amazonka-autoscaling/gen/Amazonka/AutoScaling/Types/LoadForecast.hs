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
-- Module      : Amazonka.AutoScaling.Types.LoadForecast
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.LoadForecast where

import Amazonka.AutoScaling.Types.PredictiveScalingMetricSpecification
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A @GetPredictiveScalingForecast@ call returns the load forecast for a
-- predictive scaling policy. This structure includes the data points for
-- that load forecast, along with the timestamps of those data points and
-- the metric specification.
--
-- /See:/ 'newLoadForecast' smart constructor.
data LoadForecast = LoadForecast'
  { -- | The timestamps for the data points, in UTC format.
    timestamps :: [Core.ISO8601],
    -- | The values of the data points.
    values :: [Prelude.Double],
    -- | The metric specification for the load forecast.
    metricSpecification :: PredictiveScalingMetricSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadForecast' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamps', 'loadForecast_timestamps' - The timestamps for the data points, in UTC format.
--
-- 'values', 'loadForecast_values' - The values of the data points.
--
-- 'metricSpecification', 'loadForecast_metricSpecification' - The metric specification for the load forecast.
newLoadForecast ::
  -- | 'metricSpecification'
  PredictiveScalingMetricSpecification ->
  LoadForecast
newLoadForecast pMetricSpecification_ =
  LoadForecast'
    { timestamps = Prelude.mempty,
      values = Prelude.mempty,
      metricSpecification = pMetricSpecification_
    }

-- | The timestamps for the data points, in UTC format.
loadForecast_timestamps :: Lens.Lens' LoadForecast [Prelude.UTCTime]
loadForecast_timestamps = Lens.lens (\LoadForecast' {timestamps} -> timestamps) (\s@LoadForecast' {} a -> s {timestamps = a} :: LoadForecast) Prelude.. Lens.coerced

-- | The values of the data points.
loadForecast_values :: Lens.Lens' LoadForecast [Prelude.Double]
loadForecast_values = Lens.lens (\LoadForecast' {values} -> values) (\s@LoadForecast' {} a -> s {values = a} :: LoadForecast) Prelude.. Lens.coerced

-- | The metric specification for the load forecast.
loadForecast_metricSpecification :: Lens.Lens' LoadForecast PredictiveScalingMetricSpecification
loadForecast_metricSpecification = Lens.lens (\LoadForecast' {metricSpecification} -> metricSpecification) (\s@LoadForecast' {} a -> s {metricSpecification = a} :: LoadForecast)

instance Core.FromXML LoadForecast where
  parseXML x =
    LoadForecast'
      Prelude.<$> ( x Core..@? "Timestamps" Core..!@ Prelude.mempty
                      Prelude.>>= Core.parseXMLList "member"
                  )
      Prelude.<*> ( x Core..@? "Values" Core..!@ Prelude.mempty
                      Prelude.>>= Core.parseXMLList "member"
                  )
      Prelude.<*> (x Core..@ "MetricSpecification")

instance Prelude.Hashable LoadForecast where
  hashWithSalt _salt LoadForecast' {..} =
    _salt `Prelude.hashWithSalt` timestamps
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` metricSpecification

instance Prelude.NFData LoadForecast where
  rnf LoadForecast' {..} =
    Prelude.rnf timestamps
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf metricSpecification
