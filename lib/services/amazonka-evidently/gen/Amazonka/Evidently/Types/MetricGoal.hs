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
-- Module      : Amazonka.Evidently.Types.MetricGoal
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.MetricGoal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Evidently.Types.ChangeDirectionEnum
import Amazonka.Evidently.Types.MetricDefinition
import qualified Amazonka.Prelude as Prelude

-- | A structure that tells Evidently whether higher or lower values are
-- desired for a metric that is used in an experiment.
--
-- /See:/ 'newMetricGoal' smart constructor.
data MetricGoal = MetricGoal'
  { -- | @INCREASE@ means that a variation with a higher number for this metric
    -- is performing better.
    --
    -- @DECREASE@ means that a variation with a lower number for this metric is
    -- performing better.
    desiredChange :: Prelude.Maybe ChangeDirectionEnum,
    -- | A structure that contains details about the metric.
    metricDefinition :: MetricDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricGoal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredChange', 'metricGoal_desiredChange' - @INCREASE@ means that a variation with a higher number for this metric
-- is performing better.
--
-- @DECREASE@ means that a variation with a lower number for this metric is
-- performing better.
--
-- 'metricDefinition', 'metricGoal_metricDefinition' - A structure that contains details about the metric.
newMetricGoal ::
  -- | 'metricDefinition'
  MetricDefinition ->
  MetricGoal
newMetricGoal pMetricDefinition_ =
  MetricGoal'
    { desiredChange = Prelude.Nothing,
      metricDefinition = pMetricDefinition_
    }

-- | @INCREASE@ means that a variation with a higher number for this metric
-- is performing better.
--
-- @DECREASE@ means that a variation with a lower number for this metric is
-- performing better.
metricGoal_desiredChange :: Lens.Lens' MetricGoal (Prelude.Maybe ChangeDirectionEnum)
metricGoal_desiredChange = Lens.lens (\MetricGoal' {desiredChange} -> desiredChange) (\s@MetricGoal' {} a -> s {desiredChange = a} :: MetricGoal)

-- | A structure that contains details about the metric.
metricGoal_metricDefinition :: Lens.Lens' MetricGoal MetricDefinition
metricGoal_metricDefinition = Lens.lens (\MetricGoal' {metricDefinition} -> metricDefinition) (\s@MetricGoal' {} a -> s {metricDefinition = a} :: MetricGoal)

instance Core.FromJSON MetricGoal where
  parseJSON =
    Core.withObject
      "MetricGoal"
      ( \x ->
          MetricGoal'
            Prelude.<$> (x Core..:? "desiredChange")
            Prelude.<*> (x Core..: "metricDefinition")
      )

instance Prelude.Hashable MetricGoal where
  hashWithSalt _salt MetricGoal' {..} =
    _salt `Prelude.hashWithSalt` desiredChange
      `Prelude.hashWithSalt` metricDefinition

instance Prelude.NFData MetricGoal where
  rnf MetricGoal' {..} =
    Prelude.rnf desiredChange
      `Prelude.seq` Prelude.rnf metricDefinition
