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
-- Module      : Amazonka.Evidently.Types.MetricGoalConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.MetricGoalConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.ChangeDirectionEnum
import Amazonka.Evidently.Types.MetricDefinitionConfig
import qualified Amazonka.Prelude as Prelude

-- | Use this structure to tell Evidently whether higher or lower values are
-- desired for a metric that is used in an experiment.
--
-- /See:/ 'newMetricGoalConfig' smart constructor.
data MetricGoalConfig = MetricGoalConfig'
  { -- | @INCREASE@ means that a variation with a higher number for this metric
    -- is performing better.
    --
    -- @DECREASE@ means that a variation with a lower number for this metric is
    -- performing better.
    desiredChange :: Prelude.Maybe ChangeDirectionEnum,
    -- | A structure that contains details about the metric.
    metricDefinition :: MetricDefinitionConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricGoalConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredChange', 'metricGoalConfig_desiredChange' - @INCREASE@ means that a variation with a higher number for this metric
-- is performing better.
--
-- @DECREASE@ means that a variation with a lower number for this metric is
-- performing better.
--
-- 'metricDefinition', 'metricGoalConfig_metricDefinition' - A structure that contains details about the metric.
newMetricGoalConfig ::
  -- | 'metricDefinition'
  MetricDefinitionConfig ->
  MetricGoalConfig
newMetricGoalConfig pMetricDefinition_ =
  MetricGoalConfig'
    { desiredChange = Prelude.Nothing,
      metricDefinition = pMetricDefinition_
    }

-- | @INCREASE@ means that a variation with a higher number for this metric
-- is performing better.
--
-- @DECREASE@ means that a variation with a lower number for this metric is
-- performing better.
metricGoalConfig_desiredChange :: Lens.Lens' MetricGoalConfig (Prelude.Maybe ChangeDirectionEnum)
metricGoalConfig_desiredChange = Lens.lens (\MetricGoalConfig' {desiredChange} -> desiredChange) (\s@MetricGoalConfig' {} a -> s {desiredChange = a} :: MetricGoalConfig)

-- | A structure that contains details about the metric.
metricGoalConfig_metricDefinition :: Lens.Lens' MetricGoalConfig MetricDefinitionConfig
metricGoalConfig_metricDefinition = Lens.lens (\MetricGoalConfig' {metricDefinition} -> metricDefinition) (\s@MetricGoalConfig' {} a -> s {metricDefinition = a} :: MetricGoalConfig)

instance Prelude.Hashable MetricGoalConfig where
  hashWithSalt _salt MetricGoalConfig' {..} =
    _salt
      `Prelude.hashWithSalt` desiredChange
      `Prelude.hashWithSalt` metricDefinition

instance Prelude.NFData MetricGoalConfig where
  rnf MetricGoalConfig' {..} =
    Prelude.rnf desiredChange
      `Prelude.seq` Prelude.rnf metricDefinition

instance Data.ToJSON MetricGoalConfig where
  toJSON MetricGoalConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("desiredChange" Data..=) Prelude.<$> desiredChange,
            Prelude.Just
              ("metricDefinition" Data..= metricDefinition)
          ]
      )
