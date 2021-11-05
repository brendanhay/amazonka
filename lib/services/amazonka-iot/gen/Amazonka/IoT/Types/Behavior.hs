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
-- Module      : Amazonka.IoT.Types.Behavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.Behavior where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.BehaviorCriteria
import Amazonka.IoT.Types.MetricDimension
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A Device Defender security profile behavior.
--
-- /See:/ 'newBehavior' smart constructor.
data Behavior = Behavior'
  { -- | Suppresses alerts.
    suppressAlerts :: Prelude.Maybe Prelude.Bool,
    -- | The dimension for a metric in your behavior. For example, using a
    -- @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric to
    -- only MQTT topics where the name matches the pattern specified in the
    -- dimension. This can\'t be used with custom metrics.
    metricDimension :: Prelude.Maybe MetricDimension,
    -- | What is measured by the behavior.
    metric :: Prelude.Maybe Prelude.Text,
    -- | The criteria that determine if a device is behaving normally in regard
    -- to the @metric@.
    criteria :: Prelude.Maybe BehaviorCriteria,
    -- | The name you\'ve given to the behavior.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Behavior' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suppressAlerts', 'behavior_suppressAlerts' - Suppresses alerts.
--
-- 'metricDimension', 'behavior_metricDimension' - The dimension for a metric in your behavior. For example, using a
-- @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric to
-- only MQTT topics where the name matches the pattern specified in the
-- dimension. This can\'t be used with custom metrics.
--
-- 'metric', 'behavior_metric' - What is measured by the behavior.
--
-- 'criteria', 'behavior_criteria' - The criteria that determine if a device is behaving normally in regard
-- to the @metric@.
--
-- 'name', 'behavior_name' - The name you\'ve given to the behavior.
newBehavior ::
  -- | 'name'
  Prelude.Text ->
  Behavior
newBehavior pName_ =
  Behavior'
    { suppressAlerts = Prelude.Nothing,
      metricDimension = Prelude.Nothing,
      metric = Prelude.Nothing,
      criteria = Prelude.Nothing,
      name = pName_
    }

-- | Suppresses alerts.
behavior_suppressAlerts :: Lens.Lens' Behavior (Prelude.Maybe Prelude.Bool)
behavior_suppressAlerts = Lens.lens (\Behavior' {suppressAlerts} -> suppressAlerts) (\s@Behavior' {} a -> s {suppressAlerts = a} :: Behavior)

-- | The dimension for a metric in your behavior. For example, using a
-- @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric to
-- only MQTT topics where the name matches the pattern specified in the
-- dimension. This can\'t be used with custom metrics.
behavior_metricDimension :: Lens.Lens' Behavior (Prelude.Maybe MetricDimension)
behavior_metricDimension = Lens.lens (\Behavior' {metricDimension} -> metricDimension) (\s@Behavior' {} a -> s {metricDimension = a} :: Behavior)

-- | What is measured by the behavior.
behavior_metric :: Lens.Lens' Behavior (Prelude.Maybe Prelude.Text)
behavior_metric = Lens.lens (\Behavior' {metric} -> metric) (\s@Behavior' {} a -> s {metric = a} :: Behavior)

-- | The criteria that determine if a device is behaving normally in regard
-- to the @metric@.
behavior_criteria :: Lens.Lens' Behavior (Prelude.Maybe BehaviorCriteria)
behavior_criteria = Lens.lens (\Behavior' {criteria} -> criteria) (\s@Behavior' {} a -> s {criteria = a} :: Behavior)

-- | The name you\'ve given to the behavior.
behavior_name :: Lens.Lens' Behavior Prelude.Text
behavior_name = Lens.lens (\Behavior' {name} -> name) (\s@Behavior' {} a -> s {name = a} :: Behavior)

instance Core.FromJSON Behavior where
  parseJSON =
    Core.withObject
      "Behavior"
      ( \x ->
          Behavior'
            Prelude.<$> (x Core..:? "suppressAlerts")
            Prelude.<*> (x Core..:? "metricDimension")
            Prelude.<*> (x Core..:? "metric")
            Prelude.<*> (x Core..:? "criteria")
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable Behavior

instance Prelude.NFData Behavior

instance Core.ToJSON Behavior where
  toJSON Behavior' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("suppressAlerts" Core..=)
              Prelude.<$> suppressAlerts,
            ("metricDimension" Core..=)
              Prelude.<$> metricDimension,
            ("metric" Core..=) Prelude.<$> metric,
            ("criteria" Core..=) Prelude.<$> criteria,
            Prelude.Just ("name" Core..= name)
          ]
      )
