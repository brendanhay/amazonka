{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.Behavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Behavior where

import Network.AWS.IoT.Types.BehaviorCriteria
import Network.AWS.IoT.Types.MetricDimension
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A Device Defender security profile behavior.
--
-- /See:/ 'newBehavior' smart constructor.
data Behavior = Behavior'
  { -- | The dimension for a metric in your behavior. For example, using a
    -- @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric to
    -- only MQTT topics where the name matches the pattern specified in the
    -- dimension. This can\'t be used with custom metrics.
    metricDimension :: Prelude.Maybe MetricDimension,
    -- | Suppresses alerts.
    suppressAlerts :: Prelude.Maybe Prelude.Bool,
    -- | What is measured by the behavior.
    metric :: Prelude.Maybe Prelude.Text,
    -- | The criteria that determine if a device is behaving normally in regard
    -- to the @metric@.
    criteria :: Prelude.Maybe BehaviorCriteria,
    -- | The name you\'ve given to the behavior.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Behavior' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDimension', 'behavior_metricDimension' - The dimension for a metric in your behavior. For example, using a
-- @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric to
-- only MQTT topics where the name matches the pattern specified in the
-- dimension. This can\'t be used with custom metrics.
--
-- 'suppressAlerts', 'behavior_suppressAlerts' - Suppresses alerts.
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
    { metricDimension = Prelude.Nothing,
      suppressAlerts = Prelude.Nothing,
      metric = Prelude.Nothing,
      criteria = Prelude.Nothing,
      name = pName_
    }

-- | The dimension for a metric in your behavior. For example, using a
-- @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric to
-- only MQTT topics where the name matches the pattern specified in the
-- dimension. This can\'t be used with custom metrics.
behavior_metricDimension :: Lens.Lens' Behavior (Prelude.Maybe MetricDimension)
behavior_metricDimension = Lens.lens (\Behavior' {metricDimension} -> metricDimension) (\s@Behavior' {} a -> s {metricDimension = a} :: Behavior)

-- | Suppresses alerts.
behavior_suppressAlerts :: Lens.Lens' Behavior (Prelude.Maybe Prelude.Bool)
behavior_suppressAlerts = Lens.lens (\Behavior' {suppressAlerts} -> suppressAlerts) (\s@Behavior' {} a -> s {suppressAlerts = a} :: Behavior)

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

instance Prelude.FromJSON Behavior where
  parseJSON =
    Prelude.withObject
      "Behavior"
      ( \x ->
          Behavior'
            Prelude.<$> (x Prelude..:? "metricDimension")
            Prelude.<*> (x Prelude..:? "suppressAlerts")
            Prelude.<*> (x Prelude..:? "metric")
            Prelude.<*> (x Prelude..:? "criteria")
            Prelude.<*> (x Prelude..: "name")
      )

instance Prelude.Hashable Behavior

instance Prelude.NFData Behavior

instance Prelude.ToJSON Behavior where
  toJSON Behavior' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("metricDimension" Prelude..=)
              Prelude.<$> metricDimension,
            ("suppressAlerts" Prelude..=)
              Prelude.<$> suppressAlerts,
            ("metric" Prelude..=) Prelude.<$> metric,
            ("criteria" Prelude..=) Prelude.<$> criteria,
            Prelude.Just ("name" Prelude..= name)
          ]
      )
