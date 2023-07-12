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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.Behavior where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.BehaviorCriteria
import Amazonka.IoT.Types.MetricDimension
import qualified Amazonka.Prelude as Prelude

-- | A Device Defender security profile behavior.
--
-- /See:/ 'newBehavior' smart constructor.
data Behavior = Behavior'
  { -- | The criteria that determine if a device is behaving normally in regard
    -- to the @metric@.
    criteria :: Prelude.Maybe BehaviorCriteria,
    -- | What is measured by the behavior.
    metric :: Prelude.Maybe Prelude.Text,
    -- | The dimension for a metric in your behavior. For example, using a
    -- @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric to
    -- only MQTT topics where the name matches the pattern specified in the
    -- dimension. This can\'t be used with custom metrics.
    metricDimension :: Prelude.Maybe MetricDimension,
    -- | Suppresses alerts.
    suppressAlerts :: Prelude.Maybe Prelude.Bool,
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
-- 'criteria', 'behavior_criteria' - The criteria that determine if a device is behaving normally in regard
-- to the @metric@.
--
-- 'metric', 'behavior_metric' - What is measured by the behavior.
--
-- 'metricDimension', 'behavior_metricDimension' - The dimension for a metric in your behavior. For example, using a
-- @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric to
-- only MQTT topics where the name matches the pattern specified in the
-- dimension. This can\'t be used with custom metrics.
--
-- 'suppressAlerts', 'behavior_suppressAlerts' - Suppresses alerts.
--
-- 'name', 'behavior_name' - The name you\'ve given to the behavior.
newBehavior ::
  -- | 'name'
  Prelude.Text ->
  Behavior
newBehavior pName_ =
  Behavior'
    { criteria = Prelude.Nothing,
      metric = Prelude.Nothing,
      metricDimension = Prelude.Nothing,
      suppressAlerts = Prelude.Nothing,
      name = pName_
    }

-- | The criteria that determine if a device is behaving normally in regard
-- to the @metric@.
behavior_criteria :: Lens.Lens' Behavior (Prelude.Maybe BehaviorCriteria)
behavior_criteria = Lens.lens (\Behavior' {criteria} -> criteria) (\s@Behavior' {} a -> s {criteria = a} :: Behavior)

-- | What is measured by the behavior.
behavior_metric :: Lens.Lens' Behavior (Prelude.Maybe Prelude.Text)
behavior_metric = Lens.lens (\Behavior' {metric} -> metric) (\s@Behavior' {} a -> s {metric = a} :: Behavior)

-- | The dimension for a metric in your behavior. For example, using a
-- @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric to
-- only MQTT topics where the name matches the pattern specified in the
-- dimension. This can\'t be used with custom metrics.
behavior_metricDimension :: Lens.Lens' Behavior (Prelude.Maybe MetricDimension)
behavior_metricDimension = Lens.lens (\Behavior' {metricDimension} -> metricDimension) (\s@Behavior' {} a -> s {metricDimension = a} :: Behavior)

-- | Suppresses alerts.
behavior_suppressAlerts :: Lens.Lens' Behavior (Prelude.Maybe Prelude.Bool)
behavior_suppressAlerts = Lens.lens (\Behavior' {suppressAlerts} -> suppressAlerts) (\s@Behavior' {} a -> s {suppressAlerts = a} :: Behavior)

-- | The name you\'ve given to the behavior.
behavior_name :: Lens.Lens' Behavior Prelude.Text
behavior_name = Lens.lens (\Behavior' {name} -> name) (\s@Behavior' {} a -> s {name = a} :: Behavior)

instance Data.FromJSON Behavior where
  parseJSON =
    Data.withObject
      "Behavior"
      ( \x ->
          Behavior'
            Prelude.<$> (x Data..:? "criteria")
            Prelude.<*> (x Data..:? "metric")
            Prelude.<*> (x Data..:? "metricDimension")
            Prelude.<*> (x Data..:? "suppressAlerts")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable Behavior where
  hashWithSalt _salt Behavior' {..} =
    _salt
      `Prelude.hashWithSalt` criteria
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` metricDimension
      `Prelude.hashWithSalt` suppressAlerts
      `Prelude.hashWithSalt` name

instance Prelude.NFData Behavior where
  rnf Behavior' {..} =
    Prelude.rnf criteria
      `Prelude.seq` Prelude.rnf metric
      `Prelude.seq` Prelude.rnf metricDimension
      `Prelude.seq` Prelude.rnf suppressAlerts
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON Behavior where
  toJSON Behavior' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("criteria" Data..=) Prelude.<$> criteria,
            ("metric" Data..=) Prelude.<$> metric,
            ("metricDimension" Data..=)
              Prelude.<$> metricDimension,
            ("suppressAlerts" Data..=)
              Prelude.<$> suppressAlerts,
            Prelude.Just ("name" Data..= name)
          ]
      )
