{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Behavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Behavior
  ( Behavior (..),

    -- * Smart constructor
    mkBehavior,

    -- * Lenses
    bMetricDimension,
    bMetric,
    bName,
    bCriteria,
  )
where

import Network.AWS.IoT.Types.BehaviorCriteria
import Network.AWS.IoT.Types.MetricDimension
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A Device Defender security profile behavior.
--
-- /See:/ 'mkBehavior' smart constructor.
data Behavior = Behavior'
  { -- | The dimension for a metric in your behavior. For example, using a @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric only to MQTT topics whose name match the pattern specified in the dimension.
    metricDimension :: Lude.Maybe MetricDimension,
    -- | What is measured by the behavior.
    metric :: Lude.Maybe Lude.Text,
    -- | The name you have given to the behavior.
    name :: Lude.Text,
    -- | The criteria that determine if a device is behaving normally in regard to the @metric@ .
    criteria :: Lude.Maybe BehaviorCriteria
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Behavior' with the minimum fields required to make a request.
--
-- * 'metricDimension' - The dimension for a metric in your behavior. For example, using a @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric only to MQTT topics whose name match the pattern specified in the dimension.
-- * 'metric' - What is measured by the behavior.
-- * 'name' - The name you have given to the behavior.
-- * 'criteria' - The criteria that determine if a device is behaving normally in regard to the @metric@ .
mkBehavior ::
  -- | 'name'
  Lude.Text ->
  Behavior
mkBehavior pName_ =
  Behavior'
    { metricDimension = Lude.Nothing,
      metric = Lude.Nothing,
      name = pName_,
      criteria = Lude.Nothing
    }

-- | The dimension for a metric in your behavior. For example, using a @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric only to MQTT topics whose name match the pattern specified in the dimension.
--
-- /Note:/ Consider using 'metricDimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bMetricDimension :: Lens.Lens' Behavior (Lude.Maybe MetricDimension)
bMetricDimension = Lens.lens (metricDimension :: Behavior -> Lude.Maybe MetricDimension) (\s a -> s {metricDimension = a} :: Behavior)
{-# DEPRECATED bMetricDimension "Use generic-lens or generic-optics with 'metricDimension' instead." #-}

-- | What is measured by the behavior.
--
-- /Note:/ Consider using 'metric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bMetric :: Lens.Lens' Behavior (Lude.Maybe Lude.Text)
bMetric = Lens.lens (metric :: Behavior -> Lude.Maybe Lude.Text) (\s a -> s {metric = a} :: Behavior)
{-# DEPRECATED bMetric "Use generic-lens or generic-optics with 'metric' instead." #-}

-- | The name you have given to the behavior.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bName :: Lens.Lens' Behavior Lude.Text
bName = Lens.lens (name :: Behavior -> Lude.Text) (\s a -> s {name = a} :: Behavior)
{-# DEPRECATED bName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The criteria that determine if a device is behaving normally in regard to the @metric@ .
--
-- /Note:/ Consider using 'criteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCriteria :: Lens.Lens' Behavior (Lude.Maybe BehaviorCriteria)
bCriteria = Lens.lens (criteria :: Behavior -> Lude.Maybe BehaviorCriteria) (\s a -> s {criteria = a} :: Behavior)
{-# DEPRECATED bCriteria "Use generic-lens or generic-optics with 'criteria' instead." #-}

instance Lude.FromJSON Behavior where
  parseJSON =
    Lude.withObject
      "Behavior"
      ( \x ->
          Behavior'
            Lude.<$> (x Lude..:? "metricDimension")
            Lude.<*> (x Lude..:? "metric")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..:? "criteria")
      )

instance Lude.ToJSON Behavior where
  toJSON Behavior' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("metricDimension" Lude..=) Lude.<$> metricDimension,
            ("metric" Lude..=) Lude.<$> metric,
            Lude.Just ("name" Lude..= name),
            ("criteria" Lude..=) Lude.<$> criteria
          ]
      )
