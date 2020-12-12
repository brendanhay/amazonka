{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.EnabledMetric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.EnabledMetric
  ( EnabledMetric (..),

    -- * Smart constructor
    mkEnabledMetric,

    -- * Lenses
    emGranularity,
    emMetric,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an enabled metric.
--
-- /See:/ 'mkEnabledMetric' smart constructor.
data EnabledMetric = EnabledMetric'
  { granularity ::
      Lude.Maybe Lude.Text,
    metric :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnabledMetric' with the minimum fields required to make a request.
--
-- * 'granularity' - The granularity of the metric. The only valid value is @1Minute@ .
-- * 'metric' - One of the following metrics:
--
--
--     * @GroupMinSize@
--
--
--     * @GroupMaxSize@
--
--
--     * @GroupDesiredCapacity@
--
--
--     * @GroupInServiceInstances@
--
--
--     * @GroupPendingInstances@
--
--
--     * @GroupStandbyInstances@
--
--
--     * @GroupTerminatingInstances@
--
--
--     * @GroupTotalInstances@
--
--
--     * @GroupInServiceCapacity@
--
--
--     * @GroupPendingCapacity@
--
--
--     * @GroupStandbyCapacity@
--
--
--     * @GroupTerminatingCapacity@
--
--
--     * @GroupTotalCapacity@
mkEnabledMetric ::
  EnabledMetric
mkEnabledMetric =
  EnabledMetric' {granularity = Lude.Nothing, metric = Lude.Nothing}

-- | The granularity of the metric. The only valid value is @1Minute@ .
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emGranularity :: Lens.Lens' EnabledMetric (Lude.Maybe Lude.Text)
emGranularity = Lens.lens (granularity :: EnabledMetric -> Lude.Maybe Lude.Text) (\s a -> s {granularity = a} :: EnabledMetric)
{-# DEPRECATED emGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

-- | One of the following metrics:
--
--
--     * @GroupMinSize@
--
--
--     * @GroupMaxSize@
--
--
--     * @GroupDesiredCapacity@
--
--
--     * @GroupInServiceInstances@
--
--
--     * @GroupPendingInstances@
--
--
--     * @GroupStandbyInstances@
--
--
--     * @GroupTerminatingInstances@
--
--
--     * @GroupTotalInstances@
--
--
--     * @GroupInServiceCapacity@
--
--
--     * @GroupPendingCapacity@
--
--
--     * @GroupStandbyCapacity@
--
--
--     * @GroupTerminatingCapacity@
--
--
--     * @GroupTotalCapacity@
--
--
--
-- /Note:/ Consider using 'metric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emMetric :: Lens.Lens' EnabledMetric (Lude.Maybe Lude.Text)
emMetric = Lens.lens (metric :: EnabledMetric -> Lude.Maybe Lude.Text) (\s a -> s {metric = a} :: EnabledMetric)
{-# DEPRECATED emMetric "Use generic-lens or generic-optics with 'metric' instead." #-}

instance Lude.FromXML EnabledMetric where
  parseXML x =
    EnabledMetric'
      Lude.<$> (x Lude..@? "Granularity") Lude.<*> (x Lude..@? "Metric")
