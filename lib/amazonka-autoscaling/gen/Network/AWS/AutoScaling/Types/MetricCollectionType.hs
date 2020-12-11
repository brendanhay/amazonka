-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.MetricCollectionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.MetricCollectionType
  ( MetricCollectionType (..),

    -- * Smart constructor
    mkMetricCollectionType,

    -- * Lenses
    mctMetric,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a metric.
--
-- /See:/ 'mkMetricCollectionType' smart constructor.
newtype MetricCollectionType = MetricCollectionType'
  { metric ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricCollectionType' with the minimum fields required to make a request.
--
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
mkMetricCollectionType ::
  MetricCollectionType
mkMetricCollectionType =
  MetricCollectionType' {metric = Lude.Nothing}

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
mctMetric :: Lens.Lens' MetricCollectionType (Lude.Maybe Lude.Text)
mctMetric = Lens.lens (metric :: MetricCollectionType -> Lude.Maybe Lude.Text) (\s a -> s {metric = a} :: MetricCollectionType)
{-# DEPRECATED mctMetric "Use generic-lens or generic-optics with 'metric' instead." #-}

instance Lude.FromXML MetricCollectionType where
  parseXML x = MetricCollectionType' Lude.<$> (x Lude..@? "Metric")
