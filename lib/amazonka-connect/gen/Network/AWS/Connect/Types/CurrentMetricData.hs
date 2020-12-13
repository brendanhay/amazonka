{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.CurrentMetricData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.CurrentMetricData
  ( CurrentMetricData (..),

    -- * Smart constructor
    mkCurrentMetricData,

    -- * Lenses
    cmdValue,
    cmdMetric,
  )
where

import Network.AWS.Connect.Types.CurrentMetric
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the data for a real-time metric.
--
-- /See:/ 'mkCurrentMetricData' smart constructor.
data CurrentMetricData = CurrentMetricData'
  { -- | The value of the metric.
    value :: Lude.Maybe Lude.Double,
    -- | Information about the metric.
    metric :: Lude.Maybe CurrentMetric
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CurrentMetricData' with the minimum fields required to make a request.
--
-- * 'value' - The value of the metric.
-- * 'metric' - Information about the metric.
mkCurrentMetricData ::
  CurrentMetricData
mkCurrentMetricData =
  CurrentMetricData' {value = Lude.Nothing, metric = Lude.Nothing}

-- | The value of the metric.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdValue :: Lens.Lens' CurrentMetricData (Lude.Maybe Lude.Double)
cmdValue = Lens.lens (value :: CurrentMetricData -> Lude.Maybe Lude.Double) (\s a -> s {value = a} :: CurrentMetricData)
{-# DEPRECATED cmdValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Information about the metric.
--
-- /Note:/ Consider using 'metric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdMetric :: Lens.Lens' CurrentMetricData (Lude.Maybe CurrentMetric)
cmdMetric = Lens.lens (metric :: CurrentMetricData -> Lude.Maybe CurrentMetric) (\s a -> s {metric = a} :: CurrentMetricData)
{-# DEPRECATED cmdMetric "Use generic-lens or generic-optics with 'metric' instead." #-}

instance Lude.FromJSON CurrentMetricData where
  parseJSON =
    Lude.withObject
      "CurrentMetricData"
      ( \x ->
          CurrentMetricData'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Metric")
      )
