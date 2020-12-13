{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HistoricalMetricData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HistoricalMetricData
  ( HistoricalMetricData (..),

    -- * Smart constructor
    mkHistoricalMetricData,

    -- * Lenses
    hmdValue,
    hmdMetric,
  )
where

import Network.AWS.Connect.Types.HistoricalMetric
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the data for a historical metric.
--
-- /See:/ 'mkHistoricalMetricData' smart constructor.
data HistoricalMetricData = HistoricalMetricData'
  { -- | The value of the metric.
    value :: Lude.Maybe Lude.Double,
    -- | Information about the metric.
    metric :: Lude.Maybe HistoricalMetric
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HistoricalMetricData' with the minimum fields required to make a request.
--
-- * 'value' - The value of the metric.
-- * 'metric' - Information about the metric.
mkHistoricalMetricData ::
  HistoricalMetricData
mkHistoricalMetricData =
  HistoricalMetricData'
    { value = Lude.Nothing,
      metric = Lude.Nothing
    }

-- | The value of the metric.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmdValue :: Lens.Lens' HistoricalMetricData (Lude.Maybe Lude.Double)
hmdValue = Lens.lens (value :: HistoricalMetricData -> Lude.Maybe Lude.Double) (\s a -> s {value = a} :: HistoricalMetricData)
{-# DEPRECATED hmdValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Information about the metric.
--
-- /Note:/ Consider using 'metric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hmdMetric :: Lens.Lens' HistoricalMetricData (Lude.Maybe HistoricalMetric)
hmdMetric = Lens.lens (metric :: HistoricalMetricData -> Lude.Maybe HistoricalMetric) (\s a -> s {metric = a} :: HistoricalMetricData)
{-# DEPRECATED hmdMetric "Use generic-lens or generic-optics with 'metric' instead." #-}

instance Lude.FromJSON HistoricalMetricData where
  parseJSON =
    Lude.withObject
      "HistoricalMetricData"
      ( \x ->
          HistoricalMetricData'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Metric")
      )
