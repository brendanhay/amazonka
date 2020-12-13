{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Datapoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Datapoint
  ( Datapoint (..),

    -- * Smart constructor
    mkDatapoint,

    -- * Lenses
    dSampleCount,
    dMaximum,
    dAverage,
    dMinimum,
    dExtendedStatistics,
    dSum,
    dUnit,
    dTimestamp,
  )
where

import Network.AWS.CloudWatch.Types.StandardUnit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Encapsulates the statistical data that CloudWatch computes from metric data.
--
-- /See:/ 'mkDatapoint' smart constructor.
data Datapoint = Datapoint'
  { -- | The number of metric values that contributed to the aggregate value of this data point.
    sampleCount :: Lude.Maybe Lude.Double,
    -- | The maximum metric value for the data point.
    maximum :: Lude.Maybe Lude.Double,
    -- | The average of the metric values that correspond to the data point.
    average :: Lude.Maybe Lude.Double,
    -- | The minimum metric value for the data point.
    minimum :: Lude.Maybe Lude.Double,
    -- | The percentile statistic for the data point.
    extendedStatistics :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)),
    -- | The sum of the metric values for the data point.
    sum :: Lude.Maybe Lude.Double,
    -- | The standard unit for the data point.
    unit :: Lude.Maybe StandardUnit,
    -- | The time stamp used for the data point.
    timestamp :: Lude.Maybe Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Datapoint' with the minimum fields required to make a request.
--
-- * 'sampleCount' - The number of metric values that contributed to the aggregate value of this data point.
-- * 'maximum' - The maximum metric value for the data point.
-- * 'average' - The average of the metric values that correspond to the data point.
-- * 'minimum' - The minimum metric value for the data point.
-- * 'extendedStatistics' - The percentile statistic for the data point.
-- * 'sum' - The sum of the metric values for the data point.
-- * 'unit' - The standard unit for the data point.
-- * 'timestamp' - The time stamp used for the data point.
mkDatapoint ::
  Datapoint
mkDatapoint =
  Datapoint'
    { sampleCount = Lude.Nothing,
      maximum = Lude.Nothing,
      average = Lude.Nothing,
      minimum = Lude.Nothing,
      extendedStatistics = Lude.Nothing,
      sum = Lude.Nothing,
      unit = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | The number of metric values that contributed to the aggregate value of this data point.
--
-- /Note:/ Consider using 'sampleCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSampleCount :: Lens.Lens' Datapoint (Lude.Maybe Lude.Double)
dSampleCount = Lens.lens (sampleCount :: Datapoint -> Lude.Maybe Lude.Double) (\s a -> s {sampleCount = a} :: Datapoint)
{-# DEPRECATED dSampleCount "Use generic-lens or generic-optics with 'sampleCount' instead." #-}

-- | The maximum metric value for the data point.
--
-- /Note:/ Consider using 'maximum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaximum :: Lens.Lens' Datapoint (Lude.Maybe Lude.Double)
dMaximum = Lens.lens (maximum :: Datapoint -> Lude.Maybe Lude.Double) (\s a -> s {maximum = a} :: Datapoint)
{-# DEPRECATED dMaximum "Use generic-lens or generic-optics with 'maximum' instead." #-}

-- | The average of the metric values that correspond to the data point.
--
-- /Note:/ Consider using 'average' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAverage :: Lens.Lens' Datapoint (Lude.Maybe Lude.Double)
dAverage = Lens.lens (average :: Datapoint -> Lude.Maybe Lude.Double) (\s a -> s {average = a} :: Datapoint)
{-# DEPRECATED dAverage "Use generic-lens or generic-optics with 'average' instead." #-}

-- | The minimum metric value for the data point.
--
-- /Note:/ Consider using 'minimum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMinimum :: Lens.Lens' Datapoint (Lude.Maybe Lude.Double)
dMinimum = Lens.lens (minimum :: Datapoint -> Lude.Maybe Lude.Double) (\s a -> s {minimum = a} :: Datapoint)
{-# DEPRECATED dMinimum "Use generic-lens or generic-optics with 'minimum' instead." #-}

-- | The percentile statistic for the data point.
--
-- /Note:/ Consider using 'extendedStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExtendedStatistics :: Lens.Lens' Datapoint (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)))
dExtendedStatistics = Lens.lens (extendedStatistics :: Datapoint -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double))) (\s a -> s {extendedStatistics = a} :: Datapoint)
{-# DEPRECATED dExtendedStatistics "Use generic-lens or generic-optics with 'extendedStatistics' instead." #-}

-- | The sum of the metric values for the data point.
--
-- /Note:/ Consider using 'sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSum :: Lens.Lens' Datapoint (Lude.Maybe Lude.Double)
dSum = Lens.lens (sum :: Datapoint -> Lude.Maybe Lude.Double) (\s a -> s {sum = a} :: Datapoint)
{-# DEPRECATED dSum "Use generic-lens or generic-optics with 'sum' instead." #-}

-- | The standard unit for the data point.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUnit :: Lens.Lens' Datapoint (Lude.Maybe StandardUnit)
dUnit = Lens.lens (unit :: Datapoint -> Lude.Maybe StandardUnit) (\s a -> s {unit = a} :: Datapoint)
{-# DEPRECATED dUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

-- | The time stamp used for the data point.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTimestamp :: Lens.Lens' Datapoint (Lude.Maybe Lude.DateTime)
dTimestamp = Lens.lens (timestamp :: Datapoint -> Lude.Maybe Lude.DateTime) (\s a -> s {timestamp = a} :: Datapoint)
{-# DEPRECATED dTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromXML Datapoint where
  parseXML x =
    Datapoint'
      Lude.<$> (x Lude..@? "SampleCount")
      Lude.<*> (x Lude..@? "Maximum")
      Lude.<*> (x Lude..@? "Average")
      Lude.<*> (x Lude..@? "Minimum")
      Lude.<*> ( x Lude..@? "ExtendedStatistics" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLMap "entry" "key" "value")
               )
      Lude.<*> (x Lude..@? "Sum")
      Lude.<*> (x Lude..@? "Unit")
      Lude.<*> (x Lude..@? "Timestamp")
