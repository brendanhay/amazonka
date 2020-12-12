{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.AnomalyDetectorConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.AnomalyDetectorConfiguration
  ( AnomalyDetectorConfiguration (..),

    -- * Smart constructor
    mkAnomalyDetectorConfiguration,

    -- * Lenses
    adcMetricTimezone,
    adcExcludedTimeRanges,
  )
where

import Network.AWS.CloudWatch.Types.Range
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude from use for training the model and the time zone to use for the metric.
--
-- /See:/ 'mkAnomalyDetectorConfiguration' smart constructor.
data AnomalyDetectorConfiguration = AnomalyDetectorConfiguration'
  { metricTimezone ::
      Lude.Maybe Lude.Text,
    excludedTimeRanges ::
      Lude.Maybe [Range]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnomalyDetectorConfiguration' with the minimum fields required to make a request.
--
-- * 'excludedTimeRanges' - An array of time ranges to exclude from use when the anomaly detection model is trained. Use this to make sure that events that could cause unusual values for the metric, such as deployments, aren't used when CloudWatch creates the model.
-- * 'metricTimezone' - The time zone to use for the metric. This is useful to enable the model to automatically account for daylight savings time changes if the metric is sensitive to such time changes.
--
-- To specify a time zone, use the name of the time zone as specified in the standard tz database. For more information, see <https://en.wikipedia.org/wiki/Tz_database tz database> .
mkAnomalyDetectorConfiguration ::
  AnomalyDetectorConfiguration
mkAnomalyDetectorConfiguration =
  AnomalyDetectorConfiguration'
    { metricTimezone = Lude.Nothing,
      excludedTimeRanges = Lude.Nothing
    }

-- | The time zone to use for the metric. This is useful to enable the model to automatically account for daylight savings time changes if the metric is sensitive to such time changes.
--
-- To specify a time zone, use the name of the time zone as specified in the standard tz database. For more information, see <https://en.wikipedia.org/wiki/Tz_database tz database> .
--
-- /Note:/ Consider using 'metricTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcMetricTimezone :: Lens.Lens' AnomalyDetectorConfiguration (Lude.Maybe Lude.Text)
adcMetricTimezone = Lens.lens (metricTimezone :: AnomalyDetectorConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {metricTimezone = a} :: AnomalyDetectorConfiguration)
{-# DEPRECATED adcMetricTimezone "Use generic-lens or generic-optics with 'metricTimezone' instead." #-}

-- | An array of time ranges to exclude from use when the anomaly detection model is trained. Use this to make sure that events that could cause unusual values for the metric, such as deployments, aren't used when CloudWatch creates the model.
--
-- /Note:/ Consider using 'excludedTimeRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcExcludedTimeRanges :: Lens.Lens' AnomalyDetectorConfiguration (Lude.Maybe [Range])
adcExcludedTimeRanges = Lens.lens (excludedTimeRanges :: AnomalyDetectorConfiguration -> Lude.Maybe [Range]) (\s a -> s {excludedTimeRanges = a} :: AnomalyDetectorConfiguration)
{-# DEPRECATED adcExcludedTimeRanges "Use generic-lens or generic-optics with 'excludedTimeRanges' instead." #-}

instance Lude.FromXML AnomalyDetectorConfiguration where
  parseXML x =
    AnomalyDetectorConfiguration'
      Lude.<$> (x Lude..@? "MetricTimezone")
      Lude.<*> ( x Lude..@? "ExcludedTimeRanges" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )

instance Lude.ToQuery AnomalyDetectorConfiguration where
  toQuery AnomalyDetectorConfiguration' {..} =
    Lude.mconcat
      [ "MetricTimezone" Lude.=: metricTimezone,
        "ExcludedTimeRanges"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> excludedTimeRanges)
      ]
