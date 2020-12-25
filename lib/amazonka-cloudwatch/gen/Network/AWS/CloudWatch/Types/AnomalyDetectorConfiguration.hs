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
    adcExcludedTimeRanges,
    adcMetricTimezone,
  )
where

import qualified Network.AWS.CloudWatch.Types.AnomalyDetectorMetricTimezone as Types
import qualified Network.AWS.CloudWatch.Types.Range as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration specifies details about how the anomaly detection model is to be trained, including time ranges to exclude from use for training the model and the time zone to use for the metric.
--
-- /See:/ 'mkAnomalyDetectorConfiguration' smart constructor.
data AnomalyDetectorConfiguration = AnomalyDetectorConfiguration'
  { -- | An array of time ranges to exclude from use when the anomaly detection model is trained. Use this to make sure that events that could cause unusual values for the metric, such as deployments, aren't used when CloudWatch creates the model.
    excludedTimeRanges :: Core.Maybe [Types.Range],
    -- | The time zone to use for the metric. This is useful to enable the model to automatically account for daylight savings time changes if the metric is sensitive to such time changes.
    --
    -- To specify a time zone, use the name of the time zone as specified in the standard tz database. For more information, see <https://en.wikipedia.org/wiki/Tz_database tz database> .
    metricTimezone :: Core.Maybe Types.AnomalyDetectorMetricTimezone
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AnomalyDetectorConfiguration' value with any optional fields omitted.
mkAnomalyDetectorConfiguration ::
  AnomalyDetectorConfiguration
mkAnomalyDetectorConfiguration =
  AnomalyDetectorConfiguration'
    { excludedTimeRanges = Core.Nothing,
      metricTimezone = Core.Nothing
    }

-- | An array of time ranges to exclude from use when the anomaly detection model is trained. Use this to make sure that events that could cause unusual values for the metric, such as deployments, aren't used when CloudWatch creates the model.
--
-- /Note:/ Consider using 'excludedTimeRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcExcludedTimeRanges :: Lens.Lens' AnomalyDetectorConfiguration (Core.Maybe [Types.Range])
adcExcludedTimeRanges = Lens.field @"excludedTimeRanges"
{-# DEPRECATED adcExcludedTimeRanges "Use generic-lens or generic-optics with 'excludedTimeRanges' instead." #-}

-- | The time zone to use for the metric. This is useful to enable the model to automatically account for daylight savings time changes if the metric is sensitive to such time changes.
--
-- To specify a time zone, use the name of the time zone as specified in the standard tz database. For more information, see <https://en.wikipedia.org/wiki/Tz_database tz database> .
--
-- /Note:/ Consider using 'metricTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adcMetricTimezone :: Lens.Lens' AnomalyDetectorConfiguration (Core.Maybe Types.AnomalyDetectorMetricTimezone)
adcMetricTimezone = Lens.field @"metricTimezone"
{-# DEPRECATED adcMetricTimezone "Use generic-lens or generic-optics with 'metricTimezone' instead." #-}

instance Core.FromXML AnomalyDetectorConfiguration where
  parseXML x =
    AnomalyDetectorConfiguration'
      Core.<$> ( x Core..@? "ExcludedTimeRanges"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "MetricTimezone")
