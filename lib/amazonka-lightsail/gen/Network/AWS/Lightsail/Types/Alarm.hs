{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Alarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Alarm
  ( Alarm (..),

    -- * Smart constructor
    mkAlarm,

    -- * Lenses
    aState,
    aTreatMissingData,
    aResourceType,
    aArn,
    aCreatedAt,
    aLocation,
    aContactProtocols,
    aPeriod,
    aEvaluationPeriods,
    aMetricName,
    aComparisonOperator,
    aName,
    aThreshold,
    aDatapointsToAlarm,
    aSupportCode,
    aNotificationEnabled,
    aNotificationTriggers,
    aStatistic,
    aUnit,
    aMonitoredResourceInfo,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AlarmState
import Network.AWS.Lightsail.Types.ComparisonOperator
import Network.AWS.Lightsail.Types.ContactProtocol
import Network.AWS.Lightsail.Types.MetricName
import Network.AWS.Lightsail.Types.MetricStatistic
import Network.AWS.Lightsail.Types.MetricUnit
import Network.AWS.Lightsail.Types.MonitoredResourceInfo
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.TreatMissingData
import qualified Network.AWS.Prelude as Lude

-- | Describes an alarm.
--
-- An alarm is a way to monitor your Amazon Lightsail resource metrics. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail> .
--
-- /See:/ 'mkAlarm' smart constructor.
data Alarm = Alarm'
  { -- | The current state of the alarm.
    --
    -- An alarm has the following possible states:
    --
    --     * @ALARM@ - The metric is outside of the defined threshold.
    --
    --
    --     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.
    --
    --
    --     * @OK@ - The metric is within the defined threshold.
    state :: Lude.Maybe AlarmState,
    -- | Specifies how the alarm handles missing data points.
    --
    -- An alarm can treat missing data in the following ways:
    --
    --     * @breaching@ - Assume the missing data is not within the threshold. Missing data counts towards the number of times the metric is not within the threshold.
    --
    --
    --     * @notBreaching@ - Assume the missing data is within the threshold. Missing data does not count towards the number of times the metric is not within the threshold.
    --
    --
    --     * @ignore@ - Ignore the missing data. Maintains the current alarm state.
    --
    --
    --     * @missing@ - Missing data is treated as missing.
    treatMissingData :: Lude.Maybe TreatMissingData,
    -- | The Lightsail resource type (e.g., @Alarm@ ).
    resourceType :: Lude.Maybe ResourceType,
    -- | The Amazon Resource Name (ARN) of the alarm.
    arn :: Lude.Maybe Lude.Text,
    -- | The timestamp when the alarm was created.
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | An object that lists information about the location of the alarm.
    location :: Lude.Maybe ResourceLocation,
    -- | The contact protocols for the alarm, such as @Email@ , @SMS@ (text messaging), or both.
    contactProtocols :: Lude.Maybe [ContactProtocol],
    -- | The period, in seconds, over which the statistic is applied.
    period :: Lude.Maybe Lude.Natural,
    -- | The number of periods over which data is compared to the specified threshold.
    evaluationPeriods :: Lude.Maybe Lude.Int,
    -- | The name of the metric associated with the alarm.
    metricName :: Lude.Maybe MetricName,
    -- | The arithmetic operation used when comparing the specified statistic and threshold.
    comparisonOperator :: Lude.Maybe ComparisonOperator,
    -- | The name of the alarm.
    name :: Lude.Maybe Lude.Text,
    -- | The value against which the specified statistic is compared.
    threshold :: Lude.Maybe Lude.Double,
    -- | The number of data points that must not within the specified threshold to trigger the alarm.
    datapointsToAlarm :: Lude.Maybe Lude.Int,
    -- | The support code. Include this code in your email to support when you have questions about your Lightsail alarm. This code enables our support team to look up your Lightsail information more easily.
    supportCode :: Lude.Maybe Lude.Text,
    -- | Indicates whether the alarm is enabled.
    notificationEnabled :: Lude.Maybe Lude.Bool,
    -- | The alarm states that trigger a notification.
    notificationTriggers :: Lude.Maybe [AlarmState],
    -- | The statistic for the metric associated with the alarm.
    --
    -- The following statistics are available:
    --
    --     * @Minimum@ - The lowest value observed during the specified period. Use this value to determine low volumes of activity for your application.
    --
    --
    --     * @Maximum@ - The highest value observed during the specified period. Use this value to determine high volumes of activity for your application.
    --
    --
    --     * @Sum@ - All values submitted for the matching metric added together. You can use this statistic to determine the total volume of a metric.
    --
    --
    --     * @Average@ - The value of Sum / SampleCount during the specified period. By comparing this statistic with the Minimum and Maximum values, you can determine the full scope of a metric and how close the average use is to the Minimum and Maximum values. This comparison helps you to know when to increase or decrease your resources.
    --
    --
    --     * @SampleCount@ - The count, or number, of data points used for the statistical calculation.
    statistic :: Lude.Maybe MetricStatistic,
    -- | The unit of the metric associated with the alarm.
    unit :: Lude.Maybe MetricUnit,
    -- | An object that lists information about the resource monitored by the alarm.
    monitoredResourceInfo :: Lude.Maybe MonitoredResourceInfo
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Alarm' with the minimum fields required to make a request.
--
-- * 'state' - The current state of the alarm.
--
-- An alarm has the following possible states:
--
--     * @ALARM@ - The metric is outside of the defined threshold.
--
--
--     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.
--
--
--     * @OK@ - The metric is within the defined threshold.
--
--
-- * 'treatMissingData' - Specifies how the alarm handles missing data points.
--
-- An alarm can treat missing data in the following ways:
--
--     * @breaching@ - Assume the missing data is not within the threshold. Missing data counts towards the number of times the metric is not within the threshold.
--
--
--     * @notBreaching@ - Assume the missing data is within the threshold. Missing data does not count towards the number of times the metric is not within the threshold.
--
--
--     * @ignore@ - Ignore the missing data. Maintains the current alarm state.
--
--
--     * @missing@ - Missing data is treated as missing.
--
--
-- * 'resourceType' - The Lightsail resource type (e.g., @Alarm@ ).
-- * 'arn' - The Amazon Resource Name (ARN) of the alarm.
-- * 'createdAt' - The timestamp when the alarm was created.
-- * 'location' - An object that lists information about the location of the alarm.
-- * 'contactProtocols' - The contact protocols for the alarm, such as @Email@ , @SMS@ (text messaging), or both.
-- * 'period' - The period, in seconds, over which the statistic is applied.
-- * 'evaluationPeriods' - The number of periods over which data is compared to the specified threshold.
-- * 'metricName' - The name of the metric associated with the alarm.
-- * 'comparisonOperator' - The arithmetic operation used when comparing the specified statistic and threshold.
-- * 'name' - The name of the alarm.
-- * 'threshold' - The value against which the specified statistic is compared.
-- * 'datapointsToAlarm' - The number of data points that must not within the specified threshold to trigger the alarm.
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about your Lightsail alarm. This code enables our support team to look up your Lightsail information more easily.
-- * 'notificationEnabled' - Indicates whether the alarm is enabled.
-- * 'notificationTriggers' - The alarm states that trigger a notification.
-- * 'statistic' - The statistic for the metric associated with the alarm.
--
-- The following statistics are available:
--
--     * @Minimum@ - The lowest value observed during the specified period. Use this value to determine low volumes of activity for your application.
--
--
--     * @Maximum@ - The highest value observed during the specified period. Use this value to determine high volumes of activity for your application.
--
--
--     * @Sum@ - All values submitted for the matching metric added together. You can use this statistic to determine the total volume of a metric.
--
--
--     * @Average@ - The value of Sum / SampleCount during the specified period. By comparing this statistic with the Minimum and Maximum values, you can determine the full scope of a metric and how close the average use is to the Minimum and Maximum values. This comparison helps you to know when to increase or decrease your resources.
--
--
--     * @SampleCount@ - The count, or number, of data points used for the statistical calculation.
--
--
-- * 'unit' - The unit of the metric associated with the alarm.
-- * 'monitoredResourceInfo' - An object that lists information about the resource monitored by the alarm.
mkAlarm ::
  Alarm
mkAlarm =
  Alarm'
    { state = Lude.Nothing,
      treatMissingData = Lude.Nothing,
      resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      contactProtocols = Lude.Nothing,
      period = Lude.Nothing,
      evaluationPeriods = Lude.Nothing,
      metricName = Lude.Nothing,
      comparisonOperator = Lude.Nothing,
      name = Lude.Nothing,
      threshold = Lude.Nothing,
      datapointsToAlarm = Lude.Nothing,
      supportCode = Lude.Nothing,
      notificationEnabled = Lude.Nothing,
      notificationTriggers = Lude.Nothing,
      statistic = Lude.Nothing,
      unit = Lude.Nothing,
      monitoredResourceInfo = Lude.Nothing
    }

-- | The current state of the alarm.
--
-- An alarm has the following possible states:
--
--     * @ALARM@ - The metric is outside of the defined threshold.
--
--
--     * @INSUFFICIENT_DATA@ - The alarm has just started, the metric is not available, or not enough data is available for the metric to determine the alarm state.
--
--
--     * @OK@ - The metric is within the defined threshold.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aState :: Lens.Lens' Alarm (Lude.Maybe AlarmState)
aState = Lens.lens (state :: Alarm -> Lude.Maybe AlarmState) (\s a -> s {state = a} :: Alarm)
{-# DEPRECATED aState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Specifies how the alarm handles missing data points.
--
-- An alarm can treat missing data in the following ways:
--
--     * @breaching@ - Assume the missing data is not within the threshold. Missing data counts towards the number of times the metric is not within the threshold.
--
--
--     * @notBreaching@ - Assume the missing data is within the threshold. Missing data does not count towards the number of times the metric is not within the threshold.
--
--
--     * @ignore@ - Ignore the missing data. Maintains the current alarm state.
--
--
--     * @missing@ - Missing data is treated as missing.
--
--
--
-- /Note:/ Consider using 'treatMissingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTreatMissingData :: Lens.Lens' Alarm (Lude.Maybe TreatMissingData)
aTreatMissingData = Lens.lens (treatMissingData :: Alarm -> Lude.Maybe TreatMissingData) (\s a -> s {treatMissingData = a} :: Alarm)
{-# DEPRECATED aTreatMissingData "Use generic-lens or generic-optics with 'treatMissingData' instead." #-}

-- | The Lightsail resource type (e.g., @Alarm@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aResourceType :: Lens.Lens' Alarm (Lude.Maybe ResourceType)
aResourceType = Lens.lens (resourceType :: Alarm -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: Alarm)
{-# DEPRECATED aResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the alarm.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aArn :: Lens.Lens' Alarm (Lude.Maybe Lude.Text)
aArn = Lens.lens (arn :: Alarm -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Alarm)
{-# DEPRECATED aArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp when the alarm was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreatedAt :: Lens.Lens' Alarm (Lude.Maybe Lude.Timestamp)
aCreatedAt = Lens.lens (createdAt :: Alarm -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Alarm)
{-# DEPRECATED aCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | An object that lists information about the location of the alarm.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLocation :: Lens.Lens' Alarm (Lude.Maybe ResourceLocation)
aLocation = Lens.lens (location :: Alarm -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: Alarm)
{-# DEPRECATED aLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The contact protocols for the alarm, such as @Email@ , @SMS@ (text messaging), or both.
--
-- /Note:/ Consider using 'contactProtocols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aContactProtocols :: Lens.Lens' Alarm (Lude.Maybe [ContactProtocol])
aContactProtocols = Lens.lens (contactProtocols :: Alarm -> Lude.Maybe [ContactProtocol]) (\s a -> s {contactProtocols = a} :: Alarm)
{-# DEPRECATED aContactProtocols "Use generic-lens or generic-optics with 'contactProtocols' instead." #-}

-- | The period, in seconds, over which the statistic is applied.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPeriod :: Lens.Lens' Alarm (Lude.Maybe Lude.Natural)
aPeriod = Lens.lens (period :: Alarm -> Lude.Maybe Lude.Natural) (\s a -> s {period = a} :: Alarm)
{-# DEPRECATED aPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The number of periods over which data is compared to the specified threshold.
--
-- /Note:/ Consider using 'evaluationPeriods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEvaluationPeriods :: Lens.Lens' Alarm (Lude.Maybe Lude.Int)
aEvaluationPeriods = Lens.lens (evaluationPeriods :: Alarm -> Lude.Maybe Lude.Int) (\s a -> s {evaluationPeriods = a} :: Alarm)
{-# DEPRECATED aEvaluationPeriods "Use generic-lens or generic-optics with 'evaluationPeriods' instead." #-}

-- | The name of the metric associated with the alarm.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMetricName :: Lens.Lens' Alarm (Lude.Maybe MetricName)
aMetricName = Lens.lens (metricName :: Alarm -> Lude.Maybe MetricName) (\s a -> s {metricName = a} :: Alarm)
{-# DEPRECATED aMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The arithmetic operation used when comparing the specified statistic and threshold.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aComparisonOperator :: Lens.Lens' Alarm (Lude.Maybe ComparisonOperator)
aComparisonOperator = Lens.lens (comparisonOperator :: Alarm -> Lude.Maybe ComparisonOperator) (\s a -> s {comparisonOperator = a} :: Alarm)
{-# DEPRECATED aComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | The name of the alarm.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Alarm (Lude.Maybe Lude.Text)
aName = Lens.lens (name :: Alarm -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Alarm)
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value against which the specified statistic is compared.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aThreshold :: Lens.Lens' Alarm (Lude.Maybe Lude.Double)
aThreshold = Lens.lens (threshold :: Alarm -> Lude.Maybe Lude.Double) (\s a -> s {threshold = a} :: Alarm)
{-# DEPRECATED aThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | The number of data points that must not within the specified threshold to trigger the alarm.
--
-- /Note:/ Consider using 'datapointsToAlarm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDatapointsToAlarm :: Lens.Lens' Alarm (Lude.Maybe Lude.Int)
aDatapointsToAlarm = Lens.lens (datapointsToAlarm :: Alarm -> Lude.Maybe Lude.Int) (\s a -> s {datapointsToAlarm = a} :: Alarm)
{-# DEPRECATED aDatapointsToAlarm "Use generic-lens or generic-optics with 'datapointsToAlarm' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about your Lightsail alarm. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSupportCode :: Lens.Lens' Alarm (Lude.Maybe Lude.Text)
aSupportCode = Lens.lens (supportCode :: Alarm -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: Alarm)
{-# DEPRECATED aSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | Indicates whether the alarm is enabled.
--
-- /Note:/ Consider using 'notificationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNotificationEnabled :: Lens.Lens' Alarm (Lude.Maybe Lude.Bool)
aNotificationEnabled = Lens.lens (notificationEnabled :: Alarm -> Lude.Maybe Lude.Bool) (\s a -> s {notificationEnabled = a} :: Alarm)
{-# DEPRECATED aNotificationEnabled "Use generic-lens or generic-optics with 'notificationEnabled' instead." #-}

-- | The alarm states that trigger a notification.
--
-- /Note:/ Consider using 'notificationTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNotificationTriggers :: Lens.Lens' Alarm (Lude.Maybe [AlarmState])
aNotificationTriggers = Lens.lens (notificationTriggers :: Alarm -> Lude.Maybe [AlarmState]) (\s a -> s {notificationTriggers = a} :: Alarm)
{-# DEPRECATED aNotificationTriggers "Use generic-lens or generic-optics with 'notificationTriggers' instead." #-}

-- | The statistic for the metric associated with the alarm.
--
-- The following statistics are available:
--
--     * @Minimum@ - The lowest value observed during the specified period. Use this value to determine low volumes of activity for your application.
--
--
--     * @Maximum@ - The highest value observed during the specified period. Use this value to determine high volumes of activity for your application.
--
--
--     * @Sum@ - All values submitted for the matching metric added together. You can use this statistic to determine the total volume of a metric.
--
--
--     * @Average@ - The value of Sum / SampleCount during the specified period. By comparing this statistic with the Minimum and Maximum values, you can determine the full scope of a metric and how close the average use is to the Minimum and Maximum values. This comparison helps you to know when to increase or decrease your resources.
--
--
--     * @SampleCount@ - The count, or number, of data points used for the statistical calculation.
--
--
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStatistic :: Lens.Lens' Alarm (Lude.Maybe MetricStatistic)
aStatistic = Lens.lens (statistic :: Alarm -> Lude.Maybe MetricStatistic) (\s a -> s {statistic = a} :: Alarm)
{-# DEPRECATED aStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

-- | The unit of the metric associated with the alarm.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aUnit :: Lens.Lens' Alarm (Lude.Maybe MetricUnit)
aUnit = Lens.lens (unit :: Alarm -> Lude.Maybe MetricUnit) (\s a -> s {unit = a} :: Alarm)
{-# DEPRECATED aUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

-- | An object that lists information about the resource monitored by the alarm.
--
-- /Note:/ Consider using 'monitoredResourceInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMonitoredResourceInfo :: Lens.Lens' Alarm (Lude.Maybe MonitoredResourceInfo)
aMonitoredResourceInfo = Lens.lens (monitoredResourceInfo :: Alarm -> Lude.Maybe MonitoredResourceInfo) (\s a -> s {monitoredResourceInfo = a} :: Alarm)
{-# DEPRECATED aMonitoredResourceInfo "Use generic-lens or generic-optics with 'monitoredResourceInfo' instead." #-}

instance Lude.FromJSON Alarm where
  parseJSON =
    Lude.withObject
      "Alarm"
      ( \x ->
          Alarm'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "treatMissingData")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "contactProtocols" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "period")
            Lude.<*> (x Lude..:? "evaluationPeriods")
            Lude.<*> (x Lude..:? "metricName")
            Lude.<*> (x Lude..:? "comparisonOperator")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "threshold")
            Lude.<*> (x Lude..:? "datapointsToAlarm")
            Lude.<*> (x Lude..:? "supportCode")
            Lude.<*> (x Lude..:? "notificationEnabled")
            Lude.<*> (x Lude..:? "notificationTriggers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "statistic")
            Lude.<*> (x Lude..:? "unit")
            Lude.<*> (x Lude..:? "monitoredResourceInfo")
      )
