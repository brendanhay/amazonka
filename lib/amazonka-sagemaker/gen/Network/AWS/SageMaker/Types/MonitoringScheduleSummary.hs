{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringScheduleSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringScheduleSummary
  ( MonitoringScheduleSummary (..),

    -- * Smart constructor
    mkMonitoringScheduleSummary,

    -- * Lenses
    mssEndpointName,
    mssMonitoringScheduleName,
    mssMonitoringScheduleARN,
    mssCreationTime,
    mssLastModifiedTime,
    mssMonitoringScheduleStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ScheduleStatus

-- | Summarizes the monitoring schedule.
--
-- /See:/ 'mkMonitoringScheduleSummary' smart constructor.
data MonitoringScheduleSummary = MonitoringScheduleSummary'
  { endpointName ::
      Lude.Maybe Lude.Text,
    monitoringScheduleName :: Lude.Text,
    monitoringScheduleARN :: Lude.Text,
    creationTime :: Lude.Timestamp,
    lastModifiedTime :: Lude.Timestamp,
    monitoringScheduleStatus ::
      ScheduleStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoringScheduleSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - The creation time of the monitoring schedule.
-- * 'endpointName' - The name of the endpoint using the monitoring schedule.
-- * 'lastModifiedTime' - The last time the monitoring schedule was modified.
-- * 'monitoringScheduleARN' - The Amazon Resource Name (ARN) of the monitoring schedule.
-- * 'monitoringScheduleName' - The name of the monitoring schedule.
-- * 'monitoringScheduleStatus' - The status of the monitoring schedule.
mkMonitoringScheduleSummary ::
  -- | 'monitoringScheduleName'
  Lude.Text ->
  -- | 'monitoringScheduleARN'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  -- | 'monitoringScheduleStatus'
  ScheduleStatus ->
  MonitoringScheduleSummary
mkMonitoringScheduleSummary
  pMonitoringScheduleName_
  pMonitoringScheduleARN_
  pCreationTime_
  pLastModifiedTime_
  pMonitoringScheduleStatus_ =
    MonitoringScheduleSummary'
      { endpointName = Lude.Nothing,
        monitoringScheduleName = pMonitoringScheduleName_,
        monitoringScheduleARN = pMonitoringScheduleARN_,
        creationTime = pCreationTime_,
        lastModifiedTime = pLastModifiedTime_,
        monitoringScheduleStatus = pMonitoringScheduleStatus_
      }

-- | The name of the endpoint using the monitoring schedule.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssEndpointName :: Lens.Lens' MonitoringScheduleSummary (Lude.Maybe Lude.Text)
mssEndpointName = Lens.lens (endpointName :: MonitoringScheduleSummary -> Lude.Maybe Lude.Text) (\s a -> s {endpointName = a} :: MonitoringScheduleSummary)
{-# DEPRECATED mssEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | The name of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssMonitoringScheduleName :: Lens.Lens' MonitoringScheduleSummary Lude.Text
mssMonitoringScheduleName = Lens.lens (monitoringScheduleName :: MonitoringScheduleSummary -> Lude.Text) (\s a -> s {monitoringScheduleName = a} :: MonitoringScheduleSummary)
{-# DEPRECATED mssMonitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead." #-}

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssMonitoringScheduleARN :: Lens.Lens' MonitoringScheduleSummary Lude.Text
mssMonitoringScheduleARN = Lens.lens (monitoringScheduleARN :: MonitoringScheduleSummary -> Lude.Text) (\s a -> s {monitoringScheduleARN = a} :: MonitoringScheduleSummary)
{-# DEPRECATED mssMonitoringScheduleARN "Use generic-lens or generic-optics with 'monitoringScheduleARN' instead." #-}

-- | The creation time of the monitoring schedule.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssCreationTime :: Lens.Lens' MonitoringScheduleSummary Lude.Timestamp
mssCreationTime = Lens.lens (creationTime :: MonitoringScheduleSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: MonitoringScheduleSummary)
{-# DEPRECATED mssCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The last time the monitoring schedule was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssLastModifiedTime :: Lens.Lens' MonitoringScheduleSummary Lude.Timestamp
mssLastModifiedTime = Lens.lens (lastModifiedTime :: MonitoringScheduleSummary -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: MonitoringScheduleSummary)
{-# DEPRECATED mssLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The status of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssMonitoringScheduleStatus :: Lens.Lens' MonitoringScheduleSummary ScheduleStatus
mssMonitoringScheduleStatus = Lens.lens (monitoringScheduleStatus :: MonitoringScheduleSummary -> ScheduleStatus) (\s a -> s {monitoringScheduleStatus = a} :: MonitoringScheduleSummary)
{-# DEPRECATED mssMonitoringScheduleStatus "Use generic-lens or generic-optics with 'monitoringScheduleStatus' instead." #-}

instance Lude.FromJSON MonitoringScheduleSummary where
  parseJSON =
    Lude.withObject
      "MonitoringScheduleSummary"
      ( \x ->
          MonitoringScheduleSummary'
            Lude.<$> (x Lude..:? "EndpointName")
            Lude.<*> (x Lude..: "MonitoringScheduleName")
            Lude.<*> (x Lude..: "MonitoringScheduleArn")
            Lude.<*> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "LastModifiedTime")
            Lude.<*> (x Lude..: "MonitoringScheduleStatus")
      )
