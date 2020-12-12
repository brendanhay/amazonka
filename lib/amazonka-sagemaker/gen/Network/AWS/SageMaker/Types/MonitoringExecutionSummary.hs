{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringExecutionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringExecutionSummary
  ( MonitoringExecutionSummary (..),

    -- * Smart constructor
    mkMonitoringExecutionSummary,

    -- * Lenses
    mesFailureReason,
    mesEndpointName,
    mesProcessingJobARN,
    mesMonitoringScheduleName,
    mesScheduledTime,
    mesCreationTime,
    mesLastModifiedTime,
    mesMonitoringExecutionStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ExecutionStatus

-- | Summary of information about the last monitoring job to run.
--
-- /See:/ 'mkMonitoringExecutionSummary' smart constructor.
data MonitoringExecutionSummary = MonitoringExecutionSummary'
  { failureReason ::
      Lude.Maybe Lude.Text,
    endpointName :: Lude.Maybe Lude.Text,
    processingJobARN ::
      Lude.Maybe Lude.Text,
    monitoringScheduleName :: Lude.Text,
    scheduledTime :: Lude.Timestamp,
    creationTime :: Lude.Timestamp,
    lastModifiedTime :: Lude.Timestamp,
    monitoringExecutionStatus ::
      ExecutionStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoringExecutionSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time at which the monitoring job was created.
-- * 'endpointName' - The name of teh endpoint used to run the monitoring job.
-- * 'failureReason' - Contains the reason a monitoring job failed, if it failed.
-- * 'lastModifiedTime' - A timestamp that indicates the last time the monitoring job was modified.
-- * 'monitoringExecutionStatus' - The status of the monitoring job.
-- * 'monitoringScheduleName' - The name of the monitoring schedule.
-- * 'processingJobARN' - The Amazon Resource Name (ARN) of the monitoring job.
-- * 'scheduledTime' - The time the monitoring job was scheduled.
mkMonitoringExecutionSummary ::
  -- | 'monitoringScheduleName'
  Lude.Text ->
  -- | 'scheduledTime'
  Lude.Timestamp ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  -- | 'monitoringExecutionStatus'
  ExecutionStatus ->
  MonitoringExecutionSummary
mkMonitoringExecutionSummary
  pMonitoringScheduleName_
  pScheduledTime_
  pCreationTime_
  pLastModifiedTime_
  pMonitoringExecutionStatus_ =
    MonitoringExecutionSummary'
      { failureReason = Lude.Nothing,
        endpointName = Lude.Nothing,
        processingJobARN = Lude.Nothing,
        monitoringScheduleName = pMonitoringScheduleName_,
        scheduledTime = pScheduledTime_,
        creationTime = pCreationTime_,
        lastModifiedTime = pLastModifiedTime_,
        monitoringExecutionStatus = pMonitoringExecutionStatus_
      }

-- | Contains the reason a monitoring job failed, if it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesFailureReason :: Lens.Lens' MonitoringExecutionSummary (Lude.Maybe Lude.Text)
mesFailureReason = Lens.lens (failureReason :: MonitoringExecutionSummary -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: MonitoringExecutionSummary)
{-# DEPRECATED mesFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The name of teh endpoint used to run the monitoring job.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesEndpointName :: Lens.Lens' MonitoringExecutionSummary (Lude.Maybe Lude.Text)
mesEndpointName = Lens.lens (endpointName :: MonitoringExecutionSummary -> Lude.Maybe Lude.Text) (\s a -> s {endpointName = a} :: MonitoringExecutionSummary)
{-# DEPRECATED mesEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | The Amazon Resource Name (ARN) of the monitoring job.
--
-- /Note:/ Consider using 'processingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesProcessingJobARN :: Lens.Lens' MonitoringExecutionSummary (Lude.Maybe Lude.Text)
mesProcessingJobARN = Lens.lens (processingJobARN :: MonitoringExecutionSummary -> Lude.Maybe Lude.Text) (\s a -> s {processingJobARN = a} :: MonitoringExecutionSummary)
{-# DEPRECATED mesProcessingJobARN "Use generic-lens or generic-optics with 'processingJobARN' instead." #-}

-- | The name of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesMonitoringScheduleName :: Lens.Lens' MonitoringExecutionSummary Lude.Text
mesMonitoringScheduleName = Lens.lens (monitoringScheduleName :: MonitoringExecutionSummary -> Lude.Text) (\s a -> s {monitoringScheduleName = a} :: MonitoringExecutionSummary)
{-# DEPRECATED mesMonitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead." #-}

-- | The time the monitoring job was scheduled.
--
-- /Note:/ Consider using 'scheduledTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesScheduledTime :: Lens.Lens' MonitoringExecutionSummary Lude.Timestamp
mesScheduledTime = Lens.lens (scheduledTime :: MonitoringExecutionSummary -> Lude.Timestamp) (\s a -> s {scheduledTime = a} :: MonitoringExecutionSummary)
{-# DEPRECATED mesScheduledTime "Use generic-lens or generic-optics with 'scheduledTime' instead." #-}

-- | The time at which the monitoring job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesCreationTime :: Lens.Lens' MonitoringExecutionSummary Lude.Timestamp
mesCreationTime = Lens.lens (creationTime :: MonitoringExecutionSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: MonitoringExecutionSummary)
{-# DEPRECATED mesCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A timestamp that indicates the last time the monitoring job was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesLastModifiedTime :: Lens.Lens' MonitoringExecutionSummary Lude.Timestamp
mesLastModifiedTime = Lens.lens (lastModifiedTime :: MonitoringExecutionSummary -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: MonitoringExecutionSummary)
{-# DEPRECATED mesLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The status of the monitoring job.
--
-- /Note:/ Consider using 'monitoringExecutionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesMonitoringExecutionStatus :: Lens.Lens' MonitoringExecutionSummary ExecutionStatus
mesMonitoringExecutionStatus = Lens.lens (monitoringExecutionStatus :: MonitoringExecutionSummary -> ExecutionStatus) (\s a -> s {monitoringExecutionStatus = a} :: MonitoringExecutionSummary)
{-# DEPRECATED mesMonitoringExecutionStatus "Use generic-lens or generic-optics with 'monitoringExecutionStatus' instead." #-}

instance Lude.FromJSON MonitoringExecutionSummary where
  parseJSON =
    Lude.withObject
      "MonitoringExecutionSummary"
      ( \x ->
          MonitoringExecutionSummary'
            Lude.<$> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "EndpointName")
            Lude.<*> (x Lude..:? "ProcessingJobArn")
            Lude.<*> (x Lude..: "MonitoringScheduleName")
            Lude.<*> (x Lude..: "ScheduledTime")
            Lude.<*> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "LastModifiedTime")
            Lude.<*> (x Lude..: "MonitoringExecutionStatus")
      )
