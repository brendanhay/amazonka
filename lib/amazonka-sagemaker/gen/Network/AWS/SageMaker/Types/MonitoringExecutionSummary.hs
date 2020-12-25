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
    mesMonitoringScheduleName,
    mesScheduledTime,
    mesCreationTime,
    mesLastModifiedTime,
    mesMonitoringExecutionStatus,
    mesEndpointName,
    mesFailureReason,
    mesProcessingJobArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.EndpointName as Types
import qualified Network.AWS.SageMaker.Types.ExecutionStatus as Types
import qualified Network.AWS.SageMaker.Types.FailureReason as Types
import qualified Network.AWS.SageMaker.Types.MonitoringScheduleName as Types
import qualified Network.AWS.SageMaker.Types.ProcessingJobArn as Types

-- | Summary of information about the last monitoring job to run.
--
-- /See:/ 'mkMonitoringExecutionSummary' smart constructor.
data MonitoringExecutionSummary = MonitoringExecutionSummary'
  { -- | The name of the monitoring schedule.
    monitoringScheduleName :: Types.MonitoringScheduleName,
    -- | The time the monitoring job was scheduled.
    scheduledTime :: Core.NominalDiffTime,
    -- | The time at which the monitoring job was created.
    creationTime :: Core.NominalDiffTime,
    -- | A timestamp that indicates the last time the monitoring job was modified.
    lastModifiedTime :: Core.NominalDiffTime,
    -- | The status of the monitoring job.
    monitoringExecutionStatus :: Types.ExecutionStatus,
    -- | The name of teh endpoint used to run the monitoring job.
    endpointName :: Core.Maybe Types.EndpointName,
    -- | Contains the reason a monitoring job failed, if it failed.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | The Amazon Resource Name (ARN) of the monitoring job.
    processingJobArn :: Core.Maybe Types.ProcessingJobArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'MonitoringExecutionSummary' value with any optional fields omitted.
mkMonitoringExecutionSummary ::
  -- | 'monitoringScheduleName'
  Types.MonitoringScheduleName ->
  -- | 'scheduledTime'
  Core.NominalDiffTime ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'lastModifiedTime'
  Core.NominalDiffTime ->
  -- | 'monitoringExecutionStatus'
  Types.ExecutionStatus ->
  MonitoringExecutionSummary
mkMonitoringExecutionSummary
  monitoringScheduleName
  scheduledTime
  creationTime
  lastModifiedTime
  monitoringExecutionStatus =
    MonitoringExecutionSummary'
      { monitoringScheduleName,
        scheduledTime,
        creationTime,
        lastModifiedTime,
        monitoringExecutionStatus,
        endpointName = Core.Nothing,
        failureReason = Core.Nothing,
        processingJobArn = Core.Nothing
      }

-- | The name of the monitoring schedule.
--
-- /Note:/ Consider using 'monitoringScheduleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesMonitoringScheduleName :: Lens.Lens' MonitoringExecutionSummary Types.MonitoringScheduleName
mesMonitoringScheduleName = Lens.field @"monitoringScheduleName"
{-# DEPRECATED mesMonitoringScheduleName "Use generic-lens or generic-optics with 'monitoringScheduleName' instead." #-}

-- | The time the monitoring job was scheduled.
--
-- /Note:/ Consider using 'scheduledTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesScheduledTime :: Lens.Lens' MonitoringExecutionSummary Core.NominalDiffTime
mesScheduledTime = Lens.field @"scheduledTime"
{-# DEPRECATED mesScheduledTime "Use generic-lens or generic-optics with 'scheduledTime' instead." #-}

-- | The time at which the monitoring job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesCreationTime :: Lens.Lens' MonitoringExecutionSummary Core.NominalDiffTime
mesCreationTime = Lens.field @"creationTime"
{-# DEPRECATED mesCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A timestamp that indicates the last time the monitoring job was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesLastModifiedTime :: Lens.Lens' MonitoringExecutionSummary Core.NominalDiffTime
mesLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED mesLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The status of the monitoring job.
--
-- /Note:/ Consider using 'monitoringExecutionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesMonitoringExecutionStatus :: Lens.Lens' MonitoringExecutionSummary Types.ExecutionStatus
mesMonitoringExecutionStatus = Lens.field @"monitoringExecutionStatus"
{-# DEPRECATED mesMonitoringExecutionStatus "Use generic-lens or generic-optics with 'monitoringExecutionStatus' instead." #-}

-- | The name of teh endpoint used to run the monitoring job.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesEndpointName :: Lens.Lens' MonitoringExecutionSummary (Core.Maybe Types.EndpointName)
mesEndpointName = Lens.field @"endpointName"
{-# DEPRECATED mesEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

-- | Contains the reason a monitoring job failed, if it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesFailureReason :: Lens.Lens' MonitoringExecutionSummary (Core.Maybe Types.FailureReason)
mesFailureReason = Lens.field @"failureReason"
{-# DEPRECATED mesFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The Amazon Resource Name (ARN) of the monitoring job.
--
-- /Note:/ Consider using 'processingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mesProcessingJobArn :: Lens.Lens' MonitoringExecutionSummary (Core.Maybe Types.ProcessingJobArn)
mesProcessingJobArn = Lens.field @"processingJobArn"
{-# DEPRECATED mesProcessingJobArn "Use generic-lens or generic-optics with 'processingJobArn' instead." #-}

instance Core.FromJSON MonitoringExecutionSummary where
  parseJSON =
    Core.withObject "MonitoringExecutionSummary" Core.$
      \x ->
        MonitoringExecutionSummary'
          Core.<$> (x Core..: "MonitoringScheduleName")
          Core.<*> (x Core..: "ScheduledTime")
          Core.<*> (x Core..: "CreationTime")
          Core.<*> (x Core..: "LastModifiedTime")
          Core.<*> (x Core..: "MonitoringExecutionStatus")
          Core.<*> (x Core..:? "EndpointName")
          Core.<*> (x Core..:? "FailureReason")
          Core.<*> (x Core..:? "ProcessingJobArn")
