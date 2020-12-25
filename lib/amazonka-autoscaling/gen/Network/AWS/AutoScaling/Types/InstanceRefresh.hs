{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.InstanceRefresh
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceRefresh
  ( InstanceRefresh (..),

    -- * Smart constructor
    mkInstanceRefresh,

    -- * Lenses
    irAutoScalingGroupName,
    irEndTime,
    irInstanceRefreshId,
    irInstancesToUpdate,
    irPercentageComplete,
    irStartTime,
    irStatus,
    irStatusReason,
  )
where

import qualified Network.AWS.AutoScaling.Types.AutoScalingGroupName as Types
import qualified Network.AWS.AutoScaling.Types.InstanceRefreshId as Types
import qualified Network.AWS.AutoScaling.Types.InstanceRefreshStatus as Types
import qualified Network.AWS.AutoScaling.Types.StatusReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an instance refresh for an Auto Scaling group.
--
-- /See:/ 'mkInstanceRefresh' smart constructor.
data InstanceRefresh = InstanceRefresh'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Maybe Types.AutoScalingGroupName,
    -- | The date and time at which the instance refresh ended.
    endTime :: Core.Maybe Core.UTCTime,
    -- | The instance refresh ID.
    instanceRefreshId :: Core.Maybe Types.InstanceRefreshId,
    -- | The number of instances remaining to update before the instance refresh is complete.
    instancesToUpdate :: Core.Maybe Core.Natural,
    -- | The percentage of the instance refresh that is complete. For each instance replacement, Amazon EC2 Auto Scaling tracks the instance's health status and warm-up time. When the instance's health status changes to healthy and the specified warm-up time passes, the instance is considered updated and added to the percentage complete.
    percentageComplete :: Core.Maybe Core.Natural,
    -- | The date and time at which the instance refresh began.
    startTime :: Core.Maybe Core.UTCTime,
    -- | The current status for the instance refresh operation:
    --
    --
    --     * @Pending@ - The request was created, but the operation has not started.
    --
    --
    --     * @InProgress@ - The operation is in progress.
    --
    --
    --     * @Successful@ - The operation completed successfully.
    --
    --
    --     * @Failed@ - The operation failed to complete. You can troubleshoot using the status reason and the scaling activities.
    --
    --
    --     * @Cancelling@ - An ongoing operation is being cancelled. Cancellation does not roll back any replacements that have already been completed, but it prevents new replacements from being started.
    --
    --
    --     * @Cancelled@ - The operation is cancelled.
    status :: Core.Maybe Types.InstanceRefreshStatus,
    -- | Provides more details about the current status of the instance refresh.
    statusReason :: Core.Maybe Types.StatusReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'InstanceRefresh' value with any optional fields omitted.
mkInstanceRefresh ::
  InstanceRefresh
mkInstanceRefresh =
  InstanceRefresh'
    { autoScalingGroupName = Core.Nothing,
      endTime = Core.Nothing,
      instanceRefreshId = Core.Nothing,
      instancesToUpdate = Core.Nothing,
      percentageComplete = Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing,
      statusReason = Core.Nothing
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irAutoScalingGroupName :: Lens.Lens' InstanceRefresh (Core.Maybe Types.AutoScalingGroupName)
irAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED irAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The date and time at which the instance refresh ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irEndTime :: Lens.Lens' InstanceRefresh (Core.Maybe Core.UTCTime)
irEndTime = Lens.field @"endTime"
{-# DEPRECATED irEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The instance refresh ID.
--
-- /Note:/ Consider using 'instanceRefreshId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irInstanceRefreshId :: Lens.Lens' InstanceRefresh (Core.Maybe Types.InstanceRefreshId)
irInstanceRefreshId = Lens.field @"instanceRefreshId"
{-# DEPRECATED irInstanceRefreshId "Use generic-lens or generic-optics with 'instanceRefreshId' instead." #-}

-- | The number of instances remaining to update before the instance refresh is complete.
--
-- /Note:/ Consider using 'instancesToUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irInstancesToUpdate :: Lens.Lens' InstanceRefresh (Core.Maybe Core.Natural)
irInstancesToUpdate = Lens.field @"instancesToUpdate"
{-# DEPRECATED irInstancesToUpdate "Use generic-lens or generic-optics with 'instancesToUpdate' instead." #-}

-- | The percentage of the instance refresh that is complete. For each instance replacement, Amazon EC2 Auto Scaling tracks the instance's health status and warm-up time. When the instance's health status changes to healthy and the specified warm-up time passes, the instance is considered updated and added to the percentage complete.
--
-- /Note:/ Consider using 'percentageComplete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irPercentageComplete :: Lens.Lens' InstanceRefresh (Core.Maybe Core.Natural)
irPercentageComplete = Lens.field @"percentageComplete"
{-# DEPRECATED irPercentageComplete "Use generic-lens or generic-optics with 'percentageComplete' instead." #-}

-- | The date and time at which the instance refresh began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irStartTime :: Lens.Lens' InstanceRefresh (Core.Maybe Core.UTCTime)
irStartTime = Lens.field @"startTime"
{-# DEPRECATED irStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The current status for the instance refresh operation:
--
--
--     * @Pending@ - The request was created, but the operation has not started.
--
--
--     * @InProgress@ - The operation is in progress.
--
--
--     * @Successful@ - The operation completed successfully.
--
--
--     * @Failed@ - The operation failed to complete. You can troubleshoot using the status reason and the scaling activities.
--
--
--     * @Cancelling@ - An ongoing operation is being cancelled. Cancellation does not roll back any replacements that have already been completed, but it prevents new replacements from being started.
--
--
--     * @Cancelled@ - The operation is cancelled.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irStatus :: Lens.Lens' InstanceRefresh (Core.Maybe Types.InstanceRefreshStatus)
irStatus = Lens.field @"status"
{-# DEPRECATED irStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Provides more details about the current status of the instance refresh.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irStatusReason :: Lens.Lens' InstanceRefresh (Core.Maybe Types.StatusReason)
irStatusReason = Lens.field @"statusReason"
{-# DEPRECATED irStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

instance Core.FromXML InstanceRefresh where
  parseXML x =
    InstanceRefresh'
      Core.<$> (x Core..@? "AutoScalingGroupName")
      Core.<*> (x Core..@? "EndTime")
      Core.<*> (x Core..@? "InstanceRefreshId")
      Core.<*> (x Core..@? "InstancesToUpdate")
      Core.<*> (x Core..@? "PercentageComplete")
      Core.<*> (x Core..@? "StartTime")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "StatusReason")
