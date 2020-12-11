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
    irStatus,
    irStartTime,
    irInstancesToUpdate,
    irPercentageComplete,
    irAutoScalingGroupName,
    irEndTime,
    irStatusReason,
    irInstanceRefreshId,
  )
where

import Network.AWS.AutoScaling.Types.InstanceRefreshStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance refresh for an Auto Scaling group.
--
-- /See:/ 'mkInstanceRefresh' smart constructor.
data InstanceRefresh = InstanceRefresh'
  { status ::
      Lude.Maybe InstanceRefreshStatus,
    startTime :: Lude.Maybe Lude.ISO8601,
    instancesToUpdate :: Lude.Maybe Lude.Natural,
    percentageComplete :: Lude.Maybe Lude.Natural,
    autoScalingGroupName :: Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.ISO8601,
    statusReason :: Lude.Maybe Lude.Text,
    instanceRefreshId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceRefresh' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'endTime' - The date and time at which the instance refresh ended.
-- * 'instanceRefreshId' - The instance refresh ID.
-- * 'instancesToUpdate' - The number of instances remaining to update before the instance refresh is complete.
-- * 'percentageComplete' - The percentage of the instance refresh that is complete. For each instance replacement, Amazon EC2 Auto Scaling tracks the instance's health status and warm-up time. When the instance's health status changes to healthy and the specified warm-up time passes, the instance is considered updated and added to the percentage complete.
-- * 'startTime' - The date and time at which the instance refresh began.
-- * 'status' - The current status for the instance refresh operation:
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
-- * 'statusReason' - Provides more details about the current status of the instance refresh.
mkInstanceRefresh ::
  InstanceRefresh
mkInstanceRefresh =
  InstanceRefresh'
    { status = Lude.Nothing,
      startTime = Lude.Nothing,
      instancesToUpdate = Lude.Nothing,
      percentageComplete = Lude.Nothing,
      autoScalingGroupName = Lude.Nothing,
      endTime = Lude.Nothing,
      statusReason = Lude.Nothing,
      instanceRefreshId = Lude.Nothing
    }

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
irStatus :: Lens.Lens' InstanceRefresh (Lude.Maybe InstanceRefreshStatus)
irStatus = Lens.lens (status :: InstanceRefresh -> Lude.Maybe InstanceRefreshStatus) (\s a -> s {status = a} :: InstanceRefresh)
{-# DEPRECATED irStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time at which the instance refresh began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irStartTime :: Lens.Lens' InstanceRefresh (Lude.Maybe Lude.ISO8601)
irStartTime = Lens.lens (startTime :: InstanceRefresh -> Lude.Maybe Lude.ISO8601) (\s a -> s {startTime = a} :: InstanceRefresh)
{-# DEPRECATED irStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The number of instances remaining to update before the instance refresh is complete.
--
-- /Note:/ Consider using 'instancesToUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irInstancesToUpdate :: Lens.Lens' InstanceRefresh (Lude.Maybe Lude.Natural)
irInstancesToUpdate = Lens.lens (instancesToUpdate :: InstanceRefresh -> Lude.Maybe Lude.Natural) (\s a -> s {instancesToUpdate = a} :: InstanceRefresh)
{-# DEPRECATED irInstancesToUpdate "Use generic-lens or generic-optics with 'instancesToUpdate' instead." #-}

-- | The percentage of the instance refresh that is complete. For each instance replacement, Amazon EC2 Auto Scaling tracks the instance's health status and warm-up time. When the instance's health status changes to healthy and the specified warm-up time passes, the instance is considered updated and added to the percentage complete.
--
-- /Note:/ Consider using 'percentageComplete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irPercentageComplete :: Lens.Lens' InstanceRefresh (Lude.Maybe Lude.Natural)
irPercentageComplete = Lens.lens (percentageComplete :: InstanceRefresh -> Lude.Maybe Lude.Natural) (\s a -> s {percentageComplete = a} :: InstanceRefresh)
{-# DEPRECATED irPercentageComplete "Use generic-lens or generic-optics with 'percentageComplete' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irAutoScalingGroupName :: Lens.Lens' InstanceRefresh (Lude.Maybe Lude.Text)
irAutoScalingGroupName = Lens.lens (autoScalingGroupName :: InstanceRefresh -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingGroupName = a} :: InstanceRefresh)
{-# DEPRECATED irAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The date and time at which the instance refresh ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irEndTime :: Lens.Lens' InstanceRefresh (Lude.Maybe Lude.ISO8601)
irEndTime = Lens.lens (endTime :: InstanceRefresh -> Lude.Maybe Lude.ISO8601) (\s a -> s {endTime = a} :: InstanceRefresh)
{-# DEPRECATED irEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Provides more details about the current status of the instance refresh.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irStatusReason :: Lens.Lens' InstanceRefresh (Lude.Maybe Lude.Text)
irStatusReason = Lens.lens (statusReason :: InstanceRefresh -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: InstanceRefresh)
{-# DEPRECATED irStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The instance refresh ID.
--
-- /Note:/ Consider using 'instanceRefreshId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irInstanceRefreshId :: Lens.Lens' InstanceRefresh (Lude.Maybe Lude.Text)
irInstanceRefreshId = Lens.lens (instanceRefreshId :: InstanceRefresh -> Lude.Maybe Lude.Text) (\s a -> s {instanceRefreshId = a} :: InstanceRefresh)
{-# DEPRECATED irInstanceRefreshId "Use generic-lens or generic-optics with 'instanceRefreshId' instead." #-}

instance Lude.FromXML InstanceRefresh where
  parseXML x =
    InstanceRefresh'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "StartTime")
      Lude.<*> (x Lude..@? "InstancesToUpdate")
      Lude.<*> (x Lude..@? "PercentageComplete")
      Lude.<*> (x Lude..@? "AutoScalingGroupName")
      Lude.<*> (x Lude..@? "EndTime")
      Lude.<*> (x Lude..@? "StatusReason")
      Lude.<*> (x Lude..@? "InstanceRefreshId")
