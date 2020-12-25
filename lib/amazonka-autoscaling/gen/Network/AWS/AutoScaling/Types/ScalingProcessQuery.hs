{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScalingProcessQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ScalingProcessQuery
  ( ScalingProcessQuery (..),

    -- * Smart constructor
    mkScalingProcessQuery,

    -- * Lenses
    spqAutoScalingGroupName,
    spqScalingProcesses,
  )
where

import qualified Network.AWS.AutoScaling.Types.ResourceName as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkScalingProcessQuery' smart constructor.
data ScalingProcessQuery = ScalingProcessQuery'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | One or more of the following processes:
    --
    --
    --     * @Launch@
    --
    --
    --     * @Terminate@
    --
    --
    --     * @AddToLoadBalancer@
    --
    --
    --     * @AlarmNotification@
    --
    --
    --     * @AZRebalance@
    --
    --
    --     * @HealthCheck@
    --
    --
    --     * @InstanceRefresh@
    --
    --
    --     * @ReplaceUnhealthy@
    --
    --
    --     * @ScheduledActions@
    --
    --
    -- If you omit this parameter, all processes are specified.
    scalingProcesses :: Core.Maybe [Types.XmlStringMaxLen255]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScalingProcessQuery' value with any optional fields omitted.
mkScalingProcessQuery ::
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  ScalingProcessQuery
mkScalingProcessQuery autoScalingGroupName =
  ScalingProcessQuery'
    { autoScalingGroupName,
      scalingProcesses = Core.Nothing
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spqAutoScalingGroupName :: Lens.Lens' ScalingProcessQuery Types.ResourceName
spqAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED spqAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | One or more of the following processes:
--
--
--     * @Launch@
--
--
--     * @Terminate@
--
--
--     * @AddToLoadBalancer@
--
--
--     * @AlarmNotification@
--
--
--     * @AZRebalance@
--
--
--     * @HealthCheck@
--
--
--     * @InstanceRefresh@
--
--
--     * @ReplaceUnhealthy@
--
--
--     * @ScheduledActions@
--
--
-- If you omit this parameter, all processes are specified.
--
-- /Note:/ Consider using 'scalingProcesses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spqScalingProcesses :: Lens.Lens' ScalingProcessQuery (Core.Maybe [Types.XmlStringMaxLen255])
spqScalingProcesses = Lens.field @"scalingProcesses"
{-# DEPRECATED spqScalingProcesses "Use generic-lens or generic-optics with 'scalingProcesses' instead." #-}
