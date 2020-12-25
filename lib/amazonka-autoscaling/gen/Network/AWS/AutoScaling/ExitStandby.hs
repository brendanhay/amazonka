{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.ExitStandby
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves the specified instances out of the standby state.
--
-- After you put the instances back in service, the desired capacity is incremented.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enter-exit-standby.html Temporarily removing instances from your Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.ExitStandby
  ( -- * Creating a request
    ExitStandby (..),
    mkExitStandby,

    -- ** Request lenses
    eAutoScalingGroupName,
    eInstanceIds,

    -- * Destructuring the response
    ExitStandbyResponse (..),
    mkExitStandbyResponse,

    -- ** Response lenses
    ersActivities,
    ersResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkExitStandby' smart constructor.
data ExitStandby = ExitStandby'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | The IDs of the instances. You can specify up to 20 instances.
    instanceIds :: Core.Maybe [Types.XmlStringMaxLen19]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExitStandby' value with any optional fields omitted.
mkExitStandby ::
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  ExitStandby
mkExitStandby autoScalingGroupName =
  ExitStandby' {autoScalingGroupName, instanceIds = Core.Nothing}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAutoScalingGroupName :: Lens.Lens' ExitStandby Types.ResourceName
eAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED eAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The IDs of the instances. You can specify up to 20 instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eInstanceIds :: Lens.Lens' ExitStandby (Core.Maybe [Types.XmlStringMaxLen19])
eInstanceIds = Lens.field @"instanceIds"
{-# DEPRECATED eInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

instance Core.AWSRequest ExitStandby where
  type Rs ExitStandby = ExitStandbyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ExitStandby")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> ( Core.toQueryValue
                            "InstanceIds"
                            (Core.toQueryList "member" Core.<$> instanceIds)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ExitStandbyResult"
      ( \s h x ->
          ExitStandbyResponse'
            Core.<$> (x Core..@? "Activities" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkExitStandbyResponse' smart constructor.
data ExitStandbyResponse = ExitStandbyResponse'
  { -- | The activities related to moving instances out of @Standby@ mode.
    activities :: Core.Maybe [Types.Activity],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ExitStandbyResponse' value with any optional fields omitted.
mkExitStandbyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ExitStandbyResponse
mkExitStandbyResponse responseStatus =
  ExitStandbyResponse' {activities = Core.Nothing, responseStatus}

-- | The activities related to moving instances out of @Standby@ mode.
--
-- /Note:/ Consider using 'activities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ersActivities :: Lens.Lens' ExitStandbyResponse (Core.Maybe [Types.Activity])
ersActivities = Lens.field @"activities"
{-# DEPRECATED ersActivities "Use generic-lens or generic-optics with 'activities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ersResponseStatus :: Lens.Lens' ExitStandbyResponse Core.Int
ersResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
