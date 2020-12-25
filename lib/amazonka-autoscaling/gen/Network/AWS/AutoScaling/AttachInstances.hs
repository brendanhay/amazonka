{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.AttachInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more EC2 instances to the specified Auto Scaling group.
--
-- When you attach instances, Amazon EC2 Auto Scaling increases the desired capacity of the group by the number of instances being attached. If the number of instances being attached plus the desired capacity of the group exceeds the maximum size of the group, the operation fails.
-- If there is a Classic Load Balancer attached to your Auto Scaling group, the instances are also registered with the load balancer. If there are target groups attached to your Auto Scaling group, the instances are also registered with the target groups.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/attach-instance-asg.html Attach EC2 instances to your Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.AttachInstances
  ( -- * Creating a request
    AttachInstances (..),
    mkAttachInstances,

    -- ** Request lenses
    aiAutoScalingGroupName,
    aiInstanceIds,

    -- * Destructuring the response
    AttachInstancesResponse (..),
    mkAttachInstancesResponse,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachInstances' smart constructor.
data AttachInstances = AttachInstances'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | The IDs of the instances. You can specify up to 20 instances.
    instanceIds :: Core.Maybe [Types.XmlStringMaxLen19]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachInstances' value with any optional fields omitted.
mkAttachInstances ::
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  AttachInstances
mkAttachInstances autoScalingGroupName =
  AttachInstances'
    { autoScalingGroupName,
      instanceIds = Core.Nothing
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiAutoScalingGroupName :: Lens.Lens' AttachInstances Types.ResourceName
aiAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED aiAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The IDs of the instances. You can specify up to 20 instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiInstanceIds :: Lens.Lens' AttachInstances (Core.Maybe [Types.XmlStringMaxLen19])
aiInstanceIds = Lens.field @"instanceIds"
{-# DEPRECATED aiInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

instance Core.AWSRequest AttachInstances where
  type Rs AttachInstances = AttachInstancesResponse
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
            ( Core.pure ("Action", "AttachInstances")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> ( Core.toQueryValue
                            "InstanceIds"
                            (Core.toQueryList "member" Core.<$> instanceIds)
                        )
            )
      }
  response = Response.receiveNull AttachInstancesResponse'

-- | /See:/ 'mkAttachInstancesResponse' smart constructor.
data AttachInstancesResponse = AttachInstancesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachInstancesResponse' value with any optional fields omitted.
mkAttachInstancesResponse ::
  AttachInstancesResponse
mkAttachInstancesResponse = AttachInstancesResponse'
