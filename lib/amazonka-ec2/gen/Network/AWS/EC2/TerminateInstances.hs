{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.TerminateInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shuts down the specified instances. This operation is idempotent; if you terminate an instance more than once, each call succeeds.
--
-- If you specify multiple instances and the request fails (for example, because of a single incorrect instance ID), none of the instances are terminated.
-- Terminated instances remain visible after termination (for approximately one hour).
-- By default, Amazon EC2 deletes all EBS volumes that were attached when the instance launched. Volumes attached after instance launch continue running.
-- You can stop, start, and terminate EBS-backed instances. You can only terminate instance store-backed instances. What happens to an instance differs if you stop it or terminate it. For example, when you stop an instance, the root device and any other devices attached to the instance persist. When you terminate an instance, any attached EBS volumes with the @DeleteOnTermination@ block device mapping parameter set to @true@ are automatically deleted. For more information about the differences between stopping and terminating instances, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html Instance lifecycle> in the /Amazon Elastic Compute Cloud User Guide/ .
-- For more information about troubleshooting, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesShuttingDown.html Troubleshooting terminating your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.TerminateInstances
  ( -- * Creating a request
    TerminateInstances (..),
    mkTerminateInstances,

    -- ** Request lenses
    tiInstanceIds,
    tiDryRun,

    -- * Destructuring the response
    TerminateInstancesResponse (..),
    mkTerminateInstancesResponse,

    -- ** Response lenses
    tirrsTerminatingInstances,
    tirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTerminateInstances' smart constructor.
data TerminateInstances = TerminateInstances'
  { -- | The IDs of the instances.
    --
    -- Constraints: Up to 1000 instance IDs. We recommend breaking up this request into smaller batches.
    instanceIds :: [Types.InstanceId],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateInstances' value with any optional fields omitted.
mkTerminateInstances ::
  TerminateInstances
mkTerminateInstances =
  TerminateInstances'
    { instanceIds = Core.mempty,
      dryRun = Core.Nothing
    }

-- | The IDs of the instances.
--
-- Constraints: Up to 1000 instance IDs. We recommend breaking up this request into smaller batches.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiInstanceIds :: Lens.Lens' TerminateInstances [Types.InstanceId]
tiInstanceIds = Lens.field @"instanceIds"
{-# DEPRECATED tiInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiDryRun :: Lens.Lens' TerminateInstances (Core.Maybe Core.Bool)
tiDryRun = Lens.field @"dryRun"
{-# DEPRECATED tiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest TerminateInstances where
  type Rs TerminateInstances = TerminateInstancesResponse
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
            ( Core.pure ("Action", "TerminateInstances")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "InstanceId" instanceIds)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          TerminateInstancesResponse'
            Core.<$> (x Core..@? "instancesSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTerminateInstancesResponse' smart constructor.
data TerminateInstancesResponse = TerminateInstancesResponse'
  { -- | Information about the terminated instances.
    terminatingInstances :: Core.Maybe [Types.InstanceStateChange],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateInstancesResponse' value with any optional fields omitted.
mkTerminateInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TerminateInstancesResponse
mkTerminateInstancesResponse responseStatus =
  TerminateInstancesResponse'
    { terminatingInstances = Core.Nothing,
      responseStatus
    }

-- | Information about the terminated instances.
--
-- /Note:/ Consider using 'terminatingInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tirrsTerminatingInstances :: Lens.Lens' TerminateInstancesResponse (Core.Maybe [Types.InstanceStateChange])
tirrsTerminatingInstances = Lens.field @"terminatingInstances"
{-# DEPRECATED tirrsTerminatingInstances "Use generic-lens or generic-optics with 'terminatingInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tirrsResponseStatus :: Lens.Lens' TerminateInstancesResponse Core.Int
tirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED tirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
