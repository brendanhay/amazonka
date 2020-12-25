{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.StopInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an Amazon EBS-backed instance.
--
-- You can use the Stop action to hibernate an instance if the instance is <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#enabling-hibernation enabled for hibernation> and it meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
-- We don't charge usage for a stopped instance, or data transfer fees; however, your root partition Amazon EBS volume remains and continues to persist your data, and you are charged for Amazon EBS volume usage. Every time you start your Windows instance, Amazon EC2 charges you for a full instance hour. If you stop and restart your Windows instance, a new instance hour begins and Amazon EC2 charges you for another full instance hour even if you are still within the same 60-minute period when it was stopped. Every time you start your Linux instance, Amazon EC2 charges a one-minute minimum for instance usage, and thereafter charges per second for instance usage.
-- You can't stop or hibernate instance store-backed instances. You can't use the Stop action to hibernate Spot Instances, but you can specify that Amazon EC2 should hibernate Spot Instances when they are interrupted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-interruptions.html#hibernate-spot-instances Hibernating interrupted Spot Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
-- When you stop or hibernate an instance, we shut it down. You can restart your instance at any time. Before stopping or hibernating an instance, make sure it is in a state from which it can be restarted. Stopping an instance does not preserve data stored in RAM, but hibernating an instance does preserve data stored in RAM. If an instance cannot hibernate successfully, a normal shutdown occurs.
-- Stopping and hibernating an instance is different to rebooting or terminating it. For example, when you stop or hibernate an instance, the root device and any other devices attached to the instance persist. When you terminate an instance, the root device and any other devices attached during the instance launch are automatically deleted. For more information about the differences between rebooting, stopping, hibernating, and terminating instances, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html Instance lifecycle> in the /Amazon Elastic Compute Cloud User Guide/ .
-- When you stop an instance, we attempt to shut it down forcibly after a short while. If your instance appears stuck in the stopping state after a period of time, there may be an issue with the underlying host computer. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesStopping.html Troubleshooting stopping your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.StopInstances
  ( -- * Creating a request
    StopInstances (..),
    mkStopInstances,

    -- ** Request lenses
    siInstanceIds,
    siDryRun,
    siForce,
    siHibernate,

    -- * Destructuring the response
    StopInstancesResponse (..),
    mkStopInstancesResponse,

    -- ** Response lenses
    sirrsStoppingInstances,
    sirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopInstances' smart constructor.
data StopInstances = StopInstances'
  { -- | The IDs of the instances.
    instanceIds :: [Types.InstanceId],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Forces the instances to stop. The instances do not have an opportunity to flush file system caches or file system metadata. If you use this option, you must perform file system check and repair procedures. This option is not recommended for Windows instances.
    --
    -- Default: @false@
    force :: Core.Maybe Core.Bool,
    -- | Hibernates the instance if the instance was enabled for hibernation at launch. If the instance cannot hibernate successfully, a normal shutdown occurs. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
    --
    -- Default: @false@
    hibernate :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopInstances' value with any optional fields omitted.
mkStopInstances ::
  StopInstances
mkStopInstances =
  StopInstances'
    { instanceIds = Core.mempty,
      dryRun = Core.Nothing,
      force = Core.Nothing,
      hibernate = Core.Nothing
    }

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siInstanceIds :: Lens.Lens' StopInstances [Types.InstanceId]
siInstanceIds = Lens.field @"instanceIds"
{-# DEPRECATED siInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDryRun :: Lens.Lens' StopInstances (Core.Maybe Core.Bool)
siDryRun = Lens.field @"dryRun"
{-# DEPRECATED siDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Forces the instances to stop. The instances do not have an opportunity to flush file system caches or file system metadata. If you use this option, you must perform file system check and repair procedures. This option is not recommended for Windows instances.
--
-- Default: @false@
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siForce :: Lens.Lens' StopInstances (Core.Maybe Core.Bool)
siForce = Lens.field @"force"
{-# DEPRECATED siForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | Hibernates the instance if the instance was enabled for hibernation at launch. If the instance cannot hibernate successfully, a normal shutdown occurs. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Default: @false@
--
-- /Note:/ Consider using 'hibernate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siHibernate :: Lens.Lens' StopInstances (Core.Maybe Core.Bool)
siHibernate = Lens.field @"hibernate"
{-# DEPRECATED siHibernate "Use generic-lens or generic-optics with 'hibernate' instead." #-}

instance Core.AWSRequest StopInstances where
  type Rs StopInstances = StopInstancesResponse
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
            ( Core.pure ("Action", "StopInstances")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "InstanceId" instanceIds)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "Force" Core.<$> force)
                Core.<> (Core.toQueryValue "Hibernate" Core.<$> hibernate)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          StopInstancesResponse'
            Core.<$> (x Core..@? "instancesSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopInstancesResponse' smart constructor.
data StopInstancesResponse = StopInstancesResponse'
  { -- | Information about the stopped instances.
    stoppingInstances :: Core.Maybe [Types.InstanceStateChange],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopInstancesResponse' value with any optional fields omitted.
mkStopInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopInstancesResponse
mkStopInstancesResponse responseStatus =
  StopInstancesResponse'
    { stoppingInstances = Core.Nothing,
      responseStatus
    }

-- | Information about the stopped instances.
--
-- /Note:/ Consider using 'stoppingInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsStoppingInstances :: Lens.Lens' StopInstancesResponse (Core.Maybe [Types.InstanceStateChange])
sirrsStoppingInstances = Lens.field @"stoppingInstances"
{-# DEPRECATED sirrsStoppingInstances "Use generic-lens or generic-optics with 'stoppingInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sirrsResponseStatus :: Lens.Lens' StopInstancesResponse Core.Int
sirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
