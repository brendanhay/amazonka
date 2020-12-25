{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RunScheduledInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches the specified Scheduled Instances.
--
-- Before you can launch a Scheduled Instance, you must purchase it and obtain an identifier using 'PurchaseScheduledInstances' .
-- You must launch a Scheduled Instance during its scheduled time period. You can't stop or reboot a Scheduled Instance, but you can terminate it as needed. If you terminate a Scheduled Instance before the current scheduled time period ends, you can launch it again after a few minutes. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-scheduled-instances.html Scheduled Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.RunScheduledInstances
  ( -- * Creating a request
    RunScheduledInstances (..),
    mkRunScheduledInstances,

    -- ** Request lenses
    rsiLaunchSpecification,
    rsiScheduledInstanceId,
    rsiClientToken,
    rsiDryRun,
    rsiInstanceCount,

    -- * Destructuring the response
    RunScheduledInstancesResponse (..),
    mkRunScheduledInstancesResponse,

    -- ** Response lenses
    rsirrsInstanceIdSet,
    rsirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for RunScheduledInstances.
--
-- /See:/ 'mkRunScheduledInstances' smart constructor.
data RunScheduledInstances = RunScheduledInstances'
  { -- | The launch specification. You must match the instance type, Availability Zone, network, and platform of the schedule that you purchased.
    launchSpecification :: Types.ScheduledInstancesLaunchSpecification,
    -- | The Scheduled Instance ID.
    scheduledInstanceId :: Types.ScheduledInstanceId,
    -- | Unique, case-sensitive identifier that ensures the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
    clientToken :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The number of instances.
    --
    -- Default: 1
    instanceCount :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RunScheduledInstances' value with any optional fields omitted.
mkRunScheduledInstances ::
  -- | 'launchSpecification'
  Types.ScheduledInstancesLaunchSpecification ->
  -- | 'scheduledInstanceId'
  Types.ScheduledInstanceId ->
  RunScheduledInstances
mkRunScheduledInstances launchSpecification scheduledInstanceId =
  RunScheduledInstances'
    { launchSpecification,
      scheduledInstanceId,
      clientToken = Core.Nothing,
      dryRun = Core.Nothing,
      instanceCount = Core.Nothing
    }

-- | The launch specification. You must match the instance type, Availability Zone, network, and platform of the schedule that you purchased.
--
-- /Note:/ Consider using 'launchSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsiLaunchSpecification :: Lens.Lens' RunScheduledInstances Types.ScheduledInstancesLaunchSpecification
rsiLaunchSpecification = Lens.field @"launchSpecification"
{-# DEPRECATED rsiLaunchSpecification "Use generic-lens or generic-optics with 'launchSpecification' instead." #-}

-- | The Scheduled Instance ID.
--
-- /Note:/ Consider using 'scheduledInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsiScheduledInstanceId :: Lens.Lens' RunScheduledInstances Types.ScheduledInstanceId
rsiScheduledInstanceId = Lens.field @"scheduledInstanceId"
{-# DEPRECATED rsiScheduledInstanceId "Use generic-lens or generic-optics with 'scheduledInstanceId' instead." #-}

-- | Unique, case-sensitive identifier that ensures the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsiClientToken :: Lens.Lens' RunScheduledInstances (Core.Maybe Types.String)
rsiClientToken = Lens.field @"clientToken"
{-# DEPRECATED rsiClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsiDryRun :: Lens.Lens' RunScheduledInstances (Core.Maybe Core.Bool)
rsiDryRun = Lens.field @"dryRun"
{-# DEPRECATED rsiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The number of instances.
--
-- Default: 1
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsiInstanceCount :: Lens.Lens' RunScheduledInstances (Core.Maybe Core.Int)
rsiInstanceCount = Lens.field @"instanceCount"
{-# DEPRECATED rsiInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

instance Core.AWSRequest RunScheduledInstances where
  type Rs RunScheduledInstances = RunScheduledInstancesResponse
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
            ( Core.pure ("Action", "RunScheduledInstances")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "LaunchSpecification" launchSpecification)
                Core.<> (Core.toQueryValue "ScheduledInstanceId" scheduledInstanceId)
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "InstanceCount" Core.<$> instanceCount)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          RunScheduledInstancesResponse'
            Core.<$> (x Core..@? "instanceIdSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of RunScheduledInstances.
--
-- /See:/ 'mkRunScheduledInstancesResponse' smart constructor.
data RunScheduledInstancesResponse = RunScheduledInstancesResponse'
  { -- | The IDs of the newly launched instances.
    instanceIdSet :: Core.Maybe [Types.InstanceId],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RunScheduledInstancesResponse' value with any optional fields omitted.
mkRunScheduledInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RunScheduledInstancesResponse
mkRunScheduledInstancesResponse responseStatus =
  RunScheduledInstancesResponse'
    { instanceIdSet = Core.Nothing,
      responseStatus
    }

-- | The IDs of the newly launched instances.
--
-- /Note:/ Consider using 'instanceIdSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsirrsInstanceIdSet :: Lens.Lens' RunScheduledInstancesResponse (Core.Maybe [Types.InstanceId])
rsirrsInstanceIdSet = Lens.field @"instanceIdSet"
{-# DEPRECATED rsirrsInstanceIdSet "Use generic-lens or generic-optics with 'instanceIdSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsirrsResponseStatus :: Lens.Lens' RunScheduledInstancesResponse Core.Int
rsirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rsirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
