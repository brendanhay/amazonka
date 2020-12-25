{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.PurchaseScheduledInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchases the Scheduled Instances with the specified schedule.
--
-- Scheduled Instances enable you to purchase Amazon EC2 compute capacity by the hour for a one-year term. Before you can purchase a Scheduled Instance, you must call 'DescribeScheduledInstanceAvailability' to check for available schedules and obtain a purchase token. After you purchase a Scheduled Instance, you must call 'RunScheduledInstances' during each scheduled time period.
-- After you purchase a Scheduled Instance, you can't cancel, modify, or resell your purchase.
module Network.AWS.EC2.PurchaseScheduledInstances
  ( -- * Creating a request
    PurchaseScheduledInstances (..),
    mkPurchaseScheduledInstances,

    -- ** Request lenses
    psiPurchaseRequests,
    psiClientToken,
    psiDryRun,

    -- * Destructuring the response
    PurchaseScheduledInstancesResponse (..),
    mkPurchaseScheduledInstancesResponse,

    -- ** Response lenses
    psirrsScheduledInstanceSet,
    psirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for PurchaseScheduledInstances.
--
-- /See:/ 'mkPurchaseScheduledInstances' smart constructor.
data PurchaseScheduledInstances = PurchaseScheduledInstances'
  { -- | The purchase requests.
    purchaseRequests :: Core.NonEmpty Types.PurchaseRequest,
    -- | Unique, case-sensitive identifier that ensures the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
    clientToken :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PurchaseScheduledInstances' value with any optional fields omitted.
mkPurchaseScheduledInstances ::
  -- | 'purchaseRequests'
  Core.NonEmpty Types.PurchaseRequest ->
  PurchaseScheduledInstances
mkPurchaseScheduledInstances purchaseRequests =
  PurchaseScheduledInstances'
    { purchaseRequests,
      clientToken = Core.Nothing,
      dryRun = Core.Nothing
    }

-- | The purchase requests.
--
-- /Note:/ Consider using 'purchaseRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psiPurchaseRequests :: Lens.Lens' PurchaseScheduledInstances (Core.NonEmpty Types.PurchaseRequest)
psiPurchaseRequests = Lens.field @"purchaseRequests"
{-# DEPRECATED psiPurchaseRequests "Use generic-lens or generic-optics with 'purchaseRequests' instead." #-}

-- | Unique, case-sensitive identifier that ensures the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psiClientToken :: Lens.Lens' PurchaseScheduledInstances (Core.Maybe Types.String)
psiClientToken = Lens.field @"clientToken"
{-# DEPRECATED psiClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psiDryRun :: Lens.Lens' PurchaseScheduledInstances (Core.Maybe Core.Bool)
psiDryRun = Lens.field @"dryRun"
{-# DEPRECATED psiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest PurchaseScheduledInstances where
  type
    Rs PurchaseScheduledInstances =
      PurchaseScheduledInstancesResponse
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
            ( Core.pure ("Action", "PurchaseScheduledInstances")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "PurchaseRequest" purchaseRequests)
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          PurchaseScheduledInstancesResponse'
            Core.<$> ( x Core..@? "scheduledInstanceSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of PurchaseScheduledInstances.
--
-- /See:/ 'mkPurchaseScheduledInstancesResponse' smart constructor.
data PurchaseScheduledInstancesResponse = PurchaseScheduledInstancesResponse'
  { -- | Information about the Scheduled Instances.
    scheduledInstanceSet :: Core.Maybe [Types.ScheduledInstance],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PurchaseScheduledInstancesResponse' value with any optional fields omitted.
mkPurchaseScheduledInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PurchaseScheduledInstancesResponse
mkPurchaseScheduledInstancesResponse responseStatus =
  PurchaseScheduledInstancesResponse'
    { scheduledInstanceSet =
        Core.Nothing,
      responseStatus
    }

-- | Information about the Scheduled Instances.
--
-- /Note:/ Consider using 'scheduledInstanceSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psirrsScheduledInstanceSet :: Lens.Lens' PurchaseScheduledInstancesResponse (Core.Maybe [Types.ScheduledInstance])
psirrsScheduledInstanceSet = Lens.field @"scheduledInstanceSet"
{-# DEPRECATED psirrsScheduledInstanceSet "Use generic-lens or generic-optics with 'scheduledInstanceSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psirrsResponseStatus :: Lens.Lens' PurchaseScheduledInstancesResponse Core.Int
psirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED psirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
