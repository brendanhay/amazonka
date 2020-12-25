{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteMonitoringSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables additional CloudWatch metrics for the specified CloudFront distribution.
module Network.AWS.CloudFront.DeleteMonitoringSubscription
  ( -- * Creating a request
    DeleteMonitoringSubscription (..),
    mkDeleteMonitoringSubscription,

    -- ** Request lenses
    dmsDistributionId,

    -- * Destructuring the response
    DeleteMonitoringSubscriptionResponse (..),
    mkDeleteMonitoringSubscriptionResponse,

    -- ** Response lenses
    dmsrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteMonitoringSubscription' smart constructor.
newtype DeleteMonitoringSubscription = DeleteMonitoringSubscription'
  { -- | The ID of the distribution that you are disabling metrics for.
    distributionId :: Types.DistributionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMonitoringSubscription' value with any optional fields omitted.
mkDeleteMonitoringSubscription ::
  -- | 'distributionId'
  Types.DistributionId ->
  DeleteMonitoringSubscription
mkDeleteMonitoringSubscription distributionId =
  DeleteMonitoringSubscription' {distributionId}

-- | The ID of the distribution that you are disabling metrics for.
--
-- /Note:/ Consider using 'distributionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsDistributionId :: Lens.Lens' DeleteMonitoringSubscription Types.DistributionId
dmsDistributionId = Lens.field @"distributionId"
{-# DEPRECATED dmsDistributionId "Use generic-lens or generic-optics with 'distributionId' instead." #-}

instance Core.AWSRequest DeleteMonitoringSubscription where
  type
    Rs DeleteMonitoringSubscription =
      DeleteMonitoringSubscriptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/2020-05-31/distributions/" Core.<> (Core.toText distributionId)
                Core.<> ("/monitoring-subscription")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMonitoringSubscriptionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteMonitoringSubscriptionResponse' smart constructor.
newtype DeleteMonitoringSubscriptionResponse = DeleteMonitoringSubscriptionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMonitoringSubscriptionResponse' value with any optional fields omitted.
mkDeleteMonitoringSubscriptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteMonitoringSubscriptionResponse
mkDeleteMonitoringSubscriptionResponse responseStatus =
  DeleteMonitoringSubscriptionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrrsResponseStatus :: Lens.Lens' DeleteMonitoringSubscriptionResponse Core.Int
dmsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
