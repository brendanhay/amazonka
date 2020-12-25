{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetMonitoringSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about whether additional CloudWatch metrics are enabled for the specified CloudFront distribution.
module Network.AWS.CloudFront.GetMonitoringSubscription
  ( -- * Creating a request
    GetMonitoringSubscription (..),
    mkGetMonitoringSubscription,

    -- ** Request lenses
    gmsDistributionId,

    -- * Destructuring the response
    GetMonitoringSubscriptionResponse (..),
    mkGetMonitoringSubscriptionResponse,

    -- ** Response lenses
    gmsrrsMonitoringSubscription,
    gmsrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMonitoringSubscription' smart constructor.
newtype GetMonitoringSubscription = GetMonitoringSubscription'
  { -- | The ID of the distribution that you are getting metrics information for.
    distributionId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetMonitoringSubscription' value with any optional fields omitted.
mkGetMonitoringSubscription ::
  -- | 'distributionId'
  Types.String ->
  GetMonitoringSubscription
mkGetMonitoringSubscription distributionId =
  GetMonitoringSubscription' {distributionId}

-- | The ID of the distribution that you are getting metrics information for.
--
-- /Note:/ Consider using 'distributionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsDistributionId :: Lens.Lens' GetMonitoringSubscription Types.String
gmsDistributionId = Lens.field @"distributionId"
{-# DEPRECATED gmsDistributionId "Use generic-lens or generic-optics with 'distributionId' instead." #-}

instance Core.AWSRequest GetMonitoringSubscription where
  type
    Rs GetMonitoringSubscription =
      GetMonitoringSubscriptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
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
    Response.receiveXML
      ( \s h x ->
          GetMonitoringSubscriptionResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetMonitoringSubscriptionResponse' smart constructor.
data GetMonitoringSubscriptionResponse = GetMonitoringSubscriptionResponse'
  { -- | A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
    monitoringSubscription :: Core.Maybe Types.MonitoringSubscription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMonitoringSubscriptionResponse' value with any optional fields omitted.
mkGetMonitoringSubscriptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetMonitoringSubscriptionResponse
mkGetMonitoringSubscriptionResponse responseStatus =
  GetMonitoringSubscriptionResponse'
    { monitoringSubscription =
        Core.Nothing,
      responseStatus
    }

-- | A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
--
-- /Note:/ Consider using 'monitoringSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsrrsMonitoringSubscription :: Lens.Lens' GetMonitoringSubscriptionResponse (Core.Maybe Types.MonitoringSubscription)
gmsrrsMonitoringSubscription = Lens.field @"monitoringSubscription"
{-# DEPRECATED gmsrrsMonitoringSubscription "Use generic-lens or generic-optics with 'monitoringSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsrrsResponseStatus :: Lens.Lens' GetMonitoringSubscriptionResponse Core.Int
gmsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
