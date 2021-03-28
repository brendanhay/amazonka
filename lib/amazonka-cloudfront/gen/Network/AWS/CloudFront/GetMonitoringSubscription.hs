{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetMonitoringSubscription (..)
    , mkGetMonitoringSubscription
    -- ** Request lenses
    , gmsDistributionId

    -- * Destructuring the response
    , GetMonitoringSubscriptionResponse (..)
    , mkGetMonitoringSubscriptionResponse
    -- ** Response lenses
    , gmsrrsMonitoringSubscription
    , gmsrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMonitoringSubscription' smart constructor.
newtype GetMonitoringSubscription = GetMonitoringSubscription'
  { distributionId :: Core.Text
    -- ^ The ID of the distribution that you are getting metrics information for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetMonitoringSubscription' value with any optional fields omitted.
mkGetMonitoringSubscription
    :: Core.Text -- ^ 'distributionId'
    -> GetMonitoringSubscription
mkGetMonitoringSubscription distributionId
  = GetMonitoringSubscription'{distributionId}

-- | The ID of the distribution that you are getting metrics information for.
--
-- /Note:/ Consider using 'distributionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsDistributionId :: Lens.Lens' GetMonitoringSubscription Core.Text
gmsDistributionId = Lens.field @"distributionId"
{-# INLINEABLE gmsDistributionId #-}
{-# DEPRECATED distributionId "Use generic-lens or generic-optics with 'distributionId' instead"  #-}

instance Core.ToQuery GetMonitoringSubscription where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetMonitoringSubscription where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetMonitoringSubscription where
        type Rs GetMonitoringSubscription =
             GetMonitoringSubscriptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/distributions/" Core.<> Core.toText distributionId
                             Core.<> "/monitoring-subscription",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetMonitoringSubscriptionResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetMonitoringSubscriptionResponse' smart constructor.
data GetMonitoringSubscriptionResponse = GetMonitoringSubscriptionResponse'
  { monitoringSubscription :: Core.Maybe Types.MonitoringSubscription
    -- ^ A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMonitoringSubscriptionResponse' value with any optional fields omitted.
mkGetMonitoringSubscriptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetMonitoringSubscriptionResponse
mkGetMonitoringSubscriptionResponse responseStatus
  = GetMonitoringSubscriptionResponse'{monitoringSubscription =
                                         Core.Nothing,
                                       responseStatus}

-- | A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
--
-- /Note:/ Consider using 'monitoringSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsrrsMonitoringSubscription :: Lens.Lens' GetMonitoringSubscriptionResponse (Core.Maybe Types.MonitoringSubscription)
gmsrrsMonitoringSubscription = Lens.field @"monitoringSubscription"
{-# INLINEABLE gmsrrsMonitoringSubscription #-}
{-# DEPRECATED monitoringSubscription "Use generic-lens or generic-optics with 'monitoringSubscription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsrrsResponseStatus :: Lens.Lens' GetMonitoringSubscriptionResponse Core.Int
gmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
