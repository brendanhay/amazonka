{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateMonitoringSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables additional CloudWatch metrics for the specified CloudFront distribution. The additional metrics incur an additional cost.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/viewing-cloudfront-metrics.html#monitoring-console.distributions-additional Viewing additional CloudFront distribution metrics> in the /Amazon CloudFront Developer Guide/ .
module Network.AWS.CloudFront.CreateMonitoringSubscription
    (
    -- * Creating a request
      CreateMonitoringSubscription (..)
    , mkCreateMonitoringSubscription
    -- ** Request lenses
    , cmsMonitoringSubscription
    , cmsDistributionId

    -- * Destructuring the response
    , CreateMonitoringSubscriptionResponse (..)
    , mkCreateMonitoringSubscriptionResponse
    -- ** Response lenses
    , cmsrrsMonitoringSubscription
    , cmsrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateMonitoringSubscription' smart constructor.
data CreateMonitoringSubscription = CreateMonitoringSubscription'
  { monitoringSubscription :: Types.MonitoringSubscription
    -- ^ A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
  , distributionId :: Core.Text
    -- ^ The ID of the distribution that you are enabling metrics for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMonitoringSubscription' value with any optional fields omitted.
mkCreateMonitoringSubscription
    :: Types.MonitoringSubscription -- ^ 'monitoringSubscription'
    -> Core.Text -- ^ 'distributionId'
    -> CreateMonitoringSubscription
mkCreateMonitoringSubscription monitoringSubscription
  distributionId
  = CreateMonitoringSubscription'{monitoringSubscription,
                                  distributionId}

-- | A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
--
-- /Note:/ Consider using 'monitoringSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsMonitoringSubscription :: Lens.Lens' CreateMonitoringSubscription Types.MonitoringSubscription
cmsMonitoringSubscription = Lens.field @"monitoringSubscription"
{-# INLINEABLE cmsMonitoringSubscription #-}
{-# DEPRECATED monitoringSubscription "Use generic-lens or generic-optics with 'monitoringSubscription' instead"  #-}

-- | The ID of the distribution that you are enabling metrics for.
--
-- /Note:/ Consider using 'distributionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsDistributionId :: Lens.Lens' CreateMonitoringSubscription Core.Text
cmsDistributionId = Lens.field @"distributionId"
{-# INLINEABLE cmsDistributionId #-}
{-# DEPRECATED distributionId "Use generic-lens or generic-optics with 'distributionId' instead"  #-}

instance Core.ToQuery CreateMonitoringSubscription where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateMonitoringSubscription where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateMonitoringSubscription where
        type Rs CreateMonitoringSubscription =
             CreateMonitoringSubscriptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/2020-05-31/distributions/" Core.<> Core.toText distributionId
                             Core.<> "/monitoring-subscription",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateMonitoringSubscriptionResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateMonitoringSubscriptionResponse' smart constructor.
data CreateMonitoringSubscriptionResponse = CreateMonitoringSubscriptionResponse'
  { monitoringSubscription :: Core.Maybe Types.MonitoringSubscription
    -- ^ A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMonitoringSubscriptionResponse' value with any optional fields omitted.
mkCreateMonitoringSubscriptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateMonitoringSubscriptionResponse
mkCreateMonitoringSubscriptionResponse responseStatus
  = CreateMonitoringSubscriptionResponse'{monitoringSubscription =
                                            Core.Nothing,
                                          responseStatus}

-- | A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
--
-- /Note:/ Consider using 'monitoringSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsrrsMonitoringSubscription :: Lens.Lens' CreateMonitoringSubscriptionResponse (Core.Maybe Types.MonitoringSubscription)
cmsrrsMonitoringSubscription = Lens.field @"monitoringSubscription"
{-# INLINEABLE cmsrrsMonitoringSubscription #-}
{-# DEPRECATED monitoringSubscription "Use generic-lens or generic-optics with 'monitoringSubscription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsrrsResponseStatus :: Lens.Lens' CreateMonitoringSubscriptionResponse Core.Int
cmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
