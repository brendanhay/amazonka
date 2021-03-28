{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteMonitoringSubscription (..)
    , mkDeleteMonitoringSubscription
    -- ** Request lenses
    , dmsDistributionId

    -- * Destructuring the response
    , DeleteMonitoringSubscriptionResponse (..)
    , mkDeleteMonitoringSubscriptionResponse
    -- ** Response lenses
    , dmsrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteMonitoringSubscription' smart constructor.
newtype DeleteMonitoringSubscription = DeleteMonitoringSubscription'
  { distributionId :: Core.Text
    -- ^ The ID of the distribution that you are disabling metrics for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMonitoringSubscription' value with any optional fields omitted.
mkDeleteMonitoringSubscription
    :: Core.Text -- ^ 'distributionId'
    -> DeleteMonitoringSubscription
mkDeleteMonitoringSubscription distributionId
  = DeleteMonitoringSubscription'{distributionId}

-- | The ID of the distribution that you are disabling metrics for.
--
-- /Note:/ Consider using 'distributionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsDistributionId :: Lens.Lens' DeleteMonitoringSubscription Core.Text
dmsDistributionId = Lens.field @"distributionId"
{-# INLINEABLE dmsDistributionId #-}
{-# DEPRECATED distributionId "Use generic-lens or generic-optics with 'distributionId' instead"  #-}

instance Core.ToQuery DeleteMonitoringSubscription where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteMonitoringSubscription where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteMonitoringSubscription where
        type Rs DeleteMonitoringSubscription =
             DeleteMonitoringSubscriptionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/2020-05-31/distributions/" Core.<> Core.toText distributionId
                             Core.<> "/monitoring-subscription",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteMonitoringSubscriptionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteMonitoringSubscriptionResponse' smart constructor.
newtype DeleteMonitoringSubscriptionResponse = DeleteMonitoringSubscriptionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMonitoringSubscriptionResponse' value with any optional fields omitted.
mkDeleteMonitoringSubscriptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteMonitoringSubscriptionResponse
mkDeleteMonitoringSubscriptionResponse responseStatus
  = DeleteMonitoringSubscriptionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrrsResponseStatus :: Lens.Lens' DeleteMonitoringSubscriptionResponse Core.Int
dmsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
