{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.CreateUsageReportSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage report subscription. Usage reports are generated daily.
module Network.AWS.AppStream.CreateUsageReportSubscription
    (
    -- * Creating a request
      CreateUsageReportSubscription (..)
    , mkCreateUsageReportSubscription

    -- * Destructuring the response
    , CreateUsageReportSubscriptionResponse (..)
    , mkCreateUsageReportSubscriptionResponse
    -- ** Response lenses
    , cursrrsS3BucketName
    , cursrrsSchedule
    , cursrrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateUsageReportSubscription' smart constructor.
data CreateUsageReportSubscription = CreateUsageReportSubscription'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUsageReportSubscription' value with any optional fields omitted.
mkCreateUsageReportSubscription
    :: CreateUsageReportSubscription
mkCreateUsageReportSubscription = CreateUsageReportSubscription'

instance Core.ToQuery CreateUsageReportSubscription where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateUsageReportSubscription where
        toHeaders CreateUsageReportSubscription{..}
          = Core.pure
              ("X-Amz-Target",
               "PhotonAdminProxyService.CreateUsageReportSubscription")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateUsageReportSubscription where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest CreateUsageReportSubscription where
        type Rs CreateUsageReportSubscription =
             CreateUsageReportSubscriptionResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateUsageReportSubscriptionResponse' Core.<$>
                   (x Core..:? "S3BucketName") Core.<*> x Core..:? "Schedule" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateUsageReportSubscriptionResponse' smart constructor.
data CreateUsageReportSubscriptionResponse = CreateUsageReportSubscriptionResponse'
  { s3BucketName :: Core.Maybe Core.Text
    -- ^ The Amazon S3 bucket where generated reports are stored.
--
-- If you enabled on-instance session scripts and Amazon S3 logging for your session script configuration, AppStream 2.0 created an S3 bucket to store the script output. The bucket is unique to your account and Region. When you enable usage reporting in this case, AppStream 2.0 uses the same bucket to store your usage reports. If you haven't already enabled on-instance session scripts, when you enable usage reports, AppStream 2.0 creates a new S3 bucket.
  , schedule :: Core.Maybe Types.UsageReportSchedule
    -- ^ The schedule for generating usage reports.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUsageReportSubscriptionResponse' value with any optional fields omitted.
mkCreateUsageReportSubscriptionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateUsageReportSubscriptionResponse
mkCreateUsageReportSubscriptionResponse responseStatus
  = CreateUsageReportSubscriptionResponse'{s3BucketName =
                                             Core.Nothing,
                                           schedule = Core.Nothing, responseStatus}

-- | The Amazon S3 bucket where generated reports are stored.
--
-- If you enabled on-instance session scripts and Amazon S3 logging for your session script configuration, AppStream 2.0 created an S3 bucket to store the script output. The bucket is unique to your account and Region. When you enable usage reporting in this case, AppStream 2.0 uses the same bucket to store your usage reports. If you haven't already enabled on-instance session scripts, when you enable usage reports, AppStream 2.0 creates a new S3 bucket.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursrrsS3BucketName :: Lens.Lens' CreateUsageReportSubscriptionResponse (Core.Maybe Core.Text)
cursrrsS3BucketName = Lens.field @"s3BucketName"
{-# INLINEABLE cursrrsS3BucketName #-}
{-# DEPRECATED s3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead"  #-}

-- | The schedule for generating usage reports.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursrrsSchedule :: Lens.Lens' CreateUsageReportSubscriptionResponse (Core.Maybe Types.UsageReportSchedule)
cursrrsSchedule = Lens.field @"schedule"
{-# INLINEABLE cursrrsSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursrrsResponseStatus :: Lens.Lens' CreateUsageReportSubscriptionResponse Core.Int
cursrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cursrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
