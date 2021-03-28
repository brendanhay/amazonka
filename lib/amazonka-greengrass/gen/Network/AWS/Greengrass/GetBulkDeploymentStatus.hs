{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetBulkDeploymentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of a bulk deployment.
module Network.AWS.Greengrass.GetBulkDeploymentStatus
    (
    -- * Creating a request
      GetBulkDeploymentStatus (..)
    , mkGetBulkDeploymentStatus
    -- ** Request lenses
    , gbdsBulkDeploymentId

    -- * Destructuring the response
    , GetBulkDeploymentStatusResponse (..)
    , mkGetBulkDeploymentStatusResponse
    -- ** Response lenses
    , gbdsrrsBulkDeploymentMetrics
    , gbdsrrsBulkDeploymentStatus
    , gbdsrrsCreatedAt
    , gbdsrrsErrorDetails
    , gbdsrrsErrorMessage
    , gbdsrrsTags
    , gbdsrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBulkDeploymentStatus' smart constructor.
newtype GetBulkDeploymentStatus = GetBulkDeploymentStatus'
  { bulkDeploymentId :: Core.Text
    -- ^ The ID of the bulk deployment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetBulkDeploymentStatus' value with any optional fields omitted.
mkGetBulkDeploymentStatus
    :: Core.Text -- ^ 'bulkDeploymentId'
    -> GetBulkDeploymentStatus
mkGetBulkDeploymentStatus bulkDeploymentId
  = GetBulkDeploymentStatus'{bulkDeploymentId}

-- | The ID of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsBulkDeploymentId :: Lens.Lens' GetBulkDeploymentStatus Core.Text
gbdsBulkDeploymentId = Lens.field @"bulkDeploymentId"
{-# INLINEABLE gbdsBulkDeploymentId #-}
{-# DEPRECATED bulkDeploymentId "Use generic-lens or generic-optics with 'bulkDeploymentId' instead"  #-}

instance Core.ToQuery GetBulkDeploymentStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetBulkDeploymentStatus where
        toHeaders GetBulkDeploymentStatus{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetBulkDeploymentStatus where
        type Rs GetBulkDeploymentStatus = GetBulkDeploymentStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/bulk/deployments/" Core.<>
                             Core.toText bulkDeploymentId
                             Core.<> "/status",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetBulkDeploymentStatusResponse' Core.<$>
                   (x Core..:? "BulkDeploymentMetrics") Core.<*>
                     x Core..:? "BulkDeploymentStatus"
                     Core.<*> x Core..:? "CreatedAt"
                     Core.<*> x Core..:? "ErrorDetails"
                     Core.<*> x Core..:? "ErrorMessage"
                     Core.<*> x Core..:? "tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetBulkDeploymentStatusResponse' smart constructor.
data GetBulkDeploymentStatusResponse = GetBulkDeploymentStatusResponse'
  { bulkDeploymentMetrics :: Core.Maybe Types.BulkDeploymentMetrics
    -- ^ Relevant metrics on input records processed during bulk deployment.
  , bulkDeploymentStatus :: Core.Maybe Types.BulkDeploymentStatus
    -- ^ The status of the bulk deployment.
  , createdAt :: Core.Maybe Core.Text
    -- ^ The time, in ISO format, when the deployment was created.
  , errorDetails :: Core.Maybe [Types.ErrorDetail]
    -- ^ Error details
  , errorMessage :: Core.Maybe Core.Text
    -- ^ Error message
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Tag(s) attached to the resource arn.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBulkDeploymentStatusResponse' value with any optional fields omitted.
mkGetBulkDeploymentStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBulkDeploymentStatusResponse
mkGetBulkDeploymentStatusResponse responseStatus
  = GetBulkDeploymentStatusResponse'{bulkDeploymentMetrics =
                                       Core.Nothing,
                                     bulkDeploymentStatus = Core.Nothing, createdAt = Core.Nothing,
                                     errorDetails = Core.Nothing, errorMessage = Core.Nothing,
                                     tags = Core.Nothing, responseStatus}

-- | Relevant metrics on input records processed during bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrrsBulkDeploymentMetrics :: Lens.Lens' GetBulkDeploymentStatusResponse (Core.Maybe Types.BulkDeploymentMetrics)
gbdsrrsBulkDeploymentMetrics = Lens.field @"bulkDeploymentMetrics"
{-# INLINEABLE gbdsrrsBulkDeploymentMetrics #-}
{-# DEPRECATED bulkDeploymentMetrics "Use generic-lens or generic-optics with 'bulkDeploymentMetrics' instead"  #-}

-- | The status of the bulk deployment.
--
-- /Note:/ Consider using 'bulkDeploymentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrrsBulkDeploymentStatus :: Lens.Lens' GetBulkDeploymentStatusResponse (Core.Maybe Types.BulkDeploymentStatus)
gbdsrrsBulkDeploymentStatus = Lens.field @"bulkDeploymentStatus"
{-# INLINEABLE gbdsrrsBulkDeploymentStatus #-}
{-# DEPRECATED bulkDeploymentStatus "Use generic-lens or generic-optics with 'bulkDeploymentStatus' instead"  #-}

-- | The time, in ISO format, when the deployment was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrrsCreatedAt :: Lens.Lens' GetBulkDeploymentStatusResponse (Core.Maybe Core.Text)
gbdsrrsCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE gbdsrrsCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | Error details
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrrsErrorDetails :: Lens.Lens' GetBulkDeploymentStatusResponse (Core.Maybe [Types.ErrorDetail])
gbdsrrsErrorDetails = Lens.field @"errorDetails"
{-# INLINEABLE gbdsrrsErrorDetails #-}
{-# DEPRECATED errorDetails "Use generic-lens or generic-optics with 'errorDetails' instead"  #-}

-- | Error message
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrrsErrorMessage :: Lens.Lens' GetBulkDeploymentStatusResponse (Core.Maybe Core.Text)
gbdsrrsErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE gbdsrrsErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrrsTags :: Lens.Lens' GetBulkDeploymentStatusResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
gbdsrrsTags = Lens.field @"tags"
{-# INLINEABLE gbdsrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbdsrrsResponseStatus :: Lens.Lens' GetBulkDeploymentStatusResponse Core.Int
gbdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
