{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteQueryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a saved CloudWatch Logs Insights query definition. A query definition contains details about a saved CloudWatch Logs Insights query.
--
-- Each @DeleteQueryDefinition@ operation can delete one query definition.
-- You must have the @logs:DeleteQueryDefinition@ permission to be able to perform this operation.
module Network.AWS.CloudWatchLogs.DeleteQueryDefinition
    (
    -- * Creating a request
      DeleteQueryDefinition (..)
    , mkDeleteQueryDefinition
    -- ** Request lenses
    , dqdQueryDefinitionId

    -- * Destructuring the response
    , DeleteQueryDefinitionResponse (..)
    , mkDeleteQueryDefinitionResponse
    -- ** Response lenses
    , drsSuccess
    , drsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteQueryDefinition' smart constructor.
newtype DeleteQueryDefinition = DeleteQueryDefinition'
  { queryDefinitionId :: Types.QueryId
    -- ^ The ID of the query definition that you want to delete. You can use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions> to retrieve the IDs of your saved query definitions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQueryDefinition' value with any optional fields omitted.
mkDeleteQueryDefinition
    :: Types.QueryId -- ^ 'queryDefinitionId'
    -> DeleteQueryDefinition
mkDeleteQueryDefinition queryDefinitionId
  = DeleteQueryDefinition'{queryDefinitionId}

-- | The ID of the query definition that you want to delete. You can use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeQueryDefinitions.html DescribeQueryDefinitions> to retrieve the IDs of your saved query definitions.
--
-- /Note:/ Consider using 'queryDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdQueryDefinitionId :: Lens.Lens' DeleteQueryDefinition Types.QueryId
dqdQueryDefinitionId = Lens.field @"queryDefinitionId"
{-# INLINEABLE dqdQueryDefinitionId #-}
{-# DEPRECATED queryDefinitionId "Use generic-lens or generic-optics with 'queryDefinitionId' instead"  #-}

instance Core.ToQuery DeleteQueryDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteQueryDefinition where
        toHeaders DeleteQueryDefinition{..}
          = Core.pure ("X-Amz-Target", "Logs_20140328.DeleteQueryDefinition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteQueryDefinition where
        toJSON DeleteQueryDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("queryDefinitionId" Core..= queryDefinitionId)])

instance Core.AWSRequest DeleteQueryDefinition where
        type Rs DeleteQueryDefinition = DeleteQueryDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteQueryDefinitionResponse' Core.<$>
                   (x Core..:? "success") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteQueryDefinitionResponse' smart constructor.
data DeleteQueryDefinitionResponse = DeleteQueryDefinitionResponse'
  { success :: Core.Maybe Core.Bool
    -- ^ A value of TRUE indicates that the operation succeeded. FALSE indicates that the operation failed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteQueryDefinitionResponse' value with any optional fields omitted.
mkDeleteQueryDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteQueryDefinitionResponse
mkDeleteQueryDefinitionResponse responseStatus
  = DeleteQueryDefinitionResponse'{success = Core.Nothing,
                                   responseStatus}

-- | A value of TRUE indicates that the operation succeeded. FALSE indicates that the operation failed.
--
-- /Note:/ Consider using 'success' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsSuccess :: Lens.Lens' DeleteQueryDefinitionResponse (Core.Maybe Core.Bool)
drsSuccess = Lens.field @"success"
{-# INLINEABLE drsSuccess #-}
{-# DEPRECATED success "Use generic-lens or generic-optics with 'success' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteQueryDefinitionResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
