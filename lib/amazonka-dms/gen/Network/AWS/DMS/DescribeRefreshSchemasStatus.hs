{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeRefreshSchemasStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of the RefreshSchemas operation.
module Network.AWS.DMS.DescribeRefreshSchemasStatus
    (
    -- * Creating a request
      DescribeRefreshSchemasStatus (..)
    , mkDescribeRefreshSchemasStatus
    -- ** Request lenses
    , drssEndpointArn

    -- * Destructuring the response
    , DescribeRefreshSchemasStatusResponse (..)
    , mkDescribeRefreshSchemasStatusResponse
    -- ** Response lenses
    , drssrrsRefreshSchemasStatus
    , drssrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeRefreshSchemasStatus' smart constructor.
newtype DescribeRefreshSchemasStatus = DescribeRefreshSchemasStatus'
  { endpointArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRefreshSchemasStatus' value with any optional fields omitted.
mkDescribeRefreshSchemasStatus
    :: Core.Text -- ^ 'endpointArn'
    -> DescribeRefreshSchemasStatus
mkDescribeRefreshSchemasStatus endpointArn
  = DescribeRefreshSchemasStatus'{endpointArn}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drssEndpointArn :: Lens.Lens' DescribeRefreshSchemasStatus Core.Text
drssEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE drssEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

instance Core.ToQuery DescribeRefreshSchemasStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeRefreshSchemasStatus where
        toHeaders DescribeRefreshSchemasStatus{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.DescribeRefreshSchemasStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeRefreshSchemasStatus where
        toJSON DescribeRefreshSchemasStatus{..}
          = Core.object
              (Core.catMaybes [Core.Just ("EndpointArn" Core..= endpointArn)])

instance Core.AWSRequest DescribeRefreshSchemasStatus where
        type Rs DescribeRefreshSchemasStatus =
             DescribeRefreshSchemasStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeRefreshSchemasStatusResponse' Core.<$>
                   (x Core..:? "RefreshSchemasStatus") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkDescribeRefreshSchemasStatusResponse' smart constructor.
data DescribeRefreshSchemasStatusResponse = DescribeRefreshSchemasStatusResponse'
  { refreshSchemasStatus :: Core.Maybe Types.RefreshSchemasStatus
    -- ^ The status of the schema.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeRefreshSchemasStatusResponse' value with any optional fields omitted.
mkDescribeRefreshSchemasStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeRefreshSchemasStatusResponse
mkDescribeRefreshSchemasStatusResponse responseStatus
  = DescribeRefreshSchemasStatusResponse'{refreshSchemasStatus =
                                            Core.Nothing,
                                          responseStatus}

-- | The status of the schema.
--
-- /Note:/ Consider using 'refreshSchemasStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drssrrsRefreshSchemasStatus :: Lens.Lens' DescribeRefreshSchemasStatusResponse (Core.Maybe Types.RefreshSchemasStatus)
drssrrsRefreshSchemasStatus = Lens.field @"refreshSchemasStatus"
{-# INLINEABLE drssrrsRefreshSchemasStatus #-}
{-# DEPRECATED refreshSchemasStatus "Use generic-lens or generic-optics with 'refreshSchemasStatus' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drssrrsResponseStatus :: Lens.Lens' DescribeRefreshSchemasStatusResponse Core.Int
drssrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drssrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
