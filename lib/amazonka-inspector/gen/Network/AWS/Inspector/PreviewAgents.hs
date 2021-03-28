{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.PreviewAgents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Previews the agents installed on the EC2 instances that are part of the specified assessment target.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.PreviewAgents
    (
    -- * Creating a request
      PreviewAgents (..)
    , mkPreviewAgents
    -- ** Request lenses
    , paPreviewAgentsArn
    , paMaxResults
    , paNextToken

    -- * Destructuring the response
    , PreviewAgentsResponse (..)
    , mkPreviewAgentsResponse
    -- ** Response lenses
    , parrsAgentPreviews
    , parrsNextToken
    , parrsResponseStatus
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPreviewAgents' smart constructor.
data PreviewAgents = PreviewAgents'
  { previewAgentsArn :: Types.Arn
    -- ^ The ARN of the assessment target whose agents you want to preview.
  , maxResults :: Core.Maybe Core.Int
    -- ^ You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __PreviewAgents__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PreviewAgents' value with any optional fields omitted.
mkPreviewAgents
    :: Types.Arn -- ^ 'previewAgentsArn'
    -> PreviewAgents
mkPreviewAgents previewAgentsArn
  = PreviewAgents'{previewAgentsArn, maxResults = Core.Nothing,
                   nextToken = Core.Nothing}

-- | The ARN of the assessment target whose agents you want to preview.
--
-- /Note:/ Consider using 'previewAgentsArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paPreviewAgentsArn :: Lens.Lens' PreviewAgents Types.Arn
paPreviewAgentsArn = Lens.field @"previewAgentsArn"
{-# INLINEABLE paPreviewAgentsArn #-}
{-# DEPRECATED previewAgentsArn "Use generic-lens or generic-optics with 'previewAgentsArn' instead"  #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paMaxResults :: Lens.Lens' PreviewAgents (Core.Maybe Core.Int)
paMaxResults = Lens.field @"maxResults"
{-# INLINEABLE paMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __PreviewAgents__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paNextToken :: Lens.Lens' PreviewAgents (Core.Maybe Types.PaginationToken)
paNextToken = Lens.field @"nextToken"
{-# INLINEABLE paNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery PreviewAgents where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PreviewAgents where
        toHeaders PreviewAgents{..}
          = Core.pure ("X-Amz-Target", "InspectorService.PreviewAgents")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PreviewAgents where
        toJSON PreviewAgents{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("previewAgentsArn" Core..= previewAgentsArn),
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest PreviewAgents where
        type Rs PreviewAgents = PreviewAgentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PreviewAgentsResponse' Core.<$>
                   (x Core..:? "agentPreviews" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager PreviewAgents where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"agentPreviews") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkPreviewAgentsResponse' smart constructor.
data PreviewAgentsResponse = PreviewAgentsResponse'
  { agentPreviews :: [Types.AgentPreview]
    -- ^ The resulting list of agents.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PreviewAgentsResponse' value with any optional fields omitted.
mkPreviewAgentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PreviewAgentsResponse
mkPreviewAgentsResponse responseStatus
  = PreviewAgentsResponse'{agentPreviews = Core.mempty,
                           nextToken = Core.Nothing, responseStatus}

-- | The resulting list of agents.
--
-- /Note:/ Consider using 'agentPreviews' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parrsAgentPreviews :: Lens.Lens' PreviewAgentsResponse [Types.AgentPreview]
parrsAgentPreviews = Lens.field @"agentPreviews"
{-# INLINEABLE parrsAgentPreviews #-}
{-# DEPRECATED agentPreviews "Use generic-lens or generic-optics with 'agentPreviews' instead"  #-}

-- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parrsNextToken :: Lens.Lens' PreviewAgentsResponse (Core.Maybe Types.PaginationToken)
parrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE parrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parrsResponseStatus :: Lens.Lens' PreviewAgentsResponse Core.Int
parrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE parrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
