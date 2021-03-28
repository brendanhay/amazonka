{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetModels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes existing 'Models' defined for a 'RestApi' resource.
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetModels
    (
    -- * Creating a request
      GetModels (..)
    , mkGetModels
    -- ** Request lenses
    , gmRestApiId
    , gmLimit
    , gmPosition

    -- * Destructuring the response
    , GetModelsResponse (..)
    , mkGetModelsResponse
    -- ** Response lenses
    , gmrrsItems
    , gmrrsPosition
    , gmrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to list existing 'Models' defined for a 'RestApi' resource.
--
-- /See:/ 'mkGetModels' smart constructor.
data GetModels = GetModels'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetModels' value with any optional fields omitted.
mkGetModels
    :: Core.Text -- ^ 'restApiId'
    -> GetModels
mkGetModels restApiId
  = GetModels'{restApiId, limit = Core.Nothing,
               position = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmRestApiId :: Lens.Lens' GetModels Core.Text
gmRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gmRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmLimit :: Lens.Lens' GetModels (Core.Maybe Core.Int)
gmLimit = Lens.field @"limit"
{-# INLINEABLE gmLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmPosition :: Lens.Lens' GetModels (Core.Maybe Core.Text)
gmPosition = Lens.field @"position"
{-# INLINEABLE gmPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetModels where
        toQuery GetModels{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetModels where
        toHeaders GetModels{..} = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetModels where
        type Rs GetModels = GetModelsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/models",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetModelsResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetModels where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | Represents a collection of 'Model' resources.
--
-- 'Method' , 'MethodResponse' , <https://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html Models and Mappings> 
--
-- /See:/ 'mkGetModelsResponse' smart constructor.
data GetModelsResponse = GetModelsResponse'
  { items :: Core.Maybe [Types.Model]
    -- ^ The current page of elements from this collection.
  , position :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetModelsResponse' value with any optional fields omitted.
mkGetModelsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetModelsResponse
mkGetModelsResponse responseStatus
  = GetModelsResponse'{items = Core.Nothing, position = Core.Nothing,
                       responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrrsItems :: Lens.Lens' GetModelsResponse (Core.Maybe [Types.Model])
gmrrsItems = Lens.field @"items"
{-# INLINEABLE gmrrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrrsPosition :: Lens.Lens' GetModelsResponse (Core.Maybe Core.Text)
gmrrsPosition = Lens.field @"position"
{-# INLINEABLE gmrrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrrsResponseStatus :: Lens.Lens' GetModelsResponse Core.Int
gmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
