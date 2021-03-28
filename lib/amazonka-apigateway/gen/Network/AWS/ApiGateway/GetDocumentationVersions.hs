{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetDocumentationVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetDocumentationVersions
    (
    -- * Creating a request
      GetDocumentationVersions (..)
    , mkGetDocumentationVersions
    -- ** Request lenses
    , gdvRestApiId
    , gdvLimit
    , gdvPosition

    -- * Destructuring the response
    , GetDocumentationVersionsResponse (..)
    , mkGetDocumentationVersionsResponse
    -- ** Response lenses
    , gdvrrsItems
    , gdvrrsPosition
    , gdvrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets the documentation versions of an API.
--
-- /See:/ 'mkGetDocumentationVersions' smart constructor.
data GetDocumentationVersions = GetDocumentationVersions'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDocumentationVersions' value with any optional fields omitted.
mkGetDocumentationVersions
    :: Core.Text -- ^ 'restApiId'
    -> GetDocumentationVersions
mkGetDocumentationVersions restApiId
  = GetDocumentationVersions'{restApiId, limit = Core.Nothing,
                              position = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvRestApiId :: Lens.Lens' GetDocumentationVersions Core.Text
gdvRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gdvRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvLimit :: Lens.Lens' GetDocumentationVersions (Core.Maybe Core.Int)
gdvLimit = Lens.field @"limit"
{-# INLINEABLE gdvLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvPosition :: Lens.Lens' GetDocumentationVersions (Core.Maybe Core.Text)
gdvPosition = Lens.field @"position"
{-# INLINEABLE gdvPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetDocumentationVersions where
        toQuery GetDocumentationVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetDocumentationVersions where
        toHeaders GetDocumentationVersions{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetDocumentationVersions where
        type Rs GetDocumentationVersions = GetDocumentationVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/documentation/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDocumentationVersionsResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetDocumentationVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | The collection of documentation snapshots of an API. 
--
-- Use the 'DocumentationVersions' to manage documentation snapshots associated with various API stages.
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API> , 'DocumentationPart' , 'DocumentationVersion' 
--
-- /See:/ 'mkGetDocumentationVersionsResponse' smart constructor.
data GetDocumentationVersionsResponse = GetDocumentationVersionsResponse'
  { items :: Core.Maybe [Types.DocumentationVersion]
    -- ^ The current page of elements from this collection.
  , position :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDocumentationVersionsResponse' value with any optional fields omitted.
mkGetDocumentationVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDocumentationVersionsResponse
mkGetDocumentationVersionsResponse responseStatus
  = GetDocumentationVersionsResponse'{items = Core.Nothing,
                                      position = Core.Nothing, responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrrsItems :: Lens.Lens' GetDocumentationVersionsResponse (Core.Maybe [Types.DocumentationVersion])
gdvrrsItems = Lens.field @"items"
{-# INLINEABLE gdvrrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrrsPosition :: Lens.Lens' GetDocumentationVersionsResponse (Core.Maybe Core.Text)
gdvrrsPosition = Lens.field @"position"
{-# INLINEABLE gdvrrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrrsResponseStatus :: Lens.Lens' GetDocumentationVersionsResponse Core.Int
gdvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
