{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetDocumentationParts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetDocumentationParts
    (
    -- * Creating a request
      GetDocumentationParts (..)
    , mkGetDocumentationParts
    -- ** Request lenses
    , gdpRestApiId
    , gdpLimit
    , gdpLocationStatus
    , gdpNameQuery
    , gdpPath
    , gdpPosition
    , gdpType

    -- * Destructuring the response
    , GetDocumentationPartsResponse (..)
    , mkGetDocumentationPartsResponse
    -- ** Response lenses
    , gdprrsItems
    , gdprrsPosition
    , gdprrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets the documentation parts of an API. The result may be filtered by the type, name, or path of API entities (targets).
--
-- /See:/ 'mkGetDocumentationParts' smart constructor.
data GetDocumentationParts = GetDocumentationParts'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , locationStatus :: Core.Maybe Types.LocationStatusType
    -- ^ The status of the API documentation parts to retrieve. Valid values are @DOCUMENTED@ for retrieving 'DocumentationPart' resources with content and @UNDOCUMENTED@ for 'DocumentationPart' resources without content.
  , nameQuery :: Core.Maybe Core.Text
    -- ^ The name of API entities of the to-be-retrieved documentation parts.
  , path :: Core.Maybe Core.Text
    -- ^ The path of API entities of the to-be-retrieved documentation parts.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  , type' :: Core.Maybe Types.DocumentationPartType
    -- ^ The type of API entities of the to-be-retrieved documentation parts. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDocumentationParts' value with any optional fields omitted.
mkGetDocumentationParts
    :: Core.Text -- ^ 'restApiId'
    -> GetDocumentationParts
mkGetDocumentationParts restApiId
  = GetDocumentationParts'{restApiId, limit = Core.Nothing,
                           locationStatus = Core.Nothing, nameQuery = Core.Nothing,
                           path = Core.Nothing, position = Core.Nothing, type' = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpRestApiId :: Lens.Lens' GetDocumentationParts Core.Text
gdpRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gdpRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpLimit :: Lens.Lens' GetDocumentationParts (Core.Maybe Core.Int)
gdpLimit = Lens.field @"limit"
{-# INLINEABLE gdpLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The status of the API documentation parts to retrieve. Valid values are @DOCUMENTED@ for retrieving 'DocumentationPart' resources with content and @UNDOCUMENTED@ for 'DocumentationPart' resources without content.
--
-- /Note:/ Consider using 'locationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpLocationStatus :: Lens.Lens' GetDocumentationParts (Core.Maybe Types.LocationStatusType)
gdpLocationStatus = Lens.field @"locationStatus"
{-# INLINEABLE gdpLocationStatus #-}
{-# DEPRECATED locationStatus "Use generic-lens or generic-optics with 'locationStatus' instead"  #-}

-- | The name of API entities of the to-be-retrieved documentation parts.
--
-- /Note:/ Consider using 'nameQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpNameQuery :: Lens.Lens' GetDocumentationParts (Core.Maybe Core.Text)
gdpNameQuery = Lens.field @"nameQuery"
{-# INLINEABLE gdpNameQuery #-}
{-# DEPRECATED nameQuery "Use generic-lens or generic-optics with 'nameQuery' instead"  #-}

-- | The path of API entities of the to-be-retrieved documentation parts.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpPath :: Lens.Lens' GetDocumentationParts (Core.Maybe Core.Text)
gdpPath = Lens.field @"path"
{-# INLINEABLE gdpPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpPosition :: Lens.Lens' GetDocumentationParts (Core.Maybe Core.Text)
gdpPosition = Lens.field @"position"
{-# INLINEABLE gdpPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The type of API entities of the to-be-retrieved documentation parts. 
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpType :: Lens.Lens' GetDocumentationParts (Core.Maybe Types.DocumentationPartType)
gdpType = Lens.field @"type'"
{-# INLINEABLE gdpType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery GetDocumentationParts where
        toQuery GetDocumentationParts{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "locationStatus")
                locationStatus
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "name") nameQuery
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "path") path
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "type") type'

instance Core.ToHeaders GetDocumentationParts where
        toHeaders GetDocumentationParts{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetDocumentationParts where
        type Rs GetDocumentationParts = GetDocumentationPartsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/documentation/parts",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDocumentationPartsResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetDocumentationParts where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | The collection of documentation parts of an API.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API> , 'DocumentationPart' 
--
-- /See:/ 'mkGetDocumentationPartsResponse' smart constructor.
data GetDocumentationPartsResponse = GetDocumentationPartsResponse'
  { items :: Core.Maybe [Types.DocumentationPart]
    -- ^ The current page of elements from this collection.
  , position :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDocumentationPartsResponse' value with any optional fields omitted.
mkGetDocumentationPartsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDocumentationPartsResponse
mkGetDocumentationPartsResponse responseStatus
  = GetDocumentationPartsResponse'{items = Core.Nothing,
                                   position = Core.Nothing, responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprrsItems :: Lens.Lens' GetDocumentationPartsResponse (Core.Maybe [Types.DocumentationPart])
gdprrsItems = Lens.field @"items"
{-# INLINEABLE gdprrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprrsPosition :: Lens.Lens' GetDocumentationPartsResponse (Core.Maybe Core.Text)
gdprrsPosition = Lens.field @"position"
{-# INLINEABLE gdprrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprrsResponseStatus :: Lens.Lens' GetDocumentationPartsResponse Core.Int
gdprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
