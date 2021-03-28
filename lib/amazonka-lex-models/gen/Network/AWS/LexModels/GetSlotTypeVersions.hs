{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetSlotTypeVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all versions of a slot type.
--
-- The @GetSlotTypeVersions@ operation returns a @SlotTypeMetadata@ object for each version of a slot type. For example, if a slot type has three numbered versions, the @GetSlotTypeVersions@ operation returns four @SlotTypeMetadata@ objects in the response, one for each numbered version and one for the @> LATEST@ version. 
-- The @GetSlotTypeVersions@ operation always returns at least one version, the @> LATEST@ version.
-- This operation requires permissions for the @lex:GetSlotTypeVersions@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetSlotTypeVersions
    (
    -- * Creating a request
      GetSlotTypeVersions (..)
    , mkGetSlotTypeVersions
    -- ** Request lenses
    , gstvName
    , gstvMaxResults
    , gstvNextToken

    -- * Destructuring the response
    , GetSlotTypeVersionsResponse (..)
    , mkGetSlotTypeVersionsResponse
    -- ** Response lenses
    , gstvrrsNextToken
    , gstvrrsSlotTypes
    , gstvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSlotTypeVersions' smart constructor.
data GetSlotTypeVersions = GetSlotTypeVersions'
  { name :: Types.Name
    -- ^ The name of the slot type for which versions should be returned.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of slot type versions to return in the response. The default is 10.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token for fetching the next page of slot type versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSlotTypeVersions' value with any optional fields omitted.
mkGetSlotTypeVersions
    :: Types.Name -- ^ 'name'
    -> GetSlotTypeVersions
mkGetSlotTypeVersions name
  = GetSlotTypeVersions'{name, maxResults = Core.Nothing,
                         nextToken = Core.Nothing}

-- | The name of the slot type for which versions should be returned.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstvName :: Lens.Lens' GetSlotTypeVersions Types.Name
gstvName = Lens.field @"name"
{-# INLINEABLE gstvName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The maximum number of slot type versions to return in the response. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstvMaxResults :: Lens.Lens' GetSlotTypeVersions (Core.Maybe Core.Natural)
gstvMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gstvMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A pagination token for fetching the next page of slot type versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstvNextToken :: Lens.Lens' GetSlotTypeVersions (Core.Maybe Types.NextToken)
gstvNextToken = Lens.field @"nextToken"
{-# INLINEABLE gstvNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetSlotTypeVersions where
        toQuery GetSlotTypeVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders GetSlotTypeVersions where
        toHeaders GetSlotTypeVersions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetSlotTypeVersions where
        type Rs GetSlotTypeVersions = GetSlotTypeVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/slottypes/" Core.<> Core.toText name Core.<> "/versions/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSlotTypeVersionsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "slotTypes" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetSlotTypeVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"slotTypes" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetSlotTypeVersionsResponse' smart constructor.
data GetSlotTypeVersionsResponse = GetSlotTypeVersionsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token for fetching the next page of slot type versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request. 
  , slotTypes :: Core.Maybe [Types.SlotTypeMetadata]
    -- ^ An array of @SlotTypeMetadata@ objects, one for each numbered version of the slot type plus one for the @> LATEST@ version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetSlotTypeVersionsResponse' value with any optional fields omitted.
mkGetSlotTypeVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSlotTypeVersionsResponse
mkGetSlotTypeVersionsResponse responseStatus
  = GetSlotTypeVersionsResponse'{nextToken = Core.Nothing,
                                 slotTypes = Core.Nothing, responseStatus}

-- | A pagination token for fetching the next page of slot type versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstvrrsNextToken :: Lens.Lens' GetSlotTypeVersionsResponse (Core.Maybe Types.NextToken)
gstvrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gstvrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of @SlotTypeMetadata@ objects, one for each numbered version of the slot type plus one for the @> LATEST@ version.
--
-- /Note:/ Consider using 'slotTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstvrrsSlotTypes :: Lens.Lens' GetSlotTypeVersionsResponse (Core.Maybe [Types.SlotTypeMetadata])
gstvrrsSlotTypes = Lens.field @"slotTypes"
{-# INLINEABLE gstvrrsSlotTypes #-}
{-# DEPRECATED slotTypes "Use generic-lens or generic-optics with 'slotTypes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstvrrsResponseStatus :: Lens.Lens' GetSlotTypeVersionsResponse Core.Int
gstvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gstvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
