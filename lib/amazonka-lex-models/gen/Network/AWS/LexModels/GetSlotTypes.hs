{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetSlotTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns slot type information as follows: 
--
--
--     * If you specify the @nameContains@ field, returns the @> LATEST@ version of all slot types that contain the specified string.
--
--
--     * If you don't specify the @nameContains@ field, returns information about the @> LATEST@ version of all slot types. 
--
--
-- The operation requires permission for the @lex:GetSlotTypes@ action. 
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetSlotTypes
    (
    -- * Creating a request
      GetSlotTypes (..)
    , mkGetSlotTypes
    -- ** Request lenses
    , gstMaxResults
    , gstNameContains
    , gstNextToken

    -- * Destructuring the response
    , GetSlotTypesResponse (..)
    , mkGetSlotTypesResponse
    -- ** Response lenses
    , gstrrsNextToken
    , gstrrsSlotTypes
    , gstrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSlotTypes' smart constructor.
data GetSlotTypes = GetSlotTypes'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of slot types to return in the response. The default is 10.
  , nameContains :: Core.Maybe Types.NameContains
    -- ^ Substring to match in slot type names. A slot type will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A pagination token that fetches the next page of slot types. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch next page of slot types, specify the pagination token in the next request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSlotTypes' value with any optional fields omitted.
mkGetSlotTypes
    :: GetSlotTypes
mkGetSlotTypes
  = GetSlotTypes'{maxResults = Core.Nothing,
                  nameContains = Core.Nothing, nextToken = Core.Nothing}

-- | The maximum number of slot types to return in the response. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstMaxResults :: Lens.Lens' GetSlotTypes (Core.Maybe Core.Natural)
gstMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gstMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Substring to match in slot type names. A slot type will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstNameContains :: Lens.Lens' GetSlotTypes (Core.Maybe Types.NameContains)
gstNameContains = Lens.field @"nameContains"
{-# INLINEABLE gstNameContains #-}
{-# DEPRECATED nameContains "Use generic-lens or generic-optics with 'nameContains' instead"  #-}

-- | A pagination token that fetches the next page of slot types. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch next page of slot types, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstNextToken :: Lens.Lens' GetSlotTypes (Core.Maybe Types.NextToken)
gstNextToken = Lens.field @"nextToken"
{-# INLINEABLE gstNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetSlotTypes where
        toQuery GetSlotTypes{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nameContains")
                nameContains
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders GetSlotTypes where
        toHeaders GetSlotTypes{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetSlotTypes where
        type Rs GetSlotTypes = GetSlotTypesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/slottypes/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSlotTypesResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "slotTypes" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetSlotTypes where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"slotTypes" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetSlotTypesResponse' smart constructor.
data GetSlotTypesResponse = GetSlotTypesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If the response is truncated, it includes a pagination token that you can specify in your next request to fetch the next page of slot types.
  , slotTypes :: Core.Maybe [Types.SlotTypeMetadata]
    -- ^ An array of objects, one for each slot type, that provides information such as the name of the slot type, the version, and a description.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetSlotTypesResponse' value with any optional fields omitted.
mkGetSlotTypesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSlotTypesResponse
mkGetSlotTypesResponse responseStatus
  = GetSlotTypesResponse'{nextToken = Core.Nothing,
                          slotTypes = Core.Nothing, responseStatus}

-- | If the response is truncated, it includes a pagination token that you can specify in your next request to fetch the next page of slot types.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrrsNextToken :: Lens.Lens' GetSlotTypesResponse (Core.Maybe Types.NextToken)
gstrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gstrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of objects, one for each slot type, that provides information such as the name of the slot type, the version, and a description.
--
-- /Note:/ Consider using 'slotTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrrsSlotTypes :: Lens.Lens' GetSlotTypesResponse (Core.Maybe [Types.SlotTypeMetadata])
gstrrsSlotTypes = Lens.field @"slotTypes"
{-# INLINEABLE gstrrsSlotTypes #-}
{-# DEPRECATED slotTypes "Use generic-lens or generic-optics with 'slotTypes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrrsResponseStatus :: Lens.Lens' GetSlotTypesResponse Core.Int
gstrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gstrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
