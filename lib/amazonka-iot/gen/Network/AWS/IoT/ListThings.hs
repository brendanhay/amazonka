{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your things. Use the __attributeName__ and __attributeValue__ parameters to filter your things. For example, calling @ListThings@ with attributeName=Color and attributeValue=Red retrieves all things in the registry that contain an attribute __Color__ with the value __Red__ . 
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThings
    (
    -- * Creating a request
      ListThings (..)
    , mkListThings
    -- ** Request lenses
    , ltAttributeName
    , ltAttributeValue
    , ltMaxResults
    , ltNextToken
    , ltThingTypeName

    -- * Destructuring the response
    , ListThingsResponse (..)
    , mkListThingsResponse
    -- ** Response lenses
    , ltrrsNextToken
    , ltrrsThings
    , ltrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListThings operation.
--
-- /See:/ 'mkListThings' smart constructor.
data ListThings = ListThings'
  { attributeName :: Core.Maybe Types.AttributeName
    -- ^ The attribute name used to search for things.
  , attributeValue :: Core.Maybe Types.AttributeValue
    -- ^ The attribute value used to search for things.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in this operation.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
  , thingTypeName :: Core.Maybe Types.ThingTypeName
    -- ^ The name of the thing type used to search for things.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThings' value with any optional fields omitted.
mkListThings
    :: ListThings
mkListThings
  = ListThings'{attributeName = Core.Nothing,
                attributeValue = Core.Nothing, maxResults = Core.Nothing,
                nextToken = Core.Nothing, thingTypeName = Core.Nothing}

-- | The attribute name used to search for things.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltAttributeName :: Lens.Lens' ListThings (Core.Maybe Types.AttributeName)
ltAttributeName = Lens.field @"attributeName"
{-# INLINEABLE ltAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

-- | The attribute value used to search for things.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltAttributeValue :: Lens.Lens' ListThings (Core.Maybe Types.AttributeValue)
ltAttributeValue = Lens.field @"attributeValue"
{-# INLINEABLE ltAttributeValue #-}
{-# DEPRECATED attributeValue "Use generic-lens or generic-optics with 'attributeValue' instead"  #-}

-- | The maximum number of results to return in this operation.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListThings (Core.Maybe Core.Natural)
ltMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListThings (Core.Maybe Types.NextToken)
ltNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The name of the thing type used to search for things.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltThingTypeName :: Lens.Lens' ListThings (Core.Maybe Types.ThingTypeName)
ltThingTypeName = Lens.field @"thingTypeName"
{-# INLINEABLE ltThingTypeName #-}
{-# DEPRECATED thingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead"  #-}

instance Core.ToQuery ListThings where
        toQuery ListThings{..}
          = Core.maybe Core.mempty (Core.toQueryPair "attributeName")
              attributeName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "attributeValue")
                attributeValue
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "thingTypeName")
                thingTypeName

instance Core.ToHeaders ListThings where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListThings where
        type Rs ListThings = ListThingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/things",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListThingsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "things" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListThings where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"things" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | The output from the ListThings operation.
--
-- /See:/ 'mkListThingsResponse' smart constructor.
data ListThingsResponse = ListThingsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to get the next set of results. Will not be returned if operation has returned all results.
  , things :: Core.Maybe [Types.ThingAttribute]
    -- ^ The things.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingsResponse' value with any optional fields omitted.
mkListThingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListThingsResponse
mkListThingsResponse responseStatus
  = ListThingsResponse'{nextToken = Core.Nothing,
                        things = Core.Nothing, responseStatus}

-- | The token to use to get the next set of results. Will not be returned if operation has returned all results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListThingsResponse (Core.Maybe Types.NextToken)
ltrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The things.
--
-- /Note:/ Consider using 'things' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsThings :: Lens.Lens' ListThingsResponse (Core.Maybe [Types.ThingAttribute])
ltrrsThings = Lens.field @"things"
{-# INLINEABLE ltrrsThings #-}
{-# DEPRECATED things "Use generic-lens or generic-optics with 'things' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListThingsResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
