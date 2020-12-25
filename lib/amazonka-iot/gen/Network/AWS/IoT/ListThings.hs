{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListThings (..),
    mkListThings,

    -- ** Request lenses
    ltAttributeName,
    ltAttributeValue,
    ltMaxResults,
    ltNextToken,
    ltThingTypeName,

    -- * Destructuring the response
    ListThingsResponse (..),
    mkListThingsResponse,

    -- ** Response lenses
    ltrrsNextToken,
    ltrrsThings,
    ltrrsResponseStatus,
  )
where

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
  { -- | The attribute name used to search for things.
    attributeName :: Core.Maybe Types.AttributeName,
    -- | The attribute value used to search for things.
    attributeValue :: Core.Maybe Types.AttributeValue,
    -- | The maximum number of results to return in this operation.
    maxResults :: Core.Maybe Core.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The name of the thing type used to search for things.
    thingTypeName :: Core.Maybe Types.ThingTypeName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThings' value with any optional fields omitted.
mkListThings ::
  ListThings
mkListThings =
  ListThings'
    { attributeName = Core.Nothing,
      attributeValue = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      thingTypeName = Core.Nothing
    }

-- | The attribute name used to search for things.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltAttributeName :: Lens.Lens' ListThings (Core.Maybe Types.AttributeName)
ltAttributeName = Lens.field @"attributeName"
{-# DEPRECATED ltAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The attribute value used to search for things.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltAttributeValue :: Lens.Lens' ListThings (Core.Maybe Types.AttributeValue)
ltAttributeValue = Lens.field @"attributeValue"
{-# DEPRECATED ltAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

-- | The maximum number of results to return in this operation.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListThings (Core.Maybe Core.Natural)
ltMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListThings (Core.Maybe Types.NextToken)
ltNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the thing type used to search for things.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltThingTypeName :: Lens.Lens' ListThings (Core.Maybe Types.ThingTypeName)
ltThingTypeName = Lens.field @"thingTypeName"
{-# DEPRECATED ltThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

instance Core.AWSRequest ListThings where
  type Rs ListThings = ListThingsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/things",
        Core._rqQuery =
          Core.toQueryValue "attributeName" Core.<$> attributeName
            Core.<> (Core.toQueryValue "attributeValue" Core.<$> attributeValue)
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "thingTypeName" Core.<$> thingTypeName),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingsResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "things")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListThings where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"things" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | The output from the ListThings operation.
--
-- /See:/ 'mkListThingsResponse' smart constructor.
data ListThingsResponse = ListThingsResponse'
  { -- | The token to use to get the next set of results. Will not be returned if operation has returned all results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The things.
    things :: Core.Maybe [Types.ThingAttribute],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingsResponse' value with any optional fields omitted.
mkListThingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListThingsResponse
mkListThingsResponse responseStatus =
  ListThingsResponse'
    { nextToken = Core.Nothing,
      things = Core.Nothing,
      responseStatus
    }

-- | The token to use to get the next set of results. Will not be returned if operation has returned all results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsNextToken :: Lens.Lens' ListThingsResponse (Core.Maybe Types.NextToken)
ltrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The things.
--
-- /Note:/ Consider using 'things' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsThings :: Lens.Lens' ListThingsResponse (Core.Maybe [Types.ThingAttribute])
ltrrsThings = Lens.field @"things"
{-# DEPRECATED ltrrsThings "Use generic-lens or generic-optics with 'things' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrrsResponseStatus :: Lens.Lens' ListThingsResponse Core.Int
ltrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
