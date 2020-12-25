{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThingTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the existing thing types.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingTypes
  ( -- * Creating a request
    ListThingTypes (..),
    mkListThingTypes,

    -- ** Request lenses
    lttMaxResults,
    lttNextToken,
    lttThingTypeName,

    -- * Destructuring the response
    ListThingTypesResponse (..),
    mkListThingTypesResponse,

    -- ** Response lenses
    lttrrsNextToken,
    lttrrsThingTypes,
    lttrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListThingTypes operation.
--
-- /See:/ 'mkListThingTypes' smart constructor.
data ListThingTypes = ListThingTypes'
  { -- | The maximum number of results to return in this operation.
    maxResults :: Core.Maybe Core.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The name of the thing type.
    thingTypeName :: Core.Maybe Types.ThingTypeName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingTypes' value with any optional fields omitted.
mkListThingTypes ::
  ListThingTypes
mkListThingTypes =
  ListThingTypes'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      thingTypeName = Core.Nothing
    }

-- | The maximum number of results to return in this operation.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttMaxResults :: Lens.Lens' ListThingTypes (Core.Maybe Core.Natural)
lttMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lttMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttNextToken :: Lens.Lens' ListThingTypes (Core.Maybe Types.NextToken)
lttNextToken = Lens.field @"nextToken"
{-# DEPRECATED lttNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttThingTypeName :: Lens.Lens' ListThingTypes (Core.Maybe Types.ThingTypeName)
lttThingTypeName = Lens.field @"thingTypeName"
{-# DEPRECATED lttThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

instance Core.AWSRequest ListThingTypes where
  type Rs ListThingTypes = ListThingTypesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/thing-types",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "thingTypeName" Core.<$> thingTypeName),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingTypesResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "thingTypes")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListThingTypes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"thingTypes" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | The output for the ListThingTypes operation.
--
-- /See:/ 'mkListThingTypesResponse' smart constructor.
data ListThingTypesResponse = ListThingTypesResponse'
  { -- | The token for the next set of results. Will not be returned if operation has returned all results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The thing types.
    thingTypes :: Core.Maybe [Types.ThingTypeDefinition],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListThingTypesResponse' value with any optional fields omitted.
mkListThingTypesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListThingTypesResponse
mkListThingTypesResponse responseStatus =
  ListThingTypesResponse'
    { nextToken = Core.Nothing,
      thingTypes = Core.Nothing,
      responseStatus
    }

-- | The token for the next set of results. Will not be returned if operation has returned all results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttrrsNextToken :: Lens.Lens' ListThingTypesResponse (Core.Maybe Types.NextToken)
lttrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lttrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The thing types.
--
-- /Note:/ Consider using 'thingTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttrrsThingTypes :: Lens.Lens' ListThingTypesResponse (Core.Maybe [Types.ThingTypeDefinition])
lttrrsThingTypes = Lens.field @"thingTypes"
{-# DEPRECATED lttrrsThingTypes "Use generic-lens or generic-optics with 'thingTypes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lttrrsResponseStatus :: Lens.Lens' ListThingTypesResponse Core.Int
lttrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lttrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
