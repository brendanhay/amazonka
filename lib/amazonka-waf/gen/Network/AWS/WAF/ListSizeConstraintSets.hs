{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.ListSizeConstraintSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'SizeConstraintSetSummary' objects.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListSizeConstraintSets
  ( -- * Creating a request
    ListSizeConstraintSets (..),
    mkListSizeConstraintSets,

    -- ** Request lenses
    lscsLimit,
    lscsNextMarker,

    -- * Destructuring the response
    ListSizeConstraintSetsResponse (..),
    mkListSizeConstraintSetsResponse,

    -- ** Response lenses
    lscsrrsNextMarker,
    lscsrrsSizeConstraintSets,
    lscsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkListSizeConstraintSets' smart constructor.
data ListSizeConstraintSets = ListSizeConstraintSets'
  { -- | Specifies the number of @SizeConstraintSet@ objects that you want AWS WAF to return for this request. If you have more @SizeConstraintSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @SizeConstraintSet@ objects.
    limit :: Core.Maybe Core.Natural,
    -- | If you specify a value for @Limit@ and you have more @SizeConstraintSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @SizeConstraintSets@ . For the second and subsequent @ListSizeConstraintSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @SizeConstraintSets@ .
    nextMarker :: Core.Maybe Types.NextMarker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSizeConstraintSets' value with any optional fields omitted.
mkListSizeConstraintSets ::
  ListSizeConstraintSets
mkListSizeConstraintSets =
  ListSizeConstraintSets'
    { limit = Core.Nothing,
      nextMarker = Core.Nothing
    }

-- | Specifies the number of @SizeConstraintSet@ objects that you want AWS WAF to return for this request. If you have more @SizeConstraintSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @SizeConstraintSet@ objects.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscsLimit :: Lens.Lens' ListSizeConstraintSets (Core.Maybe Core.Natural)
lscsLimit = Lens.field @"limit"
{-# DEPRECATED lscsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | If you specify a value for @Limit@ and you have more @SizeConstraintSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @SizeConstraintSets@ . For the second and subsequent @ListSizeConstraintSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @SizeConstraintSets@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscsNextMarker :: Lens.Lens' ListSizeConstraintSets (Core.Maybe Types.NextMarker)
lscsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lscsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

instance Core.FromJSON ListSizeConstraintSets where
  toJSON ListSizeConstraintSets {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("NextMarker" Core..=) Core.<$> nextMarker
          ]
      )

instance Core.AWSRequest ListSizeConstraintSets where
  type Rs ListSizeConstraintSets = ListSizeConstraintSetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSWAF_20150824.ListSizeConstraintSets")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSizeConstraintSetsResponse'
            Core.<$> (x Core..:? "NextMarker")
            Core.<*> (x Core..:? "SizeConstraintSets")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListSizeConstraintSets where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"sizeConstraintSets" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextMarker"
            Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkListSizeConstraintSetsResponse' smart constructor.
data ListSizeConstraintSetsResponse = ListSizeConstraintSetsResponse'
  { -- | If you have more @SizeConstraintSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @SizeConstraintSet@ objects, submit another @ListSizeConstraintSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
    nextMarker :: Core.Maybe Types.NextMarker,
    -- | An array of 'SizeConstraintSetSummary' objects.
    sizeConstraintSets :: Core.Maybe [Types.SizeConstraintSetSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSizeConstraintSetsResponse' value with any optional fields omitted.
mkListSizeConstraintSetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListSizeConstraintSetsResponse
mkListSizeConstraintSetsResponse responseStatus =
  ListSizeConstraintSetsResponse'
    { nextMarker = Core.Nothing,
      sizeConstraintSets = Core.Nothing,
      responseStatus
    }

-- | If you have more @SizeConstraintSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @SizeConstraintSet@ objects, submit another @ListSizeConstraintSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscsrrsNextMarker :: Lens.Lens' ListSizeConstraintSetsResponse (Core.Maybe Types.NextMarker)
lscsrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lscsrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | An array of 'SizeConstraintSetSummary' objects.
--
-- /Note:/ Consider using 'sizeConstraintSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscsrrsSizeConstraintSets :: Lens.Lens' ListSizeConstraintSetsResponse (Core.Maybe [Types.SizeConstraintSetSummary])
lscsrrsSizeConstraintSets = Lens.field @"sizeConstraintSets"
{-# DEPRECATED lscsrrsSizeConstraintSets "Use generic-lens or generic-optics with 'sizeConstraintSets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lscsrrsResponseStatus :: Lens.Lens' ListSizeConstraintSetsResponse Core.Int
lscsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lscsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
