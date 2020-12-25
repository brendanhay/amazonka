{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListIPSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'IPSetSummary' objects in the response.
module Network.AWS.WAFRegional.ListIPSets
  ( -- * Creating a request
    ListIPSets (..),
    mkListIPSets,

    -- ** Request lenses
    lipsLimit,
    lipsNextMarker,

    -- * Destructuring the response
    ListIPSetsResponse (..),
    mkListIPSetsResponse,

    -- ** Response lenses
    lipsrrsIPSets,
    lipsrrsNextMarker,
    lipsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkListIPSets' smart constructor.
data ListIPSets = ListIPSets'
  { -- | Specifies the number of @IPSet@ objects that you want AWS WAF to return for this request. If you have more @IPSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @IPSet@ objects.
    limit :: Core.Maybe Core.Natural,
    -- | AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @IPSets@ . For the second and subsequent @ListIPSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @IPSets@ .
    nextMarker :: Core.Maybe Types.NextMarker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListIPSets' value with any optional fields omitted.
mkListIPSets ::
  ListIPSets
mkListIPSets =
  ListIPSets' {limit = Core.Nothing, nextMarker = Core.Nothing}

-- | Specifies the number of @IPSet@ objects that you want AWS WAF to return for this request. If you have more @IPSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @IPSet@ objects.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsLimit :: Lens.Lens' ListIPSets (Core.Maybe Core.Natural)
lipsLimit = Lens.field @"limit"
{-# DEPRECATED lipsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @IPSets@ . For the second and subsequent @ListIPSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @IPSets@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsNextMarker :: Lens.Lens' ListIPSets (Core.Maybe Types.NextMarker)
lipsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lipsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

instance Core.FromJSON ListIPSets where
  toJSON ListIPSets {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("NextMarker" Core..=) Core.<$> nextMarker
          ]
      )

instance Core.AWSRequest ListIPSets where
  type Rs ListIPSets = ListIPSetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_Regional_20161128.ListIPSets")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIPSetsResponse'
            Core.<$> (x Core..:? "IPSets")
            Core.<*> (x Core..:? "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListIPSetsResponse' smart constructor.
data ListIPSetsResponse = ListIPSetsResponse'
  { -- | An array of 'IPSetSummary' objects.
    iPSets :: Core.Maybe [Types.IPSetSummary],
    -- | To list more @IPSet@ objects, submit another @ListIPSets@ request, and in the next request use the @NextMarker@ response value as the @NextMarker@ value.
    nextMarker :: Core.Maybe Types.NextMarker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListIPSetsResponse' value with any optional fields omitted.
mkListIPSetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListIPSetsResponse
mkListIPSetsResponse responseStatus =
  ListIPSetsResponse'
    { iPSets = Core.Nothing,
      nextMarker = Core.Nothing,
      responseStatus
    }

-- | An array of 'IPSetSummary' objects.
--
-- /Note:/ Consider using 'iPSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsrrsIPSets :: Lens.Lens' ListIPSetsResponse (Core.Maybe [Types.IPSetSummary])
lipsrrsIPSets = Lens.field @"iPSets"
{-# DEPRECATED lipsrrsIPSets "Use generic-lens or generic-optics with 'iPSets' instead." #-}

-- | To list more @IPSet@ objects, submit another @ListIPSets@ request, and in the next request use the @NextMarker@ response value as the @NextMarker@ value.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsrrsNextMarker :: Lens.Lens' ListIPSetsResponse (Core.Maybe Types.NextMarker)
lipsrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lipsrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsrrsResponseStatus :: Lens.Lens' ListIPSetsResponse Core.Int
lipsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lipsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
