{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.ListRateBasedRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'RuleSummary' objects.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListRateBasedRules
  ( -- * Creating a request
    ListRateBasedRules (..),
    mkListRateBasedRules,

    -- ** Request lenses
    lrbrLimit,
    lrbrNextMarker,

    -- * Destructuring the response
    ListRateBasedRulesResponse (..),
    mkListRateBasedRulesResponse,

    -- ** Response lenses
    lrbrrrsNextMarker,
    lrbrrrsRules,
    lrbrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkListRateBasedRules' smart constructor.
data ListRateBasedRules = ListRateBasedRules'
  { -- | Specifies the number of @Rules@ that you want AWS WAF to return for this request. If you have more @Rules@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
    limit :: Core.Maybe Core.Natural,
    -- | If you specify a value for @Limit@ and you have more @Rules@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @Rules@ . For the second and subsequent @ListRateBasedRules@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @Rules@ .
    nextMarker :: Core.Maybe Types.NextMarker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRateBasedRules' value with any optional fields omitted.
mkListRateBasedRules ::
  ListRateBasedRules
mkListRateBasedRules =
  ListRateBasedRules'
    { limit = Core.Nothing,
      nextMarker = Core.Nothing
    }

-- | Specifies the number of @Rules@ that you want AWS WAF to return for this request. If you have more @Rules@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbrLimit :: Lens.Lens' ListRateBasedRules (Core.Maybe Core.Natural)
lrbrLimit = Lens.field @"limit"
{-# DEPRECATED lrbrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | If you specify a value for @Limit@ and you have more @Rules@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @Rules@ . For the second and subsequent @ListRateBasedRules@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @Rules@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbrNextMarker :: Lens.Lens' ListRateBasedRules (Core.Maybe Types.NextMarker)
lrbrNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lrbrNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

instance Core.FromJSON ListRateBasedRules where
  toJSON ListRateBasedRules {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("NextMarker" Core..=) Core.<$> nextMarker
          ]
      )

instance Core.AWSRequest ListRateBasedRules where
  type Rs ListRateBasedRules = ListRateBasedRulesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSWAF_20150824.ListRateBasedRules")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRateBasedRulesResponse'
            Core.<$> (x Core..:? "NextMarker")
            Core.<*> (x Core..:? "Rules")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListRateBasedRules where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"rules" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextMarker"
            Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkListRateBasedRulesResponse' smart constructor.
data ListRateBasedRulesResponse = ListRateBasedRulesResponse'
  { -- | If you have more @Rules@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @Rules@ , submit another @ListRateBasedRules@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
    nextMarker :: Core.Maybe Types.NextMarker,
    -- | An array of 'RuleSummary' objects.
    rules :: Core.Maybe [Types.RuleSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRateBasedRulesResponse' value with any optional fields omitted.
mkListRateBasedRulesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListRateBasedRulesResponse
mkListRateBasedRulesResponse responseStatus =
  ListRateBasedRulesResponse'
    { nextMarker = Core.Nothing,
      rules = Core.Nothing,
      responseStatus
    }

-- | If you have more @Rules@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @Rules@ , submit another @ListRateBasedRules@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbrrrsNextMarker :: Lens.Lens' ListRateBasedRulesResponse (Core.Maybe Types.NextMarker)
lrbrrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED lrbrrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | An array of 'RuleSummary' objects.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbrrrsRules :: Lens.Lens' ListRateBasedRulesResponse (Core.Maybe [Types.RuleSummary])
lrbrrrsRules = Lens.field @"rules"
{-# DEPRECATED lrbrrrsRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrbrrrsResponseStatus :: Lens.Lens' ListRateBasedRulesResponse Core.Int
lrbrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrbrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
