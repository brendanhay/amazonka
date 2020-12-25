{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns bot information as follows:
--
--
--     * If you provide the @nameContains@ field, the response includes information for the @> LATEST@ version of all bots whose name contains the specified string.
--
--
--     * If you don't specify the @nameContains@ field, the operation returns information about the @> LATEST@ version of all of your bots.
--
--
-- This operation requires permission for the @lex:GetBots@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBots
  ( -- * Creating a request
    GetBots (..),
    mkGetBots,

    -- ** Request lenses
    gbMaxResults,
    gbNameContains,
    gbNextToken,

    -- * Destructuring the response
    GetBotsResponse (..),
    mkGetBotsResponse,

    -- ** Response lenses
    gbrfrsBots,
    gbrfrsNextToken,
    gbrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBots' smart constructor.
data GetBots = GetBots'
  { -- | The maximum number of bots to return in the response that the request will return. The default is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | Substring to match in bot names. A bot will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
    nameContains :: Core.Maybe Types.NameContains,
    -- | A pagination token that fetches the next page of bots. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of bots, specify the pagination token in the next request.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBots' value with any optional fields omitted.
mkGetBots ::
  GetBots
mkGetBots =
  GetBots'
    { maxResults = Core.Nothing,
      nameContains = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of bots to return in the response that the request will return. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbMaxResults :: Lens.Lens' GetBots (Core.Maybe Core.Natural)
gbMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Substring to match in bot names. A bot will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbNameContains :: Lens.Lens' GetBots (Core.Maybe Types.NameContains)
gbNameContains = Lens.field @"nameContains"
{-# DEPRECATED gbNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A pagination token that fetches the next page of bots. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of bots, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbNextToken :: Lens.Lens' GetBots (Core.Maybe Types.NextToken)
gbNextToken = Lens.field @"nextToken"
{-# DEPRECATED gbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetBots where
  type Rs GetBots = GetBotsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/bots/",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nameContains" Core.<$> nameContains)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotsResponse'
            Core.<$> (x Core..:? "bots")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetBots where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"bots" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetBotsResponse' smart constructor.
data GetBotsResponse = GetBotsResponse'
  { -- | An array of @botMetadata@ objects, with one entry for each bot.
    bots :: Core.Maybe [Types.BotMetadata],
    -- | If the response is truncated, it includes a pagination token that you can specify in your next request to fetch the next page of bots.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetBotsResponse' value with any optional fields omitted.
mkGetBotsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBotsResponse
mkGetBotsResponse responseStatus =
  GetBotsResponse'
    { bots = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of @botMetadata@ objects, with one entry for each bot.
--
-- /Note:/ Consider using 'bots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrfrsBots :: Lens.Lens' GetBotsResponse (Core.Maybe [Types.BotMetadata])
gbrfrsBots = Lens.field @"bots"
{-# DEPRECATED gbrfrsBots "Use generic-lens or generic-optics with 'bots' instead." #-}

-- | If the response is truncated, it includes a pagination token that you can specify in your next request to fetch the next page of bots.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrfrsNextToken :: Lens.Lens' GetBotsResponse (Core.Maybe Types.NextToken)
gbrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gbrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbrfrsResponseStatus :: Lens.Lens' GetBotsResponse Core.Int
gbrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
