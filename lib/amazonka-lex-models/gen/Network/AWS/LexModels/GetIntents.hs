{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetIntents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns intent information as follows:
--
--
--     * If you specify the @nameContains@ field, returns the @> LATEST@ version of all intents that contain the specified string.
--
--
--     * If you don't specify the @nameContains@ field, returns information about the @> LATEST@ version of all intents.
--
--
-- The operation requires permission for the @lex:GetIntents@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetIntents
  ( -- * Creating a request
    GetIntents (..),
    mkGetIntents,

    -- ** Request lenses
    giMaxResults,
    giNameContains,
    giNextToken,

    -- * Destructuring the response
    GetIntentsResponse (..),
    mkGetIntentsResponse,

    -- ** Response lenses
    girgrsIntents,
    girgrsNextToken,
    girgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetIntents' smart constructor.
data GetIntents = GetIntents'
  { -- | The maximum number of intents to return in the response. The default is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | Substring to match in intent names. An intent will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
    nameContains :: Core.Maybe Types.IntentName,
    -- | A pagination token that fetches the next page of intents. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, specify the pagination token in the next request.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIntents' value with any optional fields omitted.
mkGetIntents ::
  GetIntents
mkGetIntents =
  GetIntents'
    { maxResults = Core.Nothing,
      nameContains = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of intents to return in the response. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giMaxResults :: Lens.Lens' GetIntents (Core.Maybe Core.Natural)
giMaxResults = Lens.field @"maxResults"
{-# DEPRECATED giMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Substring to match in intent names. An intent will be returned if any part of its name matches the substring. For example, "xyz" matches both "xyzabc" and "abcxyz."
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giNameContains :: Lens.Lens' GetIntents (Core.Maybe Types.IntentName)
giNameContains = Lens.field @"nameContains"
{-# DEPRECATED giNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | A pagination token that fetches the next page of intents. If the response to this API call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of intents, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giNextToken :: Lens.Lens' GetIntents (Core.Maybe Types.NextToken)
giNextToken = Lens.field @"nextToken"
{-# DEPRECATED giNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetIntents where
  type Rs GetIntents = GetIntentsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/intents/",
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
          GetIntentsResponse'
            Core.<$> (x Core..:? "intents")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetIntents where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"intents" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetIntentsResponse' smart constructor.
data GetIntentsResponse = GetIntentsResponse'
  { -- | An array of @Intent@ objects. For more information, see 'PutBot' .
    intents :: Core.Maybe [Types.IntentMetadata],
    -- | If the response is truncated, the response includes a pagination token that you can specify in your next request to fetch the next page of intents.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetIntentsResponse' value with any optional fields omitted.
mkGetIntentsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetIntentsResponse
mkGetIntentsResponse responseStatus =
  GetIntentsResponse'
    { intents = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of @Intent@ objects. For more information, see 'PutBot' .
--
-- /Note:/ Consider using 'intents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girgrsIntents :: Lens.Lens' GetIntentsResponse (Core.Maybe [Types.IntentMetadata])
girgrsIntents = Lens.field @"intents"
{-# DEPRECATED girgrsIntents "Use generic-lens or generic-optics with 'intents' instead." #-}

-- | If the response is truncated, the response includes a pagination token that you can specify in your next request to fetch the next page of intents.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girgrsNextToken :: Lens.Lens' GetIntentsResponse (Core.Maybe Types.NextToken)
girgrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED girgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girgrsResponseStatus :: Lens.Lens' GetIntentsResponse Core.Int
girgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED girgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
