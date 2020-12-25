{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBotVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all of the versions of a bot.
--
-- The @GetBotVersions@ operation returns a @BotMetadata@ object for each version of a bot. For example, if a bot has three numbered versions, the @GetBotVersions@ operation returns four @BotMetadata@ objects in the response, one for each numbered version and one for the @> LATEST@ version.
-- The @GetBotVersions@ operation always returns at least one version, the @> LATEST@ version.
-- This operation requires permissions for the @lex:GetBotVersions@ action.
--
-- This operation returns paginated results.
module Network.AWS.LexModels.GetBotVersions
  ( -- * Creating a request
    GetBotVersions (..),
    mkGetBotVersions,

    -- ** Request lenses
    gbvName,
    gbvMaxResults,
    gbvNextToken,

    -- * Destructuring the response
    GetBotVersionsResponse (..),
    mkGetBotVersionsResponse,

    -- ** Response lenses
    gbvrrsBots,
    gbvrrsNextToken,
    gbvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBotVersions' smart constructor.
data GetBotVersions = GetBotVersions'
  { -- | The name of the bot for which versions should be returned.
    name :: Types.BotName,
    -- | The maximum number of bot versions to return in the response. The default is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A pagination token for fetching the next page of bot versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBotVersions' value with any optional fields omitted.
mkGetBotVersions ::
  -- | 'name'
  Types.BotName ->
  GetBotVersions
mkGetBotVersions name =
  GetBotVersions'
    { name,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the bot for which versions should be returned.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvName :: Lens.Lens' GetBotVersions Types.BotName
gbvName = Lens.field @"name"
{-# DEPRECATED gbvName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The maximum number of bot versions to return in the response. The default is 10.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvMaxResults :: Lens.Lens' GetBotVersions (Core.Maybe Core.Natural)
gbvMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gbvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A pagination token for fetching the next page of bot versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvNextToken :: Lens.Lens' GetBotVersions (Core.Maybe Types.NextToken)
gbvNextToken = Lens.field @"nextToken"
{-# DEPRECATED gbvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetBotVersions where
  type Rs GetBotVersions = GetBotVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/bots/" Core.<> (Core.toText name) Core.<> ("/versions/")),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotVersionsResponse'
            Core.<$> (x Core..:? "bots")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetBotVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"bots" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetBotVersionsResponse' smart constructor.
data GetBotVersionsResponse = GetBotVersionsResponse'
  { -- | An array of @BotMetadata@ objects, one for each numbered version of the bot plus one for the @> LATEST@ version.
    bots :: Core.Maybe [Types.BotMetadata],
    -- | A pagination token for fetching the next page of bot versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetBotVersionsResponse' value with any optional fields omitted.
mkGetBotVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBotVersionsResponse
mkGetBotVersionsResponse responseStatus =
  GetBotVersionsResponse'
    { bots = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of @BotMetadata@ objects, one for each numbered version of the bot plus one for the @> LATEST@ version.
--
-- /Note:/ Consider using 'bots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvrrsBots :: Lens.Lens' GetBotVersionsResponse (Core.Maybe [Types.BotMetadata])
gbvrrsBots = Lens.field @"bots"
{-# DEPRECATED gbvrrsBots "Use generic-lens or generic-optics with 'bots' instead." #-}

-- | A pagination token for fetching the next page of bot versions. If the response to this call is truncated, Amazon Lex returns a pagination token in the response. To fetch the next page of versions, specify the pagination token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvrrsNextToken :: Lens.Lens' GetBotVersionsResponse (Core.Maybe Types.NextToken)
gbvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gbvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbvrrsResponseStatus :: Lens.Lens' GetBotVersionsResponse Core.Int
gbvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
