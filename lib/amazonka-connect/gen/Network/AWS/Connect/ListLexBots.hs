{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListLexBots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all the Amazon Lex bots currently associated with the instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListLexBots
  ( -- * Creating a request
    ListLexBots (..),
    mkListLexBots,

    -- ** Request lenses
    llbInstanceId,
    llbMaxResults,
    llbNextToken,

    -- * Destructuring the response
    ListLexBotsResponse (..),
    mkListLexBotsResponse,

    -- ** Response lenses
    llbrrsLexBots,
    llbrrsNextToken,
    llbrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListLexBots' smart constructor.
data ListLexBots = ListLexBots'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The maximimum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLexBots' value with any optional fields omitted.
mkListLexBots ::
  -- | 'instanceId'
  Types.InstanceId ->
  ListLexBots
mkListLexBots instanceId =
  ListLexBots'
    { instanceId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbInstanceId :: Lens.Lens' ListLexBots Types.InstanceId
llbInstanceId = Lens.field @"instanceId"
{-# DEPRECATED llbInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbMaxResults :: Lens.Lens' ListLexBots (Core.Maybe Core.Natural)
llbMaxResults = Lens.field @"maxResults"
{-# DEPRECATED llbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbNextToken :: Lens.Lens' ListLexBots (Core.Maybe Types.NextToken)
llbNextToken = Lens.field @"nextToken"
{-# DEPRECATED llbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListLexBots where
  type Rs ListLexBots = ListLexBotsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/instance/" Core.<> (Core.toText instanceId)
                Core.<> ("/lex-bots")
            ),
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
          ListLexBotsResponse'
            Core.<$> (x Core..:? "LexBots")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListLexBots where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"lexBots" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListLexBotsResponse' smart constructor.
data ListLexBotsResponse = ListLexBotsResponse'
  { -- | The the names and regions of the Amazon Lex bots associated with the specified instance.
    lexBots :: Core.Maybe [Types.LexBot],
    -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLexBotsResponse' value with any optional fields omitted.
mkListLexBotsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListLexBotsResponse
mkListLexBotsResponse responseStatus =
  ListLexBotsResponse'
    { lexBots = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The the names and regions of the Amazon Lex bots associated with the specified instance.
--
-- /Note:/ Consider using 'lexBots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbrrsLexBots :: Lens.Lens' ListLexBotsResponse (Core.Maybe [Types.LexBot])
llbrrsLexBots = Lens.field @"lexBots"
{-# DEPRECATED llbrrsLexBots "Use generic-lens or generic-optics with 'lexBots' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbrrsNextToken :: Lens.Lens' ListLexBotsResponse (Core.Maybe Types.NextToken)
llbrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED llbrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llbrrsResponseStatus :: Lens.Lens' ListLexBotsResponse Core.Int
llbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED llbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
