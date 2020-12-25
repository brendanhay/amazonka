{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListPrompts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the prompts for the specified Amazon Connect instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListPrompts
  ( -- * Creating a request
    ListPrompts (..),
    mkListPrompts,

    -- ** Request lenses
    lpInstanceId,
    lpMaxResults,
    lpNextToken,

    -- * Destructuring the response
    ListPromptsResponse (..),
    mkListPromptsResponse,

    -- ** Response lenses
    lprrsNextToken,
    lprrsPromptSummaryList,
    lprrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPrompts' smart constructor.
data ListPrompts = ListPrompts'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPrompts' value with any optional fields omitted.
mkListPrompts ::
  -- | 'instanceId'
  Types.InstanceId ->
  ListPrompts
mkListPrompts instanceId =
  ListPrompts'
    { instanceId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpInstanceId :: Lens.Lens' ListPrompts Types.InstanceId
lpInstanceId = Lens.field @"instanceId"
{-# DEPRECATED lpInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The maximum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxResults :: Lens.Lens' ListPrompts (Core.Maybe Core.Natural)
lpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListPrompts (Core.Maybe Types.NextToken)
lpNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListPrompts where
  type Rs ListPrompts = ListPromptsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/prompts-summary/" Core.<> (Core.toText instanceId)),
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
          ListPromptsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "PromptSummaryList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListPrompts where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"promptSummaryList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListPromptsResponse' smart constructor.
data ListPromptsResponse = ListPromptsResponse'
  { -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the prompts.
    promptSummaryList :: Core.Maybe [Types.PromptSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPromptsResponse' value with any optional fields omitted.
mkListPromptsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPromptsResponse
mkListPromptsResponse responseStatus =
  ListPromptsResponse'
    { nextToken = Core.Nothing,
      promptSummaryList = Core.Nothing,
      responseStatus
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextToken :: Lens.Lens' ListPromptsResponse (Core.Maybe Types.NextToken)
lprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the prompts.
--
-- /Note:/ Consider using 'promptSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsPromptSummaryList :: Lens.Lens' ListPromptsResponse (Core.Maybe [Types.PromptSummary])
lprrsPromptSummaryList = Lens.field @"promptSummaryList"
{-# DEPRECATED lprrsPromptSummaryList "Use generic-lens or generic-optics with 'promptSummaryList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListPromptsResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
