{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListSubscribedWorkteams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the work teams that you are subscribed to in the AWS Marketplace. The list may be empty if no work team satisfies the filter specified in the @NameContains@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListSubscribedWorkteams
  ( -- * Creating a request
    ListSubscribedWorkteams (..),
    mkListSubscribedWorkteams,

    -- ** Request lenses
    lswMaxResults,
    lswNameContains,
    lswNextToken,

    -- * Destructuring the response
    ListSubscribedWorkteamsResponse (..),
    mkListSubscribedWorkteamsResponse,

    -- ** Response lenses
    lswrrsSubscribedWorkteams,
    lswrrsNextToken,
    lswrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListSubscribedWorkteams' smart constructor.
data ListSubscribedWorkteams = ListSubscribedWorkteams'
  { -- | The maximum number of work teams to return in each page of the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A string in the work team name. This filter returns only work teams whose name contains the specified string.
    nameContains :: Core.Maybe Types.NameContains,
    -- | If the result of the previous @ListSubscribedWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSubscribedWorkteams' value with any optional fields omitted.
mkListSubscribedWorkteams ::
  ListSubscribedWorkteams
mkListSubscribedWorkteams =
  ListSubscribedWorkteams'
    { maxResults = Core.Nothing,
      nameContains = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of work teams to return in each page of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lswMaxResults :: Lens.Lens' ListSubscribedWorkteams (Core.Maybe Core.Natural)
lswMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lswMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A string in the work team name. This filter returns only work teams whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lswNameContains :: Lens.Lens' ListSubscribedWorkteams (Core.Maybe Types.NameContains)
lswNameContains = Lens.field @"nameContains"
{-# DEPRECATED lswNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of the previous @ListSubscribedWorkteams@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of labeling jobs, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lswNextToken :: Lens.Lens' ListSubscribedWorkteams (Core.Maybe Types.NextToken)
lswNextToken = Lens.field @"nextToken"
{-# DEPRECATED lswNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListSubscribedWorkteams where
  toJSON ListSubscribedWorkteams {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListSubscribedWorkteams where
  type Rs ListSubscribedWorkteams = ListSubscribedWorkteamsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListSubscribedWorkteams")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSubscribedWorkteamsResponse'
            Core.<$> (x Core..:? "SubscribedWorkteams" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListSubscribedWorkteams where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"subscribedWorkteams") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListSubscribedWorkteamsResponse' smart constructor.
data ListSubscribedWorkteamsResponse = ListSubscribedWorkteamsResponse'
  { -- | An array of @Workteam@ objects, each describing a work team.
    subscribedWorkteams :: [Types.SubscribedWorkteam],
    -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSubscribedWorkteamsResponse' value with any optional fields omitted.
mkListSubscribedWorkteamsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListSubscribedWorkteamsResponse
mkListSubscribedWorkteamsResponse responseStatus =
  ListSubscribedWorkteamsResponse'
    { subscribedWorkteams =
        Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of @Workteam@ objects, each describing a work team.
--
-- /Note:/ Consider using 'subscribedWorkteams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lswrrsSubscribedWorkteams :: Lens.Lens' ListSubscribedWorkteamsResponse [Types.SubscribedWorkteam]
lswrrsSubscribedWorkteams = Lens.field @"subscribedWorkteams"
{-# DEPRECATED lswrrsSubscribedWorkteams "Use generic-lens or generic-optics with 'subscribedWorkteams' instead." #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of work teams, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lswrrsNextToken :: Lens.Lens' ListSubscribedWorkteamsResponse (Core.Maybe Types.NextToken)
lswrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lswrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lswrrsResponseStatus :: Lens.Lens' ListSubscribedWorkteamsResponse Core.Int
lswrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lswrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
