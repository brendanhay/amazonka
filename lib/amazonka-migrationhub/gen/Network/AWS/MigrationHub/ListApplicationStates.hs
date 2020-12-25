{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.ListApplicationStates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the migration statuses for your applications. If you use the optional @ApplicationIds@ parameter, only the migration statuses for those applications will be returned.
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListApplicationStates
  ( -- * Creating a request
    ListApplicationStates (..),
    mkListApplicationStates,

    -- ** Request lenses
    lasApplicationIds,
    lasMaxResults,
    lasNextToken,

    -- * Destructuring the response
    ListApplicationStatesResponse (..),
    mkListApplicationStatesResponse,

    -- ** Response lenses
    lasrrsApplicationStateList,
    lasrrsNextToken,
    lasrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListApplicationStates' smart constructor.
data ListApplicationStates = ListApplicationStates'
  { -- | The configurationIds from the Application Discovery Service that uniquely identifies your applications.
    applicationIds :: Core.Maybe (Core.NonEmpty Types.ApplicationId),
    -- | Maximum number of results to be returned per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
    nextToken :: Core.Maybe Types.Token
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApplicationStates' value with any optional fields omitted.
mkListApplicationStates ::
  ListApplicationStates
mkListApplicationStates =
  ListApplicationStates'
    { applicationIds = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The configurationIds from the Application Discovery Service that uniquely identifies your applications.
--
-- /Note:/ Consider using 'applicationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasApplicationIds :: Lens.Lens' ListApplicationStates (Core.Maybe (Core.NonEmpty Types.ApplicationId))
lasApplicationIds = Lens.field @"applicationIds"
{-# DEPRECATED lasApplicationIds "Use generic-lens or generic-optics with 'applicationIds' instead." #-}

-- | Maximum number of results to be returned per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasMaxResults :: Lens.Lens' ListApplicationStates (Core.Maybe Core.Natural)
lasMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lasMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasNextToken :: Lens.Lens' ListApplicationStates (Core.Maybe Types.Token)
lasNextToken = Lens.field @"nextToken"
{-# DEPRECATED lasNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListApplicationStates where
  toJSON ListApplicationStates {..} =
    Core.object
      ( Core.catMaybes
          [ ("ApplicationIds" Core..=) Core.<$> applicationIds,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListApplicationStates where
  type Rs ListApplicationStates = ListApplicationStatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSMigrationHub.ListApplicationStates")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationStatesResponse'
            Core.<$> (x Core..:? "ApplicationStateList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListApplicationStates where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"applicationStateList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListApplicationStatesResponse' smart constructor.
data ListApplicationStatesResponse = ListApplicationStatesResponse'
  { -- | A list of Applications that exist in Application Discovery Service.
    applicationStateList :: Core.Maybe [Types.ApplicationState],
    -- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListApplicationStatesResponse' value with any optional fields omitted.
mkListApplicationStatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListApplicationStatesResponse
mkListApplicationStatesResponse responseStatus =
  ListApplicationStatesResponse'
    { applicationStateList =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of Applications that exist in Application Discovery Service.
--
-- /Note:/ Consider using 'applicationStateList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrrsApplicationStateList :: Lens.Lens' ListApplicationStatesResponse (Core.Maybe [Types.ApplicationState])
lasrrsApplicationStateList = Lens.field @"applicationStateList"
{-# DEPRECATED lasrrsApplicationStateList "Use generic-lens or generic-optics with 'applicationStateList' instead." #-}

-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrrsNextToken :: Lens.Lens' ListApplicationStatesResponse (Core.Maybe Types.NextToken)
lasrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lasrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrrsResponseStatus :: Lens.Lens' ListApplicationStatesResponse Core.Int
lasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
