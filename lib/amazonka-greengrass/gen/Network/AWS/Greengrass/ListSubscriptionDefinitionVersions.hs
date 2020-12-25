{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListSubscriptionDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a subscription definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListSubscriptionDefinitionVersions
  ( -- * Creating a request
    ListSubscriptionDefinitionVersions (..),
    mkListSubscriptionDefinitionVersions,

    -- ** Request lenses
    lsdvSubscriptionDefinitionId,
    lsdvMaxResults,
    lsdvNextToken,

    -- * Destructuring the response
    ListSubscriptionDefinitionVersionsResponse (..),
    mkListSubscriptionDefinitionVersionsResponse,

    -- ** Response lenses
    lsdvrrsNextToken,
    lsdvrrsVersions,
    lsdvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSubscriptionDefinitionVersions' smart constructor.
data ListSubscriptionDefinitionVersions = ListSubscriptionDefinitionVersions'
  { -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSubscriptionDefinitionVersions' value with any optional fields omitted.
mkListSubscriptionDefinitionVersions ::
  -- | 'subscriptionDefinitionId'
  Core.Text ->
  ListSubscriptionDefinitionVersions
mkListSubscriptionDefinitionVersions subscriptionDefinitionId =
  ListSubscriptionDefinitionVersions'
    { subscriptionDefinitionId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the subscription definition.
--
-- /Note:/ Consider using 'subscriptionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvSubscriptionDefinitionId :: Lens.Lens' ListSubscriptionDefinitionVersions Core.Text
lsdvSubscriptionDefinitionId = Lens.field @"subscriptionDefinitionId"
{-# DEPRECATED lsdvSubscriptionDefinitionId "Use generic-lens or generic-optics with 'subscriptionDefinitionId' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvMaxResults :: Lens.Lens' ListSubscriptionDefinitionVersions (Core.Maybe Core.Text)
lsdvMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lsdvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvNextToken :: Lens.Lens' ListSubscriptionDefinitionVersions (Core.Maybe Core.Text)
lsdvNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsdvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListSubscriptionDefinitionVersions where
  type
    Rs ListSubscriptionDefinitionVersions =
      ListSubscriptionDefinitionVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/subscriptions/"
                Core.<> (Core.toText subscriptionDefinitionId)
                Core.<> ("/versions")
            ),
        Core._rqQuery =
          Core.toQueryValue "MaxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSubscriptionDefinitionVersionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Versions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListSubscriptionDefinitionVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListSubscriptionDefinitionVersionsResponse' smart constructor.
data ListSubscriptionDefinitionVersionsResponse = ListSubscriptionDefinitionVersionsResponse'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about a version.
    versions :: Core.Maybe [Types.VersionInformation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSubscriptionDefinitionVersionsResponse' value with any optional fields omitted.
mkListSubscriptionDefinitionVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListSubscriptionDefinitionVersionsResponse
mkListSubscriptionDefinitionVersionsResponse responseStatus =
  ListSubscriptionDefinitionVersionsResponse'
    { nextToken =
        Core.Nothing,
      versions = Core.Nothing,
      responseStatus
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvrrsNextToken :: Lens.Lens' ListSubscriptionDefinitionVersionsResponse (Core.Maybe Core.Text)
lsdvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsdvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvrrsVersions :: Lens.Lens' ListSubscriptionDefinitionVersionsResponse (Core.Maybe [Types.VersionInformation])
lsdvrrsVersions = Lens.field @"versions"
{-# DEPRECATED lsdvrrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvrrsResponseStatus :: Lens.Lens' ListSubscriptionDefinitionVersionsResponse Core.Int
lsdvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsdvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
