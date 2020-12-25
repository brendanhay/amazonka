{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListFunctionDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a Lambda function definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListFunctionDefinitionVersions
  ( -- * Creating a request
    ListFunctionDefinitionVersions (..),
    mkListFunctionDefinitionVersions,

    -- ** Request lenses
    lfdvFunctionDefinitionId,
    lfdvMaxResults,
    lfdvNextToken,

    -- * Destructuring the response
    ListFunctionDefinitionVersionsResponse (..),
    mkListFunctionDefinitionVersionsResponse,

    -- ** Response lenses
    lfdvrrsNextToken,
    lfdvrrsVersions,
    lfdvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFunctionDefinitionVersions' smart constructor.
data ListFunctionDefinitionVersions = ListFunctionDefinitionVersions'
  { -- | The ID of the Lambda function definition.
    functionDefinitionId :: Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFunctionDefinitionVersions' value with any optional fields omitted.
mkListFunctionDefinitionVersions ::
  -- | 'functionDefinitionId'
  Core.Text ->
  ListFunctionDefinitionVersions
mkListFunctionDefinitionVersions functionDefinitionId =
  ListFunctionDefinitionVersions'
    { functionDefinitionId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the Lambda function definition.
--
-- /Note:/ Consider using 'functionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvFunctionDefinitionId :: Lens.Lens' ListFunctionDefinitionVersions Core.Text
lfdvFunctionDefinitionId = Lens.field @"functionDefinitionId"
{-# DEPRECATED lfdvFunctionDefinitionId "Use generic-lens or generic-optics with 'functionDefinitionId' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvMaxResults :: Lens.Lens' ListFunctionDefinitionVersions (Core.Maybe Core.Text)
lfdvMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lfdvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvNextToken :: Lens.Lens' ListFunctionDefinitionVersions (Core.Maybe Core.Text)
lfdvNextToken = Lens.field @"nextToken"
{-# DEPRECATED lfdvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListFunctionDefinitionVersions where
  type
    Rs ListFunctionDefinitionVersions =
      ListFunctionDefinitionVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/functions/"
                Core.<> (Core.toText functionDefinitionId)
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
          ListFunctionDefinitionVersionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Versions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListFunctionDefinitionVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListFunctionDefinitionVersionsResponse' smart constructor.
data ListFunctionDefinitionVersionsResponse = ListFunctionDefinitionVersionsResponse'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about a version.
    versions :: Core.Maybe [Types.VersionInformation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFunctionDefinitionVersionsResponse' value with any optional fields omitted.
mkListFunctionDefinitionVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListFunctionDefinitionVersionsResponse
mkListFunctionDefinitionVersionsResponse responseStatus =
  ListFunctionDefinitionVersionsResponse'
    { nextToken = Core.Nothing,
      versions = Core.Nothing,
      responseStatus
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvrrsNextToken :: Lens.Lens' ListFunctionDefinitionVersionsResponse (Core.Maybe Core.Text)
lfdvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lfdvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvrrsVersions :: Lens.Lens' ListFunctionDefinitionVersionsResponse (Core.Maybe [Types.VersionInformation])
lfdvrrsVersions = Lens.field @"versions"
{-# DEPRECATED lfdvrrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdvrrsResponseStatus :: Lens.Lens' ListFunctionDefinitionVersionsResponse Core.Int
lfdvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lfdvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
