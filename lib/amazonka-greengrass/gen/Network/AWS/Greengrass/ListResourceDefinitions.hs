{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListResourceDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of resource definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListResourceDefinitions
  ( -- * Creating a request
    ListResourceDefinitions (..),
    mkListResourceDefinitions,

    -- ** Request lenses
    lrdMaxResults,
    lrdNextToken,

    -- * Destructuring the response
    ListResourceDefinitionsResponse (..),
    mkListResourceDefinitionsResponse,

    -- ** Response lenses
    lrdrrsDefinitions,
    lrdrrsNextToken,
    lrdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListResourceDefinitions' smart constructor.
data ListResourceDefinitions = ListResourceDefinitions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceDefinitions' value with any optional fields omitted.
mkListResourceDefinitions ::
  ListResourceDefinitions
mkListResourceDefinitions =
  ListResourceDefinitions'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdMaxResults :: Lens.Lens' ListResourceDefinitions (Core.Maybe Core.Text)
lrdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lrdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdNextToken :: Lens.Lens' ListResourceDefinitions (Core.Maybe Core.Text)
lrdNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListResourceDefinitions where
  type Rs ListResourceDefinitions = ListResourceDefinitionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/greengrass/definition/resources",
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
          ListResourceDefinitionsResponse'
            Core.<$> (x Core..:? "Definitions")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListResourceDefinitions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"definitions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListResourceDefinitionsResponse' smart constructor.
data ListResourceDefinitionsResponse = ListResourceDefinitionsResponse'
  { -- | Information about a definition.
    definitions :: Core.Maybe [Types.DefinitionInformation],
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceDefinitionsResponse' value with any optional fields omitted.
mkListResourceDefinitionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListResourceDefinitionsResponse
mkListResourceDefinitionsResponse responseStatus =
  ListResourceDefinitionsResponse'
    { definitions = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about a definition.
--
-- /Note:/ Consider using 'definitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdrrsDefinitions :: Lens.Lens' ListResourceDefinitionsResponse (Core.Maybe [Types.DefinitionInformation])
lrdrrsDefinitions = Lens.field @"definitions"
{-# DEPRECATED lrdrrsDefinitions "Use generic-lens or generic-optics with 'definitions' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdrrsNextToken :: Lens.Lens' ListResourceDefinitionsResponse (Core.Maybe Core.Text)
lrdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdrrsResponseStatus :: Lens.Lens' ListResourceDefinitionsResponse Core.Int
lrdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
