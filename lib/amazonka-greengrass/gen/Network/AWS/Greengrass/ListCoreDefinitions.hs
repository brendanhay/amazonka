{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListCoreDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of core definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListCoreDefinitions
  ( -- * Creating a request
    ListCoreDefinitions (..),
    mkListCoreDefinitions,

    -- ** Request lenses
    lcdMaxResults,
    lcdNextToken,

    -- * Destructuring the response
    ListCoreDefinitionsResponse (..),
    mkListCoreDefinitionsResponse,

    -- ** Response lenses
    lcdrrsDefinitions,
    lcdrrsNextToken,
    lcdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCoreDefinitions' smart constructor.
data ListCoreDefinitions = ListCoreDefinitions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCoreDefinitions' value with any optional fields omitted.
mkListCoreDefinitions ::
  ListCoreDefinitions
mkListCoreDefinitions =
  ListCoreDefinitions'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdMaxResults :: Lens.Lens' ListCoreDefinitions (Core.Maybe Core.Text)
lcdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lcdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdNextToken :: Lens.Lens' ListCoreDefinitions (Core.Maybe Core.Text)
lcdNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListCoreDefinitions where
  type Rs ListCoreDefinitions = ListCoreDefinitionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/greengrass/definition/cores",
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
          ListCoreDefinitionsResponse'
            Core.<$> (x Core..:? "Definitions")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListCoreDefinitions where
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

-- | /See:/ 'mkListCoreDefinitionsResponse' smart constructor.
data ListCoreDefinitionsResponse = ListCoreDefinitionsResponse'
  { -- | Information about a definition.
    definitions :: Core.Maybe [Types.DefinitionInformation],
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCoreDefinitionsResponse' value with any optional fields omitted.
mkListCoreDefinitionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCoreDefinitionsResponse
mkListCoreDefinitionsResponse responseStatus =
  ListCoreDefinitionsResponse'
    { definitions = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about a definition.
--
-- /Note:/ Consider using 'definitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdrrsDefinitions :: Lens.Lens' ListCoreDefinitionsResponse (Core.Maybe [Types.DefinitionInformation])
lcdrrsDefinitions = Lens.field @"definitions"
{-# DEPRECATED lcdrrsDefinitions "Use generic-lens or generic-optics with 'definitions' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdrrsNextToken :: Lens.Lens' ListCoreDefinitionsResponse (Core.Maybe Core.Text)
lcdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdrrsResponseStatus :: Lens.Lens' ListCoreDefinitionsResponse Core.Int
lcdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
