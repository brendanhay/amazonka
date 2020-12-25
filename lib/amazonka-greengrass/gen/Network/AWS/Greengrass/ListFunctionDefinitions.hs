{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListFunctionDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of Lambda function definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListFunctionDefinitions
  ( -- * Creating a request
    ListFunctionDefinitions (..),
    mkListFunctionDefinitions,

    -- ** Request lenses
    lfdMaxResults,
    lfdNextToken,

    -- * Destructuring the response
    ListFunctionDefinitionsResponse (..),
    mkListFunctionDefinitionsResponse,

    -- ** Response lenses
    lfdrrsDefinitions,
    lfdrrsNextToken,
    lfdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFunctionDefinitions' smart constructor.
data ListFunctionDefinitions = ListFunctionDefinitions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFunctionDefinitions' value with any optional fields omitted.
mkListFunctionDefinitions ::
  ListFunctionDefinitions
mkListFunctionDefinitions =
  ListFunctionDefinitions'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdMaxResults :: Lens.Lens' ListFunctionDefinitions (Core.Maybe Core.Text)
lfdMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lfdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdNextToken :: Lens.Lens' ListFunctionDefinitions (Core.Maybe Core.Text)
lfdNextToken = Lens.field @"nextToken"
{-# DEPRECATED lfdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListFunctionDefinitions where
  type Rs ListFunctionDefinitions = ListFunctionDefinitionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/greengrass/definition/functions",
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
          ListFunctionDefinitionsResponse'
            Core.<$> (x Core..:? "Definitions")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListFunctionDefinitions where
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

-- | /See:/ 'mkListFunctionDefinitionsResponse' smart constructor.
data ListFunctionDefinitionsResponse = ListFunctionDefinitionsResponse'
  { -- | Information about a definition.
    definitions :: Core.Maybe [Types.DefinitionInformation],
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFunctionDefinitionsResponse' value with any optional fields omitted.
mkListFunctionDefinitionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListFunctionDefinitionsResponse
mkListFunctionDefinitionsResponse responseStatus =
  ListFunctionDefinitionsResponse'
    { definitions = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about a definition.
--
-- /Note:/ Consider using 'definitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdrrsDefinitions :: Lens.Lens' ListFunctionDefinitionsResponse (Core.Maybe [Types.DefinitionInformation])
lfdrrsDefinitions = Lens.field @"definitions"
{-# DEPRECATED lfdrrsDefinitions "Use generic-lens or generic-optics with 'definitions' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdrrsNextToken :: Lens.Lens' ListFunctionDefinitionsResponse (Core.Maybe Core.Text)
lfdrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lfdrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdrrsResponseStatus :: Lens.Lens' ListFunctionDefinitionsResponse Core.Int
lfdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lfdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
