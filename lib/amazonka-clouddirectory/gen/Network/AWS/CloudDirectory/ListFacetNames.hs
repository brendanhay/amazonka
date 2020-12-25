{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListFacetNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of facets that exist in a schema.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListFacetNames
  ( -- * Creating a request
    ListFacetNames (..),
    mkListFacetNames,

    -- ** Request lenses
    lfnSchemaArn,
    lfnMaxResults,
    lfnNextToken,

    -- * Destructuring the response
    ListFacetNamesResponse (..),
    mkListFacetNamesResponse,

    -- ** Response lenses
    lfnrrsFacetNames,
    lfnrrsNextToken,
    lfnrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFacetNames' smart constructor.
data ListFacetNames = ListFacetNames'
  { -- | The Amazon Resource Name (ARN) to retrieve facet names from.
    schemaArn :: Types.Arn,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFacetNames' value with any optional fields omitted.
mkListFacetNames ::
  -- | 'schemaArn'
  Types.Arn ->
  ListFacetNames
mkListFacetNames schemaArn =
  ListFacetNames'
    { schemaArn,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) to retrieve facet names from.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfnSchemaArn :: Lens.Lens' ListFacetNames Types.Arn
lfnSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED lfnSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfnMaxResults :: Lens.Lens' ListFacetNames (Core.Maybe Core.Natural)
lfnMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lfnMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfnNextToken :: Lens.Lens' ListFacetNames (Core.Maybe Types.NextToken)
lfnNextToken = Lens.field @"nextToken"
{-# DEPRECATED lfnNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListFacetNames where
  toJSON ListFacetNames {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListFacetNames where
  type Rs ListFacetNames = ListFacetNamesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/facet/list",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "x-amz-data-partition" schemaArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFacetNamesResponse'
            Core.<$> (x Core..:? "FacetNames")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListFacetNames where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"facetNames" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListFacetNamesResponse' smart constructor.
data ListFacetNamesResponse = ListFacetNamesResponse'
  { -- | The names of facets that exist within the schema.
    facetNames :: Core.Maybe [Types.FacetName],
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFacetNamesResponse' value with any optional fields omitted.
mkListFacetNamesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListFacetNamesResponse
mkListFacetNamesResponse responseStatus =
  ListFacetNamesResponse'
    { facetNames = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The names of facets that exist within the schema.
--
-- /Note:/ Consider using 'facetNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfnrrsFacetNames :: Lens.Lens' ListFacetNamesResponse (Core.Maybe [Types.FacetName])
lfnrrsFacetNames = Lens.field @"facetNames"
{-# DEPRECATED lfnrrsFacetNames "Use generic-lens or generic-optics with 'facetNames' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfnrrsNextToken :: Lens.Lens' ListFacetNamesResponse (Core.Maybe Types.NextToken)
lfnrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lfnrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfnrrsResponseStatus :: Lens.Lens' ListFacetNamesResponse Core.Int
lfnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lfnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
