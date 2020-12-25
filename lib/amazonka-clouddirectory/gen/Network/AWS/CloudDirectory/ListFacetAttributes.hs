{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListFacetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attributes attached to the facet.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListFacetAttributes
  ( -- * Creating a request
    ListFacetAttributes (..),
    mkListFacetAttributes,

    -- ** Request lenses
    lfaSchemaArn,
    lfaName,
    lfaMaxResults,
    lfaNextToken,

    -- * Destructuring the response
    ListFacetAttributesResponse (..),
    mkListFacetAttributesResponse,

    -- ** Response lenses
    lfarrsAttributes,
    lfarrsNextToken,
    lfarrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListFacetAttributes' smart constructor.
data ListFacetAttributes = ListFacetAttributes'
  { -- | The ARN of the schema where the facet resides.
    schemaArn :: Types.Arn,
    -- | The name of the facet whose attributes will be retrieved.
    name :: Types.FacetName,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFacetAttributes' value with any optional fields omitted.
mkListFacetAttributes ::
  -- | 'schemaArn'
  Types.Arn ->
  -- | 'name'
  Types.FacetName ->
  ListFacetAttributes
mkListFacetAttributes schemaArn name =
  ListFacetAttributes'
    { schemaArn,
      name,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ARN of the schema where the facet resides.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaSchemaArn :: Lens.Lens' ListFacetAttributes Types.Arn
lfaSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED lfaSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The name of the facet whose attributes will be retrieved.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaName :: Lens.Lens' ListFacetAttributes Types.FacetName
lfaName = Lens.field @"name"
{-# DEPRECATED lfaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaMaxResults :: Lens.Lens' ListFacetAttributes (Core.Maybe Core.Natural)
lfaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lfaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaNextToken :: Lens.Lens' ListFacetAttributes (Core.Maybe Types.NextToken)
lfaNextToken = Lens.field @"nextToken"
{-# DEPRECATED lfaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListFacetAttributes where
  toJSON ListFacetAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListFacetAttributes where
  type Rs ListFacetAttributes = ListFacetAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/facet/attributes",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "x-amz-data-partition" schemaArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFacetAttributesResponse'
            Core.<$> (x Core..:? "Attributes")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListFacetAttributes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"attributes" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListFacetAttributesResponse' smart constructor.
data ListFacetAttributesResponse = ListFacetAttributesResponse'
  { -- | The attributes attached to the facet.
    attributes :: Core.Maybe [Types.FacetAttribute],
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListFacetAttributesResponse' value with any optional fields omitted.
mkListFacetAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListFacetAttributesResponse
mkListFacetAttributesResponse responseStatus =
  ListFacetAttributesResponse'
    { attributes = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The attributes attached to the facet.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfarrsAttributes :: Lens.Lens' ListFacetAttributesResponse (Core.Maybe [Types.FacetAttribute])
lfarrsAttributes = Lens.field @"attributes"
{-# DEPRECATED lfarrsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfarrsNextToken :: Lens.Lens' ListFacetAttributesResponse (Core.Maybe Types.NextToken)
lfarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lfarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfarrsResponseStatus :: Lens.Lens' ListFacetAttributesResponse Core.Int
lfarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lfarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
