{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListTypedLinkFacetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all attribute definitions for a particular 'TypedLinkFacet' . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListTypedLinkFacetAttributes
  ( -- * Creating a request
    ListTypedLinkFacetAttributes (..),
    mkListTypedLinkFacetAttributes,

    -- ** Request lenses
    ltlfaSchemaArn,
    ltlfaName,
    ltlfaMaxResults,
    ltlfaNextToken,

    -- * Destructuring the response
    ListTypedLinkFacetAttributesResponse (..),
    mkListTypedLinkFacetAttributesResponse,

    -- ** Response lenses
    ltlfarrsAttributes,
    ltlfarrsNextToken,
    ltlfarrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTypedLinkFacetAttributes' smart constructor.
data ListTypedLinkFacetAttributes = ListTypedLinkFacetAttributes'
  { -- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
    schemaArn :: Types.Arn,
    -- | The unique name of the typed link facet.
    name :: Types.Name,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTypedLinkFacetAttributes' value with any optional fields omitted.
mkListTypedLinkFacetAttributes ::
  -- | 'schemaArn'
  Types.Arn ->
  -- | 'name'
  Types.Name ->
  ListTypedLinkFacetAttributes
mkListTypedLinkFacetAttributes schemaArn name =
  ListTypedLinkFacetAttributes'
    { schemaArn,
      name,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfaSchemaArn :: Lens.Lens' ListTypedLinkFacetAttributes Types.Arn
ltlfaSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED ltlfaSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The unique name of the typed link facet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfaName :: Lens.Lens' ListTypedLinkFacetAttributes Types.Name
ltlfaName = Lens.field @"name"
{-# DEPRECATED ltlfaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfaMaxResults :: Lens.Lens' ListTypedLinkFacetAttributes (Core.Maybe Core.Natural)
ltlfaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltlfaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfaNextToken :: Lens.Lens' ListTypedLinkFacetAttributes (Core.Maybe Types.NextToken)
ltlfaNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltlfaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListTypedLinkFacetAttributes where
  toJSON ListTypedLinkFacetAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListTypedLinkFacetAttributes where
  type
    Rs ListTypedLinkFacetAttributes =
      ListTypedLinkFacetAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            "/amazonclouddirectory/2017-01-11/typedlink/facet/attributes",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "x-amz-data-partition" schemaArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTypedLinkFacetAttributesResponse'
            Core.<$> (x Core..:? "Attributes")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListTypedLinkFacetAttributes where
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

-- | /See:/ 'mkListTypedLinkFacetAttributesResponse' smart constructor.
data ListTypedLinkFacetAttributesResponse = ListTypedLinkFacetAttributesResponse'
  { -- | An ordered set of attributes associate with the typed link.
    attributes :: Core.Maybe [Types.TypedLinkAttributeDefinition],
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListTypedLinkFacetAttributesResponse' value with any optional fields omitted.
mkListTypedLinkFacetAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTypedLinkFacetAttributesResponse
mkListTypedLinkFacetAttributesResponse responseStatus =
  ListTypedLinkFacetAttributesResponse'
    { attributes = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An ordered set of attributes associate with the typed link.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfarrsAttributes :: Lens.Lens' ListTypedLinkFacetAttributesResponse (Core.Maybe [Types.TypedLinkAttributeDefinition])
ltlfarrsAttributes = Lens.field @"attributes"
{-# DEPRECATED ltlfarrsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfarrsNextToken :: Lens.Lens' ListTypedLinkFacetAttributesResponse (Core.Maybe Types.NextToken)
ltlfarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltlfarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfarrsResponseStatus :: Lens.Lens' ListTypedLinkFacetAttributesResponse Core.Int
ltlfarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltlfarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
