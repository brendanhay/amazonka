{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListSchemas
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of schemas with minimal details. Schemas in Deleting status will not be included in the results. Empty results will be returned if there are no schemas available.
--
-- When the @RegistryId@ is not provided, all the schemas across registries will be part of the API response.
--
-- This operation returns paginated results.
module Network.AWS.Glue.ListSchemas
  ( -- * Creating a request
    ListSchemas (..),
    mkListSchemas,

    -- ** Request lenses
    lsMaxResults,
    lsNextToken,
    lsRegistryId,

    -- * Destructuring the response
    ListSchemasResponse (..),
    mkListSchemasResponse,

    -- ** Response lenses
    lsrrsNextToken,
    lsrrsSchemas,
    lsrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSchemas' smart constructor.
data ListSchemas = ListSchemas'
  { -- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
    registryId :: Core.Maybe Types.RegistryId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSchemas' value with any optional fields omitted.
mkListSchemas ::
  ListSchemas
mkListSchemas =
  ListSchemas'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      registryId = Core.Nothing
    }

-- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMaxResults :: Lens.Lens' ListSchemas (Core.Maybe Core.Natural)
lsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListSchemas (Core.Maybe Types.NextToken)
lsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsRegistryId :: Lens.Lens' ListSchemas (Core.Maybe Types.RegistryId)
lsRegistryId = Lens.field @"registryId"
{-# DEPRECATED lsRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

instance Core.FromJSON ListSchemas where
  toJSON ListSchemas {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("RegistryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.AWSRequest ListSchemas where
  type Rs ListSchemas = ListSchemasResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.ListSchemas")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSchemasResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Schemas")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListSchemas where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"schemas" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListSchemasResponse' smart constructor.
data ListSchemasResponse = ListSchemasResponse'
  { -- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
    nextToken :: Core.Maybe Types.NextToken,
    -- | An array of @SchemaListItem@ objects containing details of each schema.
    schemas :: Core.Maybe [Types.SchemaListItem],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSchemasResponse' value with any optional fields omitted.
mkListSchemasResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListSchemasResponse
mkListSchemasResponse responseStatus =
  ListSchemasResponse'
    { nextToken = Core.Nothing,
      schemas = Core.Nothing,
      responseStatus
    }

-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsNextToken :: Lens.Lens' ListSchemasResponse (Core.Maybe Types.NextToken)
lsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @SchemaListItem@ objects containing details of each schema.
--
-- /Note:/ Consider using 'schemas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsSchemas :: Lens.Lens' ListSchemasResponse (Core.Maybe [Types.SchemaListItem])
lsrrsSchemas = Lens.field @"schemas"
{-# DEPRECATED lsrrsSchemas "Use generic-lens or generic-optics with 'schemas' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsResponseStatus :: Lens.Lens' ListSchemasResponse Core.Int
lsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
