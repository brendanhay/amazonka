{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListSchemaVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of schema versions that you have created, with minimal information. Schema versions in Deleted status will not be included in the results. Empty results will be returned if there are no schema versions available.
--
-- This operation returns paginated results.
module Network.AWS.Glue.ListSchemaVersions
    (
    -- * Creating a request
      ListSchemaVersions (..)
    , mkListSchemaVersions
    -- ** Request lenses
    , lsvSchemaId
    , lsvMaxResults
    , lsvNextToken

    -- * Destructuring the response
    , ListSchemaVersionsResponse (..)
    , mkListSchemaVersionsResponse
    -- ** Response lenses
    , lsvrrsNextToken
    , lsvrrsSchemas
    , lsvrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSchemaVersions' smart constructor.
data ListSchemaVersions = ListSchemaVersions'
  { schemaId :: Types.SchemaId
    -- ^ This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token, if this is a continuation call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSchemaVersions' value with any optional fields omitted.
mkListSchemaVersions
    :: Types.SchemaId -- ^ 'schemaId'
    -> ListSchemaVersions
mkListSchemaVersions schemaId
  = ListSchemaVersions'{schemaId, maxResults = Core.Nothing,
                        nextToken = Core.Nothing}

-- | This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvSchemaId :: Lens.Lens' ListSchemaVersions Types.SchemaId
lsvSchemaId = Lens.field @"schemaId"
{-# INLINEABLE lsvSchemaId #-}
{-# DEPRECATED schemaId "Use generic-lens or generic-optics with 'schemaId' instead"  #-}

-- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvMaxResults :: Lens.Lens' ListSchemaVersions (Core.Maybe Core.Natural)
lsvMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lsvMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvNextToken :: Lens.Lens' ListSchemaVersions (Core.Maybe Types.NextToken)
lsvNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsvNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListSchemaVersions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListSchemaVersions where
        toHeaders ListSchemaVersions{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.ListSchemaVersions") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListSchemaVersions where
        toJSON ListSchemaVersions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SchemaId" Core..= schemaId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListSchemaVersions where
        type Rs ListSchemaVersions = ListSchemaVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListSchemaVersionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Schemas" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListSchemaVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"schemas" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListSchemaVersionsResponse' smart constructor.
data ListSchemaVersionsResponse = ListSchemaVersionsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
  , schemas :: Core.Maybe [Types.SchemaVersionListItem]
    -- ^ An array of @SchemaVersionList@ objects containing details of each schema version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSchemaVersionsResponse' value with any optional fields omitted.
mkListSchemaVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSchemaVersionsResponse
mkListSchemaVersionsResponse responseStatus
  = ListSchemaVersionsResponse'{nextToken = Core.Nothing,
                                schemas = Core.Nothing, responseStatus}

-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvrrsNextToken :: Lens.Lens' ListSchemaVersionsResponse (Core.Maybe Types.NextToken)
lsvrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsvrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of @SchemaVersionList@ objects containing details of each schema version.
--
-- /Note:/ Consider using 'schemas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvrrsSchemas :: Lens.Lens' ListSchemaVersionsResponse (Core.Maybe [Types.SchemaVersionListItem])
lsvrrsSchemas = Lens.field @"schemas"
{-# INLINEABLE lsvrrsSchemas #-}
{-# DEPRECATED schemas "Use generic-lens or generic-optics with 'schemas' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvrrsResponseStatus :: Lens.Lens' ListSchemaVersionsResponse Core.Int
lsvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
