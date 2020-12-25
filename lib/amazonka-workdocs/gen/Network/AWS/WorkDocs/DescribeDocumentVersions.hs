{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeDocumentVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the document versions for the specified document.
--
-- By default, only active versions are returned.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeDocumentVersions
  ( -- * Creating a request
    DescribeDocumentVersions (..),
    mkDescribeDocumentVersions,

    -- ** Request lenses
    ddvDocumentId,
    ddvAuthenticationToken,
    ddvFields,
    ddvInclude,
    ddvLimit,
    ddvMarker,

    -- * Destructuring the response
    DescribeDocumentVersionsResponse (..),
    mkDescribeDocumentVersionsResponse,

    -- ** Response lenses
    ddvrrsDocumentVersions,
    ddvrrsMarker,
    ddvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDescribeDocumentVersions' smart constructor.
data DescribeDocumentVersions = DescribeDocumentVersions'
  { -- | The ID of the document.
    documentId :: Types.DocumentId,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | Specify "SOURCE" to include initialized versions and a URL for the source document.
    fields :: Core.Maybe Types.Fields,
    -- | A comma-separated list of values. Specify "INITIALIZED" to include incomplete versions.
    include :: Core.Maybe Types.Include,
    -- | The maximum number of versions to return with this call.
    limit :: Core.Maybe Core.Natural,
    -- | The marker for the next set of results. (You received this marker from a previous call.)
    marker :: Core.Maybe Types.Marker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDocumentVersions' value with any optional fields omitted.
mkDescribeDocumentVersions ::
  -- | 'documentId'
  Types.DocumentId ->
  DescribeDocumentVersions
mkDescribeDocumentVersions documentId =
  DescribeDocumentVersions'
    { documentId,
      authenticationToken = Core.Nothing,
      fields = Core.Nothing,
      include = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing
    }

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvDocumentId :: Lens.Lens' DescribeDocumentVersions Types.DocumentId
ddvDocumentId = Lens.field @"documentId"
{-# DEPRECATED ddvDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvAuthenticationToken :: Lens.Lens' DescribeDocumentVersions (Core.Maybe Types.AuthenticationHeaderType)
ddvAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED ddvAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | Specify "SOURCE" to include initialized versions and a URL for the source document.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvFields :: Lens.Lens' DescribeDocumentVersions (Core.Maybe Types.Fields)
ddvFields = Lens.field @"fields"
{-# DEPRECATED ddvFields "Use generic-lens or generic-optics with 'fields' instead." #-}

-- | A comma-separated list of values. Specify "INITIALIZED" to include incomplete versions.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvInclude :: Lens.Lens' DescribeDocumentVersions (Core.Maybe Types.Include)
ddvInclude = Lens.field @"include"
{-# DEPRECATED ddvInclude "Use generic-lens or generic-optics with 'include' instead." #-}

-- | The maximum number of versions to return with this call.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvLimit :: Lens.Lens' DescribeDocumentVersions (Core.Maybe Core.Natural)
ddvLimit = Lens.field @"limit"
{-# DEPRECATED ddvLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvMarker :: Lens.Lens' DescribeDocumentVersions (Core.Maybe Types.Marker)
ddvMarker = Lens.field @"marker"
{-# DEPRECATED ddvMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Core.AWSRequest DescribeDocumentVersions where
  type Rs DescribeDocumentVersions = DescribeDocumentVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/documents/" Core.<> (Core.toText documentId)
                Core.<> ("/versions")
            ),
        Core._rqQuery =
          Core.toQueryValue "fields" Core.<$> fields
            Core.<> (Core.toQueryValue "include" Core.<$> include)
            Core.<> (Core.toQueryValue "limit" Core.<$> limit)
            Core.<> (Core.toQueryValue "marker" Core.<$> marker),
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDocumentVersionsResponse'
            Core.<$> (x Core..:? "DocumentVersions")
            Core.<*> (x Core..:? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeDocumentVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"documentVersions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeDocumentVersionsResponse' smart constructor.
data DescribeDocumentVersionsResponse = DescribeDocumentVersionsResponse'
  { -- | The document versions.
    documentVersions :: Core.Maybe [Types.DocumentVersionMetadata],
    -- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
    marker :: Core.Maybe Types.Marker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDocumentVersionsResponse' value with any optional fields omitted.
mkDescribeDocumentVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDocumentVersionsResponse
mkDescribeDocumentVersionsResponse responseStatus =
  DescribeDocumentVersionsResponse'
    { documentVersions =
        Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | The document versions.
--
-- /Note:/ Consider using 'documentVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvrrsDocumentVersions :: Lens.Lens' DescribeDocumentVersionsResponse (Core.Maybe [Types.DocumentVersionMetadata])
ddvrrsDocumentVersions = Lens.field @"documentVersions"
{-# DEPRECATED ddvrrsDocumentVersions "Use generic-lens or generic-optics with 'documentVersions' instead." #-}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvrrsMarker :: Lens.Lens' DescribeDocumentVersionsResponse (Core.Maybe Types.Marker)
ddvrrsMarker = Lens.field @"marker"
{-# DEPRECATED ddvrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvrrsResponseStatus :: Lens.Lens' DescribeDocumentVersionsResponse Core.Int
ddvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
