{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeComments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all the comments for the specified document version.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeComments
  ( -- * Creating a request
    DescribeComments (..),
    mkDescribeComments,

    -- ** Request lenses
    dcDocumentId,
    dcVersionId,
    dcAuthenticationToken,
    dcLimit,
    dcMarker,

    -- * Destructuring the response
    DescribeCommentsResponse (..),
    mkDescribeCommentsResponse,

    -- ** Response lenses
    dcrrsComments,
    dcrrsMarker,
    dcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDescribeComments' smart constructor.
data DescribeComments = DescribeComments'
  { -- | The ID of the document.
    documentId :: Types.DocumentId,
    -- | The ID of the document version.
    versionId :: Types.VersionId,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationToken,
    -- | The maximum number of items to return.
    limit :: Core.Maybe Core.Natural,
    -- | The marker for the next set of results. This marker was received from a previous call.
    marker :: Core.Maybe Types.Marker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeComments' value with any optional fields omitted.
mkDescribeComments ::
  -- | 'documentId'
  Types.DocumentId ->
  -- | 'versionId'
  Types.VersionId ->
  DescribeComments
mkDescribeComments documentId versionId =
  DescribeComments'
    { documentId,
      versionId,
      authenticationToken = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing
    }

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDocumentId :: Lens.Lens' DescribeComments Types.DocumentId
dcDocumentId = Lens.field @"documentId"
{-# DEPRECATED dcDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | The ID of the document version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcVersionId :: Lens.Lens' DescribeComments Types.VersionId
dcVersionId = Lens.field @"versionId"
{-# DEPRECATED dcVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcAuthenticationToken :: Lens.Lens' DescribeComments (Core.Maybe Types.AuthenticationToken)
dcAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED dcAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcLimit :: Lens.Lens' DescribeComments (Core.Maybe Core.Natural)
dcLimit = Lens.field @"limit"
{-# DEPRECATED dcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The marker for the next set of results. This marker was received from a previous call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMarker :: Lens.Lens' DescribeComments (Core.Maybe Types.Marker)
dcMarker = Lens.field @"marker"
{-# DEPRECATED dcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Core.AWSRequest DescribeComments where
  type Rs DescribeComments = DescribeCommentsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/documents/" Core.<> (Core.toText documentId)
                Core.<> ("/versions/")
                Core.<> (Core.toText versionId)
                Core.<> ("/comments")
            ),
        Core._rqQuery =
          Core.toQueryValue "limit" Core.<$> limit
            Core.<> (Core.toQueryValue "marker" Core.<$> marker),
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCommentsResponse'
            Core.<$> (x Core..:? "Comments")
            Core.<*> (x Core..:? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeComments where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"comments" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeCommentsResponse' smart constructor.
data DescribeCommentsResponse = DescribeCommentsResponse'
  { -- | The list of comments for the specified document version.
    comments :: Core.Maybe [Types.Comment],
    -- | The marker for the next set of results. This marker was received from a previous call.
    marker :: Core.Maybe Types.MarkerType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeCommentsResponse' value with any optional fields omitted.
mkDescribeCommentsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCommentsResponse
mkDescribeCommentsResponse responseStatus =
  DescribeCommentsResponse'
    { comments = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | The list of comments for the specified document version.
--
-- /Note:/ Consider using 'comments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsComments :: Lens.Lens' DescribeCommentsResponse (Core.Maybe [Types.Comment])
dcrrsComments = Lens.field @"comments"
{-# DEPRECATED dcrrsComments "Use generic-lens or generic-optics with 'comments' instead." #-}

-- | The marker for the next set of results. This marker was received from a previous call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsMarker :: Lens.Lens' DescribeCommentsResponse (Core.Maybe Types.MarkerType)
dcrrsMarker = Lens.field @"marker"
{-# DEPRECATED dcrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeCommentsResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
