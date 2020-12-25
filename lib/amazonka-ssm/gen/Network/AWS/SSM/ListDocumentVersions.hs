{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListDocumentVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all versions for a document.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListDocumentVersions
  ( -- * Creating a request
    ListDocumentVersions (..),
    mkListDocumentVersions,

    -- ** Request lenses
    ldvName,
    ldvMaxResults,
    ldvNextToken,

    -- * Destructuring the response
    ListDocumentVersionsResponse (..),
    mkListDocumentVersionsResponse,

    -- ** Response lenses
    ldvrrsDocumentVersions,
    ldvrrsNextToken,
    ldvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkListDocumentVersions' smart constructor.
data ListDocumentVersions = ListDocumentVersions'
  { -- | The name of the document. You can specify an Amazon Resource Name (ARN).
    name :: Types.DocumentARN,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDocumentVersions' value with any optional fields omitted.
mkListDocumentVersions ::
  -- | 'name'
  Types.DocumentARN ->
  ListDocumentVersions
mkListDocumentVersions name =
  ListDocumentVersions'
    { name,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the document. You can specify an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldvName :: Lens.Lens' ListDocumentVersions Types.DocumentARN
ldvName = Lens.field @"name"
{-# DEPRECATED ldvName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldvMaxResults :: Lens.Lens' ListDocumentVersions (Core.Maybe Core.Natural)
ldvMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ldvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldvNextToken :: Lens.Lens' ListDocumentVersions (Core.Maybe Types.NextToken)
ldvNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListDocumentVersions where
  toJSON ListDocumentVersions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListDocumentVersions where
  type Rs ListDocumentVersions = ListDocumentVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.ListDocumentVersions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDocumentVersionsResponse'
            Core.<$> (x Core..:? "DocumentVersions")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDocumentVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"documentVersions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListDocumentVersionsResponse' smart constructor.
data ListDocumentVersionsResponse = ListDocumentVersionsResponse'
  { -- | The document versions.
    documentVersions :: Core.Maybe (Core.NonEmpty Types.DocumentVersionInfo),
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDocumentVersionsResponse' value with any optional fields omitted.
mkListDocumentVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDocumentVersionsResponse
mkListDocumentVersionsResponse responseStatus =
  ListDocumentVersionsResponse'
    { documentVersions = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The document versions.
--
-- /Note:/ Consider using 'documentVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldvrrsDocumentVersions :: Lens.Lens' ListDocumentVersionsResponse (Core.Maybe (Core.NonEmpty Types.DocumentVersionInfo))
ldvrrsDocumentVersions = Lens.field @"documentVersions"
{-# DEPRECATED ldvrrsDocumentVersions "Use generic-lens or generic-optics with 'documentVersions' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldvrrsNextToken :: Lens.Lens' ListDocumentVersionsResponse (Core.Maybe Types.NextToken)
ldvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldvrrsResponseStatus :: Lens.Lens' ListDocumentVersionsResponse Core.Int
ldvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
