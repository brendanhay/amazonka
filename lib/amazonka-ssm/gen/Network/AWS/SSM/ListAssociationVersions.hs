{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListAssociationVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all versions of an association for a specific association ID.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListAssociationVersions
  ( -- * Creating a request
    ListAssociationVersions (..),
    mkListAssociationVersions,

    -- ** Request lenses
    lavAssociationId,
    lavMaxResults,
    lavNextToken,

    -- * Destructuring the response
    ListAssociationVersionsResponse (..),
    mkListAssociationVersionsResponse,

    -- ** Response lenses
    lavrrsAssociationVersions,
    lavrrsNextToken,
    lavrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkListAssociationVersions' smart constructor.
data ListAssociationVersions = ListAssociationVersions'
  { -- | The association ID for which you want to view all versions.
    associationId :: Types.AssociationId,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | A token to start the list. Use this token to get the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssociationVersions' value with any optional fields omitted.
mkListAssociationVersions ::
  -- | 'associationId'
  Types.AssociationId ->
  ListAssociationVersions
mkListAssociationVersions associationId =
  ListAssociationVersions'
    { associationId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The association ID for which you want to view all versions.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavAssociationId :: Lens.Lens' ListAssociationVersions Types.AssociationId
lavAssociationId = Lens.field @"associationId"
{-# DEPRECATED lavAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavMaxResults :: Lens.Lens' ListAssociationVersions (Core.Maybe Core.Natural)
lavMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lavMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavNextToken :: Lens.Lens' ListAssociationVersions (Core.Maybe Types.NextToken)
lavNextToken = Lens.field @"nextToken"
{-# DEPRECATED lavNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListAssociationVersions where
  toJSON ListAssociationVersions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AssociationId" Core..= associationId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListAssociationVersions where
  type Rs ListAssociationVersions = ListAssociationVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.ListAssociationVersions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociationVersionsResponse'
            Core.<$> (x Core..:? "AssociationVersions")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAssociationVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"associationVersions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListAssociationVersionsResponse' smart constructor.
data ListAssociationVersionsResponse = ListAssociationVersionsResponse'
  { -- | Information about all versions of the association for the specified association ID.
    associationVersions :: Core.Maybe (Core.NonEmpty Types.AssociationVersionInfo),
    -- | The token for the next set of items to return. Use this token to get the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAssociationVersionsResponse' value with any optional fields omitted.
mkListAssociationVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAssociationVersionsResponse
mkListAssociationVersionsResponse responseStatus =
  ListAssociationVersionsResponse'
    { associationVersions =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about all versions of the association for the specified association ID.
--
-- /Note:/ Consider using 'associationVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrrsAssociationVersions :: Lens.Lens' ListAssociationVersionsResponse (Core.Maybe (Core.NonEmpty Types.AssociationVersionInfo))
lavrrsAssociationVersions = Lens.field @"associationVersions"
{-# DEPRECATED lavrrsAssociationVersions "Use generic-lens or generic-optics with 'associationVersions' instead." #-}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrrsNextToken :: Lens.Lens' ListAssociationVersionsResponse (Core.Maybe Types.NextToken)
lavrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lavrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lavrrsResponseStatus :: Lens.Lens' ListAssociationVersionsResponse Core.Int
lavrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lavrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
