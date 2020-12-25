{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListAttachedIndices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists indices attached to the specified object.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListAttachedIndices
  ( -- * Creating a request
    ListAttachedIndices (..),
    mkListAttachedIndices,

    -- ** Request lenses
    laiDirectoryArn,
    laiTargetReference,
    laiConsistencyLevel,
    laiMaxResults,
    laiNextToken,

    -- * Destructuring the response
    ListAttachedIndicesResponse (..),
    mkListAttachedIndicesResponse,

    -- ** Response lenses
    lairrsIndexAttachments,
    lairrsNextToken,
    lairrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAttachedIndices' smart constructor.
data ListAttachedIndices = ListAttachedIndices'
  { -- | The ARN of the directory.
    directoryArn :: Types.Arn,
    -- | A reference to the object that has indices attached.
    targetReference :: Types.ObjectReference,
    -- | The consistency level to use for this operation.
    consistencyLevel :: Core.Maybe Types.ConsistencyLevel,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAttachedIndices' value with any optional fields omitted.
mkListAttachedIndices ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'targetReference'
  Types.ObjectReference ->
  ListAttachedIndices
mkListAttachedIndices directoryArn targetReference =
  ListAttachedIndices'
    { directoryArn,
      targetReference,
      consistencyLevel = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ARN of the directory.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laiDirectoryArn :: Lens.Lens' ListAttachedIndices Types.Arn
laiDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED laiDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | A reference to the object that has indices attached.
--
-- /Note:/ Consider using 'targetReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laiTargetReference :: Lens.Lens' ListAttachedIndices Types.ObjectReference
laiTargetReference = Lens.field @"targetReference"
{-# DEPRECATED laiTargetReference "Use generic-lens or generic-optics with 'targetReference' instead." #-}

-- | The consistency level to use for this operation.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laiConsistencyLevel :: Lens.Lens' ListAttachedIndices (Core.Maybe Types.ConsistencyLevel)
laiConsistencyLevel = Lens.field @"consistencyLevel"
{-# DEPRECATED laiConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laiMaxResults :: Lens.Lens' ListAttachedIndices (Core.Maybe Core.Natural)
laiMaxResults = Lens.field @"maxResults"
{-# DEPRECATED laiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laiNextToken :: Lens.Lens' ListAttachedIndices (Core.Maybe Types.NextToken)
laiNextToken = Lens.field @"nextToken"
{-# DEPRECATED laiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListAttachedIndices where
  toJSON ListAttachedIndices {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TargetReference" Core..= targetReference),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListAttachedIndices where
  type Rs ListAttachedIndices = ListAttachedIndicesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/object/indices",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn
            Core.<> (Core.toHeaders "x-amz-consistency-level" consistencyLevel),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttachedIndicesResponse'
            Core.<$> (x Core..:? "IndexAttachments")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAttachedIndices where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"indexAttachments" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListAttachedIndicesResponse' smart constructor.
data ListAttachedIndicesResponse = ListAttachedIndicesResponse'
  { -- | The indices attached to the specified object.
    indexAttachments :: Core.Maybe [Types.IndexAttachment],
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAttachedIndicesResponse' value with any optional fields omitted.
mkListAttachedIndicesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAttachedIndicesResponse
mkListAttachedIndicesResponse responseStatus =
  ListAttachedIndicesResponse'
    { indexAttachments = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The indices attached to the specified object.
--
-- /Note:/ Consider using 'indexAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lairrsIndexAttachments :: Lens.Lens' ListAttachedIndicesResponse (Core.Maybe [Types.IndexAttachment])
lairrsIndexAttachments = Lens.field @"indexAttachments"
{-# DEPRECATED lairrsIndexAttachments "Use generic-lens or generic-optics with 'indexAttachments' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lairrsNextToken :: Lens.Lens' ListAttachedIndicesResponse (Core.Maybe Types.NextToken)
lairrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lairrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lairrsResponseStatus :: Lens.Lens' ListAttachedIndicesResponse Core.Int
lairrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lairrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
