{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists objects attached to the specified index.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListIndex
  ( -- * Creating a request
    ListIndex (..),
    mkListIndex,

    -- ** Request lenses
    liDirectoryArn,
    liIndexReference,
    liConsistencyLevel,
    liMaxResults,
    liNextToken,
    liRangesOnIndexedValues,

    -- * Destructuring the response
    ListIndexResponse (..),
    mkListIndexResponse,

    -- ** Response lenses
    lirrsIndexAttachments,
    lirrsNextToken,
    lirrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListIndex' smart constructor.
data ListIndex = ListIndex'
  { -- | The ARN of the directory that the index exists in.
    directoryArn :: Types.Arn,
    -- | The reference to the index to list.
    indexReference :: Types.ObjectReference,
    -- | The consistency level to execute the request at.
    consistencyLevel :: Core.Maybe Types.ConsistencyLevel,
    -- | The maximum number of objects in a single page to retrieve from the index during a request. For more information, see <http://docs.aws.amazon.com/clouddirectory/latest/developerguide/limits.html Amazon Cloud Directory Limits> .
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Specifies the ranges of indexed values that you want to query.
    rangesOnIndexedValues :: Core.Maybe [Types.ObjectAttributeRange]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListIndex' value with any optional fields omitted.
mkListIndex ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'indexReference'
  Types.ObjectReference ->
  ListIndex
mkListIndex directoryArn indexReference =
  ListIndex'
    { directoryArn,
      indexReference,
      consistencyLevel = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      rangesOnIndexedValues = Core.Nothing
    }

-- | The ARN of the directory that the index exists in.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liDirectoryArn :: Lens.Lens' ListIndex Types.Arn
liDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED liDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | The reference to the index to list.
--
-- /Note:/ Consider using 'indexReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liIndexReference :: Lens.Lens' ListIndex Types.ObjectReference
liIndexReference = Lens.field @"indexReference"
{-# DEPRECATED liIndexReference "Use generic-lens or generic-optics with 'indexReference' instead." #-}

-- | The consistency level to execute the request at.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liConsistencyLevel :: Lens.Lens' ListIndex (Core.Maybe Types.ConsistencyLevel)
liConsistencyLevel = Lens.field @"consistencyLevel"
{-# DEPRECATED liConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The maximum number of objects in a single page to retrieve from the index during a request. For more information, see <http://docs.aws.amazon.com/clouddirectory/latest/developerguide/limits.html Amazon Cloud Directory Limits> .
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListIndex (Core.Maybe Core.Natural)
liMaxResults = Lens.field @"maxResults"
{-# DEPRECATED liMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListIndex (Core.Maybe Types.NextToken)
liNextToken = Lens.field @"nextToken"
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies the ranges of indexed values that you want to query.
--
-- /Note:/ Consider using 'rangesOnIndexedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liRangesOnIndexedValues :: Lens.Lens' ListIndex (Core.Maybe [Types.ObjectAttributeRange])
liRangesOnIndexedValues = Lens.field @"rangesOnIndexedValues"
{-# DEPRECATED liRangesOnIndexedValues "Use generic-lens or generic-optics with 'rangesOnIndexedValues' instead." #-}

instance Core.FromJSON ListIndex where
  toJSON ListIndex {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IndexReference" Core..= indexReference),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("RangesOnIndexedValues" Core..=) Core.<$> rangesOnIndexedValues
          ]
      )

instance Core.AWSRequest ListIndex where
  type Rs ListIndex = ListIndexResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/index/targets",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn
            Core.<> (Core.toHeaders "x-amz-consistency-level" consistencyLevel),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIndexResponse'
            Core.<$> (x Core..:? "IndexAttachments")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListIndex where
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

-- | /See:/ 'mkListIndexResponse' smart constructor.
data ListIndexResponse = ListIndexResponse'
  { -- | The objects and indexed values attached to the index.
    indexAttachments :: Core.Maybe [Types.IndexAttachment],
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListIndexResponse' value with any optional fields omitted.
mkListIndexResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListIndexResponse
mkListIndexResponse responseStatus =
  ListIndexResponse'
    { indexAttachments = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The objects and indexed values attached to the index.
--
-- /Note:/ Consider using 'indexAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsIndexAttachments :: Lens.Lens' ListIndexResponse (Core.Maybe [Types.IndexAttachment])
lirrsIndexAttachments = Lens.field @"indexAttachments"
{-# DEPRECATED lirrsIndexAttachments "Use generic-lens or generic-optics with 'indexAttachments' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsNextToken :: Lens.Lens' ListIndexResponse (Core.Maybe Types.NextToken)
lirrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsResponseStatus :: Lens.Lens' ListIndexResponse Core.Int
lirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
