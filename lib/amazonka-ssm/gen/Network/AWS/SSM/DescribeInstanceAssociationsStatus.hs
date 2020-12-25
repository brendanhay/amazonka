{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeInstanceAssociationsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The status of the associations for the instance(s).
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstanceAssociationsStatus
  ( -- * Creating a request
    DescribeInstanceAssociationsStatus (..),
    mkDescribeInstanceAssociationsStatus,

    -- ** Request lenses
    diasInstanceId,
    diasMaxResults,
    diasNextToken,

    -- * Destructuring the response
    DescribeInstanceAssociationsStatusResponse (..),
    mkDescribeInstanceAssociationsStatusResponse,

    -- ** Response lenses
    diasrrsInstanceAssociationStatusInfos,
    diasrrsNextToken,
    diasrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeInstanceAssociationsStatus' smart constructor.
data DescribeInstanceAssociationsStatus = DescribeInstanceAssociationsStatus'
  { -- | The instance IDs for which you want association status information.
    instanceId :: Types.InstanceId,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceAssociationsStatus' value with any optional fields omitted.
mkDescribeInstanceAssociationsStatus ::
  -- | 'instanceId'
  Types.InstanceId ->
  DescribeInstanceAssociationsStatus
mkDescribeInstanceAssociationsStatus instanceId =
  DescribeInstanceAssociationsStatus'
    { instanceId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The instance IDs for which you want association status information.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasInstanceId :: Lens.Lens' DescribeInstanceAssociationsStatus Types.InstanceId
diasInstanceId = Lens.field @"instanceId"
{-# DEPRECATED diasInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasMaxResults :: Lens.Lens' DescribeInstanceAssociationsStatus (Core.Maybe Core.Natural)
diasMaxResults = Lens.field @"maxResults"
{-# DEPRECATED diasMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasNextToken :: Lens.Lens' DescribeInstanceAssociationsStatus (Core.Maybe Types.NextToken)
diasNextToken = Lens.field @"nextToken"
{-# DEPRECATED diasNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeInstanceAssociationsStatus where
  toJSON DescribeInstanceAssociationsStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceId" Core..= instanceId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeInstanceAssociationsStatus where
  type
    Rs DescribeInstanceAssociationsStatus =
      DescribeInstanceAssociationsStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.DescribeInstanceAssociationsStatus")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstanceAssociationsStatusResponse'
            Core.<$> (x Core..:? "InstanceAssociationStatusInfos")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeInstanceAssociationsStatus where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"instanceAssociationStatusInfos" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeInstanceAssociationsStatusResponse' smart constructor.
data DescribeInstanceAssociationsStatusResponse = DescribeInstanceAssociationsStatusResponse'
  { -- | Status information about the association.
    instanceAssociationStatusInfos :: Core.Maybe [Types.InstanceAssociationStatusInfo],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeInstanceAssociationsStatusResponse' value with any optional fields omitted.
mkDescribeInstanceAssociationsStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInstanceAssociationsStatusResponse
mkDescribeInstanceAssociationsStatusResponse responseStatus =
  DescribeInstanceAssociationsStatusResponse'
    { instanceAssociationStatusInfos =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Status information about the association.
--
-- /Note:/ Consider using 'instanceAssociationStatusInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasrrsInstanceAssociationStatusInfos :: Lens.Lens' DescribeInstanceAssociationsStatusResponse (Core.Maybe [Types.InstanceAssociationStatusInfo])
diasrrsInstanceAssociationStatusInfos = Lens.field @"instanceAssociationStatusInfos"
{-# DEPRECATED diasrrsInstanceAssociationStatusInfos "Use generic-lens or generic-optics with 'instanceAssociationStatusInfos' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasrrsNextToken :: Lens.Lens' DescribeInstanceAssociationsStatusResponse (Core.Maybe Types.NextToken)
diasrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED diasrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diasrrsResponseStatus :: Lens.Lens' DescribeInstanceAssociationsStatusResponse Core.Int
diasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED diasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
