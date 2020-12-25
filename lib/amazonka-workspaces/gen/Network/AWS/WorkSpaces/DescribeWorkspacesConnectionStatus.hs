{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspacesConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the connection status of the specified WorkSpaces.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspacesConnectionStatus
  ( -- * Creating a request
    DescribeWorkspacesConnectionStatus (..),
    mkDescribeWorkspacesConnectionStatus,

    -- ** Request lenses
    dwcsNextToken,
    dwcsWorkspaceIds,

    -- * Destructuring the response
    DescribeWorkspacesConnectionStatusResponse (..),
    mkDescribeWorkspacesConnectionStatusResponse,

    -- ** Response lenses
    dwcsrrsNextToken,
    dwcsrrsWorkspacesConnectionStatus,
    dwcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDescribeWorkspacesConnectionStatus' smart constructor.
data DescribeWorkspacesConnectionStatus = DescribeWorkspacesConnectionStatus'
  { -- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The identifiers of the WorkSpaces. You can specify up to 25 WorkSpaces.
    workspaceIds :: Core.Maybe (Core.NonEmpty Types.WorkspaceId)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkspacesConnectionStatus' value with any optional fields omitted.
mkDescribeWorkspacesConnectionStatus ::
  DescribeWorkspacesConnectionStatus
mkDescribeWorkspacesConnectionStatus =
  DescribeWorkspacesConnectionStatus'
    { nextToken = Core.Nothing,
      workspaceIds = Core.Nothing
    }

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcsNextToken :: Lens.Lens' DescribeWorkspacesConnectionStatus (Core.Maybe Types.PaginationToken)
dwcsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dwcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The identifiers of the WorkSpaces. You can specify up to 25 WorkSpaces.
--
-- /Note:/ Consider using 'workspaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcsWorkspaceIds :: Lens.Lens' DescribeWorkspacesConnectionStatus (Core.Maybe (Core.NonEmpty Types.WorkspaceId))
dwcsWorkspaceIds = Lens.field @"workspaceIds"
{-# DEPRECATED dwcsWorkspaceIds "Use generic-lens or generic-optics with 'workspaceIds' instead." #-}

instance Core.FromJSON DescribeWorkspacesConnectionStatus where
  toJSON DescribeWorkspacesConnectionStatus {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("WorkspaceIds" Core..=) Core.<$> workspaceIds
          ]
      )

instance Core.AWSRequest DescribeWorkspacesConnectionStatus where
  type
    Rs DescribeWorkspacesConnectionStatus =
      DescribeWorkspacesConnectionStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "WorkspacesService.DescribeWorkspacesConnectionStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspacesConnectionStatusResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "WorkspacesConnectionStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeWorkspacesConnectionStatus where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"workspacesConnectionStatus" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeWorkspacesConnectionStatusResponse' smart constructor.
data DescribeWorkspacesConnectionStatusResponse = DescribeWorkspacesConnectionStatusResponse'
  { -- | The token to use to retrieve the next set of results, or null if no more results are available.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | Information about the connection status of the WorkSpace.
    workspacesConnectionStatus :: Core.Maybe [Types.WorkspaceConnectionStatus],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeWorkspacesConnectionStatusResponse' value with any optional fields omitted.
mkDescribeWorkspacesConnectionStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeWorkspacesConnectionStatusResponse
mkDescribeWorkspacesConnectionStatusResponse responseStatus =
  DescribeWorkspacesConnectionStatusResponse'
    { nextToken =
        Core.Nothing,
      workspacesConnectionStatus = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcsrrsNextToken :: Lens.Lens' DescribeWorkspacesConnectionStatusResponse (Core.Maybe Types.PaginationToken)
dwcsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dwcsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the connection status of the WorkSpace.
--
-- /Note:/ Consider using 'workspacesConnectionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcsrrsWorkspacesConnectionStatus :: Lens.Lens' DescribeWorkspacesConnectionStatusResponse (Core.Maybe [Types.WorkspaceConnectionStatus])
dwcsrrsWorkspacesConnectionStatus = Lens.field @"workspacesConnectionStatus"
{-# DEPRECATED dwcsrrsWorkspacesConnectionStatus "Use generic-lens or generic-optics with 'workspacesConnectionStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwcsrrsResponseStatus :: Lens.Lens' DescribeWorkspacesConnectionStatusResponse Core.Int
dwcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dwcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
