{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceImagePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions that the owner of an image has granted to other AWS accounts for an image.
module Network.AWS.WorkSpaces.DescribeWorkspaceImagePermissions
  ( -- * Creating a request
    DescribeWorkspaceImagePermissions (..),
    mkDescribeWorkspaceImagePermissions,

    -- ** Request lenses
    dwipImageId,
    dwipMaxResults,
    dwipNextToken,

    -- * Destructuring the response
    DescribeWorkspaceImagePermissionsResponse (..),
    mkDescribeWorkspaceImagePermissionsResponse,

    -- ** Response lenses
    dwiprrsImageId,
    dwiprrsImagePermissions,
    dwiprrsNextToken,
    dwiprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDescribeWorkspaceImagePermissions' smart constructor.
data DescribeWorkspaceImagePermissions = DescribeWorkspaceImagePermissions'
  { -- | The identifier of the image.
    imageId :: Types.WorkspaceImageId,
    -- | The maximum number of items to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkspaceImagePermissions' value with any optional fields omitted.
mkDescribeWorkspaceImagePermissions ::
  -- | 'imageId'
  Types.WorkspaceImageId ->
  DescribeWorkspaceImagePermissions
mkDescribeWorkspaceImagePermissions imageId =
  DescribeWorkspaceImagePermissions'
    { imageId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwipImageId :: Lens.Lens' DescribeWorkspaceImagePermissions Types.WorkspaceImageId
dwipImageId = Lens.field @"imageId"
{-# DEPRECATED dwipImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwipMaxResults :: Lens.Lens' DescribeWorkspaceImagePermissions (Core.Maybe Core.Natural)
dwipMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dwipMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwipNextToken :: Lens.Lens' DescribeWorkspaceImagePermissions (Core.Maybe Types.PaginationToken)
dwipNextToken = Lens.field @"nextToken"
{-# DEPRECATED dwipNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeWorkspaceImagePermissions where
  toJSON DescribeWorkspaceImagePermissions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ImageId" Core..= imageId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeWorkspaceImagePermissions where
  type
    Rs DescribeWorkspaceImagePermissions =
      DescribeWorkspaceImagePermissionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "WorkspacesService.DescribeWorkspaceImagePermissions"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspaceImagePermissionsResponse'
            Core.<$> (x Core..:? "ImageId")
            Core.<*> (x Core..:? "ImagePermissions")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeWorkspaceImagePermissionsResponse' smart constructor.
data DescribeWorkspaceImagePermissionsResponse = DescribeWorkspaceImagePermissionsResponse'
  { -- | The identifier of the image.
    imageId :: Core.Maybe Types.WorkspaceImageId,
    -- | The identifiers of the AWS accounts that the image has been shared with.
    imagePermissions :: Core.Maybe [Types.ImagePermission],
    -- | The token to use to retrieve the next set of results, or null if no more results are available.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkspaceImagePermissionsResponse' value with any optional fields omitted.
mkDescribeWorkspaceImagePermissionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeWorkspaceImagePermissionsResponse
mkDescribeWorkspaceImagePermissionsResponse responseStatus =
  DescribeWorkspaceImagePermissionsResponse'
    { imageId =
        Core.Nothing,
      imagePermissions = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The identifier of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiprrsImageId :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse (Core.Maybe Types.WorkspaceImageId)
dwiprrsImageId = Lens.field @"imageId"
{-# DEPRECATED dwiprrsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The identifiers of the AWS accounts that the image has been shared with.
--
-- /Note:/ Consider using 'imagePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiprrsImagePermissions :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse (Core.Maybe [Types.ImagePermission])
dwiprrsImagePermissions = Lens.field @"imagePermissions"
{-# DEPRECATED dwiprrsImagePermissions "Use generic-lens or generic-optics with 'imagePermissions' instead." #-}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiprrsNextToken :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse (Core.Maybe Types.PaginationToken)
dwiprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dwiprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwiprrsResponseStatus :: Lens.Lens' DescribeWorkspaceImagePermissionsResponse Core.Int
dwiprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dwiprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
