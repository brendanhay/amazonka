{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified WorkSpaces.
--
-- You can filter the results by using the bundle identifier, directory identifier, or owner, but you can specify only one filter at a time.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspaces
    (
    -- * Creating a request
      DescribeWorkspaces (..)
    , mkDescribeWorkspaces
    -- ** Request lenses
    , dwBundleId
    , dwDirectoryId
    , dwLimit
    , dwNextToken
    , dwUserName
    , dwWorkspaceIds

    -- * Destructuring the response
    , DescribeWorkspacesResponse (..)
    , mkDescribeWorkspacesResponse
    -- ** Response lenses
    , dwrrsNextToken
    , dwrrsWorkspaces
    , dwrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDescribeWorkspaces' smart constructor.
data DescribeWorkspaces = DescribeWorkspaces'
  { bundleId :: Core.Maybe Types.BundleId
    -- ^ The identifier of the bundle. All WorkSpaces that are created from this bundle are retrieved. You cannot combine this parameter with any other filter.
  , directoryId :: Core.Maybe Types.DirectoryId
    -- ^ The identifier of the directory. In addition, you can optionally specify a specific directory user (see @UserName@ ). You cannot combine this parameter with any other filter.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
  , userName :: Core.Maybe Types.UserName
    -- ^ The name of the directory user. You must specify this parameter with @DirectoryId@ .
  , workspaceIds :: Core.Maybe (Core.NonEmpty Types.WorkspaceId)
    -- ^ The identifiers of the WorkSpaces. You cannot combine this parameter with any other filter.
--
-- Because the 'CreateWorkspaces' operation is asynchronous, the identifier it returns is not immediately available. If you immediately call 'DescribeWorkspaces' with this identifier, no information is returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkspaces' value with any optional fields omitted.
mkDescribeWorkspaces
    :: DescribeWorkspaces
mkDescribeWorkspaces
  = DescribeWorkspaces'{bundleId = Core.Nothing,
                        directoryId = Core.Nothing, limit = Core.Nothing,
                        nextToken = Core.Nothing, userName = Core.Nothing,
                        workspaceIds = Core.Nothing}

-- | The identifier of the bundle. All WorkSpaces that are created from this bundle are retrieved. You cannot combine this parameter with any other filter.
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwBundleId :: Lens.Lens' DescribeWorkspaces (Core.Maybe Types.BundleId)
dwBundleId = Lens.field @"bundleId"
{-# INLINEABLE dwBundleId #-}
{-# DEPRECATED bundleId "Use generic-lens or generic-optics with 'bundleId' instead"  #-}

-- | The identifier of the directory. In addition, you can optionally specify a specific directory user (see @UserName@ ). You cannot combine this parameter with any other filter.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwDirectoryId :: Lens.Lens' DescribeWorkspaces (Core.Maybe Types.DirectoryId)
dwDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE dwDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwLimit :: Lens.Lens' DescribeWorkspaces (Core.Maybe Core.Natural)
dwLimit = Lens.field @"limit"
{-# INLINEABLE dwLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwNextToken :: Lens.Lens' DescribeWorkspaces (Core.Maybe Types.PaginationToken)
dwNextToken = Lens.field @"nextToken"
{-# INLINEABLE dwNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The name of the directory user. You must specify this parameter with @DirectoryId@ .
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwUserName :: Lens.Lens' DescribeWorkspaces (Core.Maybe Types.UserName)
dwUserName = Lens.field @"userName"
{-# INLINEABLE dwUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | The identifiers of the WorkSpaces. You cannot combine this parameter with any other filter.
--
-- Because the 'CreateWorkspaces' operation is asynchronous, the identifier it returns is not immediately available. If you immediately call 'DescribeWorkspaces' with this identifier, no information is returned.
--
-- /Note:/ Consider using 'workspaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwWorkspaceIds :: Lens.Lens' DescribeWorkspaces (Core.Maybe (Core.NonEmpty Types.WorkspaceId))
dwWorkspaceIds = Lens.field @"workspaceIds"
{-# INLINEABLE dwWorkspaceIds #-}
{-# DEPRECATED workspaceIds "Use generic-lens or generic-optics with 'workspaceIds' instead"  #-}

instance Core.ToQuery DescribeWorkspaces where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeWorkspaces where
        toHeaders DescribeWorkspaces{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.DescribeWorkspaces")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeWorkspaces where
        toJSON DescribeWorkspaces{..}
          = Core.object
              (Core.catMaybes
                 [("BundleId" Core..=) Core.<$> bundleId,
                  ("DirectoryId" Core..=) Core.<$> directoryId,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("UserName" Core..=) Core.<$> userName,
                  ("WorkspaceIds" Core..=) Core.<$> workspaceIds])

instance Core.AWSRequest DescribeWorkspaces where
        type Rs DescribeWorkspaces = DescribeWorkspacesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeWorkspacesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Workspaces" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeWorkspaces where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"workspaces" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeWorkspacesResponse' smart constructor.
data DescribeWorkspacesResponse = DescribeWorkspacesResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The token to use to retrieve the next set of results, or null if no more results are available.
  , workspaces :: Core.Maybe [Types.Workspace]
    -- ^ Information about the WorkSpaces.
--
-- Because 'CreateWorkspaces' is an asynchronous operation, some of the returned information could be incomplete.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkspacesResponse' value with any optional fields omitted.
mkDescribeWorkspacesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeWorkspacesResponse
mkDescribeWorkspacesResponse responseStatus
  = DescribeWorkspacesResponse'{nextToken = Core.Nothing,
                                workspaces = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next set of results, or null if no more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrrsNextToken :: Lens.Lens' DescribeWorkspacesResponse (Core.Maybe Types.PaginationToken)
dwrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dwrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the WorkSpaces.
--
-- Because 'CreateWorkspaces' is an asynchronous operation, some of the returned information could be incomplete.
--
-- /Note:/ Consider using 'workspaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrrsWorkspaces :: Lens.Lens' DescribeWorkspacesResponse (Core.Maybe [Types.Workspace])
dwrrsWorkspaces = Lens.field @"workspaces"
{-# INLINEABLE dwrrsWorkspaces #-}
{-# DEPRECATED workspaces "Use generic-lens or generic-optics with 'workspaces' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrrsResponseStatus :: Lens.Lens' DescribeWorkspacesResponse Core.Int
dwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
