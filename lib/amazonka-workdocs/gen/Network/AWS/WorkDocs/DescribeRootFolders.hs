{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeRootFolders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the current user's special folders; the @RootFolder@ and the @RecycleBin@ . @RootFolder@ is the root of user's files and folders and @RecycleBin@ is the root of recycled items. This is not a valid action for SigV4 (administrative API) clients.
--
-- This action requires an authentication token. To get an authentication token, register an application with Amazon WorkDocs. For more information, see <https://docs.aws.amazon.com/workdocs/latest/developerguide/wd-auth-user.html Authentication and Access Control for User Applications> in the /Amazon WorkDocs Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeRootFolders
    (
    -- * Creating a request
      DescribeRootFolders (..)
    , mkDescribeRootFolders
    -- ** Request lenses
    , drfAuthenticationToken
    , drfLimit
    , drfMarker

    -- * Destructuring the response
    , DescribeRootFoldersResponse (..)
    , mkDescribeRootFoldersResponse
    -- ** Response lenses
    , drfrrsFolders
    , drfrrsMarker
    , drfrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDescribeRootFolders' smart constructor.
data DescribeRootFolders = DescribeRootFolders'
  { authenticationToken :: Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return.
  , marker :: Core.Maybe Types.Marker
    -- ^ The marker for the next set of results. (You received this marker from a previous call.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRootFolders' value with any optional fields omitted.
mkDescribeRootFolders
    :: Types.AuthenticationHeaderType -- ^ 'authenticationToken'
    -> DescribeRootFolders
mkDescribeRootFolders authenticationToken
  = DescribeRootFolders'{authenticationToken, limit = Core.Nothing,
                         marker = Core.Nothing}

-- | Amazon WorkDocs authentication token.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfAuthenticationToken :: Lens.Lens' DescribeRootFolders Types.AuthenticationHeaderType
drfAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE drfAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfLimit :: Lens.Lens' DescribeRootFolders (Core.Maybe Core.Natural)
drfLimit = Lens.field @"limit"
{-# INLINEABLE drfLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfMarker :: Lens.Lens' DescribeRootFolders (Core.Maybe Types.Marker)
drfMarker = Lens.field @"marker"
{-# INLINEABLE drfMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery DescribeRootFolders where
        toQuery DescribeRootFolders{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "marker") marker

instance Core.ToHeaders DescribeRootFolders where
        toHeaders DescribeRootFolders{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeRootFolders where
        type Rs DescribeRootFolders = DescribeRootFoldersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/api/v1/me/root",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeRootFoldersResponse' Core.<$>
                   (x Core..:? "Folders") Core.<*> x Core..:? "Marker" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeRootFolders where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"folders" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeRootFoldersResponse' smart constructor.
data DescribeRootFoldersResponse = DescribeRootFoldersResponse'
  { folders :: Core.Maybe [Types.FolderMetadata]
    -- ^ The user's special folders.
  , marker :: Core.Maybe Types.PageMarkerType
    -- ^ The marker for the next set of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeRootFoldersResponse' value with any optional fields omitted.
mkDescribeRootFoldersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeRootFoldersResponse
mkDescribeRootFoldersResponse responseStatus
  = DescribeRootFoldersResponse'{folders = Core.Nothing,
                                 marker = Core.Nothing, responseStatus}

-- | The user's special folders.
--
-- /Note:/ Consider using 'folders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrrsFolders :: Lens.Lens' DescribeRootFoldersResponse (Core.Maybe [Types.FolderMetadata])
drfrrsFolders = Lens.field @"folders"
{-# INLINEABLE drfrrsFolders #-}
{-# DEPRECATED folders "Use generic-lens or generic-optics with 'folders' instead"  #-}

-- | The marker for the next set of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrrsMarker :: Lens.Lens' DescribeRootFoldersResponse (Core.Maybe Types.PageMarkerType)
drfrrsMarker = Lens.field @"marker"
{-# INLINEABLE drfrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrrsResponseStatus :: Lens.Lens' DescribeRootFoldersResponse Core.Int
drfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
