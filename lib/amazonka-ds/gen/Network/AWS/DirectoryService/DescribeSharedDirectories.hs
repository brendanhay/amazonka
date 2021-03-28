{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeSharedDirectories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the shared directories in your account. 
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.DescribeSharedDirectories
    (
    -- * Creating a request
      DescribeSharedDirectories (..)
    , mkDescribeSharedDirectories
    -- ** Request lenses
    , dsdOwnerDirectoryId
    , dsdLimit
    , dsdNextToken
    , dsdSharedDirectoryIds

    -- * Destructuring the response
    , DescribeSharedDirectoriesResponse (..)
    , mkDescribeSharedDirectoriesResponse
    -- ** Response lenses
    , dsdrrsNextToken
    , dsdrrsSharedDirectories
    , dsdrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSharedDirectories' smart constructor.
data DescribeSharedDirectories = DescribeSharedDirectories'
  { ownerDirectoryId :: Types.DirectoryId
    -- ^ Returns the identifier of the directory in the directory owner account. 
  , limit :: Core.Maybe Core.Natural
    -- ^ The number of shared directories to return in the response object.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @DescribeSharedDirectoriesResult.NextToken@ value from a previous call to 'DescribeSharedDirectories' . Pass null if this is the first call. 
  , sharedDirectoryIds :: Core.Maybe [Types.DirectoryId]
    -- ^ A list of identifiers of all shared directories in your account. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSharedDirectories' value with any optional fields omitted.
mkDescribeSharedDirectories
    :: Types.DirectoryId -- ^ 'ownerDirectoryId'
    -> DescribeSharedDirectories
mkDescribeSharedDirectories ownerDirectoryId
  = DescribeSharedDirectories'{ownerDirectoryId,
                               limit = Core.Nothing, nextToken = Core.Nothing,
                               sharedDirectoryIds = Core.Nothing}

-- | Returns the identifier of the directory in the directory owner account. 
--
-- /Note:/ Consider using 'ownerDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdOwnerDirectoryId :: Lens.Lens' DescribeSharedDirectories Types.DirectoryId
dsdOwnerDirectoryId = Lens.field @"ownerDirectoryId"
{-# INLINEABLE dsdOwnerDirectoryId #-}
{-# DEPRECATED ownerDirectoryId "Use generic-lens or generic-optics with 'ownerDirectoryId' instead"  #-}

-- | The number of shared directories to return in the response object.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdLimit :: Lens.Lens' DescribeSharedDirectories (Core.Maybe Core.Natural)
dsdLimit = Lens.field @"limit"
{-# INLINEABLE dsdLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @DescribeSharedDirectoriesResult.NextToken@ value from a previous call to 'DescribeSharedDirectories' . Pass null if this is the first call. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdNextToken :: Lens.Lens' DescribeSharedDirectories (Core.Maybe Types.NextToken)
dsdNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of identifiers of all shared directories in your account. 
--
-- /Note:/ Consider using 'sharedDirectoryIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdSharedDirectoryIds :: Lens.Lens' DescribeSharedDirectories (Core.Maybe [Types.DirectoryId])
dsdSharedDirectoryIds = Lens.field @"sharedDirectoryIds"
{-# INLINEABLE dsdSharedDirectoryIds #-}
{-# DEPRECATED sharedDirectoryIds "Use generic-lens or generic-optics with 'sharedDirectoryIds' instead"  #-}

instance Core.ToQuery DescribeSharedDirectories where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeSharedDirectories where
        toHeaders DescribeSharedDirectories{..}
          = Core.pure
              ("X-Amz-Target",
               "DirectoryService_20150416.DescribeSharedDirectories")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeSharedDirectories where
        toJSON DescribeSharedDirectories{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OwnerDirectoryId" Core..= ownerDirectoryId),
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SharedDirectoryIds" Core..=) Core.<$> sharedDirectoryIds])

instance Core.AWSRequest DescribeSharedDirectories where
        type Rs DescribeSharedDirectories =
             DescribeSharedDirectoriesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeSharedDirectoriesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "SharedDirectories"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeSharedDirectories where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"sharedDirectories" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeSharedDirectoriesResponse' smart constructor.
data DescribeSharedDirectoriesResponse = DescribeSharedDirectoriesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If not null, token that indicates that more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to 'DescribeSharedDirectories' to retrieve the next set of items.
  , sharedDirectories :: Core.Maybe [Types.SharedDirectory]
    -- ^ A list of all shared directories in your account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeSharedDirectoriesResponse' value with any optional fields omitted.
mkDescribeSharedDirectoriesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSharedDirectoriesResponse
mkDescribeSharedDirectoriesResponse responseStatus
  = DescribeSharedDirectoriesResponse'{nextToken = Core.Nothing,
                                       sharedDirectories = Core.Nothing, responseStatus}

-- | If not null, token that indicates that more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to 'DescribeSharedDirectories' to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdrrsNextToken :: Lens.Lens' DescribeSharedDirectoriesResponse (Core.Maybe Types.NextToken)
dsdrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsdrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of all shared directories in your account.
--
-- /Note:/ Consider using 'sharedDirectories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdrrsSharedDirectories :: Lens.Lens' DescribeSharedDirectoriesResponse (Core.Maybe [Types.SharedDirectory])
dsdrrsSharedDirectories = Lens.field @"sharedDirectories"
{-# INLINEABLE dsdrrsSharedDirectories #-}
{-# DEPRECATED sharedDirectories "Use generic-lens or generic-optics with 'sharedDirectories' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdrrsResponseStatus :: Lens.Lens' DescribeSharedDirectoriesResponse Core.Int
dsdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
