{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeDirectories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the directories that belong to this account.
--
-- You can retrieve information about specific directories by passing the directory identifiers in the @DirectoryIds@ parameter. Otherwise, all directories that belong to the current account are returned.
-- This operation supports pagination with the use of the @NextToken@ request and response parameters. If more results are available, the @DescribeDirectoriesResult.NextToken@ member contains a token that you pass in the next call to 'DescribeDirectories' to retrieve the next set of items.
-- You can also specify a maximum number of return results with the @Limit@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.DescribeDirectories
    (
    -- * Creating a request
      DescribeDirectories (..)
    , mkDescribeDirectories
    -- ** Request lenses
    , ddDirectoryIds
    , ddLimit
    , ddNextToken

    -- * Destructuring the response
    , DescribeDirectoriesResponse (..)
    , mkDescribeDirectoriesResponse
    -- ** Response lenses
    , ddrrsDirectoryDescriptions
    , ddrrsNextToken
    , ddrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'DescribeDirectories' operation.
--
-- /See:/ 'mkDescribeDirectories' smart constructor.
data DescribeDirectories = DescribeDirectories'
  { directoryIds :: Core.Maybe [Types.DirectoryId]
    -- ^ A list of identifiers of the directories for which to obtain the information. If this member is null, all directories that belong to the current account are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return. If this value is zero, the maximum number of items is specified by the limitations of the operation.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @DescribeDirectoriesResult.NextToken@ value from a previous call to 'DescribeDirectories' . Pass null if this is the first call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDirectories' value with any optional fields omitted.
mkDescribeDirectories
    :: DescribeDirectories
mkDescribeDirectories
  = DescribeDirectories'{directoryIds = Core.Nothing,
                         limit = Core.Nothing, nextToken = Core.Nothing}

-- | A list of identifiers of the directories for which to obtain the information. If this member is null, all directories that belong to the current account are returned.
--
-- An empty list results in an @InvalidParameterException@ being thrown.
--
-- /Note:/ Consider using 'directoryIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDirectoryIds :: Lens.Lens' DescribeDirectories (Core.Maybe [Types.DirectoryId])
ddDirectoryIds = Lens.field @"directoryIds"
{-# INLINEABLE ddDirectoryIds #-}
{-# DEPRECATED directoryIds "Use generic-lens or generic-optics with 'directoryIds' instead"  #-}

-- | The maximum number of items to return. If this value is zero, the maximum number of items is specified by the limitations of the operation.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddLimit :: Lens.Lens' DescribeDirectories (Core.Maybe Core.Natural)
ddLimit = Lens.field @"limit"
{-# INLINEABLE ddLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @DescribeDirectoriesResult.NextToken@ value from a previous call to 'DescribeDirectories' . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddNextToken :: Lens.Lens' DescribeDirectories (Core.Maybe Types.NextToken)
ddNextToken = Lens.field @"nextToken"
{-# INLINEABLE ddNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeDirectories where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeDirectories where
        toHeaders DescribeDirectories{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.DescribeDirectories")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeDirectories where
        toJSON DescribeDirectories{..}
          = Core.object
              (Core.catMaybes
                 [("DirectoryIds" Core..=) Core.<$> directoryIds,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeDirectories where
        type Rs DescribeDirectories = DescribeDirectoriesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeDirectoriesResponse' Core.<$>
                   (x Core..:? "DirectoryDescriptions") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDirectories where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"directoryDescriptions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Contains the results of the 'DescribeDirectories' operation.
--
-- /See:/ 'mkDescribeDirectoriesResponse' smart constructor.
data DescribeDirectoriesResponse = DescribeDirectoriesResponse'
  { directoryDescriptions :: Core.Maybe [Types.DirectoryDescription]
    -- ^ The list of 'DirectoryDescription' objects that were retrieved.
--
-- It is possible that this list contains less than the number of items specified in the @Limit@ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to 'DescribeDirectories' to retrieve the next set of items.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeDirectoriesResponse' value with any optional fields omitted.
mkDescribeDirectoriesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDirectoriesResponse
mkDescribeDirectoriesResponse responseStatus
  = DescribeDirectoriesResponse'{directoryDescriptions =
                                   Core.Nothing,
                                 nextToken = Core.Nothing, responseStatus}

-- | The list of 'DirectoryDescription' objects that were retrieved.
--
-- It is possible that this list contains less than the number of items specified in the @Limit@ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
--
-- /Note:/ Consider using 'directoryDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDirectoryDescriptions :: Lens.Lens' DescribeDirectoriesResponse (Core.Maybe [Types.DirectoryDescription])
ddrrsDirectoryDescriptions = Lens.field @"directoryDescriptions"
{-# INLINEABLE ddrrsDirectoryDescriptions #-}
{-# DEPRECATED directoryDescriptions "Use generic-lens or generic-optics with 'directoryDescriptions' instead"  #-}

-- | If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to 'DescribeDirectories' to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsNextToken :: Lens.Lens' DescribeDirectoriesResponse (Core.Maybe Types.NextToken)
ddrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ddrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DescribeDirectoriesResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
