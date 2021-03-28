{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the directory snapshots that belong to this account.
--
-- This operation supports pagination with the use of the /NextToken/ request and response parameters. If more results are available, the /DescribeSnapshots.NextToken/ member contains a token that you pass in the next call to 'DescribeSnapshots' to retrieve the next set of items.
-- You can also specify a maximum number of return results with the /Limit/ parameter.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.DescribeSnapshots
    (
    -- * Creating a request
      DescribeSnapshots (..)
    , mkDescribeSnapshots
    -- ** Request lenses
    , dsDirectoryId
    , dsLimit
    , dsNextToken
    , dsSnapshotIds

    -- * Destructuring the response
    , DescribeSnapshotsResponse (..)
    , mkDescribeSnapshotsResponse
    -- ** Response lenses
    , dsrrsNextToken
    , dsrrsSnapshots
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'DescribeSnapshots' operation.
--
-- /See:/ 'mkDescribeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { directoryId :: Core.Maybe Types.DirectoryId
    -- ^ The identifier of the directory for which to retrieve snapshot information.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of objects to return.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The /DescribeSnapshotsResult.NextToken/ value from a previous call to 'DescribeSnapshots' . Pass null if this is the first call.
  , snapshotIds :: Core.Maybe [Types.SnapshotId]
    -- ^ A list of identifiers of the snapshots to obtain the information for. If this member is null or empty, all snapshots are returned using the /Limit/ and /NextToken/ members.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshots' value with any optional fields omitted.
mkDescribeSnapshots
    :: DescribeSnapshots
mkDescribeSnapshots
  = DescribeSnapshots'{directoryId = Core.Nothing,
                       limit = Core.Nothing, nextToken = Core.Nothing,
                       snapshotIds = Core.Nothing}

-- | The identifier of the directory for which to retrieve snapshot information.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDirectoryId :: Lens.Lens' DescribeSnapshots (Core.Maybe Types.DirectoryId)
dsDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE dsDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The maximum number of objects to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLimit :: Lens.Lens' DescribeSnapshots (Core.Maybe Core.Natural)
dsLimit = Lens.field @"limit"
{-# INLINEABLE dsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The /DescribeSnapshotsResult.NextToken/ value from a previous call to 'DescribeSnapshots' . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNextToken :: Lens.Lens' DescribeSnapshots (Core.Maybe Types.NextToken)
dsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of identifiers of the snapshots to obtain the information for. If this member is null or empty, all snapshots are returned using the /Limit/ and /NextToken/ members.
--
-- /Note:/ Consider using 'snapshotIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSnapshotIds :: Lens.Lens' DescribeSnapshots (Core.Maybe [Types.SnapshotId])
dsSnapshotIds = Lens.field @"snapshotIds"
{-# INLINEABLE dsSnapshotIds #-}
{-# DEPRECATED snapshotIds "Use generic-lens or generic-optics with 'snapshotIds' instead"  #-}

instance Core.ToQuery DescribeSnapshots where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeSnapshots where
        toHeaders DescribeSnapshots{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.DescribeSnapshots")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeSnapshots where
        toJSON DescribeSnapshots{..}
          = Core.object
              (Core.catMaybes
                 [("DirectoryId" Core..=) Core.<$> directoryId,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SnapshotIds" Core..=) Core.<$> snapshotIds])

instance Core.AWSRequest DescribeSnapshots where
        type Rs DescribeSnapshots = DescribeSnapshotsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeSnapshotsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Snapshots" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeSnapshots where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"snapshots" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Contains the results of the 'DescribeSnapshots' operation.
--
-- /See:/ 'mkDescribeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If not null, more results are available. Pass this value in the /NextToken/ member of a subsequent call to 'DescribeSnapshots' .
  , snapshots :: Core.Maybe [Types.Snapshot]
    -- ^ The list of 'Snapshot' objects that were retrieved.
--
-- It is possible that this list contains less than the number of items specified in the /Limit/ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeSnapshotsResponse' value with any optional fields omitted.
mkDescribeSnapshotsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSnapshotsResponse
mkDescribeSnapshotsResponse responseStatus
  = DescribeSnapshotsResponse'{nextToken = Core.Nothing,
                               snapshots = Core.Nothing, responseStatus}

-- | If not null, more results are available. Pass this value in the /NextToken/ member of a subsequent call to 'DescribeSnapshots' .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsNextToken :: Lens.Lens' DescribeSnapshotsResponse (Core.Maybe Types.NextToken)
dsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of 'Snapshot' objects that were retrieved.
--
-- It is possible that this list contains less than the number of items specified in the /Limit/ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
--
-- /Note:/ Consider using 'snapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSnapshots :: Lens.Lens' DescribeSnapshotsResponse (Core.Maybe [Types.Snapshot])
dsrrsSnapshots = Lens.field @"snapshots"
{-# INLINEABLE dsrrsSnapshots #-}
{-# DEPRECATED snapshots "Use generic-lens or generic-optics with 'snapshots' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DescribeSnapshotsResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
