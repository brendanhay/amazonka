{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeReplicationInstanceTaskLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the task logs for the specified task.
module Network.AWS.DMS.DescribeReplicationInstanceTaskLogs
    (
    -- * Creating a request
      DescribeReplicationInstanceTaskLogs (..)
    , mkDescribeReplicationInstanceTaskLogs
    -- ** Request lenses
    , dritlReplicationInstanceArn
    , dritlMarker
    , dritlMaxRecords

    -- * Destructuring the response
    , DescribeReplicationInstanceTaskLogsResponse (..)
    , mkDescribeReplicationInstanceTaskLogsResponse
    -- ** Response lenses
    , dritlrrsMarker
    , dritlrrsReplicationInstanceArn
    , dritlrrsReplicationInstanceTaskLogs
    , dritlrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeReplicationInstanceTaskLogs' smart constructor.
data DescribeReplicationInstanceTaskLogs = DescribeReplicationInstanceTaskLogs'
  { replicationInstanceArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the replication instance.
  , marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReplicationInstanceTaskLogs' value with any optional fields omitted.
mkDescribeReplicationInstanceTaskLogs
    :: Core.Text -- ^ 'replicationInstanceArn'
    -> DescribeReplicationInstanceTaskLogs
mkDescribeReplicationInstanceTaskLogs replicationInstanceArn
  = DescribeReplicationInstanceTaskLogs'{replicationInstanceArn,
                                         marker = Core.Nothing, maxRecords = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dritlReplicationInstanceArn :: Lens.Lens' DescribeReplicationInstanceTaskLogs Core.Text
dritlReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# INLINEABLE dritlReplicationInstanceArn #-}
{-# DEPRECATED replicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead"  #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dritlMarker :: Lens.Lens' DescribeReplicationInstanceTaskLogs (Core.Maybe Core.Text)
dritlMarker = Lens.field @"marker"
{-# INLINEABLE dritlMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved. 
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dritlMaxRecords :: Lens.Lens' DescribeReplicationInstanceTaskLogs (Core.Maybe Core.Int)
dritlMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dritlMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

instance Core.ToQuery DescribeReplicationInstanceTaskLogs where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeReplicationInstanceTaskLogs where
        toHeaders DescribeReplicationInstanceTaskLogs{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonDMSv20160101.DescribeReplicationInstanceTaskLogs")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeReplicationInstanceTaskLogs where
        toJSON DescribeReplicationInstanceTaskLogs{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ReplicationInstanceArn" Core..= replicationInstanceArn),
                  ("Marker" Core..=) Core.<$> marker,
                  ("MaxRecords" Core..=) Core.<$> maxRecords])

instance Core.AWSRequest DescribeReplicationInstanceTaskLogs where
        type Rs DescribeReplicationInstanceTaskLogs =
             DescribeReplicationInstanceTaskLogsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeReplicationInstanceTaskLogsResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*> x Core..:? "ReplicationInstanceArn"
                     Core.<*> x Core..:? "ReplicationInstanceTaskLogs"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeReplicationInstanceTaskLogsResponse' smart constructor.
data DescribeReplicationInstanceTaskLogsResponse = DescribeReplicationInstanceTaskLogsResponse'
  { marker :: Core.Maybe Core.Text
    -- ^ An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
  , replicationInstanceArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the replication instance.
  , replicationInstanceTaskLogs :: Core.Maybe [Types.ReplicationInstanceTaskLog]
    -- ^ An array of replication task log metadata. Each member of the array contains the replication task name, ARN, and task log size (in bytes). 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReplicationInstanceTaskLogsResponse' value with any optional fields omitted.
mkDescribeReplicationInstanceTaskLogsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeReplicationInstanceTaskLogsResponse
mkDescribeReplicationInstanceTaskLogsResponse responseStatus
  = DescribeReplicationInstanceTaskLogsResponse'{marker =
                                                   Core.Nothing,
                                                 replicationInstanceArn = Core.Nothing,
                                                 replicationInstanceTaskLogs = Core.Nothing,
                                                 responseStatus}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dritlrrsMarker :: Lens.Lens' DescribeReplicationInstanceTaskLogsResponse (Core.Maybe Core.Text)
dritlrrsMarker = Lens.field @"marker"
{-# INLINEABLE dritlrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dritlrrsReplicationInstanceArn :: Lens.Lens' DescribeReplicationInstanceTaskLogsResponse (Core.Maybe Core.Text)
dritlrrsReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# INLINEABLE dritlrrsReplicationInstanceArn #-}
{-# DEPRECATED replicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead"  #-}

-- | An array of replication task log metadata. Each member of the array contains the replication task name, ARN, and task log size (in bytes). 
--
-- /Note:/ Consider using 'replicationInstanceTaskLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dritlrrsReplicationInstanceTaskLogs :: Lens.Lens' DescribeReplicationInstanceTaskLogsResponse (Core.Maybe [Types.ReplicationInstanceTaskLog])
dritlrrsReplicationInstanceTaskLogs = Lens.field @"replicationInstanceTaskLogs"
{-# INLINEABLE dritlrrsReplicationInstanceTaskLogs #-}
{-# DEPRECATED replicationInstanceTaskLogs "Use generic-lens or generic-optics with 'replicationInstanceTaskLogs' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dritlrrsResponseStatus :: Lens.Lens' DescribeReplicationInstanceTaskLogsResponse Core.Int
dritlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dritlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
