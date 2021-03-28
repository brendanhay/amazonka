{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeExportTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified export instance tasks or all of your export instance tasks.
module Network.AWS.EC2.DescribeExportTasks
    (
    -- * Creating a request
      DescribeExportTasks (..)
    , mkDescribeExportTasks
    -- ** Request lenses
    , detExportTaskIds
    , detFilters

    -- * Destructuring the response
    , DescribeExportTasksResponse (..)
    , mkDescribeExportTasksResponse
    -- ** Response lenses
    , detrrsExportTasks
    , detrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { exportTaskIds :: Core.Maybe [Types.ExportTaskId]
    -- ^ The export task IDs.
  , filters :: Core.Maybe [Types.Filter]
    -- ^ the filters for the export tasks.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExportTasks' value with any optional fields omitted.
mkDescribeExportTasks
    :: DescribeExportTasks
mkDescribeExportTasks
  = DescribeExportTasks'{exportTaskIds = Core.Nothing,
                         filters = Core.Nothing}

-- | The export task IDs.
--
-- /Note:/ Consider using 'exportTaskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detExportTaskIds :: Lens.Lens' DescribeExportTasks (Core.Maybe [Types.ExportTaskId])
detExportTaskIds = Lens.field @"exportTaskIds"
{-# INLINEABLE detExportTaskIds #-}
{-# DEPRECATED exportTaskIds "Use generic-lens or generic-optics with 'exportTaskIds' instead"  #-}

-- | the filters for the export tasks.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detFilters :: Lens.Lens' DescribeExportTasks (Core.Maybe [Types.Filter])
detFilters = Lens.field @"filters"
{-# INLINEABLE detFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

instance Core.ToQuery DescribeExportTasks where
        toQuery DescribeExportTasks{..}
          = Core.toQueryPair "Action" ("DescribeExportTasks" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "ExportTaskId")
                exportTaskIds
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters

instance Core.ToHeaders DescribeExportTasks where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeExportTasks where
        type Rs DescribeExportTasks = DescribeExportTasksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DescribeExportTasksResponse' Core.<$>
                   (x Core..@? "exportTaskSet" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { exportTasks :: Core.Maybe [Types.ExportTask]
    -- ^ Information about the export tasks.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExportTasksResponse' value with any optional fields omitted.
mkDescribeExportTasksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeExportTasksResponse
mkDescribeExportTasksResponse responseStatus
  = DescribeExportTasksResponse'{exportTasks = Core.Nothing,
                                 responseStatus}

-- | Information about the export tasks.
--
-- /Note:/ Consider using 'exportTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsExportTasks :: Lens.Lens' DescribeExportTasksResponse (Core.Maybe [Types.ExportTask])
detrrsExportTasks = Lens.field @"exportTasks"
{-# INLINEABLE detrrsExportTasks #-}
{-# DEPRECATED exportTasks "Use generic-lens or generic-optics with 'exportTasks' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsResponseStatus :: Lens.Lens' DescribeExportTasksResponse Core.Int
detrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE detrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
