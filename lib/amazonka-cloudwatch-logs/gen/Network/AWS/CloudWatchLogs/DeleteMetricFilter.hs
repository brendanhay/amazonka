{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteMetricFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified metric filter.
module Network.AWS.CloudWatchLogs.DeleteMetricFilter
  ( -- * Creating a request
    DeleteMetricFilter (..),
    mkDeleteMetricFilter,

    -- ** Request lenses
    dmffLogGroupName,
    dmffFilterName,

    -- * Destructuring the response
    DeleteMetricFilterResponse (..),
    mkDeleteMetricFilterResponse,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteMetricFilter' smart constructor.
data DeleteMetricFilter = DeleteMetricFilter'
  { -- | The name of the log group.
    logGroupName :: Types.LogGroupName,
    -- | The name of the metric filter.
    filterName :: Types.FilterName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMetricFilter' value with any optional fields omitted.
mkDeleteMetricFilter ::
  -- | 'logGroupName'
  Types.LogGroupName ->
  -- | 'filterName'
  Types.FilterName ->
  DeleteMetricFilter
mkDeleteMetricFilter logGroupName filterName =
  DeleteMetricFilter' {logGroupName, filterName}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmffLogGroupName :: Lens.Lens' DeleteMetricFilter Types.LogGroupName
dmffLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED dmffLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The name of the metric filter.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmffFilterName :: Lens.Lens' DeleteMetricFilter Types.FilterName
dmffFilterName = Lens.field @"filterName"
{-# DEPRECATED dmffFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

instance Core.FromJSON DeleteMetricFilter where
  toJSON DeleteMetricFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("filterName" Core..= filterName)
          ]
      )

instance Core.AWSRequest DeleteMetricFilter where
  type Rs DeleteMetricFilter = DeleteMetricFilterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.DeleteMetricFilter")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteMetricFilterResponse'

-- | /See:/ 'mkDeleteMetricFilterResponse' smart constructor.
data DeleteMetricFilterResponse = DeleteMetricFilterResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMetricFilterResponse' value with any optional fields omitted.
mkDeleteMetricFilterResponse ::
  DeleteMetricFilterResponse
mkDeleteMetricFilterResponse = DeleteMetricFilterResponse'
