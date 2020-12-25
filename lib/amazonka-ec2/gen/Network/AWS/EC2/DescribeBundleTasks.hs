{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeBundleTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified bundle tasks or all of your bundle tasks.
module Network.AWS.EC2.DescribeBundleTasks
  ( -- * Creating a request
    DescribeBundleTasks (..),
    mkDescribeBundleTasks,

    -- ** Request lenses
    dbtBundleIds,
    dbtDryRun,
    dbtFilters,

    -- * Destructuring the response
    DescribeBundleTasksResponse (..),
    mkDescribeBundleTasksResponse,

    -- ** Response lenses
    dbtrrsBundleTasks,
    dbtrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBundleTasks' smart constructor.
data DescribeBundleTasks = DescribeBundleTasks'
  { -- | The bundle task IDs.
    --
    -- Default: Describes all your bundle tasks.
    bundleIds :: Core.Maybe [Types.BundleId],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The filters.
    --
    --
    --     * @bundle-id@ - The ID of the bundle task.
    --
    --
    --     * @error-code@ - If the task failed, the error code returned.
    --
    --
    --     * @error-message@ - If the task failed, the error message returned.
    --
    --
    --     * @instance-id@ - The ID of the instance.
    --
    --
    --     * @progress@ - The level of task completion, as a percentage (for example, 20%).
    --
    --
    --     * @s3-bucket@ - The Amazon S3 bucket to store the AMI.
    --
    --
    --     * @s3-prefix@ - The beginning of the AMI name.
    --
    --
    --     * @start-time@ - The time the task started (for example, 2013-09-15T17:15:20.000Z).
    --
    --
    --     * @state@ - The state of the task (@pending@ | @waiting-for-shutdown@ | @bundling@ | @storing@ | @cancelling@ | @complete@ | @failed@ ).
    --
    --
    --     * @update-time@ - The time of the most recent update for the task.
    filters :: Core.Maybe [Types.Filter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBundleTasks' value with any optional fields omitted.
mkDescribeBundleTasks ::
  DescribeBundleTasks
mkDescribeBundleTasks =
  DescribeBundleTasks'
    { bundleIds = Core.Nothing,
      dryRun = Core.Nothing,
      filters = Core.Nothing
    }

-- | The bundle task IDs.
--
-- Default: Describes all your bundle tasks.
--
-- /Note:/ Consider using 'bundleIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbtBundleIds :: Lens.Lens' DescribeBundleTasks (Core.Maybe [Types.BundleId])
dbtBundleIds = Lens.field @"bundleIds"
{-# DEPRECATED dbtBundleIds "Use generic-lens or generic-optics with 'bundleIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbtDryRun :: Lens.Lens' DescribeBundleTasks (Core.Maybe Core.Bool)
dbtDryRun = Lens.field @"dryRun"
{-# DEPRECATED dbtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The filters.
--
--
--     * @bundle-id@ - The ID of the bundle task.
--
--
--     * @error-code@ - If the task failed, the error code returned.
--
--
--     * @error-message@ - If the task failed, the error message returned.
--
--
--     * @instance-id@ - The ID of the instance.
--
--
--     * @progress@ - The level of task completion, as a percentage (for example, 20%).
--
--
--     * @s3-bucket@ - The Amazon S3 bucket to store the AMI.
--
--
--     * @s3-prefix@ - The beginning of the AMI name.
--
--
--     * @start-time@ - The time the task started (for example, 2013-09-15T17:15:20.000Z).
--
--
--     * @state@ - The state of the task (@pending@ | @waiting-for-shutdown@ | @bundling@ | @storing@ | @cancelling@ | @complete@ | @failed@ ).
--
--
--     * @update-time@ - The time of the most recent update for the task.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbtFilters :: Lens.Lens' DescribeBundleTasks (Core.Maybe [Types.Filter])
dbtFilters = Lens.field @"filters"
{-# DEPRECATED dbtFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

instance Core.AWSRequest DescribeBundleTasks where
  type Rs DescribeBundleTasks = DescribeBundleTasksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeBundleTasks")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "BundleId" Core.<$> bundleIds)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeBundleTasksResponse'
            Core.<$> ( x Core..@? "bundleInstanceTasksSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeBundleTasksResponse' smart constructor.
data DescribeBundleTasksResponse = DescribeBundleTasksResponse'
  { -- | Information about the bundle tasks.
    bundleTasks :: Core.Maybe [Types.BundleTask],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeBundleTasksResponse' value with any optional fields omitted.
mkDescribeBundleTasksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeBundleTasksResponse
mkDescribeBundleTasksResponse responseStatus =
  DescribeBundleTasksResponse'
    { bundleTasks = Core.Nothing,
      responseStatus
    }

-- | Information about the bundle tasks.
--
-- /Note:/ Consider using 'bundleTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbtrrsBundleTasks :: Lens.Lens' DescribeBundleTasksResponse (Core.Maybe [Types.BundleTask])
dbtrrsBundleTasks = Lens.field @"bundleTasks"
{-# DEPRECATED dbtrrsBundleTasks "Use generic-lens or generic-optics with 'bundleTasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbtrrsResponseStatus :: Lens.Lens' DescribeBundleTasksResponse Core.Int
dbtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
