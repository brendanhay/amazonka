{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowsForTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the maintenance window targets or tasks that an instance is associated with.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindowsForTarget
  ( -- * Creating a request
    DescribeMaintenanceWindowsForTarget (..),
    mkDescribeMaintenanceWindowsForTarget,

    -- ** Request lenses
    dmwftTargets,
    dmwftResourceType,
    dmwftMaxResults,
    dmwftNextToken,

    -- * Destructuring the response
    DescribeMaintenanceWindowsForTargetResponse (..),
    mkDescribeMaintenanceWindowsForTargetResponse,

    -- ** Response lenses
    dmwftrrsNextToken,
    dmwftrrsWindowIdentities,
    dmwftrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeMaintenanceWindowsForTarget' smart constructor.
data DescribeMaintenanceWindowsForTarget = DescribeMaintenanceWindowsForTarget'
  { -- | The instance ID or key/value pair to retrieve information about.
    targets :: [Types.Target],
    -- | The type of resource you want to retrieve information about. For example, "INSTANCE".
    resourceType :: Types.MaintenanceWindowResourceType,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMaintenanceWindowsForTarget' value with any optional fields omitted.
mkDescribeMaintenanceWindowsForTarget ::
  -- | 'resourceType'
  Types.MaintenanceWindowResourceType ->
  DescribeMaintenanceWindowsForTarget
mkDescribeMaintenanceWindowsForTarget resourceType =
  DescribeMaintenanceWindowsForTarget'
    { targets = Core.mempty,
      resourceType,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The instance ID or key/value pair to retrieve information about.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwftTargets :: Lens.Lens' DescribeMaintenanceWindowsForTarget [Types.Target]
dmwftTargets = Lens.field @"targets"
{-# DEPRECATED dmwftTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The type of resource you want to retrieve information about. For example, "INSTANCE".
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwftResourceType :: Lens.Lens' DescribeMaintenanceWindowsForTarget Types.MaintenanceWindowResourceType
dmwftResourceType = Lens.field @"resourceType"
{-# DEPRECATED dmwftResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwftMaxResults :: Lens.Lens' DescribeMaintenanceWindowsForTarget (Core.Maybe Core.Natural)
dmwftMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dmwftMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwftNextToken :: Lens.Lens' DescribeMaintenanceWindowsForTarget (Core.Maybe Types.NextToken)
dmwftNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmwftNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeMaintenanceWindowsForTarget where
  toJSON DescribeMaintenanceWindowsForTarget {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Targets" Core..= targets),
            Core.Just ("ResourceType" Core..= resourceType),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeMaintenanceWindowsForTarget where
  type
    Rs DescribeMaintenanceWindowsForTarget =
      DescribeMaintenanceWindowsForTargetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.DescribeMaintenanceWindowsForTarget")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMaintenanceWindowsForTargetResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "WindowIdentities")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeMaintenanceWindowsForTarget where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"windowIdentities" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeMaintenanceWindowsForTargetResponse' smart constructor.
data DescribeMaintenanceWindowsForTargetResponse = DescribeMaintenanceWindowsForTargetResponse'
  { -- | The token for the next set of items to return. (You use this token in the next call.)
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the maintenance window targets and tasks an instance is associated with.
    windowIdentities :: Core.Maybe [Types.MaintenanceWindowIdentityForTarget],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMaintenanceWindowsForTargetResponse' value with any optional fields omitted.
mkDescribeMaintenanceWindowsForTargetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeMaintenanceWindowsForTargetResponse
mkDescribeMaintenanceWindowsForTargetResponse responseStatus =
  DescribeMaintenanceWindowsForTargetResponse'
    { nextToken =
        Core.Nothing,
      windowIdentities = Core.Nothing,
      responseStatus
    }

-- | The token for the next set of items to return. (You use this token in the next call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwftrrsNextToken :: Lens.Lens' DescribeMaintenanceWindowsForTargetResponse (Core.Maybe Types.NextToken)
dmwftrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dmwftrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the maintenance window targets and tasks an instance is associated with.
--
-- /Note:/ Consider using 'windowIdentities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwftrrsWindowIdentities :: Lens.Lens' DescribeMaintenanceWindowsForTargetResponse (Core.Maybe [Types.MaintenanceWindowIdentityForTarget])
dmwftrrsWindowIdentities = Lens.field @"windowIdentities"
{-# DEPRECATED dmwftrrsWindowIdentities "Use generic-lens or generic-optics with 'windowIdentities' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwftrrsResponseStatus :: Lens.Lens' DescribeMaintenanceWindowsForTargetResponse Core.Int
dmwftrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmwftrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
