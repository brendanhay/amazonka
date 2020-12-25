{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListDeploymentTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of target IDs that are associated a deployment.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeploymentTargets
  ( -- * Creating a request
    ListDeploymentTargets (..),
    mkListDeploymentTargets,

    -- ** Request lenses
    ldtDeploymentId,
    ldtNextToken,
    ldtTargetFilters,

    -- * Destructuring the response
    ListDeploymentTargetsResponse (..),
    mkListDeploymentTargetsResponse,

    -- ** Response lenses
    ldtrrsNextToken,
    ldtrrsTargetIds,
    ldtrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDeploymentTargets' smart constructor.
data ListDeploymentTargets = ListDeploymentTargets'
  { -- | The unique ID of a deployment.
    deploymentId :: Core.Maybe Types.DeploymentId,
    -- | A token identifier returned from the previous @ListDeploymentTargets@ call. It can be used to return the next set of deployment targets in the list.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A key used to filter the returned targets. The two valid values are:
    --
    --
    --     * @TargetStatus@ - A @TargetStatus@ filter string can be @Failed@ , @InProgress@ , @Pending@ , @Ready@ , @Skipped@ , @Succeeded@ , or @Unknown@ .
    --
    --
    --     * @ServerInstanceLabel@ - A @ServerInstanceLabel@ filter string can be @Blue@ or @Green@ .
    targetFilters :: Core.Maybe (Core.HashMap Types.TargetFilterName [Types.FilterValue])
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeploymentTargets' value with any optional fields omitted.
mkListDeploymentTargets ::
  ListDeploymentTargets
mkListDeploymentTargets =
  ListDeploymentTargets'
    { deploymentId = Core.Nothing,
      nextToken = Core.Nothing,
      targetFilters = Core.Nothing
    }

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldtDeploymentId :: Lens.Lens' ListDeploymentTargets (Core.Maybe Types.DeploymentId)
ldtDeploymentId = Lens.field @"deploymentId"
{-# DEPRECATED ldtDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | A token identifier returned from the previous @ListDeploymentTargets@ call. It can be used to return the next set of deployment targets in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldtNextToken :: Lens.Lens' ListDeploymentTargets (Core.Maybe Types.NextToken)
ldtNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A key used to filter the returned targets. The two valid values are:
--
--
--     * @TargetStatus@ - A @TargetStatus@ filter string can be @Failed@ , @InProgress@ , @Pending@ , @Ready@ , @Skipped@ , @Succeeded@ , or @Unknown@ .
--
--
--     * @ServerInstanceLabel@ - A @ServerInstanceLabel@ filter string can be @Blue@ or @Green@ .
--
--
--
-- /Note:/ Consider using 'targetFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldtTargetFilters :: Lens.Lens' ListDeploymentTargets (Core.Maybe (Core.HashMap Types.TargetFilterName [Types.FilterValue]))
ldtTargetFilters = Lens.field @"targetFilters"
{-# DEPRECATED ldtTargetFilters "Use generic-lens or generic-optics with 'targetFilters' instead." #-}

instance Core.FromJSON ListDeploymentTargets where
  toJSON ListDeploymentTargets {..} =
    Core.object
      ( Core.catMaybes
          [ ("deploymentId" Core..=) Core.<$> deploymentId,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("targetFilters" Core..=) Core.<$> targetFilters
          ]
      )

instance Core.AWSRequest ListDeploymentTargets where
  type Rs ListDeploymentTargets = ListDeploymentTargetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.ListDeploymentTargets")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentTargetsResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "targetIds")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDeploymentTargets where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"targetIds" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListDeploymentTargetsResponse' smart constructor.
data ListDeploymentTargetsResponse = ListDeploymentTargetsResponse'
  { -- | If a large amount of information is returned, a token identifier is also returned. It can be used in a subsequent @ListDeploymentTargets@ call to return the next set of deployment targets in the list.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The unique IDs of deployment targets.
    targetIds :: Core.Maybe [Types.TargetId],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeploymentTargetsResponse' value with any optional fields omitted.
mkListDeploymentTargetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDeploymentTargetsResponse
mkListDeploymentTargetsResponse responseStatus =
  ListDeploymentTargetsResponse'
    { nextToken = Core.Nothing,
      targetIds = Core.Nothing,
      responseStatus
    }

-- | If a large amount of information is returned, a token identifier is also returned. It can be used in a subsequent @ListDeploymentTargets@ call to return the next set of deployment targets in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldtrrsNextToken :: Lens.Lens' ListDeploymentTargetsResponse (Core.Maybe Types.NextToken)
ldtrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldtrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The unique IDs of deployment targets.
--
-- /Note:/ Consider using 'targetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldtrrsTargetIds :: Lens.Lens' ListDeploymentTargetsResponse (Core.Maybe [Types.TargetId])
ldtrrsTargetIds = Lens.field @"targetIds"
{-# DEPRECATED ldtrrsTargetIds "Use generic-lens or generic-optics with 'targetIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldtrrsResponseStatus :: Lens.Lens' ListDeploymentTargetsResponse Core.Int
ldtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
