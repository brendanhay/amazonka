{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.GroupResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified resources to the specified group.
module Network.AWS.ResourceGroups.GroupResources
  ( -- * Creating a request
    GroupResources (..),
    mkGroupResources,

    -- ** Request lenses
    grGroup,
    grResourceArns,

    -- * Destructuring the response
    GroupResourcesResponse (..),
    mkGroupResourcesResponse,

    -- ** Response lenses
    grrrsFailed,
    grrrsSucceeded,
    grrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGroupResources' smart constructor.
data GroupResources = GroupResources'
  { -- | The name or the ARN of the resource group to add resources to.
    group :: Types.GroupString,
    -- | The list of ARNs for resources to be added to the group.
    resourceArns :: Core.NonEmpty Types.ResourceArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupResources' value with any optional fields omitted.
mkGroupResources ::
  -- | 'group'
  Types.GroupString ->
  -- | 'resourceArns'
  Core.NonEmpty Types.ResourceArn ->
  GroupResources
mkGroupResources group resourceArns =
  GroupResources' {group, resourceArns}

-- | The name or the ARN of the resource group to add resources to.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grGroup :: Lens.Lens' GroupResources Types.GroupString
grGroup = Lens.field @"group"
{-# DEPRECATED grGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The list of ARNs for resources to be added to the group.
--
-- /Note:/ Consider using 'resourceArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grResourceArns :: Lens.Lens' GroupResources (Core.NonEmpty Types.ResourceArn)
grResourceArns = Lens.field @"resourceArns"
{-# DEPRECATED grResourceArns "Use generic-lens or generic-optics with 'resourceArns' instead." #-}

instance Core.FromJSON GroupResources where
  toJSON GroupResources {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Group" Core..= group),
            Core.Just ("ResourceArns" Core..= resourceArns)
          ]
      )

instance Core.AWSRequest GroupResources where
  type Rs GroupResources = GroupResourcesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/group-resources",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GroupResourcesResponse'
            Core.<$> (x Core..:? "Failed")
            Core.<*> (x Core..:? "Succeeded")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGroupResourcesResponse' smart constructor.
data GroupResourcesResponse = GroupResourcesResponse'
  { -- | The ARNs of the resources that failed to be added to the group by this operation.
    failed :: Core.Maybe [Types.FailedResource],
    -- | The ARNs of the resources that were successfully added to the group by this operation.
    succeeded :: Core.Maybe (Core.NonEmpty Types.ResourceArn),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupResourcesResponse' value with any optional fields omitted.
mkGroupResourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GroupResourcesResponse
mkGroupResourcesResponse responseStatus =
  GroupResourcesResponse'
    { failed = Core.Nothing,
      succeeded = Core.Nothing,
      responseStatus
    }

-- | The ARNs of the resources that failed to be added to the group by this operation.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsFailed :: Lens.Lens' GroupResourcesResponse (Core.Maybe [Types.FailedResource])
grrrsFailed = Lens.field @"failed"
{-# DEPRECATED grrrsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The ARNs of the resources that were successfully added to the group by this operation.
--
-- /Note:/ Consider using 'succeeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsSucceeded :: Lens.Lens' GroupResourcesResponse (Core.Maybe (Core.NonEmpty Types.ResourceArn))
grrrsSucceeded = Lens.field @"succeeded"
{-# DEPRECATED grrrsSucceeded "Use generic-lens or generic-optics with 'succeeded' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GroupResourcesResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
