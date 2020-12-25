{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.UngroupResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified resources from the specified group.
module Network.AWS.ResourceGroups.UngroupResources
  ( -- * Creating a request
    UngroupResources (..),
    mkUngroupResources,

    -- ** Request lenses
    urGroup,
    urResourceArns,

    -- * Destructuring the response
    UngroupResourcesResponse (..),
    mkUngroupResourcesResponse,

    -- ** Response lenses
    urrrsFailed,
    urrrsSucceeded,
    urrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.ResourceGroups.Types as Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUngroupResources' smart constructor.
data UngroupResources = UngroupResources'
  { -- | The name or the ARN of the resource group from which to remove the resources.
    group :: Types.GroupString,
    -- | The ARNs of the resources to be removed from the group.
    resourceArns :: Core.NonEmpty Types.ResourceArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UngroupResources' value with any optional fields omitted.
mkUngroupResources ::
  -- | 'group'
  Types.GroupString ->
  -- | 'resourceArns'
  Core.NonEmpty Types.ResourceArn ->
  UngroupResources
mkUngroupResources group resourceArns =
  UngroupResources' {group, resourceArns}

-- | The name or the ARN of the resource group from which to remove the resources.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urGroup :: Lens.Lens' UngroupResources Types.GroupString
urGroup = Lens.field @"group"
{-# DEPRECATED urGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The ARNs of the resources to be removed from the group.
--
-- /Note:/ Consider using 'resourceArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceArns :: Lens.Lens' UngroupResources (Core.NonEmpty Types.ResourceArn)
urResourceArns = Lens.field @"resourceArns"
{-# DEPRECATED urResourceArns "Use generic-lens or generic-optics with 'resourceArns' instead." #-}

instance Core.FromJSON UngroupResources where
  toJSON UngroupResources {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Group" Core..= group),
            Core.Just ("ResourceArns" Core..= resourceArns)
          ]
      )

instance Core.AWSRequest UngroupResources where
  type Rs UngroupResources = UngroupResourcesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/ungroup-resources",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UngroupResourcesResponse'
            Core.<$> (x Core..:? "Failed")
            Core.<*> (x Core..:? "Succeeded")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUngroupResourcesResponse' smart constructor.
data UngroupResourcesResponse = UngroupResourcesResponse'
  { -- | The resources that failed to be removed from the group.
    failed :: Core.Maybe [Types.FailedResource],
    -- | The ARNs of the resources that were successfully removed from the group.
    succeeded :: Core.Maybe (Core.NonEmpty Types.ResourceArn),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UngroupResourcesResponse' value with any optional fields omitted.
mkUngroupResourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UngroupResourcesResponse
mkUngroupResourcesResponse responseStatus =
  UngroupResourcesResponse'
    { failed = Core.Nothing,
      succeeded = Core.Nothing,
      responseStatus
    }

-- | The resources that failed to be removed from the group.
--
-- /Note:/ Consider using 'failed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsFailed :: Lens.Lens' UngroupResourcesResponse (Core.Maybe [Types.FailedResource])
urrrsFailed = Lens.field @"failed"
{-# DEPRECATED urrrsFailed "Use generic-lens or generic-optics with 'failed' instead." #-}

-- | The ARNs of the resources that were successfully removed from the group.
--
-- /Note:/ Consider using 'succeeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsSucceeded :: Lens.Lens' UngroupResourcesResponse (Core.Maybe (Core.NonEmpty Types.ResourceArn))
urrrsSucceeded = Lens.field @"succeeded"
{-# DEPRECATED urrrsSucceeded "Use generic-lens or generic-optics with 'succeeded' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UngroupResourcesResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
