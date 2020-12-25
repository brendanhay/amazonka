{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeResourceGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the resource groups that are specified by the ARNs of the resource groups.
module Network.AWS.Inspector.DescribeResourceGroups
  ( -- * Creating a request
    DescribeResourceGroups (..),
    mkDescribeResourceGroups,

    -- ** Request lenses
    drgResourceGroupArns,

    -- * Destructuring the response
    DescribeResourceGroupsResponse (..),
    mkDescribeResourceGroupsResponse,

    -- ** Response lenses
    drgrrsResourceGroups,
    drgrrsFailedItems,
    drgrrsResponseStatus,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeResourceGroups' smart constructor.
newtype DescribeResourceGroups = DescribeResourceGroups'
  { -- | The ARN that specifies the resource group that you want to describe.
    resourceGroupArns :: Core.NonEmpty Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeResourceGroups' value with any optional fields omitted.
mkDescribeResourceGroups ::
  -- | 'resourceGroupArns'
  Core.NonEmpty Types.Arn ->
  DescribeResourceGroups
mkDescribeResourceGroups resourceGroupArns =
  DescribeResourceGroups' {resourceGroupArns}

-- | The ARN that specifies the resource group that you want to describe.
--
-- /Note:/ Consider using 'resourceGroupArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgResourceGroupArns :: Lens.Lens' DescribeResourceGroups (Core.NonEmpty Types.Arn)
drgResourceGroupArns = Lens.field @"resourceGroupArns"
{-# DEPRECATED drgResourceGroupArns "Use generic-lens or generic-optics with 'resourceGroupArns' instead." #-}

instance Core.FromJSON DescribeResourceGroups where
  toJSON DescribeResourceGroups {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("resourceGroupArns" Core..= resourceGroupArns)]
      )

instance Core.AWSRequest DescribeResourceGroups where
  type Rs DescribeResourceGroups = DescribeResourceGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "InspectorService.DescribeResourceGroups")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourceGroupsResponse'
            Core.<$> (x Core..:? "resourceGroups" Core..!= Core.mempty)
            Core.<*> (x Core..:? "failedItems" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeResourceGroupsResponse' smart constructor.
data DescribeResourceGroupsResponse = DescribeResourceGroupsResponse'
  { -- | Information about a resource group.
    resourceGroups :: [Types.ResourceGroup],
    -- | Resource group details that cannot be described. An error code is provided for each failed item.
    failedItems :: Core.HashMap Types.Arn Types.FailedItemDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeResourceGroupsResponse' value with any optional fields omitted.
mkDescribeResourceGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeResourceGroupsResponse
mkDescribeResourceGroupsResponse responseStatus =
  DescribeResourceGroupsResponse'
    { resourceGroups = Core.mempty,
      failedItems = Core.mempty,
      responseStatus
    }

-- | Information about a resource group.
--
-- /Note:/ Consider using 'resourceGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrrsResourceGroups :: Lens.Lens' DescribeResourceGroupsResponse [Types.ResourceGroup]
drgrrsResourceGroups = Lens.field @"resourceGroups"
{-# DEPRECATED drgrrsResourceGroups "Use generic-lens or generic-optics with 'resourceGroups' instead." #-}

-- | Resource group details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrrsFailedItems :: Lens.Lens' DescribeResourceGroupsResponse (Core.HashMap Types.Arn Types.FailedItemDetails)
drgrrsFailedItems = Lens.field @"failedItems"
{-# DEPRECATED drgrrsFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgrrsResponseStatus :: Lens.Lens' DescribeResourceGroupsResponse Core.Int
drgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
