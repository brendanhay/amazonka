{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.UpdateSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing subnet group.
module Network.AWS.DAX.UpdateSubnetGroup
  ( -- * Creating a request
    UpdateSubnetGroup (..),
    mkUpdateSubnetGroup,

    -- ** Request lenses
    usgSubnetGroupName,
    usgDescription,
    usgSubnetIds,

    -- * Destructuring the response
    UpdateSubnetGroupResponse (..),
    mkUpdateSubnetGroupResponse,

    -- ** Response lenses
    usgrrsSubnetGroup,
    usgrrsResponseStatus,
  )
where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSubnetGroup' smart constructor.
data UpdateSubnetGroup = UpdateSubnetGroup'
  { -- | The name of the subnet group.
    subnetGroupName :: Types.String,
    -- | A description of the subnet group.
    description :: Core.Maybe Types.String,
    -- | A list of subnet IDs in the subnet group.
    subnetIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSubnetGroup' value with any optional fields omitted.
mkUpdateSubnetGroup ::
  -- | 'subnetGroupName'
  Types.String ->
  UpdateSubnetGroup
mkUpdateSubnetGroup subnetGroupName =
  UpdateSubnetGroup'
    { subnetGroupName,
      description = Core.Nothing,
      subnetIds = Core.Nothing
    }

-- | The name of the subnet group.
--
-- /Note:/ Consider using 'subnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgSubnetGroupName :: Lens.Lens' UpdateSubnetGroup Types.String
usgSubnetGroupName = Lens.field @"subnetGroupName"
{-# DEPRECATED usgSubnetGroupName "Use generic-lens or generic-optics with 'subnetGroupName' instead." #-}

-- | A description of the subnet group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgDescription :: Lens.Lens' UpdateSubnetGroup (Core.Maybe Types.String)
usgDescription = Lens.field @"description"
{-# DEPRECATED usgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of subnet IDs in the subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgSubnetIds :: Lens.Lens' UpdateSubnetGroup (Core.Maybe [Types.String])
usgSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED usgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

instance Core.FromJSON UpdateSubnetGroup where
  toJSON UpdateSubnetGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SubnetGroupName" Core..= subnetGroupName),
            ("Description" Core..=) Core.<$> description,
            ("SubnetIds" Core..=) Core.<$> subnetIds
          ]
      )

instance Core.AWSRequest UpdateSubnetGroup where
  type Rs UpdateSubnetGroup = UpdateSubnetGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDAXV3.UpdateSubnetGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSubnetGroupResponse'
            Core.<$> (x Core..:? "SubnetGroup") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateSubnetGroupResponse' smart constructor.
data UpdateSubnetGroupResponse = UpdateSubnetGroupResponse'
  { -- | The subnet group that has been modified.
    subnetGroup :: Core.Maybe Types.SubnetGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSubnetGroupResponse' value with any optional fields omitted.
mkUpdateSubnetGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateSubnetGroupResponse
mkUpdateSubnetGroupResponse responseStatus =
  UpdateSubnetGroupResponse'
    { subnetGroup = Core.Nothing,
      responseStatus
    }

-- | The subnet group that has been modified.
--
-- /Note:/ Consider using 'subnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrrsSubnetGroup :: Lens.Lens' UpdateSubnetGroupResponse (Core.Maybe Types.SubnetGroup)
usgrrsSubnetGroup = Lens.field @"subnetGroup"
{-# DEPRECATED usgrrsSubnetGroup "Use generic-lens or generic-optics with 'subnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrrsResponseStatus :: Lens.Lens' UpdateSubnetGroupResponse Core.Int
usgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
