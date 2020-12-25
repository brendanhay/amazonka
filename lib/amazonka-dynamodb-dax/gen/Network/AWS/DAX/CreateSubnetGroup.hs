{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.CreateSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new subnet group.
module Network.AWS.DAX.CreateSubnetGroup
  ( -- * Creating a request
    CreateSubnetGroup (..),
    mkCreateSubnetGroup,

    -- ** Request lenses
    csgSubnetGroupName,
    csgSubnetIds,
    csgDescription,

    -- * Destructuring the response
    CreateSubnetGroupResponse (..),
    mkCreateSubnetGroupResponse,

    -- ** Response lenses
    csgrrsSubnetGroup,
    csgrrsResponseStatus,
  )
where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSubnetGroup' smart constructor.
data CreateSubnetGroup = CreateSubnetGroup'
  { -- | A name for the subnet group. This value is stored as a lowercase string.
    subnetGroupName :: Types.String,
    -- | A list of VPC subnet IDs for the subnet group.
    subnetIds :: [Types.String],
    -- | A description for the subnet group
    description :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSubnetGroup' value with any optional fields omitted.
mkCreateSubnetGroup ::
  -- | 'subnetGroupName'
  Types.String ->
  CreateSubnetGroup
mkCreateSubnetGroup subnetGroupName =
  CreateSubnetGroup'
    { subnetGroupName,
      subnetIds = Core.mempty,
      description = Core.Nothing
    }

-- | A name for the subnet group. This value is stored as a lowercase string.
--
-- /Note:/ Consider using 'subnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgSubnetGroupName :: Lens.Lens' CreateSubnetGroup Types.String
csgSubnetGroupName = Lens.field @"subnetGroupName"
{-# DEPRECATED csgSubnetGroupName "Use generic-lens or generic-optics with 'subnetGroupName' instead." #-}

-- | A list of VPC subnet IDs for the subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgSubnetIds :: Lens.Lens' CreateSubnetGroup [Types.String]
csgSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED csgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | A description for the subnet group
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgDescription :: Lens.Lens' CreateSubnetGroup (Core.Maybe Types.String)
csgDescription = Lens.field @"description"
{-# DEPRECATED csgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON CreateSubnetGroup where
  toJSON CreateSubnetGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SubnetGroupName" Core..= subnetGroupName),
            Core.Just ("SubnetIds" Core..= subnetIds),
            ("Description" Core..=) Core.<$> description
          ]
      )

instance Core.AWSRequest CreateSubnetGroup where
  type Rs CreateSubnetGroup = CreateSubnetGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDAXV3.CreateSubnetGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSubnetGroupResponse'
            Core.<$> (x Core..:? "SubnetGroup") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSubnetGroupResponse' smart constructor.
data CreateSubnetGroupResponse = CreateSubnetGroupResponse'
  { -- | Represents the output of a /CreateSubnetGroup/ operation.
    subnetGroup :: Core.Maybe Types.SubnetGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSubnetGroupResponse' value with any optional fields omitted.
mkCreateSubnetGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSubnetGroupResponse
mkCreateSubnetGroupResponse responseStatus =
  CreateSubnetGroupResponse'
    { subnetGroup = Core.Nothing,
      responseStatus
    }

-- | Represents the output of a /CreateSubnetGroup/ operation.
--
-- /Note:/ Consider using 'subnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgrrsSubnetGroup :: Lens.Lens' CreateSubnetGroupResponse (Core.Maybe Types.SubnetGroup)
csgrrsSubnetGroup = Lens.field @"subnetGroup"
{-# DEPRECATED csgrrsSubnetGroup "Use generic-lens or generic-optics with 'subnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgrrsResponseStatus :: Lens.Lens' CreateSubnetGroupResponse Core.Int
csgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
