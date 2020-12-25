{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DeleteSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subnet group.
module Network.AWS.DAX.DeleteSubnetGroup
  ( -- * Creating a request
    DeleteSubnetGroup (..),
    mkDeleteSubnetGroup,

    -- ** Request lenses
    dsgSubnetGroupName,

    -- * Destructuring the response
    DeleteSubnetGroupResponse (..),
    mkDeleteSubnetGroupResponse,

    -- ** Response lenses
    dsgrrsDeletionMessage,
    dsgrrsResponseStatus,
  )
where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSubnetGroup' smart constructor.
newtype DeleteSubnetGroup = DeleteSubnetGroup'
  { -- | The name of the subnet group to delete.
    subnetGroupName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSubnetGroup' value with any optional fields omitted.
mkDeleteSubnetGroup ::
  -- | 'subnetGroupName'
  Types.String ->
  DeleteSubnetGroup
mkDeleteSubnetGroup subnetGroupName =
  DeleteSubnetGroup' {subnetGroupName}

-- | The name of the subnet group to delete.
--
-- /Note:/ Consider using 'subnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgSubnetGroupName :: Lens.Lens' DeleteSubnetGroup Types.String
dsgSubnetGroupName = Lens.field @"subnetGroupName"
{-# DEPRECATED dsgSubnetGroupName "Use generic-lens or generic-optics with 'subnetGroupName' instead." #-}

instance Core.FromJSON DeleteSubnetGroup where
  toJSON DeleteSubnetGroup {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SubnetGroupName" Core..= subnetGroupName)]
      )

instance Core.AWSRequest DeleteSubnetGroup where
  type Rs DeleteSubnetGroup = DeleteSubnetGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDAXV3.DeleteSubnetGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSubnetGroupResponse'
            Core.<$> (x Core..:? "DeletionMessage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteSubnetGroupResponse' smart constructor.
data DeleteSubnetGroupResponse = DeleteSubnetGroupResponse'
  { -- | A user-specified message for this action (i.e., a reason for deleting the subnet group).
    deletionMessage :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSubnetGroupResponse' value with any optional fields omitted.
mkDeleteSubnetGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteSubnetGroupResponse
mkDeleteSubnetGroupResponse responseStatus =
  DeleteSubnetGroupResponse'
    { deletionMessage = Core.Nothing,
      responseStatus
    }

-- | A user-specified message for this action (i.e., a reason for deleting the subnet group).
--
-- /Note:/ Consider using 'deletionMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrrsDeletionMessage :: Lens.Lens' DeleteSubnetGroupResponse (Core.Maybe Types.String)
dsgrrsDeletionMessage = Lens.field @"deletionMessage"
{-# DEPRECATED dsgrrsDeletionMessage "Use generic-lens or generic-optics with 'deletionMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrrsResponseStatus :: Lens.Lens' DeleteSubnetGroupResponse Core.Int
dsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
