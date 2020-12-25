{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DeleteParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified parameter group. You cannot delete a parameter group if it is associated with any DAX clusters.
module Network.AWS.DAX.DeleteParameterGroup
  ( -- * Creating a request
    DeleteParameterGroup (..),
    mkDeleteParameterGroup,

    -- ** Request lenses
    dpgParameterGroupName,

    -- * Destructuring the response
    DeleteParameterGroupResponse (..),
    mkDeleteParameterGroupResponse,

    -- ** Response lenses
    dpgrrsDeletionMessage,
    dpgrrsResponseStatus,
  )
where

import qualified Network.AWS.DAX.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteParameterGroup' smart constructor.
newtype DeleteParameterGroup = DeleteParameterGroup'
  { -- | The name of the parameter group to delete.
    parameterGroupName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteParameterGroup' value with any optional fields omitted.
mkDeleteParameterGroup ::
  -- | 'parameterGroupName'
  Types.String ->
  DeleteParameterGroup
mkDeleteParameterGroup parameterGroupName =
  DeleteParameterGroup' {parameterGroupName}

-- | The name of the parameter group to delete.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgParameterGroupName :: Lens.Lens' DeleteParameterGroup Types.String
dpgParameterGroupName = Lens.field @"parameterGroupName"
{-# DEPRECATED dpgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Core.FromJSON DeleteParameterGroup where
  toJSON DeleteParameterGroup {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ParameterGroupName" Core..= parameterGroupName)]
      )

instance Core.AWSRequest DeleteParameterGroup where
  type Rs DeleteParameterGroup = DeleteParameterGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDAXV3.DeleteParameterGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteParameterGroupResponse'
            Core.<$> (x Core..:? "DeletionMessage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteParameterGroupResponse' smart constructor.
data DeleteParameterGroupResponse = DeleteParameterGroupResponse'
  { -- | A user-specified message for this action (i.e., a reason for deleting the parameter group).
    deletionMessage :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteParameterGroupResponse' value with any optional fields omitted.
mkDeleteParameterGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteParameterGroupResponse
mkDeleteParameterGroupResponse responseStatus =
  DeleteParameterGroupResponse'
    { deletionMessage = Core.Nothing,
      responseStatus
    }

-- | A user-specified message for this action (i.e., a reason for deleting the parameter group).
--
-- /Note:/ Consider using 'deletionMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrrsDeletionMessage :: Lens.Lens' DeleteParameterGroupResponse (Core.Maybe Types.String)
dpgrrsDeletionMessage = Lens.field @"deletionMessage"
{-# DEPRECATED dpgrrsDeletionMessage "Use generic-lens or generic-optics with 'deletionMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrrsResponseStatus :: Lens.Lens' DeleteParameterGroupResponse Core.Int
dpgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
