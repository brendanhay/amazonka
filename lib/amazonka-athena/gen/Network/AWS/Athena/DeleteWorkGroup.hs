{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.DeleteWorkGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the workgroup with the specified name. The primary workgroup cannot be deleted.
module Network.AWS.Athena.DeleteWorkGroup
  ( -- * Creating a request
    DeleteWorkGroup (..),
    mkDeleteWorkGroup,

    -- ** Request lenses
    dwgWorkGroup,
    dwgRecursiveDeleteOption,

    -- * Destructuring the response
    DeleteWorkGroupResponse (..),
    mkDeleteWorkGroupResponse,

    -- ** Response lenses
    dwgrrsResponseStatus,
  )
where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteWorkGroup' smart constructor.
data DeleteWorkGroup = DeleteWorkGroup'
  { -- | The unique name of the workgroup to delete.
    workGroup :: Types.WorkGroupName,
    -- | The option to delete the workgroup and its contents even if the workgroup contains any named queries.
    recursiveDeleteOption :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkGroup' value with any optional fields omitted.
mkDeleteWorkGroup ::
  -- | 'workGroup'
  Types.WorkGroupName ->
  DeleteWorkGroup
mkDeleteWorkGroup workGroup =
  DeleteWorkGroup' {workGroup, recursiveDeleteOption = Core.Nothing}

-- | The unique name of the workgroup to delete.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwgWorkGroup :: Lens.Lens' DeleteWorkGroup Types.WorkGroupName
dwgWorkGroup = Lens.field @"workGroup"
{-# DEPRECATED dwgWorkGroup "Use generic-lens or generic-optics with 'workGroup' instead." #-}

-- | The option to delete the workgroup and its contents even if the workgroup contains any named queries.
--
-- /Note:/ Consider using 'recursiveDeleteOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwgRecursiveDeleteOption :: Lens.Lens' DeleteWorkGroup (Core.Maybe Core.Bool)
dwgRecursiveDeleteOption = Lens.field @"recursiveDeleteOption"
{-# DEPRECATED dwgRecursiveDeleteOption "Use generic-lens or generic-optics with 'recursiveDeleteOption' instead." #-}

instance Core.FromJSON DeleteWorkGroup where
  toJSON DeleteWorkGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WorkGroup" Core..= workGroup),
            ("RecursiveDeleteOption" Core..=) Core.<$> recursiveDeleteOption
          ]
      )

instance Core.AWSRequest DeleteWorkGroup where
  type Rs DeleteWorkGroup = DeleteWorkGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonAthena.DeleteWorkGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkGroupResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteWorkGroupResponse' smart constructor.
newtype DeleteWorkGroupResponse = DeleteWorkGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkGroupResponse' value with any optional fields omitted.
mkDeleteWorkGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteWorkGroupResponse
mkDeleteWorkGroupResponse responseStatus =
  DeleteWorkGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwgrrsResponseStatus :: Lens.Lens' DeleteWorkGroupResponse Core.Int
dwgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dwgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
