{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified WorkSpace properties. For important information about how to modify the size of the root and user volumes, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace> .
module Network.AWS.WorkSpaces.ModifyWorkspaceProperties
  ( -- * Creating a request
    ModifyWorkspaceProperties (..),
    mkModifyWorkspaceProperties,

    -- ** Request lenses
    mwpWorkspaceId,
    mwpWorkspaceProperties,

    -- * Destructuring the response
    ModifyWorkspacePropertiesResponse (..),
    mkModifyWorkspacePropertiesResponse,

    -- ** Response lenses
    mwprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkModifyWorkspaceProperties' smart constructor.
data ModifyWorkspaceProperties = ModifyWorkspaceProperties'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Types.WorkspaceId,
    -- | The properties of the WorkSpace.
    workspaceProperties :: Types.WorkspaceProperties
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyWorkspaceProperties' value with any optional fields omitted.
mkModifyWorkspaceProperties ::
  -- | 'workspaceId'
  Types.WorkspaceId ->
  -- | 'workspaceProperties'
  Types.WorkspaceProperties ->
  ModifyWorkspaceProperties
mkModifyWorkspaceProperties workspaceId workspaceProperties =
  ModifyWorkspaceProperties' {workspaceId, workspaceProperties}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwpWorkspaceId :: Lens.Lens' ModifyWorkspaceProperties Types.WorkspaceId
mwpWorkspaceId = Lens.field @"workspaceId"
{-# DEPRECATED mwpWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

-- | The properties of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwpWorkspaceProperties :: Lens.Lens' ModifyWorkspaceProperties Types.WorkspaceProperties
mwpWorkspaceProperties = Lens.field @"workspaceProperties"
{-# DEPRECATED mwpWorkspaceProperties "Use generic-lens or generic-optics with 'workspaceProperties' instead." #-}

instance Core.FromJSON ModifyWorkspaceProperties where
  toJSON ModifyWorkspaceProperties {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WorkspaceId" Core..= workspaceId),
            Core.Just ("WorkspaceProperties" Core..= workspaceProperties)
          ]
      )

instance Core.AWSRequest ModifyWorkspaceProperties where
  type
    Rs ModifyWorkspaceProperties =
      ModifyWorkspacePropertiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkspacesService.ModifyWorkspaceProperties")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyWorkspacePropertiesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyWorkspacePropertiesResponse' smart constructor.
newtype ModifyWorkspacePropertiesResponse = ModifyWorkspacePropertiesResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyWorkspacePropertiesResponse' value with any optional fields omitted.
mkModifyWorkspacePropertiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyWorkspacePropertiesResponse
mkModifyWorkspacePropertiesResponse responseStatus =
  ModifyWorkspacePropertiesResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwprrsResponseStatus :: Lens.Lens' ModifyWorkspacePropertiesResponse Core.Int
mwprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mwprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
