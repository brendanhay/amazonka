{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceAccessProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies which devices and operating systems users can use to access their WorkSpaces. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/update-directory-details.html#control-device-access Control Device Access> .
module Network.AWS.WorkSpaces.ModifyWorkspaceAccessProperties
  ( -- * Creating a request
    ModifyWorkspaceAccessProperties (..),
    mkModifyWorkspaceAccessProperties,

    -- ** Request lenses
    mwapResourceId,
    mwapWorkspaceAccessProperties,

    -- * Destructuring the response
    ModifyWorkspaceAccessPropertiesResponse (..),
    mkModifyWorkspaceAccessPropertiesResponse,

    -- ** Response lenses
    mwaprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkModifyWorkspaceAccessProperties' smart constructor.
data ModifyWorkspaceAccessProperties = ModifyWorkspaceAccessProperties'
  { -- | The identifier of the directory.
    resourceId :: Types.DirectoryId,
    -- | The device types and operating systems to enable or disable for access.
    workspaceAccessProperties :: Types.WorkspaceAccessProperties
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyWorkspaceAccessProperties' value with any optional fields omitted.
mkModifyWorkspaceAccessProperties ::
  -- | 'resourceId'
  Types.DirectoryId ->
  -- | 'workspaceAccessProperties'
  Types.WorkspaceAccessProperties ->
  ModifyWorkspaceAccessProperties
mkModifyWorkspaceAccessProperties
  resourceId
  workspaceAccessProperties =
    ModifyWorkspaceAccessProperties'
      { resourceId,
        workspaceAccessProperties
      }

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwapResourceId :: Lens.Lens' ModifyWorkspaceAccessProperties Types.DirectoryId
mwapResourceId = Lens.field @"resourceId"
{-# DEPRECATED mwapResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The device types and operating systems to enable or disable for access.
--
-- /Note:/ Consider using 'workspaceAccessProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwapWorkspaceAccessProperties :: Lens.Lens' ModifyWorkspaceAccessProperties Types.WorkspaceAccessProperties
mwapWorkspaceAccessProperties = Lens.field @"workspaceAccessProperties"
{-# DEPRECATED mwapWorkspaceAccessProperties "Use generic-lens or generic-optics with 'workspaceAccessProperties' instead." #-}

instance Core.FromJSON ModifyWorkspaceAccessProperties where
  toJSON ModifyWorkspaceAccessProperties {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceId" Core..= resourceId),
            Core.Just
              ("WorkspaceAccessProperties" Core..= workspaceAccessProperties)
          ]
      )

instance Core.AWSRequest ModifyWorkspaceAccessProperties where
  type
    Rs ModifyWorkspaceAccessProperties =
      ModifyWorkspaceAccessPropertiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "WorkspacesService.ModifyWorkspaceAccessProperties"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyWorkspaceAccessPropertiesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyWorkspaceAccessPropertiesResponse' smart constructor.
newtype ModifyWorkspaceAccessPropertiesResponse = ModifyWorkspaceAccessPropertiesResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyWorkspaceAccessPropertiesResponse' value with any optional fields omitted.
mkModifyWorkspaceAccessPropertiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyWorkspaceAccessPropertiesResponse
mkModifyWorkspaceAccessPropertiesResponse responseStatus =
  ModifyWorkspaceAccessPropertiesResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwaprrsResponseStatus :: Lens.Lens' ModifyWorkspaceAccessPropertiesResponse Core.Int
mwaprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mwaprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
