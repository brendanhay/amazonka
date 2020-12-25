{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceCreationProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the default properties used to create WorkSpaces.
module Network.AWS.WorkSpaces.ModifyWorkspaceCreationProperties
  ( -- * Creating a request
    ModifyWorkspaceCreationProperties (..),
    mkModifyWorkspaceCreationProperties,

    -- ** Request lenses
    mwcpResourceId,
    mwcpWorkspaceCreationProperties,

    -- * Destructuring the response
    ModifyWorkspaceCreationPropertiesResponse (..),
    mkModifyWorkspaceCreationPropertiesResponse,

    -- ** Response lenses
    mwcprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkModifyWorkspaceCreationProperties' smart constructor.
data ModifyWorkspaceCreationProperties = ModifyWorkspaceCreationProperties'
  { -- | The identifier of the directory.
    resourceId :: Types.ResourceId,
    -- | The default properties for creating WorkSpaces.
    workspaceCreationProperties :: Types.WorkspaceCreationProperties
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyWorkspaceCreationProperties' value with any optional fields omitted.
mkModifyWorkspaceCreationProperties ::
  -- | 'resourceId'
  Types.ResourceId ->
  -- | 'workspaceCreationProperties'
  Types.WorkspaceCreationProperties ->
  ModifyWorkspaceCreationProperties
mkModifyWorkspaceCreationProperties
  resourceId
  workspaceCreationProperties =
    ModifyWorkspaceCreationProperties'
      { resourceId,
        workspaceCreationProperties
      }

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwcpResourceId :: Lens.Lens' ModifyWorkspaceCreationProperties Types.ResourceId
mwcpResourceId = Lens.field @"resourceId"
{-# DEPRECATED mwcpResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The default properties for creating WorkSpaces.
--
-- /Note:/ Consider using 'workspaceCreationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwcpWorkspaceCreationProperties :: Lens.Lens' ModifyWorkspaceCreationProperties Types.WorkspaceCreationProperties
mwcpWorkspaceCreationProperties = Lens.field @"workspaceCreationProperties"
{-# DEPRECATED mwcpWorkspaceCreationProperties "Use generic-lens or generic-optics with 'workspaceCreationProperties' instead." #-}

instance Core.FromJSON ModifyWorkspaceCreationProperties where
  toJSON ModifyWorkspaceCreationProperties {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceId" Core..= resourceId),
            Core.Just
              ( "WorkspaceCreationProperties"
                  Core..= workspaceCreationProperties
              )
          ]
      )

instance Core.AWSRequest ModifyWorkspaceCreationProperties where
  type
    Rs ModifyWorkspaceCreationProperties =
      ModifyWorkspaceCreationPropertiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "WorkspacesService.ModifyWorkspaceCreationProperties"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyWorkspaceCreationPropertiesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyWorkspaceCreationPropertiesResponse' smart constructor.
newtype ModifyWorkspaceCreationPropertiesResponse = ModifyWorkspaceCreationPropertiesResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyWorkspaceCreationPropertiesResponse' value with any optional fields omitted.
mkModifyWorkspaceCreationPropertiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyWorkspaceCreationPropertiesResponse
mkModifyWorkspaceCreationPropertiesResponse responseStatus =
  ModifyWorkspaceCreationPropertiesResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwcprrsResponseStatus :: Lens.Lens' ModifyWorkspaceCreationPropertiesResponse Core.Int
mwcprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mwcprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
