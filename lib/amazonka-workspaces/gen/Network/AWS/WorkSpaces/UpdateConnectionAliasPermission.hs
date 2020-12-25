{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.UpdateConnectionAliasPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares or unshares a connection alias with one account by specifying whether that account has permission to associate the connection alias with a directory. If the association permission is granted, the connection alias is shared with that account. If the association permission is revoked, the connection alias is unshared with the account. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
module Network.AWS.WorkSpaces.UpdateConnectionAliasPermission
  ( -- * Creating a request
    UpdateConnectionAliasPermission (..),
    mkUpdateConnectionAliasPermission,

    -- ** Request lenses
    ucapAliasId,
    ucapConnectionAliasPermission,

    -- * Destructuring the response
    UpdateConnectionAliasPermissionResponse (..),
    mkUpdateConnectionAliasPermissionResponse,

    -- ** Response lenses
    ucaprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkUpdateConnectionAliasPermission' smart constructor.
data UpdateConnectionAliasPermission = UpdateConnectionAliasPermission'
  { -- | The identifier of the connection alias that you want to update permissions for.
    aliasId :: Types.AliasId,
    -- | Indicates whether to share or unshare the connection alias with the specified AWS account.
    connectionAliasPermission :: Types.ConnectionAliasPermission
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConnectionAliasPermission' value with any optional fields omitted.
mkUpdateConnectionAliasPermission ::
  -- | 'aliasId'
  Types.AliasId ->
  -- | 'connectionAliasPermission'
  Types.ConnectionAliasPermission ->
  UpdateConnectionAliasPermission
mkUpdateConnectionAliasPermission aliasId connectionAliasPermission =
  UpdateConnectionAliasPermission'
    { aliasId,
      connectionAliasPermission
    }

-- | The identifier of the connection alias that you want to update permissions for.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucapAliasId :: Lens.Lens' UpdateConnectionAliasPermission Types.AliasId
ucapAliasId = Lens.field @"aliasId"
{-# DEPRECATED ucapAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

-- | Indicates whether to share or unshare the connection alias with the specified AWS account.
--
-- /Note:/ Consider using 'connectionAliasPermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucapConnectionAliasPermission :: Lens.Lens' UpdateConnectionAliasPermission Types.ConnectionAliasPermission
ucapConnectionAliasPermission = Lens.field @"connectionAliasPermission"
{-# DEPRECATED ucapConnectionAliasPermission "Use generic-lens or generic-optics with 'connectionAliasPermission' instead." #-}

instance Core.FromJSON UpdateConnectionAliasPermission where
  toJSON UpdateConnectionAliasPermission {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AliasId" Core..= aliasId),
            Core.Just
              ("ConnectionAliasPermission" Core..= connectionAliasPermission)
          ]
      )

instance Core.AWSRequest UpdateConnectionAliasPermission where
  type
    Rs UpdateConnectionAliasPermission =
      UpdateConnectionAliasPermissionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "WorkspacesService.UpdateConnectionAliasPermission"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConnectionAliasPermissionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateConnectionAliasPermissionResponse' smart constructor.
newtype UpdateConnectionAliasPermissionResponse = UpdateConnectionAliasPermissionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConnectionAliasPermissionResponse' value with any optional fields omitted.
mkUpdateConnectionAliasPermissionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateConnectionAliasPermissionResponse
mkUpdateConnectionAliasPermissionResponse responseStatus =
  UpdateConnectionAliasPermissionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaprrsResponseStatus :: Lens.Lens' UpdateConnectionAliasPermissionResponse Core.Int
ucaprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucaprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
