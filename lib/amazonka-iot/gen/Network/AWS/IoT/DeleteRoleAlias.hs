{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteRoleAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a role alias
module Network.AWS.IoT.DeleteRoleAlias
  ( -- * Creating a request
    DeleteRoleAlias (..),
    mkDeleteRoleAlias,

    -- ** Request lenses
    dRoleAlias,

    -- * Destructuring the response
    DeleteRoleAliasResponse (..),
    mkDeleteRoleAliasResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRoleAlias' smart constructor.
newtype DeleteRoleAlias = DeleteRoleAlias'
  { -- | The role alias to delete.
    roleAlias :: Types.RoleAlias
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRoleAlias' value with any optional fields omitted.
mkDeleteRoleAlias ::
  -- | 'roleAlias'
  Types.RoleAlias ->
  DeleteRoleAlias
mkDeleteRoleAlias roleAlias = DeleteRoleAlias' {roleAlias}

-- | The role alias to delete.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRoleAlias :: Lens.Lens' DeleteRoleAlias Types.RoleAlias
dRoleAlias = Lens.field @"roleAlias"
{-# DEPRECATED dRoleAlias "Use generic-lens or generic-optics with 'roleAlias' instead." #-}

instance Core.AWSRequest DeleteRoleAlias where
  type Rs DeleteRoleAlias = DeleteRoleAliasResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath ("/role-aliases/" Core.<> (Core.toText roleAlias)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRoleAliasResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteRoleAliasResponse' smart constructor.
newtype DeleteRoleAliasResponse = DeleteRoleAliasResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRoleAliasResponse' value with any optional fields omitted.
mkDeleteRoleAliasResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteRoleAliasResponse
mkDeleteRoleAliasResponse responseStatus =
  DeleteRoleAliasResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteRoleAliasResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
