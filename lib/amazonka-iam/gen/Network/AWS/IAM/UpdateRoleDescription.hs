{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateRoleDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use 'UpdateRole' instead.
--
-- Modifies only the description of a role. This operation performs the same function as the @Description@ parameter in the @UpdateRole@ operation.
module Network.AWS.IAM.UpdateRoleDescription
  ( -- * Creating a request
    UpdateRoleDescription (..),
    mkUpdateRoleDescription,

    -- ** Request lenses
    urdRoleName,
    urdDescription,

    -- * Destructuring the response
    UpdateRoleDescriptionResponse (..),
    mkUpdateRoleDescriptionResponse,

    -- ** Response lenses
    urdrrsRole,
    urdrrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateRoleDescription' smart constructor.
data UpdateRoleDescription = UpdateRoleDescription'
  { -- | The name of the role that you want to modify.
    roleName :: Types.RoleName,
    -- | The new description that you want to apply to the specified role.
    description :: Types.RoleDescriptionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoleDescription' value with any optional fields omitted.
mkUpdateRoleDescription ::
  -- | 'roleName'
  Types.RoleName ->
  -- | 'description'
  Types.RoleDescriptionType ->
  UpdateRoleDescription
mkUpdateRoleDescription roleName description =
  UpdateRoleDescription' {roleName, description}

-- | The name of the role that you want to modify.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdRoleName :: Lens.Lens' UpdateRoleDescription Types.RoleName
urdRoleName = Lens.field @"roleName"
{-# DEPRECATED urdRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The new description that you want to apply to the specified role.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdDescription :: Lens.Lens' UpdateRoleDescription Types.RoleDescriptionType
urdDescription = Lens.field @"description"
{-# DEPRECATED urdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.AWSRequest UpdateRoleDescription where
  type Rs UpdateRoleDescription = UpdateRoleDescriptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "UpdateRoleDescription")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "RoleName" roleName)
                Core.<> (Core.toQueryValue "Description" description)
            )
      }
  response =
    Response.receiveXMLWrapper
      "UpdateRoleDescriptionResult"
      ( \s h x ->
          UpdateRoleDescriptionResponse'
            Core.<$> (x Core..@? "Role") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateRoleDescriptionResponse' smart constructor.
data UpdateRoleDescriptionResponse = UpdateRoleDescriptionResponse'
  { -- | A structure that contains details about the modified role.
    role' :: Core.Maybe Types.Role,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateRoleDescriptionResponse' value with any optional fields omitted.
mkUpdateRoleDescriptionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateRoleDescriptionResponse
mkUpdateRoleDescriptionResponse responseStatus =
  UpdateRoleDescriptionResponse'
    { role' = Core.Nothing,
      responseStatus
    }

-- | A structure that contains details about the modified role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdrrsRole :: Lens.Lens' UpdateRoleDescriptionResponse (Core.Maybe Types.Role)
urdrrsRole = Lens.field @"role'"
{-# DEPRECATED urdrrsRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdrrsResponseStatus :: Lens.Lens' UpdateRoleDescriptionResponse Core.Int
urdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
