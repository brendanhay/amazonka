{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified role, including the role's path, GUID, ARN, and the role's trust policy that grants permission to assume the role. For more information about roles, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles> .
module Network.AWS.IAM.GetRole
  ( -- * Creating a request
    GetRole (..),
    mkGetRole,

    -- ** Request lenses
    grRoleName,

    -- * Destructuring the response
    GetRoleResponse (..),
    mkGetRoleResponse,

    -- ** Response lenses
    grrrsRole,
    grrrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRole' smart constructor.
newtype GetRole = GetRole'
  { -- | The name of the IAM role to get information about.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    roleName :: Types.RoleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRole' value with any optional fields omitted.
mkGetRole ::
  -- | 'roleName'
  Types.RoleName ->
  GetRole
mkGetRole roleName = GetRole' {roleName}

-- | The name of the IAM role to get information about.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grRoleName :: Lens.Lens' GetRole Types.RoleName
grRoleName = Lens.field @"roleName"
{-# DEPRECATED grRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

instance Core.AWSRequest GetRole where
  type Rs GetRole = GetRoleResponse
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
            ( Core.pure ("Action", "GetRole")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "RoleName" roleName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetRoleResult"
      ( \s h x ->
          GetRoleResponse'
            Core.<$> (x Core..@ "Role") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'GetRole' request.
--
-- /See:/ 'mkGetRoleResponse' smart constructor.
data GetRoleResponse = GetRoleResponse'
  { -- | A structure containing details about the IAM role.
    role' :: Types.Role,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetRoleResponse' value with any optional fields omitted.
mkGetRoleResponse ::
  -- | 'role\''
  Types.Role ->
  -- | 'responseStatus'
  Core.Int ->
  GetRoleResponse
mkGetRoleResponse role' responseStatus =
  GetRoleResponse' {role', responseStatus}

-- | A structure containing details about the IAM role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsRole :: Lens.Lens' GetRoleResponse Types.Role
grrrsRole = Lens.field @"role'"
{-# DEPRECATED grrrsRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetRoleResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
