{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetIdentityPoolRoles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the roles for an identity pool.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.GetIdentityPoolRoles
  ( -- * Creating a request
    GetIdentityPoolRoles (..),
    mkGetIdentityPoolRoles,

    -- ** Request lenses
    giprIdentityPoolId,

    -- * Destructuring the response
    GetIdentityPoolRolesResponse (..),
    mkGetIdentityPoolRolesResponse,

    -- ** Response lenses
    giprrrsIdentityPoolId,
    giprrrsRoleMappings,
    giprrrsRoles,
    giprrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @GetIdentityPoolRoles@ action.
--
-- /See:/ 'mkGetIdentityPoolRoles' smart constructor.
newtype GetIdentityPoolRoles = GetIdentityPoolRoles'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Types.IdentityPoolId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetIdentityPoolRoles' value with any optional fields omitted.
mkGetIdentityPoolRoles ::
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  GetIdentityPoolRoles
mkGetIdentityPoolRoles identityPoolId =
  GetIdentityPoolRoles' {identityPoolId}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprIdentityPoolId :: Lens.Lens' GetIdentityPoolRoles Types.IdentityPoolId
giprIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED giprIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

instance Core.FromJSON GetIdentityPoolRoles where
  toJSON GetIdentityPoolRoles {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("IdentityPoolId" Core..= identityPoolId)]
      )

instance Core.AWSRequest GetIdentityPoolRoles where
  type Rs GetIdentityPoolRoles = GetIdentityPoolRolesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSCognitoIdentityService.GetIdentityPoolRoles")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIdentityPoolRolesResponse'
            Core.<$> (x Core..:? "IdentityPoolId")
            Core.<*> (x Core..:? "RoleMappings")
            Core.<*> (x Core..:? "Roles")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Returned in response to a successful @GetIdentityPoolRoles@ operation.
--
-- /See:/ 'mkGetIdentityPoolRolesResponse' smart constructor.
data GetIdentityPoolRolesResponse = GetIdentityPoolRolesResponse'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Core.Maybe Types.IdentityPoolId,
    -- | How users for a specific identity provider are to mapped to roles. This is a String-to-'RoleMapping' object map. The string identifies the identity provider, for example, "graph.facebook.com" or "cognito-idp.us-east-1.amazonaws.com/us-east-1_abcdefghi:app_client_id".
    roleMappings :: Core.Maybe (Core.HashMap Types.IdentityProviderName Types.RoleMapping),
    -- | The map of roles associated with this pool. Currently only authenticated and unauthenticated roles are supported.
    roles :: Core.Maybe (Core.HashMap Types.RoleType Types.ARNString),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIdentityPoolRolesResponse' value with any optional fields omitted.
mkGetIdentityPoolRolesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetIdentityPoolRolesResponse
mkGetIdentityPoolRolesResponse responseStatus =
  GetIdentityPoolRolesResponse'
    { identityPoolId = Core.Nothing,
      roleMappings = Core.Nothing,
      roles = Core.Nothing,
      responseStatus
    }

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrrsIdentityPoolId :: Lens.Lens' GetIdentityPoolRolesResponse (Core.Maybe Types.IdentityPoolId)
giprrrsIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED giprrrsIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | How users for a specific identity provider are to mapped to roles. This is a String-to-'RoleMapping' object map. The string identifies the identity provider, for example, "graph.facebook.com" or "cognito-idp.us-east-1.amazonaws.com/us-east-1_abcdefghi:app_client_id".
--
-- /Note:/ Consider using 'roleMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrrsRoleMappings :: Lens.Lens' GetIdentityPoolRolesResponse (Core.Maybe (Core.HashMap Types.IdentityProviderName Types.RoleMapping))
giprrrsRoleMappings = Lens.field @"roleMappings"
{-# DEPRECATED giprrrsRoleMappings "Use generic-lens or generic-optics with 'roleMappings' instead." #-}

-- | The map of roles associated with this pool. Currently only authenticated and unauthenticated roles are supported.
--
-- /Note:/ Consider using 'roles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrrsRoles :: Lens.Lens' GetIdentityPoolRolesResponse (Core.Maybe (Core.HashMap Types.RoleType Types.ARNString))
giprrrsRoles = Lens.field @"roles"
{-# DEPRECATED giprrrsRoles "Use generic-lens or generic-optics with 'roles' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrrsResponseStatus :: Lens.Lens' GetIdentityPoolRolesResponse Core.Int
giprrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED giprrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
