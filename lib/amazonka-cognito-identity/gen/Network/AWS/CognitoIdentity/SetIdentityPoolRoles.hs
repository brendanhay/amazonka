{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.SetIdentityPoolRoles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the roles for an identity pool. These roles are used when making calls to 'GetCredentialsForIdentity' action.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.SetIdentityPoolRoles
    (
    -- * Creating a request
      SetIdentityPoolRoles (..)
    , mkSetIdentityPoolRoles
    -- ** Request lenses
    , siprIdentityPoolId
    , siprRoles
    , siprRoleMappings

    -- * Destructuring the response
    , SetIdentityPoolRolesResponse (..)
    , mkSetIdentityPoolRolesResponse
    ) where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @SetIdentityPoolRoles@ action.
--
-- /See:/ 'mkSetIdentityPoolRoles' smart constructor.
data SetIdentityPoolRoles = SetIdentityPoolRoles'
  { identityPoolId :: Types.IdentityPoolId
    -- ^ An identity pool ID in the format REGION:GUID.
  , roles :: Core.HashMap Types.RoleType Types.ARNString
    -- ^ The map of roles associated with this pool. For a given role, the key will be either "authenticated" or "unauthenticated" and the value will be the Role ARN.
  , roleMappings :: Core.Maybe (Core.HashMap Types.IdentityProviderName Types.RoleMapping)
    -- ^ How users for a specific identity provider are to mapped to roles. This is a string to 'RoleMapping' object map. The string identifies the identity provider, for example, "graph.facebook.com" or "cognito-idp-east-1.amazonaws.com/us-east-1_abcdefghi:app_client_id".
--
-- Up to 25 rules can be specified per identity provider.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetIdentityPoolRoles' value with any optional fields omitted.
mkSetIdentityPoolRoles
    :: Types.IdentityPoolId -- ^ 'identityPoolId'
    -> SetIdentityPoolRoles
mkSetIdentityPoolRoles identityPoolId
  = SetIdentityPoolRoles'{identityPoolId, roles = Core.mempty,
                          roleMappings = Core.Nothing}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siprIdentityPoolId :: Lens.Lens' SetIdentityPoolRoles Types.IdentityPoolId
siprIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE siprIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

-- | The map of roles associated with this pool. For a given role, the key will be either "authenticated" or "unauthenticated" and the value will be the Role ARN.
--
-- /Note:/ Consider using 'roles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siprRoles :: Lens.Lens' SetIdentityPoolRoles (Core.HashMap Types.RoleType Types.ARNString)
siprRoles = Lens.field @"roles"
{-# INLINEABLE siprRoles #-}
{-# DEPRECATED roles "Use generic-lens or generic-optics with 'roles' instead"  #-}

-- | How users for a specific identity provider are to mapped to roles. This is a string to 'RoleMapping' object map. The string identifies the identity provider, for example, "graph.facebook.com" or "cognito-idp-east-1.amazonaws.com/us-east-1_abcdefghi:app_client_id".
--
-- Up to 25 rules can be specified per identity provider.
--
-- /Note:/ Consider using 'roleMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siprRoleMappings :: Lens.Lens' SetIdentityPoolRoles (Core.Maybe (Core.HashMap Types.IdentityProviderName Types.RoleMapping))
siprRoleMappings = Lens.field @"roleMappings"
{-# INLINEABLE siprRoleMappings #-}
{-# DEPRECATED roleMappings "Use generic-lens or generic-optics with 'roleMappings' instead"  #-}

instance Core.ToQuery SetIdentityPoolRoles where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetIdentityPoolRoles where
        toHeaders SetIdentityPoolRoles{..}
          = Core.pure
              ("X-Amz-Target", "AWSCognitoIdentityService.SetIdentityPoolRoles")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SetIdentityPoolRoles where
        toJSON SetIdentityPoolRoles{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IdentityPoolId" Core..= identityPoolId),
                  Core.Just ("Roles" Core..= roles),
                  ("RoleMappings" Core..=) Core.<$> roleMappings])

instance Core.AWSRequest SetIdentityPoolRoles where
        type Rs SetIdentityPoolRoles = SetIdentityPoolRolesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull SetIdentityPoolRolesResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetIdentityPoolRolesResponse' smart constructor.
data SetIdentityPoolRolesResponse = SetIdentityPoolRolesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetIdentityPoolRolesResponse' value with any optional fields omitted.
mkSetIdentityPoolRolesResponse
    :: SetIdentityPoolRolesResponse
mkSetIdentityPoolRolesResponse = SetIdentityPoolRolesResponse'
