{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetIdentityPoolRoles (..)
    , mkGetIdentityPoolRoles
    -- ** Request lenses
    , giprIdentityPoolId

    -- * Destructuring the response
    , GetIdentityPoolRolesResponse (..)
    , mkGetIdentityPoolRolesResponse
    -- ** Response lenses
    , giprrrsIdentityPoolId
    , giprrrsRoleMappings
    , giprrrsRoles
    , giprrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @GetIdentityPoolRoles@ action.
--
-- /See:/ 'mkGetIdentityPoolRoles' smart constructor.
newtype GetIdentityPoolRoles = GetIdentityPoolRoles'
  { identityPoolId :: Types.IdentityPoolId
    -- ^ An identity pool ID in the format REGION:GUID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetIdentityPoolRoles' value with any optional fields omitted.
mkGetIdentityPoolRoles
    :: Types.IdentityPoolId -- ^ 'identityPoolId'
    -> GetIdentityPoolRoles
mkGetIdentityPoolRoles identityPoolId
  = GetIdentityPoolRoles'{identityPoolId}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprIdentityPoolId :: Lens.Lens' GetIdentityPoolRoles Types.IdentityPoolId
giprIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE giprIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

instance Core.ToQuery GetIdentityPoolRoles where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetIdentityPoolRoles where
        toHeaders GetIdentityPoolRoles{..}
          = Core.pure
              ("X-Amz-Target", "AWSCognitoIdentityService.GetIdentityPoolRoles")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetIdentityPoolRoles where
        toJSON GetIdentityPoolRoles{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IdentityPoolId" Core..= identityPoolId)])

instance Core.AWSRequest GetIdentityPoolRoles where
        type Rs GetIdentityPoolRoles = GetIdentityPoolRolesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetIdentityPoolRolesResponse' Core.<$>
                   (x Core..:? "IdentityPoolId") Core.<*> x Core..:? "RoleMappings"
                     Core.<*> x Core..:? "Roles"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Returned in response to a successful @GetIdentityPoolRoles@ operation.
--
-- /See:/ 'mkGetIdentityPoolRolesResponse' smart constructor.
data GetIdentityPoolRolesResponse = GetIdentityPoolRolesResponse'
  { identityPoolId :: Core.Maybe Types.IdentityPoolId
    -- ^ An identity pool ID in the format REGION:GUID.
  , roleMappings :: Core.Maybe (Core.HashMap Types.IdentityProviderName Types.RoleMapping)
    -- ^ How users for a specific identity provider are to mapped to roles. This is a String-to-'RoleMapping' object map. The string identifies the identity provider, for example, "graph.facebook.com" or "cognito-idp.us-east-1.amazonaws.com/us-east-1_abcdefghi:app_client_id".
  , roles :: Core.Maybe (Core.HashMap Types.RoleType Types.ARNString)
    -- ^ The map of roles associated with this pool. Currently only authenticated and unauthenticated roles are supported.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetIdentityPoolRolesResponse' value with any optional fields omitted.
mkGetIdentityPoolRolesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetIdentityPoolRolesResponse
mkGetIdentityPoolRolesResponse responseStatus
  = GetIdentityPoolRolesResponse'{identityPoolId = Core.Nothing,
                                  roleMappings = Core.Nothing, roles = Core.Nothing, responseStatus}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrrsIdentityPoolId :: Lens.Lens' GetIdentityPoolRolesResponse (Core.Maybe Types.IdentityPoolId)
giprrrsIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE giprrrsIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

-- | How users for a specific identity provider are to mapped to roles. This is a String-to-'RoleMapping' object map. The string identifies the identity provider, for example, "graph.facebook.com" or "cognito-idp.us-east-1.amazonaws.com/us-east-1_abcdefghi:app_client_id".
--
-- /Note:/ Consider using 'roleMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrrsRoleMappings :: Lens.Lens' GetIdentityPoolRolesResponse (Core.Maybe (Core.HashMap Types.IdentityProviderName Types.RoleMapping))
giprrrsRoleMappings = Lens.field @"roleMappings"
{-# INLINEABLE giprrrsRoleMappings #-}
{-# DEPRECATED roleMappings "Use generic-lens or generic-optics with 'roleMappings' instead"  #-}

-- | The map of roles associated with this pool. Currently only authenticated and unauthenticated roles are supported.
--
-- /Note:/ Consider using 'roles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrrsRoles :: Lens.Lens' GetIdentityPoolRolesResponse (Core.Maybe (Core.HashMap Types.RoleType Types.ARNString))
giprrrsRoles = Lens.field @"roles"
{-# INLINEABLE giprrrsRoles #-}
{-# DEPRECATED roles "Use generic-lens or generic-optics with 'roles' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrrsResponseStatus :: Lens.Lens' GetIdentityPoolRolesResponse Core.Int
giprrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE giprrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
