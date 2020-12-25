{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.OidcConfigForResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OidcConfigForResponse
  ( OidcConfigForResponse (..),

    -- * Smart constructor
    mkOidcConfigForResponse,

    -- * Lenses
    ocfrAuthorizationEndpoint,
    ocfrClientId,
    ocfrIssuer,
    ocfrJwksUri,
    ocfrLogoutEndpoint,
    ocfrTokenEndpoint,
    ocfrUserInfoEndpoint,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ClientId as Types
import qualified Network.AWS.SageMaker.Types.OidcEndpoint as Types

-- | Your OIDC IdP workforce configuration.
--
-- /See:/ 'mkOidcConfigForResponse' smart constructor.
data OidcConfigForResponse = OidcConfigForResponse'
  { -- | The OIDC IdP authorization endpoint used to configure your private workforce.
    authorizationEndpoint :: Core.Maybe Types.OidcEndpoint,
    -- | The OIDC IdP client ID used to configure your private workforce.
    clientId :: Core.Maybe Types.ClientId,
    -- | The OIDC IdP issuer used to configure your private workforce.
    issuer :: Core.Maybe Types.OidcEndpoint,
    -- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private workforce.
    jwksUri :: Core.Maybe Types.OidcEndpoint,
    -- | The OIDC IdP logout endpoint used to configure your private workforce.
    logoutEndpoint :: Core.Maybe Types.OidcEndpoint,
    -- | The OIDC IdP token endpoint used to configure your private workforce.
    tokenEndpoint :: Core.Maybe Types.OidcEndpoint,
    -- | The OIDC IdP user information endpoint used to configure your private workforce.
    userInfoEndpoint :: Core.Maybe Types.OidcEndpoint
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OidcConfigForResponse' value with any optional fields omitted.
mkOidcConfigForResponse ::
  OidcConfigForResponse
mkOidcConfigForResponse =
  OidcConfigForResponse'
    { authorizationEndpoint = Core.Nothing,
      clientId = Core.Nothing,
      issuer = Core.Nothing,
      jwksUri = Core.Nothing,
      logoutEndpoint = Core.Nothing,
      tokenEndpoint = Core.Nothing,
      userInfoEndpoint = Core.Nothing
    }

-- | The OIDC IdP authorization endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'authorizationEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocfrAuthorizationEndpoint :: Lens.Lens' OidcConfigForResponse (Core.Maybe Types.OidcEndpoint)
ocfrAuthorizationEndpoint = Lens.field @"authorizationEndpoint"
{-# DEPRECATED ocfrAuthorizationEndpoint "Use generic-lens or generic-optics with 'authorizationEndpoint' instead." #-}

-- | The OIDC IdP client ID used to configure your private workforce.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocfrClientId :: Lens.Lens' OidcConfigForResponse (Core.Maybe Types.ClientId)
ocfrClientId = Lens.field @"clientId"
{-# DEPRECATED ocfrClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The OIDC IdP issuer used to configure your private workforce.
--
-- /Note:/ Consider using 'issuer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocfrIssuer :: Lens.Lens' OidcConfigForResponse (Core.Maybe Types.OidcEndpoint)
ocfrIssuer = Lens.field @"issuer"
{-# DEPRECATED ocfrIssuer "Use generic-lens or generic-optics with 'issuer' instead." #-}

-- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private workforce.
--
-- /Note:/ Consider using 'jwksUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocfrJwksUri :: Lens.Lens' OidcConfigForResponse (Core.Maybe Types.OidcEndpoint)
ocfrJwksUri = Lens.field @"jwksUri"
{-# DEPRECATED ocfrJwksUri "Use generic-lens or generic-optics with 'jwksUri' instead." #-}

-- | The OIDC IdP logout endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'logoutEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocfrLogoutEndpoint :: Lens.Lens' OidcConfigForResponse (Core.Maybe Types.OidcEndpoint)
ocfrLogoutEndpoint = Lens.field @"logoutEndpoint"
{-# DEPRECATED ocfrLogoutEndpoint "Use generic-lens or generic-optics with 'logoutEndpoint' instead." #-}

-- | The OIDC IdP token endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'tokenEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocfrTokenEndpoint :: Lens.Lens' OidcConfigForResponse (Core.Maybe Types.OidcEndpoint)
ocfrTokenEndpoint = Lens.field @"tokenEndpoint"
{-# DEPRECATED ocfrTokenEndpoint "Use generic-lens or generic-optics with 'tokenEndpoint' instead." #-}

-- | The OIDC IdP user information endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'userInfoEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocfrUserInfoEndpoint :: Lens.Lens' OidcConfigForResponse (Core.Maybe Types.OidcEndpoint)
ocfrUserInfoEndpoint = Lens.field @"userInfoEndpoint"
{-# DEPRECATED ocfrUserInfoEndpoint "Use generic-lens or generic-optics with 'userInfoEndpoint' instead." #-}

instance Core.FromJSON OidcConfigForResponse where
  parseJSON =
    Core.withObject "OidcConfigForResponse" Core.$
      \x ->
        OidcConfigForResponse'
          Core.<$> (x Core..:? "AuthorizationEndpoint")
          Core.<*> (x Core..:? "ClientId")
          Core.<*> (x Core..:? "Issuer")
          Core.<*> (x Core..:? "JwksUri")
          Core.<*> (x Core..:? "LogoutEndpoint")
          Core.<*> (x Core..:? "TokenEndpoint")
          Core.<*> (x Core..:? "UserInfoEndpoint")
