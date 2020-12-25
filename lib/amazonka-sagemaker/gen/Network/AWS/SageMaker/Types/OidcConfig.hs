{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.OidcConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OidcConfig
  ( OidcConfig (..),

    -- * Smart constructor
    mkOidcConfig,

    -- * Lenses
    ocClientId,
    ocClientSecret,
    ocIssuer,
    ocAuthorizationEndpoint,
    ocTokenEndpoint,
    ocUserInfoEndpoint,
    ocLogoutEndpoint,
    ocJwksUri,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ClientId as Types
import qualified Network.AWS.SageMaker.Types.ClientSecret as Types
import qualified Network.AWS.SageMaker.Types.OidcEndpoint as Types

-- | Use this parameter to configure your OIDC Identity Provider (IdP).
--
-- /See:/ 'mkOidcConfig' smart constructor.
data OidcConfig = OidcConfig'
  { -- | The OIDC IdP client ID used to configure your private workforce.
    clientId :: Types.ClientId,
    -- | The OIDC IdP client secret used to configure your private workforce.
    clientSecret :: Types.ClientSecret,
    -- | The OIDC IdP issuer used to configure your private workforce.
    issuer :: Types.OidcEndpoint,
    -- | The OIDC IdP authorization endpoint used to configure your private workforce.
    authorizationEndpoint :: Types.OidcEndpoint,
    -- | The OIDC IdP token endpoint used to configure your private workforce.
    tokenEndpoint :: Types.OidcEndpoint,
    -- | The OIDC IdP user information endpoint used to configure your private workforce.
    userInfoEndpoint :: Types.OidcEndpoint,
    -- | The OIDC IdP logout endpoint used to configure your private workforce.
    logoutEndpoint :: Types.OidcEndpoint,
    -- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private workforce.
    jwksUri :: Types.OidcEndpoint
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OidcConfig' value with any optional fields omitted.
mkOidcConfig ::
  -- | 'clientId'
  Types.ClientId ->
  -- | 'clientSecret'
  Types.ClientSecret ->
  -- | 'issuer'
  Types.OidcEndpoint ->
  -- | 'authorizationEndpoint'
  Types.OidcEndpoint ->
  -- | 'tokenEndpoint'
  Types.OidcEndpoint ->
  -- | 'userInfoEndpoint'
  Types.OidcEndpoint ->
  -- | 'logoutEndpoint'
  Types.OidcEndpoint ->
  -- | 'jwksUri'
  Types.OidcEndpoint ->
  OidcConfig
mkOidcConfig
  clientId
  clientSecret
  issuer
  authorizationEndpoint
  tokenEndpoint
  userInfoEndpoint
  logoutEndpoint
  jwksUri =
    OidcConfig'
      { clientId,
        clientSecret,
        issuer,
        authorizationEndpoint,
        tokenEndpoint,
        userInfoEndpoint,
        logoutEndpoint,
        jwksUri
      }

-- | The OIDC IdP client ID used to configure your private workforce.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocClientId :: Lens.Lens' OidcConfig Types.ClientId
ocClientId = Lens.field @"clientId"
{-# DEPRECATED ocClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The OIDC IdP client secret used to configure your private workforce.
--
-- /Note:/ Consider using 'clientSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocClientSecret :: Lens.Lens' OidcConfig Types.ClientSecret
ocClientSecret = Lens.field @"clientSecret"
{-# DEPRECATED ocClientSecret "Use generic-lens or generic-optics with 'clientSecret' instead." #-}

-- | The OIDC IdP issuer used to configure your private workforce.
--
-- /Note:/ Consider using 'issuer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocIssuer :: Lens.Lens' OidcConfig Types.OidcEndpoint
ocIssuer = Lens.field @"issuer"
{-# DEPRECATED ocIssuer "Use generic-lens or generic-optics with 'issuer' instead." #-}

-- | The OIDC IdP authorization endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'authorizationEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocAuthorizationEndpoint :: Lens.Lens' OidcConfig Types.OidcEndpoint
ocAuthorizationEndpoint = Lens.field @"authorizationEndpoint"
{-# DEPRECATED ocAuthorizationEndpoint "Use generic-lens or generic-optics with 'authorizationEndpoint' instead." #-}

-- | The OIDC IdP token endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'tokenEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocTokenEndpoint :: Lens.Lens' OidcConfig Types.OidcEndpoint
ocTokenEndpoint = Lens.field @"tokenEndpoint"
{-# DEPRECATED ocTokenEndpoint "Use generic-lens or generic-optics with 'tokenEndpoint' instead." #-}

-- | The OIDC IdP user information endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'userInfoEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocUserInfoEndpoint :: Lens.Lens' OidcConfig Types.OidcEndpoint
ocUserInfoEndpoint = Lens.field @"userInfoEndpoint"
{-# DEPRECATED ocUserInfoEndpoint "Use generic-lens or generic-optics with 'userInfoEndpoint' instead." #-}

-- | The OIDC IdP logout endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'logoutEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocLogoutEndpoint :: Lens.Lens' OidcConfig Types.OidcEndpoint
ocLogoutEndpoint = Lens.field @"logoutEndpoint"
{-# DEPRECATED ocLogoutEndpoint "Use generic-lens or generic-optics with 'logoutEndpoint' instead." #-}

-- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private workforce.
--
-- /Note:/ Consider using 'jwksUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocJwksUri :: Lens.Lens' OidcConfig Types.OidcEndpoint
ocJwksUri = Lens.field @"jwksUri"
{-# DEPRECATED ocJwksUri "Use generic-lens or generic-optics with 'jwksUri' instead." #-}

instance Core.FromJSON OidcConfig where
  toJSON OidcConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClientId" Core..= clientId),
            Core.Just ("ClientSecret" Core..= clientSecret),
            Core.Just ("Issuer" Core..= issuer),
            Core.Just ("AuthorizationEndpoint" Core..= authorizationEndpoint),
            Core.Just ("TokenEndpoint" Core..= tokenEndpoint),
            Core.Just ("UserInfoEndpoint" Core..= userInfoEndpoint),
            Core.Just ("LogoutEndpoint" Core..= logoutEndpoint),
            Core.Just ("JwksUri" Core..= jwksUri)
          ]
      )
