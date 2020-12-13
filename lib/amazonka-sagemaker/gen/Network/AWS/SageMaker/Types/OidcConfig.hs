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
    ocJwksURI,
    ocUserInfoEndpoint,
    ocAuthorizationEndpoint,
    ocTokenEndpoint,
    ocIssuer,
    ocLogoutEndpoint,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Use this parameter to configure your OIDC Identity Provider (IdP).
--
-- /See:/ 'mkOidcConfig' smart constructor.
data OidcConfig = OidcConfig'
  { -- | The OIDC IdP client ID used to configure your private workforce.
    clientId :: Lude.Text,
    -- | The OIDC IdP client secret used to configure your private workforce.
    clientSecret :: Lude.Sensitive Lude.Text,
    -- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private workforce.
    jwksURI :: Lude.Text,
    -- | The OIDC IdP user information endpoint used to configure your private workforce.
    userInfoEndpoint :: Lude.Text,
    -- | The OIDC IdP authorization endpoint used to configure your private workforce.
    authorizationEndpoint :: Lude.Text,
    -- | The OIDC IdP token endpoint used to configure your private workforce.
    tokenEndpoint :: Lude.Text,
    -- | The OIDC IdP issuer used to configure your private workforce.
    issuer :: Lude.Text,
    -- | The OIDC IdP logout endpoint used to configure your private workforce.
    logoutEndpoint :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OidcConfig' with the minimum fields required to make a request.
--
-- * 'clientId' - The OIDC IdP client ID used to configure your private workforce.
-- * 'clientSecret' - The OIDC IdP client secret used to configure your private workforce.
-- * 'jwksURI' - The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private workforce.
-- * 'userInfoEndpoint' - The OIDC IdP user information endpoint used to configure your private workforce.
-- * 'authorizationEndpoint' - The OIDC IdP authorization endpoint used to configure your private workforce.
-- * 'tokenEndpoint' - The OIDC IdP token endpoint used to configure your private workforce.
-- * 'issuer' - The OIDC IdP issuer used to configure your private workforce.
-- * 'logoutEndpoint' - The OIDC IdP logout endpoint used to configure your private workforce.
mkOidcConfig ::
  -- | 'clientId'
  Lude.Text ->
  -- | 'clientSecret'
  Lude.Sensitive Lude.Text ->
  -- | 'jwksURI'
  Lude.Text ->
  -- | 'userInfoEndpoint'
  Lude.Text ->
  -- | 'authorizationEndpoint'
  Lude.Text ->
  -- | 'tokenEndpoint'
  Lude.Text ->
  -- | 'issuer'
  Lude.Text ->
  -- | 'logoutEndpoint'
  Lude.Text ->
  OidcConfig
mkOidcConfig
  pClientId_
  pClientSecret_
  pJwksURI_
  pUserInfoEndpoint_
  pAuthorizationEndpoint_
  pTokenEndpoint_
  pIssuer_
  pLogoutEndpoint_ =
    OidcConfig'
      { clientId = pClientId_,
        clientSecret = pClientSecret_,
        jwksURI = pJwksURI_,
        userInfoEndpoint = pUserInfoEndpoint_,
        authorizationEndpoint = pAuthorizationEndpoint_,
        tokenEndpoint = pTokenEndpoint_,
        issuer = pIssuer_,
        logoutEndpoint = pLogoutEndpoint_
      }

-- | The OIDC IdP client ID used to configure your private workforce.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocClientId :: Lens.Lens' OidcConfig Lude.Text
ocClientId = Lens.lens (clientId :: OidcConfig -> Lude.Text) (\s a -> s {clientId = a} :: OidcConfig)
{-# DEPRECATED ocClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The OIDC IdP client secret used to configure your private workforce.
--
-- /Note:/ Consider using 'clientSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocClientSecret :: Lens.Lens' OidcConfig (Lude.Sensitive Lude.Text)
ocClientSecret = Lens.lens (clientSecret :: OidcConfig -> Lude.Sensitive Lude.Text) (\s a -> s {clientSecret = a} :: OidcConfig)
{-# DEPRECATED ocClientSecret "Use generic-lens or generic-optics with 'clientSecret' instead." #-}

-- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private workforce.
--
-- /Note:/ Consider using 'jwksURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocJwksURI :: Lens.Lens' OidcConfig Lude.Text
ocJwksURI = Lens.lens (jwksURI :: OidcConfig -> Lude.Text) (\s a -> s {jwksURI = a} :: OidcConfig)
{-# DEPRECATED ocJwksURI "Use generic-lens or generic-optics with 'jwksURI' instead." #-}

-- | The OIDC IdP user information endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'userInfoEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocUserInfoEndpoint :: Lens.Lens' OidcConfig Lude.Text
ocUserInfoEndpoint = Lens.lens (userInfoEndpoint :: OidcConfig -> Lude.Text) (\s a -> s {userInfoEndpoint = a} :: OidcConfig)
{-# DEPRECATED ocUserInfoEndpoint "Use generic-lens or generic-optics with 'userInfoEndpoint' instead." #-}

-- | The OIDC IdP authorization endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'authorizationEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocAuthorizationEndpoint :: Lens.Lens' OidcConfig Lude.Text
ocAuthorizationEndpoint = Lens.lens (authorizationEndpoint :: OidcConfig -> Lude.Text) (\s a -> s {authorizationEndpoint = a} :: OidcConfig)
{-# DEPRECATED ocAuthorizationEndpoint "Use generic-lens or generic-optics with 'authorizationEndpoint' instead." #-}

-- | The OIDC IdP token endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'tokenEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocTokenEndpoint :: Lens.Lens' OidcConfig Lude.Text
ocTokenEndpoint = Lens.lens (tokenEndpoint :: OidcConfig -> Lude.Text) (\s a -> s {tokenEndpoint = a} :: OidcConfig)
{-# DEPRECATED ocTokenEndpoint "Use generic-lens or generic-optics with 'tokenEndpoint' instead." #-}

-- | The OIDC IdP issuer used to configure your private workforce.
--
-- /Note:/ Consider using 'issuer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocIssuer :: Lens.Lens' OidcConfig Lude.Text
ocIssuer = Lens.lens (issuer :: OidcConfig -> Lude.Text) (\s a -> s {issuer = a} :: OidcConfig)
{-# DEPRECATED ocIssuer "Use generic-lens or generic-optics with 'issuer' instead." #-}

-- | The OIDC IdP logout endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'logoutEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocLogoutEndpoint :: Lens.Lens' OidcConfig Lude.Text
ocLogoutEndpoint = Lens.lens (logoutEndpoint :: OidcConfig -> Lude.Text) (\s a -> s {logoutEndpoint = a} :: OidcConfig)
{-# DEPRECATED ocLogoutEndpoint "Use generic-lens or generic-optics with 'logoutEndpoint' instead." #-}

instance Lude.ToJSON OidcConfig where
  toJSON OidcConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClientId" Lude..= clientId),
            Lude.Just ("ClientSecret" Lude..= clientSecret),
            Lude.Just ("JwksUri" Lude..= jwksURI),
            Lude.Just ("UserInfoEndpoint" Lude..= userInfoEndpoint),
            Lude.Just ("AuthorizationEndpoint" Lude..= authorizationEndpoint),
            Lude.Just ("TokenEndpoint" Lude..= tokenEndpoint),
            Lude.Just ("Issuer" Lude..= issuer),
            Lude.Just ("LogoutEndpoint" Lude..= logoutEndpoint)
          ]
      )
