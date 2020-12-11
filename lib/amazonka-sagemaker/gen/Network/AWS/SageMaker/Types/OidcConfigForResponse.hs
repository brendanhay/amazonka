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
    ocfClientId,
    ocfJwksURI,
    ocfUserInfoEndpoint,
    ocfAuthorizationEndpoint,
    ocfTokenEndpoint,
    ocfIssuer,
    ocfLogoutEndpoint,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Your OIDC IdP workforce configuration.
--
-- /See:/ 'mkOidcConfigForResponse' smart constructor.
data OidcConfigForResponse = OidcConfigForResponse'
  { clientId ::
      Lude.Maybe Lude.Text,
    jwksURI :: Lude.Maybe Lude.Text,
    userInfoEndpoint :: Lude.Maybe Lude.Text,
    authorizationEndpoint :: Lude.Maybe Lude.Text,
    tokenEndpoint :: Lude.Maybe Lude.Text,
    issuer :: Lude.Maybe Lude.Text,
    logoutEndpoint :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OidcConfigForResponse' with the minimum fields required to make a request.
--
-- * 'authorizationEndpoint' - The OIDC IdP authorization endpoint used to configure your private workforce.
-- * 'clientId' - The OIDC IdP client ID used to configure your private workforce.
-- * 'issuer' - The OIDC IdP issuer used to configure your private workforce.
-- * 'jwksURI' - The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private workforce.
-- * 'logoutEndpoint' - The OIDC IdP logout endpoint used to configure your private workforce.
-- * 'tokenEndpoint' - The OIDC IdP token endpoint used to configure your private workforce.
-- * 'userInfoEndpoint' - The OIDC IdP user information endpoint used to configure your private workforce.
mkOidcConfigForResponse ::
  OidcConfigForResponse
mkOidcConfigForResponse =
  OidcConfigForResponse'
    { clientId = Lude.Nothing,
      jwksURI = Lude.Nothing,
      userInfoEndpoint = Lude.Nothing,
      authorizationEndpoint = Lude.Nothing,
      tokenEndpoint = Lude.Nothing,
      issuer = Lude.Nothing,
      logoutEndpoint = Lude.Nothing
    }

-- | The OIDC IdP client ID used to configure your private workforce.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocfClientId :: Lens.Lens' OidcConfigForResponse (Lude.Maybe Lude.Text)
ocfClientId = Lens.lens (clientId :: OidcConfigForResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientId = a} :: OidcConfigForResponse)
{-# DEPRECATED ocfClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private workforce.
--
-- /Note:/ Consider using 'jwksURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocfJwksURI :: Lens.Lens' OidcConfigForResponse (Lude.Maybe Lude.Text)
ocfJwksURI = Lens.lens (jwksURI :: OidcConfigForResponse -> Lude.Maybe Lude.Text) (\s a -> s {jwksURI = a} :: OidcConfigForResponse)
{-# DEPRECATED ocfJwksURI "Use generic-lens or generic-optics with 'jwksURI' instead." #-}

-- | The OIDC IdP user information endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'userInfoEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocfUserInfoEndpoint :: Lens.Lens' OidcConfigForResponse (Lude.Maybe Lude.Text)
ocfUserInfoEndpoint = Lens.lens (userInfoEndpoint :: OidcConfigForResponse -> Lude.Maybe Lude.Text) (\s a -> s {userInfoEndpoint = a} :: OidcConfigForResponse)
{-# DEPRECATED ocfUserInfoEndpoint "Use generic-lens or generic-optics with 'userInfoEndpoint' instead." #-}

-- | The OIDC IdP authorization endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'authorizationEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocfAuthorizationEndpoint :: Lens.Lens' OidcConfigForResponse (Lude.Maybe Lude.Text)
ocfAuthorizationEndpoint = Lens.lens (authorizationEndpoint :: OidcConfigForResponse -> Lude.Maybe Lude.Text) (\s a -> s {authorizationEndpoint = a} :: OidcConfigForResponse)
{-# DEPRECATED ocfAuthorizationEndpoint "Use generic-lens or generic-optics with 'authorizationEndpoint' instead." #-}

-- | The OIDC IdP token endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'tokenEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocfTokenEndpoint :: Lens.Lens' OidcConfigForResponse (Lude.Maybe Lude.Text)
ocfTokenEndpoint = Lens.lens (tokenEndpoint :: OidcConfigForResponse -> Lude.Maybe Lude.Text) (\s a -> s {tokenEndpoint = a} :: OidcConfigForResponse)
{-# DEPRECATED ocfTokenEndpoint "Use generic-lens or generic-optics with 'tokenEndpoint' instead." #-}

-- | The OIDC IdP issuer used to configure your private workforce.
--
-- /Note:/ Consider using 'issuer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocfIssuer :: Lens.Lens' OidcConfigForResponse (Lude.Maybe Lude.Text)
ocfIssuer = Lens.lens (issuer :: OidcConfigForResponse -> Lude.Maybe Lude.Text) (\s a -> s {issuer = a} :: OidcConfigForResponse)
{-# DEPRECATED ocfIssuer "Use generic-lens or generic-optics with 'issuer' instead." #-}

-- | The OIDC IdP logout endpoint used to configure your private workforce.
--
-- /Note:/ Consider using 'logoutEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocfLogoutEndpoint :: Lens.Lens' OidcConfigForResponse (Lude.Maybe Lude.Text)
ocfLogoutEndpoint = Lens.lens (logoutEndpoint :: OidcConfigForResponse -> Lude.Maybe Lude.Text) (\s a -> s {logoutEndpoint = a} :: OidcConfigForResponse)
{-# DEPRECATED ocfLogoutEndpoint "Use generic-lens or generic-optics with 'logoutEndpoint' instead." #-}

instance Lude.FromJSON OidcConfigForResponse where
  parseJSON =
    Lude.withObject
      "OidcConfigForResponse"
      ( \x ->
          OidcConfigForResponse'
            Lude.<$> (x Lude..:? "ClientId")
            Lude.<*> (x Lude..:? "JwksUri")
            Lude.<*> (x Lude..:? "UserInfoEndpoint")
            Lude.<*> (x Lude..:? "AuthorizationEndpoint")
            Lude.<*> (x Lude..:? "TokenEndpoint")
            Lude.<*> (x Lude..:? "Issuer")
            Lude.<*> (x Lude..:? "LogoutEndpoint")
      )
