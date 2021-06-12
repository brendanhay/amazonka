{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.OidcConfigForResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OidcConfigForResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Your OIDC IdP workforce configuration.
--
-- /See:/ 'newOidcConfigForResponse' smart constructor.
data OidcConfigForResponse = OidcConfigForResponse'
  { -- | The OIDC IdP client ID used to configure your private workforce.
    clientId :: Core.Maybe Core.Text,
    -- | The OIDC IdP token endpoint used to configure your private workforce.
    tokenEndpoint :: Core.Maybe Core.Text,
    -- | The OIDC IdP authorization endpoint used to configure your private
    -- workforce.
    authorizationEndpoint :: Core.Maybe Core.Text,
    -- | The OIDC IdP user information endpoint used to configure your private
    -- workforce.
    userInfoEndpoint :: Core.Maybe Core.Text,
    -- | The OIDC IdP logout endpoint used to configure your private workforce.
    logoutEndpoint :: Core.Maybe Core.Text,
    -- | The OIDC IdP issuer used to configure your private workforce.
    issuer :: Core.Maybe Core.Text,
    -- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private
    -- workforce.
    jwksUri :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OidcConfigForResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'oidcConfigForResponse_clientId' - The OIDC IdP client ID used to configure your private workforce.
--
-- 'tokenEndpoint', 'oidcConfigForResponse_tokenEndpoint' - The OIDC IdP token endpoint used to configure your private workforce.
--
-- 'authorizationEndpoint', 'oidcConfigForResponse_authorizationEndpoint' - The OIDC IdP authorization endpoint used to configure your private
-- workforce.
--
-- 'userInfoEndpoint', 'oidcConfigForResponse_userInfoEndpoint' - The OIDC IdP user information endpoint used to configure your private
-- workforce.
--
-- 'logoutEndpoint', 'oidcConfigForResponse_logoutEndpoint' - The OIDC IdP logout endpoint used to configure your private workforce.
--
-- 'issuer', 'oidcConfigForResponse_issuer' - The OIDC IdP issuer used to configure your private workforce.
--
-- 'jwksUri', 'oidcConfigForResponse_jwksUri' - The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private
-- workforce.
newOidcConfigForResponse ::
  OidcConfigForResponse
newOidcConfigForResponse =
  OidcConfigForResponse'
    { clientId = Core.Nothing,
      tokenEndpoint = Core.Nothing,
      authorizationEndpoint = Core.Nothing,
      userInfoEndpoint = Core.Nothing,
      logoutEndpoint = Core.Nothing,
      issuer = Core.Nothing,
      jwksUri = Core.Nothing
    }

-- | The OIDC IdP client ID used to configure your private workforce.
oidcConfigForResponse_clientId :: Lens.Lens' OidcConfigForResponse (Core.Maybe Core.Text)
oidcConfigForResponse_clientId = Lens.lens (\OidcConfigForResponse' {clientId} -> clientId) (\s@OidcConfigForResponse' {} a -> s {clientId = a} :: OidcConfigForResponse)

-- | The OIDC IdP token endpoint used to configure your private workforce.
oidcConfigForResponse_tokenEndpoint :: Lens.Lens' OidcConfigForResponse (Core.Maybe Core.Text)
oidcConfigForResponse_tokenEndpoint = Lens.lens (\OidcConfigForResponse' {tokenEndpoint} -> tokenEndpoint) (\s@OidcConfigForResponse' {} a -> s {tokenEndpoint = a} :: OidcConfigForResponse)

-- | The OIDC IdP authorization endpoint used to configure your private
-- workforce.
oidcConfigForResponse_authorizationEndpoint :: Lens.Lens' OidcConfigForResponse (Core.Maybe Core.Text)
oidcConfigForResponse_authorizationEndpoint = Lens.lens (\OidcConfigForResponse' {authorizationEndpoint} -> authorizationEndpoint) (\s@OidcConfigForResponse' {} a -> s {authorizationEndpoint = a} :: OidcConfigForResponse)

-- | The OIDC IdP user information endpoint used to configure your private
-- workforce.
oidcConfigForResponse_userInfoEndpoint :: Lens.Lens' OidcConfigForResponse (Core.Maybe Core.Text)
oidcConfigForResponse_userInfoEndpoint = Lens.lens (\OidcConfigForResponse' {userInfoEndpoint} -> userInfoEndpoint) (\s@OidcConfigForResponse' {} a -> s {userInfoEndpoint = a} :: OidcConfigForResponse)

-- | The OIDC IdP logout endpoint used to configure your private workforce.
oidcConfigForResponse_logoutEndpoint :: Lens.Lens' OidcConfigForResponse (Core.Maybe Core.Text)
oidcConfigForResponse_logoutEndpoint = Lens.lens (\OidcConfigForResponse' {logoutEndpoint} -> logoutEndpoint) (\s@OidcConfigForResponse' {} a -> s {logoutEndpoint = a} :: OidcConfigForResponse)

-- | The OIDC IdP issuer used to configure your private workforce.
oidcConfigForResponse_issuer :: Lens.Lens' OidcConfigForResponse (Core.Maybe Core.Text)
oidcConfigForResponse_issuer = Lens.lens (\OidcConfigForResponse' {issuer} -> issuer) (\s@OidcConfigForResponse' {} a -> s {issuer = a} :: OidcConfigForResponse)

-- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private
-- workforce.
oidcConfigForResponse_jwksUri :: Lens.Lens' OidcConfigForResponse (Core.Maybe Core.Text)
oidcConfigForResponse_jwksUri = Lens.lens (\OidcConfigForResponse' {jwksUri} -> jwksUri) (\s@OidcConfigForResponse' {} a -> s {jwksUri = a} :: OidcConfigForResponse)

instance Core.FromJSON OidcConfigForResponse where
  parseJSON =
    Core.withObject
      "OidcConfigForResponse"
      ( \x ->
          OidcConfigForResponse'
            Core.<$> (x Core..:? "ClientId")
            Core.<*> (x Core..:? "TokenEndpoint")
            Core.<*> (x Core..:? "AuthorizationEndpoint")
            Core.<*> (x Core..:? "UserInfoEndpoint")
            Core.<*> (x Core..:? "LogoutEndpoint")
            Core.<*> (x Core..:? "Issuer")
            Core.<*> (x Core..:? "JwksUri")
      )

instance Core.Hashable OidcConfigForResponse

instance Core.NFData OidcConfigForResponse
