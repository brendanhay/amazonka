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
-- Module      : Network.AWS.SageMaker.Types.OidcConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OidcConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Use this parameter to configure your OIDC Identity Provider (IdP).
--
-- /See:/ 'newOidcConfig' smart constructor.
data OidcConfig = OidcConfig'
  { -- | The OIDC IdP client ID used to configure your private workforce.
    clientId :: Core.Text,
    -- | The OIDC IdP client secret used to configure your private workforce.
    clientSecret :: Core.Sensitive Core.Text,
    -- | The OIDC IdP issuer used to configure your private workforce.
    issuer :: Core.Text,
    -- | The OIDC IdP authorization endpoint used to configure your private
    -- workforce.
    authorizationEndpoint :: Core.Text,
    -- | The OIDC IdP token endpoint used to configure your private workforce.
    tokenEndpoint :: Core.Text,
    -- | The OIDC IdP user information endpoint used to configure your private
    -- workforce.
    userInfoEndpoint :: Core.Text,
    -- | The OIDC IdP logout endpoint used to configure your private workforce.
    logoutEndpoint :: Core.Text,
    -- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private
    -- workforce.
    jwksUri :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'OidcConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'oidcConfig_clientId' - The OIDC IdP client ID used to configure your private workforce.
--
-- 'clientSecret', 'oidcConfig_clientSecret' - The OIDC IdP client secret used to configure your private workforce.
--
-- 'issuer', 'oidcConfig_issuer' - The OIDC IdP issuer used to configure your private workforce.
--
-- 'authorizationEndpoint', 'oidcConfig_authorizationEndpoint' - The OIDC IdP authorization endpoint used to configure your private
-- workforce.
--
-- 'tokenEndpoint', 'oidcConfig_tokenEndpoint' - The OIDC IdP token endpoint used to configure your private workforce.
--
-- 'userInfoEndpoint', 'oidcConfig_userInfoEndpoint' - The OIDC IdP user information endpoint used to configure your private
-- workforce.
--
-- 'logoutEndpoint', 'oidcConfig_logoutEndpoint' - The OIDC IdP logout endpoint used to configure your private workforce.
--
-- 'jwksUri', 'oidcConfig_jwksUri' - The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private
-- workforce.
newOidcConfig ::
  -- | 'clientId'
  Core.Text ->
  -- | 'clientSecret'
  Core.Text ->
  -- | 'issuer'
  Core.Text ->
  -- | 'authorizationEndpoint'
  Core.Text ->
  -- | 'tokenEndpoint'
  Core.Text ->
  -- | 'userInfoEndpoint'
  Core.Text ->
  -- | 'logoutEndpoint'
  Core.Text ->
  -- | 'jwksUri'
  Core.Text ->
  OidcConfig
newOidcConfig
  pClientId_
  pClientSecret_
  pIssuer_
  pAuthorizationEndpoint_
  pTokenEndpoint_
  pUserInfoEndpoint_
  pLogoutEndpoint_
  pJwksUri_ =
    OidcConfig'
      { clientId = pClientId_,
        clientSecret = Core._Sensitive Lens.# pClientSecret_,
        issuer = pIssuer_,
        authorizationEndpoint = pAuthorizationEndpoint_,
        tokenEndpoint = pTokenEndpoint_,
        userInfoEndpoint = pUserInfoEndpoint_,
        logoutEndpoint = pLogoutEndpoint_,
        jwksUri = pJwksUri_
      }

-- | The OIDC IdP client ID used to configure your private workforce.
oidcConfig_clientId :: Lens.Lens' OidcConfig Core.Text
oidcConfig_clientId = Lens.lens (\OidcConfig' {clientId} -> clientId) (\s@OidcConfig' {} a -> s {clientId = a} :: OidcConfig)

-- | The OIDC IdP client secret used to configure your private workforce.
oidcConfig_clientSecret :: Lens.Lens' OidcConfig Core.Text
oidcConfig_clientSecret = Lens.lens (\OidcConfig' {clientSecret} -> clientSecret) (\s@OidcConfig' {} a -> s {clientSecret = a} :: OidcConfig) Core.. Core._Sensitive

-- | The OIDC IdP issuer used to configure your private workforce.
oidcConfig_issuer :: Lens.Lens' OidcConfig Core.Text
oidcConfig_issuer = Lens.lens (\OidcConfig' {issuer} -> issuer) (\s@OidcConfig' {} a -> s {issuer = a} :: OidcConfig)

-- | The OIDC IdP authorization endpoint used to configure your private
-- workforce.
oidcConfig_authorizationEndpoint :: Lens.Lens' OidcConfig Core.Text
oidcConfig_authorizationEndpoint = Lens.lens (\OidcConfig' {authorizationEndpoint} -> authorizationEndpoint) (\s@OidcConfig' {} a -> s {authorizationEndpoint = a} :: OidcConfig)

-- | The OIDC IdP token endpoint used to configure your private workforce.
oidcConfig_tokenEndpoint :: Lens.Lens' OidcConfig Core.Text
oidcConfig_tokenEndpoint = Lens.lens (\OidcConfig' {tokenEndpoint} -> tokenEndpoint) (\s@OidcConfig' {} a -> s {tokenEndpoint = a} :: OidcConfig)

-- | The OIDC IdP user information endpoint used to configure your private
-- workforce.
oidcConfig_userInfoEndpoint :: Lens.Lens' OidcConfig Core.Text
oidcConfig_userInfoEndpoint = Lens.lens (\OidcConfig' {userInfoEndpoint} -> userInfoEndpoint) (\s@OidcConfig' {} a -> s {userInfoEndpoint = a} :: OidcConfig)

-- | The OIDC IdP logout endpoint used to configure your private workforce.
oidcConfig_logoutEndpoint :: Lens.Lens' OidcConfig Core.Text
oidcConfig_logoutEndpoint = Lens.lens (\OidcConfig' {logoutEndpoint} -> logoutEndpoint) (\s@OidcConfig' {} a -> s {logoutEndpoint = a} :: OidcConfig)

-- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private
-- workforce.
oidcConfig_jwksUri :: Lens.Lens' OidcConfig Core.Text
oidcConfig_jwksUri = Lens.lens (\OidcConfig' {jwksUri} -> jwksUri) (\s@OidcConfig' {} a -> s {jwksUri = a} :: OidcConfig)

instance Core.Hashable OidcConfig

instance Core.NFData OidcConfig

instance Core.ToJSON OidcConfig where
  toJSON OidcConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClientId" Core..= clientId),
            Core.Just ("ClientSecret" Core..= clientSecret),
            Core.Just ("Issuer" Core..= issuer),
            Core.Just
              ( "AuthorizationEndpoint"
                  Core..= authorizationEndpoint
              ),
            Core.Just ("TokenEndpoint" Core..= tokenEndpoint),
            Core.Just
              ("UserInfoEndpoint" Core..= userInfoEndpoint),
            Core.Just ("LogoutEndpoint" Core..= logoutEndpoint),
            Core.Just ("JwksUri" Core..= jwksUri)
          ]
      )
