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
-- Module      : Amazonka.SageMaker.Types.OidcConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.OidcConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this parameter to configure your OIDC Identity Provider (IdP).
--
-- /See:/ 'newOidcConfig' smart constructor.
data OidcConfig = OidcConfig'
  { -- | The OIDC IdP client ID used to configure your private workforce.
    clientId :: Prelude.Text,
    -- | The OIDC IdP client secret used to configure your private workforce.
    clientSecret :: Data.Sensitive Prelude.Text,
    -- | The OIDC IdP issuer used to configure your private workforce.
    issuer :: Prelude.Text,
    -- | The OIDC IdP authorization endpoint used to configure your private
    -- workforce.
    authorizationEndpoint :: Prelude.Text,
    -- | The OIDC IdP token endpoint used to configure your private workforce.
    tokenEndpoint :: Prelude.Text,
    -- | The OIDC IdP user information endpoint used to configure your private
    -- workforce.
    userInfoEndpoint :: Prelude.Text,
    -- | The OIDC IdP logout endpoint used to configure your private workforce.
    logoutEndpoint :: Prelude.Text,
    -- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private
    -- workforce.
    jwksUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'clientSecret'
  Prelude.Text ->
  -- | 'issuer'
  Prelude.Text ->
  -- | 'authorizationEndpoint'
  Prelude.Text ->
  -- | 'tokenEndpoint'
  Prelude.Text ->
  -- | 'userInfoEndpoint'
  Prelude.Text ->
  -- | 'logoutEndpoint'
  Prelude.Text ->
  -- | 'jwksUri'
  Prelude.Text ->
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
        clientSecret = Data._Sensitive Lens.# pClientSecret_,
        issuer = pIssuer_,
        authorizationEndpoint = pAuthorizationEndpoint_,
        tokenEndpoint = pTokenEndpoint_,
        userInfoEndpoint = pUserInfoEndpoint_,
        logoutEndpoint = pLogoutEndpoint_,
        jwksUri = pJwksUri_
      }

-- | The OIDC IdP client ID used to configure your private workforce.
oidcConfig_clientId :: Lens.Lens' OidcConfig Prelude.Text
oidcConfig_clientId = Lens.lens (\OidcConfig' {clientId} -> clientId) (\s@OidcConfig' {} a -> s {clientId = a} :: OidcConfig)

-- | The OIDC IdP client secret used to configure your private workforce.
oidcConfig_clientSecret :: Lens.Lens' OidcConfig Prelude.Text
oidcConfig_clientSecret = Lens.lens (\OidcConfig' {clientSecret} -> clientSecret) (\s@OidcConfig' {} a -> s {clientSecret = a} :: OidcConfig) Prelude.. Data._Sensitive

-- | The OIDC IdP issuer used to configure your private workforce.
oidcConfig_issuer :: Lens.Lens' OidcConfig Prelude.Text
oidcConfig_issuer = Lens.lens (\OidcConfig' {issuer} -> issuer) (\s@OidcConfig' {} a -> s {issuer = a} :: OidcConfig)

-- | The OIDC IdP authorization endpoint used to configure your private
-- workforce.
oidcConfig_authorizationEndpoint :: Lens.Lens' OidcConfig Prelude.Text
oidcConfig_authorizationEndpoint = Lens.lens (\OidcConfig' {authorizationEndpoint} -> authorizationEndpoint) (\s@OidcConfig' {} a -> s {authorizationEndpoint = a} :: OidcConfig)

-- | The OIDC IdP token endpoint used to configure your private workforce.
oidcConfig_tokenEndpoint :: Lens.Lens' OidcConfig Prelude.Text
oidcConfig_tokenEndpoint = Lens.lens (\OidcConfig' {tokenEndpoint} -> tokenEndpoint) (\s@OidcConfig' {} a -> s {tokenEndpoint = a} :: OidcConfig)

-- | The OIDC IdP user information endpoint used to configure your private
-- workforce.
oidcConfig_userInfoEndpoint :: Lens.Lens' OidcConfig Prelude.Text
oidcConfig_userInfoEndpoint = Lens.lens (\OidcConfig' {userInfoEndpoint} -> userInfoEndpoint) (\s@OidcConfig' {} a -> s {userInfoEndpoint = a} :: OidcConfig)

-- | The OIDC IdP logout endpoint used to configure your private workforce.
oidcConfig_logoutEndpoint :: Lens.Lens' OidcConfig Prelude.Text
oidcConfig_logoutEndpoint = Lens.lens (\OidcConfig' {logoutEndpoint} -> logoutEndpoint) (\s@OidcConfig' {} a -> s {logoutEndpoint = a} :: OidcConfig)

-- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private
-- workforce.
oidcConfig_jwksUri :: Lens.Lens' OidcConfig Prelude.Text
oidcConfig_jwksUri = Lens.lens (\OidcConfig' {jwksUri} -> jwksUri) (\s@OidcConfig' {} a -> s {jwksUri = a} :: OidcConfig)

instance Prelude.Hashable OidcConfig where
  hashWithSalt _salt OidcConfig' {..} =
    _salt
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` clientSecret
      `Prelude.hashWithSalt` issuer
      `Prelude.hashWithSalt` authorizationEndpoint
      `Prelude.hashWithSalt` tokenEndpoint
      `Prelude.hashWithSalt` userInfoEndpoint
      `Prelude.hashWithSalt` logoutEndpoint
      `Prelude.hashWithSalt` jwksUri

instance Prelude.NFData OidcConfig where
  rnf OidcConfig' {..} =
    Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf clientSecret
      `Prelude.seq` Prelude.rnf issuer
      `Prelude.seq` Prelude.rnf authorizationEndpoint
      `Prelude.seq` Prelude.rnf tokenEndpoint
      `Prelude.seq` Prelude.rnf userInfoEndpoint
      `Prelude.seq` Prelude.rnf logoutEndpoint
      `Prelude.seq` Prelude.rnf jwksUri

instance Data.ToJSON OidcConfig where
  toJSON OidcConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClientId" Data..= clientId),
            Prelude.Just ("ClientSecret" Data..= clientSecret),
            Prelude.Just ("Issuer" Data..= issuer),
            Prelude.Just
              ( "AuthorizationEndpoint"
                  Data..= authorizationEndpoint
              ),
            Prelude.Just ("TokenEndpoint" Data..= tokenEndpoint),
            Prelude.Just
              ("UserInfoEndpoint" Data..= userInfoEndpoint),
            Prelude.Just
              ("LogoutEndpoint" Data..= logoutEndpoint),
            Prelude.Just ("JwksUri" Data..= jwksUri)
          ]
      )
