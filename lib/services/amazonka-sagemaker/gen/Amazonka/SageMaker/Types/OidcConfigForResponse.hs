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
-- Module      : Amazonka.SageMaker.Types.OidcConfigForResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.OidcConfigForResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Your OIDC IdP workforce configuration.
--
-- /See:/ 'newOidcConfigForResponse' smart constructor.
data OidcConfigForResponse = OidcConfigForResponse'
  { -- | The OIDC IdP authorization endpoint used to configure your private
    -- workforce.
    authorizationEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The OIDC IdP client ID used to configure your private workforce.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The OIDC IdP issuer used to configure your private workforce.
    issuer :: Prelude.Maybe Prelude.Text,
    -- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private
    -- workforce.
    jwksUri :: Prelude.Maybe Prelude.Text,
    -- | The OIDC IdP logout endpoint used to configure your private workforce.
    logoutEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The OIDC IdP token endpoint used to configure your private workforce.
    tokenEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The OIDC IdP user information endpoint used to configure your private
    -- workforce.
    userInfoEndpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OidcConfigForResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationEndpoint', 'oidcConfigForResponse_authorizationEndpoint' - The OIDC IdP authorization endpoint used to configure your private
-- workforce.
--
-- 'clientId', 'oidcConfigForResponse_clientId' - The OIDC IdP client ID used to configure your private workforce.
--
-- 'issuer', 'oidcConfigForResponse_issuer' - The OIDC IdP issuer used to configure your private workforce.
--
-- 'jwksUri', 'oidcConfigForResponse_jwksUri' - The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private
-- workforce.
--
-- 'logoutEndpoint', 'oidcConfigForResponse_logoutEndpoint' - The OIDC IdP logout endpoint used to configure your private workforce.
--
-- 'tokenEndpoint', 'oidcConfigForResponse_tokenEndpoint' - The OIDC IdP token endpoint used to configure your private workforce.
--
-- 'userInfoEndpoint', 'oidcConfigForResponse_userInfoEndpoint' - The OIDC IdP user information endpoint used to configure your private
-- workforce.
newOidcConfigForResponse ::
  OidcConfigForResponse
newOidcConfigForResponse =
  OidcConfigForResponse'
    { authorizationEndpoint =
        Prelude.Nothing,
      clientId = Prelude.Nothing,
      issuer = Prelude.Nothing,
      jwksUri = Prelude.Nothing,
      logoutEndpoint = Prelude.Nothing,
      tokenEndpoint = Prelude.Nothing,
      userInfoEndpoint = Prelude.Nothing
    }

-- | The OIDC IdP authorization endpoint used to configure your private
-- workforce.
oidcConfigForResponse_authorizationEndpoint :: Lens.Lens' OidcConfigForResponse (Prelude.Maybe Prelude.Text)
oidcConfigForResponse_authorizationEndpoint = Lens.lens (\OidcConfigForResponse' {authorizationEndpoint} -> authorizationEndpoint) (\s@OidcConfigForResponse' {} a -> s {authorizationEndpoint = a} :: OidcConfigForResponse)

-- | The OIDC IdP client ID used to configure your private workforce.
oidcConfigForResponse_clientId :: Lens.Lens' OidcConfigForResponse (Prelude.Maybe Prelude.Text)
oidcConfigForResponse_clientId = Lens.lens (\OidcConfigForResponse' {clientId} -> clientId) (\s@OidcConfigForResponse' {} a -> s {clientId = a} :: OidcConfigForResponse)

-- | The OIDC IdP issuer used to configure your private workforce.
oidcConfigForResponse_issuer :: Lens.Lens' OidcConfigForResponse (Prelude.Maybe Prelude.Text)
oidcConfigForResponse_issuer = Lens.lens (\OidcConfigForResponse' {issuer} -> issuer) (\s@OidcConfigForResponse' {} a -> s {issuer = a} :: OidcConfigForResponse)

-- | The OIDC IdP JSON Web Key Set (Jwks) URI used to configure your private
-- workforce.
oidcConfigForResponse_jwksUri :: Lens.Lens' OidcConfigForResponse (Prelude.Maybe Prelude.Text)
oidcConfigForResponse_jwksUri = Lens.lens (\OidcConfigForResponse' {jwksUri} -> jwksUri) (\s@OidcConfigForResponse' {} a -> s {jwksUri = a} :: OidcConfigForResponse)

-- | The OIDC IdP logout endpoint used to configure your private workforce.
oidcConfigForResponse_logoutEndpoint :: Lens.Lens' OidcConfigForResponse (Prelude.Maybe Prelude.Text)
oidcConfigForResponse_logoutEndpoint = Lens.lens (\OidcConfigForResponse' {logoutEndpoint} -> logoutEndpoint) (\s@OidcConfigForResponse' {} a -> s {logoutEndpoint = a} :: OidcConfigForResponse)

-- | The OIDC IdP token endpoint used to configure your private workforce.
oidcConfigForResponse_tokenEndpoint :: Lens.Lens' OidcConfigForResponse (Prelude.Maybe Prelude.Text)
oidcConfigForResponse_tokenEndpoint = Lens.lens (\OidcConfigForResponse' {tokenEndpoint} -> tokenEndpoint) (\s@OidcConfigForResponse' {} a -> s {tokenEndpoint = a} :: OidcConfigForResponse)

-- | The OIDC IdP user information endpoint used to configure your private
-- workforce.
oidcConfigForResponse_userInfoEndpoint :: Lens.Lens' OidcConfigForResponse (Prelude.Maybe Prelude.Text)
oidcConfigForResponse_userInfoEndpoint = Lens.lens (\OidcConfigForResponse' {userInfoEndpoint} -> userInfoEndpoint) (\s@OidcConfigForResponse' {} a -> s {userInfoEndpoint = a} :: OidcConfigForResponse)

instance Data.FromJSON OidcConfigForResponse where
  parseJSON =
    Data.withObject
      "OidcConfigForResponse"
      ( \x ->
          OidcConfigForResponse'
            Prelude.<$> (x Data..:? "AuthorizationEndpoint")
            Prelude.<*> (x Data..:? "ClientId")
            Prelude.<*> (x Data..:? "Issuer")
            Prelude.<*> (x Data..:? "JwksUri")
            Prelude.<*> (x Data..:? "LogoutEndpoint")
            Prelude.<*> (x Data..:? "TokenEndpoint")
            Prelude.<*> (x Data..:? "UserInfoEndpoint")
      )

instance Prelude.Hashable OidcConfigForResponse where
  hashWithSalt _salt OidcConfigForResponse' {..} =
    _salt
      `Prelude.hashWithSalt` authorizationEndpoint
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` issuer
      `Prelude.hashWithSalt` jwksUri
      `Prelude.hashWithSalt` logoutEndpoint
      `Prelude.hashWithSalt` tokenEndpoint
      `Prelude.hashWithSalt` userInfoEndpoint

instance Prelude.NFData OidcConfigForResponse where
  rnf OidcConfigForResponse' {..} =
    Prelude.rnf authorizationEndpoint
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf issuer
      `Prelude.seq` Prelude.rnf jwksUri
      `Prelude.seq` Prelude.rnf logoutEndpoint
      `Prelude.seq` Prelude.rnf tokenEndpoint
      `Prelude.seq` Prelude.rnf userInfoEndpoint
