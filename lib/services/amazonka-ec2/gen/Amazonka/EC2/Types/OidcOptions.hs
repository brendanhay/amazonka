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
-- Module      : Amazonka.EC2.Types.OidcOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.OidcOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Options for OIDC-based, user-identity type trust provider.
--
-- /See:/ 'newOidcOptions' smart constructor.
data OidcOptions = OidcOptions'
  { -- | The OIDC authorization endpoint.
    authorizationEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The client identifier.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The client secret.
    clientSecret :: Prelude.Maybe Prelude.Text,
    -- | The OIDC issuer.
    issuer :: Prelude.Maybe Prelude.Text,
    -- | The OpenID Connect (OIDC) scope specified.
    scope :: Prelude.Maybe Prelude.Text,
    -- | The OIDC token endpoint.
    tokenEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The OIDC user info endpoint.
    userInfoEndpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OidcOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationEndpoint', 'oidcOptions_authorizationEndpoint' - The OIDC authorization endpoint.
--
-- 'clientId', 'oidcOptions_clientId' - The client identifier.
--
-- 'clientSecret', 'oidcOptions_clientSecret' - The client secret.
--
-- 'issuer', 'oidcOptions_issuer' - The OIDC issuer.
--
-- 'scope', 'oidcOptions_scope' - The OpenID Connect (OIDC) scope specified.
--
-- 'tokenEndpoint', 'oidcOptions_tokenEndpoint' - The OIDC token endpoint.
--
-- 'userInfoEndpoint', 'oidcOptions_userInfoEndpoint' - The OIDC user info endpoint.
newOidcOptions ::
  OidcOptions
newOidcOptions =
  OidcOptions'
    { authorizationEndpoint =
        Prelude.Nothing,
      clientId = Prelude.Nothing,
      clientSecret = Prelude.Nothing,
      issuer = Prelude.Nothing,
      scope = Prelude.Nothing,
      tokenEndpoint = Prelude.Nothing,
      userInfoEndpoint = Prelude.Nothing
    }

-- | The OIDC authorization endpoint.
oidcOptions_authorizationEndpoint :: Lens.Lens' OidcOptions (Prelude.Maybe Prelude.Text)
oidcOptions_authorizationEndpoint = Lens.lens (\OidcOptions' {authorizationEndpoint} -> authorizationEndpoint) (\s@OidcOptions' {} a -> s {authorizationEndpoint = a} :: OidcOptions)

-- | The client identifier.
oidcOptions_clientId :: Lens.Lens' OidcOptions (Prelude.Maybe Prelude.Text)
oidcOptions_clientId = Lens.lens (\OidcOptions' {clientId} -> clientId) (\s@OidcOptions' {} a -> s {clientId = a} :: OidcOptions)

-- | The client secret.
oidcOptions_clientSecret :: Lens.Lens' OidcOptions (Prelude.Maybe Prelude.Text)
oidcOptions_clientSecret = Lens.lens (\OidcOptions' {clientSecret} -> clientSecret) (\s@OidcOptions' {} a -> s {clientSecret = a} :: OidcOptions)

-- | The OIDC issuer.
oidcOptions_issuer :: Lens.Lens' OidcOptions (Prelude.Maybe Prelude.Text)
oidcOptions_issuer = Lens.lens (\OidcOptions' {issuer} -> issuer) (\s@OidcOptions' {} a -> s {issuer = a} :: OidcOptions)

-- | The OpenID Connect (OIDC) scope specified.
oidcOptions_scope :: Lens.Lens' OidcOptions (Prelude.Maybe Prelude.Text)
oidcOptions_scope = Lens.lens (\OidcOptions' {scope} -> scope) (\s@OidcOptions' {} a -> s {scope = a} :: OidcOptions)

-- | The OIDC token endpoint.
oidcOptions_tokenEndpoint :: Lens.Lens' OidcOptions (Prelude.Maybe Prelude.Text)
oidcOptions_tokenEndpoint = Lens.lens (\OidcOptions' {tokenEndpoint} -> tokenEndpoint) (\s@OidcOptions' {} a -> s {tokenEndpoint = a} :: OidcOptions)

-- | The OIDC user info endpoint.
oidcOptions_userInfoEndpoint :: Lens.Lens' OidcOptions (Prelude.Maybe Prelude.Text)
oidcOptions_userInfoEndpoint = Lens.lens (\OidcOptions' {userInfoEndpoint} -> userInfoEndpoint) (\s@OidcOptions' {} a -> s {userInfoEndpoint = a} :: OidcOptions)

instance Data.FromXML OidcOptions where
  parseXML x =
    OidcOptions'
      Prelude.<$> (x Data..@? "authorizationEndpoint")
      Prelude.<*> (x Data..@? "clientId")
      Prelude.<*> (x Data..@? "clientSecret")
      Prelude.<*> (x Data..@? "issuer")
      Prelude.<*> (x Data..@? "scope")
      Prelude.<*> (x Data..@? "tokenEndpoint")
      Prelude.<*> (x Data..@? "userInfoEndpoint")

instance Prelude.Hashable OidcOptions where
  hashWithSalt _salt OidcOptions' {..} =
    _salt
      `Prelude.hashWithSalt` authorizationEndpoint
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` clientSecret
      `Prelude.hashWithSalt` issuer
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` tokenEndpoint
      `Prelude.hashWithSalt` userInfoEndpoint

instance Prelude.NFData OidcOptions where
  rnf OidcOptions' {..} =
    Prelude.rnf authorizationEndpoint
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf clientSecret
      `Prelude.seq` Prelude.rnf issuer
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf tokenEndpoint
      `Prelude.seq` Prelude.rnf userInfoEndpoint
