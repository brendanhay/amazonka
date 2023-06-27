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
-- Module      : Amazonka.EC2.Types.ModifyVerifiedAccessTrustProviderOidcOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ModifyVerifiedAccessTrustProviderOidcOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Options for an OpenID Connect-compatible user-identity trust provider.
--
-- /See:/ 'newModifyVerifiedAccessTrustProviderOidcOptions' smart constructor.
data ModifyVerifiedAccessTrustProviderOidcOptions = ModifyVerifiedAccessTrustProviderOidcOptions'
  { -- | The OIDC authorization endpoint.
    authorizationEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The client identifier.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The client secret.
    clientSecret :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The OIDC issuer.
    issuer :: Prelude.Maybe Prelude.Text,
    -- | OpenID Connect (OIDC) scopes are used by an application during
    -- authentication to authorize access to a user\'s details. Each scope
    -- returns a specific set of user attributes.
    scope :: Prelude.Maybe Prelude.Text,
    -- | The OIDC token endpoint.
    tokenEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The OIDC user info endpoint.
    userInfoEndpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessTrustProviderOidcOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationEndpoint', 'modifyVerifiedAccessTrustProviderOidcOptions_authorizationEndpoint' - The OIDC authorization endpoint.
--
-- 'clientId', 'modifyVerifiedAccessTrustProviderOidcOptions_clientId' - The client identifier.
--
-- 'clientSecret', 'modifyVerifiedAccessTrustProviderOidcOptions_clientSecret' - The client secret.
--
-- 'issuer', 'modifyVerifiedAccessTrustProviderOidcOptions_issuer' - The OIDC issuer.
--
-- 'scope', 'modifyVerifiedAccessTrustProviderOidcOptions_scope' - OpenID Connect (OIDC) scopes are used by an application during
-- authentication to authorize access to a user\'s details. Each scope
-- returns a specific set of user attributes.
--
-- 'tokenEndpoint', 'modifyVerifiedAccessTrustProviderOidcOptions_tokenEndpoint' - The OIDC token endpoint.
--
-- 'userInfoEndpoint', 'modifyVerifiedAccessTrustProviderOidcOptions_userInfoEndpoint' - The OIDC user info endpoint.
newModifyVerifiedAccessTrustProviderOidcOptions ::
  ModifyVerifiedAccessTrustProviderOidcOptions
newModifyVerifiedAccessTrustProviderOidcOptions =
  ModifyVerifiedAccessTrustProviderOidcOptions'
    { authorizationEndpoint =
        Prelude.Nothing,
      clientId = Prelude.Nothing,
      clientSecret =
        Prelude.Nothing,
      issuer = Prelude.Nothing,
      scope = Prelude.Nothing,
      tokenEndpoint =
        Prelude.Nothing,
      userInfoEndpoint =
        Prelude.Nothing
    }

-- | The OIDC authorization endpoint.
modifyVerifiedAccessTrustProviderOidcOptions_authorizationEndpoint :: Lens.Lens' ModifyVerifiedAccessTrustProviderOidcOptions (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessTrustProviderOidcOptions_authorizationEndpoint = Lens.lens (\ModifyVerifiedAccessTrustProviderOidcOptions' {authorizationEndpoint} -> authorizationEndpoint) (\s@ModifyVerifiedAccessTrustProviderOidcOptions' {} a -> s {authorizationEndpoint = a} :: ModifyVerifiedAccessTrustProviderOidcOptions)

-- | The client identifier.
modifyVerifiedAccessTrustProviderOidcOptions_clientId :: Lens.Lens' ModifyVerifiedAccessTrustProviderOidcOptions (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessTrustProviderOidcOptions_clientId = Lens.lens (\ModifyVerifiedAccessTrustProviderOidcOptions' {clientId} -> clientId) (\s@ModifyVerifiedAccessTrustProviderOidcOptions' {} a -> s {clientId = a} :: ModifyVerifiedAccessTrustProviderOidcOptions)

-- | The client secret.
modifyVerifiedAccessTrustProviderOidcOptions_clientSecret :: Lens.Lens' ModifyVerifiedAccessTrustProviderOidcOptions (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessTrustProviderOidcOptions_clientSecret = Lens.lens (\ModifyVerifiedAccessTrustProviderOidcOptions' {clientSecret} -> clientSecret) (\s@ModifyVerifiedAccessTrustProviderOidcOptions' {} a -> s {clientSecret = a} :: ModifyVerifiedAccessTrustProviderOidcOptions) Prelude.. Lens.mapping Data._Sensitive

-- | The OIDC issuer.
modifyVerifiedAccessTrustProviderOidcOptions_issuer :: Lens.Lens' ModifyVerifiedAccessTrustProviderOidcOptions (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessTrustProviderOidcOptions_issuer = Lens.lens (\ModifyVerifiedAccessTrustProviderOidcOptions' {issuer} -> issuer) (\s@ModifyVerifiedAccessTrustProviderOidcOptions' {} a -> s {issuer = a} :: ModifyVerifiedAccessTrustProviderOidcOptions)

-- | OpenID Connect (OIDC) scopes are used by an application during
-- authentication to authorize access to a user\'s details. Each scope
-- returns a specific set of user attributes.
modifyVerifiedAccessTrustProviderOidcOptions_scope :: Lens.Lens' ModifyVerifiedAccessTrustProviderOidcOptions (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessTrustProviderOidcOptions_scope = Lens.lens (\ModifyVerifiedAccessTrustProviderOidcOptions' {scope} -> scope) (\s@ModifyVerifiedAccessTrustProviderOidcOptions' {} a -> s {scope = a} :: ModifyVerifiedAccessTrustProviderOidcOptions)

-- | The OIDC token endpoint.
modifyVerifiedAccessTrustProviderOidcOptions_tokenEndpoint :: Lens.Lens' ModifyVerifiedAccessTrustProviderOidcOptions (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessTrustProviderOidcOptions_tokenEndpoint = Lens.lens (\ModifyVerifiedAccessTrustProviderOidcOptions' {tokenEndpoint} -> tokenEndpoint) (\s@ModifyVerifiedAccessTrustProviderOidcOptions' {} a -> s {tokenEndpoint = a} :: ModifyVerifiedAccessTrustProviderOidcOptions)

-- | The OIDC user info endpoint.
modifyVerifiedAccessTrustProviderOidcOptions_userInfoEndpoint :: Lens.Lens' ModifyVerifiedAccessTrustProviderOidcOptions (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessTrustProviderOidcOptions_userInfoEndpoint = Lens.lens (\ModifyVerifiedAccessTrustProviderOidcOptions' {userInfoEndpoint} -> userInfoEndpoint) (\s@ModifyVerifiedAccessTrustProviderOidcOptions' {} a -> s {userInfoEndpoint = a} :: ModifyVerifiedAccessTrustProviderOidcOptions)

instance
  Prelude.Hashable
    ModifyVerifiedAccessTrustProviderOidcOptions
  where
  hashWithSalt
    _salt
    ModifyVerifiedAccessTrustProviderOidcOptions' {..} =
      _salt
        `Prelude.hashWithSalt` authorizationEndpoint
        `Prelude.hashWithSalt` clientId
        `Prelude.hashWithSalt` clientSecret
        `Prelude.hashWithSalt` issuer
        `Prelude.hashWithSalt` scope
        `Prelude.hashWithSalt` tokenEndpoint
        `Prelude.hashWithSalt` userInfoEndpoint

instance
  Prelude.NFData
    ModifyVerifiedAccessTrustProviderOidcOptions
  where
  rnf ModifyVerifiedAccessTrustProviderOidcOptions' {..} =
    Prelude.rnf authorizationEndpoint
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf clientSecret
      `Prelude.seq` Prelude.rnf issuer
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf tokenEndpoint
      `Prelude.seq` Prelude.rnf userInfoEndpoint

instance
  Data.ToQuery
    ModifyVerifiedAccessTrustProviderOidcOptions
  where
  toQuery
    ModifyVerifiedAccessTrustProviderOidcOptions' {..} =
      Prelude.mconcat
        [ "AuthorizationEndpoint"
            Data.=: authorizationEndpoint,
          "ClientId" Data.=: clientId,
          "ClientSecret" Data.=: clientSecret,
          "Issuer" Data.=: issuer,
          "Scope" Data.=: scope,
          "TokenEndpoint" Data.=: tokenEndpoint,
          "UserInfoEndpoint" Data.=: userInfoEndpoint
        ]
