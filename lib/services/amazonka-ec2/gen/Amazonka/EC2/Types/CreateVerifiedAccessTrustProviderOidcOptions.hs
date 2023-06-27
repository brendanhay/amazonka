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
-- Module      : Amazonka.EC2.Types.CreateVerifiedAccessTrustProviderOidcOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CreateVerifiedAccessTrustProviderOidcOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the options when creating an Amazon Web Services Verified
-- Access trust provider using the @user@ type.
--
-- /See:/ 'newCreateVerifiedAccessTrustProviderOidcOptions' smart constructor.
data CreateVerifiedAccessTrustProviderOidcOptions = CreateVerifiedAccessTrustProviderOidcOptions'
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
-- Create a value of 'CreateVerifiedAccessTrustProviderOidcOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationEndpoint', 'createVerifiedAccessTrustProviderOidcOptions_authorizationEndpoint' - The OIDC authorization endpoint.
--
-- 'clientId', 'createVerifiedAccessTrustProviderOidcOptions_clientId' - The client identifier.
--
-- 'clientSecret', 'createVerifiedAccessTrustProviderOidcOptions_clientSecret' - The client secret.
--
-- 'issuer', 'createVerifiedAccessTrustProviderOidcOptions_issuer' - The OIDC issuer.
--
-- 'scope', 'createVerifiedAccessTrustProviderOidcOptions_scope' - OpenID Connect (OIDC) scopes are used by an application during
-- authentication to authorize access to a user\'s details. Each scope
-- returns a specific set of user attributes.
--
-- 'tokenEndpoint', 'createVerifiedAccessTrustProviderOidcOptions_tokenEndpoint' - The OIDC token endpoint.
--
-- 'userInfoEndpoint', 'createVerifiedAccessTrustProviderOidcOptions_userInfoEndpoint' - The OIDC user info endpoint.
newCreateVerifiedAccessTrustProviderOidcOptions ::
  CreateVerifiedAccessTrustProviderOidcOptions
newCreateVerifiedAccessTrustProviderOidcOptions =
  CreateVerifiedAccessTrustProviderOidcOptions'
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
createVerifiedAccessTrustProviderOidcOptions_authorizationEndpoint :: Lens.Lens' CreateVerifiedAccessTrustProviderOidcOptions (Prelude.Maybe Prelude.Text)
createVerifiedAccessTrustProviderOidcOptions_authorizationEndpoint = Lens.lens (\CreateVerifiedAccessTrustProviderOidcOptions' {authorizationEndpoint} -> authorizationEndpoint) (\s@CreateVerifiedAccessTrustProviderOidcOptions' {} a -> s {authorizationEndpoint = a} :: CreateVerifiedAccessTrustProviderOidcOptions)

-- | The client identifier.
createVerifiedAccessTrustProviderOidcOptions_clientId :: Lens.Lens' CreateVerifiedAccessTrustProviderOidcOptions (Prelude.Maybe Prelude.Text)
createVerifiedAccessTrustProviderOidcOptions_clientId = Lens.lens (\CreateVerifiedAccessTrustProviderOidcOptions' {clientId} -> clientId) (\s@CreateVerifiedAccessTrustProviderOidcOptions' {} a -> s {clientId = a} :: CreateVerifiedAccessTrustProviderOidcOptions)

-- | The client secret.
createVerifiedAccessTrustProviderOidcOptions_clientSecret :: Lens.Lens' CreateVerifiedAccessTrustProviderOidcOptions (Prelude.Maybe Prelude.Text)
createVerifiedAccessTrustProviderOidcOptions_clientSecret = Lens.lens (\CreateVerifiedAccessTrustProviderOidcOptions' {clientSecret} -> clientSecret) (\s@CreateVerifiedAccessTrustProviderOidcOptions' {} a -> s {clientSecret = a} :: CreateVerifiedAccessTrustProviderOidcOptions) Prelude.. Lens.mapping Data._Sensitive

-- | The OIDC issuer.
createVerifiedAccessTrustProviderOidcOptions_issuer :: Lens.Lens' CreateVerifiedAccessTrustProviderOidcOptions (Prelude.Maybe Prelude.Text)
createVerifiedAccessTrustProviderOidcOptions_issuer = Lens.lens (\CreateVerifiedAccessTrustProviderOidcOptions' {issuer} -> issuer) (\s@CreateVerifiedAccessTrustProviderOidcOptions' {} a -> s {issuer = a} :: CreateVerifiedAccessTrustProviderOidcOptions)

-- | OpenID Connect (OIDC) scopes are used by an application during
-- authentication to authorize access to a user\'s details. Each scope
-- returns a specific set of user attributes.
createVerifiedAccessTrustProviderOidcOptions_scope :: Lens.Lens' CreateVerifiedAccessTrustProviderOidcOptions (Prelude.Maybe Prelude.Text)
createVerifiedAccessTrustProviderOidcOptions_scope = Lens.lens (\CreateVerifiedAccessTrustProviderOidcOptions' {scope} -> scope) (\s@CreateVerifiedAccessTrustProviderOidcOptions' {} a -> s {scope = a} :: CreateVerifiedAccessTrustProviderOidcOptions)

-- | The OIDC token endpoint.
createVerifiedAccessTrustProviderOidcOptions_tokenEndpoint :: Lens.Lens' CreateVerifiedAccessTrustProviderOidcOptions (Prelude.Maybe Prelude.Text)
createVerifiedAccessTrustProviderOidcOptions_tokenEndpoint = Lens.lens (\CreateVerifiedAccessTrustProviderOidcOptions' {tokenEndpoint} -> tokenEndpoint) (\s@CreateVerifiedAccessTrustProviderOidcOptions' {} a -> s {tokenEndpoint = a} :: CreateVerifiedAccessTrustProviderOidcOptions)

-- | The OIDC user info endpoint.
createVerifiedAccessTrustProviderOidcOptions_userInfoEndpoint :: Lens.Lens' CreateVerifiedAccessTrustProviderOidcOptions (Prelude.Maybe Prelude.Text)
createVerifiedAccessTrustProviderOidcOptions_userInfoEndpoint = Lens.lens (\CreateVerifiedAccessTrustProviderOidcOptions' {userInfoEndpoint} -> userInfoEndpoint) (\s@CreateVerifiedAccessTrustProviderOidcOptions' {} a -> s {userInfoEndpoint = a} :: CreateVerifiedAccessTrustProviderOidcOptions)

instance
  Prelude.Hashable
    CreateVerifiedAccessTrustProviderOidcOptions
  where
  hashWithSalt
    _salt
    CreateVerifiedAccessTrustProviderOidcOptions' {..} =
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
    CreateVerifiedAccessTrustProviderOidcOptions
  where
  rnf CreateVerifiedAccessTrustProviderOidcOptions' {..} =
    Prelude.rnf authorizationEndpoint
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf clientSecret
      `Prelude.seq` Prelude.rnf issuer
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf tokenEndpoint
      `Prelude.seq` Prelude.rnf userInfoEndpoint

instance
  Data.ToQuery
    CreateVerifiedAccessTrustProviderOidcOptions
  where
  toQuery
    CreateVerifiedAccessTrustProviderOidcOptions' {..} =
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
