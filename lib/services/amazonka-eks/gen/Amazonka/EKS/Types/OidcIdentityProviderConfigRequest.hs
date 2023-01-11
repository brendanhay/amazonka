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
-- Module      : Amazonka.EKS.Types.OidcIdentityProviderConfigRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.OidcIdentityProviderConfigRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing an OpenID Connect (OIDC) configuration. Before
-- associating an OIDC identity provider to your cluster, review the
-- considerations in
-- <https://docs.aws.amazon.com/eks/latest/userguide/authenticate-oidc-identity-provider.html Authenticating users for your cluster from an OpenID Connect identity provider>
-- in the /Amazon EKS User Guide/.
--
-- /See:/ 'newOidcIdentityProviderConfigRequest' smart constructor.
data OidcIdentityProviderConfigRequest = OidcIdentityProviderConfigRequest'
  { -- | The JWT claim that the provider uses to return your groups.
    groupsClaim :: Prelude.Maybe Prelude.Text,
    -- | The prefix that is prepended to group claims to prevent clashes with
    -- existing names (such as @system:@ groups). For example, the
    -- value@ oidc:@ will create group names like @oidc:engineering@ and
    -- @oidc:infra@.
    groupsPrefix :: Prelude.Maybe Prelude.Text,
    -- | The key value pairs that describe required claims in the identity token.
    -- If set, each claim is verified to be present in the token with a
    -- matching value. For the maximum number of claims that you can require,
    -- see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/service-quotas.html Amazon EKS service quotas>
    -- in the /Amazon EKS User Guide/.
    requiredClaims :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The JSON Web Token (JWT) claim to use as the username. The default is
    -- @sub@, which is expected to be a unique identifier of the end user. You
    -- can choose other claims, such as @email@ or @name@, depending on the
    -- OpenID identity provider. Claims other than @email@ are prefixed with
    -- the issuer URL to prevent naming clashes with other plug-ins.
    usernameClaim :: Prelude.Maybe Prelude.Text,
    -- | The prefix that is prepended to username claims to prevent clashes with
    -- existing names. If you do not provide this field, and @username@ is a
    -- value other than @email@, the prefix defaults to @issuerurl#@. You can
    -- use the value @-@ to disable all prefixing.
    usernamePrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the OIDC provider configuration.
    identityProviderConfigName :: Prelude.Text,
    -- | The URL of the OpenID identity provider that allows the API server to
    -- discover public signing keys for verifying tokens. The URL must begin
    -- with @https:\/\/@ and should correspond to the @iss@ claim in the
    -- provider\'s OIDC ID tokens. Per the OIDC standard, path components are
    -- allowed but query parameters are not. Typically the URL consists of only
    -- a hostname, like @https:\/\/server.example.org@ or
    -- @https:\/\/example.com@. This URL should point to the level below
    -- @.well-known\/openid-configuration@ and must be publicly accessible over
    -- the internet.
    issuerUrl :: Prelude.Text,
    -- | This is also known as /audience/. The ID for the client application that
    -- makes authentication requests to the OpenID identity provider.
    clientId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OidcIdentityProviderConfigRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupsClaim', 'oidcIdentityProviderConfigRequest_groupsClaim' - The JWT claim that the provider uses to return your groups.
--
-- 'groupsPrefix', 'oidcIdentityProviderConfigRequest_groupsPrefix' - The prefix that is prepended to group claims to prevent clashes with
-- existing names (such as @system:@ groups). For example, the
-- value@ oidc:@ will create group names like @oidc:engineering@ and
-- @oidc:infra@.
--
-- 'requiredClaims', 'oidcIdentityProviderConfigRequest_requiredClaims' - The key value pairs that describe required claims in the identity token.
-- If set, each claim is verified to be present in the token with a
-- matching value. For the maximum number of claims that you can require,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/service-quotas.html Amazon EKS service quotas>
-- in the /Amazon EKS User Guide/.
--
-- 'usernameClaim', 'oidcIdentityProviderConfigRequest_usernameClaim' - The JSON Web Token (JWT) claim to use as the username. The default is
-- @sub@, which is expected to be a unique identifier of the end user. You
-- can choose other claims, such as @email@ or @name@, depending on the
-- OpenID identity provider. Claims other than @email@ are prefixed with
-- the issuer URL to prevent naming clashes with other plug-ins.
--
-- 'usernamePrefix', 'oidcIdentityProviderConfigRequest_usernamePrefix' - The prefix that is prepended to username claims to prevent clashes with
-- existing names. If you do not provide this field, and @username@ is a
-- value other than @email@, the prefix defaults to @issuerurl#@. You can
-- use the value @-@ to disable all prefixing.
--
-- 'identityProviderConfigName', 'oidcIdentityProviderConfigRequest_identityProviderConfigName' - The name of the OIDC provider configuration.
--
-- 'issuerUrl', 'oidcIdentityProviderConfigRequest_issuerUrl' - The URL of the OpenID identity provider that allows the API server to
-- discover public signing keys for verifying tokens. The URL must begin
-- with @https:\/\/@ and should correspond to the @iss@ claim in the
-- provider\'s OIDC ID tokens. Per the OIDC standard, path components are
-- allowed but query parameters are not. Typically the URL consists of only
-- a hostname, like @https:\/\/server.example.org@ or
-- @https:\/\/example.com@. This URL should point to the level below
-- @.well-known\/openid-configuration@ and must be publicly accessible over
-- the internet.
--
-- 'clientId', 'oidcIdentityProviderConfigRequest_clientId' - This is also known as /audience/. The ID for the client application that
-- makes authentication requests to the OpenID identity provider.
newOidcIdentityProviderConfigRequest ::
  -- | 'identityProviderConfigName'
  Prelude.Text ->
  -- | 'issuerUrl'
  Prelude.Text ->
  -- | 'clientId'
  Prelude.Text ->
  OidcIdentityProviderConfigRequest
newOidcIdentityProviderConfigRequest
  pIdentityProviderConfigName_
  pIssuerUrl_
  pClientId_ =
    OidcIdentityProviderConfigRequest'
      { groupsClaim =
          Prelude.Nothing,
        groupsPrefix = Prelude.Nothing,
        requiredClaims = Prelude.Nothing,
        usernameClaim = Prelude.Nothing,
        usernamePrefix = Prelude.Nothing,
        identityProviderConfigName =
          pIdentityProviderConfigName_,
        issuerUrl = pIssuerUrl_,
        clientId = pClientId_
      }

-- | The JWT claim that the provider uses to return your groups.
oidcIdentityProviderConfigRequest_groupsClaim :: Lens.Lens' OidcIdentityProviderConfigRequest (Prelude.Maybe Prelude.Text)
oidcIdentityProviderConfigRequest_groupsClaim = Lens.lens (\OidcIdentityProviderConfigRequest' {groupsClaim} -> groupsClaim) (\s@OidcIdentityProviderConfigRequest' {} a -> s {groupsClaim = a} :: OidcIdentityProviderConfigRequest)

-- | The prefix that is prepended to group claims to prevent clashes with
-- existing names (such as @system:@ groups). For example, the
-- value@ oidc:@ will create group names like @oidc:engineering@ and
-- @oidc:infra@.
oidcIdentityProviderConfigRequest_groupsPrefix :: Lens.Lens' OidcIdentityProviderConfigRequest (Prelude.Maybe Prelude.Text)
oidcIdentityProviderConfigRequest_groupsPrefix = Lens.lens (\OidcIdentityProviderConfigRequest' {groupsPrefix} -> groupsPrefix) (\s@OidcIdentityProviderConfigRequest' {} a -> s {groupsPrefix = a} :: OidcIdentityProviderConfigRequest)

-- | The key value pairs that describe required claims in the identity token.
-- If set, each claim is verified to be present in the token with a
-- matching value. For the maximum number of claims that you can require,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/service-quotas.html Amazon EKS service quotas>
-- in the /Amazon EKS User Guide/.
oidcIdentityProviderConfigRequest_requiredClaims :: Lens.Lens' OidcIdentityProviderConfigRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
oidcIdentityProviderConfigRequest_requiredClaims = Lens.lens (\OidcIdentityProviderConfigRequest' {requiredClaims} -> requiredClaims) (\s@OidcIdentityProviderConfigRequest' {} a -> s {requiredClaims = a} :: OidcIdentityProviderConfigRequest) Prelude.. Lens.mapping Lens.coerced

-- | The JSON Web Token (JWT) claim to use as the username. The default is
-- @sub@, which is expected to be a unique identifier of the end user. You
-- can choose other claims, such as @email@ or @name@, depending on the
-- OpenID identity provider. Claims other than @email@ are prefixed with
-- the issuer URL to prevent naming clashes with other plug-ins.
oidcIdentityProviderConfigRequest_usernameClaim :: Lens.Lens' OidcIdentityProviderConfigRequest (Prelude.Maybe Prelude.Text)
oidcIdentityProviderConfigRequest_usernameClaim = Lens.lens (\OidcIdentityProviderConfigRequest' {usernameClaim} -> usernameClaim) (\s@OidcIdentityProviderConfigRequest' {} a -> s {usernameClaim = a} :: OidcIdentityProviderConfigRequest)

-- | The prefix that is prepended to username claims to prevent clashes with
-- existing names. If you do not provide this field, and @username@ is a
-- value other than @email@, the prefix defaults to @issuerurl#@. You can
-- use the value @-@ to disable all prefixing.
oidcIdentityProviderConfigRequest_usernamePrefix :: Lens.Lens' OidcIdentityProviderConfigRequest (Prelude.Maybe Prelude.Text)
oidcIdentityProviderConfigRequest_usernamePrefix = Lens.lens (\OidcIdentityProviderConfigRequest' {usernamePrefix} -> usernamePrefix) (\s@OidcIdentityProviderConfigRequest' {} a -> s {usernamePrefix = a} :: OidcIdentityProviderConfigRequest)

-- | The name of the OIDC provider configuration.
oidcIdentityProviderConfigRequest_identityProviderConfigName :: Lens.Lens' OidcIdentityProviderConfigRequest Prelude.Text
oidcIdentityProviderConfigRequest_identityProviderConfigName = Lens.lens (\OidcIdentityProviderConfigRequest' {identityProviderConfigName} -> identityProviderConfigName) (\s@OidcIdentityProviderConfigRequest' {} a -> s {identityProviderConfigName = a} :: OidcIdentityProviderConfigRequest)

-- | The URL of the OpenID identity provider that allows the API server to
-- discover public signing keys for verifying tokens. The URL must begin
-- with @https:\/\/@ and should correspond to the @iss@ claim in the
-- provider\'s OIDC ID tokens. Per the OIDC standard, path components are
-- allowed but query parameters are not. Typically the URL consists of only
-- a hostname, like @https:\/\/server.example.org@ or
-- @https:\/\/example.com@. This URL should point to the level below
-- @.well-known\/openid-configuration@ and must be publicly accessible over
-- the internet.
oidcIdentityProviderConfigRequest_issuerUrl :: Lens.Lens' OidcIdentityProviderConfigRequest Prelude.Text
oidcIdentityProviderConfigRequest_issuerUrl = Lens.lens (\OidcIdentityProviderConfigRequest' {issuerUrl} -> issuerUrl) (\s@OidcIdentityProviderConfigRequest' {} a -> s {issuerUrl = a} :: OidcIdentityProviderConfigRequest)

-- | This is also known as /audience/. The ID for the client application that
-- makes authentication requests to the OpenID identity provider.
oidcIdentityProviderConfigRequest_clientId :: Lens.Lens' OidcIdentityProviderConfigRequest Prelude.Text
oidcIdentityProviderConfigRequest_clientId = Lens.lens (\OidcIdentityProviderConfigRequest' {clientId} -> clientId) (\s@OidcIdentityProviderConfigRequest' {} a -> s {clientId = a} :: OidcIdentityProviderConfigRequest)

instance
  Prelude.Hashable
    OidcIdentityProviderConfigRequest
  where
  hashWithSalt
    _salt
    OidcIdentityProviderConfigRequest' {..} =
      _salt `Prelude.hashWithSalt` groupsClaim
        `Prelude.hashWithSalt` groupsPrefix
        `Prelude.hashWithSalt` requiredClaims
        `Prelude.hashWithSalt` usernameClaim
        `Prelude.hashWithSalt` usernamePrefix
        `Prelude.hashWithSalt` identityProviderConfigName
        `Prelude.hashWithSalt` issuerUrl
        `Prelude.hashWithSalt` clientId

instance
  Prelude.NFData
    OidcIdentityProviderConfigRequest
  where
  rnf OidcIdentityProviderConfigRequest' {..} =
    Prelude.rnf groupsClaim
      `Prelude.seq` Prelude.rnf groupsPrefix
      `Prelude.seq` Prelude.rnf requiredClaims
      `Prelude.seq` Prelude.rnf usernameClaim
      `Prelude.seq` Prelude.rnf usernamePrefix
      `Prelude.seq` Prelude.rnf identityProviderConfigName
      `Prelude.seq` Prelude.rnf issuerUrl
      `Prelude.seq` Prelude.rnf clientId

instance
  Data.ToJSON
    OidcIdentityProviderConfigRequest
  where
  toJSON OidcIdentityProviderConfigRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("groupsClaim" Data..=) Prelude.<$> groupsClaim,
            ("groupsPrefix" Data..=) Prelude.<$> groupsPrefix,
            ("requiredClaims" Data..=)
              Prelude.<$> requiredClaims,
            ("usernameClaim" Data..=) Prelude.<$> usernameClaim,
            ("usernamePrefix" Data..=)
              Prelude.<$> usernamePrefix,
            Prelude.Just
              ( "identityProviderConfigName"
                  Data..= identityProviderConfigName
              ),
            Prelude.Just ("issuerUrl" Data..= issuerUrl),
            Prelude.Just ("clientId" Data..= clientId)
          ]
      )
