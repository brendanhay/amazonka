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
-- Module      : Network.AWS.EKS.Types.OidcIdentityProviderConfigRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.OidcIdentityProviderConfigRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing an OpenID Connect (OIDC) configuration. Before
-- associating an OIDC identity provider to your cluster, review the
-- considerations in
-- <https://docs.aws.amazon.com/eks/latest/userguide/authenticate-oidc-identity-provider.html Authenticating users for your cluster from an OpenID Connect identity provider>
-- in the /Amazon EKS User Guide/.
--
-- /See:/ 'newOidcIdentityProviderConfigRequest' smart constructor.
data OidcIdentityProviderConfigRequest = OidcIdentityProviderConfigRequest'
  { -- | The prefix that is prepended to group claims to prevent clashes with
    -- existing names (such as @system:@ groups). For example, the
    -- value@ oidc:@ will create group names like @oidc:engineering@ and
    -- @oidc:infra@.
    groupsPrefix :: Core.Maybe Core.Text,
    -- | The JWT claim that the provider uses to return your groups.
    groupsClaim :: Core.Maybe Core.Text,
    -- | The key value pairs that describe required claims in the identity token.
    -- If set, each claim is verified to be present in the token with a
    -- matching value. For the maximum number of claims that you can require,
    -- see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/service-quotas.html Amazon EKS service quotas>
    -- in the /Amazon EKS User Guide/.
    requiredClaims :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The JSON Web Token (JWT) claim to use as the username. The default is
    -- @sub@, which is expected to be a unique identifier of the end user. You
    -- can choose other claims, such as @email@ or @name@, depending on the
    -- OpenID identity provider. Claims other than @email@ are prefixed with
    -- the issuer URL to prevent naming clashes with other plug-ins.
    usernameClaim :: Core.Maybe Core.Text,
    -- | The prefix that is prepended to username claims to prevent clashes with
    -- existing names. If you do not provide this field, and @username@ is a
    -- value other than @email@, the prefix defaults to @issuerurl#@. You can
    -- use the value @-@ to disable all prefixing.
    usernamePrefix :: Core.Maybe Core.Text,
    -- | The name of the OIDC provider configuration.
    identityProviderConfigName :: Core.Text,
    -- | The URL of the OpenID identity provider that allows the API server to
    -- discover public signing keys for verifying tokens. The URL must begin
    -- with @https:\/\/@ and should correspond to the @iss@ claim in the
    -- provider\'s OIDC ID tokens. Per the OIDC standard, path components are
    -- allowed but query parameters are not. Typically the URL consists of only
    -- a hostname, like @https:\/\/server.example.org@ or
    -- @https:\/\/example.com@. This URL should point to the level below
    -- @.well-known\/openid-configuration@ and must be publicly accessible over
    -- the internet.
    issuerUrl :: Core.Text,
    -- | This is also known as /audience/. The ID for the client application that
    -- makes authentication requests to the OpenID identity provider.
    clientId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OidcIdentityProviderConfigRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupsPrefix', 'oidcIdentityProviderConfigRequest_groupsPrefix' - The prefix that is prepended to group claims to prevent clashes with
-- existing names (such as @system:@ groups). For example, the
-- value@ oidc:@ will create group names like @oidc:engineering@ and
-- @oidc:infra@.
--
-- 'groupsClaim', 'oidcIdentityProviderConfigRequest_groupsClaim' - The JWT claim that the provider uses to return your groups.
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
  Core.Text ->
  -- | 'issuerUrl'
  Core.Text ->
  -- | 'clientId'
  Core.Text ->
  OidcIdentityProviderConfigRequest
newOidcIdentityProviderConfigRequest
  pIdentityProviderConfigName_
  pIssuerUrl_
  pClientId_ =
    OidcIdentityProviderConfigRequest'
      { groupsPrefix =
          Core.Nothing,
        groupsClaim = Core.Nothing,
        requiredClaims = Core.Nothing,
        usernameClaim = Core.Nothing,
        usernamePrefix = Core.Nothing,
        identityProviderConfigName =
          pIdentityProviderConfigName_,
        issuerUrl = pIssuerUrl_,
        clientId = pClientId_
      }

-- | The prefix that is prepended to group claims to prevent clashes with
-- existing names (such as @system:@ groups). For example, the
-- value@ oidc:@ will create group names like @oidc:engineering@ and
-- @oidc:infra@.
oidcIdentityProviderConfigRequest_groupsPrefix :: Lens.Lens' OidcIdentityProviderConfigRequest (Core.Maybe Core.Text)
oidcIdentityProviderConfigRequest_groupsPrefix = Lens.lens (\OidcIdentityProviderConfigRequest' {groupsPrefix} -> groupsPrefix) (\s@OidcIdentityProviderConfigRequest' {} a -> s {groupsPrefix = a} :: OidcIdentityProviderConfigRequest)

-- | The JWT claim that the provider uses to return your groups.
oidcIdentityProviderConfigRequest_groupsClaim :: Lens.Lens' OidcIdentityProviderConfigRequest (Core.Maybe Core.Text)
oidcIdentityProviderConfigRequest_groupsClaim = Lens.lens (\OidcIdentityProviderConfigRequest' {groupsClaim} -> groupsClaim) (\s@OidcIdentityProviderConfigRequest' {} a -> s {groupsClaim = a} :: OidcIdentityProviderConfigRequest)

-- | The key value pairs that describe required claims in the identity token.
-- If set, each claim is verified to be present in the token with a
-- matching value. For the maximum number of claims that you can require,
-- see
-- <https://docs.aws.amazon.com/eks/latest/userguide/service-quotas.html Amazon EKS service quotas>
-- in the /Amazon EKS User Guide/.
oidcIdentityProviderConfigRequest_requiredClaims :: Lens.Lens' OidcIdentityProviderConfigRequest (Core.Maybe (Core.HashMap Core.Text Core.Text))
oidcIdentityProviderConfigRequest_requiredClaims = Lens.lens (\OidcIdentityProviderConfigRequest' {requiredClaims} -> requiredClaims) (\s@OidcIdentityProviderConfigRequest' {} a -> s {requiredClaims = a} :: OidcIdentityProviderConfigRequest) Core.. Lens.mapping Lens._Coerce

-- | The JSON Web Token (JWT) claim to use as the username. The default is
-- @sub@, which is expected to be a unique identifier of the end user. You
-- can choose other claims, such as @email@ or @name@, depending on the
-- OpenID identity provider. Claims other than @email@ are prefixed with
-- the issuer URL to prevent naming clashes with other plug-ins.
oidcIdentityProviderConfigRequest_usernameClaim :: Lens.Lens' OidcIdentityProviderConfigRequest (Core.Maybe Core.Text)
oidcIdentityProviderConfigRequest_usernameClaim = Lens.lens (\OidcIdentityProviderConfigRequest' {usernameClaim} -> usernameClaim) (\s@OidcIdentityProviderConfigRequest' {} a -> s {usernameClaim = a} :: OidcIdentityProviderConfigRequest)

-- | The prefix that is prepended to username claims to prevent clashes with
-- existing names. If you do not provide this field, and @username@ is a
-- value other than @email@, the prefix defaults to @issuerurl#@. You can
-- use the value @-@ to disable all prefixing.
oidcIdentityProviderConfigRequest_usernamePrefix :: Lens.Lens' OidcIdentityProviderConfigRequest (Core.Maybe Core.Text)
oidcIdentityProviderConfigRequest_usernamePrefix = Lens.lens (\OidcIdentityProviderConfigRequest' {usernamePrefix} -> usernamePrefix) (\s@OidcIdentityProviderConfigRequest' {} a -> s {usernamePrefix = a} :: OidcIdentityProviderConfigRequest)

-- | The name of the OIDC provider configuration.
oidcIdentityProviderConfigRequest_identityProviderConfigName :: Lens.Lens' OidcIdentityProviderConfigRequest Core.Text
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
oidcIdentityProviderConfigRequest_issuerUrl :: Lens.Lens' OidcIdentityProviderConfigRequest Core.Text
oidcIdentityProviderConfigRequest_issuerUrl = Lens.lens (\OidcIdentityProviderConfigRequest' {issuerUrl} -> issuerUrl) (\s@OidcIdentityProviderConfigRequest' {} a -> s {issuerUrl = a} :: OidcIdentityProviderConfigRequest)

-- | This is also known as /audience/. The ID for the client application that
-- makes authentication requests to the OpenID identity provider.
oidcIdentityProviderConfigRequest_clientId :: Lens.Lens' OidcIdentityProviderConfigRequest Core.Text
oidcIdentityProviderConfigRequest_clientId = Lens.lens (\OidcIdentityProviderConfigRequest' {clientId} -> clientId) (\s@OidcIdentityProviderConfigRequest' {} a -> s {clientId = a} :: OidcIdentityProviderConfigRequest)

instance
  Core.Hashable
    OidcIdentityProviderConfigRequest

instance
  Core.NFData
    OidcIdentityProviderConfigRequest

instance
  Core.ToJSON
    OidcIdentityProviderConfigRequest
  where
  toJSON OidcIdentityProviderConfigRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("groupsPrefix" Core..=) Core.<$> groupsPrefix,
            ("groupsClaim" Core..=) Core.<$> groupsClaim,
            ("requiredClaims" Core..=) Core.<$> requiredClaims,
            ("usernameClaim" Core..=) Core.<$> usernameClaim,
            ("usernamePrefix" Core..=) Core.<$> usernamePrefix,
            Core.Just
              ( "identityProviderConfigName"
                  Core..= identityProviderConfigName
              ),
            Core.Just ("issuerUrl" Core..= issuerUrl),
            Core.Just ("clientId" Core..= clientId)
          ]
      )
