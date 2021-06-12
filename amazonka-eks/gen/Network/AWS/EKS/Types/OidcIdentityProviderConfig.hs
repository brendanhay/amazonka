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
-- Module      : Network.AWS.EKS.Types.OidcIdentityProviderConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.OidcIdentityProviderConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.ConfigStatus
import qualified Network.AWS.Lens as Lens

-- | An object that represents the configuration for an OpenID Connect (OIDC)
-- identity provider.
--
-- /See:/ 'newOidcIdentityProviderConfig' smart constructor.
data OidcIdentityProviderConfig = OidcIdentityProviderConfig'
  { -- | This is also known as /audience/. The ID of the client application that
    -- makes authentication requests to the OIDC identity provider.
    clientId :: Core.Maybe Core.Text,
    -- | The prefix that is prepended to group claims to prevent clashes with
    -- existing names (such as @system:@ groups). For example, the
    -- value@ oidc:@ creates group names like @oidc:engineering@ and
    -- @oidc:infra@. The prefix can\'t contain @system:@
    groupsPrefix :: Core.Maybe Core.Text,
    -- | The status of the OIDC identity provider.
    status :: Core.Maybe ConfigStatus,
    -- | The JSON web token (JWT) claim that the provider uses to return your
    -- groups.
    groupsClaim :: Core.Maybe Core.Text,
    -- | The key-value pairs that describe required claims in the identity token.
    -- If set, each claim is verified to be present in the token with a
    -- matching value.
    requiredClaims :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the configuration.
    identityProviderConfigName :: Core.Maybe Core.Text,
    -- | The JSON Web token (JWT) claim that is used as the username.
    usernameClaim :: Core.Maybe Core.Text,
    -- | The metadata to apply to the provider configuration to assist with
    -- categorization and organization. Each tag consists of a key and an
    -- optional value, both of which you defined.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The prefix that is prepended to username claims to prevent clashes with
    -- existing names. The prefix can\'t contain @system:@
    usernamePrefix :: Core.Maybe Core.Text,
    -- | The URL of the OIDC identity provider that allows the API server to
    -- discover public signing keys for verifying tokens.
    issuerUrl :: Core.Maybe Core.Text,
    -- | The ARN of the configuration.
    identityProviderConfigArn :: Core.Maybe Core.Text,
    -- | The cluster that the configuration is associated to.
    clusterName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OidcIdentityProviderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'oidcIdentityProviderConfig_clientId' - This is also known as /audience/. The ID of the client application that
-- makes authentication requests to the OIDC identity provider.
--
-- 'groupsPrefix', 'oidcIdentityProviderConfig_groupsPrefix' - The prefix that is prepended to group claims to prevent clashes with
-- existing names (such as @system:@ groups). For example, the
-- value@ oidc:@ creates group names like @oidc:engineering@ and
-- @oidc:infra@. The prefix can\'t contain @system:@
--
-- 'status', 'oidcIdentityProviderConfig_status' - The status of the OIDC identity provider.
--
-- 'groupsClaim', 'oidcIdentityProviderConfig_groupsClaim' - The JSON web token (JWT) claim that the provider uses to return your
-- groups.
--
-- 'requiredClaims', 'oidcIdentityProviderConfig_requiredClaims' - The key-value pairs that describe required claims in the identity token.
-- If set, each claim is verified to be present in the token with a
-- matching value.
--
-- 'identityProviderConfigName', 'oidcIdentityProviderConfig_identityProviderConfigName' - The name of the configuration.
--
-- 'usernameClaim', 'oidcIdentityProviderConfig_usernameClaim' - The JSON Web token (JWT) claim that is used as the username.
--
-- 'tags', 'oidcIdentityProviderConfig_tags' - The metadata to apply to the provider configuration to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value, both of which you defined.
--
-- 'usernamePrefix', 'oidcIdentityProviderConfig_usernamePrefix' - The prefix that is prepended to username claims to prevent clashes with
-- existing names. The prefix can\'t contain @system:@
--
-- 'issuerUrl', 'oidcIdentityProviderConfig_issuerUrl' - The URL of the OIDC identity provider that allows the API server to
-- discover public signing keys for verifying tokens.
--
-- 'identityProviderConfigArn', 'oidcIdentityProviderConfig_identityProviderConfigArn' - The ARN of the configuration.
--
-- 'clusterName', 'oidcIdentityProviderConfig_clusterName' - The cluster that the configuration is associated to.
newOidcIdentityProviderConfig ::
  OidcIdentityProviderConfig
newOidcIdentityProviderConfig =
  OidcIdentityProviderConfig'
    { clientId =
        Core.Nothing,
      groupsPrefix = Core.Nothing,
      status = Core.Nothing,
      groupsClaim = Core.Nothing,
      requiredClaims = Core.Nothing,
      identityProviderConfigName = Core.Nothing,
      usernameClaim = Core.Nothing,
      tags = Core.Nothing,
      usernamePrefix = Core.Nothing,
      issuerUrl = Core.Nothing,
      identityProviderConfigArn = Core.Nothing,
      clusterName = Core.Nothing
    }

-- | This is also known as /audience/. The ID of the client application that
-- makes authentication requests to the OIDC identity provider.
oidcIdentityProviderConfig_clientId :: Lens.Lens' OidcIdentityProviderConfig (Core.Maybe Core.Text)
oidcIdentityProviderConfig_clientId = Lens.lens (\OidcIdentityProviderConfig' {clientId} -> clientId) (\s@OidcIdentityProviderConfig' {} a -> s {clientId = a} :: OidcIdentityProviderConfig)

-- | The prefix that is prepended to group claims to prevent clashes with
-- existing names (such as @system:@ groups). For example, the
-- value@ oidc:@ creates group names like @oidc:engineering@ and
-- @oidc:infra@. The prefix can\'t contain @system:@
oidcIdentityProviderConfig_groupsPrefix :: Lens.Lens' OidcIdentityProviderConfig (Core.Maybe Core.Text)
oidcIdentityProviderConfig_groupsPrefix = Lens.lens (\OidcIdentityProviderConfig' {groupsPrefix} -> groupsPrefix) (\s@OidcIdentityProviderConfig' {} a -> s {groupsPrefix = a} :: OidcIdentityProviderConfig)

-- | The status of the OIDC identity provider.
oidcIdentityProviderConfig_status :: Lens.Lens' OidcIdentityProviderConfig (Core.Maybe ConfigStatus)
oidcIdentityProviderConfig_status = Lens.lens (\OidcIdentityProviderConfig' {status} -> status) (\s@OidcIdentityProviderConfig' {} a -> s {status = a} :: OidcIdentityProviderConfig)

-- | The JSON web token (JWT) claim that the provider uses to return your
-- groups.
oidcIdentityProviderConfig_groupsClaim :: Lens.Lens' OidcIdentityProviderConfig (Core.Maybe Core.Text)
oidcIdentityProviderConfig_groupsClaim = Lens.lens (\OidcIdentityProviderConfig' {groupsClaim} -> groupsClaim) (\s@OidcIdentityProviderConfig' {} a -> s {groupsClaim = a} :: OidcIdentityProviderConfig)

-- | The key-value pairs that describe required claims in the identity token.
-- If set, each claim is verified to be present in the token with a
-- matching value.
oidcIdentityProviderConfig_requiredClaims :: Lens.Lens' OidcIdentityProviderConfig (Core.Maybe (Core.HashMap Core.Text Core.Text))
oidcIdentityProviderConfig_requiredClaims = Lens.lens (\OidcIdentityProviderConfig' {requiredClaims} -> requiredClaims) (\s@OidcIdentityProviderConfig' {} a -> s {requiredClaims = a} :: OidcIdentityProviderConfig) Core.. Lens.mapping Lens._Coerce

-- | The name of the configuration.
oidcIdentityProviderConfig_identityProviderConfigName :: Lens.Lens' OidcIdentityProviderConfig (Core.Maybe Core.Text)
oidcIdentityProviderConfig_identityProviderConfigName = Lens.lens (\OidcIdentityProviderConfig' {identityProviderConfigName} -> identityProviderConfigName) (\s@OidcIdentityProviderConfig' {} a -> s {identityProviderConfigName = a} :: OidcIdentityProviderConfig)

-- | The JSON Web token (JWT) claim that is used as the username.
oidcIdentityProviderConfig_usernameClaim :: Lens.Lens' OidcIdentityProviderConfig (Core.Maybe Core.Text)
oidcIdentityProviderConfig_usernameClaim = Lens.lens (\OidcIdentityProviderConfig' {usernameClaim} -> usernameClaim) (\s@OidcIdentityProviderConfig' {} a -> s {usernameClaim = a} :: OidcIdentityProviderConfig)

-- | The metadata to apply to the provider configuration to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value, both of which you defined.
oidcIdentityProviderConfig_tags :: Lens.Lens' OidcIdentityProviderConfig (Core.Maybe (Core.HashMap Core.Text Core.Text))
oidcIdentityProviderConfig_tags = Lens.lens (\OidcIdentityProviderConfig' {tags} -> tags) (\s@OidcIdentityProviderConfig' {} a -> s {tags = a} :: OidcIdentityProviderConfig) Core.. Lens.mapping Lens._Coerce

-- | The prefix that is prepended to username claims to prevent clashes with
-- existing names. The prefix can\'t contain @system:@
oidcIdentityProviderConfig_usernamePrefix :: Lens.Lens' OidcIdentityProviderConfig (Core.Maybe Core.Text)
oidcIdentityProviderConfig_usernamePrefix = Lens.lens (\OidcIdentityProviderConfig' {usernamePrefix} -> usernamePrefix) (\s@OidcIdentityProviderConfig' {} a -> s {usernamePrefix = a} :: OidcIdentityProviderConfig)

-- | The URL of the OIDC identity provider that allows the API server to
-- discover public signing keys for verifying tokens.
oidcIdentityProviderConfig_issuerUrl :: Lens.Lens' OidcIdentityProviderConfig (Core.Maybe Core.Text)
oidcIdentityProviderConfig_issuerUrl = Lens.lens (\OidcIdentityProviderConfig' {issuerUrl} -> issuerUrl) (\s@OidcIdentityProviderConfig' {} a -> s {issuerUrl = a} :: OidcIdentityProviderConfig)

-- | The ARN of the configuration.
oidcIdentityProviderConfig_identityProviderConfigArn :: Lens.Lens' OidcIdentityProviderConfig (Core.Maybe Core.Text)
oidcIdentityProviderConfig_identityProviderConfigArn = Lens.lens (\OidcIdentityProviderConfig' {identityProviderConfigArn} -> identityProviderConfigArn) (\s@OidcIdentityProviderConfig' {} a -> s {identityProviderConfigArn = a} :: OidcIdentityProviderConfig)

-- | The cluster that the configuration is associated to.
oidcIdentityProviderConfig_clusterName :: Lens.Lens' OidcIdentityProviderConfig (Core.Maybe Core.Text)
oidcIdentityProviderConfig_clusterName = Lens.lens (\OidcIdentityProviderConfig' {clusterName} -> clusterName) (\s@OidcIdentityProviderConfig' {} a -> s {clusterName = a} :: OidcIdentityProviderConfig)

instance Core.FromJSON OidcIdentityProviderConfig where
  parseJSON =
    Core.withObject
      "OidcIdentityProviderConfig"
      ( \x ->
          OidcIdentityProviderConfig'
            Core.<$> (x Core..:? "clientId")
            Core.<*> (x Core..:? "groupsPrefix")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "groupsClaim")
            Core.<*> (x Core..:? "requiredClaims" Core..!= Core.mempty)
            Core.<*> (x Core..:? "identityProviderConfigName")
            Core.<*> (x Core..:? "usernameClaim")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "usernamePrefix")
            Core.<*> (x Core..:? "issuerUrl")
            Core.<*> (x Core..:? "identityProviderConfigArn")
            Core.<*> (x Core..:? "clusterName")
      )

instance Core.Hashable OidcIdentityProviderConfig

instance Core.NFData OidcIdentityProviderConfig
