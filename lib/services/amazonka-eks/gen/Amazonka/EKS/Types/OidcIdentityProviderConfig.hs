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
-- Module      : Amazonka.EKS.Types.OidcIdentityProviderConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.OidcIdentityProviderConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.ConfigStatus
import qualified Amazonka.Prelude as Prelude

-- | An object representing the configuration for an OpenID Connect (OIDC)
-- identity provider.
--
-- /See:/ 'newOidcIdentityProviderConfig' smart constructor.
data OidcIdentityProviderConfig = OidcIdentityProviderConfig'
  { -- | The metadata to apply to the provider configuration to assist with
    -- categorization and organization. Each tag consists of a key and an
    -- optional value. You define both.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The key-value pairs that describe required claims in the identity token.
    -- If set, each claim is verified to be present in the token with a
    -- matching value.
    requiredClaims :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | This is also known as /audience/. The ID of the client application that
    -- makes authentication requests to the OIDC identity provider.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration.
    identityProviderConfigName :: Prelude.Maybe Prelude.Text,
    -- | The status of the OIDC identity provider.
    status :: Prelude.Maybe ConfigStatus,
    -- | The prefix that is prepended to username claims to prevent clashes with
    -- existing names. The prefix can\'t contain @system:@
    usernamePrefix :: Prelude.Maybe Prelude.Text,
    -- | The JSON web token (JWT) claim that the provider uses to return your
    -- groups.
    groupsClaim :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the configuration.
    identityProviderConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The URL of the OIDC identity provider that allows the API server to
    -- discover public signing keys for verifying tokens.
    issuerUrl :: Prelude.Maybe Prelude.Text,
    -- | The prefix that is prepended to group claims to prevent clashes with
    -- existing names (such as @system:@ groups). For example, the
    -- value@ oidc:@ creates group names like @oidc:engineering@ and
    -- @oidc:infra@. The prefix can\'t contain @system:@
    groupsPrefix :: Prelude.Maybe Prelude.Text,
    -- | The cluster that the configuration is associated to.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The JSON Web token (JWT) claim that is used as the username.
    usernameClaim :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OidcIdentityProviderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'oidcIdentityProviderConfig_tags' - The metadata to apply to the provider configuration to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value. You define both.
--
-- 'requiredClaims', 'oidcIdentityProviderConfig_requiredClaims' - The key-value pairs that describe required claims in the identity token.
-- If set, each claim is verified to be present in the token with a
-- matching value.
--
-- 'clientId', 'oidcIdentityProviderConfig_clientId' - This is also known as /audience/. The ID of the client application that
-- makes authentication requests to the OIDC identity provider.
--
-- 'identityProviderConfigName', 'oidcIdentityProviderConfig_identityProviderConfigName' - The name of the configuration.
--
-- 'status', 'oidcIdentityProviderConfig_status' - The status of the OIDC identity provider.
--
-- 'usernamePrefix', 'oidcIdentityProviderConfig_usernamePrefix' - The prefix that is prepended to username claims to prevent clashes with
-- existing names. The prefix can\'t contain @system:@
--
-- 'groupsClaim', 'oidcIdentityProviderConfig_groupsClaim' - The JSON web token (JWT) claim that the provider uses to return your
-- groups.
--
-- 'identityProviderConfigArn', 'oidcIdentityProviderConfig_identityProviderConfigArn' - The ARN of the configuration.
--
-- 'issuerUrl', 'oidcIdentityProviderConfig_issuerUrl' - The URL of the OIDC identity provider that allows the API server to
-- discover public signing keys for verifying tokens.
--
-- 'groupsPrefix', 'oidcIdentityProviderConfig_groupsPrefix' - The prefix that is prepended to group claims to prevent clashes with
-- existing names (such as @system:@ groups). For example, the
-- value@ oidc:@ creates group names like @oidc:engineering@ and
-- @oidc:infra@. The prefix can\'t contain @system:@
--
-- 'clusterName', 'oidcIdentityProviderConfig_clusterName' - The cluster that the configuration is associated to.
--
-- 'usernameClaim', 'oidcIdentityProviderConfig_usernameClaim' - The JSON Web token (JWT) claim that is used as the username.
newOidcIdentityProviderConfig ::
  OidcIdentityProviderConfig
newOidcIdentityProviderConfig =
  OidcIdentityProviderConfig'
    { tags = Prelude.Nothing,
      requiredClaims = Prelude.Nothing,
      clientId = Prelude.Nothing,
      identityProviderConfigName = Prelude.Nothing,
      status = Prelude.Nothing,
      usernamePrefix = Prelude.Nothing,
      groupsClaim = Prelude.Nothing,
      identityProviderConfigArn = Prelude.Nothing,
      issuerUrl = Prelude.Nothing,
      groupsPrefix = Prelude.Nothing,
      clusterName = Prelude.Nothing,
      usernameClaim = Prelude.Nothing
    }

-- | The metadata to apply to the provider configuration to assist with
-- categorization and organization. Each tag consists of a key and an
-- optional value. You define both.
oidcIdentityProviderConfig_tags :: Lens.Lens' OidcIdentityProviderConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
oidcIdentityProviderConfig_tags = Lens.lens (\OidcIdentityProviderConfig' {tags} -> tags) (\s@OidcIdentityProviderConfig' {} a -> s {tags = a} :: OidcIdentityProviderConfig) Prelude.. Lens.mapping Lens.coerced

-- | The key-value pairs that describe required claims in the identity token.
-- If set, each claim is verified to be present in the token with a
-- matching value.
oidcIdentityProviderConfig_requiredClaims :: Lens.Lens' OidcIdentityProviderConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
oidcIdentityProviderConfig_requiredClaims = Lens.lens (\OidcIdentityProviderConfig' {requiredClaims} -> requiredClaims) (\s@OidcIdentityProviderConfig' {} a -> s {requiredClaims = a} :: OidcIdentityProviderConfig) Prelude.. Lens.mapping Lens.coerced

-- | This is also known as /audience/. The ID of the client application that
-- makes authentication requests to the OIDC identity provider.
oidcIdentityProviderConfig_clientId :: Lens.Lens' OidcIdentityProviderConfig (Prelude.Maybe Prelude.Text)
oidcIdentityProviderConfig_clientId = Lens.lens (\OidcIdentityProviderConfig' {clientId} -> clientId) (\s@OidcIdentityProviderConfig' {} a -> s {clientId = a} :: OidcIdentityProviderConfig)

-- | The name of the configuration.
oidcIdentityProviderConfig_identityProviderConfigName :: Lens.Lens' OidcIdentityProviderConfig (Prelude.Maybe Prelude.Text)
oidcIdentityProviderConfig_identityProviderConfigName = Lens.lens (\OidcIdentityProviderConfig' {identityProviderConfigName} -> identityProviderConfigName) (\s@OidcIdentityProviderConfig' {} a -> s {identityProviderConfigName = a} :: OidcIdentityProviderConfig)

-- | The status of the OIDC identity provider.
oidcIdentityProviderConfig_status :: Lens.Lens' OidcIdentityProviderConfig (Prelude.Maybe ConfigStatus)
oidcIdentityProviderConfig_status = Lens.lens (\OidcIdentityProviderConfig' {status} -> status) (\s@OidcIdentityProviderConfig' {} a -> s {status = a} :: OidcIdentityProviderConfig)

-- | The prefix that is prepended to username claims to prevent clashes with
-- existing names. The prefix can\'t contain @system:@
oidcIdentityProviderConfig_usernamePrefix :: Lens.Lens' OidcIdentityProviderConfig (Prelude.Maybe Prelude.Text)
oidcIdentityProviderConfig_usernamePrefix = Lens.lens (\OidcIdentityProviderConfig' {usernamePrefix} -> usernamePrefix) (\s@OidcIdentityProviderConfig' {} a -> s {usernamePrefix = a} :: OidcIdentityProviderConfig)

-- | The JSON web token (JWT) claim that the provider uses to return your
-- groups.
oidcIdentityProviderConfig_groupsClaim :: Lens.Lens' OidcIdentityProviderConfig (Prelude.Maybe Prelude.Text)
oidcIdentityProviderConfig_groupsClaim = Lens.lens (\OidcIdentityProviderConfig' {groupsClaim} -> groupsClaim) (\s@OidcIdentityProviderConfig' {} a -> s {groupsClaim = a} :: OidcIdentityProviderConfig)

-- | The ARN of the configuration.
oidcIdentityProviderConfig_identityProviderConfigArn :: Lens.Lens' OidcIdentityProviderConfig (Prelude.Maybe Prelude.Text)
oidcIdentityProviderConfig_identityProviderConfigArn = Lens.lens (\OidcIdentityProviderConfig' {identityProviderConfigArn} -> identityProviderConfigArn) (\s@OidcIdentityProviderConfig' {} a -> s {identityProviderConfigArn = a} :: OidcIdentityProviderConfig)

-- | The URL of the OIDC identity provider that allows the API server to
-- discover public signing keys for verifying tokens.
oidcIdentityProviderConfig_issuerUrl :: Lens.Lens' OidcIdentityProviderConfig (Prelude.Maybe Prelude.Text)
oidcIdentityProviderConfig_issuerUrl = Lens.lens (\OidcIdentityProviderConfig' {issuerUrl} -> issuerUrl) (\s@OidcIdentityProviderConfig' {} a -> s {issuerUrl = a} :: OidcIdentityProviderConfig)

-- | The prefix that is prepended to group claims to prevent clashes with
-- existing names (such as @system:@ groups). For example, the
-- value@ oidc:@ creates group names like @oidc:engineering@ and
-- @oidc:infra@. The prefix can\'t contain @system:@
oidcIdentityProviderConfig_groupsPrefix :: Lens.Lens' OidcIdentityProviderConfig (Prelude.Maybe Prelude.Text)
oidcIdentityProviderConfig_groupsPrefix = Lens.lens (\OidcIdentityProviderConfig' {groupsPrefix} -> groupsPrefix) (\s@OidcIdentityProviderConfig' {} a -> s {groupsPrefix = a} :: OidcIdentityProviderConfig)

-- | The cluster that the configuration is associated to.
oidcIdentityProviderConfig_clusterName :: Lens.Lens' OidcIdentityProviderConfig (Prelude.Maybe Prelude.Text)
oidcIdentityProviderConfig_clusterName = Lens.lens (\OidcIdentityProviderConfig' {clusterName} -> clusterName) (\s@OidcIdentityProviderConfig' {} a -> s {clusterName = a} :: OidcIdentityProviderConfig)

-- | The JSON Web token (JWT) claim that is used as the username.
oidcIdentityProviderConfig_usernameClaim :: Lens.Lens' OidcIdentityProviderConfig (Prelude.Maybe Prelude.Text)
oidcIdentityProviderConfig_usernameClaim = Lens.lens (\OidcIdentityProviderConfig' {usernameClaim} -> usernameClaim) (\s@OidcIdentityProviderConfig' {} a -> s {usernameClaim = a} :: OidcIdentityProviderConfig)

instance Data.FromJSON OidcIdentityProviderConfig where
  parseJSON =
    Data.withObject
      "OidcIdentityProviderConfig"
      ( \x ->
          OidcIdentityProviderConfig'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "requiredClaims" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "clientId")
            Prelude.<*> (x Data..:? "identityProviderConfigName")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "usernamePrefix")
            Prelude.<*> (x Data..:? "groupsClaim")
            Prelude.<*> (x Data..:? "identityProviderConfigArn")
            Prelude.<*> (x Data..:? "issuerUrl")
            Prelude.<*> (x Data..:? "groupsPrefix")
            Prelude.<*> (x Data..:? "clusterName")
            Prelude.<*> (x Data..:? "usernameClaim")
      )

instance Prelude.Hashable OidcIdentityProviderConfig where
  hashWithSalt _salt OidcIdentityProviderConfig' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` requiredClaims
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` identityProviderConfigName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` usernamePrefix
      `Prelude.hashWithSalt` groupsClaim
      `Prelude.hashWithSalt` identityProviderConfigArn
      `Prelude.hashWithSalt` issuerUrl
      `Prelude.hashWithSalt` groupsPrefix
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` usernameClaim

instance Prelude.NFData OidcIdentityProviderConfig where
  rnf OidcIdentityProviderConfig' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf requiredClaims
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf identityProviderConfigName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf usernamePrefix
      `Prelude.seq` Prelude.rnf groupsClaim
      `Prelude.seq` Prelude.rnf identityProviderConfigArn
      `Prelude.seq` Prelude.rnf issuerUrl
      `Prelude.seq` Prelude.rnf groupsPrefix
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf usernameClaim
