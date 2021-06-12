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
-- Module      : Network.AWS.AppSync.Types.GraphqlApi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.GraphqlApi where

import Network.AWS.AppSync.Types.AdditionalAuthenticationProvider
import Network.AWS.AppSync.Types.AuthenticationType
import Network.AWS.AppSync.Types.LogConfig
import Network.AWS.AppSync.Types.OpenIDConnectConfig
import Network.AWS.AppSync.Types.UserPoolConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a GraphQL API.
--
-- /See:/ 'newGraphqlApi' smart constructor.
data GraphqlApi = GraphqlApi'
  { -- | The ARN of the AWS Web Application Firewall (WAF) ACL associated with
    -- this @GraphqlApi@, if one exists.
    wafWebAclArn :: Core.Maybe Core.Text,
    -- | The OpenID Connect configuration.
    openIDConnectConfig :: Core.Maybe OpenIDConnectConfig,
    -- | The API ID.
    apiId :: Core.Maybe Core.Text,
    -- | The ARN.
    arn :: Core.Maybe Core.Text,
    -- | The API name.
    name :: Core.Maybe Core.Text,
    -- | The Amazon Cognito user pool configuration.
    userPoolConfig :: Core.Maybe UserPoolConfig,
    -- | A flag representing whether X-Ray tracing is enabled for this
    -- @GraphqlApi@.
    xrayEnabled :: Core.Maybe Core.Bool,
    -- | The tags.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The Amazon CloudWatch Logs configuration.
    logConfig :: Core.Maybe LogConfig,
    -- | A list of additional authentication providers for the @GraphqlApi@ API.
    additionalAuthenticationProviders :: Core.Maybe [AdditionalAuthenticationProvider],
    -- | The authentication type.
    authenticationType :: Core.Maybe AuthenticationType,
    -- | The URIs.
    uris :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GraphqlApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'wafWebAclArn', 'graphqlApi_wafWebAclArn' - The ARN of the AWS Web Application Firewall (WAF) ACL associated with
-- this @GraphqlApi@, if one exists.
--
-- 'openIDConnectConfig', 'graphqlApi_openIDConnectConfig' - The OpenID Connect configuration.
--
-- 'apiId', 'graphqlApi_apiId' - The API ID.
--
-- 'arn', 'graphqlApi_arn' - The ARN.
--
-- 'name', 'graphqlApi_name' - The API name.
--
-- 'userPoolConfig', 'graphqlApi_userPoolConfig' - The Amazon Cognito user pool configuration.
--
-- 'xrayEnabled', 'graphqlApi_xrayEnabled' - A flag representing whether X-Ray tracing is enabled for this
-- @GraphqlApi@.
--
-- 'tags', 'graphqlApi_tags' - The tags.
--
-- 'logConfig', 'graphqlApi_logConfig' - The Amazon CloudWatch Logs configuration.
--
-- 'additionalAuthenticationProviders', 'graphqlApi_additionalAuthenticationProviders' - A list of additional authentication providers for the @GraphqlApi@ API.
--
-- 'authenticationType', 'graphqlApi_authenticationType' - The authentication type.
--
-- 'uris', 'graphqlApi_uris' - The URIs.
newGraphqlApi ::
  GraphqlApi
newGraphqlApi =
  GraphqlApi'
    { wafWebAclArn = Core.Nothing,
      openIDConnectConfig = Core.Nothing,
      apiId = Core.Nothing,
      arn = Core.Nothing,
      name = Core.Nothing,
      userPoolConfig = Core.Nothing,
      xrayEnabled = Core.Nothing,
      tags = Core.Nothing,
      logConfig = Core.Nothing,
      additionalAuthenticationProviders = Core.Nothing,
      authenticationType = Core.Nothing,
      uris = Core.Nothing
    }

-- | The ARN of the AWS Web Application Firewall (WAF) ACL associated with
-- this @GraphqlApi@, if one exists.
graphqlApi_wafWebAclArn :: Lens.Lens' GraphqlApi (Core.Maybe Core.Text)
graphqlApi_wafWebAclArn = Lens.lens (\GraphqlApi' {wafWebAclArn} -> wafWebAclArn) (\s@GraphqlApi' {} a -> s {wafWebAclArn = a} :: GraphqlApi)

-- | The OpenID Connect configuration.
graphqlApi_openIDConnectConfig :: Lens.Lens' GraphqlApi (Core.Maybe OpenIDConnectConfig)
graphqlApi_openIDConnectConfig = Lens.lens (\GraphqlApi' {openIDConnectConfig} -> openIDConnectConfig) (\s@GraphqlApi' {} a -> s {openIDConnectConfig = a} :: GraphqlApi)

-- | The API ID.
graphqlApi_apiId :: Lens.Lens' GraphqlApi (Core.Maybe Core.Text)
graphqlApi_apiId = Lens.lens (\GraphqlApi' {apiId} -> apiId) (\s@GraphqlApi' {} a -> s {apiId = a} :: GraphqlApi)

-- | The ARN.
graphqlApi_arn :: Lens.Lens' GraphqlApi (Core.Maybe Core.Text)
graphqlApi_arn = Lens.lens (\GraphqlApi' {arn} -> arn) (\s@GraphqlApi' {} a -> s {arn = a} :: GraphqlApi)

-- | The API name.
graphqlApi_name :: Lens.Lens' GraphqlApi (Core.Maybe Core.Text)
graphqlApi_name = Lens.lens (\GraphqlApi' {name} -> name) (\s@GraphqlApi' {} a -> s {name = a} :: GraphqlApi)

-- | The Amazon Cognito user pool configuration.
graphqlApi_userPoolConfig :: Lens.Lens' GraphqlApi (Core.Maybe UserPoolConfig)
graphqlApi_userPoolConfig = Lens.lens (\GraphqlApi' {userPoolConfig} -> userPoolConfig) (\s@GraphqlApi' {} a -> s {userPoolConfig = a} :: GraphqlApi)

-- | A flag representing whether X-Ray tracing is enabled for this
-- @GraphqlApi@.
graphqlApi_xrayEnabled :: Lens.Lens' GraphqlApi (Core.Maybe Core.Bool)
graphqlApi_xrayEnabled = Lens.lens (\GraphqlApi' {xrayEnabled} -> xrayEnabled) (\s@GraphqlApi' {} a -> s {xrayEnabled = a} :: GraphqlApi)

-- | The tags.
graphqlApi_tags :: Lens.Lens' GraphqlApi (Core.Maybe (Core.HashMap Core.Text Core.Text))
graphqlApi_tags = Lens.lens (\GraphqlApi' {tags} -> tags) (\s@GraphqlApi' {} a -> s {tags = a} :: GraphqlApi) Core.. Lens.mapping Lens._Coerce

-- | The Amazon CloudWatch Logs configuration.
graphqlApi_logConfig :: Lens.Lens' GraphqlApi (Core.Maybe LogConfig)
graphqlApi_logConfig = Lens.lens (\GraphqlApi' {logConfig} -> logConfig) (\s@GraphqlApi' {} a -> s {logConfig = a} :: GraphqlApi)

-- | A list of additional authentication providers for the @GraphqlApi@ API.
graphqlApi_additionalAuthenticationProviders :: Lens.Lens' GraphqlApi (Core.Maybe [AdditionalAuthenticationProvider])
graphqlApi_additionalAuthenticationProviders = Lens.lens (\GraphqlApi' {additionalAuthenticationProviders} -> additionalAuthenticationProviders) (\s@GraphqlApi' {} a -> s {additionalAuthenticationProviders = a} :: GraphqlApi) Core.. Lens.mapping Lens._Coerce

-- | The authentication type.
graphqlApi_authenticationType :: Lens.Lens' GraphqlApi (Core.Maybe AuthenticationType)
graphqlApi_authenticationType = Lens.lens (\GraphqlApi' {authenticationType} -> authenticationType) (\s@GraphqlApi' {} a -> s {authenticationType = a} :: GraphqlApi)

-- | The URIs.
graphqlApi_uris :: Lens.Lens' GraphqlApi (Core.Maybe (Core.HashMap Core.Text Core.Text))
graphqlApi_uris = Lens.lens (\GraphqlApi' {uris} -> uris) (\s@GraphqlApi' {} a -> s {uris = a} :: GraphqlApi) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON GraphqlApi where
  parseJSON =
    Core.withObject
      "GraphqlApi"
      ( \x ->
          GraphqlApi'
            Core.<$> (x Core..:? "wafWebAclArn")
            Core.<*> (x Core..:? "openIDConnectConfig")
            Core.<*> (x Core..:? "apiId")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "userPoolConfig")
            Core.<*> (x Core..:? "xrayEnabled")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "logConfig")
            Core.<*> ( x Core..:? "additionalAuthenticationProviders"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "authenticationType")
            Core.<*> (x Core..:? "uris" Core..!= Core.mempty)
      )

instance Core.Hashable GraphqlApi

instance Core.NFData GraphqlApi
