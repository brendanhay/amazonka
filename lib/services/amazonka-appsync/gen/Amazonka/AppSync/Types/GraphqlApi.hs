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
-- Module      : Amazonka.AppSync.Types.GraphqlApi
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.GraphqlApi where

import Amazonka.AppSync.Types.AdditionalAuthenticationProvider
import Amazonka.AppSync.Types.AuthenticationType
import Amazonka.AppSync.Types.LambdaAuthorizerConfig
import Amazonka.AppSync.Types.LogConfig
import Amazonka.AppSync.Types.OpenIDConnectConfig
import Amazonka.AppSync.Types.UserPoolConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a GraphQL API.
--
-- /See:/ 'newGraphqlApi' smart constructor.
data GraphqlApi = GraphqlApi'
  { -- | A list of additional authentication providers for the @GraphqlApi@ API.
    additionalAuthenticationProviders :: Prelude.Maybe [AdditionalAuthenticationProvider],
    -- | The API ID.
    apiId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The authentication type.
    authenticationType :: Prelude.Maybe AuthenticationType,
    -- | Configuration for Lambda function authorization.
    lambdaAuthorizerConfig :: Prelude.Maybe LambdaAuthorizerConfig,
    -- | The Amazon CloudWatch Logs configuration.
    logConfig :: Prelude.Maybe LogConfig,
    -- | The API name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The OpenID Connect configuration.
    openIDConnectConfig :: Prelude.Maybe OpenIDConnectConfig,
    -- | The tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The URIs.
    uris :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Cognito user pool configuration.
    userPoolConfig :: Prelude.Maybe UserPoolConfig,
    -- | The ARN of the WAF access control list (ACL) associated with this
    -- @GraphqlApi@, if one exists.
    wafWebAclArn :: Prelude.Maybe Prelude.Text,
    -- | A flag indicating whether to use X-Ray tracing for this @GraphqlApi@.
    xrayEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GraphqlApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalAuthenticationProviders', 'graphqlApi_additionalAuthenticationProviders' - A list of additional authentication providers for the @GraphqlApi@ API.
--
-- 'apiId', 'graphqlApi_apiId' - The API ID.
--
-- 'arn', 'graphqlApi_arn' - The Amazon Resource Name (ARN).
--
-- 'authenticationType', 'graphqlApi_authenticationType' - The authentication type.
--
-- 'lambdaAuthorizerConfig', 'graphqlApi_lambdaAuthorizerConfig' - Configuration for Lambda function authorization.
--
-- 'logConfig', 'graphqlApi_logConfig' - The Amazon CloudWatch Logs configuration.
--
-- 'name', 'graphqlApi_name' - The API name.
--
-- 'openIDConnectConfig', 'graphqlApi_openIDConnectConfig' - The OpenID Connect configuration.
--
-- 'tags', 'graphqlApi_tags' - The tags.
--
-- 'uris', 'graphqlApi_uris' - The URIs.
--
-- 'userPoolConfig', 'graphqlApi_userPoolConfig' - The Amazon Cognito user pool configuration.
--
-- 'wafWebAclArn', 'graphqlApi_wafWebAclArn' - The ARN of the WAF access control list (ACL) associated with this
-- @GraphqlApi@, if one exists.
--
-- 'xrayEnabled', 'graphqlApi_xrayEnabled' - A flag indicating whether to use X-Ray tracing for this @GraphqlApi@.
newGraphqlApi ::
  GraphqlApi
newGraphqlApi =
  GraphqlApi'
    { additionalAuthenticationProviders =
        Prelude.Nothing,
      apiId = Prelude.Nothing,
      arn = Prelude.Nothing,
      authenticationType = Prelude.Nothing,
      lambdaAuthorizerConfig = Prelude.Nothing,
      logConfig = Prelude.Nothing,
      name = Prelude.Nothing,
      openIDConnectConfig = Prelude.Nothing,
      tags = Prelude.Nothing,
      uris = Prelude.Nothing,
      userPoolConfig = Prelude.Nothing,
      wafWebAclArn = Prelude.Nothing,
      xrayEnabled = Prelude.Nothing
    }

-- | A list of additional authentication providers for the @GraphqlApi@ API.
graphqlApi_additionalAuthenticationProviders :: Lens.Lens' GraphqlApi (Prelude.Maybe [AdditionalAuthenticationProvider])
graphqlApi_additionalAuthenticationProviders = Lens.lens (\GraphqlApi' {additionalAuthenticationProviders} -> additionalAuthenticationProviders) (\s@GraphqlApi' {} a -> s {additionalAuthenticationProviders = a} :: GraphqlApi) Prelude.. Lens.mapping Lens.coerced

-- | The API ID.
graphqlApi_apiId :: Lens.Lens' GraphqlApi (Prelude.Maybe Prelude.Text)
graphqlApi_apiId = Lens.lens (\GraphqlApi' {apiId} -> apiId) (\s@GraphqlApi' {} a -> s {apiId = a} :: GraphqlApi)

-- | The Amazon Resource Name (ARN).
graphqlApi_arn :: Lens.Lens' GraphqlApi (Prelude.Maybe Prelude.Text)
graphqlApi_arn = Lens.lens (\GraphqlApi' {arn} -> arn) (\s@GraphqlApi' {} a -> s {arn = a} :: GraphqlApi)

-- | The authentication type.
graphqlApi_authenticationType :: Lens.Lens' GraphqlApi (Prelude.Maybe AuthenticationType)
graphqlApi_authenticationType = Lens.lens (\GraphqlApi' {authenticationType} -> authenticationType) (\s@GraphqlApi' {} a -> s {authenticationType = a} :: GraphqlApi)

-- | Configuration for Lambda function authorization.
graphqlApi_lambdaAuthorizerConfig :: Lens.Lens' GraphqlApi (Prelude.Maybe LambdaAuthorizerConfig)
graphqlApi_lambdaAuthorizerConfig = Lens.lens (\GraphqlApi' {lambdaAuthorizerConfig} -> lambdaAuthorizerConfig) (\s@GraphqlApi' {} a -> s {lambdaAuthorizerConfig = a} :: GraphqlApi)

-- | The Amazon CloudWatch Logs configuration.
graphqlApi_logConfig :: Lens.Lens' GraphqlApi (Prelude.Maybe LogConfig)
graphqlApi_logConfig = Lens.lens (\GraphqlApi' {logConfig} -> logConfig) (\s@GraphqlApi' {} a -> s {logConfig = a} :: GraphqlApi)

-- | The API name.
graphqlApi_name :: Lens.Lens' GraphqlApi (Prelude.Maybe Prelude.Text)
graphqlApi_name = Lens.lens (\GraphqlApi' {name} -> name) (\s@GraphqlApi' {} a -> s {name = a} :: GraphqlApi)

-- | The OpenID Connect configuration.
graphqlApi_openIDConnectConfig :: Lens.Lens' GraphqlApi (Prelude.Maybe OpenIDConnectConfig)
graphqlApi_openIDConnectConfig = Lens.lens (\GraphqlApi' {openIDConnectConfig} -> openIDConnectConfig) (\s@GraphqlApi' {} a -> s {openIDConnectConfig = a} :: GraphqlApi)

-- | The tags.
graphqlApi_tags :: Lens.Lens' GraphqlApi (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
graphqlApi_tags = Lens.lens (\GraphqlApi' {tags} -> tags) (\s@GraphqlApi' {} a -> s {tags = a} :: GraphqlApi) Prelude.. Lens.mapping Lens.coerced

-- | The URIs.
graphqlApi_uris :: Lens.Lens' GraphqlApi (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
graphqlApi_uris = Lens.lens (\GraphqlApi' {uris} -> uris) (\s@GraphqlApi' {} a -> s {uris = a} :: GraphqlApi) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Cognito user pool configuration.
graphqlApi_userPoolConfig :: Lens.Lens' GraphqlApi (Prelude.Maybe UserPoolConfig)
graphqlApi_userPoolConfig = Lens.lens (\GraphqlApi' {userPoolConfig} -> userPoolConfig) (\s@GraphqlApi' {} a -> s {userPoolConfig = a} :: GraphqlApi)

-- | The ARN of the WAF access control list (ACL) associated with this
-- @GraphqlApi@, if one exists.
graphqlApi_wafWebAclArn :: Lens.Lens' GraphqlApi (Prelude.Maybe Prelude.Text)
graphqlApi_wafWebAclArn = Lens.lens (\GraphqlApi' {wafWebAclArn} -> wafWebAclArn) (\s@GraphqlApi' {} a -> s {wafWebAclArn = a} :: GraphqlApi)

-- | A flag indicating whether to use X-Ray tracing for this @GraphqlApi@.
graphqlApi_xrayEnabled :: Lens.Lens' GraphqlApi (Prelude.Maybe Prelude.Bool)
graphqlApi_xrayEnabled = Lens.lens (\GraphqlApi' {xrayEnabled} -> xrayEnabled) (\s@GraphqlApi' {} a -> s {xrayEnabled = a} :: GraphqlApi)

instance Data.FromJSON GraphqlApi where
  parseJSON =
    Data.withObject
      "GraphqlApi"
      ( \x ->
          GraphqlApi'
            Prelude.<$> ( x Data..:? "additionalAuthenticationProviders"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "apiId")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "authenticationType")
            Prelude.<*> (x Data..:? "lambdaAuthorizerConfig")
            Prelude.<*> (x Data..:? "logConfig")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "openIDConnectConfig")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "uris" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "userPoolConfig")
            Prelude.<*> (x Data..:? "wafWebAclArn")
            Prelude.<*> (x Data..:? "xrayEnabled")
      )

instance Prelude.Hashable GraphqlApi where
  hashWithSalt _salt GraphqlApi' {..} =
    _salt
      `Prelude.hashWithSalt` additionalAuthenticationProviders
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` authenticationType
      `Prelude.hashWithSalt` lambdaAuthorizerConfig
      `Prelude.hashWithSalt` logConfig
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` openIDConnectConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` uris
      `Prelude.hashWithSalt` userPoolConfig
      `Prelude.hashWithSalt` wafWebAclArn
      `Prelude.hashWithSalt` xrayEnabled

instance Prelude.NFData GraphqlApi where
  rnf GraphqlApi' {..} =
    Prelude.rnf additionalAuthenticationProviders
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authenticationType
      `Prelude.seq` Prelude.rnf lambdaAuthorizerConfig
      `Prelude.seq` Prelude.rnf logConfig
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf openIDConnectConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf uris
      `Prelude.seq` Prelude.rnf userPoolConfig
      `Prelude.seq` Prelude.rnf wafWebAclArn
      `Prelude.seq` Prelude.rnf xrayEnabled
