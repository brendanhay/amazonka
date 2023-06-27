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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.GraphqlApi where

import Amazonka.AppSync.Types.AdditionalAuthenticationProvider
import Amazonka.AppSync.Types.AuthenticationType
import Amazonka.AppSync.Types.GraphQLApiType
import Amazonka.AppSync.Types.GraphQLApiVisibility
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
    -- | The value that indicates whether the GraphQL API is a standard API
    -- (@GRAPHQL@) or merged API (@MERGED@).
    apiType :: Prelude.Maybe GraphQLApiType,
    -- | The Amazon Resource Name (ARN).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The authentication type.
    authenticationType :: Prelude.Maybe AuthenticationType,
    -- | The DNS records for the API.
    dns :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Configuration for Lambda function authorization.
    lambdaAuthorizerConfig :: Prelude.Maybe LambdaAuthorizerConfig,
    -- | The Amazon CloudWatch Logs configuration.
    logConfig :: Prelude.Maybe LogConfig,
    -- | The Identity and Access Management service role ARN for a merged API.
    -- The AppSync service assumes this role on behalf of the Merged API to
    -- validate access to source APIs at runtime and to prompt the @AUTO_MERGE@
    -- to update the merged API endpoint with the source API changes
    -- automatically.
    mergedApiExecutionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The API name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The OpenID Connect configuration.
    openIDConnectConfig :: Prelude.Maybe OpenIDConnectConfig,
    -- | The account owner of the GraphQL API.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The owner contact information for an API resource.
    --
    -- This field accepts any string input with a length of 0 - 256 characters.
    ownerContact :: Prelude.Maybe Prelude.Text,
    -- | The tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The URIs.
    uris :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Cognito user pool configuration.
    userPoolConfig :: Prelude.Maybe UserPoolConfig,
    -- | Sets the value of the GraphQL API to public (@GLOBAL@) or private
    -- (@PRIVATE@). If no value is provided, the visibility will be set to
    -- @GLOBAL@ by default. This value cannot be changed once the API has been
    -- created.
    visibility :: Prelude.Maybe GraphQLApiVisibility,
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
-- 'apiType', 'graphqlApi_apiType' - The value that indicates whether the GraphQL API is a standard API
-- (@GRAPHQL@) or merged API (@MERGED@).
--
-- 'arn', 'graphqlApi_arn' - The Amazon Resource Name (ARN).
--
-- 'authenticationType', 'graphqlApi_authenticationType' - The authentication type.
--
-- 'dns', 'graphqlApi_dns' - The DNS records for the API.
--
-- 'lambdaAuthorizerConfig', 'graphqlApi_lambdaAuthorizerConfig' - Configuration for Lambda function authorization.
--
-- 'logConfig', 'graphqlApi_logConfig' - The Amazon CloudWatch Logs configuration.
--
-- 'mergedApiExecutionRoleArn', 'graphqlApi_mergedApiExecutionRoleArn' - The Identity and Access Management service role ARN for a merged API.
-- The AppSync service assumes this role on behalf of the Merged API to
-- validate access to source APIs at runtime and to prompt the @AUTO_MERGE@
-- to update the merged API endpoint with the source API changes
-- automatically.
--
-- 'name', 'graphqlApi_name' - The API name.
--
-- 'openIDConnectConfig', 'graphqlApi_openIDConnectConfig' - The OpenID Connect configuration.
--
-- 'owner', 'graphqlApi_owner' - The account owner of the GraphQL API.
--
-- 'ownerContact', 'graphqlApi_ownerContact' - The owner contact information for an API resource.
--
-- This field accepts any string input with a length of 0 - 256 characters.
--
-- 'tags', 'graphqlApi_tags' - The tags.
--
-- 'uris', 'graphqlApi_uris' - The URIs.
--
-- 'userPoolConfig', 'graphqlApi_userPoolConfig' - The Amazon Cognito user pool configuration.
--
-- 'visibility', 'graphqlApi_visibility' - Sets the value of the GraphQL API to public (@GLOBAL@) or private
-- (@PRIVATE@). If no value is provided, the visibility will be set to
-- @GLOBAL@ by default. This value cannot be changed once the API has been
-- created.
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
      apiType = Prelude.Nothing,
      arn = Prelude.Nothing,
      authenticationType = Prelude.Nothing,
      dns = Prelude.Nothing,
      lambdaAuthorizerConfig = Prelude.Nothing,
      logConfig = Prelude.Nothing,
      mergedApiExecutionRoleArn = Prelude.Nothing,
      name = Prelude.Nothing,
      openIDConnectConfig = Prelude.Nothing,
      owner = Prelude.Nothing,
      ownerContact = Prelude.Nothing,
      tags = Prelude.Nothing,
      uris = Prelude.Nothing,
      userPoolConfig = Prelude.Nothing,
      visibility = Prelude.Nothing,
      wafWebAclArn = Prelude.Nothing,
      xrayEnabled = Prelude.Nothing
    }

-- | A list of additional authentication providers for the @GraphqlApi@ API.
graphqlApi_additionalAuthenticationProviders :: Lens.Lens' GraphqlApi (Prelude.Maybe [AdditionalAuthenticationProvider])
graphqlApi_additionalAuthenticationProviders = Lens.lens (\GraphqlApi' {additionalAuthenticationProviders} -> additionalAuthenticationProviders) (\s@GraphqlApi' {} a -> s {additionalAuthenticationProviders = a} :: GraphqlApi) Prelude.. Lens.mapping Lens.coerced

-- | The API ID.
graphqlApi_apiId :: Lens.Lens' GraphqlApi (Prelude.Maybe Prelude.Text)
graphqlApi_apiId = Lens.lens (\GraphqlApi' {apiId} -> apiId) (\s@GraphqlApi' {} a -> s {apiId = a} :: GraphqlApi)

-- | The value that indicates whether the GraphQL API is a standard API
-- (@GRAPHQL@) or merged API (@MERGED@).
graphqlApi_apiType :: Lens.Lens' GraphqlApi (Prelude.Maybe GraphQLApiType)
graphqlApi_apiType = Lens.lens (\GraphqlApi' {apiType} -> apiType) (\s@GraphqlApi' {} a -> s {apiType = a} :: GraphqlApi)

-- | The Amazon Resource Name (ARN).
graphqlApi_arn :: Lens.Lens' GraphqlApi (Prelude.Maybe Prelude.Text)
graphqlApi_arn = Lens.lens (\GraphqlApi' {arn} -> arn) (\s@GraphqlApi' {} a -> s {arn = a} :: GraphqlApi)

-- | The authentication type.
graphqlApi_authenticationType :: Lens.Lens' GraphqlApi (Prelude.Maybe AuthenticationType)
graphqlApi_authenticationType = Lens.lens (\GraphqlApi' {authenticationType} -> authenticationType) (\s@GraphqlApi' {} a -> s {authenticationType = a} :: GraphqlApi)

-- | The DNS records for the API.
graphqlApi_dns :: Lens.Lens' GraphqlApi (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
graphqlApi_dns = Lens.lens (\GraphqlApi' {dns} -> dns) (\s@GraphqlApi' {} a -> s {dns = a} :: GraphqlApi) Prelude.. Lens.mapping Lens.coerced

-- | Configuration for Lambda function authorization.
graphqlApi_lambdaAuthorizerConfig :: Lens.Lens' GraphqlApi (Prelude.Maybe LambdaAuthorizerConfig)
graphqlApi_lambdaAuthorizerConfig = Lens.lens (\GraphqlApi' {lambdaAuthorizerConfig} -> lambdaAuthorizerConfig) (\s@GraphqlApi' {} a -> s {lambdaAuthorizerConfig = a} :: GraphqlApi)

-- | The Amazon CloudWatch Logs configuration.
graphqlApi_logConfig :: Lens.Lens' GraphqlApi (Prelude.Maybe LogConfig)
graphqlApi_logConfig = Lens.lens (\GraphqlApi' {logConfig} -> logConfig) (\s@GraphqlApi' {} a -> s {logConfig = a} :: GraphqlApi)

-- | The Identity and Access Management service role ARN for a merged API.
-- The AppSync service assumes this role on behalf of the Merged API to
-- validate access to source APIs at runtime and to prompt the @AUTO_MERGE@
-- to update the merged API endpoint with the source API changes
-- automatically.
graphqlApi_mergedApiExecutionRoleArn :: Lens.Lens' GraphqlApi (Prelude.Maybe Prelude.Text)
graphqlApi_mergedApiExecutionRoleArn = Lens.lens (\GraphqlApi' {mergedApiExecutionRoleArn} -> mergedApiExecutionRoleArn) (\s@GraphqlApi' {} a -> s {mergedApiExecutionRoleArn = a} :: GraphqlApi)

-- | The API name.
graphqlApi_name :: Lens.Lens' GraphqlApi (Prelude.Maybe Prelude.Text)
graphqlApi_name = Lens.lens (\GraphqlApi' {name} -> name) (\s@GraphqlApi' {} a -> s {name = a} :: GraphqlApi)

-- | The OpenID Connect configuration.
graphqlApi_openIDConnectConfig :: Lens.Lens' GraphqlApi (Prelude.Maybe OpenIDConnectConfig)
graphqlApi_openIDConnectConfig = Lens.lens (\GraphqlApi' {openIDConnectConfig} -> openIDConnectConfig) (\s@GraphqlApi' {} a -> s {openIDConnectConfig = a} :: GraphqlApi)

-- | The account owner of the GraphQL API.
graphqlApi_owner :: Lens.Lens' GraphqlApi (Prelude.Maybe Prelude.Text)
graphqlApi_owner = Lens.lens (\GraphqlApi' {owner} -> owner) (\s@GraphqlApi' {} a -> s {owner = a} :: GraphqlApi)

-- | The owner contact information for an API resource.
--
-- This field accepts any string input with a length of 0 - 256 characters.
graphqlApi_ownerContact :: Lens.Lens' GraphqlApi (Prelude.Maybe Prelude.Text)
graphqlApi_ownerContact = Lens.lens (\GraphqlApi' {ownerContact} -> ownerContact) (\s@GraphqlApi' {} a -> s {ownerContact = a} :: GraphqlApi)

-- | The tags.
graphqlApi_tags :: Lens.Lens' GraphqlApi (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
graphqlApi_tags = Lens.lens (\GraphqlApi' {tags} -> tags) (\s@GraphqlApi' {} a -> s {tags = a} :: GraphqlApi) Prelude.. Lens.mapping Lens.coerced

-- | The URIs.
graphqlApi_uris :: Lens.Lens' GraphqlApi (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
graphqlApi_uris = Lens.lens (\GraphqlApi' {uris} -> uris) (\s@GraphqlApi' {} a -> s {uris = a} :: GraphqlApi) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Cognito user pool configuration.
graphqlApi_userPoolConfig :: Lens.Lens' GraphqlApi (Prelude.Maybe UserPoolConfig)
graphqlApi_userPoolConfig = Lens.lens (\GraphqlApi' {userPoolConfig} -> userPoolConfig) (\s@GraphqlApi' {} a -> s {userPoolConfig = a} :: GraphqlApi)

-- | Sets the value of the GraphQL API to public (@GLOBAL@) or private
-- (@PRIVATE@). If no value is provided, the visibility will be set to
-- @GLOBAL@ by default. This value cannot be changed once the API has been
-- created.
graphqlApi_visibility :: Lens.Lens' GraphqlApi (Prelude.Maybe GraphQLApiVisibility)
graphqlApi_visibility = Lens.lens (\GraphqlApi' {visibility} -> visibility) (\s@GraphqlApi' {} a -> s {visibility = a} :: GraphqlApi)

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
            Prelude.<$> ( x
                            Data..:? "additionalAuthenticationProviders"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "apiId")
            Prelude.<*> (x Data..:? "apiType")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "authenticationType")
            Prelude.<*> (x Data..:? "dns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "lambdaAuthorizerConfig")
            Prelude.<*> (x Data..:? "logConfig")
            Prelude.<*> (x Data..:? "mergedApiExecutionRoleArn")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "openIDConnectConfig")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "ownerContact")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "uris" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "userPoolConfig")
            Prelude.<*> (x Data..:? "visibility")
            Prelude.<*> (x Data..:? "wafWebAclArn")
            Prelude.<*> (x Data..:? "xrayEnabled")
      )

instance Prelude.Hashable GraphqlApi where
  hashWithSalt _salt GraphqlApi' {..} =
    _salt
      `Prelude.hashWithSalt` additionalAuthenticationProviders
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` apiType
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` authenticationType
      `Prelude.hashWithSalt` dns
      `Prelude.hashWithSalt` lambdaAuthorizerConfig
      `Prelude.hashWithSalt` logConfig
      `Prelude.hashWithSalt` mergedApiExecutionRoleArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` openIDConnectConfig
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` ownerContact
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` uris
      `Prelude.hashWithSalt` userPoolConfig
      `Prelude.hashWithSalt` visibility
      `Prelude.hashWithSalt` wafWebAclArn
      `Prelude.hashWithSalt` xrayEnabled

instance Prelude.NFData GraphqlApi where
  rnf GraphqlApi' {..} =
    Prelude.rnf additionalAuthenticationProviders
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf apiType
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authenticationType
      `Prelude.seq` Prelude.rnf dns
      `Prelude.seq` Prelude.rnf lambdaAuthorizerConfig
      `Prelude.seq` Prelude.rnf logConfig
      `Prelude.seq` Prelude.rnf mergedApiExecutionRoleArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf openIDConnectConfig
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf ownerContact
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf uris
      `Prelude.seq` Prelude.rnf userPoolConfig
      `Prelude.seq` Prelude.rnf visibility
      `Prelude.seq` Prelude.rnf wafWebAclArn
      `Prelude.seq` Prelude.rnf xrayEnabled
