{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppSync.CreateGraphqlApi
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @GraphqlApi@ object.
module Amazonka.AppSync.CreateGraphqlApi
  ( -- * Creating a Request
    CreateGraphqlApi (..),
    newCreateGraphqlApi,

    -- * Request Lenses
    createGraphqlApi_additionalAuthenticationProviders,
    createGraphqlApi_apiType,
    createGraphqlApi_lambdaAuthorizerConfig,
    createGraphqlApi_logConfig,
    createGraphqlApi_mergedApiExecutionRoleArn,
    createGraphqlApi_openIDConnectConfig,
    createGraphqlApi_ownerContact,
    createGraphqlApi_tags,
    createGraphqlApi_userPoolConfig,
    createGraphqlApi_visibility,
    createGraphqlApi_xrayEnabled,
    createGraphqlApi_name,
    createGraphqlApi_authenticationType,

    -- * Destructuring the Response
    CreateGraphqlApiResponse (..),
    newCreateGraphqlApiResponse,

    -- * Response Lenses
    createGraphqlApiResponse_graphqlApi,
    createGraphqlApiResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGraphqlApi' smart constructor.
data CreateGraphqlApi = CreateGraphqlApi'
  { -- | A list of additional authentication providers for the @GraphqlApi@ API.
    additionalAuthenticationProviders :: Prelude.Maybe [AdditionalAuthenticationProvider],
    -- | The value that indicates whether the GraphQL API is a standard API
    -- (@GRAPHQL@) or merged API (@MERGED@).
    apiType :: Prelude.Maybe GraphQLApiType,
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
    -- | The OIDC configuration.
    openIDConnectConfig :: Prelude.Maybe OpenIDConnectConfig,
    -- | The owner contact information for an API resource.
    --
    -- This field accepts any string input with a length of 0 - 256 characters.
    ownerContact :: Prelude.Maybe Prelude.Text,
    -- | A @TagMap@ object.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Cognito user pool configuration.
    userPoolConfig :: Prelude.Maybe UserPoolConfig,
    -- | Sets the value of the GraphQL API to public (@GLOBAL@) or private
    -- (@PRIVATE@). If no value is provided, the visibility will be set to
    -- @GLOBAL@ by default. This value cannot be changed once the API has been
    -- created.
    visibility :: Prelude.Maybe GraphQLApiVisibility,
    -- | A flag indicating whether to use X-Ray tracing for the @GraphqlApi@.
    xrayEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A user-supplied name for the @GraphqlApi@.
    name :: Prelude.Text,
    -- | The authentication type: API key, Identity and Access Management (IAM),
    -- OpenID Connect (OIDC), Amazon Cognito user pools, or Lambda.
    authenticationType :: AuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGraphqlApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalAuthenticationProviders', 'createGraphqlApi_additionalAuthenticationProviders' - A list of additional authentication providers for the @GraphqlApi@ API.
--
-- 'apiType', 'createGraphqlApi_apiType' - The value that indicates whether the GraphQL API is a standard API
-- (@GRAPHQL@) or merged API (@MERGED@).
--
-- 'lambdaAuthorizerConfig', 'createGraphqlApi_lambdaAuthorizerConfig' - Configuration for Lambda function authorization.
--
-- 'logConfig', 'createGraphqlApi_logConfig' - The Amazon CloudWatch Logs configuration.
--
-- 'mergedApiExecutionRoleArn', 'createGraphqlApi_mergedApiExecutionRoleArn' - The Identity and Access Management service role ARN for a merged API.
-- The AppSync service assumes this role on behalf of the Merged API to
-- validate access to source APIs at runtime and to prompt the @AUTO_MERGE@
-- to update the merged API endpoint with the source API changes
-- automatically.
--
-- 'openIDConnectConfig', 'createGraphqlApi_openIDConnectConfig' - The OIDC configuration.
--
-- 'ownerContact', 'createGraphqlApi_ownerContact' - The owner contact information for an API resource.
--
-- This field accepts any string input with a length of 0 - 256 characters.
--
-- 'tags', 'createGraphqlApi_tags' - A @TagMap@ object.
--
-- 'userPoolConfig', 'createGraphqlApi_userPoolConfig' - The Amazon Cognito user pool configuration.
--
-- 'visibility', 'createGraphqlApi_visibility' - Sets the value of the GraphQL API to public (@GLOBAL@) or private
-- (@PRIVATE@). If no value is provided, the visibility will be set to
-- @GLOBAL@ by default. This value cannot be changed once the API has been
-- created.
--
-- 'xrayEnabled', 'createGraphqlApi_xrayEnabled' - A flag indicating whether to use X-Ray tracing for the @GraphqlApi@.
--
-- 'name', 'createGraphqlApi_name' - A user-supplied name for the @GraphqlApi@.
--
-- 'authenticationType', 'createGraphqlApi_authenticationType' - The authentication type: API key, Identity and Access Management (IAM),
-- OpenID Connect (OIDC), Amazon Cognito user pools, or Lambda.
newCreateGraphqlApi ::
  -- | 'name'
  Prelude.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  CreateGraphqlApi
newCreateGraphqlApi pName_ pAuthenticationType_ =
  CreateGraphqlApi'
    { additionalAuthenticationProviders =
        Prelude.Nothing,
      apiType = Prelude.Nothing,
      lambdaAuthorizerConfig = Prelude.Nothing,
      logConfig = Prelude.Nothing,
      mergedApiExecutionRoleArn = Prelude.Nothing,
      openIDConnectConfig = Prelude.Nothing,
      ownerContact = Prelude.Nothing,
      tags = Prelude.Nothing,
      userPoolConfig = Prelude.Nothing,
      visibility = Prelude.Nothing,
      xrayEnabled = Prelude.Nothing,
      name = pName_,
      authenticationType = pAuthenticationType_
    }

-- | A list of additional authentication providers for the @GraphqlApi@ API.
createGraphqlApi_additionalAuthenticationProviders :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe [AdditionalAuthenticationProvider])
createGraphqlApi_additionalAuthenticationProviders = Lens.lens (\CreateGraphqlApi' {additionalAuthenticationProviders} -> additionalAuthenticationProviders) (\s@CreateGraphqlApi' {} a -> s {additionalAuthenticationProviders = a} :: CreateGraphqlApi) Prelude.. Lens.mapping Lens.coerced

-- | The value that indicates whether the GraphQL API is a standard API
-- (@GRAPHQL@) or merged API (@MERGED@).
createGraphqlApi_apiType :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe GraphQLApiType)
createGraphqlApi_apiType = Lens.lens (\CreateGraphqlApi' {apiType} -> apiType) (\s@CreateGraphqlApi' {} a -> s {apiType = a} :: CreateGraphqlApi)

-- | Configuration for Lambda function authorization.
createGraphqlApi_lambdaAuthorizerConfig :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe LambdaAuthorizerConfig)
createGraphqlApi_lambdaAuthorizerConfig = Lens.lens (\CreateGraphqlApi' {lambdaAuthorizerConfig} -> lambdaAuthorizerConfig) (\s@CreateGraphqlApi' {} a -> s {lambdaAuthorizerConfig = a} :: CreateGraphqlApi)

-- | The Amazon CloudWatch Logs configuration.
createGraphqlApi_logConfig :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe LogConfig)
createGraphqlApi_logConfig = Lens.lens (\CreateGraphqlApi' {logConfig} -> logConfig) (\s@CreateGraphqlApi' {} a -> s {logConfig = a} :: CreateGraphqlApi)

-- | The Identity and Access Management service role ARN for a merged API.
-- The AppSync service assumes this role on behalf of the Merged API to
-- validate access to source APIs at runtime and to prompt the @AUTO_MERGE@
-- to update the merged API endpoint with the source API changes
-- automatically.
createGraphqlApi_mergedApiExecutionRoleArn :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe Prelude.Text)
createGraphqlApi_mergedApiExecutionRoleArn = Lens.lens (\CreateGraphqlApi' {mergedApiExecutionRoleArn} -> mergedApiExecutionRoleArn) (\s@CreateGraphqlApi' {} a -> s {mergedApiExecutionRoleArn = a} :: CreateGraphqlApi)

-- | The OIDC configuration.
createGraphqlApi_openIDConnectConfig :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe OpenIDConnectConfig)
createGraphqlApi_openIDConnectConfig = Lens.lens (\CreateGraphqlApi' {openIDConnectConfig} -> openIDConnectConfig) (\s@CreateGraphqlApi' {} a -> s {openIDConnectConfig = a} :: CreateGraphqlApi)

-- | The owner contact information for an API resource.
--
-- This field accepts any string input with a length of 0 - 256 characters.
createGraphqlApi_ownerContact :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe Prelude.Text)
createGraphqlApi_ownerContact = Lens.lens (\CreateGraphqlApi' {ownerContact} -> ownerContact) (\s@CreateGraphqlApi' {} a -> s {ownerContact = a} :: CreateGraphqlApi)

-- | A @TagMap@ object.
createGraphqlApi_tags :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createGraphqlApi_tags = Lens.lens (\CreateGraphqlApi' {tags} -> tags) (\s@CreateGraphqlApi' {} a -> s {tags = a} :: CreateGraphqlApi) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Cognito user pool configuration.
createGraphqlApi_userPoolConfig :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe UserPoolConfig)
createGraphqlApi_userPoolConfig = Lens.lens (\CreateGraphqlApi' {userPoolConfig} -> userPoolConfig) (\s@CreateGraphqlApi' {} a -> s {userPoolConfig = a} :: CreateGraphqlApi)

-- | Sets the value of the GraphQL API to public (@GLOBAL@) or private
-- (@PRIVATE@). If no value is provided, the visibility will be set to
-- @GLOBAL@ by default. This value cannot be changed once the API has been
-- created.
createGraphqlApi_visibility :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe GraphQLApiVisibility)
createGraphqlApi_visibility = Lens.lens (\CreateGraphqlApi' {visibility} -> visibility) (\s@CreateGraphqlApi' {} a -> s {visibility = a} :: CreateGraphqlApi)

-- | A flag indicating whether to use X-Ray tracing for the @GraphqlApi@.
createGraphqlApi_xrayEnabled :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe Prelude.Bool)
createGraphqlApi_xrayEnabled = Lens.lens (\CreateGraphqlApi' {xrayEnabled} -> xrayEnabled) (\s@CreateGraphqlApi' {} a -> s {xrayEnabled = a} :: CreateGraphqlApi)

-- | A user-supplied name for the @GraphqlApi@.
createGraphqlApi_name :: Lens.Lens' CreateGraphqlApi Prelude.Text
createGraphqlApi_name = Lens.lens (\CreateGraphqlApi' {name} -> name) (\s@CreateGraphqlApi' {} a -> s {name = a} :: CreateGraphqlApi)

-- | The authentication type: API key, Identity and Access Management (IAM),
-- OpenID Connect (OIDC), Amazon Cognito user pools, or Lambda.
createGraphqlApi_authenticationType :: Lens.Lens' CreateGraphqlApi AuthenticationType
createGraphqlApi_authenticationType = Lens.lens (\CreateGraphqlApi' {authenticationType} -> authenticationType) (\s@CreateGraphqlApi' {} a -> s {authenticationType = a} :: CreateGraphqlApi)

instance Core.AWSRequest CreateGraphqlApi where
  type
    AWSResponse CreateGraphqlApi =
      CreateGraphqlApiResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGraphqlApiResponse'
            Prelude.<$> (x Data..?> "graphqlApi")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGraphqlApi where
  hashWithSalt _salt CreateGraphqlApi' {..} =
    _salt
      `Prelude.hashWithSalt` additionalAuthenticationProviders
      `Prelude.hashWithSalt` apiType
      `Prelude.hashWithSalt` lambdaAuthorizerConfig
      `Prelude.hashWithSalt` logConfig
      `Prelude.hashWithSalt` mergedApiExecutionRoleArn
      `Prelude.hashWithSalt` openIDConnectConfig
      `Prelude.hashWithSalt` ownerContact
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` userPoolConfig
      `Prelude.hashWithSalt` visibility
      `Prelude.hashWithSalt` xrayEnabled
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` authenticationType

instance Prelude.NFData CreateGraphqlApi where
  rnf CreateGraphqlApi' {..} =
    Prelude.rnf additionalAuthenticationProviders
      `Prelude.seq` Prelude.rnf apiType
      `Prelude.seq` Prelude.rnf lambdaAuthorizerConfig
      `Prelude.seq` Prelude.rnf logConfig
      `Prelude.seq` Prelude.rnf mergedApiExecutionRoleArn
      `Prelude.seq` Prelude.rnf openIDConnectConfig
      `Prelude.seq` Prelude.rnf ownerContact
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf userPoolConfig
      `Prelude.seq` Prelude.rnf visibility
      `Prelude.seq` Prelude.rnf xrayEnabled
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf authenticationType

instance Data.ToHeaders CreateGraphqlApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGraphqlApi where
  toJSON CreateGraphqlApi' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalAuthenticationProviders" Data..=)
              Prelude.<$> additionalAuthenticationProviders,
            ("apiType" Data..=) Prelude.<$> apiType,
            ("lambdaAuthorizerConfig" Data..=)
              Prelude.<$> lambdaAuthorizerConfig,
            ("logConfig" Data..=) Prelude.<$> logConfig,
            ("mergedApiExecutionRoleArn" Data..=)
              Prelude.<$> mergedApiExecutionRoleArn,
            ("openIDConnectConfig" Data..=)
              Prelude.<$> openIDConnectConfig,
            ("ownerContact" Data..=) Prelude.<$> ownerContact,
            ("tags" Data..=) Prelude.<$> tags,
            ("userPoolConfig" Data..=)
              Prelude.<$> userPoolConfig,
            ("visibility" Data..=) Prelude.<$> visibility,
            ("xrayEnabled" Data..=) Prelude.<$> xrayEnabled,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("authenticationType" Data..= authenticationType)
          ]
      )

instance Data.ToPath CreateGraphqlApi where
  toPath = Prelude.const "/v1/apis"

instance Data.ToQuery CreateGraphqlApi where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGraphqlApiResponse' smart constructor.
data CreateGraphqlApiResponse = CreateGraphqlApiResponse'
  { -- | The @GraphqlApi@.
    graphqlApi :: Prelude.Maybe GraphqlApi,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGraphqlApiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'graphqlApi', 'createGraphqlApiResponse_graphqlApi' - The @GraphqlApi@.
--
-- 'httpStatus', 'createGraphqlApiResponse_httpStatus' - The response's http status code.
newCreateGraphqlApiResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGraphqlApiResponse
newCreateGraphqlApiResponse pHttpStatus_ =
  CreateGraphqlApiResponse'
    { graphqlApi =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @GraphqlApi@.
createGraphqlApiResponse_graphqlApi :: Lens.Lens' CreateGraphqlApiResponse (Prelude.Maybe GraphqlApi)
createGraphqlApiResponse_graphqlApi = Lens.lens (\CreateGraphqlApiResponse' {graphqlApi} -> graphqlApi) (\s@CreateGraphqlApiResponse' {} a -> s {graphqlApi = a} :: CreateGraphqlApiResponse)

-- | The response's http status code.
createGraphqlApiResponse_httpStatus :: Lens.Lens' CreateGraphqlApiResponse Prelude.Int
createGraphqlApiResponse_httpStatus = Lens.lens (\CreateGraphqlApiResponse' {httpStatus} -> httpStatus) (\s@CreateGraphqlApiResponse' {} a -> s {httpStatus = a} :: CreateGraphqlApiResponse)

instance Prelude.NFData CreateGraphqlApiResponse where
  rnf CreateGraphqlApiResponse' {..} =
    Prelude.rnf graphqlApi
      `Prelude.seq` Prelude.rnf httpStatus
