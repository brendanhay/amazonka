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
-- Maintainer  : Brendan Hay
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
    createGraphqlApi_lambdaAuthorizerConfig,
    createGraphqlApi_logConfig,
    createGraphqlApi_openIDConnectConfig,
    createGraphqlApi_tags,
    createGraphqlApi_userPoolConfig,
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
    -- | Configuration for Lambda function authorization.
    lambdaAuthorizerConfig :: Prelude.Maybe LambdaAuthorizerConfig,
    -- | The Amazon CloudWatch Logs configuration.
    logConfig :: Prelude.Maybe LogConfig,
    -- | The OIDC configuration.
    openIDConnectConfig :: Prelude.Maybe OpenIDConnectConfig,
    -- | A @TagMap@ object.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Cognito user pool configuration.
    userPoolConfig :: Prelude.Maybe UserPoolConfig,
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
-- 'lambdaAuthorizerConfig', 'createGraphqlApi_lambdaAuthorizerConfig' - Configuration for Lambda function authorization.
--
-- 'logConfig', 'createGraphqlApi_logConfig' - The Amazon CloudWatch Logs configuration.
--
-- 'openIDConnectConfig', 'createGraphqlApi_openIDConnectConfig' - The OIDC configuration.
--
-- 'tags', 'createGraphqlApi_tags' - A @TagMap@ object.
--
-- 'userPoolConfig', 'createGraphqlApi_userPoolConfig' - The Amazon Cognito user pool configuration.
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
      lambdaAuthorizerConfig = Prelude.Nothing,
      logConfig = Prelude.Nothing,
      openIDConnectConfig = Prelude.Nothing,
      tags = Prelude.Nothing,
      userPoolConfig = Prelude.Nothing,
      xrayEnabled = Prelude.Nothing,
      name = pName_,
      authenticationType = pAuthenticationType_
    }

-- | A list of additional authentication providers for the @GraphqlApi@ API.
createGraphqlApi_additionalAuthenticationProviders :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe [AdditionalAuthenticationProvider])
createGraphqlApi_additionalAuthenticationProviders = Lens.lens (\CreateGraphqlApi' {additionalAuthenticationProviders} -> additionalAuthenticationProviders) (\s@CreateGraphqlApi' {} a -> s {additionalAuthenticationProviders = a} :: CreateGraphqlApi) Prelude.. Lens.mapping Lens.coerced

-- | Configuration for Lambda function authorization.
createGraphqlApi_lambdaAuthorizerConfig :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe LambdaAuthorizerConfig)
createGraphqlApi_lambdaAuthorizerConfig = Lens.lens (\CreateGraphqlApi' {lambdaAuthorizerConfig} -> lambdaAuthorizerConfig) (\s@CreateGraphqlApi' {} a -> s {lambdaAuthorizerConfig = a} :: CreateGraphqlApi)

-- | The Amazon CloudWatch Logs configuration.
createGraphqlApi_logConfig :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe LogConfig)
createGraphqlApi_logConfig = Lens.lens (\CreateGraphqlApi' {logConfig} -> logConfig) (\s@CreateGraphqlApi' {} a -> s {logConfig = a} :: CreateGraphqlApi)

-- | The OIDC configuration.
createGraphqlApi_openIDConnectConfig :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe OpenIDConnectConfig)
createGraphqlApi_openIDConnectConfig = Lens.lens (\CreateGraphqlApi' {openIDConnectConfig} -> openIDConnectConfig) (\s@CreateGraphqlApi' {} a -> s {openIDConnectConfig = a} :: CreateGraphqlApi)

-- | A @TagMap@ object.
createGraphqlApi_tags :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createGraphqlApi_tags = Lens.lens (\CreateGraphqlApi' {tags} -> tags) (\s@CreateGraphqlApi' {} a -> s {tags = a} :: CreateGraphqlApi) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Cognito user pool configuration.
createGraphqlApi_userPoolConfig :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe UserPoolConfig)
createGraphqlApi_userPoolConfig = Lens.lens (\CreateGraphqlApi' {userPoolConfig} -> userPoolConfig) (\s@CreateGraphqlApi' {} a -> s {userPoolConfig = a} :: CreateGraphqlApi)

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
      `Prelude.hashWithSalt` lambdaAuthorizerConfig
      `Prelude.hashWithSalt` logConfig
      `Prelude.hashWithSalt` openIDConnectConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` userPoolConfig
      `Prelude.hashWithSalt` xrayEnabled
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` authenticationType

instance Prelude.NFData CreateGraphqlApi where
  rnf CreateGraphqlApi' {..} =
    Prelude.rnf additionalAuthenticationProviders
      `Prelude.seq` Prelude.rnf lambdaAuthorizerConfig
      `Prelude.seq` Prelude.rnf logConfig
      `Prelude.seq` Prelude.rnf openIDConnectConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf userPoolConfig
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
            ("lambdaAuthorizerConfig" Data..=)
              Prelude.<$> lambdaAuthorizerConfig,
            ("logConfig" Data..=) Prelude.<$> logConfig,
            ("openIDConnectConfig" Data..=)
              Prelude.<$> openIDConnectConfig,
            ("tags" Data..=) Prelude.<$> tags,
            ("userPoolConfig" Data..=)
              Prelude.<$> userPoolConfig,
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
