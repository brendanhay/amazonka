{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AppSync.CreateGraphqlApi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @GraphqlApi@ object.
module Network.AWS.AppSync.CreateGraphqlApi
  ( -- * Creating a Request
    CreateGraphqlApi (..),
    newCreateGraphqlApi,

    -- * Request Lenses
    createGraphqlApi_openIDConnectConfig,
    createGraphqlApi_userPoolConfig,
    createGraphqlApi_xrayEnabled,
    createGraphqlApi_tags,
    createGraphqlApi_logConfig,
    createGraphqlApi_additionalAuthenticationProviders,
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

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateGraphqlApi' smart constructor.
data CreateGraphqlApi = CreateGraphqlApi'
  { -- | The OpenID Connect configuration.
    openIDConnectConfig :: Prelude.Maybe OpenIDConnectConfig,
    -- | The Amazon Cognito user pool configuration.
    userPoolConfig :: Prelude.Maybe UserPoolConfig,
    -- | A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@.
    xrayEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A @TagMap@ object.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon CloudWatch Logs configuration.
    logConfig :: Prelude.Maybe LogConfig,
    -- | A list of additional authentication providers for the @GraphqlApi@ API.
    additionalAuthenticationProviders :: Prelude.Maybe [AdditionalAuthenticationProvider],
    -- | A user-supplied name for the @GraphqlApi@.
    name :: Prelude.Text,
    -- | The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user
    -- pools.
    authenticationType :: AuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateGraphqlApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openIDConnectConfig', 'createGraphqlApi_openIDConnectConfig' - The OpenID Connect configuration.
--
-- 'userPoolConfig', 'createGraphqlApi_userPoolConfig' - The Amazon Cognito user pool configuration.
--
-- 'xrayEnabled', 'createGraphqlApi_xrayEnabled' - A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@.
--
-- 'tags', 'createGraphqlApi_tags' - A @TagMap@ object.
--
-- 'logConfig', 'createGraphqlApi_logConfig' - The Amazon CloudWatch Logs configuration.
--
-- 'additionalAuthenticationProviders', 'createGraphqlApi_additionalAuthenticationProviders' - A list of additional authentication providers for the @GraphqlApi@ API.
--
-- 'name', 'createGraphqlApi_name' - A user-supplied name for the @GraphqlApi@.
--
-- 'authenticationType', 'createGraphqlApi_authenticationType' - The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user
-- pools.
newCreateGraphqlApi ::
  -- | 'name'
  Prelude.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  CreateGraphqlApi
newCreateGraphqlApi pName_ pAuthenticationType_ =
  CreateGraphqlApi'
    { openIDConnectConfig =
        Prelude.Nothing,
      userPoolConfig = Prelude.Nothing,
      xrayEnabled = Prelude.Nothing,
      tags = Prelude.Nothing,
      logConfig = Prelude.Nothing,
      additionalAuthenticationProviders = Prelude.Nothing,
      name = pName_,
      authenticationType = pAuthenticationType_
    }

-- | The OpenID Connect configuration.
createGraphqlApi_openIDConnectConfig :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe OpenIDConnectConfig)
createGraphqlApi_openIDConnectConfig = Lens.lens (\CreateGraphqlApi' {openIDConnectConfig} -> openIDConnectConfig) (\s@CreateGraphqlApi' {} a -> s {openIDConnectConfig = a} :: CreateGraphqlApi)

-- | The Amazon Cognito user pool configuration.
createGraphqlApi_userPoolConfig :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe UserPoolConfig)
createGraphqlApi_userPoolConfig = Lens.lens (\CreateGraphqlApi' {userPoolConfig} -> userPoolConfig) (\s@CreateGraphqlApi' {} a -> s {userPoolConfig = a} :: CreateGraphqlApi)

-- | A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@.
createGraphqlApi_xrayEnabled :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe Prelude.Bool)
createGraphqlApi_xrayEnabled = Lens.lens (\CreateGraphqlApi' {xrayEnabled} -> xrayEnabled) (\s@CreateGraphqlApi' {} a -> s {xrayEnabled = a} :: CreateGraphqlApi)

-- | A @TagMap@ object.
createGraphqlApi_tags :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createGraphqlApi_tags = Lens.lens (\CreateGraphqlApi' {tags} -> tags) (\s@CreateGraphqlApi' {} a -> s {tags = a} :: CreateGraphqlApi) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon CloudWatch Logs configuration.
createGraphqlApi_logConfig :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe LogConfig)
createGraphqlApi_logConfig = Lens.lens (\CreateGraphqlApi' {logConfig} -> logConfig) (\s@CreateGraphqlApi' {} a -> s {logConfig = a} :: CreateGraphqlApi)

-- | A list of additional authentication providers for the @GraphqlApi@ API.
createGraphqlApi_additionalAuthenticationProviders :: Lens.Lens' CreateGraphqlApi (Prelude.Maybe [AdditionalAuthenticationProvider])
createGraphqlApi_additionalAuthenticationProviders = Lens.lens (\CreateGraphqlApi' {additionalAuthenticationProviders} -> additionalAuthenticationProviders) (\s@CreateGraphqlApi' {} a -> s {additionalAuthenticationProviders = a} :: CreateGraphqlApi) Prelude.. Lens.mapping Prelude._Coerce

-- | A user-supplied name for the @GraphqlApi@.
createGraphqlApi_name :: Lens.Lens' CreateGraphqlApi Prelude.Text
createGraphqlApi_name = Lens.lens (\CreateGraphqlApi' {name} -> name) (\s@CreateGraphqlApi' {} a -> s {name = a} :: CreateGraphqlApi)

-- | The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user
-- pools.
createGraphqlApi_authenticationType :: Lens.Lens' CreateGraphqlApi AuthenticationType
createGraphqlApi_authenticationType = Lens.lens (\CreateGraphqlApi' {authenticationType} -> authenticationType) (\s@CreateGraphqlApi' {} a -> s {authenticationType = a} :: CreateGraphqlApi)

instance Prelude.AWSRequest CreateGraphqlApi where
  type Rs CreateGraphqlApi = CreateGraphqlApiResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGraphqlApiResponse'
            Prelude.<$> (x Prelude..?> "graphqlApi")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGraphqlApi

instance Prelude.NFData CreateGraphqlApi

instance Prelude.ToHeaders CreateGraphqlApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateGraphqlApi where
  toJSON CreateGraphqlApi' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("openIDConnectConfig" Prelude..=)
              Prelude.<$> openIDConnectConfig,
            ("userPoolConfig" Prelude..=)
              Prelude.<$> userPoolConfig,
            ("xrayEnabled" Prelude..=) Prelude.<$> xrayEnabled,
            ("tags" Prelude..=) Prelude.<$> tags,
            ("logConfig" Prelude..=) Prelude.<$> logConfig,
            ("additionalAuthenticationProviders" Prelude..=)
              Prelude.<$> additionalAuthenticationProviders,
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just
              ( "authenticationType"
                  Prelude..= authenticationType
              )
          ]
      )

instance Prelude.ToPath CreateGraphqlApi where
  toPath = Prelude.const "/v1/apis"

instance Prelude.ToQuery CreateGraphqlApi where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGraphqlApiResponse' smart constructor.
data CreateGraphqlApiResponse = CreateGraphqlApiResponse'
  { -- | The @GraphqlApi@.
    graphqlApi :: Prelude.Maybe GraphqlApi,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CreateGraphqlApiResponse
