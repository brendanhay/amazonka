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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateGraphqlApi' smart constructor.
data CreateGraphqlApi = CreateGraphqlApi'
  { -- | The OpenID Connect configuration.
    openIDConnectConfig :: Core.Maybe OpenIDConnectConfig,
    -- | The Amazon Cognito user pool configuration.
    userPoolConfig :: Core.Maybe UserPoolConfig,
    -- | A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@.
    xrayEnabled :: Core.Maybe Core.Bool,
    -- | A @TagMap@ object.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The Amazon CloudWatch Logs configuration.
    logConfig :: Core.Maybe LogConfig,
    -- | A list of additional authentication providers for the @GraphqlApi@ API.
    additionalAuthenticationProviders :: Core.Maybe [AdditionalAuthenticationProvider],
    -- | A user-supplied name for the @GraphqlApi@.
    name :: Core.Text,
    -- | The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user
    -- pools.
    authenticationType :: AuthenticationType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  CreateGraphqlApi
newCreateGraphqlApi pName_ pAuthenticationType_ =
  CreateGraphqlApi'
    { openIDConnectConfig =
        Core.Nothing,
      userPoolConfig = Core.Nothing,
      xrayEnabled = Core.Nothing,
      tags = Core.Nothing,
      logConfig = Core.Nothing,
      additionalAuthenticationProviders = Core.Nothing,
      name = pName_,
      authenticationType = pAuthenticationType_
    }

-- | The OpenID Connect configuration.
createGraphqlApi_openIDConnectConfig :: Lens.Lens' CreateGraphqlApi (Core.Maybe OpenIDConnectConfig)
createGraphqlApi_openIDConnectConfig = Lens.lens (\CreateGraphqlApi' {openIDConnectConfig} -> openIDConnectConfig) (\s@CreateGraphqlApi' {} a -> s {openIDConnectConfig = a} :: CreateGraphqlApi)

-- | The Amazon Cognito user pool configuration.
createGraphqlApi_userPoolConfig :: Lens.Lens' CreateGraphqlApi (Core.Maybe UserPoolConfig)
createGraphqlApi_userPoolConfig = Lens.lens (\CreateGraphqlApi' {userPoolConfig} -> userPoolConfig) (\s@CreateGraphqlApi' {} a -> s {userPoolConfig = a} :: CreateGraphqlApi)

-- | A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@.
createGraphqlApi_xrayEnabled :: Lens.Lens' CreateGraphqlApi (Core.Maybe Core.Bool)
createGraphqlApi_xrayEnabled = Lens.lens (\CreateGraphqlApi' {xrayEnabled} -> xrayEnabled) (\s@CreateGraphqlApi' {} a -> s {xrayEnabled = a} :: CreateGraphqlApi)

-- | A @TagMap@ object.
createGraphqlApi_tags :: Lens.Lens' CreateGraphqlApi (Core.Maybe (Core.HashMap Core.Text Core.Text))
createGraphqlApi_tags = Lens.lens (\CreateGraphqlApi' {tags} -> tags) (\s@CreateGraphqlApi' {} a -> s {tags = a} :: CreateGraphqlApi) Core.. Lens.mapping Lens._Coerce

-- | The Amazon CloudWatch Logs configuration.
createGraphqlApi_logConfig :: Lens.Lens' CreateGraphqlApi (Core.Maybe LogConfig)
createGraphqlApi_logConfig = Lens.lens (\CreateGraphqlApi' {logConfig} -> logConfig) (\s@CreateGraphqlApi' {} a -> s {logConfig = a} :: CreateGraphqlApi)

-- | A list of additional authentication providers for the @GraphqlApi@ API.
createGraphqlApi_additionalAuthenticationProviders :: Lens.Lens' CreateGraphqlApi (Core.Maybe [AdditionalAuthenticationProvider])
createGraphqlApi_additionalAuthenticationProviders = Lens.lens (\CreateGraphqlApi' {additionalAuthenticationProviders} -> additionalAuthenticationProviders) (\s@CreateGraphqlApi' {} a -> s {additionalAuthenticationProviders = a} :: CreateGraphqlApi) Core.. Lens.mapping Lens._Coerce

-- | A user-supplied name for the @GraphqlApi@.
createGraphqlApi_name :: Lens.Lens' CreateGraphqlApi Core.Text
createGraphqlApi_name = Lens.lens (\CreateGraphqlApi' {name} -> name) (\s@CreateGraphqlApi' {} a -> s {name = a} :: CreateGraphqlApi)

-- | The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user
-- pools.
createGraphqlApi_authenticationType :: Lens.Lens' CreateGraphqlApi AuthenticationType
createGraphqlApi_authenticationType = Lens.lens (\CreateGraphqlApi' {authenticationType} -> authenticationType) (\s@CreateGraphqlApi' {} a -> s {authenticationType = a} :: CreateGraphqlApi)

instance Core.AWSRequest CreateGraphqlApi where
  type
    AWSResponse CreateGraphqlApi =
      CreateGraphqlApiResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGraphqlApiResponse'
            Core.<$> (x Core..?> "graphqlApi")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateGraphqlApi

instance Core.NFData CreateGraphqlApi

instance Core.ToHeaders CreateGraphqlApi where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateGraphqlApi where
  toJSON CreateGraphqlApi' {..} =
    Core.object
      ( Core.catMaybes
          [ ("openIDConnectConfig" Core..=)
              Core.<$> openIDConnectConfig,
            ("userPoolConfig" Core..=) Core.<$> userPoolConfig,
            ("xrayEnabled" Core..=) Core.<$> xrayEnabled,
            ("tags" Core..=) Core.<$> tags,
            ("logConfig" Core..=) Core.<$> logConfig,
            ("additionalAuthenticationProviders" Core..=)
              Core.<$> additionalAuthenticationProviders,
            Core.Just ("name" Core..= name),
            Core.Just
              ("authenticationType" Core..= authenticationType)
          ]
      )

instance Core.ToPath CreateGraphqlApi where
  toPath = Core.const "/v1/apis"

instance Core.ToQuery CreateGraphqlApi where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateGraphqlApiResponse' smart constructor.
data CreateGraphqlApiResponse = CreateGraphqlApiResponse'
  { -- | The @GraphqlApi@.
    graphqlApi :: Core.Maybe GraphqlApi,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateGraphqlApiResponse
newCreateGraphqlApiResponse pHttpStatus_ =
  CreateGraphqlApiResponse'
    { graphqlApi =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @GraphqlApi@.
createGraphqlApiResponse_graphqlApi :: Lens.Lens' CreateGraphqlApiResponse (Core.Maybe GraphqlApi)
createGraphqlApiResponse_graphqlApi = Lens.lens (\CreateGraphqlApiResponse' {graphqlApi} -> graphqlApi) (\s@CreateGraphqlApiResponse' {} a -> s {graphqlApi = a} :: CreateGraphqlApiResponse)

-- | The response's http status code.
createGraphqlApiResponse_httpStatus :: Lens.Lens' CreateGraphqlApiResponse Core.Int
createGraphqlApiResponse_httpStatus = Lens.lens (\CreateGraphqlApiResponse' {httpStatus} -> httpStatus) (\s@CreateGraphqlApiResponse' {} a -> s {httpStatus = a} :: CreateGraphqlApiResponse)

instance Core.NFData CreateGraphqlApiResponse
