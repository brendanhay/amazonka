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
-- Module      : Network.AWS.AppSync.UpdateGraphqlApi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @GraphqlApi@ object.
module Network.AWS.AppSync.UpdateGraphqlApi
  ( -- * Creating a Request
    UpdateGraphqlApi (..),
    newUpdateGraphqlApi,

    -- * Request Lenses
    updateGraphqlApi_openIDConnectConfig,
    updateGraphqlApi_userPoolConfig,
    updateGraphqlApi_xrayEnabled,
    updateGraphqlApi_logConfig,
    updateGraphqlApi_additionalAuthenticationProviders,
    updateGraphqlApi_authenticationType,
    updateGraphqlApi_apiId,
    updateGraphqlApi_name,

    -- * Destructuring the Response
    UpdateGraphqlApiResponse (..),
    newUpdateGraphqlApiResponse,

    -- * Response Lenses
    updateGraphqlApiResponse_graphqlApi,
    updateGraphqlApiResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateGraphqlApi' smart constructor.
data UpdateGraphqlApi = UpdateGraphqlApi'
  { -- | The OpenID Connect configuration for the @GraphqlApi@ object.
    openIDConnectConfig :: Core.Maybe OpenIDConnectConfig,
    -- | The new Amazon Cognito user pool configuration for the @GraphqlApi@
    -- object.
    userPoolConfig :: Core.Maybe UserPoolConfig,
    -- | A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@.
    xrayEnabled :: Core.Maybe Core.Bool,
    -- | The Amazon CloudWatch Logs configuration for the @GraphqlApi@ object.
    logConfig :: Core.Maybe LogConfig,
    -- | A list of additional authentication providers for the @GraphqlApi@ API.
    additionalAuthenticationProviders :: Core.Maybe [AdditionalAuthenticationProvider],
    -- | The new authentication type for the @GraphqlApi@ object.
    authenticationType :: Core.Maybe AuthenticationType,
    -- | The API ID.
    apiId :: Core.Text,
    -- | The new name for the @GraphqlApi@ object.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGraphqlApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openIDConnectConfig', 'updateGraphqlApi_openIDConnectConfig' - The OpenID Connect configuration for the @GraphqlApi@ object.
--
-- 'userPoolConfig', 'updateGraphqlApi_userPoolConfig' - The new Amazon Cognito user pool configuration for the @GraphqlApi@
-- object.
--
-- 'xrayEnabled', 'updateGraphqlApi_xrayEnabled' - A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@.
--
-- 'logConfig', 'updateGraphqlApi_logConfig' - The Amazon CloudWatch Logs configuration for the @GraphqlApi@ object.
--
-- 'additionalAuthenticationProviders', 'updateGraphqlApi_additionalAuthenticationProviders' - A list of additional authentication providers for the @GraphqlApi@ API.
--
-- 'authenticationType', 'updateGraphqlApi_authenticationType' - The new authentication type for the @GraphqlApi@ object.
--
-- 'apiId', 'updateGraphqlApi_apiId' - The API ID.
--
-- 'name', 'updateGraphqlApi_name' - The new name for the @GraphqlApi@ object.
newUpdateGraphqlApi ::
  -- | 'apiId'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  UpdateGraphqlApi
newUpdateGraphqlApi pApiId_ pName_ =
  UpdateGraphqlApi'
    { openIDConnectConfig =
        Core.Nothing,
      userPoolConfig = Core.Nothing,
      xrayEnabled = Core.Nothing,
      logConfig = Core.Nothing,
      additionalAuthenticationProviders = Core.Nothing,
      authenticationType = Core.Nothing,
      apiId = pApiId_,
      name = pName_
    }

-- | The OpenID Connect configuration for the @GraphqlApi@ object.
updateGraphqlApi_openIDConnectConfig :: Lens.Lens' UpdateGraphqlApi (Core.Maybe OpenIDConnectConfig)
updateGraphqlApi_openIDConnectConfig = Lens.lens (\UpdateGraphqlApi' {openIDConnectConfig} -> openIDConnectConfig) (\s@UpdateGraphqlApi' {} a -> s {openIDConnectConfig = a} :: UpdateGraphqlApi)

-- | The new Amazon Cognito user pool configuration for the @GraphqlApi@
-- object.
updateGraphqlApi_userPoolConfig :: Lens.Lens' UpdateGraphqlApi (Core.Maybe UserPoolConfig)
updateGraphqlApi_userPoolConfig = Lens.lens (\UpdateGraphqlApi' {userPoolConfig} -> userPoolConfig) (\s@UpdateGraphqlApi' {} a -> s {userPoolConfig = a} :: UpdateGraphqlApi)

-- | A flag indicating whether to enable X-Ray tracing for the @GraphqlApi@.
updateGraphqlApi_xrayEnabled :: Lens.Lens' UpdateGraphqlApi (Core.Maybe Core.Bool)
updateGraphqlApi_xrayEnabled = Lens.lens (\UpdateGraphqlApi' {xrayEnabled} -> xrayEnabled) (\s@UpdateGraphqlApi' {} a -> s {xrayEnabled = a} :: UpdateGraphqlApi)

-- | The Amazon CloudWatch Logs configuration for the @GraphqlApi@ object.
updateGraphqlApi_logConfig :: Lens.Lens' UpdateGraphqlApi (Core.Maybe LogConfig)
updateGraphqlApi_logConfig = Lens.lens (\UpdateGraphqlApi' {logConfig} -> logConfig) (\s@UpdateGraphqlApi' {} a -> s {logConfig = a} :: UpdateGraphqlApi)

-- | A list of additional authentication providers for the @GraphqlApi@ API.
updateGraphqlApi_additionalAuthenticationProviders :: Lens.Lens' UpdateGraphqlApi (Core.Maybe [AdditionalAuthenticationProvider])
updateGraphqlApi_additionalAuthenticationProviders = Lens.lens (\UpdateGraphqlApi' {additionalAuthenticationProviders} -> additionalAuthenticationProviders) (\s@UpdateGraphqlApi' {} a -> s {additionalAuthenticationProviders = a} :: UpdateGraphqlApi) Core.. Lens.mapping Lens._Coerce

-- | The new authentication type for the @GraphqlApi@ object.
updateGraphqlApi_authenticationType :: Lens.Lens' UpdateGraphqlApi (Core.Maybe AuthenticationType)
updateGraphqlApi_authenticationType = Lens.lens (\UpdateGraphqlApi' {authenticationType} -> authenticationType) (\s@UpdateGraphqlApi' {} a -> s {authenticationType = a} :: UpdateGraphqlApi)

-- | The API ID.
updateGraphqlApi_apiId :: Lens.Lens' UpdateGraphqlApi Core.Text
updateGraphqlApi_apiId = Lens.lens (\UpdateGraphqlApi' {apiId} -> apiId) (\s@UpdateGraphqlApi' {} a -> s {apiId = a} :: UpdateGraphqlApi)

-- | The new name for the @GraphqlApi@ object.
updateGraphqlApi_name :: Lens.Lens' UpdateGraphqlApi Core.Text
updateGraphqlApi_name = Lens.lens (\UpdateGraphqlApi' {name} -> name) (\s@UpdateGraphqlApi' {} a -> s {name = a} :: UpdateGraphqlApi)

instance Core.AWSRequest UpdateGraphqlApi where
  type
    AWSResponse UpdateGraphqlApi =
      UpdateGraphqlApiResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGraphqlApiResponse'
            Core.<$> (x Core..?> "graphqlApi")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateGraphqlApi

instance Core.NFData UpdateGraphqlApi

instance Core.ToHeaders UpdateGraphqlApi where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateGraphqlApi where
  toJSON UpdateGraphqlApi' {..} =
    Core.object
      ( Core.catMaybes
          [ ("openIDConnectConfig" Core..=)
              Core.<$> openIDConnectConfig,
            ("userPoolConfig" Core..=) Core.<$> userPoolConfig,
            ("xrayEnabled" Core..=) Core.<$> xrayEnabled,
            ("logConfig" Core..=) Core.<$> logConfig,
            ("additionalAuthenticationProviders" Core..=)
              Core.<$> additionalAuthenticationProviders,
            ("authenticationType" Core..=)
              Core.<$> authenticationType,
            Core.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath UpdateGraphqlApi where
  toPath UpdateGraphqlApi' {..} =
    Core.mconcat ["/v1/apis/", Core.toBS apiId]

instance Core.ToQuery UpdateGraphqlApi where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateGraphqlApiResponse' smart constructor.
data UpdateGraphqlApiResponse = UpdateGraphqlApiResponse'
  { -- | The updated @GraphqlApi@ object.
    graphqlApi :: Core.Maybe GraphqlApi,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGraphqlApiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'graphqlApi', 'updateGraphqlApiResponse_graphqlApi' - The updated @GraphqlApi@ object.
--
-- 'httpStatus', 'updateGraphqlApiResponse_httpStatus' - The response's http status code.
newUpdateGraphqlApiResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateGraphqlApiResponse
newUpdateGraphqlApiResponse pHttpStatus_ =
  UpdateGraphqlApiResponse'
    { graphqlApi =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated @GraphqlApi@ object.
updateGraphqlApiResponse_graphqlApi :: Lens.Lens' UpdateGraphqlApiResponse (Core.Maybe GraphqlApi)
updateGraphqlApiResponse_graphqlApi = Lens.lens (\UpdateGraphqlApiResponse' {graphqlApi} -> graphqlApi) (\s@UpdateGraphqlApiResponse' {} a -> s {graphqlApi = a} :: UpdateGraphqlApiResponse)

-- | The response's http status code.
updateGraphqlApiResponse_httpStatus :: Lens.Lens' UpdateGraphqlApiResponse Core.Int
updateGraphqlApiResponse_httpStatus = Lens.lens (\UpdateGraphqlApiResponse' {httpStatus} -> httpStatus) (\s@UpdateGraphqlApiResponse' {} a -> s {httpStatus = a} :: UpdateGraphqlApiResponse)

instance Core.NFData UpdateGraphqlApiResponse
