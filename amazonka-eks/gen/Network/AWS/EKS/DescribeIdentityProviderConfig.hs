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
-- Module      : Network.AWS.EKS.DescribeIdentityProviderConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptive information about an identity provider
-- configuration.
module Network.AWS.EKS.DescribeIdentityProviderConfig
  ( -- * Creating a Request
    DescribeIdentityProviderConfig (..),
    newDescribeIdentityProviderConfig,

    -- * Request Lenses
    describeIdentityProviderConfig_clusterName,
    describeIdentityProviderConfig_identityProviderConfig,

    -- * Destructuring the Response
    DescribeIdentityProviderConfigResponse (..),
    newDescribeIdentityProviderConfigResponse,

    -- * Response Lenses
    describeIdentityProviderConfigResponse_identityProviderConfig,
    describeIdentityProviderConfigResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeIdentityProviderConfig' smart constructor.
data DescribeIdentityProviderConfig = DescribeIdentityProviderConfig'
  { -- | The cluster name that the identity provider configuration is associated
    -- to.
    clusterName :: Core.Text,
    -- | An object that represents an identity provider configuration.
    identityProviderConfig :: IdentityProviderConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeIdentityProviderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterName', 'describeIdentityProviderConfig_clusterName' - The cluster name that the identity provider configuration is associated
-- to.
--
-- 'identityProviderConfig', 'describeIdentityProviderConfig_identityProviderConfig' - An object that represents an identity provider configuration.
newDescribeIdentityProviderConfig ::
  -- | 'clusterName'
  Core.Text ->
  -- | 'identityProviderConfig'
  IdentityProviderConfig ->
  DescribeIdentityProviderConfig
newDescribeIdentityProviderConfig
  pClusterName_
  pIdentityProviderConfig_ =
    DescribeIdentityProviderConfig'
      { clusterName =
          pClusterName_,
        identityProviderConfig =
          pIdentityProviderConfig_
      }

-- | The cluster name that the identity provider configuration is associated
-- to.
describeIdentityProviderConfig_clusterName :: Lens.Lens' DescribeIdentityProviderConfig Core.Text
describeIdentityProviderConfig_clusterName = Lens.lens (\DescribeIdentityProviderConfig' {clusterName} -> clusterName) (\s@DescribeIdentityProviderConfig' {} a -> s {clusterName = a} :: DescribeIdentityProviderConfig)

-- | An object that represents an identity provider configuration.
describeIdentityProviderConfig_identityProviderConfig :: Lens.Lens' DescribeIdentityProviderConfig IdentityProviderConfig
describeIdentityProviderConfig_identityProviderConfig = Lens.lens (\DescribeIdentityProviderConfig' {identityProviderConfig} -> identityProviderConfig) (\s@DescribeIdentityProviderConfig' {} a -> s {identityProviderConfig = a} :: DescribeIdentityProviderConfig)

instance
  Core.AWSRequest
    DescribeIdentityProviderConfig
  where
  type
    AWSResponse DescribeIdentityProviderConfig =
      DescribeIdentityProviderConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIdentityProviderConfigResponse'
            Core.<$> (x Core..?> "identityProviderConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeIdentityProviderConfig

instance Core.NFData DescribeIdentityProviderConfig

instance
  Core.ToHeaders
    DescribeIdentityProviderConfig
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeIdentityProviderConfig where
  toJSON DescribeIdentityProviderConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "identityProviderConfig"
                  Core..= identityProviderConfig
              )
          ]
      )

instance Core.ToPath DescribeIdentityProviderConfig where
  toPath DescribeIdentityProviderConfig' {..} =
    Core.mconcat
      [ "/clusters/",
        Core.toBS clusterName,
        "/identity-provider-configs/describe"
      ]

instance Core.ToQuery DescribeIdentityProviderConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeIdentityProviderConfigResponse' smart constructor.
data DescribeIdentityProviderConfigResponse = DescribeIdentityProviderConfigResponse'
  { -- | The object that represents an OpenID Connect (OIDC) identity provider
    -- configuration.
    identityProviderConfig :: Core.Maybe IdentityProviderConfigResponse,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeIdentityProviderConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityProviderConfig', 'describeIdentityProviderConfigResponse_identityProviderConfig' - The object that represents an OpenID Connect (OIDC) identity provider
-- configuration.
--
-- 'httpStatus', 'describeIdentityProviderConfigResponse_httpStatus' - The response's http status code.
newDescribeIdentityProviderConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeIdentityProviderConfigResponse
newDescribeIdentityProviderConfigResponse
  pHttpStatus_ =
    DescribeIdentityProviderConfigResponse'
      { identityProviderConfig =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The object that represents an OpenID Connect (OIDC) identity provider
-- configuration.
describeIdentityProviderConfigResponse_identityProviderConfig :: Lens.Lens' DescribeIdentityProviderConfigResponse (Core.Maybe IdentityProviderConfigResponse)
describeIdentityProviderConfigResponse_identityProviderConfig = Lens.lens (\DescribeIdentityProviderConfigResponse' {identityProviderConfig} -> identityProviderConfig) (\s@DescribeIdentityProviderConfigResponse' {} a -> s {identityProviderConfig = a} :: DescribeIdentityProviderConfigResponse)

-- | The response's http status code.
describeIdentityProviderConfigResponse_httpStatus :: Lens.Lens' DescribeIdentityProviderConfigResponse Core.Int
describeIdentityProviderConfigResponse_httpStatus = Lens.lens (\DescribeIdentityProviderConfigResponse' {httpStatus} -> httpStatus) (\s@DescribeIdentityProviderConfigResponse' {} a -> s {httpStatus = a} :: DescribeIdentityProviderConfigResponse)

instance
  Core.NFData
    DescribeIdentityProviderConfigResponse
