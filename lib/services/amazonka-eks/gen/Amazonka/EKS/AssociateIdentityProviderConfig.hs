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
-- Module      : Amazonka.EKS.AssociateIdentityProviderConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate an identity provider configuration to a cluster.
--
-- If you want to authenticate identities using an identity provider, you
-- can create an identity provider configuration and associate it to your
-- cluster. After configuring authentication to your cluster you can create
-- Kubernetes @roles@ and @clusterroles@ to assign permissions to the
-- roles, and then bind the roles to the identities using Kubernetes
-- @rolebindings@ and @clusterrolebindings@. For more information see
-- <https://kubernetes.io/docs/reference/access-authn-authz/rbac/ Using RBAC Authorization>
-- in the Kubernetes documentation.
module Amazonka.EKS.AssociateIdentityProviderConfig
  ( -- * Creating a Request
    AssociateIdentityProviderConfig (..),
    newAssociateIdentityProviderConfig,

    -- * Request Lenses
    associateIdentityProviderConfig_clientRequestToken,
    associateIdentityProviderConfig_tags,
    associateIdentityProviderConfig_clusterName,
    associateIdentityProviderConfig_oidc,

    -- * Destructuring the Response
    AssociateIdentityProviderConfigResponse (..),
    newAssociateIdentityProviderConfigResponse,

    -- * Response Lenses
    associateIdentityProviderConfigResponse_update,
    associateIdentityProviderConfigResponse_tags,
    associateIdentityProviderConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EKS.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateIdentityProviderConfig' smart constructor.
data AssociateIdentityProviderConfig = AssociateIdentityProviderConfig'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The metadata to apply to the configuration to assist with categorization
    -- and organization. Each tag consists of a key and an optional value, both
    -- of which you define.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the cluster to associate the configuration to.
    clusterName :: Prelude.Text,
    -- | An object that represents an OpenID Connect (OIDC) identity provider
    -- configuration.
    oidc :: OidcIdentityProviderConfigRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateIdentityProviderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'associateIdentityProviderConfig_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'tags', 'associateIdentityProviderConfig_tags' - The metadata to apply to the configuration to assist with categorization
-- and organization. Each tag consists of a key and an optional value, both
-- of which you define.
--
-- 'clusterName', 'associateIdentityProviderConfig_clusterName' - The name of the cluster to associate the configuration to.
--
-- 'oidc', 'associateIdentityProviderConfig_oidc' - An object that represents an OpenID Connect (OIDC) identity provider
-- configuration.
newAssociateIdentityProviderConfig ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'oidc'
  OidcIdentityProviderConfigRequest ->
  AssociateIdentityProviderConfig
newAssociateIdentityProviderConfig
  pClusterName_
  pOidc_ =
    AssociateIdentityProviderConfig'
      { clientRequestToken =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        clusterName = pClusterName_,
        oidc = pOidc_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
associateIdentityProviderConfig_clientRequestToken :: Lens.Lens' AssociateIdentityProviderConfig (Prelude.Maybe Prelude.Text)
associateIdentityProviderConfig_clientRequestToken = Lens.lens (\AssociateIdentityProviderConfig' {clientRequestToken} -> clientRequestToken) (\s@AssociateIdentityProviderConfig' {} a -> s {clientRequestToken = a} :: AssociateIdentityProviderConfig)

-- | The metadata to apply to the configuration to assist with categorization
-- and organization. Each tag consists of a key and an optional value, both
-- of which you define.
associateIdentityProviderConfig_tags :: Lens.Lens' AssociateIdentityProviderConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
associateIdentityProviderConfig_tags = Lens.lens (\AssociateIdentityProviderConfig' {tags} -> tags) (\s@AssociateIdentityProviderConfig' {} a -> s {tags = a} :: AssociateIdentityProviderConfig) Prelude.. Lens.mapping Lens.coerced

-- | The name of the cluster to associate the configuration to.
associateIdentityProviderConfig_clusterName :: Lens.Lens' AssociateIdentityProviderConfig Prelude.Text
associateIdentityProviderConfig_clusterName = Lens.lens (\AssociateIdentityProviderConfig' {clusterName} -> clusterName) (\s@AssociateIdentityProviderConfig' {} a -> s {clusterName = a} :: AssociateIdentityProviderConfig)

-- | An object that represents an OpenID Connect (OIDC) identity provider
-- configuration.
associateIdentityProviderConfig_oidc :: Lens.Lens' AssociateIdentityProviderConfig OidcIdentityProviderConfigRequest
associateIdentityProviderConfig_oidc = Lens.lens (\AssociateIdentityProviderConfig' {oidc} -> oidc) (\s@AssociateIdentityProviderConfig' {} a -> s {oidc = a} :: AssociateIdentityProviderConfig)

instance
  Core.AWSRequest
    AssociateIdentityProviderConfig
  where
  type
    AWSResponse AssociateIdentityProviderConfig =
      AssociateIdentityProviderConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateIdentityProviderConfigResponse'
            Prelude.<$> (x Core..?> "update")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateIdentityProviderConfig

instance
  Prelude.NFData
    AssociateIdentityProviderConfig

instance
  Core.ToHeaders
    AssociateIdentityProviderConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateIdentityProviderConfig where
  toJSON AssociateIdentityProviderConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("oidc" Core..= oidc)
          ]
      )

instance Core.ToPath AssociateIdentityProviderConfig where
  toPath AssociateIdentityProviderConfig' {..} =
    Prelude.mconcat
      [ "/clusters/",
        Core.toBS clusterName,
        "/identity-provider-configs/associate"
      ]

instance Core.ToQuery AssociateIdentityProviderConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateIdentityProviderConfigResponse' smart constructor.
data AssociateIdentityProviderConfigResponse = AssociateIdentityProviderConfigResponse'
  { update :: Prelude.Maybe Update,
    -- | The tags for the resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateIdentityProviderConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'update', 'associateIdentityProviderConfigResponse_update' - Undocumented member.
--
-- 'tags', 'associateIdentityProviderConfigResponse_tags' - The tags for the resource.
--
-- 'httpStatus', 'associateIdentityProviderConfigResponse_httpStatus' - The response's http status code.
newAssociateIdentityProviderConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateIdentityProviderConfigResponse
newAssociateIdentityProviderConfigResponse
  pHttpStatus_ =
    AssociateIdentityProviderConfigResponse'
      { update =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
associateIdentityProviderConfigResponse_update :: Lens.Lens' AssociateIdentityProviderConfigResponse (Prelude.Maybe Update)
associateIdentityProviderConfigResponse_update = Lens.lens (\AssociateIdentityProviderConfigResponse' {update} -> update) (\s@AssociateIdentityProviderConfigResponse' {} a -> s {update = a} :: AssociateIdentityProviderConfigResponse)

-- | The tags for the resource.
associateIdentityProviderConfigResponse_tags :: Lens.Lens' AssociateIdentityProviderConfigResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
associateIdentityProviderConfigResponse_tags = Lens.lens (\AssociateIdentityProviderConfigResponse' {tags} -> tags) (\s@AssociateIdentityProviderConfigResponse' {} a -> s {tags = a} :: AssociateIdentityProviderConfigResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
associateIdentityProviderConfigResponse_httpStatus :: Lens.Lens' AssociateIdentityProviderConfigResponse Prelude.Int
associateIdentityProviderConfigResponse_httpStatus = Lens.lens (\AssociateIdentityProviderConfigResponse' {httpStatus} -> httpStatus) (\s@AssociateIdentityProviderConfigResponse' {} a -> s {httpStatus = a} :: AssociateIdentityProviderConfigResponse)

instance
  Prelude.NFData
    AssociateIdentityProviderConfigResponse
