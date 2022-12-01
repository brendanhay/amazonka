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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    associateIdentityProviderConfig_tags,
    associateIdentityProviderConfig_clientRequestToken,
    associateIdentityProviderConfig_clusterName,
    associateIdentityProviderConfig_oidc,

    -- * Destructuring the Response
    AssociateIdentityProviderConfigResponse (..),
    newAssociateIdentityProviderConfigResponse,

    -- * Response Lenses
    associateIdentityProviderConfigResponse_tags,
    associateIdentityProviderConfigResponse_update,
    associateIdentityProviderConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateIdentityProviderConfig' smart constructor.
data AssociateIdentityProviderConfig = AssociateIdentityProviderConfig'
  { -- | The metadata to apply to the configuration to assist with categorization
    -- and organization. Each tag consists of a key and an optional value. You
    -- define both.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster to associate the configuration to.
    clusterName :: Prelude.Text,
    -- | An object representing an OpenID Connect (OIDC) identity provider
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
-- 'tags', 'associateIdentityProviderConfig_tags' - The metadata to apply to the configuration to assist with categorization
-- and organization. Each tag consists of a key and an optional value. You
-- define both.
--
-- 'clientRequestToken', 'associateIdentityProviderConfig_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'clusterName', 'associateIdentityProviderConfig_clusterName' - The name of the cluster to associate the configuration to.
--
-- 'oidc', 'associateIdentityProviderConfig_oidc' - An object representing an OpenID Connect (OIDC) identity provider
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
      { tags =
          Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        clusterName = pClusterName_,
        oidc = pOidc_
      }

-- | The metadata to apply to the configuration to assist with categorization
-- and organization. Each tag consists of a key and an optional value. You
-- define both.
associateIdentityProviderConfig_tags :: Lens.Lens' AssociateIdentityProviderConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
associateIdentityProviderConfig_tags = Lens.lens (\AssociateIdentityProviderConfig' {tags} -> tags) (\s@AssociateIdentityProviderConfig' {} a -> s {tags = a} :: AssociateIdentityProviderConfig) Prelude.. Lens.mapping Lens.coerced

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
associateIdentityProviderConfig_clientRequestToken :: Lens.Lens' AssociateIdentityProviderConfig (Prelude.Maybe Prelude.Text)
associateIdentityProviderConfig_clientRequestToken = Lens.lens (\AssociateIdentityProviderConfig' {clientRequestToken} -> clientRequestToken) (\s@AssociateIdentityProviderConfig' {} a -> s {clientRequestToken = a} :: AssociateIdentityProviderConfig)

-- | The name of the cluster to associate the configuration to.
associateIdentityProviderConfig_clusterName :: Lens.Lens' AssociateIdentityProviderConfig Prelude.Text
associateIdentityProviderConfig_clusterName = Lens.lens (\AssociateIdentityProviderConfig' {clusterName} -> clusterName) (\s@AssociateIdentityProviderConfig' {} a -> s {clusterName = a} :: AssociateIdentityProviderConfig)

-- | An object representing an OpenID Connect (OIDC) identity provider
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateIdentityProviderConfigResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "update")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateIdentityProviderConfig
  where
  hashWithSalt
    _salt
    AssociateIdentityProviderConfig' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` clientRequestToken
        `Prelude.hashWithSalt` clusterName
        `Prelude.hashWithSalt` oidc

instance
  Prelude.NFData
    AssociateIdentityProviderConfig
  where
  rnf AssociateIdentityProviderConfig' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf oidc

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
          [ ("tags" Core..=) Prelude.<$> tags,
            ("clientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
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
  { -- | The tags for the resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    update :: Prelude.Maybe Update,
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
-- 'tags', 'associateIdentityProviderConfigResponse_tags' - The tags for the resource.
--
-- 'update', 'associateIdentityProviderConfigResponse_update' - Undocumented member.
--
-- 'httpStatus', 'associateIdentityProviderConfigResponse_httpStatus' - The response's http status code.
newAssociateIdentityProviderConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateIdentityProviderConfigResponse
newAssociateIdentityProviderConfigResponse
  pHttpStatus_ =
    AssociateIdentityProviderConfigResponse'
      { tags =
          Prelude.Nothing,
        update = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The tags for the resource.
associateIdentityProviderConfigResponse_tags :: Lens.Lens' AssociateIdentityProviderConfigResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
associateIdentityProviderConfigResponse_tags = Lens.lens (\AssociateIdentityProviderConfigResponse' {tags} -> tags) (\s@AssociateIdentityProviderConfigResponse' {} a -> s {tags = a} :: AssociateIdentityProviderConfigResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
associateIdentityProviderConfigResponse_update :: Lens.Lens' AssociateIdentityProviderConfigResponse (Prelude.Maybe Update)
associateIdentityProviderConfigResponse_update = Lens.lens (\AssociateIdentityProviderConfigResponse' {update} -> update) (\s@AssociateIdentityProviderConfigResponse' {} a -> s {update = a} :: AssociateIdentityProviderConfigResponse)

-- | The response's http status code.
associateIdentityProviderConfigResponse_httpStatus :: Lens.Lens' AssociateIdentityProviderConfigResponse Prelude.Int
associateIdentityProviderConfigResponse_httpStatus = Lens.lens (\AssociateIdentityProviderConfigResponse' {httpStatus} -> httpStatus) (\s@AssociateIdentityProviderConfigResponse' {} a -> s {httpStatus = a} :: AssociateIdentityProviderConfigResponse)

instance
  Prelude.NFData
    AssociateIdentityProviderConfigResponse
  where
  rnf AssociateIdentityProviderConfigResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf update
      `Prelude.seq` Prelude.rnf httpStatus
