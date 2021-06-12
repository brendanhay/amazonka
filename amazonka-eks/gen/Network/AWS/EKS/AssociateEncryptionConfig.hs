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
-- Module      : Network.AWS.EKS.AssociateEncryptionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate encryption configuration to an existing cluster.
--
-- You can use this API to enable encryption on existing clusters which do
-- not have encryption already enabled. This allows you to implement a
-- defense-in-depth security strategy without migrating applications to new
-- EKS clusters.
module Network.AWS.EKS.AssociateEncryptionConfig
  ( -- * Creating a Request
    AssociateEncryptionConfig (..),
    newAssociateEncryptionConfig,

    -- * Request Lenses
    associateEncryptionConfig_clientRequestToken,
    associateEncryptionConfig_clusterName,
    associateEncryptionConfig_encryptionConfig,

    -- * Destructuring the Response
    AssociateEncryptionConfigResponse (..),
    newAssociateEncryptionConfigResponse,

    -- * Response Lenses
    associateEncryptionConfigResponse_update,
    associateEncryptionConfigResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateEncryptionConfig' smart constructor.
data AssociateEncryptionConfig = AssociateEncryptionConfig'
  { -- | The client request token you are using with the encryption
    -- configuration.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The name of the cluster that you are associating with encryption
    -- configuration.
    clusterName :: Core.Text,
    -- | The configuration you are using for encryption.
    encryptionConfig :: [EncryptionConfig]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateEncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'associateEncryptionConfig_clientRequestToken' - The client request token you are using with the encryption
-- configuration.
--
-- 'clusterName', 'associateEncryptionConfig_clusterName' - The name of the cluster that you are associating with encryption
-- configuration.
--
-- 'encryptionConfig', 'associateEncryptionConfig_encryptionConfig' - The configuration you are using for encryption.
newAssociateEncryptionConfig ::
  -- | 'clusterName'
  Core.Text ->
  AssociateEncryptionConfig
newAssociateEncryptionConfig pClusterName_ =
  AssociateEncryptionConfig'
    { clientRequestToken =
        Core.Nothing,
      clusterName = pClusterName_,
      encryptionConfig = Core.mempty
    }

-- | The client request token you are using with the encryption
-- configuration.
associateEncryptionConfig_clientRequestToken :: Lens.Lens' AssociateEncryptionConfig (Core.Maybe Core.Text)
associateEncryptionConfig_clientRequestToken = Lens.lens (\AssociateEncryptionConfig' {clientRequestToken} -> clientRequestToken) (\s@AssociateEncryptionConfig' {} a -> s {clientRequestToken = a} :: AssociateEncryptionConfig)

-- | The name of the cluster that you are associating with encryption
-- configuration.
associateEncryptionConfig_clusterName :: Lens.Lens' AssociateEncryptionConfig Core.Text
associateEncryptionConfig_clusterName = Lens.lens (\AssociateEncryptionConfig' {clusterName} -> clusterName) (\s@AssociateEncryptionConfig' {} a -> s {clusterName = a} :: AssociateEncryptionConfig)

-- | The configuration you are using for encryption.
associateEncryptionConfig_encryptionConfig :: Lens.Lens' AssociateEncryptionConfig [EncryptionConfig]
associateEncryptionConfig_encryptionConfig = Lens.lens (\AssociateEncryptionConfig' {encryptionConfig} -> encryptionConfig) (\s@AssociateEncryptionConfig' {} a -> s {encryptionConfig = a} :: AssociateEncryptionConfig) Core.. Lens._Coerce

instance Core.AWSRequest AssociateEncryptionConfig where
  type
    AWSResponse AssociateEncryptionConfig =
      AssociateEncryptionConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateEncryptionConfigResponse'
            Core.<$> (x Core..?> "update")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateEncryptionConfig

instance Core.NFData AssociateEncryptionConfig

instance Core.ToHeaders AssociateEncryptionConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateEncryptionConfig where
  toJSON AssociateEncryptionConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("clientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            Core.Just
              ("encryptionConfig" Core..= encryptionConfig)
          ]
      )

instance Core.ToPath AssociateEncryptionConfig where
  toPath AssociateEncryptionConfig' {..} =
    Core.mconcat
      [ "/clusters/",
        Core.toBS clusterName,
        "/encryption-config/associate"
      ]

instance Core.ToQuery AssociateEncryptionConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateEncryptionConfigResponse' smart constructor.
data AssociateEncryptionConfigResponse = AssociateEncryptionConfigResponse'
  { update :: Core.Maybe Update,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateEncryptionConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'update', 'associateEncryptionConfigResponse_update' - Undocumented member.
--
-- 'httpStatus', 'associateEncryptionConfigResponse_httpStatus' - The response's http status code.
newAssociateEncryptionConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateEncryptionConfigResponse
newAssociateEncryptionConfigResponse pHttpStatus_ =
  AssociateEncryptionConfigResponse'
    { update =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
associateEncryptionConfigResponse_update :: Lens.Lens' AssociateEncryptionConfigResponse (Core.Maybe Update)
associateEncryptionConfigResponse_update = Lens.lens (\AssociateEncryptionConfigResponse' {update} -> update) (\s@AssociateEncryptionConfigResponse' {} a -> s {update = a} :: AssociateEncryptionConfigResponse)

-- | The response's http status code.
associateEncryptionConfigResponse_httpStatus :: Lens.Lens' AssociateEncryptionConfigResponse Core.Int
associateEncryptionConfigResponse_httpStatus = Lens.lens (\AssociateEncryptionConfigResponse' {httpStatus} -> httpStatus) (\s@AssociateEncryptionConfigResponse' {} a -> s {httpStatus = a} :: AssociateEncryptionConfigResponse)

instance
  Core.NFData
    AssociateEncryptionConfigResponse
