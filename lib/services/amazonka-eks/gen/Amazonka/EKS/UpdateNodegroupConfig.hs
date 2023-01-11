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
-- Module      : Amazonka.EKS.UpdateNodegroupConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon EKS managed node group configuration. Your node group
-- continues to function during the update. The response output includes an
-- update ID that you can use to track the status of your node group update
-- with the DescribeUpdate API operation. Currently you can update the
-- Kubernetes labels for a node group or the scaling configuration.
module Amazonka.EKS.UpdateNodegroupConfig
  ( -- * Creating a Request
    UpdateNodegroupConfig (..),
    newUpdateNodegroupConfig,

    -- * Request Lenses
    updateNodegroupConfig_clientRequestToken,
    updateNodegroupConfig_labels,
    updateNodegroupConfig_scalingConfig,
    updateNodegroupConfig_taints,
    updateNodegroupConfig_updateConfig,
    updateNodegroupConfig_clusterName,
    updateNodegroupConfig_nodegroupName,

    -- * Destructuring the Response
    UpdateNodegroupConfigResponse (..),
    newUpdateNodegroupConfigResponse,

    -- * Response Lenses
    updateNodegroupConfigResponse_update,
    updateNodegroupConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateNodegroupConfig' smart constructor.
data UpdateNodegroupConfig = UpdateNodegroupConfig'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The Kubernetes labels to be applied to the nodes in the node group after
    -- the update.
    labels :: Prelude.Maybe UpdateLabelsPayload,
    -- | The scaling configuration details for the Auto Scaling group after the
    -- update.
    scalingConfig :: Prelude.Maybe NodegroupScalingConfig,
    -- | The Kubernetes taints to be applied to the nodes in the node group after
    -- the update. For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/node-taints-managed-node-groups.html Node taints on managed node groups>.
    taints :: Prelude.Maybe UpdateTaintsPayload,
    -- | The node group update configuration.
    updateConfig :: Prelude.Maybe NodegroupUpdateConfig,
    -- | The name of the Amazon EKS cluster that the managed node group resides
    -- in.
    clusterName :: Prelude.Text,
    -- | The name of the managed node group to update.
    nodegroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNodegroupConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'updateNodegroupConfig_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'labels', 'updateNodegroupConfig_labels' - The Kubernetes labels to be applied to the nodes in the node group after
-- the update.
--
-- 'scalingConfig', 'updateNodegroupConfig_scalingConfig' - The scaling configuration details for the Auto Scaling group after the
-- update.
--
-- 'taints', 'updateNodegroupConfig_taints' - The Kubernetes taints to be applied to the nodes in the node group after
-- the update. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/node-taints-managed-node-groups.html Node taints on managed node groups>.
--
-- 'updateConfig', 'updateNodegroupConfig_updateConfig' - The node group update configuration.
--
-- 'clusterName', 'updateNodegroupConfig_clusterName' - The name of the Amazon EKS cluster that the managed node group resides
-- in.
--
-- 'nodegroupName', 'updateNodegroupConfig_nodegroupName' - The name of the managed node group to update.
newUpdateNodegroupConfig ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'nodegroupName'
  Prelude.Text ->
  UpdateNodegroupConfig
newUpdateNodegroupConfig
  pClusterName_
  pNodegroupName_ =
    UpdateNodegroupConfig'
      { clientRequestToken =
          Prelude.Nothing,
        labels = Prelude.Nothing,
        scalingConfig = Prelude.Nothing,
        taints = Prelude.Nothing,
        updateConfig = Prelude.Nothing,
        clusterName = pClusterName_,
        nodegroupName = pNodegroupName_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
updateNodegroupConfig_clientRequestToken :: Lens.Lens' UpdateNodegroupConfig (Prelude.Maybe Prelude.Text)
updateNodegroupConfig_clientRequestToken = Lens.lens (\UpdateNodegroupConfig' {clientRequestToken} -> clientRequestToken) (\s@UpdateNodegroupConfig' {} a -> s {clientRequestToken = a} :: UpdateNodegroupConfig)

-- | The Kubernetes labels to be applied to the nodes in the node group after
-- the update.
updateNodegroupConfig_labels :: Lens.Lens' UpdateNodegroupConfig (Prelude.Maybe UpdateLabelsPayload)
updateNodegroupConfig_labels = Lens.lens (\UpdateNodegroupConfig' {labels} -> labels) (\s@UpdateNodegroupConfig' {} a -> s {labels = a} :: UpdateNodegroupConfig)

-- | The scaling configuration details for the Auto Scaling group after the
-- update.
updateNodegroupConfig_scalingConfig :: Lens.Lens' UpdateNodegroupConfig (Prelude.Maybe NodegroupScalingConfig)
updateNodegroupConfig_scalingConfig = Lens.lens (\UpdateNodegroupConfig' {scalingConfig} -> scalingConfig) (\s@UpdateNodegroupConfig' {} a -> s {scalingConfig = a} :: UpdateNodegroupConfig)

-- | The Kubernetes taints to be applied to the nodes in the node group after
-- the update. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/node-taints-managed-node-groups.html Node taints on managed node groups>.
updateNodegroupConfig_taints :: Lens.Lens' UpdateNodegroupConfig (Prelude.Maybe UpdateTaintsPayload)
updateNodegroupConfig_taints = Lens.lens (\UpdateNodegroupConfig' {taints} -> taints) (\s@UpdateNodegroupConfig' {} a -> s {taints = a} :: UpdateNodegroupConfig)

-- | The node group update configuration.
updateNodegroupConfig_updateConfig :: Lens.Lens' UpdateNodegroupConfig (Prelude.Maybe NodegroupUpdateConfig)
updateNodegroupConfig_updateConfig = Lens.lens (\UpdateNodegroupConfig' {updateConfig} -> updateConfig) (\s@UpdateNodegroupConfig' {} a -> s {updateConfig = a} :: UpdateNodegroupConfig)

-- | The name of the Amazon EKS cluster that the managed node group resides
-- in.
updateNodegroupConfig_clusterName :: Lens.Lens' UpdateNodegroupConfig Prelude.Text
updateNodegroupConfig_clusterName = Lens.lens (\UpdateNodegroupConfig' {clusterName} -> clusterName) (\s@UpdateNodegroupConfig' {} a -> s {clusterName = a} :: UpdateNodegroupConfig)

-- | The name of the managed node group to update.
updateNodegroupConfig_nodegroupName :: Lens.Lens' UpdateNodegroupConfig Prelude.Text
updateNodegroupConfig_nodegroupName = Lens.lens (\UpdateNodegroupConfig' {nodegroupName} -> nodegroupName) (\s@UpdateNodegroupConfig' {} a -> s {nodegroupName = a} :: UpdateNodegroupConfig)

instance Core.AWSRequest UpdateNodegroupConfig where
  type
    AWSResponse UpdateNodegroupConfig =
      UpdateNodegroupConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateNodegroupConfigResponse'
            Prelude.<$> (x Data..?> "update")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateNodegroupConfig where
  hashWithSalt _salt UpdateNodegroupConfig' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` scalingConfig
      `Prelude.hashWithSalt` taints
      `Prelude.hashWithSalt` updateConfig
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` nodegroupName

instance Prelude.NFData UpdateNodegroupConfig where
  rnf UpdateNodegroupConfig' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf scalingConfig
      `Prelude.seq` Prelude.rnf taints
      `Prelude.seq` Prelude.rnf updateConfig
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf nodegroupName

instance Data.ToHeaders UpdateNodegroupConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateNodegroupConfig where
  toJSON UpdateNodegroupConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("labels" Data..=) Prelude.<$> labels,
            ("scalingConfig" Data..=) Prelude.<$> scalingConfig,
            ("taints" Data..=) Prelude.<$> taints,
            ("updateConfig" Data..=) Prelude.<$> updateConfig
          ]
      )

instance Data.ToPath UpdateNodegroupConfig where
  toPath UpdateNodegroupConfig' {..} =
    Prelude.mconcat
      [ "/clusters/",
        Data.toBS clusterName,
        "/node-groups/",
        Data.toBS nodegroupName,
        "/update-config"
      ]

instance Data.ToQuery UpdateNodegroupConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNodegroupConfigResponse' smart constructor.
data UpdateNodegroupConfigResponse = UpdateNodegroupConfigResponse'
  { update :: Prelude.Maybe Update,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNodegroupConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'update', 'updateNodegroupConfigResponse_update' - Undocumented member.
--
-- 'httpStatus', 'updateNodegroupConfigResponse_httpStatus' - The response's http status code.
newUpdateNodegroupConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateNodegroupConfigResponse
newUpdateNodegroupConfigResponse pHttpStatus_ =
  UpdateNodegroupConfigResponse'
    { update =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateNodegroupConfigResponse_update :: Lens.Lens' UpdateNodegroupConfigResponse (Prelude.Maybe Update)
updateNodegroupConfigResponse_update = Lens.lens (\UpdateNodegroupConfigResponse' {update} -> update) (\s@UpdateNodegroupConfigResponse' {} a -> s {update = a} :: UpdateNodegroupConfigResponse)

-- | The response's http status code.
updateNodegroupConfigResponse_httpStatus :: Lens.Lens' UpdateNodegroupConfigResponse Prelude.Int
updateNodegroupConfigResponse_httpStatus = Lens.lens (\UpdateNodegroupConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateNodegroupConfigResponse' {} a -> s {httpStatus = a} :: UpdateNodegroupConfigResponse)

instance Prelude.NFData UpdateNodegroupConfigResponse where
  rnf UpdateNodegroupConfigResponse' {..} =
    Prelude.rnf update
      `Prelude.seq` Prelude.rnf httpStatus
