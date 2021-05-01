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
-- Module      : Network.AWS.EKS.UpdateNodegroupConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.EKS.UpdateNodegroupConfig
  ( -- * Creating a Request
    UpdateNodegroupConfig (..),
    newUpdateNodegroupConfig,

    -- * Request Lenses
    updateNodegroupConfig_scalingConfig,
    updateNodegroupConfig_labels,
    updateNodegroupConfig_clientRequestToken,
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

import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateNodegroupConfig' smart constructor.
data UpdateNodegroupConfig = UpdateNodegroupConfig'
  { -- | The scaling configuration details for the Auto Scaling group after the
    -- update.
    scalingConfig :: Prelude.Maybe NodegroupScalingConfig,
    -- | The Kubernetes labels to be applied to the nodes in the node group after
    -- the update.
    labels :: Prelude.Maybe UpdateLabelsPayload,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon EKS cluster that the managed node group resides
    -- in.
    clusterName :: Prelude.Text,
    -- | The name of the managed node group to update.
    nodegroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateNodegroupConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scalingConfig', 'updateNodegroupConfig_scalingConfig' - The scaling configuration details for the Auto Scaling group after the
-- update.
--
-- 'labels', 'updateNodegroupConfig_labels' - The Kubernetes labels to be applied to the nodes in the node group after
-- the update.
--
-- 'clientRequestToken', 'updateNodegroupConfig_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
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
      { scalingConfig =
          Prelude.Nothing,
        labels = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        clusterName = pClusterName_,
        nodegroupName = pNodegroupName_
      }

-- | The scaling configuration details for the Auto Scaling group after the
-- update.
updateNodegroupConfig_scalingConfig :: Lens.Lens' UpdateNodegroupConfig (Prelude.Maybe NodegroupScalingConfig)
updateNodegroupConfig_scalingConfig = Lens.lens (\UpdateNodegroupConfig' {scalingConfig} -> scalingConfig) (\s@UpdateNodegroupConfig' {} a -> s {scalingConfig = a} :: UpdateNodegroupConfig)

-- | The Kubernetes labels to be applied to the nodes in the node group after
-- the update.
updateNodegroupConfig_labels :: Lens.Lens' UpdateNodegroupConfig (Prelude.Maybe UpdateLabelsPayload)
updateNodegroupConfig_labels = Lens.lens (\UpdateNodegroupConfig' {labels} -> labels) (\s@UpdateNodegroupConfig' {} a -> s {labels = a} :: UpdateNodegroupConfig)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
updateNodegroupConfig_clientRequestToken :: Lens.Lens' UpdateNodegroupConfig (Prelude.Maybe Prelude.Text)
updateNodegroupConfig_clientRequestToken = Lens.lens (\UpdateNodegroupConfig' {clientRequestToken} -> clientRequestToken) (\s@UpdateNodegroupConfig' {} a -> s {clientRequestToken = a} :: UpdateNodegroupConfig)

-- | The name of the Amazon EKS cluster that the managed node group resides
-- in.
updateNodegroupConfig_clusterName :: Lens.Lens' UpdateNodegroupConfig Prelude.Text
updateNodegroupConfig_clusterName = Lens.lens (\UpdateNodegroupConfig' {clusterName} -> clusterName) (\s@UpdateNodegroupConfig' {} a -> s {clusterName = a} :: UpdateNodegroupConfig)

-- | The name of the managed node group to update.
updateNodegroupConfig_nodegroupName :: Lens.Lens' UpdateNodegroupConfig Prelude.Text
updateNodegroupConfig_nodegroupName = Lens.lens (\UpdateNodegroupConfig' {nodegroupName} -> nodegroupName) (\s@UpdateNodegroupConfig' {} a -> s {nodegroupName = a} :: UpdateNodegroupConfig)

instance Prelude.AWSRequest UpdateNodegroupConfig where
  type
    Rs UpdateNodegroupConfig =
      UpdateNodegroupConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateNodegroupConfigResponse'
            Prelude.<$> (x Prelude..?> "update")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateNodegroupConfig

instance Prelude.NFData UpdateNodegroupConfig

instance Prelude.ToHeaders UpdateNodegroupConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateNodegroupConfig where
  toJSON UpdateNodegroupConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("scalingConfig" Prelude..=)
              Prelude.<$> scalingConfig,
            ("labels" Prelude..=) Prelude.<$> labels,
            ("clientRequestToken" Prelude..=)
              Prelude.<$> clientRequestToken
          ]
      )

instance Prelude.ToPath UpdateNodegroupConfig where
  toPath UpdateNodegroupConfig' {..} =
    Prelude.mconcat
      [ "/clusters/",
        Prelude.toBS clusterName,
        "/node-groups/",
        Prelude.toBS nodegroupName,
        "/update-config"
      ]

instance Prelude.ToQuery UpdateNodegroupConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNodegroupConfigResponse' smart constructor.
data UpdateNodegroupConfigResponse = UpdateNodegroupConfigResponse'
  { update :: Prelude.Maybe Update,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateNodegroupConfigResponse
