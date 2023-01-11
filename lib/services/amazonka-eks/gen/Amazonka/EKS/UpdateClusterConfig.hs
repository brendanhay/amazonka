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
-- Module      : Amazonka.EKS.UpdateClusterConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon EKS cluster configuration. Your cluster continues to
-- function during the update. The response output includes an update ID
-- that you can use to track the status of your cluster update with the
-- DescribeUpdate API operation.
--
-- You can use this API operation to enable or disable exporting the
-- Kubernetes control plane logs for your cluster to CloudWatch Logs. By
-- default, cluster control plane logs aren\'t exported to CloudWatch Logs.
-- For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/control-plane-logs.html Amazon EKS Cluster Control Plane Logs>
-- in the //Amazon EKS User Guide// .
--
-- CloudWatch Logs ingestion, archive storage, and data scanning rates
-- apply to exported control plane logs. For more information, see
-- <http://aws.amazon.com/cloudwatch/pricing/ CloudWatch Pricing>.
--
-- You can also use this API operation to enable or disable public and
-- private access to your cluster\'s Kubernetes API server endpoint. By
-- default, public access is enabled, and private access is disabled. For
-- more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/cluster-endpoint.html Amazon EKS cluster endpoint access control>
-- in the //Amazon EKS User Guide// .
--
-- You can\'t update the subnets or security group IDs for an existing
-- cluster.
--
-- Cluster updates are asynchronous, and they should finish within a few
-- minutes. During an update, the cluster status moves to @UPDATING@ (this
-- status transition is eventually consistent). When the update is complete
-- (either @Failed@ or @Successful@), the cluster status moves to @Active@.
module Amazonka.EKS.UpdateClusterConfig
  ( -- * Creating a Request
    UpdateClusterConfig (..),
    newUpdateClusterConfig,

    -- * Request Lenses
    updateClusterConfig_clientRequestToken,
    updateClusterConfig_logging,
    updateClusterConfig_resourcesVpcConfig,
    updateClusterConfig_name,

    -- * Destructuring the Response
    UpdateClusterConfigResponse (..),
    newUpdateClusterConfigResponse,

    -- * Response Lenses
    updateClusterConfigResponse_update,
    updateClusterConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateClusterConfig' smart constructor.
data UpdateClusterConfig = UpdateClusterConfig'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Enable or disable exporting the Kubernetes control plane logs for your
    -- cluster to CloudWatch Logs. By default, cluster control plane logs
    -- aren\'t exported to CloudWatch Logs. For more information, see
    -- <https://docs.aws.amazon.com/eks/latest/userguide/control-plane-logs.html Amazon EKS cluster control plane logs>
    -- in the //Amazon EKS User Guide// .
    --
    -- CloudWatch Logs ingestion, archive storage, and data scanning rates
    -- apply to exported control plane logs. For more information, see
    -- <http://aws.amazon.com/cloudwatch/pricing/ CloudWatch Pricing>.
    logging :: Prelude.Maybe Logging,
    resourcesVpcConfig :: Prelude.Maybe VpcConfigRequest,
    -- | The name of the Amazon EKS cluster to update.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClusterConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'updateClusterConfig_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'logging', 'updateClusterConfig_logging' - Enable or disable exporting the Kubernetes control plane logs for your
-- cluster to CloudWatch Logs. By default, cluster control plane logs
-- aren\'t exported to CloudWatch Logs. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/control-plane-logs.html Amazon EKS cluster control plane logs>
-- in the //Amazon EKS User Guide// .
--
-- CloudWatch Logs ingestion, archive storage, and data scanning rates
-- apply to exported control plane logs. For more information, see
-- <http://aws.amazon.com/cloudwatch/pricing/ CloudWatch Pricing>.
--
-- 'resourcesVpcConfig', 'updateClusterConfig_resourcesVpcConfig' - Undocumented member.
--
-- 'name', 'updateClusterConfig_name' - The name of the Amazon EKS cluster to update.
newUpdateClusterConfig ::
  -- | 'name'
  Prelude.Text ->
  UpdateClusterConfig
newUpdateClusterConfig pName_ =
  UpdateClusterConfig'
    { clientRequestToken =
        Prelude.Nothing,
      logging = Prelude.Nothing,
      resourcesVpcConfig = Prelude.Nothing,
      name = pName_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
updateClusterConfig_clientRequestToken :: Lens.Lens' UpdateClusterConfig (Prelude.Maybe Prelude.Text)
updateClusterConfig_clientRequestToken = Lens.lens (\UpdateClusterConfig' {clientRequestToken} -> clientRequestToken) (\s@UpdateClusterConfig' {} a -> s {clientRequestToken = a} :: UpdateClusterConfig)

-- | Enable or disable exporting the Kubernetes control plane logs for your
-- cluster to CloudWatch Logs. By default, cluster control plane logs
-- aren\'t exported to CloudWatch Logs. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/control-plane-logs.html Amazon EKS cluster control plane logs>
-- in the //Amazon EKS User Guide// .
--
-- CloudWatch Logs ingestion, archive storage, and data scanning rates
-- apply to exported control plane logs. For more information, see
-- <http://aws.amazon.com/cloudwatch/pricing/ CloudWatch Pricing>.
updateClusterConfig_logging :: Lens.Lens' UpdateClusterConfig (Prelude.Maybe Logging)
updateClusterConfig_logging = Lens.lens (\UpdateClusterConfig' {logging} -> logging) (\s@UpdateClusterConfig' {} a -> s {logging = a} :: UpdateClusterConfig)

-- | Undocumented member.
updateClusterConfig_resourcesVpcConfig :: Lens.Lens' UpdateClusterConfig (Prelude.Maybe VpcConfigRequest)
updateClusterConfig_resourcesVpcConfig = Lens.lens (\UpdateClusterConfig' {resourcesVpcConfig} -> resourcesVpcConfig) (\s@UpdateClusterConfig' {} a -> s {resourcesVpcConfig = a} :: UpdateClusterConfig)

-- | The name of the Amazon EKS cluster to update.
updateClusterConfig_name :: Lens.Lens' UpdateClusterConfig Prelude.Text
updateClusterConfig_name = Lens.lens (\UpdateClusterConfig' {name} -> name) (\s@UpdateClusterConfig' {} a -> s {name = a} :: UpdateClusterConfig)

instance Core.AWSRequest UpdateClusterConfig where
  type
    AWSResponse UpdateClusterConfig =
      UpdateClusterConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateClusterConfigResponse'
            Prelude.<$> (x Data..?> "update")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateClusterConfig where
  hashWithSalt _salt UpdateClusterConfig' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` logging
      `Prelude.hashWithSalt` resourcesVpcConfig
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateClusterConfig where
  rnf UpdateClusterConfig' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf logging
      `Prelude.seq` Prelude.rnf resourcesVpcConfig
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateClusterConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateClusterConfig where
  toJSON UpdateClusterConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("logging" Data..=) Prelude.<$> logging,
            ("resourcesVpcConfig" Data..=)
              Prelude.<$> resourcesVpcConfig
          ]
      )

instance Data.ToPath UpdateClusterConfig where
  toPath UpdateClusterConfig' {..} =
    Prelude.mconcat
      ["/clusters/", Data.toBS name, "/update-config"]

instance Data.ToQuery UpdateClusterConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateClusterConfigResponse' smart constructor.
data UpdateClusterConfigResponse = UpdateClusterConfigResponse'
  { update :: Prelude.Maybe Update,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClusterConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'update', 'updateClusterConfigResponse_update' - Undocumented member.
--
-- 'httpStatus', 'updateClusterConfigResponse_httpStatus' - The response's http status code.
newUpdateClusterConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateClusterConfigResponse
newUpdateClusterConfigResponse pHttpStatus_ =
  UpdateClusterConfigResponse'
    { update =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateClusterConfigResponse_update :: Lens.Lens' UpdateClusterConfigResponse (Prelude.Maybe Update)
updateClusterConfigResponse_update = Lens.lens (\UpdateClusterConfigResponse' {update} -> update) (\s@UpdateClusterConfigResponse' {} a -> s {update = a} :: UpdateClusterConfigResponse)

-- | The response's http status code.
updateClusterConfigResponse_httpStatus :: Lens.Lens' UpdateClusterConfigResponse Prelude.Int
updateClusterConfigResponse_httpStatus = Lens.lens (\UpdateClusterConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateClusterConfigResponse' {} a -> s {httpStatus = a} :: UpdateClusterConfigResponse)

instance Prelude.NFData UpdateClusterConfigResponse where
  rnf UpdateClusterConfigResponse' {..} =
    Prelude.rnf update
      `Prelude.seq` Prelude.rnf httpStatus
