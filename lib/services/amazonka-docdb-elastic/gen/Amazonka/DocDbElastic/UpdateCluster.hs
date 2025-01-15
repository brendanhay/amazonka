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
-- Module      : Amazonka.DocDbElastic.UpdateCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a Elastic DocumentDB cluster. This includes updating
-- admin-username\/password, upgrading API version setting up a backup
-- window and maintenance window
module Amazonka.DocDbElastic.UpdateCluster
  ( -- * Creating a Request
    UpdateCluster (..),
    newUpdateCluster,

    -- * Request Lenses
    updateCluster_adminUserPassword,
    updateCluster_authType,
    updateCluster_clientToken,
    updateCluster_preferredMaintenanceWindow,
    updateCluster_shardCapacity,
    updateCluster_shardCount,
    updateCluster_subnetIds,
    updateCluster_vpcSecurityGroupIds,
    updateCluster_clusterArn,

    -- * Destructuring the Response
    UpdateClusterResponse (..),
    newUpdateClusterResponse,

    -- * Response Lenses
    updateClusterResponse_httpStatus,
    updateClusterResponse_cluster,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocDbElastic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCluster' smart constructor.
data UpdateCluster = UpdateCluster'
  { -- | The password for the Elastic DocumentDB cluster administrator. This
    -- password can contain any printable ASCII character except forward slash
    -- (\/), double quote (\"), or the \"at\" symbol (\@).
    --
    -- /Constraints/: Must contain from 8 to 100 characters.
    adminUserPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The authentication type for the Elastic DocumentDB cluster.
    authType :: Prelude.Maybe Auth,
    -- | The client token for the Elastic DocumentDB cluster.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- /Format/: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- /Default/: a 30-minute window selected at random from an 8-hour block of
    -- time for each Amazon Web Services Region, occurring on a random day of
    -- the week.
    --
    -- /Valid days/: Mon, Tue, Wed, Thu, Fri, Sat, Sun
    --
    -- /Constraints/: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The capacity of each shard in the Elastic DocumentDB cluster.
    shardCapacity :: Prelude.Maybe Prelude.Int,
    -- | The number of shards to create in the Elastic DocumentDB cluster.
    shardCount :: Prelude.Maybe Prelude.Int,
    -- | The number of shards to create in the Elastic DocumentDB cluster.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of EC2 VPC security groups to associate with the new Elastic
    -- DocumentDB cluster.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The arn of the Elastic DocumentDB cluster.
    clusterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminUserPassword', 'updateCluster_adminUserPassword' - The password for the Elastic DocumentDB cluster administrator. This
-- password can contain any printable ASCII character except forward slash
-- (\/), double quote (\"), or the \"at\" symbol (\@).
--
-- /Constraints/: Must contain from 8 to 100 characters.
--
-- 'authType', 'updateCluster_authType' - The authentication type for the Elastic DocumentDB cluster.
--
-- 'clientToken', 'updateCluster_clientToken' - The client token for the Elastic DocumentDB cluster.
--
-- 'preferredMaintenanceWindow', 'updateCluster_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- /Format/: @ddd:hh24:mi-ddd:hh24:mi@
--
-- /Default/: a 30-minute window selected at random from an 8-hour block of
-- time for each Amazon Web Services Region, occurring on a random day of
-- the week.
--
-- /Valid days/: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- /Constraints/: Minimum 30-minute window.
--
-- 'shardCapacity', 'updateCluster_shardCapacity' - The capacity of each shard in the Elastic DocumentDB cluster.
--
-- 'shardCount', 'updateCluster_shardCount' - The number of shards to create in the Elastic DocumentDB cluster.
--
-- 'subnetIds', 'updateCluster_subnetIds' - The number of shards to create in the Elastic DocumentDB cluster.
--
-- 'vpcSecurityGroupIds', 'updateCluster_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with the new Elastic
-- DocumentDB cluster.
--
-- 'clusterArn', 'updateCluster_clusterArn' - The arn of the Elastic DocumentDB cluster.
newUpdateCluster ::
  -- | 'clusterArn'
  Prelude.Text ->
  UpdateCluster
newUpdateCluster pClusterArn_ =
  UpdateCluster'
    { adminUserPassword = Prelude.Nothing,
      authType = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      shardCapacity = Prelude.Nothing,
      shardCount = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcSecurityGroupIds = Prelude.Nothing,
      clusterArn = pClusterArn_
    }

-- | The password for the Elastic DocumentDB cluster administrator. This
-- password can contain any printable ASCII character except forward slash
-- (\/), double quote (\"), or the \"at\" symbol (\@).
--
-- /Constraints/: Must contain from 8 to 100 characters.
updateCluster_adminUserPassword :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_adminUserPassword = Lens.lens (\UpdateCluster' {adminUserPassword} -> adminUserPassword) (\s@UpdateCluster' {} a -> s {adminUserPassword = a} :: UpdateCluster) Prelude.. Lens.mapping Data._Sensitive

-- | The authentication type for the Elastic DocumentDB cluster.
updateCluster_authType :: Lens.Lens' UpdateCluster (Prelude.Maybe Auth)
updateCluster_authType = Lens.lens (\UpdateCluster' {authType} -> authType) (\s@UpdateCluster' {} a -> s {authType = a} :: UpdateCluster)

-- | The client token for the Elastic DocumentDB cluster.
updateCluster_clientToken :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_clientToken = Lens.lens (\UpdateCluster' {clientToken} -> clientToken) (\s@UpdateCluster' {} a -> s {clientToken = a} :: UpdateCluster)

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- /Format/: @ddd:hh24:mi-ddd:hh24:mi@
--
-- /Default/: a 30-minute window selected at random from an 8-hour block of
-- time for each Amazon Web Services Region, occurring on a random day of
-- the week.
--
-- /Valid days/: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- /Constraints/: Minimum 30-minute window.
updateCluster_preferredMaintenanceWindow :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_preferredMaintenanceWindow = Lens.lens (\UpdateCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@UpdateCluster' {} a -> s {preferredMaintenanceWindow = a} :: UpdateCluster)

-- | The capacity of each shard in the Elastic DocumentDB cluster.
updateCluster_shardCapacity :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Int)
updateCluster_shardCapacity = Lens.lens (\UpdateCluster' {shardCapacity} -> shardCapacity) (\s@UpdateCluster' {} a -> s {shardCapacity = a} :: UpdateCluster)

-- | The number of shards to create in the Elastic DocumentDB cluster.
updateCluster_shardCount :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Int)
updateCluster_shardCount = Lens.lens (\UpdateCluster' {shardCount} -> shardCount) (\s@UpdateCluster' {} a -> s {shardCount = a} :: UpdateCluster)

-- | The number of shards to create in the Elastic DocumentDB cluster.
updateCluster_subnetIds :: Lens.Lens' UpdateCluster (Prelude.Maybe [Prelude.Text])
updateCluster_subnetIds = Lens.lens (\UpdateCluster' {subnetIds} -> subnetIds) (\s@UpdateCluster' {} a -> s {subnetIds = a} :: UpdateCluster) Prelude.. Lens.mapping Lens.coerced

-- | A list of EC2 VPC security groups to associate with the new Elastic
-- DocumentDB cluster.
updateCluster_vpcSecurityGroupIds :: Lens.Lens' UpdateCluster (Prelude.Maybe [Prelude.Text])
updateCluster_vpcSecurityGroupIds = Lens.lens (\UpdateCluster' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@UpdateCluster' {} a -> s {vpcSecurityGroupIds = a} :: UpdateCluster) Prelude.. Lens.mapping Lens.coerced

-- | The arn of the Elastic DocumentDB cluster.
updateCluster_clusterArn :: Lens.Lens' UpdateCluster Prelude.Text
updateCluster_clusterArn = Lens.lens (\UpdateCluster' {clusterArn} -> clusterArn) (\s@UpdateCluster' {} a -> s {clusterArn = a} :: UpdateCluster)

instance Core.AWSRequest UpdateCluster where
  type
    AWSResponse UpdateCluster =
      UpdateClusterResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateClusterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "cluster")
      )

instance Prelude.Hashable UpdateCluster where
  hashWithSalt _salt UpdateCluster' {..} =
    _salt
      `Prelude.hashWithSalt` adminUserPassword
      `Prelude.hashWithSalt` authType
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` shardCapacity
      `Prelude.hashWithSalt` shardCount
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` clusterArn

instance Prelude.NFData UpdateCluster where
  rnf UpdateCluster' {..} =
    Prelude.rnf adminUserPassword `Prelude.seq`
      Prelude.rnf authType `Prelude.seq`
        Prelude.rnf clientToken `Prelude.seq`
          Prelude.rnf preferredMaintenanceWindow `Prelude.seq`
            Prelude.rnf shardCapacity `Prelude.seq`
              Prelude.rnf shardCount `Prelude.seq`
                Prelude.rnf subnetIds `Prelude.seq`
                  Prelude.rnf vpcSecurityGroupIds `Prelude.seq`
                    Prelude.rnf clusterArn

instance Data.ToHeaders UpdateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCluster where
  toJSON UpdateCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("adminUserPassword" Data..=)
              Prelude.<$> adminUserPassword,
            ("authType" Data..=) Prelude.<$> authType,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("preferredMaintenanceWindow" Data..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("shardCapacity" Data..=) Prelude.<$> shardCapacity,
            ("shardCount" Data..=) Prelude.<$> shardCount,
            ("subnetIds" Data..=) Prelude.<$> subnetIds,
            ("vpcSecurityGroupIds" Data..=)
              Prelude.<$> vpcSecurityGroupIds
          ]
      )

instance Data.ToPath UpdateCluster where
  toPath UpdateCluster' {..} =
    Prelude.mconcat ["/cluster/", Data.toBS clusterArn]

instance Data.ToQuery UpdateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateClusterResponse' smart constructor.
data UpdateClusterResponse = UpdateClusterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns information about the updated Elastic DocumentDB cluster.
    cluster :: Cluster
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateClusterResponse_httpStatus' - The response's http status code.
--
-- 'cluster', 'updateClusterResponse_cluster' - Returns information about the updated Elastic DocumentDB cluster.
newUpdateClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'cluster'
  Cluster ->
  UpdateClusterResponse
newUpdateClusterResponse pHttpStatus_ pCluster_ =
  UpdateClusterResponse'
    { httpStatus = pHttpStatus_,
      cluster = pCluster_
    }

-- | The response's http status code.
updateClusterResponse_httpStatus :: Lens.Lens' UpdateClusterResponse Prelude.Int
updateClusterResponse_httpStatus = Lens.lens (\UpdateClusterResponse' {httpStatus} -> httpStatus) (\s@UpdateClusterResponse' {} a -> s {httpStatus = a} :: UpdateClusterResponse)

-- | Returns information about the updated Elastic DocumentDB cluster.
updateClusterResponse_cluster :: Lens.Lens' UpdateClusterResponse Cluster
updateClusterResponse_cluster = Lens.lens (\UpdateClusterResponse' {cluster} -> cluster) (\s@UpdateClusterResponse' {} a -> s {cluster = a} :: UpdateClusterResponse)

instance Prelude.NFData UpdateClusterResponse where
  rnf UpdateClusterResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf cluster
