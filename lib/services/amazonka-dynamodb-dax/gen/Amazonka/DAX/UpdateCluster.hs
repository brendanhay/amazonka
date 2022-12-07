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
-- Module      : Amazonka.DAX.UpdateCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a DAX cluster. You can use this action to
-- change one or more cluster configuration parameters by specifying the
-- parameters and the new values.
module Amazonka.DAX.UpdateCluster
  ( -- * Creating a Request
    UpdateCluster (..),
    newUpdateCluster,

    -- * Request Lenses
    updateCluster_notificationTopicStatus,
    updateCluster_parameterGroupName,
    updateCluster_securityGroupIds,
    updateCluster_description,
    updateCluster_notificationTopicArn,
    updateCluster_preferredMaintenanceWindow,
    updateCluster_clusterName,

    -- * Destructuring the Response
    UpdateClusterResponse (..),
    newUpdateClusterResponse,

    -- * Response Lenses
    updateClusterResponse_cluster,
    updateClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DAX.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCluster' smart constructor.
data UpdateCluster = UpdateCluster'
  { -- | The current state of the topic. A value of “active” means that
    -- notifications will be sent to the topic. A value of “inactive” means
    -- that notifications will not be sent to the topic.
    notificationTopicStatus :: Prelude.Maybe Prelude.Text,
    -- | The name of a parameter group for this cluster.
    parameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | A list of user-specified security group IDs to be assigned to each node
    -- in the DAX cluster. If this parameter is not specified, DAX assigns the
    -- default VPC security group to each node.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A description of the changes being made to the cluster.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that identifies the topic.
    notificationTopicArn :: Prelude.Maybe Prelude.Text,
    -- | A range of time when maintenance of DAX cluster software will be
    -- performed. For example: @sun:01:00-sun:09:00@. Cluster maintenance
    -- normally takes less than 30 minutes, and is performed automatically
    -- within the maintenance window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The name of the DAX cluster to be modified.
    clusterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationTopicStatus', 'updateCluster_notificationTopicStatus' - The current state of the topic. A value of “active” means that
-- notifications will be sent to the topic. A value of “inactive” means
-- that notifications will not be sent to the topic.
--
-- 'parameterGroupName', 'updateCluster_parameterGroupName' - The name of a parameter group for this cluster.
--
-- 'securityGroupIds', 'updateCluster_securityGroupIds' - A list of user-specified security group IDs to be assigned to each node
-- in the DAX cluster. If this parameter is not specified, DAX assigns the
-- default VPC security group to each node.
--
-- 'description', 'updateCluster_description' - A description of the changes being made to the cluster.
--
-- 'notificationTopicArn', 'updateCluster_notificationTopicArn' - The Amazon Resource Name (ARN) that identifies the topic.
--
-- 'preferredMaintenanceWindow', 'updateCluster_preferredMaintenanceWindow' - A range of time when maintenance of DAX cluster software will be
-- performed. For example: @sun:01:00-sun:09:00@. Cluster maintenance
-- normally takes less than 30 minutes, and is performed automatically
-- within the maintenance window.
--
-- 'clusterName', 'updateCluster_clusterName' - The name of the DAX cluster to be modified.
newUpdateCluster ::
  -- | 'clusterName'
  Prelude.Text ->
  UpdateCluster
newUpdateCluster pClusterName_ =
  UpdateCluster'
    { notificationTopicStatus =
        Prelude.Nothing,
      parameterGroupName = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      description = Prelude.Nothing,
      notificationTopicArn = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      clusterName = pClusterName_
    }

-- | The current state of the topic. A value of “active” means that
-- notifications will be sent to the topic. A value of “inactive” means
-- that notifications will not be sent to the topic.
updateCluster_notificationTopicStatus :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_notificationTopicStatus = Lens.lens (\UpdateCluster' {notificationTopicStatus} -> notificationTopicStatus) (\s@UpdateCluster' {} a -> s {notificationTopicStatus = a} :: UpdateCluster)

-- | The name of a parameter group for this cluster.
updateCluster_parameterGroupName :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_parameterGroupName = Lens.lens (\UpdateCluster' {parameterGroupName} -> parameterGroupName) (\s@UpdateCluster' {} a -> s {parameterGroupName = a} :: UpdateCluster)

-- | A list of user-specified security group IDs to be assigned to each node
-- in the DAX cluster. If this parameter is not specified, DAX assigns the
-- default VPC security group to each node.
updateCluster_securityGroupIds :: Lens.Lens' UpdateCluster (Prelude.Maybe [Prelude.Text])
updateCluster_securityGroupIds = Lens.lens (\UpdateCluster' {securityGroupIds} -> securityGroupIds) (\s@UpdateCluster' {} a -> s {securityGroupIds = a} :: UpdateCluster) Prelude.. Lens.mapping Lens.coerced

-- | A description of the changes being made to the cluster.
updateCluster_description :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_description = Lens.lens (\UpdateCluster' {description} -> description) (\s@UpdateCluster' {} a -> s {description = a} :: UpdateCluster)

-- | The Amazon Resource Name (ARN) that identifies the topic.
updateCluster_notificationTopicArn :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_notificationTopicArn = Lens.lens (\UpdateCluster' {notificationTopicArn} -> notificationTopicArn) (\s@UpdateCluster' {} a -> s {notificationTopicArn = a} :: UpdateCluster)

-- | A range of time when maintenance of DAX cluster software will be
-- performed. For example: @sun:01:00-sun:09:00@. Cluster maintenance
-- normally takes less than 30 minutes, and is performed automatically
-- within the maintenance window.
updateCluster_preferredMaintenanceWindow :: Lens.Lens' UpdateCluster (Prelude.Maybe Prelude.Text)
updateCluster_preferredMaintenanceWindow = Lens.lens (\UpdateCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@UpdateCluster' {} a -> s {preferredMaintenanceWindow = a} :: UpdateCluster)

-- | The name of the DAX cluster to be modified.
updateCluster_clusterName :: Lens.Lens' UpdateCluster Prelude.Text
updateCluster_clusterName = Lens.lens (\UpdateCluster' {clusterName} -> clusterName) (\s@UpdateCluster' {} a -> s {clusterName = a} :: UpdateCluster)

instance Core.AWSRequest UpdateCluster where
  type
    AWSResponse UpdateCluster =
      UpdateClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateClusterResponse'
            Prelude.<$> (x Data..?> "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCluster where
  hashWithSalt _salt UpdateCluster' {..} =
    _salt
      `Prelude.hashWithSalt` notificationTopicStatus
      `Prelude.hashWithSalt` parameterGroupName
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` notificationTopicArn
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` clusterName

instance Prelude.NFData UpdateCluster where
  rnf UpdateCluster' {..} =
    Prelude.rnf notificationTopicStatus
      `Prelude.seq` Prelude.rnf parameterGroupName
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf notificationTopicArn
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf clusterName

instance Data.ToHeaders UpdateCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonDAXV3.UpdateCluster" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCluster where
  toJSON UpdateCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NotificationTopicStatus" Data..=)
              Prelude.<$> notificationTopicStatus,
            ("ParameterGroupName" Data..=)
              Prelude.<$> parameterGroupName,
            ("SecurityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("Description" Data..=) Prelude.<$> description,
            ("NotificationTopicArn" Data..=)
              Prelude.<$> notificationTopicArn,
            ("PreferredMaintenanceWindow" Data..=)
              Prelude.<$> preferredMaintenanceWindow,
            Prelude.Just ("ClusterName" Data..= clusterName)
          ]
      )

instance Data.ToPath UpdateCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateClusterResponse' smart constructor.
data UpdateClusterResponse = UpdateClusterResponse'
  { -- | A description of the DAX cluster, after it has been modified.
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'cluster', 'updateClusterResponse_cluster' - A description of the DAX cluster, after it has been modified.
--
-- 'httpStatus', 'updateClusterResponse_httpStatus' - The response's http status code.
newUpdateClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateClusterResponse
newUpdateClusterResponse pHttpStatus_ =
  UpdateClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the DAX cluster, after it has been modified.
updateClusterResponse_cluster :: Lens.Lens' UpdateClusterResponse (Prelude.Maybe Cluster)
updateClusterResponse_cluster = Lens.lens (\UpdateClusterResponse' {cluster} -> cluster) (\s@UpdateClusterResponse' {} a -> s {cluster = a} :: UpdateClusterResponse)

-- | The response's http status code.
updateClusterResponse_httpStatus :: Lens.Lens' UpdateClusterResponse Prelude.Int
updateClusterResponse_httpStatus = Lens.lens (\UpdateClusterResponse' {httpStatus} -> httpStatus) (\s@UpdateClusterResponse' {} a -> s {httpStatus = a} :: UpdateClusterResponse)

instance Prelude.NFData UpdateClusterResponse where
  rnf UpdateClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
