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
-- Module      : Amazonka.Redshift.DeleteCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously provisioned cluster without its final snapshot
-- being created. A successful response from the web service indicates that
-- the request was received correctly. Use DescribeClusters to monitor the
-- status of the deletion. The delete operation cannot be canceled or
-- reverted once submitted. For more information about managing clusters,
-- go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- If you want to shut down the cluster and retain it for future use, set
-- /SkipFinalClusterSnapshot/ to @false@ and specify a name for
-- /FinalClusterSnapshotIdentifier/. You can later restore this snapshot to
-- resume using the cluster. If a final cluster snapshot is requested, the
-- status of the cluster will be \"final-snapshot\" while the snapshot is
-- being taken, then it\'s \"deleting\" once Amazon Redshift begins
-- deleting the cluster.
--
-- For more information about managing clusters, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters>
-- in the /Amazon Redshift Cluster Management Guide/.
module Amazonka.Redshift.DeleteCluster
  ( -- * Creating a Request
    DeleteCluster (..),
    newDeleteCluster,

    -- * Request Lenses
    deleteCluster_finalClusterSnapshotIdentifier,
    deleteCluster_finalClusterSnapshotRetentionPeriod,
    deleteCluster_skipFinalClusterSnapshot,
    deleteCluster_clusterIdentifier,

    -- * Destructuring the Response
    DeleteClusterResponse (..),
    newDeleteClusterResponse,

    -- * Response Lenses
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteCluster' smart constructor.
data DeleteCluster = DeleteCluster'
  { -- | The identifier of the final snapshot that is to be created immediately
    -- before deleting the cluster. If this parameter is provided,
    -- /SkipFinalClusterSnapshot/ must be @false@.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 alphanumeric characters.
    --
    -- -   First character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    finalClusterSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The number of days that a manual snapshot is retained. If the value is
    -- -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    --
    -- The default value is -1.
    finalClusterSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Determines whether a final snapshot of the cluster is created before
    -- Amazon Redshift deletes the cluster. If @true@, a final cluster snapshot
    -- is not created. If @false@, a final cluster snapshot is created before
    -- the cluster is deleted.
    --
    -- The /FinalClusterSnapshotIdentifier/ parameter must be specified if
    -- /SkipFinalClusterSnapshot/ is @false@.
    --
    -- Default: @false@
    skipFinalClusterSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the cluster to be deleted.
    --
    -- Constraints:
    --
    -- -   Must contain lowercase characters.
    --
    -- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalClusterSnapshotIdentifier', 'deleteCluster_finalClusterSnapshotIdentifier' - The identifier of the final snapshot that is to be created immediately
-- before deleting the cluster. If this parameter is provided,
-- /SkipFinalClusterSnapshot/ must be @false@.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- 'finalClusterSnapshotRetentionPeriod', 'deleteCluster_finalClusterSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- The default value is -1.
--
-- 'skipFinalClusterSnapshot', 'deleteCluster_skipFinalClusterSnapshot' - Determines whether a final snapshot of the cluster is created before
-- Amazon Redshift deletes the cluster. If @true@, a final cluster snapshot
-- is not created. If @false@, a final cluster snapshot is created before
-- the cluster is deleted.
--
-- The /FinalClusterSnapshotIdentifier/ parameter must be specified if
-- /SkipFinalClusterSnapshot/ is @false@.
--
-- Default: @false@
--
-- 'clusterIdentifier', 'deleteCluster_clusterIdentifier' - The identifier of the cluster to be deleted.
--
-- Constraints:
--
-- -   Must contain lowercase characters.
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
newDeleteCluster ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  DeleteCluster
newDeleteCluster pClusterIdentifier_ =
  DeleteCluster'
    { finalClusterSnapshotIdentifier =
        Prelude.Nothing,
      finalClusterSnapshotRetentionPeriod =
        Prelude.Nothing,
      skipFinalClusterSnapshot = Prelude.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | The identifier of the final snapshot that is to be created immediately
-- before deleting the cluster. If this parameter is provided,
-- /SkipFinalClusterSnapshot/ must be @false@.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
deleteCluster_finalClusterSnapshotIdentifier :: Lens.Lens' DeleteCluster (Prelude.Maybe Prelude.Text)
deleteCluster_finalClusterSnapshotIdentifier = Lens.lens (\DeleteCluster' {finalClusterSnapshotIdentifier} -> finalClusterSnapshotIdentifier) (\s@DeleteCluster' {} a -> s {finalClusterSnapshotIdentifier = a} :: DeleteCluster)

-- | The number of days that a manual snapshot is retained. If the value is
-- -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- The default value is -1.
deleteCluster_finalClusterSnapshotRetentionPeriod :: Lens.Lens' DeleteCluster (Prelude.Maybe Prelude.Int)
deleteCluster_finalClusterSnapshotRetentionPeriod = Lens.lens (\DeleteCluster' {finalClusterSnapshotRetentionPeriod} -> finalClusterSnapshotRetentionPeriod) (\s@DeleteCluster' {} a -> s {finalClusterSnapshotRetentionPeriod = a} :: DeleteCluster)

-- | Determines whether a final snapshot of the cluster is created before
-- Amazon Redshift deletes the cluster. If @true@, a final cluster snapshot
-- is not created. If @false@, a final cluster snapshot is created before
-- the cluster is deleted.
--
-- The /FinalClusterSnapshotIdentifier/ parameter must be specified if
-- /SkipFinalClusterSnapshot/ is @false@.
--
-- Default: @false@
deleteCluster_skipFinalClusterSnapshot :: Lens.Lens' DeleteCluster (Prelude.Maybe Prelude.Bool)
deleteCluster_skipFinalClusterSnapshot = Lens.lens (\DeleteCluster' {skipFinalClusterSnapshot} -> skipFinalClusterSnapshot) (\s@DeleteCluster' {} a -> s {skipFinalClusterSnapshot = a} :: DeleteCluster)

-- | The identifier of the cluster to be deleted.
--
-- Constraints:
--
-- -   Must contain lowercase characters.
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
deleteCluster_clusterIdentifier :: Lens.Lens' DeleteCluster Prelude.Text
deleteCluster_clusterIdentifier = Lens.lens (\DeleteCluster' {clusterIdentifier} -> clusterIdentifier) (\s@DeleteCluster' {} a -> s {clusterIdentifier = a} :: DeleteCluster)

instance Core.AWSRequest DeleteCluster where
  type
    AWSResponse DeleteCluster =
      DeleteClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteClusterResult"
      ( \s h x ->
          DeleteClusterResponse'
            Prelude.<$> (x Data..@? "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCluster where
  hashWithSalt _salt DeleteCluster' {..} =
    _salt
      `Prelude.hashWithSalt` finalClusterSnapshotIdentifier
      `Prelude.hashWithSalt` finalClusterSnapshotRetentionPeriod
      `Prelude.hashWithSalt` skipFinalClusterSnapshot
      `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData DeleteCluster where
  rnf DeleteCluster' {..} =
    Prelude.rnf finalClusterSnapshotIdentifier
      `Prelude.seq` Prelude.rnf finalClusterSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf skipFinalClusterSnapshot
      `Prelude.seq` Prelude.rnf clusterIdentifier

instance Data.ToHeaders DeleteCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCluster where
  toQuery DeleteCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "FinalClusterSnapshotIdentifier"
          Data.=: finalClusterSnapshotIdentifier,
        "FinalClusterSnapshotRetentionPeriod"
          Data.=: finalClusterSnapshotRetentionPeriod,
        "SkipFinalClusterSnapshot"
          Data.=: skipFinalClusterSnapshot,
        "ClusterIdentifier" Data.=: clusterIdentifier
      ]

-- | /See:/ 'newDeleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
  { cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'deleteClusterResponse_cluster' - Undocumented member.
--
-- 'httpStatus', 'deleteClusterResponse_httpStatus' - The response's http status code.
newDeleteClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteClusterResponse
newDeleteClusterResponse pHttpStatus_ =
  DeleteClusterResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteClusterResponse_cluster :: Lens.Lens' DeleteClusterResponse (Prelude.Maybe Cluster)
deleteClusterResponse_cluster = Lens.lens (\DeleteClusterResponse' {cluster} -> cluster) (\s@DeleteClusterResponse' {} a -> s {cluster = a} :: DeleteClusterResponse)

-- | The response's http status code.
deleteClusterResponse_httpStatus :: Lens.Lens' DeleteClusterResponse Prelude.Int
deleteClusterResponse_httpStatus = Lens.lens (\DeleteClusterResponse' {httpStatus} -> httpStatus) (\s@DeleteClusterResponse' {} a -> s {httpStatus = a} :: DeleteClusterResponse)

instance Prelude.NFData DeleteClusterResponse where
  rnf DeleteClusterResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
