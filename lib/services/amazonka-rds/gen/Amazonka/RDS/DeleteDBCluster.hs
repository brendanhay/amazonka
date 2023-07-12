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
-- Module      : Amazonka.RDS.DeleteDBCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeleteDBCluster action deletes a previously provisioned DB cluster.
-- When you delete a DB cluster, all automated backups for that DB cluster
-- are deleted and can\'t be recovered. Manual DB cluster snapshots of the
-- specified DB cluster are not deleted.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What is Amazon Aurora?>
-- in the /Amazon Aurora User Guide/.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
-- in the /Amazon RDS User Guide/.
module Amazonka.RDS.DeleteDBCluster
  ( -- * Creating a Request
    DeleteDBCluster (..),
    newDeleteDBCluster,

    -- * Request Lenses
    deleteDBCluster_finalDBSnapshotIdentifier,
    deleteDBCluster_skipFinalSnapshot,
    deleteDBCluster_dbClusterIdentifier,

    -- * Destructuring the Response
    DeleteDBClusterResponse (..),
    newDeleteDBClusterResponse,

    -- * Response Lenses
    deleteDBClusterResponse_dbCluster,
    deleteDBClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteDBCluster' smart constructor.
data DeleteDBCluster = DeleteDBCluster'
  { -- | The DB cluster snapshot identifier of the new DB cluster snapshot
    -- created when @SkipFinalSnapshot@ is disabled.
    --
    -- Specifying this parameter and also skipping the creation of a final DB
    -- cluster snapshot with the @SkipFinalShapshot@ parameter results in an
    -- error.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    finalDBSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to skip the creation of a final DB
    -- cluster snapshot before the DB cluster is deleted. If skip is specified,
    -- no DB cluster snapshot is created. If skip isn\'t specified, a DB
    -- cluster snapshot is created before the DB cluster is deleted. By
    -- default, skip isn\'t specified, and the DB cluster snapshot is created.
    -- By default, this parameter is disabled.
    --
    -- You must specify a @FinalDBSnapshotIdentifier@ parameter if
    -- @SkipFinalSnapshot@ is disabled.
    skipFinalSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The DB cluster identifier for the DB cluster to be deleted. This
    -- parameter isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must match an existing DBClusterIdentifier.
    dbClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalDBSnapshotIdentifier', 'deleteDBCluster_finalDBSnapshotIdentifier' - The DB cluster snapshot identifier of the new DB cluster snapshot
-- created when @SkipFinalSnapshot@ is disabled.
--
-- Specifying this parameter and also skipping the creation of a final DB
-- cluster snapshot with the @SkipFinalShapshot@ parameter results in an
-- error.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- 'skipFinalSnapshot', 'deleteDBCluster_skipFinalSnapshot' - A value that indicates whether to skip the creation of a final DB
-- cluster snapshot before the DB cluster is deleted. If skip is specified,
-- no DB cluster snapshot is created. If skip isn\'t specified, a DB
-- cluster snapshot is created before the DB cluster is deleted. By
-- default, skip isn\'t specified, and the DB cluster snapshot is created.
-- By default, this parameter is disabled.
--
-- You must specify a @FinalDBSnapshotIdentifier@ parameter if
-- @SkipFinalSnapshot@ is disabled.
--
-- 'dbClusterIdentifier', 'deleteDBCluster_dbClusterIdentifier' - The DB cluster identifier for the DB cluster to be deleted. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match an existing DBClusterIdentifier.
newDeleteDBCluster ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  DeleteDBCluster
newDeleteDBCluster pDBClusterIdentifier_ =
  DeleteDBCluster'
    { finalDBSnapshotIdentifier =
        Prelude.Nothing,
      skipFinalSnapshot = Prelude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_
    }

-- | The DB cluster snapshot identifier of the new DB cluster snapshot
-- created when @SkipFinalSnapshot@ is disabled.
--
-- Specifying this parameter and also skipping the creation of a final DB
-- cluster snapshot with the @SkipFinalShapshot@ parameter results in an
-- error.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
deleteDBCluster_finalDBSnapshotIdentifier :: Lens.Lens' DeleteDBCluster (Prelude.Maybe Prelude.Text)
deleteDBCluster_finalDBSnapshotIdentifier = Lens.lens (\DeleteDBCluster' {finalDBSnapshotIdentifier} -> finalDBSnapshotIdentifier) (\s@DeleteDBCluster' {} a -> s {finalDBSnapshotIdentifier = a} :: DeleteDBCluster)

-- | A value that indicates whether to skip the creation of a final DB
-- cluster snapshot before the DB cluster is deleted. If skip is specified,
-- no DB cluster snapshot is created. If skip isn\'t specified, a DB
-- cluster snapshot is created before the DB cluster is deleted. By
-- default, skip isn\'t specified, and the DB cluster snapshot is created.
-- By default, this parameter is disabled.
--
-- You must specify a @FinalDBSnapshotIdentifier@ parameter if
-- @SkipFinalSnapshot@ is disabled.
deleteDBCluster_skipFinalSnapshot :: Lens.Lens' DeleteDBCluster (Prelude.Maybe Prelude.Bool)
deleteDBCluster_skipFinalSnapshot = Lens.lens (\DeleteDBCluster' {skipFinalSnapshot} -> skipFinalSnapshot) (\s@DeleteDBCluster' {} a -> s {skipFinalSnapshot = a} :: DeleteDBCluster)

-- | The DB cluster identifier for the DB cluster to be deleted. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match an existing DBClusterIdentifier.
deleteDBCluster_dbClusterIdentifier :: Lens.Lens' DeleteDBCluster Prelude.Text
deleteDBCluster_dbClusterIdentifier = Lens.lens (\DeleteDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DeleteDBCluster' {} a -> s {dbClusterIdentifier = a} :: DeleteDBCluster)

instance Core.AWSRequest DeleteDBCluster where
  type
    AWSResponse DeleteDBCluster =
      DeleteDBClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteDBClusterResult"
      ( \s h x ->
          DeleteDBClusterResponse'
            Prelude.<$> (x Data..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDBCluster where
  hashWithSalt _salt DeleteDBCluster' {..} =
    _salt
      `Prelude.hashWithSalt` finalDBSnapshotIdentifier
      `Prelude.hashWithSalt` skipFinalSnapshot
      `Prelude.hashWithSalt` dbClusterIdentifier

instance Prelude.NFData DeleteDBCluster where
  rnf DeleteDBCluster' {..} =
    Prelude.rnf finalDBSnapshotIdentifier
      `Prelude.seq` Prelude.rnf skipFinalSnapshot
      `Prelude.seq` Prelude.rnf dbClusterIdentifier

instance Data.ToHeaders DeleteDBCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDBCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDBCluster where
  toQuery DeleteDBCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteDBCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "FinalDBSnapshotIdentifier"
          Data.=: finalDBSnapshotIdentifier,
        "SkipFinalSnapshot" Data.=: skipFinalSnapshot,
        "DBClusterIdentifier" Data.=: dbClusterIdentifier
      ]

-- | /See:/ 'newDeleteDBClusterResponse' smart constructor.
data DeleteDBClusterResponse = DeleteDBClusterResponse'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'deleteDBClusterResponse_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'deleteDBClusterResponse_httpStatus' - The response's http status code.
newDeleteDBClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDBClusterResponse
newDeleteDBClusterResponse pHttpStatus_ =
  DeleteDBClusterResponse'
    { dbCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteDBClusterResponse_dbCluster :: Lens.Lens' DeleteDBClusterResponse (Prelude.Maybe DBCluster)
deleteDBClusterResponse_dbCluster = Lens.lens (\DeleteDBClusterResponse' {dbCluster} -> dbCluster) (\s@DeleteDBClusterResponse' {} a -> s {dbCluster = a} :: DeleteDBClusterResponse)

-- | The response's http status code.
deleteDBClusterResponse_httpStatus :: Lens.Lens' DeleteDBClusterResponse Prelude.Int
deleteDBClusterResponse_httpStatus = Lens.lens (\DeleteDBClusterResponse' {httpStatus} -> httpStatus) (\s@DeleteDBClusterResponse' {} a -> s {httpStatus = a} :: DeleteDBClusterResponse)

instance Prelude.NFData DeleteDBClusterResponse where
  rnf DeleteDBClusterResponse' {..} =
    Prelude.rnf dbCluster
      `Prelude.seq` Prelude.rnf httpStatus
