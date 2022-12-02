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
-- Module      : Amazonka.Neptune.DeleteDBCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeleteDBCluster action deletes a previously provisioned DB cluster.
-- When you delete a DB cluster, all automated backups for that DB cluster
-- are deleted and can\'t be recovered. Manual DB cluster snapshots of the
-- specified DB cluster are not deleted.
--
-- Note that the DB Cluster cannot be deleted if deletion protection is
-- enabled. To delete it, you must first set its @DeletionProtection@ field
-- to @False@.
module Amazonka.Neptune.DeleteDBCluster
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
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDBCluster' smart constructor.
data DeleteDBCluster = DeleteDBCluster'
  { -- | The DB cluster snapshot identifier of the new DB cluster snapshot
    -- created when @SkipFinalSnapshot@ is set to @false@.
    --
    -- Specifying this parameter and also setting the @SkipFinalShapshot@
    -- parameter to true results in an error.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens
    finalDBSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Determines whether a final DB cluster snapshot is created before the DB
    -- cluster is deleted. If @true@ is specified, no DB cluster snapshot is
    -- created. If @false@ is specified, a DB cluster snapshot is created
    -- before the DB cluster is deleted.
    --
    -- You must specify a @FinalDBSnapshotIdentifier@ parameter if
    -- @SkipFinalSnapshot@ is @false@.
    --
    -- Default: @false@
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
-- created when @SkipFinalSnapshot@ is set to @false@.
--
-- Specifying this parameter and also setting the @SkipFinalShapshot@
-- parameter to true results in an error.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- 'skipFinalSnapshot', 'deleteDBCluster_skipFinalSnapshot' - Determines whether a final DB cluster snapshot is created before the DB
-- cluster is deleted. If @true@ is specified, no DB cluster snapshot is
-- created. If @false@ is specified, a DB cluster snapshot is created
-- before the DB cluster is deleted.
--
-- You must specify a @FinalDBSnapshotIdentifier@ parameter if
-- @SkipFinalSnapshot@ is @false@.
--
-- Default: @false@
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
-- created when @SkipFinalSnapshot@ is set to @false@.
--
-- Specifying this parameter and also setting the @SkipFinalShapshot@
-- parameter to true results in an error.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
deleteDBCluster_finalDBSnapshotIdentifier :: Lens.Lens' DeleteDBCluster (Prelude.Maybe Prelude.Text)
deleteDBCluster_finalDBSnapshotIdentifier = Lens.lens (\DeleteDBCluster' {finalDBSnapshotIdentifier} -> finalDBSnapshotIdentifier) (\s@DeleteDBCluster' {} a -> s {finalDBSnapshotIdentifier = a} :: DeleteDBCluster)

-- | Determines whether a final DB cluster snapshot is created before the DB
-- cluster is deleted. If @true@ is specified, no DB cluster snapshot is
-- created. If @false@ is specified, a DB cluster snapshot is created
-- before the DB cluster is deleted.
--
-- You must specify a @FinalDBSnapshotIdentifier@ parameter if
-- @SkipFinalSnapshot@ is @false@.
--
-- Default: @false@
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
