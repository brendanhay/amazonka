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
-- Module      : Amazonka.RDS.DeleteDBClusterSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB cluster snapshot. If the snapshot is being copied, the copy
-- operation is terminated.
--
-- The DB cluster snapshot must be in the @available@ state to be deleted.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What is Amazon Aurora?>
-- in the /Amazon Aurora User Guide/.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
-- in the /Amazon RDS User Guide/.
module Amazonka.RDS.DeleteDBClusterSnapshot
  ( -- * Creating a Request
    DeleteDBClusterSnapshot (..),
    newDeleteDBClusterSnapshot,

    -- * Request Lenses
    deleteDBClusterSnapshot_dbClusterSnapshotIdentifier,

    -- * Destructuring the Response
    DeleteDBClusterSnapshotResponse (..),
    newDeleteDBClusterSnapshotResponse,

    -- * Response Lenses
    deleteDBClusterSnapshotResponse_dbClusterSnapshot,
    deleteDBClusterSnapshotResponse_httpStatus,
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
-- /See:/ 'newDeleteDBClusterSnapshot' smart constructor.
data DeleteDBClusterSnapshot = DeleteDBClusterSnapshot'
  { -- | The identifier of the DB cluster snapshot to delete.
    --
    -- Constraints: Must be the name of an existing DB cluster snapshot in the
    -- @available@ state.
    dbClusterSnapshotIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterSnapshotIdentifier', 'deleteDBClusterSnapshot_dbClusterSnapshotIdentifier' - The identifier of the DB cluster snapshot to delete.
--
-- Constraints: Must be the name of an existing DB cluster snapshot in the
-- @available@ state.
newDeleteDBClusterSnapshot ::
  -- | 'dbClusterSnapshotIdentifier'
  Prelude.Text ->
  DeleteDBClusterSnapshot
newDeleteDBClusterSnapshot
  pDBClusterSnapshotIdentifier_ =
    DeleteDBClusterSnapshot'
      { dbClusterSnapshotIdentifier =
          pDBClusterSnapshotIdentifier_
      }

-- | The identifier of the DB cluster snapshot to delete.
--
-- Constraints: Must be the name of an existing DB cluster snapshot in the
-- @available@ state.
deleteDBClusterSnapshot_dbClusterSnapshotIdentifier :: Lens.Lens' DeleteDBClusterSnapshot Prelude.Text
deleteDBClusterSnapshot_dbClusterSnapshotIdentifier = Lens.lens (\DeleteDBClusterSnapshot' {dbClusterSnapshotIdentifier} -> dbClusterSnapshotIdentifier) (\s@DeleteDBClusterSnapshot' {} a -> s {dbClusterSnapshotIdentifier = a} :: DeleteDBClusterSnapshot)

instance Core.AWSRequest DeleteDBClusterSnapshot where
  type
    AWSResponse DeleteDBClusterSnapshot =
      DeleteDBClusterSnapshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteDBClusterSnapshotResult"
      ( \s h x ->
          DeleteDBClusterSnapshotResponse'
            Prelude.<$> (x Data..@? "DBClusterSnapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDBClusterSnapshot where
  hashWithSalt _salt DeleteDBClusterSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` dbClusterSnapshotIdentifier

instance Prelude.NFData DeleteDBClusterSnapshot where
  rnf DeleteDBClusterSnapshot' {..} =
    Prelude.rnf dbClusterSnapshotIdentifier

instance Data.ToHeaders DeleteDBClusterSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDBClusterSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDBClusterSnapshot where
  toQuery DeleteDBClusterSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteDBClusterSnapshot" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterSnapshotIdentifier"
          Data.=: dbClusterSnapshotIdentifier
      ]

-- | /See:/ 'newDeleteDBClusterSnapshotResponse' smart constructor.
data DeleteDBClusterSnapshotResponse = DeleteDBClusterSnapshotResponse'
  { dbClusterSnapshot :: Prelude.Maybe DBClusterSnapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBClusterSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterSnapshot', 'deleteDBClusterSnapshotResponse_dbClusterSnapshot' - Undocumented member.
--
-- 'httpStatus', 'deleteDBClusterSnapshotResponse_httpStatus' - The response's http status code.
newDeleteDBClusterSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDBClusterSnapshotResponse
newDeleteDBClusterSnapshotResponse pHttpStatus_ =
  DeleteDBClusterSnapshotResponse'
    { dbClusterSnapshot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteDBClusterSnapshotResponse_dbClusterSnapshot :: Lens.Lens' DeleteDBClusterSnapshotResponse (Prelude.Maybe DBClusterSnapshot)
deleteDBClusterSnapshotResponse_dbClusterSnapshot = Lens.lens (\DeleteDBClusterSnapshotResponse' {dbClusterSnapshot} -> dbClusterSnapshot) (\s@DeleteDBClusterSnapshotResponse' {} a -> s {dbClusterSnapshot = a} :: DeleteDBClusterSnapshotResponse)

-- | The response's http status code.
deleteDBClusterSnapshotResponse_httpStatus :: Lens.Lens' DeleteDBClusterSnapshotResponse Prelude.Int
deleteDBClusterSnapshotResponse_httpStatus = Lens.lens (\DeleteDBClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeleteDBClusterSnapshotResponse' {} a -> s {httpStatus = a} :: DeleteDBClusterSnapshotResponse)

instance
  Prelude.NFData
    DeleteDBClusterSnapshotResponse
  where
  rnf DeleteDBClusterSnapshotResponse' {..} =
    Prelude.rnf dbClusterSnapshot
      `Prelude.seq` Prelude.rnf httpStatus
