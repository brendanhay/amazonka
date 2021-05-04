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
-- Module      : Network.AWS.RDS.DeleteDBClusterSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB cluster snapshot. If the snapshot is being copied, the copy
-- operation is terminated.
--
-- The DB cluster snapshot must be in the @available@ state to be deleted.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.DeleteDBClusterSnapshot
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteDBClusterSnapshot where
  type
    Rs DeleteDBClusterSnapshot =
      DeleteDBClusterSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteDBClusterSnapshotResult"
      ( \s h x ->
          DeleteDBClusterSnapshotResponse'
            Prelude.<$> (x Prelude..@? "DBClusterSnapshot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDBClusterSnapshot

instance Prelude.NFData DeleteDBClusterSnapshot

instance Prelude.ToHeaders DeleteDBClusterSnapshot where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteDBClusterSnapshot where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteDBClusterSnapshot where
  toQuery DeleteDBClusterSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteDBClusterSnapshot" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterSnapshotIdentifier"
          Prelude.=: dbClusterSnapshotIdentifier
      ]

-- | /See:/ 'newDeleteDBClusterSnapshotResponse' smart constructor.
data DeleteDBClusterSnapshotResponse = DeleteDBClusterSnapshotResponse'
  { dbClusterSnapshot :: Prelude.Maybe DBClusterSnapshot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
