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
-- Module      : Network.AWS.RDS.DeleteDBInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeleteDBInstance action deletes a previously provisioned DB
-- instance. When you delete a DB instance, all automated backups for that
-- instance are deleted and can\'t be recovered. Manual DB snapshots of the
-- DB instance to be deleted by @DeleteDBInstance@ are not deleted.
--
-- If you request a final DB snapshot the status of the Amazon RDS DB
-- instance is @deleting@ until the DB snapshot is created. The API action
-- @DescribeDBInstance@ is used to monitor the status of this operation.
-- The action can\'t be canceled or reverted once submitted.
--
-- When a DB instance is in a failure state and has a status of @failed@,
-- @incompatible-restore@, or @incompatible-network@, you can only delete
-- it when you skip creation of the final snapshot with the
-- @SkipFinalSnapshot@ parameter.
--
-- If the specified DB instance is part of an Amazon Aurora DB cluster, you
-- can\'t delete the DB instance if both of the following conditions are
-- true:
--
-- -   The DB cluster is a read replica of another Amazon Aurora DB
--     cluster.
--
-- -   The DB instance is the only instance in the DB cluster.
--
-- To delete a DB instance in this case, first call the
-- @PromoteReadReplicaDBCluster@ API action to promote the DB cluster so
-- it\'s no longer a read replica. After the promotion completes, then call
-- the @DeleteDBInstance@ API action to delete the final instance in the DB
-- cluster.
module Network.AWS.RDS.DeleteDBInstance
  ( -- * Creating a Request
    DeleteDBInstance (..),
    newDeleteDBInstance,

    -- * Request Lenses
    deleteDBInstance_skipFinalSnapshot,
    deleteDBInstance_finalDBSnapshotIdentifier,
    deleteDBInstance_deleteAutomatedBackups,
    deleteDBInstance_dbInstanceIdentifier,

    -- * Destructuring the Response
    DeleteDBInstanceResponse (..),
    newDeleteDBInstanceResponse,

    -- * Response Lenses
    deleteDBInstanceResponse_dbInstance,
    deleteDBInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteDBInstance' smart constructor.
data DeleteDBInstance = DeleteDBInstance'
  { -- | A value that indicates whether to skip the creation of a final DB
    -- snapshot before the DB instance is deleted. If skip is specified, no DB
    -- snapshot is created. If skip isn\'t specified, a DB snapshot is created
    -- before the DB instance is deleted. By default, skip isn\'t specified,
    -- and the DB snapshot is created.
    --
    -- When a DB instance is in a failure state and has a status of \'failed\',
    -- \'incompatible-restore\', or \'incompatible-network\', it can only be
    -- deleted when skip is specified.
    --
    -- Specify skip when deleting a read replica.
    --
    -- The FinalDBSnapshotIdentifier parameter must be specified if skip isn\'t
    -- specified.
    skipFinalSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The @DBSnapshotIdentifier@ of the new @DBSnapshot@ created when the
    -- @SkipFinalSnapshot@ parameter is disabled.
    --
    -- Specifying this parameter and also specifying to skip final DB snapshot
    -- creation in SkipFinalShapshot results in an error.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 letters or numbers.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    --
    -- -   Can\'t be specified when deleting a read replica.
    finalDBSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to remove automated backups immediately
    -- after the DB instance is deleted. This parameter isn\'t case-sensitive.
    -- The default is to remove automated backups immediately after the DB
    -- instance is deleted.
    deleteAutomatedBackups :: Prelude.Maybe Prelude.Bool,
    -- | The DB instance identifier for the DB instance to be deleted. This
    -- parameter isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must match the name of an existing DB instance.
    dbInstanceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skipFinalSnapshot', 'deleteDBInstance_skipFinalSnapshot' - A value that indicates whether to skip the creation of a final DB
-- snapshot before the DB instance is deleted. If skip is specified, no DB
-- snapshot is created. If skip isn\'t specified, a DB snapshot is created
-- before the DB instance is deleted. By default, skip isn\'t specified,
-- and the DB snapshot is created.
--
-- When a DB instance is in a failure state and has a status of \'failed\',
-- \'incompatible-restore\', or \'incompatible-network\', it can only be
-- deleted when skip is specified.
--
-- Specify skip when deleting a read replica.
--
-- The FinalDBSnapshotIdentifier parameter must be specified if skip isn\'t
-- specified.
--
-- 'finalDBSnapshotIdentifier', 'deleteDBInstance_finalDBSnapshotIdentifier' - The @DBSnapshotIdentifier@ of the new @DBSnapshot@ created when the
-- @SkipFinalSnapshot@ parameter is disabled.
--
-- Specifying this parameter and also specifying to skip final DB snapshot
-- creation in SkipFinalShapshot results in an error.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- -   Can\'t be specified when deleting a read replica.
--
-- 'deleteAutomatedBackups', 'deleteDBInstance_deleteAutomatedBackups' - A value that indicates whether to remove automated backups immediately
-- after the DB instance is deleted. This parameter isn\'t case-sensitive.
-- The default is to remove automated backups immediately after the DB
-- instance is deleted.
--
-- 'dbInstanceIdentifier', 'deleteDBInstance_dbInstanceIdentifier' - The DB instance identifier for the DB instance to be deleted. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match the name of an existing DB instance.
newDeleteDBInstance ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  DeleteDBInstance
newDeleteDBInstance pDBInstanceIdentifier_ =
  DeleteDBInstance'
    { skipFinalSnapshot =
        Prelude.Nothing,
      finalDBSnapshotIdentifier = Prelude.Nothing,
      deleteAutomatedBackups = Prelude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | A value that indicates whether to skip the creation of a final DB
-- snapshot before the DB instance is deleted. If skip is specified, no DB
-- snapshot is created. If skip isn\'t specified, a DB snapshot is created
-- before the DB instance is deleted. By default, skip isn\'t specified,
-- and the DB snapshot is created.
--
-- When a DB instance is in a failure state and has a status of \'failed\',
-- \'incompatible-restore\', or \'incompatible-network\', it can only be
-- deleted when skip is specified.
--
-- Specify skip when deleting a read replica.
--
-- The FinalDBSnapshotIdentifier parameter must be specified if skip isn\'t
-- specified.
deleteDBInstance_skipFinalSnapshot :: Lens.Lens' DeleteDBInstance (Prelude.Maybe Prelude.Bool)
deleteDBInstance_skipFinalSnapshot = Lens.lens (\DeleteDBInstance' {skipFinalSnapshot} -> skipFinalSnapshot) (\s@DeleteDBInstance' {} a -> s {skipFinalSnapshot = a} :: DeleteDBInstance)

-- | The @DBSnapshotIdentifier@ of the new @DBSnapshot@ created when the
-- @SkipFinalSnapshot@ parameter is disabled.
--
-- Specifying this parameter and also specifying to skip final DB snapshot
-- creation in SkipFinalShapshot results in an error.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- -   Can\'t be specified when deleting a read replica.
deleteDBInstance_finalDBSnapshotIdentifier :: Lens.Lens' DeleteDBInstance (Prelude.Maybe Prelude.Text)
deleteDBInstance_finalDBSnapshotIdentifier = Lens.lens (\DeleteDBInstance' {finalDBSnapshotIdentifier} -> finalDBSnapshotIdentifier) (\s@DeleteDBInstance' {} a -> s {finalDBSnapshotIdentifier = a} :: DeleteDBInstance)

-- | A value that indicates whether to remove automated backups immediately
-- after the DB instance is deleted. This parameter isn\'t case-sensitive.
-- The default is to remove automated backups immediately after the DB
-- instance is deleted.
deleteDBInstance_deleteAutomatedBackups :: Lens.Lens' DeleteDBInstance (Prelude.Maybe Prelude.Bool)
deleteDBInstance_deleteAutomatedBackups = Lens.lens (\DeleteDBInstance' {deleteAutomatedBackups} -> deleteAutomatedBackups) (\s@DeleteDBInstance' {} a -> s {deleteAutomatedBackups = a} :: DeleteDBInstance)

-- | The DB instance identifier for the DB instance to be deleted. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must match the name of an existing DB instance.
deleteDBInstance_dbInstanceIdentifier :: Lens.Lens' DeleteDBInstance Prelude.Text
deleteDBInstance_dbInstanceIdentifier = Lens.lens (\DeleteDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DeleteDBInstance' {} a -> s {dbInstanceIdentifier = a} :: DeleteDBInstance)

instance Core.AWSRequest DeleteDBInstance where
  type
    AWSResponse DeleteDBInstance =
      DeleteDBInstanceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteDBInstanceResult"
      ( \s h x ->
          DeleteDBInstanceResponse'
            Prelude.<$> (x Core..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDBInstance

instance Prelude.NFData DeleteDBInstance

instance Core.ToHeaders DeleteDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteDBInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteDBInstance where
  toQuery DeleteDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteDBInstance" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "SkipFinalSnapshot" Core.=: skipFinalSnapshot,
        "FinalDBSnapshotIdentifier"
          Core.=: finalDBSnapshotIdentifier,
        "DeleteAutomatedBackups"
          Core.=: deleteAutomatedBackups,
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newDeleteDBInstanceResponse' smart constructor.
data DeleteDBInstanceResponse = DeleteDBInstanceResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'deleteDBInstanceResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'deleteDBInstanceResponse_httpStatus' - The response's http status code.
newDeleteDBInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDBInstanceResponse
newDeleteDBInstanceResponse pHttpStatus_ =
  DeleteDBInstanceResponse'
    { dbInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteDBInstanceResponse_dbInstance :: Lens.Lens' DeleteDBInstanceResponse (Prelude.Maybe DBInstance)
deleteDBInstanceResponse_dbInstance = Lens.lens (\DeleteDBInstanceResponse' {dbInstance} -> dbInstance) (\s@DeleteDBInstanceResponse' {} a -> s {dbInstance = a} :: DeleteDBInstanceResponse)

-- | The response's http status code.
deleteDBInstanceResponse_httpStatus :: Lens.Lens' DeleteDBInstanceResponse Prelude.Int
deleteDBInstanceResponse_httpStatus = Lens.lens (\DeleteDBInstanceResponse' {httpStatus} -> httpStatus) (\s@DeleteDBInstanceResponse' {} a -> s {httpStatus = a} :: DeleteDBInstanceResponse)

instance Prelude.NFData DeleteDBInstanceResponse
