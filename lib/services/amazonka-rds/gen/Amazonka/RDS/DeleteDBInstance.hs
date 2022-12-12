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
-- Module      : Amazonka.RDS.DeleteDBInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.RDS.DeleteDBInstance
  ( -- * Creating a Request
    DeleteDBInstance (..),
    newDeleteDBInstance,

    -- * Request Lenses
    deleteDBInstance_deleteAutomatedBackups,
    deleteDBInstance_finalDBSnapshotIdentifier,
    deleteDBInstance_skipFinalSnapshot,
    deleteDBInstance_dbInstanceIdentifier,

    -- * Destructuring the Response
    DeleteDBInstanceResponse (..),
    newDeleteDBInstanceResponse,

    -- * Response Lenses
    deleteDBInstanceResponse_dbInstance,
    deleteDBInstanceResponse_httpStatus,
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
-- /See:/ 'newDeleteDBInstance' smart constructor.
data DeleteDBInstance = DeleteDBInstance'
  { -- | A value that indicates whether to remove automated backups immediately
    -- after the DB instance is deleted. This parameter isn\'t case-sensitive.
    -- The default is to remove automated backups immediately after the DB
    -- instance is deleted.
    deleteAutomatedBackups :: Prelude.Maybe Prelude.Bool,
    -- | The @DBSnapshotIdentifier@ of the new @DBSnapshot@ created when the
    -- @SkipFinalSnapshot@ parameter is disabled.
    --
    -- If you enable this parameter and also enable SkipFinalShapshot, the
    -- command results in an error.
    --
    -- This setting doesn\'t apply to RDS Custom.
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
    -- | A value that indicates whether to skip the creation of a final DB
    -- snapshot before deleting the instance. If you enable this parameter, RDS
    -- doesn\'t create a DB snapshot. If you don\'t enable this parameter, RDS
    -- creates a DB snapshot before the DB instance is deleted. By default,
    -- skip isn\'t enabled, and the DB snapshot is created.
    --
    -- If you don\'t enable this parameter, you must specify the
    -- @FinalDBSnapshotIdentifier@ parameter.
    --
    -- When a DB instance is in a failure state and has a status of @failed@,
    -- @incompatible-restore@, or @incompatible-network@, RDS can delete the
    -- instance only if you enable this parameter.
    --
    -- If you delete a read replica or an RDS Custom instance, you must enable
    -- this setting.
    --
    -- This setting is required for RDS Custom.
    skipFinalSnapshot :: Prelude.Maybe Prelude.Bool,
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
-- 'deleteAutomatedBackups', 'deleteDBInstance_deleteAutomatedBackups' - A value that indicates whether to remove automated backups immediately
-- after the DB instance is deleted. This parameter isn\'t case-sensitive.
-- The default is to remove automated backups immediately after the DB
-- instance is deleted.
--
-- 'finalDBSnapshotIdentifier', 'deleteDBInstance_finalDBSnapshotIdentifier' - The @DBSnapshotIdentifier@ of the new @DBSnapshot@ created when the
-- @SkipFinalSnapshot@ parameter is disabled.
--
-- If you enable this parameter and also enable SkipFinalShapshot, the
-- command results in an error.
--
-- This setting doesn\'t apply to RDS Custom.
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
-- 'skipFinalSnapshot', 'deleteDBInstance_skipFinalSnapshot' - A value that indicates whether to skip the creation of a final DB
-- snapshot before deleting the instance. If you enable this parameter, RDS
-- doesn\'t create a DB snapshot. If you don\'t enable this parameter, RDS
-- creates a DB snapshot before the DB instance is deleted. By default,
-- skip isn\'t enabled, and the DB snapshot is created.
--
-- If you don\'t enable this parameter, you must specify the
-- @FinalDBSnapshotIdentifier@ parameter.
--
-- When a DB instance is in a failure state and has a status of @failed@,
-- @incompatible-restore@, or @incompatible-network@, RDS can delete the
-- instance only if you enable this parameter.
--
-- If you delete a read replica or an RDS Custom instance, you must enable
-- this setting.
--
-- This setting is required for RDS Custom.
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
    { deleteAutomatedBackups =
        Prelude.Nothing,
      finalDBSnapshotIdentifier = Prelude.Nothing,
      skipFinalSnapshot = Prelude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | A value that indicates whether to remove automated backups immediately
-- after the DB instance is deleted. This parameter isn\'t case-sensitive.
-- The default is to remove automated backups immediately after the DB
-- instance is deleted.
deleteDBInstance_deleteAutomatedBackups :: Lens.Lens' DeleteDBInstance (Prelude.Maybe Prelude.Bool)
deleteDBInstance_deleteAutomatedBackups = Lens.lens (\DeleteDBInstance' {deleteAutomatedBackups} -> deleteAutomatedBackups) (\s@DeleteDBInstance' {} a -> s {deleteAutomatedBackups = a} :: DeleteDBInstance)

-- | The @DBSnapshotIdentifier@ of the new @DBSnapshot@ created when the
-- @SkipFinalSnapshot@ parameter is disabled.
--
-- If you enable this parameter and also enable SkipFinalShapshot, the
-- command results in an error.
--
-- This setting doesn\'t apply to RDS Custom.
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

-- | A value that indicates whether to skip the creation of a final DB
-- snapshot before deleting the instance. If you enable this parameter, RDS
-- doesn\'t create a DB snapshot. If you don\'t enable this parameter, RDS
-- creates a DB snapshot before the DB instance is deleted. By default,
-- skip isn\'t enabled, and the DB snapshot is created.
--
-- If you don\'t enable this parameter, you must specify the
-- @FinalDBSnapshotIdentifier@ parameter.
--
-- When a DB instance is in a failure state and has a status of @failed@,
-- @incompatible-restore@, or @incompatible-network@, RDS can delete the
-- instance only if you enable this parameter.
--
-- If you delete a read replica or an RDS Custom instance, you must enable
-- this setting.
--
-- This setting is required for RDS Custom.
deleteDBInstance_skipFinalSnapshot :: Lens.Lens' DeleteDBInstance (Prelude.Maybe Prelude.Bool)
deleteDBInstance_skipFinalSnapshot = Lens.lens (\DeleteDBInstance' {skipFinalSnapshot} -> skipFinalSnapshot) (\s@DeleteDBInstance' {} a -> s {skipFinalSnapshot = a} :: DeleteDBInstance)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteDBInstanceResult"
      ( \s h x ->
          DeleteDBInstanceResponse'
            Prelude.<$> (x Data..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDBInstance where
  hashWithSalt _salt DeleteDBInstance' {..} =
    _salt `Prelude.hashWithSalt` deleteAutomatedBackups
      `Prelude.hashWithSalt` finalDBSnapshotIdentifier
      `Prelude.hashWithSalt` skipFinalSnapshot
      `Prelude.hashWithSalt` dbInstanceIdentifier

instance Prelude.NFData DeleteDBInstance where
  rnf DeleteDBInstance' {..} =
    Prelude.rnf deleteAutomatedBackups
      `Prelude.seq` Prelude.rnf finalDBSnapshotIdentifier
      `Prelude.seq` Prelude.rnf skipFinalSnapshot
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier

instance Data.ToHeaders DeleteDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDBInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDBInstance where
  toQuery DeleteDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteDBInstance" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DeleteAutomatedBackups"
          Data.=: deleteAutomatedBackups,
        "FinalDBSnapshotIdentifier"
          Data.=: finalDBSnapshotIdentifier,
        "SkipFinalSnapshot" Data.=: skipFinalSnapshot,
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier
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

instance Prelude.NFData DeleteDBInstanceResponse where
  rnf DeleteDBInstanceResponse' {..} =
    Prelude.rnf dbInstance
      `Prelude.seq` Prelude.rnf httpStatus
