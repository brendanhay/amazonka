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
-- Module      : Amazonka.Neptune.DeleteDBInstance
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
-- If you request a final DB snapshot the status of the Amazon Neptune DB
-- instance is @deleting@ until the DB snapshot is created. The API action
-- @DescribeDBInstance@ is used to monitor the status of this operation.
-- The action can\'t be canceled or reverted once submitted.
--
-- Note that when a DB instance is in a failure state and has a status of
-- @failed@, @incompatible-restore@, or @incompatible-network@, you can
-- only delete it when the @SkipFinalSnapshot@ parameter is set to @true@.
--
-- You can\'t delete a DB instance if it is the only instance in the DB
-- cluster, or if it has deletion protection enabled.
module Amazonka.Neptune.DeleteDBInstance
  ( -- * Creating a Request
    DeleteDBInstance (..),
    newDeleteDBInstance,

    -- * Request Lenses
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
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDBInstance' smart constructor.
data DeleteDBInstance = DeleteDBInstance'
  { -- | The DBSnapshotIdentifier of the new DBSnapshot created when
    -- SkipFinalSnapshot is set to @false@.
    --
    -- Specifying this parameter and also setting the SkipFinalShapshot
    -- parameter to true results in an error.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 letters or numbers.
    --
    -- -   First character must be a letter
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens
    --
    -- -   Cannot be specified when deleting a Read Replica.
    finalDBSnapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Determines whether a final DB snapshot is created before the DB instance
    -- is deleted. If @true@ is specified, no DBSnapshot is created. If @false@
    -- is specified, a DB snapshot is created before the DB instance is
    -- deleted.
    --
    -- Note that when a DB instance is in a failure state and has a status of
    -- \'failed\', \'incompatible-restore\', or \'incompatible-network\', it
    -- can only be deleted when the SkipFinalSnapshot parameter is set to
    -- \"true\".
    --
    -- Specify @true@ when deleting a Read Replica.
    --
    -- The FinalDBSnapshotIdentifier parameter must be specified if
    -- SkipFinalSnapshot is @false@.
    --
    -- Default: @false@
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
-- 'finalDBSnapshotIdentifier', 'deleteDBInstance_finalDBSnapshotIdentifier' - The DBSnapshotIdentifier of the new DBSnapshot created when
-- SkipFinalSnapshot is set to @false@.
--
-- Specifying this parameter and also setting the SkipFinalShapshot
-- parameter to true results in an error.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters or numbers.
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- -   Cannot be specified when deleting a Read Replica.
--
-- 'skipFinalSnapshot', 'deleteDBInstance_skipFinalSnapshot' - Determines whether a final DB snapshot is created before the DB instance
-- is deleted. If @true@ is specified, no DBSnapshot is created. If @false@
-- is specified, a DB snapshot is created before the DB instance is
-- deleted.
--
-- Note that when a DB instance is in a failure state and has a status of
-- \'failed\', \'incompatible-restore\', or \'incompatible-network\', it
-- can only be deleted when the SkipFinalSnapshot parameter is set to
-- \"true\".
--
-- Specify @true@ when deleting a Read Replica.
--
-- The FinalDBSnapshotIdentifier parameter must be specified if
-- SkipFinalSnapshot is @false@.
--
-- Default: @false@
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
    { finalDBSnapshotIdentifier =
        Prelude.Nothing,
      skipFinalSnapshot = Prelude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | The DBSnapshotIdentifier of the new DBSnapshot created when
-- SkipFinalSnapshot is set to @false@.
--
-- Specifying this parameter and also setting the SkipFinalShapshot
-- parameter to true results in an error.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters or numbers.
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- -   Cannot be specified when deleting a Read Replica.
deleteDBInstance_finalDBSnapshotIdentifier :: Lens.Lens' DeleteDBInstance (Prelude.Maybe Prelude.Text)
deleteDBInstance_finalDBSnapshotIdentifier = Lens.lens (\DeleteDBInstance' {finalDBSnapshotIdentifier} -> finalDBSnapshotIdentifier) (\s@DeleteDBInstance' {} a -> s {finalDBSnapshotIdentifier = a} :: DeleteDBInstance)

-- | Determines whether a final DB snapshot is created before the DB instance
-- is deleted. If @true@ is specified, no DBSnapshot is created. If @false@
-- is specified, a DB snapshot is created before the DB instance is
-- deleted.
--
-- Note that when a DB instance is in a failure state and has a status of
-- \'failed\', \'incompatible-restore\', or \'incompatible-network\', it
-- can only be deleted when the SkipFinalSnapshot parameter is set to
-- \"true\".
--
-- Specify @true@ when deleting a Read Replica.
--
-- The FinalDBSnapshotIdentifier parameter must be specified if
-- SkipFinalSnapshot is @false@.
--
-- Default: @false@
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
            Prelude.<$> (x Core..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDBInstance where
  hashWithSalt _salt DeleteDBInstance' {..} =
    _salt
      `Prelude.hashWithSalt` finalDBSnapshotIdentifier
      `Prelude.hashWithSalt` skipFinalSnapshot
      `Prelude.hashWithSalt` dbInstanceIdentifier

instance Prelude.NFData DeleteDBInstance where
  rnf DeleteDBInstance' {..} =
    Prelude.rnf finalDBSnapshotIdentifier
      `Prelude.seq` Prelude.rnf skipFinalSnapshot
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier

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
        "FinalDBSnapshotIdentifier"
          Core.=: finalDBSnapshotIdentifier,
        "SkipFinalSnapshot" Core.=: skipFinalSnapshot,
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

instance Prelude.NFData DeleteDBInstanceResponse where
  rnf DeleteDBInstanceResponse' {..} =
    Prelude.rnf dbInstance
      `Prelude.seq` Prelude.rnf httpStatus
