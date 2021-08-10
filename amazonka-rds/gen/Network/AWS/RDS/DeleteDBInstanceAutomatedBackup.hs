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
-- Module      : Network.AWS.RDS.DeleteDBInstanceAutomatedBackup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes automated backups using the @DbiResourceId@ value of the source
-- DB instance or the Amazon Resource Name (ARN) of the automated backups.
module Network.AWS.RDS.DeleteDBInstanceAutomatedBackup
  ( -- * Creating a Request
    DeleteDBInstanceAutomatedBackup (..),
    newDeleteDBInstanceAutomatedBackup,

    -- * Request Lenses
    deleteDBInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn,
    deleteDBInstanceAutomatedBackup_dbiResourceId,

    -- * Destructuring the Response
    DeleteDBInstanceAutomatedBackupResponse (..),
    newDeleteDBInstanceAutomatedBackupResponse,

    -- * Response Lenses
    deleteDBInstanceAutomatedBackupResponse_dbInstanceAutomatedBackup,
    deleteDBInstanceAutomatedBackupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Parameter input for the @DeleteDBInstanceAutomatedBackup@ operation.
--
-- /See:/ 'newDeleteDBInstanceAutomatedBackup' smart constructor.
data DeleteDBInstanceAutomatedBackup = DeleteDBInstanceAutomatedBackup'
  { -- | The Amazon Resource Name (ARN) of the automated backups to delete, for
    -- example,
    -- @arn:aws:rds:us-east-1:123456789012:auto-backup:ab-L2IJCEXJP7XQ7HOJ4SIEXAMPLE@.
    dbInstanceAutomatedBackupsArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the source DB instance, which can\'t be changed and
    -- which is unique to an AWS Region.
    dbiResourceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBInstanceAutomatedBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceAutomatedBackupsArn', 'deleteDBInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn' - The Amazon Resource Name (ARN) of the automated backups to delete, for
-- example,
-- @arn:aws:rds:us-east-1:123456789012:auto-backup:ab-L2IJCEXJP7XQ7HOJ4SIEXAMPLE@.
--
-- 'dbiResourceId', 'deleteDBInstanceAutomatedBackup_dbiResourceId' - The identifier for the source DB instance, which can\'t be changed and
-- which is unique to an AWS Region.
newDeleteDBInstanceAutomatedBackup ::
  DeleteDBInstanceAutomatedBackup
newDeleteDBInstanceAutomatedBackup =
  DeleteDBInstanceAutomatedBackup'
    { dbInstanceAutomatedBackupsArn =
        Prelude.Nothing,
      dbiResourceId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the automated backups to delete, for
-- example,
-- @arn:aws:rds:us-east-1:123456789012:auto-backup:ab-L2IJCEXJP7XQ7HOJ4SIEXAMPLE@.
deleteDBInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn :: Lens.Lens' DeleteDBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
deleteDBInstanceAutomatedBackup_dbInstanceAutomatedBackupsArn = Lens.lens (\DeleteDBInstanceAutomatedBackup' {dbInstanceAutomatedBackupsArn} -> dbInstanceAutomatedBackupsArn) (\s@DeleteDBInstanceAutomatedBackup' {} a -> s {dbInstanceAutomatedBackupsArn = a} :: DeleteDBInstanceAutomatedBackup)

-- | The identifier for the source DB instance, which can\'t be changed and
-- which is unique to an AWS Region.
deleteDBInstanceAutomatedBackup_dbiResourceId :: Lens.Lens' DeleteDBInstanceAutomatedBackup (Prelude.Maybe Prelude.Text)
deleteDBInstanceAutomatedBackup_dbiResourceId = Lens.lens (\DeleteDBInstanceAutomatedBackup' {dbiResourceId} -> dbiResourceId) (\s@DeleteDBInstanceAutomatedBackup' {} a -> s {dbiResourceId = a} :: DeleteDBInstanceAutomatedBackup)

instance
  Core.AWSRequest
    DeleteDBInstanceAutomatedBackup
  where
  type
    AWSResponse DeleteDBInstanceAutomatedBackup =
      DeleteDBInstanceAutomatedBackupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteDBInstanceAutomatedBackupResult"
      ( \s h x ->
          DeleteDBInstanceAutomatedBackupResponse'
            Prelude.<$> (x Core..@? "DBInstanceAutomatedBackup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteDBInstanceAutomatedBackup

instance
  Prelude.NFData
    DeleteDBInstanceAutomatedBackup

instance
  Core.ToHeaders
    DeleteDBInstanceAutomatedBackup
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteDBInstanceAutomatedBackup where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteDBInstanceAutomatedBackup where
  toQuery DeleteDBInstanceAutomatedBackup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeleteDBInstanceAutomatedBackup" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "DBInstanceAutomatedBackupsArn"
          Core.=: dbInstanceAutomatedBackupsArn,
        "DbiResourceId" Core.=: dbiResourceId
      ]

-- | /See:/ 'newDeleteDBInstanceAutomatedBackupResponse' smart constructor.
data DeleteDBInstanceAutomatedBackupResponse = DeleteDBInstanceAutomatedBackupResponse'
  { dbInstanceAutomatedBackup :: Prelude.Maybe DBInstanceAutomatedBackup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDBInstanceAutomatedBackupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceAutomatedBackup', 'deleteDBInstanceAutomatedBackupResponse_dbInstanceAutomatedBackup' - Undocumented member.
--
-- 'httpStatus', 'deleteDBInstanceAutomatedBackupResponse_httpStatus' - The response's http status code.
newDeleteDBInstanceAutomatedBackupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDBInstanceAutomatedBackupResponse
newDeleteDBInstanceAutomatedBackupResponse
  pHttpStatus_ =
    DeleteDBInstanceAutomatedBackupResponse'
      { dbInstanceAutomatedBackup =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
deleteDBInstanceAutomatedBackupResponse_dbInstanceAutomatedBackup :: Lens.Lens' DeleteDBInstanceAutomatedBackupResponse (Prelude.Maybe DBInstanceAutomatedBackup)
deleteDBInstanceAutomatedBackupResponse_dbInstanceAutomatedBackup = Lens.lens (\DeleteDBInstanceAutomatedBackupResponse' {dbInstanceAutomatedBackup} -> dbInstanceAutomatedBackup) (\s@DeleteDBInstanceAutomatedBackupResponse' {} a -> s {dbInstanceAutomatedBackup = a} :: DeleteDBInstanceAutomatedBackupResponse)

-- | The response's http status code.
deleteDBInstanceAutomatedBackupResponse_httpStatus :: Lens.Lens' DeleteDBInstanceAutomatedBackupResponse Prelude.Int
deleteDBInstanceAutomatedBackupResponse_httpStatus = Lens.lens (\DeleteDBInstanceAutomatedBackupResponse' {httpStatus} -> httpStatus) (\s@DeleteDBInstanceAutomatedBackupResponse' {} a -> s {httpStatus = a} :: DeleteDBInstanceAutomatedBackupResponse)

instance
  Prelude.NFData
    DeleteDBInstanceAutomatedBackupResponse
