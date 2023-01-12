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
-- Module      : Amazonka.OpsWorksCM.DeleteBackup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a backup. You can delete both manual and automated backups. This
-- operation is asynchronous.
--
-- An @InvalidStateException@ is thrown when a backup deletion is already
-- in progress. A @ResourceNotFoundException@ is thrown when the backup
-- does not exist. A @ValidationException@ is thrown when parameters of the
-- request are not valid.
module Amazonka.OpsWorksCM.DeleteBackup
  ( -- * Creating a Request
    DeleteBackup (..),
    newDeleteBackup,

    -- * Request Lenses
    deleteBackup_backupId,

    -- * Destructuring the Response
    DeleteBackupResponse (..),
    newDeleteBackupResponse,

    -- * Response Lenses
    deleteBackupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorksCM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBackup' smart constructor.
data DeleteBackup = DeleteBackup'
  { -- | The ID of the backup to delete. Run the DescribeBackups command to get a
    -- list of backup IDs. Backup IDs are in the format
    -- @ServerName-yyyyMMddHHmmssSSS@.
    backupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupId', 'deleteBackup_backupId' - The ID of the backup to delete. Run the DescribeBackups command to get a
-- list of backup IDs. Backup IDs are in the format
-- @ServerName-yyyyMMddHHmmssSSS@.
newDeleteBackup ::
  -- | 'backupId'
  Prelude.Text ->
  DeleteBackup
newDeleteBackup pBackupId_ =
  DeleteBackup' {backupId = pBackupId_}

-- | The ID of the backup to delete. Run the DescribeBackups command to get a
-- list of backup IDs. Backup IDs are in the format
-- @ServerName-yyyyMMddHHmmssSSS@.
deleteBackup_backupId :: Lens.Lens' DeleteBackup Prelude.Text
deleteBackup_backupId = Lens.lens (\DeleteBackup' {backupId} -> backupId) (\s@DeleteBackup' {} a -> s {backupId = a} :: DeleteBackup)

instance Core.AWSRequest DeleteBackup where
  type AWSResponse DeleteBackup = DeleteBackupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteBackupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBackup where
  hashWithSalt _salt DeleteBackup' {..} =
    _salt `Prelude.hashWithSalt` backupId

instance Prelude.NFData DeleteBackup where
  rnf DeleteBackup' {..} = Prelude.rnf backupId

instance Data.ToHeaders DeleteBackup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorksCM_V2016_11_01.DeleteBackup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteBackup where
  toJSON DeleteBackup' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("BackupId" Data..= backupId)]
      )

instance Data.ToPath DeleteBackup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteBackup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBackupResponse' smart constructor.
data DeleteBackupResponse = DeleteBackupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBackupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteBackupResponse_httpStatus' - The response's http status code.
newDeleteBackupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBackupResponse
newDeleteBackupResponse pHttpStatus_ =
  DeleteBackupResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteBackupResponse_httpStatus :: Lens.Lens' DeleteBackupResponse Prelude.Int
deleteBackupResponse_httpStatus = Lens.lens (\DeleteBackupResponse' {httpStatus} -> httpStatus) (\s@DeleteBackupResponse' {} a -> s {httpStatus = a} :: DeleteBackupResponse)

instance Prelude.NFData DeleteBackupResponse where
  rnf DeleteBackupResponse' {..} =
    Prelude.rnf httpStatus
