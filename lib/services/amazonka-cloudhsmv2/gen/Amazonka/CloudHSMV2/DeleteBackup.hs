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
-- Module      : Amazonka.CloudHSMV2.DeleteBackup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified AWS CloudHSM backup. A backup can be restored up to
-- 7 days after the DeleteBackup request is made. For more information on
-- restoring a backup, see RestoreBackup.
module Amazonka.CloudHSMV2.DeleteBackup
  ( -- * Creating a Request
    DeleteBackup (..),
    newDeleteBackup,

    -- * Request Lenses
    deleteBackup_backupId,

    -- * Destructuring the Response
    DeleteBackupResponse (..),
    newDeleteBackupResponse,

    -- * Response Lenses
    deleteBackupResponse_backup,
    deleteBackupResponse_httpStatus,
  )
where

import Amazonka.CloudHSMV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBackup' smart constructor.
data DeleteBackup = DeleteBackup'
  { -- | The ID of the backup to be deleted. To find the ID of a backup, use the
    -- DescribeBackups operation.
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
-- 'backupId', 'deleteBackup_backupId' - The ID of the backup to be deleted. To find the ID of a backup, use the
-- DescribeBackups operation.
newDeleteBackup ::
  -- | 'backupId'
  Prelude.Text ->
  DeleteBackup
newDeleteBackup pBackupId_ =
  DeleteBackup' {backupId = pBackupId_}

-- | The ID of the backup to be deleted. To find the ID of a backup, use the
-- DescribeBackups operation.
deleteBackup_backupId :: Lens.Lens' DeleteBackup Prelude.Text
deleteBackup_backupId = Lens.lens (\DeleteBackup' {backupId} -> backupId) (\s@DeleteBackup' {} a -> s {backupId = a} :: DeleteBackup)

instance Core.AWSRequest DeleteBackup where
  type AWSResponse DeleteBackup = DeleteBackupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBackupResponse'
            Prelude.<$> (x Data..?> "Backup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
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
              Data.=# ( "BaldrApiService.DeleteBackup" ::
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
  { -- | Information on the @Backup@ object deleted.
    backup :: Prelude.Maybe Backup,
    -- | The response's http status code.
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
-- 'backup', 'deleteBackupResponse_backup' - Information on the @Backup@ object deleted.
--
-- 'httpStatus', 'deleteBackupResponse_httpStatus' - The response's http status code.
newDeleteBackupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBackupResponse
newDeleteBackupResponse pHttpStatus_ =
  DeleteBackupResponse'
    { backup = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information on the @Backup@ object deleted.
deleteBackupResponse_backup :: Lens.Lens' DeleteBackupResponse (Prelude.Maybe Backup)
deleteBackupResponse_backup = Lens.lens (\DeleteBackupResponse' {backup} -> backup) (\s@DeleteBackupResponse' {} a -> s {backup = a} :: DeleteBackupResponse)

-- | The response's http status code.
deleteBackupResponse_httpStatus :: Lens.Lens' DeleteBackupResponse Prelude.Int
deleteBackupResponse_httpStatus = Lens.lens (\DeleteBackupResponse' {httpStatus} -> httpStatus) (\s@DeleteBackupResponse' {} a -> s {httpStatus = a} :: DeleteBackupResponse)

instance Prelude.NFData DeleteBackupResponse where
  rnf DeleteBackupResponse' {..} =
    Prelude.rnf backup
      `Prelude.seq` Prelude.rnf httpStatus
