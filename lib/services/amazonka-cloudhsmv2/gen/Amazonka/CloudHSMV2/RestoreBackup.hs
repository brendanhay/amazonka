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
-- Module      : Amazonka.CloudHSMV2.RestoreBackup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a specified AWS CloudHSM backup that is in the
-- @PENDING_DELETION@ state. For mor information on deleting a backup, see
-- DeleteBackup.
module Amazonka.CloudHSMV2.RestoreBackup
  ( -- * Creating a Request
    RestoreBackup (..),
    newRestoreBackup,

    -- * Request Lenses
    restoreBackup_backupId,

    -- * Destructuring the Response
    RestoreBackupResponse (..),
    newRestoreBackupResponse,

    -- * Response Lenses
    restoreBackupResponse_backup,
    restoreBackupResponse_httpStatus,
  )
where

import Amazonka.CloudHSMV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreBackup' smart constructor.
data RestoreBackup = RestoreBackup'
  { -- | The ID of the backup to be restored. To find the ID of a backup, use the
    -- DescribeBackups operation.
    backupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupId', 'restoreBackup_backupId' - The ID of the backup to be restored. To find the ID of a backup, use the
-- DescribeBackups operation.
newRestoreBackup ::
  -- | 'backupId'
  Prelude.Text ->
  RestoreBackup
newRestoreBackup pBackupId_ =
  RestoreBackup' {backupId = pBackupId_}

-- | The ID of the backup to be restored. To find the ID of a backup, use the
-- DescribeBackups operation.
restoreBackup_backupId :: Lens.Lens' RestoreBackup Prelude.Text
restoreBackup_backupId = Lens.lens (\RestoreBackup' {backupId} -> backupId) (\s@RestoreBackup' {} a -> s {backupId = a} :: RestoreBackup)

instance Core.AWSRequest RestoreBackup where
  type
    AWSResponse RestoreBackup =
      RestoreBackupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreBackupResponse'
            Prelude.<$> (x Data..?> "Backup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreBackup where
  hashWithSalt _salt RestoreBackup' {..} =
    _salt `Prelude.hashWithSalt` backupId

instance Prelude.NFData RestoreBackup where
  rnf RestoreBackup' {..} = Prelude.rnf backupId

instance Data.ToHeaders RestoreBackup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BaldrApiService.RestoreBackup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RestoreBackup where
  toJSON RestoreBackup' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("BackupId" Data..= backupId)]
      )

instance Data.ToPath RestoreBackup where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreBackup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreBackupResponse' smart constructor.
data RestoreBackupResponse = RestoreBackupResponse'
  { -- | Information on the @Backup@ object created.
    backup :: Prelude.Maybe Backup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreBackupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backup', 'restoreBackupResponse_backup' - Information on the @Backup@ object created.
--
-- 'httpStatus', 'restoreBackupResponse_httpStatus' - The response's http status code.
newRestoreBackupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreBackupResponse
newRestoreBackupResponse pHttpStatus_ =
  RestoreBackupResponse'
    { backup = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information on the @Backup@ object created.
restoreBackupResponse_backup :: Lens.Lens' RestoreBackupResponse (Prelude.Maybe Backup)
restoreBackupResponse_backup = Lens.lens (\RestoreBackupResponse' {backup} -> backup) (\s@RestoreBackupResponse' {} a -> s {backup = a} :: RestoreBackupResponse)

-- | The response's http status code.
restoreBackupResponse_httpStatus :: Lens.Lens' RestoreBackupResponse Prelude.Int
restoreBackupResponse_httpStatus = Lens.lens (\RestoreBackupResponse' {httpStatus} -> httpStatus) (\s@RestoreBackupResponse' {} a -> s {httpStatus = a} :: RestoreBackupResponse)

instance Prelude.NFData RestoreBackupResponse where
  rnf RestoreBackupResponse' {..} =
    Prelude.rnf backup
      `Prelude.seq` Prelude.rnf httpStatus
