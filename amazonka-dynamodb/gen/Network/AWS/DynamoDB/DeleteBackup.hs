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
-- Module      : Network.AWS.DynamoDB.DeleteBackup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing backup of a table.
--
-- You can call @DeleteBackup@ at a maximum rate of 10 times per second.
module Network.AWS.DynamoDB.DeleteBackup
  ( -- * Creating a Request
    DeleteBackup (..),
    newDeleteBackup,

    -- * Request Lenses
    deleteBackup_backupArn,

    -- * Destructuring the Response
    DeleteBackupResponse (..),
    newDeleteBackupResponse,

    -- * Response Lenses
    deleteBackupResponse_backupDescription,
    deleteBackupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBackup' smart constructor.
data DeleteBackup = DeleteBackup'
  { -- | The ARN associated with the backup.
    backupArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupArn', 'deleteBackup_backupArn' - The ARN associated with the backup.
newDeleteBackup ::
  -- | 'backupArn'
  Core.Text ->
  DeleteBackup
newDeleteBackup pBackupArn_ =
  DeleteBackup' {backupArn = pBackupArn_}

-- | The ARN associated with the backup.
deleteBackup_backupArn :: Lens.Lens' DeleteBackup Core.Text
deleteBackup_backupArn = Lens.lens (\DeleteBackup' {backupArn} -> backupArn) (\s@DeleteBackup' {} a -> s {backupArn = a} :: DeleteBackup)

instance Core.AWSRequest DeleteBackup where
  type AWSResponse DeleteBackup = DeleteBackupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBackupResponse'
            Core.<$> (x Core..?> "BackupDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteBackup

instance Core.NFData DeleteBackup

instance Core.ToHeaders DeleteBackup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.DeleteBackup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteBackup where
  toJSON DeleteBackup' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("BackupArn" Core..= backupArn)]
      )

instance Core.ToPath DeleteBackup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteBackup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteBackupResponse' smart constructor.
data DeleteBackupResponse = DeleteBackupResponse'
  { -- | Contains the description of the backup created for the table.
    backupDescription :: Core.Maybe BackupDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteBackupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupDescription', 'deleteBackupResponse_backupDescription' - Contains the description of the backup created for the table.
--
-- 'httpStatus', 'deleteBackupResponse_httpStatus' - The response's http status code.
newDeleteBackupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteBackupResponse
newDeleteBackupResponse pHttpStatus_ =
  DeleteBackupResponse'
    { backupDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the description of the backup created for the table.
deleteBackupResponse_backupDescription :: Lens.Lens' DeleteBackupResponse (Core.Maybe BackupDescription)
deleteBackupResponse_backupDescription = Lens.lens (\DeleteBackupResponse' {backupDescription} -> backupDescription) (\s@DeleteBackupResponse' {} a -> s {backupDescription = a} :: DeleteBackupResponse)

-- | The response's http status code.
deleteBackupResponse_httpStatus :: Lens.Lens' DeleteBackupResponse Core.Int
deleteBackupResponse_httpStatus = Lens.lens (\DeleteBackupResponse' {httpStatus} -> httpStatus) (\s@DeleteBackupResponse' {} a -> s {httpStatus = a} :: DeleteBackupResponse)

instance Core.NFData DeleteBackupResponse
