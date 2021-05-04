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

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBackup' smart constructor.
data DeleteBackup = DeleteBackup'
  { -- | The ARN associated with the backup.
    backupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteBackup
newDeleteBackup pBackupArn_ =
  DeleteBackup' {backupArn = pBackupArn_}

-- | The ARN associated with the backup.
deleteBackup_backupArn :: Lens.Lens' DeleteBackup Prelude.Text
deleteBackup_backupArn = Lens.lens (\DeleteBackup' {backupArn} -> backupArn) (\s@DeleteBackup' {} a -> s {backupArn = a} :: DeleteBackup)

instance Prelude.AWSRequest DeleteBackup where
  type Rs DeleteBackup = DeleteBackupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBackupResponse'
            Prelude.<$> (x Prelude..?> "BackupDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBackup

instance Prelude.NFData DeleteBackup

instance Prelude.ToHeaders DeleteBackup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DynamoDB_20120810.DeleteBackup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteBackup where
  toJSON DeleteBackup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("BackupArn" Prelude..= backupArn)]
      )

instance Prelude.ToPath DeleteBackup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteBackup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBackupResponse' smart constructor.
data DeleteBackupResponse = DeleteBackupResponse'
  { -- | Contains the description of the backup created for the table.
    backupDescription :: Prelude.Maybe BackupDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteBackupResponse
newDeleteBackupResponse pHttpStatus_ =
  DeleteBackupResponse'
    { backupDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the description of the backup created for the table.
deleteBackupResponse_backupDescription :: Lens.Lens' DeleteBackupResponse (Prelude.Maybe BackupDescription)
deleteBackupResponse_backupDescription = Lens.lens (\DeleteBackupResponse' {backupDescription} -> backupDescription) (\s@DeleteBackupResponse' {} a -> s {backupDescription = a} :: DeleteBackupResponse)

-- | The response's http status code.
deleteBackupResponse_httpStatus :: Lens.Lens' DeleteBackupResponse Prelude.Int
deleteBackupResponse_httpStatus = Lens.lens (\DeleteBackupResponse' {httpStatus} -> httpStatus) (\s@DeleteBackupResponse' {} a -> s {httpStatus = a} :: DeleteBackupResponse)

instance Prelude.NFData DeleteBackupResponse
