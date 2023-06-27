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
-- Module      : Amazonka.FSx.DeleteBackup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon FSx backup. After deletion, the backup no longer
-- exists, and its data is gone.
--
-- The @DeleteBackup@ call returns instantly. The backup won\'t show up in
-- later @DescribeBackups@ calls.
--
-- The data in a deleted backup is also deleted and can\'t be recovered by
-- any means.
module Amazonka.FSx.DeleteBackup
  ( -- * Creating a Request
    DeleteBackup (..),
    newDeleteBackup,

    -- * Request Lenses
    deleteBackup_clientRequestToken,
    deleteBackup_backupId,

    -- * Destructuring the Response
    DeleteBackupResponse (..),
    newDeleteBackupResponse,

    -- * Response Lenses
    deleteBackupResponse_backupId,
    deleteBackupResponse_lifecycle,
    deleteBackupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request object for the @DeleteBackup@ operation.
--
-- /See:/ 'newDeleteBackup' smart constructor.
data DeleteBackup = DeleteBackup'
  { -- | A string of up to 63 ASCII characters that Amazon FSx uses to ensure
    -- idempotent deletion. This parameter is automatically filled on your
    -- behalf when using the CLI or SDK.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the backup that you want to delete.
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
-- 'clientRequestToken', 'deleteBackup_clientRequestToken' - A string of up to 63 ASCII characters that Amazon FSx uses to ensure
-- idempotent deletion. This parameter is automatically filled on your
-- behalf when using the CLI or SDK.
--
-- 'backupId', 'deleteBackup_backupId' - The ID of the backup that you want to delete.
newDeleteBackup ::
  -- | 'backupId'
  Prelude.Text ->
  DeleteBackup
newDeleteBackup pBackupId_ =
  DeleteBackup'
    { clientRequestToken = Prelude.Nothing,
      backupId = pBackupId_
    }

-- | A string of up to 63 ASCII characters that Amazon FSx uses to ensure
-- idempotent deletion. This parameter is automatically filled on your
-- behalf when using the CLI or SDK.
deleteBackup_clientRequestToken :: Lens.Lens' DeleteBackup (Prelude.Maybe Prelude.Text)
deleteBackup_clientRequestToken = Lens.lens (\DeleteBackup' {clientRequestToken} -> clientRequestToken) (\s@DeleteBackup' {} a -> s {clientRequestToken = a} :: DeleteBackup)

-- | The ID of the backup that you want to delete.
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
            Prelude.<$> (x Data..?> "BackupId")
            Prelude.<*> (x Data..?> "Lifecycle")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBackup where
  hashWithSalt _salt DeleteBackup' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` backupId

instance Prelude.NFData DeleteBackup where
  rnf DeleteBackup' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf backupId

instance Data.ToHeaders DeleteBackup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.DeleteBackup" ::
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
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("BackupId" Data..= backupId)
          ]
      )

instance Data.ToPath DeleteBackup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteBackup where
  toQuery = Prelude.const Prelude.mempty

-- | The response object for the @DeleteBackup@ operation.
--
-- /See:/ 'newDeleteBackupResponse' smart constructor.
data DeleteBackupResponse = DeleteBackupResponse'
  { -- | The ID of the backup that was deleted.
    backupId :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle status of the backup. If the @DeleteBackup@ operation is
    -- successful, the status is @DELETED@.
    lifecycle :: Prelude.Maybe BackupLifecycle,
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
-- 'backupId', 'deleteBackupResponse_backupId' - The ID of the backup that was deleted.
--
-- 'lifecycle', 'deleteBackupResponse_lifecycle' - The lifecycle status of the backup. If the @DeleteBackup@ operation is
-- successful, the status is @DELETED@.
--
-- 'httpStatus', 'deleteBackupResponse_httpStatus' - The response's http status code.
newDeleteBackupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBackupResponse
newDeleteBackupResponse pHttpStatus_ =
  DeleteBackupResponse'
    { backupId = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the backup that was deleted.
deleteBackupResponse_backupId :: Lens.Lens' DeleteBackupResponse (Prelude.Maybe Prelude.Text)
deleteBackupResponse_backupId = Lens.lens (\DeleteBackupResponse' {backupId} -> backupId) (\s@DeleteBackupResponse' {} a -> s {backupId = a} :: DeleteBackupResponse)

-- | The lifecycle status of the backup. If the @DeleteBackup@ operation is
-- successful, the status is @DELETED@.
deleteBackupResponse_lifecycle :: Lens.Lens' DeleteBackupResponse (Prelude.Maybe BackupLifecycle)
deleteBackupResponse_lifecycle = Lens.lens (\DeleteBackupResponse' {lifecycle} -> lifecycle) (\s@DeleteBackupResponse' {} a -> s {lifecycle = a} :: DeleteBackupResponse)

-- | The response's http status code.
deleteBackupResponse_httpStatus :: Lens.Lens' DeleteBackupResponse Prelude.Int
deleteBackupResponse_httpStatus = Lens.lens (\DeleteBackupResponse' {httpStatus} -> httpStatus) (\s@DeleteBackupResponse' {} a -> s {httpStatus = a} :: DeleteBackupResponse)

instance Prelude.NFData DeleteBackupResponse where
  rnf DeleteBackupResponse' {..} =
    Prelude.rnf backupId
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf httpStatus
