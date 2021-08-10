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
-- Module      : Network.AWS.DynamoDB.CreateBackup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a backup for an existing table.
--
-- Each time you create an on-demand backup, the entire table data is
-- backed up. There is no limit to the number of on-demand backups that can
-- be taken.
--
-- When you create an on-demand backup, a time marker of the request is
-- cataloged, and the backup is created asynchronously, by applying all
-- changes until the time of the request to the last full table snapshot.
-- Backup requests are processed instantaneously and become available for
-- restore within minutes.
--
-- You can call @CreateBackup@ at a maximum rate of 50 times per second.
--
-- All backups in DynamoDB work without consuming any provisioned
-- throughput on the table.
--
-- If you submit a backup request on 2018-12-14 at 14:25:00, the backup is
-- guaranteed to contain all data committed to the table up to 14:24:00,
-- and data committed after 14:26:00 will not be. The backup might contain
-- data modifications made between 14:24:00 and 14:26:00. On-demand backup
-- does not support causal consistency.
--
-- Along with data, the following are also included on the backups:
--
-- -   Global secondary indexes (GSIs)
--
-- -   Local secondary indexes (LSIs)
--
-- -   Streams
--
-- -   Provisioned read and write capacity
module Network.AWS.DynamoDB.CreateBackup
  ( -- * Creating a Request
    CreateBackup (..),
    newCreateBackup,

    -- * Request Lenses
    createBackup_tableName,
    createBackup_backupName,

    -- * Destructuring the Response
    CreateBackupResponse (..),
    newCreateBackupResponse,

    -- * Response Lenses
    createBackupResponse_backupDetails,
    createBackupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateBackup' smart constructor.
data CreateBackup = CreateBackup'
  { -- | The name of the table.
    tableName :: Prelude.Text,
    -- | Specified name for the backup.
    backupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'createBackup_tableName' - The name of the table.
--
-- 'backupName', 'createBackup_backupName' - Specified name for the backup.
newCreateBackup ::
  -- | 'tableName'
  Prelude.Text ->
  -- | 'backupName'
  Prelude.Text ->
  CreateBackup
newCreateBackup pTableName_ pBackupName_ =
  CreateBackup'
    { tableName = pTableName_,
      backupName = pBackupName_
    }

-- | The name of the table.
createBackup_tableName :: Lens.Lens' CreateBackup Prelude.Text
createBackup_tableName = Lens.lens (\CreateBackup' {tableName} -> tableName) (\s@CreateBackup' {} a -> s {tableName = a} :: CreateBackup)

-- | Specified name for the backup.
createBackup_backupName :: Lens.Lens' CreateBackup Prelude.Text
createBackup_backupName = Lens.lens (\CreateBackup' {backupName} -> backupName) (\s@CreateBackup' {} a -> s {backupName = a} :: CreateBackup)

instance Core.AWSRequest CreateBackup where
  type AWSResponse CreateBackup = CreateBackupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBackupResponse'
            Prelude.<$> (x Core..?> "BackupDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBackup

instance Prelude.NFData CreateBackup

instance Core.ToHeaders CreateBackup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.CreateBackup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateBackup where
  toJSON CreateBackup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TableName" Core..= tableName),
            Prelude.Just ("BackupName" Core..= backupName)
          ]
      )

instance Core.ToPath CreateBackup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateBackup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBackupResponse' smart constructor.
data CreateBackupResponse = CreateBackupResponse'
  { -- | Contains the details of the backup created for the table.
    backupDetails :: Prelude.Maybe BackupDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupDetails', 'createBackupResponse_backupDetails' - Contains the details of the backup created for the table.
--
-- 'httpStatus', 'createBackupResponse_httpStatus' - The response's http status code.
newCreateBackupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBackupResponse
newCreateBackupResponse pHttpStatus_ =
  CreateBackupResponse'
    { backupDetails =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the details of the backup created for the table.
createBackupResponse_backupDetails :: Lens.Lens' CreateBackupResponse (Prelude.Maybe BackupDetails)
createBackupResponse_backupDetails = Lens.lens (\CreateBackupResponse' {backupDetails} -> backupDetails) (\s@CreateBackupResponse' {} a -> s {backupDetails = a} :: CreateBackupResponse)

-- | The response's http status code.
createBackupResponse_httpStatus :: Lens.Lens' CreateBackupResponse Prelude.Int
createBackupResponse_httpStatus = Lens.lens (\CreateBackupResponse' {httpStatus} -> httpStatus) (\s@CreateBackupResponse' {} a -> s {httpStatus = a} :: CreateBackupResponse)

instance Prelude.NFData CreateBackupResponse
