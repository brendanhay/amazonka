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
-- Module      : Network.AWS.Backup.GetBackupPlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns @BackupPlan@ details for the specified @BackupPlanId@. The
-- details are the body of a backup plan in JSON format, in addition to
-- plan metadata.
module Network.AWS.Backup.GetBackupPlan
  ( -- * Creating a Request
    GetBackupPlan (..),
    newGetBackupPlan,

    -- * Request Lenses
    getBackupPlan_versionId,
    getBackupPlan_backupPlanId,

    -- * Destructuring the Response
    GetBackupPlanResponse (..),
    newGetBackupPlanResponse,

    -- * Response Lenses
    getBackupPlanResponse_versionId,
    getBackupPlanResponse_advancedBackupSettings,
    getBackupPlanResponse_backupPlanId,
    getBackupPlanResponse_creatorRequestId,
    getBackupPlanResponse_backupPlanArn,
    getBackupPlanResponse_lastExecutionDate,
    getBackupPlanResponse_backupPlan,
    getBackupPlanResponse_creationDate,
    getBackupPlanResponse_deletionDate,
    getBackupPlanResponse_httpStatus,
  )
where

import Network.AWS.Backup.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBackupPlan' smart constructor.
data GetBackupPlan = GetBackupPlan'
  { -- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
    -- most 1,024 bytes long. Version IDs cannot be edited.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackupPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'getBackupPlan_versionId' - Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version IDs cannot be edited.
--
-- 'backupPlanId', 'getBackupPlan_backupPlanId' - Uniquely identifies a backup plan.
newGetBackupPlan ::
  -- | 'backupPlanId'
  Prelude.Text ->
  GetBackupPlan
newGetBackupPlan pBackupPlanId_ =
  GetBackupPlan'
    { versionId = Prelude.Nothing,
      backupPlanId = pBackupPlanId_
    }

-- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version IDs cannot be edited.
getBackupPlan_versionId :: Lens.Lens' GetBackupPlan (Prelude.Maybe Prelude.Text)
getBackupPlan_versionId = Lens.lens (\GetBackupPlan' {versionId} -> versionId) (\s@GetBackupPlan' {} a -> s {versionId = a} :: GetBackupPlan)

-- | Uniquely identifies a backup plan.
getBackupPlan_backupPlanId :: Lens.Lens' GetBackupPlan Prelude.Text
getBackupPlan_backupPlanId = Lens.lens (\GetBackupPlan' {backupPlanId} -> backupPlanId) (\s@GetBackupPlan' {} a -> s {backupPlanId = a} :: GetBackupPlan)

instance Core.AWSRequest GetBackupPlan where
  type
    AWSResponse GetBackupPlan =
      GetBackupPlanResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBackupPlanResponse'
            Prelude.<$> (x Core..?> "VersionId")
            Prelude.<*> ( x Core..?> "AdvancedBackupSettings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "BackupPlanId")
            Prelude.<*> (x Core..?> "CreatorRequestId")
            Prelude.<*> (x Core..?> "BackupPlanArn")
            Prelude.<*> (x Core..?> "LastExecutionDate")
            Prelude.<*> (x Core..?> "BackupPlan")
            Prelude.<*> (x Core..?> "CreationDate")
            Prelude.<*> (x Core..?> "DeletionDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBackupPlan

instance Prelude.NFData GetBackupPlan

instance Core.ToHeaders GetBackupPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetBackupPlan where
  toPath GetBackupPlan' {..} =
    Prelude.mconcat
      ["/backup/plans/", Core.toBS backupPlanId, "/"]

instance Core.ToQuery GetBackupPlan where
  toQuery GetBackupPlan' {..} =
    Prelude.mconcat ["versionId" Core.=: versionId]

-- | /See:/ 'newGetBackupPlanResponse' smart constructor.
data GetBackupPlanResponse = GetBackupPlanResponse'
  { -- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
    -- most 1,024 bytes long. Version IDs cannot be edited.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | Contains a list of @BackupOptions@ for each resource type. The list is
    -- populated only if the advanced option is set for the backup plan.
    advancedBackupSettings :: Prelude.Maybe [AdvancedBackupSetting],
    -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Maybe Prelude.Text,
    -- | A unique string that identifies the request and allows failed requests
    -- to be retried without the risk of running the operation twice.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
    -- for example,
    -- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
    backupPlanArn :: Prelude.Maybe Prelude.Text,
    -- | The last time a job to back up resources was run with this backup plan.
    -- A date and time, in Unix format and Coordinated Universal Time (UTC).
    -- The value of @LastExecutionDate@ is accurate to milliseconds. For
    -- example, the value 1516925490.087 represents Friday, January 26, 2018
    -- 12:11:30.087 AM.
    lastExecutionDate :: Prelude.Maybe Core.POSIX,
    -- | Specifies the body of a backup plan. Includes a @BackupPlanName@ and one
    -- or more sets of @Rules@.
    backupPlan :: Prelude.Maybe BackupPlan,
    -- | The date and time that a backup plan is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The date and time that a backup plan is deleted, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @DeletionDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    deletionDate :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackupPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'getBackupPlanResponse_versionId' - Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version IDs cannot be edited.
--
-- 'advancedBackupSettings', 'getBackupPlanResponse_advancedBackupSettings' - Contains a list of @BackupOptions@ for each resource type. The list is
-- populated only if the advanced option is set for the backup plan.
--
-- 'backupPlanId', 'getBackupPlanResponse_backupPlanId' - Uniquely identifies a backup plan.
--
-- 'creatorRequestId', 'getBackupPlanResponse_creatorRequestId' - A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice.
--
-- 'backupPlanArn', 'getBackupPlanResponse_backupPlanArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
--
-- 'lastExecutionDate', 'getBackupPlanResponse_lastExecutionDate' - The last time a job to back up resources was run with this backup plan.
-- A date and time, in Unix format and Coordinated Universal Time (UTC).
-- The value of @LastExecutionDate@ is accurate to milliseconds. For
-- example, the value 1516925490.087 represents Friday, January 26, 2018
-- 12:11:30.087 AM.
--
-- 'backupPlan', 'getBackupPlanResponse_backupPlan' - Specifies the body of a backup plan. Includes a @BackupPlanName@ and one
-- or more sets of @Rules@.
--
-- 'creationDate', 'getBackupPlanResponse_creationDate' - The date and time that a backup plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'deletionDate', 'getBackupPlanResponse_deletionDate' - The date and time that a backup plan is deleted, in Unix format and
-- Coordinated Universal Time (UTC). The value of @DeletionDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'httpStatus', 'getBackupPlanResponse_httpStatus' - The response's http status code.
newGetBackupPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBackupPlanResponse
newGetBackupPlanResponse pHttpStatus_ =
  GetBackupPlanResponse'
    { versionId = Prelude.Nothing,
      advancedBackupSettings = Prelude.Nothing,
      backupPlanId = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      backupPlanArn = Prelude.Nothing,
      lastExecutionDate = Prelude.Nothing,
      backupPlan = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      deletionDate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version IDs cannot be edited.
getBackupPlanResponse_versionId :: Lens.Lens' GetBackupPlanResponse (Prelude.Maybe Prelude.Text)
getBackupPlanResponse_versionId = Lens.lens (\GetBackupPlanResponse' {versionId} -> versionId) (\s@GetBackupPlanResponse' {} a -> s {versionId = a} :: GetBackupPlanResponse)

-- | Contains a list of @BackupOptions@ for each resource type. The list is
-- populated only if the advanced option is set for the backup plan.
getBackupPlanResponse_advancedBackupSettings :: Lens.Lens' GetBackupPlanResponse (Prelude.Maybe [AdvancedBackupSetting])
getBackupPlanResponse_advancedBackupSettings = Lens.lens (\GetBackupPlanResponse' {advancedBackupSettings} -> advancedBackupSettings) (\s@GetBackupPlanResponse' {} a -> s {advancedBackupSettings = a} :: GetBackupPlanResponse) Prelude.. Lens.mapping Lens.coerced

-- | Uniquely identifies a backup plan.
getBackupPlanResponse_backupPlanId :: Lens.Lens' GetBackupPlanResponse (Prelude.Maybe Prelude.Text)
getBackupPlanResponse_backupPlanId = Lens.lens (\GetBackupPlanResponse' {backupPlanId} -> backupPlanId) (\s@GetBackupPlanResponse' {} a -> s {backupPlanId = a} :: GetBackupPlanResponse)

-- | A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice.
getBackupPlanResponse_creatorRequestId :: Lens.Lens' GetBackupPlanResponse (Prelude.Maybe Prelude.Text)
getBackupPlanResponse_creatorRequestId = Lens.lens (\GetBackupPlanResponse' {creatorRequestId} -> creatorRequestId) (\s@GetBackupPlanResponse' {} a -> s {creatorRequestId = a} :: GetBackupPlanResponse)

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
getBackupPlanResponse_backupPlanArn :: Lens.Lens' GetBackupPlanResponse (Prelude.Maybe Prelude.Text)
getBackupPlanResponse_backupPlanArn = Lens.lens (\GetBackupPlanResponse' {backupPlanArn} -> backupPlanArn) (\s@GetBackupPlanResponse' {} a -> s {backupPlanArn = a} :: GetBackupPlanResponse)

-- | The last time a job to back up resources was run with this backup plan.
-- A date and time, in Unix format and Coordinated Universal Time (UTC).
-- The value of @LastExecutionDate@ is accurate to milliseconds. For
-- example, the value 1516925490.087 represents Friday, January 26, 2018
-- 12:11:30.087 AM.
getBackupPlanResponse_lastExecutionDate :: Lens.Lens' GetBackupPlanResponse (Prelude.Maybe Prelude.UTCTime)
getBackupPlanResponse_lastExecutionDate = Lens.lens (\GetBackupPlanResponse' {lastExecutionDate} -> lastExecutionDate) (\s@GetBackupPlanResponse' {} a -> s {lastExecutionDate = a} :: GetBackupPlanResponse) Prelude.. Lens.mapping Core._Time

-- | Specifies the body of a backup plan. Includes a @BackupPlanName@ and one
-- or more sets of @Rules@.
getBackupPlanResponse_backupPlan :: Lens.Lens' GetBackupPlanResponse (Prelude.Maybe BackupPlan)
getBackupPlanResponse_backupPlan = Lens.lens (\GetBackupPlanResponse' {backupPlan} -> backupPlan) (\s@GetBackupPlanResponse' {} a -> s {backupPlan = a} :: GetBackupPlanResponse)

-- | The date and time that a backup plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
getBackupPlanResponse_creationDate :: Lens.Lens' GetBackupPlanResponse (Prelude.Maybe Prelude.UTCTime)
getBackupPlanResponse_creationDate = Lens.lens (\GetBackupPlanResponse' {creationDate} -> creationDate) (\s@GetBackupPlanResponse' {} a -> s {creationDate = a} :: GetBackupPlanResponse) Prelude.. Lens.mapping Core._Time

-- | The date and time that a backup plan is deleted, in Unix format and
-- Coordinated Universal Time (UTC). The value of @DeletionDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
getBackupPlanResponse_deletionDate :: Lens.Lens' GetBackupPlanResponse (Prelude.Maybe Prelude.UTCTime)
getBackupPlanResponse_deletionDate = Lens.lens (\GetBackupPlanResponse' {deletionDate} -> deletionDate) (\s@GetBackupPlanResponse' {} a -> s {deletionDate = a} :: GetBackupPlanResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
getBackupPlanResponse_httpStatus :: Lens.Lens' GetBackupPlanResponse Prelude.Int
getBackupPlanResponse_httpStatus = Lens.lens (\GetBackupPlanResponse' {httpStatus} -> httpStatus) (\s@GetBackupPlanResponse' {} a -> s {httpStatus = a} :: GetBackupPlanResponse)

instance Prelude.NFData GetBackupPlanResponse
