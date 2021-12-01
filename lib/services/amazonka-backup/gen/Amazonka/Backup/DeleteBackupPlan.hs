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
-- Module      : Amazonka.Backup.DeleteBackupPlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a backup plan. A backup plan can only be deleted after all
-- associated selections of resources have been deleted. Deleting a backup
-- plan deletes the current version of a backup plan. Previous versions, if
-- any, will still exist.
module Amazonka.Backup.DeleteBackupPlan
  ( -- * Creating a Request
    DeleteBackupPlan (..),
    newDeleteBackupPlan,

    -- * Request Lenses
    deleteBackupPlan_backupPlanId,

    -- * Destructuring the Response
    DeleteBackupPlanResponse (..),
    newDeleteBackupPlanResponse,

    -- * Response Lenses
    deleteBackupPlanResponse_versionId,
    deleteBackupPlanResponse_backupPlanId,
    deleteBackupPlanResponse_backupPlanArn,
    deleteBackupPlanResponse_deletionDate,
    deleteBackupPlanResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBackupPlan' smart constructor.
data DeleteBackupPlan = DeleteBackupPlan'
  { -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBackupPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPlanId', 'deleteBackupPlan_backupPlanId' - Uniquely identifies a backup plan.
newDeleteBackupPlan ::
  -- | 'backupPlanId'
  Prelude.Text ->
  DeleteBackupPlan
newDeleteBackupPlan pBackupPlanId_ =
  DeleteBackupPlan' {backupPlanId = pBackupPlanId_}

-- | Uniquely identifies a backup plan.
deleteBackupPlan_backupPlanId :: Lens.Lens' DeleteBackupPlan Prelude.Text
deleteBackupPlan_backupPlanId = Lens.lens (\DeleteBackupPlan' {backupPlanId} -> backupPlanId) (\s@DeleteBackupPlan' {} a -> s {backupPlanId = a} :: DeleteBackupPlan)

instance Core.AWSRequest DeleteBackupPlan where
  type
    AWSResponse DeleteBackupPlan =
      DeleteBackupPlanResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBackupPlanResponse'
            Prelude.<$> (x Core..?> "VersionId")
            Prelude.<*> (x Core..?> "BackupPlanId")
            Prelude.<*> (x Core..?> "BackupPlanArn")
            Prelude.<*> (x Core..?> "DeletionDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBackupPlan where
  hashWithSalt salt' DeleteBackupPlan' {..} =
    salt' `Prelude.hashWithSalt` backupPlanId

instance Prelude.NFData DeleteBackupPlan where
  rnf DeleteBackupPlan' {..} = Prelude.rnf backupPlanId

instance Core.ToHeaders DeleteBackupPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteBackupPlan where
  toPath DeleteBackupPlan' {..} =
    Prelude.mconcat
      ["/backup/plans/", Core.toBS backupPlanId]

instance Core.ToQuery DeleteBackupPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBackupPlanResponse' smart constructor.
data DeleteBackupPlanResponse = DeleteBackupPlanResponse'
  { -- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
    -- most 1,024 bytes long. Version IDs cannot be edited.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
    -- for example,
    -- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
    backupPlanArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time a backup plan is deleted, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @DeletionDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    deletionDate :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBackupPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'deleteBackupPlanResponse_versionId' - Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version IDs cannot be edited.
--
-- 'backupPlanId', 'deleteBackupPlanResponse_backupPlanId' - Uniquely identifies a backup plan.
--
-- 'backupPlanArn', 'deleteBackupPlanResponse_backupPlanArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
--
-- 'deletionDate', 'deleteBackupPlanResponse_deletionDate' - The date and time a backup plan is deleted, in Unix format and
-- Coordinated Universal Time (UTC). The value of @DeletionDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'httpStatus', 'deleteBackupPlanResponse_httpStatus' - The response's http status code.
newDeleteBackupPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBackupPlanResponse
newDeleteBackupPlanResponse pHttpStatus_ =
  DeleteBackupPlanResponse'
    { versionId =
        Prelude.Nothing,
      backupPlanId = Prelude.Nothing,
      backupPlanArn = Prelude.Nothing,
      deletionDate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version IDs cannot be edited.
deleteBackupPlanResponse_versionId :: Lens.Lens' DeleteBackupPlanResponse (Prelude.Maybe Prelude.Text)
deleteBackupPlanResponse_versionId = Lens.lens (\DeleteBackupPlanResponse' {versionId} -> versionId) (\s@DeleteBackupPlanResponse' {} a -> s {versionId = a} :: DeleteBackupPlanResponse)

-- | Uniquely identifies a backup plan.
deleteBackupPlanResponse_backupPlanId :: Lens.Lens' DeleteBackupPlanResponse (Prelude.Maybe Prelude.Text)
deleteBackupPlanResponse_backupPlanId = Lens.lens (\DeleteBackupPlanResponse' {backupPlanId} -> backupPlanId) (\s@DeleteBackupPlanResponse' {} a -> s {backupPlanId = a} :: DeleteBackupPlanResponse)

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
deleteBackupPlanResponse_backupPlanArn :: Lens.Lens' DeleteBackupPlanResponse (Prelude.Maybe Prelude.Text)
deleteBackupPlanResponse_backupPlanArn = Lens.lens (\DeleteBackupPlanResponse' {backupPlanArn} -> backupPlanArn) (\s@DeleteBackupPlanResponse' {} a -> s {backupPlanArn = a} :: DeleteBackupPlanResponse)

-- | The date and time a backup plan is deleted, in Unix format and
-- Coordinated Universal Time (UTC). The value of @DeletionDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
deleteBackupPlanResponse_deletionDate :: Lens.Lens' DeleteBackupPlanResponse (Prelude.Maybe Prelude.UTCTime)
deleteBackupPlanResponse_deletionDate = Lens.lens (\DeleteBackupPlanResponse' {deletionDate} -> deletionDate) (\s@DeleteBackupPlanResponse' {} a -> s {deletionDate = a} :: DeleteBackupPlanResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
deleteBackupPlanResponse_httpStatus :: Lens.Lens' DeleteBackupPlanResponse Prelude.Int
deleteBackupPlanResponse_httpStatus = Lens.lens (\DeleteBackupPlanResponse' {httpStatus} -> httpStatus) (\s@DeleteBackupPlanResponse' {} a -> s {httpStatus = a} :: DeleteBackupPlanResponse)

instance Prelude.NFData DeleteBackupPlanResponse where
  rnf DeleteBackupPlanResponse' {..} =
    Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf deletionDate
      `Prelude.seq` Prelude.rnf backupPlanArn
      `Prelude.seq` Prelude.rnf backupPlanId
