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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    deleteBackupPlanResponse_backupPlanArn,
    deleteBackupPlanResponse_backupPlanId,
    deleteBackupPlanResponse_deletionDate,
    deleteBackupPlanResponse_versionId,
    deleteBackupPlanResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBackupPlanResponse'
            Prelude.<$> (x Core..?> "BackupPlanArn")
            Prelude.<*> (x Core..?> "BackupPlanId")
            Prelude.<*> (x Core..?> "DeletionDate")
            Prelude.<*> (x Core..?> "VersionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBackupPlan where
  hashWithSalt _salt DeleteBackupPlan' {..} =
    _salt `Prelude.hashWithSalt` backupPlanId

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
  { -- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
    -- for example,
    -- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
    backupPlanArn :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Maybe Prelude.Text,
    -- | The date and time a backup plan is deleted, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @DeletionDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    deletionDate :: Prelude.Maybe Core.POSIX,
    -- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
    -- most 1,024 bytes long. Version IDs cannot be edited.
    versionId :: Prelude.Maybe Prelude.Text,
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
-- 'backupPlanArn', 'deleteBackupPlanResponse_backupPlanArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
--
-- 'backupPlanId', 'deleteBackupPlanResponse_backupPlanId' - Uniquely identifies a backup plan.
--
-- 'deletionDate', 'deleteBackupPlanResponse_deletionDate' - The date and time a backup plan is deleted, in Unix format and
-- Coordinated Universal Time (UTC). The value of @DeletionDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'versionId', 'deleteBackupPlanResponse_versionId' - Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version IDs cannot be edited.
--
-- 'httpStatus', 'deleteBackupPlanResponse_httpStatus' - The response's http status code.
newDeleteBackupPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBackupPlanResponse
newDeleteBackupPlanResponse pHttpStatus_ =
  DeleteBackupPlanResponse'
    { backupPlanArn =
        Prelude.Nothing,
      backupPlanId = Prelude.Nothing,
      deletionDate = Prelude.Nothing,
      versionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
deleteBackupPlanResponse_backupPlanArn :: Lens.Lens' DeleteBackupPlanResponse (Prelude.Maybe Prelude.Text)
deleteBackupPlanResponse_backupPlanArn = Lens.lens (\DeleteBackupPlanResponse' {backupPlanArn} -> backupPlanArn) (\s@DeleteBackupPlanResponse' {} a -> s {backupPlanArn = a} :: DeleteBackupPlanResponse)

-- | Uniquely identifies a backup plan.
deleteBackupPlanResponse_backupPlanId :: Lens.Lens' DeleteBackupPlanResponse (Prelude.Maybe Prelude.Text)
deleteBackupPlanResponse_backupPlanId = Lens.lens (\DeleteBackupPlanResponse' {backupPlanId} -> backupPlanId) (\s@DeleteBackupPlanResponse' {} a -> s {backupPlanId = a} :: DeleteBackupPlanResponse)

-- | The date and time a backup plan is deleted, in Unix format and
-- Coordinated Universal Time (UTC). The value of @DeletionDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
deleteBackupPlanResponse_deletionDate :: Lens.Lens' DeleteBackupPlanResponse (Prelude.Maybe Prelude.UTCTime)
deleteBackupPlanResponse_deletionDate = Lens.lens (\DeleteBackupPlanResponse' {deletionDate} -> deletionDate) (\s@DeleteBackupPlanResponse' {} a -> s {deletionDate = a} :: DeleteBackupPlanResponse) Prelude.. Lens.mapping Core._Time

-- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version IDs cannot be edited.
deleteBackupPlanResponse_versionId :: Lens.Lens' DeleteBackupPlanResponse (Prelude.Maybe Prelude.Text)
deleteBackupPlanResponse_versionId = Lens.lens (\DeleteBackupPlanResponse' {versionId} -> versionId) (\s@DeleteBackupPlanResponse' {} a -> s {versionId = a} :: DeleteBackupPlanResponse)

-- | The response's http status code.
deleteBackupPlanResponse_httpStatus :: Lens.Lens' DeleteBackupPlanResponse Prelude.Int
deleteBackupPlanResponse_httpStatus = Lens.lens (\DeleteBackupPlanResponse' {httpStatus} -> httpStatus) (\s@DeleteBackupPlanResponse' {} a -> s {httpStatus = a} :: DeleteBackupPlanResponse)

instance Prelude.NFData DeleteBackupPlanResponse where
  rnf DeleteBackupPlanResponse' {..} =
    Prelude.rnf backupPlanArn
      `Prelude.seq` Prelude.rnf backupPlanId
      `Prelude.seq` Prelude.rnf deletionDate
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus
