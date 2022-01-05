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
-- Module      : Amazonka.Backup.UpdateBackupPlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing backup plan identified by its @backupPlanId@ with
-- the input document in JSON format. The new version is uniquely
-- identified by a @VersionId@.
module Amazonka.Backup.UpdateBackupPlan
  ( -- * Creating a Request
    UpdateBackupPlan (..),
    newUpdateBackupPlan,

    -- * Request Lenses
    updateBackupPlan_backupPlanId,
    updateBackupPlan_backupPlan,

    -- * Destructuring the Response
    UpdateBackupPlanResponse (..),
    newUpdateBackupPlanResponse,

    -- * Response Lenses
    updateBackupPlanResponse_versionId,
    updateBackupPlanResponse_advancedBackupSettings,
    updateBackupPlanResponse_backupPlanId,
    updateBackupPlanResponse_backupPlanArn,
    updateBackupPlanResponse_creationDate,
    updateBackupPlanResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateBackupPlan' smart constructor.
data UpdateBackupPlan = UpdateBackupPlan'
  { -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Text,
    -- | Specifies the body of a backup plan. Includes a @BackupPlanName@ and one
    -- or more sets of @Rules@.
    backupPlan :: BackupPlanInput
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackupPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPlanId', 'updateBackupPlan_backupPlanId' - Uniquely identifies a backup plan.
--
-- 'backupPlan', 'updateBackupPlan_backupPlan' - Specifies the body of a backup plan. Includes a @BackupPlanName@ and one
-- or more sets of @Rules@.
newUpdateBackupPlan ::
  -- | 'backupPlanId'
  Prelude.Text ->
  -- | 'backupPlan'
  BackupPlanInput ->
  UpdateBackupPlan
newUpdateBackupPlan pBackupPlanId_ pBackupPlan_ =
  UpdateBackupPlan'
    { backupPlanId = pBackupPlanId_,
      backupPlan = pBackupPlan_
    }

-- | Uniquely identifies a backup plan.
updateBackupPlan_backupPlanId :: Lens.Lens' UpdateBackupPlan Prelude.Text
updateBackupPlan_backupPlanId = Lens.lens (\UpdateBackupPlan' {backupPlanId} -> backupPlanId) (\s@UpdateBackupPlan' {} a -> s {backupPlanId = a} :: UpdateBackupPlan)

-- | Specifies the body of a backup plan. Includes a @BackupPlanName@ and one
-- or more sets of @Rules@.
updateBackupPlan_backupPlan :: Lens.Lens' UpdateBackupPlan BackupPlanInput
updateBackupPlan_backupPlan = Lens.lens (\UpdateBackupPlan' {backupPlan} -> backupPlan) (\s@UpdateBackupPlan' {} a -> s {backupPlan = a} :: UpdateBackupPlan)

instance Core.AWSRequest UpdateBackupPlan where
  type
    AWSResponse UpdateBackupPlan =
      UpdateBackupPlanResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBackupPlanResponse'
            Prelude.<$> (x Core..?> "VersionId")
            Prelude.<*> ( x Core..?> "AdvancedBackupSettings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "BackupPlanId")
            Prelude.<*> (x Core..?> "BackupPlanArn")
            Prelude.<*> (x Core..?> "CreationDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBackupPlan where
  hashWithSalt _salt UpdateBackupPlan' {..} =
    _salt `Prelude.hashWithSalt` backupPlanId
      `Prelude.hashWithSalt` backupPlan

instance Prelude.NFData UpdateBackupPlan where
  rnf UpdateBackupPlan' {..} =
    Prelude.rnf backupPlanId
      `Prelude.seq` Prelude.rnf backupPlan

instance Core.ToHeaders UpdateBackupPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateBackupPlan where
  toJSON UpdateBackupPlan' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("BackupPlan" Core..= backupPlan)]
      )

instance Core.ToPath UpdateBackupPlan where
  toPath UpdateBackupPlan' {..} =
    Prelude.mconcat
      ["/backup/plans/", Core.toBS backupPlanId]

instance Core.ToQuery UpdateBackupPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBackupPlanResponse' smart constructor.
data UpdateBackupPlanResponse = UpdateBackupPlanResponse'
  { -- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
    -- most 1,024 bytes long. Version Ids cannot be edited.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | Contains a list of @BackupOptions@ for each resource type.
    advancedBackupSettings :: Prelude.Maybe [AdvancedBackupSetting],
    -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
    -- for example,
    -- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
    backupPlanArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time a backup plan is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBackupPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'updateBackupPlanResponse_versionId' - Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version Ids cannot be edited.
--
-- 'advancedBackupSettings', 'updateBackupPlanResponse_advancedBackupSettings' - Contains a list of @BackupOptions@ for each resource type.
--
-- 'backupPlanId', 'updateBackupPlanResponse_backupPlanId' - Uniquely identifies a backup plan.
--
-- 'backupPlanArn', 'updateBackupPlanResponse_backupPlanArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
--
-- 'creationDate', 'updateBackupPlanResponse_creationDate' - The date and time a backup plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'httpStatus', 'updateBackupPlanResponse_httpStatus' - The response's http status code.
newUpdateBackupPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBackupPlanResponse
newUpdateBackupPlanResponse pHttpStatus_ =
  UpdateBackupPlanResponse'
    { versionId =
        Prelude.Nothing,
      advancedBackupSettings = Prelude.Nothing,
      backupPlanId = Prelude.Nothing,
      backupPlanArn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version Ids cannot be edited.
updateBackupPlanResponse_versionId :: Lens.Lens' UpdateBackupPlanResponse (Prelude.Maybe Prelude.Text)
updateBackupPlanResponse_versionId = Lens.lens (\UpdateBackupPlanResponse' {versionId} -> versionId) (\s@UpdateBackupPlanResponse' {} a -> s {versionId = a} :: UpdateBackupPlanResponse)

-- | Contains a list of @BackupOptions@ for each resource type.
updateBackupPlanResponse_advancedBackupSettings :: Lens.Lens' UpdateBackupPlanResponse (Prelude.Maybe [AdvancedBackupSetting])
updateBackupPlanResponse_advancedBackupSettings = Lens.lens (\UpdateBackupPlanResponse' {advancedBackupSettings} -> advancedBackupSettings) (\s@UpdateBackupPlanResponse' {} a -> s {advancedBackupSettings = a} :: UpdateBackupPlanResponse) Prelude.. Lens.mapping Lens.coerced

-- | Uniquely identifies a backup plan.
updateBackupPlanResponse_backupPlanId :: Lens.Lens' UpdateBackupPlanResponse (Prelude.Maybe Prelude.Text)
updateBackupPlanResponse_backupPlanId = Lens.lens (\UpdateBackupPlanResponse' {backupPlanId} -> backupPlanId) (\s@UpdateBackupPlanResponse' {} a -> s {backupPlanId = a} :: UpdateBackupPlanResponse)

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
updateBackupPlanResponse_backupPlanArn :: Lens.Lens' UpdateBackupPlanResponse (Prelude.Maybe Prelude.Text)
updateBackupPlanResponse_backupPlanArn = Lens.lens (\UpdateBackupPlanResponse' {backupPlanArn} -> backupPlanArn) (\s@UpdateBackupPlanResponse' {} a -> s {backupPlanArn = a} :: UpdateBackupPlanResponse)

-- | The date and time a backup plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
updateBackupPlanResponse_creationDate :: Lens.Lens' UpdateBackupPlanResponse (Prelude.Maybe Prelude.UTCTime)
updateBackupPlanResponse_creationDate = Lens.lens (\UpdateBackupPlanResponse' {creationDate} -> creationDate) (\s@UpdateBackupPlanResponse' {} a -> s {creationDate = a} :: UpdateBackupPlanResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
updateBackupPlanResponse_httpStatus :: Lens.Lens' UpdateBackupPlanResponse Prelude.Int
updateBackupPlanResponse_httpStatus = Lens.lens (\UpdateBackupPlanResponse' {httpStatus} -> httpStatus) (\s@UpdateBackupPlanResponse' {} a -> s {httpStatus = a} :: UpdateBackupPlanResponse)

instance Prelude.NFData UpdateBackupPlanResponse where
  rnf UpdateBackupPlanResponse' {..} =
    Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf advancedBackupSettings
      `Prelude.seq` Prelude.rnf backupPlanId
      `Prelude.seq` Prelude.rnf backupPlanArn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf httpStatus
