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
-- Module      : Amazonka.Backup.CreateBackupPlan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a backup plan using a backup plan name and backup rules. A
-- backup plan is a document that contains information that Backup uses to
-- schedule tasks that create recovery points for resources.
--
-- If you call @CreateBackupPlan@ with a plan that already exists, you
-- receive an @AlreadyExistsException@ exception.
module Amazonka.Backup.CreateBackupPlan
  ( -- * Creating a Request
    CreateBackupPlan (..),
    newCreateBackupPlan,

    -- * Request Lenses
    createBackupPlan_backupPlanTags,
    createBackupPlan_creatorRequestId,
    createBackupPlan_backupPlan,

    -- * Destructuring the Response
    CreateBackupPlanResponse (..),
    newCreateBackupPlanResponse,

    -- * Response Lenses
    createBackupPlanResponse_creationDate,
    createBackupPlanResponse_backupPlanArn,
    createBackupPlanResponse_backupPlanId,
    createBackupPlanResponse_advancedBackupSettings,
    createBackupPlanResponse_versionId,
    createBackupPlanResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBackupPlan' smart constructor.
data CreateBackupPlan = CreateBackupPlan'
  { -- | To help organize your resources, you can assign your own metadata to the
    -- resources that you create. Each tag is a key-value pair. The specified
    -- tags are assigned to all backups created with this plan.
    backupPlanTags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Identifies the request and allows failed requests to be retried without
    -- the risk of running the operation twice. If the request includes a
    -- @CreatorRequestId@ that matches an existing backup plan, that plan is
    -- returned. This parameter is optional.
    --
    -- If used, this parameter must contain 1 to 50 alphanumeric or \'-_.\'
    -- characters.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the body of a backup plan. Includes a @BackupPlanName@ and one
    -- or more sets of @Rules@.
    backupPlan :: BackupPlanInput
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackupPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPlanTags', 'createBackupPlan_backupPlanTags' - To help organize your resources, you can assign your own metadata to the
-- resources that you create. Each tag is a key-value pair. The specified
-- tags are assigned to all backups created with this plan.
--
-- 'creatorRequestId', 'createBackupPlan_creatorRequestId' - Identifies the request and allows failed requests to be retried without
-- the risk of running the operation twice. If the request includes a
-- @CreatorRequestId@ that matches an existing backup plan, that plan is
-- returned. This parameter is optional.
--
-- If used, this parameter must contain 1 to 50 alphanumeric or \'-_.\'
-- characters.
--
-- 'backupPlan', 'createBackupPlan_backupPlan' - Specifies the body of a backup plan. Includes a @BackupPlanName@ and one
-- or more sets of @Rules@.
newCreateBackupPlan ::
  -- | 'backupPlan'
  BackupPlanInput ->
  CreateBackupPlan
newCreateBackupPlan pBackupPlan_ =
  CreateBackupPlan'
    { backupPlanTags = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      backupPlan = pBackupPlan_
    }

-- | To help organize your resources, you can assign your own metadata to the
-- resources that you create. Each tag is a key-value pair. The specified
-- tags are assigned to all backups created with this plan.
createBackupPlan_backupPlanTags :: Lens.Lens' CreateBackupPlan (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createBackupPlan_backupPlanTags = Lens.lens (\CreateBackupPlan' {backupPlanTags} -> backupPlanTags) (\s@CreateBackupPlan' {} a -> s {backupPlanTags = a} :: CreateBackupPlan) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Identifies the request and allows failed requests to be retried without
-- the risk of running the operation twice. If the request includes a
-- @CreatorRequestId@ that matches an existing backup plan, that plan is
-- returned. This parameter is optional.
--
-- If used, this parameter must contain 1 to 50 alphanumeric or \'-_.\'
-- characters.
createBackupPlan_creatorRequestId :: Lens.Lens' CreateBackupPlan (Prelude.Maybe Prelude.Text)
createBackupPlan_creatorRequestId = Lens.lens (\CreateBackupPlan' {creatorRequestId} -> creatorRequestId) (\s@CreateBackupPlan' {} a -> s {creatorRequestId = a} :: CreateBackupPlan)

-- | Specifies the body of a backup plan. Includes a @BackupPlanName@ and one
-- or more sets of @Rules@.
createBackupPlan_backupPlan :: Lens.Lens' CreateBackupPlan BackupPlanInput
createBackupPlan_backupPlan = Lens.lens (\CreateBackupPlan' {backupPlan} -> backupPlan) (\s@CreateBackupPlan' {} a -> s {backupPlan = a} :: CreateBackupPlan)

instance Core.AWSRequest CreateBackupPlan where
  type
    AWSResponse CreateBackupPlan =
      CreateBackupPlanResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBackupPlanResponse'
            Prelude.<$> (x Data..?> "CreationDate")
            Prelude.<*> (x Data..?> "BackupPlanArn")
            Prelude.<*> (x Data..?> "BackupPlanId")
            Prelude.<*> ( x Data..?> "AdvancedBackupSettings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "VersionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBackupPlan where
  hashWithSalt _salt CreateBackupPlan' {..} =
    _salt `Prelude.hashWithSalt` backupPlanTags
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` backupPlan

instance Prelude.NFData CreateBackupPlan where
  rnf CreateBackupPlan' {..} =
    Prelude.rnf backupPlanTags
      `Prelude.seq` Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf backupPlan

instance Data.ToHeaders CreateBackupPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBackupPlan where
  toJSON CreateBackupPlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackupPlanTags" Data..=)
              Prelude.<$> backupPlanTags,
            ("CreatorRequestId" Data..=)
              Prelude.<$> creatorRequestId,
            Prelude.Just ("BackupPlan" Data..= backupPlan)
          ]
      )

instance Data.ToPath CreateBackupPlan where
  toPath = Prelude.const "/backup/plans/"

instance Data.ToQuery CreateBackupPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBackupPlanResponse' smart constructor.
data CreateBackupPlanResponse = CreateBackupPlanResponse'
  { -- | The date and time that a backup plan is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
    -- for example,
    -- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
    backupPlanArn :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Maybe Prelude.Text,
    -- | A list of @BackupOptions@ settings for a resource type. This option is
    -- only available for Windows Volume Shadow Copy Service (VSS) backup jobs.
    advancedBackupSettings :: Prelude.Maybe [AdvancedBackupSetting],
    -- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
    -- most 1,024 bytes long. They cannot be edited.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackupPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'createBackupPlanResponse_creationDate' - The date and time that a backup plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'backupPlanArn', 'createBackupPlanResponse_backupPlanArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
--
-- 'backupPlanId', 'createBackupPlanResponse_backupPlanId' - Uniquely identifies a backup plan.
--
-- 'advancedBackupSettings', 'createBackupPlanResponse_advancedBackupSettings' - A list of @BackupOptions@ settings for a resource type. This option is
-- only available for Windows Volume Shadow Copy Service (VSS) backup jobs.
--
-- 'versionId', 'createBackupPlanResponse_versionId' - Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. They cannot be edited.
--
-- 'httpStatus', 'createBackupPlanResponse_httpStatus' - The response's http status code.
newCreateBackupPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBackupPlanResponse
newCreateBackupPlanResponse pHttpStatus_ =
  CreateBackupPlanResponse'
    { creationDate =
        Prelude.Nothing,
      backupPlanArn = Prelude.Nothing,
      backupPlanId = Prelude.Nothing,
      advancedBackupSettings = Prelude.Nothing,
      versionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time that a backup plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
createBackupPlanResponse_creationDate :: Lens.Lens' CreateBackupPlanResponse (Prelude.Maybe Prelude.UTCTime)
createBackupPlanResponse_creationDate = Lens.lens (\CreateBackupPlanResponse' {creationDate} -> creationDate) (\s@CreateBackupPlanResponse' {} a -> s {creationDate = a} :: CreateBackupPlanResponse) Prelude.. Lens.mapping Data._Time

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
createBackupPlanResponse_backupPlanArn :: Lens.Lens' CreateBackupPlanResponse (Prelude.Maybe Prelude.Text)
createBackupPlanResponse_backupPlanArn = Lens.lens (\CreateBackupPlanResponse' {backupPlanArn} -> backupPlanArn) (\s@CreateBackupPlanResponse' {} a -> s {backupPlanArn = a} :: CreateBackupPlanResponse)

-- | Uniquely identifies a backup plan.
createBackupPlanResponse_backupPlanId :: Lens.Lens' CreateBackupPlanResponse (Prelude.Maybe Prelude.Text)
createBackupPlanResponse_backupPlanId = Lens.lens (\CreateBackupPlanResponse' {backupPlanId} -> backupPlanId) (\s@CreateBackupPlanResponse' {} a -> s {backupPlanId = a} :: CreateBackupPlanResponse)

-- | A list of @BackupOptions@ settings for a resource type. This option is
-- only available for Windows Volume Shadow Copy Service (VSS) backup jobs.
createBackupPlanResponse_advancedBackupSettings :: Lens.Lens' CreateBackupPlanResponse (Prelude.Maybe [AdvancedBackupSetting])
createBackupPlanResponse_advancedBackupSettings = Lens.lens (\CreateBackupPlanResponse' {advancedBackupSettings} -> advancedBackupSettings) (\s@CreateBackupPlanResponse' {} a -> s {advancedBackupSettings = a} :: CreateBackupPlanResponse) Prelude.. Lens.mapping Lens.coerced

-- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. They cannot be edited.
createBackupPlanResponse_versionId :: Lens.Lens' CreateBackupPlanResponse (Prelude.Maybe Prelude.Text)
createBackupPlanResponse_versionId = Lens.lens (\CreateBackupPlanResponse' {versionId} -> versionId) (\s@CreateBackupPlanResponse' {} a -> s {versionId = a} :: CreateBackupPlanResponse)

-- | The response's http status code.
createBackupPlanResponse_httpStatus :: Lens.Lens' CreateBackupPlanResponse Prelude.Int
createBackupPlanResponse_httpStatus = Lens.lens (\CreateBackupPlanResponse' {httpStatus} -> httpStatus) (\s@CreateBackupPlanResponse' {} a -> s {httpStatus = a} :: CreateBackupPlanResponse)

instance Prelude.NFData CreateBackupPlanResponse where
  rnf CreateBackupPlanResponse' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf backupPlanArn
      `Prelude.seq` Prelude.rnf backupPlanId
      `Prelude.seq` Prelude.rnf advancedBackupSettings
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus
