{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.Types.AwsBackupBackupPlanDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsBackupBackupPlanDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanBackupPlanDetails

-- | Provides details about an Backup backup plan and an array of
-- @BackupRule@ objects, each of which specifies a backup rule.
--
-- /See:/ 'newAwsBackupBackupPlanDetails' smart constructor.
data AwsBackupBackupPlanDetails = AwsBackupBackupPlanDetails'
  { -- | Uniquely identifies the backup plan to be associated with the selection
    -- of resources.
    backupPlan :: Prelude.Maybe AwsBackupBackupPlanBackupPlanDetails,
    -- | An Amazon Resource Name (ARN) that uniquely identifies the backup plan.
    backupPlanArn :: Prelude.Maybe Prelude.Text,
    -- | A unique ID for the backup plan.
    backupPlanId :: Prelude.Maybe Prelude.Text,
    -- | Unique, randomly generated, Unicode, UTF-8 encoded strings. Version IDs
    -- cannot be edited.
    versionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsBackupBackupPlanDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPlan', 'awsBackupBackupPlanDetails_backupPlan' - Uniquely identifies the backup plan to be associated with the selection
-- of resources.
--
-- 'backupPlanArn', 'awsBackupBackupPlanDetails_backupPlanArn' - An Amazon Resource Name (ARN) that uniquely identifies the backup plan.
--
-- 'backupPlanId', 'awsBackupBackupPlanDetails_backupPlanId' - A unique ID for the backup plan.
--
-- 'versionId', 'awsBackupBackupPlanDetails_versionId' - Unique, randomly generated, Unicode, UTF-8 encoded strings. Version IDs
-- cannot be edited.
newAwsBackupBackupPlanDetails ::
  AwsBackupBackupPlanDetails
newAwsBackupBackupPlanDetails =
  AwsBackupBackupPlanDetails'
    { backupPlan =
        Prelude.Nothing,
      backupPlanArn = Prelude.Nothing,
      backupPlanId = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | Uniquely identifies the backup plan to be associated with the selection
-- of resources.
awsBackupBackupPlanDetails_backupPlan :: Lens.Lens' AwsBackupBackupPlanDetails (Prelude.Maybe AwsBackupBackupPlanBackupPlanDetails)
awsBackupBackupPlanDetails_backupPlan = Lens.lens (\AwsBackupBackupPlanDetails' {backupPlan} -> backupPlan) (\s@AwsBackupBackupPlanDetails' {} a -> s {backupPlan = a} :: AwsBackupBackupPlanDetails)

-- | An Amazon Resource Name (ARN) that uniquely identifies the backup plan.
awsBackupBackupPlanDetails_backupPlanArn :: Lens.Lens' AwsBackupBackupPlanDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupPlanDetails_backupPlanArn = Lens.lens (\AwsBackupBackupPlanDetails' {backupPlanArn} -> backupPlanArn) (\s@AwsBackupBackupPlanDetails' {} a -> s {backupPlanArn = a} :: AwsBackupBackupPlanDetails)

-- | A unique ID for the backup plan.
awsBackupBackupPlanDetails_backupPlanId :: Lens.Lens' AwsBackupBackupPlanDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupPlanDetails_backupPlanId = Lens.lens (\AwsBackupBackupPlanDetails' {backupPlanId} -> backupPlanId) (\s@AwsBackupBackupPlanDetails' {} a -> s {backupPlanId = a} :: AwsBackupBackupPlanDetails)

-- | Unique, randomly generated, Unicode, UTF-8 encoded strings. Version IDs
-- cannot be edited.
awsBackupBackupPlanDetails_versionId :: Lens.Lens' AwsBackupBackupPlanDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupPlanDetails_versionId = Lens.lens (\AwsBackupBackupPlanDetails' {versionId} -> versionId) (\s@AwsBackupBackupPlanDetails' {} a -> s {versionId = a} :: AwsBackupBackupPlanDetails)

instance Data.FromJSON AwsBackupBackupPlanDetails where
  parseJSON =
    Data.withObject
      "AwsBackupBackupPlanDetails"
      ( \x ->
          AwsBackupBackupPlanDetails'
            Prelude.<$> (x Data..:? "BackupPlan")
            Prelude.<*> (x Data..:? "BackupPlanArn")
            Prelude.<*> (x Data..:? "BackupPlanId")
            Prelude.<*> (x Data..:? "VersionId")
      )

instance Prelude.Hashable AwsBackupBackupPlanDetails where
  hashWithSalt _salt AwsBackupBackupPlanDetails' {..} =
    _salt `Prelude.hashWithSalt` backupPlan
      `Prelude.hashWithSalt` backupPlanArn
      `Prelude.hashWithSalt` backupPlanId
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData AwsBackupBackupPlanDetails where
  rnf AwsBackupBackupPlanDetails' {..} =
    Prelude.rnf backupPlan
      `Prelude.seq` Prelude.rnf backupPlanArn
      `Prelude.seq` Prelude.rnf backupPlanId
      `Prelude.seq` Prelude.rnf versionId

instance Data.ToJSON AwsBackupBackupPlanDetails where
  toJSON AwsBackupBackupPlanDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackupPlan" Data..=) Prelude.<$> backupPlan,
            ("BackupPlanArn" Data..=) Prelude.<$> backupPlanArn,
            ("BackupPlanId" Data..=) Prelude.<$> backupPlanId,
            ("VersionId" Data..=) Prelude.<$> versionId
          ]
      )
