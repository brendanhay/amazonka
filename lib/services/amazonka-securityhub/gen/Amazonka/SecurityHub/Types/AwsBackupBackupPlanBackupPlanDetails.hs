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
-- Module      : Amazonka.SecurityHub.Types.AwsBackupBackupPlanBackupPlanDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsBackupBackupPlanBackupPlanDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanAdvancedBackupSettingsDetails
import Amazonka.SecurityHub.Types.AwsBackupBackupPlanRuleDetails

-- | Provides details about an Backup backup plan and an array of
-- @BackupRule@ objects, each of which specifies a backup rule.
--
-- /See:/ 'newAwsBackupBackupPlanBackupPlanDetails' smart constructor.
data AwsBackupBackupPlanBackupPlanDetails = AwsBackupBackupPlanBackupPlanDetails'
  { -- | A list of backup options for each resource type.
    advancedBackupSettings :: Prelude.Maybe [AwsBackupBackupPlanAdvancedBackupSettingsDetails],
    -- | The display name of a backup plan.
    backupPlanName :: Prelude.Maybe Prelude.Text,
    -- | An array of @BackupRule@ objects, each of which specifies a scheduled
    -- task that is used to back up a selection of resources.
    backupPlanRule :: Prelude.Maybe [AwsBackupBackupPlanRuleDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsBackupBackupPlanBackupPlanDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'advancedBackupSettings', 'awsBackupBackupPlanBackupPlanDetails_advancedBackupSettings' - A list of backup options for each resource type.
--
-- 'backupPlanName', 'awsBackupBackupPlanBackupPlanDetails_backupPlanName' - The display name of a backup plan.
--
-- 'backupPlanRule', 'awsBackupBackupPlanBackupPlanDetails_backupPlanRule' - An array of @BackupRule@ objects, each of which specifies a scheduled
-- task that is used to back up a selection of resources.
newAwsBackupBackupPlanBackupPlanDetails ::
  AwsBackupBackupPlanBackupPlanDetails
newAwsBackupBackupPlanBackupPlanDetails =
  AwsBackupBackupPlanBackupPlanDetails'
    { advancedBackupSettings =
        Prelude.Nothing,
      backupPlanName = Prelude.Nothing,
      backupPlanRule = Prelude.Nothing
    }

-- | A list of backup options for each resource type.
awsBackupBackupPlanBackupPlanDetails_advancedBackupSettings :: Lens.Lens' AwsBackupBackupPlanBackupPlanDetails (Prelude.Maybe [AwsBackupBackupPlanAdvancedBackupSettingsDetails])
awsBackupBackupPlanBackupPlanDetails_advancedBackupSettings = Lens.lens (\AwsBackupBackupPlanBackupPlanDetails' {advancedBackupSettings} -> advancedBackupSettings) (\s@AwsBackupBackupPlanBackupPlanDetails' {} a -> s {advancedBackupSettings = a} :: AwsBackupBackupPlanBackupPlanDetails) Prelude.. Lens.mapping Lens.coerced

-- | The display name of a backup plan.
awsBackupBackupPlanBackupPlanDetails_backupPlanName :: Lens.Lens' AwsBackupBackupPlanBackupPlanDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupPlanBackupPlanDetails_backupPlanName = Lens.lens (\AwsBackupBackupPlanBackupPlanDetails' {backupPlanName} -> backupPlanName) (\s@AwsBackupBackupPlanBackupPlanDetails' {} a -> s {backupPlanName = a} :: AwsBackupBackupPlanBackupPlanDetails)

-- | An array of @BackupRule@ objects, each of which specifies a scheduled
-- task that is used to back up a selection of resources.
awsBackupBackupPlanBackupPlanDetails_backupPlanRule :: Lens.Lens' AwsBackupBackupPlanBackupPlanDetails (Prelude.Maybe [AwsBackupBackupPlanRuleDetails])
awsBackupBackupPlanBackupPlanDetails_backupPlanRule = Lens.lens (\AwsBackupBackupPlanBackupPlanDetails' {backupPlanRule} -> backupPlanRule) (\s@AwsBackupBackupPlanBackupPlanDetails' {} a -> s {backupPlanRule = a} :: AwsBackupBackupPlanBackupPlanDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsBackupBackupPlanBackupPlanDetails
  where
  parseJSON =
    Data.withObject
      "AwsBackupBackupPlanBackupPlanDetails"
      ( \x ->
          AwsBackupBackupPlanBackupPlanDetails'
            Prelude.<$> ( x Data..:? "AdvancedBackupSettings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "BackupPlanName")
            Prelude.<*> ( x Data..:? "BackupPlanRule"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AwsBackupBackupPlanBackupPlanDetails
  where
  hashWithSalt
    _salt
    AwsBackupBackupPlanBackupPlanDetails' {..} =
      _salt `Prelude.hashWithSalt` advancedBackupSettings
        `Prelude.hashWithSalt` backupPlanName
        `Prelude.hashWithSalt` backupPlanRule

instance
  Prelude.NFData
    AwsBackupBackupPlanBackupPlanDetails
  where
  rnf AwsBackupBackupPlanBackupPlanDetails' {..} =
    Prelude.rnf advancedBackupSettings
      `Prelude.seq` Prelude.rnf backupPlanName
      `Prelude.seq` Prelude.rnf backupPlanRule

instance
  Data.ToJSON
    AwsBackupBackupPlanBackupPlanDetails
  where
  toJSON AwsBackupBackupPlanBackupPlanDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdvancedBackupSettings" Data..=)
              Prelude.<$> advancedBackupSettings,
            ("BackupPlanName" Data..=)
              Prelude.<$> backupPlanName,
            ("BackupPlanRule" Data..=)
              Prelude.<$> backupPlanRule
          ]
      )
