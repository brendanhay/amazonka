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
-- Module      : Amazonka.Backup.Types.BackupPlanInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.BackupPlanInput where

import Amazonka.Backup.Types.AdvancedBackupSetting
import Amazonka.Backup.Types.BackupRuleInput
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains an optional backup plan display name and an array of
-- @BackupRule@ objects, each of which specifies a backup rule. Each rule
-- in a backup plan is a separate scheduled task.
--
-- /See:/ 'newBackupPlanInput' smart constructor.
data BackupPlanInput = BackupPlanInput'
  { -- | Specifies a list of @BackupOptions@ for each resource type. These
    -- settings are only available for Windows Volume Shadow Copy Service (VSS)
    -- backup jobs.
    advancedBackupSettings :: Prelude.Maybe [AdvancedBackupSetting],
    -- | The display name of a backup plan. Must contain 1 to 50 alphanumeric or
    -- \'-_.\' characters.
    backupPlanName :: Prelude.Text,
    -- | An array of @BackupRule@ objects, each of which specifies a scheduled
    -- task that is used to back up a selection of resources.
    rules :: [BackupRuleInput]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackupPlanInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'advancedBackupSettings', 'backupPlanInput_advancedBackupSettings' - Specifies a list of @BackupOptions@ for each resource type. These
-- settings are only available for Windows Volume Shadow Copy Service (VSS)
-- backup jobs.
--
-- 'backupPlanName', 'backupPlanInput_backupPlanName' - The display name of a backup plan. Must contain 1 to 50 alphanumeric or
-- \'-_.\' characters.
--
-- 'rules', 'backupPlanInput_rules' - An array of @BackupRule@ objects, each of which specifies a scheduled
-- task that is used to back up a selection of resources.
newBackupPlanInput ::
  -- | 'backupPlanName'
  Prelude.Text ->
  BackupPlanInput
newBackupPlanInput pBackupPlanName_ =
  BackupPlanInput'
    { advancedBackupSettings =
        Prelude.Nothing,
      backupPlanName = pBackupPlanName_,
      rules = Prelude.mempty
    }

-- | Specifies a list of @BackupOptions@ for each resource type. These
-- settings are only available for Windows Volume Shadow Copy Service (VSS)
-- backup jobs.
backupPlanInput_advancedBackupSettings :: Lens.Lens' BackupPlanInput (Prelude.Maybe [AdvancedBackupSetting])
backupPlanInput_advancedBackupSettings = Lens.lens (\BackupPlanInput' {advancedBackupSettings} -> advancedBackupSettings) (\s@BackupPlanInput' {} a -> s {advancedBackupSettings = a} :: BackupPlanInput) Prelude.. Lens.mapping Lens.coerced

-- | The display name of a backup plan. Must contain 1 to 50 alphanumeric or
-- \'-_.\' characters.
backupPlanInput_backupPlanName :: Lens.Lens' BackupPlanInput Prelude.Text
backupPlanInput_backupPlanName = Lens.lens (\BackupPlanInput' {backupPlanName} -> backupPlanName) (\s@BackupPlanInput' {} a -> s {backupPlanName = a} :: BackupPlanInput)

-- | An array of @BackupRule@ objects, each of which specifies a scheduled
-- task that is used to back up a selection of resources.
backupPlanInput_rules :: Lens.Lens' BackupPlanInput [BackupRuleInput]
backupPlanInput_rules = Lens.lens (\BackupPlanInput' {rules} -> rules) (\s@BackupPlanInput' {} a -> s {rules = a} :: BackupPlanInput) Prelude.. Lens.coerced

instance Prelude.Hashable BackupPlanInput where
  hashWithSalt _salt BackupPlanInput' {..} =
    _salt `Prelude.hashWithSalt` advancedBackupSettings
      `Prelude.hashWithSalt` backupPlanName
      `Prelude.hashWithSalt` rules

instance Prelude.NFData BackupPlanInput where
  rnf BackupPlanInput' {..} =
    Prelude.rnf advancedBackupSettings
      `Prelude.seq` Prelude.rnf backupPlanName
      `Prelude.seq` Prelude.rnf rules

instance Core.ToJSON BackupPlanInput where
  toJSON BackupPlanInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AdvancedBackupSettings" Core..=)
              Prelude.<$> advancedBackupSettings,
            Prelude.Just
              ("BackupPlanName" Core..= backupPlanName),
            Prelude.Just ("Rules" Core..= rules)
          ]
      )
