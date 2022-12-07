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
-- Module      : Amazonka.Backup.Types.AdvancedBackupSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.AdvancedBackupSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of backup options for each resource type.
--
-- /See:/ 'newAdvancedBackupSetting' smart constructor.
data AdvancedBackupSetting = AdvancedBackupSetting'
  { -- | Specifies an object containing resource type and backup options. The
    -- only supported resource type is Amazon EC2 instances with Windows Volume
    -- Shadow Copy Service (VSS). For a CloudFormation example, see the
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/integrate-cloudformation-with-aws-backup.html sample CloudFormation template to enable Windows VSS>
    -- in the /Backup User Guide/.
    --
    -- Valid values: @EC2@.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | Specifies the backup option for a selected resource. This option is only
    -- available for Windows VSS backup jobs.
    --
    -- Valid values:
    --
    -- Set to @\"WindowsVSS\":\"enabled\"@ to enable the @WindowsVSS@ backup
    -- option and create a Windows VSS backup.
    --
    -- Set to @\"WindowsVSS\":\"disabled\"@ to create a regular backup. The
    -- @WindowsVSS@ option is not enabled by default.
    --
    -- If you specify an invalid option, you get an
    -- @InvalidParameterValueException@ exception.
    --
    -- For more information about Windows VSS backups, see
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/windows-backups.html Creating a VSS-Enabled Windows Backup>.
    backupOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdvancedBackupSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'advancedBackupSetting_resourceType' - Specifies an object containing resource type and backup options. The
-- only supported resource type is Amazon EC2 instances with Windows Volume
-- Shadow Copy Service (VSS). For a CloudFormation example, see the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/integrate-cloudformation-with-aws-backup.html sample CloudFormation template to enable Windows VSS>
-- in the /Backup User Guide/.
--
-- Valid values: @EC2@.
--
-- 'backupOptions', 'advancedBackupSetting_backupOptions' - Specifies the backup option for a selected resource. This option is only
-- available for Windows VSS backup jobs.
--
-- Valid values:
--
-- Set to @\"WindowsVSS\":\"enabled\"@ to enable the @WindowsVSS@ backup
-- option and create a Windows VSS backup.
--
-- Set to @\"WindowsVSS\":\"disabled\"@ to create a regular backup. The
-- @WindowsVSS@ option is not enabled by default.
--
-- If you specify an invalid option, you get an
-- @InvalidParameterValueException@ exception.
--
-- For more information about Windows VSS backups, see
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/windows-backups.html Creating a VSS-Enabled Windows Backup>.
newAdvancedBackupSetting ::
  AdvancedBackupSetting
newAdvancedBackupSetting =
  AdvancedBackupSetting'
    { resourceType =
        Prelude.Nothing,
      backupOptions = Prelude.Nothing
    }

-- | Specifies an object containing resource type and backup options. The
-- only supported resource type is Amazon EC2 instances with Windows Volume
-- Shadow Copy Service (VSS). For a CloudFormation example, see the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/integrate-cloudformation-with-aws-backup.html sample CloudFormation template to enable Windows VSS>
-- in the /Backup User Guide/.
--
-- Valid values: @EC2@.
advancedBackupSetting_resourceType :: Lens.Lens' AdvancedBackupSetting (Prelude.Maybe Prelude.Text)
advancedBackupSetting_resourceType = Lens.lens (\AdvancedBackupSetting' {resourceType} -> resourceType) (\s@AdvancedBackupSetting' {} a -> s {resourceType = a} :: AdvancedBackupSetting)

-- | Specifies the backup option for a selected resource. This option is only
-- available for Windows VSS backup jobs.
--
-- Valid values:
--
-- Set to @\"WindowsVSS\":\"enabled\"@ to enable the @WindowsVSS@ backup
-- option and create a Windows VSS backup.
--
-- Set to @\"WindowsVSS\":\"disabled\"@ to create a regular backup. The
-- @WindowsVSS@ option is not enabled by default.
--
-- If you specify an invalid option, you get an
-- @InvalidParameterValueException@ exception.
--
-- For more information about Windows VSS backups, see
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/windows-backups.html Creating a VSS-Enabled Windows Backup>.
advancedBackupSetting_backupOptions :: Lens.Lens' AdvancedBackupSetting (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
advancedBackupSetting_backupOptions = Lens.lens (\AdvancedBackupSetting' {backupOptions} -> backupOptions) (\s@AdvancedBackupSetting' {} a -> s {backupOptions = a} :: AdvancedBackupSetting) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AdvancedBackupSetting where
  parseJSON =
    Data.withObject
      "AdvancedBackupSetting"
      ( \x ->
          AdvancedBackupSetting'
            Prelude.<$> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "BackupOptions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AdvancedBackupSetting where
  hashWithSalt _salt AdvancedBackupSetting' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` backupOptions

instance Prelude.NFData AdvancedBackupSetting where
  rnf AdvancedBackupSetting' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf backupOptions

instance Data.ToJSON AdvancedBackupSetting where
  toJSON AdvancedBackupSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceType" Data..=) Prelude.<$> resourceType,
            ("BackupOptions" Data..=) Prelude.<$> backupOptions
          ]
      )
