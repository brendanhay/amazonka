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
-- Module      : Network.AWS.Backup.Types.AdvancedBackupSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Backup.Types.AdvancedBackupSetting where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON AdvancedBackupSetting where
  parseJSON =
    Core.withObject
      "AdvancedBackupSetting"
      ( \x ->
          AdvancedBackupSetting'
            Prelude.<$> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "BackupOptions" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable AdvancedBackupSetting

instance Prelude.NFData AdvancedBackupSetting

instance Core.ToJSON AdvancedBackupSetting where
  toJSON AdvancedBackupSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResourceType" Core..=) Prelude.<$> resourceType,
            ("BackupOptions" Core..=) Prelude.<$> backupOptions
          ]
      )
