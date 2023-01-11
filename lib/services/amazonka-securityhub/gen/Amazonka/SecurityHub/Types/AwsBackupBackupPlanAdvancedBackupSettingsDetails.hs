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
-- Module      : Amazonka.SecurityHub.Types.AwsBackupBackupPlanAdvancedBackupSettingsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsBackupBackupPlanAdvancedBackupSettingsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a list of backup options for each resource type.
--
-- /See:/ 'newAwsBackupBackupPlanAdvancedBackupSettingsDetails' smart constructor.
data AwsBackupBackupPlanAdvancedBackupSettingsDetails = AwsBackupBackupPlanAdvancedBackupSettingsDetails'
  { -- | Specifies the backup option for a selected resource. This option is only
    -- available for Windows Volume Shadow Copy Service (VSS) backup jobs.
    -- Valid values are as follows:
    --
    -- -   Set to @WindowsVSS: enabled@ to enable the WindowsVSS backup option
    --     and create a Windows VSS backup.
    --
    -- -   Set to @WindowsVSS: disabled@ to create a regular backup. The
    --     @WindowsVSS@ option is not enabled by default.
    backupOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of a resource type. The only supported resource type is Amazon
    -- EC2 instances with Windows VSS.
    --
    -- The only valid value is @EC2@.
    resourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsBackupBackupPlanAdvancedBackupSettingsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupOptions', 'awsBackupBackupPlanAdvancedBackupSettingsDetails_backupOptions' - Specifies the backup option for a selected resource. This option is only
-- available for Windows Volume Shadow Copy Service (VSS) backup jobs.
-- Valid values are as follows:
--
-- -   Set to @WindowsVSS: enabled@ to enable the WindowsVSS backup option
--     and create a Windows VSS backup.
--
-- -   Set to @WindowsVSS: disabled@ to create a regular backup. The
--     @WindowsVSS@ option is not enabled by default.
--
-- 'resourceType', 'awsBackupBackupPlanAdvancedBackupSettingsDetails_resourceType' - The name of a resource type. The only supported resource type is Amazon
-- EC2 instances with Windows VSS.
--
-- The only valid value is @EC2@.
newAwsBackupBackupPlanAdvancedBackupSettingsDetails ::
  AwsBackupBackupPlanAdvancedBackupSettingsDetails
newAwsBackupBackupPlanAdvancedBackupSettingsDetails =
  AwsBackupBackupPlanAdvancedBackupSettingsDetails'
    { backupOptions =
        Prelude.Nothing,
      resourceType =
        Prelude.Nothing
    }

-- | Specifies the backup option for a selected resource. This option is only
-- available for Windows Volume Shadow Copy Service (VSS) backup jobs.
-- Valid values are as follows:
--
-- -   Set to @WindowsVSS: enabled@ to enable the WindowsVSS backup option
--     and create a Windows VSS backup.
--
-- -   Set to @WindowsVSS: disabled@ to create a regular backup. The
--     @WindowsVSS@ option is not enabled by default.
awsBackupBackupPlanAdvancedBackupSettingsDetails_backupOptions :: Lens.Lens' AwsBackupBackupPlanAdvancedBackupSettingsDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsBackupBackupPlanAdvancedBackupSettingsDetails_backupOptions = Lens.lens (\AwsBackupBackupPlanAdvancedBackupSettingsDetails' {backupOptions} -> backupOptions) (\s@AwsBackupBackupPlanAdvancedBackupSettingsDetails' {} a -> s {backupOptions = a} :: AwsBackupBackupPlanAdvancedBackupSettingsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of a resource type. The only supported resource type is Amazon
-- EC2 instances with Windows VSS.
--
-- The only valid value is @EC2@.
awsBackupBackupPlanAdvancedBackupSettingsDetails_resourceType :: Lens.Lens' AwsBackupBackupPlanAdvancedBackupSettingsDetails (Prelude.Maybe Prelude.Text)
awsBackupBackupPlanAdvancedBackupSettingsDetails_resourceType = Lens.lens (\AwsBackupBackupPlanAdvancedBackupSettingsDetails' {resourceType} -> resourceType) (\s@AwsBackupBackupPlanAdvancedBackupSettingsDetails' {} a -> s {resourceType = a} :: AwsBackupBackupPlanAdvancedBackupSettingsDetails)

instance
  Data.FromJSON
    AwsBackupBackupPlanAdvancedBackupSettingsDetails
  where
  parseJSON =
    Data.withObject
      "AwsBackupBackupPlanAdvancedBackupSettingsDetails"
      ( \x ->
          AwsBackupBackupPlanAdvancedBackupSettingsDetails'
            Prelude.<$> (x Data..:? "BackupOptions" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "ResourceType")
      )

instance
  Prelude.Hashable
    AwsBackupBackupPlanAdvancedBackupSettingsDetails
  where
  hashWithSalt
    _salt
    AwsBackupBackupPlanAdvancedBackupSettingsDetails' {..} =
      _salt `Prelude.hashWithSalt` backupOptions
        `Prelude.hashWithSalt` resourceType

instance
  Prelude.NFData
    AwsBackupBackupPlanAdvancedBackupSettingsDetails
  where
  rnf
    AwsBackupBackupPlanAdvancedBackupSettingsDetails' {..} =
      Prelude.rnf backupOptions
        `Prelude.seq` Prelude.rnf resourceType

instance
  Data.ToJSON
    AwsBackupBackupPlanAdvancedBackupSettingsDetails
  where
  toJSON
    AwsBackupBackupPlanAdvancedBackupSettingsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("BackupOptions" Data..=) Prelude.<$> backupOptions,
              ("ResourceType" Data..=) Prelude.<$> resourceType
            ]
        )
