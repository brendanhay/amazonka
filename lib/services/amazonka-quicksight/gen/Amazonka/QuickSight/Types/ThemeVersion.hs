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
-- Module      : Amazonka.QuickSight.Types.ThemeVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ThemeVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ResourceStatus
import Amazonka.QuickSight.Types.ThemeConfiguration
import Amazonka.QuickSight.Types.ThemeError

-- | A version of a theme.
--
-- /See:/ 'newThemeVersion' smart constructor.
data ThemeVersion = ThemeVersion'
  { -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon QuickSight-defined ID of the theme that a custom theme
    -- inherits from. All themes initially inherit from a default Amazon
    -- QuickSight theme.
    baseThemeId :: Prelude.Maybe Prelude.Text,
    -- | The theme configuration, which contains all the theme display
    -- properties.
    configuration :: Prelude.Maybe ThemeConfiguration,
    -- | The date and time that this theme version was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the theme.
    description :: Prelude.Maybe Prelude.Text,
    -- | Errors associated with the theme.
    errors :: Prelude.Maybe (Prelude.NonEmpty ThemeError),
    -- | The status of the theme version.
    status :: Prelude.Maybe ResourceStatus,
    -- | The version number of the theme.
    versionNumber :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThemeVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'themeVersion_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'baseThemeId', 'themeVersion_baseThemeId' - The Amazon QuickSight-defined ID of the theme that a custom theme
-- inherits from. All themes initially inherit from a default Amazon
-- QuickSight theme.
--
-- 'configuration', 'themeVersion_configuration' - The theme configuration, which contains all the theme display
-- properties.
--
-- 'createdTime', 'themeVersion_createdTime' - The date and time that this theme version was created.
--
-- 'description', 'themeVersion_description' - The description of the theme.
--
-- 'errors', 'themeVersion_errors' - Errors associated with the theme.
--
-- 'status', 'themeVersion_status' - The status of the theme version.
--
-- 'versionNumber', 'themeVersion_versionNumber' - The version number of the theme.
newThemeVersion ::
  ThemeVersion
newThemeVersion =
  ThemeVersion'
    { arn = Prelude.Nothing,
      baseThemeId = Prelude.Nothing,
      configuration = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      errors = Prelude.Nothing,
      status = Prelude.Nothing,
      versionNumber = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the resource.
themeVersion_arn :: Lens.Lens' ThemeVersion (Prelude.Maybe Prelude.Text)
themeVersion_arn = Lens.lens (\ThemeVersion' {arn} -> arn) (\s@ThemeVersion' {} a -> s {arn = a} :: ThemeVersion)

-- | The Amazon QuickSight-defined ID of the theme that a custom theme
-- inherits from. All themes initially inherit from a default Amazon
-- QuickSight theme.
themeVersion_baseThemeId :: Lens.Lens' ThemeVersion (Prelude.Maybe Prelude.Text)
themeVersion_baseThemeId = Lens.lens (\ThemeVersion' {baseThemeId} -> baseThemeId) (\s@ThemeVersion' {} a -> s {baseThemeId = a} :: ThemeVersion)

-- | The theme configuration, which contains all the theme display
-- properties.
themeVersion_configuration :: Lens.Lens' ThemeVersion (Prelude.Maybe ThemeConfiguration)
themeVersion_configuration = Lens.lens (\ThemeVersion' {configuration} -> configuration) (\s@ThemeVersion' {} a -> s {configuration = a} :: ThemeVersion)

-- | The date and time that this theme version was created.
themeVersion_createdTime :: Lens.Lens' ThemeVersion (Prelude.Maybe Prelude.UTCTime)
themeVersion_createdTime = Lens.lens (\ThemeVersion' {createdTime} -> createdTime) (\s@ThemeVersion' {} a -> s {createdTime = a} :: ThemeVersion) Prelude.. Lens.mapping Data._Time

-- | The description of the theme.
themeVersion_description :: Lens.Lens' ThemeVersion (Prelude.Maybe Prelude.Text)
themeVersion_description = Lens.lens (\ThemeVersion' {description} -> description) (\s@ThemeVersion' {} a -> s {description = a} :: ThemeVersion)

-- | Errors associated with the theme.
themeVersion_errors :: Lens.Lens' ThemeVersion (Prelude.Maybe (Prelude.NonEmpty ThemeError))
themeVersion_errors = Lens.lens (\ThemeVersion' {errors} -> errors) (\s@ThemeVersion' {} a -> s {errors = a} :: ThemeVersion) Prelude.. Lens.mapping Lens.coerced

-- | The status of the theme version.
themeVersion_status :: Lens.Lens' ThemeVersion (Prelude.Maybe ResourceStatus)
themeVersion_status = Lens.lens (\ThemeVersion' {status} -> status) (\s@ThemeVersion' {} a -> s {status = a} :: ThemeVersion)

-- | The version number of the theme.
themeVersion_versionNumber :: Lens.Lens' ThemeVersion (Prelude.Maybe Prelude.Natural)
themeVersion_versionNumber = Lens.lens (\ThemeVersion' {versionNumber} -> versionNumber) (\s@ThemeVersion' {} a -> s {versionNumber = a} :: ThemeVersion)

instance Data.FromJSON ThemeVersion where
  parseJSON =
    Data.withObject
      "ThemeVersion"
      ( \x ->
          ThemeVersion'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "BaseThemeId")
            Prelude.<*> (x Data..:? "Configuration")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Errors")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "VersionNumber")
      )

instance Prelude.Hashable ThemeVersion where
  hashWithSalt _salt ThemeVersion' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` baseThemeId
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` errors
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` versionNumber

instance Prelude.NFData ThemeVersion where
  rnf ThemeVersion' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf baseThemeId
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf versionNumber
