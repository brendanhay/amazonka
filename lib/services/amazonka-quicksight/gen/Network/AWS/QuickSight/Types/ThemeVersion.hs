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
-- Module      : Network.AWS.QuickSight.Types.ThemeVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.ThemeVersion where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types.ResourceStatus
import Network.AWS.QuickSight.Types.ThemeConfiguration
import Network.AWS.QuickSight.Types.ThemeError

-- | A version of a theme.
--
-- /See:/ 'newThemeVersion' smart constructor.
data ThemeVersion = ThemeVersion'
  { -- | The status of the theme version.
    status :: Prelude.Maybe ResourceStatus,
    -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that this theme version was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The version number of the theme.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The theme configuration, which contains all the theme display
    -- properties.
    configuration :: Prelude.Maybe ThemeConfiguration,
    -- | Errors associated with the theme.
    errors :: Prelude.Maybe (Prelude.NonEmpty ThemeError),
    -- | The description of the theme.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon QuickSight-defined ID of the theme that a custom theme
    -- inherits from. All themes initially inherit from a default Amazon
    -- QuickSight theme.
    baseThemeId :: Prelude.Maybe Prelude.Text
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
-- 'status', 'themeVersion_status' - The status of the theme version.
--
-- 'arn', 'themeVersion_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'createdTime', 'themeVersion_createdTime' - The date and time that this theme version was created.
--
-- 'versionNumber', 'themeVersion_versionNumber' - The version number of the theme.
--
-- 'configuration', 'themeVersion_configuration' - The theme configuration, which contains all the theme display
-- properties.
--
-- 'errors', 'themeVersion_errors' - Errors associated with the theme.
--
-- 'description', 'themeVersion_description' - The description of the theme.
--
-- 'baseThemeId', 'themeVersion_baseThemeId' - The Amazon QuickSight-defined ID of the theme that a custom theme
-- inherits from. All themes initially inherit from a default Amazon
-- QuickSight theme.
newThemeVersion ::
  ThemeVersion
newThemeVersion =
  ThemeVersion'
    { status = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      configuration = Prelude.Nothing,
      errors = Prelude.Nothing,
      description = Prelude.Nothing,
      baseThemeId = Prelude.Nothing
    }

-- | The status of the theme version.
themeVersion_status :: Lens.Lens' ThemeVersion (Prelude.Maybe ResourceStatus)
themeVersion_status = Lens.lens (\ThemeVersion' {status} -> status) (\s@ThemeVersion' {} a -> s {status = a} :: ThemeVersion)

-- | The Amazon Resource Name (ARN) of the resource.
themeVersion_arn :: Lens.Lens' ThemeVersion (Prelude.Maybe Prelude.Text)
themeVersion_arn = Lens.lens (\ThemeVersion' {arn} -> arn) (\s@ThemeVersion' {} a -> s {arn = a} :: ThemeVersion)

-- | The date and time that this theme version was created.
themeVersion_createdTime :: Lens.Lens' ThemeVersion (Prelude.Maybe Prelude.UTCTime)
themeVersion_createdTime = Lens.lens (\ThemeVersion' {createdTime} -> createdTime) (\s@ThemeVersion' {} a -> s {createdTime = a} :: ThemeVersion) Prelude.. Lens.mapping Core._Time

-- | The version number of the theme.
themeVersion_versionNumber :: Lens.Lens' ThemeVersion (Prelude.Maybe Prelude.Natural)
themeVersion_versionNumber = Lens.lens (\ThemeVersion' {versionNumber} -> versionNumber) (\s@ThemeVersion' {} a -> s {versionNumber = a} :: ThemeVersion)

-- | The theme configuration, which contains all the theme display
-- properties.
themeVersion_configuration :: Lens.Lens' ThemeVersion (Prelude.Maybe ThemeConfiguration)
themeVersion_configuration = Lens.lens (\ThemeVersion' {configuration} -> configuration) (\s@ThemeVersion' {} a -> s {configuration = a} :: ThemeVersion)

-- | Errors associated with the theme.
themeVersion_errors :: Lens.Lens' ThemeVersion (Prelude.Maybe (Prelude.NonEmpty ThemeError))
themeVersion_errors = Lens.lens (\ThemeVersion' {errors} -> errors) (\s@ThemeVersion' {} a -> s {errors = a} :: ThemeVersion) Prelude.. Lens.mapping Lens.coerced

-- | The description of the theme.
themeVersion_description :: Lens.Lens' ThemeVersion (Prelude.Maybe Prelude.Text)
themeVersion_description = Lens.lens (\ThemeVersion' {description} -> description) (\s@ThemeVersion' {} a -> s {description = a} :: ThemeVersion)

-- | The Amazon QuickSight-defined ID of the theme that a custom theme
-- inherits from. All themes initially inherit from a default Amazon
-- QuickSight theme.
themeVersion_baseThemeId :: Lens.Lens' ThemeVersion (Prelude.Maybe Prelude.Text)
themeVersion_baseThemeId = Lens.lens (\ThemeVersion' {baseThemeId} -> baseThemeId) (\s@ThemeVersion' {} a -> s {baseThemeId = a} :: ThemeVersion)

instance Core.FromJSON ThemeVersion where
  parseJSON =
    Core.withObject
      "ThemeVersion"
      ( \x ->
          ThemeVersion'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "VersionNumber")
            Prelude.<*> (x Core..:? "Configuration")
            Prelude.<*> (x Core..:? "Errors")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "BaseThemeId")
      )

instance Prelude.Hashable ThemeVersion

instance Prelude.NFData ThemeVersion
