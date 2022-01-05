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
-- Module      : Amazonka.QuickSight.Types.ThemeVersionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ThemeVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ResourceStatus

-- | The theme version.
--
-- /See:/ 'newThemeVersionSummary' smart constructor.
data ThemeVersionSummary = ThemeVersionSummary'
  { -- | The status of the theme version.
    status :: Prelude.Maybe ResourceStatus,
    -- | The Amazon Resource Name (ARN) of the theme version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that this theme version was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The version number of the theme version.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The description of the theme version.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThemeVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'themeVersionSummary_status' - The status of the theme version.
--
-- 'arn', 'themeVersionSummary_arn' - The Amazon Resource Name (ARN) of the theme version.
--
-- 'createdTime', 'themeVersionSummary_createdTime' - The date and time that this theme version was created.
--
-- 'versionNumber', 'themeVersionSummary_versionNumber' - The version number of the theme version.
--
-- 'description', 'themeVersionSummary_description' - The description of the theme version.
newThemeVersionSummary ::
  ThemeVersionSummary
newThemeVersionSummary =
  ThemeVersionSummary'
    { status = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The status of the theme version.
themeVersionSummary_status :: Lens.Lens' ThemeVersionSummary (Prelude.Maybe ResourceStatus)
themeVersionSummary_status = Lens.lens (\ThemeVersionSummary' {status} -> status) (\s@ThemeVersionSummary' {} a -> s {status = a} :: ThemeVersionSummary)

-- | The Amazon Resource Name (ARN) of the theme version.
themeVersionSummary_arn :: Lens.Lens' ThemeVersionSummary (Prelude.Maybe Prelude.Text)
themeVersionSummary_arn = Lens.lens (\ThemeVersionSummary' {arn} -> arn) (\s@ThemeVersionSummary' {} a -> s {arn = a} :: ThemeVersionSummary)

-- | The date and time that this theme version was created.
themeVersionSummary_createdTime :: Lens.Lens' ThemeVersionSummary (Prelude.Maybe Prelude.UTCTime)
themeVersionSummary_createdTime = Lens.lens (\ThemeVersionSummary' {createdTime} -> createdTime) (\s@ThemeVersionSummary' {} a -> s {createdTime = a} :: ThemeVersionSummary) Prelude.. Lens.mapping Core._Time

-- | The version number of the theme version.
themeVersionSummary_versionNumber :: Lens.Lens' ThemeVersionSummary (Prelude.Maybe Prelude.Natural)
themeVersionSummary_versionNumber = Lens.lens (\ThemeVersionSummary' {versionNumber} -> versionNumber) (\s@ThemeVersionSummary' {} a -> s {versionNumber = a} :: ThemeVersionSummary)

-- | The description of the theme version.
themeVersionSummary_description :: Lens.Lens' ThemeVersionSummary (Prelude.Maybe Prelude.Text)
themeVersionSummary_description = Lens.lens (\ThemeVersionSummary' {description} -> description) (\s@ThemeVersionSummary' {} a -> s {description = a} :: ThemeVersionSummary)

instance Core.FromJSON ThemeVersionSummary where
  parseJSON =
    Core.withObject
      "ThemeVersionSummary"
      ( \x ->
          ThemeVersionSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "VersionNumber")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable ThemeVersionSummary where
  hashWithSalt _salt ThemeVersionSummary' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` description

instance Prelude.NFData ThemeVersionSummary where
  rnf ThemeVersionSummary' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf description
