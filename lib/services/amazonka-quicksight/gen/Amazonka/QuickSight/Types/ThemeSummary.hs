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
-- Module      : Amazonka.QuickSight.Types.ThemeSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ThemeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The theme summary.
--
-- /See:/ 'newThemeSummary' smart constructor.
data ThemeSummary = ThemeSummary'
  { -- | The ID of the theme. This ID is unique per Amazon Web Services Region
    -- for each Amazon Web Services account.
    themeId :: Prelude.Maybe Prelude.Text,
    -- | The last date and time that this theme was updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The latest version number for the theme.
    latestVersionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that this theme was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | the display name for the theme.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThemeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'themeId', 'themeSummary_themeId' - The ID of the theme. This ID is unique per Amazon Web Services Region
-- for each Amazon Web Services account.
--
-- 'lastUpdatedTime', 'themeSummary_lastUpdatedTime' - The last date and time that this theme was updated.
--
-- 'latestVersionNumber', 'themeSummary_latestVersionNumber' - The latest version number for the theme.
--
-- 'arn', 'themeSummary_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'createdTime', 'themeSummary_createdTime' - The date and time that this theme was created.
--
-- 'name', 'themeSummary_name' - the display name for the theme.
newThemeSummary ::
  ThemeSummary
newThemeSummary =
  ThemeSummary'
    { themeId = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      latestVersionNumber = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The ID of the theme. This ID is unique per Amazon Web Services Region
-- for each Amazon Web Services account.
themeSummary_themeId :: Lens.Lens' ThemeSummary (Prelude.Maybe Prelude.Text)
themeSummary_themeId = Lens.lens (\ThemeSummary' {themeId} -> themeId) (\s@ThemeSummary' {} a -> s {themeId = a} :: ThemeSummary)

-- | The last date and time that this theme was updated.
themeSummary_lastUpdatedTime :: Lens.Lens' ThemeSummary (Prelude.Maybe Prelude.UTCTime)
themeSummary_lastUpdatedTime = Lens.lens (\ThemeSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@ThemeSummary' {} a -> s {lastUpdatedTime = a} :: ThemeSummary) Prelude.. Lens.mapping Core._Time

-- | The latest version number for the theme.
themeSummary_latestVersionNumber :: Lens.Lens' ThemeSummary (Prelude.Maybe Prelude.Natural)
themeSummary_latestVersionNumber = Lens.lens (\ThemeSummary' {latestVersionNumber} -> latestVersionNumber) (\s@ThemeSummary' {} a -> s {latestVersionNumber = a} :: ThemeSummary)

-- | The Amazon Resource Name (ARN) of the resource.
themeSummary_arn :: Lens.Lens' ThemeSummary (Prelude.Maybe Prelude.Text)
themeSummary_arn = Lens.lens (\ThemeSummary' {arn} -> arn) (\s@ThemeSummary' {} a -> s {arn = a} :: ThemeSummary)

-- | The date and time that this theme was created.
themeSummary_createdTime :: Lens.Lens' ThemeSummary (Prelude.Maybe Prelude.UTCTime)
themeSummary_createdTime = Lens.lens (\ThemeSummary' {createdTime} -> createdTime) (\s@ThemeSummary' {} a -> s {createdTime = a} :: ThemeSummary) Prelude.. Lens.mapping Core._Time

-- | the display name for the theme.
themeSummary_name :: Lens.Lens' ThemeSummary (Prelude.Maybe Prelude.Text)
themeSummary_name = Lens.lens (\ThemeSummary' {name} -> name) (\s@ThemeSummary' {} a -> s {name = a} :: ThemeSummary)

instance Core.FromJSON ThemeSummary where
  parseJSON =
    Core.withObject
      "ThemeSummary"
      ( \x ->
          ThemeSummary'
            Prelude.<$> (x Core..:? "ThemeId")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "LatestVersionNumber")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable ThemeSummary where
  hashWithSalt _salt ThemeSummary' {..} =
    _salt `Prelude.hashWithSalt` themeId
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` latestVersionNumber
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` name

instance Prelude.NFData ThemeSummary where
  rnf ThemeSummary' {..} =
    Prelude.rnf themeId
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf latestVersionNumber
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf name
