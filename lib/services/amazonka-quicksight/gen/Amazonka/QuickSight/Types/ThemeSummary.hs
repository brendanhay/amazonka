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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ThemeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The theme summary.
--
-- /See:/ 'newThemeSummary' smart constructor.
data ThemeSummary = ThemeSummary'
  { -- | the display name for the theme.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time that this theme was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The latest version number for the theme.
    latestVersionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The last date and time that this theme was updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the theme. This ID is unique per Amazon Web Services Region
    -- for each Amazon Web Services account.
    themeId :: Prelude.Maybe Prelude.Text
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
-- 'name', 'themeSummary_name' - the display name for the theme.
--
-- 'createdTime', 'themeSummary_createdTime' - The date and time that this theme was created.
--
-- 'latestVersionNumber', 'themeSummary_latestVersionNumber' - The latest version number for the theme.
--
-- 'arn', 'themeSummary_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'lastUpdatedTime', 'themeSummary_lastUpdatedTime' - The last date and time that this theme was updated.
--
-- 'themeId', 'themeSummary_themeId' - The ID of the theme. This ID is unique per Amazon Web Services Region
-- for each Amazon Web Services account.
newThemeSummary ::
  ThemeSummary
newThemeSummary =
  ThemeSummary'
    { name = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      latestVersionNumber = Prelude.Nothing,
      arn = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      themeId = Prelude.Nothing
    }

-- | the display name for the theme.
themeSummary_name :: Lens.Lens' ThemeSummary (Prelude.Maybe Prelude.Text)
themeSummary_name = Lens.lens (\ThemeSummary' {name} -> name) (\s@ThemeSummary' {} a -> s {name = a} :: ThemeSummary)

-- | The date and time that this theme was created.
themeSummary_createdTime :: Lens.Lens' ThemeSummary (Prelude.Maybe Prelude.UTCTime)
themeSummary_createdTime = Lens.lens (\ThemeSummary' {createdTime} -> createdTime) (\s@ThemeSummary' {} a -> s {createdTime = a} :: ThemeSummary) Prelude.. Lens.mapping Data._Time

-- | The latest version number for the theme.
themeSummary_latestVersionNumber :: Lens.Lens' ThemeSummary (Prelude.Maybe Prelude.Natural)
themeSummary_latestVersionNumber = Lens.lens (\ThemeSummary' {latestVersionNumber} -> latestVersionNumber) (\s@ThemeSummary' {} a -> s {latestVersionNumber = a} :: ThemeSummary)

-- | The Amazon Resource Name (ARN) of the resource.
themeSummary_arn :: Lens.Lens' ThemeSummary (Prelude.Maybe Prelude.Text)
themeSummary_arn = Lens.lens (\ThemeSummary' {arn} -> arn) (\s@ThemeSummary' {} a -> s {arn = a} :: ThemeSummary)

-- | The last date and time that this theme was updated.
themeSummary_lastUpdatedTime :: Lens.Lens' ThemeSummary (Prelude.Maybe Prelude.UTCTime)
themeSummary_lastUpdatedTime = Lens.lens (\ThemeSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@ThemeSummary' {} a -> s {lastUpdatedTime = a} :: ThemeSummary) Prelude.. Lens.mapping Data._Time

-- | The ID of the theme. This ID is unique per Amazon Web Services Region
-- for each Amazon Web Services account.
themeSummary_themeId :: Lens.Lens' ThemeSummary (Prelude.Maybe Prelude.Text)
themeSummary_themeId = Lens.lens (\ThemeSummary' {themeId} -> themeId) (\s@ThemeSummary' {} a -> s {themeId = a} :: ThemeSummary)

instance Data.FromJSON ThemeSummary where
  parseJSON =
    Data.withObject
      "ThemeSummary"
      ( \x ->
          ThemeSummary'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "LatestVersionNumber")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "ThemeId")
      )

instance Prelude.Hashable ThemeSummary where
  hashWithSalt _salt ThemeSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` latestVersionNumber
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` themeId

instance Prelude.NFData ThemeSummary where
  rnf ThemeSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf latestVersionNumber
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf themeId
