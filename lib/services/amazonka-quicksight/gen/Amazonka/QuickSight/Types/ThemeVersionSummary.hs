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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ThemeVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ResourceStatus

-- | The theme version.
--
-- /See:/ 'newThemeVersionSummary' smart constructor.
data ThemeVersionSummary = ThemeVersionSummary'
  { -- | The Amazon Resource Name (ARN) of the theme version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that this theme version was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the theme version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The status of the theme version.
    status :: Prelude.Maybe ResourceStatus,
    -- | The version number of the theme version.
    versionNumber :: Prelude.Maybe Prelude.Natural
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
-- 'arn', 'themeVersionSummary_arn' - The Amazon Resource Name (ARN) of the theme version.
--
-- 'createdTime', 'themeVersionSummary_createdTime' - The date and time that this theme version was created.
--
-- 'description', 'themeVersionSummary_description' - The description of the theme version.
--
-- 'status', 'themeVersionSummary_status' - The status of the theme version.
--
-- 'versionNumber', 'themeVersionSummary_versionNumber' - The version number of the theme version.
newThemeVersionSummary ::
  ThemeVersionSummary
newThemeVersionSummary =
  ThemeVersionSummary'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      status = Prelude.Nothing,
      versionNumber = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the theme version.
themeVersionSummary_arn :: Lens.Lens' ThemeVersionSummary (Prelude.Maybe Prelude.Text)
themeVersionSummary_arn = Lens.lens (\ThemeVersionSummary' {arn} -> arn) (\s@ThemeVersionSummary' {} a -> s {arn = a} :: ThemeVersionSummary)

-- | The date and time that this theme version was created.
themeVersionSummary_createdTime :: Lens.Lens' ThemeVersionSummary (Prelude.Maybe Prelude.UTCTime)
themeVersionSummary_createdTime = Lens.lens (\ThemeVersionSummary' {createdTime} -> createdTime) (\s@ThemeVersionSummary' {} a -> s {createdTime = a} :: ThemeVersionSummary) Prelude.. Lens.mapping Data._Time

-- | The description of the theme version.
themeVersionSummary_description :: Lens.Lens' ThemeVersionSummary (Prelude.Maybe Prelude.Text)
themeVersionSummary_description = Lens.lens (\ThemeVersionSummary' {description} -> description) (\s@ThemeVersionSummary' {} a -> s {description = a} :: ThemeVersionSummary)

-- | The status of the theme version.
themeVersionSummary_status :: Lens.Lens' ThemeVersionSummary (Prelude.Maybe ResourceStatus)
themeVersionSummary_status = Lens.lens (\ThemeVersionSummary' {status} -> status) (\s@ThemeVersionSummary' {} a -> s {status = a} :: ThemeVersionSummary)

-- | The version number of the theme version.
themeVersionSummary_versionNumber :: Lens.Lens' ThemeVersionSummary (Prelude.Maybe Prelude.Natural)
themeVersionSummary_versionNumber = Lens.lens (\ThemeVersionSummary' {versionNumber} -> versionNumber) (\s@ThemeVersionSummary' {} a -> s {versionNumber = a} :: ThemeVersionSummary)

instance Data.FromJSON ThemeVersionSummary where
  parseJSON =
    Data.withObject
      "ThemeVersionSummary"
      ( \x ->
          ThemeVersionSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "VersionNumber")
      )

instance Prelude.Hashable ThemeVersionSummary where
  hashWithSalt _salt ThemeVersionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` versionNumber

instance Prelude.NFData ThemeVersionSummary where
  rnf ThemeVersionSummary' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf createdTime `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf status `Prelude.seq`
            Prelude.rnf versionNumber
