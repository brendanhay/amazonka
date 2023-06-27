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
-- Module      : Amazonka.IoT.Types.PackageSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.PackageSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of information about a software package.
--
-- /See:/ 'newPackageSummary' smart constructor.
data PackageSummary = PackageSummary'
  { -- | The date that the package was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the default package version.
    defaultVersionName :: Prelude.Maybe Prelude.Text,
    -- | The date that the package was last updated.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The name for the target package.
    packageName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'packageSummary_creationDate' - The date that the package was created.
--
-- 'defaultVersionName', 'packageSummary_defaultVersionName' - The name of the default package version.
--
-- 'lastModifiedDate', 'packageSummary_lastModifiedDate' - The date that the package was last updated.
--
-- 'packageName', 'packageSummary_packageName' - The name for the target package.
newPackageSummary ::
  PackageSummary
newPackageSummary =
  PackageSummary'
    { creationDate = Prelude.Nothing,
      defaultVersionName = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      packageName = Prelude.Nothing
    }

-- | The date that the package was created.
packageSummary_creationDate :: Lens.Lens' PackageSummary (Prelude.Maybe Prelude.UTCTime)
packageSummary_creationDate = Lens.lens (\PackageSummary' {creationDate} -> creationDate) (\s@PackageSummary' {} a -> s {creationDate = a} :: PackageSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the default package version.
packageSummary_defaultVersionName :: Lens.Lens' PackageSummary (Prelude.Maybe Prelude.Text)
packageSummary_defaultVersionName = Lens.lens (\PackageSummary' {defaultVersionName} -> defaultVersionName) (\s@PackageSummary' {} a -> s {defaultVersionName = a} :: PackageSummary)

-- | The date that the package was last updated.
packageSummary_lastModifiedDate :: Lens.Lens' PackageSummary (Prelude.Maybe Prelude.UTCTime)
packageSummary_lastModifiedDate = Lens.lens (\PackageSummary' {lastModifiedDate} -> lastModifiedDate) (\s@PackageSummary' {} a -> s {lastModifiedDate = a} :: PackageSummary) Prelude.. Lens.mapping Data._Time

-- | The name for the target package.
packageSummary_packageName :: Lens.Lens' PackageSummary (Prelude.Maybe Prelude.Text)
packageSummary_packageName = Lens.lens (\PackageSummary' {packageName} -> packageName) (\s@PackageSummary' {} a -> s {packageName = a} :: PackageSummary)

instance Data.FromJSON PackageSummary where
  parseJSON =
    Data.withObject
      "PackageSummary"
      ( \x ->
          PackageSummary'
            Prelude.<$> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "defaultVersionName")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "packageName")
      )

instance Prelude.Hashable PackageSummary where
  hashWithSalt _salt PackageSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` defaultVersionName
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` packageName

instance Prelude.NFData PackageSummary where
  rnf PackageSummary' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf defaultVersionName
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf packageName
