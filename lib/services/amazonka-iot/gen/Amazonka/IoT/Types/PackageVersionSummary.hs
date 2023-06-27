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
-- Module      : Amazonka.IoT.Types.PackageVersionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.PackageVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.PackageVersionStatus
import qualified Amazonka.Prelude as Prelude

-- | A summary of information about a package version.
--
-- /See:/ 'newPackageVersionSummary' smart constructor.
data PackageVersionSummary = PackageVersionSummary'
  { -- | The date that the package version was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The date that the package version was last updated.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the associated software package.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | The status of the package version. For more information, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/preparing-to-use-software-package-catalog.html#package-version-lifecycle Package version lifecycle>.
    status :: Prelude.Maybe PackageVersionStatus,
    -- | The name of the target package version.
    versionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'packageVersionSummary_creationDate' - The date that the package version was created.
--
-- 'lastModifiedDate', 'packageVersionSummary_lastModifiedDate' - The date that the package version was last updated.
--
-- 'packageName', 'packageVersionSummary_packageName' - The name of the associated software package.
--
-- 'status', 'packageVersionSummary_status' - The status of the package version. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/preparing-to-use-software-package-catalog.html#package-version-lifecycle Package version lifecycle>.
--
-- 'versionName', 'packageVersionSummary_versionName' - The name of the target package version.
newPackageVersionSummary ::
  PackageVersionSummary
newPackageVersionSummary =
  PackageVersionSummary'
    { creationDate =
        Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      packageName = Prelude.Nothing,
      status = Prelude.Nothing,
      versionName = Prelude.Nothing
    }

-- | The date that the package version was created.
packageVersionSummary_creationDate :: Lens.Lens' PackageVersionSummary (Prelude.Maybe Prelude.UTCTime)
packageVersionSummary_creationDate = Lens.lens (\PackageVersionSummary' {creationDate} -> creationDate) (\s@PackageVersionSummary' {} a -> s {creationDate = a} :: PackageVersionSummary) Prelude.. Lens.mapping Data._Time

-- | The date that the package version was last updated.
packageVersionSummary_lastModifiedDate :: Lens.Lens' PackageVersionSummary (Prelude.Maybe Prelude.UTCTime)
packageVersionSummary_lastModifiedDate = Lens.lens (\PackageVersionSummary' {lastModifiedDate} -> lastModifiedDate) (\s@PackageVersionSummary' {} a -> s {lastModifiedDate = a} :: PackageVersionSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the associated software package.
packageVersionSummary_packageName :: Lens.Lens' PackageVersionSummary (Prelude.Maybe Prelude.Text)
packageVersionSummary_packageName = Lens.lens (\PackageVersionSummary' {packageName} -> packageName) (\s@PackageVersionSummary' {} a -> s {packageName = a} :: PackageVersionSummary)

-- | The status of the package version. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/preparing-to-use-software-package-catalog.html#package-version-lifecycle Package version lifecycle>.
packageVersionSummary_status :: Lens.Lens' PackageVersionSummary (Prelude.Maybe PackageVersionStatus)
packageVersionSummary_status = Lens.lens (\PackageVersionSummary' {status} -> status) (\s@PackageVersionSummary' {} a -> s {status = a} :: PackageVersionSummary)

-- | The name of the target package version.
packageVersionSummary_versionName :: Lens.Lens' PackageVersionSummary (Prelude.Maybe Prelude.Text)
packageVersionSummary_versionName = Lens.lens (\PackageVersionSummary' {versionName} -> versionName) (\s@PackageVersionSummary' {} a -> s {versionName = a} :: PackageVersionSummary)

instance Data.FromJSON PackageVersionSummary where
  parseJSON =
    Data.withObject
      "PackageVersionSummary"
      ( \x ->
          PackageVersionSummary'
            Prelude.<$> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "packageName")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "versionName")
      )

instance Prelude.Hashable PackageVersionSummary where
  hashWithSalt _salt PackageVersionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` packageName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` versionName

instance Prelude.NFData PackageVersionSummary where
  rnf PackageVersionSummary' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf versionName
