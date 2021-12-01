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
-- Module      : Amazonka.ElasticSearch.Types.PackageVersionHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.PackageVersionHistory where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details of a package version.
--
-- /See:/ 'newPackageVersionHistory' smart constructor.
data PackageVersionHistory = PackageVersionHistory'
  { -- | Timestamp which tells creation time of the package version.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Version of the package.
    packageVersion :: Prelude.Maybe Prelude.Text,
    -- | A message associated with the version.
    commitMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageVersionHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'packageVersionHistory_createdAt' - Timestamp which tells creation time of the package version.
--
-- 'packageVersion', 'packageVersionHistory_packageVersion' - Version of the package.
--
-- 'commitMessage', 'packageVersionHistory_commitMessage' - A message associated with the version.
newPackageVersionHistory ::
  PackageVersionHistory
newPackageVersionHistory =
  PackageVersionHistory'
    { createdAt = Prelude.Nothing,
      packageVersion = Prelude.Nothing,
      commitMessage = Prelude.Nothing
    }

-- | Timestamp which tells creation time of the package version.
packageVersionHistory_createdAt :: Lens.Lens' PackageVersionHistory (Prelude.Maybe Prelude.UTCTime)
packageVersionHistory_createdAt = Lens.lens (\PackageVersionHistory' {createdAt} -> createdAt) (\s@PackageVersionHistory' {} a -> s {createdAt = a} :: PackageVersionHistory) Prelude.. Lens.mapping Core._Time

-- | Version of the package.
packageVersionHistory_packageVersion :: Lens.Lens' PackageVersionHistory (Prelude.Maybe Prelude.Text)
packageVersionHistory_packageVersion = Lens.lens (\PackageVersionHistory' {packageVersion} -> packageVersion) (\s@PackageVersionHistory' {} a -> s {packageVersion = a} :: PackageVersionHistory)

-- | A message associated with the version.
packageVersionHistory_commitMessage :: Lens.Lens' PackageVersionHistory (Prelude.Maybe Prelude.Text)
packageVersionHistory_commitMessage = Lens.lens (\PackageVersionHistory' {commitMessage} -> commitMessage) (\s@PackageVersionHistory' {} a -> s {commitMessage = a} :: PackageVersionHistory)

instance Core.FromJSON PackageVersionHistory where
  parseJSON =
    Core.withObject
      "PackageVersionHistory"
      ( \x ->
          PackageVersionHistory'
            Prelude.<$> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "PackageVersion")
            Prelude.<*> (x Core..:? "CommitMessage")
      )

instance Prelude.Hashable PackageVersionHistory where
  hashWithSalt salt' PackageVersionHistory' {..} =
    salt' `Prelude.hashWithSalt` commitMessage
      `Prelude.hashWithSalt` packageVersion
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData PackageVersionHistory where
  rnf PackageVersionHistory' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf commitMessage
      `Prelude.seq` Prelude.rnf packageVersion
