{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElasticSearch.Types.PackageVersionHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.PackageVersionHistory where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details of a package version.
--
-- /See:/ 'newPackageVersionHistory' smart constructor.
data PackageVersionHistory = PackageVersionHistory'
  { -- | Version of the package.
    packageVersion :: Prelude.Maybe Prelude.Text,
    -- | Timestamp which tells creation time of the package version.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | A message associated with the version.
    commitMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PackageVersionHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packageVersion', 'packageVersionHistory_packageVersion' - Version of the package.
--
-- 'createdAt', 'packageVersionHistory_createdAt' - Timestamp which tells creation time of the package version.
--
-- 'commitMessage', 'packageVersionHistory_commitMessage' - A message associated with the version.
newPackageVersionHistory ::
  PackageVersionHistory
newPackageVersionHistory =
  PackageVersionHistory'
    { packageVersion =
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      commitMessage = Prelude.Nothing
    }

-- | Version of the package.
packageVersionHistory_packageVersion :: Lens.Lens' PackageVersionHistory (Prelude.Maybe Prelude.Text)
packageVersionHistory_packageVersion = Lens.lens (\PackageVersionHistory' {packageVersion} -> packageVersion) (\s@PackageVersionHistory' {} a -> s {packageVersion = a} :: PackageVersionHistory)

-- | Timestamp which tells creation time of the package version.
packageVersionHistory_createdAt :: Lens.Lens' PackageVersionHistory (Prelude.Maybe Prelude.UTCTime)
packageVersionHistory_createdAt = Lens.lens (\PackageVersionHistory' {createdAt} -> createdAt) (\s@PackageVersionHistory' {} a -> s {createdAt = a} :: PackageVersionHistory) Prelude.. Lens.mapping Prelude._Time

-- | A message associated with the version.
packageVersionHistory_commitMessage :: Lens.Lens' PackageVersionHistory (Prelude.Maybe Prelude.Text)
packageVersionHistory_commitMessage = Lens.lens (\PackageVersionHistory' {commitMessage} -> commitMessage) (\s@PackageVersionHistory' {} a -> s {commitMessage = a} :: PackageVersionHistory)

instance Prelude.FromJSON PackageVersionHistory where
  parseJSON =
    Prelude.withObject
      "PackageVersionHistory"
      ( \x ->
          PackageVersionHistory'
            Prelude.<$> (x Prelude..:? "PackageVersion")
            Prelude.<*> (x Prelude..:? "CreatedAt")
            Prelude.<*> (x Prelude..:? "CommitMessage")
      )

instance Prelude.Hashable PackageVersionHistory

instance Prelude.NFData PackageVersionHistory
