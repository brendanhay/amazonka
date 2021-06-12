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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details of a package version.
--
-- /See:/ 'newPackageVersionHistory' smart constructor.
data PackageVersionHistory = PackageVersionHistory'
  { -- | Version of the package.
    packageVersion :: Core.Maybe Core.Text,
    -- | Timestamp which tells creation time of the package version.
    createdAt :: Core.Maybe Core.POSIX,
    -- | A message associated with the version.
    commitMessage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      createdAt = Core.Nothing,
      commitMessage = Core.Nothing
    }

-- | Version of the package.
packageVersionHistory_packageVersion :: Lens.Lens' PackageVersionHistory (Core.Maybe Core.Text)
packageVersionHistory_packageVersion = Lens.lens (\PackageVersionHistory' {packageVersion} -> packageVersion) (\s@PackageVersionHistory' {} a -> s {packageVersion = a} :: PackageVersionHistory)

-- | Timestamp which tells creation time of the package version.
packageVersionHistory_createdAt :: Lens.Lens' PackageVersionHistory (Core.Maybe Core.UTCTime)
packageVersionHistory_createdAt = Lens.lens (\PackageVersionHistory' {createdAt} -> createdAt) (\s@PackageVersionHistory' {} a -> s {createdAt = a} :: PackageVersionHistory) Core.. Lens.mapping Core._Time

-- | A message associated with the version.
packageVersionHistory_commitMessage :: Lens.Lens' PackageVersionHistory (Core.Maybe Core.Text)
packageVersionHistory_commitMessage = Lens.lens (\PackageVersionHistory' {commitMessage} -> commitMessage) (\s@PackageVersionHistory' {} a -> s {commitMessage = a} :: PackageVersionHistory)

instance Core.FromJSON PackageVersionHistory where
  parseJSON =
    Core.withObject
      "PackageVersionHistory"
      ( \x ->
          PackageVersionHistory'
            Core.<$> (x Core..:? "PackageVersion")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "CommitMessage")
      )

instance Core.Hashable PackageVersionHistory

instance Core.NFData PackageVersionHistory
