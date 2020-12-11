-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.PackageVersionHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.PackageVersionHistory
  ( PackageVersionHistory (..),

    -- * Smart constructor
    mkPackageVersionHistory,

    -- * Lenses
    pvhCreatedAt,
    pvhPackageVersion,
    pvhCommitMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details of a package version.
--
-- /See:/ 'mkPackageVersionHistory' smart constructor.
data PackageVersionHistory = PackageVersionHistory'
  { createdAt ::
      Lude.Maybe Lude.Timestamp,
    packageVersion :: Lude.Maybe Lude.Text,
    commitMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PackageVersionHistory' with the minimum fields required to make a request.
--
-- * 'commitMessage' - A message associated with the version.
-- * 'createdAt' - Timestamp which tells creation time of the package version.
-- * 'packageVersion' - Version of the package.
mkPackageVersionHistory ::
  PackageVersionHistory
mkPackageVersionHistory =
  PackageVersionHistory'
    { createdAt = Lude.Nothing,
      packageVersion = Lude.Nothing,
      commitMessage = Lude.Nothing
    }

-- | Timestamp which tells creation time of the package version.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvhCreatedAt :: Lens.Lens' PackageVersionHistory (Lude.Maybe Lude.Timestamp)
pvhCreatedAt = Lens.lens (createdAt :: PackageVersionHistory -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: PackageVersionHistory)
{-# DEPRECATED pvhCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Version of the package.
--
-- /Note:/ Consider using 'packageVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvhPackageVersion :: Lens.Lens' PackageVersionHistory (Lude.Maybe Lude.Text)
pvhPackageVersion = Lens.lens (packageVersion :: PackageVersionHistory -> Lude.Maybe Lude.Text) (\s a -> s {packageVersion = a} :: PackageVersionHistory)
{-# DEPRECATED pvhPackageVersion "Use generic-lens or generic-optics with 'packageVersion' instead." #-}

-- | A message associated with the version.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvhCommitMessage :: Lens.Lens' PackageVersionHistory (Lude.Maybe Lude.Text)
pvhCommitMessage = Lens.lens (commitMessage :: PackageVersionHistory -> Lude.Maybe Lude.Text) (\s a -> s {commitMessage = a} :: PackageVersionHistory)
{-# DEPRECATED pvhCommitMessage "Use generic-lens or generic-optics with 'commitMessage' instead." #-}

instance Lude.FromJSON PackageVersionHistory where
  parseJSON =
    Lude.withObject
      "PackageVersionHistory"
      ( \x ->
          PackageVersionHistory'
            Lude.<$> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "PackageVersion")
            Lude.<*> (x Lude..:? "CommitMessage")
      )
