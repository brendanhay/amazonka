{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.PackageVersionHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.PackageVersionHistory
  ( PackageVersionHistory (..)
  -- * Smart constructor
  , mkPackageVersionHistory
  -- * Lenses
  , pvhCommitMessage
  , pvhCreatedAt
  , pvhPackageVersion
  ) where

import qualified Network.AWS.ElasticSearch.Types.CommitMessage as Types
import qualified Network.AWS.ElasticSearch.Types.PackageVersion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details of a package version.
--
-- /See:/ 'mkPackageVersionHistory' smart constructor.
data PackageVersionHistory = PackageVersionHistory'
  { commitMessage :: Core.Maybe Types.CommitMessage
    -- ^ A message associated with the version.
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ Timestamp which tells creation time of the package version.
  , packageVersion :: Core.Maybe Types.PackageVersion
    -- ^ Version of the package.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PackageVersionHistory' value with any optional fields omitted.
mkPackageVersionHistory
    :: PackageVersionHistory
mkPackageVersionHistory
  = PackageVersionHistory'{commitMessage = Core.Nothing,
                           createdAt = Core.Nothing, packageVersion = Core.Nothing}

-- | A message associated with the version.
--
-- /Note:/ Consider using 'commitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvhCommitMessage :: Lens.Lens' PackageVersionHistory (Core.Maybe Types.CommitMessage)
pvhCommitMessage = Lens.field @"commitMessage"
{-# INLINEABLE pvhCommitMessage #-}
{-# DEPRECATED commitMessage "Use generic-lens or generic-optics with 'commitMessage' instead"  #-}

-- | Timestamp which tells creation time of the package version.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvhCreatedAt :: Lens.Lens' PackageVersionHistory (Core.Maybe Core.NominalDiffTime)
pvhCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE pvhCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | Version of the package.
--
-- /Note:/ Consider using 'packageVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvhPackageVersion :: Lens.Lens' PackageVersionHistory (Core.Maybe Types.PackageVersion)
pvhPackageVersion = Lens.field @"packageVersion"
{-# INLINEABLE pvhPackageVersion #-}
{-# DEPRECATED packageVersion "Use generic-lens or generic-optics with 'packageVersion' instead"  #-}

instance Core.FromJSON PackageVersionHistory where
        parseJSON
          = Core.withObject "PackageVersionHistory" Core.$
              \ x ->
                PackageVersionHistory' Core.<$>
                  (x Core..:? "CommitMessage") Core.<*> x Core..:? "CreatedAt"
                    Core.<*> x Core..:? "PackageVersion"
