{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.RepositoryNameIdPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryNameIdPair
  ( RepositoryNameIdPair (..),

    -- * Smart constructor
    mkRepositoryNameIdPair,

    -- * Lenses
    rnipRepositoryId,
    rnipRepositoryName,
  )
where

import qualified Network.AWS.CodeCommit.Types.RepositoryId as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a repository name and ID.
--
-- /See:/ 'mkRepositoryNameIdPair' smart constructor.
data RepositoryNameIdPair = RepositoryNameIdPair'
  { -- | The ID associated with the repository.
    repositoryId :: Core.Maybe Types.RepositoryId,
    -- | The name associated with the repository.
    repositoryName :: Core.Maybe Types.RepositoryName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RepositoryNameIdPair' value with any optional fields omitted.
mkRepositoryNameIdPair ::
  RepositoryNameIdPair
mkRepositoryNameIdPair =
  RepositoryNameIdPair'
    { repositoryId = Core.Nothing,
      repositoryName = Core.Nothing
    }

-- | The ID associated with the repository.
--
-- /Note:/ Consider using 'repositoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnipRepositoryId :: Lens.Lens' RepositoryNameIdPair (Core.Maybe Types.RepositoryId)
rnipRepositoryId = Lens.field @"repositoryId"
{-# DEPRECATED rnipRepositoryId "Use generic-lens or generic-optics with 'repositoryId' instead." #-}

-- | The name associated with the repository.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnipRepositoryName :: Lens.Lens' RepositoryNameIdPair (Core.Maybe Types.RepositoryName)
rnipRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED rnipRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Core.FromJSON RepositoryNameIdPair where
  parseJSON =
    Core.withObject "RepositoryNameIdPair" Core.$
      \x ->
        RepositoryNameIdPair'
          Core.<$> (x Core..:? "repositoryId") Core.<*> (x Core..:? "repositoryName")
