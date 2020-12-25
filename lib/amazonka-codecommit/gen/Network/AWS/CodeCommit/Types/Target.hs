{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Target
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Target
  ( Target (..),

    -- * Smart constructor
    mkTarget,

    -- * Lenses
    tRepositoryName,
    tSourceReference,
    tDestinationReference,
  )
where

import qualified Network.AWS.CodeCommit.Types.ReferenceName as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about a target for a pull request.
--
-- /See:/ 'mkTarget' smart constructor.
data Target = Target'
  { -- | The name of the repository that contains the pull request.
    repositoryName :: Types.RepositoryName,
    -- | The branch of the repository that contains the changes for the pull request. Also known as the source branch.
    sourceReference :: Types.ReferenceName,
    -- | The branch of the repository where the pull request changes are merged. Also known as the destination branch.
    destinationReference :: Core.Maybe Types.ReferenceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Target' value with any optional fields omitted.
mkTarget ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  -- | 'sourceReference'
  Types.ReferenceName ->
  Target
mkTarget repositoryName sourceReference =
  Target'
    { repositoryName,
      sourceReference,
      destinationReference = Core.Nothing
    }

-- | The name of the repository that contains the pull request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRepositoryName :: Lens.Lens' Target Types.RepositoryName
tRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED tRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The branch of the repository that contains the changes for the pull request. Also known as the source branch.
--
-- /Note:/ Consider using 'sourceReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSourceReference :: Lens.Lens' Target Types.ReferenceName
tSourceReference = Lens.field @"sourceReference"
{-# DEPRECATED tSourceReference "Use generic-lens or generic-optics with 'sourceReference' instead." #-}

-- | The branch of the repository where the pull request changes are merged. Also known as the destination branch.
--
-- /Note:/ Consider using 'destinationReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDestinationReference :: Lens.Lens' Target (Core.Maybe Types.ReferenceName)
tDestinationReference = Lens.field @"destinationReference"
{-# DEPRECATED tDestinationReference "Use generic-lens or generic-optics with 'destinationReference' instead." #-}

instance Core.FromJSON Target where
  toJSON Target {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("sourceReference" Core..= sourceReference),
            ("destinationReference" Core..=) Core.<$> destinationReference
          ]
      )
