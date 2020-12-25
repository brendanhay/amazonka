{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.MergeOperations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.MergeOperations
  ( MergeOperations (..),

    -- * Smart constructor
    mkMergeOperations,

    -- * Lenses
    moDestination,
    moSource,
  )
where

import qualified Network.AWS.CodeCommit.Types.ChangeTypeEnum as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the file operation conflicts in a merge operation.
--
-- /See:/ 'mkMergeOperations' smart constructor.
data MergeOperations = MergeOperations'
  { -- | The operation on a file in the destination of a merge or pull request.
    destination :: Core.Maybe Types.ChangeTypeEnum,
    -- | The operation (add, modify, or delete) on a file in the source of a merge or pull request.
    source :: Core.Maybe Types.ChangeTypeEnum
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MergeOperations' value with any optional fields omitted.
mkMergeOperations ::
  MergeOperations
mkMergeOperations =
  MergeOperations'
    { destination = Core.Nothing,
      source = Core.Nothing
    }

-- | The operation on a file in the destination of a merge or pull request.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
moDestination :: Lens.Lens' MergeOperations (Core.Maybe Types.ChangeTypeEnum)
moDestination = Lens.field @"destination"
{-# DEPRECATED moDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The operation (add, modify, or delete) on a file in the source of a merge or pull request.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
moSource :: Lens.Lens' MergeOperations (Core.Maybe Types.ChangeTypeEnum)
moSource = Lens.field @"source"
{-# DEPRECATED moSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Core.FromJSON MergeOperations where
  parseJSON =
    Core.withObject "MergeOperations" Core.$
      \x ->
        MergeOperations'
          Core.<$> (x Core..:? "destination") Core.<*> (x Core..:? "source")
