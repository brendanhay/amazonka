{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.FileSizes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.FileSizes
  ( FileSizes (..),

    -- * Smart constructor
    mkFileSizes,

    -- * Lenses
    fsBase,
    fsDestination,
    fsSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the size of files in a merge or pull request.
--
-- /See:/ 'mkFileSizes' smart constructor.
data FileSizes = FileSizes'
  { -- | The size of a file in the base of a merge or pull request.
    base :: Core.Maybe Core.Integer,
    -- | The size of a file in the destination of a merge or pull request.
    destination :: Core.Maybe Core.Integer,
    -- | The size of a file in the source of a merge or pull request.
    source :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FileSizes' value with any optional fields omitted.
mkFileSizes ::
  FileSizes
mkFileSizes =
  FileSizes'
    { base = Core.Nothing,
      destination = Core.Nothing,
      source = Core.Nothing
    }

-- | The size of a file in the base of a merge or pull request.
--
-- /Note:/ Consider using 'base' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsBase :: Lens.Lens' FileSizes (Core.Maybe Core.Integer)
fsBase = Lens.field @"base"
{-# DEPRECATED fsBase "Use generic-lens or generic-optics with 'base' instead." #-}

-- | The size of a file in the destination of a merge or pull request.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsDestination :: Lens.Lens' FileSizes (Core.Maybe Core.Integer)
fsDestination = Lens.field @"destination"
{-# DEPRECATED fsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The size of a file in the source of a merge or pull request.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsSource :: Lens.Lens' FileSizes (Core.Maybe Core.Integer)
fsSource = Lens.field @"source"
{-# DEPRECATED fsSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Core.FromJSON FileSizes where
  parseJSON =
    Core.withObject "FileSizes" Core.$
      \x ->
        FileSizes'
          Core.<$> (x Core..:? "base")
          Core.<*> (x Core..:? "destination")
          Core.<*> (x Core..:? "source")
