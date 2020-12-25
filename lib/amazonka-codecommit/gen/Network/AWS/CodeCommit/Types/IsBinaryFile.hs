{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.IsBinaryFile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.IsBinaryFile
  ( IsBinaryFile (..),

    -- * Smart constructor
    mkIsBinaryFile,

    -- * Lenses
    ibfBase,
    ibfDestination,
    ibfSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about whether a file is binary or textual in a merge or pull request operation.
--
-- /See:/ 'mkIsBinaryFile' smart constructor.
data IsBinaryFile = IsBinaryFile'
  { -- | The binary or non-binary status of a file in the base of a merge or pull request.
    base :: Core.Maybe Core.Bool,
    -- | The binary or non-binary status of a file in the destination of a merge or pull request.
    destination :: Core.Maybe Core.Bool,
    -- | The binary or non-binary status of file in the source of a merge or pull request.
    source :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IsBinaryFile' value with any optional fields omitted.
mkIsBinaryFile ::
  IsBinaryFile
mkIsBinaryFile =
  IsBinaryFile'
    { base = Core.Nothing,
      destination = Core.Nothing,
      source = Core.Nothing
    }

-- | The binary or non-binary status of a file in the base of a merge or pull request.
--
-- /Note:/ Consider using 'base' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibfBase :: Lens.Lens' IsBinaryFile (Core.Maybe Core.Bool)
ibfBase = Lens.field @"base"
{-# DEPRECATED ibfBase "Use generic-lens or generic-optics with 'base' instead." #-}

-- | The binary or non-binary status of a file in the destination of a merge or pull request.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibfDestination :: Lens.Lens' IsBinaryFile (Core.Maybe Core.Bool)
ibfDestination = Lens.field @"destination"
{-# DEPRECATED ibfDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The binary or non-binary status of file in the source of a merge or pull request.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibfSource :: Lens.Lens' IsBinaryFile (Core.Maybe Core.Bool)
ibfSource = Lens.field @"source"
{-# DEPRECATED ibfSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Core.FromJSON IsBinaryFile where
  parseJSON =
    Core.withObject "IsBinaryFile" Core.$
      \x ->
        IsBinaryFile'
          Core.<$> (x Core..:? "base")
          Core.<*> (x Core..:? "destination")
          Core.<*> (x Core..:? "source")
