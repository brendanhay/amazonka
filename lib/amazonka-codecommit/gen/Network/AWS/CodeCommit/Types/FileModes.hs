{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.FileModes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.FileModes
  ( FileModes (..)
  -- * Smart constructor
  , mkFileModes
  -- * Lenses
  , fmBase
  , fmDestination
  , fmSource
  ) where

import qualified Network.AWS.CodeCommit.Types.FileModeTypeEnum as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about file modes in a merge or pull request.
--
-- /See:/ 'mkFileModes' smart constructor.
data FileModes = FileModes'
  { base :: Core.Maybe Types.FileModeTypeEnum
    -- ^ The file mode of a file in the base of a merge or pull request.
  , destination :: Core.Maybe Types.FileModeTypeEnum
    -- ^ The file mode of a file in the destination of a merge or pull request.
  , source :: Core.Maybe Types.FileModeTypeEnum
    -- ^ The file mode of a file in the source of a merge or pull request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FileModes' value with any optional fields omitted.
mkFileModes
    :: FileModes
mkFileModes
  = FileModes'{base = Core.Nothing, destination = Core.Nothing,
               source = Core.Nothing}

-- | The file mode of a file in the base of a merge or pull request.
--
-- /Note:/ Consider using 'base' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmBase :: Lens.Lens' FileModes (Core.Maybe Types.FileModeTypeEnum)
fmBase = Lens.field @"base"
{-# INLINEABLE fmBase #-}
{-# DEPRECATED base "Use generic-lens or generic-optics with 'base' instead"  #-}

-- | The file mode of a file in the destination of a merge or pull request.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmDestination :: Lens.Lens' FileModes (Core.Maybe Types.FileModeTypeEnum)
fmDestination = Lens.field @"destination"
{-# INLINEABLE fmDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

-- | The file mode of a file in the source of a merge or pull request.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmSource :: Lens.Lens' FileModes (Core.Maybe Types.FileModeTypeEnum)
fmSource = Lens.field @"source"
{-# INLINEABLE fmSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

instance Core.FromJSON FileModes where
        parseJSON
          = Core.withObject "FileModes" Core.$
              \ x ->
                FileModes' Core.<$>
                  (x Core..:? "base") Core.<*> x Core..:? "destination" Core.<*>
                    x Core..:? "source"
