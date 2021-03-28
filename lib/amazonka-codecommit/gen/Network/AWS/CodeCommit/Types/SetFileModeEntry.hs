{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.SetFileModeEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.SetFileModeEntry
  ( SetFileModeEntry (..)
  -- * Smart constructor
  , mkSetFileModeEntry
  -- * Lenses
  , sfmeFilePath
  , sfmeFileMode
  ) where

import qualified Network.AWS.CodeCommit.Types.FileModeTypeEnum as Types
import qualified Network.AWS.CodeCommit.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the file mode changes.
--
-- /See:/ 'mkSetFileModeEntry' smart constructor.
data SetFileModeEntry = SetFileModeEntry'
  { filePath :: Types.Path
    -- ^ The full path to the file, including the name of the file.
  , fileMode :: Types.FileModeTypeEnum
    -- ^ The file mode for the file.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetFileModeEntry' value with any optional fields omitted.
mkSetFileModeEntry
    :: Types.Path -- ^ 'filePath'
    -> Types.FileModeTypeEnum -- ^ 'fileMode'
    -> SetFileModeEntry
mkSetFileModeEntry filePath fileMode
  = SetFileModeEntry'{filePath, fileMode}

-- | The full path to the file, including the name of the file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfmeFilePath :: Lens.Lens' SetFileModeEntry Types.Path
sfmeFilePath = Lens.field @"filePath"
{-# INLINEABLE sfmeFilePath #-}
{-# DEPRECATED filePath "Use generic-lens or generic-optics with 'filePath' instead"  #-}

-- | The file mode for the file.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfmeFileMode :: Lens.Lens' SetFileModeEntry Types.FileModeTypeEnum
sfmeFileMode = Lens.field @"fileMode"
{-# INLINEABLE sfmeFileMode #-}
{-# DEPRECATED fileMode "Use generic-lens or generic-optics with 'fileMode' instead"  #-}

instance Core.FromJSON SetFileModeEntry where
        toJSON SetFileModeEntry{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("filePath" Core..= filePath),
                  Core.Just ("fileMode" Core..= fileMode)])
