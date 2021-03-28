{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.SourceFileSpecifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.SourceFileSpecifier
  ( SourceFileSpecifier (..)
  -- * Smart constructor
  , mkSourceFileSpecifier
  -- * Lenses
  , sfsFilePath
  , sfsIsMove
  ) where

import qualified Network.AWS.CodeCommit.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a source file that is part of changes made in a commit.
--
-- /See:/ 'mkSourceFileSpecifier' smart constructor.
data SourceFileSpecifier = SourceFileSpecifier'
  { filePath :: Types.Path
    -- ^ The full path to the file, including the name of the file.
  , isMove :: Core.Maybe Core.Bool
    -- ^ Whether to remove the source file from the parent commit.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SourceFileSpecifier' value with any optional fields omitted.
mkSourceFileSpecifier
    :: Types.Path -- ^ 'filePath'
    -> SourceFileSpecifier
mkSourceFileSpecifier filePath
  = SourceFileSpecifier'{filePath, isMove = Core.Nothing}

-- | The full path to the file, including the name of the file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsFilePath :: Lens.Lens' SourceFileSpecifier Types.Path
sfsFilePath = Lens.field @"filePath"
{-# INLINEABLE sfsFilePath #-}
{-# DEPRECATED filePath "Use generic-lens or generic-optics with 'filePath' instead"  #-}

-- | Whether to remove the source file from the parent commit.
--
-- /Note:/ Consider using 'isMove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsIsMove :: Lens.Lens' SourceFileSpecifier (Core.Maybe Core.Bool)
sfsIsMove = Lens.field @"isMove"
{-# INLINEABLE sfsIsMove #-}
{-# DEPRECATED isMove "Use generic-lens or generic-optics with 'isMove' instead"  #-}

instance Core.FromJSON SourceFileSpecifier where
        toJSON SourceFileSpecifier{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("filePath" Core..= filePath),
                  ("isMove" Core..=) Core.<$> isMove])
