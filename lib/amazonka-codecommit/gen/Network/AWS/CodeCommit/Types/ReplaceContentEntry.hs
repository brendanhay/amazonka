{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ReplaceContentEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ReplaceContentEntry
  ( ReplaceContentEntry (..),

    -- * Smart constructor
    mkReplaceContentEntry,

    -- * Lenses
    rceFilePath,
    rceReplacementType,
    rceContent,
    rceFileMode,
  )
where

import qualified Network.AWS.CodeCommit.Types.FileModeTypeEnum as Types
import qualified Network.AWS.CodeCommit.Types.Path as Types
import qualified Network.AWS.CodeCommit.Types.ReplacementTypeEnum as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a replacement content entry in the conflict of a merge or pull request operation.
--
-- /See:/ 'mkReplaceContentEntry' smart constructor.
data ReplaceContentEntry = ReplaceContentEntry'
  { -- | The path of the conflicting file.
    filePath :: Types.Path,
    -- | The replacement type to use when determining how to resolve the conflict.
    replacementType :: Types.ReplacementTypeEnum,
    -- | The base-64 encoded content to use when the replacement type is USE_NEW_CONTENT.
    content :: Core.Maybe Core.Base64,
    -- | The file mode to apply during conflict resoltion.
    fileMode :: Core.Maybe Types.FileModeTypeEnum
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceContentEntry' value with any optional fields omitted.
mkReplaceContentEntry ::
  -- | 'filePath'
  Types.Path ->
  -- | 'replacementType'
  Types.ReplacementTypeEnum ->
  ReplaceContentEntry
mkReplaceContentEntry filePath replacementType =
  ReplaceContentEntry'
    { filePath,
      replacementType,
      content = Core.Nothing,
      fileMode = Core.Nothing
    }

-- | The path of the conflicting file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceFilePath :: Lens.Lens' ReplaceContentEntry Types.Path
rceFilePath = Lens.field @"filePath"
{-# DEPRECATED rceFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The replacement type to use when determining how to resolve the conflict.
--
-- /Note:/ Consider using 'replacementType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceReplacementType :: Lens.Lens' ReplaceContentEntry Types.ReplacementTypeEnum
rceReplacementType = Lens.field @"replacementType"
{-# DEPRECATED rceReplacementType "Use generic-lens or generic-optics with 'replacementType' instead." #-}

-- | The base-64 encoded content to use when the replacement type is USE_NEW_CONTENT.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceContent :: Lens.Lens' ReplaceContentEntry (Core.Maybe Core.Base64)
rceContent = Lens.field @"content"
{-# DEPRECATED rceContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The file mode to apply during conflict resoltion.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceFileMode :: Lens.Lens' ReplaceContentEntry (Core.Maybe Types.FileModeTypeEnum)
rceFileMode = Lens.field @"fileMode"
{-# DEPRECATED rceFileMode "Use generic-lens or generic-optics with 'fileMode' instead." #-}

instance Core.FromJSON ReplaceContentEntry where
  toJSON ReplaceContentEntry {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("filePath" Core..= filePath),
            Core.Just ("replacementType" Core..= replacementType),
            ("content" Core..=) Core.<$> content,
            ("fileMode" Core..=) Core.<$> fileMode
          ]
      )
