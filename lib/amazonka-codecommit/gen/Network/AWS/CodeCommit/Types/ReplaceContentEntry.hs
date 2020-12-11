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
    rceFileMode,
    rceContent,
    rceFilePath,
    rceReplacementType,
  )
where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import Network.AWS.CodeCommit.Types.ReplacementTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a replacement content entry in the conflict of a merge or pull request operation.
--
-- /See:/ 'mkReplaceContentEntry' smart constructor.
data ReplaceContentEntry = ReplaceContentEntry'
  { fileMode ::
      Lude.Maybe FileModeTypeEnum,
    content :: Lude.Maybe Lude.Base64,
    filePath :: Lude.Text,
    replacementType :: ReplacementTypeEnum
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplaceContentEntry' with the minimum fields required to make a request.
--
-- * 'content' - The base-64 encoded content to use when the replacement type is USE_NEW_CONTENT.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'fileMode' - The file mode to apply during conflict resoltion.
-- * 'filePath' - The path of the conflicting file.
-- * 'replacementType' - The replacement type to use when determining how to resolve the conflict.
mkReplaceContentEntry ::
  -- | 'filePath'
  Lude.Text ->
  -- | 'replacementType'
  ReplacementTypeEnum ->
  ReplaceContentEntry
mkReplaceContentEntry pFilePath_ pReplacementType_ =
  ReplaceContentEntry'
    { fileMode = Lude.Nothing,
      content = Lude.Nothing,
      filePath = pFilePath_,
      replacementType = pReplacementType_
    }

-- | The file mode to apply during conflict resoltion.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceFileMode :: Lens.Lens' ReplaceContentEntry (Lude.Maybe FileModeTypeEnum)
rceFileMode = Lens.lens (fileMode :: ReplaceContentEntry -> Lude.Maybe FileModeTypeEnum) (\s a -> s {fileMode = a} :: ReplaceContentEntry)
{-# DEPRECATED rceFileMode "Use generic-lens or generic-optics with 'fileMode' instead." #-}

-- | The base-64 encoded content to use when the replacement type is USE_NEW_CONTENT.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceContent :: Lens.Lens' ReplaceContentEntry (Lude.Maybe Lude.Base64)
rceContent = Lens.lens (content :: ReplaceContentEntry -> Lude.Maybe Lude.Base64) (\s a -> s {content = a} :: ReplaceContentEntry)
{-# DEPRECATED rceContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The path of the conflicting file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceFilePath :: Lens.Lens' ReplaceContentEntry Lude.Text
rceFilePath = Lens.lens (filePath :: ReplaceContentEntry -> Lude.Text) (\s a -> s {filePath = a} :: ReplaceContentEntry)
{-# DEPRECATED rceFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The replacement type to use when determining how to resolve the conflict.
--
-- /Note:/ Consider using 'replacementType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceReplacementType :: Lens.Lens' ReplaceContentEntry ReplacementTypeEnum
rceReplacementType = Lens.lens (replacementType :: ReplaceContentEntry -> ReplacementTypeEnum) (\s a -> s {replacementType = a} :: ReplaceContentEntry)
{-# DEPRECATED rceReplacementType "Use generic-lens or generic-optics with 'replacementType' instead." #-}

instance Lude.ToJSON ReplaceContentEntry where
  toJSON ReplaceContentEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fileMode" Lude..=) Lude.<$> fileMode,
            ("content" Lude..=) Lude.<$> content,
            Lude.Just ("filePath" Lude..= filePath),
            Lude.Just ("replacementType" Lude..= replacementType)
          ]
      )
