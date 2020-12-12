{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.SetFileModeEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.SetFileModeEntry
  ( SetFileModeEntry (..),

    -- * Smart constructor
    mkSetFileModeEntry,

    -- * Lenses
    sfmeFilePath,
    sfmeFileMode,
  )
where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the file mode changes.
--
-- /See:/ 'mkSetFileModeEntry' smart constructor.
data SetFileModeEntry = SetFileModeEntry'
  { filePath :: Lude.Text,
    fileMode :: FileModeTypeEnum
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetFileModeEntry' with the minimum fields required to make a request.
--
-- * 'fileMode' - The file mode for the file.
-- * 'filePath' - The full path to the file, including the name of the file.
mkSetFileModeEntry ::
  -- | 'filePath'
  Lude.Text ->
  -- | 'fileMode'
  FileModeTypeEnum ->
  SetFileModeEntry
mkSetFileModeEntry pFilePath_ pFileMode_ =
  SetFileModeEntry' {filePath = pFilePath_, fileMode = pFileMode_}

-- | The full path to the file, including the name of the file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfmeFilePath :: Lens.Lens' SetFileModeEntry Lude.Text
sfmeFilePath = Lens.lens (filePath :: SetFileModeEntry -> Lude.Text) (\s a -> s {filePath = a} :: SetFileModeEntry)
{-# DEPRECATED sfmeFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The file mode for the file.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfmeFileMode :: Lens.Lens' SetFileModeEntry FileModeTypeEnum
sfmeFileMode = Lens.lens (fileMode :: SetFileModeEntry -> FileModeTypeEnum) (\s a -> s {fileMode = a} :: SetFileModeEntry)
{-# DEPRECATED sfmeFileMode "Use generic-lens or generic-optics with 'fileMode' instead." #-}

instance Lude.ToJSON SetFileModeEntry where
  toJSON SetFileModeEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("filePath" Lude..= filePath),
            Lude.Just ("fileMode" Lude..= fileMode)
          ]
      )
