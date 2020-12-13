{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PutFileEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PutFileEntry
  ( PutFileEntry (..),

    -- * Smart constructor
    mkPutFileEntry,

    -- * Lenses
    pfeFileContent,
    pfeFileMode,
    pfeFilePath,
    pfeSourceFile,
  )
where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import Network.AWS.CodeCommit.Types.SourceFileSpecifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a file added or updated as part of a commit.
--
-- /See:/ 'mkPutFileEntry' smart constructor.
data PutFileEntry = PutFileEntry'
  { -- | The content of the file, if a source file is not specified.
    fileContent :: Lude.Maybe Lude.Base64,
    -- | The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
    fileMode :: Lude.Maybe FileModeTypeEnum,
    -- | The full path to the file in the repository, including the name of the file.
    filePath :: Lude.Text,
    -- | The name and full path of the file that contains the changes you want to make as part of the commit, if you are not providing the file content directly.
    sourceFile :: Lude.Maybe SourceFileSpecifier
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutFileEntry' with the minimum fields required to make a request.
--
-- * 'fileContent' - The content of the file, if a source file is not specified.
-- * 'fileMode' - The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
-- * 'filePath' - The full path to the file in the repository, including the name of the file.
-- * 'sourceFile' - The name and full path of the file that contains the changes you want to make as part of the commit, if you are not providing the file content directly.
mkPutFileEntry ::
  -- | 'filePath'
  Lude.Text ->
  PutFileEntry
mkPutFileEntry pFilePath_ =
  PutFileEntry'
    { fileContent = Lude.Nothing,
      fileMode = Lude.Nothing,
      filePath = pFilePath_,
      sourceFile = Lude.Nothing
    }

-- | The content of the file, if a source file is not specified.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'fileContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfeFileContent :: Lens.Lens' PutFileEntry (Lude.Maybe Lude.Base64)
pfeFileContent = Lens.lens (fileContent :: PutFileEntry -> Lude.Maybe Lude.Base64) (\s a -> s {fileContent = a} :: PutFileEntry)
{-# DEPRECATED pfeFileContent "Use generic-lens or generic-optics with 'fileContent' instead." #-}

-- | The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfeFileMode :: Lens.Lens' PutFileEntry (Lude.Maybe FileModeTypeEnum)
pfeFileMode = Lens.lens (fileMode :: PutFileEntry -> Lude.Maybe FileModeTypeEnum) (\s a -> s {fileMode = a} :: PutFileEntry)
{-# DEPRECATED pfeFileMode "Use generic-lens or generic-optics with 'fileMode' instead." #-}

-- | The full path to the file in the repository, including the name of the file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfeFilePath :: Lens.Lens' PutFileEntry Lude.Text
pfeFilePath = Lens.lens (filePath :: PutFileEntry -> Lude.Text) (\s a -> s {filePath = a} :: PutFileEntry)
{-# DEPRECATED pfeFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | The name and full path of the file that contains the changes you want to make as part of the commit, if you are not providing the file content directly.
--
-- /Note:/ Consider using 'sourceFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfeSourceFile :: Lens.Lens' PutFileEntry (Lude.Maybe SourceFileSpecifier)
pfeSourceFile = Lens.lens (sourceFile :: PutFileEntry -> Lude.Maybe SourceFileSpecifier) (\s a -> s {sourceFile = a} :: PutFileEntry)
{-# DEPRECATED pfeSourceFile "Use generic-lens or generic-optics with 'sourceFile' instead." #-}

instance Lude.ToJSON PutFileEntry where
  toJSON PutFileEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fileContent" Lude..=) Lude.<$> fileContent,
            ("fileMode" Lude..=) Lude.<$> fileMode,
            Lude.Just ("filePath" Lude..= filePath),
            ("sourceFile" Lude..=) Lude.<$> sourceFile
          ]
      )
