{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.File
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.File
  ( File (..),

    -- * Smart constructor
    mkFile,

    -- * Lenses
    fAbsolutePath,
    fFileMode,
    fBlobId,
    fRelativePath,
  )
where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a file in a repository.
--
-- /See:/ 'mkFile' smart constructor.
data File = File'
  { absolutePath :: Lude.Maybe Lude.Text,
    fileMode :: Lude.Maybe FileModeTypeEnum,
    blobId :: Lude.Maybe Lude.Text,
    relativePath :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'File' with the minimum fields required to make a request.
--
-- * 'absolutePath' - The fully qualified path to the file in the repository.
-- * 'blobId' - The blob ID that contains the file information.
-- * 'fileMode' - The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
-- * 'relativePath' - The relative path of the file from the folder where the query originated.
mkFile ::
  File
mkFile =
  File'
    { absolutePath = Lude.Nothing,
      fileMode = Lude.Nothing,
      blobId = Lude.Nothing,
      relativePath = Lude.Nothing
    }

-- | The fully qualified path to the file in the repository.
--
-- /Note:/ Consider using 'absolutePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fAbsolutePath :: Lens.Lens' File (Lude.Maybe Lude.Text)
fAbsolutePath = Lens.lens (absolutePath :: File -> Lude.Maybe Lude.Text) (\s a -> s {absolutePath = a} :: File)
{-# DEPRECATED fAbsolutePath "Use generic-lens or generic-optics with 'absolutePath' instead." #-}

-- | The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFileMode :: Lens.Lens' File (Lude.Maybe FileModeTypeEnum)
fFileMode = Lens.lens (fileMode :: File -> Lude.Maybe FileModeTypeEnum) (\s a -> s {fileMode = a} :: File)
{-# DEPRECATED fFileMode "Use generic-lens or generic-optics with 'fileMode' instead." #-}

-- | The blob ID that contains the file information.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fBlobId :: Lens.Lens' File (Lude.Maybe Lude.Text)
fBlobId = Lens.lens (blobId :: File -> Lude.Maybe Lude.Text) (\s a -> s {blobId = a} :: File)
{-# DEPRECATED fBlobId "Use generic-lens or generic-optics with 'blobId' instead." #-}

-- | The relative path of the file from the folder where the query originated.
--
-- /Note:/ Consider using 'relativePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fRelativePath :: Lens.Lens' File (Lude.Maybe Lude.Text)
fRelativePath = Lens.lens (relativePath :: File -> Lude.Maybe Lude.Text) (\s a -> s {relativePath = a} :: File)
{-# DEPRECATED fRelativePath "Use generic-lens or generic-optics with 'relativePath' instead." #-}

instance Lude.FromJSON File where
  parseJSON =
    Lude.withObject
      "File"
      ( \x ->
          File'
            Lude.<$> (x Lude..:? "absolutePath")
            Lude.<*> (x Lude..:? "fileMode")
            Lude.<*> (x Lude..:? "blobId")
            Lude.<*> (x Lude..:? "relativePath")
      )
