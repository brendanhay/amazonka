{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.FileMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.FileMetadata
  ( FileMetadata (..),

    -- * Smart constructor
    mkFileMetadata,

    -- * Lenses
    fmAbsolutePath,
    fmFileMode,
    fmBlobId,
  )
where

import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A file to be added, updated, or deleted as part of a commit.
--
-- /See:/ 'mkFileMetadata' smart constructor.
data FileMetadata = FileMetadata'
  { -- | The full path to the file to be added or updated, including the name of the file.
    absolutePath :: Lude.Maybe Lude.Text,
    -- | The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
    fileMode :: Lude.Maybe FileModeTypeEnum,
    -- | The blob ID that contains the file information.
    blobId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FileMetadata' with the minimum fields required to make a request.
--
-- * 'absolutePath' - The full path to the file to be added or updated, including the name of the file.
-- * 'fileMode' - The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
-- * 'blobId' - The blob ID that contains the file information.
mkFileMetadata ::
  FileMetadata
mkFileMetadata =
  FileMetadata'
    { absolutePath = Lude.Nothing,
      fileMode = Lude.Nothing,
      blobId = Lude.Nothing
    }

-- | The full path to the file to be added or updated, including the name of the file.
--
-- /Note:/ Consider using 'absolutePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmAbsolutePath :: Lens.Lens' FileMetadata (Lude.Maybe Lude.Text)
fmAbsolutePath = Lens.lens (absolutePath :: FileMetadata -> Lude.Maybe Lude.Text) (\s a -> s {absolutePath = a} :: FileMetadata)
{-# DEPRECATED fmAbsolutePath "Use generic-lens or generic-optics with 'absolutePath' instead." #-}

-- | The extrapolated file mode permissions for the file. Valid values include EXECUTABLE and NORMAL.
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmFileMode :: Lens.Lens' FileMetadata (Lude.Maybe FileModeTypeEnum)
fmFileMode = Lens.lens (fileMode :: FileMetadata -> Lude.Maybe FileModeTypeEnum) (\s a -> s {fileMode = a} :: FileMetadata)
{-# DEPRECATED fmFileMode "Use generic-lens or generic-optics with 'fileMode' instead." #-}

-- | The blob ID that contains the file information.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmBlobId :: Lens.Lens' FileMetadata (Lude.Maybe Lude.Text)
fmBlobId = Lens.lens (blobId :: FileMetadata -> Lude.Maybe Lude.Text) (\s a -> s {blobId = a} :: FileMetadata)
{-# DEPRECATED fmBlobId "Use generic-lens or generic-optics with 'blobId' instead." #-}

instance Lude.FromJSON FileMetadata where
  parseJSON =
    Lude.withObject
      "FileMetadata"
      ( \x ->
          FileMetadata'
            Lude.<$> (x Lude..:? "absolutePath")
            Lude.<*> (x Lude..:? "fileMode")
            Lude.<*> (x Lude..:? "blobId")
      )
