{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.BlobMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BlobMetadata
  ( BlobMetadata (..),

    -- * Smart constructor
    mkBlobMetadata,

    -- * Lenses
    bmPath,
    bmMode,
    bmBlobId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a specific Git blob object.
--
-- /See:/ 'mkBlobMetadata' smart constructor.
data BlobMetadata = BlobMetadata'
  { path :: Lude.Maybe Lude.Text,
    mode :: Lude.Maybe Lude.Text,
    blobId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BlobMetadata' with the minimum fields required to make a request.
--
-- * 'blobId' - The full ID of the blob.
-- * 'mode' - The file mode permissions of the blob. File mode permission codes include:
--
--
--     * @100644@ indicates read/write
--
--
--     * @100755@ indicates read/write/execute
--
--
--     * @160000@ indicates a submodule
--
--
--     * @120000@ indicates a symlink
--
--
-- * 'path' - The path to the blob and associated file name, if any.
mkBlobMetadata ::
  BlobMetadata
mkBlobMetadata =
  BlobMetadata'
    { path = Lude.Nothing,
      mode = Lude.Nothing,
      blobId = Lude.Nothing
    }

-- | The path to the blob and associated file name, if any.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmPath :: Lens.Lens' BlobMetadata (Lude.Maybe Lude.Text)
bmPath = Lens.lens (path :: BlobMetadata -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: BlobMetadata)
{-# DEPRECATED bmPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The file mode permissions of the blob. File mode permission codes include:
--
--
--     * @100644@ indicates read/write
--
--
--     * @100755@ indicates read/write/execute
--
--
--     * @160000@ indicates a submodule
--
--
--     * @120000@ indicates a symlink
--
--
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmMode :: Lens.Lens' BlobMetadata (Lude.Maybe Lude.Text)
bmMode = Lens.lens (mode :: BlobMetadata -> Lude.Maybe Lude.Text) (\s a -> s {mode = a} :: BlobMetadata)
{-# DEPRECATED bmMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The full ID of the blob.
--
-- /Note:/ Consider using 'blobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmBlobId :: Lens.Lens' BlobMetadata (Lude.Maybe Lude.Text)
bmBlobId = Lens.lens (blobId :: BlobMetadata -> Lude.Maybe Lude.Text) (\s a -> s {blobId = a} :: BlobMetadata)
{-# DEPRECATED bmBlobId "Use generic-lens or generic-optics with 'blobId' instead." #-}

instance Lude.FromJSON BlobMetadata where
  parseJSON =
    Lude.withObject
      "BlobMetadata"
      ( \x ->
          BlobMetadata'
            Lude.<$> (x Lude..:? "path")
            Lude.<*> (x Lude..:? "mode")
            Lude.<*> (x Lude..:? "blobId")
      )
