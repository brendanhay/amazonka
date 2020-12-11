-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.UploadListElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.UploadListElement
  ( UploadListElement (..),

    -- * Smart constructor
    mkUploadListElement,

    -- * Lenses
    uleMultipartUploadId,
    ulePartSizeInBytes,
    uleArchiveDescription,
    uleVaultARN,
    uleCreationDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of in-progress multipart uploads for a vault.
--
-- /See:/ 'mkUploadListElement' smart constructor.
data UploadListElement = UploadListElement'
  { multipartUploadId ::
      Lude.Maybe Lude.Text,
    partSizeInBytes :: Lude.Maybe Lude.Integer,
    archiveDescription :: Lude.Maybe Lude.Text,
    vaultARN :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UploadListElement' with the minimum fields required to make a request.
--
-- * 'archiveDescription' - The description of the archive that was specified in the Initiate Multipart Upload request.
-- * 'creationDate' - The UTC time at which the multipart upload was initiated.
-- * 'multipartUploadId' - The ID of a multipart upload.
-- * 'partSizeInBytes' - The part size, in bytes, specified in the Initiate Multipart Upload request. This is the size of all the parts in the upload except the last part, which may be smaller than this size.
-- * 'vaultARN' - The Amazon Resource Name (ARN) of the vault that contains the archive.
mkUploadListElement ::
  UploadListElement
mkUploadListElement =
  UploadListElement'
    { multipartUploadId = Lude.Nothing,
      partSizeInBytes = Lude.Nothing,
      archiveDescription = Lude.Nothing,
      vaultARN = Lude.Nothing,
      creationDate = Lude.Nothing
    }

-- | The ID of a multipart upload.
--
-- /Note:/ Consider using 'multipartUploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uleMultipartUploadId :: Lens.Lens' UploadListElement (Lude.Maybe Lude.Text)
uleMultipartUploadId = Lens.lens (multipartUploadId :: UploadListElement -> Lude.Maybe Lude.Text) (\s a -> s {multipartUploadId = a} :: UploadListElement)
{-# DEPRECATED uleMultipartUploadId "Use generic-lens or generic-optics with 'multipartUploadId' instead." #-}

-- | The part size, in bytes, specified in the Initiate Multipart Upload request. This is the size of all the parts in the upload except the last part, which may be smaller than this size.
--
-- /Note:/ Consider using 'partSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulePartSizeInBytes :: Lens.Lens' UploadListElement (Lude.Maybe Lude.Integer)
ulePartSizeInBytes = Lens.lens (partSizeInBytes :: UploadListElement -> Lude.Maybe Lude.Integer) (\s a -> s {partSizeInBytes = a} :: UploadListElement)
{-# DEPRECATED ulePartSizeInBytes "Use generic-lens or generic-optics with 'partSizeInBytes' instead." #-}

-- | The description of the archive that was specified in the Initiate Multipart Upload request.
--
-- /Note:/ Consider using 'archiveDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uleArchiveDescription :: Lens.Lens' UploadListElement (Lude.Maybe Lude.Text)
uleArchiveDescription = Lens.lens (archiveDescription :: UploadListElement -> Lude.Maybe Lude.Text) (\s a -> s {archiveDescription = a} :: UploadListElement)
{-# DEPRECATED uleArchiveDescription "Use generic-lens or generic-optics with 'archiveDescription' instead." #-}

-- | The Amazon Resource Name (ARN) of the vault that contains the archive.
--
-- /Note:/ Consider using 'vaultARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uleVaultARN :: Lens.Lens' UploadListElement (Lude.Maybe Lude.Text)
uleVaultARN = Lens.lens (vaultARN :: UploadListElement -> Lude.Maybe Lude.Text) (\s a -> s {vaultARN = a} :: UploadListElement)
{-# DEPRECATED uleVaultARN "Use generic-lens or generic-optics with 'vaultARN' instead." #-}

-- | The UTC time at which the multipart upload was initiated.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uleCreationDate :: Lens.Lens' UploadListElement (Lude.Maybe Lude.Text)
uleCreationDate = Lens.lens (creationDate :: UploadListElement -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: UploadListElement)
{-# DEPRECATED uleCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

instance Lude.FromJSON UploadListElement where
  parseJSON =
    Lude.withObject
      "UploadListElement"
      ( \x ->
          UploadListElement'
            Lude.<$> (x Lude..:? "MultipartUploadId")
            Lude.<*> (x Lude..:? "PartSizeInBytes")
            Lude.<*> (x Lude..:? "ArchiveDescription")
            Lude.<*> (x Lude..:? "VaultARN")
            Lude.<*> (x Lude..:? "CreationDate")
      )
