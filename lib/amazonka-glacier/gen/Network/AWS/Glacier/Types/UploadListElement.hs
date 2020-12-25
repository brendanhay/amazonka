{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    uleArchiveDescription,
    uleCreationDate,
    uleMultipartUploadId,
    ulePartSizeInBytes,
    uleVaultARN,
  )
where

import qualified Network.AWS.Glacier.Types.ArchiveDescription as Types
import qualified Network.AWS.Glacier.Types.CreationDate as Types
import qualified Network.AWS.Glacier.Types.MultipartUploadId as Types
import qualified Network.AWS.Glacier.Types.VaultARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of in-progress multipart uploads for a vault.
--
-- /See:/ 'mkUploadListElement' smart constructor.
data UploadListElement = UploadListElement'
  { -- | The description of the archive that was specified in the Initiate Multipart Upload request.
    archiveDescription :: Core.Maybe Types.ArchiveDescription,
    -- | The UTC time at which the multipart upload was initiated.
    creationDate :: Core.Maybe Types.CreationDate,
    -- | The ID of a multipart upload.
    multipartUploadId :: Core.Maybe Types.MultipartUploadId,
    -- | The part size, in bytes, specified in the Initiate Multipart Upload request. This is the size of all the parts in the upload except the last part, which may be smaller than this size.
    partSizeInBytes :: Core.Maybe Core.Integer,
    -- | The Amazon Resource Name (ARN) of the vault that contains the archive.
    vaultARN :: Core.Maybe Types.VaultARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UploadListElement' value with any optional fields omitted.
mkUploadListElement ::
  UploadListElement
mkUploadListElement =
  UploadListElement'
    { archiveDescription = Core.Nothing,
      creationDate = Core.Nothing,
      multipartUploadId = Core.Nothing,
      partSizeInBytes = Core.Nothing,
      vaultARN = Core.Nothing
    }

-- | The description of the archive that was specified in the Initiate Multipart Upload request.
--
-- /Note:/ Consider using 'archiveDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uleArchiveDescription :: Lens.Lens' UploadListElement (Core.Maybe Types.ArchiveDescription)
uleArchiveDescription = Lens.field @"archiveDescription"
{-# DEPRECATED uleArchiveDescription "Use generic-lens or generic-optics with 'archiveDescription' instead." #-}

-- | The UTC time at which the multipart upload was initiated.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uleCreationDate :: Lens.Lens' UploadListElement (Core.Maybe Types.CreationDate)
uleCreationDate = Lens.field @"creationDate"
{-# DEPRECATED uleCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The ID of a multipart upload.
--
-- /Note:/ Consider using 'multipartUploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uleMultipartUploadId :: Lens.Lens' UploadListElement (Core.Maybe Types.MultipartUploadId)
uleMultipartUploadId = Lens.field @"multipartUploadId"
{-# DEPRECATED uleMultipartUploadId "Use generic-lens or generic-optics with 'multipartUploadId' instead." #-}

-- | The part size, in bytes, specified in the Initiate Multipart Upload request. This is the size of all the parts in the upload except the last part, which may be smaller than this size.
--
-- /Note:/ Consider using 'partSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulePartSizeInBytes :: Lens.Lens' UploadListElement (Core.Maybe Core.Integer)
ulePartSizeInBytes = Lens.field @"partSizeInBytes"
{-# DEPRECATED ulePartSizeInBytes "Use generic-lens or generic-optics with 'partSizeInBytes' instead." #-}

-- | The Amazon Resource Name (ARN) of the vault that contains the archive.
--
-- /Note:/ Consider using 'vaultARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uleVaultARN :: Lens.Lens' UploadListElement (Core.Maybe Types.VaultARN)
uleVaultARN = Lens.field @"vaultARN"
{-# DEPRECATED uleVaultARN "Use generic-lens or generic-optics with 'vaultARN' instead." #-}

instance Core.FromJSON UploadListElement where
  parseJSON =
    Core.withObject "UploadListElement" Core.$
      \x ->
        UploadListElement'
          Core.<$> (x Core..:? "ArchiveDescription")
          Core.<*> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "MultipartUploadId")
          Core.<*> (x Core..:? "PartSizeInBytes")
          Core.<*> (x Core..:? "VaultARN")
