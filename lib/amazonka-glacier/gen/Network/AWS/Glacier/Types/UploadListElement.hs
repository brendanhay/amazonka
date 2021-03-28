{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.UploadListElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.UploadListElement
  ( UploadListElement (..)
  -- * Smart constructor
  , mkUploadListElement
  -- * Lenses
  , uleArchiveDescription
  , uleCreationDate
  , uleMultipartUploadId
  , ulePartSizeInBytes
  , uleVaultARN
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of in-progress multipart uploads for a vault.
--
-- /See:/ 'mkUploadListElement' smart constructor.
data UploadListElement = UploadListElement'
  { archiveDescription :: Core.Maybe Core.Text
    -- ^ The description of the archive that was specified in the Initiate Multipart Upload request.
  , creationDate :: Core.Maybe Core.Text
    -- ^ The UTC time at which the multipart upload was initiated.
  , multipartUploadId :: Core.Maybe Core.Text
    -- ^ The ID of a multipart upload.
  , partSizeInBytes :: Core.Maybe Core.Integer
    -- ^ The part size, in bytes, specified in the Initiate Multipart Upload request. This is the size of all the parts in the upload except the last part, which may be smaller than this size.
  , vaultARN :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the vault that contains the archive.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UploadListElement' value with any optional fields omitted.
mkUploadListElement
    :: UploadListElement
mkUploadListElement
  = UploadListElement'{archiveDescription = Core.Nothing,
                       creationDate = Core.Nothing, multipartUploadId = Core.Nothing,
                       partSizeInBytes = Core.Nothing, vaultARN = Core.Nothing}

-- | The description of the archive that was specified in the Initiate Multipart Upload request.
--
-- /Note:/ Consider using 'archiveDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uleArchiveDescription :: Lens.Lens' UploadListElement (Core.Maybe Core.Text)
uleArchiveDescription = Lens.field @"archiveDescription"
{-# INLINEABLE uleArchiveDescription #-}
{-# DEPRECATED archiveDescription "Use generic-lens or generic-optics with 'archiveDescription' instead"  #-}

-- | The UTC time at which the multipart upload was initiated.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uleCreationDate :: Lens.Lens' UploadListElement (Core.Maybe Core.Text)
uleCreationDate = Lens.field @"creationDate"
{-# INLINEABLE uleCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The ID of a multipart upload.
--
-- /Note:/ Consider using 'multipartUploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uleMultipartUploadId :: Lens.Lens' UploadListElement (Core.Maybe Core.Text)
uleMultipartUploadId = Lens.field @"multipartUploadId"
{-# INLINEABLE uleMultipartUploadId #-}
{-# DEPRECATED multipartUploadId "Use generic-lens or generic-optics with 'multipartUploadId' instead"  #-}

-- | The part size, in bytes, specified in the Initiate Multipart Upload request. This is the size of all the parts in the upload except the last part, which may be smaller than this size.
--
-- /Note:/ Consider using 'partSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulePartSizeInBytes :: Lens.Lens' UploadListElement (Core.Maybe Core.Integer)
ulePartSizeInBytes = Lens.field @"partSizeInBytes"
{-# INLINEABLE ulePartSizeInBytes #-}
{-# DEPRECATED partSizeInBytes "Use generic-lens or generic-optics with 'partSizeInBytes' instead"  #-}

-- | The Amazon Resource Name (ARN) of the vault that contains the archive.
--
-- /Note:/ Consider using 'vaultARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uleVaultARN :: Lens.Lens' UploadListElement (Core.Maybe Core.Text)
uleVaultARN = Lens.field @"vaultARN"
{-# INLINEABLE uleVaultARN #-}
{-# DEPRECATED vaultARN "Use generic-lens or generic-optics with 'vaultARN' instead"  #-}

instance Core.FromJSON UploadListElement where
        parseJSON
          = Core.withObject "UploadListElement" Core.$
              \ x ->
                UploadListElement' Core.<$>
                  (x Core..:? "ArchiveDescription") Core.<*>
                    x Core..:? "CreationDate"
                    Core.<*> x Core..:? "MultipartUploadId"
                    Core.<*> x Core..:? "PartSizeInBytes"
                    Core.<*> x Core..:? "VaultARN"
