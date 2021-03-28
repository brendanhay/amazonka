{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.MultipartUpload
  ( MultipartUpload (..)
  -- * Smart constructor
  , mkMultipartUpload
  -- * Lenses
  , muInitiated
  , muInitiator
  , muKey
  , muOwner
  , muStorageClass
  , muUploadId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Initiator as Types
import qualified Network.AWS.S3.Types.MultipartUploadId as Types
import qualified Network.AWS.S3.Types.Owner as Types
import qualified Network.AWS.S3.Types.StorageClass as Types

-- | Container for the @MultipartUpload@ for the Amazon S3 object.
--
-- /See:/ 'mkMultipartUpload' smart constructor.
data MultipartUpload = MultipartUpload'
  { initiated :: Core.Maybe Core.UTCTime
    -- ^ Date and time at which the multipart upload was initiated.
  , initiator :: Core.Maybe Types.Initiator
    -- ^ Identifies who initiated the multipart upload.
  , key :: Core.Maybe Types.ObjectKey
    -- ^ Key of the object for which the multipart upload was initiated.
  , owner :: Core.Maybe Types.Owner
    -- ^ Specifies the owner of the object that is part of the multipart upload. 
  , storageClass :: Core.Maybe Types.StorageClass
    -- ^ The class of storage used to store the object.
  , uploadId :: Core.Maybe Types.MultipartUploadId
    -- ^ Upload ID that identifies the multipart upload.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'MultipartUpload' value with any optional fields omitted.
mkMultipartUpload
    :: MultipartUpload
mkMultipartUpload
  = MultipartUpload'{initiated = Core.Nothing,
                     initiator = Core.Nothing, key = Core.Nothing, owner = Core.Nothing,
                     storageClass = Core.Nothing, uploadId = Core.Nothing}

-- | Date and time at which the multipart upload was initiated.
--
-- /Note:/ Consider using 'initiated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muInitiated :: Lens.Lens' MultipartUpload (Core.Maybe Core.UTCTime)
muInitiated = Lens.field @"initiated"
{-# INLINEABLE muInitiated #-}
{-# DEPRECATED initiated "Use generic-lens or generic-optics with 'initiated' instead"  #-}

-- | Identifies who initiated the multipart upload.
--
-- /Note:/ Consider using 'initiator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muInitiator :: Lens.Lens' MultipartUpload (Core.Maybe Types.Initiator)
muInitiator = Lens.field @"initiator"
{-# INLINEABLE muInitiator #-}
{-# DEPRECATED initiator "Use generic-lens or generic-optics with 'initiator' instead"  #-}

-- | Key of the object for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muKey :: Lens.Lens' MultipartUpload (Core.Maybe Types.ObjectKey)
muKey = Lens.field @"key"
{-# INLINEABLE muKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | Specifies the owner of the object that is part of the multipart upload. 
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muOwner :: Lens.Lens' MultipartUpload (Core.Maybe Types.Owner)
muOwner = Lens.field @"owner"
{-# INLINEABLE muOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | The class of storage used to store the object.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muStorageClass :: Lens.Lens' MultipartUpload (Core.Maybe Types.StorageClass)
muStorageClass = Lens.field @"storageClass"
{-# INLINEABLE muStorageClass #-}
{-# DEPRECATED storageClass "Use generic-lens or generic-optics with 'storageClass' instead"  #-}

-- | Upload ID that identifies the multipart upload.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muUploadId :: Lens.Lens' MultipartUpload (Core.Maybe Types.MultipartUploadId)
muUploadId = Lens.field @"uploadId"
{-# INLINEABLE muUploadId #-}
{-# DEPRECATED uploadId "Use generic-lens or generic-optics with 'uploadId' instead"  #-}

instance Core.FromXML MultipartUpload where
        parseXML x
          = MultipartUpload' Core.<$>
              (x Core..@? "Initiated") Core.<*> x Core..@? "Initiator" Core.<*>
                x Core..@? "Key"
                Core.<*> x Core..@? "Owner"
                Core.<*> x Core..@? "StorageClass"
                Core.<*> x Core..@? "UploadId"
