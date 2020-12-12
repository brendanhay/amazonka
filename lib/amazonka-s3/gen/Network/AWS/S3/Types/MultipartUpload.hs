{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MultipartUpload
  ( MultipartUpload (..),

    -- * Smart constructor
    mkMultipartUpload,

    -- * Lenses
    muInitiated,
    muInitiator,
    muOwner,
    muKey,
    muStorageClass,
    muUploadId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Initiator
import Network.AWS.S3.Types.Owner
import Network.AWS.S3.Types.StorageClass

-- | Container for the @MultipartUpload@ for the Amazon S3 object.
--
-- /See:/ 'mkMultipartUpload' smart constructor.
data MultipartUpload = MultipartUpload'
  { initiated ::
      Lude.Maybe Lude.DateTime,
    initiator :: Lude.Maybe Initiator,
    owner :: Lude.Maybe Owner,
    key :: Lude.Maybe ObjectKey,
    storageClass :: Lude.Maybe StorageClass,
    uploadId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultipartUpload' with the minimum fields required to make a request.
--
-- * 'initiated' - Date and time at which the multipart upload was initiated.
-- * 'initiator' - Identifies who initiated the multipart upload.
-- * 'key' - Key of the object for which the multipart upload was initiated.
-- * 'owner' - Specifies the owner of the object that is part of the multipart upload.
-- * 'storageClass' - The class of storage used to store the object.
-- * 'uploadId' - Upload ID that identifies the multipart upload.
mkMultipartUpload ::
  MultipartUpload
mkMultipartUpload =
  MultipartUpload'
    { initiated = Lude.Nothing,
      initiator = Lude.Nothing,
      owner = Lude.Nothing,
      key = Lude.Nothing,
      storageClass = Lude.Nothing,
      uploadId = Lude.Nothing
    }

-- | Date and time at which the multipart upload was initiated.
--
-- /Note:/ Consider using 'initiated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muInitiated :: Lens.Lens' MultipartUpload (Lude.Maybe Lude.DateTime)
muInitiated = Lens.lens (initiated :: MultipartUpload -> Lude.Maybe Lude.DateTime) (\s a -> s {initiated = a} :: MultipartUpload)
{-# DEPRECATED muInitiated "Use generic-lens or generic-optics with 'initiated' instead." #-}

-- | Identifies who initiated the multipart upload.
--
-- /Note:/ Consider using 'initiator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muInitiator :: Lens.Lens' MultipartUpload (Lude.Maybe Initiator)
muInitiator = Lens.lens (initiator :: MultipartUpload -> Lude.Maybe Initiator) (\s a -> s {initiator = a} :: MultipartUpload)
{-# DEPRECATED muInitiator "Use generic-lens or generic-optics with 'initiator' instead." #-}

-- | Specifies the owner of the object that is part of the multipart upload.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muOwner :: Lens.Lens' MultipartUpload (Lude.Maybe Owner)
muOwner = Lens.lens (owner :: MultipartUpload -> Lude.Maybe Owner) (\s a -> s {owner = a} :: MultipartUpload)
{-# DEPRECATED muOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | Key of the object for which the multipart upload was initiated.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muKey :: Lens.Lens' MultipartUpload (Lude.Maybe ObjectKey)
muKey = Lens.lens (key :: MultipartUpload -> Lude.Maybe ObjectKey) (\s a -> s {key = a} :: MultipartUpload)
{-# DEPRECATED muKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The class of storage used to store the object.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muStorageClass :: Lens.Lens' MultipartUpload (Lude.Maybe StorageClass)
muStorageClass = Lens.lens (storageClass :: MultipartUpload -> Lude.Maybe StorageClass) (\s a -> s {storageClass = a} :: MultipartUpload)
{-# DEPRECATED muStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | Upload ID that identifies the multipart upload.
--
-- /Note:/ Consider using 'uploadId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
muUploadId :: Lens.Lens' MultipartUpload (Lude.Maybe Lude.Text)
muUploadId = Lens.lens (uploadId :: MultipartUpload -> Lude.Maybe Lude.Text) (\s a -> s {uploadId = a} :: MultipartUpload)
{-# DEPRECATED muUploadId "Use generic-lens or generic-optics with 'uploadId' instead." #-}

instance Lude.FromXML MultipartUpload where
  parseXML x =
    MultipartUpload'
      Lude.<$> (x Lude..@? "Initiated")
      Lude.<*> (x Lude..@? "Initiator")
      Lude.<*> (x Lude..@? "Owner")
      Lude.<*> (x Lude..@? "Key")
      Lude.<*> (x Lude..@? "StorageClass")
      Lude.<*> (x Lude..@? "UploadId")
