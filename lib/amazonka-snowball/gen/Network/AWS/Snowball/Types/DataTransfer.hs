{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.DataTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.DataTransfer
  ( DataTransfer (..),

    -- * Smart constructor
    mkDataTransfer,

    -- * Lenses
    dtTotalObjects,
    dtTotalBytes,
    dtObjectsTransferred,
    dtBytesTransferred,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines the real-time status of a Snow device's data transfer while the device is at AWS. This data is only available while a job has a @JobState@ value of @InProgress@ , for both import and export jobs.
--
-- /See:/ 'mkDataTransfer' smart constructor.
data DataTransfer = DataTransfer'
  { totalObjects ::
      Lude.Maybe Lude.Integer,
    totalBytes :: Lude.Maybe Lude.Integer,
    objectsTransferred :: Lude.Maybe Lude.Integer,
    bytesTransferred :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataTransfer' with the minimum fields required to make a request.
--
-- * 'bytesTransferred' - The number of bytes transferred between a Snow device and Amazon S3.
-- * 'objectsTransferred' - The number of objects transferred between a Snow device and Amazon S3.
-- * 'totalBytes' - The total bytes of data for a transfer between a Snow device and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
-- * 'totalObjects' - The total number of objects for a transfer between a Snow device and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
mkDataTransfer ::
  DataTransfer
mkDataTransfer =
  DataTransfer'
    { totalObjects = Lude.Nothing,
      totalBytes = Lude.Nothing,
      objectsTransferred = Lude.Nothing,
      bytesTransferred = Lude.Nothing
    }

-- | The total number of objects for a transfer between a Snow device and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
--
-- /Note:/ Consider using 'totalObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTotalObjects :: Lens.Lens' DataTransfer (Lude.Maybe Lude.Integer)
dtTotalObjects = Lens.lens (totalObjects :: DataTransfer -> Lude.Maybe Lude.Integer) (\s a -> s {totalObjects = a} :: DataTransfer)
{-# DEPRECATED dtTotalObjects "Use generic-lens or generic-optics with 'totalObjects' instead." #-}

-- | The total bytes of data for a transfer between a Snow device and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
--
-- /Note:/ Consider using 'totalBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTotalBytes :: Lens.Lens' DataTransfer (Lude.Maybe Lude.Integer)
dtTotalBytes = Lens.lens (totalBytes :: DataTransfer -> Lude.Maybe Lude.Integer) (\s a -> s {totalBytes = a} :: DataTransfer)
{-# DEPRECATED dtTotalBytes "Use generic-lens or generic-optics with 'totalBytes' instead." #-}

-- | The number of objects transferred between a Snow device and Amazon S3.
--
-- /Note:/ Consider using 'objectsTransferred' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtObjectsTransferred :: Lens.Lens' DataTransfer (Lude.Maybe Lude.Integer)
dtObjectsTransferred = Lens.lens (objectsTransferred :: DataTransfer -> Lude.Maybe Lude.Integer) (\s a -> s {objectsTransferred = a} :: DataTransfer)
{-# DEPRECATED dtObjectsTransferred "Use generic-lens or generic-optics with 'objectsTransferred' instead." #-}

-- | The number of bytes transferred between a Snow device and Amazon S3.
--
-- /Note:/ Consider using 'bytesTransferred' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtBytesTransferred :: Lens.Lens' DataTransfer (Lude.Maybe Lude.Integer)
dtBytesTransferred = Lens.lens (bytesTransferred :: DataTransfer -> Lude.Maybe Lude.Integer) (\s a -> s {bytesTransferred = a} :: DataTransfer)
{-# DEPRECATED dtBytesTransferred "Use generic-lens or generic-optics with 'bytesTransferred' instead." #-}

instance Lude.FromJSON DataTransfer where
  parseJSON =
    Lude.withObject
      "DataTransfer"
      ( \x ->
          DataTransfer'
            Lude.<$> (x Lude..:? "TotalObjects")
            Lude.<*> (x Lude..:? "TotalBytes")
            Lude.<*> (x Lude..:? "ObjectsTransferred")
            Lude.<*> (x Lude..:? "BytesTransferred")
      )
