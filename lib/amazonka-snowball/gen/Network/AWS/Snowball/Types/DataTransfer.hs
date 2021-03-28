{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.DataTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.DataTransfer
  ( DataTransfer (..)
  -- * Smart constructor
  , mkDataTransfer
  -- * Lenses
  , dtBytesTransferred
  , dtObjectsTransferred
  , dtTotalBytes
  , dtTotalObjects
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines the real-time status of a Snow device's data transfer while the device is at AWS. This data is only available while a job has a @JobState@ value of @InProgress@ , for both import and export jobs.
--
-- /See:/ 'mkDataTransfer' smart constructor.
data DataTransfer = DataTransfer'
  { bytesTransferred :: Core.Maybe Core.Integer
    -- ^ The number of bytes transferred between a Snow device and Amazon S3.
  , objectsTransferred :: Core.Maybe Core.Integer
    -- ^ The number of objects transferred between a Snow device and Amazon S3.
  , totalBytes :: Core.Maybe Core.Integer
    -- ^ The total bytes of data for a transfer between a Snow device and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
  , totalObjects :: Core.Maybe Core.Integer
    -- ^ The total number of objects for a transfer between a Snow device and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DataTransfer' value with any optional fields omitted.
mkDataTransfer
    :: DataTransfer
mkDataTransfer
  = DataTransfer'{bytesTransferred = Core.Nothing,
                  objectsTransferred = Core.Nothing, totalBytes = Core.Nothing,
                  totalObjects = Core.Nothing}

-- | The number of bytes transferred between a Snow device and Amazon S3.
--
-- /Note:/ Consider using 'bytesTransferred' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtBytesTransferred :: Lens.Lens' DataTransfer (Core.Maybe Core.Integer)
dtBytesTransferred = Lens.field @"bytesTransferred"
{-# INLINEABLE dtBytesTransferred #-}
{-# DEPRECATED bytesTransferred "Use generic-lens or generic-optics with 'bytesTransferred' instead"  #-}

-- | The number of objects transferred between a Snow device and Amazon S3.
--
-- /Note:/ Consider using 'objectsTransferred' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtObjectsTransferred :: Lens.Lens' DataTransfer (Core.Maybe Core.Integer)
dtObjectsTransferred = Lens.field @"objectsTransferred"
{-# INLINEABLE dtObjectsTransferred #-}
{-# DEPRECATED objectsTransferred "Use generic-lens or generic-optics with 'objectsTransferred' instead"  #-}

-- | The total bytes of data for a transfer between a Snow device and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
--
-- /Note:/ Consider using 'totalBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTotalBytes :: Lens.Lens' DataTransfer (Core.Maybe Core.Integer)
dtTotalBytes = Lens.field @"totalBytes"
{-# INLINEABLE dtTotalBytes #-}
{-# DEPRECATED totalBytes "Use generic-lens or generic-optics with 'totalBytes' instead"  #-}

-- | The total number of objects for a transfer between a Snow device and Amazon S3. This value is set to 0 (zero) until all the keys that will be transferred have been listed.
--
-- /Note:/ Consider using 'totalObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTotalObjects :: Lens.Lens' DataTransfer (Core.Maybe Core.Integer)
dtTotalObjects = Lens.field @"totalObjects"
{-# INLINEABLE dtTotalObjects #-}
{-# DEPRECATED totalObjects "Use generic-lens or generic-optics with 'totalObjects' instead"  #-}

instance Core.FromJSON DataTransfer where
        parseJSON
          = Core.withObject "DataTransfer" Core.$
              \ x ->
                DataTransfer' Core.<$>
                  (x Core..:? "BytesTransferred") Core.<*>
                    x Core..:? "ObjectsTransferred"
                    Core.<*> x Core..:? "TotalBytes"
                    Core.<*> x Core..:? "TotalObjects"
