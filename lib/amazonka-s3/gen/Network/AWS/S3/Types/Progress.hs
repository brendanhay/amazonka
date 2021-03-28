{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Progress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Progress
  ( Progress (..)
  -- * Smart constructor
  , mkProgress
  -- * Lenses
  , pBytesProcessed
  , pBytesReturned
  , pBytesScanned
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | This data type contains information about progress of an operation.
--
-- /See:/ 'mkProgress' smart constructor.
data Progress = Progress'
  { bytesProcessed :: Core.Maybe Core.Integer
    -- ^ The current number of uncompressed object bytes processed.
  , bytesReturned :: Core.Maybe Core.Integer
    -- ^ The current number of bytes of records payload data returned.
  , bytesScanned :: Core.Maybe Core.Integer
    -- ^ The current number of object bytes scanned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Progress' value with any optional fields omitted.
mkProgress
    :: Progress
mkProgress
  = Progress'{bytesProcessed = Core.Nothing,
              bytesReturned = Core.Nothing, bytesScanned = Core.Nothing}

-- | The current number of uncompressed object bytes processed.
--
-- /Note:/ Consider using 'bytesProcessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pBytesProcessed :: Lens.Lens' Progress (Core.Maybe Core.Integer)
pBytesProcessed = Lens.field @"bytesProcessed"
{-# INLINEABLE pBytesProcessed #-}
{-# DEPRECATED bytesProcessed "Use generic-lens or generic-optics with 'bytesProcessed' instead"  #-}

-- | The current number of bytes of records payload data returned.
--
-- /Note:/ Consider using 'bytesReturned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pBytesReturned :: Lens.Lens' Progress (Core.Maybe Core.Integer)
pBytesReturned = Lens.field @"bytesReturned"
{-# INLINEABLE pBytesReturned #-}
{-# DEPRECATED bytesReturned "Use generic-lens or generic-optics with 'bytesReturned' instead"  #-}

-- | The current number of object bytes scanned.
--
-- /Note:/ Consider using 'bytesScanned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pBytesScanned :: Lens.Lens' Progress (Core.Maybe Core.Integer)
pBytesScanned = Lens.field @"bytesScanned"
{-# INLINEABLE pBytesScanned #-}
{-# DEPRECATED bytesScanned "Use generic-lens or generic-optics with 'bytesScanned' instead"  #-}

instance Core.FromXML Progress where
        parseXML x
          = Progress' Core.<$>
              (x Core..@? "BytesProcessed") Core.<*> x Core..@? "BytesReturned"
                Core.<*> x Core..@? "BytesScanned"
