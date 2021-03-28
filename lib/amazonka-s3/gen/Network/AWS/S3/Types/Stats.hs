{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Stats
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Stats
  ( Stats (..)
  -- * Smart constructor
  , mkStats
  -- * Lenses
  , sBytesProcessed
  , sBytesReturned
  , sBytesScanned
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | Container for the stats details.
--
-- /See:/ 'mkStats' smart constructor.
data Stats = Stats'
  { bytesProcessed :: Core.Maybe Core.Integer
    -- ^ The total number of uncompressed object bytes processed.
  , bytesReturned :: Core.Maybe Core.Integer
    -- ^ The total number of bytes of records payload data returned.
  , bytesScanned :: Core.Maybe Core.Integer
    -- ^ The total number of object bytes scanned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Stats' value with any optional fields omitted.
mkStats
    :: Stats
mkStats
  = Stats'{bytesProcessed = Core.Nothing,
           bytesReturned = Core.Nothing, bytesScanned = Core.Nothing}

-- | The total number of uncompressed object bytes processed.
--
-- /Note:/ Consider using 'bytesProcessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sBytesProcessed :: Lens.Lens' Stats (Core.Maybe Core.Integer)
sBytesProcessed = Lens.field @"bytesProcessed"
{-# INLINEABLE sBytesProcessed #-}
{-# DEPRECATED bytesProcessed "Use generic-lens or generic-optics with 'bytesProcessed' instead"  #-}

-- | The total number of bytes of records payload data returned.
--
-- /Note:/ Consider using 'bytesReturned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sBytesReturned :: Lens.Lens' Stats (Core.Maybe Core.Integer)
sBytesReturned = Lens.field @"bytesReturned"
{-# INLINEABLE sBytesReturned #-}
{-# DEPRECATED bytesReturned "Use generic-lens or generic-optics with 'bytesReturned' instead"  #-}

-- | The total number of object bytes scanned.
--
-- /Note:/ Consider using 'bytesScanned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sBytesScanned :: Lens.Lens' Stats (Core.Maybe Core.Integer)
sBytesScanned = Lens.field @"bytesScanned"
{-# INLINEABLE sBytesScanned #-}
{-# DEPRECATED bytesScanned "Use generic-lens or generic-optics with 'bytesScanned' instead"  #-}

instance Core.FromXML Stats where
        parseXML x
          = Stats' Core.<$>
              (x Core..@? "BytesProcessed") Core.<*> x Core..@? "BytesReturned"
                Core.<*> x Core..@? "BytesScanned"
