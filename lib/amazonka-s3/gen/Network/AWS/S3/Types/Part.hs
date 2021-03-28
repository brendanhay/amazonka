{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Part
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Part
  ( Part (..)
  -- * Smart constructor
  , mkPart
  -- * Lenses
  , pETag
  , pLastModified
  , pPartNumber
  , pSize
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | Container for elements related to a part.
--
-- /See:/ 'mkPart' smart constructor.
data Part = Part'
  { eTag :: Core.Maybe Types.ETag
    -- ^ Entity tag returned when the part was uploaded.
  , lastModified :: Core.Maybe Core.UTCTime
    -- ^ Date and time at which the part was uploaded.
  , partNumber :: Core.Maybe Core.Int
    -- ^ Part number identifying the part. This is a positive integer between 1 and 10,000.
  , size :: Core.Maybe Core.Int
    -- ^ Size in bytes of the uploaded part data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Part' value with any optional fields omitted.
mkPart
    :: Part
mkPart
  = Part'{eTag = Core.Nothing, lastModified = Core.Nothing,
          partNumber = Core.Nothing, size = Core.Nothing}

-- | Entity tag returned when the part was uploaded.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pETag :: Lens.Lens' Part (Core.Maybe Types.ETag)
pETag = Lens.field @"eTag"
{-# INLINEABLE pETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | Date and time at which the part was uploaded.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLastModified :: Lens.Lens' Part (Core.Maybe Core.UTCTime)
pLastModified = Lens.field @"lastModified"
{-# INLINEABLE pLastModified #-}
{-# DEPRECATED lastModified "Use generic-lens or generic-optics with 'lastModified' instead"  #-}

-- | Part number identifying the part. This is a positive integer between 1 and 10,000.
--
-- /Note:/ Consider using 'partNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPartNumber :: Lens.Lens' Part (Core.Maybe Core.Int)
pPartNumber = Lens.field @"partNumber"
{-# INLINEABLE pPartNumber #-}
{-# DEPRECATED partNumber "Use generic-lens or generic-optics with 'partNumber' instead"  #-}

-- | Size in bytes of the uploaded part data.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSize :: Lens.Lens' Part (Core.Maybe Core.Int)
pSize = Lens.field @"size"
{-# INLINEABLE pSize #-}
{-# DEPRECATED size "Use generic-lens or generic-optics with 'size' instead"  #-}

instance Core.FromXML Part where
        parseXML x
          = Part' Core.<$>
              (x Core..@? "ETag") Core.<*> x Core..@? "LastModified" Core.<*>
                x Core..@? "PartNumber"
                Core.<*> x Core..@? "Size"
