{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.PartListElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.PartListElement
  ( PartListElement (..)
  -- * Smart constructor
  , mkPartListElement
  -- * Lenses
  , pleRangeInBytes
  , pleSHA256TreeHash
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of the part sizes of the multipart upload.
--
-- /See:/ 'mkPartListElement' smart constructor.
data PartListElement = PartListElement'
  { rangeInBytes :: Core.Maybe Core.Text
    -- ^ The byte range of a part, inclusive of the upper value of the range.
  , sHA256TreeHash :: Core.Maybe Core.Text
    -- ^ The SHA256 tree hash value that Amazon S3 Glacier calculated for the part. This field is never @null@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PartListElement' value with any optional fields omitted.
mkPartListElement
    :: PartListElement
mkPartListElement
  = PartListElement'{rangeInBytes = Core.Nothing,
                     sHA256TreeHash = Core.Nothing}

-- | The byte range of a part, inclusive of the upper value of the range.
--
-- /Note:/ Consider using 'rangeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleRangeInBytes :: Lens.Lens' PartListElement (Core.Maybe Core.Text)
pleRangeInBytes = Lens.field @"rangeInBytes"
{-# INLINEABLE pleRangeInBytes #-}
{-# DEPRECATED rangeInBytes "Use generic-lens or generic-optics with 'rangeInBytes' instead"  #-}

-- | The SHA256 tree hash value that Amazon S3 Glacier calculated for the part. This field is never @null@ .
--
-- /Note:/ Consider using 'sHA256TreeHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleSHA256TreeHash :: Lens.Lens' PartListElement (Core.Maybe Core.Text)
pleSHA256TreeHash = Lens.field @"sHA256TreeHash"
{-# INLINEABLE pleSHA256TreeHash #-}
{-# DEPRECATED sHA256TreeHash "Use generic-lens or generic-optics with 'sHA256TreeHash' instead"  #-}

instance Core.FromJSON PartListElement where
        parseJSON
          = Core.withObject "PartListElement" Core.$
              \ x ->
                PartListElement' Core.<$>
                  (x Core..:? "RangeInBytes") Core.<*> x Core..:? "SHA256TreeHash"
