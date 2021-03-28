{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.KeyRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.KeyRange
  ( KeyRange (..)
  -- * Smart constructor
  , mkKeyRange
  -- * Lenses
  , krBeginMarker
  , krEndMarker
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains a key range. For export jobs, a @S3Resource@ object can have an optional @KeyRange@ value. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
--
-- /See:/ 'mkKeyRange' smart constructor.
data KeyRange = KeyRange'
  { beginMarker :: Core.Maybe Core.Text
    -- ^ The key that starts an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
  , endMarker :: Core.Maybe Core.Text
    -- ^ The key that ends an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KeyRange' value with any optional fields omitted.
mkKeyRange
    :: KeyRange
mkKeyRange
  = KeyRange'{beginMarker = Core.Nothing, endMarker = Core.Nothing}

-- | The key that starts an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
--
-- /Note:/ Consider using 'beginMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
krBeginMarker :: Lens.Lens' KeyRange (Core.Maybe Core.Text)
krBeginMarker = Lens.field @"beginMarker"
{-# INLINEABLE krBeginMarker #-}
{-# DEPRECATED beginMarker "Use generic-lens or generic-optics with 'beginMarker' instead"  #-}

-- | The key that ends an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
--
-- /Note:/ Consider using 'endMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
krEndMarker :: Lens.Lens' KeyRange (Core.Maybe Core.Text)
krEndMarker = Lens.field @"endMarker"
{-# INLINEABLE krEndMarker #-}
{-# DEPRECATED endMarker "Use generic-lens or generic-optics with 'endMarker' instead"  #-}

instance Core.FromJSON KeyRange where
        toJSON KeyRange{..}
          = Core.object
              (Core.catMaybes
                 [("BeginMarker" Core..=) Core.<$> beginMarker,
                  ("EndMarker" Core..=) Core.<$> endMarker])

instance Core.FromJSON KeyRange where
        parseJSON
          = Core.withObject "KeyRange" Core.$
              \ x ->
                KeyRange' Core.<$>
                  (x Core..:? "BeginMarker") Core.<*> x Core..:? "EndMarker"
