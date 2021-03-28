{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AssetPropertyTimestamp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AssetPropertyTimestamp
  ( AssetPropertyTimestamp (..)
  -- * Smart constructor
  , mkAssetPropertyTimestamp
  -- * Lenses
  , aptTimeInSeconds
  , aptOffsetInNanos
  ) where

import qualified Network.AWS.IoT.Types.AssetPropertyOffsetInNanos as Types
import qualified Network.AWS.IoT.Types.AssetPropertyTimeInSeconds as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An asset property timestamp entry containing the following information.
--
-- /See:/ 'mkAssetPropertyTimestamp' smart constructor.
data AssetPropertyTimestamp = AssetPropertyTimestamp'
  { timeInSeconds :: Types.AssetPropertyTimeInSeconds
    -- ^ A string that contains the time in seconds since epoch. Accepts substitution templates.
  , offsetInNanos :: Core.Maybe Types.AssetPropertyOffsetInNanos
    -- ^ Optional. A string that contains the nanosecond time offset. Accepts substitution templates.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssetPropertyTimestamp' value with any optional fields omitted.
mkAssetPropertyTimestamp
    :: Types.AssetPropertyTimeInSeconds -- ^ 'timeInSeconds'
    -> AssetPropertyTimestamp
mkAssetPropertyTimestamp timeInSeconds
  = AssetPropertyTimestamp'{timeInSeconds,
                            offsetInNanos = Core.Nothing}

-- | A string that contains the time in seconds since epoch. Accepts substitution templates.
--
-- /Note:/ Consider using 'timeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aptTimeInSeconds :: Lens.Lens' AssetPropertyTimestamp Types.AssetPropertyTimeInSeconds
aptTimeInSeconds = Lens.field @"timeInSeconds"
{-# INLINEABLE aptTimeInSeconds #-}
{-# DEPRECATED timeInSeconds "Use generic-lens or generic-optics with 'timeInSeconds' instead"  #-}

-- | Optional. A string that contains the nanosecond time offset. Accepts substitution templates.
--
-- /Note:/ Consider using 'offsetInNanos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aptOffsetInNanos :: Lens.Lens' AssetPropertyTimestamp (Core.Maybe Types.AssetPropertyOffsetInNanos)
aptOffsetInNanos = Lens.field @"offsetInNanos"
{-# INLINEABLE aptOffsetInNanos #-}
{-# DEPRECATED offsetInNanos "Use generic-lens or generic-optics with 'offsetInNanos' instead"  #-}

instance Core.FromJSON AssetPropertyTimestamp where
        toJSON AssetPropertyTimestamp{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("timeInSeconds" Core..= timeInSeconds),
                  ("offsetInNanos" Core..=) Core.<$> offsetInNanos])

instance Core.FromJSON AssetPropertyTimestamp where
        parseJSON
          = Core.withObject "AssetPropertyTimestamp" Core.$
              \ x ->
                AssetPropertyTimestamp' Core.<$>
                  (x Core..: "timeInSeconds") Core.<*> x Core..:? "offsetInNanos"
