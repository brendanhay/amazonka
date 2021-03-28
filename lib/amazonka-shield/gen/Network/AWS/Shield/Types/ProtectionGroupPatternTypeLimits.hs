{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionGroupPatternTypeLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.ProtectionGroupPatternTypeLimits
  ( ProtectionGroupPatternTypeLimits (..)
  -- * Smart constructor
  , mkProtectionGroupPatternTypeLimits
  -- * Lenses
  , pgptlArbitraryPatternLimits
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.ProtectionGroupArbitraryPatternLimits as Types

-- | Limits settings by pattern type in the protection groups for your subscription. 
--
-- /See:/ 'mkProtectionGroupPatternTypeLimits' smart constructor.
newtype ProtectionGroupPatternTypeLimits = ProtectionGroupPatternTypeLimits'
  { arbitraryPatternLimits :: Types.ProtectionGroupArbitraryPatternLimits
    -- ^ Limits settings on protection groups with arbitrary pattern type. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ProtectionGroupPatternTypeLimits' value with any optional fields omitted.
mkProtectionGroupPatternTypeLimits
    :: Types.ProtectionGroupArbitraryPatternLimits -- ^ 'arbitraryPatternLimits'
    -> ProtectionGroupPatternTypeLimits
mkProtectionGroupPatternTypeLimits arbitraryPatternLimits
  = ProtectionGroupPatternTypeLimits'{arbitraryPatternLimits}

-- | Limits settings on protection groups with arbitrary pattern type. 
--
-- /Note:/ Consider using 'arbitraryPatternLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgptlArbitraryPatternLimits :: Lens.Lens' ProtectionGroupPatternTypeLimits Types.ProtectionGroupArbitraryPatternLimits
pgptlArbitraryPatternLimits = Lens.field @"arbitraryPatternLimits"
{-# INLINEABLE pgptlArbitraryPatternLimits #-}
{-# DEPRECATED arbitraryPatternLimits "Use generic-lens or generic-optics with 'arbitraryPatternLimits' instead"  #-}

instance Core.FromJSON ProtectionGroupPatternTypeLimits where
        parseJSON
          = Core.withObject "ProtectionGroupPatternTypeLimits" Core.$
              \ x ->
                ProtectionGroupPatternTypeLimits' Core.<$>
                  (x Core..: "ArbitraryPatternLimits")
