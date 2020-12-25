{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionGroupLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroupLimits
  ( ProtectionGroupLimits (..),

    -- * Smart constructor
    mkProtectionGroupLimits,

    -- * Lenses
    pglMaxProtectionGroups,
    pglPatternTypeLimits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.ProtectionGroupPatternTypeLimits as Types

-- | Limits settings on protection groups for your subscription.
--
-- /See:/ 'mkProtectionGroupLimits' smart constructor.
data ProtectionGroupLimits = ProtectionGroupLimits'
  { -- | The maximum number of protection groups that you can have at one time.
    maxProtectionGroups :: Core.Integer,
    -- | Limits settings by pattern type in the protection groups for your subscription.
    patternTypeLimits :: Types.ProtectionGroupPatternTypeLimits
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProtectionGroupLimits' value with any optional fields omitted.
mkProtectionGroupLimits ::
  -- | 'maxProtectionGroups'
  Core.Integer ->
  -- | 'patternTypeLimits'
  Types.ProtectionGroupPatternTypeLimits ->
  ProtectionGroupLimits
mkProtectionGroupLimits maxProtectionGroups patternTypeLimits =
  ProtectionGroupLimits' {maxProtectionGroups, patternTypeLimits}

-- | The maximum number of protection groups that you can have at one time.
--
-- /Note:/ Consider using 'maxProtectionGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pglMaxProtectionGroups :: Lens.Lens' ProtectionGroupLimits Core.Integer
pglMaxProtectionGroups = Lens.field @"maxProtectionGroups"
{-# DEPRECATED pglMaxProtectionGroups "Use generic-lens or generic-optics with 'maxProtectionGroups' instead." #-}

-- | Limits settings by pattern type in the protection groups for your subscription.
--
-- /Note:/ Consider using 'patternTypeLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pglPatternTypeLimits :: Lens.Lens' ProtectionGroupLimits Types.ProtectionGroupPatternTypeLimits
pglPatternTypeLimits = Lens.field @"patternTypeLimits"
{-# DEPRECATED pglPatternTypeLimits "Use generic-lens or generic-optics with 'patternTypeLimits' instead." #-}

instance Core.FromJSON ProtectionGroupLimits where
  parseJSON =
    Core.withObject "ProtectionGroupLimits" Core.$
      \x ->
        ProtectionGroupLimits'
          Core.<$> (x Core..: "MaxProtectionGroups")
          Core.<*> (x Core..: "PatternTypeLimits")
