{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionGroupArbitraryPatternLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroupArbitraryPatternLimits
  ( ProtectionGroupArbitraryPatternLimits (..),

    -- * Smart constructor
    mkProtectionGroupArbitraryPatternLimits,

    -- * Lenses
    pgaplMaxMembers,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Limits settings on protection groups with arbitrary pattern type.
--
-- /See:/ 'mkProtectionGroupArbitraryPatternLimits' smart constructor.
newtype ProtectionGroupArbitraryPatternLimits = ProtectionGroupArbitraryPatternLimits'
  { -- | The maximum number of resources you can specify for a single arbitrary pattern in a protection group.
    maxMembers :: Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ProtectionGroupArbitraryPatternLimits' value with any optional fields omitted.
mkProtectionGroupArbitraryPatternLimits ::
  -- | 'maxMembers'
  Core.Integer ->
  ProtectionGroupArbitraryPatternLimits
mkProtectionGroupArbitraryPatternLimits maxMembers =
  ProtectionGroupArbitraryPatternLimits' {maxMembers}

-- | The maximum number of resources you can specify for a single arbitrary pattern in a protection group.
--
-- /Note:/ Consider using 'maxMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgaplMaxMembers :: Lens.Lens' ProtectionGroupArbitraryPatternLimits Core.Integer
pgaplMaxMembers = Lens.field @"maxMembers"
{-# DEPRECATED pgaplMaxMembers "Use generic-lens or generic-optics with 'maxMembers' instead." #-}

instance Core.FromJSON ProtectionGroupArbitraryPatternLimits where
  parseJSON =
    Core.withObject "ProtectionGroupArbitraryPatternLimits" Core.$
      \x ->
        ProtectionGroupArbitraryPatternLimits'
          Core.<$> (x Core..: "MaxMembers")
