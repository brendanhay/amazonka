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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Shield.Types.ProtectionGroupPatternTypeLimits

-- | Limits settings on protection groups for your subscription.
--
-- /See:/ 'mkProtectionGroupLimits' smart constructor.
data ProtectionGroupLimits = ProtectionGroupLimits'
  { -- | The maximum number of protection groups that you can have at one time.
    maxProtectionGroups :: Lude.Integer,
    -- | Limits settings by pattern type in the protection groups for your subscription.
    patternTypeLimits :: ProtectionGroupPatternTypeLimits
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProtectionGroupLimits' with the minimum fields required to make a request.
--
-- * 'maxProtectionGroups' - The maximum number of protection groups that you can have at one time.
-- * 'patternTypeLimits' - Limits settings by pattern type in the protection groups for your subscription.
mkProtectionGroupLimits ::
  -- | 'maxProtectionGroups'
  Lude.Integer ->
  -- | 'patternTypeLimits'
  ProtectionGroupPatternTypeLimits ->
  ProtectionGroupLimits
mkProtectionGroupLimits pMaxProtectionGroups_ pPatternTypeLimits_ =
  ProtectionGroupLimits'
    { maxProtectionGroups =
        pMaxProtectionGroups_,
      patternTypeLimits = pPatternTypeLimits_
    }

-- | The maximum number of protection groups that you can have at one time.
--
-- /Note:/ Consider using 'maxProtectionGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pglMaxProtectionGroups :: Lens.Lens' ProtectionGroupLimits Lude.Integer
pglMaxProtectionGroups = Lens.lens (maxProtectionGroups :: ProtectionGroupLimits -> Lude.Integer) (\s a -> s {maxProtectionGroups = a} :: ProtectionGroupLimits)
{-# DEPRECATED pglMaxProtectionGroups "Use generic-lens or generic-optics with 'maxProtectionGroups' instead." #-}

-- | Limits settings by pattern type in the protection groups for your subscription.
--
-- /Note:/ Consider using 'patternTypeLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pglPatternTypeLimits :: Lens.Lens' ProtectionGroupLimits ProtectionGroupPatternTypeLimits
pglPatternTypeLimits = Lens.lens (patternTypeLimits :: ProtectionGroupLimits -> ProtectionGroupPatternTypeLimits) (\s a -> s {patternTypeLimits = a} :: ProtectionGroupLimits)
{-# DEPRECATED pglPatternTypeLimits "Use generic-lens or generic-optics with 'patternTypeLimits' instead." #-}

instance Lude.FromJSON ProtectionGroupLimits where
  parseJSON =
    Lude.withObject
      "ProtectionGroupLimits"
      ( \x ->
          ProtectionGroupLimits'
            Lude.<$> (x Lude..: "MaxProtectionGroups")
            Lude.<*> (x Lude..: "PatternTypeLimits")
      )
