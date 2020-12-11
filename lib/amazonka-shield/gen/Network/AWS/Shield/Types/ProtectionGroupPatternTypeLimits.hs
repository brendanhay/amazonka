-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionGroupPatternTypeLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroupPatternTypeLimits
  ( ProtectionGroupPatternTypeLimits (..),

    -- * Smart constructor
    mkProtectionGroupPatternTypeLimits,

    -- * Lenses
    pgptlArbitraryPatternLimits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Shield.Types.ProtectionGroupArbitraryPatternLimits

-- | Limits settings by pattern type in the protection groups for your subscription.
--
-- /See:/ 'mkProtectionGroupPatternTypeLimits' smart constructor.
newtype ProtectionGroupPatternTypeLimits = ProtectionGroupPatternTypeLimits'
  { arbitraryPatternLimits ::
      ProtectionGroupArbitraryPatternLimits
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProtectionGroupPatternTypeLimits' with the minimum fields required to make a request.
--
-- * 'arbitraryPatternLimits' - Limits settings on protection groups with arbitrary pattern type.
mkProtectionGroupPatternTypeLimits ::
  -- | 'arbitraryPatternLimits'
  ProtectionGroupArbitraryPatternLimits ->
  ProtectionGroupPatternTypeLimits
mkProtectionGroupPatternTypeLimits pArbitraryPatternLimits_ =
  ProtectionGroupPatternTypeLimits'
    { arbitraryPatternLimits =
        pArbitraryPatternLimits_
    }

-- | Limits settings on protection groups with arbitrary pattern type.
--
-- /Note:/ Consider using 'arbitraryPatternLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgptlArbitraryPatternLimits :: Lens.Lens' ProtectionGroupPatternTypeLimits ProtectionGroupArbitraryPatternLimits
pgptlArbitraryPatternLimits = Lens.lens (arbitraryPatternLimits :: ProtectionGroupPatternTypeLimits -> ProtectionGroupArbitraryPatternLimits) (\s a -> s {arbitraryPatternLimits = a} :: ProtectionGroupPatternTypeLimits)
{-# DEPRECATED pgptlArbitraryPatternLimits "Use generic-lens or generic-optics with 'arbitraryPatternLimits' instead." #-}

instance Lude.FromJSON ProtectionGroupPatternTypeLimits where
  parseJSON =
    Lude.withObject
      "ProtectionGroupPatternTypeLimits"
      ( \x ->
          ProtectionGroupPatternTypeLimits'
            Lude.<$> (x Lude..: "ArbitraryPatternLimits")
      )
