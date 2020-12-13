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
import qualified Network.AWS.Prelude as Lude

-- | Limits settings on protection groups with arbitrary pattern type.
--
-- /See:/ 'mkProtectionGroupArbitraryPatternLimits' smart constructor.
newtype ProtectionGroupArbitraryPatternLimits = ProtectionGroupArbitraryPatternLimits'
  { -- | The maximum number of resources you can specify for a single arbitrary pattern in a protection group.
    maxMembers :: Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProtectionGroupArbitraryPatternLimits' with the minimum fields required to make a request.
--
-- * 'maxMembers' - The maximum number of resources you can specify for a single arbitrary pattern in a protection group.
mkProtectionGroupArbitraryPatternLimits ::
  -- | 'maxMembers'
  Lude.Integer ->
  ProtectionGroupArbitraryPatternLimits
mkProtectionGroupArbitraryPatternLimits pMaxMembers_ =
  ProtectionGroupArbitraryPatternLimits' {maxMembers = pMaxMembers_}

-- | The maximum number of resources you can specify for a single arbitrary pattern in a protection group.
--
-- /Note:/ Consider using 'maxMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgaplMaxMembers :: Lens.Lens' ProtectionGroupArbitraryPatternLimits Lude.Integer
pgaplMaxMembers = Lens.lens (maxMembers :: ProtectionGroupArbitraryPatternLimits -> Lude.Integer) (\s a -> s {maxMembers = a} :: ProtectionGroupArbitraryPatternLimits)
{-# DEPRECATED pgaplMaxMembers "Use generic-lens or generic-optics with 'maxMembers' instead." #-}

instance Lude.FromJSON ProtectionGroupArbitraryPatternLimits where
  parseJSON =
    Lude.withObject
      "ProtectionGroupArbitraryPatternLimits"
      ( \x ->
          ProtectionGroupArbitraryPatternLimits'
            Lude.<$> (x Lude..: "MaxMembers")
      )
