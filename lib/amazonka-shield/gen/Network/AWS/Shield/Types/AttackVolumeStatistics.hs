{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackVolumeStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackVolumeStatistics
  ( AttackVolumeStatistics (..),

    -- * Smart constructor
    mkAttackVolumeStatistics,

    -- * Lenses
    avsMax,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Statistics objects for the various data types in 'AttackVolume' .
--
-- /See:/ 'mkAttackVolumeStatistics' smart constructor.
newtype AttackVolumeStatistics = AttackVolumeStatistics'
  { -- | The maximum attack volume observed for the given unit.
    max :: Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttackVolumeStatistics' with the minimum fields required to make a request.
--
-- * 'max' - The maximum attack volume observed for the given unit.
mkAttackVolumeStatistics ::
  -- | 'max'
  Lude.Double ->
  AttackVolumeStatistics
mkAttackVolumeStatistics pMax_ =
  AttackVolumeStatistics' {max = pMax_}

-- | The maximum attack volume observed for the given unit.
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avsMax :: Lens.Lens' AttackVolumeStatistics Lude.Double
avsMax = Lens.lens (max :: AttackVolumeStatistics -> Lude.Double) (\s a -> s {max = a} :: AttackVolumeStatistics)
{-# DEPRECATED avsMax "Use generic-lens or generic-optics with 'max' instead." #-}

instance Lude.FromJSON AttackVolumeStatistics where
  parseJSON =
    Lude.withObject
      "AttackVolumeStatistics"
      (\x -> AttackVolumeStatistics' Lude.<$> (x Lude..: "Max"))
