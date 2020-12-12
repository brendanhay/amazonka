{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackStatisticsDataItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackStatisticsDataItem
  ( AttackStatisticsDataItem (..),

    -- * Smart constructor
    mkAttackStatisticsDataItem,

    -- * Lenses
    asdiAttackVolume,
    asdiAttackCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Shield.Types.AttackVolume

-- | A single attack statistics data record. This is returned by 'DescribeAttackStatistics' along with a time range indicating the time period that the attack statistics apply to.
--
-- /See:/ 'mkAttackStatisticsDataItem' smart constructor.
data AttackStatisticsDataItem = AttackStatisticsDataItem'
  { attackVolume ::
      Lude.Maybe AttackVolume,
    attackCount :: Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttackStatisticsDataItem' with the minimum fields required to make a request.
--
-- * 'attackCount' - The number of attacks detected during the time period. This is always present, but might be zero.
-- * 'attackVolume' - Information about the volume of attacks during the time period. If the accompanying @AttackCount@ is zero, this setting might be empty.
mkAttackStatisticsDataItem ::
  -- | 'attackCount'
  Lude.Integer ->
  AttackStatisticsDataItem
mkAttackStatisticsDataItem pAttackCount_ =
  AttackStatisticsDataItem'
    { attackVolume = Lude.Nothing,
      attackCount = pAttackCount_
    }

-- | Information about the volume of attacks during the time period. If the accompanying @AttackCount@ is zero, this setting might be empty.
--
-- /Note:/ Consider using 'attackVolume' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdiAttackVolume :: Lens.Lens' AttackStatisticsDataItem (Lude.Maybe AttackVolume)
asdiAttackVolume = Lens.lens (attackVolume :: AttackStatisticsDataItem -> Lude.Maybe AttackVolume) (\s a -> s {attackVolume = a} :: AttackStatisticsDataItem)
{-# DEPRECATED asdiAttackVolume "Use generic-lens or generic-optics with 'attackVolume' instead." #-}

-- | The number of attacks detected during the time period. This is always present, but might be zero.
--
-- /Note:/ Consider using 'attackCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdiAttackCount :: Lens.Lens' AttackStatisticsDataItem Lude.Integer
asdiAttackCount = Lens.lens (attackCount :: AttackStatisticsDataItem -> Lude.Integer) (\s a -> s {attackCount = a} :: AttackStatisticsDataItem)
{-# DEPRECATED asdiAttackCount "Use generic-lens or generic-optics with 'attackCount' instead." #-}

instance Lude.FromJSON AttackStatisticsDataItem where
  parseJSON =
    Lude.withObject
      "AttackStatisticsDataItem"
      ( \x ->
          AttackStatisticsDataItem'
            Lude.<$> (x Lude..:? "AttackVolume") Lude.<*> (x Lude..: "AttackCount")
      )
