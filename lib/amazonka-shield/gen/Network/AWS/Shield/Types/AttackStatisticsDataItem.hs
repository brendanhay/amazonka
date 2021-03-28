{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackStatisticsDataItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.AttackStatisticsDataItem
  ( AttackStatisticsDataItem (..)
  -- * Smart constructor
  , mkAttackStatisticsDataItem
  -- * Lenses
  , asdiAttackCount
  , asdiAttackVolume
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.AttackVolume as Types

-- | A single attack statistics data record. This is returned by 'DescribeAttackStatistics' along with a time range indicating the time period that the attack statistics apply to. 
--
-- /See:/ 'mkAttackStatisticsDataItem' smart constructor.
data AttackStatisticsDataItem = AttackStatisticsDataItem'
  { attackCount :: Core.Integer
    -- ^ The number of attacks detected during the time period. This is always present, but might be zero. 
  , attackVolume :: Core.Maybe Types.AttackVolume
    -- ^ Information about the volume of attacks during the time period. If the accompanying @AttackCount@ is zero, this setting might be empty.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttackStatisticsDataItem' value with any optional fields omitted.
mkAttackStatisticsDataItem
    :: Core.Integer -- ^ 'attackCount'
    -> AttackStatisticsDataItem
mkAttackStatisticsDataItem attackCount
  = AttackStatisticsDataItem'{attackCount,
                              attackVolume = Core.Nothing}

-- | The number of attacks detected during the time period. This is always present, but might be zero. 
--
-- /Note:/ Consider using 'attackCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdiAttackCount :: Lens.Lens' AttackStatisticsDataItem Core.Integer
asdiAttackCount = Lens.field @"attackCount"
{-# INLINEABLE asdiAttackCount #-}
{-# DEPRECATED attackCount "Use generic-lens or generic-optics with 'attackCount' instead"  #-}

-- | Information about the volume of attacks during the time period. If the accompanying @AttackCount@ is zero, this setting might be empty.
--
-- /Note:/ Consider using 'attackVolume' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdiAttackVolume :: Lens.Lens' AttackStatisticsDataItem (Core.Maybe Types.AttackVolume)
asdiAttackVolume = Lens.field @"attackVolume"
{-# INLINEABLE asdiAttackVolume #-}
{-# DEPRECATED attackVolume "Use generic-lens or generic-optics with 'attackVolume' instead"  #-}

instance Core.FromJSON AttackStatisticsDataItem where
        parseJSON
          = Core.withObject "AttackStatisticsDataItem" Core.$
              \ x ->
                AttackStatisticsDataItem' Core.<$>
                  (x Core..: "AttackCount") Core.<*> x Core..:? "AttackVolume"
