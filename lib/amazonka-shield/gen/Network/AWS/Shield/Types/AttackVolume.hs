{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.AttackVolume
  ( AttackVolume (..)
  -- * Smart constructor
  , mkAttackVolume
  -- * Lenses
  , avBitsPerSecond
  , avPacketsPerSecond
  , avRequestsPerSecond
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.AttackVolumeStatistics as Types

-- | Information about the volume of attacks during the time period, included in an 'AttackStatisticsDataItem' . If the accompanying @AttackCount@ in the statistics object is zero, this setting might be empty.
--
-- /See:/ 'mkAttackVolume' smart constructor.
data AttackVolume = AttackVolume'
  { bitsPerSecond :: Core.Maybe Types.AttackVolumeStatistics
    -- ^ A statistics object that uses bits per second as the unit. This is included for network level attacks. 
  , packetsPerSecond :: Core.Maybe Types.AttackVolumeStatistics
    -- ^ A statistics object that uses packets per second as the unit. This is included for network level attacks. 
  , requestsPerSecond :: Core.Maybe Types.AttackVolumeStatistics
    -- ^ A statistics object that uses requests per second as the unit. This is included for application level attacks, and is only available for accounts that are subscribed to Shield Advanced.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttackVolume' value with any optional fields omitted.
mkAttackVolume
    :: AttackVolume
mkAttackVolume
  = AttackVolume'{bitsPerSecond = Core.Nothing,
                  packetsPerSecond = Core.Nothing, requestsPerSecond = Core.Nothing}

-- | A statistics object that uses bits per second as the unit. This is included for network level attacks. 
--
-- /Note:/ Consider using 'bitsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avBitsPerSecond :: Lens.Lens' AttackVolume (Core.Maybe Types.AttackVolumeStatistics)
avBitsPerSecond = Lens.field @"bitsPerSecond"
{-# INLINEABLE avBitsPerSecond #-}
{-# DEPRECATED bitsPerSecond "Use generic-lens or generic-optics with 'bitsPerSecond' instead"  #-}

-- | A statistics object that uses packets per second as the unit. This is included for network level attacks. 
--
-- /Note:/ Consider using 'packetsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avPacketsPerSecond :: Lens.Lens' AttackVolume (Core.Maybe Types.AttackVolumeStatistics)
avPacketsPerSecond = Lens.field @"packetsPerSecond"
{-# INLINEABLE avPacketsPerSecond #-}
{-# DEPRECATED packetsPerSecond "Use generic-lens or generic-optics with 'packetsPerSecond' instead"  #-}

-- | A statistics object that uses requests per second as the unit. This is included for application level attacks, and is only available for accounts that are subscribed to Shield Advanced.
--
-- /Note:/ Consider using 'requestsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avRequestsPerSecond :: Lens.Lens' AttackVolume (Core.Maybe Types.AttackVolumeStatistics)
avRequestsPerSecond = Lens.field @"requestsPerSecond"
{-# INLINEABLE avRequestsPerSecond #-}
{-# DEPRECATED requestsPerSecond "Use generic-lens or generic-optics with 'requestsPerSecond' instead"  #-}

instance Core.FromJSON AttackVolume where
        parseJSON
          = Core.withObject "AttackVolume" Core.$
              \ x ->
                AttackVolume' Core.<$>
                  (x Core..:? "BitsPerSecond") Core.<*> x Core..:? "PacketsPerSecond"
                    Core.<*> x Core..:? "RequestsPerSecond"
