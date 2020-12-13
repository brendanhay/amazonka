{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackVolume
  ( AttackVolume (..),

    -- * Smart constructor
    mkAttackVolume,

    -- * Lenses
    avPacketsPerSecond,
    avRequestsPerSecond,
    avBitsPerSecond,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Shield.Types.AttackVolumeStatistics

-- | Information about the volume of attacks during the time period, included in an 'AttackStatisticsDataItem' . If the accompanying @AttackCount@ in the statistics object is zero, this setting might be empty.
--
-- /See:/ 'mkAttackVolume' smart constructor.
data AttackVolume = AttackVolume'
  { -- | A statistics object that uses packets per second as the unit. This is included for network level attacks.
    packetsPerSecond :: Lude.Maybe AttackVolumeStatistics,
    -- | A statistics object that uses requests per second as the unit. This is included for application level attacks, and is only available for accounts that are subscribed to Shield Advanced.
    requestsPerSecond :: Lude.Maybe AttackVolumeStatistics,
    -- | A statistics object that uses bits per second as the unit. This is included for network level attacks.
    bitsPerSecond :: Lude.Maybe AttackVolumeStatistics
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttackVolume' with the minimum fields required to make a request.
--
-- * 'packetsPerSecond' - A statistics object that uses packets per second as the unit. This is included for network level attacks.
-- * 'requestsPerSecond' - A statistics object that uses requests per second as the unit. This is included for application level attacks, and is only available for accounts that are subscribed to Shield Advanced.
-- * 'bitsPerSecond' - A statistics object that uses bits per second as the unit. This is included for network level attacks.
mkAttackVolume ::
  AttackVolume
mkAttackVolume =
  AttackVolume'
    { packetsPerSecond = Lude.Nothing,
      requestsPerSecond = Lude.Nothing,
      bitsPerSecond = Lude.Nothing
    }

-- | A statistics object that uses packets per second as the unit. This is included for network level attacks.
--
-- /Note:/ Consider using 'packetsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avPacketsPerSecond :: Lens.Lens' AttackVolume (Lude.Maybe AttackVolumeStatistics)
avPacketsPerSecond = Lens.lens (packetsPerSecond :: AttackVolume -> Lude.Maybe AttackVolumeStatistics) (\s a -> s {packetsPerSecond = a} :: AttackVolume)
{-# DEPRECATED avPacketsPerSecond "Use generic-lens or generic-optics with 'packetsPerSecond' instead." #-}

-- | A statistics object that uses requests per second as the unit. This is included for application level attacks, and is only available for accounts that are subscribed to Shield Advanced.
--
-- /Note:/ Consider using 'requestsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avRequestsPerSecond :: Lens.Lens' AttackVolume (Lude.Maybe AttackVolumeStatistics)
avRequestsPerSecond = Lens.lens (requestsPerSecond :: AttackVolume -> Lude.Maybe AttackVolumeStatistics) (\s a -> s {requestsPerSecond = a} :: AttackVolume)
{-# DEPRECATED avRequestsPerSecond "Use generic-lens or generic-optics with 'requestsPerSecond' instead." #-}

-- | A statistics object that uses bits per second as the unit. This is included for network level attacks.
--
-- /Note:/ Consider using 'bitsPerSecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avBitsPerSecond :: Lens.Lens' AttackVolume (Lude.Maybe AttackVolumeStatistics)
avBitsPerSecond = Lens.lens (bitsPerSecond :: AttackVolume -> Lude.Maybe AttackVolumeStatistics) (\s a -> s {bitsPerSecond = a} :: AttackVolume)
{-# DEPRECATED avBitsPerSecond "Use generic-lens or generic-optics with 'bitsPerSecond' instead." #-}

instance Lude.FromJSON AttackVolume where
  parseJSON =
    Lude.withObject
      "AttackVolume"
      ( \x ->
          AttackVolume'
            Lude.<$> (x Lude..:? "PacketsPerSecond")
            Lude.<*> (x Lude..:? "RequestsPerSecond")
            Lude.<*> (x Lude..:? "BitsPerSecond")
      )
