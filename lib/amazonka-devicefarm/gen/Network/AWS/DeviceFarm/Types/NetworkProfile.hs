{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.NetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.NetworkProfile
  ( NetworkProfile (..),

    -- * Smart constructor
    mkNetworkProfile,

    -- * Lenses
    npUplinkJitterMs,
    npArn,
    npUplinkLossPercent,
    npDownlinkJitterMs,
    npName,
    npDownlinkLossPercent,
    npType,
    npUplinkDelayMs,
    npUplinkBandwidthBits,
    npDescription,
    npDownlinkDelayMs,
    npDownlinkBandwidthBits,
  )
where

import Network.AWS.DeviceFarm.Types.NetworkProfileType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An array of settings that describes characteristics of a network profile.
--
-- /See:/ 'mkNetworkProfile' smart constructor.
data NetworkProfile = NetworkProfile'
  { -- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
    uplinkJitterMs :: Lude.Maybe Lude.Integer,
    -- | The Amazon Resource Name (ARN) of the network profile.
    arn :: Lude.Maybe Lude.Text,
    -- | Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
    uplinkLossPercent :: Lude.Maybe Lude.Natural,
    -- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
    downlinkJitterMs :: Lude.Maybe Lude.Integer,
    -- | The name of the network profile.
    name :: Lude.Maybe Lude.Text,
    -- | Proportion of received packets that fail to arrive from 0 to 100 percent.
    downlinkLossPercent :: Lude.Maybe Lude.Natural,
    -- | The type of network profile. Valid values are listed here.
    type' :: Lude.Maybe NetworkProfileType,
    -- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
    uplinkDelayMs :: Lude.Maybe Lude.Integer,
    -- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
    uplinkBandwidthBits :: Lude.Maybe Lude.Integer,
    -- | The description of the network profile.
    description :: Lude.Maybe Lude.Text,
    -- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
    downlinkDelayMs :: Lude.Maybe Lude.Integer,
    -- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
    downlinkBandwidthBits :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkProfile' with the minimum fields required to make a request.
--
-- * 'uplinkJitterMs' - Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
-- * 'arn' - The Amazon Resource Name (ARN) of the network profile.
-- * 'uplinkLossPercent' - Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
-- * 'downlinkJitterMs' - Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
-- * 'name' - The name of the network profile.
-- * 'downlinkLossPercent' - Proportion of received packets that fail to arrive from 0 to 100 percent.
-- * 'type'' - The type of network profile. Valid values are listed here.
-- * 'uplinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
-- * 'uplinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to 104857600.
-- * 'description' - The description of the network profile.
-- * 'downlinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
-- * 'downlinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to 104857600.
mkNetworkProfile ::
  NetworkProfile
mkNetworkProfile =
  NetworkProfile'
    { uplinkJitterMs = Lude.Nothing,
      arn = Lude.Nothing,
      uplinkLossPercent = Lude.Nothing,
      downlinkJitterMs = Lude.Nothing,
      name = Lude.Nothing,
      downlinkLossPercent = Lude.Nothing,
      type' = Lude.Nothing,
      uplinkDelayMs = Lude.Nothing,
      uplinkBandwidthBits = Lude.Nothing,
      description = Lude.Nothing,
      downlinkDelayMs = Lude.Nothing,
      downlinkBandwidthBits = Lude.Nothing
    }

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'uplinkJitterMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npUplinkJitterMs :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Integer)
npUplinkJitterMs = Lens.lens (uplinkJitterMs :: NetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {uplinkJitterMs = a} :: NetworkProfile)
{-# DEPRECATED npUplinkJitterMs "Use generic-lens or generic-optics with 'uplinkJitterMs' instead." #-}

-- | The Amazon Resource Name (ARN) of the network profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npArn :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Text)
npArn = Lens.lens (arn :: NetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: NetworkProfile)
{-# DEPRECATED npArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
--
-- /Note:/ Consider using 'uplinkLossPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npUplinkLossPercent :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Natural)
npUplinkLossPercent = Lens.lens (uplinkLossPercent :: NetworkProfile -> Lude.Maybe Lude.Natural) (\s a -> s {uplinkLossPercent = a} :: NetworkProfile)
{-# DEPRECATED npUplinkLossPercent "Use generic-lens or generic-optics with 'uplinkLossPercent' instead." #-}

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'downlinkJitterMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npDownlinkJitterMs :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Integer)
npDownlinkJitterMs = Lens.lens (downlinkJitterMs :: NetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {downlinkJitterMs = a} :: NetworkProfile)
{-# DEPRECATED npDownlinkJitterMs "Use generic-lens or generic-optics with 'downlinkJitterMs' instead." #-}

-- | The name of the network profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npName :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Text)
npName = Lens.lens (name :: NetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: NetworkProfile)
{-# DEPRECATED npName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Proportion of received packets that fail to arrive from 0 to 100 percent.
--
-- /Note:/ Consider using 'downlinkLossPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npDownlinkLossPercent :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Natural)
npDownlinkLossPercent = Lens.lens (downlinkLossPercent :: NetworkProfile -> Lude.Maybe Lude.Natural) (\s a -> s {downlinkLossPercent = a} :: NetworkProfile)
{-# DEPRECATED npDownlinkLossPercent "Use generic-lens or generic-optics with 'downlinkLossPercent' instead." #-}

-- | The type of network profile. Valid values are listed here.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npType :: Lens.Lens' NetworkProfile (Lude.Maybe NetworkProfileType)
npType = Lens.lens (type' :: NetworkProfile -> Lude.Maybe NetworkProfileType) (\s a -> s {type' = a} :: NetworkProfile)
{-# DEPRECATED npType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'uplinkDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npUplinkDelayMs :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Integer)
npUplinkDelayMs = Lens.lens (uplinkDelayMs :: NetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {uplinkDelayMs = a} :: NetworkProfile)
{-# DEPRECATED npUplinkDelayMs "Use generic-lens or generic-optics with 'uplinkDelayMs' instead." #-}

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- /Note:/ Consider using 'uplinkBandwidthBits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npUplinkBandwidthBits :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Integer)
npUplinkBandwidthBits = Lens.lens (uplinkBandwidthBits :: NetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {uplinkBandwidthBits = a} :: NetworkProfile)
{-# DEPRECATED npUplinkBandwidthBits "Use generic-lens or generic-optics with 'uplinkBandwidthBits' instead." #-}

-- | The description of the network profile.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npDescription :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Text)
npDescription = Lens.lens (description :: NetworkProfile -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: NetworkProfile)
{-# DEPRECATED npDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- /Note:/ Consider using 'downlinkDelayMs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npDownlinkDelayMs :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Integer)
npDownlinkDelayMs = Lens.lens (downlinkDelayMs :: NetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {downlinkDelayMs = a} :: NetworkProfile)
{-# DEPRECATED npDownlinkDelayMs "Use generic-lens or generic-optics with 'downlinkDelayMs' instead." #-}

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- /Note:/ Consider using 'downlinkBandwidthBits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npDownlinkBandwidthBits :: Lens.Lens' NetworkProfile (Lude.Maybe Lude.Integer)
npDownlinkBandwidthBits = Lens.lens (downlinkBandwidthBits :: NetworkProfile -> Lude.Maybe Lude.Integer) (\s a -> s {downlinkBandwidthBits = a} :: NetworkProfile)
{-# DEPRECATED npDownlinkBandwidthBits "Use generic-lens or generic-optics with 'downlinkBandwidthBits' instead." #-}

instance Lude.FromJSON NetworkProfile where
  parseJSON =
    Lude.withObject
      "NetworkProfile"
      ( \x ->
          NetworkProfile'
            Lude.<$> (x Lude..:? "uplinkJitterMs")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "uplinkLossPercent")
            Lude.<*> (x Lude..:? "downlinkJitterMs")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "downlinkLossPercent")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "uplinkDelayMs")
            Lude.<*> (x Lude..:? "uplinkBandwidthBits")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "downlinkDelayMs")
            Lude.<*> (x Lude..:? "downlinkBandwidthBits")
      )
