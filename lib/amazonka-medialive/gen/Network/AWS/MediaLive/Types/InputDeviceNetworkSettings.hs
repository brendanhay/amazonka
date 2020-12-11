-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceNetworkSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceNetworkSettings
  ( InputDeviceNetworkSettings (..),

    -- * Smart constructor
    mkInputDeviceNetworkSettings,

    -- * Lenses
    idnsIPAddress,
    idnsGateway,
    idnsDNSAddresses,
    idnsIPScheme,
    idnsSubnetMask,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputDeviceIPScheme
import qualified Network.AWS.Prelude as Lude

-- | The network settings for the input device.
--
-- /See:/ 'mkInputDeviceNetworkSettings' smart constructor.
data InputDeviceNetworkSettings = InputDeviceNetworkSettings'
  { ipAddress ::
      Lude.Maybe Lude.Text,
    gateway :: Lude.Maybe Lude.Text,
    dnsAddresses ::
      Lude.Maybe [Lude.Text],
    ipScheme ::
      Lude.Maybe InputDeviceIPScheme,
    subnetMask :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputDeviceNetworkSettings' with the minimum fields required to make a request.
--
-- * 'dnsAddresses' - The DNS addresses of the input device.
-- * 'gateway' - The network gateway IP address.
-- * 'ipAddress' - The IP address of the input device.
-- * 'ipScheme' - Specifies whether the input device has been configured (outside of MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP address.
-- * 'subnetMask' - The subnet mask of the input device.
mkInputDeviceNetworkSettings ::
  InputDeviceNetworkSettings
mkInputDeviceNetworkSettings =
  InputDeviceNetworkSettings'
    { ipAddress = Lude.Nothing,
      gateway = Lude.Nothing,
      dnsAddresses = Lude.Nothing,
      ipScheme = Lude.Nothing,
      subnetMask = Lude.Nothing
    }

-- | The IP address of the input device.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idnsIPAddress :: Lens.Lens' InputDeviceNetworkSettings (Lude.Maybe Lude.Text)
idnsIPAddress = Lens.lens (ipAddress :: InputDeviceNetworkSettings -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: InputDeviceNetworkSettings)
{-# DEPRECATED idnsIPAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | The network gateway IP address.
--
-- /Note:/ Consider using 'gateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idnsGateway :: Lens.Lens' InputDeviceNetworkSettings (Lude.Maybe Lude.Text)
idnsGateway = Lens.lens (gateway :: InputDeviceNetworkSettings -> Lude.Maybe Lude.Text) (\s a -> s {gateway = a} :: InputDeviceNetworkSettings)
{-# DEPRECATED idnsGateway "Use generic-lens or generic-optics with 'gateway' instead." #-}

-- | The DNS addresses of the input device.
--
-- /Note:/ Consider using 'dnsAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idnsDNSAddresses :: Lens.Lens' InputDeviceNetworkSettings (Lude.Maybe [Lude.Text])
idnsDNSAddresses = Lens.lens (dnsAddresses :: InputDeviceNetworkSettings -> Lude.Maybe [Lude.Text]) (\s a -> s {dnsAddresses = a} :: InputDeviceNetworkSettings)
{-# DEPRECATED idnsDNSAddresses "Use generic-lens or generic-optics with 'dnsAddresses' instead." #-}

-- | Specifies whether the input device has been configured (outside of MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP address.
--
-- /Note:/ Consider using 'ipScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idnsIPScheme :: Lens.Lens' InputDeviceNetworkSettings (Lude.Maybe InputDeviceIPScheme)
idnsIPScheme = Lens.lens (ipScheme :: InputDeviceNetworkSettings -> Lude.Maybe InputDeviceIPScheme) (\s a -> s {ipScheme = a} :: InputDeviceNetworkSettings)
{-# DEPRECATED idnsIPScheme "Use generic-lens or generic-optics with 'ipScheme' instead." #-}

-- | The subnet mask of the input device.
--
-- /Note:/ Consider using 'subnetMask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idnsSubnetMask :: Lens.Lens' InputDeviceNetworkSettings (Lude.Maybe Lude.Text)
idnsSubnetMask = Lens.lens (subnetMask :: InputDeviceNetworkSettings -> Lude.Maybe Lude.Text) (\s a -> s {subnetMask = a} :: InputDeviceNetworkSettings)
{-# DEPRECATED idnsSubnetMask "Use generic-lens or generic-optics with 'subnetMask' instead." #-}

instance Lude.FromJSON InputDeviceNetworkSettings where
  parseJSON =
    Lude.withObject
      "InputDeviceNetworkSettings"
      ( \x ->
          InputDeviceNetworkSettings'
            Lude.<$> (x Lude..:? "ipAddress")
            Lude.<*> (x Lude..:? "gateway")
            Lude.<*> (x Lude..:? "dnsAddresses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ipScheme")
            Lude.<*> (x Lude..:? "subnetMask")
      )
