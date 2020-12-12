{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInfo
  ( NetworkInfo (..),

    -- * Smart constructor
    mkNetworkInfo,

    -- * Lenses
    niEfaSupported,
    niIPv6Supported,
    niEnaSupport,
    niMaximumNetworkInterfaces,
    niIPv6AddressesPerInterface,
    niNetworkPerformance,
    niMaximumNetworkCards,
    niNetworkCards,
    niDefaultNetworkCardIndex,
    niIPv4AddressesPerInterface,
  )
where

import Network.AWS.EC2.Types.EnaSupport
import Network.AWS.EC2.Types.NetworkCardInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the networking features of the instance type.
--
-- /See:/ 'mkNetworkInfo' smart constructor.
data NetworkInfo = NetworkInfo'
  { efaSupported ::
      Lude.Maybe Lude.Bool,
    ipv6Supported :: Lude.Maybe Lude.Bool,
    enaSupport :: Lude.Maybe EnaSupport,
    maximumNetworkInterfaces :: Lude.Maybe Lude.Int,
    ipv6AddressesPerInterface :: Lude.Maybe Lude.Int,
    networkPerformance :: Lude.Maybe Lude.Text,
    maximumNetworkCards :: Lude.Maybe Lude.Int,
    networkCards :: Lude.Maybe [NetworkCardInfo],
    defaultNetworkCardIndex :: Lude.Maybe Lude.Int,
    ipv4AddressesPerInterface :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkInfo' with the minimum fields required to make a request.
--
-- * 'defaultNetworkCardIndex' - The index of the default network card, starting at 0.
-- * 'efaSupported' - Indicates whether Elastic Fabric Adapter (EFA) is supported.
-- * 'enaSupport' - Indicates whether Elastic Network Adapter (ENA) is supported.
-- * 'ipv4AddressesPerInterface' - The maximum number of IPv4 addresses per network interface.
-- * 'ipv6AddressesPerInterface' - The maximum number of IPv6 addresses per network interface.
-- * 'ipv6Supported' - Indicates whether IPv6 is supported.
-- * 'maximumNetworkCards' - The maximum number of physical network cards that can be allocated to the instance.
-- * 'maximumNetworkInterfaces' - The maximum number of network interfaces for the instance type.
-- * 'networkCards' - Describes the network cards for the instance type.
-- * 'networkPerformance' - The network performance.
mkNetworkInfo ::
  NetworkInfo
mkNetworkInfo =
  NetworkInfo'
    { efaSupported = Lude.Nothing,
      ipv6Supported = Lude.Nothing,
      enaSupport = Lude.Nothing,
      maximumNetworkInterfaces = Lude.Nothing,
      ipv6AddressesPerInterface = Lude.Nothing,
      networkPerformance = Lude.Nothing,
      maximumNetworkCards = Lude.Nothing,
      networkCards = Lude.Nothing,
      defaultNetworkCardIndex = Lude.Nothing,
      ipv4AddressesPerInterface = Lude.Nothing
    }

-- | Indicates whether Elastic Fabric Adapter (EFA) is supported.
--
-- /Note:/ Consider using 'efaSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niEfaSupported :: Lens.Lens' NetworkInfo (Lude.Maybe Lude.Bool)
niEfaSupported = Lens.lens (efaSupported :: NetworkInfo -> Lude.Maybe Lude.Bool) (\s a -> s {efaSupported = a} :: NetworkInfo)
{-# DEPRECATED niEfaSupported "Use generic-lens or generic-optics with 'efaSupported' instead." #-}

-- | Indicates whether IPv6 is supported.
--
-- /Note:/ Consider using 'ipv6Supported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIPv6Supported :: Lens.Lens' NetworkInfo (Lude.Maybe Lude.Bool)
niIPv6Supported = Lens.lens (ipv6Supported :: NetworkInfo -> Lude.Maybe Lude.Bool) (\s a -> s {ipv6Supported = a} :: NetworkInfo)
{-# DEPRECATED niIPv6Supported "Use generic-lens or generic-optics with 'ipv6Supported' instead." #-}

-- | Indicates whether Elastic Network Adapter (ENA) is supported.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niEnaSupport :: Lens.Lens' NetworkInfo (Lude.Maybe EnaSupport)
niEnaSupport = Lens.lens (enaSupport :: NetworkInfo -> Lude.Maybe EnaSupport) (\s a -> s {enaSupport = a} :: NetworkInfo)
{-# DEPRECATED niEnaSupport "Use generic-lens or generic-optics with 'enaSupport' instead." #-}

-- | The maximum number of network interfaces for the instance type.
--
-- /Note:/ Consider using 'maximumNetworkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niMaximumNetworkInterfaces :: Lens.Lens' NetworkInfo (Lude.Maybe Lude.Int)
niMaximumNetworkInterfaces = Lens.lens (maximumNetworkInterfaces :: NetworkInfo -> Lude.Maybe Lude.Int) (\s a -> s {maximumNetworkInterfaces = a} :: NetworkInfo)
{-# DEPRECATED niMaximumNetworkInterfaces "Use generic-lens or generic-optics with 'maximumNetworkInterfaces' instead." #-}

-- | The maximum number of IPv6 addresses per network interface.
--
-- /Note:/ Consider using 'ipv6AddressesPerInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIPv6AddressesPerInterface :: Lens.Lens' NetworkInfo (Lude.Maybe Lude.Int)
niIPv6AddressesPerInterface = Lens.lens (ipv6AddressesPerInterface :: NetworkInfo -> Lude.Maybe Lude.Int) (\s a -> s {ipv6AddressesPerInterface = a} :: NetworkInfo)
{-# DEPRECATED niIPv6AddressesPerInterface "Use generic-lens or generic-optics with 'ipv6AddressesPerInterface' instead." #-}

-- | The network performance.
--
-- /Note:/ Consider using 'networkPerformance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niNetworkPerformance :: Lens.Lens' NetworkInfo (Lude.Maybe Lude.Text)
niNetworkPerformance = Lens.lens (networkPerformance :: NetworkInfo -> Lude.Maybe Lude.Text) (\s a -> s {networkPerformance = a} :: NetworkInfo)
{-# DEPRECATED niNetworkPerformance "Use generic-lens or generic-optics with 'networkPerformance' instead." #-}

-- | The maximum number of physical network cards that can be allocated to the instance.
--
-- /Note:/ Consider using 'maximumNetworkCards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niMaximumNetworkCards :: Lens.Lens' NetworkInfo (Lude.Maybe Lude.Int)
niMaximumNetworkCards = Lens.lens (maximumNetworkCards :: NetworkInfo -> Lude.Maybe Lude.Int) (\s a -> s {maximumNetworkCards = a} :: NetworkInfo)
{-# DEPRECATED niMaximumNetworkCards "Use generic-lens or generic-optics with 'maximumNetworkCards' instead." #-}

-- | Describes the network cards for the instance type.
--
-- /Note:/ Consider using 'networkCards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niNetworkCards :: Lens.Lens' NetworkInfo (Lude.Maybe [NetworkCardInfo])
niNetworkCards = Lens.lens (networkCards :: NetworkInfo -> Lude.Maybe [NetworkCardInfo]) (\s a -> s {networkCards = a} :: NetworkInfo)
{-# DEPRECATED niNetworkCards "Use generic-lens or generic-optics with 'networkCards' instead." #-}

-- | The index of the default network card, starting at 0.
--
-- /Note:/ Consider using 'defaultNetworkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niDefaultNetworkCardIndex :: Lens.Lens' NetworkInfo (Lude.Maybe Lude.Int)
niDefaultNetworkCardIndex = Lens.lens (defaultNetworkCardIndex :: NetworkInfo -> Lude.Maybe Lude.Int) (\s a -> s {defaultNetworkCardIndex = a} :: NetworkInfo)
{-# DEPRECATED niDefaultNetworkCardIndex "Use generic-lens or generic-optics with 'defaultNetworkCardIndex' instead." #-}

-- | The maximum number of IPv4 addresses per network interface.
--
-- /Note:/ Consider using 'ipv4AddressesPerInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIPv4AddressesPerInterface :: Lens.Lens' NetworkInfo (Lude.Maybe Lude.Int)
niIPv4AddressesPerInterface = Lens.lens (ipv4AddressesPerInterface :: NetworkInfo -> Lude.Maybe Lude.Int) (\s a -> s {ipv4AddressesPerInterface = a} :: NetworkInfo)
{-# DEPRECATED niIPv4AddressesPerInterface "Use generic-lens or generic-optics with 'ipv4AddressesPerInterface' instead." #-}

instance Lude.FromXML NetworkInfo where
  parseXML x =
    NetworkInfo'
      Lude.<$> (x Lude..@? "efaSupported")
      Lude.<*> (x Lude..@? "ipv6Supported")
      Lude.<*> (x Lude..@? "enaSupport")
      Lude.<*> (x Lude..@? "maximumNetworkInterfaces")
      Lude.<*> (x Lude..@? "ipv6AddressesPerInterface")
      Lude.<*> (x Lude..@? "networkPerformance")
      Lude.<*> (x Lude..@? "maximumNetworkCards")
      Lude.<*> ( x Lude..@? "networkCards" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "defaultNetworkCardIndex")
      Lude.<*> (x Lude..@? "ipv4AddressesPerInterface")
