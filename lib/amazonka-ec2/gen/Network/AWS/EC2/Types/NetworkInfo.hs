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
    niDefaultNetworkCardIndex,
    niEfaSupported,
    niEnaSupport,
    niIpv4AddressesPerInterface,
    niIpv6AddressesPerInterface,
    niIpv6Supported,
    niMaximumNetworkCards,
    niMaximumNetworkInterfaces,
    niNetworkCards,
    niNetworkPerformance,
  )
where

import qualified Network.AWS.EC2.Types.EnaSupport as Types
import qualified Network.AWS.EC2.Types.NetworkCardInfo as Types
import qualified Network.AWS.EC2.Types.NetworkPerformance as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the networking features of the instance type.
--
-- /See:/ 'mkNetworkInfo' smart constructor.
data NetworkInfo = NetworkInfo'
  { -- | The index of the default network card, starting at 0.
    defaultNetworkCardIndex :: Core.Maybe Core.Int,
    -- | Indicates whether Elastic Fabric Adapter (EFA) is supported.
    efaSupported :: Core.Maybe Core.Bool,
    -- | Indicates whether Elastic Network Adapter (ENA) is supported.
    enaSupport :: Core.Maybe Types.EnaSupport,
    -- | The maximum number of IPv4 addresses per network interface.
    ipv4AddressesPerInterface :: Core.Maybe Core.Int,
    -- | The maximum number of IPv6 addresses per network interface.
    ipv6AddressesPerInterface :: Core.Maybe Core.Int,
    -- | Indicates whether IPv6 is supported.
    ipv6Supported :: Core.Maybe Core.Bool,
    -- | The maximum number of physical network cards that can be allocated to the instance.
    maximumNetworkCards :: Core.Maybe Core.Int,
    -- | The maximum number of network interfaces for the instance type.
    maximumNetworkInterfaces :: Core.Maybe Core.Int,
    -- | Describes the network cards for the instance type.
    networkCards :: Core.Maybe [Types.NetworkCardInfo],
    -- | The network performance.
    networkPerformance :: Core.Maybe Types.NetworkPerformance
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkInfo' value with any optional fields omitted.
mkNetworkInfo ::
  NetworkInfo
mkNetworkInfo =
  NetworkInfo'
    { defaultNetworkCardIndex = Core.Nothing,
      efaSupported = Core.Nothing,
      enaSupport = Core.Nothing,
      ipv4AddressesPerInterface = Core.Nothing,
      ipv6AddressesPerInterface = Core.Nothing,
      ipv6Supported = Core.Nothing,
      maximumNetworkCards = Core.Nothing,
      maximumNetworkInterfaces = Core.Nothing,
      networkCards = Core.Nothing,
      networkPerformance = Core.Nothing
    }

-- | The index of the default network card, starting at 0.
--
-- /Note:/ Consider using 'defaultNetworkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niDefaultNetworkCardIndex :: Lens.Lens' NetworkInfo (Core.Maybe Core.Int)
niDefaultNetworkCardIndex = Lens.field @"defaultNetworkCardIndex"
{-# DEPRECATED niDefaultNetworkCardIndex "Use generic-lens or generic-optics with 'defaultNetworkCardIndex' instead." #-}

-- | Indicates whether Elastic Fabric Adapter (EFA) is supported.
--
-- /Note:/ Consider using 'efaSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niEfaSupported :: Lens.Lens' NetworkInfo (Core.Maybe Core.Bool)
niEfaSupported = Lens.field @"efaSupported"
{-# DEPRECATED niEfaSupported "Use generic-lens or generic-optics with 'efaSupported' instead." #-}

-- | Indicates whether Elastic Network Adapter (ENA) is supported.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niEnaSupport :: Lens.Lens' NetworkInfo (Core.Maybe Types.EnaSupport)
niEnaSupport = Lens.field @"enaSupport"
{-# DEPRECATED niEnaSupport "Use generic-lens or generic-optics with 'enaSupport' instead." #-}

-- | The maximum number of IPv4 addresses per network interface.
--
-- /Note:/ Consider using 'ipv4AddressesPerInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIpv4AddressesPerInterface :: Lens.Lens' NetworkInfo (Core.Maybe Core.Int)
niIpv4AddressesPerInterface = Lens.field @"ipv4AddressesPerInterface"
{-# DEPRECATED niIpv4AddressesPerInterface "Use generic-lens or generic-optics with 'ipv4AddressesPerInterface' instead." #-}

-- | The maximum number of IPv6 addresses per network interface.
--
-- /Note:/ Consider using 'ipv6AddressesPerInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIpv6AddressesPerInterface :: Lens.Lens' NetworkInfo (Core.Maybe Core.Int)
niIpv6AddressesPerInterface = Lens.field @"ipv6AddressesPerInterface"
{-# DEPRECATED niIpv6AddressesPerInterface "Use generic-lens or generic-optics with 'ipv6AddressesPerInterface' instead." #-}

-- | Indicates whether IPv6 is supported.
--
-- /Note:/ Consider using 'ipv6Supported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIpv6Supported :: Lens.Lens' NetworkInfo (Core.Maybe Core.Bool)
niIpv6Supported = Lens.field @"ipv6Supported"
{-# DEPRECATED niIpv6Supported "Use generic-lens or generic-optics with 'ipv6Supported' instead." #-}

-- | The maximum number of physical network cards that can be allocated to the instance.
--
-- /Note:/ Consider using 'maximumNetworkCards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niMaximumNetworkCards :: Lens.Lens' NetworkInfo (Core.Maybe Core.Int)
niMaximumNetworkCards = Lens.field @"maximumNetworkCards"
{-# DEPRECATED niMaximumNetworkCards "Use generic-lens or generic-optics with 'maximumNetworkCards' instead." #-}

-- | The maximum number of network interfaces for the instance type.
--
-- /Note:/ Consider using 'maximumNetworkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niMaximumNetworkInterfaces :: Lens.Lens' NetworkInfo (Core.Maybe Core.Int)
niMaximumNetworkInterfaces = Lens.field @"maximumNetworkInterfaces"
{-# DEPRECATED niMaximumNetworkInterfaces "Use generic-lens or generic-optics with 'maximumNetworkInterfaces' instead." #-}

-- | Describes the network cards for the instance type.
--
-- /Note:/ Consider using 'networkCards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niNetworkCards :: Lens.Lens' NetworkInfo (Core.Maybe [Types.NetworkCardInfo])
niNetworkCards = Lens.field @"networkCards"
{-# DEPRECATED niNetworkCards "Use generic-lens or generic-optics with 'networkCards' instead." #-}

-- | The network performance.
--
-- /Note:/ Consider using 'networkPerformance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niNetworkPerformance :: Lens.Lens' NetworkInfo (Core.Maybe Types.NetworkPerformance)
niNetworkPerformance = Lens.field @"networkPerformance"
{-# DEPRECATED niNetworkPerformance "Use generic-lens or generic-optics with 'networkPerformance' instead." #-}

instance Core.FromXML NetworkInfo where
  parseXML x =
    NetworkInfo'
      Core.<$> (x Core..@? "defaultNetworkCardIndex")
      Core.<*> (x Core..@? "efaSupported")
      Core.<*> (x Core..@? "enaSupport")
      Core.<*> (x Core..@? "ipv4AddressesPerInterface")
      Core.<*> (x Core..@? "ipv6AddressesPerInterface")
      Core.<*> (x Core..@? "ipv6Supported")
      Core.<*> (x Core..@? "maximumNetworkCards")
      Core.<*> (x Core..@? "maximumNetworkInterfaces")
      Core.<*> (x Core..@? "networkCards" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "networkPerformance")
