{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.EnaSupport
import Network.AWS.EC2.Types.NetworkCardInfo
import qualified Network.AWS.Lens as Lens

-- | Describes the networking features of the instance type.
--
-- /See:/ 'newNetworkInfo' smart constructor.
data NetworkInfo = NetworkInfo'
  { -- | The index of the default network card, starting at 0.
    defaultNetworkCardIndex :: Core.Maybe Core.Int,
    -- | Indicates whether Elastic Fabric Adapter (EFA) is supported.
    efaSupported :: Core.Maybe Core.Bool,
    -- | Describes the network cards for the instance type.
    networkCards :: Core.Maybe [NetworkCardInfo],
    -- | The maximum number of IPv4 addresses per network interface.
    ipv4AddressesPerInterface :: Core.Maybe Core.Int,
    -- | The maximum number of network interfaces for the instance type.
    maximumNetworkInterfaces :: Core.Maybe Core.Int,
    -- | Indicates whether IPv6 is supported.
    ipv6Supported :: Core.Maybe Core.Bool,
    -- | The maximum number of physical network cards that can be allocated to
    -- the instance.
    maximumNetworkCards :: Core.Maybe Core.Int,
    -- | The network performance.
    networkPerformance :: Core.Maybe Core.Text,
    -- | The maximum number of IPv6 addresses per network interface.
    ipv6AddressesPerInterface :: Core.Maybe Core.Int,
    -- | Indicates whether Elastic Network Adapter (ENA) is supported.
    enaSupport :: Core.Maybe EnaSupport
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NetworkInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultNetworkCardIndex', 'networkInfo_defaultNetworkCardIndex' - The index of the default network card, starting at 0.
--
-- 'efaSupported', 'networkInfo_efaSupported' - Indicates whether Elastic Fabric Adapter (EFA) is supported.
--
-- 'networkCards', 'networkInfo_networkCards' - Describes the network cards for the instance type.
--
-- 'ipv4AddressesPerInterface', 'networkInfo_ipv4AddressesPerInterface' - The maximum number of IPv4 addresses per network interface.
--
-- 'maximumNetworkInterfaces', 'networkInfo_maximumNetworkInterfaces' - The maximum number of network interfaces for the instance type.
--
-- 'ipv6Supported', 'networkInfo_ipv6Supported' - Indicates whether IPv6 is supported.
--
-- 'maximumNetworkCards', 'networkInfo_maximumNetworkCards' - The maximum number of physical network cards that can be allocated to
-- the instance.
--
-- 'networkPerformance', 'networkInfo_networkPerformance' - The network performance.
--
-- 'ipv6AddressesPerInterface', 'networkInfo_ipv6AddressesPerInterface' - The maximum number of IPv6 addresses per network interface.
--
-- 'enaSupport', 'networkInfo_enaSupport' - Indicates whether Elastic Network Adapter (ENA) is supported.
newNetworkInfo ::
  NetworkInfo
newNetworkInfo =
  NetworkInfo'
    { defaultNetworkCardIndex =
        Core.Nothing,
      efaSupported = Core.Nothing,
      networkCards = Core.Nothing,
      ipv4AddressesPerInterface = Core.Nothing,
      maximumNetworkInterfaces = Core.Nothing,
      ipv6Supported = Core.Nothing,
      maximumNetworkCards = Core.Nothing,
      networkPerformance = Core.Nothing,
      ipv6AddressesPerInterface = Core.Nothing,
      enaSupport = Core.Nothing
    }

-- | The index of the default network card, starting at 0.
networkInfo_defaultNetworkCardIndex :: Lens.Lens' NetworkInfo (Core.Maybe Core.Int)
networkInfo_defaultNetworkCardIndex = Lens.lens (\NetworkInfo' {defaultNetworkCardIndex} -> defaultNetworkCardIndex) (\s@NetworkInfo' {} a -> s {defaultNetworkCardIndex = a} :: NetworkInfo)

-- | Indicates whether Elastic Fabric Adapter (EFA) is supported.
networkInfo_efaSupported :: Lens.Lens' NetworkInfo (Core.Maybe Core.Bool)
networkInfo_efaSupported = Lens.lens (\NetworkInfo' {efaSupported} -> efaSupported) (\s@NetworkInfo' {} a -> s {efaSupported = a} :: NetworkInfo)

-- | Describes the network cards for the instance type.
networkInfo_networkCards :: Lens.Lens' NetworkInfo (Core.Maybe [NetworkCardInfo])
networkInfo_networkCards = Lens.lens (\NetworkInfo' {networkCards} -> networkCards) (\s@NetworkInfo' {} a -> s {networkCards = a} :: NetworkInfo) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of IPv4 addresses per network interface.
networkInfo_ipv4AddressesPerInterface :: Lens.Lens' NetworkInfo (Core.Maybe Core.Int)
networkInfo_ipv4AddressesPerInterface = Lens.lens (\NetworkInfo' {ipv4AddressesPerInterface} -> ipv4AddressesPerInterface) (\s@NetworkInfo' {} a -> s {ipv4AddressesPerInterface = a} :: NetworkInfo)

-- | The maximum number of network interfaces for the instance type.
networkInfo_maximumNetworkInterfaces :: Lens.Lens' NetworkInfo (Core.Maybe Core.Int)
networkInfo_maximumNetworkInterfaces = Lens.lens (\NetworkInfo' {maximumNetworkInterfaces} -> maximumNetworkInterfaces) (\s@NetworkInfo' {} a -> s {maximumNetworkInterfaces = a} :: NetworkInfo)

-- | Indicates whether IPv6 is supported.
networkInfo_ipv6Supported :: Lens.Lens' NetworkInfo (Core.Maybe Core.Bool)
networkInfo_ipv6Supported = Lens.lens (\NetworkInfo' {ipv6Supported} -> ipv6Supported) (\s@NetworkInfo' {} a -> s {ipv6Supported = a} :: NetworkInfo)

-- | The maximum number of physical network cards that can be allocated to
-- the instance.
networkInfo_maximumNetworkCards :: Lens.Lens' NetworkInfo (Core.Maybe Core.Int)
networkInfo_maximumNetworkCards = Lens.lens (\NetworkInfo' {maximumNetworkCards} -> maximumNetworkCards) (\s@NetworkInfo' {} a -> s {maximumNetworkCards = a} :: NetworkInfo)

-- | The network performance.
networkInfo_networkPerformance :: Lens.Lens' NetworkInfo (Core.Maybe Core.Text)
networkInfo_networkPerformance = Lens.lens (\NetworkInfo' {networkPerformance} -> networkPerformance) (\s@NetworkInfo' {} a -> s {networkPerformance = a} :: NetworkInfo)

-- | The maximum number of IPv6 addresses per network interface.
networkInfo_ipv6AddressesPerInterface :: Lens.Lens' NetworkInfo (Core.Maybe Core.Int)
networkInfo_ipv6AddressesPerInterface = Lens.lens (\NetworkInfo' {ipv6AddressesPerInterface} -> ipv6AddressesPerInterface) (\s@NetworkInfo' {} a -> s {ipv6AddressesPerInterface = a} :: NetworkInfo)

-- | Indicates whether Elastic Network Adapter (ENA) is supported.
networkInfo_enaSupport :: Lens.Lens' NetworkInfo (Core.Maybe EnaSupport)
networkInfo_enaSupport = Lens.lens (\NetworkInfo' {enaSupport} -> enaSupport) (\s@NetworkInfo' {} a -> s {enaSupport = a} :: NetworkInfo)

instance Core.FromXML NetworkInfo where
  parseXML x =
    NetworkInfo'
      Core.<$> (x Core..@? "defaultNetworkCardIndex")
      Core.<*> (x Core..@? "efaSupported")
      Core.<*> ( x Core..@? "networkCards" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "ipv4AddressesPerInterface")
      Core.<*> (x Core..@? "maximumNetworkInterfaces")
      Core.<*> (x Core..@? "ipv6Supported")
      Core.<*> (x Core..@? "maximumNetworkCards")
      Core.<*> (x Core..@? "networkPerformance")
      Core.<*> (x Core..@? "ipv6AddressesPerInterface")
      Core.<*> (x Core..@? "enaSupport")

instance Core.Hashable NetworkInfo

instance Core.NFData NetworkInfo
