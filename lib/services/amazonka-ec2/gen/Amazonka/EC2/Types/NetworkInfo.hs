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
-- Module      : Amazonka.EC2.Types.NetworkInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.EfaInfo
import Amazonka.EC2.Types.EnaSupport
import Amazonka.EC2.Types.NetworkCardInfo
import qualified Amazonka.Prelude as Prelude

-- | Describes the networking features of the instance type.
--
-- /See:/ 'newNetworkInfo' smart constructor.
data NetworkInfo = NetworkInfo'
  { -- | The maximum number of IPv4 addresses per network interface.
    ipv4AddressesPerInterface :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether IPv6 is supported.
    ipv6Supported :: Prelude.Maybe Prelude.Bool,
    -- | The network performance.
    networkPerformance :: Prelude.Maybe Prelude.Text,
    -- | Describes the Elastic Fabric Adapters for the instance type.
    efaInfo :: Prelude.Maybe EfaInfo,
    -- | The index of the default network card, starting at 0.
    defaultNetworkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether Elastic Fabric Adapter (EFA) is supported.
    efaSupported :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of IPv6 addresses per network interface.
    ipv6AddressesPerInterface :: Prelude.Maybe Prelude.Int,
    -- | Describes the network cards for the instance type.
    networkCards :: Prelude.Maybe [NetworkCardInfo],
    -- | The maximum number of network interfaces for the instance type.
    maximumNetworkInterfaces :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the instance type automatically encrypts in-transit
    -- traffic between instances.
    encryptionInTransitSupported :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether Elastic Network Adapter (ENA) is supported.
    enaSupport :: Prelude.Maybe EnaSupport,
    -- | The maximum number of physical network cards that can be allocated to
    -- the instance.
    maximumNetworkCards :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv4AddressesPerInterface', 'networkInfo_ipv4AddressesPerInterface' - The maximum number of IPv4 addresses per network interface.
--
-- 'ipv6Supported', 'networkInfo_ipv6Supported' - Indicates whether IPv6 is supported.
--
-- 'networkPerformance', 'networkInfo_networkPerformance' - The network performance.
--
-- 'efaInfo', 'networkInfo_efaInfo' - Describes the Elastic Fabric Adapters for the instance type.
--
-- 'defaultNetworkCardIndex', 'networkInfo_defaultNetworkCardIndex' - The index of the default network card, starting at 0.
--
-- 'efaSupported', 'networkInfo_efaSupported' - Indicates whether Elastic Fabric Adapter (EFA) is supported.
--
-- 'ipv6AddressesPerInterface', 'networkInfo_ipv6AddressesPerInterface' - The maximum number of IPv6 addresses per network interface.
--
-- 'networkCards', 'networkInfo_networkCards' - Describes the network cards for the instance type.
--
-- 'maximumNetworkInterfaces', 'networkInfo_maximumNetworkInterfaces' - The maximum number of network interfaces for the instance type.
--
-- 'encryptionInTransitSupported', 'networkInfo_encryptionInTransitSupported' - Indicates whether the instance type automatically encrypts in-transit
-- traffic between instances.
--
-- 'enaSupport', 'networkInfo_enaSupport' - Indicates whether Elastic Network Adapter (ENA) is supported.
--
-- 'maximumNetworkCards', 'networkInfo_maximumNetworkCards' - The maximum number of physical network cards that can be allocated to
-- the instance.
newNetworkInfo ::
  NetworkInfo
newNetworkInfo =
  NetworkInfo'
    { ipv4AddressesPerInterface =
        Prelude.Nothing,
      ipv6Supported = Prelude.Nothing,
      networkPerformance = Prelude.Nothing,
      efaInfo = Prelude.Nothing,
      defaultNetworkCardIndex = Prelude.Nothing,
      efaSupported = Prelude.Nothing,
      ipv6AddressesPerInterface = Prelude.Nothing,
      networkCards = Prelude.Nothing,
      maximumNetworkInterfaces = Prelude.Nothing,
      encryptionInTransitSupported = Prelude.Nothing,
      enaSupport = Prelude.Nothing,
      maximumNetworkCards = Prelude.Nothing
    }

-- | The maximum number of IPv4 addresses per network interface.
networkInfo_ipv4AddressesPerInterface :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Int)
networkInfo_ipv4AddressesPerInterface = Lens.lens (\NetworkInfo' {ipv4AddressesPerInterface} -> ipv4AddressesPerInterface) (\s@NetworkInfo' {} a -> s {ipv4AddressesPerInterface = a} :: NetworkInfo)

-- | Indicates whether IPv6 is supported.
networkInfo_ipv6Supported :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Bool)
networkInfo_ipv6Supported = Lens.lens (\NetworkInfo' {ipv6Supported} -> ipv6Supported) (\s@NetworkInfo' {} a -> s {ipv6Supported = a} :: NetworkInfo)

-- | The network performance.
networkInfo_networkPerformance :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Text)
networkInfo_networkPerformance = Lens.lens (\NetworkInfo' {networkPerformance} -> networkPerformance) (\s@NetworkInfo' {} a -> s {networkPerformance = a} :: NetworkInfo)

-- | Describes the Elastic Fabric Adapters for the instance type.
networkInfo_efaInfo :: Lens.Lens' NetworkInfo (Prelude.Maybe EfaInfo)
networkInfo_efaInfo = Lens.lens (\NetworkInfo' {efaInfo} -> efaInfo) (\s@NetworkInfo' {} a -> s {efaInfo = a} :: NetworkInfo)

-- | The index of the default network card, starting at 0.
networkInfo_defaultNetworkCardIndex :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Int)
networkInfo_defaultNetworkCardIndex = Lens.lens (\NetworkInfo' {defaultNetworkCardIndex} -> defaultNetworkCardIndex) (\s@NetworkInfo' {} a -> s {defaultNetworkCardIndex = a} :: NetworkInfo)

-- | Indicates whether Elastic Fabric Adapter (EFA) is supported.
networkInfo_efaSupported :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Bool)
networkInfo_efaSupported = Lens.lens (\NetworkInfo' {efaSupported} -> efaSupported) (\s@NetworkInfo' {} a -> s {efaSupported = a} :: NetworkInfo)

-- | The maximum number of IPv6 addresses per network interface.
networkInfo_ipv6AddressesPerInterface :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Int)
networkInfo_ipv6AddressesPerInterface = Lens.lens (\NetworkInfo' {ipv6AddressesPerInterface} -> ipv6AddressesPerInterface) (\s@NetworkInfo' {} a -> s {ipv6AddressesPerInterface = a} :: NetworkInfo)

-- | Describes the network cards for the instance type.
networkInfo_networkCards :: Lens.Lens' NetworkInfo (Prelude.Maybe [NetworkCardInfo])
networkInfo_networkCards = Lens.lens (\NetworkInfo' {networkCards} -> networkCards) (\s@NetworkInfo' {} a -> s {networkCards = a} :: NetworkInfo) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of network interfaces for the instance type.
networkInfo_maximumNetworkInterfaces :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Int)
networkInfo_maximumNetworkInterfaces = Lens.lens (\NetworkInfo' {maximumNetworkInterfaces} -> maximumNetworkInterfaces) (\s@NetworkInfo' {} a -> s {maximumNetworkInterfaces = a} :: NetworkInfo)

-- | Indicates whether the instance type automatically encrypts in-transit
-- traffic between instances.
networkInfo_encryptionInTransitSupported :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Bool)
networkInfo_encryptionInTransitSupported = Lens.lens (\NetworkInfo' {encryptionInTransitSupported} -> encryptionInTransitSupported) (\s@NetworkInfo' {} a -> s {encryptionInTransitSupported = a} :: NetworkInfo)

-- | Indicates whether Elastic Network Adapter (ENA) is supported.
networkInfo_enaSupport :: Lens.Lens' NetworkInfo (Prelude.Maybe EnaSupport)
networkInfo_enaSupport = Lens.lens (\NetworkInfo' {enaSupport} -> enaSupport) (\s@NetworkInfo' {} a -> s {enaSupport = a} :: NetworkInfo)

-- | The maximum number of physical network cards that can be allocated to
-- the instance.
networkInfo_maximumNetworkCards :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Int)
networkInfo_maximumNetworkCards = Lens.lens (\NetworkInfo' {maximumNetworkCards} -> maximumNetworkCards) (\s@NetworkInfo' {} a -> s {maximumNetworkCards = a} :: NetworkInfo)

instance Core.FromXML NetworkInfo where
  parseXML x =
    NetworkInfo'
      Prelude.<$> (x Core..@? "ipv4AddressesPerInterface")
      Prelude.<*> (x Core..@? "ipv6Supported")
      Prelude.<*> (x Core..@? "networkPerformance")
      Prelude.<*> (x Core..@? "efaInfo")
      Prelude.<*> (x Core..@? "defaultNetworkCardIndex")
      Prelude.<*> (x Core..@? "efaSupported")
      Prelude.<*> (x Core..@? "ipv6AddressesPerInterface")
      Prelude.<*> ( x Core..@? "networkCards" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "maximumNetworkInterfaces")
      Prelude.<*> (x Core..@? "encryptionInTransitSupported")
      Prelude.<*> (x Core..@? "enaSupport")
      Prelude.<*> (x Core..@? "maximumNetworkCards")

instance Prelude.Hashable NetworkInfo where
  hashWithSalt _salt NetworkInfo' {..} =
    _salt
      `Prelude.hashWithSalt` ipv4AddressesPerInterface
      `Prelude.hashWithSalt` ipv6Supported
      `Prelude.hashWithSalt` networkPerformance
      `Prelude.hashWithSalt` efaInfo
      `Prelude.hashWithSalt` defaultNetworkCardIndex
      `Prelude.hashWithSalt` efaSupported
      `Prelude.hashWithSalt` ipv6AddressesPerInterface
      `Prelude.hashWithSalt` networkCards
      `Prelude.hashWithSalt` maximumNetworkInterfaces
      `Prelude.hashWithSalt` encryptionInTransitSupported
      `Prelude.hashWithSalt` enaSupport
      `Prelude.hashWithSalt` maximumNetworkCards

instance Prelude.NFData NetworkInfo where
  rnf NetworkInfo' {..} =
    Prelude.rnf ipv4AddressesPerInterface
      `Prelude.seq` Prelude.rnf ipv6Supported
      `Prelude.seq` Prelude.rnf networkPerformance
      `Prelude.seq` Prelude.rnf efaInfo
      `Prelude.seq` Prelude.rnf defaultNetworkCardIndex
      `Prelude.seq` Prelude.rnf efaSupported
      `Prelude.seq` Prelude.rnf ipv6AddressesPerInterface
      `Prelude.seq` Prelude.rnf networkCards
      `Prelude.seq` Prelude.rnf maximumNetworkInterfaces
      `Prelude.seq` Prelude.rnf encryptionInTransitSupported
      `Prelude.seq` Prelude.rnf enaSupport
      `Prelude.seq` Prelude.rnf maximumNetworkCards
