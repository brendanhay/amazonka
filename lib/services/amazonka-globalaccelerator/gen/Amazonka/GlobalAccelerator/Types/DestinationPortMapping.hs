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
-- Module      : Amazonka.GlobalAccelerator.Types.DestinationPortMapping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.DestinationPortMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types.CustomRoutingDestinationTrafficState
import Amazonka.GlobalAccelerator.Types.IpAddressType
import Amazonka.GlobalAccelerator.Types.SocketAddress
import qualified Amazonka.Prelude as Prelude

-- | The port mappings for a specified endpoint IP address (destination).
--
-- /See:/ 'newDestinationPortMapping' smart constructor.
data DestinationPortMapping = DestinationPortMapping'
  { -- | The Amazon Resource Name (ARN) of the custom routing accelerator that
    -- you have port mappings for.
    acceleratorArn :: Prelude.Maybe Prelude.Text,
    -- | The IP address\/port combinations (sockets) that map to a given
    -- destination socket address.
    acceleratorSocketAddresses :: Prelude.Maybe [SocketAddress],
    -- | The endpoint IP address\/port combination for traffic received on the
    -- accelerator socket address.
    destinationSocketAddress :: Prelude.Maybe SocketAddress,
    -- | Indicates whether or not a port mapping destination can receive traffic.
    -- The value is either ALLOW, if traffic is allowed to the destination, or
    -- DENY, if traffic is not allowed to the destination.
    destinationTrafficState :: Prelude.Maybe CustomRoutingDestinationTrafficState,
    -- | The Amazon Resource Name (ARN) of the endpoint group.
    endpointGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region for the endpoint group.
    endpointGroupRegion :: Prelude.Maybe Prelude.Text,
    -- | The ID for the virtual private cloud (VPC) subnet.
    endpointId :: Prelude.Maybe Prelude.Text,
    -- | The IP address type that an accelerator supports. For a custom routing
    -- accelerator, the value must be IPV4.
    ipAddressType :: Prelude.Maybe IpAddressType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationPortMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorArn', 'destinationPortMapping_acceleratorArn' - The Amazon Resource Name (ARN) of the custom routing accelerator that
-- you have port mappings for.
--
-- 'acceleratorSocketAddresses', 'destinationPortMapping_acceleratorSocketAddresses' - The IP address\/port combinations (sockets) that map to a given
-- destination socket address.
--
-- 'destinationSocketAddress', 'destinationPortMapping_destinationSocketAddress' - The endpoint IP address\/port combination for traffic received on the
-- accelerator socket address.
--
-- 'destinationTrafficState', 'destinationPortMapping_destinationTrafficState' - Indicates whether or not a port mapping destination can receive traffic.
-- The value is either ALLOW, if traffic is allowed to the destination, or
-- DENY, if traffic is not allowed to the destination.
--
-- 'endpointGroupArn', 'destinationPortMapping_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group.
--
-- 'endpointGroupRegion', 'destinationPortMapping_endpointGroupRegion' - The Amazon Web Services Region for the endpoint group.
--
-- 'endpointId', 'destinationPortMapping_endpointId' - The ID for the virtual private cloud (VPC) subnet.
--
-- 'ipAddressType', 'destinationPortMapping_ipAddressType' - The IP address type that an accelerator supports. For a custom routing
-- accelerator, the value must be IPV4.
newDestinationPortMapping ::
  DestinationPortMapping
newDestinationPortMapping =
  DestinationPortMapping'
    { acceleratorArn =
        Prelude.Nothing,
      acceleratorSocketAddresses = Prelude.Nothing,
      destinationSocketAddress = Prelude.Nothing,
      destinationTrafficState = Prelude.Nothing,
      endpointGroupArn = Prelude.Nothing,
      endpointGroupRegion = Prelude.Nothing,
      endpointId = Prelude.Nothing,
      ipAddressType = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the custom routing accelerator that
-- you have port mappings for.
destinationPortMapping_acceleratorArn :: Lens.Lens' DestinationPortMapping (Prelude.Maybe Prelude.Text)
destinationPortMapping_acceleratorArn = Lens.lens (\DestinationPortMapping' {acceleratorArn} -> acceleratorArn) (\s@DestinationPortMapping' {} a -> s {acceleratorArn = a} :: DestinationPortMapping)

-- | The IP address\/port combinations (sockets) that map to a given
-- destination socket address.
destinationPortMapping_acceleratorSocketAddresses :: Lens.Lens' DestinationPortMapping (Prelude.Maybe [SocketAddress])
destinationPortMapping_acceleratorSocketAddresses = Lens.lens (\DestinationPortMapping' {acceleratorSocketAddresses} -> acceleratorSocketAddresses) (\s@DestinationPortMapping' {} a -> s {acceleratorSocketAddresses = a} :: DestinationPortMapping) Prelude.. Lens.mapping Lens.coerced

-- | The endpoint IP address\/port combination for traffic received on the
-- accelerator socket address.
destinationPortMapping_destinationSocketAddress :: Lens.Lens' DestinationPortMapping (Prelude.Maybe SocketAddress)
destinationPortMapping_destinationSocketAddress = Lens.lens (\DestinationPortMapping' {destinationSocketAddress} -> destinationSocketAddress) (\s@DestinationPortMapping' {} a -> s {destinationSocketAddress = a} :: DestinationPortMapping)

-- | Indicates whether or not a port mapping destination can receive traffic.
-- The value is either ALLOW, if traffic is allowed to the destination, or
-- DENY, if traffic is not allowed to the destination.
destinationPortMapping_destinationTrafficState :: Lens.Lens' DestinationPortMapping (Prelude.Maybe CustomRoutingDestinationTrafficState)
destinationPortMapping_destinationTrafficState = Lens.lens (\DestinationPortMapping' {destinationTrafficState} -> destinationTrafficState) (\s@DestinationPortMapping' {} a -> s {destinationTrafficState = a} :: DestinationPortMapping)

-- | The Amazon Resource Name (ARN) of the endpoint group.
destinationPortMapping_endpointGroupArn :: Lens.Lens' DestinationPortMapping (Prelude.Maybe Prelude.Text)
destinationPortMapping_endpointGroupArn = Lens.lens (\DestinationPortMapping' {endpointGroupArn} -> endpointGroupArn) (\s@DestinationPortMapping' {} a -> s {endpointGroupArn = a} :: DestinationPortMapping)

-- | The Amazon Web Services Region for the endpoint group.
destinationPortMapping_endpointGroupRegion :: Lens.Lens' DestinationPortMapping (Prelude.Maybe Prelude.Text)
destinationPortMapping_endpointGroupRegion = Lens.lens (\DestinationPortMapping' {endpointGroupRegion} -> endpointGroupRegion) (\s@DestinationPortMapping' {} a -> s {endpointGroupRegion = a} :: DestinationPortMapping)

-- | The ID for the virtual private cloud (VPC) subnet.
destinationPortMapping_endpointId :: Lens.Lens' DestinationPortMapping (Prelude.Maybe Prelude.Text)
destinationPortMapping_endpointId = Lens.lens (\DestinationPortMapping' {endpointId} -> endpointId) (\s@DestinationPortMapping' {} a -> s {endpointId = a} :: DestinationPortMapping)

-- | The IP address type that an accelerator supports. For a custom routing
-- accelerator, the value must be IPV4.
destinationPortMapping_ipAddressType :: Lens.Lens' DestinationPortMapping (Prelude.Maybe IpAddressType)
destinationPortMapping_ipAddressType = Lens.lens (\DestinationPortMapping' {ipAddressType} -> ipAddressType) (\s@DestinationPortMapping' {} a -> s {ipAddressType = a} :: DestinationPortMapping)

instance Data.FromJSON DestinationPortMapping where
  parseJSON =
    Data.withObject
      "DestinationPortMapping"
      ( \x ->
          DestinationPortMapping'
            Prelude.<$> (x Data..:? "AcceleratorArn")
            Prelude.<*> ( x Data..:? "AcceleratorSocketAddresses"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DestinationSocketAddress")
            Prelude.<*> (x Data..:? "DestinationTrafficState")
            Prelude.<*> (x Data..:? "EndpointGroupArn")
            Prelude.<*> (x Data..:? "EndpointGroupRegion")
            Prelude.<*> (x Data..:? "EndpointId")
            Prelude.<*> (x Data..:? "IpAddressType")
      )

instance Prelude.Hashable DestinationPortMapping where
  hashWithSalt _salt DestinationPortMapping' {..} =
    _salt `Prelude.hashWithSalt` acceleratorArn
      `Prelude.hashWithSalt` acceleratorSocketAddresses
      `Prelude.hashWithSalt` destinationSocketAddress
      `Prelude.hashWithSalt` destinationTrafficState
      `Prelude.hashWithSalt` endpointGroupArn
      `Prelude.hashWithSalt` endpointGroupRegion
      `Prelude.hashWithSalt` endpointId
      `Prelude.hashWithSalt` ipAddressType

instance Prelude.NFData DestinationPortMapping where
  rnf DestinationPortMapping' {..} =
    Prelude.rnf acceleratorArn
      `Prelude.seq` Prelude.rnf acceleratorSocketAddresses
      `Prelude.seq` Prelude.rnf destinationSocketAddress
      `Prelude.seq` Prelude.rnf destinationTrafficState
      `Prelude.seq` Prelude.rnf endpointGroupArn
      `Prelude.seq` Prelude.rnf endpointGroupRegion
      `Prelude.seq` Prelude.rnf endpointId
      `Prelude.seq` Prelude.rnf ipAddressType
