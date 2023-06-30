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
-- Module      : Amazonka.GlobalAccelerator.Types.PortMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.PortMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types.CustomRoutingDestinationTrafficState
import Amazonka.GlobalAccelerator.Types.CustomRoutingProtocol
import Amazonka.GlobalAccelerator.Types.SocketAddress
import qualified Amazonka.Prelude as Prelude

-- | Returns the ports and associated IP addresses and ports of Amazon EC2
-- instances in your virtual private cloud (VPC) subnets. Custom routing is
-- a port mapping protocol in Global Accelerator that statically associates
-- port ranges with VPC subnets, which allows Global Accelerator to route
-- to specific instances and ports within one or more subnets.
--
-- /See:/ 'newPortMapping' smart constructor.
data PortMapping = PortMapping'
  { -- | The accelerator port.
    acceleratorPort :: Prelude.Maybe Prelude.Natural,
    -- | The EC2 instance IP address and port number in the virtual private cloud
    -- (VPC) subnet.
    destinationSocketAddress :: Prelude.Maybe SocketAddress,
    -- | Indicates whether or not a port mapping destination can receive traffic.
    -- The value is either ALLOW, if traffic is allowed to the destination, or
    -- DENY, if traffic is not allowed to the destination.
    destinationTrafficState :: Prelude.Maybe CustomRoutingDestinationTrafficState,
    -- | The Amazon Resource Name (ARN) of the endpoint group.
    endpointGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the VPC subnet (the subnet ID).
    endpointId :: Prelude.Maybe Prelude.Text,
    -- | The protocols supported by the endpoint group.
    protocols :: Prelude.Maybe (Prelude.NonEmpty CustomRoutingProtocol)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorPort', 'portMapping_acceleratorPort' - The accelerator port.
--
-- 'destinationSocketAddress', 'portMapping_destinationSocketAddress' - The EC2 instance IP address and port number in the virtual private cloud
-- (VPC) subnet.
--
-- 'destinationTrafficState', 'portMapping_destinationTrafficState' - Indicates whether or not a port mapping destination can receive traffic.
-- The value is either ALLOW, if traffic is allowed to the destination, or
-- DENY, if traffic is not allowed to the destination.
--
-- 'endpointGroupArn', 'portMapping_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group.
--
-- 'endpointId', 'portMapping_endpointId' - The IP address of the VPC subnet (the subnet ID).
--
-- 'protocols', 'portMapping_protocols' - The protocols supported by the endpoint group.
newPortMapping ::
  PortMapping
newPortMapping =
  PortMapping'
    { acceleratorPort = Prelude.Nothing,
      destinationSocketAddress = Prelude.Nothing,
      destinationTrafficState = Prelude.Nothing,
      endpointGroupArn = Prelude.Nothing,
      endpointId = Prelude.Nothing,
      protocols = Prelude.Nothing
    }

-- | The accelerator port.
portMapping_acceleratorPort :: Lens.Lens' PortMapping (Prelude.Maybe Prelude.Natural)
portMapping_acceleratorPort = Lens.lens (\PortMapping' {acceleratorPort} -> acceleratorPort) (\s@PortMapping' {} a -> s {acceleratorPort = a} :: PortMapping)

-- | The EC2 instance IP address and port number in the virtual private cloud
-- (VPC) subnet.
portMapping_destinationSocketAddress :: Lens.Lens' PortMapping (Prelude.Maybe SocketAddress)
portMapping_destinationSocketAddress = Lens.lens (\PortMapping' {destinationSocketAddress} -> destinationSocketAddress) (\s@PortMapping' {} a -> s {destinationSocketAddress = a} :: PortMapping)

-- | Indicates whether or not a port mapping destination can receive traffic.
-- The value is either ALLOW, if traffic is allowed to the destination, or
-- DENY, if traffic is not allowed to the destination.
portMapping_destinationTrafficState :: Lens.Lens' PortMapping (Prelude.Maybe CustomRoutingDestinationTrafficState)
portMapping_destinationTrafficState = Lens.lens (\PortMapping' {destinationTrafficState} -> destinationTrafficState) (\s@PortMapping' {} a -> s {destinationTrafficState = a} :: PortMapping)

-- | The Amazon Resource Name (ARN) of the endpoint group.
portMapping_endpointGroupArn :: Lens.Lens' PortMapping (Prelude.Maybe Prelude.Text)
portMapping_endpointGroupArn = Lens.lens (\PortMapping' {endpointGroupArn} -> endpointGroupArn) (\s@PortMapping' {} a -> s {endpointGroupArn = a} :: PortMapping)

-- | The IP address of the VPC subnet (the subnet ID).
portMapping_endpointId :: Lens.Lens' PortMapping (Prelude.Maybe Prelude.Text)
portMapping_endpointId = Lens.lens (\PortMapping' {endpointId} -> endpointId) (\s@PortMapping' {} a -> s {endpointId = a} :: PortMapping)

-- | The protocols supported by the endpoint group.
portMapping_protocols :: Lens.Lens' PortMapping (Prelude.Maybe (Prelude.NonEmpty CustomRoutingProtocol))
portMapping_protocols = Lens.lens (\PortMapping' {protocols} -> protocols) (\s@PortMapping' {} a -> s {protocols = a} :: PortMapping) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PortMapping where
  parseJSON =
    Data.withObject
      "PortMapping"
      ( \x ->
          PortMapping'
            Prelude.<$> (x Data..:? "AcceleratorPort")
            Prelude.<*> (x Data..:? "DestinationSocketAddress")
            Prelude.<*> (x Data..:? "DestinationTrafficState")
            Prelude.<*> (x Data..:? "EndpointGroupArn")
            Prelude.<*> (x Data..:? "EndpointId")
            Prelude.<*> (x Data..:? "Protocols")
      )

instance Prelude.Hashable PortMapping where
  hashWithSalt _salt PortMapping' {..} =
    _salt
      `Prelude.hashWithSalt` acceleratorPort
      `Prelude.hashWithSalt` destinationSocketAddress
      `Prelude.hashWithSalt` destinationTrafficState
      `Prelude.hashWithSalt` endpointGroupArn
      `Prelude.hashWithSalt` endpointId
      `Prelude.hashWithSalt` protocols

instance Prelude.NFData PortMapping where
  rnf PortMapping' {..} =
    Prelude.rnf acceleratorPort
      `Prelude.seq` Prelude.rnf destinationSocketAddress
      `Prelude.seq` Prelude.rnf destinationTrafficState
      `Prelude.seq` Prelude.rnf endpointGroupArn
      `Prelude.seq` Prelude.rnf endpointId
      `Prelude.seq` Prelude.rnf protocols
