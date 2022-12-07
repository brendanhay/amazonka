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
-- Module      : Amazonka.EC2.Types.NatGatewayAddress
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NatGatewayAddress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the IP addresses and network interface associated with a NAT
-- gateway.
--
-- /See:/ 'newNatGatewayAddress' smart constructor.
data NatGatewayAddress = NatGatewayAddress'
  { -- | [Public NAT gateway only] The allocation ID of the Elastic IP address
    -- that\'s associated with the NAT gateway.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | [Public NAT gateway only] The Elastic IP address associated with the NAT
    -- gateway.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface associated with the NAT gateway.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The private IP address associated with the NAT gateway.
    privateIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NatGatewayAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationId', 'natGatewayAddress_allocationId' - [Public NAT gateway only] The allocation ID of the Elastic IP address
-- that\'s associated with the NAT gateway.
--
-- 'publicIp', 'natGatewayAddress_publicIp' - [Public NAT gateway only] The Elastic IP address associated with the NAT
-- gateway.
--
-- 'networkInterfaceId', 'natGatewayAddress_networkInterfaceId' - The ID of the network interface associated with the NAT gateway.
--
-- 'privateIp', 'natGatewayAddress_privateIp' - The private IP address associated with the NAT gateway.
newNatGatewayAddress ::
  NatGatewayAddress
newNatGatewayAddress =
  NatGatewayAddress'
    { allocationId = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      privateIp = Prelude.Nothing
    }

-- | [Public NAT gateway only] The allocation ID of the Elastic IP address
-- that\'s associated with the NAT gateway.
natGatewayAddress_allocationId :: Lens.Lens' NatGatewayAddress (Prelude.Maybe Prelude.Text)
natGatewayAddress_allocationId = Lens.lens (\NatGatewayAddress' {allocationId} -> allocationId) (\s@NatGatewayAddress' {} a -> s {allocationId = a} :: NatGatewayAddress)

-- | [Public NAT gateway only] The Elastic IP address associated with the NAT
-- gateway.
natGatewayAddress_publicIp :: Lens.Lens' NatGatewayAddress (Prelude.Maybe Prelude.Text)
natGatewayAddress_publicIp = Lens.lens (\NatGatewayAddress' {publicIp} -> publicIp) (\s@NatGatewayAddress' {} a -> s {publicIp = a} :: NatGatewayAddress)

-- | The ID of the network interface associated with the NAT gateway.
natGatewayAddress_networkInterfaceId :: Lens.Lens' NatGatewayAddress (Prelude.Maybe Prelude.Text)
natGatewayAddress_networkInterfaceId = Lens.lens (\NatGatewayAddress' {networkInterfaceId} -> networkInterfaceId) (\s@NatGatewayAddress' {} a -> s {networkInterfaceId = a} :: NatGatewayAddress)

-- | The private IP address associated with the NAT gateway.
natGatewayAddress_privateIp :: Lens.Lens' NatGatewayAddress (Prelude.Maybe Prelude.Text)
natGatewayAddress_privateIp = Lens.lens (\NatGatewayAddress' {privateIp} -> privateIp) (\s@NatGatewayAddress' {} a -> s {privateIp = a} :: NatGatewayAddress)

instance Data.FromXML NatGatewayAddress where
  parseXML x =
    NatGatewayAddress'
      Prelude.<$> (x Data..@? "allocationId")
      Prelude.<*> (x Data..@? "publicIp")
      Prelude.<*> (x Data..@? "networkInterfaceId")
      Prelude.<*> (x Data..@? "privateIp")

instance Prelude.Hashable NatGatewayAddress where
  hashWithSalt _salt NatGatewayAddress' {..} =
    _salt `Prelude.hashWithSalt` allocationId
      `Prelude.hashWithSalt` publicIp
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` privateIp

instance Prelude.NFData NatGatewayAddress where
  rnf NatGatewayAddress' {..} =
    Prelude.rnf allocationId
      `Prelude.seq` Prelude.rnf publicIp
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf privateIp
