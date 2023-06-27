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
-- Module      : Amazonka.EC2.Types.InstanceNetworkInterfaceAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceNetworkInterfaceAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes association information for an Elastic IP address (IPv4).
--
-- /See:/ 'newInstanceNetworkInterfaceAssociation' smart constructor.
data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation'
  { -- | The carrier IP address associated with the network interface.
    carrierIp :: Prelude.Maybe Prelude.Text,
    -- | The customer-owned IP address associated with the network interface.
    customerOwnedIp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the owner of the Elastic IP address.
    ipOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The public DNS name.
    publicDnsName :: Prelude.Maybe Prelude.Text,
    -- | The public IP address or Elastic IP address bound to the network
    -- interface.
    publicIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceNetworkInterfaceAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'carrierIp', 'instanceNetworkInterfaceAssociation_carrierIp' - The carrier IP address associated with the network interface.
--
-- 'customerOwnedIp', 'instanceNetworkInterfaceAssociation_customerOwnedIp' - The customer-owned IP address associated with the network interface.
--
-- 'ipOwnerId', 'instanceNetworkInterfaceAssociation_ipOwnerId' - The ID of the owner of the Elastic IP address.
--
-- 'publicDnsName', 'instanceNetworkInterfaceAssociation_publicDnsName' - The public DNS name.
--
-- 'publicIp', 'instanceNetworkInterfaceAssociation_publicIp' - The public IP address or Elastic IP address bound to the network
-- interface.
newInstanceNetworkInterfaceAssociation ::
  InstanceNetworkInterfaceAssociation
newInstanceNetworkInterfaceAssociation =
  InstanceNetworkInterfaceAssociation'
    { carrierIp =
        Prelude.Nothing,
      customerOwnedIp = Prelude.Nothing,
      ipOwnerId = Prelude.Nothing,
      publicDnsName = Prelude.Nothing,
      publicIp = Prelude.Nothing
    }

-- | The carrier IP address associated with the network interface.
instanceNetworkInterfaceAssociation_carrierIp :: Lens.Lens' InstanceNetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceAssociation_carrierIp = Lens.lens (\InstanceNetworkInterfaceAssociation' {carrierIp} -> carrierIp) (\s@InstanceNetworkInterfaceAssociation' {} a -> s {carrierIp = a} :: InstanceNetworkInterfaceAssociation)

-- | The customer-owned IP address associated with the network interface.
instanceNetworkInterfaceAssociation_customerOwnedIp :: Lens.Lens' InstanceNetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceAssociation_customerOwnedIp = Lens.lens (\InstanceNetworkInterfaceAssociation' {customerOwnedIp} -> customerOwnedIp) (\s@InstanceNetworkInterfaceAssociation' {} a -> s {customerOwnedIp = a} :: InstanceNetworkInterfaceAssociation)

-- | The ID of the owner of the Elastic IP address.
instanceNetworkInterfaceAssociation_ipOwnerId :: Lens.Lens' InstanceNetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceAssociation_ipOwnerId = Lens.lens (\InstanceNetworkInterfaceAssociation' {ipOwnerId} -> ipOwnerId) (\s@InstanceNetworkInterfaceAssociation' {} a -> s {ipOwnerId = a} :: InstanceNetworkInterfaceAssociation)

-- | The public DNS name.
instanceNetworkInterfaceAssociation_publicDnsName :: Lens.Lens' InstanceNetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceAssociation_publicDnsName = Lens.lens (\InstanceNetworkInterfaceAssociation' {publicDnsName} -> publicDnsName) (\s@InstanceNetworkInterfaceAssociation' {} a -> s {publicDnsName = a} :: InstanceNetworkInterfaceAssociation)

-- | The public IP address or Elastic IP address bound to the network
-- interface.
instanceNetworkInterfaceAssociation_publicIp :: Lens.Lens' InstanceNetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceAssociation_publicIp = Lens.lens (\InstanceNetworkInterfaceAssociation' {publicIp} -> publicIp) (\s@InstanceNetworkInterfaceAssociation' {} a -> s {publicIp = a} :: InstanceNetworkInterfaceAssociation)

instance
  Data.FromXML
    InstanceNetworkInterfaceAssociation
  where
  parseXML x =
    InstanceNetworkInterfaceAssociation'
      Prelude.<$> (x Data..@? "carrierIp")
      Prelude.<*> (x Data..@? "customerOwnedIp")
      Prelude.<*> (x Data..@? "ipOwnerId")
      Prelude.<*> (x Data..@? "publicDnsName")
      Prelude.<*> (x Data..@? "publicIp")

instance
  Prelude.Hashable
    InstanceNetworkInterfaceAssociation
  where
  hashWithSalt
    _salt
    InstanceNetworkInterfaceAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` carrierIp
        `Prelude.hashWithSalt` customerOwnedIp
        `Prelude.hashWithSalt` ipOwnerId
        `Prelude.hashWithSalt` publicDnsName
        `Prelude.hashWithSalt` publicIp

instance
  Prelude.NFData
    InstanceNetworkInterfaceAssociation
  where
  rnf InstanceNetworkInterfaceAssociation' {..} =
    Prelude.rnf carrierIp
      `Prelude.seq` Prelude.rnf customerOwnedIp
      `Prelude.seq` Prelude.rnf ipOwnerId
      `Prelude.seq` Prelude.rnf publicDnsName
      `Prelude.seq` Prelude.rnf publicIp
