{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes association information for an Elastic IP address (IPv4).
--
-- /See:/ 'newInstanceNetworkInterfaceAssociation' smart constructor.
data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation'
  { -- | The ID of the owner of the Elastic IP address.
    ipOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The carrier IP address associated with the network interface.
    carrierIp :: Prelude.Maybe Prelude.Text,
    -- | The public DNS name.
    publicDnsName :: Prelude.Maybe Prelude.Text,
    -- | The public IP address or Elastic IP address bound to the network
    -- interface.
    publicIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceNetworkInterfaceAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipOwnerId', 'instanceNetworkInterfaceAssociation_ipOwnerId' - The ID of the owner of the Elastic IP address.
--
-- 'carrierIp', 'instanceNetworkInterfaceAssociation_carrierIp' - The carrier IP address associated with the network interface.
--
-- 'publicDnsName', 'instanceNetworkInterfaceAssociation_publicDnsName' - The public DNS name.
--
-- 'publicIp', 'instanceNetworkInterfaceAssociation_publicIp' - The public IP address or Elastic IP address bound to the network
-- interface.
newInstanceNetworkInterfaceAssociation ::
  InstanceNetworkInterfaceAssociation
newInstanceNetworkInterfaceAssociation =
  InstanceNetworkInterfaceAssociation'
    { ipOwnerId =
        Prelude.Nothing,
      carrierIp = Prelude.Nothing,
      publicDnsName = Prelude.Nothing,
      publicIp = Prelude.Nothing
    }

-- | The ID of the owner of the Elastic IP address.
instanceNetworkInterfaceAssociation_ipOwnerId :: Lens.Lens' InstanceNetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceAssociation_ipOwnerId = Lens.lens (\InstanceNetworkInterfaceAssociation' {ipOwnerId} -> ipOwnerId) (\s@InstanceNetworkInterfaceAssociation' {} a -> s {ipOwnerId = a} :: InstanceNetworkInterfaceAssociation)

-- | The carrier IP address associated with the network interface.
instanceNetworkInterfaceAssociation_carrierIp :: Lens.Lens' InstanceNetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceAssociation_carrierIp = Lens.lens (\InstanceNetworkInterfaceAssociation' {carrierIp} -> carrierIp) (\s@InstanceNetworkInterfaceAssociation' {} a -> s {carrierIp = a} :: InstanceNetworkInterfaceAssociation)

-- | The public DNS name.
instanceNetworkInterfaceAssociation_publicDnsName :: Lens.Lens' InstanceNetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceAssociation_publicDnsName = Lens.lens (\InstanceNetworkInterfaceAssociation' {publicDnsName} -> publicDnsName) (\s@InstanceNetworkInterfaceAssociation' {} a -> s {publicDnsName = a} :: InstanceNetworkInterfaceAssociation)

-- | The public IP address or Elastic IP address bound to the network
-- interface.
instanceNetworkInterfaceAssociation_publicIp :: Lens.Lens' InstanceNetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceAssociation_publicIp = Lens.lens (\InstanceNetworkInterfaceAssociation' {publicIp} -> publicIp) (\s@InstanceNetworkInterfaceAssociation' {} a -> s {publicIp = a} :: InstanceNetworkInterfaceAssociation)

instance
  Prelude.FromXML
    InstanceNetworkInterfaceAssociation
  where
  parseXML x =
    InstanceNetworkInterfaceAssociation'
      Prelude.<$> (x Prelude..@? "ipOwnerId")
      Prelude.<*> (x Prelude..@? "carrierIp")
      Prelude.<*> (x Prelude..@? "publicDnsName")
      Prelude.<*> (x Prelude..@? "publicIp")

instance
  Prelude.Hashable
    InstanceNetworkInterfaceAssociation

instance
  Prelude.NFData
    InstanceNetworkInterfaceAssociation
