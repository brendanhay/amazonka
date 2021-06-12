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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes association information for an Elastic IP address (IPv4).
--
-- /See:/ 'newInstanceNetworkInterfaceAssociation' smart constructor.
data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation'
  { -- | The ID of the owner of the Elastic IP address.
    ipOwnerId :: Core.Maybe Core.Text,
    -- | The carrier IP address associated with the network interface.
    carrierIp :: Core.Maybe Core.Text,
    -- | The public DNS name.
    publicDnsName :: Core.Maybe Core.Text,
    -- | The public IP address or Elastic IP address bound to the network
    -- interface.
    publicIp :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      carrierIp = Core.Nothing,
      publicDnsName = Core.Nothing,
      publicIp = Core.Nothing
    }

-- | The ID of the owner of the Elastic IP address.
instanceNetworkInterfaceAssociation_ipOwnerId :: Lens.Lens' InstanceNetworkInterfaceAssociation (Core.Maybe Core.Text)
instanceNetworkInterfaceAssociation_ipOwnerId = Lens.lens (\InstanceNetworkInterfaceAssociation' {ipOwnerId} -> ipOwnerId) (\s@InstanceNetworkInterfaceAssociation' {} a -> s {ipOwnerId = a} :: InstanceNetworkInterfaceAssociation)

-- | The carrier IP address associated with the network interface.
instanceNetworkInterfaceAssociation_carrierIp :: Lens.Lens' InstanceNetworkInterfaceAssociation (Core.Maybe Core.Text)
instanceNetworkInterfaceAssociation_carrierIp = Lens.lens (\InstanceNetworkInterfaceAssociation' {carrierIp} -> carrierIp) (\s@InstanceNetworkInterfaceAssociation' {} a -> s {carrierIp = a} :: InstanceNetworkInterfaceAssociation)

-- | The public DNS name.
instanceNetworkInterfaceAssociation_publicDnsName :: Lens.Lens' InstanceNetworkInterfaceAssociation (Core.Maybe Core.Text)
instanceNetworkInterfaceAssociation_publicDnsName = Lens.lens (\InstanceNetworkInterfaceAssociation' {publicDnsName} -> publicDnsName) (\s@InstanceNetworkInterfaceAssociation' {} a -> s {publicDnsName = a} :: InstanceNetworkInterfaceAssociation)

-- | The public IP address or Elastic IP address bound to the network
-- interface.
instanceNetworkInterfaceAssociation_publicIp :: Lens.Lens' InstanceNetworkInterfaceAssociation (Core.Maybe Core.Text)
instanceNetworkInterfaceAssociation_publicIp = Lens.lens (\InstanceNetworkInterfaceAssociation' {publicIp} -> publicIp) (\s@InstanceNetworkInterfaceAssociation' {} a -> s {publicIp = a} :: InstanceNetworkInterfaceAssociation)

instance
  Core.FromXML
    InstanceNetworkInterfaceAssociation
  where
  parseXML x =
    InstanceNetworkInterfaceAssociation'
      Core.<$> (x Core..@? "ipOwnerId")
      Core.<*> (x Core..@? "carrierIp")
      Core.<*> (x Core..@? "publicDnsName")
      Core.<*> (x Core..@? "publicIp")

instance
  Core.Hashable
    InstanceNetworkInterfaceAssociation

instance
  Core.NFData
    InstanceNetworkInterfaceAssociation
