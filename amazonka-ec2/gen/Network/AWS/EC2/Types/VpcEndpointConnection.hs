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
-- Module      : Network.AWS.EC2.Types.VpcEndpointConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpcEndpointConnection where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DnsEntry
import Network.AWS.EC2.Types.State
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a VPC endpoint connection to a service.
--
-- /See:/ 'newVpcEndpointConnection' smart constructor.
data VpcEndpointConnection = VpcEndpointConnection'
  { -- | The date and time that the VPC endpoint was created.
    creationTimestamp :: Prelude.Maybe Prelude.ISO8601,
    -- | The AWS account ID of the owner of the VPC endpoint.
    vpcEndpointOwner :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
    -- service.
    gatewayLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The state of the VPC endpoint.
    vpcEndpointState :: Prelude.Maybe State,
    -- | The DNS entries for the VPC endpoint.
    dnsEntries :: Prelude.Maybe [DnsEntry],
    -- | The ID of the VPC endpoint.
    vpcEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service to which the endpoint is connected.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the network load balancers for the
    -- service.
    networkLoadBalancerArns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VpcEndpointConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'vpcEndpointConnection_creationTimestamp' - The date and time that the VPC endpoint was created.
--
-- 'vpcEndpointOwner', 'vpcEndpointConnection_vpcEndpointOwner' - The AWS account ID of the owner of the VPC endpoint.
--
-- 'gatewayLoadBalancerArns', 'vpcEndpointConnection_gatewayLoadBalancerArns' - The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
-- service.
--
-- 'vpcEndpointState', 'vpcEndpointConnection_vpcEndpointState' - The state of the VPC endpoint.
--
-- 'dnsEntries', 'vpcEndpointConnection_dnsEntries' - The DNS entries for the VPC endpoint.
--
-- 'vpcEndpointId', 'vpcEndpointConnection_vpcEndpointId' - The ID of the VPC endpoint.
--
-- 'serviceId', 'vpcEndpointConnection_serviceId' - The ID of the service to which the endpoint is connected.
--
-- 'networkLoadBalancerArns', 'vpcEndpointConnection_networkLoadBalancerArns' - The Amazon Resource Names (ARNs) of the network load balancers for the
-- service.
newVpcEndpointConnection ::
  VpcEndpointConnection
newVpcEndpointConnection =
  VpcEndpointConnection'
    { creationTimestamp =
        Prelude.Nothing,
      vpcEndpointOwner = Prelude.Nothing,
      gatewayLoadBalancerArns = Prelude.Nothing,
      vpcEndpointState = Prelude.Nothing,
      dnsEntries = Prelude.Nothing,
      vpcEndpointId = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      networkLoadBalancerArns = Prelude.Nothing
    }

-- | The date and time that the VPC endpoint was created.
vpcEndpointConnection_creationTimestamp :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe Prelude.UTCTime)
vpcEndpointConnection_creationTimestamp = Lens.lens (\VpcEndpointConnection' {creationTimestamp} -> creationTimestamp) (\s@VpcEndpointConnection' {} a -> s {creationTimestamp = a} :: VpcEndpointConnection) Prelude.. Lens.mapping Prelude._Time

-- | The AWS account ID of the owner of the VPC endpoint.
vpcEndpointConnection_vpcEndpointOwner :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe Prelude.Text)
vpcEndpointConnection_vpcEndpointOwner = Lens.lens (\VpcEndpointConnection' {vpcEndpointOwner} -> vpcEndpointOwner) (\s@VpcEndpointConnection' {} a -> s {vpcEndpointOwner = a} :: VpcEndpointConnection)

-- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
-- service.
vpcEndpointConnection_gatewayLoadBalancerArns :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe [Prelude.Text])
vpcEndpointConnection_gatewayLoadBalancerArns = Lens.lens (\VpcEndpointConnection' {gatewayLoadBalancerArns} -> gatewayLoadBalancerArns) (\s@VpcEndpointConnection' {} a -> s {gatewayLoadBalancerArns = a} :: VpcEndpointConnection) Prelude.. Lens.mapping Prelude._Coerce

-- | The state of the VPC endpoint.
vpcEndpointConnection_vpcEndpointState :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe State)
vpcEndpointConnection_vpcEndpointState = Lens.lens (\VpcEndpointConnection' {vpcEndpointState} -> vpcEndpointState) (\s@VpcEndpointConnection' {} a -> s {vpcEndpointState = a} :: VpcEndpointConnection)

-- | The DNS entries for the VPC endpoint.
vpcEndpointConnection_dnsEntries :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe [DnsEntry])
vpcEndpointConnection_dnsEntries = Lens.lens (\VpcEndpointConnection' {dnsEntries} -> dnsEntries) (\s@VpcEndpointConnection' {} a -> s {dnsEntries = a} :: VpcEndpointConnection) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the VPC endpoint.
vpcEndpointConnection_vpcEndpointId :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe Prelude.Text)
vpcEndpointConnection_vpcEndpointId = Lens.lens (\VpcEndpointConnection' {vpcEndpointId} -> vpcEndpointId) (\s@VpcEndpointConnection' {} a -> s {vpcEndpointId = a} :: VpcEndpointConnection)

-- | The ID of the service to which the endpoint is connected.
vpcEndpointConnection_serviceId :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe Prelude.Text)
vpcEndpointConnection_serviceId = Lens.lens (\VpcEndpointConnection' {serviceId} -> serviceId) (\s@VpcEndpointConnection' {} a -> s {serviceId = a} :: VpcEndpointConnection)

-- | The Amazon Resource Names (ARNs) of the network load balancers for the
-- service.
vpcEndpointConnection_networkLoadBalancerArns :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe [Prelude.Text])
vpcEndpointConnection_networkLoadBalancerArns = Lens.lens (\VpcEndpointConnection' {networkLoadBalancerArns} -> networkLoadBalancerArns) (\s@VpcEndpointConnection' {} a -> s {networkLoadBalancerArns = a} :: VpcEndpointConnection) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML VpcEndpointConnection where
  parseXML x =
    VpcEndpointConnection'
      Prelude.<$> (x Prelude..@? "creationTimestamp")
      Prelude.<*> (x Prelude..@? "vpcEndpointOwner")
      Prelude.<*> ( x Prelude..@? "gatewayLoadBalancerArnSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "vpcEndpointState")
      Prelude.<*> ( x Prelude..@? "dnsEntrySet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "vpcEndpointId")
      Prelude.<*> (x Prelude..@? "serviceId")
      Prelude.<*> ( x Prelude..@? "networkLoadBalancerArnSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable VpcEndpointConnection

instance Prelude.NFData VpcEndpointConnection
