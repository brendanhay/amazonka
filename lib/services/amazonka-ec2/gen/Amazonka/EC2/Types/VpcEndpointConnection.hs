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
-- Module      : Amazonka.EC2.Types.VpcEndpointConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpcEndpointConnection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DnsEntry
import Amazonka.EC2.Types.IpAddressType
import Amazonka.EC2.Types.State
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a VPC endpoint connection to a service.
--
-- /See:/ 'newVpcEndpointConnection' smart constructor.
data VpcEndpointConnection = VpcEndpointConnection'
  { -- | The date and time that the VPC endpoint was created.
    creationTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The DNS entries for the VPC endpoint.
    dnsEntries :: Prelude.Maybe [DnsEntry],
    -- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
    -- service.
    gatewayLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The IP address type for the endpoint.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | The Amazon Resource Names (ARNs) of the network load balancers for the
    -- service.
    networkLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the service to which the endpoint is connected.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The tags.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the VPC endpoint connection.
    vpcEndpointConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC endpoint.
    vpcEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the VPC endpoint.
    vpcEndpointOwner :: Prelude.Maybe Prelude.Text,
    -- | The state of the VPC endpoint.
    vpcEndpointState :: Prelude.Maybe State
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'dnsEntries', 'vpcEndpointConnection_dnsEntries' - The DNS entries for the VPC endpoint.
--
-- 'gatewayLoadBalancerArns', 'vpcEndpointConnection_gatewayLoadBalancerArns' - The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
-- service.
--
-- 'ipAddressType', 'vpcEndpointConnection_ipAddressType' - The IP address type for the endpoint.
--
-- 'networkLoadBalancerArns', 'vpcEndpointConnection_networkLoadBalancerArns' - The Amazon Resource Names (ARNs) of the network load balancers for the
-- service.
--
-- 'serviceId', 'vpcEndpointConnection_serviceId' - The ID of the service to which the endpoint is connected.
--
-- 'tags', 'vpcEndpointConnection_tags' - The tags.
--
-- 'vpcEndpointConnectionId', 'vpcEndpointConnection_vpcEndpointConnectionId' - The ID of the VPC endpoint connection.
--
-- 'vpcEndpointId', 'vpcEndpointConnection_vpcEndpointId' - The ID of the VPC endpoint.
--
-- 'vpcEndpointOwner', 'vpcEndpointConnection_vpcEndpointOwner' - The ID of the Amazon Web Services account that owns the VPC endpoint.
--
-- 'vpcEndpointState', 'vpcEndpointConnection_vpcEndpointState' - The state of the VPC endpoint.
newVpcEndpointConnection ::
  VpcEndpointConnection
newVpcEndpointConnection =
  VpcEndpointConnection'
    { creationTimestamp =
        Prelude.Nothing,
      dnsEntries = Prelude.Nothing,
      gatewayLoadBalancerArns = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      networkLoadBalancerArns = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcEndpointConnectionId = Prelude.Nothing,
      vpcEndpointId = Prelude.Nothing,
      vpcEndpointOwner = Prelude.Nothing,
      vpcEndpointState = Prelude.Nothing
    }

-- | The date and time that the VPC endpoint was created.
vpcEndpointConnection_creationTimestamp :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe Prelude.UTCTime)
vpcEndpointConnection_creationTimestamp = Lens.lens (\VpcEndpointConnection' {creationTimestamp} -> creationTimestamp) (\s@VpcEndpointConnection' {} a -> s {creationTimestamp = a} :: VpcEndpointConnection) Prelude.. Lens.mapping Data._Time

-- | The DNS entries for the VPC endpoint.
vpcEndpointConnection_dnsEntries :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe [DnsEntry])
vpcEndpointConnection_dnsEntries = Lens.lens (\VpcEndpointConnection' {dnsEntries} -> dnsEntries) (\s@VpcEndpointConnection' {} a -> s {dnsEntries = a} :: VpcEndpointConnection) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
-- service.
vpcEndpointConnection_gatewayLoadBalancerArns :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe [Prelude.Text])
vpcEndpointConnection_gatewayLoadBalancerArns = Lens.lens (\VpcEndpointConnection' {gatewayLoadBalancerArns} -> gatewayLoadBalancerArns) (\s@VpcEndpointConnection' {} a -> s {gatewayLoadBalancerArns = a} :: VpcEndpointConnection) Prelude.. Lens.mapping Lens.coerced

-- | The IP address type for the endpoint.
vpcEndpointConnection_ipAddressType :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe IpAddressType)
vpcEndpointConnection_ipAddressType = Lens.lens (\VpcEndpointConnection' {ipAddressType} -> ipAddressType) (\s@VpcEndpointConnection' {} a -> s {ipAddressType = a} :: VpcEndpointConnection)

-- | The Amazon Resource Names (ARNs) of the network load balancers for the
-- service.
vpcEndpointConnection_networkLoadBalancerArns :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe [Prelude.Text])
vpcEndpointConnection_networkLoadBalancerArns = Lens.lens (\VpcEndpointConnection' {networkLoadBalancerArns} -> networkLoadBalancerArns) (\s@VpcEndpointConnection' {} a -> s {networkLoadBalancerArns = a} :: VpcEndpointConnection) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the service to which the endpoint is connected.
vpcEndpointConnection_serviceId :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe Prelude.Text)
vpcEndpointConnection_serviceId = Lens.lens (\VpcEndpointConnection' {serviceId} -> serviceId) (\s@VpcEndpointConnection' {} a -> s {serviceId = a} :: VpcEndpointConnection)

-- | The tags.
vpcEndpointConnection_tags :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe [Tag])
vpcEndpointConnection_tags = Lens.lens (\VpcEndpointConnection' {tags} -> tags) (\s@VpcEndpointConnection' {} a -> s {tags = a} :: VpcEndpointConnection) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC endpoint connection.
vpcEndpointConnection_vpcEndpointConnectionId :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe Prelude.Text)
vpcEndpointConnection_vpcEndpointConnectionId = Lens.lens (\VpcEndpointConnection' {vpcEndpointConnectionId} -> vpcEndpointConnectionId) (\s@VpcEndpointConnection' {} a -> s {vpcEndpointConnectionId = a} :: VpcEndpointConnection)

-- | The ID of the VPC endpoint.
vpcEndpointConnection_vpcEndpointId :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe Prelude.Text)
vpcEndpointConnection_vpcEndpointId = Lens.lens (\VpcEndpointConnection' {vpcEndpointId} -> vpcEndpointId) (\s@VpcEndpointConnection' {} a -> s {vpcEndpointId = a} :: VpcEndpointConnection)

-- | The ID of the Amazon Web Services account that owns the VPC endpoint.
vpcEndpointConnection_vpcEndpointOwner :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe Prelude.Text)
vpcEndpointConnection_vpcEndpointOwner = Lens.lens (\VpcEndpointConnection' {vpcEndpointOwner} -> vpcEndpointOwner) (\s@VpcEndpointConnection' {} a -> s {vpcEndpointOwner = a} :: VpcEndpointConnection)

-- | The state of the VPC endpoint.
vpcEndpointConnection_vpcEndpointState :: Lens.Lens' VpcEndpointConnection (Prelude.Maybe State)
vpcEndpointConnection_vpcEndpointState = Lens.lens (\VpcEndpointConnection' {vpcEndpointState} -> vpcEndpointState) (\s@VpcEndpointConnection' {} a -> s {vpcEndpointState = a} :: VpcEndpointConnection)

instance Data.FromXML VpcEndpointConnection where
  parseXML x =
    VpcEndpointConnection'
      Prelude.<$> (x Data..@? "creationTimestamp")
      Prelude.<*> ( x Data..@? "dnsEntrySet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "gatewayLoadBalancerArnSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ipAddressType")
      Prelude.<*> ( x
                      Data..@? "networkLoadBalancerArnSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "serviceId")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "vpcEndpointConnectionId")
      Prelude.<*> (x Data..@? "vpcEndpointId")
      Prelude.<*> (x Data..@? "vpcEndpointOwner")
      Prelude.<*> (x Data..@? "vpcEndpointState")

instance Prelude.Hashable VpcEndpointConnection where
  hashWithSalt _salt VpcEndpointConnection' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` dnsEntries
      `Prelude.hashWithSalt` gatewayLoadBalancerArns
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` networkLoadBalancerArns
      `Prelude.hashWithSalt` serviceId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcEndpointConnectionId
      `Prelude.hashWithSalt` vpcEndpointId
      `Prelude.hashWithSalt` vpcEndpointOwner
      `Prelude.hashWithSalt` vpcEndpointState

instance Prelude.NFData VpcEndpointConnection where
  rnf VpcEndpointConnection' {..} =
    Prelude.rnf creationTimestamp `Prelude.seq`
      Prelude.rnf dnsEntries `Prelude.seq`
        Prelude.rnf gatewayLoadBalancerArns `Prelude.seq`
          Prelude.rnf ipAddressType `Prelude.seq`
            Prelude.rnf networkLoadBalancerArns `Prelude.seq`
              Prelude.rnf serviceId `Prelude.seq`
                Prelude.rnf tags `Prelude.seq`
                  Prelude.rnf vpcEndpointConnectionId `Prelude.seq`
                    Prelude.rnf vpcEndpointId `Prelude.seq`
                      Prelude.rnf vpcEndpointOwner `Prelude.seq`
                        Prelude.rnf vpcEndpointState
