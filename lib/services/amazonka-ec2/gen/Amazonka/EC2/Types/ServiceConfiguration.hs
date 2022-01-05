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
-- Module      : Amazonka.EC2.Types.ServiceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ServiceConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PrivateDnsNameConfiguration
import Amazonka.EC2.Types.ServiceState
import Amazonka.EC2.Types.ServiceTypeDetail
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a service configuration for a VPC endpoint service.
--
-- /See:/ 'newServiceConfiguration' smart constructor.
data ServiceConfiguration = ServiceConfiguration'
  { -- | The Amazon Resource Names (ARNs) of the Network Load Balancers for the
    -- service.
    networkLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The DNS names for the service.
    baseEndpointDnsNames :: Prelude.Maybe [Prelude.Text],
    -- | The Availability Zones in which the service is available.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
    -- service.
    gatewayLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether the service manages its VPC endpoints. Management of
    -- the service VPC endpoints using the VPC endpoint API is restricted.
    managesVpcEndpoints :: Prelude.Maybe Prelude.Bool,
    -- | The name of the service.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The service state.
    serviceState :: Prelude.Maybe ServiceState,
    -- | The type of service.
    serviceType :: Prelude.Maybe [ServiceTypeDetail],
    -- | Indicates whether requests from other Amazon Web Services accounts to
    -- create an endpoint to the service must first be accepted.
    acceptanceRequired :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The private DNS name for the service.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | Information about the endpoint service private DNS name configuration.
    privateDnsNameConfiguration :: Prelude.Maybe PrivateDnsNameConfiguration,
    -- | Any tags assigned to the service.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkLoadBalancerArns', 'serviceConfiguration_networkLoadBalancerArns' - The Amazon Resource Names (ARNs) of the Network Load Balancers for the
-- service.
--
-- 'baseEndpointDnsNames', 'serviceConfiguration_baseEndpointDnsNames' - The DNS names for the service.
--
-- 'availabilityZones', 'serviceConfiguration_availabilityZones' - The Availability Zones in which the service is available.
--
-- 'gatewayLoadBalancerArns', 'serviceConfiguration_gatewayLoadBalancerArns' - The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
-- service.
--
-- 'managesVpcEndpoints', 'serviceConfiguration_managesVpcEndpoints' - Indicates whether the service manages its VPC endpoints. Management of
-- the service VPC endpoints using the VPC endpoint API is restricted.
--
-- 'serviceName', 'serviceConfiguration_serviceName' - The name of the service.
--
-- 'serviceState', 'serviceConfiguration_serviceState' - The service state.
--
-- 'serviceType', 'serviceConfiguration_serviceType' - The type of service.
--
-- 'acceptanceRequired', 'serviceConfiguration_acceptanceRequired' - Indicates whether requests from other Amazon Web Services accounts to
-- create an endpoint to the service must first be accepted.
--
-- 'serviceId', 'serviceConfiguration_serviceId' - The ID of the service.
--
-- 'privateDnsName', 'serviceConfiguration_privateDnsName' - The private DNS name for the service.
--
-- 'privateDnsNameConfiguration', 'serviceConfiguration_privateDnsNameConfiguration' - Information about the endpoint service private DNS name configuration.
--
-- 'tags', 'serviceConfiguration_tags' - Any tags assigned to the service.
newServiceConfiguration ::
  ServiceConfiguration
newServiceConfiguration =
  ServiceConfiguration'
    { networkLoadBalancerArns =
        Prelude.Nothing,
      baseEndpointDnsNames = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      gatewayLoadBalancerArns = Prelude.Nothing,
      managesVpcEndpoints = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      serviceState = Prelude.Nothing,
      serviceType = Prelude.Nothing,
      acceptanceRequired = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      privateDnsNameConfiguration = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Names (ARNs) of the Network Load Balancers for the
-- service.
serviceConfiguration_networkLoadBalancerArns :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [Prelude.Text])
serviceConfiguration_networkLoadBalancerArns = Lens.lens (\ServiceConfiguration' {networkLoadBalancerArns} -> networkLoadBalancerArns) (\s@ServiceConfiguration' {} a -> s {networkLoadBalancerArns = a} :: ServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The DNS names for the service.
serviceConfiguration_baseEndpointDnsNames :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [Prelude.Text])
serviceConfiguration_baseEndpointDnsNames = Lens.lens (\ServiceConfiguration' {baseEndpointDnsNames} -> baseEndpointDnsNames) (\s@ServiceConfiguration' {} a -> s {baseEndpointDnsNames = a} :: ServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Availability Zones in which the service is available.
serviceConfiguration_availabilityZones :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [Prelude.Text])
serviceConfiguration_availabilityZones = Lens.lens (\ServiceConfiguration' {availabilityZones} -> availabilityZones) (\s@ServiceConfiguration' {} a -> s {availabilityZones = a} :: ServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
-- service.
serviceConfiguration_gatewayLoadBalancerArns :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [Prelude.Text])
serviceConfiguration_gatewayLoadBalancerArns = Lens.lens (\ServiceConfiguration' {gatewayLoadBalancerArns} -> gatewayLoadBalancerArns) (\s@ServiceConfiguration' {} a -> s {gatewayLoadBalancerArns = a} :: ServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the service manages its VPC endpoints. Management of
-- the service VPC endpoints using the VPC endpoint API is restricted.
serviceConfiguration_managesVpcEndpoints :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Bool)
serviceConfiguration_managesVpcEndpoints = Lens.lens (\ServiceConfiguration' {managesVpcEndpoints} -> managesVpcEndpoints) (\s@ServiceConfiguration' {} a -> s {managesVpcEndpoints = a} :: ServiceConfiguration)

-- | The name of the service.
serviceConfiguration_serviceName :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Text)
serviceConfiguration_serviceName = Lens.lens (\ServiceConfiguration' {serviceName} -> serviceName) (\s@ServiceConfiguration' {} a -> s {serviceName = a} :: ServiceConfiguration)

-- | The service state.
serviceConfiguration_serviceState :: Lens.Lens' ServiceConfiguration (Prelude.Maybe ServiceState)
serviceConfiguration_serviceState = Lens.lens (\ServiceConfiguration' {serviceState} -> serviceState) (\s@ServiceConfiguration' {} a -> s {serviceState = a} :: ServiceConfiguration)

-- | The type of service.
serviceConfiguration_serviceType :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [ServiceTypeDetail])
serviceConfiguration_serviceType = Lens.lens (\ServiceConfiguration' {serviceType} -> serviceType) (\s@ServiceConfiguration' {} a -> s {serviceType = a} :: ServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether requests from other Amazon Web Services accounts to
-- create an endpoint to the service must first be accepted.
serviceConfiguration_acceptanceRequired :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Bool)
serviceConfiguration_acceptanceRequired = Lens.lens (\ServiceConfiguration' {acceptanceRequired} -> acceptanceRequired) (\s@ServiceConfiguration' {} a -> s {acceptanceRequired = a} :: ServiceConfiguration)

-- | The ID of the service.
serviceConfiguration_serviceId :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Text)
serviceConfiguration_serviceId = Lens.lens (\ServiceConfiguration' {serviceId} -> serviceId) (\s@ServiceConfiguration' {} a -> s {serviceId = a} :: ServiceConfiguration)

-- | The private DNS name for the service.
serviceConfiguration_privateDnsName :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Text)
serviceConfiguration_privateDnsName = Lens.lens (\ServiceConfiguration' {privateDnsName} -> privateDnsName) (\s@ServiceConfiguration' {} a -> s {privateDnsName = a} :: ServiceConfiguration)

-- | Information about the endpoint service private DNS name configuration.
serviceConfiguration_privateDnsNameConfiguration :: Lens.Lens' ServiceConfiguration (Prelude.Maybe PrivateDnsNameConfiguration)
serviceConfiguration_privateDnsNameConfiguration = Lens.lens (\ServiceConfiguration' {privateDnsNameConfiguration} -> privateDnsNameConfiguration) (\s@ServiceConfiguration' {} a -> s {privateDnsNameConfiguration = a} :: ServiceConfiguration)

-- | Any tags assigned to the service.
serviceConfiguration_tags :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [Tag])
serviceConfiguration_tags = Lens.lens (\ServiceConfiguration' {tags} -> tags) (\s@ServiceConfiguration' {} a -> s {tags = a} :: ServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML ServiceConfiguration where
  parseXML x =
    ServiceConfiguration'
      Prelude.<$> ( x Core..@? "networkLoadBalancerArnSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "baseEndpointDnsNameSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "availabilityZoneSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "gatewayLoadBalancerArnSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "managesVpcEndpoints")
      Prelude.<*> (x Core..@? "serviceName")
      Prelude.<*> (x Core..@? "serviceState")
      Prelude.<*> ( x Core..@? "serviceType" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "acceptanceRequired")
      Prelude.<*> (x Core..@? "serviceId")
      Prelude.<*> (x Core..@? "privateDnsName")
      Prelude.<*> (x Core..@? "privateDnsNameConfiguration")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable ServiceConfiguration where
  hashWithSalt _salt ServiceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` networkLoadBalancerArns
      `Prelude.hashWithSalt` baseEndpointDnsNames
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` gatewayLoadBalancerArns
      `Prelude.hashWithSalt` managesVpcEndpoints
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` serviceState
      `Prelude.hashWithSalt` serviceType
      `Prelude.hashWithSalt` acceptanceRequired
      `Prelude.hashWithSalt` serviceId
      `Prelude.hashWithSalt` privateDnsName
      `Prelude.hashWithSalt` privateDnsNameConfiguration
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ServiceConfiguration where
  rnf ServiceConfiguration' {..} =
    Prelude.rnf networkLoadBalancerArns
      `Prelude.seq` Prelude.rnf baseEndpointDnsNames
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf gatewayLoadBalancerArns
      `Prelude.seq` Prelude.rnf managesVpcEndpoints
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceState
      `Prelude.seq` Prelude.rnf serviceType
      `Prelude.seq` Prelude.rnf acceptanceRequired
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf privateDnsName
      `Prelude.seq` Prelude.rnf privateDnsNameConfiguration
      `Prelude.seq` Prelude.rnf tags
