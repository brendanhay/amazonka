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
-- Module      : Network.AWS.EC2.Types.ServiceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ServiceConfiguration where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PrivateDnsNameConfiguration
import Network.AWS.EC2.Types.ServiceState
import Network.AWS.EC2.Types.ServiceTypeDetail
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a service configuration for a VPC endpoint service.
--
-- /See:/ 'newServiceConfiguration' smart constructor.
data ServiceConfiguration = ServiceConfiguration'
  { -- | Indicates whether the service manages its VPC endpoints. Management of
    -- the service VPC endpoints using the VPC endpoint API is restricted.
    managesVpcEndpoints :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
    -- service.
    gatewayLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The Availability Zones in which the service is available.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | Information about the endpoint service private DNS name configuration.
    privateDnsNameConfiguration :: Prelude.Maybe PrivateDnsNameConfiguration,
    -- | The DNS names for the service.
    baseEndpointDnsNames :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the service.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the service.
    tags :: Prelude.Maybe [Tag],
    -- | The private DNS name for the service.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether requests from other AWS accounts to create an endpoint
    -- to the service must first be accepted.
    acceptanceRequired :: Prelude.Maybe Prelude.Bool,
    -- | The type of service.
    serviceType :: Prelude.Maybe [ServiceTypeDetail],
    -- | The Amazon Resource Names (ARNs) of the Network Load Balancers for the
    -- service.
    networkLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The service state.
    serviceState :: Prelude.Maybe ServiceState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServiceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managesVpcEndpoints', 'serviceConfiguration_managesVpcEndpoints' - Indicates whether the service manages its VPC endpoints. Management of
-- the service VPC endpoints using the VPC endpoint API is restricted.
--
-- 'gatewayLoadBalancerArns', 'serviceConfiguration_gatewayLoadBalancerArns' - The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
-- service.
--
-- 'availabilityZones', 'serviceConfiguration_availabilityZones' - The Availability Zones in which the service is available.
--
-- 'privateDnsNameConfiguration', 'serviceConfiguration_privateDnsNameConfiguration' - Information about the endpoint service private DNS name configuration.
--
-- 'baseEndpointDnsNames', 'serviceConfiguration_baseEndpointDnsNames' - The DNS names for the service.
--
-- 'serviceId', 'serviceConfiguration_serviceId' - The ID of the service.
--
-- 'serviceName', 'serviceConfiguration_serviceName' - The name of the service.
--
-- 'tags', 'serviceConfiguration_tags' - Any tags assigned to the service.
--
-- 'privateDnsName', 'serviceConfiguration_privateDnsName' - The private DNS name for the service.
--
-- 'acceptanceRequired', 'serviceConfiguration_acceptanceRequired' - Indicates whether requests from other AWS accounts to create an endpoint
-- to the service must first be accepted.
--
-- 'serviceType', 'serviceConfiguration_serviceType' - The type of service.
--
-- 'networkLoadBalancerArns', 'serviceConfiguration_networkLoadBalancerArns' - The Amazon Resource Names (ARNs) of the Network Load Balancers for the
-- service.
--
-- 'serviceState', 'serviceConfiguration_serviceState' - The service state.
newServiceConfiguration ::
  ServiceConfiguration
newServiceConfiguration =
  ServiceConfiguration'
    { managesVpcEndpoints =
        Prelude.Nothing,
      gatewayLoadBalancerArns = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      privateDnsNameConfiguration = Prelude.Nothing,
      baseEndpointDnsNames = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      tags = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      acceptanceRequired = Prelude.Nothing,
      serviceType = Prelude.Nothing,
      networkLoadBalancerArns = Prelude.Nothing,
      serviceState = Prelude.Nothing
    }

-- | Indicates whether the service manages its VPC endpoints. Management of
-- the service VPC endpoints using the VPC endpoint API is restricted.
serviceConfiguration_managesVpcEndpoints :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Bool)
serviceConfiguration_managesVpcEndpoints = Lens.lens (\ServiceConfiguration' {managesVpcEndpoints} -> managesVpcEndpoints) (\s@ServiceConfiguration' {} a -> s {managesVpcEndpoints = a} :: ServiceConfiguration)

-- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
-- service.
serviceConfiguration_gatewayLoadBalancerArns :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [Prelude.Text])
serviceConfiguration_gatewayLoadBalancerArns = Lens.lens (\ServiceConfiguration' {gatewayLoadBalancerArns} -> gatewayLoadBalancerArns) (\s@ServiceConfiguration' {} a -> s {gatewayLoadBalancerArns = a} :: ServiceConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The Availability Zones in which the service is available.
serviceConfiguration_availabilityZones :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [Prelude.Text])
serviceConfiguration_availabilityZones = Lens.lens (\ServiceConfiguration' {availabilityZones} -> availabilityZones) (\s@ServiceConfiguration' {} a -> s {availabilityZones = a} :: ServiceConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the endpoint service private DNS name configuration.
serviceConfiguration_privateDnsNameConfiguration :: Lens.Lens' ServiceConfiguration (Prelude.Maybe PrivateDnsNameConfiguration)
serviceConfiguration_privateDnsNameConfiguration = Lens.lens (\ServiceConfiguration' {privateDnsNameConfiguration} -> privateDnsNameConfiguration) (\s@ServiceConfiguration' {} a -> s {privateDnsNameConfiguration = a} :: ServiceConfiguration)

-- | The DNS names for the service.
serviceConfiguration_baseEndpointDnsNames :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [Prelude.Text])
serviceConfiguration_baseEndpointDnsNames = Lens.lens (\ServiceConfiguration' {baseEndpointDnsNames} -> baseEndpointDnsNames) (\s@ServiceConfiguration' {} a -> s {baseEndpointDnsNames = a} :: ServiceConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the service.
serviceConfiguration_serviceId :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Text)
serviceConfiguration_serviceId = Lens.lens (\ServiceConfiguration' {serviceId} -> serviceId) (\s@ServiceConfiguration' {} a -> s {serviceId = a} :: ServiceConfiguration)

-- | The name of the service.
serviceConfiguration_serviceName :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Text)
serviceConfiguration_serviceName = Lens.lens (\ServiceConfiguration' {serviceName} -> serviceName) (\s@ServiceConfiguration' {} a -> s {serviceName = a} :: ServiceConfiguration)

-- | Any tags assigned to the service.
serviceConfiguration_tags :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [Tag])
serviceConfiguration_tags = Lens.lens (\ServiceConfiguration' {tags} -> tags) (\s@ServiceConfiguration' {} a -> s {tags = a} :: ServiceConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The private DNS name for the service.
serviceConfiguration_privateDnsName :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Text)
serviceConfiguration_privateDnsName = Lens.lens (\ServiceConfiguration' {privateDnsName} -> privateDnsName) (\s@ServiceConfiguration' {} a -> s {privateDnsName = a} :: ServiceConfiguration)

-- | Indicates whether requests from other AWS accounts to create an endpoint
-- to the service must first be accepted.
serviceConfiguration_acceptanceRequired :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Bool)
serviceConfiguration_acceptanceRequired = Lens.lens (\ServiceConfiguration' {acceptanceRequired} -> acceptanceRequired) (\s@ServiceConfiguration' {} a -> s {acceptanceRequired = a} :: ServiceConfiguration)

-- | The type of service.
serviceConfiguration_serviceType :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [ServiceTypeDetail])
serviceConfiguration_serviceType = Lens.lens (\ServiceConfiguration' {serviceType} -> serviceType) (\s@ServiceConfiguration' {} a -> s {serviceType = a} :: ServiceConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Names (ARNs) of the Network Load Balancers for the
-- service.
serviceConfiguration_networkLoadBalancerArns :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [Prelude.Text])
serviceConfiguration_networkLoadBalancerArns = Lens.lens (\ServiceConfiguration' {networkLoadBalancerArns} -> networkLoadBalancerArns) (\s@ServiceConfiguration' {} a -> s {networkLoadBalancerArns = a} :: ServiceConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The service state.
serviceConfiguration_serviceState :: Lens.Lens' ServiceConfiguration (Prelude.Maybe ServiceState)
serviceConfiguration_serviceState = Lens.lens (\ServiceConfiguration' {serviceState} -> serviceState) (\s@ServiceConfiguration' {} a -> s {serviceState = a} :: ServiceConfiguration)

instance Prelude.FromXML ServiceConfiguration where
  parseXML x =
    ServiceConfiguration'
      Prelude.<$> (x Prelude..@? "managesVpcEndpoints")
      Prelude.<*> ( x Prelude..@? "gatewayLoadBalancerArnSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "availabilityZoneSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "privateDnsNameConfiguration")
      Prelude.<*> ( x Prelude..@? "baseEndpointDnsNameSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "serviceId")
      Prelude.<*> (x Prelude..@? "serviceName")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "privateDnsName")
      Prelude.<*> (x Prelude..@? "acceptanceRequired")
      Prelude.<*> ( x Prelude..@? "serviceType"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "networkLoadBalancerArnSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "serviceState")

instance Prelude.Hashable ServiceConfiguration

instance Prelude.NFData ServiceConfiguration
