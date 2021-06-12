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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PrivateDnsNameConfiguration
import Network.AWS.EC2.Types.ServiceState
import Network.AWS.EC2.Types.ServiceTypeDetail
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a service configuration for a VPC endpoint service.
--
-- /See:/ 'newServiceConfiguration' smart constructor.
data ServiceConfiguration = ServiceConfiguration'
  { -- | Indicates whether the service manages its VPC endpoints. Management of
    -- the service VPC endpoints using the VPC endpoint API is restricted.
    managesVpcEndpoints :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
    -- service.
    gatewayLoadBalancerArns :: Core.Maybe [Core.Text],
    -- | The Availability Zones in which the service is available.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | Information about the endpoint service private DNS name configuration.
    privateDnsNameConfiguration :: Core.Maybe PrivateDnsNameConfiguration,
    -- | The DNS names for the service.
    baseEndpointDnsNames :: Core.Maybe [Core.Text],
    -- | The ID of the service.
    serviceId :: Core.Maybe Core.Text,
    -- | The name of the service.
    serviceName :: Core.Maybe Core.Text,
    -- | Any tags assigned to the service.
    tags :: Core.Maybe [Tag],
    -- | The private DNS name for the service.
    privateDnsName :: Core.Maybe Core.Text,
    -- | Indicates whether requests from other AWS accounts to create an endpoint
    -- to the service must first be accepted.
    acceptanceRequired :: Core.Maybe Core.Bool,
    -- | The type of service.
    serviceType :: Core.Maybe [ServiceTypeDetail],
    -- | The Amazon Resource Names (ARNs) of the Network Load Balancers for the
    -- service.
    networkLoadBalancerArns :: Core.Maybe [Core.Text],
    -- | The service state.
    serviceState :: Core.Maybe ServiceState
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      gatewayLoadBalancerArns = Core.Nothing,
      availabilityZones = Core.Nothing,
      privateDnsNameConfiguration = Core.Nothing,
      baseEndpointDnsNames = Core.Nothing,
      serviceId = Core.Nothing,
      serviceName = Core.Nothing,
      tags = Core.Nothing,
      privateDnsName = Core.Nothing,
      acceptanceRequired = Core.Nothing,
      serviceType = Core.Nothing,
      networkLoadBalancerArns = Core.Nothing,
      serviceState = Core.Nothing
    }

-- | Indicates whether the service manages its VPC endpoints. Management of
-- the service VPC endpoints using the VPC endpoint API is restricted.
serviceConfiguration_managesVpcEndpoints :: Lens.Lens' ServiceConfiguration (Core.Maybe Core.Bool)
serviceConfiguration_managesVpcEndpoints = Lens.lens (\ServiceConfiguration' {managesVpcEndpoints} -> managesVpcEndpoints) (\s@ServiceConfiguration' {} a -> s {managesVpcEndpoints = a} :: ServiceConfiguration)

-- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
-- service.
serviceConfiguration_gatewayLoadBalancerArns :: Lens.Lens' ServiceConfiguration (Core.Maybe [Core.Text])
serviceConfiguration_gatewayLoadBalancerArns = Lens.lens (\ServiceConfiguration' {gatewayLoadBalancerArns} -> gatewayLoadBalancerArns) (\s@ServiceConfiguration' {} a -> s {gatewayLoadBalancerArns = a} :: ServiceConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The Availability Zones in which the service is available.
serviceConfiguration_availabilityZones :: Lens.Lens' ServiceConfiguration (Core.Maybe [Core.Text])
serviceConfiguration_availabilityZones = Lens.lens (\ServiceConfiguration' {availabilityZones} -> availabilityZones) (\s@ServiceConfiguration' {} a -> s {availabilityZones = a} :: ServiceConfiguration) Core.. Lens.mapping Lens._Coerce

-- | Information about the endpoint service private DNS name configuration.
serviceConfiguration_privateDnsNameConfiguration :: Lens.Lens' ServiceConfiguration (Core.Maybe PrivateDnsNameConfiguration)
serviceConfiguration_privateDnsNameConfiguration = Lens.lens (\ServiceConfiguration' {privateDnsNameConfiguration} -> privateDnsNameConfiguration) (\s@ServiceConfiguration' {} a -> s {privateDnsNameConfiguration = a} :: ServiceConfiguration)

-- | The DNS names for the service.
serviceConfiguration_baseEndpointDnsNames :: Lens.Lens' ServiceConfiguration (Core.Maybe [Core.Text])
serviceConfiguration_baseEndpointDnsNames = Lens.lens (\ServiceConfiguration' {baseEndpointDnsNames} -> baseEndpointDnsNames) (\s@ServiceConfiguration' {} a -> s {baseEndpointDnsNames = a} :: ServiceConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The ID of the service.
serviceConfiguration_serviceId :: Lens.Lens' ServiceConfiguration (Core.Maybe Core.Text)
serviceConfiguration_serviceId = Lens.lens (\ServiceConfiguration' {serviceId} -> serviceId) (\s@ServiceConfiguration' {} a -> s {serviceId = a} :: ServiceConfiguration)

-- | The name of the service.
serviceConfiguration_serviceName :: Lens.Lens' ServiceConfiguration (Core.Maybe Core.Text)
serviceConfiguration_serviceName = Lens.lens (\ServiceConfiguration' {serviceName} -> serviceName) (\s@ServiceConfiguration' {} a -> s {serviceName = a} :: ServiceConfiguration)

-- | Any tags assigned to the service.
serviceConfiguration_tags :: Lens.Lens' ServiceConfiguration (Core.Maybe [Tag])
serviceConfiguration_tags = Lens.lens (\ServiceConfiguration' {tags} -> tags) (\s@ServiceConfiguration' {} a -> s {tags = a} :: ServiceConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The private DNS name for the service.
serviceConfiguration_privateDnsName :: Lens.Lens' ServiceConfiguration (Core.Maybe Core.Text)
serviceConfiguration_privateDnsName = Lens.lens (\ServiceConfiguration' {privateDnsName} -> privateDnsName) (\s@ServiceConfiguration' {} a -> s {privateDnsName = a} :: ServiceConfiguration)

-- | Indicates whether requests from other AWS accounts to create an endpoint
-- to the service must first be accepted.
serviceConfiguration_acceptanceRequired :: Lens.Lens' ServiceConfiguration (Core.Maybe Core.Bool)
serviceConfiguration_acceptanceRequired = Lens.lens (\ServiceConfiguration' {acceptanceRequired} -> acceptanceRequired) (\s@ServiceConfiguration' {} a -> s {acceptanceRequired = a} :: ServiceConfiguration)

-- | The type of service.
serviceConfiguration_serviceType :: Lens.Lens' ServiceConfiguration (Core.Maybe [ServiceTypeDetail])
serviceConfiguration_serviceType = Lens.lens (\ServiceConfiguration' {serviceType} -> serviceType) (\s@ServiceConfiguration' {} a -> s {serviceType = a} :: ServiceConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Names (ARNs) of the Network Load Balancers for the
-- service.
serviceConfiguration_networkLoadBalancerArns :: Lens.Lens' ServiceConfiguration (Core.Maybe [Core.Text])
serviceConfiguration_networkLoadBalancerArns = Lens.lens (\ServiceConfiguration' {networkLoadBalancerArns} -> networkLoadBalancerArns) (\s@ServiceConfiguration' {} a -> s {networkLoadBalancerArns = a} :: ServiceConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The service state.
serviceConfiguration_serviceState :: Lens.Lens' ServiceConfiguration (Core.Maybe ServiceState)
serviceConfiguration_serviceState = Lens.lens (\ServiceConfiguration' {serviceState} -> serviceState) (\s@ServiceConfiguration' {} a -> s {serviceState = a} :: ServiceConfiguration)

instance Core.FromXML ServiceConfiguration where
  parseXML x =
    ServiceConfiguration'
      Core.<$> (x Core..@? "managesVpcEndpoints")
      Core.<*> ( x Core..@? "gatewayLoadBalancerArnSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "availabilityZoneSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "privateDnsNameConfiguration")
      Core.<*> ( x Core..@? "baseEndpointDnsNameSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "serviceId")
      Core.<*> (x Core..@? "serviceName")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "privateDnsName")
      Core.<*> (x Core..@? "acceptanceRequired")
      Core.<*> ( x Core..@? "serviceType" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "networkLoadBalancerArnSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "serviceState")

instance Core.Hashable ServiceConfiguration

instance Core.NFData ServiceConfiguration
