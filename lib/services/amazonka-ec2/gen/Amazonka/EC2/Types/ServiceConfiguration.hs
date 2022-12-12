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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ServiceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PayerResponsibility
import Amazonka.EC2.Types.PrivateDnsNameConfiguration
import Amazonka.EC2.Types.ServiceConnectivityType
import Amazonka.EC2.Types.ServiceState
import Amazonka.EC2.Types.ServiceTypeDetail
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a service configuration for a VPC endpoint service.
--
-- /See:/ 'newServiceConfiguration' smart constructor.
data ServiceConfiguration = ServiceConfiguration'
  { -- | Indicates whether requests from other Amazon Web Services accounts to
    -- create an endpoint to the service must first be accepted.
    acceptanceRequired :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zones in which the service is available.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The DNS names for the service.
    baseEndpointDnsNames :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
    -- service.
    gatewayLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether the service manages its VPC endpoints. Management of
    -- the service VPC endpoints using the VPC endpoint API is restricted.
    managesVpcEndpoints :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Names (ARNs) of the Network Load Balancers for the
    -- service.
    networkLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The payer responsibility.
    payerResponsibility :: Prelude.Maybe PayerResponsibility,
    -- | The private DNS name for the service.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | Information about the endpoint service private DNS name configuration.
    privateDnsNameConfiguration :: Prelude.Maybe PrivateDnsNameConfiguration,
    -- | The ID of the service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the service.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The service state.
    serviceState :: Prelude.Maybe ServiceState,
    -- | The type of service.
    serviceType :: Prelude.Maybe [ServiceTypeDetail],
    -- | The supported IP address types.
    supportedIpAddressTypes :: Prelude.Maybe [ServiceConnectivityType],
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
-- 'acceptanceRequired', 'serviceConfiguration_acceptanceRequired' - Indicates whether requests from other Amazon Web Services accounts to
-- create an endpoint to the service must first be accepted.
--
-- 'availabilityZones', 'serviceConfiguration_availabilityZones' - The Availability Zones in which the service is available.
--
-- 'baseEndpointDnsNames', 'serviceConfiguration_baseEndpointDnsNames' - The DNS names for the service.
--
-- 'gatewayLoadBalancerArns', 'serviceConfiguration_gatewayLoadBalancerArns' - The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
-- service.
--
-- 'managesVpcEndpoints', 'serviceConfiguration_managesVpcEndpoints' - Indicates whether the service manages its VPC endpoints. Management of
-- the service VPC endpoints using the VPC endpoint API is restricted.
--
-- 'networkLoadBalancerArns', 'serviceConfiguration_networkLoadBalancerArns' - The Amazon Resource Names (ARNs) of the Network Load Balancers for the
-- service.
--
-- 'payerResponsibility', 'serviceConfiguration_payerResponsibility' - The payer responsibility.
--
-- 'privateDnsName', 'serviceConfiguration_privateDnsName' - The private DNS name for the service.
--
-- 'privateDnsNameConfiguration', 'serviceConfiguration_privateDnsNameConfiguration' - Information about the endpoint service private DNS name configuration.
--
-- 'serviceId', 'serviceConfiguration_serviceId' - The ID of the service.
--
-- 'serviceName', 'serviceConfiguration_serviceName' - The name of the service.
--
-- 'serviceState', 'serviceConfiguration_serviceState' - The service state.
--
-- 'serviceType', 'serviceConfiguration_serviceType' - The type of service.
--
-- 'supportedIpAddressTypes', 'serviceConfiguration_supportedIpAddressTypes' - The supported IP address types.
--
-- 'tags', 'serviceConfiguration_tags' - Any tags assigned to the service.
newServiceConfiguration ::
  ServiceConfiguration
newServiceConfiguration =
  ServiceConfiguration'
    { acceptanceRequired =
        Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      baseEndpointDnsNames = Prelude.Nothing,
      gatewayLoadBalancerArns = Prelude.Nothing,
      managesVpcEndpoints = Prelude.Nothing,
      networkLoadBalancerArns = Prelude.Nothing,
      payerResponsibility = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      privateDnsNameConfiguration = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      serviceState = Prelude.Nothing,
      serviceType = Prelude.Nothing,
      supportedIpAddressTypes = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Indicates whether requests from other Amazon Web Services accounts to
-- create an endpoint to the service must first be accepted.
serviceConfiguration_acceptanceRequired :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Bool)
serviceConfiguration_acceptanceRequired = Lens.lens (\ServiceConfiguration' {acceptanceRequired} -> acceptanceRequired) (\s@ServiceConfiguration' {} a -> s {acceptanceRequired = a} :: ServiceConfiguration)

-- | The Availability Zones in which the service is available.
serviceConfiguration_availabilityZones :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [Prelude.Text])
serviceConfiguration_availabilityZones = Lens.lens (\ServiceConfiguration' {availabilityZones} -> availabilityZones) (\s@ServiceConfiguration' {} a -> s {availabilityZones = a} :: ServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The DNS names for the service.
serviceConfiguration_baseEndpointDnsNames :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [Prelude.Text])
serviceConfiguration_baseEndpointDnsNames = Lens.lens (\ServiceConfiguration' {baseEndpointDnsNames} -> baseEndpointDnsNames) (\s@ServiceConfiguration' {} a -> s {baseEndpointDnsNames = a} :: ServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the
-- service.
serviceConfiguration_gatewayLoadBalancerArns :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [Prelude.Text])
serviceConfiguration_gatewayLoadBalancerArns = Lens.lens (\ServiceConfiguration' {gatewayLoadBalancerArns} -> gatewayLoadBalancerArns) (\s@ServiceConfiguration' {} a -> s {gatewayLoadBalancerArns = a} :: ServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the service manages its VPC endpoints. Management of
-- the service VPC endpoints using the VPC endpoint API is restricted.
serviceConfiguration_managesVpcEndpoints :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Bool)
serviceConfiguration_managesVpcEndpoints = Lens.lens (\ServiceConfiguration' {managesVpcEndpoints} -> managesVpcEndpoints) (\s@ServiceConfiguration' {} a -> s {managesVpcEndpoints = a} :: ServiceConfiguration)

-- | The Amazon Resource Names (ARNs) of the Network Load Balancers for the
-- service.
serviceConfiguration_networkLoadBalancerArns :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [Prelude.Text])
serviceConfiguration_networkLoadBalancerArns = Lens.lens (\ServiceConfiguration' {networkLoadBalancerArns} -> networkLoadBalancerArns) (\s@ServiceConfiguration' {} a -> s {networkLoadBalancerArns = a} :: ServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The payer responsibility.
serviceConfiguration_payerResponsibility :: Lens.Lens' ServiceConfiguration (Prelude.Maybe PayerResponsibility)
serviceConfiguration_payerResponsibility = Lens.lens (\ServiceConfiguration' {payerResponsibility} -> payerResponsibility) (\s@ServiceConfiguration' {} a -> s {payerResponsibility = a} :: ServiceConfiguration)

-- | The private DNS name for the service.
serviceConfiguration_privateDnsName :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Text)
serviceConfiguration_privateDnsName = Lens.lens (\ServiceConfiguration' {privateDnsName} -> privateDnsName) (\s@ServiceConfiguration' {} a -> s {privateDnsName = a} :: ServiceConfiguration)

-- | Information about the endpoint service private DNS name configuration.
serviceConfiguration_privateDnsNameConfiguration :: Lens.Lens' ServiceConfiguration (Prelude.Maybe PrivateDnsNameConfiguration)
serviceConfiguration_privateDnsNameConfiguration = Lens.lens (\ServiceConfiguration' {privateDnsNameConfiguration} -> privateDnsNameConfiguration) (\s@ServiceConfiguration' {} a -> s {privateDnsNameConfiguration = a} :: ServiceConfiguration)

-- | The ID of the service.
serviceConfiguration_serviceId :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Text)
serviceConfiguration_serviceId = Lens.lens (\ServiceConfiguration' {serviceId} -> serviceId) (\s@ServiceConfiguration' {} a -> s {serviceId = a} :: ServiceConfiguration)

-- | The name of the service.
serviceConfiguration_serviceName :: Lens.Lens' ServiceConfiguration (Prelude.Maybe Prelude.Text)
serviceConfiguration_serviceName = Lens.lens (\ServiceConfiguration' {serviceName} -> serviceName) (\s@ServiceConfiguration' {} a -> s {serviceName = a} :: ServiceConfiguration)

-- | The service state.
serviceConfiguration_serviceState :: Lens.Lens' ServiceConfiguration (Prelude.Maybe ServiceState)
serviceConfiguration_serviceState = Lens.lens (\ServiceConfiguration' {serviceState} -> serviceState) (\s@ServiceConfiguration' {} a -> s {serviceState = a} :: ServiceConfiguration)

-- | The type of service.
serviceConfiguration_serviceType :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [ServiceTypeDetail])
serviceConfiguration_serviceType = Lens.lens (\ServiceConfiguration' {serviceType} -> serviceType) (\s@ServiceConfiguration' {} a -> s {serviceType = a} :: ServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The supported IP address types.
serviceConfiguration_supportedIpAddressTypes :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [ServiceConnectivityType])
serviceConfiguration_supportedIpAddressTypes = Lens.lens (\ServiceConfiguration' {supportedIpAddressTypes} -> supportedIpAddressTypes) (\s@ServiceConfiguration' {} a -> s {supportedIpAddressTypes = a} :: ServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Any tags assigned to the service.
serviceConfiguration_tags :: Lens.Lens' ServiceConfiguration (Prelude.Maybe [Tag])
serviceConfiguration_tags = Lens.lens (\ServiceConfiguration' {tags} -> tags) (\s@ServiceConfiguration' {} a -> s {tags = a} :: ServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML ServiceConfiguration where
  parseXML x =
    ServiceConfiguration'
      Prelude.<$> (x Data..@? "acceptanceRequired")
      Prelude.<*> ( x Data..@? "availabilityZoneSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "baseEndpointDnsNameSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "gatewayLoadBalancerArnSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "managesVpcEndpoints")
      Prelude.<*> ( x Data..@? "networkLoadBalancerArnSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "payerResponsibility")
      Prelude.<*> (x Data..@? "privateDnsName")
      Prelude.<*> (x Data..@? "privateDnsNameConfiguration")
      Prelude.<*> (x Data..@? "serviceId")
      Prelude.<*> (x Data..@? "serviceName")
      Prelude.<*> (x Data..@? "serviceState")
      Prelude.<*> ( x Data..@? "serviceType" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "supportedIpAddressTypeSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable ServiceConfiguration where
  hashWithSalt _salt ServiceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` acceptanceRequired
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` baseEndpointDnsNames
      `Prelude.hashWithSalt` gatewayLoadBalancerArns
      `Prelude.hashWithSalt` managesVpcEndpoints
      `Prelude.hashWithSalt` networkLoadBalancerArns
      `Prelude.hashWithSalt` payerResponsibility
      `Prelude.hashWithSalt` privateDnsName
      `Prelude.hashWithSalt` privateDnsNameConfiguration
      `Prelude.hashWithSalt` serviceId
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` serviceState
      `Prelude.hashWithSalt` serviceType
      `Prelude.hashWithSalt` supportedIpAddressTypes
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ServiceConfiguration where
  rnf ServiceConfiguration' {..} =
    Prelude.rnf acceptanceRequired
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf baseEndpointDnsNames
      `Prelude.seq` Prelude.rnf gatewayLoadBalancerArns
      `Prelude.seq` Prelude.rnf managesVpcEndpoints
      `Prelude.seq` Prelude.rnf networkLoadBalancerArns
      `Prelude.seq` Prelude.rnf payerResponsibility
      `Prelude.seq` Prelude.rnf privateDnsName
      `Prelude.seq` Prelude.rnf privateDnsNameConfiguration
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceState
      `Prelude.seq` Prelude.rnf serviceType
      `Prelude.seq` Prelude.rnf supportedIpAddressTypes
      `Prelude.seq` Prelude.rnf tags
