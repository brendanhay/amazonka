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
-- Module      : Amazonka.EC2.Types.ServiceDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ServiceDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DnsNameState
import Amazonka.EC2.Types.PayerResponsibility
import Amazonka.EC2.Types.PrivateDnsDetails
import Amazonka.EC2.Types.ServiceConnectivityType
import Amazonka.EC2.Types.ServiceTypeDetail
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a VPC endpoint service.
--
-- /See:/ 'newServiceDetail' smart constructor.
data ServiceDetail = ServiceDetail'
  { -- | Any tags assigned to the service.
    tags :: Prelude.Maybe [Tag],
    -- | Indicates whether VPC endpoint connection requests to the service must
    -- be accepted by the service owner.
    acceptanceRequired :: Prelude.Maybe Prelude.Bool,
    -- | The supported IP address types.
    supportedIpAddressTypes :: Prelude.Maybe [ServiceConnectivityType],
    -- | The Availability Zones in which the service is available.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The type of service.
    serviceType :: Prelude.Maybe [ServiceTypeDetail],
    -- | The Amazon Web Services account ID of the service owner.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The payer responsibility.
    payerResponsibility :: Prelude.Maybe PayerResponsibility,
    -- | Indicates whether the service manages its VPC endpoints. Management of
    -- the service VPC endpoints using the VPC endpoint API is restricted.
    managesVpcEndpoints :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the service supports endpoint policies.
    vpcEndpointPolicySupported :: Prelude.Maybe Prelude.Bool,
    -- | The DNS names for the service.
    baseEndpointDnsNames :: Prelude.Maybe [Prelude.Text],
    -- | The verification state of the VPC endpoint service.
    --
    -- Consumers of the endpoint service cannot use the private name when the
    -- state is not @verified@.
    privateDnsNameVerificationState :: Prelude.Maybe DnsNameState,
    -- | The private DNS names assigned to the VPC endpoint service.
    privateDnsNames :: Prelude.Maybe [PrivateDnsDetails],
    -- | The private DNS name for the service.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The name of the service.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the endpoint service.
    serviceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'serviceDetail_tags' - Any tags assigned to the service.
--
-- 'acceptanceRequired', 'serviceDetail_acceptanceRequired' - Indicates whether VPC endpoint connection requests to the service must
-- be accepted by the service owner.
--
-- 'supportedIpAddressTypes', 'serviceDetail_supportedIpAddressTypes' - The supported IP address types.
--
-- 'availabilityZones', 'serviceDetail_availabilityZones' - The Availability Zones in which the service is available.
--
-- 'serviceType', 'serviceDetail_serviceType' - The type of service.
--
-- 'owner', 'serviceDetail_owner' - The Amazon Web Services account ID of the service owner.
--
-- 'payerResponsibility', 'serviceDetail_payerResponsibility' - The payer responsibility.
--
-- 'managesVpcEndpoints', 'serviceDetail_managesVpcEndpoints' - Indicates whether the service manages its VPC endpoints. Management of
-- the service VPC endpoints using the VPC endpoint API is restricted.
--
-- 'vpcEndpointPolicySupported', 'serviceDetail_vpcEndpointPolicySupported' - Indicates whether the service supports endpoint policies.
--
-- 'baseEndpointDnsNames', 'serviceDetail_baseEndpointDnsNames' - The DNS names for the service.
--
-- 'privateDnsNameVerificationState', 'serviceDetail_privateDnsNameVerificationState' - The verification state of the VPC endpoint service.
--
-- Consumers of the endpoint service cannot use the private name when the
-- state is not @verified@.
--
-- 'privateDnsNames', 'serviceDetail_privateDnsNames' - The private DNS names assigned to the VPC endpoint service.
--
-- 'privateDnsName', 'serviceDetail_privateDnsName' - The private DNS name for the service.
--
-- 'serviceName', 'serviceDetail_serviceName' - The name of the service.
--
-- 'serviceId', 'serviceDetail_serviceId' - The ID of the endpoint service.
newServiceDetail ::
  ServiceDetail
newServiceDetail =
  ServiceDetail'
    { tags = Prelude.Nothing,
      acceptanceRequired = Prelude.Nothing,
      supportedIpAddressTypes = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      serviceType = Prelude.Nothing,
      owner = Prelude.Nothing,
      payerResponsibility = Prelude.Nothing,
      managesVpcEndpoints = Prelude.Nothing,
      vpcEndpointPolicySupported = Prelude.Nothing,
      baseEndpointDnsNames = Prelude.Nothing,
      privateDnsNameVerificationState = Prelude.Nothing,
      privateDnsNames = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      serviceId = Prelude.Nothing
    }

-- | Any tags assigned to the service.
serviceDetail_tags :: Lens.Lens' ServiceDetail (Prelude.Maybe [Tag])
serviceDetail_tags = Lens.lens (\ServiceDetail' {tags} -> tags) (\s@ServiceDetail' {} a -> s {tags = a} :: ServiceDetail) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether VPC endpoint connection requests to the service must
-- be accepted by the service owner.
serviceDetail_acceptanceRequired :: Lens.Lens' ServiceDetail (Prelude.Maybe Prelude.Bool)
serviceDetail_acceptanceRequired = Lens.lens (\ServiceDetail' {acceptanceRequired} -> acceptanceRequired) (\s@ServiceDetail' {} a -> s {acceptanceRequired = a} :: ServiceDetail)

-- | The supported IP address types.
serviceDetail_supportedIpAddressTypes :: Lens.Lens' ServiceDetail (Prelude.Maybe [ServiceConnectivityType])
serviceDetail_supportedIpAddressTypes = Lens.lens (\ServiceDetail' {supportedIpAddressTypes} -> supportedIpAddressTypes) (\s@ServiceDetail' {} a -> s {supportedIpAddressTypes = a} :: ServiceDetail) Prelude.. Lens.mapping Lens.coerced

-- | The Availability Zones in which the service is available.
serviceDetail_availabilityZones :: Lens.Lens' ServiceDetail (Prelude.Maybe [Prelude.Text])
serviceDetail_availabilityZones = Lens.lens (\ServiceDetail' {availabilityZones} -> availabilityZones) (\s@ServiceDetail' {} a -> s {availabilityZones = a} :: ServiceDetail) Prelude.. Lens.mapping Lens.coerced

-- | The type of service.
serviceDetail_serviceType :: Lens.Lens' ServiceDetail (Prelude.Maybe [ServiceTypeDetail])
serviceDetail_serviceType = Lens.lens (\ServiceDetail' {serviceType} -> serviceType) (\s@ServiceDetail' {} a -> s {serviceType = a} :: ServiceDetail) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID of the service owner.
serviceDetail_owner :: Lens.Lens' ServiceDetail (Prelude.Maybe Prelude.Text)
serviceDetail_owner = Lens.lens (\ServiceDetail' {owner} -> owner) (\s@ServiceDetail' {} a -> s {owner = a} :: ServiceDetail)

-- | The payer responsibility.
serviceDetail_payerResponsibility :: Lens.Lens' ServiceDetail (Prelude.Maybe PayerResponsibility)
serviceDetail_payerResponsibility = Lens.lens (\ServiceDetail' {payerResponsibility} -> payerResponsibility) (\s@ServiceDetail' {} a -> s {payerResponsibility = a} :: ServiceDetail)

-- | Indicates whether the service manages its VPC endpoints. Management of
-- the service VPC endpoints using the VPC endpoint API is restricted.
serviceDetail_managesVpcEndpoints :: Lens.Lens' ServiceDetail (Prelude.Maybe Prelude.Bool)
serviceDetail_managesVpcEndpoints = Lens.lens (\ServiceDetail' {managesVpcEndpoints} -> managesVpcEndpoints) (\s@ServiceDetail' {} a -> s {managesVpcEndpoints = a} :: ServiceDetail)

-- | Indicates whether the service supports endpoint policies.
serviceDetail_vpcEndpointPolicySupported :: Lens.Lens' ServiceDetail (Prelude.Maybe Prelude.Bool)
serviceDetail_vpcEndpointPolicySupported = Lens.lens (\ServiceDetail' {vpcEndpointPolicySupported} -> vpcEndpointPolicySupported) (\s@ServiceDetail' {} a -> s {vpcEndpointPolicySupported = a} :: ServiceDetail)

-- | The DNS names for the service.
serviceDetail_baseEndpointDnsNames :: Lens.Lens' ServiceDetail (Prelude.Maybe [Prelude.Text])
serviceDetail_baseEndpointDnsNames = Lens.lens (\ServiceDetail' {baseEndpointDnsNames} -> baseEndpointDnsNames) (\s@ServiceDetail' {} a -> s {baseEndpointDnsNames = a} :: ServiceDetail) Prelude.. Lens.mapping Lens.coerced

-- | The verification state of the VPC endpoint service.
--
-- Consumers of the endpoint service cannot use the private name when the
-- state is not @verified@.
serviceDetail_privateDnsNameVerificationState :: Lens.Lens' ServiceDetail (Prelude.Maybe DnsNameState)
serviceDetail_privateDnsNameVerificationState = Lens.lens (\ServiceDetail' {privateDnsNameVerificationState} -> privateDnsNameVerificationState) (\s@ServiceDetail' {} a -> s {privateDnsNameVerificationState = a} :: ServiceDetail)

-- | The private DNS names assigned to the VPC endpoint service.
serviceDetail_privateDnsNames :: Lens.Lens' ServiceDetail (Prelude.Maybe [PrivateDnsDetails])
serviceDetail_privateDnsNames = Lens.lens (\ServiceDetail' {privateDnsNames} -> privateDnsNames) (\s@ServiceDetail' {} a -> s {privateDnsNames = a} :: ServiceDetail) Prelude.. Lens.mapping Lens.coerced

-- | The private DNS name for the service.
serviceDetail_privateDnsName :: Lens.Lens' ServiceDetail (Prelude.Maybe Prelude.Text)
serviceDetail_privateDnsName = Lens.lens (\ServiceDetail' {privateDnsName} -> privateDnsName) (\s@ServiceDetail' {} a -> s {privateDnsName = a} :: ServiceDetail)

-- | The name of the service.
serviceDetail_serviceName :: Lens.Lens' ServiceDetail (Prelude.Maybe Prelude.Text)
serviceDetail_serviceName = Lens.lens (\ServiceDetail' {serviceName} -> serviceName) (\s@ServiceDetail' {} a -> s {serviceName = a} :: ServiceDetail)

-- | The ID of the endpoint service.
serviceDetail_serviceId :: Lens.Lens' ServiceDetail (Prelude.Maybe Prelude.Text)
serviceDetail_serviceId = Lens.lens (\ServiceDetail' {serviceId} -> serviceId) (\s@ServiceDetail' {} a -> s {serviceId = a} :: ServiceDetail)

instance Core.FromXML ServiceDetail where
  parseXML x =
    ServiceDetail'
      Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "acceptanceRequired")
      Prelude.<*> ( x Core..@? "supportedIpAddressTypeSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "availabilityZoneSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "serviceType" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "owner")
      Prelude.<*> (x Core..@? "payerResponsibility")
      Prelude.<*> (x Core..@? "managesVpcEndpoints")
      Prelude.<*> (x Core..@? "vpcEndpointPolicySupported")
      Prelude.<*> ( x Core..@? "baseEndpointDnsNameSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "privateDnsNameVerificationState")
      Prelude.<*> ( x Core..@? "privateDnsNameSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "privateDnsName")
      Prelude.<*> (x Core..@? "serviceName")
      Prelude.<*> (x Core..@? "serviceId")

instance Prelude.Hashable ServiceDetail where
  hashWithSalt _salt ServiceDetail' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` acceptanceRequired
      `Prelude.hashWithSalt` supportedIpAddressTypes
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` serviceType
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` payerResponsibility
      `Prelude.hashWithSalt` managesVpcEndpoints
      `Prelude.hashWithSalt` vpcEndpointPolicySupported
      `Prelude.hashWithSalt` baseEndpointDnsNames
      `Prelude.hashWithSalt` privateDnsNameVerificationState
      `Prelude.hashWithSalt` privateDnsNames
      `Prelude.hashWithSalt` privateDnsName
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` serviceId

instance Prelude.NFData ServiceDetail where
  rnf ServiceDetail' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf acceptanceRequired
      `Prelude.seq` Prelude.rnf supportedIpAddressTypes
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf serviceType
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf payerResponsibility
      `Prelude.seq` Prelude.rnf managesVpcEndpoints
      `Prelude.seq` Prelude.rnf vpcEndpointPolicySupported
      `Prelude.seq` Prelude.rnf baseEndpointDnsNames
      `Prelude.seq` Prelude.rnf privateDnsNameVerificationState
      `Prelude.seq` Prelude.rnf privateDnsNames
      `Prelude.seq` Prelude.rnf privateDnsName
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceId
