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
-- Module      : Network.AWS.EC2.Types.ServiceDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ServiceDetail where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DnsNameState
import Network.AWS.EC2.Types.PrivateDnsDetails
import Network.AWS.EC2.Types.ServiceTypeDetail
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a VPC endpoint service.
--
-- /See:/ 'newServiceDetail' smart constructor.
data ServiceDetail = ServiceDetail'
  { -- | Indicates whether the service manages its VPC endpoints. Management of
    -- the service VPC endpoints using the VPC endpoint API is restricted.
    managesVpcEndpoints :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zones in which the service is available.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The DNS names for the service.
    baseEndpointDnsNames :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the endpoint service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the service supports endpoint policies.
    vpcEndpointPolicySupported :: Prelude.Maybe Prelude.Bool,
    -- | The private DNS names assigned to the VPC endpoint service.
    privateDnsNames :: Prelude.Maybe [PrivateDnsDetails],
    -- | The verification state of the VPC endpoint service.
    --
    -- Consumers of the endpoint service cannot use the private name when the
    -- state is not @verified@.
    privateDnsNameVerificationState :: Prelude.Maybe DnsNameState,
    -- | The Amazon Resource Name (ARN) of the service.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the service.
    tags :: Prelude.Maybe [Tag],
    -- | The AWS account ID of the service owner.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The private DNS name for the service.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether VPC endpoint connection requests to the service must
    -- be accepted by the service owner.
    acceptanceRequired :: Prelude.Maybe Prelude.Bool,
    -- | The type of service.
    serviceType :: Prelude.Maybe [ServiceTypeDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServiceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managesVpcEndpoints', 'serviceDetail_managesVpcEndpoints' - Indicates whether the service manages its VPC endpoints. Management of
-- the service VPC endpoints using the VPC endpoint API is restricted.
--
-- 'availabilityZones', 'serviceDetail_availabilityZones' - The Availability Zones in which the service is available.
--
-- 'baseEndpointDnsNames', 'serviceDetail_baseEndpointDnsNames' - The DNS names for the service.
--
-- 'serviceId', 'serviceDetail_serviceId' - The ID of the endpoint service.
--
-- 'vpcEndpointPolicySupported', 'serviceDetail_vpcEndpointPolicySupported' - Indicates whether the service supports endpoint policies.
--
-- 'privateDnsNames', 'serviceDetail_privateDnsNames' - The private DNS names assigned to the VPC endpoint service.
--
-- 'privateDnsNameVerificationState', 'serviceDetail_privateDnsNameVerificationState' - The verification state of the VPC endpoint service.
--
-- Consumers of the endpoint service cannot use the private name when the
-- state is not @verified@.
--
-- 'serviceName', 'serviceDetail_serviceName' - The Amazon Resource Name (ARN) of the service.
--
-- 'tags', 'serviceDetail_tags' - Any tags assigned to the service.
--
-- 'owner', 'serviceDetail_owner' - The AWS account ID of the service owner.
--
-- 'privateDnsName', 'serviceDetail_privateDnsName' - The private DNS name for the service.
--
-- 'acceptanceRequired', 'serviceDetail_acceptanceRequired' - Indicates whether VPC endpoint connection requests to the service must
-- be accepted by the service owner.
--
-- 'serviceType', 'serviceDetail_serviceType' - The type of service.
newServiceDetail ::
  ServiceDetail
newServiceDetail =
  ServiceDetail'
    { managesVpcEndpoints =
        Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      baseEndpointDnsNames = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      vpcEndpointPolicySupported = Prelude.Nothing,
      privateDnsNames = Prelude.Nothing,
      privateDnsNameVerificationState = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      tags = Prelude.Nothing,
      owner = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      acceptanceRequired = Prelude.Nothing,
      serviceType = Prelude.Nothing
    }

-- | Indicates whether the service manages its VPC endpoints. Management of
-- the service VPC endpoints using the VPC endpoint API is restricted.
serviceDetail_managesVpcEndpoints :: Lens.Lens' ServiceDetail (Prelude.Maybe Prelude.Bool)
serviceDetail_managesVpcEndpoints = Lens.lens (\ServiceDetail' {managesVpcEndpoints} -> managesVpcEndpoints) (\s@ServiceDetail' {} a -> s {managesVpcEndpoints = a} :: ServiceDetail)

-- | The Availability Zones in which the service is available.
serviceDetail_availabilityZones :: Lens.Lens' ServiceDetail (Prelude.Maybe [Prelude.Text])
serviceDetail_availabilityZones = Lens.lens (\ServiceDetail' {availabilityZones} -> availabilityZones) (\s@ServiceDetail' {} a -> s {availabilityZones = a} :: ServiceDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The DNS names for the service.
serviceDetail_baseEndpointDnsNames :: Lens.Lens' ServiceDetail (Prelude.Maybe [Prelude.Text])
serviceDetail_baseEndpointDnsNames = Lens.lens (\ServiceDetail' {baseEndpointDnsNames} -> baseEndpointDnsNames) (\s@ServiceDetail' {} a -> s {baseEndpointDnsNames = a} :: ServiceDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the endpoint service.
serviceDetail_serviceId :: Lens.Lens' ServiceDetail (Prelude.Maybe Prelude.Text)
serviceDetail_serviceId = Lens.lens (\ServiceDetail' {serviceId} -> serviceId) (\s@ServiceDetail' {} a -> s {serviceId = a} :: ServiceDetail)

-- | Indicates whether the service supports endpoint policies.
serviceDetail_vpcEndpointPolicySupported :: Lens.Lens' ServiceDetail (Prelude.Maybe Prelude.Bool)
serviceDetail_vpcEndpointPolicySupported = Lens.lens (\ServiceDetail' {vpcEndpointPolicySupported} -> vpcEndpointPolicySupported) (\s@ServiceDetail' {} a -> s {vpcEndpointPolicySupported = a} :: ServiceDetail)

-- | The private DNS names assigned to the VPC endpoint service.
serviceDetail_privateDnsNames :: Lens.Lens' ServiceDetail (Prelude.Maybe [PrivateDnsDetails])
serviceDetail_privateDnsNames = Lens.lens (\ServiceDetail' {privateDnsNames} -> privateDnsNames) (\s@ServiceDetail' {} a -> s {privateDnsNames = a} :: ServiceDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The verification state of the VPC endpoint service.
--
-- Consumers of the endpoint service cannot use the private name when the
-- state is not @verified@.
serviceDetail_privateDnsNameVerificationState :: Lens.Lens' ServiceDetail (Prelude.Maybe DnsNameState)
serviceDetail_privateDnsNameVerificationState = Lens.lens (\ServiceDetail' {privateDnsNameVerificationState} -> privateDnsNameVerificationState) (\s@ServiceDetail' {} a -> s {privateDnsNameVerificationState = a} :: ServiceDetail)

-- | The Amazon Resource Name (ARN) of the service.
serviceDetail_serviceName :: Lens.Lens' ServiceDetail (Prelude.Maybe Prelude.Text)
serviceDetail_serviceName = Lens.lens (\ServiceDetail' {serviceName} -> serviceName) (\s@ServiceDetail' {} a -> s {serviceName = a} :: ServiceDetail)

-- | Any tags assigned to the service.
serviceDetail_tags :: Lens.Lens' ServiceDetail (Prelude.Maybe [Tag])
serviceDetail_tags = Lens.lens (\ServiceDetail' {tags} -> tags) (\s@ServiceDetail' {} a -> s {tags = a} :: ServiceDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The AWS account ID of the service owner.
serviceDetail_owner :: Lens.Lens' ServiceDetail (Prelude.Maybe Prelude.Text)
serviceDetail_owner = Lens.lens (\ServiceDetail' {owner} -> owner) (\s@ServiceDetail' {} a -> s {owner = a} :: ServiceDetail)

-- | The private DNS name for the service.
serviceDetail_privateDnsName :: Lens.Lens' ServiceDetail (Prelude.Maybe Prelude.Text)
serviceDetail_privateDnsName = Lens.lens (\ServiceDetail' {privateDnsName} -> privateDnsName) (\s@ServiceDetail' {} a -> s {privateDnsName = a} :: ServiceDetail)

-- | Indicates whether VPC endpoint connection requests to the service must
-- be accepted by the service owner.
serviceDetail_acceptanceRequired :: Lens.Lens' ServiceDetail (Prelude.Maybe Prelude.Bool)
serviceDetail_acceptanceRequired = Lens.lens (\ServiceDetail' {acceptanceRequired} -> acceptanceRequired) (\s@ServiceDetail' {} a -> s {acceptanceRequired = a} :: ServiceDetail)

-- | The type of service.
serviceDetail_serviceType :: Lens.Lens' ServiceDetail (Prelude.Maybe [ServiceTypeDetail])
serviceDetail_serviceType = Lens.lens (\ServiceDetail' {serviceType} -> serviceType) (\s@ServiceDetail' {} a -> s {serviceType = a} :: ServiceDetail) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML ServiceDetail where
  parseXML x =
    ServiceDetail'
      Prelude.<$> (x Prelude..@? "managesVpcEndpoints")
      Prelude.<*> ( x Prelude..@? "availabilityZoneSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "baseEndpointDnsNameSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "serviceId")
      Prelude.<*> (x Prelude..@? "vpcEndpointPolicySupported")
      Prelude.<*> ( x Prelude..@? "privateDnsNameSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "privateDnsNameVerificationState")
      Prelude.<*> (x Prelude..@? "serviceName")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "owner")
      Prelude.<*> (x Prelude..@? "privateDnsName")
      Prelude.<*> (x Prelude..@? "acceptanceRequired")
      Prelude.<*> ( x Prelude..@? "serviceType"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable ServiceDetail

instance Prelude.NFData ServiceDetail
