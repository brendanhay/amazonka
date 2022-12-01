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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2VpcEndpointServiceDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VpcEndpointServiceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2VpcEndpointServiceServiceTypeDetails

-- | Contains details about the service configuration for a VPC endpoint
-- service.
--
-- /See:/ 'newAwsEc2VpcEndpointServiceDetails' smart constructor.
data AwsEc2VpcEndpointServiceDetails = AwsEc2VpcEndpointServiceDetails'
  { -- | The ARNs of the Gateway Load Balancers for the service.
    gatewayLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | Whether requests from other Amazon Web Services accounts to create an
    -- endpoint to the service must first be accepted.
    acceptanceRequired :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zones where the service is available.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The types for the service.
    serviceType :: Prelude.Maybe [AwsEc2VpcEndpointServiceServiceTypeDetails],
    -- | Whether the service manages its VPC endpoints.
    managesVpcEndpoints :: Prelude.Maybe Prelude.Bool,
    -- | The DNS names for the service.
    baseEndpointDnsNames :: Prelude.Maybe [Prelude.Text],
    -- | The ARNs of the Network Load Balancers for the service.
    networkLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The private DNS name for the service.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The current state of the service. Valid values are as follows:
    --
    -- -   @Available@
    --
    -- -   @Deleted@
    --
    -- -   @Deleting@
    --
    -- -   @Failed@
    --
    -- -   @Pending@
    serviceState :: Prelude.Maybe Prelude.Text,
    -- | The name of the service.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the service.
    serviceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2VpcEndpointServiceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayLoadBalancerArns', 'awsEc2VpcEndpointServiceDetails_gatewayLoadBalancerArns' - The ARNs of the Gateway Load Balancers for the service.
--
-- 'acceptanceRequired', 'awsEc2VpcEndpointServiceDetails_acceptanceRequired' - Whether requests from other Amazon Web Services accounts to create an
-- endpoint to the service must first be accepted.
--
-- 'availabilityZones', 'awsEc2VpcEndpointServiceDetails_availabilityZones' - The Availability Zones where the service is available.
--
-- 'serviceType', 'awsEc2VpcEndpointServiceDetails_serviceType' - The types for the service.
--
-- 'managesVpcEndpoints', 'awsEc2VpcEndpointServiceDetails_managesVpcEndpoints' - Whether the service manages its VPC endpoints.
--
-- 'baseEndpointDnsNames', 'awsEc2VpcEndpointServiceDetails_baseEndpointDnsNames' - The DNS names for the service.
--
-- 'networkLoadBalancerArns', 'awsEc2VpcEndpointServiceDetails_networkLoadBalancerArns' - The ARNs of the Network Load Balancers for the service.
--
-- 'privateDnsName', 'awsEc2VpcEndpointServiceDetails_privateDnsName' - The private DNS name for the service.
--
-- 'serviceState', 'awsEc2VpcEndpointServiceDetails_serviceState' - The current state of the service. Valid values are as follows:
--
-- -   @Available@
--
-- -   @Deleted@
--
-- -   @Deleting@
--
-- -   @Failed@
--
-- -   @Pending@
--
-- 'serviceName', 'awsEc2VpcEndpointServiceDetails_serviceName' - The name of the service.
--
-- 'serviceId', 'awsEc2VpcEndpointServiceDetails_serviceId' - The identifier of the service.
newAwsEc2VpcEndpointServiceDetails ::
  AwsEc2VpcEndpointServiceDetails
newAwsEc2VpcEndpointServiceDetails =
  AwsEc2VpcEndpointServiceDetails'
    { gatewayLoadBalancerArns =
        Prelude.Nothing,
      acceptanceRequired = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      serviceType = Prelude.Nothing,
      managesVpcEndpoints = Prelude.Nothing,
      baseEndpointDnsNames = Prelude.Nothing,
      networkLoadBalancerArns = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      serviceState = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      serviceId = Prelude.Nothing
    }

-- | The ARNs of the Gateway Load Balancers for the service.
awsEc2VpcEndpointServiceDetails_gatewayLoadBalancerArns :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe [Prelude.Text])
awsEc2VpcEndpointServiceDetails_gatewayLoadBalancerArns = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {gatewayLoadBalancerArns} -> gatewayLoadBalancerArns) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {gatewayLoadBalancerArns = a} :: AwsEc2VpcEndpointServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | Whether requests from other Amazon Web Services accounts to create an
-- endpoint to the service must first be accepted.
awsEc2VpcEndpointServiceDetails_acceptanceRequired :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe Prelude.Bool)
awsEc2VpcEndpointServiceDetails_acceptanceRequired = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {acceptanceRequired} -> acceptanceRequired) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {acceptanceRequired = a} :: AwsEc2VpcEndpointServiceDetails)

-- | The Availability Zones where the service is available.
awsEc2VpcEndpointServiceDetails_availabilityZones :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe [Prelude.Text])
awsEc2VpcEndpointServiceDetails_availabilityZones = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {availabilityZones} -> availabilityZones) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {availabilityZones = a} :: AwsEc2VpcEndpointServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The types for the service.
awsEc2VpcEndpointServiceDetails_serviceType :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe [AwsEc2VpcEndpointServiceServiceTypeDetails])
awsEc2VpcEndpointServiceDetails_serviceType = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {serviceType} -> serviceType) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {serviceType = a} :: AwsEc2VpcEndpointServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | Whether the service manages its VPC endpoints.
awsEc2VpcEndpointServiceDetails_managesVpcEndpoints :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe Prelude.Bool)
awsEc2VpcEndpointServiceDetails_managesVpcEndpoints = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {managesVpcEndpoints} -> managesVpcEndpoints) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {managesVpcEndpoints = a} :: AwsEc2VpcEndpointServiceDetails)

-- | The DNS names for the service.
awsEc2VpcEndpointServiceDetails_baseEndpointDnsNames :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe [Prelude.Text])
awsEc2VpcEndpointServiceDetails_baseEndpointDnsNames = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {baseEndpointDnsNames} -> baseEndpointDnsNames) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {baseEndpointDnsNames = a} :: AwsEc2VpcEndpointServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ARNs of the Network Load Balancers for the service.
awsEc2VpcEndpointServiceDetails_networkLoadBalancerArns :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe [Prelude.Text])
awsEc2VpcEndpointServiceDetails_networkLoadBalancerArns = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {networkLoadBalancerArns} -> networkLoadBalancerArns) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {networkLoadBalancerArns = a} :: AwsEc2VpcEndpointServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The private DNS name for the service.
awsEc2VpcEndpointServiceDetails_privateDnsName :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcEndpointServiceDetails_privateDnsName = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {privateDnsName} -> privateDnsName) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {privateDnsName = a} :: AwsEc2VpcEndpointServiceDetails)

-- | The current state of the service. Valid values are as follows:
--
-- -   @Available@
--
-- -   @Deleted@
--
-- -   @Deleting@
--
-- -   @Failed@
--
-- -   @Pending@
awsEc2VpcEndpointServiceDetails_serviceState :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcEndpointServiceDetails_serviceState = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {serviceState} -> serviceState) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {serviceState = a} :: AwsEc2VpcEndpointServiceDetails)

-- | The name of the service.
awsEc2VpcEndpointServiceDetails_serviceName :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcEndpointServiceDetails_serviceName = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {serviceName} -> serviceName) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {serviceName = a} :: AwsEc2VpcEndpointServiceDetails)

-- | The identifier of the service.
awsEc2VpcEndpointServiceDetails_serviceId :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcEndpointServiceDetails_serviceId = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {serviceId} -> serviceId) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {serviceId = a} :: AwsEc2VpcEndpointServiceDetails)

instance
  Core.FromJSON
    AwsEc2VpcEndpointServiceDetails
  where
  parseJSON =
    Core.withObject
      "AwsEc2VpcEndpointServiceDetails"
      ( \x ->
          AwsEc2VpcEndpointServiceDetails'
            Prelude.<$> ( x Core..:? "GatewayLoadBalancerArns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "AcceptanceRequired")
            Prelude.<*> ( x Core..:? "AvailabilityZones"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ServiceType" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ManagesVpcEndpoints")
            Prelude.<*> ( x Core..:? "BaseEndpointDnsNames"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "NetworkLoadBalancerArns"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "PrivateDnsName")
            Prelude.<*> (x Core..:? "ServiceState")
            Prelude.<*> (x Core..:? "ServiceName")
            Prelude.<*> (x Core..:? "ServiceId")
      )

instance
  Prelude.Hashable
    AwsEc2VpcEndpointServiceDetails
  where
  hashWithSalt
    _salt
    AwsEc2VpcEndpointServiceDetails' {..} =
      _salt
        `Prelude.hashWithSalt` gatewayLoadBalancerArns
        `Prelude.hashWithSalt` acceptanceRequired
        `Prelude.hashWithSalt` availabilityZones
        `Prelude.hashWithSalt` serviceType
        `Prelude.hashWithSalt` managesVpcEndpoints
        `Prelude.hashWithSalt` baseEndpointDnsNames
        `Prelude.hashWithSalt` networkLoadBalancerArns
        `Prelude.hashWithSalt` privateDnsName
        `Prelude.hashWithSalt` serviceState
        `Prelude.hashWithSalt` serviceName
        `Prelude.hashWithSalt` serviceId

instance
  Prelude.NFData
    AwsEc2VpcEndpointServiceDetails
  where
  rnf AwsEc2VpcEndpointServiceDetails' {..} =
    Prelude.rnf gatewayLoadBalancerArns
      `Prelude.seq` Prelude.rnf acceptanceRequired
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf serviceType
      `Prelude.seq` Prelude.rnf managesVpcEndpoints
      `Prelude.seq` Prelude.rnf baseEndpointDnsNames
      `Prelude.seq` Prelude.rnf networkLoadBalancerArns
      `Prelude.seq` Prelude.rnf privateDnsName
      `Prelude.seq` Prelude.rnf serviceState
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceId

instance Core.ToJSON AwsEc2VpcEndpointServiceDetails where
  toJSON AwsEc2VpcEndpointServiceDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GatewayLoadBalancerArns" Core..=)
              Prelude.<$> gatewayLoadBalancerArns,
            ("AcceptanceRequired" Core..=)
              Prelude.<$> acceptanceRequired,
            ("AvailabilityZones" Core..=)
              Prelude.<$> availabilityZones,
            ("ServiceType" Core..=) Prelude.<$> serviceType,
            ("ManagesVpcEndpoints" Core..=)
              Prelude.<$> managesVpcEndpoints,
            ("BaseEndpointDnsNames" Core..=)
              Prelude.<$> baseEndpointDnsNames,
            ("NetworkLoadBalancerArns" Core..=)
              Prelude.<$> networkLoadBalancerArns,
            ("PrivateDnsName" Core..=)
              Prelude.<$> privateDnsName,
            ("ServiceState" Core..=) Prelude.<$> serviceState,
            ("ServiceName" Core..=) Prelude.<$> serviceName,
            ("ServiceId" Core..=) Prelude.<$> serviceId
          ]
      )
