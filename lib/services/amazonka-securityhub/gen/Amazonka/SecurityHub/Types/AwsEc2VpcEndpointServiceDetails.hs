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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VpcEndpointServiceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2VpcEndpointServiceServiceTypeDetails

-- | Contains details about the service configuration for a VPC endpoint
-- service.
--
-- /See:/ 'newAwsEc2VpcEndpointServiceDetails' smart constructor.
data AwsEc2VpcEndpointServiceDetails = AwsEc2VpcEndpointServiceDetails'
  { -- | Whether requests from other Amazon Web Services accounts to create an
    -- endpoint to the service must first be accepted.
    acceptanceRequired :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zones where the service is available.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The DNS names for the service.
    baseEndpointDnsNames :: Prelude.Maybe [Prelude.Text],
    -- | The ARNs of the Gateway Load Balancers for the service.
    gatewayLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | Whether the service manages its VPC endpoints.
    managesVpcEndpoints :: Prelude.Maybe Prelude.Bool,
    -- | The ARNs of the Network Load Balancers for the service.
    networkLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The private DNS name for the service.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the service.
    serviceName :: Prelude.Maybe Prelude.Text,
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
    -- | The types for the service.
    serviceType :: Prelude.Maybe [AwsEc2VpcEndpointServiceServiceTypeDetails]
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
-- 'acceptanceRequired', 'awsEc2VpcEndpointServiceDetails_acceptanceRequired' - Whether requests from other Amazon Web Services accounts to create an
-- endpoint to the service must first be accepted.
--
-- 'availabilityZones', 'awsEc2VpcEndpointServiceDetails_availabilityZones' - The Availability Zones where the service is available.
--
-- 'baseEndpointDnsNames', 'awsEc2VpcEndpointServiceDetails_baseEndpointDnsNames' - The DNS names for the service.
--
-- 'gatewayLoadBalancerArns', 'awsEc2VpcEndpointServiceDetails_gatewayLoadBalancerArns' - The ARNs of the Gateway Load Balancers for the service.
--
-- 'managesVpcEndpoints', 'awsEc2VpcEndpointServiceDetails_managesVpcEndpoints' - Whether the service manages its VPC endpoints.
--
-- 'networkLoadBalancerArns', 'awsEc2VpcEndpointServiceDetails_networkLoadBalancerArns' - The ARNs of the Network Load Balancers for the service.
--
-- 'privateDnsName', 'awsEc2VpcEndpointServiceDetails_privateDnsName' - The private DNS name for the service.
--
-- 'serviceId', 'awsEc2VpcEndpointServiceDetails_serviceId' - The identifier of the service.
--
-- 'serviceName', 'awsEc2VpcEndpointServiceDetails_serviceName' - The name of the service.
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
-- 'serviceType', 'awsEc2VpcEndpointServiceDetails_serviceType' - The types for the service.
newAwsEc2VpcEndpointServiceDetails ::
  AwsEc2VpcEndpointServiceDetails
newAwsEc2VpcEndpointServiceDetails =
  AwsEc2VpcEndpointServiceDetails'
    { acceptanceRequired =
        Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      baseEndpointDnsNames = Prelude.Nothing,
      gatewayLoadBalancerArns = Prelude.Nothing,
      managesVpcEndpoints = Prelude.Nothing,
      networkLoadBalancerArns = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      serviceState = Prelude.Nothing,
      serviceType = Prelude.Nothing
    }

-- | Whether requests from other Amazon Web Services accounts to create an
-- endpoint to the service must first be accepted.
awsEc2VpcEndpointServiceDetails_acceptanceRequired :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe Prelude.Bool)
awsEc2VpcEndpointServiceDetails_acceptanceRequired = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {acceptanceRequired} -> acceptanceRequired) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {acceptanceRequired = a} :: AwsEc2VpcEndpointServiceDetails)

-- | The Availability Zones where the service is available.
awsEc2VpcEndpointServiceDetails_availabilityZones :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe [Prelude.Text])
awsEc2VpcEndpointServiceDetails_availabilityZones = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {availabilityZones} -> availabilityZones) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {availabilityZones = a} :: AwsEc2VpcEndpointServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The DNS names for the service.
awsEc2VpcEndpointServiceDetails_baseEndpointDnsNames :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe [Prelude.Text])
awsEc2VpcEndpointServiceDetails_baseEndpointDnsNames = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {baseEndpointDnsNames} -> baseEndpointDnsNames) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {baseEndpointDnsNames = a} :: AwsEc2VpcEndpointServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ARNs of the Gateway Load Balancers for the service.
awsEc2VpcEndpointServiceDetails_gatewayLoadBalancerArns :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe [Prelude.Text])
awsEc2VpcEndpointServiceDetails_gatewayLoadBalancerArns = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {gatewayLoadBalancerArns} -> gatewayLoadBalancerArns) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {gatewayLoadBalancerArns = a} :: AwsEc2VpcEndpointServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | Whether the service manages its VPC endpoints.
awsEc2VpcEndpointServiceDetails_managesVpcEndpoints :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe Prelude.Bool)
awsEc2VpcEndpointServiceDetails_managesVpcEndpoints = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {managesVpcEndpoints} -> managesVpcEndpoints) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {managesVpcEndpoints = a} :: AwsEc2VpcEndpointServiceDetails)

-- | The ARNs of the Network Load Balancers for the service.
awsEc2VpcEndpointServiceDetails_networkLoadBalancerArns :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe [Prelude.Text])
awsEc2VpcEndpointServiceDetails_networkLoadBalancerArns = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {networkLoadBalancerArns} -> networkLoadBalancerArns) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {networkLoadBalancerArns = a} :: AwsEc2VpcEndpointServiceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The private DNS name for the service.
awsEc2VpcEndpointServiceDetails_privateDnsName :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcEndpointServiceDetails_privateDnsName = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {privateDnsName} -> privateDnsName) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {privateDnsName = a} :: AwsEc2VpcEndpointServiceDetails)

-- | The identifier of the service.
awsEc2VpcEndpointServiceDetails_serviceId :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcEndpointServiceDetails_serviceId = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {serviceId} -> serviceId) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {serviceId = a} :: AwsEc2VpcEndpointServiceDetails)

-- | The name of the service.
awsEc2VpcEndpointServiceDetails_serviceName :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcEndpointServiceDetails_serviceName = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {serviceName} -> serviceName) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {serviceName = a} :: AwsEc2VpcEndpointServiceDetails)

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

-- | The types for the service.
awsEc2VpcEndpointServiceDetails_serviceType :: Lens.Lens' AwsEc2VpcEndpointServiceDetails (Prelude.Maybe [AwsEc2VpcEndpointServiceServiceTypeDetails])
awsEc2VpcEndpointServiceDetails_serviceType = Lens.lens (\AwsEc2VpcEndpointServiceDetails' {serviceType} -> serviceType) (\s@AwsEc2VpcEndpointServiceDetails' {} a -> s {serviceType = a} :: AwsEc2VpcEndpointServiceDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsEc2VpcEndpointServiceDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2VpcEndpointServiceDetails"
      ( \x ->
          AwsEc2VpcEndpointServiceDetails'
            Prelude.<$> (x Data..:? "AcceptanceRequired")
            Prelude.<*> ( x Data..:? "AvailabilityZones"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "BaseEndpointDnsNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "GatewayLoadBalancerArns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ManagesVpcEndpoints")
            Prelude.<*> ( x Data..:? "NetworkLoadBalancerArns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PrivateDnsName")
            Prelude.<*> (x Data..:? "ServiceId")
            Prelude.<*> (x Data..:? "ServiceName")
            Prelude.<*> (x Data..:? "ServiceState")
            Prelude.<*> (x Data..:? "ServiceType" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsEc2VpcEndpointServiceDetails
  where
  hashWithSalt
    _salt
    AwsEc2VpcEndpointServiceDetails' {..} =
      _salt `Prelude.hashWithSalt` acceptanceRequired
        `Prelude.hashWithSalt` availabilityZones
        `Prelude.hashWithSalt` baseEndpointDnsNames
        `Prelude.hashWithSalt` gatewayLoadBalancerArns
        `Prelude.hashWithSalt` managesVpcEndpoints
        `Prelude.hashWithSalt` networkLoadBalancerArns
        `Prelude.hashWithSalt` privateDnsName
        `Prelude.hashWithSalt` serviceId
        `Prelude.hashWithSalt` serviceName
        `Prelude.hashWithSalt` serviceState
        `Prelude.hashWithSalt` serviceType

instance
  Prelude.NFData
    AwsEc2VpcEndpointServiceDetails
  where
  rnf AwsEc2VpcEndpointServiceDetails' {..} =
    Prelude.rnf acceptanceRequired
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf baseEndpointDnsNames
      `Prelude.seq` Prelude.rnf gatewayLoadBalancerArns
      `Prelude.seq` Prelude.rnf managesVpcEndpoints
      `Prelude.seq` Prelude.rnf networkLoadBalancerArns
      `Prelude.seq` Prelude.rnf privateDnsName
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceState
      `Prelude.seq` Prelude.rnf serviceType

instance Data.ToJSON AwsEc2VpcEndpointServiceDetails where
  toJSON AwsEc2VpcEndpointServiceDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptanceRequired" Data..=)
              Prelude.<$> acceptanceRequired,
            ("AvailabilityZones" Data..=)
              Prelude.<$> availabilityZones,
            ("BaseEndpointDnsNames" Data..=)
              Prelude.<$> baseEndpointDnsNames,
            ("GatewayLoadBalancerArns" Data..=)
              Prelude.<$> gatewayLoadBalancerArns,
            ("ManagesVpcEndpoints" Data..=)
              Prelude.<$> managesVpcEndpoints,
            ("NetworkLoadBalancerArns" Data..=)
              Prelude.<$> networkLoadBalancerArns,
            ("PrivateDnsName" Data..=)
              Prelude.<$> privateDnsName,
            ("ServiceId" Data..=) Prelude.<$> serviceId,
            ("ServiceName" Data..=) Prelude.<$> serviceName,
            ("ServiceState" Data..=) Prelude.<$> serviceState,
            ("ServiceType" Data..=) Prelude.<$> serviceType
          ]
      )
