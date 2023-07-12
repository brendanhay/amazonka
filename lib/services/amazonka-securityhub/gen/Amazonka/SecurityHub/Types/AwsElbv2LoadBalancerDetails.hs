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
-- Module      : Amazonka.SecurityHub.Types.AwsElbv2LoadBalancerDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbv2LoadBalancerDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AvailabilityZone
import Amazonka.SecurityHub.Types.AwsElbv2LoadBalancerAttribute
import Amazonka.SecurityHub.Types.LoadBalancerState

-- | Information about a load balancer.
--
-- /See:/ 'newAwsElbv2LoadBalancerDetails' smart constructor.
data AwsElbv2LoadBalancerDetails = AwsElbv2LoadBalancerDetails'
  { -- | The Availability Zones for the load balancer.
    availabilityZones :: Prelude.Maybe [AvailabilityZone],
    -- | The ID of the Amazon Route 53 hosted zone associated with the load
    -- balancer.
    canonicalHostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the load balancer was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The public DNS name of the load balancer.
    dNSName :: Prelude.Maybe Prelude.Text,
    -- | The type of IP addresses used by the subnets for your load balancer. The
    -- possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for
    -- IPv4 and IPv6 addresses).
    ipAddressType :: Prelude.Maybe Prelude.Text,
    -- | Attributes of the load balancer.
    loadBalancerAttributes :: Prelude.Maybe [AwsElbv2LoadBalancerAttribute],
    -- | The nodes of an Internet-facing load balancer have public IP addresses.
    scheme :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the security groups for the load balancer.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The state of the load balancer.
    state :: Prelude.Maybe LoadBalancerState,
    -- | The type of load balancer.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC for the load balancer.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbv2LoadBalancerDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'awsElbv2LoadBalancerDetails_availabilityZones' - The Availability Zones for the load balancer.
--
-- 'canonicalHostedZoneId', 'awsElbv2LoadBalancerDetails_canonicalHostedZoneId' - The ID of the Amazon Route 53 hosted zone associated with the load
-- balancer.
--
-- 'createdTime', 'awsElbv2LoadBalancerDetails_createdTime' - Indicates when the load balancer was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'dNSName', 'awsElbv2LoadBalancerDetails_dNSName' - The public DNS name of the load balancer.
--
-- 'ipAddressType', 'awsElbv2LoadBalancerDetails_ipAddressType' - The type of IP addresses used by the subnets for your load balancer. The
-- possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for
-- IPv4 and IPv6 addresses).
--
-- 'loadBalancerAttributes', 'awsElbv2LoadBalancerDetails_loadBalancerAttributes' - Attributes of the load balancer.
--
-- 'scheme', 'awsElbv2LoadBalancerDetails_scheme' - The nodes of an Internet-facing load balancer have public IP addresses.
--
-- 'securityGroups', 'awsElbv2LoadBalancerDetails_securityGroups' - The IDs of the security groups for the load balancer.
--
-- 'state', 'awsElbv2LoadBalancerDetails_state' - The state of the load balancer.
--
-- 'type'', 'awsElbv2LoadBalancerDetails_type' - The type of load balancer.
--
-- 'vpcId', 'awsElbv2LoadBalancerDetails_vpcId' - The ID of the VPC for the load balancer.
newAwsElbv2LoadBalancerDetails ::
  AwsElbv2LoadBalancerDetails
newAwsElbv2LoadBalancerDetails =
  AwsElbv2LoadBalancerDetails'
    { availabilityZones =
        Prelude.Nothing,
      canonicalHostedZoneId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      dNSName = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      loadBalancerAttributes = Prelude.Nothing,
      scheme = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      state = Prelude.Nothing,
      type' = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The Availability Zones for the load balancer.
awsElbv2LoadBalancerDetails_availabilityZones :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe [AvailabilityZone])
awsElbv2LoadBalancerDetails_availabilityZones = Lens.lens (\AwsElbv2LoadBalancerDetails' {availabilityZones} -> availabilityZones) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {availabilityZones = a} :: AwsElbv2LoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Route 53 hosted zone associated with the load
-- balancer.
awsElbv2LoadBalancerDetails_canonicalHostedZoneId :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerDetails_canonicalHostedZoneId = Lens.lens (\AwsElbv2LoadBalancerDetails' {canonicalHostedZoneId} -> canonicalHostedZoneId) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {canonicalHostedZoneId = a} :: AwsElbv2LoadBalancerDetails)

-- | Indicates when the load balancer was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsElbv2LoadBalancerDetails_createdTime :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerDetails_createdTime = Lens.lens (\AwsElbv2LoadBalancerDetails' {createdTime} -> createdTime) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {createdTime = a} :: AwsElbv2LoadBalancerDetails)

-- | The public DNS name of the load balancer.
awsElbv2LoadBalancerDetails_dNSName :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerDetails_dNSName = Lens.lens (\AwsElbv2LoadBalancerDetails' {dNSName} -> dNSName) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {dNSName = a} :: AwsElbv2LoadBalancerDetails)

-- | The type of IP addresses used by the subnets for your load balancer. The
-- possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for
-- IPv4 and IPv6 addresses).
awsElbv2LoadBalancerDetails_ipAddressType :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerDetails_ipAddressType = Lens.lens (\AwsElbv2LoadBalancerDetails' {ipAddressType} -> ipAddressType) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {ipAddressType = a} :: AwsElbv2LoadBalancerDetails)

-- | Attributes of the load balancer.
awsElbv2LoadBalancerDetails_loadBalancerAttributes :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe [AwsElbv2LoadBalancerAttribute])
awsElbv2LoadBalancerDetails_loadBalancerAttributes = Lens.lens (\AwsElbv2LoadBalancerDetails' {loadBalancerAttributes} -> loadBalancerAttributes) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {loadBalancerAttributes = a} :: AwsElbv2LoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | The nodes of an Internet-facing load balancer have public IP addresses.
awsElbv2LoadBalancerDetails_scheme :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerDetails_scheme = Lens.lens (\AwsElbv2LoadBalancerDetails' {scheme} -> scheme) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {scheme = a} :: AwsElbv2LoadBalancerDetails)

-- | The IDs of the security groups for the load balancer.
awsElbv2LoadBalancerDetails_securityGroups :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe [Prelude.Text])
awsElbv2LoadBalancerDetails_securityGroups = Lens.lens (\AwsElbv2LoadBalancerDetails' {securityGroups} -> securityGroups) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {securityGroups = a} :: AwsElbv2LoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | The state of the load balancer.
awsElbv2LoadBalancerDetails_state :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe LoadBalancerState)
awsElbv2LoadBalancerDetails_state = Lens.lens (\AwsElbv2LoadBalancerDetails' {state} -> state) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {state = a} :: AwsElbv2LoadBalancerDetails)

-- | The type of load balancer.
awsElbv2LoadBalancerDetails_type :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerDetails_type = Lens.lens (\AwsElbv2LoadBalancerDetails' {type'} -> type') (\s@AwsElbv2LoadBalancerDetails' {} a -> s {type' = a} :: AwsElbv2LoadBalancerDetails)

-- | The ID of the VPC for the load balancer.
awsElbv2LoadBalancerDetails_vpcId :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerDetails_vpcId = Lens.lens (\AwsElbv2LoadBalancerDetails' {vpcId} -> vpcId) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {vpcId = a} :: AwsElbv2LoadBalancerDetails)

instance Data.FromJSON AwsElbv2LoadBalancerDetails where
  parseJSON =
    Data.withObject
      "AwsElbv2LoadBalancerDetails"
      ( \x ->
          AwsElbv2LoadBalancerDetails'
            Prelude.<$> ( x
                            Data..:? "AvailabilityZones"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CanonicalHostedZoneId")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "DNSName")
            Prelude.<*> (x Data..:? "IpAddressType")
            Prelude.<*> ( x
                            Data..:? "LoadBalancerAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Scheme")
            Prelude.<*> (x Data..:? "SecurityGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable AwsElbv2LoadBalancerDetails where
  hashWithSalt _salt AwsElbv2LoadBalancerDetails' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` canonicalHostedZoneId
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` dNSName
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` loadBalancerAttributes
      `Prelude.hashWithSalt` scheme
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData AwsElbv2LoadBalancerDetails where
  rnf AwsElbv2LoadBalancerDetails' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf canonicalHostedZoneId
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf dNSName
      `Prelude.seq` Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf loadBalancerAttributes
      `Prelude.seq` Prelude.rnf scheme
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf vpcId

instance Data.ToJSON AwsElbv2LoadBalancerDetails where
  toJSON AwsElbv2LoadBalancerDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityZones" Data..=)
              Prelude.<$> availabilityZones,
            ("CanonicalHostedZoneId" Data..=)
              Prelude.<$> canonicalHostedZoneId,
            ("CreatedTime" Data..=) Prelude.<$> createdTime,
            ("DNSName" Data..=) Prelude.<$> dNSName,
            ("IpAddressType" Data..=) Prelude.<$> ipAddressType,
            ("LoadBalancerAttributes" Data..=)
              Prelude.<$> loadBalancerAttributes,
            ("Scheme" Data..=) Prelude.<$> scheme,
            ("SecurityGroups" Data..=)
              Prelude.<$> securityGroups,
            ("State" Data..=) Prelude.<$> state,
            ("Type" Data..=) Prelude.<$> type',
            ("VpcId" Data..=) Prelude.<$> vpcId
          ]
      )
