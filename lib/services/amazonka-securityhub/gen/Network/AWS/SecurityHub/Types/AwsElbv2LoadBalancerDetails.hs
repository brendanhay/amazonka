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
-- Module      : Network.AWS.SecurityHub.Types.AwsElbv2LoadBalancerDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsElbv2LoadBalancerDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AvailabilityZone
import Network.AWS.SecurityHub.Types.AwsElbv2LoadBalancerAttribute
import Network.AWS.SecurityHub.Types.LoadBalancerState

-- | Information about a load balancer.
--
-- /See:/ 'newAwsElbv2LoadBalancerDetails' smart constructor.
data AwsElbv2LoadBalancerDetails = AwsElbv2LoadBalancerDetails'
  { -- | The state of the load balancer.
    state :: Prelude.Maybe LoadBalancerState,
    -- | The IDs of the security groups for the load balancer.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Attributes of the load balancer.
    loadBalancerAttributes :: Prelude.Maybe [AwsElbv2LoadBalancerAttribute],
    -- | Indicates when the load balancer was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC for the load balancer.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Route 53 hosted zone associated with the load
    -- balancer.
    canonicalHostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zones for the load balancer.
    availabilityZones :: Prelude.Maybe [AvailabilityZone],
    -- | The type of IP addresses used by the subnets for your load balancer. The
    -- possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for
    -- IPv4 and IPv6 addresses).
    ipAddressType :: Prelude.Maybe Prelude.Text,
    -- | The nodes of an Internet-facing load balancer have public IP addresses.
    scheme :: Prelude.Maybe Prelude.Text,
    -- | The type of load balancer.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The public DNS name of the load balancer.
    dNSName :: Prelude.Maybe Prelude.Text
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
-- 'state', 'awsElbv2LoadBalancerDetails_state' - The state of the load balancer.
--
-- 'securityGroups', 'awsElbv2LoadBalancerDetails_securityGroups' - The IDs of the security groups for the load balancer.
--
-- 'loadBalancerAttributes', 'awsElbv2LoadBalancerDetails_loadBalancerAttributes' - Attributes of the load balancer.
--
-- 'createdTime', 'awsElbv2LoadBalancerDetails_createdTime' - Indicates when the load balancer was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'vpcId', 'awsElbv2LoadBalancerDetails_vpcId' - The ID of the VPC for the load balancer.
--
-- 'canonicalHostedZoneId', 'awsElbv2LoadBalancerDetails_canonicalHostedZoneId' - The ID of the Amazon Route 53 hosted zone associated with the load
-- balancer.
--
-- 'availabilityZones', 'awsElbv2LoadBalancerDetails_availabilityZones' - The Availability Zones for the load balancer.
--
-- 'ipAddressType', 'awsElbv2LoadBalancerDetails_ipAddressType' - The type of IP addresses used by the subnets for your load balancer. The
-- possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for
-- IPv4 and IPv6 addresses).
--
-- 'scheme', 'awsElbv2LoadBalancerDetails_scheme' - The nodes of an Internet-facing load balancer have public IP addresses.
--
-- 'type'', 'awsElbv2LoadBalancerDetails_type' - The type of load balancer.
--
-- 'dNSName', 'awsElbv2LoadBalancerDetails_dNSName' - The public DNS name of the load balancer.
newAwsElbv2LoadBalancerDetails ::
  AwsElbv2LoadBalancerDetails
newAwsElbv2LoadBalancerDetails =
  AwsElbv2LoadBalancerDetails'
    { state =
        Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      loadBalancerAttributes = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      canonicalHostedZoneId = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      scheme = Prelude.Nothing,
      type' = Prelude.Nothing,
      dNSName = Prelude.Nothing
    }

-- | The state of the load balancer.
awsElbv2LoadBalancerDetails_state :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe LoadBalancerState)
awsElbv2LoadBalancerDetails_state = Lens.lens (\AwsElbv2LoadBalancerDetails' {state} -> state) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {state = a} :: AwsElbv2LoadBalancerDetails)

-- | The IDs of the security groups for the load balancer.
awsElbv2LoadBalancerDetails_securityGroups :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe [Prelude.Text])
awsElbv2LoadBalancerDetails_securityGroups = Lens.lens (\AwsElbv2LoadBalancerDetails' {securityGroups} -> securityGroups) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {securityGroups = a} :: AwsElbv2LoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | Attributes of the load balancer.
awsElbv2LoadBalancerDetails_loadBalancerAttributes :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe [AwsElbv2LoadBalancerAttribute])
awsElbv2LoadBalancerDetails_loadBalancerAttributes = Lens.lens (\AwsElbv2LoadBalancerDetails' {loadBalancerAttributes} -> loadBalancerAttributes) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {loadBalancerAttributes = a} :: AwsElbv2LoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates when the load balancer was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsElbv2LoadBalancerDetails_createdTime :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerDetails_createdTime = Lens.lens (\AwsElbv2LoadBalancerDetails' {createdTime} -> createdTime) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {createdTime = a} :: AwsElbv2LoadBalancerDetails)

-- | The ID of the VPC for the load balancer.
awsElbv2LoadBalancerDetails_vpcId :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerDetails_vpcId = Lens.lens (\AwsElbv2LoadBalancerDetails' {vpcId} -> vpcId) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {vpcId = a} :: AwsElbv2LoadBalancerDetails)

-- | The ID of the Amazon Route 53 hosted zone associated with the load
-- balancer.
awsElbv2LoadBalancerDetails_canonicalHostedZoneId :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerDetails_canonicalHostedZoneId = Lens.lens (\AwsElbv2LoadBalancerDetails' {canonicalHostedZoneId} -> canonicalHostedZoneId) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {canonicalHostedZoneId = a} :: AwsElbv2LoadBalancerDetails)

-- | The Availability Zones for the load balancer.
awsElbv2LoadBalancerDetails_availabilityZones :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe [AvailabilityZone])
awsElbv2LoadBalancerDetails_availabilityZones = Lens.lens (\AwsElbv2LoadBalancerDetails' {availabilityZones} -> availabilityZones) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {availabilityZones = a} :: AwsElbv2LoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | The type of IP addresses used by the subnets for your load balancer. The
-- possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for
-- IPv4 and IPv6 addresses).
awsElbv2LoadBalancerDetails_ipAddressType :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerDetails_ipAddressType = Lens.lens (\AwsElbv2LoadBalancerDetails' {ipAddressType} -> ipAddressType) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {ipAddressType = a} :: AwsElbv2LoadBalancerDetails)

-- | The nodes of an Internet-facing load balancer have public IP addresses.
awsElbv2LoadBalancerDetails_scheme :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerDetails_scheme = Lens.lens (\AwsElbv2LoadBalancerDetails' {scheme} -> scheme) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {scheme = a} :: AwsElbv2LoadBalancerDetails)

-- | The type of load balancer.
awsElbv2LoadBalancerDetails_type :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerDetails_type = Lens.lens (\AwsElbv2LoadBalancerDetails' {type'} -> type') (\s@AwsElbv2LoadBalancerDetails' {} a -> s {type' = a} :: AwsElbv2LoadBalancerDetails)

-- | The public DNS name of the load balancer.
awsElbv2LoadBalancerDetails_dNSName :: Lens.Lens' AwsElbv2LoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbv2LoadBalancerDetails_dNSName = Lens.lens (\AwsElbv2LoadBalancerDetails' {dNSName} -> dNSName) (\s@AwsElbv2LoadBalancerDetails' {} a -> s {dNSName = a} :: AwsElbv2LoadBalancerDetails)

instance Core.FromJSON AwsElbv2LoadBalancerDetails where
  parseJSON =
    Core.withObject
      "AwsElbv2LoadBalancerDetails"
      ( \x ->
          AwsElbv2LoadBalancerDetails'
            Prelude.<$> (x Core..:? "State")
            Prelude.<*> (x Core..:? "SecurityGroups" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "LoadBalancerAttributes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> (x Core..:? "CanonicalHostedZoneId")
            Prelude.<*> ( x Core..:? "AvailabilityZones"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "IpAddressType")
            Prelude.<*> (x Core..:? "Scheme")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "DNSName")
      )

instance Prelude.Hashable AwsElbv2LoadBalancerDetails

instance Prelude.NFData AwsElbv2LoadBalancerDetails

instance Core.ToJSON AwsElbv2LoadBalancerDetails where
  toJSON AwsElbv2LoadBalancerDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("State" Core..=) Prelude.<$> state,
            ("SecurityGroups" Core..=)
              Prelude.<$> securityGroups,
            ("LoadBalancerAttributes" Core..=)
              Prelude.<$> loadBalancerAttributes,
            ("CreatedTime" Core..=) Prelude.<$> createdTime,
            ("VpcId" Core..=) Prelude.<$> vpcId,
            ("CanonicalHostedZoneId" Core..=)
              Prelude.<$> canonicalHostedZoneId,
            ("AvailabilityZones" Core..=)
              Prelude.<$> availabilityZones,
            ("IpAddressType" Core..=) Prelude.<$> ipAddressType,
            ("Scheme" Core..=) Prelude.<$> scheme,
            ("Type" Core..=) Prelude.<$> type',
            ("DNSName" Core..=) Prelude.<$> dNSName
          ]
      )
