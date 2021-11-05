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
-- Module      : Amazonka.SecurityHub.Types.AwsElbLoadBalancerDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerAttributes
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerBackendServerDescription
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerHealthCheck
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerInstance
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerListenerDescription
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerPolicies
import Amazonka.SecurityHub.Types.AwsElbLoadBalancerSourceSecurityGroup

-- | Contains details about a Classic Load Balancer.
--
-- /See:/ 'newAwsElbLoadBalancerDetails' smart constructor.
data AwsElbLoadBalancerDetails = AwsElbLoadBalancerDetails'
  { -- | Information about the security group for the load balancer. This is the
    -- security group that is used for inbound rules.
    sourceSecurityGroup :: Prelude.Maybe AwsElbLoadBalancerSourceSecurityGroup,
    -- | The name of the Amazon Route 53 hosted zone for the load balancer.
    canonicalHostedZoneName :: Prelude.Maybe Prelude.Text,
    -- | The security groups for the load balancer. Only provided if the load
    -- balancer is in a VPC.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Information about the health checks that are conducted on the load
    -- balancer.
    healthCheck :: Prelude.Maybe AwsElbLoadBalancerHealthCheck,
    -- | The name of the load balancer.
    loadBalancerName :: Prelude.Maybe Prelude.Text,
    -- | The attributes for a load balancer.
    loadBalancerAttributes :: Prelude.Maybe AwsElbLoadBalancerAttributes,
    -- | Indicates when the load balancer was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the VPC for the load balancer.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The list of subnet identifiers for the load balancer.
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | The list of Availability Zones for the load balancer.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | Information about the configuration of the EC2 instances.
    backendServerDescriptions :: Prelude.Maybe [AwsElbLoadBalancerBackendServerDescription],
    -- | The ID of the Amazon Route 53 hosted zone for the load balancer.
    canonicalHostedZoneNameID :: Prelude.Maybe Prelude.Text,
    -- | List of EC2 instances for the load balancer.
    instances :: Prelude.Maybe [AwsElbLoadBalancerInstance],
    -- | The type of load balancer. Only provided if the load balancer is in a
    -- VPC.
    --
    -- If @Scheme@ is @internet-facing@, the load balancer has a public DNS
    -- name that resolves to a public IP address.
    --
    -- If @Scheme@ is @internal@, the load balancer has a public DNS name that
    -- resolves to a private IP address.
    scheme :: Prelude.Maybe Prelude.Text,
    -- | The policies that are enabled for the load balancer listeners.
    listenerDescriptions :: Prelude.Maybe [AwsElbLoadBalancerListenerDescription],
    -- | The DNS name of the load balancer.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | The policies for a load balancer.
    policies :: Prelude.Maybe AwsElbLoadBalancerPolicies
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbLoadBalancerDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceSecurityGroup', 'awsElbLoadBalancerDetails_sourceSecurityGroup' - Information about the security group for the load balancer. This is the
-- security group that is used for inbound rules.
--
-- 'canonicalHostedZoneName', 'awsElbLoadBalancerDetails_canonicalHostedZoneName' - The name of the Amazon Route 53 hosted zone for the load balancer.
--
-- 'securityGroups', 'awsElbLoadBalancerDetails_securityGroups' - The security groups for the load balancer. Only provided if the load
-- balancer is in a VPC.
--
-- 'healthCheck', 'awsElbLoadBalancerDetails_healthCheck' - Information about the health checks that are conducted on the load
-- balancer.
--
-- 'loadBalancerName', 'awsElbLoadBalancerDetails_loadBalancerName' - The name of the load balancer.
--
-- 'loadBalancerAttributes', 'awsElbLoadBalancerDetails_loadBalancerAttributes' - The attributes for a load balancer.
--
-- 'createdTime', 'awsElbLoadBalancerDetails_createdTime' - Indicates when the load balancer was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'vpcId', 'awsElbLoadBalancerDetails_vpcId' - The identifier of the VPC for the load balancer.
--
-- 'subnets', 'awsElbLoadBalancerDetails_subnets' - The list of subnet identifiers for the load balancer.
--
-- 'availabilityZones', 'awsElbLoadBalancerDetails_availabilityZones' - The list of Availability Zones for the load balancer.
--
-- 'backendServerDescriptions', 'awsElbLoadBalancerDetails_backendServerDescriptions' - Information about the configuration of the EC2 instances.
--
-- 'canonicalHostedZoneNameID', 'awsElbLoadBalancerDetails_canonicalHostedZoneNameID' - The ID of the Amazon Route 53 hosted zone for the load balancer.
--
-- 'instances', 'awsElbLoadBalancerDetails_instances' - List of EC2 instances for the load balancer.
--
-- 'scheme', 'awsElbLoadBalancerDetails_scheme' - The type of load balancer. Only provided if the load balancer is in a
-- VPC.
--
-- If @Scheme@ is @internet-facing@, the load balancer has a public DNS
-- name that resolves to a public IP address.
--
-- If @Scheme@ is @internal@, the load balancer has a public DNS name that
-- resolves to a private IP address.
--
-- 'listenerDescriptions', 'awsElbLoadBalancerDetails_listenerDescriptions' - The policies that are enabled for the load balancer listeners.
--
-- 'dnsName', 'awsElbLoadBalancerDetails_dnsName' - The DNS name of the load balancer.
--
-- 'policies', 'awsElbLoadBalancerDetails_policies' - The policies for a load balancer.
newAwsElbLoadBalancerDetails ::
  AwsElbLoadBalancerDetails
newAwsElbLoadBalancerDetails =
  AwsElbLoadBalancerDetails'
    { sourceSecurityGroup =
        Prelude.Nothing,
      canonicalHostedZoneName = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      healthCheck = Prelude.Nothing,
      loadBalancerName = Prelude.Nothing,
      loadBalancerAttributes = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      subnets = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      backendServerDescriptions = Prelude.Nothing,
      canonicalHostedZoneNameID = Prelude.Nothing,
      instances = Prelude.Nothing,
      scheme = Prelude.Nothing,
      listenerDescriptions = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      policies = Prelude.Nothing
    }

-- | Information about the security group for the load balancer. This is the
-- security group that is used for inbound rules.
awsElbLoadBalancerDetails_sourceSecurityGroup :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe AwsElbLoadBalancerSourceSecurityGroup)
awsElbLoadBalancerDetails_sourceSecurityGroup = Lens.lens (\AwsElbLoadBalancerDetails' {sourceSecurityGroup} -> sourceSecurityGroup) (\s@AwsElbLoadBalancerDetails' {} a -> s {sourceSecurityGroup = a} :: AwsElbLoadBalancerDetails)

-- | The name of the Amazon Route 53 hosted zone for the load balancer.
awsElbLoadBalancerDetails_canonicalHostedZoneName :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerDetails_canonicalHostedZoneName = Lens.lens (\AwsElbLoadBalancerDetails' {canonicalHostedZoneName} -> canonicalHostedZoneName) (\s@AwsElbLoadBalancerDetails' {} a -> s {canonicalHostedZoneName = a} :: AwsElbLoadBalancerDetails)

-- | The security groups for the load balancer. Only provided if the load
-- balancer is in a VPC.
awsElbLoadBalancerDetails_securityGroups :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe [Prelude.Text])
awsElbLoadBalancerDetails_securityGroups = Lens.lens (\AwsElbLoadBalancerDetails' {securityGroups} -> securityGroups) (\s@AwsElbLoadBalancerDetails' {} a -> s {securityGroups = a} :: AwsElbLoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about the health checks that are conducted on the load
-- balancer.
awsElbLoadBalancerDetails_healthCheck :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe AwsElbLoadBalancerHealthCheck)
awsElbLoadBalancerDetails_healthCheck = Lens.lens (\AwsElbLoadBalancerDetails' {healthCheck} -> healthCheck) (\s@AwsElbLoadBalancerDetails' {} a -> s {healthCheck = a} :: AwsElbLoadBalancerDetails)

-- | The name of the load balancer.
awsElbLoadBalancerDetails_loadBalancerName :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerDetails_loadBalancerName = Lens.lens (\AwsElbLoadBalancerDetails' {loadBalancerName} -> loadBalancerName) (\s@AwsElbLoadBalancerDetails' {} a -> s {loadBalancerName = a} :: AwsElbLoadBalancerDetails)

-- | The attributes for a load balancer.
awsElbLoadBalancerDetails_loadBalancerAttributes :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe AwsElbLoadBalancerAttributes)
awsElbLoadBalancerDetails_loadBalancerAttributes = Lens.lens (\AwsElbLoadBalancerDetails' {loadBalancerAttributes} -> loadBalancerAttributes) (\s@AwsElbLoadBalancerDetails' {} a -> s {loadBalancerAttributes = a} :: AwsElbLoadBalancerDetails)

-- | Indicates when the load balancer was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsElbLoadBalancerDetails_createdTime :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerDetails_createdTime = Lens.lens (\AwsElbLoadBalancerDetails' {createdTime} -> createdTime) (\s@AwsElbLoadBalancerDetails' {} a -> s {createdTime = a} :: AwsElbLoadBalancerDetails)

-- | The identifier of the VPC for the load balancer.
awsElbLoadBalancerDetails_vpcId :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerDetails_vpcId = Lens.lens (\AwsElbLoadBalancerDetails' {vpcId} -> vpcId) (\s@AwsElbLoadBalancerDetails' {} a -> s {vpcId = a} :: AwsElbLoadBalancerDetails)

-- | The list of subnet identifiers for the load balancer.
awsElbLoadBalancerDetails_subnets :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe [Prelude.Text])
awsElbLoadBalancerDetails_subnets = Lens.lens (\AwsElbLoadBalancerDetails' {subnets} -> subnets) (\s@AwsElbLoadBalancerDetails' {} a -> s {subnets = a} :: AwsElbLoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | The list of Availability Zones for the load balancer.
awsElbLoadBalancerDetails_availabilityZones :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe [Prelude.Text])
awsElbLoadBalancerDetails_availabilityZones = Lens.lens (\AwsElbLoadBalancerDetails' {availabilityZones} -> availabilityZones) (\s@AwsElbLoadBalancerDetails' {} a -> s {availabilityZones = a} :: AwsElbLoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about the configuration of the EC2 instances.
awsElbLoadBalancerDetails_backendServerDescriptions :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe [AwsElbLoadBalancerBackendServerDescription])
awsElbLoadBalancerDetails_backendServerDescriptions = Lens.lens (\AwsElbLoadBalancerDetails' {backendServerDescriptions} -> backendServerDescriptions) (\s@AwsElbLoadBalancerDetails' {} a -> s {backendServerDescriptions = a} :: AwsElbLoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Route 53 hosted zone for the load balancer.
awsElbLoadBalancerDetails_canonicalHostedZoneNameID :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerDetails_canonicalHostedZoneNameID = Lens.lens (\AwsElbLoadBalancerDetails' {canonicalHostedZoneNameID} -> canonicalHostedZoneNameID) (\s@AwsElbLoadBalancerDetails' {} a -> s {canonicalHostedZoneNameID = a} :: AwsElbLoadBalancerDetails)

-- | List of EC2 instances for the load balancer.
awsElbLoadBalancerDetails_instances :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe [AwsElbLoadBalancerInstance])
awsElbLoadBalancerDetails_instances = Lens.lens (\AwsElbLoadBalancerDetails' {instances} -> instances) (\s@AwsElbLoadBalancerDetails' {} a -> s {instances = a} :: AwsElbLoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | The type of load balancer. Only provided if the load balancer is in a
-- VPC.
--
-- If @Scheme@ is @internet-facing@, the load balancer has a public DNS
-- name that resolves to a public IP address.
--
-- If @Scheme@ is @internal@, the load balancer has a public DNS name that
-- resolves to a private IP address.
awsElbLoadBalancerDetails_scheme :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerDetails_scheme = Lens.lens (\AwsElbLoadBalancerDetails' {scheme} -> scheme) (\s@AwsElbLoadBalancerDetails' {} a -> s {scheme = a} :: AwsElbLoadBalancerDetails)

-- | The policies that are enabled for the load balancer listeners.
awsElbLoadBalancerDetails_listenerDescriptions :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe [AwsElbLoadBalancerListenerDescription])
awsElbLoadBalancerDetails_listenerDescriptions = Lens.lens (\AwsElbLoadBalancerDetails' {listenerDescriptions} -> listenerDescriptions) (\s@AwsElbLoadBalancerDetails' {} a -> s {listenerDescriptions = a} :: AwsElbLoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | The DNS name of the load balancer.
awsElbLoadBalancerDetails_dnsName :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerDetails_dnsName = Lens.lens (\AwsElbLoadBalancerDetails' {dnsName} -> dnsName) (\s@AwsElbLoadBalancerDetails' {} a -> s {dnsName = a} :: AwsElbLoadBalancerDetails)

-- | The policies for a load balancer.
awsElbLoadBalancerDetails_policies :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe AwsElbLoadBalancerPolicies)
awsElbLoadBalancerDetails_policies = Lens.lens (\AwsElbLoadBalancerDetails' {policies} -> policies) (\s@AwsElbLoadBalancerDetails' {} a -> s {policies = a} :: AwsElbLoadBalancerDetails)

instance Core.FromJSON AwsElbLoadBalancerDetails where
  parseJSON =
    Core.withObject
      "AwsElbLoadBalancerDetails"
      ( \x ->
          AwsElbLoadBalancerDetails'
            Prelude.<$> (x Core..:? "SourceSecurityGroup")
            Prelude.<*> (x Core..:? "CanonicalHostedZoneName")
            Prelude.<*> (x Core..:? "SecurityGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "HealthCheck")
            Prelude.<*> (x Core..:? "LoadBalancerName")
            Prelude.<*> (x Core..:? "LoadBalancerAttributes")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> (x Core..:? "Subnets" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "AvailabilityZones"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "BackendServerDescriptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CanonicalHostedZoneNameID")
            Prelude.<*> (x Core..:? "Instances" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Scheme")
            Prelude.<*> ( x Core..:? "ListenerDescriptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DnsName")
            Prelude.<*> (x Core..:? "Policies")
      )

instance Prelude.Hashable AwsElbLoadBalancerDetails

instance Prelude.NFData AwsElbLoadBalancerDetails

instance Core.ToJSON AwsElbLoadBalancerDetails where
  toJSON AwsElbLoadBalancerDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SourceSecurityGroup" Core..=)
              Prelude.<$> sourceSecurityGroup,
            ("CanonicalHostedZoneName" Core..=)
              Prelude.<$> canonicalHostedZoneName,
            ("SecurityGroups" Core..=)
              Prelude.<$> securityGroups,
            ("HealthCheck" Core..=) Prelude.<$> healthCheck,
            ("LoadBalancerName" Core..=)
              Prelude.<$> loadBalancerName,
            ("LoadBalancerAttributes" Core..=)
              Prelude.<$> loadBalancerAttributes,
            ("CreatedTime" Core..=) Prelude.<$> createdTime,
            ("VpcId" Core..=) Prelude.<$> vpcId,
            ("Subnets" Core..=) Prelude.<$> subnets,
            ("AvailabilityZones" Core..=)
              Prelude.<$> availabilityZones,
            ("BackendServerDescriptions" Core..=)
              Prelude.<$> backendServerDescriptions,
            ("CanonicalHostedZoneNameID" Core..=)
              Prelude.<$> canonicalHostedZoneNameID,
            ("Instances" Core..=) Prelude.<$> instances,
            ("Scheme" Core..=) Prelude.<$> scheme,
            ("ListenerDescriptions" Core..=)
              Prelude.<$> listenerDescriptions,
            ("DnsName" Core..=) Prelude.<$> dnsName,
            ("Policies" Core..=) Prelude.<$> policies
          ]
      )
