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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The list of Availability Zones for the load balancer.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | Information about the configuration of the EC2 instances.
    backendServerDescriptions :: Prelude.Maybe [AwsElbLoadBalancerBackendServerDescription],
    -- | The name of the Amazon Route 53 hosted zone for the load balancer.
    canonicalHostedZoneName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Route 53 hosted zone for the load balancer.
    canonicalHostedZoneNameID :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the load balancer was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The DNS name of the load balancer.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | Information about the health checks that are conducted on the load
    -- balancer.
    healthCheck :: Prelude.Maybe AwsElbLoadBalancerHealthCheck,
    -- | List of EC2 instances for the load balancer.
    instances :: Prelude.Maybe [AwsElbLoadBalancerInstance],
    -- | The policies that are enabled for the load balancer listeners.
    listenerDescriptions :: Prelude.Maybe [AwsElbLoadBalancerListenerDescription],
    -- | The attributes for a load balancer.
    loadBalancerAttributes :: Prelude.Maybe AwsElbLoadBalancerAttributes,
    -- | The name of the load balancer.
    loadBalancerName :: Prelude.Maybe Prelude.Text,
    -- | The policies for a load balancer.
    policies :: Prelude.Maybe AwsElbLoadBalancerPolicies,
    -- | The type of load balancer. Only provided if the load balancer is in a
    -- VPC.
    --
    -- If @Scheme@ is @internet-facing@, the load balancer has a public DNS
    -- name that resolves to a public IP address.
    --
    -- If @Scheme@ is @internal@, the load balancer has a public DNS name that
    -- resolves to a private IP address.
    scheme :: Prelude.Maybe Prelude.Text,
    -- | The security groups for the load balancer. Only provided if the load
    -- balancer is in a VPC.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Information about the security group for the load balancer. This is the
    -- security group that is used for inbound rules.
    sourceSecurityGroup :: Prelude.Maybe AwsElbLoadBalancerSourceSecurityGroup,
    -- | The list of subnet identifiers for the load balancer.
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the VPC for the load balancer.
    vpcId :: Prelude.Maybe Prelude.Text
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
-- 'availabilityZones', 'awsElbLoadBalancerDetails_availabilityZones' - The list of Availability Zones for the load balancer.
--
-- 'backendServerDescriptions', 'awsElbLoadBalancerDetails_backendServerDescriptions' - Information about the configuration of the EC2 instances.
--
-- 'canonicalHostedZoneName', 'awsElbLoadBalancerDetails_canonicalHostedZoneName' - The name of the Amazon Route 53 hosted zone for the load balancer.
--
-- 'canonicalHostedZoneNameID', 'awsElbLoadBalancerDetails_canonicalHostedZoneNameID' - The ID of the Amazon Route 53 hosted zone for the load balancer.
--
-- 'createdTime', 'awsElbLoadBalancerDetails_createdTime' - Indicates when the load balancer was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'dnsName', 'awsElbLoadBalancerDetails_dnsName' - The DNS name of the load balancer.
--
-- 'healthCheck', 'awsElbLoadBalancerDetails_healthCheck' - Information about the health checks that are conducted on the load
-- balancer.
--
-- 'instances', 'awsElbLoadBalancerDetails_instances' - List of EC2 instances for the load balancer.
--
-- 'listenerDescriptions', 'awsElbLoadBalancerDetails_listenerDescriptions' - The policies that are enabled for the load balancer listeners.
--
-- 'loadBalancerAttributes', 'awsElbLoadBalancerDetails_loadBalancerAttributes' - The attributes for a load balancer.
--
-- 'loadBalancerName', 'awsElbLoadBalancerDetails_loadBalancerName' - The name of the load balancer.
--
-- 'policies', 'awsElbLoadBalancerDetails_policies' - The policies for a load balancer.
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
-- 'securityGroups', 'awsElbLoadBalancerDetails_securityGroups' - The security groups for the load balancer. Only provided if the load
-- balancer is in a VPC.
--
-- 'sourceSecurityGroup', 'awsElbLoadBalancerDetails_sourceSecurityGroup' - Information about the security group for the load balancer. This is the
-- security group that is used for inbound rules.
--
-- 'subnets', 'awsElbLoadBalancerDetails_subnets' - The list of subnet identifiers for the load balancer.
--
-- 'vpcId', 'awsElbLoadBalancerDetails_vpcId' - The identifier of the VPC for the load balancer.
newAwsElbLoadBalancerDetails ::
  AwsElbLoadBalancerDetails
newAwsElbLoadBalancerDetails =
  AwsElbLoadBalancerDetails'
    { availabilityZones =
        Prelude.Nothing,
      backendServerDescriptions = Prelude.Nothing,
      canonicalHostedZoneName = Prelude.Nothing,
      canonicalHostedZoneNameID = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      healthCheck = Prelude.Nothing,
      instances = Prelude.Nothing,
      listenerDescriptions = Prelude.Nothing,
      loadBalancerAttributes = Prelude.Nothing,
      loadBalancerName = Prelude.Nothing,
      policies = Prelude.Nothing,
      scheme = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      sourceSecurityGroup = Prelude.Nothing,
      subnets = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The list of Availability Zones for the load balancer.
awsElbLoadBalancerDetails_availabilityZones :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe [Prelude.Text])
awsElbLoadBalancerDetails_availabilityZones = Lens.lens (\AwsElbLoadBalancerDetails' {availabilityZones} -> availabilityZones) (\s@AwsElbLoadBalancerDetails' {} a -> s {availabilityZones = a} :: AwsElbLoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about the configuration of the EC2 instances.
awsElbLoadBalancerDetails_backendServerDescriptions :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe [AwsElbLoadBalancerBackendServerDescription])
awsElbLoadBalancerDetails_backendServerDescriptions = Lens.lens (\AwsElbLoadBalancerDetails' {backendServerDescriptions} -> backendServerDescriptions) (\s@AwsElbLoadBalancerDetails' {} a -> s {backendServerDescriptions = a} :: AwsElbLoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Amazon Route 53 hosted zone for the load balancer.
awsElbLoadBalancerDetails_canonicalHostedZoneName :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerDetails_canonicalHostedZoneName = Lens.lens (\AwsElbLoadBalancerDetails' {canonicalHostedZoneName} -> canonicalHostedZoneName) (\s@AwsElbLoadBalancerDetails' {} a -> s {canonicalHostedZoneName = a} :: AwsElbLoadBalancerDetails)

-- | The ID of the Amazon Route 53 hosted zone for the load balancer.
awsElbLoadBalancerDetails_canonicalHostedZoneNameID :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerDetails_canonicalHostedZoneNameID = Lens.lens (\AwsElbLoadBalancerDetails' {canonicalHostedZoneNameID} -> canonicalHostedZoneNameID) (\s@AwsElbLoadBalancerDetails' {} a -> s {canonicalHostedZoneNameID = a} :: AwsElbLoadBalancerDetails)

-- | Indicates when the load balancer was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsElbLoadBalancerDetails_createdTime :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerDetails_createdTime = Lens.lens (\AwsElbLoadBalancerDetails' {createdTime} -> createdTime) (\s@AwsElbLoadBalancerDetails' {} a -> s {createdTime = a} :: AwsElbLoadBalancerDetails)

-- | The DNS name of the load balancer.
awsElbLoadBalancerDetails_dnsName :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerDetails_dnsName = Lens.lens (\AwsElbLoadBalancerDetails' {dnsName} -> dnsName) (\s@AwsElbLoadBalancerDetails' {} a -> s {dnsName = a} :: AwsElbLoadBalancerDetails)

-- | Information about the health checks that are conducted on the load
-- balancer.
awsElbLoadBalancerDetails_healthCheck :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe AwsElbLoadBalancerHealthCheck)
awsElbLoadBalancerDetails_healthCheck = Lens.lens (\AwsElbLoadBalancerDetails' {healthCheck} -> healthCheck) (\s@AwsElbLoadBalancerDetails' {} a -> s {healthCheck = a} :: AwsElbLoadBalancerDetails)

-- | List of EC2 instances for the load balancer.
awsElbLoadBalancerDetails_instances :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe [AwsElbLoadBalancerInstance])
awsElbLoadBalancerDetails_instances = Lens.lens (\AwsElbLoadBalancerDetails' {instances} -> instances) (\s@AwsElbLoadBalancerDetails' {} a -> s {instances = a} :: AwsElbLoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | The policies that are enabled for the load balancer listeners.
awsElbLoadBalancerDetails_listenerDescriptions :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe [AwsElbLoadBalancerListenerDescription])
awsElbLoadBalancerDetails_listenerDescriptions = Lens.lens (\AwsElbLoadBalancerDetails' {listenerDescriptions} -> listenerDescriptions) (\s@AwsElbLoadBalancerDetails' {} a -> s {listenerDescriptions = a} :: AwsElbLoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | The attributes for a load balancer.
awsElbLoadBalancerDetails_loadBalancerAttributes :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe AwsElbLoadBalancerAttributes)
awsElbLoadBalancerDetails_loadBalancerAttributes = Lens.lens (\AwsElbLoadBalancerDetails' {loadBalancerAttributes} -> loadBalancerAttributes) (\s@AwsElbLoadBalancerDetails' {} a -> s {loadBalancerAttributes = a} :: AwsElbLoadBalancerDetails)

-- | The name of the load balancer.
awsElbLoadBalancerDetails_loadBalancerName :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerDetails_loadBalancerName = Lens.lens (\AwsElbLoadBalancerDetails' {loadBalancerName} -> loadBalancerName) (\s@AwsElbLoadBalancerDetails' {} a -> s {loadBalancerName = a} :: AwsElbLoadBalancerDetails)

-- | The policies for a load balancer.
awsElbLoadBalancerDetails_policies :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe AwsElbLoadBalancerPolicies)
awsElbLoadBalancerDetails_policies = Lens.lens (\AwsElbLoadBalancerDetails' {policies} -> policies) (\s@AwsElbLoadBalancerDetails' {} a -> s {policies = a} :: AwsElbLoadBalancerDetails)

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

-- | The security groups for the load balancer. Only provided if the load
-- balancer is in a VPC.
awsElbLoadBalancerDetails_securityGroups :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe [Prelude.Text])
awsElbLoadBalancerDetails_securityGroups = Lens.lens (\AwsElbLoadBalancerDetails' {securityGroups} -> securityGroups) (\s@AwsElbLoadBalancerDetails' {} a -> s {securityGroups = a} :: AwsElbLoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about the security group for the load balancer. This is the
-- security group that is used for inbound rules.
awsElbLoadBalancerDetails_sourceSecurityGroup :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe AwsElbLoadBalancerSourceSecurityGroup)
awsElbLoadBalancerDetails_sourceSecurityGroup = Lens.lens (\AwsElbLoadBalancerDetails' {sourceSecurityGroup} -> sourceSecurityGroup) (\s@AwsElbLoadBalancerDetails' {} a -> s {sourceSecurityGroup = a} :: AwsElbLoadBalancerDetails)

-- | The list of subnet identifiers for the load balancer.
awsElbLoadBalancerDetails_subnets :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe [Prelude.Text])
awsElbLoadBalancerDetails_subnets = Lens.lens (\AwsElbLoadBalancerDetails' {subnets} -> subnets) (\s@AwsElbLoadBalancerDetails' {} a -> s {subnets = a} :: AwsElbLoadBalancerDetails) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the VPC for the load balancer.
awsElbLoadBalancerDetails_vpcId :: Lens.Lens' AwsElbLoadBalancerDetails (Prelude.Maybe Prelude.Text)
awsElbLoadBalancerDetails_vpcId = Lens.lens (\AwsElbLoadBalancerDetails' {vpcId} -> vpcId) (\s@AwsElbLoadBalancerDetails' {} a -> s {vpcId = a} :: AwsElbLoadBalancerDetails)

instance Data.FromJSON AwsElbLoadBalancerDetails where
  parseJSON =
    Data.withObject
      "AwsElbLoadBalancerDetails"
      ( \x ->
          AwsElbLoadBalancerDetails'
            Prelude.<$> ( x
                            Data..:? "AvailabilityZones"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "BackendServerDescriptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CanonicalHostedZoneName")
            Prelude.<*> (x Data..:? "CanonicalHostedZoneNameID")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "DnsName")
            Prelude.<*> (x Data..:? "HealthCheck")
            Prelude.<*> (x Data..:? "Instances" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "ListenerDescriptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "LoadBalancerAttributes")
            Prelude.<*> (x Data..:? "LoadBalancerName")
            Prelude.<*> (x Data..:? "Policies")
            Prelude.<*> (x Data..:? "Scheme")
            Prelude.<*> (x Data..:? "SecurityGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SourceSecurityGroup")
            Prelude.<*> (x Data..:? "Subnets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable AwsElbLoadBalancerDetails where
  hashWithSalt _salt AwsElbLoadBalancerDetails' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` backendServerDescriptions
      `Prelude.hashWithSalt` canonicalHostedZoneName
      `Prelude.hashWithSalt` canonicalHostedZoneNameID
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` healthCheck
      `Prelude.hashWithSalt` instances
      `Prelude.hashWithSalt` listenerDescriptions
      `Prelude.hashWithSalt` loadBalancerAttributes
      `Prelude.hashWithSalt` loadBalancerName
      `Prelude.hashWithSalt` policies
      `Prelude.hashWithSalt` scheme
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` sourceSecurityGroup
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData AwsElbLoadBalancerDetails where
  rnf AwsElbLoadBalancerDetails' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf backendServerDescriptions
      `Prelude.seq` Prelude.rnf canonicalHostedZoneName
      `Prelude.seq` Prelude.rnf canonicalHostedZoneNameID
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf healthCheck
      `Prelude.seq` Prelude.rnf instances
      `Prelude.seq` Prelude.rnf listenerDescriptions
      `Prelude.seq` Prelude.rnf loadBalancerAttributes
      `Prelude.seq` Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf policies
      `Prelude.seq` Prelude.rnf scheme
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf sourceSecurityGroup
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf vpcId

instance Data.ToJSON AwsElbLoadBalancerDetails where
  toJSON AwsElbLoadBalancerDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityZones" Data..=)
              Prelude.<$> availabilityZones,
            ("BackendServerDescriptions" Data..=)
              Prelude.<$> backendServerDescriptions,
            ("CanonicalHostedZoneName" Data..=)
              Prelude.<$> canonicalHostedZoneName,
            ("CanonicalHostedZoneNameID" Data..=)
              Prelude.<$> canonicalHostedZoneNameID,
            ("CreatedTime" Data..=) Prelude.<$> createdTime,
            ("DnsName" Data..=) Prelude.<$> dnsName,
            ("HealthCheck" Data..=) Prelude.<$> healthCheck,
            ("Instances" Data..=) Prelude.<$> instances,
            ("ListenerDescriptions" Data..=)
              Prelude.<$> listenerDescriptions,
            ("LoadBalancerAttributes" Data..=)
              Prelude.<$> loadBalancerAttributes,
            ("LoadBalancerName" Data..=)
              Prelude.<$> loadBalancerName,
            ("Policies" Data..=) Prelude.<$> policies,
            ("Scheme" Data..=) Prelude.<$> scheme,
            ("SecurityGroups" Data..=)
              Prelude.<$> securityGroups,
            ("SourceSecurityGroup" Data..=)
              Prelude.<$> sourceSecurityGroup,
            ("Subnets" Data..=) Prelude.<$> subnets,
            ("VpcId" Data..=) Prelude.<$> vpcId
          ]
      )
