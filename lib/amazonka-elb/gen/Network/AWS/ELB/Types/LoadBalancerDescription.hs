{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.LoadBalancerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.LoadBalancerDescription where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.BackendServerDescription
import Network.AWS.ELB.Types.HealthCheck
import Network.AWS.ELB.Types.Instance
import Network.AWS.ELB.Types.ListenerDescription
import Network.AWS.ELB.Types.Policies
import Network.AWS.ELB.Types.SourceSecurityGroup
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a load balancer.
--
--
--
-- /See:/ 'loadBalancerDescription' smart constructor.
data LoadBalancerDescription = LoadBalancerDescription'
  { _lbdSourceSecurityGroup ::
      !(Maybe SourceSecurityGroup),
    _lbdCanonicalHostedZoneName ::
      !(Maybe Text),
    _lbdSecurityGroups :: !(Maybe [Text]),
    _lbdHealthCheck :: !(Maybe HealthCheck),
    _lbdLoadBalancerName :: !(Maybe Text),
    _lbdCreatedTime :: !(Maybe ISO8601),
    _lbdVPCId :: !(Maybe Text),
    _lbdSubnets :: !(Maybe [Text]),
    _lbdAvailabilityZones :: !(Maybe [Text]),
    _lbdBackendServerDescriptions ::
      !(Maybe [BackendServerDescription]),
    _lbdCanonicalHostedZoneNameId ::
      !(Maybe Text),
    _lbdInstances :: !(Maybe [Instance]),
    _lbdScheme :: !(Maybe Text),
    _lbdListenerDescriptions ::
      !(Maybe [ListenerDescription]),
    _lbdDNSName :: !(Maybe Text),
    _lbdPolicies :: !(Maybe Policies)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadBalancerDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbdSourceSecurityGroup' - The security group for the load balancer, which you can use as part of your inbound rules for your registered instances. To only allow traffic from load balancers, add a security group rule that specifies this source security group as the inbound source.
--
-- * 'lbdCanonicalHostedZoneName' - The DNS name of the load balancer. For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/using-domain-names-with-elb.html Configure a Custom Domain Name> in the /Classic Load Balancers Guide/ .
--
-- * 'lbdSecurityGroups' - The security groups for the load balancer. Valid only for load balancers in a VPC.
--
-- * 'lbdHealthCheck' - Information about the health checks conducted on the load balancer.
--
-- * 'lbdLoadBalancerName' - The name of the load balancer.
--
-- * 'lbdCreatedTime' - The date and time the load balancer was created.
--
-- * 'lbdVPCId' - The ID of the VPC for the load balancer.
--
-- * 'lbdSubnets' - The IDs of the subnets for the load balancer.
--
-- * 'lbdAvailabilityZones' - The Availability Zones for the load balancer.
--
-- * 'lbdBackendServerDescriptions' - Information about your EC2 instances.
--
-- * 'lbdCanonicalHostedZoneNameId' - The ID of the Amazon Route 53 hosted zone for the load balancer.
--
-- * 'lbdInstances' - The IDs of the instances for the load balancer.
--
-- * 'lbdScheme' - The type of load balancer. Valid only for load balancers in a VPC. If @Scheme@ is @internet-facing@ , the load balancer has a public DNS name that resolves to a public IP address. If @Scheme@ is @internal@ , the load balancer has a public DNS name that resolves to a private IP address.
--
-- * 'lbdListenerDescriptions' - The listeners for the load balancer.
--
-- * 'lbdDNSName' - The DNS name of the load balancer.
--
-- * 'lbdPolicies' - The policies defined for the load balancer.
loadBalancerDescription ::
  LoadBalancerDescription
loadBalancerDescription =
  LoadBalancerDescription'
    { _lbdSourceSecurityGroup = Nothing,
      _lbdCanonicalHostedZoneName = Nothing,
      _lbdSecurityGroups = Nothing,
      _lbdHealthCheck = Nothing,
      _lbdLoadBalancerName = Nothing,
      _lbdCreatedTime = Nothing,
      _lbdVPCId = Nothing,
      _lbdSubnets = Nothing,
      _lbdAvailabilityZones = Nothing,
      _lbdBackendServerDescriptions = Nothing,
      _lbdCanonicalHostedZoneNameId = Nothing,
      _lbdInstances = Nothing,
      _lbdScheme = Nothing,
      _lbdListenerDescriptions = Nothing,
      _lbdDNSName = Nothing,
      _lbdPolicies = Nothing
    }

-- | The security group for the load balancer, which you can use as part of your inbound rules for your registered instances. To only allow traffic from load balancers, add a security group rule that specifies this source security group as the inbound source.
lbdSourceSecurityGroup :: Lens' LoadBalancerDescription (Maybe SourceSecurityGroup)
lbdSourceSecurityGroup = lens _lbdSourceSecurityGroup (\s a -> s {_lbdSourceSecurityGroup = a})

-- | The DNS name of the load balancer. For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/using-domain-names-with-elb.html Configure a Custom Domain Name> in the /Classic Load Balancers Guide/ .
lbdCanonicalHostedZoneName :: Lens' LoadBalancerDescription (Maybe Text)
lbdCanonicalHostedZoneName = lens _lbdCanonicalHostedZoneName (\s a -> s {_lbdCanonicalHostedZoneName = a})

-- | The security groups for the load balancer. Valid only for load balancers in a VPC.
lbdSecurityGroups :: Lens' LoadBalancerDescription [Text]
lbdSecurityGroups = lens _lbdSecurityGroups (\s a -> s {_lbdSecurityGroups = a}) . _Default . _Coerce

-- | Information about the health checks conducted on the load balancer.
lbdHealthCheck :: Lens' LoadBalancerDescription (Maybe HealthCheck)
lbdHealthCheck = lens _lbdHealthCheck (\s a -> s {_lbdHealthCheck = a})

-- | The name of the load balancer.
lbdLoadBalancerName :: Lens' LoadBalancerDescription (Maybe Text)
lbdLoadBalancerName = lens _lbdLoadBalancerName (\s a -> s {_lbdLoadBalancerName = a})

-- | The date and time the load balancer was created.
lbdCreatedTime :: Lens' LoadBalancerDescription (Maybe UTCTime)
lbdCreatedTime = lens _lbdCreatedTime (\s a -> s {_lbdCreatedTime = a}) . mapping _Time

-- | The ID of the VPC for the load balancer.
lbdVPCId :: Lens' LoadBalancerDescription (Maybe Text)
lbdVPCId = lens _lbdVPCId (\s a -> s {_lbdVPCId = a})

-- | The IDs of the subnets for the load balancer.
lbdSubnets :: Lens' LoadBalancerDescription [Text]
lbdSubnets = lens _lbdSubnets (\s a -> s {_lbdSubnets = a}) . _Default . _Coerce

-- | The Availability Zones for the load balancer.
lbdAvailabilityZones :: Lens' LoadBalancerDescription [Text]
lbdAvailabilityZones = lens _lbdAvailabilityZones (\s a -> s {_lbdAvailabilityZones = a}) . _Default . _Coerce

-- | Information about your EC2 instances.
lbdBackendServerDescriptions :: Lens' LoadBalancerDescription [BackendServerDescription]
lbdBackendServerDescriptions = lens _lbdBackendServerDescriptions (\s a -> s {_lbdBackendServerDescriptions = a}) . _Default . _Coerce

-- | The ID of the Amazon Route 53 hosted zone for the load balancer.
lbdCanonicalHostedZoneNameId :: Lens' LoadBalancerDescription (Maybe Text)
lbdCanonicalHostedZoneNameId = lens _lbdCanonicalHostedZoneNameId (\s a -> s {_lbdCanonicalHostedZoneNameId = a})

-- | The IDs of the instances for the load balancer.
lbdInstances :: Lens' LoadBalancerDescription [Instance]
lbdInstances = lens _lbdInstances (\s a -> s {_lbdInstances = a}) . _Default . _Coerce

-- | The type of load balancer. Valid only for load balancers in a VPC. If @Scheme@ is @internet-facing@ , the load balancer has a public DNS name that resolves to a public IP address. If @Scheme@ is @internal@ , the load balancer has a public DNS name that resolves to a private IP address.
lbdScheme :: Lens' LoadBalancerDescription (Maybe Text)
lbdScheme = lens _lbdScheme (\s a -> s {_lbdScheme = a})

-- | The listeners for the load balancer.
lbdListenerDescriptions :: Lens' LoadBalancerDescription [ListenerDescription]
lbdListenerDescriptions = lens _lbdListenerDescriptions (\s a -> s {_lbdListenerDescriptions = a}) . _Default . _Coerce

-- | The DNS name of the load balancer.
lbdDNSName :: Lens' LoadBalancerDescription (Maybe Text)
lbdDNSName = lens _lbdDNSName (\s a -> s {_lbdDNSName = a})

-- | The policies defined for the load balancer.
lbdPolicies :: Lens' LoadBalancerDescription (Maybe Policies)
lbdPolicies = lens _lbdPolicies (\s a -> s {_lbdPolicies = a})

instance FromXML LoadBalancerDescription where
  parseXML x =
    LoadBalancerDescription'
      <$> (x .@? "SourceSecurityGroup")
      <*> (x .@? "CanonicalHostedZoneName")
      <*> (x .@? "SecurityGroups" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "HealthCheck")
      <*> (x .@? "LoadBalancerName")
      <*> (x .@? "CreatedTime")
      <*> (x .@? "VPCId")
      <*> (x .@? "Subnets" .!@ mempty >>= may (parseXMLList "member"))
      <*> ( x .@? "AvailabilityZones" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> ( x .@? "BackendServerDescriptions" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "CanonicalHostedZoneNameID")
      <*> (x .@? "Instances" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Scheme")
      <*> ( x .@? "ListenerDescriptions" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "DNSName")
      <*> (x .@? "Policies")

instance Hashable LoadBalancerDescription

instance NFData LoadBalancerDescription
