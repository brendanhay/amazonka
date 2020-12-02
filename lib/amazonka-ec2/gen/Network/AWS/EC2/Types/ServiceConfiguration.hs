{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ServiceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ServiceConfiguration where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PrivateDNSNameConfiguration
import Network.AWS.EC2.Types.ServiceState
import Network.AWS.EC2.Types.ServiceTypeDetail
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a service configuration for a VPC endpoint service.
--
--
--
-- /See:/ 'serviceConfiguration' smart constructor.
data ServiceConfiguration = ServiceConfiguration'
  { _scNetworkLoadBalancerARNs ::
      !(Maybe [Text]),
    _scBaseEndpointDNSNames :: !(Maybe [Text]),
    _scAvailabilityZones :: !(Maybe [Text]),
    _scGatewayLoadBalancerARNs :: !(Maybe [Text]),
    _scManagesVPCEndpoints :: !(Maybe Bool),
    _scServiceName :: !(Maybe Text),
    _scServiceState :: !(Maybe ServiceState),
    _scServiceType :: !(Maybe [ServiceTypeDetail]),
    _scAcceptanceRequired :: !(Maybe Bool),
    _scServiceId :: !(Maybe Text),
    _scPrivateDNSName :: !(Maybe Text),
    _scPrivateDNSNameConfiguration ::
      !(Maybe PrivateDNSNameConfiguration),
    _scTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scNetworkLoadBalancerARNs' - The Amazon Resource Names (ARNs) of the Network Load Balancers for the service.
--
-- * 'scBaseEndpointDNSNames' - The DNS names for the service.
--
-- * 'scAvailabilityZones' - The Availability Zones in which the service is available.
--
-- * 'scGatewayLoadBalancerARNs' - The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the service.
--
-- * 'scManagesVPCEndpoints' - Indicates whether the service manages its VPC endpoints. Management of the service VPC endpoints using the VPC endpoint API is restricted.
--
-- * 'scServiceName' - The name of the service.
--
-- * 'scServiceState' - The service state.
--
-- * 'scServiceType' - The type of service.
--
-- * 'scAcceptanceRequired' - Indicates whether requests from other AWS accounts to create an endpoint to the service must first be accepted.
--
-- * 'scServiceId' - The ID of the service.
--
-- * 'scPrivateDNSName' - The private DNS name for the service.
--
-- * 'scPrivateDNSNameConfiguration' - Information about the endpoint service private DNS name configuration.
--
-- * 'scTags' - Any tags assigned to the service.
serviceConfiguration ::
  ServiceConfiguration
serviceConfiguration =
  ServiceConfiguration'
    { _scNetworkLoadBalancerARNs = Nothing,
      _scBaseEndpointDNSNames = Nothing,
      _scAvailabilityZones = Nothing,
      _scGatewayLoadBalancerARNs = Nothing,
      _scManagesVPCEndpoints = Nothing,
      _scServiceName = Nothing,
      _scServiceState = Nothing,
      _scServiceType = Nothing,
      _scAcceptanceRequired = Nothing,
      _scServiceId = Nothing,
      _scPrivateDNSName = Nothing,
      _scPrivateDNSNameConfiguration = Nothing,
      _scTags = Nothing
    }

-- | The Amazon Resource Names (ARNs) of the Network Load Balancers for the service.
scNetworkLoadBalancerARNs :: Lens' ServiceConfiguration [Text]
scNetworkLoadBalancerARNs = lens _scNetworkLoadBalancerARNs (\s a -> s {_scNetworkLoadBalancerARNs = a}) . _Default . _Coerce

-- | The DNS names for the service.
scBaseEndpointDNSNames :: Lens' ServiceConfiguration [Text]
scBaseEndpointDNSNames = lens _scBaseEndpointDNSNames (\s a -> s {_scBaseEndpointDNSNames = a}) . _Default . _Coerce

-- | The Availability Zones in which the service is available.
scAvailabilityZones :: Lens' ServiceConfiguration [Text]
scAvailabilityZones = lens _scAvailabilityZones (\s a -> s {_scAvailabilityZones = a}) . _Default . _Coerce

-- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers for the service.
scGatewayLoadBalancerARNs :: Lens' ServiceConfiguration [Text]
scGatewayLoadBalancerARNs = lens _scGatewayLoadBalancerARNs (\s a -> s {_scGatewayLoadBalancerARNs = a}) . _Default . _Coerce

-- | Indicates whether the service manages its VPC endpoints. Management of the service VPC endpoints using the VPC endpoint API is restricted.
scManagesVPCEndpoints :: Lens' ServiceConfiguration (Maybe Bool)
scManagesVPCEndpoints = lens _scManagesVPCEndpoints (\s a -> s {_scManagesVPCEndpoints = a})

-- | The name of the service.
scServiceName :: Lens' ServiceConfiguration (Maybe Text)
scServiceName = lens _scServiceName (\s a -> s {_scServiceName = a})

-- | The service state.
scServiceState :: Lens' ServiceConfiguration (Maybe ServiceState)
scServiceState = lens _scServiceState (\s a -> s {_scServiceState = a})

-- | The type of service.
scServiceType :: Lens' ServiceConfiguration [ServiceTypeDetail]
scServiceType = lens _scServiceType (\s a -> s {_scServiceType = a}) . _Default . _Coerce

-- | Indicates whether requests from other AWS accounts to create an endpoint to the service must first be accepted.
scAcceptanceRequired :: Lens' ServiceConfiguration (Maybe Bool)
scAcceptanceRequired = lens _scAcceptanceRequired (\s a -> s {_scAcceptanceRequired = a})

-- | The ID of the service.
scServiceId :: Lens' ServiceConfiguration (Maybe Text)
scServiceId = lens _scServiceId (\s a -> s {_scServiceId = a})

-- | The private DNS name for the service.
scPrivateDNSName :: Lens' ServiceConfiguration (Maybe Text)
scPrivateDNSName = lens _scPrivateDNSName (\s a -> s {_scPrivateDNSName = a})

-- | Information about the endpoint service private DNS name configuration.
scPrivateDNSNameConfiguration :: Lens' ServiceConfiguration (Maybe PrivateDNSNameConfiguration)
scPrivateDNSNameConfiguration = lens _scPrivateDNSNameConfiguration (\s a -> s {_scPrivateDNSNameConfiguration = a})

-- | Any tags assigned to the service.
scTags :: Lens' ServiceConfiguration [Tag]
scTags = lens _scTags (\s a -> s {_scTags = a}) . _Default . _Coerce

instance FromXML ServiceConfiguration where
  parseXML x =
    ServiceConfiguration'
      <$> ( x .@? "networkLoadBalancerArnSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> ( x .@? "baseEndpointDnsNameSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> ( x .@? "availabilityZoneSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> ( x .@? "gatewayLoadBalancerArnSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "managesVpcEndpoints")
      <*> (x .@? "serviceName")
      <*> (x .@? "serviceState")
      <*> (x .@? "serviceType" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "acceptanceRequired")
      <*> (x .@? "serviceId")
      <*> (x .@? "privateDnsName")
      <*> (x .@? "privateDnsNameConfiguration")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable ServiceConfiguration

instance NFData ServiceConfiguration
