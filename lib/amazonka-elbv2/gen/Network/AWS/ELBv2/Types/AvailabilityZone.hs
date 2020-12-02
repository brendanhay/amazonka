{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.AvailabilityZone where

import Network.AWS.ELBv2.Types.LoadBalancerAddress
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an Availability Zone.
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { _azSubnetId ::
      !(Maybe Text),
    _azZoneName :: !(Maybe Text),
    _azLoadBalancerAddresses ::
      !(Maybe [LoadBalancerAddress]),
    _azOutpostId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azSubnetId' - The ID of the subnet. You can specify one subnet per Availability Zone.
--
-- * 'azZoneName' - The name of the Availability Zone.
--
-- * 'azLoadBalancerAddresses' - [Network Load Balancers] If you need static IP addresses for your load balancer, you can specify one Elastic IP address per Availability Zone when you create an internal-facing load balancer. For internal load balancers, you can specify a private IP address from the IPv4 range of the subnet.
--
-- * 'azOutpostId' - [Application Load Balancers on Outposts] The ID of the Outpost.
availabilityZone ::
  AvailabilityZone
availabilityZone =
  AvailabilityZone'
    { _azSubnetId = Nothing,
      _azZoneName = Nothing,
      _azLoadBalancerAddresses = Nothing,
      _azOutpostId = Nothing
    }

-- | The ID of the subnet. You can specify one subnet per Availability Zone.
azSubnetId :: Lens' AvailabilityZone (Maybe Text)
azSubnetId = lens _azSubnetId (\s a -> s {_azSubnetId = a})

-- | The name of the Availability Zone.
azZoneName :: Lens' AvailabilityZone (Maybe Text)
azZoneName = lens _azZoneName (\s a -> s {_azZoneName = a})

-- | [Network Load Balancers] If you need static IP addresses for your load balancer, you can specify one Elastic IP address per Availability Zone when you create an internal-facing load balancer. For internal load balancers, you can specify a private IP address from the IPv4 range of the subnet.
azLoadBalancerAddresses :: Lens' AvailabilityZone [LoadBalancerAddress]
azLoadBalancerAddresses = lens _azLoadBalancerAddresses (\s a -> s {_azLoadBalancerAddresses = a}) . _Default . _Coerce

-- | [Application Load Balancers on Outposts] The ID of the Outpost.
azOutpostId :: Lens' AvailabilityZone (Maybe Text)
azOutpostId = lens _azOutpostId (\s a -> s {_azOutpostId = a})

instance FromXML AvailabilityZone where
  parseXML x =
    AvailabilityZone'
      <$> (x .@? "SubnetId")
      <*> (x .@? "ZoneName")
      <*> ( x .@? "LoadBalancerAddresses" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "OutpostId")

instance Hashable AvailabilityZone

instance NFData AvailabilityZone
