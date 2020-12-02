{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancerAddress where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a static IP address for a load balancer.
--
--
--
-- /See:/ 'loadBalancerAddress' smart constructor.
data LoadBalancerAddress = LoadBalancerAddress'
  { _lbaIPv6Address ::
      !(Maybe Text),
    _lbaIPAddress :: !(Maybe Text),
    _lbaAllocationId :: !(Maybe Text),
    _lbaPrivateIPv4Address :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadBalancerAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbaIPv6Address' - [Network Load Balancers] The IPv6 address.
--
-- * 'lbaIPAddress' - The static IP address.
--
-- * 'lbaAllocationId' - [Network Load Balancers] The allocation ID of the Elastic IP address for an internal-facing load balancer.
--
-- * 'lbaPrivateIPv4Address' - [Network Load Balancers] The private IPv4 address for an internal load balancer.
loadBalancerAddress ::
  LoadBalancerAddress
loadBalancerAddress =
  LoadBalancerAddress'
    { _lbaIPv6Address = Nothing,
      _lbaIPAddress = Nothing,
      _lbaAllocationId = Nothing,
      _lbaPrivateIPv4Address = Nothing
    }

-- | [Network Load Balancers] The IPv6 address.
lbaIPv6Address :: Lens' LoadBalancerAddress (Maybe Text)
lbaIPv6Address = lens _lbaIPv6Address (\s a -> s {_lbaIPv6Address = a})

-- | The static IP address.
lbaIPAddress :: Lens' LoadBalancerAddress (Maybe Text)
lbaIPAddress = lens _lbaIPAddress (\s a -> s {_lbaIPAddress = a})

-- | [Network Load Balancers] The allocation ID of the Elastic IP address for an internal-facing load balancer.
lbaAllocationId :: Lens' LoadBalancerAddress (Maybe Text)
lbaAllocationId = lens _lbaAllocationId (\s a -> s {_lbaAllocationId = a})

-- | [Network Load Balancers] The private IPv4 address for an internal load balancer.
lbaPrivateIPv4Address :: Lens' LoadBalancerAddress (Maybe Text)
lbaPrivateIPv4Address = lens _lbaPrivateIPv4Address (\s a -> s {_lbaPrivateIPv4Address = a})

instance FromXML LoadBalancerAddress where
  parseXML x =
    LoadBalancerAddress'
      <$> (x .@? "IPv6Address")
      <*> (x .@? "IpAddress")
      <*> (x .@? "AllocationId")
      <*> (x .@? "PrivateIPv4Address")

instance Hashable LoadBalancerAddress

instance NFData LoadBalancerAddress
