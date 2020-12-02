{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.SubnetMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.SubnetMapping where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a subnet mapping.
--
--
--
-- /See:/ 'subnetMapping' smart constructor.
data SubnetMapping = SubnetMapping'
  { _smIPv6Address ::
      !(Maybe Text),
    _smAllocationId :: !(Maybe Text),
    _smPrivateIPv4Address :: !(Maybe Text),
    _smSubnetId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubnetMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smIPv6Address' - [Network Load Balancers] The IPv6 address.
--
-- * 'smAllocationId' - [Network Load Balancers] The allocation ID of the Elastic IP address for an internet-facing load balancer.
--
-- * 'smPrivateIPv4Address' - [Network Load Balancers] The private IPv4 address for an internal load balancer.
--
-- * 'smSubnetId' - The ID of the subnet.
subnetMapping ::
  SubnetMapping
subnetMapping =
  SubnetMapping'
    { _smIPv6Address = Nothing,
      _smAllocationId = Nothing,
      _smPrivateIPv4Address = Nothing,
      _smSubnetId = Nothing
    }

-- | [Network Load Balancers] The IPv6 address.
smIPv6Address :: Lens' SubnetMapping (Maybe Text)
smIPv6Address = lens _smIPv6Address (\s a -> s {_smIPv6Address = a})

-- | [Network Load Balancers] The allocation ID of the Elastic IP address for an internet-facing load balancer.
smAllocationId :: Lens' SubnetMapping (Maybe Text)
smAllocationId = lens _smAllocationId (\s a -> s {_smAllocationId = a})

-- | [Network Load Balancers] The private IPv4 address for an internal load balancer.
smPrivateIPv4Address :: Lens' SubnetMapping (Maybe Text)
smPrivateIPv4Address = lens _smPrivateIPv4Address (\s a -> s {_smPrivateIPv4Address = a})

-- | The ID of the subnet.
smSubnetId :: Lens' SubnetMapping (Maybe Text)
smSubnetId = lens _smSubnetId (\s a -> s {_smSubnetId = a})

instance Hashable SubnetMapping

instance NFData SubnetMapping

instance ToQuery SubnetMapping where
  toQuery SubnetMapping' {..} =
    mconcat
      [ "IPv6Address" =: _smIPv6Address,
        "AllocationId" =: _smAllocationId,
        "PrivateIPv4Address" =: _smPrivateIPv4Address,
        "SubnetId" =: _smSubnetId
      ]
