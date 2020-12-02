{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Subnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Subnet where

import Network.AWS.ElastiCache.Types.AvailabilityZone
import Network.AWS.ElastiCache.Types.SubnetOutpost
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the subnet associated with a cluster. This parameter refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used with ElastiCache.
--
--
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'
  { _sSubnetIdentifier :: !(Maybe Text),
    _sSubnetAvailabilityZone :: !(Maybe AvailabilityZone),
    _sSubnetOutpost :: !(Maybe SubnetOutpost)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubnetIdentifier' - The unique identifier for the subnet.
--
-- * 'sSubnetAvailabilityZone' - The Availability Zone associated with the subnet.
--
-- * 'sSubnetOutpost' - The outpost ARN of the subnet.
subnet ::
  Subnet
subnet =
  Subnet'
    { _sSubnetIdentifier = Nothing,
      _sSubnetAvailabilityZone = Nothing,
      _sSubnetOutpost = Nothing
    }

-- | The unique identifier for the subnet.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier = lens _sSubnetIdentifier (\s a -> s {_sSubnetIdentifier = a})

-- | The Availability Zone associated with the subnet.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone = lens _sSubnetAvailabilityZone (\s a -> s {_sSubnetAvailabilityZone = a})

-- | The outpost ARN of the subnet.
sSubnetOutpost :: Lens' Subnet (Maybe SubnetOutpost)
sSubnetOutpost = lens _sSubnetOutpost (\s a -> s {_sSubnetOutpost = a})

instance FromXML Subnet where
  parseXML x =
    Subnet'
      <$> (x .@? "SubnetIdentifier")
      <*> (x .@? "SubnetAvailabilityZone")
      <*> (x .@? "SubnetOutpost")

instance Hashable Subnet

instance NFData Subnet
