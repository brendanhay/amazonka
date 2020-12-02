{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PublicIPv4Pool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PublicIPv4Pool where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PublicIPv4PoolRange
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IPv4 address pool.
--
--
--
-- /See:/ 'publicIPv4Pool' smart constructor.
data PublicIPv4Pool = PublicIPv4Pool'
  { _pipTotalAddressCount ::
      !(Maybe Int),
    _pipNetworkBorderGroup :: !(Maybe Text),
    _pipTotalAvailableAddressCount :: !(Maybe Int),
    _pipPoolAddressRanges :: !(Maybe [PublicIPv4PoolRange]),
    _pipPoolId :: !(Maybe Text),
    _pipDescription :: !(Maybe Text),
    _pipTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PublicIPv4Pool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pipTotalAddressCount' - The total number of addresses.
--
-- * 'pipNetworkBorderGroup' - The name of the location from which the address pool is advertised. A network border group is a unique set of Availability Zones or Local Zones from where AWS advertises public IP addresses.
--
-- * 'pipTotalAvailableAddressCount' - The total number of available addresses.
--
-- * 'pipPoolAddressRanges' - The address ranges.
--
-- * 'pipPoolId' - The ID of the address pool.
--
-- * 'pipDescription' - A description of the address pool.
--
-- * 'pipTags' - Any tags for the address pool.
publicIPv4Pool ::
  PublicIPv4Pool
publicIPv4Pool =
  PublicIPv4Pool'
    { _pipTotalAddressCount = Nothing,
      _pipNetworkBorderGroup = Nothing,
      _pipTotalAvailableAddressCount = Nothing,
      _pipPoolAddressRanges = Nothing,
      _pipPoolId = Nothing,
      _pipDescription = Nothing,
      _pipTags = Nothing
    }

-- | The total number of addresses.
pipTotalAddressCount :: Lens' PublicIPv4Pool (Maybe Int)
pipTotalAddressCount = lens _pipTotalAddressCount (\s a -> s {_pipTotalAddressCount = a})

-- | The name of the location from which the address pool is advertised. A network border group is a unique set of Availability Zones or Local Zones from where AWS advertises public IP addresses.
pipNetworkBorderGroup :: Lens' PublicIPv4Pool (Maybe Text)
pipNetworkBorderGroup = lens _pipNetworkBorderGroup (\s a -> s {_pipNetworkBorderGroup = a})

-- | The total number of available addresses.
pipTotalAvailableAddressCount :: Lens' PublicIPv4Pool (Maybe Int)
pipTotalAvailableAddressCount = lens _pipTotalAvailableAddressCount (\s a -> s {_pipTotalAvailableAddressCount = a})

-- | The address ranges.
pipPoolAddressRanges :: Lens' PublicIPv4Pool [PublicIPv4PoolRange]
pipPoolAddressRanges = lens _pipPoolAddressRanges (\s a -> s {_pipPoolAddressRanges = a}) . _Default . _Coerce

-- | The ID of the address pool.
pipPoolId :: Lens' PublicIPv4Pool (Maybe Text)
pipPoolId = lens _pipPoolId (\s a -> s {_pipPoolId = a})

-- | A description of the address pool.
pipDescription :: Lens' PublicIPv4Pool (Maybe Text)
pipDescription = lens _pipDescription (\s a -> s {_pipDescription = a})

-- | Any tags for the address pool.
pipTags :: Lens' PublicIPv4Pool [Tag]
pipTags = lens _pipTags (\s a -> s {_pipTags = a}) . _Default . _Coerce

instance FromXML PublicIPv4Pool where
  parseXML x =
    PublicIPv4Pool'
      <$> (x .@? "totalAddressCount")
      <*> (x .@? "networkBorderGroup")
      <*> (x .@? "totalAvailableAddressCount")
      <*> ( x .@? "poolAddressRangeSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "poolId")
      <*> (x .@? "description")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable PublicIPv4Pool

instance NFData PublicIPv4Pool
