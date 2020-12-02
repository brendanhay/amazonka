{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IPv6Pool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IPv6Pool where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PoolCidrBlock
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IPv6 address pool.
--
--
--
-- /See:/ 'ipv6Pool' smart constructor.
data IPv6Pool = IPv6Pool'
  { _ipPoolCidrBlocks ::
      !(Maybe [PoolCidrBlock]),
    _ipPoolId :: !(Maybe Text),
    _ipDescription :: !(Maybe Text),
    _ipTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPv6Pool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipPoolCidrBlocks' - The CIDR blocks for the address pool.
--
-- * 'ipPoolId' - The ID of the address pool.
--
-- * 'ipDescription' - The description for the address pool.
--
-- * 'ipTags' - Any tags for the address pool.
ipv6Pool ::
  IPv6Pool
ipv6Pool =
  IPv6Pool'
    { _ipPoolCidrBlocks = Nothing,
      _ipPoolId = Nothing,
      _ipDescription = Nothing,
      _ipTags = Nothing
    }

-- | The CIDR blocks for the address pool.
ipPoolCidrBlocks :: Lens' IPv6Pool [PoolCidrBlock]
ipPoolCidrBlocks = lens _ipPoolCidrBlocks (\s a -> s {_ipPoolCidrBlocks = a}) . _Default . _Coerce

-- | The ID of the address pool.
ipPoolId :: Lens' IPv6Pool (Maybe Text)
ipPoolId = lens _ipPoolId (\s a -> s {_ipPoolId = a})

-- | The description for the address pool.
ipDescription :: Lens' IPv6Pool (Maybe Text)
ipDescription = lens _ipDescription (\s a -> s {_ipDescription = a})

-- | Any tags for the address pool.
ipTags :: Lens' IPv6Pool [Tag]
ipTags = lens _ipTags (\s a -> s {_ipTags = a}) . _Default . _Coerce

instance FromXML IPv6Pool where
  parseXML x =
    IPv6Pool'
      <$> (x .@? "poolCidrBlockSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "poolId")
      <*> (x .@? "description")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable IPv6Pool

instance NFData IPv6Pool
