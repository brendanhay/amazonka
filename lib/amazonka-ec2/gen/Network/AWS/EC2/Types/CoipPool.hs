{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CoipPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CoipPool where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a customer-owned address pool.
--
--
--
-- /See:/ 'coipPool' smart constructor.
data CoipPool = CoipPool'
  { _cpPoolId :: !(Maybe Text),
    _cpLocalGatewayRouteTableId :: !(Maybe Text),
    _cpPoolCidrs :: !(Maybe [Text]),
    _cpTags :: !(Maybe [Tag]),
    _cpPoolARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CoipPool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPoolId' - The ID of the address pool.
--
-- * 'cpLocalGatewayRouteTableId' - The ID of the local gateway route table.
--
-- * 'cpPoolCidrs' - The address ranges of the address pool.
--
-- * 'cpTags' - The tags.
--
-- * 'cpPoolARN' - The ARN of the address pool.
coipPool ::
  CoipPool
coipPool =
  CoipPool'
    { _cpPoolId = Nothing,
      _cpLocalGatewayRouteTableId = Nothing,
      _cpPoolCidrs = Nothing,
      _cpTags = Nothing,
      _cpPoolARN = Nothing
    }

-- | The ID of the address pool.
cpPoolId :: Lens' CoipPool (Maybe Text)
cpPoolId = lens _cpPoolId (\s a -> s {_cpPoolId = a})

-- | The ID of the local gateway route table.
cpLocalGatewayRouteTableId :: Lens' CoipPool (Maybe Text)
cpLocalGatewayRouteTableId = lens _cpLocalGatewayRouteTableId (\s a -> s {_cpLocalGatewayRouteTableId = a})

-- | The address ranges of the address pool.
cpPoolCidrs :: Lens' CoipPool [Text]
cpPoolCidrs = lens _cpPoolCidrs (\s a -> s {_cpPoolCidrs = a}) . _Default . _Coerce

-- | The tags.
cpTags :: Lens' CoipPool [Tag]
cpTags = lens _cpTags (\s a -> s {_cpTags = a}) . _Default . _Coerce

-- | The ARN of the address pool.
cpPoolARN :: Lens' CoipPool (Maybe Text)
cpPoolARN = lens _cpPoolARN (\s a -> s {_cpPoolARN = a})

instance FromXML CoipPool where
  parseXML x =
    CoipPool'
      <$> (x .@? "poolId")
      <*> (x .@? "localGatewayRouteTableId")
      <*> (x .@? "poolCidrSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "poolArn")

instance Hashable CoipPool

instance NFData CoipPool
