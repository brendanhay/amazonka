{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheSubnetGroup where

import Network.AWS.ElastiCache.Types.Subnet
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of one of the following operations:
--
--
--     * @CreateCacheSubnetGroup@
--
--     * @ModifyCacheSubnetGroup@
--
--
--
--
-- /See:/ 'cacheSubnetGroup' smart constructor.
data CacheSubnetGroup = CacheSubnetGroup'
  { _cARN :: !(Maybe Text),
    _cVPCId :: !(Maybe Text),
    _cSubnets :: !(Maybe [Subnet]),
    _cCacheSubnetGroupName :: !(Maybe Text),
    _cCacheSubnetGroupDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CacheSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cARN' - The ARN (Amazon Resource Name) of the cache subnet group.
--
-- * 'cVPCId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group.
--
-- * 'cSubnets' - A list of subnets associated with the cache subnet group.
--
-- * 'cCacheSubnetGroupName' - The name of the cache subnet group.
--
-- * 'cCacheSubnetGroupDescription' - The description of the cache subnet group.
cacheSubnetGroup ::
  CacheSubnetGroup
cacheSubnetGroup =
  CacheSubnetGroup'
    { _cARN = Nothing,
      _cVPCId = Nothing,
      _cSubnets = Nothing,
      _cCacheSubnetGroupName = Nothing,
      _cCacheSubnetGroupDescription = Nothing
    }

-- | The ARN (Amazon Resource Name) of the cache subnet group.
cARN :: Lens' CacheSubnetGroup (Maybe Text)
cARN = lens _cARN (\s a -> s {_cARN = a})

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group.
cVPCId :: Lens' CacheSubnetGroup (Maybe Text)
cVPCId = lens _cVPCId (\s a -> s {_cVPCId = a})

-- | A list of subnets associated with the cache subnet group.
cSubnets :: Lens' CacheSubnetGroup [Subnet]
cSubnets = lens _cSubnets (\s a -> s {_cSubnets = a}) . _Default . _Coerce

-- | The name of the cache subnet group.
cCacheSubnetGroupName :: Lens' CacheSubnetGroup (Maybe Text)
cCacheSubnetGroupName = lens _cCacheSubnetGroupName (\s a -> s {_cCacheSubnetGroupName = a})

-- | The description of the cache subnet group.
cCacheSubnetGroupDescription :: Lens' CacheSubnetGroup (Maybe Text)
cCacheSubnetGroupDescription = lens _cCacheSubnetGroupDescription (\s a -> s {_cCacheSubnetGroupDescription = a})

instance FromXML CacheSubnetGroup where
  parseXML x =
    CacheSubnetGroup'
      <$> (x .@? "ARN")
      <*> (x .@? "VpcId")
      <*> (x .@? "Subnets" .!@ mempty >>= may (parseXMLList "Subnet"))
      <*> (x .@? "CacheSubnetGroupName")
      <*> (x .@? "CacheSubnetGroupDescription")

instance Hashable CacheSubnetGroup

instance NFData CacheSubnetGroup
