{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheSecurityGroup where

import Network.AWS.ElastiCache.Types.EC2SecurityGroup
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of one of the following operations:
--
--
--     * @AuthorizeCacheSecurityGroupIngress@
--
--     * @CreateCacheSecurityGroup@
--
--     * @RevokeCacheSecurityGroupIngress@
--
--
--
--
-- /See:/ 'cacheSecurityGroup' smart constructor.
data CacheSecurityGroup = CacheSecurityGroup'
  { _csgCacheSecurityGroupName ::
      !(Maybe Text),
    _csgARN :: !(Maybe Text),
    _csgOwnerId :: !(Maybe Text),
    _csgEC2SecurityGroups :: !(Maybe [EC2SecurityGroup]),
    _csgDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CacheSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgCacheSecurityGroupName' - The name of the cache security group.
--
-- * 'csgARN' - The ARN of the cache security group,
--
-- * 'csgOwnerId' - The AWS account ID of the cache security group owner.
--
-- * 'csgEC2SecurityGroups' - A list of Amazon EC2 security groups that are associated with this cache security group.
--
-- * 'csgDescription' - The description of the cache security group.
cacheSecurityGroup ::
  CacheSecurityGroup
cacheSecurityGroup =
  CacheSecurityGroup'
    { _csgCacheSecurityGroupName = Nothing,
      _csgARN = Nothing,
      _csgOwnerId = Nothing,
      _csgEC2SecurityGroups = Nothing,
      _csgDescription = Nothing
    }

-- | The name of the cache security group.
csgCacheSecurityGroupName :: Lens' CacheSecurityGroup (Maybe Text)
csgCacheSecurityGroupName = lens _csgCacheSecurityGroupName (\s a -> s {_csgCacheSecurityGroupName = a})

-- | The ARN of the cache security group,
csgARN :: Lens' CacheSecurityGroup (Maybe Text)
csgARN = lens _csgARN (\s a -> s {_csgARN = a})

-- | The AWS account ID of the cache security group owner.
csgOwnerId :: Lens' CacheSecurityGroup (Maybe Text)
csgOwnerId = lens _csgOwnerId (\s a -> s {_csgOwnerId = a})

-- | A list of Amazon EC2 security groups that are associated with this cache security group.
csgEC2SecurityGroups :: Lens' CacheSecurityGroup [EC2SecurityGroup]
csgEC2SecurityGroups = lens _csgEC2SecurityGroups (\s a -> s {_csgEC2SecurityGroups = a}) . _Default . _Coerce

-- | The description of the cache security group.
csgDescription :: Lens' CacheSecurityGroup (Maybe Text)
csgDescription = lens _csgDescription (\s a -> s {_csgDescription = a})

instance FromXML CacheSecurityGroup where
  parseXML x =
    CacheSecurityGroup'
      <$> (x .@? "CacheSecurityGroupName")
      <*> (x .@? "ARN")
      <*> (x .@? "OwnerId")
      <*> ( x .@? "EC2SecurityGroups" .!@ mempty
              >>= may (parseXMLList "EC2SecurityGroup")
          )
      <*> (x .@? "Description")

instance Hashable CacheSecurityGroup

instance NFData CacheSecurityGroup
