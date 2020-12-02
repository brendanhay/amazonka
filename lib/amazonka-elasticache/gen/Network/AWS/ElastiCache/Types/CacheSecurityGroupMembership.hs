{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheSecurityGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheSecurityGroupMembership where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a cluster's status within a particular cache security group.
--
--
--
-- /See:/ 'cacheSecurityGroupMembership' smart constructor.
data CacheSecurityGroupMembership = CacheSecurityGroupMembership'
  { _csgmStatus ::
      !(Maybe Text),
    _csgmCacheSecurityGroupName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CacheSecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgmStatus' - The membership status in the cache security group. The status changes when a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
--
-- * 'csgmCacheSecurityGroupName' - The name of the cache security group.
cacheSecurityGroupMembership ::
  CacheSecurityGroupMembership
cacheSecurityGroupMembership =
  CacheSecurityGroupMembership'
    { _csgmStatus = Nothing,
      _csgmCacheSecurityGroupName = Nothing
    }

-- | The membership status in the cache security group. The status changes when a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
csgmStatus :: Lens' CacheSecurityGroupMembership (Maybe Text)
csgmStatus = lens _csgmStatus (\s a -> s {_csgmStatus = a})

-- | The name of the cache security group.
csgmCacheSecurityGroupName :: Lens' CacheSecurityGroupMembership (Maybe Text)
csgmCacheSecurityGroupName = lens _csgmCacheSecurityGroupName (\s a -> s {_csgmCacheSecurityGroupName = a})

instance FromXML CacheSecurityGroupMembership where
  parseXML x =
    CacheSecurityGroupMembership'
      <$> (x .@? "Status") <*> (x .@? "CacheSecurityGroupName")

instance Hashable CacheSecurityGroupMembership

instance NFData CacheSecurityGroupMembership
