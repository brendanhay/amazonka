{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.AuthorizeCacheSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The AuthorizeCacheSecurityGroupIngress operation allows network ingress to
-- a cache security group. Applications using ElastiCache must be running on
-- Amazon EC2, and Amazon EC2 security groups are used as the authorization
-- mechanism. You cannot authorize ingress from an Amazon EC2 security group
-- in one region to an ElastiCache cluster in another region.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=AuthorizeCacheSecurityGroupIngress &EC2SecurityGroupName=default
-- &CacheSecurityGroupName=mygroup &EC2SecurityGroupOwnerId=1234-5678-1234
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= authorizing default
-- 565419523791 mygroup 123456781234 My security group
-- 817fa999-3647-11e0-ae57-f96cfe56749c.
module Network.AWS.ElastiCache.V2014_07_15.AuthorizeCacheSecurityGroupIngress
    (
    -- * Request
      AuthorizeCacheSecurityGroupIngress
    -- ** Request constructor
    , mkAuthorizeCacheSecurityGroupIngressMessage
    -- ** Request lenses
    , acsgimCacheSecurityGroupName
    , acsgimEC2SecurityGroupName
    , acsgimEC2SecurityGroupOwnerId

    -- * Response
    , AuthorizeCacheSecurityGroupIngressResponse
    -- ** Response lenses
    , csgwCacheSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AuthorizeCacheSecurityGroupIngress' request.
mkAuthorizeCacheSecurityGroupIngressMessage :: Text -- ^ 'acsgimCacheSecurityGroupName'
                                            -> Text -- ^ 'acsgimEC2SecurityGroupName'
                                            -> Text -- ^ 'acsgimEC2SecurityGroupOwnerId'
                                            -> AuthorizeCacheSecurityGroupIngress
mkAuthorizeCacheSecurityGroupIngressMessage p1 p2 p3 = AuthorizeCacheSecurityGroupIngress
    { _acsgimCacheSecurityGroupName = p1
    , _acsgimEC2SecurityGroupName = p2
    , _acsgimEC2SecurityGroupOwnerId = p3
    }
{-# INLINE mkAuthorizeCacheSecurityGroupIngressMessage #-}

data AuthorizeCacheSecurityGroupIngress = AuthorizeCacheSecurityGroupIngress
    { _acsgimCacheSecurityGroupName :: Text
      -- ^ The cache security group which will allow network ingress.
    , _acsgimEC2SecurityGroupName :: Text
      -- ^ The Amazon EC2 security group to be authorized for ingress to the
      -- cache security group.
    , _acsgimEC2SecurityGroupOwnerId :: Text
      -- ^ The AWS account number of the Amazon EC2 security group owner.
      -- Note that this is not the same thing as an AWS access key ID -
      -- you must provide a valid AWS account number for this parameter.
    } deriving (Show, Generic)

-- | The cache security group which will allow network ingress.
acsgimCacheSecurityGroupName :: Lens' AuthorizeCacheSecurityGroupIngress (Text)
acsgimCacheSecurityGroupName = lens _acsgimCacheSecurityGroupName (\s a -> s { _acsgimCacheSecurityGroupName = a })
{-# INLINE acsgimCacheSecurityGroupName #-}

-- | The Amazon EC2 security group to be authorized for ingress to the cache
-- security group.
acsgimEC2SecurityGroupName :: Lens' AuthorizeCacheSecurityGroupIngress (Text)
acsgimEC2SecurityGroupName = lens _acsgimEC2SecurityGroupName (\s a -> s { _acsgimEC2SecurityGroupName = a })
{-# INLINE acsgimEC2SecurityGroupName #-}

-- | The AWS account number of the Amazon EC2 security group owner. Note that
-- this is not the same thing as an AWS access key ID - you must provide a
-- valid AWS account number for this parameter.
acsgimEC2SecurityGroupOwnerId :: Lens' AuthorizeCacheSecurityGroupIngress (Text)
acsgimEC2SecurityGroupOwnerId = lens _acsgimEC2SecurityGroupOwnerId (\s a -> s { _acsgimEC2SecurityGroupOwnerId = a })
{-# INLINE acsgimEC2SecurityGroupOwnerId #-}

instance ToQuery AuthorizeCacheSecurityGroupIngress where
    toQuery = genericQuery def

newtype AuthorizeCacheSecurityGroupIngressResponse = AuthorizeCacheSecurityGroupIngressResponse
    { _csgwCacheSecurityGroup :: Maybe CacheSecurityGroup
      -- ^ Represents the output of one of the following operations:
      -- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
      -- RevokeCacheSecurityGroupIngress.
    } deriving (Show, Generic)

-- | Represents the output of one of the following operations:
-- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
-- RevokeCacheSecurityGroupIngress.
csgwCacheSecurityGroup :: Lens' AuthorizeCacheSecurityGroupIngressResponse (Maybe CacheSecurityGroup)
csgwCacheSecurityGroup = lens _csgwCacheSecurityGroup (\s a -> s { _csgwCacheSecurityGroup = a })
{-# INLINE csgwCacheSecurityGroup #-}

instance FromXML AuthorizeCacheSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AuthorizeCacheSecurityGroupIngress where
    type Sv AuthorizeCacheSecurityGroupIngress = ElastiCache
    type Rs AuthorizeCacheSecurityGroupIngress = AuthorizeCacheSecurityGroupIngressResponse

    request = post "AuthorizeCacheSecurityGroupIngress"
    response _ = xmlResponse
