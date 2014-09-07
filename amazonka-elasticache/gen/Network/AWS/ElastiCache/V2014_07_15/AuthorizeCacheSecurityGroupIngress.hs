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
    , mkAuthorizeCacheSecurityGroupIngress
    -- ** Request lenses
    , acsgiCacheSecurityGroupName
    , acsgiEC2SecurityGroupName
    , acsgiEC2SecurityGroupOwnerId

    -- * Response
    , AuthorizeCacheSecurityGroupIngressResponse
    -- ** Response lenses
    , acsgirsCacheSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Represents the input of an AuthorizeCacheSecurityGroupIngress operation.
data AuthorizeCacheSecurityGroupIngress = AuthorizeCacheSecurityGroupIngress
    { _acsgiCacheSecurityGroupName :: Text
    , _acsgiEC2SecurityGroupName :: Text
    , _acsgiEC2SecurityGroupOwnerId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AuthorizeCacheSecurityGroupIngress' request.
mkAuthorizeCacheSecurityGroupIngress :: Text -- ^ 'acsgiCacheSecurityGroupName'
                                     -> Text -- ^ 'acsgiEC2SecurityGroupName'
                                     -> Text -- ^ 'acsgiEC2SecurityGroupOwnerId'
                                     -> AuthorizeCacheSecurityGroupIngress
mkAuthorizeCacheSecurityGroupIngress p1 p2 p3 = AuthorizeCacheSecurityGroupIngress
    { _acsgiCacheSecurityGroupName = p1
    , _acsgiEC2SecurityGroupName = p2
    , _acsgiEC2SecurityGroupOwnerId = p3
    }

-- | The cache security group which will allow network ingress.
acsgiCacheSecurityGroupName :: Lens' AuthorizeCacheSecurityGroupIngress Text
acsgiCacheSecurityGroupName =
    lens _acsgiCacheSecurityGroupName
         (\s a -> s { _acsgiCacheSecurityGroupName = a })

-- | The Amazon EC2 security group to be authorized for ingress to the cache
-- security group.
acsgiEC2SecurityGroupName :: Lens' AuthorizeCacheSecurityGroupIngress Text
acsgiEC2SecurityGroupName =
    lens _acsgiEC2SecurityGroupName
         (\s a -> s { _acsgiEC2SecurityGroupName = a })

-- | The AWS account number of the Amazon EC2 security group owner. Note that
-- this is not the same thing as an AWS access key ID - you must provide a
-- valid AWS account number for this parameter.
acsgiEC2SecurityGroupOwnerId :: Lens' AuthorizeCacheSecurityGroupIngress Text
acsgiEC2SecurityGroupOwnerId =
    lens _acsgiEC2SecurityGroupOwnerId
         (\s a -> s { _acsgiEC2SecurityGroupOwnerId = a })

instance ToQuery AuthorizeCacheSecurityGroupIngress where
    toQuery = genericQuery def

newtype AuthorizeCacheSecurityGroupIngressResponse = AuthorizeCacheSecurityGroupIngressResponse
    { _acsgirsCacheSecurityGroup :: Maybe CacheSecurityGroup
    } deriving (Show, Generic)

-- | Represents the output of one of the following operations:
-- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
-- RevokeCacheSecurityGroupIngress.
acsgirsCacheSecurityGroup :: Lens' AuthorizeCacheSecurityGroupIngressResponse (Maybe CacheSecurityGroup)
acsgirsCacheSecurityGroup =
    lens _acsgirsCacheSecurityGroup
         (\s a -> s { _acsgirsCacheSecurityGroup = a })

instance FromXML AuthorizeCacheSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AuthorizeCacheSecurityGroupIngress where
    type Sv AuthorizeCacheSecurityGroupIngress = ElastiCache
    type Rs AuthorizeCacheSecurityGroupIngress = AuthorizeCacheSecurityGroupIngressResponse

    request = post "AuthorizeCacheSecurityGroupIngress"
    response _ = xmlResponse
