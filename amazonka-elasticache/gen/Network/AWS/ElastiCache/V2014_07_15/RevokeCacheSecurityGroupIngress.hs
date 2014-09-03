{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.RevokeCacheSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The RevokeCacheSecurityGroupIngress operation revokes ingress from a cache
-- security group. Use this operation to disallow access from an Amazon EC2
-- security group that had been previously authorized.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=RevokeCacheSecurityGroupIngress &EC2SecurityGroupName=default
-- &CacheSecurityGroupName=mygroup &EC2SecurityGroupOwnerId=1234-5678-1234
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= revoking default
-- 123456781234 mygroup 123456789012 My security group
-- 02ae3699-3650-11e0-a564-8f11342c56b0.
module Network.AWS.ElastiCache.V2014_07_15.RevokeCacheSecurityGroupIngress
    (
    -- * Request
      RevokeCacheSecurityGroupIngress
    -- ** Request constructor
    , revokeCacheSecurityGroupIngress
    -- ** Request lenses
    , rcsgimCacheSecurityGroupName
    , rcsgimEC2SecurityGroupName
    , rcsgimEC2SecurityGroupOwnerId

    -- * Response
    , RevokeCacheSecurityGroupIngressResponse
    -- ** Response lenses
    , csgcrCacheSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RevokeCacheSecurityGroupIngress' request.
revokeCacheSecurityGroupIngress :: Text -- ^ 'rcsgimCacheSecurityGroupName'
                                -> Text -- ^ 'rcsgimEC2SecurityGroupName'
                                -> Text -- ^ 'rcsgimEC2SecurityGroupOwnerId'
                                -> RevokeCacheSecurityGroupIngress
revokeCacheSecurityGroupIngress p1 p2 p3 = RevokeCacheSecurityGroupIngress
    { _rcsgimCacheSecurityGroupName = p1
    , _rcsgimEC2SecurityGroupName = p2
    , _rcsgimEC2SecurityGroupOwnerId = p3
    }

data RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngress
    { _rcsgimCacheSecurityGroupName :: Text
      -- ^ The name of the cache security group to revoke ingress from.
    , _rcsgimEC2SecurityGroupName :: Text
      -- ^ The name of the Amazon EC2 security group to revoke access from.
    , _rcsgimEC2SecurityGroupOwnerId :: Text
      -- ^ The AWS account number of the Amazon EC2 security group owner.
      -- Note that this is not the same thing as an AWS access key ID -
      -- you must provide a valid AWS account number for this parameter.
    } deriving (Show, Generic)

-- | The name of the cache security group to revoke ingress from.
rcsgimCacheSecurityGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> RevokeCacheSecurityGroupIngress
    -> f RevokeCacheSecurityGroupIngress
rcsgimCacheSecurityGroupName f x =
    (\y -> x { _rcsgimCacheSecurityGroupName = y })
       <$> f (_rcsgimCacheSecurityGroupName x)
{-# INLINE rcsgimCacheSecurityGroupName #-}

-- | The name of the Amazon EC2 security group to revoke access from.
rcsgimEC2SecurityGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> RevokeCacheSecurityGroupIngress
    -> f RevokeCacheSecurityGroupIngress
rcsgimEC2SecurityGroupName f x =
    (\y -> x { _rcsgimEC2SecurityGroupName = y })
       <$> f (_rcsgimEC2SecurityGroupName x)
{-# INLINE rcsgimEC2SecurityGroupName #-}

-- | The AWS account number of the Amazon EC2 security group owner. Note that
-- this is not the same thing as an AWS access key ID - you must provide a
-- valid AWS account number for this parameter.
rcsgimEC2SecurityGroupOwnerId
    :: Functor f
    => (Text
    -> f (Text))
    -> RevokeCacheSecurityGroupIngress
    -> f RevokeCacheSecurityGroupIngress
rcsgimEC2SecurityGroupOwnerId f x =
    (\y -> x { _rcsgimEC2SecurityGroupOwnerId = y })
       <$> f (_rcsgimEC2SecurityGroupOwnerId x)
{-# INLINE rcsgimEC2SecurityGroupOwnerId #-}

instance ToQuery RevokeCacheSecurityGroupIngress where
    toQuery = genericQuery def

data RevokeCacheSecurityGroupIngressResponse = RevokeCacheSecurityGroupIngressResponse
    { _csgcrCacheSecurityGroup :: Maybe CacheSecurityGroup
      -- ^ Represents the output of one of the following operations:
      -- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
      -- RevokeCacheSecurityGroupIngress.
    } deriving (Show, Generic)

-- | Represents the output of one of the following operations:
-- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
-- RevokeCacheSecurityGroupIngress.
csgcrCacheSecurityGroup
    :: Functor f
    => (Maybe CacheSecurityGroup
    -> f (Maybe CacheSecurityGroup))
    -> RevokeCacheSecurityGroupIngressResponse
    -> f RevokeCacheSecurityGroupIngressResponse
csgcrCacheSecurityGroup f x =
    (\y -> x { _csgcrCacheSecurityGroup = y })
       <$> f (_csgcrCacheSecurityGroup x)
{-# INLINE csgcrCacheSecurityGroup #-}

instance FromXML RevokeCacheSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RevokeCacheSecurityGroupIngress where
    type Sv RevokeCacheSecurityGroupIngress = ElastiCache
    type Rs RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngressResponse

    request = post "RevokeCacheSecurityGroupIngress"
    response _ = xmlResponse
