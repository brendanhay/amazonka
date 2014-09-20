{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
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
module Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
    (
    -- * Request
      RevokeCacheSecurityGroupIngress
    -- ** Request constructor
    , revokeCacheSecurityGroupIngress
    -- ** Request lenses
    , rcsgiCacheSecurityGroupName
    , rcsgiEC2SecurityGroupName
    , rcsgiEC2SecurityGroupOwnerId

    -- * Response
    , RevokeCacheSecurityGroupIngressResponse
    -- ** Response constructor
    , revokeCacheSecurityGroupIngressResponse
    -- ** Response lenses
    , rcsgirCacheSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a RevokeCacheSecurityGroupIngress operation.
data RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngress
    { _rcsgiCacheSecurityGroupName :: Text
    , _rcsgiEC2SecurityGroupName :: Text
    , _rcsgiEC2SecurityGroupOwnerId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RevokeCacheSecurityGroupIngress' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheSecurityGroupName ::@ @Text@
--
-- * @EC2SecurityGroupName ::@ @Text@
--
-- * @EC2SecurityGroupOwnerId ::@ @Text@
--
revokeCacheSecurityGroupIngress :: Text -- ^ 'rcsgiCacheSecurityGroupName'
                                -> Text -- ^ 'rcsgiEC2SecurityGroupName'
                                -> Text -- ^ 'rcsgiEC2SecurityGroupOwnerId'
                                -> RevokeCacheSecurityGroupIngress
revokeCacheSecurityGroupIngress p1 p2 p3 = RevokeCacheSecurityGroupIngress
    { _rcsgiCacheSecurityGroupName = p1
    , _rcsgiEC2SecurityGroupName = p2
    , _rcsgiEC2SecurityGroupOwnerId = p3
    }

-- | The name of the cache security group to revoke ingress from.
rcsgiCacheSecurityGroupName :: Lens' RevokeCacheSecurityGroupIngress Text
rcsgiCacheSecurityGroupName =
    lens _rcsgiCacheSecurityGroupName
         (\s a -> s { _rcsgiCacheSecurityGroupName = a })

-- | The name of the Amazon EC2 security group to revoke access from.
rcsgiEC2SecurityGroupName :: Lens' RevokeCacheSecurityGroupIngress Text
rcsgiEC2SecurityGroupName =
    lens _rcsgiEC2SecurityGroupName
         (\s a -> s { _rcsgiEC2SecurityGroupName = a })

-- | The AWS account number of the Amazon EC2 security group owner. Note that
-- this is not the same thing as an AWS access key ID - you must provide a
-- valid AWS account number for this parameter.
rcsgiEC2SecurityGroupOwnerId :: Lens' RevokeCacheSecurityGroupIngress Text
rcsgiEC2SecurityGroupOwnerId =
    lens _rcsgiEC2SecurityGroupOwnerId
         (\s a -> s { _rcsgiEC2SecurityGroupOwnerId = a })

instance ToQuery RevokeCacheSecurityGroupIngress where
    toQuery = genericQuery def

newtype RevokeCacheSecurityGroupIngressResponse = RevokeCacheSecurityGroupIngressResponse
    { _rcsgirCacheSecurityGroup :: Maybe CacheSecurityGroup
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RevokeCacheSecurityGroupIngressResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheSecurityGroup ::@ @Maybe CacheSecurityGroup@
--
revokeCacheSecurityGroupIngressResponse :: RevokeCacheSecurityGroupIngressResponse
revokeCacheSecurityGroupIngressResponse = RevokeCacheSecurityGroupIngressResponse
    { _rcsgirCacheSecurityGroup = Nothing
    }

-- | Represents the output of one of the following operations:
-- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
-- RevokeCacheSecurityGroupIngress.
rcsgirCacheSecurityGroup :: Lens' RevokeCacheSecurityGroupIngressResponse (Maybe CacheSecurityGroup)
rcsgirCacheSecurityGroup =
    lens _rcsgirCacheSecurityGroup
         (\s a -> s { _rcsgirCacheSecurityGroup = a })

instance FromXML RevokeCacheSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RevokeCacheSecurityGroupIngress where
    type Sv RevokeCacheSecurityGroupIngress = ElastiCache
    type Rs RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngressResponse

    request = post "RevokeCacheSecurityGroupIngress"
    response _ = xmlResponse
