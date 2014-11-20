{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_RevokeCacheSecurityGroupIngress.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngress
    { _rcsgiCacheSecurityGroupName  :: Text
    , _rcsgiEC2SecurityGroupName    :: Text
    , _rcsgiEC2SecurityGroupOwnerId :: Text
    } deriving (Eq, Ord, Show)

-- | 'RevokeCacheSecurityGroupIngress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcsgiCacheSecurityGroupName' @::@ 'Text'
--
-- * 'rcsgiEC2SecurityGroupName' @::@ 'Text'
--
-- * 'rcsgiEC2SecurityGroupOwnerId' @::@ 'Text'
--
revokeCacheSecurityGroupIngress :: Text -- ^ 'rcsgiCacheSecurityGroupName'
                                -> Text -- ^ 'rcsgiEC2SecurityGroupName'
                                -> Text -- ^ 'rcsgiEC2SecurityGroupOwnerId'
                                -> RevokeCacheSecurityGroupIngress
revokeCacheSecurityGroupIngress p1 p2 p3 = RevokeCacheSecurityGroupIngress
    { _rcsgiCacheSecurityGroupName  = p1
    , _rcsgiEC2SecurityGroupName    = p2
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

newtype RevokeCacheSecurityGroupIngressResponse = RevokeCacheSecurityGroupIngressResponse
    { _rcsgirCacheSecurityGroup :: Maybe CacheSecurityGroup
    } deriving (Eq, Show)

-- | 'RevokeCacheSecurityGroupIngressResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcsgirCacheSecurityGroup' @::@ 'Maybe' 'CacheSecurityGroup'
--
revokeCacheSecurityGroupIngressResponse :: RevokeCacheSecurityGroupIngressResponse
revokeCacheSecurityGroupIngressResponse = RevokeCacheSecurityGroupIngressResponse
    { _rcsgirCacheSecurityGroup = Nothing
    }

rcsgirCacheSecurityGroup :: Lens' RevokeCacheSecurityGroupIngressResponse (Maybe CacheSecurityGroup)
rcsgirCacheSecurityGroup =
    lens _rcsgirCacheSecurityGroup
        (\s a -> s { _rcsgirCacheSecurityGroup = a })

instance ToPath RevokeCacheSecurityGroupIngress where
    toPath = const "/"

instance ToQuery RevokeCacheSecurityGroupIngress where
    toQuery RevokeCacheSecurityGroupIngress{..} = mconcat
        [ "CacheSecurityGroupName"  =? _rcsgiCacheSecurityGroupName
        , "EC2SecurityGroupName"    =? _rcsgiEC2SecurityGroupName
        , "EC2SecurityGroupOwnerId" =? _rcsgiEC2SecurityGroupOwnerId
        ]

instance ToHeaders RevokeCacheSecurityGroupIngress

query

instance AWSRequest RevokeCacheSecurityGroupIngress where
    type Sv RevokeCacheSecurityGroupIngress = ElastiCache
    type Rs RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngressResponse

    request  = post "RevokeCacheSecurityGroupIngress"
    response = xmlResponse

instance FromXML RevokeCacheSecurityGroupIngressResponse where
    parseXML = withElement "RevokeCacheSecurityGroupIngressResult" $ \x -> RevokeCacheSecurityGroupIngressResponse
        <$> x .@? "CacheSecurityGroup"
