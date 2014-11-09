{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
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
-- mechanism.
module Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
    (
    -- * Request
      AuthorizeCacheSecurityGroupIngressMessage
    -- ** Request constructor
    , authorizeCacheSecurityGroupIngressMessage
    -- ** Request lenses
    , acsgimCacheSecurityGroupName
    , acsgimEC2SecurityGroupName
    , acsgimEC2SecurityGroupOwnerId

    -- * Response
    , AuthorizeCacheSecurityGroupIngressResult
    -- ** Response constructor
    , authorizeCacheSecurityGroupIngressResult
    -- ** Response lenses
    , acsgirCacheSecurityGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data AuthorizeCacheSecurityGroupIngressMessage = AuthorizeCacheSecurityGroupIngressMessage
    { _acsgimCacheSecurityGroupName  :: Text
    , _acsgimEC2SecurityGroupName    :: Text
    , _acsgimEC2SecurityGroupOwnerId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AuthorizeCacheSecurityGroupIngressMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acsgimCacheSecurityGroupName' @::@ 'Text'
--
-- * 'acsgimEC2SecurityGroupName' @::@ 'Text'
--
-- * 'acsgimEC2SecurityGroupOwnerId' @::@ 'Text'
--
authorizeCacheSecurityGroupIngressMessage :: Text -- ^ 'acsgimCacheSecurityGroupName'
                                          -> Text -- ^ 'acsgimEC2SecurityGroupName'
                                          -> Text -- ^ 'acsgimEC2SecurityGroupOwnerId'
                                          -> AuthorizeCacheSecurityGroupIngressMessage
authorizeCacheSecurityGroupIngressMessage p1 p2 p3 = AuthorizeCacheSecurityGroupIngressMessage
    { _acsgimCacheSecurityGroupName  = p1
    , _acsgimEC2SecurityGroupName    = p2
    , _acsgimEC2SecurityGroupOwnerId = p3
    }

-- | The cache security group which will allow network ingress.
acsgimCacheSecurityGroupName :: Lens' AuthorizeCacheSecurityGroupIngressMessage Text
acsgimCacheSecurityGroupName =
    lens _acsgimCacheSecurityGroupName
        (\s a -> s { _acsgimCacheSecurityGroupName = a })

-- | The Amazon EC2 security group to be authorized for ingress to the cache
-- security group.
acsgimEC2SecurityGroupName :: Lens' AuthorizeCacheSecurityGroupIngressMessage Text
acsgimEC2SecurityGroupName =
    lens _acsgimEC2SecurityGroupName
        (\s a -> s { _acsgimEC2SecurityGroupName = a })

-- | The AWS account number of the Amazon EC2 security group owner. Note that
-- this is not the same thing as an AWS access key ID - you must provide a
-- valid AWS account number for this parameter.
acsgimEC2SecurityGroupOwnerId :: Lens' AuthorizeCacheSecurityGroupIngressMessage Text
acsgimEC2SecurityGroupOwnerId =
    lens _acsgimEC2SecurityGroupOwnerId
        (\s a -> s { _acsgimEC2SecurityGroupOwnerId = a })

instance ToPath AuthorizeCacheSecurityGroupIngressMessage where
    toPath = const "/"

instance ToQuery AuthorizeCacheSecurityGroupIngressMessage

newtype AuthorizeCacheSecurityGroupIngressResult = AuthorizeCacheSecurityGroupIngressResult
    { _acsgirCacheSecurityGroup :: Maybe CacheSecurityGroup
    } deriving (Eq, Show, Generic)

-- | 'AuthorizeCacheSecurityGroupIngressResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acsgirCacheSecurityGroup' @::@ 'Maybe' 'CacheSecurityGroup'
--
authorizeCacheSecurityGroupIngressResult :: AuthorizeCacheSecurityGroupIngressResult
authorizeCacheSecurityGroupIngressResult = AuthorizeCacheSecurityGroupIngressResult
    { _acsgirCacheSecurityGroup = Nothing
    }

acsgirCacheSecurityGroup :: Lens' AuthorizeCacheSecurityGroupIngressResult (Maybe CacheSecurityGroup)
acsgirCacheSecurityGroup =
    lens _acsgirCacheSecurityGroup
        (\s a -> s { _acsgirCacheSecurityGroup = a })

instance AWSRequest AuthorizeCacheSecurityGroupIngressMessage where
    type Sv AuthorizeCacheSecurityGroupIngressMessage = ElastiCache
    type Rs AuthorizeCacheSecurityGroupIngressMessage = AuthorizeCacheSecurityGroupIngressResult

    request  = post "AuthorizeCacheSecurityGroupIngress"
    response = const . xmlResponse $ \h x -> AuthorizeCacheSecurityGroupIngressResult
newtype
