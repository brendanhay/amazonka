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
module Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
    (
    -- * Request
      RevokeCacheSecurityGroupIngressMessage
    -- ** Request constructor
    , revokeCacheSecurityGroupIngress
    -- ** Request lenses
    , rcsgimCacheSecurityGroupName
    , rcsgimEC2SecurityGroupName
    , rcsgimEC2SecurityGroupOwnerId

    -- * Response
    , RevokeCacheSecurityGroupIngressResult
    -- ** Response constructor
    , revokeCacheSecurityGroupIngressResponse
    -- ** Response lenses
    , rcsgirCacheSecurityGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data RevokeCacheSecurityGroupIngressMessage = RevokeCacheSecurityGroupIngressMessage
    { _rcsgimCacheSecurityGroupName  :: Text
    , _rcsgimEC2SecurityGroupName    :: Text
    , _rcsgimEC2SecurityGroupOwnerId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RevokeCacheSecurityGroupIngressMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcsgimCacheSecurityGroupName' @::@ 'Text'
--
-- * 'rcsgimEC2SecurityGroupName' @::@ 'Text'
--
-- * 'rcsgimEC2SecurityGroupOwnerId' @::@ 'Text'
--
revokeCacheSecurityGroupIngress :: Text -- ^ 'rcsgimCacheSecurityGroupName'
                                -> Text -- ^ 'rcsgimEC2SecurityGroupName'
                                -> Text -- ^ 'rcsgimEC2SecurityGroupOwnerId'
                                -> RevokeCacheSecurityGroupIngressMessage
revokeCacheSecurityGroupIngress p1 p2 p3 = RevokeCacheSecurityGroupIngressMessage
    { _rcsgimCacheSecurityGroupName  = p1
    , _rcsgimEC2SecurityGroupName    = p2
    , _rcsgimEC2SecurityGroupOwnerId = p3
    }

-- | The name of the cache security group to revoke ingress from.
rcsgimCacheSecurityGroupName :: Lens' RevokeCacheSecurityGroupIngressMessage Text
rcsgimCacheSecurityGroupName =
    lens _rcsgimCacheSecurityGroupName
        (\s a -> s { _rcsgimCacheSecurityGroupName = a })

-- | The name of the Amazon EC2 security group to revoke access from.
rcsgimEC2SecurityGroupName :: Lens' RevokeCacheSecurityGroupIngressMessage Text
rcsgimEC2SecurityGroupName =
    lens _rcsgimEC2SecurityGroupName
        (\s a -> s { _rcsgimEC2SecurityGroupName = a })

-- | The AWS account number of the Amazon EC2 security group owner. Note that
-- this is not the same thing as an AWS access key ID - you must provide a
-- valid AWS account number for this parameter.
rcsgimEC2SecurityGroupOwnerId :: Lens' RevokeCacheSecurityGroupIngressMessage Text
rcsgimEC2SecurityGroupOwnerId =
    lens _rcsgimEC2SecurityGroupOwnerId
        (\s a -> s { _rcsgimEC2SecurityGroupOwnerId = a })

instance ToPath RevokeCacheSecurityGroupIngressMessage where
    toPath = const "/"

instance ToQuery RevokeCacheSecurityGroupIngressMessage

newtype RevokeCacheSecurityGroupIngressResult = RevokeCacheSecurityGroupIngressResult
    { _rcsgirCacheSecurityGroup :: Maybe CacheSecurityGroup
    } deriving (Eq, Show, Generic)

-- | 'RevokeCacheSecurityGroupIngressResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcsgirCacheSecurityGroup' @::@ 'Maybe' 'CacheSecurityGroup'
--
revokeCacheSecurityGroupIngressResponse :: RevokeCacheSecurityGroupIngressResult
revokeCacheSecurityGroupIngressResponse = RevokeCacheSecurityGroupIngressResult
    { _rcsgirCacheSecurityGroup = Nothing
    }

rcsgirCacheSecurityGroup :: Lens' RevokeCacheSecurityGroupIngressResult (Maybe CacheSecurityGroup)
rcsgirCacheSecurityGroup =
    lens _rcsgirCacheSecurityGroup
        (\s a -> s { _rcsgirCacheSecurityGroup = a })

instance AWSRequest RevokeCacheSecurityGroupIngressMessage where
    type Sv RevokeCacheSecurityGroupIngressMessage = ElastiCache
    type Rs RevokeCacheSecurityGroupIngressMessage = RevokeCacheSecurityGroupIngressResult

    request  = post "RevokeCacheSecurityGroupIngress"
    response = xmlResponse $ \h x -> RevokeCacheSecurityGroupIngressResult
        <$> x %| "CacheSecurityGroup"
