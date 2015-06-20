{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /AuthorizeCacheSecurityGroupIngress/ action allows network ingress
-- to a cache security group. Applications using ElastiCache must be
-- running on Amazon EC2, and Amazon EC2 security groups are used as the
-- authorization mechanism.
--
-- You cannot authorize ingress from an Amazon EC2 security group in one
-- region to an ElastiCache cluster in another region.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_AuthorizeCacheSecurityGroupIngress.html>
module Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
    (
    -- * Request
      AuthorizeCacheSecurityGroupIngress
    -- ** Request constructor
    , authorizeCacheSecurityGroupIngress
    -- ** Request lenses
    , acsgiCacheSecurityGroupName
    , acsgiEC2SecurityGroupName
    , acsgiEC2SecurityGroupOwnerId

    -- * Response
    , AuthorizeCacheSecurityGroupIngressResponse
    -- ** Response constructor
    , authorizeCacheSecurityGroupIngressResponse
    -- ** Response lenses
    , acsgirCacheSecurityGroup
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'authorizeCacheSecurityGroupIngress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acsgiCacheSecurityGroupName'
--
-- * 'acsgiEC2SecurityGroupName'
--
-- * 'acsgiEC2SecurityGroupOwnerId'
data AuthorizeCacheSecurityGroupIngress = AuthorizeCacheSecurityGroupIngress'{_acsgiCacheSecurityGroupName :: Text, _acsgiEC2SecurityGroupName :: Text, _acsgiEC2SecurityGroupOwnerId :: Text} deriving (Eq, Read, Show)

-- | 'AuthorizeCacheSecurityGroupIngress' smart constructor.
authorizeCacheSecurityGroupIngress :: Text -> Text -> Text -> AuthorizeCacheSecurityGroupIngress
authorizeCacheSecurityGroupIngress pCacheSecurityGroupName pEC2SecurityGroupName pEC2SecurityGroupOwnerId = AuthorizeCacheSecurityGroupIngress'{_acsgiCacheSecurityGroupName = pCacheSecurityGroupName, _acsgiEC2SecurityGroupName = pEC2SecurityGroupName, _acsgiEC2SecurityGroupOwnerId = pEC2SecurityGroupOwnerId};

-- | The cache security group which will allow network ingress.
acsgiCacheSecurityGroupName :: Lens' AuthorizeCacheSecurityGroupIngress Text
acsgiCacheSecurityGroupName = lens _acsgiCacheSecurityGroupName (\ s a -> s{_acsgiCacheSecurityGroupName = a});

-- | The Amazon EC2 security group to be authorized for ingress to the cache
-- security group.
acsgiEC2SecurityGroupName :: Lens' AuthorizeCacheSecurityGroupIngress Text
acsgiEC2SecurityGroupName = lens _acsgiEC2SecurityGroupName (\ s a -> s{_acsgiEC2SecurityGroupName = a});

-- | The AWS account number of the Amazon EC2 security group owner. Note that
-- this is not the same thing as an AWS access key ID - you must provide a
-- valid AWS account number for this parameter.
acsgiEC2SecurityGroupOwnerId :: Lens' AuthorizeCacheSecurityGroupIngress Text
acsgiEC2SecurityGroupOwnerId = lens _acsgiEC2SecurityGroupOwnerId (\ s a -> s{_acsgiEC2SecurityGroupOwnerId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest
         AuthorizeCacheSecurityGroupIngress where
        type Sv AuthorizeCacheSecurityGroupIngress =
             ElastiCache
        type Rs AuthorizeCacheSecurityGroupIngress =
             AuthorizeCacheSecurityGroupIngressResponse
        request = post
        response
          = receiveXMLWrapper
              "AuthorizeCacheSecurityGroupIngressResult"
              (\ s h x ->
                 AuthorizeCacheSecurityGroupIngressResponse' <$>
                   (x .@? "CacheSecurityGroup"))

instance ToHeaders AuthorizeCacheSecurityGroupIngress
         where
        toHeaders = const mempty

instance ToPath AuthorizeCacheSecurityGroupIngress
         where
        toPath = const "/"

instance ToQuery AuthorizeCacheSecurityGroupIngress
         where
        toQuery AuthorizeCacheSecurityGroupIngress'{..}
          = mconcat
              ["Action" =:
                 ("AuthorizeCacheSecurityGroupIngress" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheSecurityGroupName" =:
                 _acsgiCacheSecurityGroupName,
               "EC2SecurityGroupName" =: _acsgiEC2SecurityGroupName,
               "EC2SecurityGroupOwnerId" =:
                 _acsgiEC2SecurityGroupOwnerId]

-- | /See:/ 'authorizeCacheSecurityGroupIngressResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acsgirCacheSecurityGroup'
newtype AuthorizeCacheSecurityGroupIngressResponse = AuthorizeCacheSecurityGroupIngressResponse'{_acsgirCacheSecurityGroup :: Maybe CacheSecurityGroup} deriving (Eq, Read, Show)

-- | 'AuthorizeCacheSecurityGroupIngressResponse' smart constructor.
authorizeCacheSecurityGroupIngressResponse :: AuthorizeCacheSecurityGroupIngressResponse
authorizeCacheSecurityGroupIngressResponse = AuthorizeCacheSecurityGroupIngressResponse'{_acsgirCacheSecurityGroup = Nothing};

-- | FIXME: Undocumented member.
acsgirCacheSecurityGroup :: Lens' AuthorizeCacheSecurityGroupIngressResponse (Maybe CacheSecurityGroup)
acsgirCacheSecurityGroup = lens _acsgirCacheSecurityGroup (\ s a -> s{_acsgirCacheSecurityGroup = a});
