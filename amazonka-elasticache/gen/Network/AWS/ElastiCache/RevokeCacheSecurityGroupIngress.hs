{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
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

-- | The /RevokeCacheSecurityGroupIngress/ action revokes ingress from a
-- cache security group. Use this action to disallow access from an Amazon
-- EC2 security group that had been previously authorized.
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
    , rcsgirStatusCode
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a /RevokeCacheSecurityGroupIngress/ action.
--
-- /See:/ 'revokeCacheSecurityGroupIngress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcsgiCacheSecurityGroupName'
--
-- * 'rcsgiEC2SecurityGroupName'
--
-- * 'rcsgiEC2SecurityGroupOwnerId'
data RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngress'{_rcsgiCacheSecurityGroupName :: Text, _rcsgiEC2SecurityGroupName :: Text, _rcsgiEC2SecurityGroupOwnerId :: Text} deriving (Eq, Read, Show)

-- | 'RevokeCacheSecurityGroupIngress' smart constructor.
revokeCacheSecurityGroupIngress :: Text -> Text -> Text -> RevokeCacheSecurityGroupIngress
revokeCacheSecurityGroupIngress pCacheSecurityGroupName pEC2SecurityGroupName pEC2SecurityGroupOwnerId = RevokeCacheSecurityGroupIngress'{_rcsgiCacheSecurityGroupName = pCacheSecurityGroupName, _rcsgiEC2SecurityGroupName = pEC2SecurityGroupName, _rcsgiEC2SecurityGroupOwnerId = pEC2SecurityGroupOwnerId};

-- | The name of the cache security group to revoke ingress from.
rcsgiCacheSecurityGroupName :: Lens' RevokeCacheSecurityGroupIngress Text
rcsgiCacheSecurityGroupName = lens _rcsgiCacheSecurityGroupName (\ s a -> s{_rcsgiCacheSecurityGroupName = a});

-- | The name of the Amazon EC2 security group to revoke access from.
rcsgiEC2SecurityGroupName :: Lens' RevokeCacheSecurityGroupIngress Text
rcsgiEC2SecurityGroupName = lens _rcsgiEC2SecurityGroupName (\ s a -> s{_rcsgiEC2SecurityGroupName = a});

-- | The AWS account number of the Amazon EC2 security group owner. Note that
-- this is not the same thing as an AWS access key ID - you must provide a
-- valid AWS account number for this parameter.
rcsgiEC2SecurityGroupOwnerId :: Lens' RevokeCacheSecurityGroupIngress Text
rcsgiEC2SecurityGroupOwnerId = lens _rcsgiEC2SecurityGroupOwnerId (\ s a -> s{_rcsgiEC2SecurityGroupOwnerId = a});

instance AWSRequest RevokeCacheSecurityGroupIngress
         where
        type Sv RevokeCacheSecurityGroupIngress = ElastiCache
        type Rs RevokeCacheSecurityGroupIngress =
             RevokeCacheSecurityGroupIngressResponse
        request = post
        response
          = receiveXMLWrapper
              "RevokeCacheSecurityGroupIngressResult"
              (\ s h x ->
                 RevokeCacheSecurityGroupIngressResponse' <$>
                   (x .@? "CacheSecurityGroup") <*> (pure (fromEnum s)))

instance ToHeaders RevokeCacheSecurityGroupIngress
         where
        toHeaders = const mempty

instance ToPath RevokeCacheSecurityGroupIngress where
        toPath = const "/"

instance ToQuery RevokeCacheSecurityGroupIngress
         where
        toQuery RevokeCacheSecurityGroupIngress'{..}
          = mconcat
              ["Action" =:
                 ("RevokeCacheSecurityGroupIngress" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheSecurityGroupName" =:
                 _rcsgiCacheSecurityGroupName,
               "EC2SecurityGroupName" =: _rcsgiEC2SecurityGroupName,
               "EC2SecurityGroupOwnerId" =:
                 _rcsgiEC2SecurityGroupOwnerId]

-- | /See:/ 'revokeCacheSecurityGroupIngressResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcsgirCacheSecurityGroup'
--
-- * 'rcsgirStatusCode'
data RevokeCacheSecurityGroupIngressResponse = RevokeCacheSecurityGroupIngressResponse'{_rcsgirCacheSecurityGroup :: Maybe CacheSecurityGroup, _rcsgirStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'RevokeCacheSecurityGroupIngressResponse' smart constructor.
revokeCacheSecurityGroupIngressResponse :: Int -> RevokeCacheSecurityGroupIngressResponse
revokeCacheSecurityGroupIngressResponse pStatusCode = RevokeCacheSecurityGroupIngressResponse'{_rcsgirCacheSecurityGroup = Nothing, _rcsgirStatusCode = pStatusCode};

-- | FIXME: Undocumented member.
rcsgirCacheSecurityGroup :: Lens' RevokeCacheSecurityGroupIngressResponse (Maybe CacheSecurityGroup)
rcsgirCacheSecurityGroup = lens _rcsgirCacheSecurityGroup (\ s a -> s{_rcsgirCacheSecurityGroup = a});

-- | FIXME: Undocumented member.
rcsgirStatusCode :: Lens' RevokeCacheSecurityGroupIngressResponse Int
rcsgirStatusCode = lens _rcsgirStatusCode (\ s a -> s{_rcsgirStatusCode = a});
