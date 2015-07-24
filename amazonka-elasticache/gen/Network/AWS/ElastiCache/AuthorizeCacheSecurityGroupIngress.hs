{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /AuthorizeCacheSecurityGroupIngress/ action allows network ingress
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
    , acsgirsCacheSecurityGroup
    , acsgirsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of an /AuthorizeCacheSecurityGroupIngress/ action.
--
-- /See:/ 'authorizeCacheSecurityGroupIngress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acsgiCacheSecurityGroupName'
--
-- * 'acsgiEC2SecurityGroupName'
--
-- * 'acsgiEC2SecurityGroupOwnerId'
data AuthorizeCacheSecurityGroupIngress = AuthorizeCacheSecurityGroupIngress'
    { _acsgiCacheSecurityGroupName  :: !Text
    , _acsgiEC2SecurityGroupName    :: !Text
    , _acsgiEC2SecurityGroupOwnerId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AuthorizeCacheSecurityGroupIngress' smart constructor.
authorizeCacheSecurityGroupIngress :: Text -> Text -> Text -> AuthorizeCacheSecurityGroupIngress
authorizeCacheSecurityGroupIngress pCacheSecurityGroupName_ pEC2SecurityGroupName_ pEC2SecurityGroupOwnerId_ =
    AuthorizeCacheSecurityGroupIngress'
    { _acsgiCacheSecurityGroupName = pCacheSecurityGroupName_
    , _acsgiEC2SecurityGroupName = pEC2SecurityGroupName_
    , _acsgiEC2SecurityGroupOwnerId = pEC2SecurityGroupOwnerId_
    }

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

instance AWSRequest
         AuthorizeCacheSecurityGroupIngress where
        type Sv AuthorizeCacheSecurityGroupIngress =
             ElastiCache
        type Rs AuthorizeCacheSecurityGroupIngress =
             AuthorizeCacheSecurityGroupIngressResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "AuthorizeCacheSecurityGroupIngressResult"
              (\ s h x ->
                 AuthorizeCacheSecurityGroupIngressResponse' <$>
                   (x .@? "CacheSecurityGroup") <*> (pure (fromEnum s)))

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
-- * 'acsgirsCacheSecurityGroup'
--
-- * 'acsgirsStatus'
data AuthorizeCacheSecurityGroupIngressResponse = AuthorizeCacheSecurityGroupIngressResponse'
    { _acsgirsCacheSecurityGroup :: !(Maybe CacheSecurityGroup)
    , _acsgirsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AuthorizeCacheSecurityGroupIngressResponse' smart constructor.
authorizeCacheSecurityGroupIngressResponse :: Int -> AuthorizeCacheSecurityGroupIngressResponse
authorizeCacheSecurityGroupIngressResponse pStatus_ =
    AuthorizeCacheSecurityGroupIngressResponse'
    { _acsgirsCacheSecurityGroup = Nothing
    , _acsgirsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
acsgirsCacheSecurityGroup :: Lens' AuthorizeCacheSecurityGroupIngressResponse (Maybe CacheSecurityGroup)
acsgirsCacheSecurityGroup = lens _acsgirsCacheSecurityGroup (\ s a -> s{_acsgirsCacheSecurityGroup = a});

-- | FIXME: Undocumented member.
acsgirsStatus :: Lens' AuthorizeCacheSecurityGroupIngressResponse Int
acsgirsStatus = lens _acsgirsStatus (\ s a -> s{_acsgirsStatus = a});
