{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /RevokeCacheSecurityGroupIngress/ action revokes ingress from a
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
    , rcsgirqCacheSecurityGroupName
    , rcsgirqEC2SecurityGroupName
    , rcsgirqEC2SecurityGroupOwnerId

    -- * Response
    , RevokeCacheSecurityGroupIngressResponse
    -- ** Response constructor
    , revokeCacheSecurityGroupIngressResponse
    -- ** Response lenses
    , rcsgirsCacheSecurityGroup
    , rcsgirsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /RevokeCacheSecurityGroupIngress/ action.
--
-- /See:/ 'revokeCacheSecurityGroupIngress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcsgirqCacheSecurityGroupName'
--
-- * 'rcsgirqEC2SecurityGroupName'
--
-- * 'rcsgirqEC2SecurityGroupOwnerId'
data RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngress'
    { _rcsgirqCacheSecurityGroupName  :: !Text
    , _rcsgirqEC2SecurityGroupName    :: !Text
    , _rcsgirqEC2SecurityGroupOwnerId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RevokeCacheSecurityGroupIngress' smart constructor.
revokeCacheSecurityGroupIngress :: Text -> Text -> Text -> RevokeCacheSecurityGroupIngress
revokeCacheSecurityGroupIngress pCacheSecurityGroupName_ pEC2SecurityGroupName_ pEC2SecurityGroupOwnerId_ =
    RevokeCacheSecurityGroupIngress'
    { _rcsgirqCacheSecurityGroupName = pCacheSecurityGroupName_
    , _rcsgirqEC2SecurityGroupName = pEC2SecurityGroupName_
    , _rcsgirqEC2SecurityGroupOwnerId = pEC2SecurityGroupOwnerId_
    }

-- | The name of the cache security group to revoke ingress from.
rcsgirqCacheSecurityGroupName :: Lens' RevokeCacheSecurityGroupIngress Text
rcsgirqCacheSecurityGroupName = lens _rcsgirqCacheSecurityGroupName (\ s a -> s{_rcsgirqCacheSecurityGroupName = a});

-- | The name of the Amazon EC2 security group to revoke access from.
rcsgirqEC2SecurityGroupName :: Lens' RevokeCacheSecurityGroupIngress Text
rcsgirqEC2SecurityGroupName = lens _rcsgirqEC2SecurityGroupName (\ s a -> s{_rcsgirqEC2SecurityGroupName = a});

-- | The AWS account number of the Amazon EC2 security group owner. Note that
-- this is not the same thing as an AWS access key ID - you must provide a
-- valid AWS account number for this parameter.
rcsgirqEC2SecurityGroupOwnerId :: Lens' RevokeCacheSecurityGroupIngress Text
rcsgirqEC2SecurityGroupOwnerId = lens _rcsgirqEC2SecurityGroupOwnerId (\ s a -> s{_rcsgirqEC2SecurityGroupOwnerId = a});

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
                 _rcsgirqCacheSecurityGroupName,
               "EC2SecurityGroupName" =:
                 _rcsgirqEC2SecurityGroupName,
               "EC2SecurityGroupOwnerId" =:
                 _rcsgirqEC2SecurityGroupOwnerId]

-- | /See:/ 'revokeCacheSecurityGroupIngressResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcsgirsCacheSecurityGroup'
--
-- * 'rcsgirsStatus'
data RevokeCacheSecurityGroupIngressResponse = RevokeCacheSecurityGroupIngressResponse'
    { _rcsgirsCacheSecurityGroup :: !(Maybe CacheSecurityGroup)
    , _rcsgirsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RevokeCacheSecurityGroupIngressResponse' smart constructor.
revokeCacheSecurityGroupIngressResponse :: Int -> RevokeCacheSecurityGroupIngressResponse
revokeCacheSecurityGroupIngressResponse pStatus_ =
    RevokeCacheSecurityGroupIngressResponse'
    { _rcsgirsCacheSecurityGroup = Nothing
    , _rcsgirsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
rcsgirsCacheSecurityGroup :: Lens' RevokeCacheSecurityGroupIngressResponse (Maybe CacheSecurityGroup)
rcsgirsCacheSecurityGroup = lens _rcsgirsCacheSecurityGroup (\ s a -> s{_rcsgirsCacheSecurityGroup = a});

-- | FIXME: Undocumented member.
rcsgirsStatus :: Lens' RevokeCacheSecurityGroupIngressResponse Int
rcsgirsStatus = lens _rcsgirsStatus (\ s a -> s{_rcsgirsStatus = a});
