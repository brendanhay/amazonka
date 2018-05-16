{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes ingress from a cache security group. Use this operation to disallow access from an Amazon EC2 security group that had been previously authorized.
--
--
module Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
    (
    -- * Creating a Request
      revokeCacheSecurityGroupIngress
    , RevokeCacheSecurityGroupIngress
    -- * Request Lenses
    , rcsgiCacheSecurityGroupName
    , rcsgiEC2SecurityGroupName
    , rcsgiEC2SecurityGroupOwnerId

    -- * Destructuring the Response
    , revokeCacheSecurityGroupIngressResponse
    , RevokeCacheSecurityGroupIngressResponse
    -- * Response Lenses
    , rcsgirsCacheSecurityGroup
    , rcsgirsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @RevokeCacheSecurityGroupIngress@ operation.
--
--
--
-- /See:/ 'revokeCacheSecurityGroupIngress' smart constructor.
data RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngress'
  { _rcsgiCacheSecurityGroupName  :: !Text
  , _rcsgiEC2SecurityGroupName    :: !Text
  , _rcsgiEC2SecurityGroupOwnerId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RevokeCacheSecurityGroupIngress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcsgiCacheSecurityGroupName' - The name of the cache security group to revoke ingress from.
--
-- * 'rcsgiEC2SecurityGroupName' - The name of the Amazon EC2 security group to revoke access from.
--
-- * 'rcsgiEC2SecurityGroupOwnerId' - The AWS account number of the Amazon EC2 security group owner. Note that this is not the same thing as an AWS access key ID - you must provide a valid AWS account number for this parameter.
revokeCacheSecurityGroupIngress
    :: Text -- ^ 'rcsgiCacheSecurityGroupName'
    -> Text -- ^ 'rcsgiEC2SecurityGroupName'
    -> Text -- ^ 'rcsgiEC2SecurityGroupOwnerId'
    -> RevokeCacheSecurityGroupIngress
revokeCacheSecurityGroupIngress pCacheSecurityGroupName_ pEC2SecurityGroupName_ pEC2SecurityGroupOwnerId_ =
  RevokeCacheSecurityGroupIngress'
    { _rcsgiCacheSecurityGroupName = pCacheSecurityGroupName_
    , _rcsgiEC2SecurityGroupName = pEC2SecurityGroupName_
    , _rcsgiEC2SecurityGroupOwnerId = pEC2SecurityGroupOwnerId_
    }


-- | The name of the cache security group to revoke ingress from.
rcsgiCacheSecurityGroupName :: Lens' RevokeCacheSecurityGroupIngress Text
rcsgiCacheSecurityGroupName = lens _rcsgiCacheSecurityGroupName (\ s a -> s{_rcsgiCacheSecurityGroupName = a})

-- | The name of the Amazon EC2 security group to revoke access from.
rcsgiEC2SecurityGroupName :: Lens' RevokeCacheSecurityGroupIngress Text
rcsgiEC2SecurityGroupName = lens _rcsgiEC2SecurityGroupName (\ s a -> s{_rcsgiEC2SecurityGroupName = a})

-- | The AWS account number of the Amazon EC2 security group owner. Note that this is not the same thing as an AWS access key ID - you must provide a valid AWS account number for this parameter.
rcsgiEC2SecurityGroupOwnerId :: Lens' RevokeCacheSecurityGroupIngress Text
rcsgiEC2SecurityGroupOwnerId = lens _rcsgiEC2SecurityGroupOwnerId (\ s a -> s{_rcsgiEC2SecurityGroupOwnerId = a})

instance AWSRequest RevokeCacheSecurityGroupIngress
         where
        type Rs RevokeCacheSecurityGroupIngress =
             RevokeCacheSecurityGroupIngressResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper
              "RevokeCacheSecurityGroupIngressResult"
              (\ s h x ->
                 RevokeCacheSecurityGroupIngressResponse' <$>
                   (x .@? "CacheSecurityGroup") <*> (pure (fromEnum s)))

instance Hashable RevokeCacheSecurityGroupIngress
         where

instance NFData RevokeCacheSecurityGroupIngress where

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
data RevokeCacheSecurityGroupIngressResponse = RevokeCacheSecurityGroupIngressResponse'
  { _rcsgirsCacheSecurityGroup :: !(Maybe CacheSecurityGroup)
  , _rcsgirsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RevokeCacheSecurityGroupIngressResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcsgirsCacheSecurityGroup' - Undocumented member.
--
-- * 'rcsgirsResponseStatus' - -- | The response status code.
revokeCacheSecurityGroupIngressResponse
    :: Int -- ^ 'rcsgirsResponseStatus'
    -> RevokeCacheSecurityGroupIngressResponse
revokeCacheSecurityGroupIngressResponse pResponseStatus_ =
  RevokeCacheSecurityGroupIngressResponse'
    { _rcsgirsCacheSecurityGroup = Nothing
    , _rcsgirsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
rcsgirsCacheSecurityGroup :: Lens' RevokeCacheSecurityGroupIngressResponse (Maybe CacheSecurityGroup)
rcsgirsCacheSecurityGroup = lens _rcsgirsCacheSecurityGroup (\ s a -> s{_rcsgirsCacheSecurityGroup = a})

-- | -- | The response status code.
rcsgirsResponseStatus :: Lens' RevokeCacheSecurityGroupIngressResponse Int
rcsgirsResponseStatus = lens _rcsgirsResponseStatus (\ s a -> s{_rcsgirsResponseStatus = a})

instance NFData
           RevokeCacheSecurityGroupIngressResponse
         where
