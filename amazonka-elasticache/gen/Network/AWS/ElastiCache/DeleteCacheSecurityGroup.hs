{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElastiCache.DeleteCacheSecurityGroup
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

-- | The /DeleteCacheSecurityGroup/ action deletes a cache security group.
--
-- You cannot delete a cache security group if it is associated with any
-- cache clusters.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DeleteCacheSecurityGroup.html>
module Network.AWS.ElastiCache.DeleteCacheSecurityGroup
    (
    -- * Request
      DeleteCacheSecurityGroup
    -- ** Request constructor
    , deleteCacheSecurityGroup
    -- ** Request lenses
    , dcsgCacheSecurityGroupName

    -- * Response
    , DeleteCacheSecurityGroupResponse
    -- ** Response constructor
    , deleteCacheSecurityGroupResponse
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCacheSecurityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgCacheSecurityGroupName'
newtype DeleteCacheSecurityGroup = DeleteCacheSecurityGroup'{_dcsgCacheSecurityGroupName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteCacheSecurityGroup' smart constructor.
deleteCacheSecurityGroup :: Text -> DeleteCacheSecurityGroup
deleteCacheSecurityGroup pCacheSecurityGroupName = DeleteCacheSecurityGroup'{_dcsgCacheSecurityGroupName = pCacheSecurityGroupName};

-- | The name of the cache security group to delete.
--
-- You cannot delete the default security group.
dcsgCacheSecurityGroupName :: Lens' DeleteCacheSecurityGroup Text
dcsgCacheSecurityGroupName = lens _dcsgCacheSecurityGroupName (\ s a -> s{_dcsgCacheSecurityGroupName = a});

instance AWSRequest DeleteCacheSecurityGroup where
        type Sv DeleteCacheSecurityGroup = ElastiCache
        type Rs DeleteCacheSecurityGroup =
             DeleteCacheSecurityGroupResponse
        request = post
        response
          = receiveNull DeleteCacheSecurityGroupResponse'

instance ToHeaders DeleteCacheSecurityGroup where
        toHeaders = const mempty

instance ToPath DeleteCacheSecurityGroup where
        toPath = const "/"

instance ToQuery DeleteCacheSecurityGroup where
        toQuery DeleteCacheSecurityGroup'{..}
          = mconcat
              ["Action" =:
                 ("DeleteCacheSecurityGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheSecurityGroupName" =:
                 _dcsgCacheSecurityGroupName]

-- | /See:/ 'deleteCacheSecurityGroupResponse' smart constructor.
data DeleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse' deriving (Eq, Read, Show)

-- | 'DeleteCacheSecurityGroupResponse' smart constructor.
deleteCacheSecurityGroupResponse :: DeleteCacheSecurityGroupResponse
deleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse';
