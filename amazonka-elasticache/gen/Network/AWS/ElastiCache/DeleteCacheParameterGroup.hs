{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.DeleteCacheParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /DeleteCacheParameterGroup/ action deletes the specified cache
-- parameter group. You cannot delete a cache parameter group if it is
-- associated with any cache clusters.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DeleteCacheParameterGroup.html>
module Network.AWS.ElastiCache.DeleteCacheParameterGroup
    (
    -- * Request
      DeleteCacheParameterGroup
    -- ** Request constructor
    , deleteCacheParameterGroup
    -- ** Request lenses
    , delCacheParameterGroupName

    -- * Response
    , DeleteCacheParameterGroupResponse
    -- ** Response constructor
    , deleteCacheParameterGroupResponse
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DeleteCacheParameterGroup/ action.
--
-- /See:/ 'deleteCacheParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delCacheParameterGroupName'
newtype DeleteCacheParameterGroup = DeleteCacheParameterGroup'
    { _delCacheParameterGroupName :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteCacheParameterGroup' smart constructor.
deleteCacheParameterGroup :: Text -> DeleteCacheParameterGroup
deleteCacheParameterGroup pCacheParameterGroupName =
    DeleteCacheParameterGroup'
    { _delCacheParameterGroupName = pCacheParameterGroupName
    }

-- | The name of the cache parameter group to delete.
--
-- The specified cache security group must not be associated with any cache
-- clusters.
delCacheParameterGroupName :: Lens' DeleteCacheParameterGroup Text
delCacheParameterGroupName = lens _delCacheParameterGroupName (\ s a -> s{_delCacheParameterGroupName = a});

instance AWSRequest DeleteCacheParameterGroup where
        type Sv DeleteCacheParameterGroup = ElastiCache
        type Rs DeleteCacheParameterGroup =
             DeleteCacheParameterGroupResponse
        request = post
        response
          = receiveNull DeleteCacheParameterGroupResponse'

instance ToHeaders DeleteCacheParameterGroup where
        toHeaders = const mempty

instance ToPath DeleteCacheParameterGroup where
        toPath = const "/"

instance ToQuery DeleteCacheParameterGroup where
        toQuery DeleteCacheParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("DeleteCacheParameterGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheParameterGroupName" =:
                 _delCacheParameterGroupName]

-- | /See:/ 'deleteCacheParameterGroupResponse' smart constructor.
data DeleteCacheParameterGroupResponse =
    DeleteCacheParameterGroupResponse'
    deriving (Eq,Read,Show)

-- | 'DeleteCacheParameterGroupResponse' smart constructor.
deleteCacheParameterGroupResponse :: DeleteCacheParameterGroupResponse
deleteCacheParameterGroupResponse = DeleteCacheParameterGroupResponse'
