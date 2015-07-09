{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteCacheSubnetGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | The /DeleteCacheSubnetGroup/ action deletes a cache subnet group.
--
-- You cannot delete a cache subnet group if it is associated with any
-- cache clusters.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DeleteCacheSubnetGroup.html>
module Network.AWS.ElastiCache.DeleteCacheSubnetGroup
    (
    -- * Request
      DeleteCacheSubnetGroup
    -- ** Request constructor
    , deleteCacheSubnetGroup
    -- ** Request lenses
    , delCacheSubnetGroupName

    -- * Response
    , DeleteCacheSubnetGroupResponse
    -- ** Response constructor
    , deleteCacheSubnetGroupResponse
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DeleteCacheSubnetGroup/ action.
--
-- /See:/ 'deleteCacheSubnetGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delCacheSubnetGroupName'
newtype DeleteCacheSubnetGroup = DeleteCacheSubnetGroup'
    { _delCacheSubnetGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCacheSubnetGroup' smart constructor.
deleteCacheSubnetGroup :: Text -> DeleteCacheSubnetGroup
deleteCacheSubnetGroup pCacheSubnetGroupName =
    DeleteCacheSubnetGroup'
    { _delCacheSubnetGroupName = pCacheSubnetGroupName
    }

-- | The name of the cache subnet group to delete.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or
-- hyphens.
delCacheSubnetGroupName :: Lens' DeleteCacheSubnetGroup Text
delCacheSubnetGroupName = lens _delCacheSubnetGroupName (\ s a -> s{_delCacheSubnetGroupName = a});

instance AWSRequest DeleteCacheSubnetGroup where
        type Sv DeleteCacheSubnetGroup = ElastiCache
        type Rs DeleteCacheSubnetGroup =
             DeleteCacheSubnetGroupResponse
        request = post
        response
          = receiveNull DeleteCacheSubnetGroupResponse'

instance ToHeaders DeleteCacheSubnetGroup where
        toHeaders = const mempty

instance ToPath DeleteCacheSubnetGroup where
        toPath = const "/"

instance ToQuery DeleteCacheSubnetGroup where
        toQuery DeleteCacheSubnetGroup'{..}
          = mconcat
              ["Action" =:
                 ("DeleteCacheSubnetGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheSubnetGroupName" =: _delCacheSubnetGroupName]

-- | /See:/ 'deleteCacheSubnetGroupResponse' smart constructor.
data DeleteCacheSubnetGroupResponse =
    DeleteCacheSubnetGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCacheSubnetGroupResponse' smart constructor.
deleteCacheSubnetGroupResponse :: DeleteCacheSubnetGroupResponse
deleteCacheSubnetGroupResponse = DeleteCacheSubnetGroupResponse'
